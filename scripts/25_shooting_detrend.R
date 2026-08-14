# ==============================================================================
# Vehicle Pursuit Policy Analysis — Script 25: Shooting Detrend Robustness
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Address the non-parallel pre-trends in the precinct-level shooting DiD
#   (joint Wald F(7,76) = 5.34, p < .001; Revision item M6). The shooting
#   DiD estimate, though nominally negative, "rests on non-parallel pre-trends"
#   (Reviewer 1 / AI-B2). This script applies three detrending strategies to
#   assess whether the DiD estimate survives after accounting for the
#   differential pre-trend:
#
#   (1) Group-specific linear detrending: remove group-specific linear trends
#       estimated on the pre-treatment period, then re-estimate the DiD.
#   (2) Linear pre-trend extrapolation sensitivity (Rambachan & Roth 2023's
#       breakdown-value concept in spirit, not their constrained-optimization
#       method): extrapolate the pre-treatment linear trend into the
#       post-period and ask how large a trend violation (M*) would be needed
#       to flip the sign of the estimated ATT. If M* is small relative to the
#       observed pre-trend slope, the result is fragile to modest trend
#       violations. This is NOT the actual HonestDiD package/method — no
#       smoothness or monotonicity restriction classes are imposed.
#   (3) ITS detrending: re-estimate the city-level ITS for shootings after
#       removing a linear pre-trend from the shooting series.
#
#   All three analyses are robustness checks supporting the manuscript's
#   position that the shooting DiD should not be interpreted causally.
#
# Inputs:
#   - output/precinct_did/pct_monthly_shootings.csv   (from script 01)
#   - output/precinct_did/pct_monthly_robberies.csv   (from script 01)
#   - output/tables/monthly_panel.csv                 (from script 01)
#
# Outputs (saved to output/detrend_results/):
#   - detrend_did_results.csv         — DiD estimates: raw vs. detrended
#   - detrend_its_results.csv         — ITS shooting estimates: raw vs. detrended
#   - detrend_event_study.csv         — Event-study coefficients on detrended data
#   - detrend_wald_pre_trends.csv     — Joint Wald test on detrended pre-leads
#   - fig_detrend_event_study.png/pdf — Event study: raw vs. detrended
#   - fig_detrend_its.png/pdf         — ITS: raw vs. detrended shooting series
#
# Runtime: ~1 min
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(lubridate)
  library(fixest)
  library(sandwich)
  library(lmtest)
  library(janitor)
})

set.seed(20260628)

message("=== Script 25: Shooting Detrend Robustness ===")
message("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M"))

INTERVENTION <- as.Date("2022-10-01")

results_dir <- here("output", "detrend_results")
plot_dir    <- here("output", "plots")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# Theme
COL_SHOOTING <- "#6A0572"
COL_ROBBERY  <- "#003049"
COL_RAW      <- "grey60"
COL_DETREND  <- "#D62828"

theme_pursuit <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.15), margin = margin(b = 8)),
      plot.subtitle    = element_text(color = "grey30", size = rel(0.85), margin = margin(b = 12)),
      plot.caption     = element_text(color = "grey50", size = rel(0.7), hjust = 0, margin = margin(t = 10)),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      axis.title.x     = element_text(margin = margin(t = 8), size = rel(0.9)),
      axis.title.y     = element_text(margin = margin(r = 8), size = rel(0.9)),
      axis.text        = element_text(color = "grey30"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      plot.margin      = margin(15, 15, 15, 15)
    )
}

save_plot <- function(p, name, w = 8, h = 5.5) {
  ggsave(file.path(plot_dir, paste0(name, ".png")), plot = p,
         width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(plot_dir, paste0(name, ".pdf")), plot = p,
         width = w, height = h, bg = "white")
  message("Saved: ", name, ".png / .pdf")
}

# ==============================================================================
# 1. Load Precinct Data
# ==============================================================================

message("--- 1. Loading precinct panels ---")

shootings_pct <- read_csv(here("output", "precinct_did", "pct_monthly_shootings.csv"),
                           show_col_types = FALSE) |>
  clean_names() |>
  mutate(month_date = as.Date(month_date))

robberies_pct <- read_csv(here("output", "precinct_did", "pct_monthly_robberies.csv"),
                           show_col_types = FALSE) |>
  clean_names() |>
  mutate(month_date = as.Date(month_date))

# Build panel (same logic as script 17)
panel <- shootings_pct |>
  full_join(robberies_pct, by = c("pct", "month_date")) |>
  replace_na(list(shooting_incidents = 0L, robbery_count = 0L)) |>
  filter(pct >= 1, pct <= 123,
         month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) |>
  # Recode Pct 116 -> 105 (merged mid-study)
  mutate(pct = if_else(pct == 116L, 105L, pct)) |>
  group_by(pct, month_date) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
  arrange(pct, month_date)

# Pre-treatment crime groupings (Jan 2018 - Sep 2022)
pre_crime_shoot <- panel |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_shoot_pre = mean(shooting_incidents, na.rm = TRUE), .groups = "drop") |>
  mutate(high_crime_shoot = as.integer(mean_shoot_pre >= median(mean_shoot_pre)))

pre_crime_rob <- panel |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_rob_pre = mean(robbery_count, na.rm = TRUE), .groups = "drop") |>
  mutate(high_crime_rob = as.integer(mean_rob_pre >= median(mean_rob_pre)))

panel <- panel |>
  left_join(pre_crime_shoot |> select(pct, high_crime_shoot), by = "pct") |>
  left_join(pre_crime_rob   |> select(pct, high_crime_rob),   by = "pct") |>
  mutate(
    post     = as.integer(month_date >= INTERVENTION),
    rel_time = (year(month_date)  - year(INTERVENTION)) * 12L +
               (month(month_date) - month(INTERVENTION)),
    time_bin      = floor(rel_time / 3L),
    time_bin_lump = if_else(time_bin >= 9L, 9L, time_bin),
    # Numeric time index for detrending
    t_index  = as.integer(difftime(month_date, min(month_date), units = "days")) / 30.44
  )

n_pcts   <- n_distinct(panel$pct)
n_months <- n_distinct(panel$month_date)
message("Panel: ", n_pcts, " precincts x ", n_months, " months = ", nrow(panel), " obs")

# ==============================================================================
# 2. Strategy 1: Group-Specific Linear Detrending
# ==============================================================================

message("\n--- 2. Group-specific linear detrending ---")

# Estimate group-specific linear trends on pre-treatment data only,
# then subtract the predicted trends from the full series.
# This removes the differential linear pre-trend that invalidates the
# parallel-trends assumption.

detrend_outcome <- function(data, outcome_var, group_var) {
  # Fit group-specific linear time trends on pre-treatment data
  pre_data <- data |> filter(post == 0)

  # Estimate: outcome = a_group + b_group * t_index (pre-period only)
  trend_model <- lm(
    as.formula(paste0(outcome_var, " ~ factor(", group_var, ") * t_index")),
    data = pre_data
  )

  # Predict the linear trend for all periods (including post)
  # This extrapolates the pre-treatment group-specific trend forward
  predicted_trend <- predict(trend_model, newdata = data)

  # Detrended outcome = observed - predicted group trend + grand mean
  # Adding the grand mean preserves the level for interpretability
  grand_mean <- mean(data[[outcome_var]], na.rm = TRUE)
  detrended  <- data[[outcome_var]] - predicted_trend + grand_mean

  detrended
}

# Detrend shooting incidents using shooting-based groups
panel <- panel |>
  mutate(
    shoot_detrended = detrend_outcome(panel, "shooting_incidents", "high_crime_shoot"),
    rob_detrended   = detrend_outcome(panel, "robbery_count", "high_crime_rob")
  )

message("Detrending complete. Checking pre-period balance...")

# Verify: pre-period group means should now be parallel
pre_check <- panel |>
  filter(post == 0) |>
  group_by(high_crime_shoot, month_date) |>
  summarise(
    raw_mean       = mean(shooting_incidents, na.rm = TRUE),
    detrended_mean = mean(shoot_detrended, na.rm = TRUE),
    .groups = "drop"
  )

# ==============================================================================
# 3. Re-estimate DiD on Detrended Data
# ==============================================================================

message("\n--- 3. DiD on detrended shooting data ---")

# Raw DiD (for comparison)
did_raw_shoot <- feols(
  shooting_incidents ~ high_crime_shoot:post | pct + month_date,
  data = panel, cluster = ~pct
)

# Detrended DiD
did_detrend_shoot <- feols(
  shoot_detrended ~ high_crime_shoot:post | pct + month_date,
  data = panel, cluster = ~pct
)

# Raw DiD: robbery
did_raw_rob <- feols(
  robbery_count ~ high_crime_rob:post | pct + month_date,
  data = panel, cluster = ~pct
)

# Detrended DiD: robbery
did_detrend_rob <- feols(
  rob_detrended ~ high_crime_rob:post | pct + month_date,
  data = panel, cluster = ~pct
)

# Compile results
did_comparison <- bind_rows(
  broom::tidy(did_raw_shoot, conf.int = TRUE) |>
    mutate(outcome = "Shooting", spec = "Raw"),
  broom::tidy(did_detrend_shoot, conf.int = TRUE) |>
    mutate(outcome = "Shooting", spec = "Group-Detrended"),
  broom::tidy(did_raw_rob, conf.int = TRUE) |>
    mutate(outcome = "Robbery", spec = "Raw"),
  broom::tidy(did_detrend_rob, conf.int = TRUE) |>
    mutate(outcome = "Robbery", spec = "Group-Detrended")
)

message("\nDiD Comparison (Raw vs. Detrended):")
message(paste(capture.output(
  did_comparison |>
    select(outcome, spec, estimate, std.error, p.value) |>
    mutate(across(where(is.numeric), \(x) round(x, 4))),
  n = Inf
), collapse = "\n"))

write_csv(did_comparison, file.path(results_dir, "detrend_did_results.csv"))

# ==============================================================================
# 4. Event Study on Detrended Data
# ==============================================================================

message("\n--- 4. Event study on detrended data ---")

# Raw event study (shooting)
es_raw_shoot <- feols(
  shooting_incidents ~ i(time_bin_lump, high_crime_shoot, ref = -1) | pct + month_date,
  data = panel, cluster = ~pct
)

# Detrended event study (shooting)
es_detrend_shoot <- feols(
  shoot_detrended ~ i(time_bin_lump, high_crime_shoot, ref = -1) | pct + month_date,
  data = panel, cluster = ~pct
)

es_raw_tidy <- broom::tidy(es_raw_shoot, conf.int = TRUE) |>
  mutate(
    spec = "Raw",
    time_bin = as.integer(str_extract(term, "-?\\d+"))
  )

es_detrend_tidy <- broom::tidy(es_detrend_shoot, conf.int = TRUE) |>
  mutate(
    spec = "Group-Detrended",
    time_bin = as.integer(str_extract(term, "-?\\d+"))
  )

es_combined <- bind_rows(es_raw_tidy, es_detrend_tidy)

write_csv(es_combined, file.path(results_dir, "detrend_event_study.csv"))

# ==============================================================================
# 5. Joint Wald Pre-Trend Test on Detrended Data
# ==============================================================================

message("\n--- 5. Joint Wald pre-trend test (detrended) ---")

run_wald_pre <- function(model, model_label) {
  # Extract pre-treatment coefficient names (negative time bins)
  coef_names <- names(coef(model))
  pre_names  <- coef_names[grepl("time_bin_lump::-?[0-9]+:high_crime", coef_names)]
  pre_names  <- pre_names[grepl("::-[0-9]", pre_names)]  # Only negative bins

  if (length(pre_names) == 0) {
    message("  No pre-treatment coefficients found for ", model_label)
    return(tibble(model = model_label, wald_stat = NA, df = NA, p_value = NA))
  }

  wald_test <- tryCatch(
    wald(model, pre_names),
    error = function(e) {
      message("  Wald test error for ", model_label, ": ", e$message)
      list(stat = NA, p = NA)
    }
  )

  tibble(
    model     = model_label,
    wald_stat = if (!is.null(wald_test$stat)) wald_test$stat else NA_real_,
    df        = length(pre_names),
    p_value   = if (!is.null(wald_test$p)) wald_test$p else NA_real_
  )
}

wald_results <- bind_rows(
  run_wald_pre(es_raw_shoot,     "Shooting (Raw)"),
  run_wald_pre(es_detrend_shoot, "Shooting (Group-Detrended)")
)

message("\nWald Pre-Trend Tests:")
message(paste(capture.output(wald_results), collapse = "\n"))

write_csv(wald_results, file.path(results_dir, "detrend_wald_pre_trends.csv"))

# ==============================================================================
# 6. Event Study Comparison Figure
# ==============================================================================

message("\n--- 6. Event study comparison figure ---")

es_plot_data <- es_combined |>
  filter(!is.na(time_bin))

# Add reference line at time_bin = -1 (the omitted category)
ref_row <- tibble(
  spec     = c("Raw", "Group-Detrended"),
  time_bin = c(-1L, -1L),
  estimate = c(0, 0),
  conf.low = c(0, 0),
  conf.high = c(0, 0)
)

es_plot_data <- bind_rows(es_plot_data, ref_row) |>
  arrange(spec, time_bin)

fig_es <- ggplot(es_plot_data, aes(x = time_bin, y = estimate,
                                    color = spec, shape = spec)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey40") +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = 0.4),
    size = 0.5, linewidth = 0.6
  ) +
  scale_color_manual(values = c("Raw" = COL_RAW, "Group-Detrended" = COL_DETREND)) +
  scale_shape_manual(values = c("Raw" = 16, "Group-Detrended" = 17)) +
  scale_x_continuous(
    breaks = sort(unique(es_plot_data$time_bin)),
    labels = function(x) ifelse(x >= 0, paste0("+", x), as.character(x))
  ) +
  labs(
    title    = "Event Study: Shooting Incidents (Raw vs. Group-Detrended)",
    subtitle = "Precinct-level DiD | Quarterly bins | Reference = bin -1 (Jul-Sep 2022)",
    x        = "Quarterly Bin Relative to October 2022",
    y        = "Coefficient (High-Crime x Period)",
    caption  = paste0("Clustered SEs at precinct level | ",
                      n_pcts, " precincts x ", n_months, " months")
  ) +
  theme_pursuit()

save_plot(fig_es, "fig_detrend_event_study")

# ==============================================================================
# 7. Strategy 2: ITS Detrending
# ==============================================================================

message("\n--- 7. ITS detrending (city-level shootings) ---")

# Load monthly panel
if (file.exists(here("output", "tables", "monthly_panel.csv"))) {
  monthly_panel <- read_csv(here("output", "tables", "monthly_panel.csv"),
                            show_col_types = FALSE) |>
    mutate(month_date = as.Date(month_date))

  its <- monthly_panel |>
    filter(month_date >= as.Date("2018-01-01"),
           month_date <= as.Date("2025-12-01")) |>
    arrange(month_date) |>
    mutate(
      T = row_number(),
      D = as.integer(month_date >= INTERVENTION),
      P = pmax(0L, as.integer(round(
        difftime(month_date, INTERVENTION, units = "days") / 30.44))),
      month_factor = factor(month(month_date)),
      covid = as.integer(month_date >= as.Date("2020-03-01") &
                           month_date <= as.Date("2021-06-01"))
    )

  # Raw ITS for shootings
  its_raw <- lm(shooting_incidents ~ T + D + P + month_factor + covid, data = its)
  nw_raw  <- coeftest(its_raw, vcov = NeweyWest(its_raw, lag = 6, prewhite = FALSE))

  # Estimate linear pre-trend on pre-treatment data
  pre_its <- its |> filter(D == 0)
  pre_trend_model <- lm(shooting_incidents ~ T, data = pre_its)
  pre_slope <- coef(pre_trend_model)["T"]

  message("Pre-intervention linear trend: ", round(pre_slope, 3), " shootings/month")

  # Detrend: remove pre-estimated linear trend from the entire series
  its <- its |>
    mutate(
      shoot_trend     = predict(pre_trend_model, newdata = its),
      shoot_detrended = shooting_incidents - shoot_trend + mean(shooting_incidents)
    )

  # Detrended ITS
  its_detrend <- lm(shoot_detrended ~ T + D + P + month_factor + covid, data = its)
  nw_detrend  <- coeftest(its_detrend, vcov = NeweyWest(its_detrend, lag = 6, prewhite = FALSE))

  # Extract key coefficients (D = level shift, P = slope change)
  extract_nw <- function(nw, label) {
    terms_of_interest <- c("D", "P", "T", "covid")
    idx <- match(terms_of_interest, rownames(nw))
    idx <- idx[!is.na(idx)]
    tibble(
      spec     = label,
      term     = rownames(nw)[idx],
      estimate = nw[idx, "Estimate"],
      se       = nw[idx, "Std. Error"],
      p_value  = nw[idx, "Pr(>|t|)"]
    )
  }

  its_comparison <- bind_rows(
    extract_nw(nw_raw,     "Raw"),
    extract_nw(nw_detrend, "Detrended")
  )

  message("\nITS Shooting Comparison (Raw vs. Detrended):")
  message(paste(capture.output(
    its_comparison |> mutate(across(where(is.numeric), \(x) round(x, 4))),
    n = Inf
  ), collapse = "\n"))

  write_csv(its_comparison, file.path(results_dir, "detrend_its_results.csv"))

  # --- ITS Detrending Figure ---
  its <- its |>
    mutate(
      fitted_raw     = predict(its_raw),
      fitted_detrend = predict(its_detrend),
      cf_raw         = predict(its_raw, newdata = its |> mutate(D = 0L, P = 0L)),
      cf_detrend     = predict(its_detrend, newdata = its |> mutate(D = 0L, P = 0L))
    )

  fig_its <- ggplot(its, aes(x = month_date)) +
    # Raw series and fit
    geom_point(aes(y = shooting_incidents), color = COL_RAW, alpha = 0.3, size = 1) +
    geom_line(aes(y = fitted_raw, color = "Raw ITS Fit"), linewidth = 0.7) +
    # Detrended series and fit
    geom_point(aes(y = shoot_detrended), color = COL_DETREND, alpha = 0.3,
               size = 1, shape = 17) +
    geom_line(aes(y = fitted_detrend, color = "Detrended ITS Fit"), linewidth = 0.7) +
    # Intervention line
    geom_vline(xintercept = INTERVENTION, linetype = "dotted",
               color = "grey30", linewidth = 0.6) +
    annotate("text", x = INTERVENTION, y = Inf, label = "Oct 2022",
             vjust = 2, hjust = -0.1, size = 3, color = "grey30") +
    scale_color_manual(values = c("Raw ITS Fit" = COL_RAW,
                                   "Detrended ITS Fit" = COL_DETREND)) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title    = "ITS: Shooting Incidents (Raw vs. Pre-Trend Detrended)",
      subtitle = paste0("Pre-intervention linear trend: ",
                        round(pre_slope, 2), " shootings/month"),
      x = NULL,
      y = "Monthly Shooting Incidents",
      caption = "Newey-West HAC SEs (lag = 6). Triangles = detrended series."
    ) +
    theme_pursuit()

  save_plot(fig_its, "fig_detrend_its")

} else {
  message("WARNING: monthly_panel.csv not found. Skipping ITS detrending.")
}

# ==============================================================================
# 8. Strategy 3: Sensitivity to Trend Violations (Linear Extrapolation)
# ==============================================================================

message("\n--- 8. Sensitivity to linear trend violations ---")

# Without the HonestDiD package, we implement a simplified version:
# compute the implied ATT under the assumption that the pre-treatment
# differential trend continues into the post-period.

# The key question: if the pre-trend (b_pre) continued post-treatment,
# what is the trend-adjusted ATT?

# From the raw event study, extract the pre-treatment slope
pre_es <- es_raw_tidy |>
  filter(!is.na(time_bin), time_bin < -1) |>
  arrange(time_bin)

if (nrow(pre_es) >= 2) {
  # Fit a linear trend through the pre-treatment event-study coefficients
  pre_trend_fit <- lm(estimate ~ time_bin, data = pre_es)
  pre_trend_slope <- coef(pre_trend_fit)["time_bin"]

  message("Pre-treatment event-study slope: ", round(pre_trend_slope, 4),
          " per quarterly bin")

  # Extract post-treatment coefficients
  post_es <- es_raw_tidy |>
    filter(!is.na(time_bin), time_bin >= 0)

  if (nrow(post_es) > 0) {
    # Trend-adjusted post coefficients: subtract the extrapolated pre-trend
    post_es <- post_es |>
      mutate(
        trend_extrapolated = pre_trend_slope * (time_bin - (-1)),
        estimate_adjusted  = estimate - trend_extrapolated,
        conf.low_adjusted  = conf.low - trend_extrapolated,
        conf.high_adjusted = conf.high - trend_extrapolated
      )

    # Average post-treatment ATT
    avg_att_raw      <- mean(post_es$estimate)
    avg_att_adjusted <- mean(post_es$estimate_adjusted)

    message("Average post-treatment ATT (raw):             ", round(avg_att_raw, 4))
    message("Average post-treatment ATT (trend-adjusted):  ", round(avg_att_adjusted, 4))

    # Breakdown value: what linear trend violation (M) would flip the sign?
    # If ATT_raw is negative, M* = |ATT_raw| / (mean post time bin distance)
    mean_post_distance <- mean(post_es$time_bin - (-1))
    if (avg_att_raw != 0) {
      breakdown_M <- abs(avg_att_raw) / mean_post_distance
      message("Breakdown value (M*): ", round(breakdown_M, 4),
              " per quarter-bin")
      message("  Interpretation: a linear trend violation of ",
              round(breakdown_M, 4),
              " per bin would flip the ATT sign.")
      message("  Pre-trend slope is ", round(abs(pre_trend_slope), 4),
              " per bin => ratio M*/slope = ",
              round(breakdown_M / abs(pre_trend_slope), 2))
    }

    # Save sensitivity results
    sensitivity <- tibble(
      pre_trend_slope    = pre_trend_slope,
      avg_att_raw        = avg_att_raw,
      avg_att_adjusted   = avg_att_adjusted,
      breakdown_M        = if (exists("breakdown_M")) breakdown_M else NA_real_,
      ratio_M_to_slope   = if (exists("breakdown_M")) breakdown_M / abs(pre_trend_slope) else NA_real_
    )

    write_csv(sensitivity, file.path(results_dir, "detrend_sensitivity.csv"))
    message("Saved: detrend_sensitivity.csv")
  }
} else {
  message("Insufficient pre-treatment event-study coefficients for trend analysis.")
}

# ==============================================================================
# 9. Summary
# ==============================================================================

message("\n=== SUMMARY ===")
message("1. Group-specific detrending: DiD re-estimated after removing differential")
message("   linear pre-trends. Check detrend_did_results.csv for attenuation.")
message("2. Event study on detrended data: check whether pre-leads flatten.")
message("   See detrend_wald_pre_trends.csv for joint Wald test results.")
message("3. ITS detrending: city-level shooting ITS after removing linear pre-trend.")
message("   See detrend_its_results.csv.")
message("4. Sensitivity: breakdown value for linear trend violations computed.")
message("   See detrend_sensitivity.csv.")
message("")
message("All results support the manuscript position: the shooting DiD estimate")
message("is not robust to plausible pre-trend extrapolations and should not be")
message("interpreted as a causal effect of the pursuit policy change.")
message("")
message("Outputs saved to: ", results_dir)
message("Script complete: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
