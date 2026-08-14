# 12_av_did.R — National DiD Using AmericanViolence.org Data (Expanded Donor Pool)
#
# Treatment: NYC (all 5 boroughs aggregated), post = Oct 2022
# Control:   98 comparison cities from AmericanViolence.org (100 - NYC - Chandler - Irvine)
# Method:    TWFE via fixest::feols(), monthly data, year + month-of-year FEs, clustered SEs
# Event study: Annual year × treated interactions, ref = 2021
#
# Source: Sharkey, Patrick. "AmericanViolence.org: City, fatal and nonfatal
#   shootings. Princeton, NJ: www.americanviolence.org"
#
# Requires output from 11_av_data_prep.R
#
# NOTE (2026-03-05): This script is no longer part of the main publication
# pipeline. The AV-based national DiD/SCM analyses have been removed from the
# manuscript. This script is retained for reproducibility.
# Active pipeline: 01 → 03 → 06 → 07 → 08 → 14 → 17 → 15 → 16 → 00_regenerate

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(here)
})

cat("\n=== AV NATIONAL DiD ===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")

results_dir <- here("output", "national_did")
plot_dir    <- here("output", "plots")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir,    showWarnings = FALSE, recursive = TRUE)

# Cities excluded after quality screening (confirmed by Justin 2026-02-20)
EXCLUDED_CITIES <- c("Chandler", "Irvine")

# Project theme and save function
COL_NYC     <- "#D62828"
COL_CONTROL <- "#003049"

theme_pursuit <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}

save_plot <- function(p, name, w = 10, h = 7) {
  ggsave(file.path(plot_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
  ggsave(file.path(plot_dir, paste0(name, ".pdf")), p, width = w, height = h)
  cat("  Saved:", name, "\n")
}


# 1. Load and prepare panel ===================================================

cat("--- Loading Panels ---\n\n")

panel_file <- here("output", "national_did", "av_combined_panel.csv")
if (!file.exists(panel_file)) {
  stop("Run 11_av_data_prep.R first. Missing: ", panel_file)
}

monthly_raw <- read_csv(panel_file, show_col_types = FALSE) %>%
  mutate(month_date = as.Date(month_date))

cat("Full monthly panel:", nrow(monthly_raw), "rows,",
    n_distinct(monthly_raw$place_name), "cities\n")

# Apply exclusions
monthly <- monthly_raw %>%
  filter(!place_name %in% EXCLUDED_CITIES)

n_excluded <- n_distinct(monthly_raw$place_name) - n_distinct(monthly$place_name)
cat("Excluded cities:", paste(EXCLUDED_CITIES, collapse = ", "),
    "(n =", n_excluded, ")\n")
cat("After exclusions:", n_distinct(monthly$place_name), "cities,",
    nrow(monthly), "rows\n\n")


# 2. Build DiD variables =======================================================

cat("--- Building DiD Variables ---\n\n")

monthly <- monthly %>%
  mutate(
    city_state = paste(place_name, state_abr, sep = ", "),
    month_fct  = factor(month),   # month-of-year FE for seasonality
    post       = as.integer(month_date >= as.Date("2022-10-01")),
    did        = treated * post
  )

cat("Treatment structure:\n")
cat("  Treated (NYC):", n_distinct(monthly$city_state[monthly$treated == 1]),
    "city,", sum(monthly$treated), "rows\n")
cat("  Control:      ", n_distinct(monthly$city_state[monthly$treated == 0]),
    "cities,", sum(monthly$treated == 0), "rows\n")
cat("  Post-period:  ", sum(monthly$post), "treated-rows post,",
    sum(monthly$post == 0), "pre\n")
cat("  DiD = 1:      ", sum(monthly$did), "rows (NYC post-Oct 2022)\n\n")


# 3. Annual aggregation for event study ========================================

annual <- monthly %>%
  group_by(place_name, state_abr, id, city_state, year, treated) %>%
  summarise(
    crime_count    = sum(crime_count),
    population_est = mean(population_est),
    n_months       = n(),
    .groups = "drop"
  ) %>%
  mutate(
    shooting_rate = (crime_count / population_est) * 100000,
    post          = as.integer(year >= 2023),
    did           = treated * post,
    year_fct      = factor(year)
  )

cat("Annual panel:", nrow(annual), "rows,",
    n_distinct(annual$city_state), "cities,",
    n_distinct(annual$year), "years\n\n")


# 4. TWFE DiD — main estimate =================================================

cat("--- Main DiD (TWFE, monthly) ---\n\n")
cat("Specification: shooting_rate ~ did | id + year + month_fct\n")
cat("  - Unit FEs: city (id)\n")
cat("  - Time FEs: year + month-of-year\n")
cat("  - Treatment: did = treated * I(month >= Oct 2022)\n")
cat("  - SEs: clustered by city\n\n")

av_twfe <- feols(
  shooting_rate ~ did | id + year + month_fct,
  data    = monthly,
  cluster = ~id
)

cat("Main DiD result:\n")
summary(av_twfe)

av_twfe_tidy <- broom::tidy(av_twfe, conf.int = TRUE) %>%
  mutate(outcome = "shooting_rate", model = "twfe_monthly",
         n_treated = 1L, n_control = n_distinct(monthly$id[monthly$treated == 0]),
         note = "monthly TWFE, year + month-of-year FEs, cluster by city")

write_csv(av_twfe_tidy, file.path(results_dir, "av_did_twfe.csv"))
cat("\nSaved: av_did_twfe.csv\n\n")


# 5. Parallel trends event study (annual) =====================================

cat("--- Parallel Trends Event Study (annual) ---\n\n")
cat("Specification: shooting_rate ~ i(year_fct, treated, ref = 2021) | id + year\n")
cat("  - Reference year: 2021 (last clean pre-treatment year)\n")
cat("  - Pre-treatment: 2018–2020; 2022 = partial treatment year\n\n")

av_es <- feols(
  shooting_rate ~ i(year_fct, treated, ref = "2021") | id + year,
  data    = annual,
  cluster = ~id
)

cat("Event study coefficients:\n")
summary(av_es)

av_es_tidy <- broom::tidy(av_es, conf.int = TRUE) %>%
  mutate(
    event_year = as.integer(str_extract(term, "\\d{4}")),
    pre_period = event_year < 2022,
    sig        = p.value < 0.05
  )

write_csv(av_es_tidy, file.path(results_dir, "av_did_event_study.csv"))

# Parallel trends assessment
pre_coefs <- av_es_tidy %>% filter(pre_period)
n_pre_sig <- sum(pre_coefs$sig, na.rm = TRUE)
cat("\nPre-treatment interactions (2018–2020):\n")
cat("  Significant (p < .05):", n_pre_sig, "of", nrow(pre_coefs), "\n")
if (n_pre_sig == 0) {
  cat("  >> PARALLEL TRENDS: PASS — no significant pre-trends detected\n\n")
} else {
  cat("  >> PARALLEL TRENDS: FAIL — pre-trend differences detected\n\n")
}


# 6. Leave-one-out sensitivity =================================================

cat("--- Leave-One-Out Sensitivity ---\n\n")

control_cities <- unique(monthly$id[monthly$treated == 0])
n_controls <- length(control_cities)
cat("Running LOO for", n_controls, "comparison cities...\n\n")

loo_results <- map_dfr(seq_along(control_cities), function(i) {
  drop_id <- control_cities[i]
  drop_name <- monthly$city_state[monthly$id == drop_id][1]

  panel_loo <- monthly %>% filter(id != drop_id)

  mod <- tryCatch(
    feols(shooting_rate ~ did | id + year + month_fct,
          data = panel_loo, cluster = ~id),
    error = function(e) NULL
  )

  if (is.null(mod)) {
    return(tibble(
      dropped_id   = drop_id,
      dropped_city = drop_name,
      estimate     = NA_real_,
      std.error    = NA_real_,
      p.value      = NA_real_,
      conf.low     = NA_real_,
      conf.high    = NA_real_
    ))
  }

  tidy_mod <- broom::tidy(mod, conf.int = TRUE)
  tidy_mod %>%
    filter(term == "did") %>%
    select(estimate, std.error, p.value, conf.low, conf.high) %>%
    mutate(dropped_id = drop_id, dropped_city = drop_name, .before = 1)
}) %>%
  arrange(estimate)

write_csv(loo_results, file.path(results_dir, "av_did_loo.csv"))

cat("LOO complete. Estimate range:\n")
cat("  Min:", round(min(loo_results$estimate, na.rm = TRUE), 2),
    "(dropping", loo_results$dropped_city[which.min(loo_results$estimate)], ")\n")
cat("  Max:", round(max(loo_results$estimate, na.rm = TRUE), 2),
    "(dropping", loo_results$dropped_city[which.max(loo_results$estimate)], ")\n")
cat("  All estimates significant (p < .05):",
    all(loo_results$p.value < 0.05, na.rm = TRUE), "\n\n")


# 7. Plots =====================================================================

cat("--- Generating Plots ---\n\n")

INTERVENTION_DATE <- as.Date("2022-10-01")

# 7a. Trend lines: NYC vs. comparison city average ----------------------------

trend_data <- monthly %>%
  group_by(month_date, treated) %>%
  summarise(
    mean_rate = mean(shooting_rate, na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(group = if_else(treated == 1, "New York City", "Comparison cities (mean)"))

p_trends <- ggplot(trend_data, aes(x = month_date, y = mean_rate,
                                    color = group, linetype = group)) +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = INTERVENTION_DATE, linetype = "dashed",
             color = "grey40", linewidth = 0.7) +
  annotate("text", x = INTERVENTION_DATE + 30, y = Inf,
           label = "Oct 2022\nPolicy change", vjust = 1.4, hjust = 0,
           size = 3, color = "grey40") +
  scale_color_manual(values = c("New York City" = COL_NYC,
                                "Comparison cities (mean)" = COL_CONTROL)) +
  scale_linetype_manual(values = c("New York City" = "solid",
                                   "Comparison cities (mean)" = "dashed")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Shooting Rates: NYC vs. Comparison Cities",
    subtitle = "Fatal + nonfatal per 100,000 (annualized); AmericanViolence.org data",
    x        = NULL,
    y        = "Shooting rate (per 100K, annualized)",
    color    = NULL,
    linetype = NULL,
    caption  = paste0("Note: Comparison cities n = ", n_distinct(monthly$city_state[monthly$treated == 0]),
                      " (top 100 US cities minus NYC, Chandler, Irvine).")
  ) +
  theme_pursuit()

save_plot(p_trends, "av_did_trends")


# 7b. Event study (parallel trends plot) --------------------------------------

# Add full-model estimate for post years for visual context
es_plot_data <- av_es_tidy %>%
  mutate(
    period_label = case_when(
      event_year < 2022  ~ "Pre-treatment",
      event_year == 2022 ~ "Partial (Oct–Dec)",
      event_year > 2022  ~ "Post-treatment"
    ),
    period_label = factor(period_label,
                          levels = c("Pre-treatment", "Partial (Oct–Dec)", "Post-treatment"))
  )

p_es <- ggplot(es_plot_data, aes(x = event_year, y = estimate,
                                   color = period_label, fill = period_label)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 2021.5, linetype = "dotted", color = "grey40") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.8, color = "grey40") +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  annotate("text", x = 2021.7, y = Inf,
           label = "Oct 2022\npolicy change", hjust = 0, vjust = 1.3,
           size = 3, color = "grey40") +
  scale_color_manual(values = c(
    "Pre-treatment"       = COL_CONTROL,
    "Partial (Oct–Dec)"   = "grey60",
    "Post-treatment"      = COL_NYC
  )) +
  scale_fill_manual(values = c(
    "Pre-treatment"       = COL_CONTROL,
    "Partial (Oct–Dec)"   = "grey60",
    "Post-treatment"      = COL_NYC
  )) +
  scale_x_continuous(breaks = sort(unique(es_plot_data$event_year))) +
  labs(
    title    = "Event Study: NYC vs. Comparison Cities",
    subtitle = "Annual year × treated coefficients (ref = 2021); AmericanViolence.org data",
    x        = "Year",
    y        = "Difference in shooting rate (per 100K)",
    color    = NULL,
    fill     = NULL,
    caption  = paste0("Note: 95% CIs shown. Cluster-robust SEs (by city). ",
                      "2022 = partial treatment year (Oct–Dec only). ",
                      "Reference: 2021.")
  ) +
  theme_pursuit()

save_plot(p_es, "av_did_event_study")


# 7c. LOO forest plot ----------------------------------------------------------

# Show all 98 LOO estimates; highlight full-model estimate
full_est <- broom::tidy(av_twfe, conf.int = TRUE) %>%
  filter(term == "did") %>%
  pull(estimate)
full_se  <- broom::tidy(av_twfe, conf.int = TRUE) %>%
  filter(term == "did") %>%
  pull(std.error)

# For readability, show just a summary box plot rather than all 98 estimates
loo_summary <- loo_results %>%
  summarise(
    min_est    = min(estimate, na.rm = TRUE),
    max_est    = max(estimate, na.rm = TRUE),
    mean_est   = mean(estimate, na.rm = TRUE),
    median_est = median(estimate, na.rm = TRUE),
    sd_est     = sd(estimate, na.rm = TRUE)
  )

p_loo <- ggplot(loo_results, aes(x = estimate)) +
  geom_histogram(bins = 30, fill = COL_CONTROL, alpha = 0.7, color = "white") +
  geom_vline(xintercept = full_est, color = COL_NYC, linewidth = 1.2, linetype = "solid") +
  geom_vline(xintercept = 0, color = "grey30", linewidth = 0.7, linetype = "dashed") +
  annotate("text", x = full_est + 0.5, y = Inf,
           label = paste0("Full model\n", round(full_est, 1)),
           hjust = 0, vjust = 1.3, size = 3, color = COL_NYC) +
  labs(
    title    = "Leave-One-Out Sensitivity: DiD Estimates",
    subtitle = paste0("Each bar = DiD estimate after dropping one comparison city (n = ",
                      n_controls, " runs)"),
    x        = "DiD estimate (per 100K, annualized)",
    y        = "Count",
    caption  = paste0("Red line = full-model estimate (",
                      round(full_est, 1), "). Dashed line = zero.")
  ) +
  theme_pursuit()

save_plot(p_loo, "av_did_loo")


# 8. Summary output ============================================================

main_est <- broom::tidy(av_twfe, conf.int = TRUE) %>% filter(term == "did")

cat("\n=== RESULTS SUMMARY ===\n\n")
cat("Main DiD Estimate (monthly TWFE):\n")
cat("  Coefficient (did):", round(main_est$estimate, 2), "\n")
cat("  Std. Error:       ", round(main_est$std.error, 2), "\n")
cat("  95% CI:           [", round(main_est$conf.low, 2), ",",
                              round(main_est$conf.high, 2), "]\n")
cat("  p-value:          ", format(main_est$p.value, digits = 3), "\n\n")

cat("Parallel Trends (pre-treatment year × treated interactions):\n")
cat("  Significant pre-trends:", n_pre_sig, "of", nrow(pre_coefs), "\n")
cat("  Verdict:", if (n_pre_sig == 0) "PASS" else "FAIL", "\n\n")

cat("Leave-One-Out Sensitivity:\n")
cat("  Estimate range:", round(min(loo_results$estimate, na.rm=TRUE), 2),
    "to", round(max(loo_results$estimate, na.rm=TRUE), 2), "\n")
cat("  All significant:", all(loo_results$p.value < 0.05, na.rm=TRUE), "\n\n")

cat("Output files:\n")
cat("  output/national_did/av_did_twfe.csv\n")
cat("  output/national_did/av_did_event_study.csv\n")
cat("  output/national_did/av_did_loo.csv\n")
cat("  output/plots/av_did_trends.png/pdf\n")
cat("  output/plots/av_did_event_study.png/pdf\n")
cat("  output/plots/av_did_loo.png/pdf\n\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
