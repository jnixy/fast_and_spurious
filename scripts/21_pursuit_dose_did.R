# ==============================================================================
# Vehicle Pursuit Policy Analysis — Script 21: Pursuit-Dose DiD
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Estimate the causal effect of pursuit intensity on crime and collision
#   outcomes using a continuous-treatment (dose-response) DiD framework at
#   the precinct level. This extends the TWFE dose-response in script 15 by
#   exploiting the October 2022 policy change as an exogenous shock to
#   pursuit intensity that varied across precincts.
#
#   Key identification strategy:
#   The Oct 2022 Maddrey escalation increased citywide pursuit activity
#   ~20-fold, but the increase was heterogeneous across precincts. Precincts
#   with higher pre-period crime (especially auto theft, robbery) experienced
#   disproportionately larger surges in pursuit activity. We use pre-period
#   (Jan 2018 – Sep 2022) precinct characteristics as instruments for
#   post-period pursuit dose, then estimate the relationship between this
#   predicted dose and outcomes.
#
#   Panel: same canonical 77-precinct balanced panel used throughout the
#   rest of the paper (filter + Precinct 116 -> 105 recode identical to
#   17_precinct_ddd.R), extended with precinct-month pursuit counts.
#
#   Five specifications:
#     (1) Shift-share DiD: post × pre_pursuit_rate interaction (dose fixed
#         pre-treatment, so not endogenous to post-period outcomes)
#     (2) Continuous DiD: outcome ~ pursuit_events | pct + month_date
#         (endogenous — pursuit_events responds to the same contemporaneous
#         shocks as the outcome — reported for comparison)
#     (3) Continuous DiD + precinct-specific linear time trends: adds a
#         precinct-varying slope on a monthly time index, per Editor
#         Comment 2 / R1 Comment 3 (JQC R&R) request for "interacted
#         precinct*time fixed effects" to absorb area-level crime trends.
#         Does not resolve the same-period simultaneity noted in (2).
#     (4) Continuous DiD + precinct × year FE: coarser alternative to (3)
#         (absorbs precinct-specific year-level shifts without eating all
#         within-year timing variation)
#     (5) Binned DiD: precincts grouped into terciles by post-period pursuit
#         intensity, with event-study-style leads and lags
#
# Inputs:
#   - output/precinct_did/pct_monthly_pursuit.csv    (from script 01)
#   - output/precinct_did/pct_monthly_shootings.csv  (from script 01)
#   - output/precinct_did/pct_monthly_robberies.csv  (from script 01)
#   - output/precinct_did/pct_monthly_collisions.csv (from script 01)
#
# Outputs (saved to output/extension/):
#   - dose_did_results.csv             — coefficient table (all specs)
#   - dose_did_event_study.csv         — event-study estimates (binned spec)
#   - dose_did_first_stage.csv         — first-stage diagnostics
#   - dose_did_trend_diagnostics.csv   — SE inflation vs. spec (2) for the
#                                         precinct-trend specs (3)-(4)
#   - dose_did_models.rds              — fitted model objects
#
# Outputs (saved to output/plots/):
#   - fig_dose_did_event_study_shooting.png/.pdf
#   - fig_dose_did_event_study_robbery.png/.pdf
#   - fig_dose_did_event_study_collision.png/.pdf
#   - fig_dose_did_binscatter.png/.pdf
#
# Runtime: < 2 min
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(fixest)
  library(here)
  library(janitor)
})

set.seed(20260628)


# 0. Setup =====================================================================

ext_dir  <- here("output", "extension")
plot_dir <- here("output", "plots")
dir.create(ext_dir,  showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

INTERVENTION <- as.Date("2022-10-01")
ANALYSIS_START <- as.Date("2018-01-01")
ANALYSIS_END   <- as.Date("2025-12-01")

message("Script 21: Pursuit-Dose DiD")
message("Working directory: ", here())

# Color palette
COL_PURSUIT   <- "#D62828"
COL_SHOOTING  <- "#6A0572"
COL_ROBBERY   <- "#003049"
COL_COLLISION <- "#F77F00"

theme_pursuit <- function(base_size = 15) {
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
      legend.position  = "top",
      legend.title     = element_blank(),
      plot.margin      = margin(15, 15, 15, 15)
    )
}

save_plot <- function(p, name, w = 8, h = 5.5) {
  ggsave(file.path(plot_dir, paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(plot_dir, paste0(name, ".pdf")),
         plot = p, width = w, height = h, bg = "white")
}


# 1. Load Data =================================================================

message("\n--- Loading precinct-level data ---")

pct_dir <- here("output", "precinct_did")

pct_files <- c(
  pursuit   = "pct_monthly_pursuit.csv",
  shooting  = "pct_monthly_shootings.csv",
  robbery   = "pct_monthly_robberies.csv",
  collision = "pct_monthly_collisions.csv"
)

for (f in pct_files) {
  if (!file.exists(file.path(pct_dir, f))) {
    stop(f, " not found in ", pct_dir, " -- run script 01 first")
  }
}

pursuit_df   <- read_csv(file.path(pct_dir, pct_files["pursuit"]),   show_col_types = FALSE) |> clean_names()
shooting_df  <- read_csv(file.path(pct_dir, pct_files["shooting"]),  show_col_types = FALSE) |> clean_names()
robbery_df   <- read_csv(file.path(pct_dir, pct_files["robbery"]),   show_col_types = FALSE) |> clean_names()
collision_df <- read_csv(file.path(pct_dir, pct_files["collision"]), show_col_types = FALSE) |> clean_names()

message("  Pursuit rows:   ", nrow(pursuit_df))
message("  Shooting rows:  ", nrow(shooting_df))
message("  Robbery rows:   ", nrow(robbery_df))
message("  Collision rows: ", nrow(collision_df))


# 2. Build Panel ===============================================================

message("\n--- Building dose-DiD panel ---")

# Ensure date type
pursuit_df   <- pursuit_df   |> mutate(month_date = as.Date(month_date))
shooting_df  <- shooting_df  |> mutate(month_date = as.Date(month_date))
robbery_df   <- robbery_df   |> mutate(month_date = as.Date(month_date))
collision_df <- collision_df |> mutate(month_date = as.Date(month_date))

# Full panel: merge all outcomes by precinct × month, then restrict to the
# same canonical 77-precinct balanced panel used throughout the rest of the
# paper (see 17_precinct_ddd.R Section 2): drop invalid precinct codes and
# precincts with zero crime activity across the whole window, then recode
# Precinct 116 into 105 (formally consolidated mid-study, December 2024) and
# re-aggregate. An earlier version of this script skipped this step and
# analyzed an unfiltered 123-"precinct" universe that is not comparable to
# the rest of the paper's precinct-level results.
# Post-join row count guard: fail loudly if the join produced unexpected
# expansion (e.g., a pct/month_date type mismatch causing a Cartesian
# product), identical safeguard to 17_precinct_ddd.R Section 2.
n_pcts_expected   <- n_distinct(shooting_df$pct)
n_months_expected <- n_distinct(shooting_df$month_date)

panel_raw <- shooting_df |>
  full_join(robbery_df,   by = c("pct", "month_date")) |>
  full_join(collision_df, by = c("pct", "month_date")) |>
  full_join(pursuit_df,   by = c("pct", "month_date")) |>
  replace_na(list(
    shooting_incidents = 0,
    robbery_count      = 0,
    collision_count    = 0,
    pursuit_events      = 0
  )) |>
  filter(pct >= 1, pct <= 123,
         month_date >= ANALYSIS_START,
         month_date <= ANALYSIS_END) |>
  group_by(pct) |>
  filter(any(shooting_incidents > 0) | any(robbery_count > 0)) |>
  ungroup() |>
  arrange(pct, month_date)

max_expected <- n_pcts_expected * n_months_expected
if (nrow(panel_raw) > max_expected * 1.05) {
  stop("panel_raw row count (", nrow(panel_raw), ") exceeds expected max (",
       max_expected, "). Likely pct or month_date type mismatch between input CSVs.")
}
message("  Panel rows after full_join: ", nrow(panel_raw),
        " (expected <= ", max_expected, ")")

# Recode Precinct 116 as 105 and re-aggregate (identical to 17_precinct_ddd.R)
panel_raw <- panel_raw |>
  mutate(pct = if_else(pct == 116L, 105L, pct)) |>
  group_by(pct, month_date) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
  arrange(pct, month_date)

panel <- panel_raw |>
  mutate(
    post = as.integer(month_date >= INTERVENTION),
    year = year(month_date),
    month_num = month(month_date),
    # Relative month for event-study
    rel_month = as.integer(round(
      difftime(month_date, INTERVENTION, units = "days") / 30.44
    )),
    # Dense integer time index (1..n_months) for precinct-specific linear
    # trend specifications in Section 5b -- avoids day-count rounding noise
    # from using month_date directly as a numeric trend.
    time_index = as.integer(factor(month_date, levels = sort(unique(month_date))))
  ) |>
  arrange(pct, month_date)

message("  Panel: ", nrow(panel), " rows, ", n_distinct(panel$pct), " precincts")
message("  Window: ", as.character(ANALYSIS_START), " to ", as.character(ANALYSIS_END))


# 3. Compute Pre-Period Pursuit Intensity (Dose Assignment) ====================

message("\n--- Computing pre-period dose assignment ---")

# Pre-period mean pursuit events per precinct (Jan 2018 – Sep 2022)
pre_pursuit <- panel |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(
    pre_pursuit_mean = mean(pursuit_events, na.rm = TRUE),
    pre_pursuit_total = sum(pursuit_events, na.rm = TRUE),
    pre_months = n(),
    .groups = "drop"
  )

# Post-period pursuit intensity (for descriptive comparison)
post_pursuit <- panel |>
  filter(month_date >= INTERVENTION) |>
  group_by(pct) |>
  summarise(
    post_pursuit_mean = mean(pursuit_events, na.rm = TRUE),
    post_pursuit_total = sum(pursuit_events, na.rm = TRUE),
    .groups = "drop"
  )

# Merge dose measures into panel
dose_info <- pre_pursuit |>
  left_join(post_pursuit, by = "pct") |>
  mutate(
    pursuit_surge = post_pursuit_mean - pre_pursuit_mean,
    # Tercile assignment based on post-period pursuit intensity
    dose_tercile = ntile(post_pursuit_mean, 3),
    dose_tercile_label = case_when(
      dose_tercile == 1 ~ "Low dose",
      dose_tercile == 2 ~ "Medium dose",
      dose_tercile == 3 ~ "High dose"
    )
  )

panel <- panel |>
  left_join(dose_info, by = "pct")

message("  Tercile counts:")
message(paste(capture.output(
  print(as.data.frame(dose_info |> count(dose_tercile_label)))
), collapse = "\n"))

# First-stage: does pre-period pursuit intensity predict post-period surge?
fs_mod <- lm(post_pursuit_mean ~ pre_pursuit_mean, data = dose_info)
fs_r2 <- summary(fs_mod)$r.squared
fs_fstat <- summary(fs_mod)$fstatistic[1]
message(sprintf("\n  First-stage R2: %.3f, F-stat: %.1f", fs_r2, fs_fstat))


# 4. Shift-Share DiD (Specification 1) =========================================
#
# Y_{pt} = a_p + a_t + beta * (post_t × pre_pursuit_mean_p) + e_{pt}
#
# beta captures: for each unit increase in pre-period pursuit intensity,
# how much did the outcome change post-intervention?
# Identification: pre_pursuit_mean is determined before the policy change
# and thus not endogenous to post-period outcomes.

message("\n--- Specification 1: Shift-share DiD ---")

# Center pre_pursuit_mean for interpretability
panel <- panel |>
  mutate(pre_pursuit_dm = pre_pursuit_mean - mean(pre_pursuit_mean, na.rm = TRUE))

outcomes <- list(
  list(var = "shooting_incidents", label = "Shootings",  col = COL_SHOOTING),
  list(var = "robbery_count",      label = "Robberies",  col = COL_ROBBERY),
  list(var = "collision_count",    label = "Collisions", col = COL_COLLISION)
)

shiftshare_results <- list()

for (o in outcomes) {
  fml <- as.formula(paste0(o$var, " ~ post:pre_pursuit_dm | pct + month_date"))
  mod <- feols(fml, data = panel, cluster = ~pct)
  shiftshare_results[[o$label]] <- mod
  message(sprintf("  %s: b = %.4f, SE = %.4f, p = %.4f",
                  o$label, coef(mod)["post:pre_pursuit_dm"],
                  se(mod)["post:pre_pursuit_dm"],
                  pvalue(mod)["post:pre_pursuit_dm"]))
}


# 5. Continuous DiD (Specification 2) ==========================================
#
# Y_{pt} = a_p + a_t + beta * pursuit_events_{pt} + e_{pt}
# (Note: endogenous — pursuit_events responds to same shocks as Y)

message("\n--- Specification 2: Continuous DiD (endogenous) ---")

continuous_results <- list()

for (o in outcomes) {
  fml <- as.formula(paste0(o$var, " ~ pursuit_events | pct + month_date"))
  mod <- feols(fml, data = panel, cluster = ~pct)
  continuous_results[[o$label]] <- mod
  message(sprintf("  %s: b = %.4f, SE = %.4f, p = %.4f",
                  o$label, coef(mod)["pursuit_events"],
                  se(mod)["pursuit_events"],
                  pvalue(mod)["pursuit_events"]))
}


# 5b. Continuous DiD with Precinct-Specific Time Trends (Specifications 3-4) ===
#
# Direct implementation of Editor Comment 2 / R1 Comment 3 (JQC R&R): the
# editor asked for the continuous pursuit-intensity regression conditioned
# on "interacted precinct*time fixed effects" to absorb area-level crime
# trends, as a partial remedy for the concern that pursuits and crime might
# both respond to the same underlying dynamics. Two variants are estimated:
#
#   (3) Precinct-specific linear trend: fixest varying-slope syntax
#       `pct[time_index]` gives every precinct its own linear trend in
#       calendar time, in addition to the month_date FE.
#   (4) Precinct x year FE: coarser alternative, `pct^year`, absorbs
#       precinct-specific year-level shifts without consuming all
#       within-year timing variation.
#
# Neither variant resolves same-period simultaneity (a same-month crime
# spike could itself drive that month's pursuit count) -- both address only
# slow-moving secular confounds. Fit with tryCatch: the Oct 2022 pursuit
# surge hit nearly every precinct in the same calendar month, so there is
# little within-precinct timing variation to separate a precinct's own
# trend from the treatment surge, and these specifications may be weakly
# identified (large SEs) or fail to converge.

message("\n--- Specifications 3-4: Continuous DiD with precinct-specific time controls ---")

fit_trend_spec <- function(outcome_var, fml_str, spec_label) {
  tryCatch(
    feols(as.formula(fml_str), data = panel, cluster = ~pct),
    error = function(e) {
      message(sprintf("  %s [%s]: FAILED TO FIT -- %s", outcome_var, spec_label, conditionMessage(e)))
      NULL
    }
  )
}

precinct_trend_results   <- list()
precinct_year_fe_results <- list()

for (o in outcomes) {
  fml_trend <- paste0(o$var, " ~ pursuit_events | month_date + pct[time_index]")
  mod_trend <- fit_trend_spec(o$var, fml_trend, "precinct_trend")
  if (!is.null(mod_trend)) {
    precinct_trend_results[[o$label]] <- mod_trend
    message(sprintf("  %s [precinct trend]:   b = %.4f, SE = %.4f, p = %.4f",
                    o$label, coef(mod_trend)["pursuit_events"],
                    se(mod_trend)["pursuit_events"],
                    pvalue(mod_trend)["pursuit_events"]))
  }

  fml_year <- paste0(o$var, " ~ pursuit_events | month_date + pct^year")
  mod_year <- fit_trend_spec(o$var, fml_year, "precinct_year_fe")
  if (!is.null(mod_year)) {
    precinct_year_fe_results[[o$label]] <- mod_year
    message(sprintf("  %s [precinct x year]:  b = %.4f, SE = %.4f, p = %.4f",
                    o$label, coef(mod_year)["pursuit_events"],
                    se(mod_year)["pursuit_events"],
                    pvalue(mod_year)["pursuit_events"]))
  }
}

# SE-inflation diagnostics relative to the base continuous spec (2): a large
# ratio signals the added precinct-specific time controls are consuming most
# of the identifying variation, per the weak-identification risk noted above.
trend_diag <- map_dfr(names(continuous_results), function(nm) {
  base_se <- se(continuous_results[[nm]])["pursuit_events"]
  tibble(
    outcome = nm,
    spec    = c("precinct_trend", "precinct_year_fe"),
    se_base_continuous = base_se,
    se_new = c(
      if (nm %in% names(precinct_trend_results))   se(precinct_trend_results[[nm]])["pursuit_events"]   else NA_real_,
      if (nm %in% names(precinct_year_fe_results)) se(precinct_year_fe_results[[nm]])["pursuit_events"] else NA_real_
    )
  ) |>
    mutate(se_inflation_ratio = se_new / se_base_continuous)
})

write_csv(trend_diag, file.path(ext_dir, "dose_did_trend_diagnostics.csv"))
message("  Saved: output/extension/dose_did_trend_diagnostics.csv")


# 6. Binned DiD with Event Study (Specification 5) ============================
#
# Tercile-based event study: high-dose vs low-dose precincts, with
# medium-dose as reference. Leads and lags around Oct 2022.

message("\n--- Specification 5: Binned DiD event study ---")

# Create relative-time bins (semi-annual for power)
panel <- panel |>
  mutate(
    rel_half = case_when(
      rel_month < -24 ~ -5L,
      rel_month < -18 ~ -4L,
      rel_month < -12 ~ -3L,
      rel_month < -6  ~ -2L,
      rel_month < 0   ~ -1L,     # reference period
      rel_month < 6   ~  1L,
      rel_month < 12  ~  2L,
      rel_month < 18  ~  3L,
      rel_month < 24  ~  4L,
      TRUE            ~  5L
    ),
    high_dose = as.integer(dose_tercile == 3),
    low_dose  = as.integer(dose_tercile == 1)
  )

# Keep all periods (including rel_half == -1) in the data — since high_dose
# is time-invariant per precinct, it is collinear with the pct FE once any
# level of rel_half_f is dropped from the data entirely (rank deficiency).
# i(..., ref = -1) instead drops only the -1 *interaction term*, leaving the
# full sample available to estimate the pct/month_date fixed effects.
panel_es <- panel |>
  mutate(rel_half_f = factor(rel_half))

binned_results <- list()
es_data <- list()

for (o in outcomes) {
  fml <- as.formula(paste0(o$var, " ~ i(rel_half_f, high_dose, ref = -1) | pct + month_date"))
  mod <- feols(fml, data = panel_es, cluster = ~pct)
  binned_results[[o$label]] <- mod

  # Extract event-study coefficients
  cf <- broom::tidy(mod, conf.int = TRUE) |>
    filter(str_detect(term, "high_dose")) |>
    mutate(
      rel_half = as.integer(str_extract(term, "-?\\d+")),
      outcome = o$label
    ) |>
    select(outcome, rel_half, estimate, std.error, p.value, conf.low, conf.high)

  es_data[[o$label]] <- cf
}

es_df <- bind_rows(es_data)

# Add the zero reference period
es_df <- es_df |>
  bind_rows(
    tibble(outcome = rep(c("Shootings", "Robberies", "Collisions"), each = 1),
           rel_half = -1, estimate = 0, std.error = 0, p.value = NA,
           conf.low = 0, conf.high = 0)
  ) |>
  arrange(outcome, rel_half)


# 7. Save Results ==============================================================

message("\n--- Saving results ---")

# Compile all specifications into one table
compile_results <- function(mod_list, spec_name) {
  map_dfr(names(mod_list), function(nm) {
    mod <- mod_list[[nm]]
    tibble(
      outcome  = nm,
      spec     = spec_name,
      term     = names(coef(mod)),
      estimate = coef(mod),
      std_err  = se(mod),
      t_stat   = tstat(mod),
      p_value  = pvalue(mod),
      n_obs    = nobs(mod),
      n_pcts   = n_distinct(panel$pct)
    )
  })
}

all_results <- bind_rows(
  compile_results(shiftshare_results, "shift_share"),
  compile_results(continuous_results, "continuous"),
  compile_results(precinct_trend_results, "continuous_precinct_trend"),
  compile_results(precinct_year_fe_results, "continuous_precinct_year_fe")
)

write_csv(all_results, file.path(ext_dir, "dose_did_results.csv"))
message("  Saved: output/extension/dose_did_results.csv")

write_csv(es_df, file.path(ext_dir, "dose_did_event_study.csv"))
message("  Saved: output/extension/dose_did_event_study.csv")

# First-stage diagnostics
fs_diag <- tibble(
  predictor        = "pre_pursuit_mean",
  response         = "post_pursuit_mean",
  r_squared        = fs_r2,
  f_statistic      = fs_fstat,
  n_precincts      = nrow(dose_info),
  correlation      = cor(dose_info$pre_pursuit_mean, dose_info$post_pursuit_mean)
)
write_csv(fs_diag, file.path(ext_dir, "dose_did_first_stage.csv"))
message("  Saved: output/extension/dose_did_first_stage.csv")

# Save model objects
saveRDS(
  list(
    shiftshare        = shiftshare_results,
    continuous        = continuous_results,
    precinct_trend    = precinct_trend_results,
    precinct_year_fe  = precinct_year_fe_results,
    binned            = binned_results,
    dose_info         = dose_info
  ),
  file.path(ext_dir, "dose_did_models.rds")
)
message("  Saved: output/extension/dose_did_models.rds")


# 8. Event-Study Figures =======================================================

message("\n--- Creating event-study figures ---")

for (o in outcomes) {
  es_sub <- es_df |> filter(outcome == o$label)

  # Half-year labels
  half_labels <- c("-30 to -25", "-24 to -19", "-18 to -13",
                   "-12 to -7", "-6 to -1", "0 to 5",
                   "6 to 11", "12 to 17", "18 to 23", "24+")

  p <- ggplot(es_sub, aes(x = rel_half, y = estimate)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey40", linewidth = 0.5) +
    geom_pointrange(
      aes(ymin = conf.low, ymax = conf.high),
      color = o$col, size = 0.6, linewidth = 0.8
    ) +
    scale_x_continuous(breaks = sort(unique(es_sub$rel_half))) +
    labs(
      title = paste0("Dose-DiD Event Study: ", o$label),
      subtitle = "High-dose vs. low/medium-dose precincts (semi-annual bins)",
      x = "Half-years relative to Oct 2022 escalation",
      y = paste("Differential", tolower(o$label), "(high-dose precincts)"),
      caption = paste0(
        "Precincts grouped into terciles by post-period pursuit intensity.\n",
        "Reference period: 6 months before intervention. Cluster-robust SEs (precinct).\n",
        "N = ", n_distinct(panel$pct), " precincts, ",
        format(nrow(panel), big.mark = ","), " precinct-months."
      )
    ) +
    theme_pursuit(base_size = 13)

  save_plot(p, paste0("fig_dose_did_event_study_", tolower(o$label)), w = 8, h = 5)
  message("  Saved: fig_dose_did_event_study_", tolower(o$label), ".png/.pdf")
}


# 9. Binned Scatter (Pursuit Surge vs. Outcome Change) =========================

message("\n--- Creating binscatter ---")

# Precinct-level change in outcomes (post minus pre mean)
pct_changes <- panel |>
  group_by(pct, post) |>
  summarise(
    across(c(shooting_incidents, robbery_count, collision_count, pursuit_events),
           \(x) mean(x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  pivot_wider(
    id_cols = pct,
    names_from = post,
    values_from = c(shooting_incidents, robbery_count, collision_count, pursuit_events),
    names_glue = "{.value}_{post}"
  ) |>
  mutate(
    d_shooting  = shooting_incidents_1 - shooting_incidents_0,
    d_robbery   = robbery_count_1      - robbery_count_0,
    d_collision  = collision_count_1    - collision_count_0,
    d_pursuit   = pursuit_events_1     - pursuit_events_0
  )

# Create 20-bin scatter for each outcome
bin_scatter_data <- pct_changes |>
  mutate(pursuit_bin = ntile(d_pursuit, 20)) |>
  group_by(pursuit_bin) |>
  summarise(
    pursuit_change   = mean(d_pursuit,   na.rm = TRUE),
    shooting_change  = mean(d_shooting,  na.rm = TRUE),
    robbery_change   = mean(d_robbery,   na.rm = TRUE),
    collision_change = mean(d_collision,  na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(shooting_change, robbery_change, collision_change),
    names_to = "outcome",
    values_to = "outcome_change"
  ) |>
  mutate(
    outcome_label = case_when(
      outcome == "shooting_change"  ~ "Shootings",
      outcome == "robbery_change"   ~ "Robberies",
      outcome == "collision_change" ~ "Collisions"
    ),
    outcome_label = factor(outcome_label, levels = c("Collisions", "Shootings", "Robberies"))
  )

p_binscatter <- ggplot(bin_scatter_data,
                        aes(x = pursuit_change, y = outcome_change)) +
  geom_hline(yintercept = 0, color = "grey60", linewidth = 0.3) +
  geom_point(aes(color = outcome_label), alpha = 0.7, size = 2) +
  geom_smooth(aes(color = outcome_label), method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~outcome_label, scales = "free_y") +
  scale_color_manual(values = c(
    "Collisions" = COL_COLLISION,
    "Shootings"  = COL_SHOOTING,
    "Robberies"  = COL_ROBBERY
  )) +
  labs(
    title = "Pursuit Surge and Outcome Changes by Precinct",
    subtitle = "Binscatter: 20-bin means of precinct-level pre/post changes",
    x = "Change in mean monthly pursuits (post - pre)",
    y = "Change in mean monthly outcome (post - pre)",
    caption = paste0(
      "Each dot = mean of ~", round(n_distinct(panel$pct) / 20),
      " precincts. OLS fit on binned means.\n",
      "Pre = Jan 2018 – Sep 2022; Post = Oct 2022 – Sep 2025."
    )
  ) +
  theme_pursuit(base_size = 12) +
  theme(legend.position = "none")

save_plot(p_binscatter, "fig_dose_did_binscatter", w = 10, h = 4)
message("  Saved: fig_dose_did_binscatter.png/.pdf")


# 10. Summary ==================================================================

message("\n=== PURSUIT-DOSE DiD SUMMARY ===")
message("\nSpecification 1 — Shift-share DiD (post × pre_pursuit_mean):")
for (o in outcomes) {
  res <- shiftshare_results[[o$label]]
  term <- "post:pre_pursuit_dm"
  message(sprintf("  %-12s  b = %7.4f  SE = %7.4f  p = %.4f",
                  o$label, coef(res)[term], se(res)[term], pvalue(res)[term]))
}
message("\nSpecification 2 — Continuous DiD (pursuit_events, endogenous):")
for (o in outcomes) {
  res <- continuous_results[[o$label]]
  term <- "pursuit_events"
  message(sprintf("  %-12s  b = %7.4f  SE = %7.4f  p = %.4f",
                  o$label, coef(res)[term], se(res)[term], pvalue(res)[term]))
}
message("\nSpecification 3 — Continuous DiD + precinct-specific linear trend:")
for (o in outcomes) {
  if (!o$label %in% names(precinct_trend_results)) {
    message(sprintf("  %-12s  [not estimable — see fit warning above]", o$label))
    next
  }
  res <- precinct_trend_results[[o$label]]
  term <- "pursuit_events"
  message(sprintf("  %-12s  b = %7.4f  SE = %7.4f  p = %.4f",
                  o$label, coef(res)[term], se(res)[term], pvalue(res)[term]))
}
message("\nSpecification 4 — Continuous DiD + precinct × year FE:")
for (o in outcomes) {
  if (!o$label %in% names(precinct_year_fe_results)) {
    message(sprintf("  %-12s  [not estimable — see fit warning above]", o$label))
    next
  }
  res <- precinct_year_fe_results[[o$label]]
  term <- "pursuit_events"
  message(sprintf("  %-12s  b = %7.4f  SE = %7.4f  p = %.4f",
                  o$label, coef(res)[term], se(res)[term], pvalue(res)[term]))
}
message("\nSE inflation vs. Spec 2 (precinct-time controls / base continuous SE):")
message(paste(capture.output(print(as.data.frame(trend_diag))), collapse = "\n"))
message(sprintf("\nFirst stage: pre_pursuit_mean → post_pursuit_mean  R2 = %.3f  F = %.1f",
                fs_r2, fs_fstat))
message("\nDone.")
