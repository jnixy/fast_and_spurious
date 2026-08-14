# ==============================================================================
# Vehicle Pursuits Study: Precinct-Level Staggered Adoption (Sun-Abraham)
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Estimate causal effects of the NYPD pursuit policy change using
#   staggered adoption at the precinct level. Treatment timing varies:
#   each precinct is "treated" in the first month it records ≥1 pursuit
#   after the October 2022 policy change. Precincts that never recorded
#   a pursuit after Oct 2022 serve as never-treated controls.
#
#   Identification logic: if deterrence operates through local learning
#   (suspects update beliefs when they observe pursuits in their precinct),
#   the relevant treatment event is the first local pursuit. Variation in
#   when precincts first experience a pursuit is partly exogenous given
#   the policy change — it depends on when crime and pursuit opportunities
#   arise in each precinct after the gate opens citywide in Oct 2022.
#
#   Estimator: Sun-Abraham (2021) via fixest::sunab(), which corrects
#   the negative weighting problem in standard TWFE under staggered adoption.
#   Cohort-specific ATTs are averaged to the aggregate ATT.
#
#   Pre-trend test: event study coefficients for months before first pursuit
#   should be statistically indistinguishable from zero. We report these
#   explicitly as an empirical test of the parallel trends assumption.
#
# Inputs:
#   - output/precinct_did/pct_monthly_pursuit.csv   (from script 01)
#   - output/precinct_did/pct_monthly_shootings.csv (from script 01)
#   - output/precinct_did/pct_monthly_robberies.csv (from script 01)
#
# Outputs (saved to output/precinct_did/):
#   - cohort_summary.csv          -- distribution of first-pursuit months
#   - pct_staggered_results.csv   -- aggregate ATT and pre-period placebos
#   - pct_pre_trends.csv          -- pre-treatment event study coefficients
#   - fig_precinct_event_study_shooting.png/.pdf
#   - fig_precinct_event_study_robbery.png/.pdf
#
# Runtime: ~3–5 min
# ==============================================================================

library(tidyverse)
library(lubridate)
library(fixest)
library(here)

set.seed(20260303)


# 0. Setup ---------------------------------------------------------------------

dir.create(here("output", "precinct_did"), showWarnings = FALSE, recursive = TRUE)
pct_dir  <- here("output", "precinct_did")
plot_dir <- here("output", "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

message("Script 16: Precinct Staggered Adoption (Sun-Abraham)")
message("Working directory: ", here())

COL_SHOOTING <- "#6A0572"
COL_ROBBERY  <- "#003049"
COL_ZERO     <- "grey50"

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
      legend.position  = "none",
      plot.margin      = margin(15, 15, 15, 15)
    )
}

save_plot <- function(p, name, w = 9, h = 6) {
  ggsave(file.path(plot_dir, paste0(name, ".png")), plot = p,
         width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(plot_dir, paste0(name, ".pdf")), plot = p,
         width = w, height = h, bg = "white")
  invisible(p)
}


# 1. Load precinct panels ======================================================

message("\n[1] Loading precinct panels...")

pursuits_pct  <- read_csv(file.path(pct_dir, "pct_monthly_pursuit.csv"),
                           show_col_types = FALSE) %>%
  mutate(month_date = as.Date(month_date))

shootings_pct <- read_csv(file.path(pct_dir, "pct_monthly_shootings.csv"),
                           show_col_types = FALSE) %>%
  mutate(month_date = as.Date(month_date))

robberies_pct <- read_csv(file.path(pct_dir, "pct_monthly_robberies.csv"),
                           show_col_types = FALSE) %>%
  mutate(month_date = as.Date(month_date))

message("  Loaded ", n_distinct(pursuits_pct$pct), " precincts × ",
        n_distinct(pursuits_pct$month_date), " months")


# 2. Build staggered panel =====================================================
#
# Define treatment timing per precinct:
#   cohort_date = first month with ≥1 pursuit AFTER Oct 2022 policy change
#   cohort = 0 for never-treated precincts (no pursuit in post-period)
#
# Policy change: Oct 2022. We look for first pursuit >= Oct 2022.
# sunab() requires integer cohort and time IDs.

message("\n[2] Defining treatment timing (first-pursuit cohorts)...")

policy_start <- as.Date("2022-10-01")
analysis_end <- as.Date("2025-12-01")
analysis_start <- as.Date("2018-01-01")

# Identify first post-policy pursuit month per precinct.
# NOTE: This initial first_pursuit is computed from the raw pursuits_pct before
# panel construction and the pct 116→105 recode. It is superseded by the
# recomputation below (after recode) and should not be used for cohort assignment.
first_pursuit <- pursuits_pct %>%
  filter(month_date >= policy_start, pursuit_events > 0) %>%
  group_by(pct) %>%
  summarise(cohort_date = min(month_date), .groups = "drop")

# Build combined panel
panel_raw <- pursuits_pct %>%
  left_join(shootings_pct,  by = c("pct", "month_date")) %>%
  left_join(robberies_pct,  by = c("pct", "month_date")) %>%
  replace_na(list(pursuit_events     = 0L,
                  shooting_incidents = 0L,
                  robbery_count      = 0L)) %>%
  filter(pct >= 1, pct <= 123) %>%
  filter(month_date >= analysis_start, month_date <= analysis_end)

# Restrict to precincts with actual crime data (drop grid-rows for non-existent precincts)
panel_raw <- panel_raw %>%
  group_by(pct) %>%
  filter(any(shooting_incidents > 0) | any(robbery_count > 0)) %>%
  ungroup()

# Recode Precinct 116 as 105 (consolidated mid-study) and re-aggregate.
# Pct 116 was formally merged into Pct 105; this ensures a stable 77-precinct panel.
panel_raw <- panel_raw |>
  mutate(pct = if_else(pct == 116L, 105L, pct)) |>
  group_by(pct, month_date) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop")

# Recompute first_pursuit from recoded panel so cohort assignments reflect merged pct
first_pursuit <- panel_raw |>
  filter(month_date >= policy_start, pursuit_events > 0) |>
  group_by(pct) |>
  summarise(cohort_date = min(month_date), .groups = "drop")

# Attach cohort
panel <- panel_raw %>%
  left_join(first_pursuit, by = "pct") %>%
  # Never-treated precincts get cohort_date = NA → convert to sentinel
  # sunab() requires cohort = 0 for never-treated
  mutate(
    cohort_date   = if_else(is.na(cohort_date), as.Date("2099-01-01"), cohort_date),
    never_treated = (cohort_date == as.Date("2099-01-01")),
    # Sequential month index: Jan 2018 = 1, Feb 2018 = 2, ..., Sep 2025 = 93
    # sunab() computes relative time as (period - cohort), so BOTH must be on the
    # same sequential integer scale for relative times to equal actual months.
    # YYYYMM would give nonsensical relative times across year boundaries.
    time_int  = (year(month_date) - 2018L) * 12L + month(month_date),
    # cohort_int: sequential index of first-pursuit month for treated precincts;
    # 0 for never-treated (fixest::sunab() sentinel; 0 is outside 1..93 range)
    cohort_int = if_else(
      never_treated,
      0L,
      (year(cohort_date) - 2018L) * 12L + month(cohort_date)
    )
  )

n_total       <- n_distinct(panel$pct)
n_treated     <- n_distinct(panel$pct[!panel$never_treated])
n_never       <- n_total - n_treated

message("  Total precincts: ", n_total)
message("  Ever-treated (≥1 post-Oct 2022 pursuit): ", n_treated)
message("  Never-treated (no post-Oct 2022 pursuit): ", n_never)

# Cohort distribution (when did each precinct first get a pursuit?)
cohort_dist <- panel %>%
  filter(!never_treated) %>%
  distinct(pct, cohort_date, cohort_int) %>%
  count(cohort_date, cohort_int, name = "n_precincts") %>%
  arrange(cohort_date)

write_csv(cohort_dist, file.path(pct_dir, "cohort_summary.csv"))
message("  Saved: cohort_summary.csv")


# 3. Sun-Abraham Staggered Event Study =========================================
#
# fixest::sunab(cohort, period) implements Sun & Abraham (2021).
# Requires:
#   - cohort: integer, 0 = never-treated (used as reference)
#   - period: integer time index
#   - Unit and time FEs included as | pct + time_int
#
# Event window: relative time from first pursuit.
# We report t = -12 through t = +24 where data allow.
# Pre-period (t < 0) coefficients serve as placebo / parallel-trends test.

message("\n[3] Estimating Sun-Abraham event study models...")

# Filter to precincts with ≥ 12 months pre-treatment for reliable pre-trends
# (never-treated are always included)
min_pre_months <- 12

# Relative time in months = time_int - cohort_int (both sequential, so this is exact)
panel <- panel %>%
  mutate(
    rel_time = if_else(
      never_treated,
      NA_integer_,
      time_int - cohort_int
    )
  )

# Verify rel_time range
message("  Relative time range (treated): ",
        min(panel$rel_time, na.rm = TRUE), " to ",
        max(panel$rel_time, na.rm = TRUE))

# For sunab(), we need to use the cohort_int and time_int variables
# Never-treated precincts need cohort_int = 0 (not a real date — used as control)
# sunab() recognizes 0 as the "never treated" indicator

fit_shooting <- feols(
  shooting_incidents ~ sunab(cohort_int, time_int) | pct + time_int,
  data    = panel,
  cluster = ~pct
)

fit_robbery <- feols(
  robbery_count ~ sunab(cohort_int, time_int) | pct + time_int,
  data    = panel,
  cluster = ~pct
)

message("  Models estimated.")
message("  Shooting N: ", fit_shooting$nobs)
message("  Robbery N:  ", fit_robbery$nobs)


# 4. Extract event study coefficients ==========================================
#
# Sun-Abraham (sunab) in fixest reports cohort-specific ATTs for each
# (calendar_time, cohort) pair. Term names follow the format:
#   "time_int::CALENDAR_YM:cohort_int::COHORT_YM"
# We parse calendar_YM and cohort_YM, compute rel_time in months,
# then aggregate across cohorts weighted by cohort size (SA aggregation).
#
# summary()$coeftable gives Estimate + Std. Error in the same row order as
# coef(), avoiding the vcov() size-mismatch problem.

message("\n[4] Extracting event study coefficients...")

extract_sunab_es <- function(fit, outcome_label) {
  # fixest::sunab() automatically aggregates cohort-specific ATTs to produce
  # a single ATT per relative time period. The coeftable row names are simply
  # "time_int::REL_TIME" (no cohort suffix in the aggregated output).
  ct       <- summary(fit)$coeftable
  cf_names <- rownames(ct)

  # Keep only sunab relative-time terms (contain "::")
  is_rt <- grepl("::", cf_names)
  ct_rt <- ct[is_rt, , drop = FALSE]
  nm_rt <- cf_names[is_rt]

  # Parse relative time from "time_int::REL_TIME"
  rel_vals <- as.integer(str_extract(nm_rt, "(?<=::)-?\\d+"))

  tibble(
    rel_time  = rel_vals,
    estimate  = ct_rt[, "Estimate"],
    se        = ct_rt[, "Std. Error"],
    conf_low  = ct_rt[, "Estimate"] - 1.96 * ct_rt[, "Std. Error"],
    conf_high = ct_rt[, "Estimate"] + 1.96 * ct_rt[, "Std. Error"],
    outcome   = outcome_label
  ) %>%
    filter(!is.na(rel_time)) %>%
    arrange(rel_time)
}

es_shoot <- extract_sunab_es(fit_shooting, "Shooting incidents")
es_rob   <- extract_sunab_es(fit_robbery,  "Robbery")

message("  Event study rows — Shooting: ", nrow(es_shoot),
        ", Robbery: ", nrow(es_rob))


# 5. Save event study results ==================================================

all_es <- bind_rows(es_shoot, es_rob)
write_csv(all_es, file.path(pct_dir, "pct_staggered_results.csv"))
message("  Saved: pct_staggered_results.csv")

# Pre-trend check: are pre-period coefficients jointly near zero?
pre_shoot <- es_shoot %>% filter(rel_time < 0)
pre_rob   <- es_rob   %>% filter(rel_time < 0)

pre_trends_out <- bind_rows(
  pre_shoot %>% mutate(outcome = "shooting"),
  pre_rob   %>% mutate(outcome = "robbery")
)
write_csv(pre_trends_out, file.path(pct_dir, "pct_pre_trends.csv"))
message("  Saved: pct_pre_trends.csv")

# Report pre-trend summary
if (nrow(pre_shoot) > 0) {
  pct_sig_shoot <- mean(
    pre_shoot$conf_low > 0 | pre_shoot$conf_high < 0
  )
  message("  Pre-period shooting: ", nrow(pre_shoot), " periods, ",
          round(pct_sig_shoot * 100), "% significantly different from 0")
}
if (nrow(pre_rob) > 0) {
  pct_sig_rob <- mean(
    pre_rob$conf_low > 0 | pre_rob$conf_high < 0
  )
  message("  Pre-period robbery: ", nrow(pre_rob), " periods, ",
          round(pct_sig_rob * 100), "% significantly different from 0")
}


# 6. Event Study Plots =========================================================

message("\n[6] Creating event study plots...")

plot_event_study <- function(es_df, col, title_str) {
  # Focus on window t = -18 to t = +30 (or data range)
  es_plot <- es_df %>%
    filter(rel_time >= -18, rel_time <= 30)

  ggplot(es_plot, aes(x = rel_time, y = estimate)) +
    # Zero reference line (null hypothesis)
    geom_hline(yintercept = 0, color = "grey40", linetype = "dashed", linewidth = 0.5) +
    # Treatment onset
    geom_vline(xintercept = -0.5, color = "grey20", linetype = "dotted", linewidth = 0.5) +
    # Confidence intervals
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
                fill = col, alpha = 0.15) +
    # Coefficient path
    geom_line(color = col, linewidth = 0.8) +
    geom_point(aes(shape = rel_time >= 0, fill = rel_time >= 0),
               size = 2, color = col) +
    scale_shape_manual(values = c(21, 19), guide = "none") +
    scale_fill_manual(values = c("white", col), guide = "none") +
    scale_x_continuous(
      breaks = seq(-18, 30, by = 6),
      labels = seq(-18, 30, by = 6),
      minor_breaks = NULL
    ) +
    annotate("text", x = 1, y = max(es_plot$conf_high, na.rm = TRUE) * 0.9,
             label = "Treatment\nonset", hjust = 0, size = 3, color = "grey30") +
    labs(
      title   = title_str,
      x       = "Months relative to first precinct pursuit (t = 0)",
      y       = "Estimated effect (Sun-Abraham ATT)",
      caption = paste0(
        "Sun-Abraham (2021) heterogeneity-robust event study.\n",
        "Never-treated precincts serve as control group. 95% CIs shown.\n",
        "SEs clustered by precinct (G = ", n_total, ")."
      )
    ) +
    theme_pursuit()
}

p_shoot_es <- plot_event_study(
  es_shoot, COL_SHOOTING,
  "Effect of Pursuit Exposure on Shooting Incidents\nPrecinct-Level Staggered Adoption"
)
save_plot(p_shoot_es, "fig_precinct_event_study_shooting", h = 4.5)
message("  Saved: fig_precinct_event_study_shooting.png/.pdf")

p_rob_es <- plot_event_study(
  es_rob, COL_ROBBERY,
  "Effect of Pursuit Exposure on Robbery\nPrecinct-Level Staggered Adoption"
)
save_plot(p_rob_es, "fig_precinct_event_study_robbery", h = 4.5)
message("  Saved: fig_precinct_event_study_robbery.png/.pdf")


# 7. Aggregate ATT =============================================================
#
# Aggregate Sun-Abraham cohort-time ATTs to a single summary ATT for reporting.

message("\n[7] Aggregate ATT estimates...")

# Aggregate Sun-Abraham cohort-time ATTs via aggregate(fit, agg = "att")
# (dispatches to aggregate.fixest S3 method; fixest::aggregate is not exported).
# This produces the population-weighted aggregate ATT with proper inference,
# accounting for cohort-size weights and the full covariance matrix.
# A simple mean over post-period event study estimates would ignore cohort
# weights and produce an incorrect point estimate (not just an invalid SE).
att_shoot <- aggregate(fit_shooting, agg = "att")
att_rob   <- aggregate(fit_robbery,  agg = "att")

# aggregate.fixest returns a 1×4 named matrix [Estimate, Std. Error, t value, Pr(>|t|)]
# directly — no summary() needed. Save to CSV so the manuscript can load these values.
att_summary <- bind_rows(
  tibble(
    outcome   = "shooting_incidents",
    estimate  = att_shoot[1, "Estimate"],
    std_error = att_shoot[1, "Std. Error"],
    t_stat    = att_shoot[1, "t value"],
    p_value   = att_shoot[1, "Pr(>|t|)"]
  ),
  tibble(
    outcome   = "robbery_count",
    estimate  = att_rob[1, "Estimate"],
    std_error = att_rob[1, "Std. Error"],
    t_stat    = att_rob[1, "t value"],
    p_value   = att_rob[1, "Pr(>|t|)"]
  )
)
write_csv(att_summary, file.path(pct_dir, "pct_aggregate_att.csv"))

message("  Shooting aggregate ATT (SA-weighted): b = ",
        round(att_shoot[1, "Estimate"], 3),
        ", SE = ", round(att_shoot[1, "Std. Error"], 3),
        ", p = ", round(att_shoot[1, "Pr(>|t|)"], 4))
message("  Robbery aggregate ATT (SA-weighted): b = ",
        round(att_rob[1, "Estimate"], 3),
        ", SE = ", round(att_rob[1, "Std. Error"], 3),
        ", p = ", round(att_rob[1, "Pr(>|t|)"], 4))
message("  Saved: pct_aggregate_att.csv")

message("\nOutputs saved to: ", pct_dir)
message("Figures saved to: ", plot_dir)
message("\nScript 16 complete.\n")
