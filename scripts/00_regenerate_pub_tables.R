# ==============================================================================
# Vehicle Pursuit Policy Analysis — Script 00: Regenerate Publication Tables
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Run this after any pipeline re-execution (scripts 01, 03, 06, 07, 17) to
#   refresh the manuscript-ready publication tables the manuscript reads at
#   render time. Table 5's cost/benefit line items (fatality rate, VSL,
#   injury rate/cost, property cost) are re-derived here from
#   cba_summary.csv/cba_net_assessment.csv where possible, but three cost
#   sub-lines (fatalities, injuries, property damage) are recomputed from the
#   same underlying constants as script 07_cba.R rather than read directly
#   from a CBA output column. These constants (0.010 fatality rate, $12.5M
#   VSL, 0.35 injury rate, 1.5 injuries/crash, $200K injury cost, $20K
#   property cost) MUST be kept in sync with 07_cba.R's
#   FATALITY_RATE_CENTRAL/VSL/INJURY_RATE/INJURIES_PER/INJURY_COST/
#   PROPERTY_COST constants — verified matching as of 2026-08-14.
#
# Inputs:
#   - output/tables/monthly_panel.csv                     (from script 01)
#   - output/its_results/its_newey_west.csv                (from script 03)
#   - output/cba_results/cba_summary.csv                   (from script 07)
#   - output/cba_results/cba_net_assessment.csv             (from script 07)
#   - output/cba_results/cba_attribution_scenarios.csv      (from script 07)
#   - output/event_results/tisch_reversal_pct_changes.csv   (from script 06)
#   - output/precinct_did/pct_ddd_es_*.csv                  (from script 17)
#
# Outputs (saved to output/publication_tables/):
#   - table1_annual_summary.csv    -- annual pursuit/collision/crime totals
#   - table2a_its_segmented.csv    -- ITS level-shift/slope-change estimates
#   - table5_cost_benefit.csv      -- CBA cost/benefit/breakeven line items
#   - table5b_net_assessment.csv   -- net assessment by scenario
#   - table7_regime_outcomes.csv   -- mean monthly outcomes by policy regime
#   - table9_tisch_reversal.csv    -- pre/post-Tisch percent changes
#   - table10_crash_ratio.csv      -- annual crash-per-pursuit ratio
#   - table_a2-a5_ddd_*.csv        -- Appendix A DDD event-study tables
#
# Runtime: <1 min
# ==============================================================================

# ── 0. Setup ──────────────────────────────────────────────────────────────────

library(tidyverse)
library(here)

message("\nREGENERATING PUBLICATION TABLES\n")

out_dir <- here("output", "publication_tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

INTERVENTION <- as.Date("2022-10-01")
MADDREY      <- as.Date("2023-08-01")
TISCH        <- as.Date("2025-02-01")

# ── Load inputs ────────────────────────────────────────────────────────────────

panel <- read_csv(here("output", "tables", "monthly_panel.csv"),
                  show_col_types = FALSE) %>%
  mutate(month_date = as.Date(month_date))

its_nw <- read_csv(here("output", "its_results", "its_newey_west.csv"),
                   show_col_types = FALSE)

cba_sum   <- read_csv(here("output", "cba_results", "cba_summary.csv"),
                      show_col_types = FALSE)
cba_net   <- read_csv(here("output", "cba_results", "cba_net_assessment.csv"),
                      show_col_types = FALSE)
cba_attr  <- read_csv(here("output", "cba_results", "cba_attribution_scenarios.csv"),
                      show_col_types = FALSE)

message("Inputs loaded.\n")


# ── Table 1: Annual Summary ────────────────────────────────────────────────────

message("Regenerating table1_annual_summary.csv...")

# Use 2025 through Sep only (right-censoring applied in monthly_panel already)
t1 <- panel %>%
  filter(month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) %>%
  mutate(year = year(month_date)) %>%
  group_by(year) %>%
  summarise(
    pursuits     = sum(pursuit_events,     na.rm = TRUE),
    collisions   = sum(pursuit_crashes,    na.rm = TRUE),
    robberies    = sum(robbery_count,      na.rm = TRUE),
    shootings    = sum(shooting_incidents, na.rm = TRUE),
    gun_violence = sum(total_gun_events,   na.rm = TRUE),
    .groups      = "drop"
  ) %>%
  mutate(
    pursuit_pct      = (pursuits     / lag(pursuits)     - 1) * 100,
    collision_pct    = (collisions   / lag(collisions)   - 1) * 100,
    robbery_pct      = (robberies    / lag(robberies)    - 1) * 100,
    shooting_pct     = (shootings    / lag(shootings)    - 1) * 100,
    gun_violence_pct = (gun_violence / lag(gun_violence) - 1) * 100
  )

write_csv(t1, file.path(out_dir, "table1_annual_summary.csv"))
message("  Done: table1_annual_summary.csv")


# ── Table 2a: ITS Segmented Regression ────────────────────────────────────────

message("Regenerating table2a_its_segmented.csv...")

# Significance stars based on NW p-value
sig_stars <- function(p) {
  case_when(
    p < .001 ~ "***",
    p < .01  ~ "**",
    p < .05  ~ "*",
    TRUE     ~ ""
  )
}

t2a <- its_nw %>%
  filter(term %in% c("post", "t_since")) %>%
  mutate(
    term_label = recode(term, "post" = "Level shift", "t_since" = "Slope change"),
    ci_low  = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error,
    stars   = sig_stars(p.value),
    estimate_fmt = paste0(round(estimate, 1), stars),
    ci = paste0("[", round(ci_low, 1), ", ", round(ci_high, 1), "]")
  ) %>%
  select(outcome, term = term_label, estimate_fmt, ci, p.value)

write_csv(t2a, file.path(out_dir, "table2a_its_segmented.csv"))
message("  Done: table2a_its_segmented.csv")


# ── Table 5: Cost-Benefit Summary ─────────────────────────────────────────────

message("Regenerating table5_cost_benefit.csv...")

# Load regime panel for baseline shooting mean
pre_panel <- panel %>%
  filter(month_date >= as.Date("2018-01-01"), month_date < INTERVENTION)
pre_shooting_mean <- mean(pre_panel$shooting_incidents, na.rm = TRUE)
pre_robbery_mean  <- mean(pre_panel$robbery_count,     na.rm = TRUE)

# Pull values from cba_sum
# Guard each string-keyed lookup: a silent component-label mismatch (e.g. after
# editing 07_cba.R's component text without updating this file) would otherwise
# return character(0)/NA here and fail confusingly deep in tibble() construction.
pull_component <- function(label) {
  v <- cba_sum %>% filter(component == label) %>% pull(value)
  if (length(v) != 1) {
    stop(sprintf("cba_summary.csv component not found (or not unique): '%s'", label))
  }
  v
}
excess_crashes     <- pull_component("Excess pursuit crashes") %>% as.numeric()
est_fatalities     <- pull_component("Estimated fatalities (central, 1%)") %>% as.numeric()
crash_cost_str     <- pull_component("Total crash costs (central)")
shooting_reduction <- pull_component("Shooting reduction (ITS estimate)") %>% as.numeric()
robbery_change     <- pull_component("Robbery change (ITS estimate)") %>% as.numeric()
shooting_benefit   <- pull_component("Shooting benefit (100% attribution)")
breakeven_str      <- pull_component("Breakeven causal share")

# N-2 fix: pull robbery benefit at 100% attribution from cba_attr (matches total_benefit in cba_net_assessment)
robbery_benefit_val <- cba_attr %>%
  filter(abs(causal_share - 1) < 1e-6) %>%
  pull(robbery_benefit) %>%
  as.numeric()
total_benefit_val <- cba_attr %>%
  filter(abs(causal_share - 1) < 1e-6) %>%
  pull(total_benefit) %>%
  as.numeric()
breakeven_pct      <- as.numeric(gsub("%", "", breakeven_str)) / 100

# Compute shooting breakeven count
post_panel <- panel %>%
  filter(month_date >= INTERVENTION, month_date <= as.Date("2025-12-01"))
n_post_months <- nrow(post_panel)

be_shootings_total  <- breakeven_pct * abs(shooting_reduction)
be_shootings_per_mo <- be_shootings_total / n_post_months
be_shootings_pct    <- be_shootings_per_mo / pre_shooting_mean * 100

be_robberies_total  <- breakeven_pct * abs(min(0, robbery_change))
be_robberies_per_mo <- be_robberies_total / n_post_months
be_robberies_pct    <- be_robberies_per_mo / pre_robbery_mean * 100

# Build table5
t5 <- tibble(
  component = c(
    "COSTS",
    "Excess pursuit crashes",
    "Estimated fatalities (1.0% of crashes, central scenario)",
    "Fatality cost (at VSL)",
    "Estimated injuries (35% of crashes, 1.5 per crash)",
    "Injury cost (blended serious/minor)",
    "Property damage",
    "TOTAL CRASH COSTS",
    NA,
    "BENEFITS",
    "Monthly robbery change (ITS counterfactual residual)",
    "Monthly shooting change (ITS counterfactual residual)",
    "Total robbery benefit (ITS upper bound, 100% attribution)",
    "Total shooting benefit (ITS upper bound, 100% attribution)",
    "TOTAL CRIME BENEFITS (100% attribution)",
    NA,
    "NET ASSESSMENT",
    "Net cost (costs - benefits)",
    "Benefit-cost ratio",
    NA,
    "BREAK-EVEN REQUIREMENTS",
    "Robberies prevented needed to break even",
    "Shootings prevented needed to break even",
    "Monthly robbery reduction needed (as % of baseline)",
    "Monthly shooting reduction needed (as % of baseline)"
  ),
  value = c(
    NA,
    round(excess_crashes, 0),
    round(excess_crashes * 0.010, 1),                                              # N-1: central 1.0% rate
    paste0("$", format(round(excess_crashes * 0.010 * 12.5e6), big.mark = ",")),  # N-1: central fatality cost
    round(excess_crashes * 0.35 * 1.5, 0),
    paste0("$", format(round(excess_crashes * 0.35 * 1.5 * 200000), big.mark = ",")),
    paste0("$", format(round(excess_crashes * 20000), big.mark = ",")),
    crash_cost_str,
    NA,
    NA,
    round(robbery_change / n_post_months, 1),                                      # N-2: ITS monthly residual, not raw post-pre
    round(mean(post_panel$shooting_incidents, na.rm = TRUE) - pre_shooting_mean, 1),
    paste0("$", format(round(robbery_benefit_val), big.mark = ",")),               # N-2: ITS robbery benefit at 100%
    shooting_benefit,
    paste0("$", format(round(total_benefit_val), big.mark = ",")),                 # N-2: total = shooting + robbery
    NA,
    NA,
    cba_net %>% filter(scenario == "central") %>%
      mutate(v = paste0("$", format(round(net), big.mark = ","))) %>% pull(v),
    round(cba_net %>% filter(scenario == "central") %>% pull(benefit_cost_ratio), 3),
    NA,
    NA,
    round(be_robberies_total, 0),
    round(be_shootings_total, 1),
    paste0(round(be_robberies_pct, 1), "%"),
    paste0(round(be_shootings_pct, 1), "%")
  )
)

write_csv(t5, file.path(out_dir, "table5_cost_benefit.csv"))
message("  Done: table5_cost_benefit.csv")
message("    Breakeven shootings: ", round(be_shootings_total, 0))
message("    Breakeven as % of baseline: ", round(be_shootings_pct, 1), "%")


# ── Table 5b: Net Assessment by Scenario ──────────────────────────────────────

message("Regenerating table5b_net_assessment.csv...")

t5b <- cba_net %>%
  mutate(
    scenario = str_to_title(scenario),
    breakeven_attribution_share = paste0(round(total_crash_cost / total_benefit * 100, 1), "%")
  ) %>%
  select(scenario, total_crash_cost, breakeven_attribution_share, benefit_cost_ratio)

write_csv(t5b, file.path(out_dir, "table5b_net_assessment.csv"))
message("  Done: table5b_net_assessment.csv")


# ── Table 7: Regime Outcomes ───────────────────────────────────────────────────

message("Regenerating table7_regime_outcomes.csv...")

regimes <- panel %>%
  filter(month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) %>%  # right-censor: pursuit CFS data trimmed at Dec 2025
  mutate(
    regime = case_when(
      month_date < INTERVENTION ~ "Pre-escalation (Jan 2018\u2013Sep 2022)",
      month_date < MADDREY      ~ "Escalation (Oct 2022\u2013Jul 2023)",
      month_date < TISCH        ~ "Post-Maddrey (Aug 2023\u2013Jan 2025)",
      TRUE                      ~ "Post-Tisch (Feb 2025\u2013Dec 2025)"
    ),
    regime = factor(regime, levels = c(
      "Pre-escalation (Jan 2018\u2013Sep 2022)",
      "Escalation (Oct 2022\u2013Jul 2023)",
      "Post-Maddrey (Aug 2023\u2013Jan 2025)",
      "Post-Tisch (Feb 2025\u2013Dec 2025)"
    ))
  ) %>%
  group_by(regime) %>%
  summarise(
    months         = n(),
    mean_pursuits  = round(mean(pursuit_events,     na.rm = TRUE), 1),
    sd_pursuits    = round(sd(pursuit_events,       na.rm = TRUE), 1),
    mean_crashes   = round(mean(pursuit_crashes,    na.rm = TRUE), 1),
    crash_rate     = paste0(round(sum(pursuit_crashes, na.rm = TRUE) / pmax(sum(pursuit_events, na.rm = TRUE), 0.001) * 100, 1), "%"),
    mean_robberies = round(mean(robbery_count,      na.rm = TRUE), 1),
    mean_shootings = round(mean(shooting_incidents, na.rm = TRUE), 1),
    mean_gun_events = round(mean(total_gun_events,  na.rm = TRUE), 1),
    .groups        = "drop"
  )

write_csv(regimes, file.path(out_dir, "table7_regime_outcomes.csv"))
message("  Done: table7_regime_outcomes.csv")
message("  Pre-escalation pursuit mean: ", regimes$mean_pursuits[regimes$regime == "Pre-escalation (Jan 2018\u2013Sep 2022)"])
message("  Escalation pursuit mean: ", regimes$mean_pursuits[regimes$regime == "Escalation (Oct 2022\u2013Jul 2023)"])
message("  Post-Maddrey pursuit mean: ", regimes$mean_pursuits[regimes$regime == "Post-Maddrey (Aug 2023\u2013Jan 2025)"])


# ── Table 10: Crash Ratio ──────────────────────────────────────────────────────

message("Regenerating table10_crash_ratio.csv...")

# Check what table10 looks like
t10_old <- tryCatch(
  read_csv(file.path(out_dir, "table10_crash_ratio.csv"), show_col_types = FALSE),
  error = function(e) NULL
)

if (!is.null(t10_old)) {
  message("  Old table10 structure: ", paste(names(t10_old), collapse = ", "))
}

t10 <- panel %>%
  filter(month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) %>%
  mutate(
    year  = year(month_date),
    # crash_rate per pursuit (capped at 1)
    crash_per_pursuit = pursuit_crashes / pmax(pursuit_events, 0.001)
  ) %>%
  group_by(year) %>%
  summarise(
    total_pursuits   = sum(pursuit_events,  na.rm = TRUE),
    total_crashes    = sum(pursuit_crashes, na.rm = TRUE),
    crash_rate       = round(total_crashes / pmax(total_pursuits, 0.001) * 100, 1),
    .groups          = "drop"
  )

write_csv(t10, file.path(out_dir, "table10_crash_ratio.csv"))
message("  Done: table10_crash_ratio.csv")


# ── Table 9: Tisch Reversal Percent Changes ───────────────────────────────────

message("Regenerating table9_tisch_reversal.csv...")

# Source: output/event_results/tisch_reversal_pct_changes.csv (from script 06)
# CRITICAL: Use the event_results source (right-censored at Dec 2025) rather
# than recomputing from panel — the comparison window is Oct 2022–Jan 2025
# (pre) vs Feb 2025–Dec 2025 (post), which script 06 enforces via TISCH date.

tisch_src <- tryCatch(
  read_csv(here("output", "event_results", "tisch_reversal_pct_changes.csv"),
           show_col_types = FALSE),
  error = function(e) {
    message("  WARNING: tisch_reversal_pct_changes.csv not found — table9 not regenerated")
    NULL
  }
)

if (!is.null(tisch_src)) {
  outcome_labels <- c(
    pursuits    = "Pursuits",
    crashes     = "Crashes",
    robberies   = "Robberies",
    shootings   = "Shootings",
    gun_violence = "Gun Violence"
  )

  deterrence <- tibble(
    outcome = names(outcome_labels),
    deterrence_prediction = c(
      "Decrease (mechanical)", "Decrease (mechanical)",
      "Increase (if deterrence real)", "Increase (if deterrence real)",
      "Increase (if deterrence real)"
    )
  )

  t9 <- tisch_src %>%
    left_join(deterrence, by = "outcome") %>%
    mutate(
      outcome          = recode(outcome, !!!outcome_labels),
      pre_tisch_fmt    = round(pre_mean,  1),
      post_tisch_fmt   = round(post_mean, 1),
      pct_change_fmt   = paste0(round(pct_change, 1), "%"),
      actual_direction = if_else(pct_change < 0, "Decrease", "Increase"),
      consistent_with_deterrence = case_when(
        deterrence_prediction == "Decrease (mechanical)" ~ "—",
        actual_direction == "Decrease"                   ~ "No",
        TRUE                                             ~ "Yes"
      )
    ) %>%
    select(outcome,
           pre_tisch_mean    = pre_mean,
           post_tisch_mean   = post_mean,
           pct_change,
           pre_tisch_fmt,
           post_tisch_fmt,
           pct_change_fmt,
           deterrence_prediction,
           actual_direction,
           consistent_with_deterrence)

  write_csv(t9, file.path(out_dir, "table9_tisch_reversal.csv"))
  message("  Done: table9_tisch_reversal.csv")
  message("  Post-Tisch pursuit mean: ", round(tisch_src$post_mean[tisch_src$outcome=="pursuits"], 1))
  message("  Post-Tisch shooting mean: ", round(tisch_src$post_mean[tisch_src$outcome=="shootings"], 1))
}


# ── Tables A2–A5: DDD Event Study Coefficient Tables ──────────────────────────
# These Appendix A tables are generated from pct_ddd_models.rds (script 17).
# Each table has one row per event-study period with estimate, SE, and 95% CI.

message("Regenerating Appendix A DDD event study tables...")

ddd_es_files <- list(
  list(file = "pct_ddd_es_shooting.csv",    out = "table_a2_ddd_shootings.csv",    label = "Shooting Incidents"),
  list(file = "pct_ddd_es_gun_violence.csv", out = "table_a3_ddd_gun_violence.csv", label = "Gun Violence"),
  list(file = "pct_ddd_es_robbery.csv",     out = "table_a4_ddd_robberies.csv",    label = "Robbery"),
  list(file = "pct_ddd_es_crashes.csv",     out = "table_a5_ddd_crashes.csv",      label = "Pursuit Crashes")
)

fmt_ci <- function(lo, hi) paste0("[", round(lo, 3), ", ", round(hi, 3), "]")

for (item in ddd_es_files) {
  src_path <- here("output", "precinct_did", item$file)
  out_path <- file.path(out_dir, item$out)

  if (!file.exists(src_path)) {
    message("  WARNING: ", item$file, " not found — skipping ", item$out)
    next
  }

  # Quarter label lookup: bin -1 = Jul–Sep 2022 (reference; not in output),
  # bin 0 = Oct–Dec 2022, bin 9 = Jan 2025+ (Tisch reversal, lumped)
  bin_labels <- c(
    "-8" = "Oct–Dec 2020", "-7" = "Jan–Mar 2021",
    "-6" = "Apr–Jun 2021", "-5" = "Jul–Sep 2021",
    "-4" = "Oct–Dec 2021", "-3" = "Jan–Mar 2022",
    "-2" = "Apr–Jun 2022", "-1" = "Jul–Sep 2022 (ref)",
    "0"  = "Oct–Dec 2022", "1"  = "Jan–Mar 2023",
    "2"  = "Apr–Jun 2023", "3"  = "Jul–Sep 2023",
    "4"  = "Oct–Dec 2023", "5"  = "Jan–Mar 2024",
    "6"  = "Apr–Jun 2024", "7"  = "Jul–Sep 2024",
    "8"  = "Oct–Dec 2024", "9"  = "Jan 2025+"  # Tisch = Feb 2025; Jan 2025 also in this bin
  )

  es_df <- read_csv(src_path, show_col_types = FALSE) %>%
    mutate(
      period       = if_else(time_bin < 0, "Pre", "Post"),
      quarter      = bin_labels[as.character(time_bin)],
      ci           = fmt_ci(conf_low, conf_high),
      sig          = case_when(
        conf_low > 0 | conf_high < 0 ~ "*",
        TRUE ~ ""
      ),
      estimate_fmt = paste0(round(estimate, 3), sig)
    ) %>%
    select(
      `Bin`       = time_bin,
      `Quarter`   = quarter,
      Period      = period,
      `Estimate`  = estimate_fmt,
      `Std. Error` = std_error,
      `95% CI`    = ci
    )

  write_csv(es_df, out_path)
  message("  Done: ", item$out, " (", nrow(es_df), " rows)")
}


# ── Done ───────────────────────────────────────────────────────────────────────

message("\nAll publication tables regenerated.\n")
