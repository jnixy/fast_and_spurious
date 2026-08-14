# ==============================================================================
# Vehicle Pursuits Study: Enforcement Intensity Controls
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Add concurrent enforcement intensity controls (B summonses and
#   violation/misdemeanor arrests) to the precinct DiD from script 17.
#   If the shooting DiD attenuates after controlling for proactive
#   enforcement intensity, the script 17 result may reflect broader
#   enforcement changes rather than pursuit-specific deterrence.
#
#   Controls:
#     - b_summons_count:  monthly precinct-level moving violation B summonses
#                         (traffic tickets, a direct measure of traffic enforcement)
#     - vm_arrests_count: monthly precinct-level violation + misdemeanor arrests
#                         (proactive enforcement; excludes felonies to avoid
#                         capturing reactive/investigative arrests)
#
# Inputs:
#   - data/Moving_Violation_B_Summons_(Historic)_20260628.csv
#   - data/NYPD_Arrests_Data_(Historic)_20260628.csv
#   - output/precinct_did/pct_monthly_shootings.csv
#   - output/precinct_did/pct_monthly_robberies.csv
#   - output/precinct_did/pct_monthly_collisions.csv
#   - output/precinct_did/pct_monthly_gun_violence.csv
#
# Outputs:
#   - output/precinct_did/enforcement_controls_panel.csv
#   - output/precinct_did/enforcement_controls_results.csv
#   - output/precinct_did/enforcement_controls_comparison.csv
#
# Runtime: ~2 min (CSV loading is slow)
# ==============================================================================

library(tidyverse)
library(lubridate)
library(fixest)
library(here)
library(janitor)

set.seed(20260628)


# 0. Setup ---------------------------------------------------------------------

dir.create(here("output", "precinct_did"), showWarnings = FALSE, recursive = TRUE)
pct_dir  <- here("output", "precinct_did")
plot_dir <- here("output", "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

INTERVENTION <- as.Date("2022-10-01")

message("Script 22: Enforcement Intensity Controls")
message("Working directory: ", here())

COL_SHOOTING  <- "#6A0572"
COL_ROBBERY   <- "#003049"
COL_COLLISION <- "#F77F00"
COL_GUNVIOL   <- "#1B998B"
COL_PURSUIT   <- "#D62828"


# 1. Load and process B summonses ==============================================

message("\n[1] Loading B summonses...")

summons_raw <- read_csv(
  here("data", "Moving_Violation_B_Summons_(Historic)_20260628.csv"),
  show_col_types = FALSE
) |>
  janitor::clean_names()

message("  Raw summons rows: ", nrow(summons_raw))

# Parse date, extract precinct, aggregate to precinct-month
summons_pct <- summons_raw |>
  mutate(
    date      = mdy(violation_date),
    month_date = floor_date(date, "month"),
    pct       = as.integer(rpt_owning_cmd)
  ) |>
  filter(!is.na(pct), !is.na(month_date)) |>
  # Recode Precinct 116 as 105 (same as script 17)
  mutate(pct = if_else(pct == 116L, 105L, pct)) |>
  group_by(pct, month_date) |>
  summarise(b_summons_count = sum(count_evnt_key, na.rm = TRUE), .groups = "drop")

message("  Summons precinct-months: ", nrow(summons_pct))
message("  Date range: ", min(summons_pct$month_date), " to ", max(summons_pct$month_date))
message("  Precincts: ", n_distinct(summons_pct$pct))


# 2. Load and process arrests (violations + misdemeanors only) =================

message("\n[2] Loading arrests (violations + misdemeanors only)...")

arrests_raw <- read_csv(
  here("data", "NYPD_Arrests_Data_(Historic)_20260628.csv"),
  show_col_types = FALSE
) |>
  janitor::clean_names()

message("  Raw arrest rows: ", nrow(arrests_raw))

# Filter to violations (V) and misdemeanors (M) only — exclude felonies (F)
# and infractions (I) to isolate proactive enforcement
arrests_pct <- arrests_raw |>
  filter(law_cat_cd %in% c("M", "V")) |>
  mutate(
    date      = mdy(arrest_date),
    month_date = floor_date(date, "month"),
    pct       = as.integer(arrest_precinct)
  ) |>
  filter(!is.na(pct), !is.na(month_date)) |>
  # Recode Precinct 116 as 105
  mutate(pct = if_else(pct == 116L, 105L, pct)) |>
  group_by(pct, month_date) |>
  summarise(vm_arrests_count = sum(count_arrest_key, na.rm = TRUE), .groups = "drop")

message("  V+M arrest precinct-months: ", nrow(arrests_pct))
message("  Date range: ", min(arrests_pct$month_date), " to ", max(arrests_pct$month_date))
message("  Precincts: ", n_distinct(arrests_pct$pct))

# Quick category check
cat_counts <- arrests_raw |>
  count(law_cat_cd, sort = TRUE)
message("  Arrest categories in raw data:")
message(paste(
  sprintf("    %s: %s", cat_counts$law_cat_cd, format(cat_counts$n, big.mark = ",")),
  collapse = "\n"
))


# 3. Build panel (same as script 17, plus enforcement controls) ================

message("\n[3] Building panel with enforcement controls...")

# Load outcome panels
shootings_pct <- read_csv(here("output", "precinct_did", "pct_monthly_shootings.csv"),
                           show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(month_date = as.Date(month_date))

robberies_pct <- read_csv(here("output", "precinct_did", "pct_monthly_robberies.csv"),
                           show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(month_date = as.Date(month_date))

gun_violence_pct <- read_csv(here("output", "precinct_did", "pct_monthly_gun_violence.csv"),
                              show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(month_date = as.Date(month_date))

collisions_pct <- read_csv(here("output", "precinct_did", "pct_monthly_collisions.csv"),
                            show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(month_date = as.Date(month_date))

# Build combined panel
panel_raw <- shootings_pct |>
  full_join(robberies_pct, by = c("pct", "month_date")) |>
  full_join(
    gun_violence_pct |> select(pct, month_date, gun_violence_count),
    by = c("pct", "month_date")
  ) |>
  full_join(
    collisions_pct |> select(pct, month_date, collision_count),
    by = c("pct", "month_date")
  ) |>
  replace_na(list(shooting_incidents = 0L, robbery_count = 0L,
                  gun_violence_count = 0L, collision_count = 0L)) |>
  filter(pct >= 1, pct <= 123,
         month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) |>
  group_by(pct) |>
  filter(any(shooting_incidents > 0) | any(robbery_count > 0)) |>
  ungroup() |>
  arrange(pct, month_date)

# Recode Precinct 116 as 105 and re-aggregate
panel_raw <- panel_raw |>
  mutate(pct = if_else(pct == 116L, 105L, pct)) |>
  group_by(pct, month_date) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
  arrange(pct, month_date)

# Pre-treatment crime groupings (same as script 17)
pre_crime_shoot <- panel_raw |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_shoot_pre = mean(shooting_incidents, na.rm = TRUE), .groups = "drop") |>
  mutate(high_crime_shoot = as.integer(mean_shoot_pre >= median(mean_shoot_pre)))

pre_crime_rob <- panel_raw |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_rob_pre = mean(robbery_count, na.rm = TRUE), .groups = "drop") |>
  mutate(high_crime_rob = as.integer(mean_rob_pre >= median(mean_rob_pre)))

pre_crime_gunviol <- panel_raw |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_gv_pre = mean(gun_violence_count, na.rm = TRUE), .groups = "drop") |>
  mutate(high_crime_gunviol = as.integer(mean_gv_pre >= median(mean_gv_pre)))

pre_crash <- panel_raw |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_crash_pre = mean(collision_count, na.rm = TRUE), .groups = "drop") |>
  mutate(high_crash = as.integer(mean_crash_pre >= median(mean_crash_pre)))

# Build analysis panel with crime groupings + enforcement controls
panel <- panel_raw |>
  left_join(pre_crime_shoot   |> select(pct, high_crime_shoot),   by = "pct") |>
  left_join(pre_crime_rob     |> select(pct, high_crime_rob),     by = "pct") |>
  left_join(pre_crime_gunviol |> select(pct, high_crime_gunviol), by = "pct") |>
  left_join(pre_crash         |> select(pct, high_crash),         by = "pct") |>
  # Join enforcement controls
  left_join(summons_pct, by = c("pct", "month_date")) |>
  left_join(arrests_pct, by = c("pct", "month_date")) |>
  replace_na(list(b_summons_count = 0L, vm_arrests_count = 0L)) |>
  mutate(
    post = as.integer(month_date >= INTERVENTION)
  )

n_pcts   <- n_distinct(panel$pct)
n_months <- n_distinct(panel$month_date)
message("  Panel: ", n_pcts, " precincts x ", n_months, " months = ", nrow(panel), " obs")
message("  Enforcement control coverage:")
message("    B summons — mean: ", round(mean(panel$b_summons_count), 1),
        " | SD: ", round(sd(panel$b_summons_count), 1),
        " | zeroes: ", sum(panel$b_summons_count == 0),
        " (", round(mean(panel$b_summons_count == 0) * 100, 1), "%)")
message("    V+M arrests — mean: ", round(mean(panel$vm_arrests_count), 1),
        " | SD: ", round(sd(panel$vm_arrests_count), 1),
        " | zeroes: ", sum(panel$vm_arrests_count == 0),
        " (", round(mean(panel$vm_arrests_count == 0) * 100, 1), "%)")

# Save the panel with enforcement controls
write_csv(panel, here("output", "precinct_did", "enforcement_controls_panel.csv"))
message("  Saved: enforcement_controls_panel.csv")


# 4. DiD with and without enforcement controls =================================

message("\n[4] Estimating DiD with and without enforcement controls...")

# Helper to extract coefficient from a feols fit
extract_coef <- function(fit, outcome_label, spec_label, term_name) {
  ct <- summary(fit)$coeftable
  if (!term_name %in% rownames(ct)) {
    stop("Term '", term_name, "' not found in coeftable. Available: ",
         paste(rownames(ct), collapse = ", "))
  }
  tibble(
    outcome     = outcome_label,
    spec        = spec_label,
    term        = term_name,
    estimate    = ct[term_name, "Estimate"],
    std_error   = ct[term_name, "Std. Error"],
    t_stat      = ct[term_name, "t value"],
    p_value     = ct[term_name, "Pr(>|t|)"],
    nobs        = fit$nobs,
    n_precincts = n_pcts
  )
}

# Define outcome-grouping pairs
outcomes <- list(
  list(var = "shooting_incidents", label = "shooting_incidents",
       group = "high_crime_shoot",   term = "post:high_crime_shoot"),
  list(var = "robbery_count",      label = "robbery_count",
       group = "high_crime_rob",     term = "post:high_crime_rob"),
  list(var = "gun_violence_count", label = "gun_violence_count",
       group = "high_crime_gunviol", term = "post:high_crime_gunviol"),
  list(var = "collision_count",    label = "collision_count",
       group = "high_crash",         term = "post:high_crash")
)

results_list <- list()

for (o in outcomes) {
  message("\n  --- ", o$label, " ---")

  # (a) Baseline: no enforcement controls (replicates script 17)
  fml_base <- as.formula(paste0(o$var, " ~ post:", o$group, " | pct + month_date"))
  fit_base <- feols(fml_base, data = panel, cluster = ~pct)

  # (b) With enforcement controls
  fml_ctrl <- as.formula(paste0(
    o$var, " ~ post:", o$group, " + b_summons_count + vm_arrests_count | pct + month_date"
  ))
  fit_ctrl <- feols(fml_ctrl, data = panel, cluster = ~pct)

  results_list[[paste0(o$label, "_base")]] <- extract_coef(
    fit_base, o$label, "no_controls", o$term
  )
  results_list[[paste0(o$label, "_ctrl")]] <- extract_coef(
    fit_ctrl, o$label, "with_controls", o$term
  )

  # Also extract enforcement control coefficients from controlled model
  ct_ctrl <- summary(fit_ctrl)$coeftable
  for (ctrl_var in c("b_summons_count", "vm_arrests_count")) {
    if (ctrl_var %in% rownames(ct_ctrl)) {
      results_list[[paste0(o$label, "_", ctrl_var)]] <- tibble(
        outcome     = o$label,
        spec        = "control_coef",
        term        = ctrl_var,
        estimate    = ct_ctrl[ctrl_var, "Estimate"],
        std_error   = ct_ctrl[ctrl_var, "Std. Error"],
        t_stat      = ct_ctrl[ctrl_var, "t value"],
        p_value     = ct_ctrl[ctrl_var, "Pr(>|t|)"],
        nobs        = fit_ctrl$nobs,
        n_precincts = n_pcts
      )
    }
  }

  # Print comparison
  b_base <- summary(fit_base)$coeftable[o$term, "Estimate"]
  p_base <- summary(fit_base)$coeftable[o$term, "Pr(>|t|)"]
  b_ctrl <- summary(fit_ctrl)$coeftable[o$term, "Estimate"]
  p_ctrl <- summary(fit_ctrl)$coeftable[o$term, "Pr(>|t|)"]

  message(sprintf("    No controls:   b = %8.4f, p = %.4f", b_base, p_base))
  message(sprintf("    With controls: b = %8.4f, p = %.4f", b_ctrl, p_ctrl))
  message(sprintf("    Change: %.1f%%",
                  (b_ctrl - b_base) / abs(b_base) * 100))
}

all_results <- bind_rows(results_list)
write_csv(all_results, here("output", "precinct_did", "enforcement_controls_results.csv"))
message("\n  Saved: enforcement_controls_results.csv (", nrow(all_results), " rows)")


# 5. Comparison table (with/without controls side by side) =====================

message("\n[5] Building comparison table...")

comparison <- all_results |>
  filter(spec %in% c("no_controls", "with_controls")) |>
  select(outcome, spec, estimate, std_error, p_value) |>
  pivot_wider(
    names_from  = spec,
    values_from = c(estimate, std_error, p_value),
    names_glue  = "{spec}_{.value}"
  ) |>
  mutate(
    pct_change = (with_controls_estimate - no_controls_estimate) /
                  abs(no_controls_estimate) * 100
  )

write_csv(comparison, here("output", "precinct_did", "enforcement_controls_comparison.csv"))
message("  Saved: enforcement_controls_comparison.csv")


# 6. Summary ===================================================================

message("\n[6] Summary")
message("=" |> strrep(60))
message("\nDiD estimates with and without enforcement controls:")
message(paste(
  sprintf("  %-22s  base: b=%7.4f (p=%.4f)  ctrl: b=%7.4f (p=%.4f)  chg=%.1f%%",
          comparison$outcome,
          comparison$no_controls_estimate,
          comparison$no_controls_p_value,
          comparison$with_controls_estimate,
          comparison$with_controls_p_value,
          comparison$pct_change),
  collapse = "\n"
))

# Report enforcement control coefficients
ctrl_coefs <- all_results |> filter(spec == "control_coef")
message("\nEnforcement control coefficients (from shooting model):")
shoot_ctrls <- ctrl_coefs |> filter(outcome == "shooting_incidents")
message(paste(
  sprintf("  %-22s  b = %8.5f  SE = %8.5f  p = %.4f",
          shoot_ctrls$term, shoot_ctrls$estimate,
          shoot_ctrls$std_error, shoot_ctrls$p_value),
  collapse = "\n"
))

message("\nScript 22 complete.")
message("Outputs: ", pct_dir)
