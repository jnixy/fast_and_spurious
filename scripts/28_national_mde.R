# ==============================================================================
# Vehicle Pursuit Policy Analysis — Script 28: National Comparison —
#   Minimum Detectable Effect / Confidence Intervals in Percentage Terms
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   JQC conditional-acceptance Editor Comment 1(b): the national comparison
#   returns a null, so the Editor asks how large an effect it can rule out,
#   expressed as a percentage of the base rate rather than as a raw rate per
#   100,000. This script assembles one table that, for each national estimate,
#   reports:
#     - the NYC pre-escalation base rate (per 100,000),
#     - the point estimate and its 95% bootstrap CI, in level and in % of base,
#     - a fit-noise minimum detectable effect (2 x pre-treatment RMSPE), the
#       smallest sustained gap the synthetic-control fit could distinguish from
#       pre-period prediction error, again in % of base.
#   It does not run any new model — it reads the fitted objects from scripts
#   18 (monthly primary), 29 (annual shooting + robbery), and the retained
#   borough-level national DiD (script 09) for context.
#
# Inputs:
#   - output/gsynth_results/gsynth_att.csv
#   - output/gsynth_results/gsynth_period_effects_clean.csv
#   - output/gsynth_results/gsynth_annual_att.csv
#   - output/national_did/did_shooting_twfe.csv
#   - output/national_did/did_robbery_twfe.csv
#   - output/national_did/shooting_annual_did.csv
#   - output/national_did/combined_annual_panel.csv
#   - output/publication_tables/table7_regime_outcomes.csv   (monthly-count anchor)
#
# Outputs (saved to output/gsynth_results/):
#   - national_mde_pct.csv  -- one row per national estimate: base rate, ATT,
#     ATT %, 95% CI (level and %), RMSPE-based MDE %
#
# Runtime: < 30 sec
# ==============================================================================

# ── 0. Setup ──────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
})

message("=== Script 28: National comparison — CIs in percentage terms ===")

results_dir <- here("output", "gsynth_results")

rd <- function(...) read_csv(here(...), show_col_types = FALSE)

# ── 1. NYC base rates (per 100,000) ───────────────────────────────────────────

# Monthly primary gsynth: NYC pre-escalation mean of the annualized GVA shooting
# rate (rel_time <= 0 is the pre-treatment window, per script 18).
gper <- rd("output", "gsynth_results", "gsynth_period_effects_clean.csv")
base_shoot_monthly <- mean(gper$Y_actual[gper$rel_time <= 0])

# Annual gsynth base rates are carried in the script-29 summary.
gann <- rd("output", "gsynth_results", "gsynth_annual_att.csv")
base_shoot_annual <- gann$nyc_base_rate[gann$outcome == "Shootings"]
base_rob_annual   <- gann$nyc_base_rate[gann$outcome == "Robbery"]

# ── 2. Assemble the estimate rows ─────────────────────────────────────────────

gatt <- rd("output", "gsynth_results", "gsynth_att.csv")
g_clean <- gatt %>% filter(str_detect(donor_pool, "SUTVA-clean"))

g_ann <- function(o, col) gann[[col]][gann$outcome == o]

rows <- tribble(
  ~outcome,    ~estimator,                  ~panel,    ~base_rate,          ~att,             ~ci_lower,             ~ci_upper,             ~pre_rmspe,               ~note,
  "Shootings", "gsynth (primary)",          "monthly", base_shoot_monthly,  g_clean$att,      g_clean$ci_lower,     g_clean$ci_upper,      g_clean$pre_rmspe,       "GVA shooting rate; parallel pre-trend; placebo-in-space primary inference",
  "Shootings", "gsynth (bridge)",           "annual",  base_shoot_annual,   g_ann("Shootings","att"), g_ann("Shootings","ci_lower"), g_ann("Shootings","ci_upper"), g_ann("Shootings","pre_rmspe"), "annual aggregation of the same GVA series; CV selects zero factors",
  "Robbery",   "gsynth",                    "annual",  base_rob_annual,     g_ann("Robbery","att"),   g_ann("Robbery","ci_lower"),   g_ann("Robbery","ci_upper"),   g_ann("Robbery","pre_rmspe"),   "Kaplan UCR robbery rate; NYC robbery diverges upward pre-treatment"
)

mde <- rows %>%
  mutate(
    att_pct        = att      / base_rate * 100,
    ci_lower_pct   = ci_lower / base_rate * 100,
    ci_upper_pct   = ci_upper / base_rate * 100,
    ci_mde_pct     = pmax(abs(ci_lower_pct), abs(ci_upper_pct)),
    rmspe_mde      = 2 * pre_rmspe,
    rmspe_mde_pct  = rmspe_mde / base_rate * 100
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

write_csv(mde, file.path(results_dir, "national_mde_pct.csv"))
message("Saved: national_mde_pct.csv")

# ── 3. Console summary ────────────────────────────────────────────────────────

message("\n=== National estimates in percentage terms ===")
for (i in seq_len(nrow(mde))) {
  r <- mde[i, ]
  message(sprintf(
    "%-9s | %-28s | base %.1f/100k | ATT %+.1f (%+.0f%%) | 95%% CI [%.1f, %.1f] = [%.0f%%, %.0f%%]%s",
    r$outcome, r$estimator, r$base_rate, r$att, r$att_pct,
    r$ci_lower, r$ci_upper, r$ci_lower_pct, r$ci_upper_pct,
    if (!is.na(r$rmspe_mde_pct))
      sprintf(" | fit-noise MDE +/-%.0f%%", r$rmspe_mde_pct) else ""))
}
message("\nScript complete.")
