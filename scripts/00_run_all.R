# ==============================================================================
# Hall & Nix — NYPD Vehicle Pursuit Analysis: Master Pipeline Runner
# ==============================================================================
#
# Purpose:
#   Executes all active analysis scripts in dependency order. Produces all
#   outputs needed to render manuscript/manuscript.qmd.
#
# Usage (from project root):
#   Rscript scripts/00_run_all.R
#   # or in RStudio:
#   source("scripts/00_run_all.R")
#
# Prerequisites:
#   - Raw data files placed in data/ (see CLAUDE.md for file list)
#   - Required packages installed (tidyverse, fixest, sandwich, lmtest,
#     broom, here, janitor, tidysynth, fwildclusterboot, sf, scales, gsynth,
#     synthdid — the last via remotes::install_github("synth-inference/synthdid"))
#   - HIDDEN PREREQUISITE: script 19 (Phase 6) requires
#     output/gsynth_results/gsynth_robustness.rds, produced by script 18,
#     which this runner does NOT invoke (see rationale below). On a truly
#     fresh checkout with no prior gsynth run, this file will run scripts
#     01-26 successfully and then fail fast at script 19 with a clear
#     "gsynth_robustness.rds not found. Run Script 18 first." error — run
#     `Rscript scripts/18_gsynth.R` once beforehand (takes several minutes)
#     to satisfy this dependency.
#
# Active pipeline runtime: ~20–40 minutes depending on hardware, plus
# ~10–20 more minutes for the Phase 6 R&R robustness extensions.
# (Script 17 precinct DDD and script 16 Sun-Abraham are the slow steps in
# Phases 1–5; script 19's 75-city permutation is the slow step in Phase 6.)
#
# Dependency order:
#   01 → 03 → 06 → 07 → 08 → 14 (ITS + dose-response) → 17 → 15 → 16
#   → 00_regenerate_pub_tables → 20/21/22/23/24/25/26_robbery/19
#   CRITICAL: 17 must precede 15 (15 reads pct_ddd_models.rds from 17)
#             08 must precede 14 (14 reads borough panels from 08)
#             19 requires output/gsynth_results/gsynth_robustness.rds from
#             script 18 (unwired here — gsynth's donor panel is unaffected by
#             the NYC-side Dec-2025 extension, so the existing RDS is current;
#             re-run script 18 manually only if the AV donor data changes)
# ==============================================================================

suppressPackageStartupMessages(library(here))

# Helper: run a script with timing, progress messages, and error capture -------

run_script <- function(path) {
  message(strrep("-", 60))
  message("Running: ", path)
  message("Time:    ", format(Sys.time(), "%H:%M:%S"))
  t0 <- proc.time()
  tryCatch(
    # Isolate each script's top-level variables in their own environment so a
    # generic name (e.g. script 19's own `t_start`) can't leak into and
    # clobber this runner's own bookkeeping variables in the global env.
    source(here(path), local = new.env(parent = globalenv())),
    error = function(e) {
      message("\nERROR in ", path, ":")
      message(conditionMessage(e))
      stop(sprintf("Script failed: %s", path), call. = FALSE)
    }
  )
  elapsed <- round((proc.time() - t0)[["elapsed"]] / 60, 1)
  message("\nCompleted: ", path, " (", elapsed, " min) at ",
          format(Sys.time(), "%H:%M:%S"), "\n")
}

t_start <- proc.time()

message(strrep("=", 60))
message("HALL & NIX — NYPD VEHICLE PURSUIT ANALYSIS")
message("Started: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message(strrep("=", 60), "\n")

# ── Phase 1: Core data preparation ────────────────────────────────────────────
run_script("scripts/01_load_clean.R")           # builds monthly_panel (prerequisite for all)

# Optional (~10 min, exploratory only — run manually if needed):
# run_script("scripts/02_eda.R")

# ── Phase 2: Primary NYC city-level analysis ───────────────────────────────────
run_script("scripts/03-its_analysis.R")          # ITS segmented regression (primary method)
run_script("scripts/06_event_study.R")           # multi-period regimes + Tisch falsification
run_script("scripts/07_cba.R")                   # cost-benefit analysis

# ── Phase 3: Borough-level analysis ────────────────────────────────────────────
run_script("scripts/08_national_data_prep.R")    # borough monthly panels (required by 14, 10)
run_script("scripts/14_borough_its.R")           # borough-level ITS
run_script("scripts/14_borough_dose_response.R") # borough dose-response robustness
run_script("scripts/10_national_scm.R")          # borough vanilla SCM; robbery result -> manuscript Table C2

# ── Phase 4: Precinct-level analysis ───────────────────────────────────────────
# CRITICAL ORDER: 17 must precede 15 (15 reads pct_ddd_models.rds from 17)
run_script("scripts/17_precinct_ddd.R")          # precinct DDD — produces pct_ddd_models.rds
run_script("scripts/15_precinct_twfe.R")         # precinct TWFE + map (reads pct_ddd_models.rds)
run_script("scripts/16_precinct_staggered.R")    # Sun-Abraham staggered adoption robustness

# ── Phase 5: Refresh publication tables ────────────────────────────────────────
run_script("scripts/00_regenerate_pub_tables.R") # regenerates all manuscript-ready tables

# ── Phase 6: R&R robustness extensions ─────────────────────────────────────────
# Addresses Reviewer 1's pre-trend concerns for the precinct DiD (shootings,
# robberies) plus the Tisch re-restriction reverse-experiment and collision
# elasticity. All read from Phase 1–4 outputs; no ordering dependency among
# themselves except 19, which requires script 18's cached RDS (see header note).
run_script("scripts/20_tisch_its.R")             # Tisch re-restriction ITS
run_script("scripts/21_pursuit_dose_did.R")      # precinct dose-response DiD
run_script("scripts/22_enforcement_controls.R")  # enforcement-intensity controls
run_script("scripts/23_synthdid.R")              # synthetic DiD (requires synthdid pkg)
run_script("scripts/24_collision_elasticity.R")  # collision-pursuit elasticity
run_script("scripts/25_shooting_detrend.R")      # group-detrended DiD robustness
run_script("scripts/26_robbery_sensitivity.R")   # HonestDiD breakdown value
run_script("scripts/19_gsynth_permutation.R")    # gsynth placebo-in-space permutation

# ── Phase 6b: National comparison — robbery gsynth + effect-size bounds ─────────
# JQC conditional-acceptance Editor Comment 1. 29 needs the borough annual panel
# from script 08; 28 reads the fitted objects from scripts 18, 29 and the
# retained borough DiD. 28 also depends on script 18's refreshed gsynth_att.csv
# (est.avg CI columns), so run script 18 beforehand if the AV donor data changed.
run_script("scripts/29_gsynth_annual.R")         # annual gsynth: shootings + robbery
run_script("scripts/28_national_mde.R")          # national CIs in percentage terms

# ── Summary ────────────────────────────────────────────────────────────────────
total_min <- round((proc.time() - t_start)[["elapsed"]] / 60, 1)

message(strrep("=", 60))
message("All active scripts completed successfully.")
message("Total elapsed: ", total_min, " minutes")
message("Finished: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
message(strrep("=", 60))
message("\nNext step: quarto render manuscript/manuscript.qmd")

# ==============================================================================
# Legacy scripts (not in main manuscript — retained for reproducibility)
# ==============================================================================
#
# Within-NYS comparisons (parallel trends violated / degenerate):
# run_script("scripts/04_did_analysis.R")
# run_script("scripts/05_scm_analysis.R")
#
# National DiD (removed from manuscript after restructuring):
# run_script("scripts/09_national_did.R")
#
# (script 10_national_scm.R moved back into Phase 3 above on 2026-07-30 —
# its robbery output now feeds manuscript Table C2, Appendix C)
#
# AmericanViolence.org robustness (appendices removed):
# run_script("scripts/11_av_data_prep.R")
# run_script("scripts/12_av_did.R")
# run_script("scripts/13_av_scm.R")
