# ==============================================================================
# NYPD Vehicle Pursuit Policy: Borough-Level Dose-Response Analysis
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Estimate the within-borough dose-response relationship between monthly
#   pursuit intensity (pursuits per 100K residents, annualized) and crime
#   outcomes (shooting incidents and robberies per 100K, annualized), using
#   borough × month panel variation. Complements the national DiD by exploiting
#   within-NYC variation in pursuit activity across boroughs over time.
#
# Inputs:
#   - output/national_did/boro_monthly_panel.csv  (from script 08)
#   - output/national_did/boro_monthly_pursuit.csv (from script 01)
#
# Outputs (saved to output/national_did/):
#   - boro_dose_response_results.csv  -- coefficients, SEs, p-values
#
# Outputs (saved to output/plots/):
#   - dose_response_scatter.png / .pdf  -- within-borough scatter (pursuit rate vs crime)
#
# Runtime: < 1 min
# ==============================================================================

library(tidyverse)
library(lubridate)
library(here)
library(fixest)
library(janitor)

message("=== BOROUGH DOSE-RESPONSE ANALYSIS ===")
message("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M"))

results_dir <- here("output", "national_did")
plots_dir   <- here("output", "plots")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plots_dir,   showWarnings = FALSE, recursive = TRUE)


# 0. Setup =====================================================================

# Project color palette (matches theme_pursuit from script 02)
COL_PURSUIT  <- "#D62828"
COL_SHOOTING <- "#003049"
COL_ROBBERY  <- "#F77F00"

ANALYSIS_START <- as.Date("2018-01-01")
ANALYSIS_END   <- as.Date("2025-12-01")

boro_pop <- tibble(
  boro       = c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island"),
  pop_2020   = c(1694251L, 2736074L, 2405464L, 1472654L, 495747L)
)


# 1. Load Data =================================================================

message("\n--- Loading data ---")

# Guard: informative error if upstream scripts have not been run
panel_path   <- here("output", "national_did", "boro_monthly_panel.csv")
pursuit_path <- here("output", "national_did", "boro_monthly_pursuit.csv")

stopifnot(
  "boro_monthly_panel.csv not found — run script 08 first"   = file.exists(panel_path),
  "boro_monthly_pursuit.csv not found — run script 01 first" = file.exists(pursuit_path)
)

boro_panel <- read_csv(panel_path, show_col_types = FALSE) |> clean_names()
message("  boro_monthly_panel: ", nrow(boro_panel), " rows")

boro_pursuit <- read_csv(pursuit_path, show_col_types = FALSE) |> clean_names()
message("  boro_monthly_pursuit: ", nrow(boro_pursuit), " rows")


# 2. Build Panel ===============================================================

message("\n--- Building dose-response panel ---")

# Spine: all borough × month combinations in the analysis window
spine <- expand_grid(
  month_date = seq.Date(ANALYSIS_START, ANALYSIS_END, by = "month"),
  boro       = c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")
)

# Join crime data (from boro_monthly_panel) and pursuit data
panel_raw <- spine %>%
  left_join(
    boro_panel %>%
      filter(month_date >= ANALYSIS_START, month_date <= ANALYSIS_END) %>%
      select(month_date, boro, shooting_incidents, robbery_count, pop_2020),
    by = c("month_date", "boro")
  ) %>%
  left_join(
    boro_pursuit %>%
      filter(month_date >= ANALYSIS_START, month_date <= ANALYSIS_END),
    by = c("month_date", "boro")
  ) %>%
  # Zero-fill only count columns — rates and population should not be NA if join succeeds
  replace_na(list(shooting_incidents = 0, robbery_count = 0, pursuit_events = 0)) %>%
  left_join(boro_pop %>% select(boro, pop_2020), by = "boro", suffix = c("", "_ref")) %>%
  mutate(
    # Use reference pop if panel value is missing (e.g., months outside panel window)
    pop_use = coalesce(pop_2020, pop_2020_ref)
  )

# Guard: every borough-month must have a population value
stopifnot(
  "Missing pop_use — borough name mismatch between boro_pop and boro_panel" =
    !any(is.na(panel_raw$pop_use))
)

panel <- panel_raw %>%
  mutate(
    # Annualized rate: monthly count scaled to per-100K-per-year (×12)
    # Consistent with shooting_rate and robbery_rate in boro_monthly_panel
    shooting_rate  = shooting_incidents / pop_use * 100000 * 12,
    robbery_rate   = robbery_count      / pop_use * 100000 * 12,
    pursuit_rate   = pursuit_events     / pop_use * 100000 * 12,
    year           = year(month_date),
    month_num      = month(month_date)
  ) %>%
  arrange(boro, month_date) %>%
  group_by(boro) %>%
  mutate(pursuit_rate_lag1 = lag(pursuit_rate, 1)) %>%
  ungroup()

message("  Panel rows: ", nrow(panel))
message("  Boroughs:   ", paste(sort(unique(panel$boro)), collapse = ", "))
message("  Window:     ", as.character(ANALYSIS_START), " to ", as.character(ANALYSIS_END))

# Verify pursuit data coverage (post-escalation)
pursuit_coverage <- panel %>%
  filter(month_date >= as.Date("2022-10-01")) %>%
  group_by(boro) %>%
  summarise(mean_pursuit_rate = round(mean(pursuit_rate, na.rm = TRUE), 1), .groups = "drop")
message("\n  Mean post-escalation pursuit rate by borough (annualized per 100K):")
message(paste(capture.output(print(as.data.frame(pursuit_coverage))), collapse = "\n"))


# 3. Dose-Response TWFE ========================================================
#
# TWFE with boro + month-year fixed effects.
# Pursuit rate is the annualized rate (monthly count × 12 / pop × 100K).
#
# SE choice: with G=5 borough clusters, cluster-robust SEs are unreliable
# (rule of thumb: ≥50 clusters for reliable asymptotic inference). HC1
# corrects only heteroskedasticity and ignores serial correlation. Monthly
# crime data exhibit significant autocorrelation. We use Newey-West HAC SEs
# with lag=6, consistent with the ITS models in script 03.
#
# Note: these results are descriptive, not causal. Endogeneity is likely —
# police pursue more aggressively in higher-crime areas and periods.
#
# Two specifications:
#   Contemporaneous: pursuit rate at t predicting crime at t
#   Lagged:          pursuit rate at t-1 predicting crime at t (robustness)

message("\n--- Estimating dose-response TWFE models ---")

# Contemporaneous
# NW(6): Newey-West HAC with lag=6, consistent with ITS convention in script 03.
# panel.id identifies the unit (boro) and time (month_date) dimensions for HAC.
mod_shoot_contemp <- feols(
  shooting_rate ~ pursuit_rate | boro + month_date,
  data     = panel,
  panel.id = ~boro + month_date,
  vcov     = NW(6)
)

mod_rob_contemp <- feols(
  robbery_rate ~ pursuit_rate | boro + month_date,
  data     = panel,
  panel.id = ~boro + month_date,
  vcov     = NW(6)
)

# Lagged (robustness)
panel_lag <- panel %>% filter(!is.na(pursuit_rate_lag1))

mod_shoot_lag <- feols(
  shooting_rate ~ pursuit_rate_lag1 | boro + month_date,
  data     = panel_lag,
  panel.id = ~boro + month_date,
  vcov     = NW(6)
)

mod_rob_lag <- feols(
  robbery_rate ~ pursuit_rate_lag1 | boro + month_date,
  data     = panel_lag,
  panel.id = ~boro + month_date,
  vcov     = NW(6)
)

message("  Contemporaneous models: done")
message("  Lagged models:          done")


# 4. Extract and Save Results ==================================================

message("\n--- Saving results ---")

extract_coef <- function(mod, outcome, spec, term) {
  tibble(
    outcome  = outcome,
    spec     = spec,
    term     = term,
    estimate = coef(mod)[term],
    std_err  = se(mod)[term],
    t_stat   = tstat(mod)[term],
    p_value  = pvalue(mod)[term],
    n_obs    = nobs(mod)
  )
}

results <- bind_rows(
  extract_coef(mod_shoot_contemp, "shooting_rate",  "contemporaneous", "pursuit_rate"),
  extract_coef(mod_rob_contemp,   "robbery_rate",   "contemporaneous", "pursuit_rate"),
  extract_coef(mod_shoot_lag,     "shooting_rate",  "lagged_t1",       "pursuit_rate_lag1"),
  extract_coef(mod_rob_lag,       "robbery_rate",   "lagged_t1",       "pursuit_rate_lag1")
)

write_csv(results, here(results_dir, "boro_dose_response_results.csv"))
message("  Saved: output/national_did/boro_dose_response_results.csv")

message("\n  Dose-response coefficients (Newey-West HAC, lag=6):")
message(paste(capture.output(
  print(as.data.frame(results %>% select(outcome, spec, estimate, std_err, p_value)))
), collapse = "\n"))


# 5. Diagnostic Figure =========================================================

message("\n--- Creating dose-response scatter ---")

# Within-borough demeaned pursuit rate and shooting rate for visualization.
# Remove boro and time means (equivalent to TWFE projection).
panel_demean <- panel %>%
  group_by(boro) %>%
  mutate(
    shoot_dm  = shooting_rate - mean(shooting_rate, na.rm = TRUE),
    pursue_dm = pursuit_rate  - mean(pursuit_rate,  na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(month_date) %>%
  mutate(
    shoot_dm  = shoot_dm  - mean(shoot_dm,  na.rm = TRUE),
    pursue_dm = pursue_dm - mean(pursue_dm, na.rm = TRUE)
  ) %>%
  ungroup()

# Note: geom_smooth(method = "lm") shows an OLS fit on the demeaned data.
# The TWFE coefficient (NW SEs) is the authoritative estimate; the scatter
# shows the conditional correlation after removing boro and time means.
p_scatter <- ggplot(panel_demean, aes(x = pursue_dm, y = shoot_dm)) +
  geom_point(alpha = 0.3, size = 1.2, color = COL_SHOOTING) +
  geom_smooth(method = "lm", se = FALSE, color = COL_PURSUIT, linewidth = 0.9) +
  labs(
    title    = "Within-Borough Dose-Response: Pursuit Rate and Shooting Rate",
    subtitle = "Borough and calendar-month means removed; OLS fit on demeaned data",
    x        = "Pursuit rate (within-unit, within-time deviation, annualized per 100K)",
    y        = "Shooting rate (within-unit, within-time deviation, annualized per 100K)",
    caption  = paste0(
      "Note: Five-borough panel, Jan 2018–Sep 2025. FEs: borough + calendar month-year.\n",
      "TWFE estimate (NW HAC, lag=6): b = ",
      round(results$estimate[results$outcome == "shooting_rate" & results$spec == "contemporaneous"], 3),
      ", p = ",
      round(results$p_value[results$outcome == "shooting_rate" & results$spec == "contemporaneous"], 3)
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 11),
    plot.subtitle    = element_text(size = 9, color = "grey40"),
    plot.caption     = element_text(size = 7, color = "grey50"),
    panel.grid.minor = element_blank()
  )

ggsave(
  here(plots_dir, "dose_response_scatter.png"),
  plot  = p_scatter,
  width = 6.5, height = 4, dpi = 300
)
ggsave(
  here(plots_dir, "dose_response_scatter.pdf"),
  plot  = p_scatter,
  width = 6.5, height = 4
)
message("  Saved: output/plots/dose_response_scatter.png")
message("  Saved: output/plots/dose_response_scatter.pdf")


# 6. Summary ===================================================================

message("\n=== DOSE-RESPONSE SUMMARY (NW HAC, lag=6) ===")
message("Contemporaneous specification (pursuit rate at t → crime at t):")
message(sprintf("  Shooting rate: b = %.4f, SE = %.4f, p = %.4f",
                results$estimate[results$outcome == "shooting_rate" & results$spec == "contemporaneous"],
                results$std_err[results$outcome  == "shooting_rate" & results$spec == "contemporaneous"],
                results$p_value[results$outcome  == "shooting_rate" & results$spec == "contemporaneous"]))
message(sprintf("  Robbery rate:  b = %.4f, SE = %.4f, p = %.4f",
                results$estimate[results$outcome == "robbery_rate"  & results$spec == "contemporaneous"],
                results$std_err[results$outcome  == "robbery_rate"  & results$spec == "contemporaneous"],
                results$p_value[results$outcome  == "robbery_rate"  & results$spec == "contemporaneous"]))
message("\nLagged specification (pursuit rate at t-1 → crime at t):")
message(sprintf("  Shooting rate: b = %.4f, SE = %.4f, p = %.4f",
                results$estimate[results$outcome == "shooting_rate" & results$spec == "lagged_t1"],
                results$std_err[results$outcome  == "shooting_rate" & results$spec == "lagged_t1"],
                results$p_value[results$outcome  == "shooting_rate" & results$spec == "lagged_t1"]))
message(sprintf("  Robbery rate:  b = %.4f, SE = %.4f, p = %.4f",
                results$estimate[results$outcome == "robbery_rate"  & results$spec == "lagged_t1"],
                results$std_err[results$outcome  == "robbery_rate"  & results$spec == "lagged_t1"],
                results$p_value[results$outcome  == "robbery_rate"  & results$spec == "lagged_t1"]))
message("\nDone.")
