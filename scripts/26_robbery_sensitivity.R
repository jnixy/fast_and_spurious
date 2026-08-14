# ==============================================================================
# Vehicle Pursuit Policy Analysis — Script 26: Robbery Trend-Sensitivity Check
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Linear pre-trend extrapolation sensitivity (Rambachan & Roth 2023's
#   breakdown-value concept in spirit, not their constrained-optimization
#   method — no smoothness or monotonicity restriction classes are imposed)
#   for the robbery precinct DiD estimate. The robbery raw DiD is +2.568
#   (positive), so the question is: how large would a linear pre-trend
#   violation need to be to flip the estimate negative (toward deterrence)?
#
# Inputs:
#   - output/precinct_did/pct_monthly_shootings.csv
#   - output/precinct_did/pct_monthly_robberies.csv
#
# Outputs (saved to output/):
#   - detrend_results/robbery_sensitivity.csv  -- breakdown value M* and
#     trend-adjusted ATT for the robbery DiD
#
# Runtime: ~1 min
# ==============================================================================

# ── 0. Setup ──────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(lubridate)
  library(fixest)
  library(janitor)
})

set.seed(20260629)

message("=== Script 26: Robbery Trend-Sensitivity Check ===")

INTERVENTION <- as.Date("2022-10-01")

results_dir <- here("output", "detrend_results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# ── 1. Load & Build Panel (identical to script 25) ─────────────────────────────

shootings_pct <- read_csv(here("output", "precinct_did", "pct_monthly_shootings.csv"),
                           show_col_types = FALSE) |>
  clean_names() |>
  mutate(month_date = as.Date(month_date))

robberies_pct <- read_csv(here("output", "precinct_did", "pct_monthly_robberies.csv"),
                           show_col_types = FALSE) |>
  clean_names() |>
  mutate(month_date = as.Date(month_date))

panel <- shootings_pct |>
  full_join(robberies_pct, by = c("pct", "month_date")) |>
  replace_na(list(shooting_incidents = 0L, robbery_count = 0L)) |>
  filter(pct >= 1, pct <= 123,
         month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) |>
  mutate(pct = if_else(pct == 116L, 105L, pct)) |>
  group_by(pct, month_date) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
  arrange(pct, month_date)

pre_crime_rob <- panel |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_rob_pre = mean(robbery_count, na.rm = TRUE), .groups = "drop") |>
  mutate(high_crime_rob = as.integer(mean_rob_pre >= median(mean_rob_pre)))

panel <- panel |>
  left_join(pre_crime_rob |> select(pct, high_crime_rob), by = "pct") |>
  mutate(
    post     = as.integer(month_date >= INTERVENTION),
    rel_time = (year(month_date)  - year(INTERVENTION)) * 12L +
               (month(month_date) - month(INTERVENTION)),
    time_bin      = floor(rel_time / 3L),
    time_bin_lump = if_else(time_bin >= 9L, 9L, time_bin)
  )

message("Panel: ", n_distinct(panel$pct), " precincts x ",
        n_distinct(panel$month_date), " months")

# ── 2. Robbery Event Study ──────────────────────────────────────────────────────

es_raw_rob <- feols(
  robbery_count ~ i(time_bin_lump, high_crime_rob, ref = -1) | pct + month_date,
  data = panel, cluster = ~pct
)

es_rob_tidy <- broom::tidy(es_raw_rob, conf.int = TRUE) |>
  mutate(
    time_bin = as.integer(str_extract(term, "-?\\d+"))
  )

message("\nRobbery event study coefficients:")
message(paste(capture.output(
  es_rob_tidy |>
    select(time_bin, estimate, std.error, p.value) |>
    filter(!is.na(time_bin)) |>
    arrange(time_bin) |>
    mutate(across(where(is.numeric), \(x) round(x, 4))),
  n = Inf
), collapse = "\n"))

# ── 3. Breakdown Value (M*) ──────────────────────────────────────────────────────

pre_es <- es_rob_tidy |>
  filter(!is.na(time_bin), time_bin < -1) |>
  arrange(time_bin)

message("\nPre-treatment coefficients (robbery):")
message(paste(capture.output(
  pre_es |> select(time_bin, estimate) |> mutate(estimate = round(estimate, 4)),
  n = Inf
), collapse = "\n"))

if (nrow(pre_es) >= 2) {
  pre_trend_fit   <- lm(estimate ~ time_bin, data = pre_es)
  pre_trend_slope <- coef(pre_trend_fit)["time_bin"]

  message("\nPre-treatment event-study slope (robbery): ",
          round(pre_trend_slope, 4), " per quarterly bin")

  post_es <- es_rob_tidy |>
    filter(!is.na(time_bin), time_bin >= 0)

  if (nrow(post_es) > 0) {
    post_es <- post_es |>
      mutate(
        trend_extrapolated = pre_trend_slope * (time_bin - (-1)),
        estimate_adjusted  = estimate - trend_extrapolated
      )

    avg_att_raw      <- mean(post_es$estimate)
    avg_att_adjusted <- mean(post_es$estimate_adjusted)

    message("Average post-treatment ATT (raw):            ",
            round(avg_att_raw, 4))
    message("Average post-treatment ATT (trend-adjusted): ",
            round(avg_att_adjusted, 4))

    mean_post_distance <- mean(post_es$time_bin - (-1))

    if (avg_att_raw != 0) {
      breakdown_M <- abs(avg_att_raw) / mean_post_distance
      ratio       <- breakdown_M / abs(pre_trend_slope)

      message("\n=== ROBBERY BREAKDOWN VALUE ===")
      message("M* = ", round(breakdown_M, 4), " per quarterly bin")
      message("Observed pre-trend slope = ", round(abs(pre_trend_slope), 4))
      message("Ratio M*/slope = ", round(ratio, 2))
      message("Interpretation: the pre-trend violation would need to be ",
              round(ratio, 1), "x larger than observed to flip the robbery ",
              "estimate to negative.")
    }

    sensitivity <- tibble(
      outcome            = "Robbery",
      pre_trend_slope    = pre_trend_slope,
      avg_att_raw        = avg_att_raw,
      avg_att_adjusted   = avg_att_adjusted,
      breakdown_M        = if (exists("breakdown_M")) breakdown_M else NA_real_,
      ratio_M_to_slope   = if (exists("breakdown_M")) ratio else NA_real_,
      n_pre_bins         = nrow(pre_es),
      n_post_bins        = nrow(post_es)
    )

    write_csv(sensitivity, file.path(results_dir, "robbery_sensitivity.csv"))
    message("\nSaved: robbery_sensitivity.csv")

    # Also report the post-period detail
    message("\nPost-period detail:")
    message(paste(capture.output(
      post_es |>
        select(time_bin, estimate, trend_extrapolated, estimate_adjusted) |>
        mutate(across(where(is.numeric), \(x) round(x, 4))),
      n = Inf
    ), collapse = "\n"))
  }
} else {
  message("Insufficient pre-treatment coefficients for robbery trend analysis.")
}

message("\n=== Done ===")
