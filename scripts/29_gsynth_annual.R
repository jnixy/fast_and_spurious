# ==============================================================================
# Vehicle Pursuit Policy Analysis — Script 29: Annual Generalized Synthetic
#   Control (Shootings and Robbery)
# ==============================================================================
#
# Author:   John Hall & Justin Nix
# Source:   Xu, Y. (2017). Generalized Synthetic Control Method. Political
#           Analysis, 25(1), 57-76.
#
# Purpose:
#   JQC conditional-acceptance Editor Comment 1. The primary national comparison
#   (script 18) estimates a gsynth model for the monthly annualized shooting
#   rate and returns a null. The Editor asks (a) for a comparable national
#   estimate for robbery, and (b) for the magnitude of effect the null can rule
#   out, in percentage terms relative to the base rate. This script runs gsynth
#   on ANNUAL city panels for two outcomes:
#     - Shootings: Gun Violence Archive / AmericanViolence.org, SUTVA-clean
#       donor pool (same exclusions as script 18), 2014-2025, treat 2023.
#     - Robbery:   Kaplan UCR Return A city panel (script 08), 2014-2024,
#       treat 2023. NYC is the five boroughs aggregated to one unit.
#   Annual aggregation removes the month-to-month noise in the monthly rate and
#   makes the two outcomes comparable. The monthly model in script 18 remains
#   the manuscript's primary national result; these annual fits feed the
#   minimum-detectable-effect table (script 28) and the response to the Editor.
#
# Inputs:
#   - data/american_violence/*.csv.csv                         (annual GVA files)
#   - output/national_did/combined_annual_panel.csv            (Kaplan UCR robbery)
#
# Outputs (saved to output/gsynth_results/):
#   - gsynth_annual_att.csv              -- ATT, bootstrap SE + 95% CI, r.cv,
#                                           pre-RMSPE, NYC pre-period base rate,
#                                           one row per outcome
#   - gsynth_annual_period_effects.csv   -- per-year actual / counterfactual / ATT
#   - gsynth_annual_shooting_cf.png/pdf  -- actual vs. counterfactual, shootings
#   - gsynth_annual_robbery_cf.png/pdf   -- actual vs. counterfactual, robbery
#
# Runtime: ~3-6 min (annual panels are small; 1,000 bootstrap iterations each)
# ==============================================================================

# ── Controls ──────────────────────────────────────────────────────────────────
BOOTSTRAP_REPS <- 1000
TREATED_UNIT   <- "New York"
TREAT_YEAR     <- 2023          # first full post-escalation calendar year
R_RANGE        <- c(0, 3)       # annual panels: few pre-periods, cap factors low

# ── 0. Setup ──────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(janitor)
  library(scales)
  library(gsynth)
})

set.seed(20241001)

message("=== Script 29: Annual gsynth (shootings + robbery) ===")
message("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M"))

results_dir <- here("output", "gsynth_results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# Cities excluded for documented pursuit-policy changes 2018-2024 — identical to
# script 18's EXCLUDE_PURSUIT_CHANGE (kept in sync by hand; see script 18).
EXCLUDE_PURSUIT_CHANGE <- c(
  "Atlanta", "Chesapeake", "Chicago", "Cincinnati", "Houston", "Indianapolis",
  "Fort Wayne", "Jersey City", "Lexington-Fayette", "Long Beach",
  "Louisville/Jefferson County", "New Orleans", "Newark", "Oakland",
  "Oklahoma City", "Portland", "San Francisco", "San Jose", "Seattle",
  "Spokane", "Stockton", "Tampa", "Toledo", "Washington"
)

# ── 1. Helpers ────────────────────────────────────────────────────────────────

#' Build a balanced annual panel: keep only units observed in every year, add a
#' sequential treatment indicator for the treated unit from TREAT_YEAR onward.
#' @param df long tibble with columns unit, year, rate
#' @return balanced tibble with an integer `treat` column
build_annual_panel <- function(df, label = "panel") {
  yr_range <- df %>%
    group_by(unit) %>%
    summarise(min_y = min(year), max_y = max(year), n = n(), .groups = "drop")

  common_start <- max(yr_range$min_y)
  common_end   <- min(yr_range$max_y)

  panel <- df %>%
    filter(year >= common_start, year <= common_end) %>%
    arrange(unit, year)

  n_years <- n_distinct(panel$year)
  full_units <- panel %>% count(unit) %>% filter(n == n_years) %>% pull(unit)
  panel <- panel %>% filter(unit %in% full_units)

  panel <- panel %>%
    mutate(treat = as.integer(unit == TREATED_UNIT & year >= TREAT_YEAR))

  n_pre  <- sum(panel$unit == TREATED_UNIT & panel$treat == 0)
  n_post <- sum(panel$unit == TREATED_UNIT & panel$treat == 1)
  message(label, ": ", n_distinct(panel$unit), " units (",
          n_distinct(panel$unit) - 1, " donors) x ", n_years, " years | ",
          "pre = ", n_pre, ", post = ", n_post)

  panel
}

#' Run gsynth on an annual panel and return a tidy one-row summary plus the
#' per-year series. Uses the bootstrap `est.avg` slot for the average-ATT CI
#' (the per-period `att.avg.bound` slot is not populated with a single treated
#' unit and a singular F-test covariance matrix; see script 18).
fit_annual_gsynth <- function(panel, outcome_label) {
  message("  fitting gsynth for ", outcome_label, " (", BOOTSTRAP_REPS,
          " bootstrap iterations) ...")
  fit <- gsynth(
    rate ~ treat,
    data      = panel,
    index     = c("unit", "year"),
    force     = "two-way",
    CV        = TRUE,
    r         = R_RANGE,
    se        = TRUE,
    nboots    = BOOTSTRAP_REPS,
    inference = "parametric",
    seed      = 20241001,
    parallel  = FALSE
  )

  nyc_col   <- which(colnames(fit$eff) == TREATED_UNIT)
  rel_time  <- fit$time
  series <- tibble(
    outcome    = outcome_label,
    year       = as.integer(rownames(fit$eff)),
    rel_time   = rel_time,
    att        = as.numeric(fit$att),
    Y_actual   = as.numeric(fit$Y.dat[, nyc_col]),
    Y_counter  = as.numeric(fit$Y.ct[, nyc_col])
  )
  if (!is.null(fit$est.att)) {
    series$ci_lower <- fit$est.att[, "CI.lower"]
    series$ci_upper <- fit$est.att[, "CI.upper"]
  } else {
    series$ci_lower <- NA_real_
    series$ci_upper <- NA_real_
  }

  pre_att   <- series$att[series$rel_time <= 0]
  pre_rmspe <- sqrt(mean(pre_att^2))
  base_rate <- mean(series$Y_actual[series$rel_time <= 0])

  ea <- fit$est.avg   # matrix: ATT.avg, S.E., CI.lower, CI.upper, p.value
  summ <- tibble(
    outcome     = outcome_label,
    n_donors    = length(unique(panel$unit)) - 1L,
    n_pre       = sum(series$rel_time <= 0),
    n_post      = sum(series$rel_time > 0),
    r_cv        = fit$r.cv,
    att         = as.numeric(fit$att.avg),
    se          = if (!is.null(ea)) ea[1, "S.E."]     else NA_real_,
    ci_lower    = if (!is.null(ea)) ea[1, "CI.lower"] else NA_real_,
    ci_upper    = if (!is.null(ea)) ea[1, "CI.upper"] else NA_real_,
    p_value     = if (!is.null(ea)) ea[1, "p.value"]  else NA_real_,
    pre_rmspe   = pre_rmspe,
    nyc_base_rate = base_rate
  )

  list(fit = fit, series = series, summary = summ)
}

save_plot <- function(p, name, width = 6.5, height = 4) {
  ggsave(file.path(results_dir, paste0(name, ".png")), p,
         width = width, height = height, dpi = 300)
  ggsave(file.path(results_dir, paste0(name, ".pdf")), p,
         width = width, height = height)
  message("  saved: ", name, ".png / .pdf")
}

COL_ACTUAL  <- "#003049"
COL_COUNTER <- "#D62828"

plot_cf <- function(series, y_lab, subtitle) {
  ggplot(series, aes(x = year)) +
    geom_line(aes(y = Y_actual, color = "NYC (actual)"), linewidth = 0.8) +
    geom_point(aes(y = Y_actual, color = "NYC (actual)"), size = 1.6) +
    geom_line(aes(y = Y_counter, color = "Counterfactual (gsynth)"),
              linewidth = 0.8, linetype = "dashed") +
    geom_point(aes(y = Y_counter, color = "Counterfactual (gsynth)"), size = 1.6) +
    geom_vline(xintercept = TREAT_YEAR - 0.5, linetype = "dotted",
               color = "gray40", linewidth = 0.6) +
    scale_color_manual(values = c("NYC (actual)" = COL_ACTUAL,
                                  "Counterfactual (gsynth)" = COL_COUNTER)) +
    scale_x_continuous(breaks = scales::breaks_width(2)) +
    labs(x = NULL, y = y_lab, color = NULL, subtitle = subtitle) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
}

# ── 2. Shooting panel (annual GVA) ────────────────────────────────────────────

message("--- 2. Building annual GVA shooting panel ---")

av_dir   <- here("data", "american_violence")
av_files <- list.files(av_dir, pattern = "\\.csv\\.csv$", full.names = TRUE)
if (length(av_files) == 0) stop("No AV files found in ", av_dir)

av_raw <- map_dfr(sort(av_files), read_csv, show_col_types = FALSE,
                  col_types = cols(.default = col_guess()))

shooting_annual <- av_raw %>%
  filter(crime_type %in% c("Fatal Shootings", "Nonfatal Shootings")) %>%
  group_by(place_name, year) %>%
  summarise(
    shootings  = sum(crime_count, na.rm = TRUE),
    population = max(population_est, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  transmute(
    unit = place_name,
    year = as.integer(year),
    rate = shootings / population * 100000
  ) %>%
  filter(!unit %in% EXCLUDE_PURSUIT_CHANGE | unit == TREATED_UNIT)

message("  SUTVA-clean cities: ", n_distinct(shooting_annual$unit),
        " | years: ", paste(range(shooting_annual$year), collapse = "-"))

panel_shoot <- build_annual_panel(shooting_annual, "Shooting annual panel")
res_shoot   <- fit_annual_gsynth(panel_shoot, "Shootings")

save_plot(
  plot_cf(res_shoot$series,
          "Shootings per 100,000 (annual)",
          sprintf("Annual gsynth | %d donors | r = %d | ATT = %.2f per 100k",
                  res_shoot$summary$n_donors, res_shoot$summary$r_cv,
                  res_shoot$summary$att)),
  "gsynth_annual_shooting_cf"
)

# ── 3. Robbery panel (annual Kaplan UCR) ──────────────────────────────────────

message("--- 3. Building annual Kaplan UCR robbery panel ---")

rob_file <- here("output", "national_did", "combined_annual_panel.csv")
if (!file.exists(rob_file)) stop("Run 08_national_data_prep.R first: ", rob_file)

rob_raw <- read_csv(rob_file, show_col_types = FALSE)

# NYC in this panel is five boroughs (treated == 1). Aggregate to one unit so
# gsynth has a single treated unit, matching the national-DiD construction.
nyc_rob <- rob_raw %>%
  filter(treated == 1) %>%
  group_by(year) %>%
  summarise(robbery = sum(robbery), pop = sum(pop_2020), .groups = "drop") %>%
  transmute(unit = TREATED_UNIT, year = as.integer(year),
            rate = robbery / pop * 100000)

donor_rob <- rob_raw %>%
  filter(treated == 0) %>%
  transmute(unit = boro, year = as.integer(year), rate = robbery_rate)

robbery_annual <- bind_rows(nyc_rob, donor_rob)

message("  donor cities: ", n_distinct(donor_rob$unit),
        " | years: ", paste(range(robbery_annual$year), collapse = "-"))

panel_rob <- build_annual_panel(robbery_annual, "Robbery annual panel")
res_rob   <- fit_annual_gsynth(panel_rob, "Robbery")

save_plot(
  plot_cf(res_rob$series,
          "Robberies per 100,000 (annual)",
          sprintf("Annual gsynth | %d donors | r = %d | ATT = %.2f per 100k",
                  res_rob$summary$n_donors, res_rob$summary$r_cv,
                  res_rob$summary$att)),
  "gsynth_annual_robbery_cf"
)

# ── 4. Export ─────────────────────────────────────────────────────────────────

message("--- 4. Exporting ---")

att_tbl <- bind_rows(res_shoot$summary, res_rob$summary) %>%
  mutate(
    att_pct      = att      / nyc_base_rate * 100,
    ci_lower_pct = ci_lower  / nyc_base_rate * 100,
    ci_upper_pct = ci_upper  / nyc_base_rate * 100,
    mde_pct      = pmax(abs(ci_lower_pct), abs(ci_upper_pct))
  )

write_csv(att_tbl, file.path(results_dir, "gsynth_annual_att.csv"))
message("  saved: gsynth_annual_att.csv")

period_tbl <- bind_rows(res_shoot$series, res_rob$series)
write_csv(period_tbl, file.path(results_dir, "gsynth_annual_period_effects.csv"))
message("  saved: gsynth_annual_period_effects.csv")

# ── 5. Summary ────────────────────────────────────────────────────────────────

message("\n=== SUMMARY (annual gsynth) ===")
for (i in seq_len(nrow(att_tbl))) {
  r <- att_tbl[i, ]
  message(sprintf(
    "%-10s | base = %.1f/100k | ATT = %+.2f (%.0f%%) | 95%% CI [%.1f, %.1f] = [%.0f%%, %.0f%%] | r = %d | pre-RMSPE = %.2f | p = %.3f",
    r$outcome, r$nyc_base_rate, r$att, r$att_pct,
    r$ci_lower, r$ci_upper, r$ci_lower_pct, r$ci_upper_pct,
    r$r_cv, r$pre_rmspe, r$p_value))
}
message("Script complete: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
