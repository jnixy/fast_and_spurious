# ==============================================================================
# Vehicle Pursuit Policy Analysis — Script 18: Generalized Synthetic Control
# ==============================================================================
#
# Author:   John Hall & Justin Nix
# Source:   Xu, Y. (2017). Generalized Synthetic Control Method. Political
#           Analysis, 25(1), 57–76.
#
# Purpose:
#   Estimates the causal effect of NYPD's October 2022 pursuit escalation on
#   shooting rates using the generalized synthetic control (gsynth) method.
#   Primary analysis uses a SUTVA-clean donor pool (24 cities with documented
#   pursuit policy changes excluded). Full 50-city panel is run as a robustness
#   check. Both analyses are null — consistent with the ITS and precinct DDD.
#
# Inputs:
#   - data/american_violence/*.csv.csv — annual GVA shooting files (2014–2025)
#
# Outputs (saved to output/gsynth_results/):
#   - gsynth_att.csv             — ATT, 95% CI, p-value (both donor pools)
#   - gsynth_actual_vs_cf.png/pdf — actual vs. counterfactual figure (primary)
#   - gsynth_att_gap.png/pdf      — ATT gap with 95% CI figure (primary)
#   - gsynth_robustness.rds       — full gsynth fit objects for reuse
#
# Runtime: ~15–25 min (primary bootstrap takes most of the time)
#
# Controls (set at top to adjust behavior):
#   RUN_PERMUTATION  — permutation inference across all donor cities (slow: +30 min)
#   BOOTSTRAP_REPS   — number of parametric bootstrap iterations
# ==============================================================================

# --- Controls ---
RUN_PERMUTATION <- FALSE   # Set TRUE to run full permutation inference
BOOTSTRAP_REPS  <- 500     # Parametric bootstrap iterations

# ==============================================================================
# 0. Setup
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(scales)
  library(gsynth)
})

set.seed(20241001)

message("=== Script 18: gsynth Analysis ===")
message("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M"))

INTERVENTION_DATE <- as.Date("2022-10-01")
# NOTE: No right-censoring for GVA data — AV files are complete through Dec 2025
# for all 100 cities. The Sep 2025 right-censor applies only to NYPD administrative
# data (Scripts 01–17), not to GVA/AmericanViolence.org data used here.
TREATED_UNIT      <- "New York"

results_dir <- here("output", "gsynth_results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# Cities excluded due to documented pursuit policy changes 2018–2024
# (city-level policy OR binding state-level statute)
EXCLUDE_PURSUIT_CHANGE <- c(
  "Atlanta",                     # Zero-chase policy, Jan 2020
  "Chesapeake",                  # Revised pursuit policy, Nov 2023
  "Chicago",                     # Tightened 2020, revised 2022
  "Cincinnati",                  # Violent felonies only, Feb 2023
  "Houston",                     # Banned Class C chases, Sep 2023
  "Indianapolis",                # IN statewide minimum standards, Jan 2023
  "Fort Wayne",                  # IN statewide minimum standards, Jan 2023
  "Jersey City",                 # NJ AG statewide directive, 2021–2022
  "Lexington-Fayette",           # KY statewide written-policy mandate, 2020
  "Long Beach",                  # Special Order 2023-5, Jun 2023
  "Louisville/Jefferson County", # Loosened Jul 2019 + KY state mandate 2020
  "New Orleans",                 # Policy revised Aug 2019 & Feb 2024
  "Newark",                      # NJ AG statewide directive, 2021–2022
  "Oakland",                     # 50 mph speed cap, Dec 2022
  "Oklahoma City",               # Tightened, Jun 2022
  "Portland",                    # Overhaul, Jan 2024
  "San Francisco",               # Prop E loosened, Mar 2024
  "San Jose",                    # Dept. Order 2022-037, 2022
  "Seattle",                     # WA state HB 1054/SB 5352/I-2113, 2021–2024
  "Spokane",                     # WA state law changes, 2021–2024
  "Stockton",                    # Voters loosened, late 2024
  "Tampa",                       # Revised after fatal crash, 2022
  "Toledo",                      # New policy, mid-2024
  "Washington"                   # Restricted 2022; loosened 2024
)

# ==============================================================================
# 1. Load Data
# ==============================================================================

message("--- 1. Loading AV data ---")

av_dir   <- here("data", "american_violence")
av_files <- list.files(av_dir, pattern = "\\.csv\\.csv$", full.names = TRUE)

if (length(av_files) == 0) {
  stop("No AV files found in ", av_dir)
}

message("Found ", length(av_files), " annual files (", min(as.integer(
  str_extract(basename(av_files), "\\d{4}"))), "–", max(as.integer(
  str_extract(basename(av_files), "\\d{4}"))), ")")

raw <- map_dfr(sort(av_files), function(f) {
  read_csv(f, show_col_types = FALSE, col_types = cols(
    timespan                 = col_character(),
    id                       = col_double(),
    year                     = col_integer(),
    month                    = col_character(),
    state_abr                = col_character(),
    county_name              = col_character(),
    place_name               = col_character(),
    population_est           = col_double(),
    crime_type               = col_character(),
    crime_count              = col_double(),
    annualized_rate_per_100k = col_double(),
    source_desc              = col_character()
  ))
})

message("Raw rows: ", comma(nrow(raw)), " | Cities: ", n_distinct(raw$place_name),
        " | Years: ", min(raw$year), "–", max(raw$year))

# ==============================================================================
# 2. Clean and Build Panel
# ==============================================================================

message("--- 2. Building city-month panel ---")

city_monthly <- raw %>%
  filter(crime_type %in% c("Fatal Shootings", "Nonfatal Shootings")) %>%
  mutate(
    month_num = as.integer(month),
    date      = ymd(paste(year, month_num, "01", sep = "-"))
  ) %>%
  group_by(place_name, state_abr, date, year, month_num, crime_type) %>%
  summarise(
    crime_count = sum(crime_count, na.rm = TRUE),
    population  = max(population_est, na.rm = TRUE),
    .groups     = "drop"
  ) %>%
  pivot_wider(
    names_from  = crime_type,
    values_from = crime_count,
    values_fill = 0
  ) %>%
  clean_names() %>%
  rename(
    gva_nonfatal = nonfatal_shootings,
    gva_fatal    = fatal_shootings
  ) %>%
  mutate(
    gva_total = gva_nonfatal + gva_fatal,
    # Annualized rate per 100k (monthly count → annualized equivalent)
    gva_rate  = (gva_total / population) * 100000 * 12
  ) %>%
  # No right-censoring — GVA data is complete through Dec 2025
  filter(!is.na(date))

message("Panel: ", n_distinct(city_monthly$place_name), " cities | ",
        format(min(city_monthly$date), "%b %Y"), "–",
        format(max(city_monthly$date), "%b %Y"))

# ==============================================================================
# 3. Build Balanced Panel Helper
# ==============================================================================

# Builds a balanced panel (drops cities missing any month) and adds treatment
# indicator and sequential time index
build_panel <- function(df, label = "panel") {
  city_range <- df %>%
    group_by(place_name) %>%
    summarise(min_date = min(date), max_date = max(date), n = n(), .groups = "drop")

  common_start <- max(city_range$min_date)
  common_end   <- min(city_range$max_date)

  panel <- df %>%
    filter(date >= common_start, date <= common_end) %>%
    rename(unit = place_name) %>%
    arrange(unit, date) %>%
    group_by(unit) %>%
    mutate(time_index = row_number()) %>%
    ungroup() %>%
    mutate(treat = as.integer(unit == TREATED_UNIT & date >= INTERVENTION_DATE))

  # Drop incomplete cities after applying window
  counts   <- panel %>% count(unit) %>% pull(n)
  max_obs  <- max(counts)
  full_cities <- panel %>% count(unit) %>% filter(n == max_obs) %>% pull(unit)
  if (length(full_cities) < n_distinct(panel$unit)) {
    panel <- panel %>% filter(unit %in% full_cities)
  }

  n_units   <- n_distinct(panel$unit)
  n_periods <- n_distinct(panel$time_index)
  n_post    <- sum(panel$unit == TREATED_UNIT & panel$treat == 1)
  n_pre     <- sum(panel$unit == TREATED_UNIT & panel$treat == 0)

  message(label, ": ", n_units, " cities \u00d7 ", n_periods, " months | ",
          "pre = ", n_pre, ", post = ", n_post,
          " | donors = ", n_units - 1)

  panel
}

# ==============================================================================
# 4. Primary Analysis: SUTVA-Clean Donor Pool
# ==============================================================================

message("--- 4. Primary analysis: SUTVA-clean donor pool ---")

eligible_clean <- city_monthly %>%
  filter(!place_name %in% EXCLUDE_PURSUIT_CHANGE | place_name == TREATED_UNIT)

n_excluded <- n_distinct(city_monthly$place_name) - n_distinct(eligible_clean$place_name)
message("Excluded ", n_excluded, " cities with documented pursuit policy changes")

panel_clean <- build_panel(eligible_clean, label = "SUTVA-clean panel")

message("Running gsynth (primary, ", BOOTSTRAP_REPS, " bootstrap iterations) ...")

fit_clean <- gsynth(
  gva_rate ~ treat,
  data      = panel_clean,
  index     = c("unit", "time_index"),
  force     = "two-way",
  CV        = TRUE,
  r         = c(0, 5),
  se        = TRUE,
  nboots    = BOOTSTRAP_REPS,
  inference = "parametric",
  seed      = 20241001,
  parallel  = FALSE
)

message("CV-selected factors: r = ", fit_clean$r.cv)
message("Average ATT (clean): ", round(fit_clean$att.avg, 3), " per 100k")

# ==============================================================================
# 5. Robustness: Full 50-City Panel (No Exclusions)
# ==============================================================================

message("--- 5. Robustness: full 50-city panel (top 50 by population) ---")

# Replicate John Hall's no-exclusions analysis: filter to top 50 cities by pop
city_pop <- city_monthly %>%
  group_by(place_name, state_abr) %>%
  summarise(pop = max(population, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(pop)) %>%
  mutate(pop_rank = row_number())

top50 <- city_pop %>% filter(pop_rank <= 50)
message("NYC population rank: ",
        top50 %>% filter(place_name == TREATED_UNIT) %>% pull(pop_rank))

top50_monthly <- city_monthly %>%
  semi_join(top50, by = c("place_name", "state_abr"))

panel_full <- build_panel(top50_monthly, label = "Full panel (50 cities)")

fit_full <- gsynth(
  gva_rate ~ treat,
  data      = panel_full,
  index     = c("unit", "time_index"),
  force     = "two-way",
  CV        = TRUE,
  r         = c(0, 5),
  se        = FALSE,   # no bootstrap needed for robustness comparison
  seed      = 20241001,
  parallel  = FALSE
)

message("CV-selected factors: r = ", fit_full$r.cv)
message("Average ATT (full):  ", round(fit_full$att.avg, 3), " per 100k")

# ==============================================================================
# 6. Extract and Summarize Results
# ==============================================================================

message("--- 6. Extracting results ---")

# Date lookup for the treated unit
date_lookup <- panel_clean %>%
  filter(unit == TREATED_UNIT) %>%
  select(time_index, date) %>%
  distinct() %>%
  arrange(time_index)

# Extract per-period estimates as a tidy data frame
extract_results <- function(fit, panel, treated = TREATED_UNIT) {
  nyc_col     <- which(colnames(fit$eff) == treated)
  Y_actual    <- fit$Y.dat[, nyc_col]
  Y_counter   <- fit$Y.ct[, nyc_col]
  time_idx    <- as.integer(rownames(fit$eff))
  rel_time    <- fit$time

  df <- tibble(
    time_index = time_idx,
    rel_time   = rel_time,
    att        = fit$att,
    Y_actual   = Y_actual,
    Y_counter  = Y_counter
  )

  # fit$est.att is the per-period matrix (time × {ATT, S.E., CI.lower, CI.upper, p.value})
  # Only present when se = TRUE (primary analysis). fit$att.bound is the scalar average CI — wrong slot.
  if (!is.null(fit$est.att)) {
    df$ci_lower <- fit$est.att[, "CI.lower"]
    df$ci_upper <- fit$est.att[, "CI.upper"]
  } else {
    df$ci_lower <- NA_real_
    df$ci_upper <- NA_real_
  }

  df %>%
    left_join(date_lookup, by = "time_index") %>%
    arrange(time_index)
}

results_clean <- extract_results(fit_clean, panel_clean)
results_full  <- extract_results(fit_full, panel_full)

# Pre-treatment fit quality: rel_time <= 0 (gsynth assigns rel_time = 0 to the last
# pre-treatment period, Sep 2022; rel_time > 0 are the true post-treatment months)
pre_clean <- results_clean %>% filter(rel_time <= 0)
pre_full  <- results_full  %>% filter(rel_time <= 0)

rmspe_clean <- sqrt(mean(pre_clean$att^2))
rmspe_full  <- sqrt(mean(pre_full$att^2))

# Post-treatment summary: use rel_time > 0 (strictly positive) to exclude the
# reference period at rel_time = 0 (Sep 2022, last pre-treatment month)
post_clean <- results_clean %>% filter(rel_time > 0)
post_full  <- results_full  %>% filter(rel_time > 0)

n_sig_clean <- sum((post_clean$ci_lower > 0) | (post_clean$ci_upper < 0), na.rm = TRUE)
n_post_clean <- nrow(post_clean)
n_post_full  <- nrow(post_full)

n_donors_clean <- n_distinct(panel_clean$unit) - 1
n_donors_full  <- n_distinct(panel_full$unit) - 1
n_months       <- n_distinct(panel_clean$time_index)

message("Clean | ATT = ", round(fit_clean$att.avg, 3),
        " | pre-RMSPE = ", round(rmspe_clean, 3),
        " | sig months = ", n_sig_clean, "/", n_post_clean)
message("Full  | ATT = ", round(fit_full$att.avg, 3),
        " | pre-RMSPE = ", round(rmspe_full, 3))

# ==============================================================================
# 7. Optional: Permutation Inference
# ==============================================================================

perm_p_att  <- NA_real_
perm_p_mspe <- NA_real_

if (RUN_PERMUTATION) {
  message("--- 7. Permutation inference (", n_donors_clean + 1, " cities) ---")

  all_cities   <- unique(panel_clean$unit)
  perm_results <- tibble(
    city       = character(),
    att_avg    = numeric(),
    mspe_ratio = numeric()
  )

  for (cname in all_cities) {
    tryCatch({
      p_data <- panel_clean %>%
        mutate(treat_perm = as.integer(unit == cname & date >= INTERVENTION_DATE))
      fit_p <- gsynth(
        gva_rate ~ treat_perm,
        data      = p_data,
        index     = c("unit", "time_index"),
        force     = "two-way",
        CV        = TRUE,
        r         = c(0, 5),
        se        = FALSE,
        seed      = 42,
        parallel  = FALSE
      )
      perm_df  <- extract_results(fit_p, p_data)
      pre_mspe <- mean(filter(perm_df, rel_time < 0)$att^2)
      pos_mspe <- mean(filter(perm_df, rel_time >= 0)$att^2)
      ratio    <- if (pre_mspe > 0) pos_mspe / pre_mspe else NA_real_
      perm_results <- add_row(
        perm_results, city = cname, att_avg = fit_p$att.avg, mspe_ratio = ratio
      )
    }, error = function(e) invisible(NULL))
  }

  nyc_row     <- perm_results %>% filter(city == TREATED_UNIT)
  n_total     <- nrow(perm_results)
  n_valid     <- sum(!is.na(perm_results$mspe_ratio))
  perm_p_att  <- sum(abs(perm_results$att_avg) >= abs(nyc_row$att_avg), na.rm = TRUE) / n_total
  perm_p_mspe <- sum(perm_results$mspe_ratio >= nyc_row$mspe_ratio, na.rm = TRUE) / n_valid

  message("Permutation ATT two-sided p = ", round(perm_p_att, 3))
  message("Permutation MSPE-ratio p    = ", round(perm_p_mspe, 3))

  saveRDS(perm_results, file.path(results_dir, "gsynth_perm_results.rds"))
}

# ==============================================================================
# 8. Export Summary Table
# ==============================================================================

message("--- 8. Exporting results ---")

gsynth_att <- tibble(
  estimator       = "gsynth",
  donor_pool      = c(paste0("SUTVA-clean (", n_donors_clean, " donors)"),
                      paste0("Full panel (", n_donors_full, " donors)")),
  att             = c(fit_clean$att.avg, fit_full$att.avg),
  ci_lower        = c(
    if (!is.null(fit_clean$att.avg.bound)) fit_clean$att.avg.bound["CI.lower"] else NA_real_,
    NA_real_
  ),
  ci_upper        = c(
    if (!is.null(fit_clean$att.avg.bound)) fit_clean$att.avg.bound["CI.upper"] else NA_real_,
    NA_real_
  ),
  p_value         = c(perm_p_att, NA_real_),
  n_factors       = c(fit_clean$r.cv, fit_full$r.cv),
  n_donors        = c(n_donors_clean, n_donors_full),
  n_months        = n_months,
  n_pre           = n_distinct(results_clean %>% filter(rel_time <= 0) %>% pull(time_index)),
  n_post          = n_post_clean,
  pre_rmspe       = c(rmspe_clean, rmspe_full),
  n_sig_months    = c(n_sig_clean, NA_integer_)
)

write_csv(gsynth_att, file.path(results_dir, "gsynth_att.csv"))
message("Saved: gsynth_att.csv (ci_lower/upper from att.avg.bound; NA if bootstrap not run)")

# Save full period-level results
write_csv(results_clean, file.path(results_dir, "gsynth_period_effects_clean.csv"))
write_csv(results_full,  file.path(results_dir, "gsynth_period_effects_full.csv"))

# Save fit objects for reuse (avoid re-running bootstrap)
saveRDS(list(fit_clean = fit_clean, fit_full = fit_full,
             panel_clean = panel_clean, panel_full = panel_full),
        file.path(results_dir, "gsynth_robustness.rds"))

message("Saved: gsynth_period_effects_*.csv, gsynth_robustness.rds")

# ==============================================================================
# 9. Figures
# ==============================================================================

message("--- 9. Generating figures ---")

# Project colors (consistent with theme_pursuit)
COL_ACTUAL      <- "#003049"   # dark blue
COL_COUNTER     <- "#D62828"   # pursuit red (dashed)
COL_SHADING     <- "#D62828"

save_plot <- function(p, name, width = 6.5, height = 4) {
  ggsave(file.path(results_dir, paste0(name, ".png")), p,
         width = width, height = height, dpi = 300)
  ggsave(file.path(results_dir, paste0(name, ".pdf")), p,
         width = width, height = height)
  message("Saved: ", name, ".png / .pdf")
}

# --- Figure B1: Actual vs. Counterfactual ---
n_donors_label <- n_donors_clean
r_label        <- fit_clean$r.cv
att_label      <- round(fit_clean$att.avg, 2)

fig_b1 <- ggplot(results_clean %>% filter(!is.na(date)), aes(x = date)) +
  geom_line(aes(y = Y_actual, color = "NYC (Actual)"),
            linewidth = 0.8) +
  geom_line(aes(y = Y_counter, color = "Counterfactual (gsynth)"),
            linewidth = 0.8, linetype = "dashed") +
  geom_vline(xintercept = INTERVENTION_DATE,
             linetype = "dotted", color = "gray40", linewidth = 0.6) +
  annotate("text",
           x     = INTERVENTION_DATE + 60,
           y     = max(results_clean$Y_actual, na.rm = TRUE) * 0.97,
           label = "Oct 2022\n(escalation)",
           hjust = 0, size = 2.8, color = "gray40") +
  scale_color_manual(
    values = c("NYC (Actual)" = COL_ACTUAL,
               "Counterfactual (gsynth)" = COL_COUNTER),
    guide  = guide_legend(override.aes = list(
      linetype = c("solid", "dashed"), linewidth = c(0.8, 0.8)
    ))
  ) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x      = NULL,
    y      = "Annualized shooting rate per 100,000",
    color  = NULL,
    caption = paste0(n_donors_label, " donor cities (pursuit-policy-change cities excluded) | ",
                     "r = ", r_label, " latent factor | Avg ATT = ", att_label, " per 100k")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position   = "bottom",
    plot.caption      = element_text(size = 8, color = "gray50"),
    panel.grid.minor  = element_blank()
  )

save_plot(fig_b1, "gsynth_actual_vs_cf")

# --- Figure B2: ATT Gap with 95% CI ---
fig_b2 <- ggplot(results_clean %>% filter(!is.na(date)), aes(x = date)) +
  geom_ribbon(
    aes(ymin = ci_lower, ymax = ci_upper),
    fill = COL_SHADING, alpha = 0.12, na.rm = TRUE
  ) +
  geom_line(aes(y = att), linewidth = 0.7, color = COL_ACTUAL) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_vline(xintercept = INTERVENTION_DATE,
             linetype = "dotted", color = "gray40", linewidth = 0.6) +
  annotate("text",
           x     = INTERVENTION_DATE + 60,
           y     = max(results_clean$ci_upper, na.rm = TRUE) * 0.92,
           label = "Oct 2022\n(escalation)",
           hjust = 0, size = 2.8, color = "gray40") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    x       = NULL,
    y       = "ATT (annualized rate per 100,000)",
    caption = paste0("95% parametric bootstrap CI | ",
                     n_donors_label, " donor cities | Avg ATT = ", att_label, " per 100k")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.caption     = element_text(size = 8, color = "gray50"),
    panel.grid.minor = element_blank()
  )

save_plot(fig_b2, "gsynth_att_gap")

# ==============================================================================
# 10. Final Summary
# ==============================================================================

message("\n=== FINAL SUMMARY ===")
message(sprintf("Primary (SUTVA-clean): ATT = %.3f per 100k | r = %d | sig months = %d/%d",
                fit_clean$att.avg, fit_clean$r.cv, n_sig_clean, n_post_clean))
message(sprintf("Robustness (full):     ATT = %.3f per 100k | r = %d",
                fit_full$att.avg, fit_full$r.cv))
if (!is.na(perm_p_att)) {
  message(sprintf("Permutation ATT p = %.3f | MSPE-ratio p = %.3f",
                  perm_p_att, perm_p_mspe))
} else {
  message("Permutation not run (set RUN_PERMUTATION = TRUE to enable)")
}
message("Outputs saved to: ", results_dir)
message("Script complete: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
