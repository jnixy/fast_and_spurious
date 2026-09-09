# ==============================================================================
# Vehicle Pursuits Study: Precinct-Level Triple-Difference (DDD)
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Estimate causal effects of the NYPD pursuit policy change using a
#   triple-difference (DDD) design at the precinct level. The design
#   exploits cross-sectional variation in pre-existing crime levels to
#   classify precincts into high- and low-crime groups, which are
#   predetermined and exogenous to the October 2022 policy change.
#
#   Grouping variables (pre-treatment period: Jan 2018 – Sep 2022):
#     - high_crime_shoot:  1 if precinct mean monthly shooting_incidents
#                          >= citywide median in the pre-treatment period
#     - high_crime_rob:    1 if precinct mean monthly robbery_count >= median
#     - high_crime_gunviol:1 if precinct mean monthly gun_violence_count >= median
#     - high_crash:        1 if precinct mean monthly collision_count >= median
#     - shoot/rob/gunviol/crash_quartile: 1–4 quartile assignment
#
#   Crash DiD directional note: unlike crime outcomes (where high-crime
#   precincts are expected to benefit more from deterrence), for crashes
#   the directional prediction is symmetric — high-crash precincts may
#   have more pursuit activity and thus more crash events post-policy.
#
# Inputs:
#   - output/precinct_did/pct_monthly_shootings.csv     (from script 01)
#   - output/precinct_did/pct_monthly_robberies.csv     (from script 01)
#   - output/precinct_did/pct_monthly_gun_violence.csv  (from script 01)
#   - output/precinct_did/pct_monthly_collisions.csv    (from script 01)
#
# Outputs (saved to output/):
#   - output/precinct_did/pct_ddd_results.csv
#   - output/precinct_did/pct_ddd_es_shooting.csv
#   - output/precinct_did/pct_ddd_es_robbery.csv
#   - output/precinct_did/pct_ddd_es_gun_violence.csv
#   - output/precinct_did/pct_ddd_es_crashes.csv
#   - output/plots/fig_precinct_ddd_shooting.png/.pdf
#   - output/plots/fig_precinct_ddd_robbery.png/.pdf
#   - output/plots/fig_precinct_ddd_gun_violence.png/.pdf
#   - output/plots/fig_precinct_ddd_crashes.png/.pdf
#   - output/plots/fig_precinct_ddd_quartile_shooting.png/.pdf
#   - output/plots/fig_precinct_ddd_quartile_robbery.png/.pdf
#   - output/precinct_did/pct_ddd_models.rds
#
# Runtime: ~1 min
# ==============================================================================

library(tidyverse)
library(lubridate)
library(fixest)
library(patchwork)
library(here)
library(janitor)

set.seed(20260304)


# 0. Setup ---------------------------------------------------------------------

dir.create(here("output", "precinct_did"), showWarnings = FALSE, recursive = TRUE)
pct_dir  <- here("output", "precinct_did")
plot_dir <- here("output", "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

INTERVENTION <- as.Date("2022-10-01")

message("Script 17: Precinct Triple-Difference (DDD)")
message("Working directory: ", here())

COL_SHOOTING  <- "#6A0572"
COL_ROBBERY   <- "#003049"
COL_GUNVIOL   <- "#1B998B"
COL_COLLISION <- "#F77F00"

theme_pursuit <- function(base_size = 15) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.15), margin = margin(b = 8)),
      plot.subtitle    = element_text(color = "grey30", size = rel(0.85), margin = margin(b = 12)),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      axis.title.x     = element_text(margin = margin(t = 8), size = rel(0.9)),
      axis.title.y     = element_text(margin = margin(r = 8), size = rel(0.9)),
      axis.text        = element_text(color = "grey30"),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      plot.margin      = margin(15, 15, 15, 15)
    )
}

save_plot <- function(p, name, w = 8, h = 5.5) {
  ggsave(file.path(plot_dir, paste0(name, ".png")), plot = p,
         width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(plot_dir, paste0(name, ".pdf")), plot = p,
         width = w, height = h, bg = "white")
  invisible(p)
}


# 1. Load precinct panels ======================================================

message("\n[1] Loading precinct panels...")

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

message("  Shooting rows: ",     nrow(shootings_pct))
message("  Robbery rows: ",      nrow(robberies_pct))
message("  Gun violence rows: ", nrow(gun_violence_pct))
message("  Collision rows: ",    nrow(collisions_pct))


# 2. Build combined panel + define pre-treatment crime groups ==================
#
# Analysis window: Jan 2018 – Sep 2025 (right-censored to avoid zero-crime months).
# Grouping is based on pre-treatment crime rates (Jan 2018 – Sep 2022), which
# are fully predetermined with respect to the October 2022 policy change.
# Using post-treatment pursuit counts as a grouping variable would be
# endogenous: pursuit counts are themselves outcomes of the policy, and
# precincts with more crime may mechanically generate more pursuits.

message("\n[2] Building panel and defining pre-treatment crime groups...")

# Guard: record expected panel dimensions before full_join.
# A pct/month_date type mismatch (e.g., pct as integer vs character) would
# cause a Cartesian product, inflating rows and biasing all DDD estimates.
n_pcts_expected   <- n_distinct(shootings_pct$pct)
n_months_expected <- n_distinct(shootings_pct$month_date)

panel_raw <- shootings_pct |>
  full_join(robberies_pct,    by = c("pct", "month_date")) |>
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

# Recode Precinct 116 as 105 (consolidated mid-study) and re-aggregate.
# Pct 116 was formally merged into Pct 105; this ensures a stable 77-precinct panel.
panel_raw <- panel_raw |>
  mutate(pct = if_else(pct == 116L, 105L, pct)) |>
  group_by(pct, month_date) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
  arrange(pct, month_date)

# Post-join row count guard: fail loudly if join produced unexpected expansion.
# After the 116→105 recode, the panel has 77 precincts (one fewer than the raw CSVs),
# so the row-count ceiling must be based on the recoded count, not n_pcts_expected.
n_pcts_recoded <- n_pcts_expected - 1L
max_expected   <- n_pcts_recoded * n_months_expected
if (nrow(panel_raw) > max_expected * 1.05) {
  stop("panel_raw row count (", nrow(panel_raw), ") exceeds expected max (",
       max_expected, "). Likely pct or month_date type mismatch between input CSVs.")
}
message("  Panel rows after full_join: ", nrow(panel_raw),
        " (expected <= ", max_expected, ")")

# Pre-treatment groupings (Jan 2018 – Sep 2022 only).
# Each outcome uses its own pre-treatment baseline to avoid cross-outcome
# contamination in the group indicator.

pre_crime_shoot <- panel_raw |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_shoot_pre = mean(shooting_incidents, na.rm = TRUE), .groups = "drop") |>
  mutate(
    high_crime_shoot = as.integer(mean_shoot_pre >= median(mean_shoot_pre)),
    shoot_quartile   = as.integer(cut(mean_shoot_pre,
                                      quantile(mean_shoot_pre, probs = 0:4 / 4, na.rm = TRUE),
                                      include.lowest = TRUE, labels = FALSE))
  )

pre_crime_rob <- panel_raw |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_rob_pre = mean(robbery_count, na.rm = TRUE), .groups = "drop") |>
  mutate(
    high_crime_rob = as.integer(mean_rob_pre >= median(mean_rob_pre)),
    rob_quartile   = as.integer(cut(mean_rob_pre,
                                    quantile(mean_rob_pre, probs = 0:4 / 4, na.rm = TRUE),
                                    include.lowest = TRUE, labels = FALSE))
  )

pre_crime_gunviol <- panel_raw |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_gv_pre = mean(gun_violence_count, na.rm = TRUE), .groups = "drop") |>
  mutate(
    high_crime_gunviol = as.integer(mean_gv_pre >= median(mean_gv_pre)),
    gunviol_quartile   = as.integer(cut(mean_gv_pre,
                                        quantile(mean_gv_pre, probs = 0:4 / 4, na.rm = TRUE),
                                        include.lowest = TRUE, labels = FALSE))
  )

pre_crash <- panel_raw |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_crash_pre = mean(collision_count, na.rm = TRUE), .groups = "drop") |>
  mutate(
    high_crash       = as.integer(mean_crash_pre >= median(mean_crash_pre)),
    crash_quartile   = as.integer(cut(mean_crash_pre,
                                      quantile(mean_crash_pre, probs = 0:4 / 4, na.rm = TRUE),
                                      include.lowest = TRUE, labels = FALSE))
  )

med_shoot  <- median(pre_crime_shoot$mean_shoot_pre)
med_rob    <- median(pre_crime_rob$mean_rob_pre)
med_gv     <- median(pre_crime_gunviol$mean_gv_pre)
med_crash  <- median(pre_crash$mean_crash_pre)

n_high_shoot  <- sum(pre_crime_shoot$high_crime_shoot)
n_low_shoot   <- nrow(pre_crime_shoot) - n_high_shoot
n_high_rob    <- sum(pre_crime_rob$high_crime_rob)
n_low_rob     <- nrow(pre_crime_rob) - n_high_rob
n_high_gv     <- sum(pre_crime_gunviol$high_crime_gunviol)
n_low_gv      <- nrow(pre_crime_gunviol) - n_high_gv
n_high_crash  <- sum(pre_crash$high_crash)
n_low_crash   <- nrow(pre_crash) - n_high_crash

message("  Pre-treatment medians — shoot: ", round(med_shoot, 3),
        " (", n_high_shoot, " high / ", n_low_shoot, " low);",
        " rob: ", round(med_rob, 3),
        " (", n_high_rob, " high / ", n_low_rob, " low);",
        " gv: ", round(med_gv, 3),
        " (", n_high_gv, " high / ", n_low_gv, " low);",
        " crash: ", round(med_crash, 3),
        " (", n_high_crash, " high / ", n_low_crash, " low)")

# Check overlap between shooting and gun violence groupings.
# Use inner_join for correct denominator (matched precincts only).
overlap_gv_shoot <- pre_crime_shoot |>
  select(pct, high_crime_shoot) |>
  inner_join(pre_crime_gunviol |> select(pct, high_crime_gunviol), by = "pct") |>
  summarise(overlap = mean(high_crime_shoot == high_crime_gunviol, na.rm = TRUE)) |>
  pull(overlap)
message("  Gun violence vs. shooting grouping overlap: ",
        round(overlap_gv_shoot * 100, 1), "% of precincts identically classified")

# Join all grouping tables and compute time variables
panel <- panel_raw |>
  left_join(pre_crime_shoot  |> select(pct, high_crime_shoot,  shoot_quartile),   by = "pct") |>
  left_join(pre_crime_rob    |> select(pct, high_crime_rob,    rob_quartile),     by = "pct") |>
  left_join(pre_crime_gunviol|> select(pct, high_crime_gunviol,gunviol_quartile), by = "pct") |>
  left_join(pre_crash        |> select(pct, high_crash,        crash_quartile),   by = "pct") |>
  mutate(
    post     = as.integer(month_date >= INTERVENTION),
    rel_time = (year(month_date)  - year(INTERVENTION)) * 12L +
               (month(month_date) - month(INTERVENTION)),
    # Quarterly bin: floor(rel_time / 3) maps each 3-month block to an integer.
    # bin -1 = Jul–Sep 2022 (reference); bin 0 = Oct–Dec 2022 (first post quarter).
    # Lump all Tisch-reversal months (Jan 2025+, bin >= 9) into a single bin 9.
    time_bin      = floor(rel_time / 3L),
    time_bin_lump = if_else(time_bin >= 9L, 9L, time_bin)
  )

# Guard: every precinct must receive all group assignments.
na_counts <- c(
  shoot   = sum(is.na(panel$high_crime_shoot)),
  rob     = sum(is.na(panel$high_crime_rob)),
  gunviol = sum(is.na(panel$high_crime_gunviol)),
  crash   = sum(is.na(panel$high_crash))
)
if (any(na_counts > 0)) {
  stop("Group indicators contain NAs: ",
       paste(names(na_counts)[na_counts > 0], na_counts[na_counts > 0], sep = "=", collapse = ", "),
       ". Check pre_crime_* for missing precincts.")
}

n_pcts   <- n_distinct(panel$pct)
n_months <- n_distinct(panel$month_date)
message("  Panel: ", n_pcts, " precincts × ", n_months, " months = ", nrow(panel), " obs")


# 2b. Pursuit exposure by pre-treatment crime group (M3 response) ==============
#
# Addresses reviewer request: demonstrate that high-crime precincts had greater
# pursuit exposure post-October 2022 (not just assumed from baseline crime level).
# Joins precinct-level monthly pursuit counts (from script 01) to the panel's
# pre-treatment grouping indicators and summarises mean post-2022 pursuit volume
# by high/low crime group.

message("\n[2b] Computing post-intervention pursuit exposure by crime group...")

pursuit_pct <- read_csv(here("output", "precinct_did", "pct_monthly_pursuit.csv"),
                        show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(
    month_date = as.Date(month_date),
    pct        = if_else(pct == 116L, 105L, as.integer(pct))
  ) |>
  group_by(pct, month_date) |>
  summarise(pursuit_events = sum(pursuit_events, na.rm = TRUE), .groups = "drop")

# Join grouping flags (shooting and robbery) for the post-intervention period
pursuit_by_group <- pursuit_pct |>
  filter(month_date >= INTERVENTION, month_date <= as.Date("2025-12-01")) |>
  left_join(pre_crime_shoot |> select(pct, high_crime_shoot), by = "pct") |>
  left_join(pre_crime_rob   |> select(pct, high_crime_rob),   by = "pct") |>
  filter(!is.na(high_crime_shoot))

pursuit_summary <- bind_rows(
  pursuit_by_group |>
    group_by(group = factor(high_crime_shoot, labels = c("Low shooting", "High shooting"))) |>
    summarise(mean_pursuits = mean(pursuit_events),
              sd_pursuits   = sd(pursuit_events),
              n_pct_months  = n(), .groups = "drop") |>
    mutate(grouping_var = "shooting"),
  pursuit_by_group |>
    group_by(group = factor(high_crime_rob, labels = c("Low robbery", "High robbery"))) |>
    summarise(mean_pursuits = mean(pursuit_events),
              sd_pursuits   = sd(pursuit_events),
              n_pct_months  = n(), .groups = "drop") |>
    mutate(grouping_var = "robbery")
) |>
  select(grouping_var, group, mean_pursuits, sd_pursuits, n_pct_months)

write_csv(pursuit_summary, here("output", "precinct_did", "pct_pursuit_by_group.csv"))
message("  Saved pct_pursuit_by_group.csv")
message(paste(
  sprintf("  %-22s mean = %.1f pursuits/month (SD = %.1f)",
          as.character(pursuit_summary$group),
          pursuit_summary$mean_pursuits,
          pursuit_summary$sd_pursuits),
  collapse = "\n"
))


# 3. DDD scalar estimates ======================================================
#
# Three specifications for each outcome:
#   (a) Median split: post × high_crime — the headline DDD estimate
#   (b) Quartile linear: post × crime_quartile — dose-response, one coefficient
#   (c) Quartile per-group: post:factor(crime_quartile) — four coefficients for dot plot
#
# post × high_crime (or post × crime_quartile) is the identified DDD coefficient:
# excess post-policy change in high-crime precincts relative to low-crime precincts,
# net of precinct baselines (pct FE) and city-wide monthly shocks (month_date FE).
# Note: post and high_crime/crime_quartile are each absorbed individually by their
# respective fixed effects, so only the interaction term is identified — as intended.

message("\n[3] Estimating DDD scalar models (4 outcomes)...")

# (a) Median split
# Written as explicit interaction post:high_crime_* (not post*high_crime_*) because
# post is absorbed by month_date FE and high_crime_* is absorbed by pct FE; only
# the interaction term survives. Explicit : avoids any ambiguity about the surviving
# term name that * expansion + silent absorption can create.
fit_shoot_ddd   <- feols(shooting_incidents ~ post:high_crime_shoot   | pct + month_date,
                         data = panel, cluster = ~pct)
fit_rob_ddd     <- feols(robbery_count      ~ post:high_crime_rob     | pct + month_date,
                         data = panel, cluster = ~pct)
fit_gv_ddd      <- feols(gun_violence_count ~ post:high_crime_gunviol | pct + month_date,
                         data = panel, cluster = ~pct)
fit_crash_ddd   <- feols(collision_count    ~ post:high_crash         | pct + month_date,
                         data = panel, cluster = ~pct)

# (b) Quartile linear dose-response
fit_shoot_qddd  <- feols(shooting_incidents ~ post:shoot_quartile   | pct + month_date,
                         data = panel, cluster = ~pct)
fit_rob_qddd    <- feols(robbery_count      ~ post:rob_quartile     | pct + month_date,
                         data = panel, cluster = ~pct)
fit_gv_qddd     <- feols(gun_violence_count ~ post:gunviol_quartile | pct + month_date,
                         data = panel, cluster = ~pct)
fit_crash_qddd  <- feols(collision_count    ~ post:crash_quartile   | pct + month_date,
                         data = panel, cluster = ~pct)

# (c) Quartile per-group (Q1 reference), for dot-plot visualization.
# Use fixest's i() to create exactly 3 non-reference interaction columns (Q2, Q3, Q4),
# guaranteeing Q1 (lowest crime) is the reference. This avoids the rank-detection
# conflict where post:factor() + full time FEs causes fixest to drop Q4 instead of Q1
# (post is a linear combination of the monthly time dummies, making 4 quartile×post
# terms rank-1-deficient; i() sidesteps this by never creating the Q1 column).
# Coefficients for Q2, Q3, Q4 show the dose-response increment relative to the
# quietest precincts — the natural comparison for the deterrence hypothesis.
fit_shoot_qpg   <- feols(shooting_incidents ~ i(shoot_quartile,   post, ref = 1) | pct + month_date,
                         data = panel, cluster = ~pct)
fit_rob_qpg     <- feols(robbery_count      ~ i(rob_quartile,     post, ref = 1) | pct + month_date,
                         data = panel, cluster = ~pct)
fit_gv_qpg      <- feols(gun_violence_count ~ i(gunviol_quartile, post, ref = 1) | pct + month_date,
                         data = panel, cluster = ~pct)
fit_crash_qpg   <- feols(collision_count    ~ i(crash_quartile,   post, ref = 1) | pct + month_date,
                         data = panel, cluster = ~pct)

# Extract one coefficient by exact term name (must be a single match)
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

# Extract per-group quartile coefficients.
# With i(quartile_var, post, ref = 1), fixest creates exactly 3 interaction columns
# (Q2, Q3, Q4) from the start, so Q1 is guaranteed to be the reference without any
# rank-detection conflict. Term names follow fixest's i() convention:
# "<quartile_var>::<level>:post" (e.g., "shoot_quartile::2:post").
# Add a Q1 reference row with estimate = 0 so the dot plot shows all four levels.
extract_quartile_pg <- function(fit, outcome_label, quartile_var) {
  ct  <- summary(fit)$coeftable
  idx <- grep(paste0("^", quartile_var, "::"), rownames(ct))

  if (length(idx) != 3L) {
    stop("Expected 3 i() interaction rows (Q2-Q4, with Q1 as reference); found ",
         length(idx), ".\n",
         "Available coeftable rows: ", paste(rownames(ct), collapse = ", "), "\n",
         "Check model formula, factor levels, and fixest version.")
  }

  estimated <- tibble(
    outcome      = outcome_label,
    spec         = "quartile_pergroup",
    term         = rownames(ct)[idx],
    estimate     = ct[idx, "Estimate"],
    std_error    = ct[idx, "Std. Error"],
    t_stat       = ct[idx, "t value"],
    p_value      = ct[idx, "Pr(>|t|)"],
    nobs         = fit$nobs,
    n_precincts  = n_pcts,
    is_reference = FALSE
  )

  # Q1 is the explicit reference (i() ref = 1); define its coefficient as 0.
  # Term name mirrors the i() convention so str_extract("(?<=::)\\d+") → "1".
  ref_row <- tibble(
    outcome      = outcome_label,
    spec         = "quartile_pergroup",
    term         = paste0(quartile_var, "::1:post"),
    estimate     = 0,
    std_error    = NA_real_,
    t_stat       = NA_real_,
    p_value      = NA_real_,
    nobs         = fit$nobs,
    n_precincts  = n_pcts,
    is_reference = TRUE
  )

  bind_rows(estimated, ref_row)
}

ddd_results <- bind_rows(
  # Median split — term name from explicit : interaction
  extract_coef(fit_shoot_ddd,  "shooting_incidents", "median", "post:high_crime_shoot"),
  extract_coef(fit_rob_ddd,    "robbery_count",      "median", "post:high_crime_rob"),
  extract_coef(fit_gv_ddd,     "gun_violence_count", "median", "post:high_crime_gunviol"),
  extract_coef(fit_crash_ddd,  "collision_count",    "median", "post:high_crash"),
  # Quartile linear
  extract_coef(fit_shoot_qddd, "shooting_incidents", "quartile_linear", "post:shoot_quartile"),
  extract_coef(fit_rob_qddd,   "robbery_count",      "quartile_linear", "post:rob_quartile"),
  extract_coef(fit_gv_qddd,    "gun_violence_count", "quartile_linear", "post:gunviol_quartile"),
  extract_coef(fit_crash_qddd, "collision_count",    "quartile_linear", "post:crash_quartile"),
  # Quartile per-group (Q1 reference row added inside extract_quartile_pg)
  extract_quartile_pg(fit_shoot_qpg, "shooting_incidents", "shoot_quartile"),
  extract_quartile_pg(fit_rob_qpg,   "robbery_count",      "rob_quartile"),
  extract_quartile_pg(fit_gv_qpg,    "gun_violence_count", "gunviol_quartile"),
  extract_quartile_pg(fit_crash_qpg, "collision_count",    "crash_quartile")
)

write_csv(ddd_results, here("output", "precinct_did", "pct_ddd_results.csv"))
s_ddd  <- ddd_results |> filter(outcome == "shooting_incidents", spec == "median")
r_ddd  <- ddd_results |> filter(outcome == "robbery_count",      spec == "median")
gv_ddd <- ddd_results |> filter(outcome == "gun_violence_count", spec == "median")
cr_ddd <- ddd_results |> filter(outcome == "collision_count",    spec == "median")
# Quartile linear estimates are re-extracted inside plot_quartile() from ddd_df directly.

message("\n[3] DDD median estimates:",
        " shoot: b=", round(s_ddd$estimate, 3),  " p=", round(s_ddd$p_value, 3),
        "; rob: b=",  round(r_ddd$estimate, 3),  " p=", round(r_ddd$p_value, 3),
        "; gv: b=",   round(gv_ddd$estimate, 3), " p=", round(gv_ddd$p_value, 3),
        "; crash: b=",round(cr_ddd$estimate, 3), " p=", round(cr_ddd$p_value, 3),
        " | saved: pct_ddd_results.csv (", nrow(ddd_results), " rows)")


# 4. Event study estimates =====================================================
#
# Model: outcome ~ i(rel_time, high_crime, ref = -1) | pct + month_date, cluster = ~pct
#
# Event study uses the median-split indicator (the headline DDD specification).
# i(rel_time, high_crime, ref = -1) produces a coefficient for each
# rel_time × high_crime combination, referenced at t = -1.
# Pre-period coefficients ≈ 0 = parallel trends passes.
# Window restricted to -24 to +35 for readability.

message("\n[4] Estimating event study models (median split, 4 outcomes)...")

# rel_time >= -24 → time_bin = floor(-24/3) = -8 (leftmost displayed bin, Oct 2020).
# rel_time <= 35  → time_bin = floor(35/3) = 11, lumped to bin 9 (Jan 2025+).
panel_es <- panel |>
  filter(rel_time >= -24, rel_time <= 35)

fit_shoot_es <- feols(
  shooting_incidents ~ i(time_bin_lump, high_crime_shoot,   ref = -1) | pct + month_date,
  data = panel_es, cluster = ~pct
)
fit_rob_es <- feols(
  robbery_count      ~ i(time_bin_lump, high_crime_rob,     ref = -1) | pct + month_date,
  data = panel_es, cluster = ~pct
)
fit_gv_es <- feols(
  gun_violence_count ~ i(time_bin_lump, high_crime_gunviol, ref = -1) | pct + month_date,
  data = panel_es, cluster = ~pct
)
fit_crash_es <- feols(
  collision_count    ~ i(time_bin_lump, high_crash,         ref = -1) | pct + month_date,
  data = panel_es, cluster = ~pct
)

# Extract event-study coefficients directly from summary()$coeftable —
# avoids relying on iplot()'s undocumented $prms internal slot. Consistent
# with project MEMORY.md: "p-values: ALWAYS read from summary(fit)$coeftable".
# CIs use fixest's confint() (t-distribution with G-1 df) rather than ±1.96
# (normal approximation), so reported 95% CIs are consistent with cluster-robust
# inference at G = 77.
tidy_es <- function(fit, outcome_label) {
  ct  <- summary(fit)$coeftable
  idx <- grep("^time_bin_lump::", rownames(ct))
  ci  <- confint(fit, level = 0.95)
  tibble(
    outcome   = outcome_label,
    time_bin  = as.integer(stringr::str_extract(rownames(ct)[idx], "(?<=::)-?\\d+")),
    estimate  = ct[idx, "Estimate"],
    std_error = ct[idx, "Std. Error"],
    conf_low  = ci[rownames(ct)[idx], 1],
    conf_high = ci[rownames(ct)[idx], 2]
  )
}

es_shoot <- tidy_es(fit_shoot_es, "Shooting incidents")
es_rob   <- tidy_es(fit_rob_es,   "Robbery")
es_gv    <- tidy_es(fit_gv_es,    "Gun violence")
es_crash <- tidy_es(fit_crash_es, "Pursuit crashes")

write_csv(es_shoot, here("output", "precinct_did", "pct_ddd_es_shooting.csv"))
write_csv(es_rob,   here("output", "precinct_did", "pct_ddd_es_robbery.csv"))
write_csv(es_gv,    here("output", "precinct_did", "pct_ddd_es_gun_violence.csv"))
write_csv(es_crash, here("output", "precinct_did", "pct_ddd_es_crashes.csv"))

pre_shoot_sig <- es_shoot |> filter(time_bin < -1) |>
  summarise(pct_sig = mean(conf_low > 0 | conf_high < 0) * 100) |> pull(pct_sig)
pre_rob_sig   <- es_rob   |> filter(time_bin < -1) |>
  summarise(pct_sig = mean(conf_low > 0 | conf_high < 0) * 100) |> pull(pct_sig)
pre_gv_sig    <- es_gv    |> filter(time_bin < -1) |>
  summarise(pct_sig = mean(conf_low > 0 | conf_high < 0) * 100) |> pull(pct_sig)
pre_crash_sig <- es_crash |> filter(time_bin < -1) |>
  summarise(pct_sig = mean(conf_low > 0 | conf_high < 0) * 100) |> pull(pct_sig)

message("\n[4] Event study pre-trend %sig: shoot=", round(pre_shoot_sig, 1),
        "% rob=", round(pre_rob_sig, 1),
        "% gv=", round(pre_gv_sig, 1),
        "% crash=", round(pre_crash_sig, 1), "%")

# Joint Wald tests for pre-trends (M6) ----------------------------------------
# H0: all pre-treatment interaction coefficients are jointly zero.
# Pre-treatment bins: time_bin_lump < -1 (bins -8 through -2, Oct 2020–Jun 2022).
# Replaces per-coefficient significance counting with a single joint chi-square test.

run_wald_pre <- function(fit, outcome_label) {
  all_names <- names(coef(fit))
  # Pre-treatment bins have time_bin_lump < -1; extract bin value from term name
  is_pre <- grepl("^time_bin_lump::", all_names) &
    (as.integer(stringr::str_extract(all_names, "(?<=::)-?\\d+")) < -1)
  pre_names <- all_names[is_pre]

  if (length(pre_names) == 0) {
    return(tibble(outcome = outcome_label, n_pre = 0L, chi2 = NA_real_,
                  df = NA_integer_, p_joint = NA_real_))
  }

  w <- fixest::wald(fit, keep = pre_names)
  # fixest::wald() returns an F-statistic (not chi-square) for cluster-robust SEs.
  # df2 = G - 1 = 76 (77 precincts). Column kept as "chi2" for backward compatibility.
  tibble(
    outcome = outcome_label,
    n_pre   = length(pre_names),
    chi2    = round(w[["stat"]], 3),   # F-statistic; mislabeled for legacy reasons
    df      = as.integer(w[["df1"]]),  # numerator df
    df2     = 76L,                     # denominator df = G - 1
    p_joint = round(w[["p"]], 4)
  )
}

wald_pre <- bind_rows(
  run_wald_pre(fit_shoot_es, "Shooting incidents"),
  run_wald_pre(fit_rob_es,   "Robbery"),
  run_wald_pre(fit_gv_es,    "Gun violence"),
  run_wald_pre(fit_crash_es, "Pursuit crashes")
)

write_csv(wald_pre, here("output", "precinct_did", "pct_wald_pre_trends.csv"))
message("\n[4b] Joint pre-trend Wald tests (H0: all pre-treatment leads = 0):")
message(paste(
  sprintf("  %-22s chi2(%.0f) = %.3f, p = %.4f",
          wald_pre$outcome, wald_pre$df, wald_pre$chi2, wald_pre$p_joint),
  collapse = "\n"
))


# 5. Figures ===================================================================
#
# Four figures:
#   (a) fig_precinct_ddd_shooting     — median-split event study, shooting
#   (b) fig_precinct_ddd_robbery      — median-split event study, robbery
#   (c) fig_precinct_ddd_quartile_shooting — quartile dot plot, shooting
#   (d) fig_precinct_ddd_quartile_robbery  — quartile dot plot, robbery

message("\n[5] Creating figures...")

# ---- 5a/5b: Median-split event study plots -----------------------------------

plot_es <- function(es_df, outcome_label, col_fill, col_line,
                    ddd_b, ddd_se, ddd_p, fig_name) {

  p_fmt <- if (ddd_p < .001) "< .001" else sprintf("= %.3f", ddd_p)

  # Quarterly bin labels: bin -1 (Jul–Sep 2022) is the reference; bin 9 = Jan 2025+
  bin_labels <- c(
    "-8" = "Oct–Dec\n2020", "-7" = "Jan–Mar\n2021",
    "-6" = "Apr–Jun\n2021", "-5" = "Jul–Sep\n2021",
    "-4" = "Oct–Dec\n2021", "-3" = "Jan–Mar\n2022",
    "-2" = "Apr–Jun\n2022", "-1" = "Jul–Sep\n2022\n(ref)",
    "0"  = "Oct–Dec\n2022", "1"  = "Jan–Mar\n2023",
    "2"  = "Apr–Jun\n2023", "3"  = "Jul–Sep\n2023",
    "4"  = "Oct–Dec\n2023", "5"  = "Jan–Mar\n2024",
    "6"  = "Apr–Jun\n2024", "7"  = "Jul–Sep\n2024",
    "8"  = "Oct–Dec\n2024", "9"  = "Jan 2025+"  # Tisch reversal = Feb 2025; Jan 2025 also in this bin (see caption)
  )

  # Insert an explicit reference row at bin -1 (estimate = 0 by construction)
  # so geom_line draws a continuous path through the intervention point rather
  # than visually jumping from bin -2 to bin 0 across the labeled break.
  es_df <- bind_rows(
    es_df,
    tibble(outcome = es_df$outcome[1], time_bin = -1L,
           estimate = 0, std_error = NA_real_,
           conf_low = 0, conf_high = 0)
  ) |>
    arrange(time_bin) |>
    mutate(period = if_else(time_bin < 0, "Pre", "Post"))

  p <- ggplot(es_df, aes(x = time_bin, y = estimate,
                          fill  = period, color = period,
                          shape = period)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.4) +
    geom_vline(xintercept = -0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.5) +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.5) +
    scale_color_manual(values = c("Pre" = "grey50", "Post" = col_line)) +
    scale_fill_manual( values = c("Pre" = "grey50", "Post" = col_fill)) +
    scale_shape_manual(values = c("Pre" = 1, "Post" = 16)) +
    # Name-based lookup (immune to label ordering): function looks up each
    # break in bin_labels by name, so reordering or extending bin_labels
    # won't silently misalign x-axis labels.
    scale_x_continuous(breaks = -8:9,
                       labels = function(x) bin_labels[as.character(x)],
                       guide  = guide_axis(n.dodge = 2)) +
    annotate("text", x = -0.5, y = Inf,
             label = "Oct 2022", vjust = 2, hjust = 1.1,
             size = 4.0, color = "grey40") +
    labs(
      title    = paste0("DiD event study: ", outcome_label),
      subtitle = sprintf(
        "High vs. low pre-treatment crime precincts | DiD: b = %.3f, SE = %.3f, p %s",
        ddd_b, ddd_se, p_fmt
      ),
      x = "Quarter relative to October 2022 policy change",
      y = paste("Estimated difference\n(high minus low pre-treatment crime precincts)")
    ) +
    theme_pursuit() +
    theme(
      legend.position = "none",
      axis.text.x     = element_text(size = 11),
      axis.title      = element_text(size = 14),
      plot.subtitle   = element_text(color = "grey30", size = 14)
    )

  save_plot(p, fig_name, w = 12, h = 6.5)
  message("  Saved: ", fig_name, ".png/.pdf")
  invisible(p)
}

p_ddd_s <- plot_es(
  es_df         = es_shoot,
  outcome_label = "Shooting Incidents",
  col_fill      = COL_SHOOTING,
  col_line      = COL_SHOOTING,
  ddd_b         = s_ddd$estimate,
  ddd_se        = s_ddd$std_error,
  ddd_p         = s_ddd$p_value,
  fig_name      = "fig_precinct_ddd_shooting"
)

p_ddd_r <- plot_es(
  es_df         = es_rob,
  outcome_label = "Robbery",
  col_fill      = COL_ROBBERY,
  col_line      = COL_ROBBERY,
  ddd_b         = r_ddd$estimate,
  ddd_se        = r_ddd$std_error,
  ddd_p         = r_ddd$p_value,
  fig_name      = "fig_precinct_ddd_robbery"
)

p_ddd_gv <- plot_es(
  es_df         = es_gv,
  outcome_label = "Gun Violence",
  col_fill      = COL_GUNVIOL,
  col_line      = COL_GUNVIOL,
  ddd_b         = gv_ddd$estimate,
  ddd_se        = gv_ddd$std_error,
  ddd_p         = gv_ddd$p_value,
  fig_name      = "fig_precinct_ddd_gun_violence"
)

p_ddd_c <- plot_es(
  es_df         = es_crash,
  outcome_label = "Pursuit Crashes",
  col_fill      = COL_COLLISION,
  col_line      = COL_COLLISION,
  ddd_b         = cr_ddd$estimate,
  ddd_se        = cr_ddd$std_error,
  ddd_p         = cr_ddd$p_value,
  fig_name      = "fig_precinct_ddd_crashes"
)

# ---- Combined 4-panel event-study figure (manuscript Figure) ----------------
# Replaces the previous magick::image_trim assembly in the .qmd (which cropped
# panel titles). One patchwork composite, one shared caption; the .qmd chunk
# just includes this PNG. Row order matches the caption: shooting / gun violence
# on top, robbery / pursuit crashes on the bottom.

slim_panel <- function(p) {
  p +
    labs(x = NULL, y = NULL) +
    theme(
      plot.title    = element_text(size = rel(0.95)),
      plot.subtitle = element_text(size = rel(0.72), color = "grey30"),
      axis.text.x   = element_text(size = 8),
      axis.text.y   = element_text(size = 8),
      plot.margin   = margin(6, 8, 6, 6)
    )
}

ddd_combined <- (slim_panel(p_ddd_s) | slim_panel(p_ddd_gv)) /
                (slim_panel(p_ddd_r) | slim_panel(p_ddd_c)) +
  plot_annotation(
    caption = paste0(
      "Points: quarterly difference-in-differences coefficients (high minus low ",
      "pre-treatment crime precincts), relative to bin -1 (Jul-Sep 2022).\n",
      "Bands: 95% confidence intervals. Standard errors clustered by precinct ",
      "(G = 77). Dashed vertical line: October 2022 policy change.\n",
      "Pre-period coefficients departing from zero for the crime outcomes ",
      "indicate non-parallel pre-trends; the collision panel is consistent with ",
      "parallel trends."
    )
  ) &
  theme(plot.caption = element_text(color = "grey45", size = 8, hjust = 0))

ggsave(file.path(plot_dir, "fig_precinct_ddd_combined.png"), ddd_combined,
       width = 13, height = 9, dpi = 300, bg = "white")
ggsave(file.path(plot_dir, "fig_precinct_ddd_combined.pdf"), ddd_combined,
       width = 13, height = 9, bg = "white")
message("  Saved: fig_precinct_ddd_combined.png/.pdf")

# ---- 5c/5d: Quartile dose-response dot plots ---------------------------------
# Visualize the 4 per-group post coefficients from the factor specification.
# Each dot = estimated post-treatment change for that quartile (relative to
# within-precinct + within-month baseline); error bars = 95% CIs.

plot_quartile <- function(ddd_df, outcome_label, col, fig_name) {
  # Parse per-group rows; add CIs from ±1.96 approximation is intentionally
  # NOT used here — instead we carry std_error and note Q4 is reference (no CI).
  df <- ddd_df |>
    filter(outcome == !!outcome_label, spec == "quartile_pergroup") |>
    mutate(
      # i() term names: "<var>::<level>:post"; extract the level number after "::"
      quartile  = as.integer(stringr::str_extract(term, "(?<=::)\\d+")),
      # CI: use t(G-1) critical value for G=77 clusters. Qt(0.975, 76) ≈ 1.992.
      # For the reference row (is_reference=TRUE), std_error=NA → CI is NA.
      conf_low  = estimate - qt(0.975, df = 76) * std_error,
      conf_high = estimate + qt(0.975, df = 76) * std_error,
      sig = case_when(
        is_reference       ~ "Reference (Q1)",
        p_value < .05      ~ "p < .05",
        TRUE               ~ "p \u2265 .05"
      )
    )

  lin     <- ddd_df |> filter(outcome == !!outcome_label, spec == "quartile_linear")
  p_fmt_lin <- if (lin$p_value < .001) "< .001" else sprintf("= %.3f", lin$p_value)

  p <- ggplot(df, aes(x = estimate, y = factor(quartile, levels = 4:1),
                      color = sig, shape = sig)) +
    geom_vline(xintercept = 0, color = "grey50", linewidth = 0.4) +
    # CI bars only for non-reference rows
    geom_errorbarh(
      data   = filter(df, !is_reference),
      aes(xmin = conf_low, xmax = conf_high),
      height = 0.2, linewidth = 0.7
    ) +
    geom_point(size = 3) +
    scale_y_discrete(
      labels = c("4" = "Quartile 4\n(highest crime)",
                 "3" = "Quartile 3",
                 "2" = "Quartile 2",
                 "1" = "Quartile 1\n(lowest crime, ref.)")
    ) +
    scale_color_manual(
      values = c("Reference (Q1)" = "grey70",
                 "p < .05"        = col,
                 "p \u2265 .05"   = "grey50")
    ) +
    scale_shape_manual(
      values = c("Reference (Q1)" = 5,
                 "p < .05"        = 16,
                 "p \u2265 .05"   = 1)
    ) +
    labs(
      title    = paste0("DDD Quartile Dose-Response: ", outcome_label),
      subtitle = sprintf(
        "Post-treatment change by pre-treatment crime quartile | Linear: b = %.3f, SE = %.3f, p %s",
        lin$estimate, lin$std_error, p_fmt_lin
      ),
      x = "Estimated post-treatment change relative to Q1\n(within-precinct + within-month baseline)",
      y = "Pre-treatment crime quartile",
      color = NULL, shape = NULL
    ) +
    theme_pursuit() +
    theme(
      axis.text.y     = element_text(lineheight = 1.1),
      legend.position = "none"
    )

  save_plot(p, fig_name, w = 8, h = 4.5)
  message("  Saved: ", fig_name, ".png/.pdf")
  invisible(p)
}

plot_quartile(ddd_results, "shooting_incidents",
              col      = COL_SHOOTING,
              fig_name = "fig_precinct_ddd_quartile_shooting")

plot_quartile(ddd_results, "robbery_count",
              col      = COL_ROBBERY,
              fig_name = "fig_precinct_ddd_quartile_robbery")


# 6. Export summary RDS ========================================================

message("\n[6] Saving model objects...")

saveRDS(
  list(
    # Shooting
    fit_ddd_shooting         = fit_shoot_ddd,
    fit_qddd_shooting        = fit_shoot_qddd,
    fit_qpg_shooting         = fit_shoot_qpg,
    fit_es_shooting          = fit_shoot_es,
    pre_crime_shoot          = pre_crime_shoot,
    median_shoot_pre         = med_shoot,
    n_high_shoot             = n_high_shoot,
    n_low_shoot              = n_low_shoot,
    # Robbery
    fit_ddd_robbery          = fit_rob_ddd,
    fit_qddd_robbery         = fit_rob_qddd,
    fit_qpg_robbery          = fit_rob_qpg,
    fit_es_robbery           = fit_rob_es,
    pre_crime_rob            = pre_crime_rob,
    median_rob_pre           = med_rob,
    n_high_rob               = n_high_rob,
    n_low_rob                = n_low_rob,
    # Gun violence
    fit_ddd_gun_violence     = fit_gv_ddd,
    fit_qddd_gun_violence    = fit_gv_qddd,
    fit_qpg_gun_violence     = fit_gv_qpg,
    fit_es_gun_violence      = fit_gv_es,
    pre_crime_gunviol        = pre_crime_gunviol,
    median_gv_pre            = med_gv,
    n_high_gv                = n_high_gv,
    n_low_gv                 = n_low_gv,
    overlap_gv_shoot_pct     = round(overlap_gv_shoot * 100, 1),
    # Crashes
    fit_ddd_crashes          = fit_crash_ddd,
    fit_qddd_crashes         = fit_crash_qddd,
    fit_qpg_crashes          = fit_crash_qpg,
    fit_es_crashes           = fit_crash_es,
    pre_crash                = pre_crash,
    median_crash_pre         = med_crash,
    n_high_crash             = n_high_crash,
    n_low_crash              = n_low_crash
  ),
  here("output", "precinct_did", "pct_ddd_models.rds")
)
message("  Saved: pct_ddd_models.rds (4 outcomes)")

message("\nScript 17 complete.")
message("Outputs: ", pct_dir)
message("Figures: ", plot_dir)
