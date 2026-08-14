# ==============================================================================
# Vehicle Pursuit Policy Analysis — Script 23: Synthetic DiD (Precinct Panel)
# ==============================================================================
#
# Author:   John Hall & Justin Nix
# Source:   Arkhangelsky, D., Athey, S., Hirshberg, D. A., Imbens, G. W., &
#           Wager, S. (2021). Synthetic Difference-in-Differences. American
#           Economic Review, 111(12), 4088-4118.
#
# Purpose:
#   Apply the synthetic difference-in-differences (SDID) estimator to the
#   77-precinct panel used in script 17. SDID reweights both control units
#   (low-crime precincts) and pre-treatment time periods to construct a more
#   plausible counterfactual than standard TWFE. This addresses non-parallel
#   pre-trends — a known issue for the shooting outcome (Wald F = 5.34).
#
#   Design:
#     - Treated units: high-crime precincts (pre-treatment median split, same
#       as script 17's high_crime_shoot / high_crime_rob)
#     - Control units: low-crime precincts
#     - Treatment onset: October 2022 (Maddrey escalation)
#
#   For each outcome (shootings, robberies), estimates three estimators from
#   the synthdid package:
#     (1) SDID: synthdid_estimate() — reweights units AND time
#     (2) SC:   sc_estimate()       — reweights units only (like Abadie SCM)
#     (3) DiD:  did_estimate()      — uniform weights (like TWFE)
#
# Inputs:
#   - output/precinct_did/pct_monthly_shootings.csv  (from script 01)
#   - output/precinct_did/pct_monthly_robberies.csv  (from script 01)
#
# Outputs (saved to output/synthdid_results/):
#   - synthdid_att.csv                 — ATT estimates across estimators
#   - synthdid_unit_weights_*.csv      — precinct weights per outcome
#   - synthdid_time_weights_*.csv      — pre-period time weights
#   - synthdid_trajectory_*.png/pdf    — treated vs. synthetic control
#   - synthdid_weights_*.png/pdf       — weight visualizations
#   - synthdid_robustness.rds          — fitted objects for reuse
#
# Runtime: ~2-5 min (placebo variance estimation is the bottleneck)
#
# Requires: synthdid (not on CRAN — install once via
#   remotes::install_github("synth-inference/synthdid") before running)
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(lubridate)
  library(fixest)
  library(janitor)
  library(synthdid)
})

set.seed(20241001)

message("=== Script 23: Synthetic DiD (77-Precinct Panel) ===")
message("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M"))

INTERVENTION <- as.Date("2022-10-01")

results_dir <- here("output", "synthdid_results")
plot_dir    <- here("output", "plots")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir,    showWarnings = FALSE, recursive = TRUE)

COL_SHOOTING  <- "#6A0572"
COL_ROBBERY   <- "#003049"
COL_COUNTER   <- "#D62828"

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
      legend.position  = "bottom",
      legend.title     = element_blank(),
      plot.margin      = margin(15, 15, 15, 15)
    )
}

save_plot <- function(p, name, w = 7, h = 5) {
  ggsave(file.path(results_dir, paste0(name, ".png")), plot = p,
         width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(results_dir, paste0(name, ".pdf")), plot = p,
         width = w, height = h, bg = "white")
  message("Saved: ", name, ".png / .pdf")
}


# ==============================================================================
# 1. Load Precinct Panels (same as script 17)
# ==============================================================================

message("--- 1. Loading precinct panels ---")

shootings_pct <- read_csv(here("output", "precinct_did", "pct_monthly_shootings.csv"),
                           show_col_types = FALSE) |>
  clean_names() |>
  mutate(month_date = as.Date(month_date))

robberies_pct <- read_csv(here("output", "precinct_did", "pct_monthly_robberies.csv"),
                           show_col_types = FALSE) |>
  clean_names() |>
  mutate(month_date = as.Date(month_date))

message("  Shooting rows: ", nrow(shootings_pct))
message("  Robbery rows:  ", nrow(robberies_pct))


# ==============================================================================
# 2. Build Combined Panel (mirrors script 17 exactly)
# ==============================================================================

message("--- 2. Building 77-precinct balanced panel ---")

build_panel <- function(df, outcome_col) {
  df |>
    filter(pct >= 1, pct <= 123,
           month_date >= as.Date("2018-01-01"),
           month_date <= as.Date("2025-12-01")) |>
    group_by(pct) |>
    filter(any(.data[[outcome_col]] > 0)) |>
    ungroup() |>
    mutate(pct = if_else(pct == 116L, 105L, pct)) |>
    group_by(pct, month_date) |>
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
    arrange(pct, month_date)
}

panel_shoot <- build_panel(shootings_pct, "shooting_incidents")
panel_rob   <- build_panel(robberies_pct, "robbery_count")

message("  Shooting panel: ", n_distinct(panel_shoot$pct), " precincts x ",
        n_distinct(panel_shoot$month_date), " months")
message("  Robbery panel:  ", n_distinct(panel_rob$pct), " precincts x ",
        n_distinct(panel_rob$month_date), " months")


# ==============================================================================
# 3. Define Treatment Groups (pre-treatment median split)
# ==============================================================================

message("--- 3. Defining treatment groups ---")

pre_shoot_means <- panel_shoot |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_pre = mean(shooting_incidents, na.rm = TRUE), .groups = "drop") |>
  mutate(high_crime = as.integer(mean_pre >= median(mean_pre)))

pre_rob_means <- panel_rob |>
  filter(month_date < INTERVENTION) |>
  group_by(pct) |>
  summarise(mean_pre = mean(robbery_count, na.rm = TRUE), .groups = "drop") |>
  mutate(high_crime = as.integer(mean_pre >= median(mean_pre)))

message("  Shooting: ", sum(pre_shoot_means$high_crime), " high-crime, ",
        sum(!pre_shoot_means$high_crime), " low-crime precincts")
message("  Robbery:  ", sum(pre_rob_means$high_crime), " high-crime, ",
        sum(!pre_rob_means$high_crime), " low-crime precincts")


# ==============================================================================
# 4. Build SDID Matrices
# ==============================================================================

message("--- 4. Building SDID matrices ---")

build_sdid_matrix <- function(panel, grouping, outcome_col, label) {
  panel_g <- panel |>
    left_join(grouping |> select(pct, high_crime), by = "pct") |>
    mutate(
      unit       = as.character(pct),
      time_index = as.integer(factor(month_date)),
      W          = as.integer(high_crime == 1L & month_date >= INTERVENTION)
    )

  # Time map for later plotting
  time_map <- panel_g |>
    distinct(month_date, time_index) |>
    arrange(time_index)

  # synthdid::panel.matrices needs a plain data.frame (tibble indexing breaks
  # its internal length(unique(panel[, col])) check)
  sdid_df <- panel_g |>
    select(unit, time_index, Y = !!sym(outcome_col), W) |>
    as.data.frame()

  # Ensure balanced panel: drop precincts with missing months
  expected_months <- n_distinct(sdid_df$time_index)
  complete_pcts <- sdid_df |>
    count(unit) |>
    filter(n == expected_months) |>
    pull(unit)

  sdid_df <- sdid_df |> filter(unit %in% complete_pcts)

  n_treated <- sdid_df |> filter(W == 1) |> distinct(unit) |> nrow()
  n_control <- sdid_df |> filter(!unit %in% (sdid_df |> filter(W == 1) |> distinct(unit) |> pull(unit))) |> distinct(unit) |> nrow()

  setup <- panel.matrices(sdid_df, unit = 1, time = 2, outcome = 3, treatment = 4)

  message("  ", label, ": ", n_control, " control + ", n_treated,
          " treated precincts x ", expected_months, " months")
  message("    N0 = ", setup$N0, " | T0 = ", setup$T0)

  list(setup = setup, time_map = time_map, panel = panel_g,
       n_control = n_control, n_treated = n_treated)
}

shoot_sdid <- build_sdid_matrix(panel_shoot, pre_shoot_means,
                                 "shooting_incidents", "Shootings")
rob_sdid   <- build_sdid_matrix(panel_rob, pre_rob_means,
                                 "robbery_count", "Robberies")


# ==============================================================================
# 5. Estimate SDID, SC, and DiD
# ==============================================================================

message("--- 5. Estimating SDID, SC, and DiD ---")

run_estimators <- function(sdid_obj, label) {
  Y  <- sdid_obj$setup$Y
  N0 <- sdid_obj$setup$N0
  T0 <- sdid_obj$setup$T0

  message("  Running ", label, " estimators...")

  tau_sdid <- synthdid_estimate(Y, N0, T0)
  tau_sc   <- sc_estimate(Y, N0, T0)
  tau_did  <- did_estimate(Y, N0, T0)

  message("    SDID ATT = ", round(tau_sdid, 3))
  message("    SC   ATT = ", round(tau_sc, 3))
  message("    DiD  ATT = ", round(tau_did, 3))

  # Placebo SEs
  message("    Computing placebo SEs...")
  se_sdid <- tryCatch(sqrt(vcov(tau_sdid, method = "placebo")),
                       error = function(e) {
                         message("    Placebo SE failed for SDID, trying jackknife...")
                         tryCatch(sqrt(vcov(tau_sdid, method = "jackknife")),
                                  error = function(e2) NA_real_)
                       })
  se_sc <- tryCatch(sqrt(vcov(tau_sc, method = "placebo")),
                     error = function(e) NA_real_)
  se_did <- tryCatch(sqrt(vcov(tau_did, method = "placebo")),
                      error = function(e) NA_real_)

  list(
    tau_sdid = tau_sdid, tau_sc = tau_sc, tau_did = tau_did,
    se_sdid = se_sdid, se_sc = se_sc, se_did = se_did
  )
}

shoot_results <- run_estimators(shoot_sdid, "Shooting")
rob_results   <- run_estimators(rob_sdid, "Robbery")


# ==============================================================================
# 6. TWFE Comparison (from script 17)
# ==============================================================================

message("--- 6. TWFE comparison from fixest ---")

run_twfe <- function(panel, grouping, outcome_col) {
  df <- panel |>
    left_join(grouping |> select(pct, high_crime), by = "pct") |>
    mutate(
      post = as.integer(month_date >= INTERVENTION),
      treat_post = high_crime * post
    )

  mod <- feols(
    as.formula(paste0(outcome_col, " ~ treat_post | pct + month_date")),
    data    = df,
    cluster = ~pct
  )

  tibble(
    estimator = "TWFE (fixest)",
    att       = coef(mod)["treat_post"],
    se        = se(mod)["treat_post"],
    p_value   = pvalue(mod)["treat_post"]
  )
}

twfe_shoot <- run_twfe(panel_shoot, pre_shoot_means, "shooting_incidents")
twfe_rob   <- run_twfe(panel_rob, pre_rob_means, "robbery_count")

message("  TWFE Shooting ATT = ", round(twfe_shoot$att, 3),
        " (SE = ", round(twfe_shoot$se, 3), ")")
message("  TWFE Robbery ATT  = ", round(twfe_rob$att, 3),
        " (SE = ", round(twfe_rob$se, 3), ")")


# ==============================================================================
# 7. Summary Table
# ==============================================================================

message("--- 7. Building summary table ---")

build_row <- function(estimator, outcome, tau, se_val) {
  att <- as.numeric(tau)
  se_num <- as.numeric(se_val)
  tibble(
    outcome   = outcome,
    estimator = estimator,
    att       = att,
    se        = if (!is.na(se_num)) se_num else NA_real_,
    ci_lower  = if (!is.na(se_num)) att - 1.96 * se_num else NA_real_,
    ci_upper  = if (!is.na(se_num)) att + 1.96 * se_num else NA_real_,
    p_value   = if (!is.na(se_num)) 2 * pnorm(-abs(att / se_num)) else NA_real_
  )
}

sdid_att <- bind_rows(
  build_row("SDID", "Shooting Incidents", shoot_results$tau_sdid, shoot_results$se_sdid),
  build_row("SC",   "Shooting Incidents", shoot_results$tau_sc,   shoot_results$se_sc),
  build_row("DiD",  "Shooting Incidents", shoot_results$tau_did,  shoot_results$se_did),
  twfe_shoot |> mutate(outcome = "Shooting Incidents",
                        ci_lower = att - 1.96 * se, ci_upper = att + 1.96 * se),
  build_row("SDID", "Robberies", rob_results$tau_sdid, rob_results$se_sdid),
  build_row("SC",   "Robberies", rob_results$tau_sc,   rob_results$se_sc),
  build_row("DiD",  "Robberies", rob_results$tau_did,  rob_results$se_did),
  twfe_rob |> mutate(outcome = "Robberies",
                      ci_lower = att - 1.96 * se, ci_upper = att + 1.96 * se)
)

write_csv(sdid_att, file.path(results_dir, "synthdid_att.csv"))
message("Saved: synthdid_att.csv")


# ==============================================================================
# 8. Extract and Save Weights
# ==============================================================================

message("--- 8. Extracting weights ---")

extract_and_save_weights <- function(tau, sdid_obj, outcome_label) {
  slug <- tolower(gsub(" ", "_", outcome_label))

  # Unit weights
  omega <- attr(tau, "weights")$omega
  unit_names <- rownames(sdid_obj$setup$Y)[1:sdid_obj$setup$N0]

  unit_w <- tibble(
    pct    = unit_names,
    weight = as.numeric(omega)
  ) |>
    arrange(desc(weight)) |>
    filter(weight > 0.001)

  write_csv(unit_w, file.path(results_dir, paste0("synthdid_unit_weights_", slug, ".csv")))
  message("  ", outcome_label, ": ", nrow(unit_w),
          " precincts with weight > 0.001 (of ", sdid_obj$n_control, " controls)")

  # Time weights
  lambda <- attr(tau, "weights")$lambda
  time_indices <- 1:sdid_obj$setup$T0

  time_w <- tibble(
    time_index = time_indices,
    weight     = as.numeric(lambda)
  ) |>
    left_join(sdid_obj$time_map, by = "time_index")

  write_csv(time_w, file.path(results_dir, paste0("synthdid_time_weights_", slug, ".csv")))

  list(unit_w = unit_w, time_w = time_w)
}

shoot_weights <- extract_and_save_weights(shoot_results$tau_sdid, shoot_sdid, "Shooting")
rob_weights   <- extract_and_save_weights(rob_results$tau_sdid, rob_sdid, "Robbery")


# ==============================================================================
# 9. Figures
# ==============================================================================

message("--- 9. Generating figures ---")

plot_sdid_trajectory <- function(tau, sdid_obj, weights, outcome_label, col_main) {
  slug <- tolower(gsub(" ", "_", outcome_label))
  Y  <- sdid_obj$setup$Y
  N0 <- sdid_obj$setup$N0
  T0 <- sdid_obj$setup$T0

  n_total <- nrow(Y)
  n_treated <- n_total - N0

  # Average treated outcome
  y_treated <- colMeans(Y[(N0 + 1):n_total, , drop = FALSE])

  # Weighted control outcome
  omega <- attr(tau, "weights")$omega
  y_control <- as.numeric(t(omega) %*% Y[1:N0, ])

  # Adjust level (SDID intercept shift)
  pre_diff  <- mean(y_treated[1:T0]) - mean(y_control[1:T0])
  y_counter <- y_control + pre_diff

  traj <- tibble(
    time_index = 1:ncol(Y),
    treated    = y_treated,
    counter    = y_counter
  ) |>
    left_join(sdid_obj$time_map, by = "time_index")

  att_val <- as.numeric(tau)

  p <- ggplot(traj |> filter(!is.na(month_date)), aes(x = month_date)) +
    geom_line(aes(y = treated, color = "High-Crime Precincts (Treated)"),
              linewidth = 0.9) +
    geom_line(aes(y = counter, color = "Synthetic Control (SDID)"),
              linewidth = 0.9, linetype = "dashed") +
    geom_vline(xintercept = INTERVENTION,
               linetype = "dotted", color = "gray40", linewidth = 0.6) +
    annotate("text", x = INTERVENTION + 60,
             y = max(traj$treated, na.rm = TRUE) * 0.97,
             label = "Oct 2022\n(escalation)",
             hjust = 0, size = 2.8, color = "gray40") +
    scale_color_manual(values = c(
      "High-Crime Precincts (Treated)" = col_main,
      "Synthetic Control (SDID)" = COL_COUNTER
    )) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = paste0("Synthetic DiD: ", outcome_label),
      subtitle = "High-crime precincts (treated) vs. SDID-weighted low-crime precincts",
      x = NULL,
      y = paste("Monthly", outcome_label),
      color = NULL,
      caption = sprintf("ATT = %.2f | %d treated, %d control precincts",
                        att_val, sdid_obj$n_treated, sdid_obj$n_control)
    ) +
    theme_pursuit()

  save_plot(p, paste0("synthdid_trajectory_", slug))
  p
}

plot_sdid_trajectory(shoot_results$tau_sdid, shoot_sdid, shoot_weights,
                     "Shooting Incidents", COL_SHOOTING)
plot_sdid_trajectory(rob_results$tau_sdid, rob_sdid, rob_weights,
                     "Robberies", COL_ROBBERY)

# Estimator comparison dot plot
fig_compare <- sdid_att |>
  filter(!is.na(se)) |>
  mutate(
    estimator = factor(estimator, levels = c("TWFE (fixest)", "DiD", "SC", "SDID"))
  ) |>
  ggplot(aes(x = att, y = estimator, color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray60") +
  geom_pointrange(aes(xmin = ci_lower, xmax = ci_upper),
                  position = position_dodge(width = 0.4), size = 0.5) +
  scale_color_manual(values = c("Shooting Incidents" = COL_SHOOTING,
                                 "Robberies" = COL_ROBBERY)) +
  labs(
    title = "Estimator Comparison: SDID vs. TWFE",
    subtitle = "ATT with 95% CI across estimation methods",
    x = "Average Treatment Effect (monthly count)",
    y = NULL, color = NULL
  ) +
  theme_pursuit()

save_plot(fig_compare, "synthdid_estimator_comparison", w = 7, h = 4)


# ==============================================================================
# 10. Save Objects
# ==============================================================================

saveRDS(
  list(
    shoot_results = shoot_results,
    rob_results   = rob_results,
    shoot_sdid    = shoot_sdid,
    rob_sdid      = rob_sdid,
    twfe_shoot    = twfe_shoot,
    twfe_rob      = twfe_rob,
    shoot_weights = shoot_weights,
    rob_weights   = rob_weights
  ),
  file.path(results_dir, "synthdid_robustness.rds")
)

message("Saved: synthdid_robustness.rds")


# ==============================================================================
# 11. Summary
# ==============================================================================

message("\n=== SDID SUMMARY (77-Precinct Panel) ===")
message("\nShooting Incidents:")
message(sprintf("  SDID: ATT = %.3f (SE = %.3f, p = %.3f)",
                as.numeric(shoot_results$tau_sdid), shoot_results$se_sdid,
                2 * pnorm(-abs(as.numeric(shoot_results$tau_sdid) / shoot_results$se_sdid))))
message(sprintf("  TWFE: ATT = %.3f (SE = %.3f, p = %.3f)",
                twfe_shoot$att, twfe_shoot$se, twfe_shoot$p_value))
message("\nRobberies:")
message(sprintf("  SDID: ATT = %.3f (SE = %.3f, p = %.3f)",
                as.numeric(rob_results$tau_sdid), rob_results$se_sdid,
                2 * pnorm(-abs(as.numeric(rob_results$tau_sdid) / rob_results$se_sdid))))
message(sprintf("  TWFE: ATT = %.3f (SE = %.3f, p = %.3f)",
                twfe_rob$att, twfe_rob$se, twfe_rob$p_value))
message("\nInterpretation:")
message("  If SDID shooting ATT shrinks toward 0 relative to TWFE,")
message("  that confirms the shooting result is driven by non-parallel trends")
message("  (consistent with the Wald pre-trend test from script 17).")
message("  If SDID robbery ATT is stable, the robbery result is robust.")
message("\nOutputs saved to: ", results_dir)
message("Script complete: ", format(Sys.time(), "%Y-%m-%d %H:%M"))
