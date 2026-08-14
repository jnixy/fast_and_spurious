# ==============================================================================
# Vehicle Pursuits Study: Precinct-Level Dose-Response TWFE
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Estimate the association between within-precinct variation in monthly
#   pursuit intensity and three crime/collision outcomes using a two-way
#   fixed effects (TWFE) panel model. The continuous "dose" (pursuit_events
#   per precinct-month) absorbs cross-sectional sorting: if high-crime
#   precincts simply have more pursuits, precinct FE controls for that stable
#   difference. Time (month-year) FE controls for city-wide shocks (e.g.,
#   seasonal crime cycles, COVID). Remaining variation is within-precinct
#   deviations in pursuit intensity — our treatment variation.
#
#   Endogeneity caveat: pursuits are not randomly assigned; months with
#   unusually high crime may also generate more pursuits. We address this
#   with a one-month lag specification (lagged dose) and note the limitation
#   explicitly. The staggered-adoption design in script 16 provides sharper
#   identification.
#
# Inputs:
#   - output/precinct_did/pct_monthly_pursuit.csv   (from script 01)
#   - output/precinct_did/pct_monthly_shootings.csv (from script 01)
#   - output/precinct_did/pct_monthly_robberies.csv (from script 01)
#   - output/precinct_did/pct_monthly_collisions.csv (from script 01)
#   - output/precinct_did/pct_ddd_models.rds        (from script 17 — for Section 6 map)
#
# Outputs (saved to output/):
#   - output/precinct_did/pct_twfe_results.csv               -- TWFE estimates (6 rows)
#   - output/precinct_did/fit_contemp.rds                    -- feols objects, contemporaneous spec
#   - output/precinct_did/fit_lagged.rds                     -- feols objects, lagged spec
#   - output/plots/fig_precinct_dose_response_shooting.png/.pdf  -- demeaned scatter
#   - output/plots/fig_precinct_dose_response_robbery.png/.pdf   -- demeaned scatter
#   - output/plots/fig_precinct_map.png/.pdf                 -- two-panel DDD grouping map (pre-crime splits)
#
# Runtime: ~1 min
# ==============================================================================

library(tidyverse)
library(lubridate)
library(fixest)
library(here)
library(sf)         # spatial features for choropleth map (Section 6)
library(cowplot)    # 2×2 map assembly via plot_grid (Section 6)

set.seed(20260303)


# 0. Setup ---------------------------------------------------------------------

dir.create(here("output", "precinct_did"), showWarnings = FALSE, recursive = TRUE)
pct_dir  <- here("output", "precinct_did")
plot_dir <- here("output", "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

message("Script 15: Precinct Dose-Response TWFE")
message("Working directory: ", here())

# Theme
COL_PURSUIT  <- "#D62828"
COL_SHOOTING <- "#6A0572"
COL_ROBBERY  <- "#003049"

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
      legend.position  = "top",
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

pursuits_pct   <- read_csv(file.path(pct_dir, "pct_monthly_pursuit.csv"),
                            show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(month_date = as.Date(month_date))

shootings_pct  <- read_csv(file.path(pct_dir, "pct_monthly_shootings.csv"),
                            show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(month_date = as.Date(month_date))

robberies_pct  <- read_csv(file.path(pct_dir, "pct_monthly_robberies.csv"),
                            show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(month_date = as.Date(month_date))

collisions_pct <- read_csv(file.path(pct_dir, "pct_monthly_collisions.csv"),
                            show_col_types = FALSE) |>
  janitor::clean_names() |>
  mutate(month_date = as.Date(month_date))

message("  Pursuit rows: ",   nrow(pursuits_pct))
message("  Shooting rows: ",  nrow(shootings_pct))
message("  Robbery rows: ",   nrow(robberies_pct))
message("  Collision rows: ", nrow(collisions_pct))


# 2. Build combined panel ======================================================
#
# One row per precinct × month. Analysis window: Jan 2018 – Sep 2025.
# Right-censored at Sep 2025 (Oct–Dec 2025 have zeros in crime data).

message("\n[2] Building combined precinct panel...")

panel <- pursuits_pct %>%
  left_join(shootings_pct,  by = c("pct", "month_date")) %>%
  left_join(robberies_pct,  by = c("pct", "month_date")) %>%
  left_join(collisions_pct, by = c("pct", "month_date")) %>%
  replace_na(list(pursuit_events    = 0L,
                  shooting_incidents = 0L,
                  robbery_count      = 0L,
                  collision_count    = 0L)) %>%
  # Filter to precincts that appear in actual NYPD data (pct 1–123, no code 999)
  filter(pct >= 1, pct <= 123) %>%
  filter(month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) %>%
  # Restrict to precincts with at least some non-zero data in any outcome
  # (drops grid rows for precincts that don't actually exist in NYPD data)
  group_by(pct) %>%
  filter(any(shooting_incidents > 0) | any(robbery_count > 0)) %>%
  ungroup() %>%
  arrange(pct, month_date) %>%
  # One-month lag of pursuit intensity (for lagged spec robustness check)
  group_by(pct) %>%
  mutate(pursuit_lag1 = lag(pursuit_events, 1)) %>%
  ungroup()

# Recode Precinct 116 as 105 (consolidated mid-study) and re-aggregate.
# Pct 116 (Jamaica) was formally merged into Pct 105; treating them as one
# unit throughout ensures a stable 77-precinct panel.
# pursuit_lag1 is dropped before aggregation (sum of lags != lag of sum)
# and recomputed after on the merged series.
panel <- panel |>
  select(-any_of("pursuit_lag1")) |>
  mutate(pct = if_else(pct == 116L, 105L, pct)) |>
  group_by(pct, month_date) |>
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)), .groups = "drop") |>
  arrange(pct, month_date) |>
  group_by(pct) |>
  mutate(pursuit_lag1 = lag(pursuit_events, 1)) |>
  ungroup()

# Guard: verify expected numeric outcome columns survived the re-aggregation
expected_cols <- c("pct", "month_date",
                   "pursuit_events", "shooting_incidents",
                   "robbery_count", "collision_count", "pursuit_lag1")
stopifnot("Column(s) missing after pct 116→105 recode" =
            all(expected_cols %in% names(panel)))

n_pcts   <- n_distinct(panel$pct)
n_months <- n_distinct(panel$month_date)
message("  Panel: ", n_pcts, " precincts × ", n_months, " months = ", nrow(panel), " obs")

# Report precincts with any post-Oct 2022 pursuit
treated <- panel %>%
  filter(month_date >= as.Date("2022-10-01"), pursuit_events > 0) %>%
  pull(pct) %>% unique()
message("  Precincts with ≥1 pursuit after Oct 2022: ", length(treated))
message("  Never-treated precincts (no pursuit after Oct 2022): ",
        n_pcts - length(treated))


# 3. TWFE Dose-Response Estimation =============================================
#
# Model: outcome ~ pursuit_events | pct + month_date, cluster = ~pct
#
# Fixed effects absorb:
#   - pct FE: stable precinct characteristics (size, crime baseline)
#   - month_date FE: city-wide monthly shocks (seasonality, policy, COVID)
#
# Cluster-robust SEs at precinct level (G=77 well above conventional threshold).

message("\n[3] Estimating TWFE dose-response models...")

outcomes <- list(
  shooting  = "shooting_incidents",
  robbery   = "robbery_count",
  collision = "collision_count"
)

# Contemporaneous specification
fit_contemp <- lapply(outcomes, function(y) {
  fml <- as.formula(paste0(y, " ~ pursuit_events | pct + month_date"))
  feols(fml, data = panel, cluster = ~pct)
})

# Lagged specification (pursuit in prior month → crime this month)
# Drop first month per precinct (NA lag); feols handles this automatically
fit_lagged <- lapply(outcomes, function(y) {
  fml <- as.formula(paste0(y, " ~ pursuit_lag1 | pct + month_date"))
  feols(fml, data = panel, cluster = ~pct)
})

# Extract results into tidy table.
# Read directly from summary()$coeftable so p-values use fixest's internal
# cluster-robust DoF (G-1), not the inflated nobs-nparams denominator.
extract_results <- function(fit_list, spec_label, treatment_var) {
  imap_dfr(fit_list, function(fit, outcome_name) {
    ct <- summary(fit)$coeftable
    tibble(
      spec         = spec_label,
      outcome      = outcome_name,
      estimate     = ct[treatment_var, "Estimate"],
      std_error    = ct[treatment_var, "Std. Error"],
      t_stat       = ct[treatment_var, "t value"],
      p_value      = ct[treatment_var, "Pr(>|t|)"],
      nobs         = fit$nobs,
      n_precincts  = n_distinct(panel$pct)
    )
  })
}

results <- bind_rows(
  extract_results(fit_contemp, "contemporaneous", "pursuit_events"),
  extract_results(fit_lagged,  "lagged_1month",   "pursuit_lag1")
)

write_csv(results, file.path(pct_dir, "pct_twfe_results.csv"))
message("  Saved: output/precinct_did/pct_twfe_results.csv (", nrow(results), " rows)")

# Save model objects for downstream use (manuscript inline R, robustness checks)
saveRDS(fit_contemp, here("output", "precinct_did", "fit_contemp.rds"))
saveRDS(fit_lagged,  here("output", "precinct_did", "fit_lagged.rds"))
message("  Saved: fit_contemp.rds, fit_lagged.rds")


# 4. Demeaned Scatter Plots ====================================================
#
# Show within-precinct, within-time variation. Demean both pursuit intensity
# and the outcome by subtracting precinct mean and monthly mean (FWL theorem).
# The TWFE slope is estimated on this demeaned variation.

message("\n[4] Creating demeaned scatter plots...")

demean_twoway <- function(df, x_var, y_var) {
  # Double-demean: subtract precinct mean and month mean, add grand mean back
  df %>%
    group_by(pct) %>%
    mutate(
      x_pct_mean = mean(.data[[x_var]], na.rm = TRUE),
      y_pct_mean = mean(.data[[y_var]], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(month_date) %>%
    mutate(
      x_month_mean = mean(.data[[x_var]], na.rm = TRUE),
      y_month_mean = mean(.data[[y_var]], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      x_grand = mean(.data[[x_var]], na.rm = TRUE),
      y_grand = mean(.data[[y_var]], na.rm = TRUE),
      x_dm = .data[[x_var]] - x_pct_mean - x_month_mean + x_grand,
      y_dm = .data[[y_var]] - y_pct_mean - y_month_mean + y_grand
    )
}

# Panel without lag NA rows for scatter
panel_complete <- panel %>% filter(!is.na(pursuit_events))

# Shooting scatter
shoot_dm <- demean_twoway(panel_complete, "pursuit_events", "shooting_incidents")
shoot_coef <- coef(fit_contemp$shooting)["pursuit_events"]
shoot_p    <- results %>% filter(spec == "contemporaneous", outcome == "shooting") %>%
  pull(p_value)

p_shoot <- ggplot(shoot_dm, aes(x = x_dm, y = y_dm)) +
  geom_point(alpha = 0.08, size = 0.6, color = COL_SHOOTING) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = COL_SHOOTING, fill = COL_SHOOTING, alpha = 0.15,
              linewidth = 0.9) +
  labs(
    title    = "Pursuit Intensity and Shooting Incidents",
    subtitle = sprintf(
      "Within-precinct, within-month variation (b = %.3f, p %s)",
      shoot_coef,
      ifelse(shoot_p < .001, "< .001", sprintf("= %.3f", shoot_p))
    ),
    x = "Pursuit events (demeaned)",
    y = "Shooting incidents (demeaned)",
    caption = "Each point is a precinct-month. Both axes demeaned by precinct and month FEs (FWL theorem).\nSEs clustered by precinct (G = 77)."
  ) +
  theme_pursuit()

save_plot(p_shoot, "fig_precinct_dose_response_shooting", h = 4)
message("  Saved: fig_precinct_dose_response_shooting.png/.pdf")

# Robbery scatter
rob_dm  <- demean_twoway(panel_complete, "pursuit_events", "robbery_count")
rob_coef <- coef(fit_contemp$robbery)["pursuit_events"]
rob_p    <- results %>% filter(spec == "contemporaneous", outcome == "robbery") %>%
  pull(p_value)

p_rob <- ggplot(rob_dm, aes(x = x_dm, y = y_dm)) +
  geom_point(alpha = 0.08, size = 0.6, color = COL_ROBBERY) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = COL_ROBBERY, fill = COL_ROBBERY, alpha = 0.15,
              linewidth = 0.9) +
  labs(
    title    = "Pursuit Intensity and Robbery",
    subtitle = sprintf(
      "Within-precinct, within-month variation (b = %.3f, p %s)",
      rob_coef,
      ifelse(rob_p < .001, "< .001", sprintf("= %.3f", rob_p))
    ),
    x = "Pursuit events (demeaned)",
    y = "Robbery count (demeaned)",
    caption = "Each point is a precinct-month. Both axes demeaned by precinct and month FEs (FWL theorem).\nSEs clustered by precinct (G = 77)."
  ) +
  theme_pursuit()

save_plot(p_rob, "fig_precinct_dose_response_robbery", h = 4)
message("  Saved: fig_precinct_dose_response_robbery.png/.pdf")


# 5. Summary ===================================================================

# Key contemporaneous estimates (full table in pct_twfe_results.csv)
s <- results |> filter(spec == "contemporaneous", outcome == "shooting")
message("\n[5] Contemporaneous shooting: b=", round(s$estimate, 4),
        " SE=", round(s$std_error, 4), " p=", round(s$p_value, 3))

message("Outputs saved to: ", pct_dir)
message("Figures saved to: ", plot_dir)


# 6. Precinct Choropleth Map ===================================================
#
# Four-panel 2×2 map showing all four DDD grouping variables:
#   Top-left:     pre-intervention crash rate median split
#   Top-right:    pre-intervention shooting rate median split
#   Bottom-left:  pre-intervention gun violence rate median split
#   Bottom-right: pre-intervention robbery rate median split
#
# Pre-crime groupings are canonical outputs of script 17 (pct_ddd_models.rds).
# Script 17 must run before script 15 for this section to work.

message("\n[6] Creating 4-panel precinct choropleth map...")

# Load canonical pre-crime groupings from script 17 output
stopifnot(file.exists(here("output", "precinct_did", "pct_ddd_models.rds")))
ddd_models       <- readRDS(here("output", "precinct_did", "pct_ddd_models.rds"))
pre_crime_shoot  <- ddd_models$pre_crime_shoot    # cols: pct, mean_shoot_pre,  high_crime_shoot
pre_crime_rob    <- ddd_models$pre_crime_rob      # cols: pct, mean_rob_pre,    high_crime_rob
pre_crime_gunviol <- ddd_models$pre_crime_gunviol # cols: pct, mean_gv_pre,     high_crime_gunviol
pre_crash_data   <- ddd_models$pre_crash          # cols: pct, mean_crash_pre,  high_crash

# Read GeoJSON — field "precinct" is character; cast to integer for join
stopifnot(file.exists(here("data", "Police_Precincts_20260303.geojson")))
precincts_sf <- st_read(here("data", "Police_Precincts_20260303.geojson"), quiet = TRUE) |>
  mutate(pct = as.integer(precinct)) |>
  # Merge pct 116 (split from 105 in Dec 2024) back into 105; mirrors the
  # 116→105 recode applied to the analysis panel in scripts 15/16/17 so
  # the left_join below finds a match for every polygon.
  mutate(pct = if_else(pct == 116L, 105L, pct)) |>
  group_by(pct) |>
  summarise(geometry = st_union(geometry), .groups = "drop")

# Join all four crime groupings to spatial file
map_data <- precincts_sf |>
  left_join(pre_crime_shoot  |> select(pct, high_crime_shoot),   by = "pct") |>
  left_join(pre_crime_rob    |> select(pct, high_crime_rob),     by = "pct") |>
  left_join(pre_crime_gunviol|> select(pct, high_crime_gunviol), by = "pct") |>
  left_join(pre_crash_data   |> select(pct, high_crash),         by = "pct")

# Guard: spatial file row count unchanged after left joins
stopifnot(nrow(map_data) == nrow(precincts_sf))

message("  high_crime_shoot:   ", sum(map_data$high_crime_shoot   == 1, na.rm = TRUE), " high")
message("  high_crime_rob:     ", sum(map_data$high_crime_rob     == 1, na.rm = TRUE), " high")
message("  high_crime_gunviol: ", sum(map_data$high_crime_gunviol == 1, na.rm = TRUE), " high")
message("  high_crash:         ", sum(map_data$high_crash         == 1, na.rm = TRUE), " high")

# Convert binary integers to factors for clean discrete fill legends
map_data <- map_data |>
  mutate(
    crash_group  = factor(high_crash,          levels = c(0, 1),
                          labels = c("Low (below median)", "High (above median)")),
    shoot_group  = factor(high_crime_shoot,    levels = c(0, 1),
                          labels = c("Low (below median)", "High (above median)")),
    gv_group     = factor(high_crime_gunviol,  levels = c(0, 1),
                          labels = c("Low (below median)", "High (above median)")),
    rob_group    = factor(high_crime_rob,      levels = c(0, 1),
                          labels = c("Low (below median)", "High (above median)"))
  )

# Shared map theme function
COL_GUNVIOL <- "#1B998B"
map_panel <- function(fill_var, title_txt, high_color) {
  colors <- c("Low (below median)"  = "gray85",
              "High (above median)" = high_color)
  ggplot(map_data) +
    geom_sf(aes(fill = .data[[fill_var]]), color = "white", linewidth = 0.2) +
    scale_fill_manual(values = colors, na.value = "gray70",
                      name = NULL, drop = FALSE) +
    labs(title = title_txt) +
    theme_void(base_size = 10) +
    theme(
      legend.position  = "bottom",
      legend.text      = element_text(size = 7),
      legend.key.size  = unit(0.4, "cm"),
      plot.title       = element_text(face = "bold", size = 10, hjust = 0.5,
                                      margin = margin(b = 4))
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}

p_crash_map <- map_panel("crash_group",  "Crash rate split",       COL_PURSUIT)
p_shoot_map <- map_panel("shoot_group",  "Shooting rate split",    COL_SHOOTING)
p_gv_map    <- map_panel("gv_group",     "Gun violence rate split", COL_GUNVIOL)
p_rob_map   <- map_panel("rob_group",    "Robbery rate split",     COL_ROBBERY)

# Assemble as 2×2 grid using cowplot
title_grob <- cowplot::ggdraw() +
  cowplot::draw_label("Pre-Intervention Crime Groupings (DiD Design)",
                      fontface = "bold", size = 11, x = 0.5, hjust = 0.5)

p_map <- cowplot::plot_grid(
  title_grob,
  cowplot::plot_grid(p_crash_map, p_shoot_map,
                     p_gv_map,    p_rob_map,
                     ncol = 2, nrow = 2),
  nrow = 2, rel_heights = c(0.06, 1)
)

# Overlap note: gun violence and shooting groupings may classify most precincts
# identically; if stored in ddd_models, report it here.
if (!is.null(ddd_models$overlap_gv_shoot_pct)) {
  message("  Gun violence vs. shooting grouping overlap: ",
          ddd_models$overlap_gv_shoot_pct, "%")
}

save_plot(p_map, "fig_precinct_map", w = 8, h = 6.5)
message("  Saved: fig_precinct_map.png/.pdf (2x2 grid, 8x6.5)")

message("\nScript 15 complete.\n")
