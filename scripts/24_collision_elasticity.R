# ==============================================================================
# Vehicle Pursuits Study: Collision-Pursuit Elasticity
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Estimate the marginal collision probability per additional pursuit —
#   i.e., for each additional pursuit broadcast, how many collisions are
#   generated? This provides a "portable number" for practitioners:
#   departments considering pursuit policy changes can apply this elasticity
#   to their own pursuit volumes.
#
#   Specifications:
#     (a) Level-level: pursuit_crashes ~ pursuit_events + month_factor + covid
#         Coefficient = marginal collisions per additional pursuit
#     (b) Log-log: log(crashes+1) ~ log(pursuits+1) + month_factor + covid
#         Coefficient = elasticity (% change in crashes per % change in pursuits)
#
#   Uses Newey-West HAC standard errors (lag = 6) to match the ITS in script 03.
#
# Inputs:
#   - output/tables/monthly_panel.csv
#
# Outputs:
#   - output/its_results/collision_elasticity.csv
#   - output/plots/fig_collision_elasticity.png/.pdf
#
# Runtime: <1 min
# ==============================================================================

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(sandwich)
library(lmtest)

set.seed(20260628)


# 0. Setup ---------------------------------------------------------------------

results_dir <- here("output", "its_results")
plot_dir    <- here("output", "plots")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir,    showWarnings = FALSE, recursive = TRUE)

INTERVENTION <- as.Date("2022-10-01")

message("Script 24: Collision-Pursuit Elasticity")
message("Working directory: ", here())

COL_SHOOTING  <- "#6A0572"
COL_ROBBERY   <- "#003049"
COL_COLLISION <- "#F77F00"
COL_GUNVIOL   <- "#1B998B"
COL_PURSUIT   <- "#D62828"

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


# 1. Load and prepare data =====================================================

message("\n[1] Loading monthly panel...")

monthly <- read_csv(here("output", "tables", "monthly_panel.csv"),
                    show_col_types = FALSE) |>
  mutate(month_date = as.Date(month_date)) |>
  filter(month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) |>
  arrange(month_date) |>
  mutate(
    month_factor = factor(month(month_date)),
    covid = as.integer(month_date >= as.Date("2020-03-01") &
                         month_date <= as.Date("2021-06-01")),
    # Policy regime for scatter plot coloring
    regime = case_when(
      month_date < INTERVENTION                    ~ "Pre-escalation\n(Jan 2018 – Sep 2022)",
      month_date < as.Date("2024-03-01")           ~ "Escalation\n(Oct 2022 – Feb 2024)",
      month_date < as.Date("2025-02-01")           ~ "Post-Maddrey\n(Mar 2024 – Jan 2025)",
      TRUE                                         ~ "Post-Tisch\n(Feb 2025+)"
    ),
    regime = factor(regime, levels = c(
      "Pre-escalation\n(Jan 2018 – Sep 2022)",
      "Escalation\n(Oct 2022 – Feb 2024)",
      "Post-Maddrey\n(Mar 2024 – Jan 2025)",
      "Post-Tisch\n(Feb 2025+)"
    ))
  )

message("  n = ", nrow(monthly), " months")
message("  Pursuit events: mean = ", round(mean(monthly$pursuit_events), 1),
        ", SD = ", round(sd(monthly$pursuit_events), 1))
message("  Pursuit crashes: mean = ", round(mean(monthly$pursuit_crashes), 1),
        ", SD = ", round(sd(monthly$pursuit_crashes), 1))
message("  Crash rate: ", round(sum(monthly$pursuit_crashes) / sum(monthly$pursuit_events) * 100, 1),
        "% of pursuits result in a crash")


# 2. Level-level regression ====================================================

message("\n[2] Level-level regression: crashes ~ pursuits + seasonality + covid")

mod_level <- lm(pursuit_crashes ~ pursuit_events + month_factor + covid,
                data = monthly)

# Newey-West HAC SEs (lag = 6, matching script 03)
nw_level <- coeftest(mod_level, vcov = NeweyWest(mod_level, lag = 6, prewhite = FALSE))

# Extract pursuit_events coefficient
b_level  <- nw_level["pursuit_events", "Estimate"]
se_level <- nw_level["pursuit_events", "Std. Error"]
t_level  <- nw_level["pursuit_events", "t value"]
p_level  <- nw_level["pursuit_events", "Pr(>|t|)"]

message(sprintf("  b = %.4f (SE = %.4f, t = %.2f, p = %.4f)",
                b_level, se_level, t_level, p_level))
message(sprintf("  Interpretation: each additional pursuit generates approximately %.2f collisions",
                b_level))


# 3. Log-log regression (elasticity) ==========================================

message("\n[3] Log-log regression: log(crashes+1) ~ log(pursuits+1) + seasonality + covid")

monthly <- monthly |>
  mutate(
    log_crashes  = log(pursuit_crashes + 1),
    log_pursuits = log(pursuit_events  + 1)
  )

mod_loglog <- lm(log_crashes ~ log_pursuits + month_factor + covid,
                 data = monthly)

nw_loglog <- coeftest(mod_loglog, vcov = NeweyWest(mod_loglog, lag = 6, prewhite = FALSE))

b_loglog  <- nw_loglog["log_pursuits", "Estimate"]
se_loglog <- nw_loglog["log_pursuits", "Std. Error"]
t_loglog  <- nw_loglog["log_pursuits", "t value"]
p_loglog  <- nw_loglog["log_pursuits", "Pr(>|t|)"]

message(sprintf("  b = %.4f (SE = %.4f, t = %.2f, p = %.4f)",
                b_loglog, se_loglog, t_loglog, p_loglog))
message(sprintf("  Interpretation: a 1%% increase in pursuits is associated with a %.2f%% increase in crashes",
                b_loglog))


# 4. Save results ==============================================================

message("\n[4] Saving results...")

elasticity_results <- tibble(
  spec = c("level_level", "log_log"),
  outcome_var = c("pursuit_crashes", "log(pursuit_crashes + 1)"),
  predictor_var = c("pursuit_events", "log(pursuit_events + 1)"),
  estimate = c(b_level, b_loglog),
  std_error = c(se_level, se_loglog),
  t_stat = c(t_level, t_loglog),
  p_value = c(p_level, p_loglog),
  se_type = "Newey-West HAC (lag 6)",
  n_months = nrow(monthly),
  r_squared_level = summary(mod_level)$r.squared,
  r_squared_loglog = summary(mod_loglog)$r.squared,
  interpretation = c(
    sprintf("Each additional pursuit generates %.4f collisions", b_level),
    sprintf("1%% increase in pursuits -> %.4f%% increase in crashes", b_loglog)
  )
)

write_csv(elasticity_results, here("output", "its_results", "collision_elasticity.csv"))
message("  Saved: collision_elasticity.csv")


# 5. Scatter plot: monthly collisions vs. pursuits =============================

message("\n[5] Creating scatter plot...")

# Fitted values from the level-level model (for the fitted line)
monthly$fitted_crashes <- predict(mod_level)

# Build scatter plot colored by policy regime
p <- ggplot(monthly, aes(x = pursuit_events, y = pursuit_crashes)) +
  # Fitted line from the full model (seasonality + covid adjusted)
  # Use a simple bivariate OLS line for visual clarity
  geom_smooth(method = "lm", formula = y ~ x,
              color = "grey30", linewidth = 0.8,
              linetype = "dashed", se = TRUE, alpha = 0.15) +
  geom_point(aes(color = regime, shape = regime), size = 3, alpha = 0.85) +
  scale_color_manual(
    values = c(
      "Pre-escalation\n(Jan 2018 – Sep 2022)" = "grey50",
      "Escalation\n(Oct 2022 – Feb 2024)"     = COL_PURSUIT,
      "Post-Maddrey\n(Mar 2024 – Jan 2025)"   = COL_COLLISION,
      "Post-Tisch\n(Feb 2025+)"                     = COL_GUNVIOL
    )
  ) +
  scale_shape_manual(values = c(1, 16, 17, 15)) +
  labs(
    title    = "Pursuit-Collision Relationship",
    subtitle = sprintf(
      "Each additional pursuit generates %.2f collisions (SE = %.2f, p %s) | Newey-West HAC",
      b_level, se_level,
      if (p_level < .001) "< .001" else sprintf("= %.3f", p_level)
    ),
    x = "Monthly pursuit broadcasts",
    y = "Monthly pursuit-related collisions",
    color = "Policy regime",
    shape = "Policy regime"
  ) +
  theme_pursuit() +
  theme(
    legend.title    = element_text(size = 12, face = "bold"),
    legend.text     = element_text(size = 11),
    legend.position = "bottom",
    plot.subtitle   = element_text(color = "grey30", size = 13)
  ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

save_plot(p, "fig_collision_elasticity", w = 9, h = 7)
message("  Saved: fig_collision_elasticity.png/.pdf")


# 6. Summary ===================================================================

message("\n[6] Summary")
message("=" |> strrep(60))
message(sprintf("\nLevel-level: b = %.4f (SE = %.4f, p = %.4f)",
                b_level, se_level, p_level))
message(sprintf("  -> Each additional pursuit generates ~%.2f collisions", b_level))
message(sprintf("  -> At mean pursuit volume (%.1f/month), expected crashes = %.1f",
                mean(monthly$pursuit_events),
                b_level * mean(monthly$pursuit_events)))
message(sprintf("\nLog-log elasticity: b = %.4f (SE = %.4f, p = %.4f)",
                b_loglog, se_loglog, p_loglog))
message(sprintf("  -> 1%% increase in pursuits -> %.2f%% increase in crashes", b_loglog))

# Portable number for practitioners
message("\n  PORTABLE NUMBER FOR PRACTITIONERS:")
message(sprintf("  Marginal collision probability: %.1f%%",
                b_level * 100))
message(sprintf("  (i.e., each additional pursuit has a ~%.0f%% chance of producing a collision)",
                b_level * 100))

message("\nScript 24 complete.")
message("Outputs: ", results_dir)
message("Figures: ", plot_dir)
