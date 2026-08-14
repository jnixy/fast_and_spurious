# ==============================================================================
# Tisch Re-Restriction ITS — The "Reverse Experiment"
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Estimate the effect of the February 2025 Tisch re-restriction on pursuit
#   activity and crime outcomes using segmented regression (interrupted time
#   series). This is the "reverse experiment": in October 2022, Maddrey
#   loosened the pursuit policy and pursuits surged; in February 2025, Tisch
#   re-tightened it and pursuits fell ~71%. Same jurisdiction, opposite
#   direction, shorter window.
#
#   The analysis window is August 2023 through December 2025. This uses the
#   post-Maddrey period as the baseline, yielding 18 pre-intervention months
#   and 11 post-intervention months (Feb 2025 onward).
#
#   Model: Y_t = b0 + b1*T + b2*D + b3*P + month_FE + e_t
#     T = time index (months from Aug 2023)
#     D = 1 if month >= Feb 2025 (level shift)
#     P = months since Feb 2025 (0 in pre-period; slope change)
#     b2 = immediate level shift at Tisch re-restriction
#     b3 = change in slope post-restriction
#   Standard errors: Newey-West HAC (lag = 3)
#
#   Note on lag choice: The main ITS (script 03) uses lag = 6 for a 93-month
#   window. Here the window is only 26 months, so a shorter lag is appropriate
#   to avoid over-parameterizing the HAC estimator. Lag = 3 is conservative
#   for monthly data in a short panel (roughly T^{1/3} rule of thumb).
#   No COVID control is needed — the window is entirely post-pandemic.
#
# Inputs:
#   - output/tables/monthly_panel.csv  (from script 01)
#
# Outputs:
#   - output/its_results/tisch_its_coefficients.csv
#   - output/plots/ITS_tisch_*.png/.pdf  (one per outcome)
#
# Runtime: < 30 seconds
# ==============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(sandwich)
library(lmtest)
library(here)


# Dirs and theme ---------------------------------------------------------------

dir.create(here("output"),             showWarnings = FALSE)
dir.create(here("output/plots"),       showWarnings = FALSE)
dir.create(here("output/its_results"), showWarnings = FALSE)

plot_dir    <- here("output/plots")
results_dir <- here("output/its_results")

if (!exists("theme_pursuit")) {
  theme_pursuit <- function(base_size = 16) {
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
}

COL_PURSUIT   <- "#D62828"
COL_COLLISION <- "#F77F00"
COL_ROBBERY   <- "#003049"
COL_SHOOTING  <- "#6A0572"
COL_GUNVIOL   <- "#1B998B"

save_plot <- function(p, name, w = 5, h = 3.5) {
  ggsave(file.path(plot_dir, paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(plot_dir, paste0(name, ".pdf")),
         plot = p, width = w, height = h, bg = "white")
}


# 1. Load data and build ITS variables =========================================

monthly_panel <- read_csv(here("output", "tables", "monthly_panel.csv"),
                          show_col_types = FALSE) %>%
  mutate(month_date = as.Date(month_date))

INTERVENTION <- as.Date("2025-02-01")
WINDOW_START <- as.Date("2023-08-01")
WINDOW_END   <- as.Date("2025-12-01")
NW_LAG       <- 3L

message("\nTISCH RE-RESTRICTION ITS -- REVERSE EXPERIMENT\n")
message("Window:       ", WINDOW_START, " to ", WINDOW_END)
message("Intervention: ", INTERVENTION)
message("NW lag:       ", NW_LAG, " (shorter window => shorter lag; see header note)\n")

tisch <- monthly_panel %>%
  filter(month_date >= WINDOW_START,
         month_date <= WINDOW_END) %>%
  arrange(month_date) %>%
  mutate(
    T = row_number(),
    D = as.integer(month_date >= INTERVENTION),
    P = pmax(0L, as.integer(round(
      difftime(month_date, INTERVENTION, units = "days") / 30.44))),
    month_factor = factor(month(month_date))
  )

message("n = ", nrow(tisch), " months")
message("Pre: ", sum(tisch$D == 0), " | Post: ", sum(tisch$D == 1), "\n")


# 2. Fit segmented regression for each outcome =================================

outcomes <- list(
  list(var = "pursuit_events",     label = "Pursuit Broadcasts",                col = COL_PURSUIT),
  list(var = "pursuit_crashes",    label = "Pursuit Collisions",                col = COL_COLLISION),
  list(var = "robbery_count",      label = "Robberies",                         col = COL_ROBBERY),
  list(var = "shooting_incidents", label = "Shooting Incidents",                col = COL_SHOOTING),
  list(var = "total_gun_events",   label = "Gun Violence (Shootings + Shots Fired)", col = COL_GUNVIOL)
)

tisch_results <- list()

for (o in outcomes) {

  message("--- ", o$label, " ---")

  y <- tisch[[o$var]]
  mod <- lm(y ~ T + D + P + month_factor, data = tisch)

  # Newey-West HAC standard errors
  nw <- coeftest(mod, vcov = NeweyWest(mod, lag = NW_LAG, prewhite = FALSE))

  # Durbin-Watson
  resids <- residuals(mod)
  dw <- sum(diff(resids)^2) / sum(resids^2)
  message("  Durbin-Watson: ", round(dw, 3))

  # Ljung-Box (use lag = 6 given short series)
  lb <- Box.test(resids, lag = 6, type = "Ljung-Box")
  message("  Ljung-Box p (lag 6): ", round(lb$p.value, 4))

  # Fitted and counterfactual
  tisch_loop <- tisch
  tisch_loop$fitted         <- predict(mod)
  tisch_loop$counterfactual <- predict(mod, newdata = tisch %>% mutate(D = 0L, P = 0L))
  tisch_loop$observed       <- y

  # Cumulative and average effect
  post <- tisch_loop %>% filter(D == 1)
  cum_eff <- sum(post$fitted - post$counterfactual)
  avg_eff <- mean(post$fitted - post$counterfactual)
  message("  Cumulative effect: ", round(cum_eff, 1))
  message("  Average monthly effect: ", round(avg_eff, 1))

  # Short label for file names
  short_label <- case_when(
    o$var == "pursuit_events"     ~ "Pursuits",
    o$var == "pursuit_crashes"    ~ "Collisions",
    o$var == "robbery_count"      ~ "Robberies",
    o$var == "shooting_incidents" ~ "Shootings",
    o$var == "total_gun_events"   ~ "GunViolence"
  )

  # Plot: observed + fitted + counterfactual
  p <- ggplot(tisch_loop, aes(month_date)) +
    geom_point(aes(y = observed), color = o$col, alpha = 0.5, size = 1.5) +
    geom_line(aes(y = fitted), color = o$col, linewidth = 1) +
    geom_line(aes(y = counterfactual), color = "grey40",
              linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = INTERVENTION, linetype = "dotted",
               color = "grey30", linewidth = 0.6) +
    annotate("text", x = INTERVENTION, y = Inf,
             label = "Tisch\nre-restriction",
             vjust = 2, hjust = -0.1, size = 3, color = "grey30") +
    geom_ribbon(data = tisch_loop %>% filter(D == 1),
                aes(ymin = pmin(counterfactual, fitted),
                    ymax = pmax(counterfactual, fitted)),
                fill = o$col, alpha = 0.15) +
    scale_x_date(date_breaks = "3 months", date_labels = "%b\n%Y") +
    labs(
      title = paste0("Tisch Re-Restriction ITS: ", short_label),
      subtitle = "Reverse experiment -- same jurisdiction, opposite policy direction",
      x = NULL,
      y = stringr::str_wrap(paste("Monthly", o$label), width = 30),
      caption = paste0(
        "Solid = fitted; dashed = counterfactual (no re-restriction).\n",
        "Window: Aug 2023 -- Dec 2025 (18 pre, 11 post). ",
        "Newey-West HAC SEs (lag = ", NW_LAG,
        "; shorter lag reflects the 26-month window)."
      )
    ) +
    theme_pursuit()

  save_plot(p, paste0("ITS_tisch_", short_label))
  message("  Saved: ITS_tisch_", short_label, ".png/.pdf\n")

  tisch_results[[o$label]] <- list(
    model      = mod,
    nw_test    = nw,
    dw_stat    = dw,
    lb_pvalue  = lb$p.value,
    cum_effect = cum_eff,
    avg_effect = avg_eff
  )
}


# 3. Summary table =============================================================

tisch_summary <- map_dfr(outcomes, function(o) {
  mod <- tisch_results[[o$label]]$model
  nw  <- coeftest(mod, vcov = NeweyWest(mod, lag = NW_LAG, prewhite = FALSE))

  # Extract NW-based estimates for the key terms
  key_terms <- c("T", "D", "P")
  nw_df <- data.frame(
    term      = rownames(nw),
    estimate  = nw[, "Estimate"],
    nw_se     = nw[, "Std. Error"],
    statistic = nw[, "t value"],
    nw_pvalue = nw[, "Pr(>|t|)"],
    stringsAsFactors = FALSE
  ) %>%
    filter(term %in% key_terms)

  # Add 95% CIs based on NW SEs
  nw_df %>%
    mutate(
      outcome  = o$label,
      conf.low  = estimate - 1.96 * nw_se,
      conf.high = estimate + 1.96 * nw_se
    ) %>%
    select(outcome, term, estimate, nw_se, statistic, nw_pvalue, conf.low, conf.high)
})

# Readable term labels
tisch_summary <- tisch_summary %>%
  mutate(
    term_label = recode(term,
      "T" = "Pre-trend (T)",
      "D" = "Level shift (D)",
      "P" = "Slope change (P)"
    )
  )

write_csv(tisch_summary, file.path(results_dir, "tisch_its_coefficients.csv"))
message("\nCoefficients saved: output/its_results/tisch_its_coefficients.csv")


# 4. Print key results to console =============================================

message("\n\n========== TISCH RE-RESTRICTION: KEY RESULTS ==========\n")

tisch_summary %>%
  filter(term %in% c("D", "P")) %>%
  mutate(
    sig = case_when(
      nw_pvalue < 0.001 ~ "***",
      nw_pvalue < 0.01  ~ "**",
      nw_pvalue < 0.05  ~ "*",
      nw_pvalue < 0.10  ~ "+",
      TRUE              ~ ""
    )
  ) %>%
  rowwise() %>%
  mutate(
    msg = sprintf("  %-45s  %s = %7.1f [%7.1f, %7.1f]  p = %.3f %s",
                  outcome, term, estimate, conf.low, conf.high, nw_pvalue, sig)
  ) %>%
  pull(msg) %>%
  walk(message)

message("\n========================================================\n")
message("Interpretation:")
message("  D (level shift) = immediate change at Feb 2025 re-restriction")
message("  P (slope change) = change in monthly trend post-restriction")
message("  Negative D for pursuits = the re-restriction reduced pursuit activity")
message("  If crime D is ~0 = no corresponding crime spike from fewer pursuits")
message("\nDone.\n")
