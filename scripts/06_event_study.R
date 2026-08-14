# 06_event_study.R — Multi-Period / Event Study Analysis
#
# Four regimes identified in the NYPD pursuit escalation:
#   1. Pre-escalation:  Jan 2018 – Sep 2022 (baseline)
#   2. Escalation:      Oct 2022 – Jul 2023 (Chell operational shift)
#   3. Post-Maddrey:    Aug 2023 – Jan 2025 (compliance memo)
#   4. Post-Tisch:      Feb 2025 – Dec 2025 (partial re-restriction)
#
# Trimmed at Dec 2025 due to right-censoring of robbery/shooting data.

library(tidyverse)
library(lubridate)
library(sandwich)
library(lmtest)
library(broom)
library(here)


# Dirs and theme ---------------------------------------------------------------

dir.create(here("output"),               showWarnings = FALSE)
dir.create(here("output/plots"),         showWarnings = FALSE)
dir.create(here("output/event_results"), showWarnings = FALSE)

plot_dir    <- here("output/plots")
results_dir <- here("output/event_results")

if (!exists("theme_pursuit")) {
  theme_pursuit <- function(base_size = 13) {
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

save_plot <- function(p, name, w = 10, h = 6) {
  ggsave(file.path(plot_dir, paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(plot_dir, paste0(name, ".pdf")),
         plot = p, width = w, height = h, bg = "white")
}


# 1. Load monthly panel ========================================================

message("\nEVENT STUDY \u2014 MULTI-PERIOD ANALYSIS\n")

panel <- read_csv(here("output/tables/monthly_panel.csv"), show_col_types = FALSE)

# Trim at Dec 2025 to avoid right-censoring of robbery/shooting
panel <- panel %>%
  filter(month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) %>%
  arrange(month_date)

message("Panel: ", nrow(panel), " months (", min(panel$month_date),
        " to ", max(panel$month_date), ")")


# 2. Define regimes ============================================================

# Breakpoints
ESCALATION <- as.Date("2022-10-01")
MADDREY    <- as.Date("2023-08-01")
TISCH      <- as.Date("2025-02-01")

es <- panel %>%
  mutate(
    T = row_number(),
    month_factor = factor(month(month_date)),
    covid = as.integer(month_date >= as.Date("2020-03-01") &
                         month_date <= as.Date("2021-06-01")),

    # Regime indicators
    regime = case_when(
      month_date < ESCALATION ~ "1_pre",
      month_date < MADDREY    ~ "2_escalation",
      month_date < TISCH      ~ "3_maddrey",
      TRUE                    ~ "4_tisch"
    ),

    # Level shift indicators (step functions)
    D_esc   = as.integer(month_date >= ESCALATION),
    D_mad   = as.integer(month_date >= MADDREY),
    D_tisch = as.integer(month_date >= TISCH),

    # Slope change indicators (time since each break, 0 before)
    P_esc   = pmax(0L, as.integer(round(
      difftime(month_date, ESCALATION, units = "days") / 30.44))),
    P_mad   = pmax(0L, as.integer(round(
      difftime(month_date, MADDREY, units = "days") / 30.44))),
    P_tisch = pmax(0L, as.integer(round(
      difftime(month_date, TISCH, units = "days") / 30.44)))
  )

regime_counts <- es %>% count(regime, name = "months")
message("Regime structure:")
print(regime_counts)


# 3. Outcomes ==================================================================

outcomes <- list(
  list(var = "pursuit_events",     label = "Pursuit Broadcasts",                col = COL_PURSUIT),
  list(var = "pursuit_crashes",    label = "Pursuit Collisions",                col = COL_COLLISION),
  list(var = "robbery_count",      label = "Robberies",                         col = COL_ROBBERY),
  list(var = "shooting_incidents", label = "Shooting Incidents",                col = COL_SHOOTING),
  list(var = "total_gun_events",   label = "Gun Violence (Shootings + Shots Fired)", col = COL_GUNVIOL)
)


# 4. Segmented regression with 3 breaks ========================================

# Model: Y ~ T + D_esc + P_esc + D_mad + P_mad + D_tisch + P_tisch + season + covid

message("--- Multi-Break Segmented Regression ---")

mb_results <- list()

for (o in outcomes) {

  y <- es[[o$var]]
  mod <- lm(y ~ T + D_esc + P_esc + D_mad + P_mad + D_tisch + P_tisch +
              month_factor + covid, data = es)

  nw <- coeftest(mod, vcov = NeweyWest(mod, lag = 6, prewhite = FALSE))

  message("=== ", o$label, " ===")
  message("Newey-West results:")
  print(nw[1:8, ])

  mb_results[[o$label]] <- list(model = mod, nw = nw)
}


# 5. Compile coefficient tables ================================================

# OLS + NW side-by-side
key_terms <- c("T", "D_esc", "P_esc", "D_mad", "P_mad", "D_tisch", "P_tisch", "covid")

mb_ols <- map_dfr(outcomes, function(o) {
  mod <- mb_results[[o$label]]$model
  nw  <- mb_results[[o$label]]$nw

  tidy(mod, conf.int = TRUE) %>%
    filter(term %in% key_terms) %>%
    mutate(
      outcome   = o$label,
      nw_se     = nw[match(term, rownames(nw)), "Std. Error"],
      nw_pvalue = nw[match(term, rownames(nw)), "Pr(>|t|)"]
    )
})

write_csv(mb_ols, file.path(results_dir, "multibreak_coefficients.csv"))

# NW-only table (cleaner for manuscript)
mb_nw <- map_dfr(outcomes, function(o) {
  nw <- mb_results[[o$label]]$nw
  as_tibble(nw[key_terms, ], rownames = "term") %>%
    rename(estimate = Estimate, se = `Std. Error`,
           t_stat = `t value`, p_value = `Pr(>|t|)`) %>%
    mutate(outcome = o$label)
})

write_csv(mb_nw, file.path(results_dir, "multibreak_newey_west.csv"))

message("Multi-break coefficients saved.")


# 6. Regime descriptive statistics =============================================

regime_means <- es %>%
  group_by(regime) %>%
  summarise(
    months            = n(),
    mean_pursuits     = mean(pursuit_events),
    sd_pursuits       = sd(pursuit_events),
    mean_crashes      = mean(pursuit_crashes),
    crash_rate_pct    = mean(pursuit_crashes) / mean(pursuit_events) * 100,
    mean_robberies    = mean(robbery_count),
    mean_shootings    = mean(shooting_incidents),
    mean_gun_violence = mean(total_gun_events),
    .groups = "drop"
  )

message("--- Regime Means ---")
print(regime_means, width = Inf)

write_csv(regime_means, file.path(results_dir, "regime_means.csv"))


# 7. Tisch falsification test ==================================================

# If deterrence drove crime reductions, re-restricting pursuits should increase
# crime. Test: is D_tisch positive for crime outcomes?

message("--- Tisch Falsification Test ---")

tisch_test <- mb_nw %>%
  filter(term == "D_tisch") %>%
  select(outcome, estimate, se, t_stat, p_value) %>%
  mutate(
    direction = ifelse(estimate > 0, "increase", "decrease"),
    interpretation = case_when(
      outcome %in% c("Pursuit Broadcasts", "Pursuit Collisions") & estimate < 0 ~
        "Expected: pursuits fell after re-restriction",
      outcome %in% c("Robberies", "Shooting Incidents",
                      "Gun Violence (Shootings + Shots Fired)") & estimate > 0 ~
        "Consistent with deterrence: crime rose after re-restriction",
      outcome %in% c("Robberies", "Shooting Incidents",
                      "Gun Violence (Shootings + Shots Fired)") & estimate <= 0 ~
        "Inconsistent with deterrence: crime did NOT rise after re-restriction",
      TRUE ~ ""
    )
  )

print(tisch_test, width = Inf)
write_csv(tisch_test, file.path(results_dir, "tisch_falsification.csv"))


# 8. Tisch pre/post comparison ================================================

tisch_comparison <- es %>%
  mutate(period = ifelse(month_date >= TISCH, "post_tisch", "pre_tisch")) %>%
  filter(month_date >= ESCALATION) %>%
  group_by(period) %>%
  summarise(
    months         = n(),
    mean_pursuits  = mean(pursuit_events),
    mean_crashes   = mean(pursuit_crashes),
    mean_robberies = mean(robbery_count),
    mean_shootings = mean(shooting_incidents),
    mean_gun       = mean(total_gun_events),
    .groups = "drop"
  )

write_csv(tisch_comparison, file.path(results_dir, "tisch_comparison.csv"))

# Percent changes
if (nrow(tisch_comparison) == 2) {
  pre  <- tisch_comparison %>% filter(period == "pre_tisch")
  post <- tisch_comparison %>% filter(period == "post_tisch")

  pct_change <- tibble(
    outcome = c("pursuits", "crashes", "robberies", "shootings", "gun_violence"),
    pre_mean = c(pre$mean_pursuits, pre$mean_crashes, pre$mean_robberies,
                 pre$mean_shootings, pre$mean_gun),
    post_mean = c(post$mean_pursuits, post$mean_crashes, post$mean_robberies,
                  post$mean_shootings, post$mean_gun),
    pct_change = (post_mean - pre_mean) / pre_mean * 100
  )

  message("--- Tisch Reversal Percent Changes ---")
  print(pct_change)
  write_csv(pct_change, file.path(results_dir, "tisch_reversal_pct_changes.csv"))
}


# 9. Visualizations ============================================================

# 9a. Time series with regime shading
regime_rects <- tibble(
  xmin = c(as.Date("2018-01-01"), ESCALATION, MADDREY, TISCH),
  xmax = c(ESCALATION, MADDREY, TISCH, as.Date("2025-12-01")),
  regime = c("Pre-escalation", "Escalation", "Post-Maddrey", "Post-Tisch"),
  fill = c("grey95", "#FDE8E8", "#FFECD2", "#E8F4FD")
)

for (o in outcomes) {

  p <- ggplot(es, aes(month_date, .data[[o$var]])) +
    geom_rect(data = regime_rects,
              aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = regime),
              inherit.aes = FALSE, alpha = 0.5) +
    scale_fill_manual(values = c("Pre-escalation" = "grey95", "Escalation" = "#FDE8E8",
                                 "Post-Maddrey" = "#FFECD2", "Post-Tisch" = "#E8F4FD")) +
    geom_line(color = o$col, linewidth = 0.8) +
    geom_point(color = o$col, size = 1.2, alpha = 0.5) +
    geom_vline(xintercept = c(ESCALATION, MADDREY, TISCH),
               linetype = "dashed", color = "grey40", linewidth = 0.5) +
    annotate("text", x = ESCALATION, y = Inf, label = "Chell\nEscalation",
             vjust = 1.5, hjust = -0.1, size = 2.8, color = "grey30") +
    annotate("text", x = MADDREY, y = Inf, label = "Maddrey\nMemo",
             vjust = 1.5, hjust = -0.1, size = 2.8, color = "grey30") +
    annotate("text", x = TISCH, y = Inf, label = "Tisch\nReversal",
             vjust = 1.5, hjust = -0.1, size = 2.8, color = "grey30") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = paste0("Four-Regime Analysis: ", o$label),
      subtitle = "Shading indicates policy regime",
      x = NULL, y = paste("Monthly", o$label)
    ) +
    theme_pursuit() +
    guides(fill = guide_legend(override.aes = list(alpha = 0.4)))

  save_plot(p, paste0("event_regime_", gsub(" ", "_", o$var)))
}

# 9b. Coefficient forest plot
coef_plot_data <- mb_nw %>%
  filter(term %in% c("D_esc", "D_mad", "D_tisch")) %>%
  mutate(
    term_label = recode(term,
      "D_esc"   = "Escalation\n(Oct 2022)",
      "D_mad"   = "Maddrey Memo\n(Aug 2023)",
      "D_tisch" = "Tisch Reversal\n(Feb 2025)"
    ),
    ci_lo = estimate - 1.96 * se,
    ci_hi = estimate + 1.96 * se,
    sig   = ifelse(p_value < 0.05, "p < .05", "n.s.")
  )

p_forest <- ggplot(coef_plot_data, aes(x = estimate, y = term_label, color = outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi, shape = sig),
                  position = position_dodge(width = 0.6), size = 0.5) +
  scale_color_manual(values = c(
    "Pursuit Broadcasts" = COL_PURSUIT, "Pursuit Collisions" = COL_COLLISION,
    "Robberies" = COL_ROBBERY, "Shooting Incidents" = COL_SHOOTING,
    "Gun Violence (Shootings + Shots Fired)" = COL_GUNVIOL
  )) +
  scale_shape_manual(values = c("p < .05" = 16, "n.s." = 1)) +
  facet_wrap(~outcome, scales = "free_x", nrow = 1) +
  labs(
    title    = "Regime-Level Shifts: Coefficient Estimates",
    subtitle = "Newey-West 95% CIs | Filled = p < .05",
    x = "Level Shift Estimate", y = NULL,
    caption  = "Segmented regression with seasonality + COVID controls"
  ) +
  theme_pursuit() +
  theme(legend.position = "none",
        strip.text = element_text(size = rel(0.75)))

save_plot(p_forest, "event_coefficient_forest", w = 14, h = 6)


# Done =========================================================================

message("Event study results saved to: ", results_dir)
message("Plots saved to: ", plot_dir)
