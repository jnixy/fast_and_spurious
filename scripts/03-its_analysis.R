# ==============================================================================
# Vehicle Pursuits Study: Interrupted Time Series (ITS) Analysis
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Estimate the effect of the October 2022 NYPD pursuit policy change using
#   segmented regression (interrupted time series). Outcome variables are
#   monthly city-level counts: pursuit events, pursuit crashes, robberies,
#   shooting incidents, and total gun violence.
#
#   Model: Y_t = b0 + b1*T + b2*D + b3*P + month_FEs + covid + e_t
#     T = time index (months from Jan 2018)
#     D = 1 if month >= Oct 2022 (level shift)
#     P = months since Oct 2022 (0 in pre-period; slope change)
#     b2 = immediate level shift at intervention
#     b3 = change in slope post-intervention
#   Standard errors: Newey-West HAC (lag = 6, prewhite = FALSE)
#
# Inputs:
#   - output/tables/monthly_panel.csv  (from script 01)
#
# Outputs (saved to output/):
#   - output/its_results/its_coefficients.csv   -- OLS + NW coefficients (all terms)
#   - output/its_results/its_newey_west.csv     -- manuscript-ready NW table (T/D/P/covid)
#   - output/its_results/its_sensitivity_dates.csv
#   - output/its_results/its_placebo_tests.csv
#   - output/its_results/comparative_its_panel.csv
#   - output/plots/ITS_*.png/.pdf               -- ITS figures (one per outcome)
#
# Runtime: ~1–2 min
# ==============================================================================
#
# Intervention: October 2022
#   Q3 2022 = 55 pursuit events, Q4 2022 = 200, Q1 2023 = 529
#   Dec 2022 = first month >100; Jan 2023 = first month >200
#   Oct 2022 chosen as conservative onset (first month of the Q4 ramp)

library(tidyverse)
library(lubridate)
library(scales)
library(sandwich)
library(lmtest)
library(patchwork)
library(here)



# Dirs and theme ---------------------------------------------------------------

dir.create(here("output"),             showWarnings = FALSE)
dir.create(here("output/plots"),       showWarnings = FALSE)
dir.create(here("output/tables"),      showWarnings = FALSE)
dir.create(here("output/its_results"), showWarnings = FALSE)

plot_dir    <- here("output/plots")
table_dir   <- here("output/tables")
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


# 1. Define intervention and build ITS variables ===============================

# Load monthly_panel from disk if not already in memory (standalone Rscript run)
if (!exists("monthly_panel")) {
  monthly_panel <- read_csv(here("output", "tables", "monthly_panel.csv"),
                            show_col_types = FALSE) %>%
    mutate(month_date = as.Date(month_date))
  message("Loaded monthly_panel from output/tables/monthly_panel.csv")
}

INTERVENTION <- as.Date("2022-10-01")

message("\nITS ANALYSIS — NYPD PURSUIT ESCALATION\n")
message("Intervention:", as.character(INTERVENTION), "\n\n")

its <- monthly_panel %>%
  filter(month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) %>%
  arrange(month_date) %>%
  mutate(
    T = row_number(),                                        # time index
    D = as.integer(month_date >= INTERVENTION),              # post indicator
    P = pmax(0L, as.integer(round(                           # time since intervention
      difftime(month_date, INTERVENTION, units = "days") / 30.44))),
    month_factor = factor(month(month_date)),                # seasonality
    covid = as.integer(month_date >= as.Date("2020-03-01") & # COVID period
                         month_date <= as.Date("2021-06-01"))
  )

message("n =", nrow(its), "months\n")
message("Pre:", sum(its$D == 0), "| Post:", sum(its$D == 1), "\n\n")


# 2. Fit segmented regression for each outcome =================================

outcomes <- list(
  list(var = "pursuit_events",     label = "Pursuit Broadcasts",                col = COL_PURSUIT),
  list(var = "pursuit_crashes",    label = "Pursuit Collisions",                col = COL_COLLISION),
  list(var = "robbery_count",      label = "Robberies",                         col = COL_ROBBERY),
  list(var = "shooting_incidents", label = "Shooting Incidents",                col = COL_SHOOTING),
  list(var = "total_gun_events",   label = "Gun Violence (Shootings + Shots Fired)", col = COL_GUNVIOL)
)

its_results <- list()

for (o in outcomes) {
  
  message("---", o$label, "---\n")
  
  # Fit: Y = b0 + b1*T + b2*D + b3*P + seasonality + covid
  y <- its[[o$var]]
  mod <- lm(y ~ T + D + P + month_factor + covid, data = its)
  
  # OLS and Newey-West results are stored in its_results; suppress direct printing
  # during automated runs. The summary table is written to its_coefficients.csv.
  # Newey-West HAC standard errors (lag = 6)
  nw <- coeftest(mod, vcov = NeweyWest(mod, lag = 6, prewhite = FALSE))
  
  # Durbin-Watson
  resids <- residuals(mod)
  dw <- sum(diff(resids)^2) / sum(resids^2)
  message("\nDurbin-Watson:", round(dw, 3), "\n")
  
  # Ljung-Box
  lb <- Box.test(resids, lag = 12, type = "Ljung-Box")
  message("Ljung-Box p (lag 12):", round(lb$p.value, 4), "\n")
  
  # Counterfactual: set D = 0, P = 0.
  # Use a local copy to avoid mutating the parent-scope `its` with columns
  # from the last iteration (which would create unexpected state for downstream code).
  its_loop <- its
  its_loop$fitted         <- predict(mod)
  its_loop$counterfactual <- predict(mod, newdata = its %>% mutate(D = 0L, P = 0L))
  its_loop$observed       <- y
  
  # Cumulative and average effect in post-period
  post <- its_loop %>% filter(D == 1)
  cum_eff <- sum(post$fitted - post$counterfactual)
  avg_eff <- mean(post$fitted - post$counterfactual)
  message("Cumulative effect: ", round(cum_eff, 1))
  message("Average monthly effect: ", round(avg_eff, 1))

  # Plot: observed + fitted + counterfactual
  p <- ggplot(its_loop, aes(month_date)) +
    geom_point(aes(y = observed), color = o$col, alpha = 0.4, size = 1.2) +
    geom_line(aes(y = fitted), color = o$col, linewidth = 1) +
    geom_line(aes(y = counterfactual), color = "grey40",
              linetype = "dashed", linewidth = 0.8) +
    geom_vline(xintercept = INTERVENTION, linetype = "dotted",
               color = "grey30", linewidth = 0.6) +
    annotate("text", x = INTERVENTION, y = Inf, label = "Intervention",
             vjust = 2, hjust = -0.1, size = 3.5, color = "grey30") +
    geom_ribbon(data = its_loop %>% filter(D == 1),
                aes(ymin = counterfactual, ymax = fitted),
                fill = o$col, alpha = 0.15) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    labs(
      title = if (o$label == "Gun Violence (Shootings + Shots Fired)")
                  "ITS: Gun Violence"
              else paste0("ITS: ", o$label),
      x = NULL, y = stringr::str_wrap(paste("Monthly", o$label), width = 30)
    ) +
    theme_pursuit()
  
  save_plot(p, paste0("ITS_", gsub(" ", "_", o$label)))
  
  # Store results
  its_results[[o$label]] <- list(
    model      = mod,
    nw_test    = nw,
    dw_stat    = dw,
    lb_pvalue  = lb$p.value,
    cum_effect = cum_eff,
    avg_effect = avg_eff,
    plot_df    = its_loop %>%
      select(month_date, observed, fitted, counterfactual, D)
  )
}


# 2b. Composite 4-panel ITS figure (manuscript Figure 2) =======================
#
# JQC R1 figure comment: the previous version stitched four separately-saved
# PNGs with magick::image_trim in the .qmd, which cropped panel titles and left
# uneven whitespace, and used a different hue per panel that carried no
# information. This builds one patchwork composite: a single neutral colour for
# every panel, a 2-year x-axis to de-crowd the labels, consistent margins, and
# one shared caption. The .qmd chunk just includes this PNG.

NEUTRAL_ITS <- "#2B2D42"   # single slate colour for all four panels

panel_its <- function(df, ttl) {
  ggplot(df, aes(month_date)) +
    geom_ribbon(data = filter(df, D == 1),
                aes(ymin = counterfactual, ymax = fitted),
                fill = NEUTRAL_ITS, alpha = 0.15) +
    geom_point(aes(y = observed), color = NEUTRAL_ITS, alpha = 0.35, size = 0.9) +
    geom_line(aes(y = fitted), color = NEUTRAL_ITS, linewidth = 0.8) +
    geom_line(aes(y = counterfactual), color = "grey55",
              linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = INTERVENTION, linetype = "dotted",
               color = "grey30", linewidth = 0.5) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 expand = expansion(mult = 0.02)) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.10))) +
    labs(title = ttl, x = NULL, y = NULL) +
    theme_pursuit(base_size = 11) +
    theme(plot.title   = element_text(size = rel(1.0), face = "bold"),
          plot.margin  = margin(6, 10, 6, 8))
}

comp_specs <- list(
  c("Pursuit Collisions",                     "Pursuit-related collisions"),
  c("Shooting Incidents",                     "Shooting incidents"),
  c("Gun Violence (Shootings + Shots Fired)", "Gun violence (shootings + shots-fired)"),
  c("Robberies",                              "Robberies")
)

its_panels <- lapply(comp_specs, function(s)
  panel_its(its_results[[s[1]]]$plot_df, s[2]))
its_panels[[1]] <- its_panels[[1]] + labs(y = "Monthly count")
its_panels[[3]] <- its_panels[[3]] + labs(y = "Monthly count")

its_composite <- (its_panels[[1]] | its_panels[[2]]) /
                 (its_panels[[3]] | its_panels[[4]]) +
  plot_annotation(
    caption = paste0(
      "Solid line: segmented regression fit. Dashed line: counterfactual ",
      "projected from the pre-intervention trend.\nShaded band: post-intervention ",
      "effect (fit minus counterfactual). Dotted vertical line: October 2022 ",
      "escalation.\nPoints: observed monthly counts. Newey-West HAC standard ",
      "errors (lag = 6)."
    )
  ) &
  theme(plot.caption = element_text(color = "grey45", size = 8, hjust = 0,
                                    margin = margin(t = 8)))

ggsave(file.path(plot_dir, "ITS_composite_4panel.png"), its_composite,
       width = 9, height = 7, dpi = 300, bg = "white")
ggsave(file.path(plot_dir, "ITS_composite_4panel.pdf"), its_composite,
       width = 9, height = 7, bg = "white")
message("Saved: ITS_composite_4panel.png / .pdf")


# 3. Summary table =============================================================

its_summary <- map_dfr(outcomes, function(o) {
  mod <- its_results[[o$label]]$model
  nw  <- coeftest(mod, vcov = NeweyWest(mod, lag = 6, prewhite = FALSE))
  
  broom::tidy(mod, conf.int = TRUE) %>%
    filter(term %in% c("T", "D", "P", "covid")) %>%
    mutate(
      outcome     = o$label,
      nw_se       = nw[match(term, rownames(nw)), "Std. Error"],
      nw_pvalue   = nw[match(term, rownames(nw)), "Pr(>|t|)"]
    )
})

message("ITS Summary (OLS + Newey-West) written to its_coefficients.csv")

write_csv(its_summary, file.path(results_dir, "its_coefficients.csv"))


# 4. Sensitivity: vary intervention date =======================================

message("\n\n--- Sensitivity: intervention date ---\n\n")

sens_dates <- as.Date(c("2022-09-01", "2022-10-01", "2022-11-01",
                        "2022-12-01", "2023-01-01"))

sens_results <- map_dfr(outcomes, function(o) {
  map_dfr(sens_dates, function(int_date) {
    d <- its %>%
      mutate(
        D_s = as.integer(month_date >= int_date),
        P_s = pmax(0L, as.integer(round(
          difftime(month_date, int_date, units = "days") / 30.44)))
      )
    mod <- lm(d[[o$var]] ~ T + D_s + P_s + month_factor + covid, data = d)
    # Use Newey-West HAC SEs (lag = 6, matching main results) for CIs.
    # OLS CIs would be too narrow given autocorrelation in monthly crime data.
    nw_s  <- coeftest(mod, vcov = NeweyWest(mod, lag = 6, prewhite = FALSE))
    nw_ci <- data.frame(
      term = rownames(nw_s),
      estimate  = nw_s[, "Estimate"],
      std.error = nw_s[, "Std. Error"],
      statistic = nw_s[, "t value"],
      p.value   = nw_s[, "Pr(>|t|)"]
    ) %>%
      filter(term %in% c("D_s", "P_s")) %>%
      mutate(
        conf.low  = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error,
        outcome = o$label,
        intervention_date = int_date
      )
    nw_ci
  })
})

message("Level shift (D) by intervention date written to its_sensitivity_dates.csv")

write_csv(sens_results, file.path(results_dir, "its_sensitivity_dates.csv"))

# Plot
p_sens <- sens_results %>%
  filter(term == "D_s") %>%
  ggplot(aes(intervention_date, estimate, color = outcome)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~outcome, scales = "free_y") +
  scale_color_manual(values = c(
    "Pursuit Broadcasts" = COL_PURSUIT, "Pursuit Collisions" = COL_COLLISION,
    "Robberies" = COL_ROBBERY, "Shooting Incidents" = COL_SHOOTING,
    "Gun Violence (Shootings + Shots Fired)" = COL_GUNVIOL
  )) +
  labs(
    title   = "Sensitivity: Level Shift by Intervention Date",
    subtitle = "Estimates and 95% CIs for D across candidate dates",
    x = "Intervention Date", y = "Estimated Level Change",
    caption = "Segmented regression with seasonality + COVID"
  ) +
  theme_pursuit() +
  theme(legend.position = "none")

save_plot(p_sens, "ITS_sensitivity_dates", w = 11, h = 7)


# 5. Placebo tests (false interventions in pre-period) =========================

message("\n\n--- Placebo tests ---\n\n")

placebo_dates <- seq.Date(as.Date("2019-01-01"), as.Date("2022-06-01"), by = "3 months")
pre <- its %>% filter(D == 0)

placebo_results <- map_dfr(
  outcomes[3:5],  # robberies, shootings, gun violence
  function(o) {
    map_dfr(placebo_dates, function(p_date) {
      d <- pre %>%
        mutate(
          D_p = as.integer(month_date >= p_date),
          P_p = pmax(0L, as.integer(round(
            difftime(month_date, p_date, units = "days") / 30.44)))
        )
      mod <- lm(d[[o$var]] ~ T + D_p + P_p + month_factor + covid, data = d)
      broom::tidy(mod, conf.int = TRUE) %>%
        filter(term == "D_p") %>%
        mutate(outcome = o$label, placebo_date = p_date)
    })
  }
)

message("Placebo test results written to its_placebo_tests.csv")

write_csv(placebo_results, file.path(results_dir, "its_placebo_tests.csv"))

# Real effects for comparison
real_d <- its_summary %>%
  filter(term == "D") %>%
  select(outcome, estimate) %>%
  mutate(placebo_date = INTERVENTION)

p_placebo <- placebo_results %>%
  ggplot(aes(placebo_date, estimate)) +
  geom_point(color = "grey50", size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                color = "grey60", width = 20) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey30") +
  geom_point(data = real_d, aes(placebo_date, estimate),
             color = COL_PURSUIT, size = 4, shape = 18) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title   = "Placebo Tests: False Intervention Dates (Pre-Period Only)",
    subtitle = "Grey = placebo estimates | Red diamond = real effect",
    x = "Placebo Date", y = "Estimated Level Shift",
    caption = "Tested at 3-month intervals in the pre-period."
  ) +
  theme_pursuit()

save_plot(p_placebo, "ITS_placebo_tests", w = 11, h = 6)


# 6. Publication table — regenerate its_newey_west.csv from canonical model ====
#
# The manuscript setup chunk loads its_newey_west.csv for all inline ITS stats.
# This section regenerates that file from the model fitted in this script
# (the canonical Y = b0 + b1*T + b2*D + b3*P + seasonality + covid specification)
# so that the manuscript always traces back to the stated model.
#
# Term mapping: T → "t", D → "post", P → "t_since" (manuscript column names).
# Outcome mapping: matches the labels used in the `outcomes` list above.

message("\n--- Regenerating its_newey_west.csv from canonical model ---\n\n")

its_pub <- its_summary %>%
  filter(term %in% c("T", "D", "P", "covid")) %>%
  mutate(
    term = recode(term,
      "T"     = "t",
      "D"     = "post",
      "P"     = "t_since",
      "covid" = "covid"
    ),
    # Rename outcomes to match manuscript filter strings
    outcome = recode(outcome,
      "Pursuit Broadcasts"                     = "Pursuits",
      "Pursuit Collisions"                     = "Collisions",
      "Robberies"                              = "Robberies",
      "Shooting Incidents"                     = "Shootings",
      "Gun Violence (Shootings + Shots Fired)" = "Gun Violence"
    )
  ) %>%
  # Column names match broom::tidy() conventions expected by the manuscript
  # setup chunk (pull(std.error), pull(p.value))
  select(outcome, term, estimate, std.error = nw_se, p.value = nw_pvalue)

write_csv(its_pub, file.path(results_dir, "its_newey_west.csv"))
message("  Regenerated: output/its_results/its_newey_west.csv\n")
message("  Source: canonical script 03 model (Y = b0 + b1*T + b2*D + b3*P + seasonality + covid)\n")
message("  Estimates should now match its_coefficients.csv point estimates.\n\n")

# Verify consistency: D coefficient for shootings should match its_coefficients.csv
shoot_d_pub  <- its_pub    %>% filter(outcome == "Shootings",         term == "post") %>% pull(estimate)
shoot_d_coef <- its_summary %>% filter(outcome == "Shooting Incidents", term == "D")    %>% pull(estimate)
stopifnot(
  "its_newey_west.csv Shooting D does not match its_coefficients.csv — check recode()" =
    abs(shoot_d_pub - shoot_d_coef) < 0.001
)
message("  Consistency check: Shooting D matches its_coefficients.csv ✓\n\n")


# 7. Comparative ITS — Stacked Annual Windows ==================================
#
# Descriptive visual comparison: stack Oct–Sep windows across five years and
# overlay them on the same axes. Seasonality is controlled by comparing the
# same calendar months across years. Gray lines = pre-intervention windows;
# bold colored lines = post-Oct 2022 windows.
#
# Windows:
#   Oct 2019 – Sep 2020 (pre)
#   Oct 2020 – Sep 2021 (pre, COVID-affected)
#   Oct 2021 – Sep 2022 (immediate pre-period)
#   Oct 2022 – Sep 2023 (post-year 1)
#   Oct 2023 – Sep 2024 (post-year 2)
#
# Note: Oct 2024 – Sep 2025 (post-year 3) is available but Sept 2025 is the
# right-censor date; included as partial year.

message("\n7. Comparative ITS — stacked annual windows...\n")

# Build stacked panel
windows <- tribble(
  ~window_label,        ~window_start,            ~window_end,              ~period,  ~alpha_val,
  "Oct 2019–Sep 2020",  as.Date("2019-10-01"),    as.Date("2020-09-01"),    "pre",    0.5,
  "Oct 2020–Sep 2021",  as.Date("2020-10-01"),    as.Date("2021-09-01"),    "pre",    0.5,
  "Oct 2021–Sep 2022",  as.Date("2021-10-01"),    as.Date("2022-09-01"),    "pre",    0.8,
  "Oct 2022–Sep 2023",  as.Date("2022-10-01"),    as.Date("2023-09-01"),    "post",   1.0,
  "Oct 2023–Sep 2024",  as.Date("2023-10-01"),    as.Date("2024-09-01"),    "post",   1.0
)

comp_its_panel <- windows %>%
  rowwise() %>%
  reframe(
    window_label = window_label,
    period       = period,
    alpha_val    = alpha_val,
    month_date   = seq.Date(window_start, window_end, by = "month")
  ) %>%
  left_join(
    monthly_panel %>% select(month_date, shooting_incidents, robbery_count),
    by = "month_date"
  ) %>%
  mutate(
    # Relative month within Oct–Sep window: Oct = 1, Sep = 12
    rel_month = ((month(month_date) - 10) %% 12) + 1,
    # Color grouping: post years highlighted
    color_group = case_when(
      period == "post" & str_detect(window_label, "2022") ~ "Post Year 1",
      period == "post" & str_detect(window_label, "2023") ~ "Post Year 2",
      str_detect(window_label, "2021.Sep 2022")           ~ "Pre (immediate)",
      TRUE                                                  ~ "Pre"
    )
  ) %>%
  filter(!is.na(shooting_incidents))

write_csv(comp_its_panel, file.path(results_dir, "comparative_its_panel.csv"))
message("  Saved: output/its_results/comparative_its_panel.csv\n")

# Color palette for comparative ITS
COMP_COLORS <- c(
  "Pre"              = "grey70",
  "Pre (immediate)"  = "grey30",
  "Post Year 1"      = COL_SHOOTING,
  "Post Year 2"      = "#A0349E"
)
LINE_SIZES <- c("Pre" = 0.5, "Pre (immediate)" = 0.8, "Post Year 1" = 1.1, "Post Year 2" = 1.1)
MONTH_LABELS <- c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar",
                  "Apr", "May", "Jun", "Jul", "Aug", "Sep")

# Shooting comparative ITS
p_comp_shoot <- ggplot(
  comp_its_panel,
  aes(x = rel_month, y = shooting_incidents,
      color = color_group, linewidth = color_group, group = window_label)
) +
  geom_line(alpha = 0.85) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_x_continuous(breaks = 1:12, labels = MONTH_LABELS) +
  scale_color_manual(values = COMP_COLORS, name = NULL) +
  scale_linewidth_manual(values = LINE_SIZES, name = NULL, guide = "none") +
  labs(
    title    = "Shooting Incidents: Annual Oct–Sep Windows",
    subtitle = "Comparing same calendar months across years — controls for seasonality",
    x        = "Month within window (Oct = start of pursuit era each year)",
    y        = "Shooting incidents (NYC monthly count)",
    caption  = paste0(
      "Gray = pre-intervention windows; colored = post-Oct 2022 windows.\n",
      "Descriptive comparison; not a formal identification test."
    )
  ) +
  theme_pursuit()

save_plot(p_comp_shoot, "fig_comparative_its_shooting")
message("  Saved: fig_comparative_its_shooting.png/.pdf\n")

# Robbery comparative ITS
p_comp_rob <- ggplot(
  comp_its_panel,
  aes(x = rel_month, y = robbery_count,
      color = color_group, linewidth = color_group, group = window_label)
) +
  geom_line(alpha = 0.85) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_x_continuous(breaks = 1:12, labels = MONTH_LABELS) +
  scale_color_manual(values = c("Pre" = "grey70", "Pre (immediate)" = "grey30",
                                "Post Year 1" = COL_ROBBERY, "Post Year 2" = "#004F6E"),
                     name = NULL) +
  scale_linewidth_manual(values = LINE_SIZES, name = NULL, guide = "none") +
  labs(
    title    = "Robbery Counts: Annual Oct–Sep Windows",
    subtitle = "Comparing same calendar months across years — controls for seasonality",
    x        = "Month within window (Oct = start of pursuit era each year)",
    y        = "Robbery count (NYC monthly count)",
    caption  = paste0(
      "Gray = pre-intervention windows; colored = post-Oct 2022 windows.\n",
      "Descriptive comparison; not a formal identification test."
    )
  ) +
  theme_pursuit()

save_plot(p_comp_rob, "fig_comparative_its_robbery")
message("  Saved: fig_comparative_its_robbery.png/.pdf\n")

message("  Comparative ITS complete.\n\n")


# Done =========================================================================

message("\nResults saved to:", results_dir, "\n")
message("Plots saved to:", plot_dir, "\n\n")

