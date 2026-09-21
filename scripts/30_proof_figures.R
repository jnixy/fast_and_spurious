# ==============================================================================
# Vehicle Pursuits: Print-Size Figures for Springer Page Proofs (Figs 1, 2, 5, 7)
# ==============================================================================
#
# Author:   Hall & Nix
#
# Purpose:
#   Springer's page proofs flagged the fonts in Figures 1, 2, 5, and 7 as too
#   small. Those figures were drawn on 9-14 inch canvases and shrunk to the
#   printed page, so text lost roughly half to two-thirds of its size. This
#   script rebuilds them directly at print width so that point sizes are what
#   readers see. It reads saved intermediates and never overwrites the original
#   figures (scripts 03, 06, 14, 17 are untouched); outputs carry a _proof suffix.
#
#   Requested changes (JQC page-proof round):
#     Fig 1: drop title + bottom note; larger regime headers and y-axis title
#     Fig 2: drop bottom note; larger y-axis titles
#     Fig 5: drop title, subtitle, and b2/p panel labels; larger strips + ticks
#     Fig 7: drop panel subtitles + bottom note; larger y ticks; x-axis labels
#            become quarter indices Q-8 ... Q9+
#
# Inputs:
#   - output/tables/monthly_panel.csv                (Figs 1, 2; from script 01)
#   - output/its_results/its_borough_models.rds      (Fig 5; from script 14)
#   - output/precinct_did/pct_ddd_es_*.csv           (Fig 7; from script 17)
#   - output/its_results/its_coefficients.csv        (Fig 2 refit check)
#
# Outputs (saved to output/plots/proof/):
#   - event_regime_pursuit_events_proof.png/.pdf
#   - ITS_composite_4panel_proof.png/.pdf
#   - fig_borough_its_combined_proof.png/.pdf
#   - fig_precinct_ddd_combined_proof.png/.pdf
#
# Runtime: < 1 min
# ==============================================================================

library(tidyverse)
library(lubridate)
library(sandwich)
library(lmtest)
library(patchwork)
library(ggh4x)
library(here)


# 0. Setup =====================================================================

proof_dir <- here("output", "plots", "proof")
dir.create(proof_dir, showWarnings = FALSE, recursive = TRUE)

# Printed figure width in inches. Springer text block is assumed to be ~12.2 cm;
# change this one constant if the proofs show a different width.
PRINT_W <- 4.8

# Type sizes in points AT PRINT SIZE (figures are drawn at PRINT_W, so these
# are what readers see). Floor is 7 pt.
PT_TICK  <- 7
PT_AXIS  <- 8.5
PT_STRIP <- 8.5
PT_TITLE <- 8.5

INTERVENTION <- as.Date("2022-10-01")
ESCALATION   <- INTERVENTION
MADDREY      <- as.Date("2023-08-01")
TISCH        <- as.Date("2025-02-01")

COL_PURSUIT   <- "#D62828"
COL_COLLISION <- "#F77F00"
COL_ROBBERY   <- "#003049"
COL_SHOOTING  <- "#6A0572"
COL_GUNVIOL   <- "#1B998B"
NEUTRAL_ITS   <- "#2B2D42"

# ggplot geom_text sizes are in mm; convert from points
pt_to_mm <- function(pt) pt / ggplot2::.pt

theme_proof <- function() {
  theme_minimal(base_size = PT_TICK) +
    theme(
      plot.background    = element_rect(fill = "white", color = NA),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.25),
      axis.text          = element_text(color = "grey25", size = PT_TICK),
      axis.title.x       = element_text(size = PT_AXIS, margin = margin(t = 4)),
      axis.title.y       = element_text(size = PT_AXIS, margin = margin(r = 4)),
      plot.title         = element_text(face = "bold", size = PT_TITLE),
      strip.text         = element_text(face = "bold", size = PT_STRIP),
      legend.position    = "none",
      plot.margin        = margin(4, 6, 4, 4)
    )
}

save_proof <- function(p, name, h) {
  ggsave(file.path(proof_dir, paste0(name, "_proof.png")),
         plot = p, width = PRINT_W, height = h, dpi = 300, bg = "white")
  ggsave(file.path(proof_dir, paste0(name, "_proof.pdf")),
         plot = p, width = PRINT_W, height = h, bg = "white")
  message("  Saved: ", name, "_proof.png/.pdf (", PRINT_W, " x ", h, " in)")
}


# 1. Load Data =================================================================

panel <- read_csv(here("output", "tables", "monthly_panel.csv"),
                  show_col_types = FALSE) %>%
  mutate(month_date = as.Date(month_date)) %>%
  filter(month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-12-01")) %>%
  arrange(month_date)


# 2. Figure 1: Four-regime pursuit series ======================================
#
# Regime headers alternate between two heights so the narrow Escalation and
# Post-Tisch bands don't collide with their neighbours at the larger type size.

regime_rects <- tibble(
  xmin   = c(as.Date("2018-01-01"), ESCALATION, MADDREY, TISCH),
  xmax   = c(ESCALATION, MADDREY, TISCH, as.Date("2025-12-01")),
  regime = c("Pre-escalation", "Escalation", "Post-Maddrey", "Post-Tisch"),
  fill   = c("grey95", "#FDE8E8", "#FFECD2", "#E8F4FD"),
  vjust  = c(-0.6, -2.0, -0.6, -2.0)
) %>%
  mutate(
    mid = as.Date((as.numeric(xmin) + as.numeric(xmax)) / 2, origin = "1970-01-01"),
    # Post-Tisch is the narrowest band and sits at the right edge of the plot;
    # right-align its header to the band so it isn't clipped by the image margin
    lab_x = if_else(regime == "Post-Tisch", xmax, mid),
    hjust = if_else(regime == "Post-Tisch", 1, 0.5)
  )

p_fig1 <- ggplot(panel, aes(month_date, pursuit_events)) +
  geom_rect(data = regime_rects,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = regime),
            inherit.aes = FALSE, alpha = 0.55) +
  scale_fill_manual(values = setNames(regime_rects$fill, regime_rects$regime),
                    guide = "none") +
  geom_vline(xintercept = c(ESCALATION, MADDREY, TISCH),
             linetype = "dashed", color = "grey45", linewidth = 0.3) +
  geom_line(color = COL_PURSUIT, linewidth = 0.55) +
  geom_point(color = COL_PURSUIT, size = 0.6, alpha = 0.5) +
  geom_text(data = regime_rects, inherit.aes = FALSE,
            aes(x = lab_x, y = Inf, label = regime, vjust = vjust, hjust = hjust),
            size = pt_to_mm(PT_STRIP), fontface = "bold", color = "grey25") +
  coord_cartesian(clip = "off") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",
               expand = expansion(mult = 0.01)) +
  labs(x = NULL, y = "Monthly pursuit broadcasts") +
  theme_proof() +
  theme(plot.margin = margin(t = 24, r = 6, b = 4, l = 4))

message("Building Figure 1...")
save_proof(p_fig1, "event_regime_pursuit_events", h = 3.0)


# 3. Figure 2: Four-panel ITS ==================================================
#
# The per-outcome plot data frames are not saved by script 03, so the four
# segmented regressions are refit here with the identical specification
# (Y ~ T + D + P + month FE + covid) and checked against its_coefficients.csv.

its <- panel %>%
  mutate(
    T = row_number(),
    D = as.integer(month_date >= INTERVENTION),
    P = pmax(0L, as.integer(round(
      difftime(month_date, INTERVENTION, units = "days") / 30.44))),
    month_factor = factor(month(month_date)),
    covid = as.integer(month_date >= as.Date("2020-03-01") &
                         month_date <= as.Date("2021-06-01"))
  )

fit_its <- function(var, label) {
  y   <- its[[var]]
  mod <- lm(y ~ T + D + P + month_factor + covid, data = its)
  its %>%
    mutate(
      observed       = y,
      fitted         = predict(mod),
      counterfactual = predict(mod, newdata = its %>% mutate(D = 0L, P = 0L))
    ) %>%
    select(month_date, observed, fitted, counterfactual, D) %>%
    structure(b2 = unname(coef(mod)["D"]), label = label)
}

its_specs <- list(
  list(var = "pursuit_crashes",    label = "Pursuit Collisions",
       title = "Pursuit-related collisions"),
  list(var = "shooting_incidents", label = "Shooting Incidents",
       title = "Shooting incidents"),
  list(var = "total_gun_events",   label = "Gun Violence (Shootings + Shots Fired)",
       title = "Gun violence (shootings + shots-fired)"),
  list(var = "robbery_count",      label = "Robberies",
       title = "Robberies")
)

its_fits <- lapply(its_specs, function(s) fit_its(s$var, s$label))

# Refit check: the D (level shift) coefficient must match the saved table
its_saved <- read_csv(here("output", "its_results", "its_coefficients.csv"),
                      show_col_types = FALSE) %>%
  filter(term == "D")
for (i in seq_along(its_specs)) {
  saved_b2 <- its_saved %>% filter(outcome == its_specs[[i]]$label) %>% pull(estimate)
  new_b2   <- attr(its_fits[[i]], "b2")
  if (length(saved_b2) != 1 || abs(saved_b2 - new_b2) > 1e-6) {
    stop("ITS refit does not match its_coefficients.csv for ", its_specs[[i]]$label,
         " (saved = ", saved_b2, ", refit = ", new_b2, ")")
  }
}
message("Fig 2 refit matches its_coefficients.csv for all four outcomes.")

panel_its <- function(df, ttl) {
  ggplot(df, aes(month_date)) +
    geom_ribbon(data = filter(df, D == 1),
                aes(ymin = counterfactual, ymax = fitted),
                fill = NEUTRAL_ITS, alpha = 0.15) +
    geom_point(aes(y = observed), color = NEUTRAL_ITS, alpha = 0.35, size = 0.5) +
    geom_line(aes(y = fitted), color = NEUTRAL_ITS, linewidth = 0.5) +
    geom_line(aes(y = counterfactual), color = "grey55",
              linetype = "dashed", linewidth = 0.4) +
    geom_vline(xintercept = INTERVENTION, linetype = "dotted",
               color = "grey30", linewidth = 0.35) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y",
                 expand = expansion(mult = 0.02)) +
    scale_y_continuous(expand = expansion(mult = c(0.02, 0.10))) +
    labs(title = ttl, x = NULL, y = NULL) +
    theme_proof()
}

its_panels <- Map(function(df, s) panel_its(df, s$title), its_fits, its_specs)
its_panels[[1]] <- its_panels[[1]] + labs(y = "Monthly count")
its_panels[[3]] <- its_panels[[3]] + labs(y = "Monthly count")

p_fig2 <- (its_panels[[1]] | its_panels[[2]]) /
          (its_panels[[3]] | its_panels[[4]])

message("Building Figure 2...")
save_proof(p_fig2, "ITS_composite_4panel", h = 4.6)


# 4. Figure 5: Borough-level ITS grid ==========================================

all_models <- readRDS(here("output", "its_results", "its_borough_models.rds"))

BOROUGHS       <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")
outcome_levels <- c("Pursuit Crashes", "Shooting Incidents", "Gun Violence", "Robbery")
outcome_colors <- c(
  "Pursuit Crashes"    = COL_COLLISION,
  "Shooting Incidents" = COL_SHOOTING,
  "Gun Violence"       = COL_GUNVIOL,
  "Robbery"            = COL_ROBBERY
)

plot_long <- map_dfr(all_models, function(m) {
  m$data %>%
    select(month_date, observed, fitted, counterfactual, D, boro) %>%
    mutate(outcome_label = m$outcome$label)
}) %>%
  mutate(
    boro          = factor(boro,          levels = BOROUGHS),
    outcome_label = factor(outcome_label, levels = outcome_levels)
  )

ribbon_data <- plot_long %>% filter(D == 1)

# Two-line column headers so each fits a ~1 inch panel at print size
col_labeller <- as_labeller(c(
  "Pursuit Crashes"    = "Pursuit\nCrashes",
  "Shooting Incidents" = "Shooting\nIncidents",
  "Gun Violence"       = "Gun\nViolence",
  "Robbery"            = "Robbery"
))

# facet_grid2(independent = "y") gives each panel its own y-scale: pursuit
# crashes (~5/month) and robberies (~1,500/month) share a row but differ ~300x.
p_fig5 <- ggplot(plot_long, aes(x = month_date)) +
  geom_ribbon(data = ribbon_data,
              aes(ymin = pmin(counterfactual, fitted),
                  ymax = pmax(counterfactual, fitted),
                  fill = outcome_label),
              alpha = 0.13) +
  geom_line(aes(y = counterfactual), color = "grey45",
            linetype = "dashed", linewidth = 0.3) +
  geom_line(aes(y = fitted, color = outcome_label), linewidth = 0.45) +
  geom_point(aes(y = observed, color = outcome_label),
             alpha = 0.30, size = 0.2) +
  geom_vline(xintercept = INTERVENTION, linetype = "dotted",
             color = "grey30", linewidth = 0.3) +
  scale_color_manual(values = outcome_colors, guide = "none") +
  scale_fill_manual( values = outcome_colors, guide = "none") +
  scale_x_date(breaks = as.Date(c("2020-01-01", "2024-01-01")),
               date_labels = "%Y") +
  scale_y_continuous(breaks = scales::breaks_pretty(n = 3)) +
  ggh4x::facet_grid2(boro ~ outcome_label, scales = "free",
                     independent = "y", labeller = labeller(outcome_label = col_labeller)) +
  labs(x = NULL, y = NULL) +
  theme_proof() +
  theme(
    panel.spacing = unit(0.4, "lines"),
    strip.text.x  = element_text(face = "bold", size = PT_STRIP, lineheight = 0.9),
    strip.text.y  = element_text(face = "bold", size = PT_STRIP)
  )

message("Building Figure 5...")
save_proof(p_fig5, "fig_borough_its_combined", h = 6.8)


# 5. Figure 7: Precinct DiD event studies ======================================
#
# Quarter labels replace calendar labels. Q0 = Oct-Dec 2022, Q-1 = reference
# quarter (Jul-Sep 2022), Q9+ = Jan 2025 onward. Defined in the caption.

q_labels <- c(setNames(paste0("Q−", 8:1), -8:-1),
              setNames(paste0("Q", 0:8), 0:8),
              "9" = "Q9+")

plot_es_proof <- function(csv, title, col) {
  es_df <- read_csv(here("output", "precinct_did", csv), show_col_types = FALSE)

  # Explicit zero row at the reference bin so the line runs through it
  es_df <- bind_rows(
    es_df,
    tibble(outcome = es_df$outcome[1], time_bin = -1L,
           estimate = 0, std_error = NA_real_, conf_low = 0, conf_high = 0)
  ) %>%
    arrange(time_bin) %>%
    mutate(period = if_else(time_bin < 0, "Pre", "Post"))

  ggplot(es_df, aes(x = time_bin, y = estimate,
                    fill = period, color = period, shape = period)) +
    geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
    geom_vline(xintercept = -0.5, linetype = "dashed",
               color = "grey30", linewidth = 0.35) +
    geom_ribbon(aes(ymin = conf_low, ymax = conf_high), alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.45) +
    geom_point(size = 1.0) +
    scale_color_manual(values = c("Pre" = "grey50", "Post" = col)) +
    scale_fill_manual( values = c("Pre" = "grey50", "Post" = col)) +
    scale_shape_manual(values = c("Pre" = 1, "Post" = 16)) +
    annotate("text", x = -0.5, y = Inf, label = "Oct 2022",
             vjust = 2, hjust = 1.1, size = pt_to_mm(PT_TICK), color = "grey40") +
    scale_x_continuous(breaks = -8:9,
                       labels = function(x) q_labels[as.character(x)],
                       guide  = guide_axis(angle = 90)) +
    # True minus signs on y ticks to match the Q-labels
    scale_y_continuous(labels = scales::label_number(style_negative = "minus")) +
    labs(title = title, x = NULL, y = NULL) +
    theme_proof() +
    theme(plot.title = element_text(face = "bold", size = PT_TITLE - 0.5))
}

p7_s  <- plot_es_proof("pct_ddd_es_shooting.csv",
                       "DiD event study: Shooting Incidents", COL_SHOOTING)
p7_gv <- plot_es_proof("pct_ddd_es_gun_violence.csv",
                       "DiD event study: Gun Violence",      COL_GUNVIOL)
p7_r  <- plot_es_proof("pct_ddd_es_robbery.csv",
                       "DiD event study: Robbery",           COL_ROBBERY)
p7_c  <- plot_es_proof("pct_ddd_es_crashes.csv",
                       "DiD event study: Pursuit Crashes",   COL_COLLISION)

p_fig7 <- (p7_s | p7_gv) / (p7_r | p7_c)

message("Building Figure 7...")
save_proof(p_fig7, "fig_precinct_ddd_combined", h = 4.8)

message("Done. Proof figures in ", proof_dir)
