# ==============================================================================
# Vehicle Pursuits: Borough-Level Interrupted Time Series Analysis
# ==============================================================================
#
# Author:   Justin Nix
# Source:   Extends script 03-its_analysis.R to borough level
#
# Purpose:
#   Run the same ITS segmented regression as script 03 separately for each of
#   the five NYC boroughs across all four outcomes (pursuit crashes, shooting
#   incidents, gun violence, robbery). Produces a single 5-row × 4-column
#   composite figure. If deterrence is operating, boroughs with greater
#   pursuit intensity should show larger crime reductions (more negative b2/b3).
#
# Inputs:
#   - output/national_did/boro_monthly_panel.csv   (from script 08; borough x month crimes)
#   - output/national_did/boro_monthly_pursuit.csv (from script 08; borough x month pursuits)
#
# Outputs (saved to output/):
#   - its_results/its_borough_coefficients.csv
#       20-row table (5 boroughs x 4 outcomes): b2, b3, NW SEs, p-values
#   - its_results/its_borough_models.rds
#       All 20 fitted lm objects with NW test results and per-borough data
#   - its_results/its_borough_pursuit_intensity.csv
#       Borough-level pursuit exposure summary (pre/post means and ratio)
#   - plots/fig_borough_its_combined.pdf/.png
#       5×4 composite ITS figure (5 boroughs x 4 outcomes, landscape)
#   - publication_tables/table_borough_its.csv
#       Formatted coefficient table for manuscript (20 rows)
#
# Runtime: ~1 min
# ==============================================================================

library(tidyverse)
library(lubridate)
library(scales)
library(sandwich)
library(lmtest)
library(janitor)
library(here)
library(ggh4x)


# 0. Setup =====================================================================
# Project dirs, theme, colour palette, and global constants

dir.create(here("output", "its_results"),        showWarnings = FALSE, recursive = TRUE)
dir.create(here("output", "plots"),              showWarnings = FALSE, recursive = TRUE)
dir.create(here("output", "publication_tables"), showWarnings = FALSE, recursive = TRUE)

# Reduced base_size for 5×4 composite (each panel ~3×2 inches at 13×11 output)
theme_pursuit <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.05), margin = margin(b = 5)),
      plot.subtitle    = element_text(color = "grey30", size = rel(0.80), margin = margin(b = 6)),
      plot.caption     = element_text(color = "grey50", size = rel(0.65), hjust = 0, margin = margin(t = 6)),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      axis.title.x     = element_text(margin = margin(t = 5), size = rel(0.85)),
      axis.title.y     = element_text(margin = margin(r = 5), size = rel(0.85)),
      axis.text        = element_text(color = "grey30", size = rel(0.85)),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      plot.margin      = margin(8, 8, 8, 8)
    )
}

# Outcome colour palette — consistent with script 03 and manuscript
COL_COLLISION <- "#F77F00"
COL_SHOOTING  <- "#6A0572"
COL_GUNVIOL   <- "#1B998B"
COL_ROBBERY   <- "#003049"

#' Save a ggplot as both PNG and PDF with consistent dimensions
save_plot <- function(p, name, w = 10, h = 7) {
  ggsave(here("output", "plots", paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(here("output", "plots", paste0(name, ".pdf")),
         plot = p, width = w, height = h, bg = "white")
}

INTERVENTION <- as.Date("2022-10-01")
BOROUGHS     <- c("Bronx", "Brooklyn", "Manhattan", "Queens", "Staten Island")


# 1. Load Data =================================================================

message("Loading borough panel data...")

boro_panel <- read_csv(
  here("output", "national_did", "boro_monthly_panel.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  mutate(month_date = as.Date(month_date))

boro_pursuit <- read_csv(
  here("output", "national_did", "boro_monthly_pursuit.csv"),
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  mutate(month_date = as.Date(month_date))

# Verify all four outcome columns are present
required_cols <- c("pursuit_crashes", "shooting_incidents", "total_gun_events", "robbery_count")
missing_cols  <- setdiff(required_cols, names(boro_panel))
if (length(missing_cols) > 0) {
  stop("boro_monthly_panel.csv is missing required columns: ",
       paste(missing_cols, collapse = ", "),
       "\nRe-run script 08 first.")
}

message("  Loaded: ", nrow(boro_panel), " borough-month rows | ",
        min(boro_panel$month_date), " to ", max(boro_panel$month_date))


# 2. Build ITS Panel ===========================================================

message("Building ITS variables...")

# Analysis window: January 2018 – September 2025 (consistent with script 03)
its_boro <- boro_panel %>%
  filter(
    month_date >= as.Date("2018-01-01"),
    month_date <= as.Date("2025-12-01")
  ) %>%
  arrange(boro, month_date) %>%
  group_by(boro) %>%
  mutate(
    T            = row_number(),
    D            = as.integer(month_date >= INTERVENTION),
    P            = pmax(0L, as.integer(round(
      as.numeric(difftime(month_date, INTERVENTION, units = "days")) / 30.44))),
    month_factor = factor(month(month_date)),
    covid        = as.integer(
      month_date >= as.Date("2020-03-01") &
        month_date <= as.Date("2021-06-01"))
  ) %>%
  ungroup()

# Join pursuit counts; fill NA with 0 (months with no recorded events)
its_boro <- its_boro %>%
  left_join(
    boro_pursuit %>% select(month_date, boro, pursuit_events),
    by = c("month_date", "boro")
  ) %>%
  mutate(pursuit_events = replace_na(pursuit_events, 0L))

message("  n = ", nrow(its_boro), " borough-month rows across ", n_distinct(its_boro$boro), " boroughs")


# 3. Pursuit Intensity Summary =================================================

message("Computing pursuit intensity by borough...")

pursuit_intensity <- its_boro %>%
  group_by(boro) %>%
  summarise(
    mean_pre  = round(mean(pursuit_events[D == 0], na.rm = TRUE), 1),
    mean_post = round(mean(pursuit_events[D == 1], na.rm = TRUE), 1),
    ratio     = round(mean(pursuit_events[D == 1], na.rm = TRUE) /
                        pmax(mean(pursuit_events[D == 0], na.rm = TRUE), 0.1), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_post))

write_csv(pursuit_intensity, here("output", "its_results", "its_borough_pursuit_intensity.csv"))
message("  Saved: its_results/its_borough_pursuit_intensity.csv")


# 4. Fit ITS Models ============================================================

message("Fitting ITS models (", length(BOROUGHS), " boroughs x 4 outcomes)...")

outcomes_boro <- list(
  list(var = "pursuit_crashes",    label = "Pursuit Crashes",    col = COL_COLLISION),
  list(var = "shooting_incidents", label = "Shooting Incidents", col = COL_SHOOTING),
  list(var = "total_gun_events",   label = "Gun Violence",       col = COL_GUNVIOL),
  list(var = "robbery_count",      label = "Robbery",            col = COL_ROBBERY)
)

all_coefs  <- list()
all_models <- list()

for (o in outcomes_boro) {
  for (b in BOROUGHS) {

    d <- its_boro %>% filter(boro == b)
    y <- d[[o$var]]

    # ITS specification: Y = b0 + b1*T + b2*D + b3*P + seasonality + covid
    mod <- lm(y ~ T + D + P + month_factor + covid, data = d)

    # Newey-West HAC SEs (lag = 6, consistent with script 03).
    nw <- coeftest(mod, vcov = NeweyWest(mod, lag = 6, prewhite = FALSE))

    # Counterfactual: project pre-period trend forward (D = 0, P = 0)
    d$fitted         <- predict(mod)
    d$counterfactual <- predict(mod, newdata = d %>% mutate(D = 0L, P = 0L))
    d$observed       <- y

    key <- paste0(b, "___", o$var)
    all_models[[key]] <- list(model = mod, nw = nw, data = d, outcome = o, boro = b)

    # Extract b2 (level shift) and b3 (slope change) with NW SEs and p-values
    all_coefs[[key]] <- tibble(
      boro       = b,
      outcome    = o$label,
      b2         = nw["D", "Estimate"],
      b2_nw_se   = nw["D", "Std. Error"],
      b2_p       = nw["D", "Pr(>|t|)"],
      b3         = nw["P", "Estimate"],
      b3_nw_se   = nw["P", "Std. Error"],
      b3_p       = nw["P", "Pr(>|t|)"]
    )
  }
}

saveRDS(all_models, here("output", "its_results", "its_borough_models.rds"))
message("  Saved: its_results/its_borough_models.rds (", length(all_models), " model objects)")


# 5. Save Coefficient Table ====================================================

message("Saving coefficient table...")

coef_table <- bind_rows(all_coefs) %>%
  arrange(outcome, boro)

write_csv(coef_table, here("output", "its_results", "its_borough_coefficients.csv"))
message("  Saved: its_results/its_borough_coefficients.csv (", nrow(coef_table), " rows)")

# Table already saved to its_borough_coefficients.csv above.
message("  Level shift (b2) summary: see its_results/its_borough_coefficients.csv")


# 6. Build 5×4 Composite Borough ITS Figure ====================================
#
# Layout: 5 rows (boroughs) × 4 columns (outcomes), landscape orientation.
# Each panel shows observed points, fitted line, counterfactual dashed line,
# intervention vline, and b2 annotation. Color varies by outcome column.
#
# The annotation data frame carries b2 and p for each panel; formatted as
# a small label in the top-right corner so it doesn't compete with the line.

message("Building 5×4 composite borough ITS figure...")

# Outcome column order and metadata
outcome_levels <- c("Pursuit Crashes", "Shooting Incidents", "Gun Violence", "Robbery")
outcome_colors <- c(
  "Pursuit Crashes"    = COL_COLLISION,
  "Shooting Incidents" = COL_SHOOTING,
  "Gun Violence"       = COL_GUNVIOL,
  "Robbery"            = COL_ROBBERY
)

# Collect all fitted series into one long data frame
plot_long <- map_dfr(BOROUGHS, function(b) {
  map_dfr(outcomes_boro, function(o) {
    key   <- paste0(b, "___", o$var)
    d     <- all_models[[key]]$data
    d %>%
      select(month_date, observed, fitted, counterfactual, D, boro) %>%
      mutate(outcome_label = o$label, col = o$col)
  })
}) %>%
  mutate(
    boro          = factor(boro,          levels = BOROUGHS),
    outcome_label = factor(outcome_label, levels = outcome_levels)
  )

# Build annotation data frame (b2 + p per panel)
annotations_df <- map_dfr(BOROUGHS, function(b) {
  map_dfr(outcomes_boro, function(o) {
    key  <- paste0(b, "___", o$var)
    nw   <- all_models[[key]]$nw
    b2   <- nw["D", "Estimate"]
    b2_p <- nw["D", "Pr(>|t|)"]
    p_str <- if (b2_p < .001) "p < .001" else
      paste0("p = ", gsub("^0\\.", ".", sprintf("%.3f", b2_p)))
    tibble(
      boro          = b,
      outcome_label = o$label,
      label         = paste0("b\u2082 = ", sprintf("%.1f", b2), "\n", p_str),
      # Anchor at the right edge of the data window (right-censored Sep 2025).
      # Use max observed date rather than Inf to stay type-safe on a Date axis.
      x             = as.Date("2025-12-01"),
      y             = Inf
    )
  })
}) %>%
  mutate(
    boro          = factor(boro,          levels = BOROUGHS),
    outcome_label = factor(outcome_label, levels = outcome_levels)
  )

# Ribbon data (post-intervention only; shows fitted vs. counterfactual gap)
ribbon_data <- plot_long %>% filter(D == 1)

# Build composite figure.
# ggh4x::facet_grid2() with independent = "y" gives each panel its own y-axis
# and tick labels — required because pursuit crashes (~5/month) and robberies
# (~1,500/month) share a row but differ ~300x in magnitude. A row-shared
# y-scale (standard facet_grid) would compress all non-robbery panels to flat lines.

p_combined <- ggplot(plot_long, aes(x = month_date)) +
  # Effect ribbon (post-period only)
  geom_ribbon(
    data     = ribbon_data,
    aes(ymin  = pmin(counterfactual, fitted),
        ymax  = pmax(counterfactual, fitted),
        fill  = outcome_label),
    alpha    = 0.13
  ) +
  # Counterfactual (dashed, grey)
  geom_line(aes(y = counterfactual), color = "grey45",
            linetype = "dashed", linewidth = 0.45) +
  # Fitted line (colored by outcome)
  geom_line(aes(y = fitted, color = outcome_label), linewidth = 0.7) +
  # Observed points (colored, semi-transparent)
  geom_point(aes(y = observed, color = outcome_label),
             alpha = 0.30, size = 0.5) +
  # Intervention vertical line
  geom_vline(xintercept = INTERVENTION, linetype = "dotted",
             color = "grey30", linewidth = 0.4) +
  # b2 + p annotation in top-right corner
  geom_text(
    data    = annotations_df,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    vjust  = 1.1, hjust = 1.0,
    size   = 4.0, color = "grey20", lineheight = 0.9
  ) +
  # Color and fill scales matched to outcomes
  scale_color_manual(values = outcome_colors, guide = "none") +
  scale_fill_manual( values = outcome_colors, guide = "none") +
  # Date axis: 2-year breaks; remove most x-axis labels except bottom row
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  # Independent y-scales: each panel gets its own axis (pursuit crashes ~5/month
  # vs. robberies ~1,500/month would otherwise share a row-level scale)
  ggh4x::facet_grid2(boro ~ outcome_label, scales = "free", independent = "y") +
  labs(
    title   = "Borough-Level Interrupted Time Series: All Four Outcomes",
    subtitle = paste0(
      "Solid = fitted ITS | Dashed = counterfactual | Shaded = effect | ",
      "Dotted vertical = Oct 2022 | b\u2082 = immediate level shift (NW HAC, lag = 6)"
    ),
    x = NULL
    # y omitted: axis.title.y blanked because each panel has its own independent y-scale
  ) +
  theme_pursuit(base_size = 14) +
  theme(
    strip.text.x    = element_text(face = "bold", size = 12),
    strip.text.y    = element_text(face = "bold", size = 12),
    panel.spacing   = unit(0.5, "lines"),
    axis.text.x     = element_text(size = 9, angle = 0),
    axis.text.y     = element_text(size = 10),
    axis.title.y    = element_blank(),
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40", size = 10)
  )

save_plot(p_combined, "fig_borough_its_combined", w = 14, h = 12)
message("  Saved: fig_borough_its_combined.png/pdf (14×12 landscape)")


# 7. Publication Table =========================================================

message("Creating publication table...")

#' Format a regression coefficient for a publication table cell
fmt_coef <- function(est, se, p) {
  sig   <- if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else ""
  p_str <- if (p < .001) "< .001" else gsub("^0\\.", ".", sprintf("%.3f", p))
  paste0(sprintf("%.2f", est), sig, " [p ", p_str, "] (SE = ", sprintf("%.2f", se), ")")
}

pub_table <- coef_table %>%
  rowwise() %>%
  mutate(
    `Level Shift b2 (SE)`  = fmt_coef(b2, b2_nw_se, b2_p),
    `Slope Change b3 (SE)` = fmt_coef(b3, b3_nw_se, b3_p)
  ) %>%
  ungroup() %>%
  select(
    Outcome = outcome,
    Borough = boro,
    `Level Shift b2 (SE)`,
    `Slope Change b3 (SE)`
  )

write_csv(pub_table, here("output", "publication_tables", "table_borough_its.csv"))
message("  Saved: publication_tables/table_borough_its.csv (", nrow(pub_table), " rows)")


# Done =========================================================================

message("Borough-Level ITS complete: ",
        nrow(coef_table), " coef rows, ",
        length(all_models), " models, 3 CSVs, composite 13×11 figure.")
