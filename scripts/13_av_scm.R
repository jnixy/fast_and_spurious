# 13_av_scm.R — National SCM Using AmericanViolence.org Data
#
# Treatment: NYC (all 5 boroughs aggregated), i_time = 2022 (annual)
# Donor pool: 97 AV cities (100 minus NYC, Chandler, Irvine)
# Method: tidysynth; predictors = individual year rates 2018–2021
# Inference: Fisher's exact p-value via permutation (97 placebos)
#
# Source: Sharkey, Patrick. "AmericanViolence.org: City, fatal and nonfatal
#   shootings. Princeton, NJ: www.americanviolence.org"
#
# Requires output from 11_av_data_prep.R
#
# NOTE (2026-03-05): This script is no longer part of the main publication
# pipeline. The AV-based national DiD/SCM analyses have been removed from the
# manuscript. This script is retained for reproducibility.
# Active pipeline: 01 → 03 → 06 → 07 → 08 → 14 → 17 → 15 → 16 → 00_regenerate

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidysynth)
  library(here)
})

cat("\n=== AV NATIONAL SCM ===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")

results_dir <- here("output", "national_scm")
plot_dir    <- here("output", "plots")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir,    showWarnings = FALSE, recursive = TRUE)

# Cities excluded after quality screening (matches script 12)
EXCLUDED_CITIES <- c("Chandler", "Irvine")

# Project theme and save function
COL_NYC     <- "#D62828"
COL_CONTROL <- "#003049"

theme_pursuit <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom"
    )
}

save_plot <- function(p, name, w = 10, h = 7) {
  ggsave(file.path(plot_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
  ggsave(file.path(plot_dir, paste0(name, ".pdf")), p, width = w, height = h)
  cat("  Saved:", name, "\n")
}


# 1. Load and prepare annual panel ============================================

cat("--- Loading and Preparing Annual Panel ---\n\n")

panel_file <- here("output", "national_did", "av_combined_panel.csv")
if (!file.exists(panel_file)) {
  stop("Run 11_av_data_prep.R first. Missing: ", panel_file)
}

monthly_raw <- read_csv(panel_file, show_col_types = FALSE) %>%
  mutate(month_date = as.Date(month_date)) %>%
  filter(!place_name %in% EXCLUDED_CITIES)

cat("Monthly panel after exclusions:", n_distinct(monthly_raw$place_name), "cities\n\n")

# Build annual panel with annualized 2025 rates.
# 2025 has only 9 months (Jan–Sep); multiply by 12/9 to get full-year equivalent.
annual_raw <- monthly_raw %>%
  group_by(place_name, state_abr, id, year, treated) %>%
  summarise(
    crime_count    = sum(crime_count),
    population_est = mean(population_est),
    n_months       = n(),
    .groups        = "drop"
  ) %>%
  mutate(
    # Annualize partial years before computing rate
    crime_count_ann = crime_count * 12 / n_months,
    shooting_rate   = (crime_count_ann / population_est) * 100000,
    city_state      = paste(place_name, state_abr, sep = ", ")
  )

cat("Annual rows (pre-balance):", nrow(annual_raw), "\n")
cat("Years:", min(annual_raw$year), "to", max(annual_raw$year), "\n\n")

# 2025 annualization check
cat("2025 annualization (n_months per city):\n")
annual_raw %>%
  filter(year == 2025) %>%
  count(n_months) %>%
  print()
cat("\n")


# 2. Require balanced panel ===================================================

cat("--- Building Balanced Panel ---\n\n")

# Every city must have data for all 8 years (2018–2025)
n_years_required <- n_distinct(annual_raw$year)   # should be 8

balanced_cities <- annual_raw %>%
  group_by(place_name, state_abr) %>%
  filter(n() == n_years_required) %>%
  ungroup()

n_dropped <- n_distinct(annual_raw$place_name) - n_distinct(balanced_cities$place_name)
cat("Required years per city:", n_years_required, "\n")
cat("Cities dropped (incomplete years):", n_dropped, "\n")
cat("Cities in balanced panel:", n_distinct(balanced_cities$place_name), "\n\n")

if (n_dropped > 0) {
  dropped_cities <- setdiff(
    unique(annual_raw$place_name),
    unique(balanced_cities$place_name)
  )
  cat("Dropped cities:", paste(dropped_cities, collapse = ", "), "\n\n")
}

# Verify NYC is treated and present
nyc_check <- balanced_cities %>% filter(place_name == "New York", state_abr == "NY")
if (nrow(nyc_check) != n_years_required) {
  stop("NYC is missing from the balanced panel. Check data.")
}
cat("NYC rows in balanced panel:", nrow(nyc_check), "(", n_years_required, "expected)\n")
cat("NYC treated:", unique(nyc_check$treated), "\n\n")

n_donors <- n_distinct(balanced_cities$place_name[balanced_cities$treated == 0])
cat("Donor cities:", n_donors, "\n\n")


# 3. SCM panel: a single unit identifier for tidysynth =======================

# tidysynth needs a clean unit identifier; use city_state as a character
scm_panel <- balanced_cities %>%
  select(city_state, year, shooting_rate, treated) %>%
  arrange(city_state, year)

cat("SCM panel:", nrow(scm_panel), "rows,", n_distinct(scm_panel$city_state), "units\n")
cat("Treated unit: New York, NY\n")
cat("Donors:", n_donors, "\n\n")


# 4. Run synthetic control =====================================================

cat("--- Running Synthetic Control (this may take ~1–2 min with", n_donors, "placebos) ---\n\n")

# SCM design:
#   i_time = 2022: first post-treatment year for annual data
#     (policy began Oct 2022; 2022 is the partial-treatment annual observation)
#   Predictors: individual annual shooting rates 2018, 2019, 2020, 2021
#   Optimization window: 2018:2021 (4 pre-treatment years, all clean)
#   generate_placebos = TRUE: enables Fisher's exact p-value from 97 permutations

set.seed(2022)  # reproducibility: permutation placebos in generate_placebos = TRUE
scm_out <- tryCatch({
  scm_panel %>%
    synthetic_control(
      outcome          = shooting_rate,
      unit             = city_state,
      time             = year,
      i_unit           = "New York, NY",
      i_time           = 2022,
      generate_placebos = TRUE
    ) %>%
    generate_predictor(time_window = 2018, shoot_2018 = shooting_rate) %>%
    generate_predictor(time_window = 2019, shoot_2019 = shooting_rate) %>%
    generate_predictor(time_window = 2020, shoot_2020 = shooting_rate) %>%
    generate_predictor(time_window = 2021, shoot_2021 = shooting_rate) %>%
    generate_weights(optimization_window = 2018:2021) %>%
    generate_control()
}, error = function(e) {
  cat("ERROR in synthetic_control():", conditionMessage(e), "\n")
  NULL
})

if (is.null(scm_out)) {
  stop("SCM failed. Check panel structure and donor pool.")
}

cat("SCM converged successfully.\n\n")


# 5. Extract results ===========================================================

cat("--- Extracting Results ---\n\n")

# Donor weights
weights <- scm_out %>%
  grab_unit_weights() %>%
  filter(weight > 0.001) %>%
  arrange(desc(weight))

cat("Non-trivial donor weights (> 0.001):\n")
print(as.data.frame(weights), row.names = FALSE)
cat("\n")

# Degeneracy check
max_weight <- max(weights$weight)
top_donor  <- weights$unit[which.max(weights$weight)]
if (max_weight > 0.80) {
  cat("WARNING: Degenerate result — top donor '", top_donor,
      "' receives", round(max_weight * 100, 1), "% of weight.\n\n",
      sep = "")
} else {
  cat("Weight distribution: well-dispersed (max weight =",
      round(max_weight * 100, 1), "% for", top_donor, ")\n\n")
}

# Gaps (actual vs. synthetic)
gaps <- scm_out %>%
  grab_synthetic_control() %>%
  mutate(gap = real_y - synth_y)

cat("NYC actual vs. synthetic shooting rate:\n")
print(as.data.frame(gaps %>% select(time_unit, real_y, synth_y, gap)), row.names = FALSE)
cat("\n")

# Pre-MSPE and post-period gap
pre_gaps  <- gaps %>% filter(time_unit < 2022)
post_gaps <- gaps %>% filter(time_unit >= 2022)
pre_mspe  <- mean(pre_gaps$gap^2)
avg_post_gap <- mean(post_gaps$gap)

cat("Pre-period MSPE:    ", round(pre_mspe, 4), "\n")
cat("Avg post-period gap:", round(avg_post_gap, 2), "per 100K\n\n")

# Fisher's exact p-value
sig_table <- scm_out %>% grab_significance()
p_fisher  <- sig_table %>%
  filter(unit_name == "New York, NY") %>%
  pull(fishers_exact_pvalue)

cat("Fisher's exact p-value:", round(p_fisher, 4), "\n\n")


# 6. Save results ==============================================================

cat("--- Saving Output ---\n\n")

weights_out <- weights %>% mutate(analysis = "av_scm_national")
write_csv(weights_out, file.path(results_dir, "av_scm_weights.csv"))
cat("  Saved: av_scm_weights.csv\n")

gaps_out <- gaps %>% mutate(analysis = "av_scm_national")
write_csv(gaps_out, file.path(results_dir, "av_scm_gaps.csv"))
cat("  Saved: av_scm_gaps.csv\n")

summary_out <- tibble(
  analysis       = "av_scm_national",
  treated_unit   = "New York, NY",
  n_donors       = n_donors,
  pre_years      = "2018–2021",
  i_time         = 2022,
  pre_mspe       = pre_mspe,
  avg_post_gap   = avg_post_gap,
  max_weight     = max_weight,
  top_donor      = top_donor,
  fisher_p       = p_fisher,
  degenerate     = max_weight > 0.80
)
write_csv(summary_out, file.path(results_dir, "av_scm_summary.csv"))
cat("  Saved: av_scm_summary.csv\n\n")


# 7. Plots =====================================================================

cat("--- Generating Plots ---\n\n")

INTERVENTION_YEAR <- 2022

# 7a. Fit plot: actual vs. synthetic ------------------------------------------

fit_data <- gaps %>%
  pivot_longer(cols = c(real_y, synth_y),
               names_to = "series", values_to = "rate") %>%
  mutate(series = recode(series,
                         "real_y"  = "New York City (actual)",
                         "synth_y" = "Synthetic NYC"))

p_fit <- ggplot(fit_data, aes(x = time_unit, y = rate,
                               color = series, linetype = series)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.5) +
  geom_vline(xintercept = INTERVENTION_YEAR - 0.5, linetype = "dashed",
             color = "grey40", linewidth = 0.7) +
  annotate("text", x = INTERVENTION_YEAR + 0.1, y = Inf,
           label = "Oct 2022\nPolicy change", hjust = 0, vjust = 1.4,
           size = 3, color = "grey40") +
  annotate("text", x = INTERVENTION_YEAR - 0.5, y = -Inf,
           label = "2022 = partial\ntreatment year",
           hjust = 0.5, vjust = -0.3, size = 2.5, color = "grey50") +
  scale_color_manual(values = c(
    "New York City (actual)" = COL_NYC,
    "Synthetic NYC"          = COL_CONTROL
  )) +
  scale_linetype_manual(values = c(
    "New York City (actual)" = "solid",
    "Synthetic NYC"          = "dashed"
  )) +
  scale_x_continuous(breaks = sort(unique(gaps$time_unit))) +
  labs(
    title    = "Actual vs. Synthetic NYC: Shooting Rate",
    subtitle = paste0("Fatal + nonfatal per 100,000; SCM with ",
                      n_donors, " comparison cities (AmericanViolence.org)"),
    x        = "Year",
    y        = "Shooting rate (per 100K)",
    color    = NULL, linetype = NULL,
    caption  = paste0("Top donor: ", top_donor,
                      " (", round(max_weight * 100, 1), "% weight). ",
                      "Pre-period MSPE = ", round(pre_mspe, 2), ".")
  ) +
  theme_pursuit()

save_plot(p_fit, "av_scm_fit")


# 7b. Gap plot ----------------------------------------------------------------

p_gap <- ggplot(gaps, aes(x = time_unit, y = gap)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = INTERVENTION_YEAR - 0.5, linetype = "dashed",
             color = "grey40", linewidth = 0.7) +
  geom_line(color = COL_NYC, linewidth = 1.0) +
  geom_point(color = COL_NYC, size = 3) +
  geom_text(aes(label = round(gap, 1)), vjust = -0.7, size = 3, color = COL_NYC) +
  scale_x_continuous(breaks = sort(unique(gaps$time_unit))) +
  labs(
    title    = "Gap: NYC Actual − Synthetic NYC",
    subtitle = paste0("Positive gap = NYC above synthetic; AmericanViolence.org data; ",
                      "Fisher's p = ", round(p_fisher, 3)),
    x        = "Year",
    y        = "Gap (per 100K)",
    caption  = paste0("Pre-period MSPE = ", round(pre_mspe, 2),
                      ". 2022 = partial treatment year (Oct–Dec 2022 only).")
  ) +
  theme_pursuit()

save_plot(p_gap, "av_scm_gap")


# 7c. Placebo plot (via tidysynth built-in, then save) ------------------------

# tidysynth's plot_placebos returns a ggplot; we can customize it
p_placebo_raw <- tryCatch(
  scm_out %>% plot_placebos(prune = TRUE),
  error = function(e) {
    cat("  Note: plot_placebos() failed:", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(p_placebo_raw)) {
  # Restyle to match project theme
  p_placebo <- p_placebo_raw +
    geom_vline(xintercept = INTERVENTION_YEAR - 0.5,
               linetype = "dashed", color = "grey40", linewidth = 0.7) +
    labs(
      title    = "Placebo Test: NYC vs. Donor Cities",
      subtitle = paste0("NYC gap (red) vs. ", n_donors,
                        " donor-city placebos (grey); Fisher's p = ",
                        round(p_fisher, 3)),
      caption  = "Pruned: donors with pre-MSPE > 2× NYC pre-MSPE excluded from plot."
    ) +
    theme_pursuit()
  save_plot(p_placebo, "av_scm_placebo")
} else {
  cat("  Placebo plot skipped.\n")
}


# 8. Summary ==================================================================

cat("\n=== RESULTS SUMMARY ===\n\n")
cat("Treated unit:       New York, NY\n")
cat("Donor cities:      ", n_donors, "\n")
cat("Pre-period:         2018–2021 (4 years)\n")
cat("Post-period:        2022–2025 (2022 = partial treatment year)\n\n")

cat("Pre-period fit:\n")
cat("  MSPE:            ", round(pre_mspe, 4), "\n\n")

cat("Post-period gaps (actual − synthetic):\n")
post_gaps %>%
  select(year = time_unit, actual = real_y, synthetic = synth_y, gap) %>%
  mutate(across(c(actual, synthetic, gap), \(x) round(x, 2))) %>%
  { print(as.data.frame(.)); . }

cat("\nTop donors (weight > 1%):\n")
weights %>%
  filter(weight >= 0.01) %>%
  mutate(weight_pct = paste0(round(weight * 100, 1), "%")) %>%
  select(city = unit, weight_pct) %>%
  { print(as.data.frame(.)); . }

cat("\nFisher's exact p-value:", round(p_fisher, 4), "\n")
cat("Interpretation:",
    if (p_fisher < 0.05) "Significant (p < .05) — NYC gap unlikely by chance"
    else if (p_fisher < 0.10) "Marginal (p < .10)"
    else "Not significant — gap not distinguishable from donor placebos",
    "\n\n")

if (max_weight > 0.80) {
  cat("WARNING: Degenerate result — interpret with caution.\n\n")
}

cat("Output files:\n")
cat("  output/national_scm/av_scm_weights.csv\n")
cat("  output/national_scm/av_scm_gaps.csv\n")
cat("  output/national_scm/av_scm_summary.csv\n")
cat("  output/plots/av_scm_fit.png/pdf\n")
cat("  output/plots/av_scm_gap.png/pdf\n")
cat("  output/plots/av_scm_placebo.png/pdf\n\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
