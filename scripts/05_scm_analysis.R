# 05_scm_analysis.R — Synthetic Control Method
#
# Treatment: NYC (5 boroughs aggregated)
# Donor pool: Non-NYC NYS counties (pop > 50,000)
# Outcomes: Robbery rate, violent crime rate (per 100,000)
# Pre-period: 2002-2021 (20 years for robust fit)
# Post-period: 2022-2024
#
# FINDING: SCM is not viable for this application. Even with per-capita rates,
# Erie County receives ~100% of the donor weight — the "synthetic NYC" is just
# Erie County. NYC's crime dynamics are too distinct from any weighted combination
# of non-NYC NYS counties. Fisher's exact p-values are not significant (robbery
# p = 0.15, violent crime p = 0.17). These results are reported as a documented
# limitation of using within-NYS comparisons, not as causal evidence.
#
# Key fix from prior analysis: uses per-capita rates to solve the scale
# mismatch (NYC robbery ~200/100K vs Erie ~70/100K, instead of raw counts
# where NYC ~17K vs Erie ~1.3K).
#
# NOTE (2026-03-05): This script is no longer part of the main publication
# pipeline. The within-NYS SCM has been removed from the manuscript
# (degenerate — Erie County receives ~100% donor weight). Retained for reproducibility.
# Active pipeline: 01 → 03 → 06 → 07 → 08 → 14 → 17 → 15 → 16 → 00_regenerate

library(tidyverse)
library(janitor)
library(tidysynth)
library(here)


# Dirs -------------------------------------------------------------------------

dir.create(here("output"),                 showWarnings = FALSE)
dir.create(here("output/plots"),           showWarnings = FALSE)
dir.create(here("output/did_scm_results"), showWarnings = FALSE)

plot_dir    <- here("output/plots")
results_dir <- here("output/did_scm_results")

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

COL_PURSUIT <- "#D62828"
COL_ROBBERY <- "#003049"
COL_SHOOTING <- "#6A0572"

save_plot <- function(p, name, w = 10, h = 6) {
  ggsave(file.path(plot_dir, paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(plot_dir, paste0(name, ".pdf")),
         plot = p, width = w, height = h, bg = "white")
}


# 1. Load and prepare data =====================================================

cat("\nSYNTHETIC CONTROL METHOD ANALYSIS\n\n")

raw <- read_csv(
  here("data", "Index_Crimes_by_County_and_Agency__Beginning_1990_20260214.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

pop <- read_csv(here("data", "nys_county_population.csv"), show_col_types = FALSE)

# Population threshold for donor pool
POP_THRESHOLD <- 50000

# County totals, 2002-2024
county <- raw %>%
  filter(agency == "County Total",
         year >= 2002, year <= 2024) %>%
  select(county, year, region, robbery, violent_total, motor_vehicle_theft,
         property_total, murder) %>%
  left_join(pop, by = "county")

# Flag eligible donors
county <- county %>%
  mutate(
    nyc = as.integer(region == "New York City"),
    eligible_donor = (!nyc & pop_2024 >= POP_THRESHOLD)
  )

n_donors <- county %>% filter(eligible_donor) %>% distinct(county) %>% nrow()
cat("Eligible donors (pop >=", POP_THRESHOLD, "):", n_donors, "counties\n")
cat("Excluded (too small):",
    county %>% filter(!nyc & !eligible_donor) %>% distinct(county) %>% nrow(),
    "counties\n\n")

# Aggregate NYC boroughs
nyc_agg <- county %>%
  filter(nyc == 1) %>%
  group_by(year) %>%
  summarise(
    county   = "NYC",
    region   = "New York City",
    robbery  = sum(robbery),
    violent_total = sum(violent_total),
    motor_vehicle_theft = sum(motor_vehicle_theft),
    property_total = sum(property_total),
    murder   = sum(murder),
    pop_2024 = sum(pop_2024),
    nyc = 1L,
    eligible_donor = FALSE,
    .groups = "drop"
  )

# Donor counties
donors <- county %>%
  filter(eligible_donor)

# Combined panel
scm_panel <- bind_rows(nyc_agg, donors) %>%
  mutate(
    robbery_rate  = robbery / pop_2024 * 100000,
    violent_rate  = violent_total / pop_2024 * 100000,
    mvt_rate      = motor_vehicle_theft / pop_2024 * 100000
  )

cat("SCM panel:", n_distinct(scm_panel$county), "units x",
    n_distinct(scm_panel$year), "years\n\n")


# 2. Robbery rate SCM ==========================================================

cat("--- SCM: Robbery Rate ---\n\n")

set.seed(42)

scm_robbery <- scm_panel %>%
  synthetic_control(
    outcome    = robbery_rate,
    unit       = county,
    time       = year,
    i_unit     = "NYC",
    i_time     = 2022,           # first treatment year
    generate_placebos = TRUE
  ) %>%
  # Use lagged outcomes at two well-separated time points
  generate_predictor(
    time_window = 2010,
    robbery_2010 = robbery_rate
  ) %>%
  generate_predictor(
    time_window = 2019,
    robbery_2019 = robbery_rate
  ) %>%
  generate_weights(optimization_window = 2002:2021) %>%
  generate_control()

# Extract results
rob_weights <- scm_robbery %>%
  grab_unit_weights() %>%
  filter(weight > 0.001) %>%
  arrange(desc(weight))

cat("Donor weights (robbery rate):\n")
print(rob_weights, n = 20)

rob_balance <- scm_robbery %>% grab_balance_table()
cat("\nPredictor balance:\n")
print(rob_balance)

# Gap and fit
rob_results <- scm_robbery %>%
  grab_synthetic_control() %>%
  mutate(gap = real_y - synth_y)

pre_mspe <- rob_results %>%
  filter(time_unit < 2022) %>%
  summarise(mspe = mean(gap^2)) %>%
  pull(mspe)

cat("\nPre-treatment MSPE:", round(pre_mspe, 2), "\n")

write_csv(rob_weights, file.path(results_dir, "scm_robbery_weights.csv"))
write_csv(rob_results, file.path(results_dir, "scm_robbery_results.csv"))
write_csv(rob_balance, file.path(results_dir, "scm_robbery_balance.csv"))


# 3. Robbery SCM plots ========================================================

# Gap plot
p_rob_gap <- ggplot(rob_results, aes(time_unit)) +
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +
  geom_vline(xintercept = 2021.5, linetype = "dotted", color = "grey40") +
  geom_line(aes(y = gap), color = COL_ROBBERY, linewidth = 1) +
  geom_point(aes(y = gap), color = COL_ROBBERY, size = 2) +
  scale_x_continuous(breaks = seq(2002, 2024, 2)) +
  labs(
    title    = "SCM Gap Plot: Robbery Rate (NYC - Synthetic NYC)",
    subtitle = paste0("Pre-treatment MSPE = ", round(pre_mspe, 1)),
    x = "Year", y = "Gap (Actual - Synthetic) per 100,000",
    caption  = "Vertical line = intervention (2022). Positive gap = NYC above synthetic."
  ) +
  theme_pursuit()

save_plot(p_rob_gap, "scm_robbery_gap")

# Actual vs synthetic
p_rob_fit <- rob_results %>%
  pivot_longer(cols = c(real_y, synth_y), names_to = "series", values_to = "rate") %>%
  mutate(series = recode(series, "real_y" = "NYC (Actual)", "synth_y" = "Synthetic NYC")) %>%
  ggplot(aes(time_unit, rate, color = series, linetype = series)) +
  geom_vline(xintercept = 2021.5, linetype = "dotted", color = "grey40") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("NYC (Actual)" = COL_ROBBERY, "Synthetic NYC" = "grey40")) +
  scale_linetype_manual(values = c("NYC (Actual)" = "solid", "Synthetic NYC" = "dashed")) +
  scale_x_continuous(breaks = seq(2002, 2024, 2)) +
  labs(
    title = "SCM: Robbery Rate — NYC vs. Synthetic NYC",
    x = "Year", y = "Robbery Rate per 100,000",
    caption = "Synthetic control constructed from non-NYC NYS counties (pop > 50K)"
  ) +
  theme_pursuit()

save_plot(p_rob_fit, "scm_robbery_fit")


# 4. Placebo inference =========================================================

cat("\n--- Placebo Tests ---\n")

# MSPE ratios
rob_mspe <- scm_robbery %>%
  grab_significance() %>%
  arrange(desc(fishers_exact_pvalue))

cat("\nFisher's exact p-value (robbery):",
    rob_mspe %>% filter(unit_name == "NYC") %>% pull(fishers_exact_pvalue), "\n")

write_csv(rob_mspe, file.path(results_dir, "scm_robbery_placebo_mspe.csv"))

# Placebo plot
p_rob_placebo <- scm_robbery %>%
  plot_placebos(prune = TRUE) +
  labs(
    title = "SCM Placebo Test: Robbery Rate",
    subtitle = "NYC highlighted in red; grey lines = donor county placebos",
    x = "Year", y = "Gap (Actual - Synthetic)"
  ) +
  theme_pursuit()

save_plot(p_rob_placebo, "scm_robbery_placebos")


# 5. Violent crime rate SCM ====================================================

cat("\n--- SCM: Violent Crime Rate ---\n\n")

scm_violent <- scm_panel %>%
  synthetic_control(
    outcome    = violent_rate,
    unit       = county,
    time       = year,
    i_unit     = "NYC",
    i_time     = 2022,
    generate_placebos = TRUE
  ) %>%
  generate_predictor(
    time_window = 2002:2007,
    violent_early = mean(violent_rate, na.rm = TRUE)
  ) %>%
  generate_predictor(
    time_window = 2008:2013,
    violent_mid = mean(violent_rate, na.rm = TRUE)
  ) %>%
  generate_predictor(
    time_window = 2014:2018,
    violent_late = mean(violent_rate, na.rm = TRUE)
  ) %>%
  generate_predictor(
    time_window = 2019:2021,
    violent_recent = mean(violent_rate, na.rm = TRUE)
  ) %>%
  generate_weights(optimization_window = 2002:2021) %>%
  generate_control()

viol_weights <- scm_violent %>%
  grab_unit_weights() %>%
  filter(weight > 0.001) %>%
  arrange(desc(weight))

cat("Donor weights (violent rate):\n")
print(viol_weights, n = 20)

viol_results <- scm_violent %>%
  grab_synthetic_control() %>%
  mutate(gap = real_y - synth_y)

viol_pre_mspe <- viol_results %>%
  filter(time_unit < 2022) %>%
  summarise(mspe = mean(gap^2)) %>%
  pull(mspe)

cat("\nPre-treatment MSPE:", round(viol_pre_mspe, 2), "\n")

write_csv(viol_weights, file.path(results_dir, "scm_violent_weights.csv"))
write_csv(viol_results, file.path(results_dir, "scm_violent_results.csv"))

# Violent crime plots
p_viol_fit <- viol_results %>%
  pivot_longer(cols = c(real_y, synth_y), names_to = "series", values_to = "rate") %>%
  mutate(series = recode(series, "real_y" = "NYC (Actual)", "synth_y" = "Synthetic NYC")) %>%
  ggplot(aes(time_unit, rate, color = series, linetype = series)) +
  geom_vline(xintercept = 2021.5, linetype = "dotted", color = "grey40") +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("NYC (Actual)" = COL_SHOOTING, "Synthetic NYC" = "grey40")) +
  scale_linetype_manual(values = c("NYC (Actual)" = "solid", "Synthetic NYC" = "dashed")) +
  scale_x_continuous(breaks = seq(2002, 2024, 2)) +
  labs(
    title = "SCM: Violent Crime Rate — NYC vs. Synthetic NYC",
    x = "Year", y = "Violent Crime Rate per 100,000",
    caption = "Synthetic control from non-NYC NYS counties (pop > 50K)"
  ) +
  theme_pursuit()

save_plot(p_viol_fit, "scm_violent_fit")

viol_mspe <- scm_violent %>%
  grab_significance() %>%
  arrange(desc(fishers_exact_pvalue))

cat("\nFisher's exact p-value (violent):",
    viol_mspe %>% filter(unit_name == "NYC") %>% pull(fishers_exact_pvalue), "\n")

write_csv(viol_mspe, file.path(results_dir, "scm_violent_placebo_mspe.csv"))

p_viol_placebo <- scm_violent %>%
  plot_placebos(prune = TRUE) +
  labs(
    title = "SCM Placebo Test: Violent Crime Rate",
    subtitle = "NYC highlighted; grey = donor county placebos",
    x = "Year", y = "Gap (Actual - Synthetic)"
  ) +
  theme_pursuit()

save_plot(p_viol_placebo, "scm_violent_placebos")


# 6. Leave-one-out robustness (robbery) ========================================

cat("\n--- Leave-One-Out Robustness (Robbery) ---\n\n")

top_donors <- rob_weights %>%
  filter(weight >= 0.05) %>%
  pull(unit)

if (length(top_donors) > 0 && length(top_donors) <= 10) {

  loo_results <- map_dfr(top_donors, function(drop_county) {
    loo_panel <- scm_panel %>% filter(county != drop_county)

    tryCatch({
      loo_scm <- loo_panel %>%
        synthetic_control(
          outcome = robbery_rate, unit = county, time = year,
          i_unit = "NYC", i_time = 2022, generate_placebos = FALSE
        ) %>%
        generate_predictor(time_window = 2002:2007,
                           robbery_early = mean(robbery_rate, na.rm = TRUE)) %>%
        generate_predictor(time_window = 2008:2013,
                           robbery_mid = mean(robbery_rate, na.rm = TRUE)) %>%
        generate_predictor(time_window = 2014:2018,
                           robbery_late = mean(robbery_rate, na.rm = TRUE)) %>%
        generate_predictor(time_window = 2019:2021,
                           robbery_recent = mean(robbery_rate, na.rm = TRUE)) %>%
        generate_weights(optimization_window = 2002:2021) %>%
        generate_control()

      loo_scm %>%
        grab_synthetic_control() %>%
        mutate(dropped = drop_county,
               gap = real_y - synth_y)

    }, error = function(e) {
      cat("  LOO failed for", drop_county, ":", e$message, "\n")
      tibble()
    })
  })

  if (nrow(loo_results) > 0) {
    write_csv(loo_results, file.path(results_dir, "scm_robbery_loo.csv"))

    p_loo <- ggplot() +
      geom_line(data = loo_results,
                aes(time_unit, synth_y, group = dropped, color = dropped),
                linewidth = 0.6, alpha = 0.7) +
      geom_line(data = rob_results,
                aes(time_unit, real_y), color = "black", linewidth = 1.2) +
      geom_line(data = rob_results,
                aes(time_unit, synth_y), color = COL_ROBBERY,
                linewidth = 1, linetype = "dashed") +
      geom_vline(xintercept = 2021.5, linetype = "dotted", color = "grey40") +
      scale_x_continuous(breaks = seq(2002, 2024, 2)) +
      labs(
        title = "Leave-One-Out Robustness: Robbery Rate SCM",
        subtitle = "Black = NYC actual | Dashed = main synthetic | Colors = LOO synthetics",
        x = "Year", y = "Robbery Rate per 100,000"
      ) +
      theme_pursuit()

    save_plot(p_loo, "scm_robbery_loo", w = 11, h = 7)
  }
} else {
  cat("Skipping LOO: top donors =", length(top_donors), "(need 1-10)\n")
}


# 7. Limitation summary ========================================================

cat("\n--- SCM Limitation Summary ---\n\n")
cat("NYC is too unique within NYS for valid synthetic control construction.\n")
cat("Even with per-capita rates, Erie County receives ~100% donor weight.\n")
cat("Fisher's exact p-values: robbery = 0.15, violent crime = 0.17 (not significant).\n")
cat("Conclusion: SCM results are reported as a documented limitation,\n")
cat("not as causal evidence for or against the pursuit policy effect.\n")
cat("The ITS and event study designs are the preferred identification strategies.\n\n")


# Done =========================================================================

cat("SCM results saved to:", results_dir, "\n")
cat("Plots saved to:", plot_dir, "\n\n")
