# 04_did_analysis.R — Difference-in-Differences Analysis
#
# Treatment: NYC (5 boroughs)
# Control: Non-NYC NYS counties (DCJS annual data)
# Intervention: 2023 (first full post-treatment year)
# Outcomes: Robbery, violent crime, MVT, property crime
# Method: TWFE with county + year FEs, clustered SEs
#
# FINDING: Parallel trends assumption is NOT met. Event-study coefficients
# show that every pre-treatment NYC × year interaction is large and highly
# significant (p < .001). NYC and non-NYC NYS were on fundamentally different
# crime trajectories before the intervention. DiD estimates are therefore
# descriptive (showing NYC vs. rest-of-NYS divergence) but should NOT be
# interpreted as causal effects of the pursuit policy change.
#
# Note: GIVE-based DiD for shootings is not viable — non-NYC GIVE data
# is near-zero after 2018. This limitation is documented but not estimated.
#
# NOTE (2026-03-05): This script is no longer part of the main publication
# pipeline. The within-NYS DiD analysis has been removed from the manuscript
# (parallel trends badly violated). This script is retained for reproducibility.
# Active pipeline: 01 → 03 → 06 → 07 → 08 → 14 → 17 → 15 → 16 → 00_regenerate

library(tidyverse)
library(lubridate)
library(janitor)
library(sandwich)
library(lmtest)
library(broom)
library(fixest)
library(here)


# Dirs and theme ---------------------------------------------------------------

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

COL_PURSUIT   <- "#D62828"
COL_ROBBERY   <- "#003049"

save_plot <- function(p, name, w = 10, h = 6) {
  ggsave(file.path(plot_dir, paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(plot_dir, paste0(name, ".pdf")),
         plot = p, width = w, height = h, bg = "white")
}


# 1. Load and prepare DCJS county data =========================================

cat("\nDIFFERENCE-IN-DIFFERENCES ANALYSIS\n\n")

raw <- read_csv(
  here("data", "Index_Crimes_by_County_and_Agency__Beginning_1990_20260214.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

# Keep county totals only, 2015-2024
county <- raw %>%
  filter(agency == "County Total",
         year >= 2015, year <= 2024) %>%
  select(county, year, region,
         robbery, violent_total, motor_vehicle_theft, property_total,
         murder, index_total)

cat("County panel:", n_distinct(county$county), "counties x",
    n_distinct(county$year), "years =", nrow(county), "rows\n")

# Load population data
pop <- read_csv(here("data", "nys_county_population.csv"), show_col_types = FALSE)

# Merge population (using 2024 estimates as constant — annual variation is small)
county <- county %>%
  left_join(pop, by = "county") %>%
  mutate(
    # Per-capita rates (per 100,000)
    robbery_rate = robbery / pop_2024 * 100000,
    violent_rate = violent_total / pop_2024 * 100000,
    mvt_rate     = motor_vehicle_theft / pop_2024 * 100000,
    property_rate = property_total / pop_2024 * 100000,

    # Log rates (add 0.5 to handle zeros in tiny counties)
    log_robbery  = log(robbery + 0.5),
    log_violent  = log(violent_total + 0.5),
    log_mvt      = log(motor_vehicle_theft + 0.5),
    log_property = log(property_total + 0.5),

    # Treatment indicators
    nyc  = as.integer(region == "New York City"),
    post = as.integer(year >= 2023),
    treat = nyc * post
  )

# Check for missing population
missing_pop <- county %>% filter(is.na(pop_2024)) %>% distinct(county)
if (nrow(missing_pop) > 0) {
  cat("WARNING: Missing population for:", missing_pop$county, "\n")
}

# NYC vs rest summary
cat("\nNYC counties:", county %>% filter(nyc == 1) %>% distinct(county) %>% pull(county) %>% paste(collapse = ", "), "\n")
cat("Control counties:", sum(county$nyc == 0) / n_distinct(county$year), "\n\n")


# 2. Aggregate NYC boroughs ====================================================

# For DiD, sum the 5 NYC boroughs into one "NYC" unit
nyc_agg <- county %>%
  filter(nyc == 1) %>%
  group_by(year) %>%
  summarise(
    county = "NYC",
    region = "New York City",
    robbery = sum(robbery),
    violent_total = sum(violent_total),
    motor_vehicle_theft = sum(motor_vehicle_theft),
    property_total = sum(property_total),
    murder = sum(murder),
    index_total = sum(index_total),
    pop_2024 = sum(pop_2024),
    .groups = "drop"
  ) %>%
  mutate(
    robbery_rate  = robbery / pop_2024 * 100000,
    violent_rate  = violent_total / pop_2024 * 100000,
    mvt_rate      = motor_vehicle_theft / pop_2024 * 100000,
    property_rate = property_total / pop_2024 * 100000,
    log_robbery   = log(robbery + 0.5),
    log_violent   = log(violent_total + 0.5),
    log_mvt       = log(motor_vehicle_theft + 0.5),
    log_property  = log(property_total + 0.5),
    nyc = 1L,
    post = as.integer(year >= 2023),
    treat = post
  )

# Non-NYC counties (keep individual)
non_nyc <- county %>%
  filter(nyc == 0)

# Combined panel for DiD
did_panel <- bind_rows(nyc_agg, non_nyc)
did_panel$county_id <- as.integer(factor(did_panel$county))

cat("DiD panel:", nrow(did_panel), "rows (",
    n_distinct(did_panel$county), "units x",
    n_distinct(did_panel$year), "years)\n\n")


# 3. TWFE DiD estimation (fixest) ==============================================

cat("--- TWFE DiD Results ---\n\n")

did_outcomes <- list(
  list(var = "robbery_rate",  log_var = "log_robbery",  label = "Robbery"),
  list(var = "violent_rate",  log_var = "log_violent",  label = "Violent Crime"),
  list(var = "mvt_rate",      log_var = "log_mvt",      label = "Motor Vehicle Theft"),
  list(var = "property_rate", log_var = "log_property", label = "Property Crime")
)

did_results <- map_dfr(did_outcomes, function(o) {

  # Rate specification
  f_rate <- as.formula(paste0(o$var, " ~ treat | county_id + year"))
  m_rate <- feols(f_rate, data = did_panel, cluster = ~county_id)

  # Log specification
  f_log <- as.formula(paste0(o$log_var, " ~ treat | county_id + year"))
  m_log <- feols(f_log, data = did_panel, cluster = ~county_id)

  cat(o$label, "(rate):\n")
  print(summary(m_rate))
  cat("\n")

  bind_rows(
    tidy(m_rate, conf.int = TRUE) %>%
      mutate(outcome = o$label, spec = "per_capita_rate"),
    tidy(m_log, conf.int = TRUE) %>%
      mutate(outcome = o$label, spec = "log_count")
  )
})

cat("\n--- DiD Summary ---\n")
print(did_results %>% select(outcome, spec, estimate, std.error, p.value), n = Inf)

write_csv(did_results, file.path(results_dir, "did_results.csv"))


# 4. Event-study (parallel trends) ============================================

cat("\n--- Event-Study / Parallel Trends ---\n\n")

# Create year interactions (omit 2022 as reference)
did_panel <- did_panel %>%
  mutate(year_fct = factor(year))

es_results <- map_dfr(did_outcomes, function(o) {

  f <- as.formula(paste0(o$var, " ~ i(year_fct, nyc, ref = '2022') | county_id + year"))
  m <- feols(f, data = did_panel, cluster = ~county_id)

  cat(o$label, "event study:\n")
  print(summary(m))
  cat("\n")

  tidy(m, conf.int = TRUE) %>%
    mutate(outcome = o$label) %>%
    # Extract year from the coefficient name
    mutate(
      event_year = as.integer(str_extract(term, "\\d{4}"))
    )
})

write_csv(es_results, file.path(results_dir, "did_parallel_trends.csv"))


# 5. Event-study plot ==========================================================

es_plot_data <- es_results %>%
  filter(!is.na(event_year))

p_es <- ggplot(es_plot_data, aes(x = event_year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 2022.5, linetype = "dotted", color = "grey40") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), color = COL_ROBBERY, size = 0.4) +
  facet_wrap(~outcome, scales = "free_y") +
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title    = "Event Study: NYC vs. Rest of NYS",
    subtitle = "TWFE with county + year FEs | Clustered SEs | Reference year: 2022",
    x = "Year", y = "Coefficient (NYC interaction)",
    caption  = "Outcomes in per-capita rates (per 100,000). Vertical line = intervention."
  ) +
  theme_pursuit() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)))

save_plot(p_es, "did_event_study", w = 12, h = 8)


# 6. Parallel trends visual (raw trends) ======================================

trends_data <- did_panel %>%
  group_by(year, nyc) %>%
  summarise(
    mean_robbery_rate = weighted.mean(robbery_rate, pop_2024),
    mean_violent_rate = weighted.mean(violent_rate, pop_2024),
    mean_mvt_rate     = weighted.mean(mvt_rate, pop_2024),
    .groups = "drop"
  ) %>%
  mutate(group = ifelse(nyc == 1, "NYC", "Rest of NYS"))

p_trends <- trends_data %>%
  pivot_longer(cols = starts_with("mean_"), names_to = "outcome", values_to = "rate") %>%
  mutate(outcome = recode(outcome,
    "mean_robbery_rate" = "Robbery",
    "mean_violent_rate" = "Violent Crime",
    "mean_mvt_rate" = "Motor Vehicle Theft"
  )) %>%
  ggplot(aes(year, rate, color = group, linetype = group)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_vline(xintercept = 2022.5, linetype = "dotted", color = "grey40") +
  facet_wrap(~outcome, scales = "free_y") +
  scale_color_manual(values = c("NYC" = COL_PURSUIT, "Rest of NYS" = "grey40")) +
  scale_linetype_manual(values = c("NYC" = "solid", "Rest of NYS" = "dashed")) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title    = "Crime Rate Trends: NYC vs. Rest of NYS",
    subtitle = "Population-weighted means | Per 100,000 residents",
    x = "Year", y = "Rate per 100,000",
    caption  = "DCJS Index Crimes, County Totals. Vertical line = intervention (2023)."
  ) +
  theme_pursuit() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.8)))

save_plot(p_trends, "did_parallel_trends", w = 12, h = 6)


# 7. GIVE shooting limitation documentation ===================================

cat("\n--- GIVE Data Limitation ---\n")
cat("Non-NYC GIVE shooting data is near-zero after mid-2018.\n")
cat("DiD on shootings via GIVE is not viable.\n")
cat("The ITS on NYPD-specific shooting data is the preferred approach.\n\n")


# 8. Parallel trends assessment ================================================

cat("--- Parallel Trends Assessment ---\n\n")

# Check pre-treatment event-study coefficients
pre_treat <- es_results %>%
  filter(!is.na(event_year), event_year < 2023)

n_sig <- sum(pre_treat$p.value < 0.05)
n_total <- nrow(pre_treat)

cat("Pre-treatment NYC × year interactions:\n")
cat("  Significant (p < .05):", n_sig, "of", n_total, "\n")
cat("  All pre-treatment coefficients are large and highly significant.\n")
cat("  NYC and non-NYC NYS were on fundamentally different crime trajectories\n")
cat("  before the intervention.\n\n")
cat("CONCLUSION: Parallel trends assumption is NOT met.\n")
cat("DiD estimates should be interpreted as descriptive comparisons, not\n")
cat("causal effects of the pursuit policy change.\n")
cat("The ITS and event study designs are the preferred identification strategies.\n\n")


# Done =========================================================================

cat("DiD results saved to:", results_dir, "\n")
cat("Plots saved to:", plot_dir, "\n\n")
