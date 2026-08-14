# 09_national_did.R — DiD with Borough-Level Treatment + National Donors
#
# Treatment: 5 NYC boroughs (post = Oct 2022 / 2023 for annual data)
# Control: Top US cities (Kaplan UCR for robbery, agency data for shootings)
# Method: TWFE via fixest::feols() with unit + year FEs, clustered SEs
# Event study: parallel trends test via year × treatment interactions
#
# Requires output from 08_national_data_prep.R
#
# NOTE (2026-03-05): This script is no longer part of the main publication
# pipeline. The national DiD/SCM analyses have been removed from the
# manuscript. This script is retained for reproducibility.
# Active pipeline: 01 → 03 → 06 → 07 → 08 → 14 → 17 → 15 → 16 → 00_regenerate

suppressPackageStartupMessages({
  library(tidyverse)
  library(fixest)
  library(fwildclusterboot)
  library(here)
})

cat("\n=== NATIONAL DiD ANALYSIS ===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")

results_dir <- here("output", "national_did")
plot_dir    <- here("output", "plots")

# Project theme and save function (from 02_eda.R conventions)
COL_PURSUIT <- "#D62828"
COL_ROBBERY <- "#003049"

theme_pursuit <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey30"),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

save_plot <- function(p, name, w = 10, h = 7) {
  ggsave(file.path(plot_dir, paste0(name, ".png")), p, width = w, height = h, dpi = 300)
  ggsave(file.path(plot_dir, paste0(name, ".pdf")), p, width = w, height = h)
  cat("  Saved:", name, "\n")
}


# 1. Load panels ==============================================================

cat("--- Loading Panels ---\n\n")

# Annual robbery panel (boroughs + UCR cities)
robbery_file <- here("output", "national_did", "combined_annual_panel.csv")
if (!file.exists(robbery_file)) {
  stop("Run 08_national_data_prep.R first. Missing: ", robbery_file)
}
robbery_panel <- read_csv(robbery_file, show_col_types = FALSE) %>%
  mutate(
    treated = as.integer(treated),
    post    = as.integer(year >= 2023),  # first full post-treatment year
    did     = treated * post
  )

cat("Robbery panel:", nrow(robbery_panel), "rows\n")
cat("  Units:", n_distinct(robbery_panel$boro), "\n")
cat("  Treated:", n_distinct(robbery_panel$boro[robbery_panel$treated == 1]), "\n")
cat("  Control:", n_distinct(robbery_panel$boro[robbery_panel$treated == 0]), "\n")
cat("  Years:", min(robbery_panel$year), "to", max(robbery_panel$year), "\n\n")

# Monthly shooting panel (boroughs + comparison cities)
shooting_file <- here("output", "national_did", "combined_shooting_monthly.csv")
if (file.exists(shooting_file)) {
  shooting_panel <- read_csv(shooting_file, show_col_types = FALSE) %>%
    mutate(
      treated = as.integer(treated),
      post    = as.integer(month_date >= as.Date("2022-10-01")),
      did     = treated * post
    )
  cat("Shooting panel:", nrow(shooting_panel), "rows\n")
  cat("  Units:", n_distinct(shooting_panel$boro), "\n\n")
} else {
  shooting_panel <- NULL
  cat("Shooting panel not found — skipping shooting DiD.\n\n")
}


# 2. Robbery DiD — TWFE =======================================================

cat("--- Robbery DiD (TWFE) ---\n\n")

# Filter to balanced window where most cities have data
robbery_balanced <- robbery_panel %>%
  group_by(boro) %>%
  filter(n_distinct(year) >= 8) %>%
  ungroup()

cat("Balanced panel (>= 8 years):", n_distinct(robbery_balanced$boro), "units\n")

# Basic TWFE
rob_twfe <- feols(
  robbery_rate ~ did | boro + year,
  data = robbery_balanced,
  cluster = ~boro
)

cat("\nRobbery DiD (TWFE):\n")
summary(rob_twfe)

# Save coefficients
rob_twfe_tidy <- broom::tidy(rob_twfe, conf.int = TRUE) %>%
  mutate(outcome = "robbery_rate", model = "twfe")
write_csv(rob_twfe_tidy, file.path(results_dir, "did_robbery_twfe.csv"))


# 3. Robbery event study — parallel trends test ===============================

cat("\n--- Robbery Event Study (Parallel Trends) ---\n\n")

# Create year dummies interacted with treatment (reference: 2022)
robbery_es <- robbery_balanced %>%
  mutate(
    year_fct = factor(year),
    nyc = treated
  )

rob_es_mod <- feols(
  robbery_rate ~ i(year_fct, nyc, ref = 2022) | boro + year,
  data = robbery_es,
  cluster = ~boro
)

cat("Event study coefficients:\n")
summary(rob_es_mod)

# Extract event study coefficients
rob_es_coefs <- broom::tidy(rob_es_mod, conf.int = TRUE) %>%
  mutate(
    outcome = "robbery_rate",
    event_year = as.numeric(str_extract(term, "\\d{4}"))
  )

write_csv(rob_es_coefs, file.path(results_dir, "did_robbery_event_study.csv"))

# Parallel trends assessment
pre_treat <- rob_es_coefs %>% filter(event_year < 2022)
n_sig <- sum(pre_treat$p.value < 0.05)
n_total <- nrow(pre_treat)

cat("\nParallel trends test (robbery):\n")
cat("  Pre-treatment interactions:", n_total, "\n")
cat("  Significant at p < .05:", n_sig, "of", n_total, "\n")

if (n_sig > n_total / 2) {
  cat("  CONCLUSION: Parallel trends assumption NOT met.\n")
  cat("  DiD estimates are descriptive, not causal.\n\n")
} else if (n_sig > 0) {
  cat("  CAUTION: Some pre-treatment coefficients significant.\n")
  cat("  Parallel trends partially supported.\n\n")
} else {
  cat("  PASS: No significant pre-treatment differences.\n")
  cat("  Parallel trends assumption supported.\n\n")
}

# Event study plot
p_rob_es <- rob_es_coefs %>%
  filter(!is.na(event_year)) %>%
  ggplot(aes(x = event_year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = COL_PURSUIT, alpha = 0.5) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = COL_ROBBERY) +
  geom_line(color = COL_ROBBERY, linewidth = 0.8) +
  geom_point(color = COL_ROBBERY, size = 2.5) +
  labs(
    title    = "Robbery Rate: Event Study (NYC Boroughs vs. US Cities)",
    subtitle = "DiD coefficients relative to 2022 | 95% CIs | Clustered SEs",
    x = "Year", y = "Robbery Rate Difference (per 100K)",
    caption  = paste0("Treatment: 5 NYC boroughs | Control: ",
                      n_distinct(robbery_balanced$boro[robbery_balanced$treated == 0]),
                      " US cities (Kaplan UCR)\nVertical line = Oct 2022 pursuit policy change")
  ) +
  theme_pursuit()

save_plot(p_rob_es, "did_robbery_event_study", w = 10, h = 6)


# 4. Trend comparison plot =====================================================

cat("--- Trend Comparison ---\n\n")

# Average trends: treated vs. control
trend_data <- robbery_balanced %>%
  group_by(year, treated) %>%
  summarise(
    mean_rate = mean(robbery_rate, na.rm = TRUE),
    se_rate   = sd(robbery_rate, na.rm = TRUE) / sqrt(n()),
    n_units   = n(),
    .groups   = "drop"
  ) %>%
  mutate(group = ifelse(treated == 1, "NYC Boroughs", "Control Cities"))

p_trends <- trend_data %>%
  ggplot(aes(x = year, y = mean_rate, color = group, fill = group)) +
  geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
  geom_ribbon(aes(ymin = mean_rate - 1.96 * se_rate,
                  ymax = mean_rate + 1.96 * se_rate),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("NYC Boroughs" = COL_PURSUIT, "Control Cities" = COL_ROBBERY)) +
  scale_fill_manual(values = c("NYC Boroughs" = COL_PURSUIT, "Control Cities" = COL_ROBBERY)) +
  labs(
    title    = "Robbery Rates: NYC Boroughs vs. Control Cities",
    subtitle = "Mean robbery rate per 100K with 95% CIs",
    x = "Year", y = "Robbery Rate (per 100K)",
    color = NULL, fill = NULL,
    caption = "Vertical line = Oct 2022 pursuit policy change"
  ) +
  theme_pursuit()

save_plot(p_trends, "did_robbery_trends", w = 10, h = 6)


# 5. Shooting DiD =============================================================

if (!is.null(shooting_panel)) {

  cat("--- Shooting DiD ---\n\n")

  # Filter to common window where all cities have data
  # Find the latest start date across all cities
  shooting_coverage <- shooting_panel %>%
    group_by(boro) %>%
    summarise(
      first = min(month_date),
      last  = max(month_date),
      n     = n(),
      .groups = "drop"
    )
  cat("Shooting coverage:\n")
  print(shooting_coverage, n = Inf)

  # Use 2018-01-01 as start (NYC data starts there) and trim to Sep 2025
  common_start <- as.Date("2018-01-01")
  common_end   <- as.Date("2025-09-01")

  shooting_balanced <- shooting_panel %>%
    filter(month_date >= common_start, month_date <= common_end) %>%
    # Drop cities with insufficient pre-period
    group_by(boro) %>%
    filter(min(month_date) <= as.Date("2018-06-01")) %>%
    ungroup()

  cat("\nBalanced shooting panel:", n_distinct(shooting_balanced$boro), "units\n")
  cat("Dropped (late start):", setdiff(unique(shooting_panel$boro),
                                        unique(shooting_balanced$boro)), "\n")

  # Aggregate to annual for cleaner DiD
  shooting_annual_did <- shooting_balanced %>%
    group_by(boro, year = year(month_date), treated) %>%
    summarise(
      shooting_incidents = sum(shooting_incidents),
      pop_2020 = first(pop_2020),
      n_months = n(),
      .groups = "drop"
    ) %>%
    # Annualize to a per-12-month equivalent rate for all years.
    # For complete years (n_months == 12) the /n_months*12 term cancels.
    # For 2025 (partial year), it scales the observed count up to a
    # 12-month equivalent before computing the per-100K rate — making
    # 2025 comparable to full-year observations without dropping the row.
    mutate(
      shooting_rate = shooting_incidents / pop_2020 * 100000 / n_months * 12,
      post = as.integer(year >= 2023),
      did  = treated * post
    )

  # Save filtered annual panel for cross-language replication.
  # This file is the authoritative source for Stata and Python replications.
  # It differs from combined_shooting_annual.csv in two ways:
  #   (1) Contains only 12 units (Cincinnati dropped — data starts 2023+)
  #   (2) Uses annualized 2025 rates (n_months scaling) rather than raw rates
  write_csv(shooting_annual_did, file.path(results_dir, "shooting_annual_did.csv"))
  cat("  Saved: output/national_did/shooting_annual_did.csv",
      "(filtered, annualized; use this for replication, not combined_shooting_annual.csv)\n")

  # TWFE
  shoot_twfe <- feols(
    shooting_rate ~ did | boro + year,
    data = shooting_annual_did,
    cluster = ~boro
  )

  cat("\nShooting DiD (TWFE):\n")
  summary(shoot_twfe)

  shoot_twfe_tidy <- broom::tidy(shoot_twfe, conf.int = TRUE) %>%
    mutate(outcome = "shooting_rate", model = "twfe")
  write_csv(shoot_twfe_tidy, file.path(results_dir, "did_shooting_twfe.csv"))

  # Wild cluster bootstrap robustness (G = 12 clusters; asymptotic SE unreliable at G < 30)
  # Webb (2023) 6-point distribution recommended for small G.
  # boottest.fixest has preprocessing issues with character cluster vars —
  # use lm with explicit FE dummies (identical point estimate) for the bootstrap.
  cat("\nWild cluster bootstrap (Webb dist., B=9999, G=12 clusters):\n")
  shoot_lm <- lm(shooting_rate ~ did + factor(boro) + factor(year),
                 data = shooting_annual_did)
  set.seed(2022)
  if (requireNamespace("dqrng", quietly = TRUE)) dqrng::dqset.seed(2022)
  wcb <- boottest(
    shoot_lm,
    param      = "did",
    B          = 9999,
    clustid    = "boro",
    type       = "webb",
    conf_int   = TRUE,
    sign_level = 0.05
  )
  wcb_p   <- wcb$p_val
  wcb_ci  <- wcb$conf_int
  cat(sprintf("  feols estimate: did = %.3f (same as lm)\n", coef(shoot_twfe)["did"]))
  cat(sprintf("  p-value (WCB):  %.4f\n", wcb_p))
  cat(sprintf("  95%% CI (WCB):  [%.3f, %.3f]\n", wcb_ci[1], wcb_ci[2]))

  wcb_summary <- tibble(
    outcome    = "shooting_rate",
    model      = "twfe_wcb_webb",
    term       = "did",
    estimate   = coef(shoot_twfe)["did"],
    p_value    = wcb_p,
    conf_low   = wcb_ci[1],
    conf_high  = wcb_ci[2],
    n_clusters = 12L,
    B          = 9999L,
    dist_type  = "webb"
  )
  write_csv(wcb_summary, file.path(results_dir, "did_shooting_wcb.csv"))
  cat("  Saved: output/national_did/did_shooting_wcb.csv\n")

  # Event study
  shooting_es <- shooting_annual_did %>%
    mutate(year_fct = factor(year), nyc = treated)

  shoot_es_mod <- feols(
    shooting_rate ~ i(year_fct, nyc, ref = 2022) | boro + year,
    data = shooting_es,
    cluster = ~boro
  )

  shoot_es_coefs <- broom::tidy(shoot_es_mod, conf.int = TRUE) %>%
    mutate(
      outcome = "shooting_rate",
      event_year = as.numeric(str_extract(term, "\\d{4}"))
    )

  write_csv(shoot_es_coefs, file.path(results_dir, "did_shooting_event_study.csv"))

  # Parallel trends assessment
  pre_shoot <- shoot_es_coefs %>% filter(event_year < 2022)
  n_sig_shoot <- sum(pre_shoot$p.value < 0.05)

  cat("\nParallel trends test (shootings):\n")
  cat("  Significant at p < .05:", n_sig_shoot, "of", nrow(pre_shoot), "\n")

  if (n_sig_shoot > nrow(pre_shoot) / 2) {
    cat("  CONCLUSION: Parallel trends NOT met.\n\n")
  } else if (n_sig_shoot > 0) {
    cat("  CAUTION: Some pre-treatment differences.\n\n")
  } else {
    cat("  PASS: Parallel trends supported.\n\n")
  }

  # Event study plot
  p_shoot_es <- shoot_es_coefs %>%
    filter(!is.na(event_year)) %>%
    ggplot(aes(x = event_year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = COL_PURSUIT, alpha = 0.5) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = COL_ROBBERY) +
    geom_line(color = COL_ROBBERY, linewidth = 0.8) +
    geom_point(color = COL_ROBBERY, size = 2.5) +
    labs(
      title    = "Shooting Rate: Event Study (NYC Boroughs vs. Comparison Cities)",
      subtitle = "DiD coefficients relative to 2022 | 95% CIs | Clustered SEs",
      x = "Year", y = "Shooting Rate Difference (per 100K)",
      caption  = paste0("Treatment: 5 NYC boroughs | Control: ",
                        n_distinct(shooting_balanced$boro[shooting_balanced$treated == 0]),
                        " cities (agency-reported data)")
    ) +
    theme_pursuit()

  save_plot(p_shoot_es, "did_shooting_event_study", w = 10, h = 6)

  # Shooting trend comparison
  shoot_trend <- shooting_annual_did %>%
    group_by(year, treated) %>%
    summarise(
      mean_rate = mean(shooting_rate, na.rm = TRUE),
      se_rate   = sd(shooting_rate, na.rm = TRUE) / sqrt(n()),
      .groups   = "drop"
    ) %>%
    mutate(group = ifelse(treated == 1, "NYC Boroughs", "Control Cities"))

  p_shoot_trends <- shoot_trend %>%
    ggplot(aes(x = year, y = mean_rate, color = group, fill = group)) +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(ymin = mean_rate - 1.96 * se_rate,
                    ymax = mean_rate + 1.96 * se_rate),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(values = c("NYC Boroughs" = COL_PURSUIT, "Control Cities" = COL_ROBBERY)) +
    scale_fill_manual(values = c("NYC Boroughs" = COL_PURSUIT, "Control Cities" = COL_ROBBERY)) +
    labs(
      title    = "Shooting Rates: NYC Boroughs vs. Control Cities",
      subtitle = "Annualized shooting rate per 100K with 95% CIs",
      x = "Year", y = "Shooting Rate (per 100K)",
      color = NULL, fill = NULL,
      caption = "Vertical line = Oct 2022 pursuit policy change"
    ) +
    theme_pursuit()

  save_plot(p_shoot_trends, "did_shooting_trends", w = 10, h = 6)


  # 5b. Leave-one-out sensitivity — shooting DiD ===============================

  cat("--- Shooting DiD: Leave-One-Out Sensitivity ---\n\n")

  control_cities <- unique(shooting_annual_did$boro[shooting_annual_did$treated == 0])
  cat("Dropping each of", length(control_cities), "comparison cities in turn:\n\n")

  # Use a for loop to avoid map_dfr column-binding issues with named vectors
  loo_rows <- vector("list", length(control_cities))
  for (ci in seq_along(control_cities)) {
    drop_city <- control_cities[ci]
    loo_data  <- shooting_annual_did %>% filter(boro != drop_city)

    est <- se_val <- pv <- cl <- ch <- NA_real_
    n_ps <- NA_integer_; n_pt <- NA_integer_; pok <- NA

    tryCatch({
      m <- feols(shooting_rate ~ did | boro + year,
                 data = loo_data, cluster = ~boro)
      # Use broom::tidy — m$pvalue["did"] returns length 0 in this fixest version
      m_tidy <- broom::tidy(m, conf.int = TRUE)
      est   <- m_tidy$estimate[m_tidy$term == "did"]
      se_val <- m_tidy$std.error[m_tidy$term == "did"]
      pv    <- m_tidy$p.value[m_tidy$term == "did"]
      cl    <- m_tidy$conf.low[m_tidy$term == "did"]
      ch    <- m_tidy$conf.high[m_tidy$term == "did"]

      loo_es <- feols(
        shooting_rate ~ i(factor(year), treated, ref = 2022) | boro + year,
        data = loo_data, cluster = ~boro
      )
      loo_pre_coefs <- broom::tidy(loo_es) %>%
        mutate(event_year = as.numeric(str_extract(term, "\\d{4}"))) %>%
        filter(!is.na(event_year), event_year < 2022)

      n_ps <- as.integer(sum(loo_pre_coefs$p.value < 0.05))
      n_pt <- as.integer(nrow(loo_pre_coefs))
      pok  <- (n_ps == 0)
    }, error = function(e) {
      cat("  FAILED for", drop_city, ":", e$message, "\n")
    })

    loo_rows[[ci]] <- data.frame(
      dropped = drop_city, estimate = est, se = se_val,
      p_value = pv, conf_low = cl, conf_high = ch,
      n_pre_sig = n_ps, n_pre_total = n_pt, parallel_ok = pok,
      stringsAsFactors = FALSE
    )
  }
  loo_results <- do.call(rbind, loo_rows)

  # Full model reference row — use broom::tidy; $pvalue["did"] returns length 0 in this fixest version
  shoot_twfe_did <- broom::tidy(shoot_twfe, conf.int = TRUE) %>% filter(term == "did")
  loo_ref <- tibble(
    dropped     = "(Full model)",
    estimate    = shoot_twfe_did$estimate,
    se          = shoot_twfe_did$std.error,
    p_value     = shoot_twfe_did$p.value,
    conf_low    = shoot_twfe_did$conf.low,
    conf_high   = shoot_twfe_did$conf.high,
    n_pre_sig   = n_sig_shoot,
    n_pre_total = nrow(pre_shoot),
    parallel_ok = n_sig_shoot == 0
  )

  loo_all <- bind_rows(loo_ref, loo_results)
  write_csv(loo_all, file.path(results_dir, "did_shooting_loo.csv"))

  cat("LOO results:\n")
  loo_all %>%
    select(dropped, estimate, se, p_value, parallel_ok) %>%
    mutate(across(c(estimate, se), ~ round(.x, 2)),
           p_value = round(p_value, 3)) %>%
    print(n = Inf)
  cat("\n")

  # Range of estimates
  loo_city <- loo_all %>% filter(dropped != "(Full model)")
  cat("Estimate range (LOO):",
      round(min(loo_city$estimate, na.rm = TRUE), 2), "to",
      round(max(loo_city$estimate, na.rm = TRUE), 2), "\n")
  cat("Full-model estimate:", round(coef(shoot_twfe)["did"], 2), "\n")
  cat("Parallel trends pass in all LOO runs:",
      all(loo_city$parallel_ok, na.rm = TRUE), "\n\n")

  # LOO forest-style plot
  p_loo <- loo_all %>%
    filter(!is.na(estimate)) %>%
    mutate(
      dropped = factor(dropped, levels = rev(loo_all$dropped[!is.na(loo_all$estimate)])),
      is_full = dropped == "(Full model)"
    ) %>%
    ggplot(aes(x = estimate, y = dropped,
               color = is_full, shape = is_full)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high),
                   height = 0.2, linewidth = 0.7) +
    geom_point(size = 3) +
    scale_color_manual(values = c("TRUE" = COL_PURSUIT, "FALSE" = COL_ROBBERY),
                       guide = "none") +
    scale_shape_manual(values = c("TRUE" = 18, "FALSE" = 16), guide = "none") +
    labs(
      title    = "Shooting DiD: Leave-One-Out Sensitivity",
      subtitle = "TWFE estimate + 95% CI | Red diamond = full model",
      x        = "DiD Estimate (shooting rate per 100K)",
      y        = NULL,
      caption  = "Each row drops one comparison city. Full model uses all 7 cities."
    ) +
    theme_pursuit() +
    theme(legend.position = "none")

  save_plot(p_loo, "did_shooting_loo", w = 9, h = 6)
}


# 6. Summary ==================================================================

cat("\n=== DiD SUMMARY ===\n\n")

# Use broom::tidy throughout — $pvalue["did"] returns length 0 in this fixest version
rob_did_est <- broom::tidy(rob_twfe, conf.int = TRUE) %>% filter(term == "did")

cat("ROBBERY (annual, TWFE):\n")
cat("  Estimate:", round(rob_did_est$estimate, 3), "\n")
cat("  SE:", round(rob_did_est$std.error, 3), "\n")
cat("  p-value:", format.pval(rob_did_est$p.value, digits = 3), "\n")
cat("  Pre-treatment parallel trends:", n_sig, "of", n_total, "significant\n\n")

if (!is.null(shooting_panel) && exists("shoot_twfe")) {
  shoot_did_est <- broom::tidy(shoot_twfe, conf.int = TRUE) %>% filter(term == "did")
  cat("SHOOTINGS (annual, TWFE):\n")
  cat("  Estimate:", round(shoot_did_est$estimate, 3), "\n")
  cat("  SE:", round(shoot_did_est$std.error, 3), "\n")
  cat("  p-value:", format.pval(shoot_did_est$p.value, digits = 3), "\n")
  cat("  Pre-treatment parallel trends:", n_sig_shoot, "of", nrow(pre_shoot), "significant\n\n")
}

cat("Results saved to:", results_dir, "\n")
cat("Plots saved to:", plot_dir, "\n\n")

quit(status = 0)
