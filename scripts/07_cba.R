# 07_cba.R — Cost-Benefit Analysis
#
# Costs: Excess pursuit-related collisions × (fatality × VSL + injury + property)
# Benefits: Shooting reductions valued at VSL-fraction; robbery changes counted
#
# Time horizon: Oct 2022 – Sep 2025 (36 months, avoids right-censoring)
# Sub-periods:
#   Escalation+Maddrey: Oct 2022 – Jan 2025 (28 months)
#   Post-Tisch:         Feb 2025 – Sep 2025  (8 months)
#
# Uses ITS coefficients for counterfactual estimation.
#
# IMPORTANT: The cost side (excess pursuit crashes) is well-identified —
# crashes are mechanically linked to pursuit policy. The benefit side (crime
# reductions) requires assuming the ITS-estimated changes are *caused* by
# the pursuit policy, not by secular trends or concurrent policy changes.
# This script includes a causal_share parameter (0–100%) to let the reader
# assess sensitivity to this assumption, and computes the breakeven share
# at which benefits equal costs.

library(tidyverse)
library(lubridate)
library(sandwich)
library(lmtest)
library(broom)
library(here)


# Dirs -------------------------------------------------------------------------

dir.create(here("output"),              showWarnings = FALSE)
dir.create(here("output/plots"),        showWarnings = FALSE)
dir.create(here("output/cba_results"),  showWarnings = FALSE)

plot_dir    <- here("output/plots")
results_dir <- here("output/cba_results")

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

save_plot <- function(p, name, w = 10, h = 6) {
  ggsave(file.path(plot_dir, paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(plot_dir, paste0(name, ".pdf")),
         plot = p, width = w, height = h, bg = "white")
}


# 1. Load data and re-fit ITS for counterfactual ===============================

message("\nCOST-BENEFIT ANALYSIS\n")

panel <- read_csv(here("output/tables/monthly_panel.csv"), show_col_types = FALSE)

INTERVENTION <- as.Date("2022-10-01")
TISCH        <- as.Date("2025-02-01")
END_DATE     <- as.Date("2025-12-01")

its <- panel %>%
  filter(month_date >= as.Date("2018-01-01"),
         month_date <= END_DATE) %>%
  arrange(month_date) %>%
  mutate(
    T = row_number(),
    D = as.integer(month_date >= INTERVENTION),
    P = pmax(0L, as.integer(round(
      difftime(month_date, INTERVENTION, units = "days") / 30.44))),
    month_factor = factor(month(month_date)),
    covid = as.integer(month_date >= as.Date("2020-03-01") &
                         month_date <= as.Date("2021-06-01"))
  )

# Fit ITS models for pursuit crashes, shootings, robberies
fit_its <- function(outcome_var) {
  y <- its[[outcome_var]]
  mod <- lm(y ~ T + D + P + month_factor + covid, data = its)

  # Counterfactual: D=0, P=0
  its_cf <- its %>%
    mutate(
      observed       = y,
      fitted         = predict(mod),
      counterfactual = predict(mod, newdata = its %>% mutate(D = 0L, P = 0L)),
      effect         = fitted - counterfactual
    )

  # NW SEs for the key coefficients
  nw <- coeftest(mod, vcov = NeweyWest(mod, lag = 6, prewhite = FALSE))

  list(data = its_cf, model = mod, nw = nw)
}

crash_its    <- fit_its("pursuit_crashes")
shooting_its <- fit_its("shooting_incidents")
robbery_its  <- fit_its("robbery_count")


# 2. CBA Parameters ============================================================

# Value of a Statistical Life (2024 DOT guidance)
VSL <- 12.5e6

# Pursuit crash parameters
FATALITY_RATE_LOW     <- 0.005  # 0.5% of crashes
FATALITY_RATE_CENTRAL <- 0.010  # 1.0%
FATALITY_RATE_HIGH    <- 0.015  # 1.5%

INJURY_RATE    <- 0.35    # 35% of crashes result in injury
INJURIES_PER   <- 1.5     # average injuries per injury-crash
INJURY_COST    <- 200000  # blended MAIS severity ($50K-$500K)
PROPERTY_COST  <- 20000   # property damage per crash

# Crime cost parameters (Miller et al. / DOJ estimates)
SHOOTING_COST <- 1.25e6   # per shooting incident (VSL-fraction)
ROBBERY_COST  <- 42310    # per robbery (victimization costs)

# Causal attribution parameter
# What share of the ITS-estimated crime reduction is attributable to the
# pursuit policy (vs. secular trends, other concurrent policies, etc.)?
# 1.0 = full attribution; 0.0 = none (costs only)
CAUSAL_SHARES <- c(0, 0.25, 0.50, 0.75, 1.00)


# 3. Compute excess events =====================================================

message("--- Excess Events (Oct 2022 \u2013 Sep 2025) ---")

# Post-period data
post_all <- crash_its$data %>% filter(D == 1)
post_pre_tisch <- post_all %>% filter(month_date < TISCH)
post_tisch     <- post_all %>% filter(month_date >= TISCH)

compute_excess <- function(its_result, period_filter = NULL) {
  d <- its_result$data %>% filter(D == 1)
  if (!is.null(period_filter)) d <- d %>% filter(period_filter(month_date))

  tibble(
    months        = nrow(d),
    total_observed = sum(d$observed),
    total_counterfactual = sum(d$counterfactual),
    excess        = sum(d$observed) - sum(d$counterfactual),
    avg_monthly_effect = mean(d$effect)
  )
}

# Overall (Oct 2022 – Sep 2025)
excess_crashes   <- compute_excess(crash_its)
excess_shootings <- compute_excess(shooting_its)
excess_robberies <- compute_excess(robbery_its)

message("Excess crashes:   ", round(excess_crashes$excess, 1), " over ", excess_crashes$months, " months")
message("Excess shootings: ", round(excess_shootings$excess, 1), " over ", excess_shootings$months, " months")
message("Excess robberies: ", round(excess_robberies$excess, 1), " over ", excess_robberies$months, " months")

# By regime
pre_tisch_filter  <- function(d) d < TISCH
post_tisch_filter <- function(d) d >= TISCH

regime_excess <- bind_rows(
  compute_excess(crash_its, pre_tisch_filter) %>% mutate(outcome = "crashes", regime = "pre_tisch"),
  compute_excess(crash_its, post_tisch_filter) %>% mutate(outcome = "crashes", regime = "post_tisch"),
  compute_excess(shooting_its, pre_tisch_filter) %>% mutate(outcome = "shootings", regime = "pre_tisch"),
  compute_excess(shooting_its, post_tisch_filter) %>% mutate(outcome = "shootings", regime = "post_tisch"),
  compute_excess(robbery_its, pre_tisch_filter) %>% mutate(outcome = "robberies", regime = "pre_tisch"),
  compute_excess(robbery_its, post_tisch_filter) %>% mutate(outcome = "robberies", regime = "post_tisch")
)


# 4. Cost computation ==========================================================

message("--- Crash Costs ---")

compute_crash_costs <- function(excess_crashes, fatality_rate) {
  n_crashes <- max(0, excess_crashes)

  fatalities     <- n_crashes * fatality_rate
  fatality_cost  <- fatalities * VSL

  injury_crashes <- n_crashes * INJURY_RATE
  injuries       <- injury_crashes * INJURIES_PER
  injury_cost    <- injuries * INJURY_COST

  property_cost  <- n_crashes * PROPERTY_COST

  total <- fatality_cost + injury_cost + property_cost

  tibble(
    excess_crashes = n_crashes,
    fatality_rate  = fatality_rate,
    est_fatalities = fatalities,
    fatality_cost  = fatality_cost,
    est_injuries   = injuries,
    injury_cost    = injury_cost,
    property_cost  = property_cost,
    total_crash_cost = total
  )
}

costs_low     <- compute_crash_costs(excess_crashes$excess, FATALITY_RATE_LOW)
costs_central <- compute_crash_costs(excess_crashes$excess, FATALITY_RATE_CENTRAL)
costs_high    <- compute_crash_costs(excess_crashes$excess, FATALITY_RATE_HIGH)

crash_costs <- bind_rows(
  costs_low %>% mutate(scenario = "low"),
  costs_central %>% mutate(scenario = "central"),
  costs_high %>% mutate(scenario = "high")
)

message("Crash cost scenarios:")
print(crash_costs %>% select(scenario, excess_crashes, est_fatalities, total_crash_cost))


# 5. Benefit computation (full attribution baseline) ===========================

message("--- Crime Benefits (Full Attribution Baseline) ---")

# Shootings reduced → benefit
shooting_reduction <- abs(min(0, excess_shootings$excess))  # only count if negative (reduction)
shooting_benefit_full <- shooting_reduction * SHOOTING_COST

# Robberies: if excess is positive (increase), count as additional cost
robbery_change <- excess_robberies$excess
if (robbery_change > 0) {
  robbery_cost_additional <- robbery_change * ROBBERY_COST
  robbery_benefit_full <- 0
} else {
  robbery_cost_additional <- 0
  robbery_benefit_full <- abs(robbery_change) * ROBBERY_COST
}

message("Shooting reduction (ITS estimate): ", round(shooting_reduction, 1), " incidents")
message("  Full-attribution value: $", format(shooting_benefit_full, big.mark = ","))
message("Robbery change (ITS estimate): ", round(robbery_change, 1), " ",
        ifelse(robbery_change > 0, "(increase)", "(decrease)"))


# 6. Costs-only scenario (most defensible) =====================================

message("--- Costs-Only Scenario (No Crime Attribution) ---")
message("Pursuit collisions are mechanically linked to pursuit policy.")
message("Crime reductions may reflect secular trends or concurrent policies.")

costs_only <- costs_central %>%
  select(excess_crashes, est_fatalities, total_crash_cost) %>%
  mutate(
    robbery_cost = robbery_cost_additional,
    total_cost   = total_crash_cost + robbery_cost
  )

message("Total crash costs (central): $", format(round(costs_only$total_crash_cost), big.mark = ","))
if (robbery_cost_additional > 0) {
  message("Robbery increase costs: $", format(round(robbery_cost_additional), big.mark = ","))
}
message("Total costs (no crime benefit): $", format(round(costs_only$total_cost), big.mark = ","))


# 7. Causal attribution scenarios ==============================================

message("--- Causal Attribution Scenarios ---")
message("What share of ITS-estimated crime changes is caused by pursuit policy?")

attribution_results <- tibble(causal_share = CAUSAL_SHARES) %>%
  mutate(
    shooting_benefit = shooting_benefit_full * causal_share,
    robbery_benefit  = robbery_benefit_full * causal_share,
    robbery_cost     = robbery_cost_additional * causal_share,
    crash_cost       = costs_central$total_crash_cost,
    total_benefit    = shooting_benefit + robbery_benefit,
    total_cost       = crash_cost + robbery_cost,
    net              = total_benefit - total_cost,
    bc_ratio         = ifelse(total_cost > 0, total_benefit / total_cost, NA_real_),
    label            = paste0(causal_share * 100, "% attribution")
  )

message("Net benefit by causal attribution share:")
print(attribution_results %>%
        select(label, crash_cost, shooting_benefit, total_cost, total_benefit, net, bc_ratio),
      width = Inf)

write_csv(attribution_results, file.path(results_dir, "cba_attribution_scenarios.csv"))


# 8. Breakeven analysis ========================================================

message("--- Breakeven Analysis ---")

# At what causal_share does total_benefit = total_cost?
# Benefit = shooting_benefit_full × s + robbery_benefit_full × s
# Cost = crash_cost + robbery_cost_additional × s
# Solve: (shooting_benefit_full + robbery_benefit_full) × s = crash_cost + robbery_cost_additional × s
# s × (shooting_benefit_full + robbery_benefit_full - robbery_cost_additional) = crash_cost
benefit_per_unit <- shooting_benefit_full + robbery_benefit_full - robbery_cost_additional
crash_cost_central <- costs_central$total_crash_cost

if (benefit_per_unit > 0) {
  breakeven_share <- crash_cost_central / benefit_per_unit
  message("Breakeven causal share: ", round(breakeven_share * 100, 1), "%")
  message("The pursuit policy is net-positive only if at least ",
          round(breakeven_share * 100, 1), "% of the")
  message("ITS-estimated shooting decline is caused by the policy.")
} else {
  breakeven_share <- NA_real_
  message("No breakeven: benefits never exceed costs at any attribution level.")
}


# 9. Net assessment (full attribution, by fatality rate scenario) ==============

message("--- Net Assessment (Full Attribution) ---")

net_assessment <- crash_costs %>%
  mutate(
    shooting_benefit   = shooting_benefit_full,
    robbery_change     = robbery_change,
    robbery_additional_cost = robbery_cost_additional,
    total_cost = total_crash_cost + robbery_cost_additional,
    total_benefit = shooting_benefit_full + robbery_benefit_full,
    net = total_benefit - total_cost,
    benefit_cost_ratio = total_benefit / total_cost
  ) %>%
  select(scenario, total_crash_cost, robbery_additional_cost, total_cost,
         shooting_benefit, total_benefit, net, benefit_cost_ratio)

message("Net assessment by fatality rate scenario (100% attribution):")
print(net_assessment, width = Inf)

write_csv(net_assessment, file.path(results_dir, "cba_net_assessment.csv"))


# 10. CBA summary table =======================================================

cba_summary <- tibble(
  component = c(
    "Time horizon",
    "--- COSTS (well-identified) ---",
    "Excess pursuit crashes",
    "Estimated fatalities (central, 1%)",
    "Total crash costs (central)",
    "--- ITS-ESTIMATED CRIME CHANGES ---",
    "Shooting reduction (ITS estimate)",
    "Robbery change (ITS estimate)",
    "--- BENEFITS (require causal assumption) ---",
    "Shooting benefit (100% attribution)",
    "Breakeven causal share",
    "--- NET ASSESSMENT ---",
    "Net at 100% attribution (central)",
    "Net at 50% attribution (central)",
    "Net at 25% attribution (central)",
    "Net at 0% attribution (costs only)",
    "B:C ratio at 100% attribution"
  ),
  value = c(
    paste0(excess_crashes$months, " months (Oct 2022 - Sep 2025)"),
    "",
    round(excess_crashes$excess, 0),
    round(costs_central$est_fatalities, 1),
    paste0("$", format(round(costs_central$total_crash_cost), big.mark = ",")),
    "",
    round(excess_shootings$excess, 0),
    round(excess_robberies$excess, 0),
    "",
    paste0("$", format(round(shooting_benefit_full), big.mark = ",")),
    ifelse(is.na(breakeven_share), "N/A",
           paste0(round(breakeven_share * 100, 1), "%")),
    "",
    paste0("$", format(round(attribution_results$net[attribution_results$causal_share == 1.0]), big.mark = ",")),
    paste0("$", format(round(attribution_results$net[attribution_results$causal_share == 0.5]), big.mark = ",")),
    paste0("$", format(round(attribution_results$net[attribution_results$causal_share == 0.25]), big.mark = ",")),
    paste0("$", format(round(attribution_results$net[attribution_results$causal_share == 0.0]), big.mark = ",")),
    round(net_assessment$benefit_cost_ratio[net_assessment$scenario == "central"], 2)
  )
)

write_csv(cba_summary, file.path(results_dir, "cba_summary.csv"))


# 11. Regime-specific CBA =====================================================

message("--- Regime-Specific CBA ---")

regime_cba <- regime_excess %>%
  group_by(regime, outcome) %>%
  summarise(across(everything(), first), .groups = "drop") %>%
  pivot_wider(
    id_cols = regime,
    names_from = outcome,
    values_from = c(months, excess, avg_monthly_effect),
    names_glue = "{outcome}_{.value}"
  )

# Add cost/benefit columns (full attribution for comparability)
regime_cba <- regime_cba %>%
  mutate(
    crash_cost_central = pmax(0, crashes_excess) * (
      FATALITY_RATE_CENTRAL * VSL +
      INJURY_RATE * INJURIES_PER * INJURY_COST +
      PROPERTY_COST
    ),
    shooting_benefit = pmax(0, -shootings_excess) * SHOOTING_COST,
    robbery_cost     = pmax(0, robberies_excess) * ROBBERY_COST,
    net_full_attribution = shooting_benefit - crash_cost_central - robbery_cost,
    net_costs_only       = -(crash_cost_central + robbery_cost)
  )

message("Regime-specific CBA:")
print(regime_cba, width = Inf)

write_csv(regime_cba, file.path(results_dir, "cba_by_regime.csv"))


# 12. Sensitivity analysis (with causal attribution) ==========================

message("--- Sensitivity Grid (with Causal Attribution) ---")

# Grid: VSL multiplier × fatality rate × shooting estimate × causal share
sens_grid <- expand_grid(
  vsl_mult      = c(0.7, 1.0, 1.3),
  fatality_rate = c(0.005, 0.010, 0.015),
  shooting_mult = c(0.7, 1.0, 1.3),
  causal_share  = CAUSAL_SHARES
)

sens_results <- sens_grid %>%
  mutate(
    vsl = VSL * vsl_mult,

    # Crash costs (always fully attributed — mechanically linked)
    crash_fatality_cost = excess_crashes$excess * fatality_rate * vsl,
    crash_injury_cost   = excess_crashes$excess * INJURY_RATE * INJURIES_PER * INJURY_COST,
    crash_property_cost = excess_crashes$excess * PROPERTY_COST,
    total_crash_cost    = pmax(0, crash_fatality_cost + crash_injury_cost + crash_property_cost),

    # Crime benefits (scaled by shooting_mult AND causal_share)
    adj_shooting_reduction = shooting_reduction * shooting_mult,
    shooting_benefit       = adj_shooting_reduction * SHOOTING_COST * vsl_mult * causal_share,

    # Robbery cost (scaled by causal_share)
    robbery_cost = robbery_cost_additional * causal_share,

    total_cost    = total_crash_cost + robbery_cost,
    total_benefit = shooting_benefit,
    net           = total_benefit - total_cost,
    bc_ratio      = ifelse(total_cost > 0, total_benefit / total_cost, NA_real_)
  )

write_csv(sens_results, file.path(results_dir, "cba_sensitivity.csv"))

message("Sensitivity grid: ", nrow(sens_results), " scenarios")
message("B:C ratio range (full attribution): ",
        round(min(sens_results$bc_ratio[sens_results$causal_share == 1], na.rm = TRUE), 2),
        " to ",
        round(max(sens_results$bc_ratio[sens_results$causal_share == 1], na.rm = TRUE), 2))
message("B:C ratio range (50% attribution): ",
        round(min(sens_results$bc_ratio[sens_results$causal_share == 0.5], na.rm = TRUE), 2),
        " to ",
        round(max(sens_results$bc_ratio[sens_results$causal_share == 0.5], na.rm = TRUE), 2))


# 13. Tornado plot (with causal attribution) ===================================

# Baseline: vsl_mult=1, fatality_rate=0.01, shooting_mult=1, causal_share=1
base_net <- sens_results %>%
  filter(vsl_mult == 1, fatality_rate == 0.01, shooting_mult == 1, causal_share == 1) %>%
  pull(net)

tornado_data <- bind_rows(
  tibble(
    parameter = "Causal Attribution",
    low  = sens_results %>%
      filter(vsl_mult == 1, fatality_rate == 0.01, shooting_mult == 1, causal_share == 0) %>%
      pull(net),
    high = sens_results %>%
      filter(vsl_mult == 1, fatality_rate == 0.01, shooting_mult == 1, causal_share == 1) %>%
      pull(net)
  ),
  tibble(
    parameter = "VSL",
    low  = sens_results %>%
      filter(vsl_mult == 0.7, fatality_rate == 0.01, shooting_mult == 1, causal_share == 1) %>%
      pull(net),
    high = sens_results %>%
      filter(vsl_mult == 1.3, fatality_rate == 0.01, shooting_mult == 1, causal_share == 1) %>%
      pull(net)
  ),
  tibble(
    parameter = "Fatality Rate",
    low  = sens_results %>%
      filter(vsl_mult == 1, fatality_rate == 0.005, shooting_mult == 1, causal_share == 1) %>%
      pull(net),
    high = sens_results %>%
      filter(vsl_mult == 1, fatality_rate == 0.015, shooting_mult == 1, causal_share == 1) %>%
      pull(net)
  ),
  tibble(
    parameter = "Shooting Reduction",
    low  = sens_results %>%
      filter(vsl_mult == 1, fatality_rate == 0.01, shooting_mult == 0.7, causal_share == 1) %>%
      pull(net),
    high = sens_results %>%
      filter(vsl_mult == 1, fatality_rate == 0.01, shooting_mult == 1.3, causal_share == 1) %>%
      pull(net)
  )
) %>%
  mutate(base = base_net, spread = abs(high - low)) %>%
  arrange(desc(spread))

p_tornado <- tornado_data %>%
  mutate(parameter = fct_reorder(parameter, spread)) %>%
  ggplot() +
  geom_segment(aes(x = low / 1e6, xend = high / 1e6, y = parameter, yend = parameter),
               linewidth = 8, color = "#003049", alpha = 0.7) +
  geom_vline(xintercept = base_net / 1e6, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "solid", color = "red", alpha = 0.5) +
  labs(
    title    = "Sensitivity: Net Benefit (Benefits - Costs)",
    subtitle = "Range across parameter values | Dashed = central | Red = breakeven",
    x = "Net Benefit ($ millions)", y = NULL,
    caption  = paste0("Parameters varied: causal share (0-100%), VSL (\u00b130%), ",
                      "fatality rate (0.5-1.5%), shooting reduction (\u00b130%)")
  ) +
  theme_pursuit()

save_plot(p_tornado, "cba_tornado", w = 10, h = 5)


# 14. Attribution plot =========================================================

p_attr <- attribution_results %>%
  ggplot(aes(x = causal_share * 100, y = net / 1e6)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red", alpha = 0.5) +
  geom_line(color = "#003049", linewidth = 1.2) +
  geom_point(color = "#003049", size = 3) +
  {if (!is.na(breakeven_share))
    geom_vline(xintercept = breakeven_share * 100,
               linetype = "dashed", color = "grey40")} +
  {if (!is.na(breakeven_share))
    annotate("text", x = breakeven_share * 100, y = min(attribution_results$net / 1e6) * 0.5,
             label = paste0("Breakeven:\n", round(breakeven_share * 100, 1), "%"),
             hjust = -0.1, size = 3.5, color = "grey30")} +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
  labs(
    title    = "Net Benefit by Causal Attribution Share",
    subtitle = "What share of ITS-estimated crime reduction is caused by pursuit policy?",
    x = "Causal Attribution (%)", y = "Net Benefit ($ millions)",
    caption  = paste0("Central parameters: VSL = $12.5M, fatality rate = 1%, ",
                      "injury rate = 35%\nRed line = breakeven (costs = benefits)")
  ) +
  theme_pursuit()

save_plot(p_attr, "cba_attribution", w = 10, h = 6)


# Done =========================================================================

message("CBA results saved to: ", results_dir)
message("Plots saved to: ", plot_dir)
