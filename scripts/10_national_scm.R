# 10_national_scm.R — SCM with Borough-Level Treatment + National Donors
#
# Treatment: Each NYC borough separately (5 SCM runs per outcome)
# Donor pool: Top US cities (Kaplan UCR for robbery, agency data for shootings)
# Method: tidysynth with permutation inference
#
# Requires output from 08_national_data_prep.R
#
# NOTE (2026-07-30): As of this date, the ROBBERY output of this script IS
# back in the manuscript, as Table C2 in Appendix C (a vanilla-SCM robustness
# check requested by the editor) — the 2026-03-05 note below describing this
# as fully cut from the manuscript is now only true of the shooting outcome,
# which remains degenerate/unreported in tabular form.
# Active pipeline: 01 → 03 → 06 → 07 → 08 → 10 → 14 → 17 → 15 → 16 → 00_regenerate

library(tidyverse)
library(tidysynth)
library(here)

cat("\n=== NATIONAL SCM ANALYSIS ===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")

results_dir <- here("output", "national_scm")
plot_dir    <- here("output", "plots")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(plot_dir, showWarnings = FALSE)

set.seed(42)

# Project theme and save function
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

robbery_file <- here("output", "national_did", "combined_annual_panel.csv")
if (!file.exists(robbery_file)) {
  stop("Run 08_national_data_prep.R first. Missing: ", robbery_file)
}
robbery_panel <- read_csv(robbery_file, show_col_types = FALSE)

cat("Robbery panel:", nrow(robbery_panel), "rows,",
    n_distinct(robbery_panel$boro), "units,",
    min(robbery_panel$year), "-", max(robbery_panel$year), "\n")

shooting_file <- here("output", "national_did", "combined_shooting_annual.csv")
if (file.exists(shooting_file)) {
  shooting_panel <- read_csv(shooting_file, show_col_types = FALSE)
  cat("Shooting panel:", nrow(shooting_panel), "rows,",
      n_distinct(shooting_panel$boro), "units\n\n")
} else {
  shooting_panel <- NULL
  cat("Shooting panel not found — skipping shooting SCM.\n\n")
}


# 2. Robbery SCM — per-borough ================================================

cat("--- Robbery SCM (Per-Borough) ---\n\n")

boroughs <- c("Manhattan", "Brooklyn", "Queens", "Bronx", "Staten Island")

# Require complete data: no NA robbery_rate values and at least 8 years
robbery_complete <- robbery_panel %>%
  filter(!is.na(robbery_rate)) %>%
  group_by(boro) %>%
  filter(n_distinct(year) >= 8) %>%
  ungroup()

# Check donor coverage
donor_coverage <- robbery_complete %>%
  filter(treated == 0) %>%
  group_by(boro) %>%
  summarise(
    first_year = min(year),
    last_year  = max(year),
    n_years    = n_distinct(year),
    .groups = "drop"
  )

cat("Donor cities with >= 8 years:\n")
print(donor_coverage, n = Inf)
cat("\n")

# Determine common year range for robbery SCM
# Need pre-period and post-period years present in most donors
robbery_years <- robbery_complete %>%
  filter(treated == 0) %>%
  count(year) %>%
  filter(n >= 5)  # year must have at least 5 donors reporting

pre_years  <- robbery_years$year[robbery_years$year <= 2022]
post_years <- robbery_years$year[robbery_years$year >= 2023]
all_scm_years <- sort(unique(c(pre_years, post_years)))

cat("Pre-treatment years with >= 5 donors:", paste(pre_years, collapse = ", "), "\n")
cat("Post-treatment years:", paste(post_years, collapse = ", "), "\n\n")

# Filter to common window (include 2022 — the treatment year)
robbery_scm_data <- robbery_complete %>%
  filter(year %in% all_scm_years)

# Drop units that are missing any year in the common window
n_required_years <- length(all_scm_years)
robbery_scm_data <- robbery_scm_data %>%
  group_by(boro) %>%
  filter(n() == n_required_years) %>%
  ungroup()

n_donors_rob <- n_distinct(robbery_scm_data$boro[robbery_scm_data$treated == 0])
cat("Balanced robbery SCM panel:", n_distinct(robbery_scm_data$boro), "units x",
    n_required_years, "years\n")
cat("  Donors:", n_donors_rob, "cities\n\n")

# Run SCM for each borough
robbery_scm_results <- list()
robbery_scm_weights <- list()
robbery_scm_gaps    <- list()

for (boro_name in boroughs) {
  cat("  SCM for", boro_name, "...\n")

  # Build panel: this borough + all donors
  panel_i <- robbery_scm_data %>%
    filter(boro == boro_name | treated == 0)

  # Skip if borough missing from panel
  if (!boro_name %in% panel_i$boro) {
    cat("    SKIPPED — borough not in balanced panel.\n")
    next
  }

  tryCatch({
    # i_time = 2023: first full post-treatment year
    # pre-period = 2014-2022 (2022 is the treatment onset year, included in optimization)
    # Use 7 individual year predictors spanning the full pre-period.
    # Averaged period predictors (early/mid/late) cause singularity with 44 donors
    # because many donors are collinear in predictor space when predictors are few.
    # Individual years add more constraints and resolve the singularity.
    scm_i <- panel_i %>%
      synthetic_control(
        outcome = robbery_rate,
        unit    = boro,
        time    = year,
        i_unit  = boro_name,
        i_time  = 2023,
        generate_placebos = TRUE
      ) %>%
      generate_predictor(time_window = 2014, rob_2014 = robbery_rate) %>%
      generate_predictor(time_window = 2015, rob_2015 = robbery_rate) %>%
      generate_predictor(time_window = 2017, rob_2017 = robbery_rate) %>%
      generate_predictor(time_window = 2018, rob_2018 = robbery_rate) %>%
      generate_predictor(time_window = 2019, rob_2019 = robbery_rate) %>%
      generate_predictor(time_window = 2021, rob_2021 = robbery_rate) %>%
      generate_predictor(time_window = 2022, rob_2022 = robbery_rate) %>%
      generate_weights(optimization_window = 2014:2022) %>%
      generate_control()

    # Store full SCM object for placebo plots
    robbery_scm_results[[boro_name]] <- scm_i

    # Extract weights
    w_i <- scm_i %>%
      grab_unit_weights() %>%
      filter(weight > 0.001) %>%
      arrange(desc(weight)) %>%
      mutate(treated_boro = boro_name)

    robbery_scm_weights[[boro_name]] <- w_i

    cat("    Top donors:", paste(head(w_i$unit, 3), round(head(w_i$weight, 3), 3),
                                 sep = "=", collapse = ", "), "\n")

    # Check degeneracy
    if (nrow(w_i) > 0 && w_i$weight[1] > 0.80) {
      cat("    WARNING: Degenerate — top donor has", round(w_i$weight[1] * 100, 1), "% weight\n")
    }

    # Extract gaps
    g_i <- scm_i %>%
      grab_synthetic_control() %>%
      mutate(gap = real_y - synth_y, treated_boro = boro_name)
    robbery_scm_gaps[[boro_name]] <- g_i

    pre_mspe <- g_i %>% filter(time_unit < 2023) %>%
      summarise(mspe = mean(gap^2)) %>% pull(mspe)
    cat("    Pre-MSPE:", round(pre_mspe, 2), "\n")

    # Fisher's exact p-value
    sig_i <- scm_i %>% grab_significance()
    p_fisher <- sig_i %>% filter(unit_name == boro_name) %>% pull(fishers_exact_pvalue)
    cat("    Fisher's p:", round(p_fisher, 3), "\n")

  }, error = function(e) {
    cat("    FAILED:", e$message, "\n")
  })
}

# Combine and save robbery results
if (length(robbery_scm_weights) > 0) {
  all_rob_weights <- bind_rows(robbery_scm_weights)
  write_csv(all_rob_weights, file.path(results_dir, "scm_robbery_weights.csv"))

  all_rob_gaps <- bind_rows(robbery_scm_gaps)
  write_csv(all_rob_gaps, file.path(results_dir, "scm_robbery_gaps.csv"))

  # Summary table
  rob_summary <- all_rob_gaps %>%
    group_by(treated_boro) %>%
    summarise(
      pre_mspe   = mean(gap[time_unit < 2023]^2),
      post_gap   = mean(gap[time_unit >= 2023]),
      max_weight = max(all_rob_weights$weight[all_rob_weights$treated_boro == first(treated_boro)]),
      top_donor  = all_rob_weights$unit[all_rob_weights$treated_boro == first(treated_boro)][1],
      .groups = "drop"
    )

  # Add Fisher's p-values
  rob_summary$fisher_p <- NA_real_
  for (bn in rob_summary$treated_boro) {
    if (!is.null(robbery_scm_results[[bn]])) {
      sig <- robbery_scm_results[[bn]] %>% grab_significance()
      rob_summary$fisher_p[rob_summary$treated_boro == bn] <-
        sig$fishers_exact_pvalue[sig$unit_name == bn]
    }
  }

  write_csv(rob_summary, file.path(results_dir, "scm_robbery_summary.csv"))

  cat("\nRobbery SCM Summary:\n")
  print(rob_summary)
  cat("\n")
}


# 3. Robbery SCM plots ========================================================

cat("--- Robbery SCM Plots ---\n\n")

if (length(robbery_scm_gaps) > 0) {

  # Fit plots: actual vs synthetic for each borough
  for (boro_name in names(robbery_scm_gaps)) {
    g_i <- robbery_scm_gaps[[boro_name]]

    p_fit <- g_i %>%
      pivot_longer(cols = c(real_y, synth_y), names_to = "series", values_to = "rate") %>%
      mutate(series = recode(series, real_y = "Actual", synth_y = "Synthetic")) %>%
      ggplot(aes(time_unit, rate, color = series, linetype = series)) +
      geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      scale_color_manual(values = c("Actual" = COL_PURSUIT, "Synthetic" = "grey40")) +
      scale_linetype_manual(values = c("Actual" = "solid", "Synthetic" = "dashed")) +
      labs(
        title = paste0("SCM: Robbery Rate — ", boro_name, " vs. Synthetic"),
        subtitle = paste0("Donor pool: ", n_donors_rob, " US cities (Kaplan UCR)"),
        x = "Year", y = "Robbery Rate (per 100K)",
        color = NULL, linetype = NULL,
        caption = "Vertical line = Oct 2022 pursuit policy change"
      ) +
      theme_pursuit()

    save_plot(p_fit, paste0("scm_robbery_", tolower(gsub(" ", "_", boro_name))))
  }

  # Combined gap plot
  p_all_gaps <- bind_rows(robbery_scm_gaps) %>%
    ggplot(aes(time_unit, gap, color = treated_boro)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2) +
    labs(
      title = "SCM Gap Plots: Robbery Rate by Borough",
      subtitle = "Actual - Synthetic | Positive = borough above synthetic",
      x = "Year", y = "Gap (per 100K)",
      color = "Borough",
      caption = paste0("Donor pool: ", n_donors_rob, " US cities | Vertical line = intervention")
    ) +
    theme_pursuit() +
    theme(legend.position = "right")

  save_plot(p_all_gaps, "scm_robbery_all_gaps", w = 11, h = 7)

  # Placebo plots for each borough
  for (boro_name in names(robbery_scm_results)) {
    tryCatch({
      p_placebo <- robbery_scm_results[[boro_name]] %>%
        plot_placebos(prune = TRUE) +
        labs(
          title = paste0("SCM Placebo Test: Robbery Rate — ", boro_name),
          subtitle = paste0(boro_name, " highlighted | Grey = donor city placebos"),
          x = "Year", y = "Gap (Actual - Synthetic)"
        ) +
        theme_pursuit()

      save_plot(p_placebo, paste0("scm_robbery_placebo_", tolower(gsub(" ", "_", boro_name))))
    }, error = function(e) {
      cat("  Placebo plot failed for", boro_name, ":", e$message, "\n")
    })
  }
}


# 4. Shooting SCM — per-borough ===============================================

if (!is.null(shooting_panel)) {

  cat("\n--- Shooting SCM (Per-Borough) ---\n\n")

  # Filter: need at least 2018-2024 for a reasonable pre/post split
  # Drop Cincinnati (2023+ only) and any city with insufficient pre-period
  shooting_scm_data <- shooting_panel %>%
    filter(year >= 2018, year <= 2024) %>%
    group_by(boro) %>%
    filter(n_distinct(year) == 7) %>%  # all 7 years
    ungroup()

  n_donors_shoot <- n_distinct(shooting_scm_data$boro[shooting_scm_data$treated == 0])
  cat("Balanced shooting SCM panel:", n_distinct(shooting_scm_data$boro), "units x 7 years\n")
  cat("  Donors:", n_donors_shoot, "cities\n\n")

  shoot_pre_years  <- 2018:2021
  shoot_post_years <- 2023:2024

  shooting_scm_results <- list()
  shooting_scm_weights <- list()
  shooting_scm_gaps    <- list()

  for (boro_name in boroughs) {
    cat("  SCM for", boro_name, "...\n")

    panel_i <- shooting_scm_data %>%
      filter(boro == boro_name | treated == 0)

    if (!boro_name %in% panel_i$boro) {
      cat("    SKIPPED — borough not in balanced panel.\n")
      next
    }

    tryCatch({
      scm_i <- panel_i %>%
        synthetic_control(
          outcome = shooting_rate,
          unit    = boro,
          time    = year,
          i_unit  = boro_name,
          i_time  = 2023,
          generate_placebos = TRUE
        ) %>%
        generate_predictor(time_window = 2018, shoot_2018 = shooting_rate) %>%
        generate_predictor(time_window = 2020, shoot_2020 = shooting_rate) %>%
        generate_predictor(time_window = 2021, shoot_2021 = shooting_rate) %>%
        generate_weights(optimization_window = shoot_pre_years) %>%
        generate_control()

      shooting_scm_results[[boro_name]] <- scm_i

      w_i <- scm_i %>%
        grab_unit_weights() %>%
        filter(weight > 0.001) %>%
        arrange(desc(weight)) %>%
        mutate(treated_boro = boro_name)

      shooting_scm_weights[[boro_name]] <- w_i

      cat("    Top donors:", paste(head(w_i$unit, 3), round(head(w_i$weight, 3), 3),
                                   sep = "=", collapse = ", "), "\n")

      if (nrow(w_i) > 0 && w_i$weight[1] > 0.80) {
        cat("    WARNING: Degenerate — top donor has", round(w_i$weight[1] * 100, 1), "% weight\n")
      }

      g_i <- scm_i %>%
        grab_synthetic_control() %>%
        mutate(gap = real_y - synth_y, treated_boro = boro_name)
      shooting_scm_gaps[[boro_name]] <- g_i

      pre_mspe <- g_i %>% filter(time_unit < 2023) %>%
        summarise(mspe = mean(gap^2)) %>% pull(mspe)
      cat("    Pre-MSPE:", round(pre_mspe, 2), "\n")

      sig_i <- scm_i %>% grab_significance()
      p_fisher <- sig_i %>% filter(unit_name == boro_name) %>% pull(fishers_exact_pvalue)
      cat("    Fisher's p:", round(p_fisher, 3), "\n")

    }, error = function(e) {
      cat("    FAILED:", e$message, "\n")
    })
  }

  # Combine and save shooting results
  if (length(shooting_scm_weights) > 0) {
    all_shoot_weights <- bind_rows(shooting_scm_weights)
    write_csv(all_shoot_weights, file.path(results_dir, "scm_shooting_weights.csv"))

    all_shoot_gaps <- bind_rows(shooting_scm_gaps)
    write_csv(all_shoot_gaps, file.path(results_dir, "scm_shooting_gaps.csv"))

    shoot_summary <- all_shoot_gaps %>%
      group_by(treated_boro) %>%
      summarise(
        pre_mspe   = mean(gap[time_unit < 2023]^2),
        post_gap   = mean(gap[time_unit >= 2023]),
        max_weight = max(all_shoot_weights$weight[all_shoot_weights$treated_boro == first(treated_boro)]),
        top_donor  = all_shoot_weights$unit[all_shoot_weights$treated_boro == first(treated_boro)][1],
        .groups = "drop"
      )

    shoot_summary$fisher_p <- NA_real_
    for (bn in shoot_summary$treated_boro) {
      if (!is.null(shooting_scm_results[[bn]])) {
        sig <- shooting_scm_results[[bn]] %>% grab_significance()
        shoot_summary$fisher_p[shoot_summary$treated_boro == bn] <-
          sig$fishers_exact_pvalue[sig$unit_name == bn]
      }
    }

    write_csv(shoot_summary, file.path(results_dir, "scm_shooting_summary.csv"))

    cat("\nShooting SCM Summary:\n")
    print(shoot_summary)
    cat("\n")
  }


  # 5. Shooting SCM plots =====================================================

  cat("--- Shooting SCM Plots ---\n\n")

  if (length(shooting_scm_gaps) > 0) {

    for (boro_name in names(shooting_scm_gaps)) {
      g_i <- shooting_scm_gaps[[boro_name]]

      p_fit <- g_i %>%
        pivot_longer(cols = c(real_y, synth_y), names_to = "series", values_to = "rate") %>%
        mutate(series = recode(series, real_y = "Actual", synth_y = "Synthetic")) %>%
        ggplot(aes(time_unit, rate, color = series, linetype = series)) +
        geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_color_manual(values = c("Actual" = COL_PURSUIT, "Synthetic" = "grey40")) +
        scale_linetype_manual(values = c("Actual" = "solid", "Synthetic" = "dashed")) +
        labs(
          title = paste0("SCM: Shooting Rate — ", boro_name, " vs. Synthetic"),
          subtitle = paste0("Donor pool: ", n_donors_shoot, " US cities (police-reported)"),
          x = "Year", y = "Shooting Rate (per 100K)",
          color = NULL, linetype = NULL,
          caption = "Vertical line = Oct 2022 pursuit policy change"
        ) +
        theme_pursuit()

      save_plot(p_fit, paste0("scm_shooting_", tolower(gsub(" ", "_", boro_name))))
    }

    # Combined gap plot
    p_shoot_gaps <- bind_rows(shooting_scm_gaps) %>%
      ggplot(aes(time_unit, gap, color = treated_boro)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      geom_vline(xintercept = 2022.5, linetype = "dashed", color = "grey50") +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2) +
      labs(
        title = "SCM Gap Plots: Shooting Rate by Borough",
        subtitle = "Actual - Synthetic | Positive = borough above synthetic",
        x = "Year", y = "Gap (per 100K)",
        color = "Borough",
        caption = paste0("Donor pool: ", n_donors_shoot, " cities | Vertical line = intervention")
      ) +
      theme_pursuit() +
      theme(legend.position = "right")

    save_plot(p_shoot_gaps, "scm_shooting_all_gaps", w = 11, h = 7)

    # Placebo plots
    for (boro_name in names(shooting_scm_results)) {
      tryCatch({
        p_placebo <- shooting_scm_results[[boro_name]] %>%
          plot_placebos(prune = TRUE) +
          labs(
            title = paste0("SCM Placebo Test: Shooting Rate — ", boro_name),
            subtitle = paste0(boro_name, " highlighted | Grey = donor city placebos"),
            x = "Year", y = "Gap (Actual - Synthetic)"
          ) +
          theme_pursuit()

        save_plot(p_placebo, paste0("scm_shooting_placebo_", tolower(gsub(" ", "_", boro_name))))
      }, error = function(e) {
        cat("  Placebo plot failed for", boro_name, ":", e$message, "\n")
      })
    }
  }
}


# 6. Summary ==================================================================

cat("\n=== SCM SUMMARY ===\n\n")

if (exists("rob_summary") && nrow(rob_summary) > 0) {
  cat("ROBBERY SCM (per-borough):\n")
  for (i in seq_len(nrow(rob_summary))) {
    cat(sprintf("  %-15s post_gap=%+7.1f  pre_MSPE=%7.1f  Fisher_p=%.3f  top_donor=%s (%.0f%%)\n",
                rob_summary$treated_boro[i],
                rob_summary$post_gap[i],
                rob_summary$pre_mspe[i],
                rob_summary$fisher_p[i],
                rob_summary$top_donor[i],
                rob_summary$max_weight[i] * 100))
  }
  cat("\n")

  n_degenerate <- sum(rob_summary$max_weight > 0.80)
  n_sig <- sum(rob_summary$fisher_p < 0.10, na.rm = TRUE)
  cat("  Degenerate (top donor > 80%):", n_degenerate, "of", nrow(rob_summary), "\n")
  cat("  Significant (Fisher p < .10):", n_sig, "of", nrow(rob_summary), "\n\n")
}

if (exists("shoot_summary") && nrow(shoot_summary) > 0) {
  cat("SHOOTING SCM (per-borough):\n")
  for (i in seq_len(nrow(shoot_summary))) {
    cat(sprintf("  %-15s post_gap=%+7.1f  pre_MSPE=%7.1f  Fisher_p=%.3f  top_donor=%s (%.0f%%)\n",
                shoot_summary$treated_boro[i],
                shoot_summary$post_gap[i],
                shoot_summary$pre_mspe[i],
                shoot_summary$fisher_p[i],
                shoot_summary$top_donor[i],
                shoot_summary$max_weight[i] * 100))
  }
  cat("\n")

  n_degenerate <- sum(shoot_summary$max_weight > 0.80)
  n_sig <- sum(shoot_summary$fisher_p < 0.10, na.rm = TRUE)
  cat("  Degenerate (top donor > 80%):", n_degenerate, "of", nrow(shoot_summary), "\n")
  cat("  Significant (Fisher p < .10):", n_sig, "of", nrow(shoot_summary), "\n\n")
}

cat("Results saved to:", results_dir, "\n")
cat("Plots saved to:", plot_dir, "\n\n")
