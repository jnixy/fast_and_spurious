# ==============================================================================
# Vehicle Pursuit Policy Analysis — Script 19: Permutation Inference (gsynth)
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Placebo-in-space permutation test for the gsynth estimates from Script 18.
#   Each of the 75 SUTVA-clean donor cities is treated as pseudo-treated in turn,
#   and a gsynth model is estimated. NYC's ATT and MSPE ratio are ranked in the
#   resulting distribution to produce nonparametric p-values -- no distributional
#   assumptions required.
#
# Inputs:
#   - output/gsynth_results/gsynth_robustness.rds (saved by Script 18)
#
# Outputs (saved to output/gsynth_results/):
#   - gsynth_perm_results.csv     -- city-level ATT, pre-RMSPE, post-MSPE, ratio
#   - gsynth_perm_summary.csv     -- NYC rank, percentile, two-sided p-values
#   - gsynth_perm_att_dist.png/pdf  -- ATT distribution with NYC marked
#   - gsynth_perm_mspe_dist.png/pdf -- MSPE ratio distribution with NYC marked
#
# Runtime: ~30-45 min (75 gsynth fits, no bootstrap)
# ==============================================================================

# ==============================================================================
# 0. Setup
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(lubridate)
  library(scales)
  library(gsynth)
})

set.seed(20241001)

message("=== Script 19: gsynth Permutation Inference ===")
message("Timestamp: ", format(Sys.time(), "%Y-%m-%d %H:%M"))

TREATED_UNIT      <- "New York"
INTERVENTION_DATE <- as.Date("2022-10-01")

results_dir <- here("output", "gsynth_results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# Project colors (consistent with Script 18 / theme_pursuit)
COL_ACTUAL  <- "#003049"   # dark blue
COL_COUNTER <- "#D62828"   # pursuit red

save_plot <- function(p, name, width = 6.5, height = 4) {
  ggsave(file.path(results_dir, paste0(name, ".png")), p,
         width = width, height = height, dpi = 300)
  ggsave(file.path(results_dir, paste0(name, ".pdf")), p,
         width = width, height = height)
  message("Saved: ", name, ".png / .pdf")
}

# ==============================================================================
# 1. Load Saved gsynth Objects
# ==============================================================================

message("--- 1. Loading gsynth fit objects from Script 18 ---")

rds_path <- file.path(results_dir, "gsynth_robustness.rds")
if (!file.exists(rds_path)) {
  stop("gsynth_robustness.rds not found. Run Script 18 first.")
}

saved <- readRDS(rds_path)
fit_clean   <- saved$fit_clean
panel_clean <- saved$panel_clean

n_cities <- n_distinct(panel_clean$unit)
n_donors <- n_cities - 1
message("Loaded SUTVA-clean panel: ", n_cities, " cities (", n_donors, " donors)")

# Date lookup for the treated unit
date_lookup <- panel_clean %>%
  filter(unit == TREATED_UNIT) %>%
  select(time_index, date) %>%
  distinct() %>%
  arrange(time_index)

# ==============================================================================
# 2. Helper: Extract ATT and MSPE from a gsynth Fit
# ==============================================================================

extract_perm_stats <- function(fit, treated_city) {
  city_col <- which(colnames(fit$eff) == treated_city)

  if (length(city_col) == 0) {
    return(list(att_avg = NA_real_, pre_rmspe = NA_real_,
                post_mspe = NA_real_, mspe_ratio = NA_real_))
  }

  att      <- fit$att
  rel_time <- fit$time

  pre_att  <- att[rel_time <= 0]
  post_att <- att[rel_time > 0]

  pre_rmspe <- sqrt(mean(pre_att^2))
  post_mspe <- mean(post_att^2)
  pre_mspe  <- mean(pre_att^2)
  ratio     <- if (pre_mspe > 0) post_mspe / pre_mspe else NA_real_

  list(
    att_avg    = fit$att.avg,
    pre_rmspe  = pre_rmspe,
    post_mspe  = post_mspe,
    mspe_ratio = ratio
  )
}

# ==============================================================================
# 3. NYC Baseline Statistics
# ==============================================================================

message("--- 2. Computing NYC baseline statistics ---")

nyc_stats <- extract_perm_stats(fit_clean, TREATED_UNIT)

message("NYC ATT (avg):    ", round(nyc_stats$att_avg, 3))
message("NYC pre-RMSPE:    ", round(nyc_stats$pre_rmspe, 3))
message("NYC MSPE ratio:   ", round(nyc_stats$mspe_ratio, 3))

# ==============================================================================
# 4. Permutation Loop
# ==============================================================================

message("--- 3. Running permutation inference (", n_cities, " cities) ---")
message("    This will take ~30-45 minutes...")

all_cities <- sort(unique(panel_clean$unit))

perm_results <- tibble(
  city       = character(),
  att_avg    = numeric(),
  pre_rmspe  = numeric(),
  post_mspe  = numeric(),
  mspe_ratio = numeric()
)

t_start <- Sys.time()

for (i in seq_along(all_cities)) {
  cname <- all_cities[i]
  elapsed <- round(difftime(Sys.time(), t_start, units = "mins"), 1)
  message(sprintf("  [%d/%d] %s (%.1f min elapsed)",
                  i, length(all_cities), cname, elapsed))

  tryCatch({
    # Build pseudo-treatment indicator: this city treated at INTERVENTION_DATE
    p_data <- panel_clean %>%
      mutate(treat_perm = as.integer(unit == cname & date >= INTERVENTION_DATE))

    fit_p <- gsynth(
      gva_rate ~ treat_perm,
      data      = p_data,
      index     = c("unit", "time_index"),
      force     = "two-way",
      CV        = TRUE,
      r         = c(0, 5),
      se        = FALSE,
      seed      = 42,
      parallel  = FALSE
    )

    stats <- extract_perm_stats(fit_p, cname)

    perm_results <- add_row(
      perm_results,
      city       = cname,
      att_avg    = stats$att_avg,
      pre_rmspe  = stats$pre_rmspe,
      post_mspe  = stats$post_mspe,
      mspe_ratio = stats$mspe_ratio
    )
  }, error = function(e) {
    message("    WARNING: Failed for ", cname, ": ", conditionMessage(e))
  })
}

elapsed_total <- round(difftime(Sys.time(), t_start, units = "mins"), 1)
message("Permutation loop complete: ", nrow(perm_results), "/",
        length(all_cities), " cities succeeded (", elapsed_total, " min)")

# ==============================================================================
# 5. Compute Rank-Based P-Values
# ==============================================================================

message("--- 4. Computing rank-based p-values ---")

n_total      <- nrow(perm_results)
n_valid_att  <- sum(!is.na(perm_results$att_avg))
n_valid_mspe <- sum(!is.na(perm_results$mspe_ratio))

nyc_row <- perm_results %>% filter(city == TREATED_UNIT)

if (nrow(nyc_row) == 0) {
  stop("NYC not found in permutation results -- check TREATED_UNIT")
}

# Two-sided ATT p-value: fraction of cities with |ATT| >= |NYC ATT|
perm_p_att <- sum(abs(perm_results$att_avg) >= abs(nyc_row$att_avg),
                  na.rm = TRUE) / n_valid_att

# One-sided MSPE ratio p-value: fraction of cities with ratio >= NYC ratio
perm_p_mspe <- sum(perm_results$mspe_ratio >= nyc_row$mspe_ratio,
                   na.rm = TRUE) / n_valid_mspe

# Ranks (1 = most extreme)
att_rank  <- sum(abs(perm_results$att_avg) >= abs(nyc_row$att_avg), na.rm = TRUE)
mspe_rank <- sum(perm_results$mspe_ratio >= nyc_row$mspe_ratio, na.rm = TRUE)

# Percentile (share of distribution NYC exceeds)
att_pctile  <- 1 - perm_p_att
mspe_pctile <- 1 - perm_p_mspe

message(sprintf("ATT  | NYC = %.3f | Rank = %d/%d | two-sided p = %.3f",
                nyc_row$att_avg, att_rank, n_valid_att, perm_p_att))
message(sprintf("MSPE | NYC ratio = %.2f | Rank = %d/%d | p = %.3f",
                nyc_row$mspe_ratio, mspe_rank, n_valid_mspe, perm_p_mspe))

# ==============================================================================
# 6. Save Results
# ==============================================================================

message("--- 5. Saving results ---")

write_csv(perm_results, file.path(results_dir, "gsynth_perm_results.csv"))
message("Saved: gsynth_perm_results.csv")

perm_summary <- tibble(
  statistic  = c("ATT (avg)", "MSPE ratio"),
  nyc_value  = c(nyc_row$att_avg, nyc_row$mspe_ratio),
  rank       = c(att_rank, mspe_rank),
  n_valid    = c(n_valid_att, n_valid_mspe),
  p_value    = c(perm_p_att, perm_p_mspe),
  percentile = c(att_pctile, mspe_pctile),
  p_type     = c("two-sided", "one-sided (upper)")
)

write_csv(perm_summary, file.path(results_dir, "gsynth_perm_summary.csv"))
message("Saved: gsynth_perm_summary.csv")

# ==============================================================================
# 7. Figures
# ==============================================================================

message("--- 6. Generating permutation distribution figures ---")

# --- Figure: ATT Distribution ---

att_label <- sprintf("NYC ATT = %.2f\nRank %d/%d (p = %.3f)",
                     nyc_row$att_avg, att_rank, n_valid_att, perm_p_att)

fig_att <- ggplot(perm_results %>% filter(!is.na(att_avg)), aes(x = att_avg)) +
  geom_histogram(
    bins      = 30,
    fill      = "gray80",
    color     = "gray50",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = nyc_row$att_avg,
    color      = COL_COUNTER,
    linewidth  = 0.9,
    linetype   = "solid"
  ) +
  annotate("text",
           x     = nyc_row$att_avg,
           y     = Inf,
           vjust = 1.5,
           hjust = if (nyc_row$att_avg > median(perm_results$att_avg,
                                                na.rm = TRUE)) 1.1 else -0.1,
           label = att_label,
           size  = 3,
           color = COL_COUNTER) +
  labs(
    x       = "Average ATT (annualized rate per 100,000)",
    y       = "Number of cities",
    caption = paste0("Placebo-in-space permutation: each of ", n_valid_att,
                     " cities treated as pseudo-treated | Two-sided p = ",
                     round(perm_p_att, 3))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.caption     = element_text(size = 8, color = "gray50"),
    panel.grid.minor = element_blank()
  )

save_plot(fig_att, "gsynth_perm_att_dist")

# --- Figure: MSPE Ratio Distribution ---

mspe_label <- sprintf("NYC ratio = %.2f\nRank %d/%d (p = %.3f)",
                      nyc_row$mspe_ratio, mspe_rank, n_valid_mspe, perm_p_mspe)

fig_mspe <- ggplot(perm_results %>% filter(!is.na(mspe_ratio)),
                   aes(x = mspe_ratio)) +
  geom_histogram(
    bins      = 30,
    fill      = "gray80",
    color     = "gray50",
    linewidth = 0.3
  ) +
  geom_vline(
    xintercept = nyc_row$mspe_ratio,
    color      = COL_COUNTER,
    linewidth  = 0.9,
    linetype   = "solid"
  ) +
  annotate("text",
           x     = nyc_row$mspe_ratio,
           y     = Inf,
           vjust = 1.5,
           hjust = if (nyc_row$mspe_ratio > median(perm_results$mspe_ratio,
                                                   na.rm = TRUE)) 1.1 else -0.1,
           label = mspe_label,
           size  = 3,
           color = COL_COUNTER) +
  labs(
    x       = "Post/Pre MSPE Ratio",
    y       = "Number of cities",
    caption = paste0("Placebo-in-space permutation: ", n_valid_mspe,
                     " cities | One-sided p = ", round(perm_p_mspe, 3))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.caption     = element_text(size = 8, color = "gray50"),
    panel.grid.minor = element_blank()
  )

save_plot(fig_mspe, "gsynth_perm_mspe_dist")

# ==============================================================================
# 8. Final Summary
# ==============================================================================

message("\n=== PERMUTATION INFERENCE SUMMARY ===")
message(sprintf("Cities in permutation: %d/%d succeeded",
                n_total, length(all_cities)))
message(sprintf("NYC ATT (avg):     %.3f per 100k", nyc_row$att_avg))
message(sprintf("  Rank: %d/%d | Two-sided p = %.3f",
                att_rank, n_valid_att, perm_p_att))
message(sprintf("NYC MSPE ratio:    %.2f", nyc_row$mspe_ratio))
message(sprintf("  Rank: %d/%d | One-sided p = %.3f",
                mspe_rank, n_valid_mspe, perm_p_mspe))
message("Total runtime: ", elapsed_total, " minutes")
message("Outputs saved to: ", results_dir)
