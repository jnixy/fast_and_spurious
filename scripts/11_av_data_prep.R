# 11_av_data_prep.R — AmericanViolence.org Data Prep for Expanded National DiD
#
# Source: Sharkey, Patrick. "AmericanViolence.org: City, fatal and nonfatal
#   shootings. Princeton, NJ: www.americanviolence.org"
#
# Loads all 10 annual AV files (2016–2025), sums fatal + nonfatal shootings
# per city-month, flags quality issues, and saves clean panels for script 12.
#
# Output:
#   output/national_did/av_combined_panel.csv   — monthly, all 100 cities
#   output/national_did/av_combined_annual.csv  — annual aggregates
#   output/national_did/av_quality_flags.csv    — cities/years with suspicious zeros

# NOTE (2026-03-05): This script is no longer part of the main publication
# pipeline. The AV-based national DiD/SCM analyses have been removed from the
# manuscript. This script is retained for reproducibility.
# Active pipeline: 01 → 03 → 06 → 07 → 08 → 14 → 17 → 15 → 16 → 00_regenerate

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})

cat("\n=== AV DATA PREP ===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")

results_dir <- here("output", "national_did")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# 1. Load all 10 AV files =====================================================

cat("--- Loading AV Files ---\n\n")

av_dir <- here("data", "american_violence")
av_files <- list.files(av_dir, pattern = "\\.csv\\.csv$", full.names = TRUE)

if (length(av_files) == 0) {
  stop("No AV files found in ", av_dir,
       "\nExpected files named 'Fatal Shootings for Jan YYYY − Dec YYYY.csv.csv'")
}

cat("Found", length(av_files), "files:\n")
for (f in sort(av_files)) cat("  ", basename(f), "\n")
cat("\n")

# Read and bind all years; suppress column type messages
raw <- map_dfr(sort(av_files), function(f) {
  read_csv(f, show_col_types = FALSE, col_types = cols(
    timespan        = col_character(),
    id              = col_double(),
    year            = col_integer(),
    month           = col_character(),    # "01"–"12" (zero-padded)
    state_abr       = col_character(),
    county_name     = col_character(),
    place_name      = col_character(),
    population_est  = col_double(),
    crime_type      = col_character(),
    crime_count     = col_double(),
    annualized_rate_per_100k = col_double(),
    source_desc     = col_character()
  ))
})

cat("Raw rows loaded:", nrow(raw), "\n")
cat("Years covered:", min(raw$year), "to", max(raw$year), "\n")
cat("Crime types found:", paste(sort(unique(raw$crime_type)), collapse = ", "), "\n\n")


# 2. Filter to fatal + nonfatal shootings =====================================

# Drop "Murder" — only available starting 2020, inconsistent with GVA coverage
av_shootings <- raw %>%
  filter(crime_type %in% c("Fatal Shootings", "Nonfatal Shootings"))

cat("Rows after filtering to Fatal/Nonfatal Shootings:", nrow(av_shootings), "\n")
cat("Unique crime types retained:", paste(unique(av_shootings$crime_type), collapse = ", "), "\n\n")


# 3. Aggregate per city-month (sum fatal + nonfatal) ==========================

av_monthly <- av_shootings %>%
  mutate(month_int = as.integer(month)) %>%
  group_by(place_name, state_abr, id, year, month_int) %>%
  summarise(
    crime_count    = sum(crime_count, na.rm = TRUE),
    population_est = first(population_est),    # same for both rows
    .groups = "drop"
  ) %>%
  rename(month = month_int) %>%
  mutate(
    month_date     = as.Date(paste(year, sprintf("%02d", month), "01", sep = "-")),
    shooting_rate  = (crime_count / population_est) * 100000 * 12   # annualized per 100K
  ) %>%
  arrange(place_name, state_abr, month_date)

cat("Monthly panel (all 100 cities):", nrow(av_monthly), "rows\n")
cat("Cities:", n_distinct(paste(av_monthly$place_name, av_monthly$state_abr)), "\n\n")


# 4. Filter analysis window ===================================================

# Jan 2018 – Sep 2025 (matches ITS/DiD window; right-censored at Sep 2025)
av_monthly <- av_monthly %>%
  filter(month_date >= as.Date("2018-01-01"),
         month_date <= as.Date("2025-09-01"))

cat("After filtering to Jan 2018 – Sep 2025:", nrow(av_monthly), "rows\n")
cat("Date range:", format(min(av_monthly$month_date), "%b %Y"),
    "to", format(max(av_monthly$month_date), "%b %Y"), "\n\n")


# 5. Add treatment indicator ===================================================

# Treatment: NYC (all 5 boroughs aggregated in AV data)
av_monthly <- av_monthly %>%
  mutate(treated = as.integer(place_name == "New York" & state_abr == "NY"))

cat("Treatment indicator:\n")
cat("  Treated units (NYC):", sum(av_monthly$treated == 1), "rows\n")
cat("  Control units:", sum(av_monthly$treated == 0), "rows\n\n")


# 6. Quality screening — flag city-years with all-zero months =================

cat("--- Quality Screening ---\n\n")

# For each city-year, count zero-count months and flag if all months are zero.
# All years are checked (including COVID) — Justin reviews case by case.

# First, get complete city-year combinations (only those with >= 10 months of data
# in the analysis window, to avoid false flags from partial-year windows)
city_year_completeness <- av_monthly %>%
  group_by(place_name, state_abr, year) %>%
  summarise(
    n_months       = n(),
    zero_months    = sum(crime_count == 0),
    total_count    = sum(crime_count),
    .groups        = "drop"
  )

# Flag: all months in that city-year have zero counts (AND the year has >= 10 months)
# 2018 starts in Jan, 2025 ends in Sep — both have full 12 months except if a city
# entered/exited the data mid-year (unlikely for a fixed 100-city panel)
quality_flags <- city_year_completeness %>%
  filter(n_months >= 10, zero_months == n_months) %>%
  mutate(flag_reason = "All months zero in this year") %>%
  select(city = place_name, state = state_abr, year,
         n_months, zero_months, total_count, flag_reason) %>%
  arrange(city, year)

cat("City-years with all-zero months (potential data quality issues):\n\n")

if (nrow(quality_flags) == 0) {
  cat("  None found — no city-years with all-zero months.\n\n")
} else {
  # Summarize by city (how many years flagged)
  city_flag_summary <- quality_flags %>%
    group_by(city, state) %>%
    summarise(
      flagged_years = paste(year, collapse = ", "),
      n_flagged     = n(),
      .groups = "drop"
    ) %>%
    arrange(desc(n_flagged), city)

  cat("Cities with flagged years:\n")
  print(as.data.frame(city_flag_summary), row.names = FALSE)
  cat("\n")

  cat("Full flag detail:\n")
  print(as.data.frame(quality_flags), row.names = FALSE)
  cat("\n")
}

write_csv(quality_flags, file.path(results_dir, "av_quality_flags.csv"))
cat("Quality flags saved to output/national_did/av_quality_flags.csv\n\n")


# 7. Data completeness summary =================================================

cat("--- City Completeness (Jan 2018 – Sep 2025) ---\n\n")

# Expected months: Jan 2018 through Sep 2025 = 7*12 + 9 = 93 months
expected_months <- 93L

completeness <- av_monthly %>%
  group_by(place_name, state_abr) %>%
  summarise(
    n_months    = n(),
    pct_complete = round(n_months / expected_months * 100, 1),
    total_shots  = sum(crime_count),
    mean_monthly = round(mean(crime_count), 1),
    .groups     = "drop"
  ) %>%
  arrange(pct_complete, place_name)

cat("Cities with < 90 months (potential coverage gaps):\n")
incomplete <- completeness %>% filter(n_months < 90)
if (nrow(incomplete) == 0) {
  cat("  None — all cities have >= 90 months.\n\n")
} else {
  print(as.data.frame(incomplete), row.names = FALSE)
  cat("\n")
}

cat("All cities (n =", nrow(completeness), "):\n")
print(as.data.frame(completeness %>% arrange(place_name)), row.names = FALSE)
cat("\n")


# 8. Save panels ===============================================================

cat("--- Saving Output ---\n\n")

# Monthly panel (all 100 cities, before quality exclusions)
write_csv(av_monthly, file.path(results_dir, "av_combined_panel.csv"))
cat("Monthly panel saved:", nrow(av_monthly), "rows\n")
cat("  File: output/national_did/av_combined_panel.csv\n\n")

# Annual aggregates
av_annual <- av_monthly %>%
  group_by(place_name, state_abr, id, year, treated) %>%
  summarise(
    crime_count    = sum(crime_count),
    population_est = mean(population_est),
    n_months       = n(),
    .groups        = "drop"
  ) %>%
  mutate(
    shooting_rate = (crime_count / population_est) * 100000  # annual rate per 100K
  )

write_csv(av_annual, file.path(results_dir, "av_combined_annual.csv"))
cat("Annual panel saved:", nrow(av_annual), "rows\n")
cat("  File: output/national_did/av_combined_annual.csv\n\n")


# 9. NYC sanity check ==========================================================

cat("--- NYC Sanity Check ---\n\n")

nyc_check <- av_monthly %>%
  filter(place_name == "New York", state_abr == "NY") %>%
  mutate(year_label = year) %>%
  group_by(year_label) %>%
  summarise(
    annual_shots = sum(crime_count),
    annual_rate  = round(mean(shooting_rate), 1),
    n_months     = n(),
    .groups = "drop"
  )

cat("NYC annual shooting totals (fatal + nonfatal, AV data):\n")
print(as.data.frame(nyc_check), row.names = FALSE)
cat("\nNote: Compare against NYPD administrative data for order-of-magnitude check.\n")
cat("      AV/GVA counts may differ from NYPD incident data — document in methods.\n\n")


# 10. Summary ==================================================================

cat("=== SUMMARY ===\n\n")
cat("Cities in panel:          ", n_distinct(paste(av_monthly$place_name, av_monthly$state_abr)), "\n")
cat("Analysis window:           Jan 2018 – Sep 2025\n")
cat("Monthly rows:             ", nrow(av_monthly), "\n")
cat("Annual rows:              ", nrow(av_annual), "\n")
cat("Treated unit (NYC rows):  ", sum(av_monthly$treated), "\n")
cat("Quality flags (city-yrs):", nrow(quality_flags), "\n\n")
cat("NEXT STEPS:\n")
cat("  1. Review output/national_did/av_quality_flags.csv\n")
cat("  2. Decide which cities/years to exclude in script 12\n")
cat("  3. Update the EXCLUDED_CITIES vector at the top of scripts/12_av_did.R\n")
cat("  4. Run scripts/12_av_did.R\n\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n")
