# ==============================================================================
# Vehicle Pursuits Study: Empirical Crash Severity (Injury/Fatality)
# ==============================================================================
#
# Author:   Hall & Nix
#
# STATUS (2026-07-28): Exploratory only — NOT wired into 00_run_all.R and NOT
# used by 07_cba.R. The empirical fatality rate this script computes came back
# as 0% (0 of 1617 pursuit collisions), and cross-validation revealed why: at
# least one media-confirmed pursuit-caused fatality (Kelvin Mitchell, Webster
# Ave/E168 St, Bronx, 2025-05-10 — see Streetsblog, "Police Chase Linked to
# Fatal Hit-and-Run in Bronx") is coded in NYC Open Data with an ordinary
# contributing factor ("Unsafe Speed"), not `pre_crash == "Police Pursuit"`.
# This indicates the pursuit-collision universe likely undercounts genuine
# pursuit-caused fatalities — a data-completeness limitation, not a true zero
# rate — so its output should NOT be used as a CBA input without further
# investigation into the scope of the undercount. Retained here as documented,
# correct exploratory work rather than folded into the manuscript pipeline.
#
# Purpose:
#   Pull actual crash-level injury/fatality records for our own pursuit
#   collision_id universe from NYC Open Data, compute an empirical fatality
#   and injury rate, and cross-validate a list of externally reported
#   "confirmed fatal" pursuit collisions (provenance undocumented) against the
#   pulled records rather than accepting the claim at face value.
#
# Inputs:
#   - data/Motor_Vehicle_Collisions_-_Vehicles_20260628.csv (pursuit collision_id
#     universe — same source as scripts/01_load_clean.R)
#   - NYC Open Data Socrata API: Motor Vehicle Collisions - Crashes (h9gi-nx95)
#
# Outputs (saved to output/cba_results/):
#   - crash_severity_empirical.csv   -- empirical fatality/injury rate summary
#   - crash_severity_by_regime.csv   -- severity broken out by policy regime
#
# Runtime: ~1-2 min on first run (API calls); instant on subsequent runs
#   (cached to data/crashes_severity.rds).
#
# Fallback if data.cityofnewyork.us is unreachable: bulk-download the full
# Crashes table once (https://data.cityofnewyork.us/resource/h9gi-nx95.csv,
# no $where filter) to data/, then filter locally to pursuit_collision_ids
# before running the rest of this script.
# ==============================================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(httr)

# 0. Setup ---------------------------------------------------------------------

results_dir <- here("output", "cba_results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

END_DATE <- as.Date("2025-12-01")

# Open-ended Post-Tisch bucket (no upper breakpoint) so future endpoint
# extensions don't silently fall into an NA regime — matches the convention
# used in scripts/06_event_study.R and scripts/24_collision_elasticity.R.
assign_regime <- function(month_date) {
  case_when(
    month_date < as.Date("2022-10-01") ~ "Pre-escalation",
    month_date < as.Date("2023-08-01") ~ "Escalation",
    month_date < as.Date("2025-02-01") ~ "Post-Maddrey",
    TRUE                                ~ "Post-Tisch"
  )
}

message("Script 27: Empirical Crash Severity")


# 1. Load pursuit collision_id universe -----------------------------------------
# Same source and filter logic as scripts/01_load_clean.R's `collisions` object,
# re-derived independently since each pipeline script is sourced standalone.

collisions_raw <- read_csv(
  here("data", "Motor_Vehicle_Collisions_-_Vehicles_20260628.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(
    collision_id = as.character(collision_id),
    date         = mdy(crash_date),
    month_date   = floor_date(date, "month")
  ) %>%
  filter(!is.na(date), !is.na(collision_id), month_date <= END_DATE)

stopifnot(
  "Collision file appears unfiltered — expected < 5,000 rows for pursuit data" =
    nrow(collisions_raw) < 5000
)

pursuit_collision_ids <- unique(collisions_raw$collision_id)
message("  Pursuit collision_ids (through ", END_DATE, "): ",
        length(pursuit_collision_ids))


# 2. Pull crash-level severity records (Socrata h9gi-nx95) ---------------------

#' Pull crash-level injury/fatality records for a set of collision IDs from the
#' NYC Open Data "Motor Vehicle Collisions - Crashes" dataset (h9gi-nx95).
#' Batches requests to respect Socrata URL length limits, batch_size default
#' chosen to stay well under Socrata's practical URL length ceiling.
#'
#' @param collision_ids Vector of collision IDs to look up.
#' @param batch_size Number of IDs per API request (default 100).
#' @param polite_pause Seconds to wait between requests (default 1).
#' @return Tibble of crash-level records with severity fields.
pull_crash_severity <- function(collision_ids, batch_size = 100, polite_pause = 1) {
  batches <- split(collision_ids, ceiling(seq_along(collision_ids) / batch_size))

  map_dfr(batches, function(batch) {
    id_str <- paste(batch, collapse = ",")
    url <- paste0(
      "https://data.cityofnewyork.us/resource/h9gi-nx95.csv",
      "?$where=collision_id%20in(", id_str, ")",
      "&$select=collision_id,crash_date,number_of_persons_injured,number_of_persons_killed",
      "&$limit=50000"
    )
    Sys.sleep(polite_pause)  # be polite to the API

    resp <- GET(url)
    stop_for_status(resp, task = "pull crash severity batch from NYC Open Data")

    batch_data <- read_csv(I(content(resp, as = "text", encoding = "UTF-8")),
                           show_col_types = FALSE)
    stopifnot(
      "Crashes API response missing expected columns" =
        all(c("collision_id", "crash_date", "number_of_persons_injured",
              "number_of_persons_killed") %in% names(batch_data))
    )
    batch_data
  })
}

cache_path <- here("data", "crashes_severity.rds")

cache_valid <- FALSE
if (file.exists(cache_path)) {
  cached <- readRDS(cache_path)
  if (length(setdiff(pursuit_collision_ids, cached$ids)) == 0) {
    message("  Loading cached crash severity data from ", cache_path)
    crashes_severity <- cached$data
    cache_valid <- TRUE
  } else {
    message("  Cache is stale (new pursuit collision_ids since last pull) — re-pulling")
  }
}

if (!cache_valid) {
  message("  Pulling crash severity data from NYC Open Data API...")
  crashes_severity <- pull_crash_severity(pursuit_collision_ids) %>%
    clean_names() %>%
    mutate(collision_id = as.character(collision_id))
  saveRDS(list(ids = pursuit_collision_ids, data = crashes_severity), cache_path)
}

message("  Crash records returned: ", nrow(crashes_severity))
message("  Unique collision_ids matched: ", n_distinct(crashes_severity$collision_id))

n_unmatched <- length(setdiff(pursuit_collision_ids, crashes_severity$collision_id))
if (n_unmatched > 0) {
  message("  NOTE: ", n_unmatched,
          " pursuit collision_id(s) have no match in the Crashes table")
}


# 3. Compute severity per collision ---------------------------------------------

severity <- crashes_severity %>%
  distinct(collision_id, .keep_all = TRUE) %>%
  mutate(
    # Socrata API returns ISO 8601 datetimes ("2021-09-11T00:00:00.000"),
    # unlike the mdy()-format portal CSV export used elsewhere in the pipeline.
    date       = as_date(crash_date),
    month_date = floor_date(date, "month"),
    any_injury = replace_na(number_of_persons_injured, 0) > 0,
    any_killed = replace_na(number_of_persons_killed, 0) > 0,
    severity   = case_when(
      any_killed ~ "Fatal",
      any_injury ~ "Injury",
      TRUE       ~ "No injury"
    ),
    severity = factor(severity, levels = c("No injury", "Injury", "Fatal")),
    regime = assign_regime(month_date)
  ) %>%
  filter(month_date <= END_DATE)


# 4. Empirical rates -------------------------------------------------------------

n_total  <- nrow(severity)
n_injury <- sum(severity$any_injury)
n_fatal  <- sum(severity$any_killed)

empirical_fatality_rate <- if (n_total > 0) n_fatal / n_total else NA_real_
empirical_injury_rate   <- if (n_total > 0) n_injury / n_total else NA_real_

if (n_total == 0) {
  message("  WARNING: no matched crash records — rates are NA")
}

message("  Empirical fatality rate: ", round(empirical_fatality_rate * 100, 2), "%")
message("  Empirical injury rate:   ", round(empirical_injury_rate * 100, 2), "%")


# 5. Cross-validate externally reported "confirmed fatal" collisions -----------
# These 11 collision_ids were flagged in a co-author-supplied exploratory
# script as "confirmed" pursuit-related fatalities; the provenance of that
# claim is undocumented, so we validate against pulled severity records
# rather than accept it at face value.

reported_fatal_ids <- as.character(c(4605745, 4612732, 4628608, 4642411, 4685068,
                                     4717868, 4723690, 4765626, 4768346, 4803024, 4811637))

validation <- tibble(collision_id = reported_fatal_ids) %>%
  left_join(severity %>% select(collision_id, any_killed), by = "collision_id") %>%
  mutate(validated = replace_na(any_killed, FALSE))

n_confirmed_reported   <- length(reported_fatal_ids)
n_confirmed_validated  <- sum(validation$validated)

message("  Confirmed-fatal cross-validation: ", n_confirmed_validated, " of ",
        n_confirmed_reported, " reported IDs validated as fatal in pulled data")

if (n_confirmed_validated < n_confirmed_reported) {
  message("  NOTE: ", n_confirmed_reported - n_confirmed_validated,
          " reported fatal collision_id(s) did not validate (missing from pull ",
          "or not flagged any_killed)")
}


# 6. Export ------------------------------------------------------------------------

crash_severity_empirical <- tibble(
  n_pursuit_collisions_total   = length(pursuit_collision_ids),
  n_matched_to_crashes_table   = n_total,
  n_unmatched                 = n_unmatched,
  n_injury_crashes             = n_injury,
  n_fatal_crashes               = n_fatal,
  empirical_injury_rate        = empirical_injury_rate,
  empirical_fatality_rate      = empirical_fatality_rate,
  n_reported_confirmed_fatal   = n_confirmed_reported,
  n_confirmed_fatal_validated  = n_confirmed_validated
)

write_csv(crash_severity_empirical, file.path(results_dir, "crash_severity_empirical.csv"))

crash_severity_by_regime <- severity %>%
  group_by(regime) %>%
  summarise(
    n_crashes     = n(),
    n_injury      = sum(any_injury),
    n_fatal       = sum(any_killed),
    injury_rate   = if_else(n_crashes > 0, n_injury / n_crashes, NA_real_),
    fatality_rate = if_else(n_crashes > 0, n_fatal / n_crashes, NA_real_),
    .groups = "drop"
  )

write_csv(crash_severity_by_regime, file.path(results_dir, "crash_severity_by_regime.csv"))

message("Script 27 complete. Outputs saved to: ", results_dir)
