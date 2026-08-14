# 08_national_data_prep.R — Borough-Level Monthly Panels + National Comparison Data (legacy)
#
# ACTIVE DEPENDENCY (2026-03-05): Borough-level monthly panels required by
# script 14 (14_borough_its.R and 14_borough_dose_response.R) are produced by:
#   - boro_monthly_panel.csv   : THIS script (script 08)
#   - boro_monthly_pursuit.csv : script 01 (01_load_clean.R ~line 395), NOT this script
# Both script 01 AND script 08 must run before script 14.
#
# Builds:
#   1. Borough-level monthly NYPD data (robbery, shootings) — ACTIVE, used by 14
#   2. Annual robbery panel: 5 NYC boroughs + top 50 US cities (Kaplan UCR) — legacy
#   3. Monthly shooting panel: NYC boroughs + 8 comparison cities
#      (agency-level police-reported data) — legacy
#
# Sources 01_load_clean.R for existing NYPD data objects, then adds
# borough-level disaggregation and processes external datasets.
#
# PREREQUISITES (manual downloads):
#   - Kaplan's UCR Return A: data/offenses_known_yearly_1960_2024.rds
#   - Agency shooting data: 8 CSV files in data/ (see section 6)

library(tidyverse)
library(lubridate)
library(janitor)
library(here)

cat("\n=== NATIONAL DATA PREP ===\n")
cat("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")

results_dir <- here("output", "national_did")
if (!dir.exists(results_dir)) dir.create(results_dir, recursive = TRUE)

scm_dir <- here("output", "national_scm")
if (!dir.exists(scm_dir)) dir.create(scm_dir, recursive = TRUE)


# 1. Source base data ==========================================================

cat("--- Sourcing 01_load_clean.R ---\n\n")
source(here("scripts", "01_load_clean.R"), local = TRUE)


# 2. Borough-level monthly panels ==============================================

cat("\n--- Building Borough-Level Monthly Panels ---\n\n")

# Borough population lookup (2020 Census, from top50_cities.csv)
boro_pop <- read_csv(here("data", "top50_cities.csv"), show_col_types = FALSE) %>%
  filter(treated == 1) %>%
  select(city, pop_2020) %>%
  rename(boro = city)

# Standardize borough names for joining
boro_name_map <- tribble(
  ~boro_raw,         ~boro,
  "MANHATTAN",       "Manhattan",
  "BROOKLYN",        "Brooklyn",
  "QUEENS",          "Queens",
  "BRONX",           "Bronx",
  "STATEN ISLAND",   "Staten Island"
)

# A. Borough-level monthly robberies
cat("Borough-level robberies...\n")
robbery_boro_monthly <- robberies %>%
  filter(!is.na(boro), boro %in% boro_name_map$boro_raw) %>%
  left_join(boro_name_map, by = c("boro" = "boro_raw")) %>%
  rename(boro_clean = boro.y) %>%
  count(month_date, boro_clean, name = "robbery_count") %>%
  rename(boro = boro_clean)

cat("  Rows:", nrow(robbery_boro_monthly), "\n")
cat("  Boroughs:", paste(sort(unique(robbery_boro_monthly$boro)), collapse = ", "), "\n")

# B. Borough-level monthly shootings
# The raw shooting data has a 'boro' column from clean_names()
cat("Borough-level shootings...\n")

shooting_boro_monthly <- shootings %>%
  distinct(incident_key, .keep_all = TRUE) %>%
  filter(!is.na(boro), boro %in% boro_name_map$boro_raw) %>%
  left_join(boro_name_map, by = c("boro" = "boro_raw")) %>%
  rename(boro_clean = boro.y) %>%
  count(month_date, boro_clean, name = "shooting_incidents") %>%
  rename(boro = boro_clean)

cat("  Rows:", nrow(shooting_boro_monthly), "\n")

# C. Borough-level monthly pursuit crashes
# pct_monthly_collisions.csv (output of script 01's spatial join) has precinct-level
# crash counts and covers 88% of incidents. Apply pct-to-boro mapping here instead of
# using the Crashes CSV borough field, which is largely blank for NYPD data.
cat("Borough-level pursuit crashes...\n")
crash_boro_monthly <- read_csv(
  here("output", "precinct_did", "pct_monthly_collisions.csv"),
  show_col_types = FALSE
) %>%
  mutate(boro = case_when(
    pct >= 1   & pct <= 39  ~ "Manhattan",
    pct >= 40  & pct <= 59  ~ "Bronx",
    pct >= 60  & pct <= 99  ~ "Brooklyn",
    pct >= 100 & pct <= 119 ~ "Queens",
    pct >= 120 & pct <= 129 ~ "Staten Island",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(boro)) %>%
  group_by(month_date, boro) %>%
  summarise(pursuit_crashes = sum(collision_count, na.rm = TRUE), .groups = "drop")

cat("  Rows:", nrow(crash_boro_monthly), "\n")

# D. Borough-level monthly shots fired
# shots_fired (from script 01) has `pct` but not `boro`. Map via NYPD precinct
# numbering convention: Manhattan=1-39, Bronx=40-59, Brooklyn=60-99,
# Queens=100-119, Staten Island=120-129. This is stable and requires no external join.
cat("Borough-level shots fired...\n")
sf_boro_monthly <- shots_fired %>%
  filter(!is.na(pct)) %>%
  mutate(boro = case_when(
    pct >= 1   & pct <= 39  ~ "Manhattan",
    pct >= 40  & pct <= 59  ~ "Bronx",
    pct >= 60  & pct <= 99  ~ "Brooklyn",
    pct >= 100 & pct <= 119 ~ "Queens",
    pct >= 120 & pct <= 129 ~ "Staten Island",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(boro)) %>%
  count(month_date, boro, name = "shots_fired_incidents")

cat("  Rows:", nrow(sf_boro_monthly), "\n")

# E. Combined borough-level monthly panel
boro_list <- boro_name_map$boro
analysis_start <- as.Date("2018-01-01")
analysis_end   <- as.Date("2025-12-01")

boro_spine <- expand_grid(
  month_date = seq.Date(analysis_start, analysis_end, by = "month"),
  boro = boro_list
)

boro_monthly <- boro_spine %>%
  left_join(robbery_boro_monthly,  by = c("month_date", "boro")) %>%
  left_join(shooting_boro_monthly, by = c("month_date", "boro")) %>%
  left_join(crash_boro_monthly,    by = c("month_date", "boro")) %>%
  left_join(sf_boro_monthly,       by = c("month_date", "boro")) %>%
  replace_na(list(robbery_count = 0, shooting_incidents = 0,
                  pursuit_crashes = 0, shots_fired_incidents = 0)) %>%
  left_join(boro_pop, by = "boro") %>%
  mutate(
    year               = year(month_date),
    month              = month(month_date),
    total_gun_events   = shooting_incidents + shots_fired_incidents,
    robbery_rate       = robbery_count       / pop_2020 * 100000 * 12,
    shooting_rate      = shooting_incidents  / pop_2020 * 100000 * 12,
    crash_rate         = pursuit_crashes     / pop_2020 * 100000 * 12,
    gunviol_rate       = total_gun_events    / pop_2020 * 100000 * 12
  )

cat("\nBorough monthly panel:", nrow(boro_monthly), "rows\n")
cat("Boroughs:", length(unique(boro_monthly$boro)), "\n")
cat("Window:", as.character(analysis_start), "to", as.character(analysis_end), "\n")
cat("Columns:", paste(names(boro_monthly), collapse = ", "), "\n")

# Verify: borough sums ≈ NYC-wide totals
verification <- boro_monthly %>%
  group_by(month_date) %>%
  summarise(
    boro_robberies = sum(robbery_count),
    boro_shootings = sum(shooting_incidents),
    boro_crashes   = sum(pursuit_crashes),
    .groups = "drop"
  ) %>%
  left_join(
    monthly_panel %>% select(month_date, robbery_count, shooting_incidents,
                              pursuit_crashes),
    by = "month_date"
  ) %>%
  filter(month_date >= analysis_start, month_date <= analysis_end)

cat("\nVerification (borough sum vs. NYC-wide):\n")
cat("  Robbery match rate:",
    round(mean(verification$boro_robberies == verification$robbery_count, na.rm = TRUE) * 100, 1),
    "%\n")
cat("  Shooting match rate:",
    round(mean(verification$boro_shootings == verification$shooting_incidents, na.rm = TRUE) * 100, 1),
    "%\n")
# Crash borough sums will NOT equal city-wide totals because pct_monthly_collisions.csv
# covers only ~88% of incidents (those with a valid precinct from the spatial join).
# Use total-volume coverage ratio rather than exact-match rate.
crash_coverage_pct <- round(
  sum(verification$boro_crashes, na.rm = TRUE) /
  sum(verification$pursuit_crashes, na.rm = TRUE) * 100, 1)
cat("  Crash borough coverage (% of city-wide total):", crash_coverage_pct, "%",
    "(expected 80-95%; <75% indicates data loss)\n")
if (crash_coverage_pct < 75) {
  warning("Crash borough coverage below 75% -- verify pct_monthly_collisions.csv is current")
}

write_csv(boro_monthly, here("output", "national_did", "boro_monthly_panel.csv"))


# 3. Annual borough panel (for DiD/SCM matching with Kaplan) ===================

cat("\n--- Annual Borough Panel (DCJS) ---\n\n")

# DCJS data has borough-level annual counts
nyc_counties <- c("Bronx", "Kings", "New York", "Queens", "Richmond")
county_to_boro <- tribble(
  ~county,     ~boro,
  "Bronx",     "Bronx",
  "Kings",     "Brooklyn",
  "New York",  "Manhattan",
  "Queens",    "Queens",
  "Richmond",  "Staten Island"
)

boro_annual <- index_county %>%
  filter(agency == "County Total", county %in% nyc_counties) %>%
  left_join(county_to_boro, by = "county") %>%
  left_join(boro_pop, by = "boro") %>%
  mutate(
    robbery_rate = robbery / pop_2020 * 100000,
    violent_rate = violent_total / pop_2020 * 100000,
    treated = 1
  ) %>%
  select(boro, year, robbery = robbery, robbery_rate, violent_total,
         violent_rate, pop_2020, treated)

cat("Borough annual panel:", nrow(boro_annual), "rows\n")
cat("Years:", min(boro_annual$year), "to", max(boro_annual$year), "\n")
cat("Boroughs:", paste(sort(unique(boro_annual$boro)), collapse = ", "), "\n")

write_csv(boro_annual, here("output", "national_did", "boro_annual_panel.csv"))


# 4. Process Kaplan's NIBRS Data ===============================================

cat("\n--- Processing Kaplan's NIBRS Data ---\n\n")

# Look for the data file
ucr_files <- list.files(here("data"), pattern = "ucr_offenses|offenses_known",
                        full.names = TRUE, ignore.case = TRUE)

if (length(ucr_files) == 0) {
  cat("WARNING: Kaplan's UCR Return A file not found in data/\n")
  cat("Expected: data/ucr_offenses_known.rds (or .csv)\n")
  cat("Download from: https://www.openicpsr.org/openicpsr/project/100707\n")
  cat("Skipping UCR processing.\n\n")
  ucr_panel <- NULL
} else {
  ucr_file <- ucr_files[1]
  cat("Found UCR file:", basename(ucr_file), "\n")

  # Load based on file extension
  if (grepl("\\.rds$", ucr_file, ignore.case = TRUE)) {
    ucr_raw <- readRDS(ucr_file)
  } else if (grepl("\\.csv$", ucr_file, ignore.case = TRUE)) {
    ucr_raw <- read_csv(ucr_file, show_col_types = FALSE)
  } else {
    ucr_raw <- readRDS(ucr_file)  # default to RDS
  }

  ucr_raw <- ucr_raw %>% clean_names()
  cat("  UCR rows:", format(nrow(ucr_raw), big.mark = ","), "\n")
  cat("  UCR columns:", ncol(ucr_raw), "\n")
  cat("  Column names (first 20):", paste(head(names(ucr_raw), 20), collapse = ", "), "\n\n")

  # Load city reference table
  cities <- read_csv(here("data", "top50_cities.csv"), show_col_types = FALSE)
  control_cities <- cities %>% filter(treated == 0)

  # Identify the robbery column — Kaplan's files use various naming conventions
  robbery_col <- names(ucr_raw)[grepl("actual.*rob|rob.*actual|robbery_actual|act_robbery",
                                       names(ucr_raw), ignore.case = TRUE)]
  if (length(robbery_col) == 0) {
    # Fallback: prefer *_total columns over partial-crime columns
    robbery_col <- names(ucr_raw)[grepl("actual_robbery_total|robbery_total",
                                        names(ucr_raw), ignore.case = TRUE)]
  }
  if (length(robbery_col) == 0) {
    # Last resort: broad match — warn if multiple candidates
    robbery_col <- names(ucr_raw)[grepl("robbery", names(ucr_raw), ignore.case = TRUE)]
  }
  cat("  Robbery column candidates:", paste(robbery_col, collapse = ", "), "\n")
  if (length(robbery_col) > 1) {
    warning("Multiple robbery column candidates found — using first: ", robbery_col[1],
            "\nAll candidates: ", paste(robbery_col, collapse = ", "))
  }

  # Identify ORI and population columns
  ori_col <- names(ucr_raw)[grepl("^ori$|ori_code|ori7", names(ucr_raw), ignore.case = TRUE)]
  pop_col <- names(ucr_raw)[grepl("^population_1$|^population$", names(ucr_raw), ignore.case = TRUE)]
  year_col <- names(ucr_raw)[grepl("^year$", names(ucr_raw), ignore.case = TRUE)]
  agency_col <- names(ucr_raw)[grepl("agency_name|agency|department",
                                      names(ucr_raw), ignore.case = TRUE)]

  cat("  ORI column:", paste(ori_col, collapse = ", "), "\n")
  cat("  Population column:", paste(head(pop_col, 3), collapse = ", "), "\n")
  cat("  Year column:", paste(year_col, collapse = ", "), "\n")
  cat("  Agency column:", paste(head(agency_col, 3), collapse = ", "), "\n\n")

  # Try to match cities by ORI first, then by name as fallback
  if (length(ori_col) > 0 && length(robbery_col) > 0 && length(year_col) > 0) {

    # Use first matching column for each
    ori_var <- ori_col[1]
    rob_var <- robbery_col[1]
    yr_var  <- year_col[1]
    cat("  → Using robbery column:", rob_var, "\n")
    pop_var <- if (length(pop_col) > 0) pop_col[1] else NULL
    ag_var  <- if (length(agency_col) > 0) agency_col[1] else NULL

    # Match by ORI (top50_cities.csv has correct Kaplan ORI codes)
    ucr_matched <- ucr_raw %>%
      filter(.data[[ori_var]] %in% control_cities$ori)

    cat("  Matched by ORI:", n_distinct(ucr_matched[[ori_var]]), "of",
        n_distinct(control_cities$ori), "agencies\n")

    city_lookup <- control_cities %>%
      select(city, state, ucr_ori = ori, pop_2020)

    ucr_panel <- ucr_matched %>%
      transmute(
        ucr_ori = .data[[ori_var]],
        year    = .data[[yr_var]],
        robbery = as.numeric(.data[[rob_var]]),
        ucr_pop = if (!is.null(pop_var)) as.numeric(.data[[pop_var]]) else NA_real_
      ) %>%
      filter(year >= 2014, year <= 2024) %>%
      left_join(city_lookup, by = "ucr_ori") %>%
      filter(!is.na(city)) %>%
      mutate(
        pop = coalesce(ucr_pop, pop_2020),
        robbery_rate = robbery / pop * 100000,
        treated = 0,
        boro = city
      ) %>%
      select(boro, year, robbery, robbery_rate, pop_2020, treated)

    # Exclude cities with any zero-robbery year — implausible for major US cities
    # and typically reflects non-reporting during the UCR-to-NIBRS transition
    complete_cities <- ucr_panel %>%
      group_by(boro) %>%
      summarise(min_robbery = min(robbery), .groups = "drop") %>%
      filter(min_robbery > 0) %>%
      pull(boro)

    excluded_cities <- sort(setdiff(unique(ucr_panel$boro), complete_cities))
    if (length(excluded_cities) > 0) {
      cat("\nExcluding", length(excluded_cities),
          "cities with implausible zero-robbery years (likely NIBRS non-reporting):\n")
      cat("  ", paste(excluded_cities, collapse = ", "), "\n")
    }
    ucr_panel <- ucr_panel %>% filter(boro %in% complete_cities)
    cat("Retained:", n_distinct(ucr_panel$boro), "cities with complete coverage\n")

    cat("\nNIBRS panel:", nrow(ucr_panel), "rows\n")
    cat("Cities:", n_distinct(ucr_panel$boro), "\n")
    cat("Years:", min(ucr_panel$year), "to", max(ucr_panel$year), "\n")

    # Check for missing years (especially 2021)
    yr_coverage <- ucr_panel %>%
      group_by(year) %>%
      summarise(n_cities = n_distinct(boro), .groups = "drop")
    cat("\nYear coverage:\n")
    print(yr_coverage)

    write_csv(ucr_panel, here("output", "national_did", "ucr_panel.csv"))

  } else {
    cat("ERROR: Could not identify required columns in UCR data.\n")
    cat("Please check column names and update matching logic.\n")
    ucr_panel <- NULL
  }
}


# 5. Combine annual panel (boroughs + UCR cities) ==============================

cat("\n--- Combined Annual Panel ---\n\n")

if (!is.null(ucr_panel)) {
  combined_annual <- bind_rows(
    boro_annual %>% select(boro, year, robbery, robbery_rate, pop_2020, treated),
    ucr_panel   %>% select(boro, year, robbery, robbery_rate, pop_2020, treated)
  ) %>%
    filter(year >= 2014, year <= 2024)

  cat("Combined annual panel:", nrow(combined_annual), "rows\n")
  cat("Units:", n_distinct(combined_annual$boro), "\n")
  cat("  Treated (boroughs):", n_distinct(combined_annual$boro[combined_annual$treated == 1]), "\n")
  cat("  Control (cities):", n_distinct(combined_annual$boro[combined_annual$treated == 0]), "\n")
  cat("Years:", min(combined_annual$year), "to", max(combined_annual$year), "\n")

  # Check for balance
  balance <- combined_annual %>%
    group_by(boro) %>%
    summarise(n_years = n_distinct(year), .groups = "drop")
  cat("\nBalance: min years =", min(balance$n_years),
      ", max years =", max(balance$n_years), "\n")

  # Flag units with missing years
  incomplete <- balance %>% filter(n_years < max(n_years))
  if (nrow(incomplete) > 0) {
    cat("Units with incomplete years:\n")
    print(incomplete)
  }

  write_csv(combined_annual, here("output", "national_did", "combined_annual_panel.csv"))
} else {
  cat("Skipping combined panel — UCR data not available.\n")
  combined_annual <- NULL
}


# 6. Process agency-level shooting data =========================================
#
# Eight comparison cities with police-reported shooting data:
#   - Baltimore (all-crime file, filter for shootings by Description)
#   - Boston (victim-level, deduplicate by incident_num)
#   - Chicago (victim-level, deduplicate by CASE_NUMBER)
#   - Cincinnati (incident-level with ShootID)
#   - Philadelphia (victim-level, deduplicate by dc_key — read as character!)
#   - Buffalo, Rochester, Syracuse (GIVE monthly aggregates)

cat("\n--- Processing Agency-Level Shooting Data ---\n\n")

# Population lookup for shooting comparison cities (2020 Census)
shooting_city_pop <- tribble(
  ~city,           ~pop_2020,  ~state,
  "Baltimore",      585708,    "MD",
  "Boston",         675647,    "MA",
  "Buffalo",        278349,    "NY",
  "Chicago",       2746388,    "IL",
  "Cincinnati",     309317,    "OH",
  "Philadelphia",  1603797,    "PA",
  "Rochester",      211328,    "NY",
  "Syracuse",       148620,    "NY"
)

shooting_panels <- list()

# --- A. Baltimore (all-crime data, filter for shootings) ---
cat("Processing Baltimore...\n")
balt_file <- here("data", "baltimore_shootings.csv")
if (file.exists(balt_file)) {
  balt <- read_csv(balt_file, show_col_types = FALSE) %>% clean_names()
  balt_shootings <- balt %>%
    filter(str_detect(description, fixed("SHOOTING", ignore_case = TRUE))) %>%
    mutate(
      date = mdy_hms(crime_date_time),
      month_date = floor_date(date, "month")
    ) %>%
    filter(!is.na(date)) %>%
    # Each row appears to be an incident (Total_Incidents = 1)
    # Use CCNumber as incident key for deduplication
    distinct(cc_number, .keep_all = TRUE) %>%
    count(month_date, name = "shooting_incidents") %>%
    mutate(city = "Baltimore")

  cat("  Shooting incidents:", sum(balt_shootings$shooting_incidents), "\n")
  cat("  Date range:", as.character(min(balt_shootings$month_date)),
      "to", as.character(max(balt_shootings$month_date)), "\n")
  shooting_panels[["Baltimore"]] <- balt_shootings
  rm(balt)
} else {
  cat("  File not found:", balt_file, "\n")
}


# --- B. Boston (victim-level, deduplicate by incident_num) ---
cat("Processing Boston...\n")
bos_file <- here("data", "boston_shootings.csv")
if (file.exists(bos_file)) {
  bos <- read_csv(bos_file, show_col_types = FALSE) %>% clean_names()
  bos_shootings <- bos %>%
    mutate(
      date = ymd_hms(shooting_date),
      month_date = floor_date(date, "month")
    ) %>%
    filter(!is.na(date)) %>%
    distinct(incident_num, .keep_all = TRUE) %>%
    count(month_date, name = "shooting_incidents") %>%
    mutate(city = "Boston")

  cat("  Shooting incidents:", sum(bos_shootings$shooting_incidents), "\n")
  cat("  Date range:", as.character(min(bos_shootings$month_date)),
      "to", as.character(max(bos_shootings$month_date)), "\n")
  shooting_panels[["Boston"]] <- bos_shootings
  rm(bos)
} else {
  cat("  File not found:", bos_file, "\n")
}


# --- C. Chicago (victim-level, deduplicate by CASE_NUMBER) ---
cat("Processing Chicago...\n")
chi_file <- here("data", "chicago_shooting.csv")
if (file.exists(chi_file)) {
  chi <- read_csv(chi_file, show_col_types = FALSE) %>% clean_names()
  chi_shootings <- chi %>%
    mutate(
      date = mdy_hm(date),
      month_date = floor_date(date, "month")
    ) %>%
    filter(!is.na(date)) %>%
    distinct(case_number, .keep_all = TRUE) %>%
    count(month_date, name = "shooting_incidents") %>%
    mutate(city = "Chicago")

  cat("  Shooting incidents:", sum(chi_shootings$shooting_incidents), "\n")
  cat("  Date range:", as.character(min(chi_shootings$month_date)),
      "to", as.character(max(chi_shootings$month_date)), "\n")
  shooting_panels[["Chicago"]] <- chi_shootings
  rm(chi)
} else {
  cat("  File not found:", chi_file, "\n")
}


# --- D. Cincinnati (incident-level with ShootID) ---
cat("Processing Cincinnati...\n")
cin_file <- here("data", "cincinatti_shooting.csv")
if (file.exists(cin_file)) {
  cin <- read_csv(cin_file, show_col_types = FALSE) %>% clean_names()
  cin_shootings <- cin %>%
    mutate(
      # DateOccurred is YYYYMMDD format
      date = ymd(date_occurred),
      month_date = floor_date(date, "month")
    ) %>%
    filter(!is.na(date)) %>%
    # ShootID may have comma-formatted numbers — use rms_no as backup key
    distinct(shoot_id, .keep_all = TRUE) %>%
    count(month_date, name = "shooting_incidents") %>%
    mutate(city = "Cincinnati")

  cat("  Shooting incidents:", sum(cin_shootings$shooting_incidents), "\n")
  cat("  Date range:", as.character(min(cin_shootings$month_date)),
      "to", as.character(max(cin_shootings$month_date)), "\n")
  shooting_panels[["Cincinnati"]] <- cin_shootings
  rm(cin)
} else {
  cat("  File not found:", cin_file, "\n")
}


# --- E. Philadelphia (victim-level, deduplicate by dc_key) ---
cat("Processing Philadelphia...\n")
phi_file <- here("data", "philly_shootings.csv")
if (file.exists(phi_file)) {
  # Read dc_key as character to preserve full precision (avoids scientific notation)
  phi <- read_csv(phi_file, show_col_types = FALSE,
                  col_types = cols(dc_key = col_character()))
  # Rename before clean_names to avoid losing the trailing underscore
  if ("date_" %in% names(phi)) phi <- phi %>% rename(date_raw = `date_`)
  phi <- phi %>% clean_names()
  # If date_ wasn't found, check for the clean_names version
  if (!"date_raw" %in% names(phi)) {
    date_col_phi <- names(phi)[grepl("^date", names(phi))][1]
    phi <- phi %>% rename(date_raw = all_of(date_col_phi))
  }
  phi_shootings <- phi %>%
    mutate(
      date = mdy(date_raw),
      month_date = floor_date(date, "month")
    ) %>%
    filter(!is.na(date)) %>%
    # Deduplicate: same dc_key + date = same incident with multiple victims
    distinct(dc_key, date, .keep_all = TRUE) %>%
    count(month_date, name = "shooting_incidents") %>%
    mutate(city = "Philadelphia")

  cat("  Total rows:", nrow(phi), "\n")
  cat("  After dedup:", sum(phi_shootings$shooting_incidents), "incidents\n")
  cat("  Date range:", as.character(min(phi_shootings$month_date)),
      "to", as.character(max(phi_shootings$month_date)), "\n")
  shooting_panels[["Philadelphia"]] <- phi_shootings
  rm(phi)
} else {
  cat("  File not found:", phi_file, "\n")
}


# --- F. GIVE-format cities (Buffalo, Rochester, Syracuse) ---
# These use the same "YY-Mon" date format as give_data4.csv
give_cities <- tribble(
  ~file,                    ~city,
  "buffalo_shootings.csv",  "Buffalo",
  "rochester_shootings.csv","Rochester",
  "syracuse_shootings.csv", "Syracuse"
)

for (i in seq_len(nrow(give_cities))) {
  city_name <- give_cities$city[i]
  city_file <- here("data", give_cities$file[i])
  cat("Processing", city_name, "(GIVE format)...\n")

  if (file.exists(city_file)) {
    give_city <- read_csv(city_file, show_col_types = FALSE) %>% clean_names()
    give_city <- give_city %>%
      filter(shooting_category == "Shooting Incidents Involving Injury") %>%
      separate(
        month_of_ym_ascending,
        into = c("yy", "mon"), sep = "-", remove = FALSE
      ) %>%
      mutate(
        year       = 2000 + as.integer(yy),
        month_num  = match(mon, month.abb),
        month_date = as.Date(sprintf("%04d-%02d-01", year, month_num))
      ) %>%
      filter(!is.na(month_date)) %>%
      # GIVE data may have duplicate rows — take distinct month counts
      distinct(month_date, .keep_all = TRUE) %>%
      transmute(
        month_date,
        shooting_incidents = as.integer(count),
        city = city_name
      )

    cat("  Rows:", nrow(give_city), "\n")
    cat("  Date range:", as.character(min(give_city$month_date)),
        "to", as.character(max(give_city$month_date)), "\n")
    shooting_panels[[city_name]] <- give_city
  } else {
    cat("  File not found:", city_file, "\n")
  }
}


# --- G. Combine all shooting panels ---
cat("\n--- Combining Shooting Panels ---\n\n")

if (length(shooting_panels) > 0) {
  comparison_shootings <- bind_rows(shooting_panels) %>%
    left_join(shooting_city_pop, by = "city") %>%
    mutate(
      year  = year(month_date),
      month = month(month_date),
      shooting_rate = shooting_incidents / pop_2020 * 100000 * 12,
      treated = 0
    ) %>%
    rename(boro = city)

  cat("Comparison shooting panel:", nrow(comparison_shootings), "rows\n")
  cat("Cities:", n_distinct(comparison_shootings$boro), "\n")
  cat("Cities included:", paste(sort(unique(comparison_shootings$boro)), collapse = ", "), "\n")

  # Coverage summary
  coverage <- comparison_shootings %>%
    group_by(boro) %>%
    summarise(
      first_month = min(month_date),
      last_month  = max(month_date),
      n_months    = n(),
      total_shootings = sum(shooting_incidents),
      .groups = "drop"
    )
  cat("\nCoverage by city:\n")
  print(coverage, n = Inf)

  write_csv(comparison_shootings, here("output", "national_did", "comparison_shootings_monthly.csv"))
} else {
  cat("WARNING: No shooting data files were processed.\n")
  comparison_shootings <- NULL
}


# 7. Combined shooting panel (boroughs + comparison cities) ====================

cat("\n--- Combined Shooting Panel ---\n\n")

# Borough-level monthly shootings (treated units)
boro_shooting_monthly <- boro_monthly %>%
  select(month_date, boro, shooting_incidents, pop_2020, year, month) %>%
  mutate(
    shooting_rate = shooting_incidents / pop_2020 * 100000 * 12,
    treated = 1
  )

if (!is.null(comparison_shootings)) {
  combined_shooting_monthly <- bind_rows(
    boro_shooting_monthly %>% select(month_date, boro, shooting_incidents,
                                      shooting_rate, pop_2020, year, month, treated),
    comparison_shootings %>% select(month_date, boro, shooting_incidents,
                                     shooting_rate, pop_2020, year, month, treated)
  )

  cat("Combined monthly shooting panel:", nrow(combined_shooting_monthly), "rows\n")
  cat("Units:", n_distinct(combined_shooting_monthly$boro), "\n")
  cat("  Treated (boroughs):", n_distinct(combined_shooting_monthly$boro[combined_shooting_monthly$treated == 1]), "\n")
  cat("  Control (cities):", n_distinct(combined_shooting_monthly$boro[combined_shooting_monthly$treated == 0]), "\n")

  write_csv(combined_shooting_monthly,
            here("output", "national_did", "combined_shooting_monthly.csv"))

  # Also create annual version for SCM
  combined_shooting_annual <- combined_shooting_monthly %>%
    group_by(boro, year, treated, pop_2020) %>%
    summarise(
      shooting_incidents = sum(shooting_incidents),
      n_months = n(),
      .groups = "drop"
    ) %>%
    mutate(shooting_rate = shooting_incidents / pop_2020 * 100000)

  write_csv(combined_shooting_annual,
            here("output", "national_did", "combined_shooting_annual.csv"))

  cat("\nAnnual shooting panel:", nrow(combined_shooting_annual), "rows\n")
} else {
  cat("Skipping combined shooting panel — no comparison data.\n")
  combined_shooting_monthly <- NULL
  combined_shooting_annual  <- NULL
}


# 8. Summary ===================================================================

cat("\n=== DATA PREP SUMMARY ===\n\n")

cat("Panels created:\n")
cat("  1. boro_monthly_panel.csv         — Borough-level monthly (crashes, shooting, gun violence, robbery)\n")
cat("  2. boro_annual_panel.csv          — Borough-level annual (DCJS robbery)\n")
if (!is.null(ucr_panel)) {
  cat("  3. ucr_panel.csv                  — NIBRS city-level annual robbery (complete coverage only)\n")
  cat("  4. combined_annual_panel.csv      — Boroughs + NIBRS cities (DiD/SCM ready)\n")
}
if (!is.null(comparison_shootings)) {
  cat("  5. comparison_shootings_monthly   — 8 cities, police-reported shootings\n")
  cat("  6. combined_shooting_monthly.csv  — Boroughs + 8 cities (monthly)\n")
  cat("  7. combined_shooting_annual.csv   — Boroughs + 8 cities (annual)\n")
}
cat("\nOutput directory:", results_dir, "\n")

cat("\n--- METHODOLOGICAL NOTE ---\n")
cat("All shooting data is police-reported (agency-level administrative records).\n")
cat("NYC boroughs use NYPD shooting incident data.\n")
cat("Comparison cities use their respective agency open data portals.\n")
cat("GIVE-format cities (Buffalo, Rochester, Syracuse) report monthly aggregates.\n")
cat("This is a significant improvement over news-sourced data (e.g., GVA).\n\n")
