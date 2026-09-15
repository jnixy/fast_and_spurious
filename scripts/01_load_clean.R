# ==============================================================================
# Vehicle Pursuits Study: Data Load and Cleaning
# ==============================================================================
#
# Author:   John Hall & Justin Nix
#
# Purpose:
#   Load all raw NYC Open Data sources, deduplicate where needed, and
#   produce: (1) the monthly analytic panel for ITS and event-study analyses,
#   (2) precinct × month panels for the TWFE, staggered, and DDD analyses,
#   and (3) borough-level monthly pursuit counts for national DiD scripts.
#
# Inputs:
#   - data/NYPD_Calls_for_Service_(Historic)_20260628.csv
#   - data/NYPD_Calls_for_Service_(Year_to_Date)_20260628.csv
#   - data/Motor_Vehicle_Collisions_-_Vehicles_20260628.csv
#   - data/Motor_Vehicle_Collisions_-_Crashes_20260303.csv
#   - data/NYPD_Complaint_Data_Historic_20260628.csv
#   - data/NYPD_Complaint_Data_Current_(Year_To_Date)_20260628.csv
#   - data/Shootings_(2006-Present)_20260628.csv
#   - data/sf_since_2017.csv / data/shots_fired_new.csv
#   - data/Police_Precincts_20260303.geojson
#   - data/Index_Crimes_by_County_and_Agency__Beginning_1990_20260214.csv
#
# Outputs (saved to output/):
#   - output/tables/monthly_panel.csv          -- monthly analytic panel (ITS)
#   - output/national_did/boro_monthly_pursuit.csv
#   - output/precinct_did/pct_monthly_pursuit.csv
#   - output/precinct_did/pct_monthly_shootings.csv
#   - output/precinct_did/pct_monthly_robberies.csv
#   - output/precinct_did/pct_monthly_collisions.csv
#   - output/precinct_did/pct_monthly_gun_violence.csv
#
# Runtime: ~3–5 min (complaint file is 683 MB)
# ==============================================================================
#
# Why robberies and shootings?
# https://ny1.com/nyc/all-boroughs/mornings-on-1/2023/07/10/nypd-chief-attributes-more-car-chases-to-rise-in--ghost-vehicles-
# "They cause traffic violence, they're uninsured, they're unregistered,
#  they cause street violence - robberies and shootings" — Chell

library(tidyverse)
library(lubridate)
library(janitor)
library(here)
library(sf)          # for precinct spatial join (Section 8)

message("\nNYPD PURSUIT PROJECT — DATA LOAD\n")
message("Project root:", here(), "\n")
message("Timestamp:", format(Sys.time(), "%Y-%m-%d %H:%M"), "\n\n")


# Pursuit calls for service ----------------------------------------------------

message("Loading pursuit CFS...\n")

calls_h <- read_csv(
  here("data", "NYPD_Calls_for_Service_(Historic)_20260628.csv"),
  show_col_types = FALSE
) %>% clean_names() %>% mutate(incident_time = as.character(incident_time))

calls_y <- read_csv(
  here("data", "NYPD_Calls_for_Service_(Year_to_Date)_20260628.csv"),
  show_col_types = FALSE
) %>% clean_names() %>% mutate(incident_time = as.character(incident_time))

pursuits_raw <- bind_rows(calls_h, calls_y) %>%
  mutate(
    date       = mdy(incident_date),
    year       = year(date),
    month      = month(date),
    month_date = floor_date(date, "month"),
    week_date  = floor_date(date, "week"),
    dow        = wday(date, label = TRUE),
    hour       = hour(hms(incident_time)),
    boro       = boro_nm,
    pct        = nypd_pct_cd
  ) %>%
  filter(!is.na(date))

message("  Raw rows (pre-dedup):", format(nrow(pursuits_raw), big.mark = ","), "\n")

# Deduplicate by CAD event ID — each CFS broadcast is one pursuit event,
# but the same event can generate multiple administrative records across
# radio units or updates within the same dispatch. Counting raw rows
# would inflate pursuit_events; we count distinct cad_evnt_id instead.
#
# NA guard: distinct(cad_evnt_id) treats NA == NA as TRUE, collapsing all
# NA-keyed rows to a single row. To avoid silently dropping valid records
# that lack a CAD ID, we deduplicate keyed and unkeyed records separately.
pursuits <- bind_rows(
  pursuits_raw %>% filter(!is.na(cad_evnt_id)) %>% distinct(cad_evnt_id, .keep_all = TRUE),
  pursuits_raw %>% filter(is.na(cad_evnt_id))   # retain all NA-keyed records as individual events
)

message("  Unique events (post-dedup):", format(nrow(pursuits), big.mark = ","), "\n")
message("  Duplicate records removed:", format(nrow(pursuits_raw) - nrow(pursuits), big.mark = ","), "\n")
message("  Date range:", as.character(min(pursuits$date)),
    "to", as.character(max(pursuits$date)), "\n")

rm(calls_h, calls_y)


# Pursuit-related collisions ---------------------------------------------------
#
# DATA NOTE: Motor_Vehicle_Collisions_-_Vehicles_20260628.csv was downloaded
# from the NYC Open Data API pre-filtered to pursuit-related events only
# (pre_crash == "Police Pursuit"). The full MVC-Vehicles dataset contains
# millions of rows across all NYC crashes; this file has ~1,800 rows,
# consistent with ~17 pursuit-related crashes per month over 9 years.
#
# Replication: apply the following NYC Open Data SODA filter at download:
#   $where=pre_crash='Police Pursuit'
# or equivalently, from the web portal, filter by "Pre-Crash Action" =
# "Police Pursuit" before exporting.

message("\nLoading pursuit collisions...\n")

collisions <- read_csv(
  here("data", "Motor_Vehicle_Collisions_-_Vehicles_20260628.csv"),
  show_col_types = FALSE
) %>%
  clean_names() %>%
  mutate(
    date       = mdy(crash_date),
    year       = year(date),
    month      = month(date),
    month_date = floor_date(date, "month"),
    week_date  = floor_date(date, "week"),
    hour       = hour(crash_time)
  ) %>%
  filter(!is.na(date))

# Verify the file is the pursuit-filtered version, not the full MVC dataset.
# The full dataset has millions of rows; pursuit-only should be < 5,000.
stopifnot(
  "Collision file appears unfiltered — expected < 5,000 rows for pursuit data" =
    nrow(collisions) < 5000
)

# Collapse to incident level (one row per crash, not per vehicle)
collisions_inc <- collisions %>%
  group_by(collision_id) %>%
  summarise(
    date       = first(date),
    month_date = first(month_date),
    week_date  = first(week_date),
    year       = first(year),
    month      = first(month),
    hour       = first(hour),
    n_vehicles = n(),
    .groups    = "drop"
  )

message("  Vehicle-level rows:", format(nrow(collisions), big.mark = ","), "\n")
message("  Unique incidents:", format(nrow(collisions_inc), big.mark = ","), "\n")
message("  Date range:", as.character(min(collisions$date)),
    "to", as.character(max(collisions$date)), "\n")


# Robberies (NYPD complaint data) ----------------------------------------------

message("\nLoading robbery complaints...\n")

complaints_h <- read_csv(
  here("data", "NYPD_Complaint_Data_Historic_20260628.csv"),
  show_col_types = FALSE
) %>% clean_names()

complaints_y <- read_csv(
  here("data", "NYPD_Complaint_Data_Current_(Year_To_Date)_20260628.csv"),
  show_col_types = FALSE
) %>% clean_names()

# housing_psa has a type mismatch across files (chr vs dbl), drop it
common_cols <- intersect(names(complaints_h), names(complaints_y))
common_cols <- setdiff(common_cols, "housing_psa")

robberies <- bind_rows(
  complaints_h %>% select(all_of(common_cols)),
  complaints_y %>% select(all_of(common_cols))
) %>%
  filter(ofns_desc == "ROBBERY") %>%
  mutate(
    date       = mdy(cmplnt_fr_dt),
    year       = year(date),
    month      = month(date),
    month_date = floor_date(date, "month"),
    week_date  = floor_date(date, "week"),
    pct        = addr_pct_cd,
    boro       = boro_nm
  ) %>%
  filter(!is.na(date))

message("  Rows:", format(nrow(robberies), big.mark = ","), "\n")
message("  Date range:", as.character(min(robberies$date)),
    "to", as.character(max(robberies$date)), "\n")

rm(complaints_h, complaints_y, common_cols)


# Shootings --------------------------------------------------------------------

message("\nLoading shooting incidents...\n")

# NOTE: NYC Open Data consolidated Historic + YTD into a single incident-level
# file "Shootings_(2006-Present)" as of 2026.
shootings <- read_csv(
  here("data", "Shootings_(2006-Present)_20260628.csv"),
  show_col_types = FALSE
) %>% clean_names() %>%
  mutate(
    date       = mdy(occur_date),
    year       = year(date),
    month      = month(date),
    month_date = floor_date(date, "month"),
    week_date  = floor_date(date, "week"),
    pct        = precinct
  ) %>%
  filter(!is.na(date))

message("  Rows:", format(nrow(shootings), big.mark = ","), "\n")
message("  Date range:", as.character(min(shootings$date)),
    "to", as.character(max(shootings$date)), "\n")


# Shots fired ------------------------------------------------------------------

message("\nLoading shots fired...\n")

sf_old <- read_csv(here("data", "sf_since_2017.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(date = mdy(rpt_dt))

sf_new <- read_csv(here("data", "shots_fired_new.csv"), show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(date = mdy(rec_create_dt))

cutoff <- min(sf_new$date, na.rm = TRUE)

shots_fired <- bind_rows(
  sf_old %>% filter(date < cutoff) %>%
    transmute(date, cmplnt_key, pct = as.integer(pct),
              x = as.numeric(x_coord_cd), y = as.numeric(y_coord_cd)),
  sf_new %>% filter(date >= cutoff) %>%
    transmute(date, cmplnt_key, pct = as.integer(cmplnt_pct_cd),
              x = as.numeric(x_coordinate_code), y = as.numeric(y_coordinate_code))
) %>%
  distinct(cmplnt_key, .keep_all = TRUE) %>%
  mutate(
    year       = year(date),
    month      = month(date),
    month_date = floor_date(date, "month"),
    week_date  = floor_date(date, "week")
  ) %>%
  filter(!is.na(date))

message("  Rows:", format(nrow(shots_fired), big.mark = ","), "\n")
message("  Temporal cutoff:", as.character(cutoff), "\n")
message("  Date range:", as.character(min(shots_fired$date)),
    "to", as.character(max(shots_fired$date)), "\n")

rm(sf_old, sf_new, cutoff)


# Gun violence: unified monthly series -----------------------------------------
# Shooting incidents are victim-level; shots_fired are incident-level.

message("\nBuilding gun-violence monthly series...\n")

shooting_monthly <- shootings %>%
  distinct(incident_key, .keep_all = TRUE) %>%
  count(month_date, name = "shooting_incidents")

sf_monthly <- shots_fired %>%
  count(month_date, name = "shots_fired_incidents")

gun_violence <- full_join(shooting_monthly, sf_monthly, by = "month_date") %>%
  replace_na(list(shooting_incidents = 0, shots_fired_incidents = 0)) %>%
  mutate(
    total_gun_events = shooting_incidents + shots_fired_incidents,
    year  = year(month_date),
    month = month(month_date)
  ) %>%
  arrange(month_date)

message("  Monthly rows:", nrow(gun_violence), "\n")
message("  Coverage:", as.character(min(gun_violence$month_date)),
    "to", as.character(max(gun_violence$month_date)), "\n")

rm(shooting_monthly, sf_monthly)


# GIVE statewide data (retained for reproducibility; not used in active pipeline)
# This data is near-zero for non-NYC jurisdictions after 2018, making it
# unusable for DiD. Load is gated to avoid ~1 s of unnecessary I/O on every
# pipeline run. To inspect: change if (FALSE) to if (TRUE).
if (FALSE) {
  give <- read_csv(here("data", "give_data4.csv"), show_col_types = FALSE) %>%
    clean_names() %>%
    filter(shooting_category == "Shooting Incidents Involving Injury") %>%
    separate(
      month_of_ym_ascending,
      into = c("yy", "mon"), sep = "-", remove = FALSE
    ) %>%
    mutate(
      year       = 2000 + as.integer(yy),
      month      = match(mon, month.abb),
      month_date = as.Date(sprintf("%04d-%02d-01", year, month))
    ) %>%
    select(-yy, -mon) %>%
    distinct()
  message("  GIVE rows: ", nrow(give),
          " | Date range: ", min(give$month_date), " to ", max(give$month_date))
}


# Index crimes by county (NYS DCJS) --------------------------------------------

message("\nLoading index crimes by county...\n")

index_county <- read_csv(
  here("data", "Index_Crimes_by_County_and_Agency__Beginning_1990_20260214.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

message("  Rows:", format(nrow(index_county), big.mark = ","), "\n")
message("  Years:", min(index_county$year, na.rm = TRUE),
    "to", max(index_county$year, na.rm = TRUE), "\n")


# Monthly analytic panel -------------------------------------------------------
# Single dataset of monthly counts for all core outcomes.
# Workhorse for ITS and time-series plots.

message("\nBuilding monthly analytic panel...\n")

pursuit_monthly <- pursuits %>%
  count(month_date, name = "pursuit_events")

collision_monthly <- collisions_inc %>%
  count(month_date, name = "pursuit_crashes")

robbery_monthly <- robberies %>%
  count(month_date, name = "robbery_count")

pursuit_start <- min(pursuits$month_date)
pursuit_end   <- max(pursuits$month_date)

month_spine <- tibble(
  month_date = seq.Date(pursuit_start, pursuit_end, by = "month")
)

monthly_panel <- month_spine %>%
  left_join(pursuit_monthly, by = "month_date") %>%
  left_join(collision_monthly, by = "month_date") %>%
  left_join(robbery_monthly, by = "month_date") %>%
  left_join(
    gun_violence %>% select(month_date, shooting_incidents,
                            shots_fired_incidents, total_gun_events),
    by = "month_date"
  ) %>%
  replace_na(list(
    pursuit_events = 0, pursuit_crashes = 0,
    robbery_count = 0, shooting_incidents = 0,
    shots_fired_incidents = 0, total_gun_events = 0
  )) %>%
  mutate(
    year  = year(month_date),
    month = month(month_date)
  )

message("  Panel rows:", nrow(monthly_panel), "\n")
message("  Window:", as.character(pursuit_start), "to",
    as.character(pursuit_end), "\n")

# Save monthly panel to disk.
# Scripts 06 and 07 read output/tables/monthly_panel.csv directly. Saving here
# (rather than relying on script 02) ensures the file exists even if scripts
# are run out of order (e.g., 01 → 03 → 06 without running 02 first).
dir.create(here("output", "tables"), showWarnings = FALSE, recursive = TRUE)
write_csv(monthly_panel, here("output", "tables", "monthly_panel.csv"))
message("  Saved: output/tables/monthly_panel.csv\n")

# Verify monthly spine has no gaps (max allowable gap is 35 days = one month)
gap_days <- as.integer(diff(monthly_panel$month_date))
if (any(gap_days > 35)) {
  gap_idx <- which(gap_days > 35)
  stop(sprintf(
    "Gap(s) in monthly_panel spine: between %s and %s",
    paste(format(monthly_panel$month_date[gap_idx]),   collapse = ", "),
    paste(format(monthly_panel$month_date[gap_idx + 1]), collapse = ", ")
  ))
}
message("  Spine check: no gaps in monthly sequence ✓\n")

# Borough-level monthly pursuit counts for dose-response analysis (script 14).
# The pursuits object carries the boro variable from boro_nm in the raw CFS data.
# Use the same uppercase name map as script 08 for consistency.
boro_name_map_pursuit <- tribble(
  ~boro_raw,        ~boro,
  "MANHATTAN",      "Manhattan",
  "BROOKLYN",       "Brooklyn",
  "QUEENS",         "Queens",
  "BRONX",          "Bronx",
  "STATEN ISLAND",  "Staten Island"
)

# output/national_did is the canonical home for borough-level outputs (scripts 08–14).
# Creating it here so boro_monthly_pursuit.csv is available even if script 08
# has not yet been run (e.g., when running scripts 01 → 14 directly).
dir.create(here("output", "national_did"), showWarnings = FALSE, recursive = TRUE)

n_pursuits_with_boro <- sum(!is.na(pursuits$boro))

pursuit_boro_monthly <- pursuits %>%
  mutate(boro_upper = toupper(boro)) %>%
  filter(!is.na(boro_upper), boro_upper %in% boro_name_map_pursuit$boro_raw) %>%
  left_join(
    boro_name_map_pursuit %>% rename(boro_clean = boro),
    by = c("boro_upper" = "boro_raw")
  ) %>%
  count(month_date, boro = boro_clean, name = "pursuit_events")

n_pursuits_mapped <- sum(pursuit_boro_monthly$pursuit_events)
message(sprintf(
  "  Borough mapping: %d pursuits with non-NA boro, %d mapped to 5 boroughs (%d unmapped)",
  n_pursuits_with_boro, n_pursuits_mapped, n_pursuits_with_boro - n_pursuits_mapped
))

write_csv(pursuit_boro_monthly, here("output", "national_did", "boro_monthly_pursuit.csv"))
message("  Saved: output/national_did/boro_monthly_pursuit.csv")
message("  Borough-month rows: ", nrow(pursuit_boro_monthly))

rm(pursuits_raw, pursuit_monthly, collision_monthly, robbery_monthly, month_spine,
   boro_name_map_pursuit, pursuit_boro_monthly)


# Precinct-level monthly aggregates -------------------------------------------
# Save precinct × month panels for scripts 15 (TWFE dose-response) and
# 16 (staggered adoption event study). G=77 precincts allows cluster-robust SEs.
# All three raw objects (pursuits, robberies, shootings) already carry `pct`.

message("\nBuilding precinct-level monthly panels...\n")

dir.create(here("output", "precinct_did"), showWarnings = FALSE, recursive = TRUE)
pct_dir <- here("output", "precinct_did")

# Define analysis window (same as ITS: Jan 2018 – Sep 2025)
pct_start <- as.Date("2018-01-01")
pct_end   <- as.Date("2025-12-01")

# Spine: all precinct × month combinations in analysis window
# Filter pct to valid precincts (1–123, excluding NAs and implausible codes)
valid_pcts <- 1:123

pct_month_spine <- expand.grid(
  pct        = valid_pcts,
  month_date = seq.Date(pct_start, pct_end, by = "month"),
  stringsAsFactors = FALSE
) %>%
  as_tibble()

# 1. Precinct monthly pursuits
pct_pursuit <- pursuits %>%
  filter(!is.na(pct), pct %in% valid_pcts) %>%
  filter(month_date >= pct_start, month_date <= pct_end) %>%
  count(pct, month_date, name = "pursuit_events")

pct_monthly_pursuit <- pct_month_spine %>%
  left_join(pct_pursuit, by = c("pct", "month_date")) %>%
  replace_na(list(pursuit_events = 0L)) %>%
  arrange(pct, month_date)

write_csv(pct_monthly_pursuit, file.path(pct_dir, "pct_monthly_pursuit.csv"))
message("  Saved pct_monthly_pursuit.csv:", nrow(pct_monthly_pursuit), "rows\n")

# 2. Precinct monthly shootings (victim-level dedup by incident_key first)
pct_shooting <- shootings %>%
  distinct(incident_key, .keep_all = TRUE) %>%
  filter(!is.na(pct), pct %in% valid_pcts) %>%
  filter(month_date >= pct_start, month_date <= pct_end) %>%
  count(pct, month_date, name = "shooting_incidents")

pct_monthly_shootings <- pct_month_spine %>%
  left_join(pct_shooting, by = c("pct", "month_date")) %>%
  replace_na(list(shooting_incidents = 0L)) %>%
  arrange(pct, month_date)

write_csv(pct_monthly_shootings, file.path(pct_dir, "pct_monthly_shootings.csv"))
message("  Saved pct_monthly_shootings.csv:", nrow(pct_monthly_shootings), "rows\n")

# 3. Precinct monthly robberies
pct_robbery <- robberies %>%
  filter(!is.na(pct), pct %in% valid_pcts) %>%
  filter(month_date >= pct_start, month_date <= pct_end) %>%
  count(pct, month_date, name = "robbery_count")

pct_monthly_robberies <- pct_month_spine %>%
  left_join(pct_robbery, by = c("pct", "month_date")) %>%
  replace_na(list(robbery_count = 0L)) %>%
  arrange(pct, month_date)

write_csv(pct_monthly_robberies, file.path(pct_dir, "pct_monthly_robberies.csv"))
message("  Saved pct_monthly_robberies.csv:", nrow(pct_monthly_robberies), "rows\n")

# 4. Precinct monthly collisions — requires spatial join with lat/lon crash data
# Join the new Crashes CSV (with lat/lon) to the existing pursuit-vehicle file
# on collision_id, then spatially assign each crash to a precinct.
# sf package required; install if not present.
# sf loaded at top of script

crashes_geo_raw <- read_csv(
  here("data", "Motor_Vehicle_Collisions_-_Crashes_20260303.csv"),
  show_col_types = FALSE
) %>%
  clean_names()

message("  Crash geo file loaded:", nrow(crashes_geo_raw), "rows\n")

# Identify pursuit-related crash collision_ids from existing vehicle file
pursuit_collision_ids <- collisions %>%
  pull(collision_id) %>%
  unique()

# Filter crash geo to pursuit-related crashes with valid coordinates
crashes_pursuit_geo <- crashes_geo_raw %>%
  mutate(collision_id = as.character(collision_id)) %>%
  filter(collision_id %in% as.character(pursuit_collision_ids)) %>%
  filter(!is.na(latitude), !is.na(longitude),
         latitude != 0, longitude != 0)

message("  Pursuit crashes with valid lat/lon:", nrow(crashes_pursuit_geo),
    "of", length(pursuit_collision_ids), "total\n")

# Load precinct boundaries
precincts_sf <- st_read(here("data", "Police_Precincts_20260303.geojson"),
                        quiet = TRUE)

# Spatial join: assign each crash point to a precinct polygon
crashes_sf <- st_as_sf(crashes_pursuit_geo,
                       coords = c("longitude", "latitude"),
                       crs = 4326)
crashes_sf <- st_transform(crashes_sf, st_crs(precincts_sf))

crashes_with_pct <- st_join(crashes_sf, precincts_sf[, "precinct"],
                            join = st_intersects) %>%  # st_intersects avoids silent boundary dropout
  st_drop_geometry() %>%
  select(collision_id, pct = precinct) %>%
  mutate(pct = as.integer(pct))  # GeoJSON precinct is character; coerce to int

message("  Crashes assigned to precinct:", nrow(crashes_with_pct), "\n")
message("  Crashes unmatched (outside all precincts):",
    nrow(crashes_pursuit_geo) - nrow(crashes_with_pct[!is.na(crashes_with_pct$pct), ]), "\n")

# Join precinct assignment back to collisions_inc (already incident-level)
collisions_pct <- collisions_inc %>%
  mutate(collision_id = as.character(collision_id)) %>%
  left_join(crashes_with_pct %>% mutate(collision_id = as.character(collision_id)),
            by = "collision_id") %>%
  filter(!is.na(pct), pct %in% valid_pcts)

# Aggregate to precinct × month
pct_collision <- collisions_pct %>%
  filter(month_date >= pct_start, month_date <= pct_end) %>%
  count(pct, month_date, name = "collision_count")

pct_monthly_collisions <- pct_month_spine %>%
  left_join(pct_collision, by = c("pct", "month_date")) %>%
  replace_na(list(collision_count = 0L)) %>%
  arrange(pct, month_date)

write_csv(pct_monthly_collisions, file.path(pct_dir, "pct_monthly_collisions.csv"))
message("  Saved pct_monthly_collisions.csv:", nrow(pct_monthly_collisions), "rows\n")

# 5. Precinct monthly gun violence (shooting_incidents + shots_fired_incidents)
# Uses CFS-derived shots_fired (has pct). ShotSpotter requires spatial join —
# out of scope here. Precinct-level gun_violence may undercount relative to the
# city-level series, which splices ShotSpotter + complaint data.
pct_shots_fired <- shots_fired %>%
  filter(!is.na(pct), pct %in% valid_pcts) %>%
  filter(month_date >= pct_start, month_date <= pct_end) %>%
  count(pct, month_date, name = "shots_fired_incidents")

pct_monthly_gun_violence <- pct_month_spine %>%
  left_join(pct_monthly_shootings %>% select(pct, month_date, shooting_incidents),
            by = c("pct", "month_date")) %>%
  left_join(pct_shots_fired, by = c("pct", "month_date")) %>%
  replace_na(list(shooting_incidents = 0L, shots_fired_incidents = 0L)) %>%
  mutate(gun_violence_count = shooting_incidents + shots_fired_incidents) %>%
  arrange(pct, month_date)

write_csv(pct_monthly_gun_violence, file.path(pct_dir, "pct_monthly_gun_violence.csv"))
message("  Saved pct_monthly_gun_violence.csv:", nrow(pct_monthly_gun_violence), "rows\n")

# Report active precinct counts (precincts with > 0 pursuits in post period)
post_active <- pct_monthly_pursuit %>%
  filter(month_date >= as.Date("2022-10-01"), pursuit_events > 0) %>%
  pull(pct) %>%
  unique() %>%
  length()
message("  Precincts with ≥1 pursuit post-Oct 2022:", post_active, "\n")

rm(crashes_geo_raw, crashes_pursuit_geo, precincts_sf, crashes_sf,
   crashes_with_pct, collisions_pct, pct_pursuit, pct_shooting, pct_robbery,
   pct_collision, pct_shots_fired, pct_month_spine, pct_start, pct_end,
   valid_pcts, post_active, pct_dir)


# Done -------------------------------------------------------------------------

message("\nLoad complete.\n\n")

datasets <- tribble(
  ~object,          ~rows,                        ~description,
  "pursuits",       nrow(pursuits),               "CFS pursuit broadcasts (deduped by cad_evnt_id)",
  "collisions",     nrow(collisions),             "Pursuit collisions (vehicle-level)",
  "collisions_inc", nrow(collisions_inc),         "Pursuit collisions (incident-level)",
  "robberies",      nrow(robberies),              "NYPD robbery complaints",
  "shootings",      nrow(shootings),              "NYPD shooting incidents",
  "shots_fired",    nrow(shots_fired),            "Shots-fired reports",
  "gun_violence",   nrow(gun_violence),           "Monthly gun-violence series",
  "index_county",   nrow(index_county),           "DCJS Index Crimes by County",
  "monthly_panel",  nrow(monthly_panel),          "Monthly analytic panel (all series)"
)

for (i in seq_len(nrow(datasets))) {
  message(sprintf("  %-16s %7s rows  %s",
                  datasets$object[i],
                  format(datasets$rows[i], big.mark = ","),
                  datasets$description[i]))
}
message("Load complete.")

