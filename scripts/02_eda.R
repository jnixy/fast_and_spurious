# 02_eda.R — Exploratory Data Analysis: NYPD Pursuit Escalation

# Saves plots to output/plots/ and tables to output/tables/.
#
# GIVE data = non-NYC GIVE jurisdictions (Rochester, Buffalo, Syracuse, etc.)
#It's the 20 original GIVE
# NYPD shootings match GIVE "Shooting Incidents w/ Injury" definition.
# Within NYC: shootings + shots_fired = broader gun violence measure.
#to-do: fix loess, it's nonsensical in some plots

library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(here)

# Output dirs ------------------------------------------------------------------

dir.create(here("output"),        showWarnings = FALSE)
dir.create(here("output/plots"),  showWarnings = FALSE)
dir.create(here("output/tables"), showWarnings = FALSE)

plot_dir  <- here("output/plots")
table_dir <- here("output/tables")

# Plot theme -------------------------------------------------------------------

theme_pursuit <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = rel(1.15),
                                      margin = margin(b = 8)),
      plot.subtitle    = element_text(color = "grey30", size = rel(0.85),
                                      margin = margin(b = 12)),
      plot.caption     = element_text(color = "grey50", size = rel(0.7),
                                      hjust = 0, margin = margin(t = 10)),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
      axis.title.x     = element_text(margin = margin(t = 8), size = rel(0.9)),
      axis.title.y     = element_text(margin = margin(r = 8), size = rel(0.9)),
      axis.text        = element_text(color = "grey30"),
      legend.position  = "top",
      legend.title     = element_blank(),
      legend.text      = element_text(size = rel(0.85)),
      strip.text       = element_text(face = "bold", size = rel(0.9)),
      plot.margin      = margin(15, 15, 15, 15)
    )
}

# Palette
COL_PURSUIT    <- "#D62828"
COL_COLLISION  <- "#F77F00"
COL_ROBBERY    <- "#003049"
COL_SHOOTING   <- "#6A0572"
COL_SHOTS_FIRED <- "#9B5DE5"
COL_GUN_TOTAL  <- "#2B2D42"
COL_NYC        <- "#D62828"
COL_GIVE       <- "#457B9D"
COL_HIGHLIGHT  <- "#E63946"

save_plot <- function(p, name, w = 10, h = 6) {
  ggsave(file.path(plot_dir, paste0(name, ".png")),
         plot = p, width = w, height = h, dpi = 300, bg = "white")
  ggsave(file.path(plot_dir, paste0(name, ".pdf")),
         plot = p, width = w, height = h, bg = "white")
}


# A. Pursuit time series =======================================================

cat("\n--- A. Pursuit time series ---\n\n")

# A1: Monthly pursuit counts
pursuit_mo <- pursuits %>%
  count(month_date, name = "n") %>%
  arrange(month_date)

cat("Pursuit monthly summary:\n")
print(summary(pursuit_mo$n))
cat("\n")

pursuit_annual <- pursuits %>%
  count(year, name = "n") %>%
  mutate(pct_change = round((n / lag(n) - 1) * 100, 1))

cat("Annual pursuit counts:\n")
print(pursuit_annual, n = Inf)
cat("\n")

pursuit_qtr <- pursuits %>%
  mutate(quarter = quarter(date),
         qtr_label = paste0(year, " Q", quarter)) %>%
  count(year, quarter, qtr_label, name = "n")

cat("Quarterly pursuit counts:\n")
print(pursuit_qtr, n = Inf)
cat("\n")

p_a1 <- ggplot(pursuit_mo, aes(month_date, n)) +
  geom_col(fill = COL_PURSUIT, alpha = 0.75, width = 25) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE,
              color = "grey20", linewidth = 0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "NYPD Pursuit Broadcasts by Month",
    subtitle = "Calls for Service coded as BROADCAST: CHASE/PURSUIT",
    x = NULL, y = "Monthly Events",
    caption  = "Source: NYPD Calls for Service (Historic + YTD)"
  ) +
  theme_pursuit()

print(p_a1)
save_plot(p_a1, "A1_pursuit_monthly")


# A2: Weekly pursuit counts
pursuit_wk <- pursuits %>%
  count(week_date, name = "n") %>%
  arrange(week_date)

p_a2 <- ggplot(pursuit_wk, aes(week_date, n)) +
  geom_line(color = COL_PURSUIT, alpha = 0.5, linewidth = 0.4) +
  geom_smooth(method = "loess", span = 0.15, se = TRUE,
              color = COL_PURSUIT, fill = COL_PURSUIT,
              alpha = 0.15, linewidth = 1) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y",
               expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Weekly Pursuit Broadcasts (Loess Trend)",
    subtitle = "Higher resolution for break-point identification",
    x = NULL, y = "Weekly Events",
    caption  = "Source: NYPD Calls for Service"
  ) +
  theme_pursuit()

print(p_a2)
save_plot(p_a2, "A2_pursuit_weekly")


# A3: By borough
pursuit_boro <- pursuits %>%
  count(month_date, boro, name = "n")

p_a3 <- ggplot(pursuit_boro, aes(month_date, n, fill = boro)) +
  geom_col(alpha = 0.8, width = 25) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Pursuit Broadcasts by Borough",
    subtitle = "Monthly counts, stacked",
    x = NULL, y = "Monthly Events",
    caption  = "Source: NYPD Calls for Service"
  ) +
  theme_pursuit()

print(p_a3)
save_plot(p_a3, "A3_pursuit_by_borough")

p_a3b <- ggplot(pursuit_boro, aes(month_date, n)) +
  geom_col(fill = COL_PURSUIT, alpha = 0.7, width = 25) +
  geom_smooth(method = "loess", span = 0.3, se = FALSE,
              color = "grey20", linewidth = 0.7) +
  facet_wrap(~boro, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Pursuit Broadcasts by Borough (Faceted)",
    x = NULL, y = "Monthly Events",
    caption  = "Source: NYPD Calls for Service"
  ) +
  theme_pursuit()

print(p_a3b)
save_plot(p_a3b, "A3b_pursuit_by_borough_facet", w = 11, h = 8)


# A4: Hour of day / day of week
p_a4a <- pursuits %>%
  filter(!is.na(hour)) %>%
  count(hour) %>%
  ggplot(aes(hour, n)) +
  geom_col(fill = COL_PURSUIT, alpha = 0.8) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(title = "Pursuit Events by Hour of Day",
       x = "Hour (0-23)", y = "Total Events") +
  theme_pursuit()

p_a4b <- pursuits %>%
  count(dow) %>%
  ggplot(aes(dow, n)) +
  geom_col(fill = COL_PURSUIT, alpha = 0.8) +
  labs(title = "Pursuit Events by Day of Week",
       x = NULL, y = "Total Events") +
  theme_pursuit()

p_a4 <- p_a4a + p_a4b +
  plot_annotation(
    title = "Temporal Patterns of NYPD Pursuit Broadcasts",
    caption = "Source: NYPD Calls for Service"
  )

print(p_a4)
save_plot(p_a4, "A4_pursuit_temporal_patterns", w = 12, h = 5.5)


# B. Pursuit collisions ========================================================

cat("\n--- B. Pursuit collisions ---\n\n")

collision_mo <- collisions_inc %>%
  count(month_date, name = "n") %>%
  arrange(month_date)

cat("Collision monthly summary:\n")
print(summary(collision_mo$n))

collision_annual <- collisions_inc %>%
  count(year, name = "n") %>%
  mutate(pct_change = round((n / lag(n) - 1) * 100, 1))

cat("\nAnnual collision incidents:\n")
print(collision_annual, n = Inf)
cat("\n")

p_b1 <- ggplot(collision_mo, aes(month_date, n)) +
  geom_col(fill = COL_COLLISION, alpha = 0.75, width = 25) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE,
              color = "grey20", linewidth = 0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title    = "Pursuit-Related Collision Incidents by Month",
    subtitle = "NYC Motor Vehicle Collisions with pre-crash = Police Pursuit",
    x = NULL, y = "Monthly Incidents",
    caption  = "Source: NYC Motor Vehicle Collisions (Vehicles)"
  ) +
  theme_pursuit()

print(p_b1)
save_plot(p_b1, "B1_collision_monthly")


# B2: Pursuit broadcasts vs. collisions overlay
common_start <- max(min(pursuit_mo$month_date), min(collision_mo$month_date))
common_end   <- min(max(pursuit_mo$month_date), max(collision_mo$month_date))

overlay_data <- full_join(
  pursuit_mo %>% rename(pursuits = n),
  collision_mo %>% rename(collisions = n),
  by = "month_date"
) %>%
  filter(month_date >= common_start, month_date <= common_end) %>%
  replace_na(list(pursuits = 0, collisions = 0))

scale_factor <- max(overlay_data$pursuits, na.rm = TRUE) /
  max(overlay_data$collisions, na.rm = TRUE)

p_b2 <- ggplot(overlay_data, aes(month_date)) +
  geom_col(aes(y = collisions * scale_factor),
           fill = COL_COLLISION, alpha = 0.45, width = 25) +
  geom_line(aes(y = pursuits), color = COL_PURSUIT, linewidth = 1) +
  geom_point(aes(y = pursuits), color = COL_PURSUIT, size = 1.2) +
  scale_y_continuous(
    name = "Pursuit Broadcasts",
    sec.axis = sec_axis(~./scale_factor, name = "Collision Incidents")
  ) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Pursuit Broadcasts and Pursuit-Related Collisions",
    subtitle = "Red line = pursuit CFS events | Orange bars = collision incidents (right axis)",
    x = NULL,
    caption  = "Sources: NYPD CFS; NYC MV Collisions"
  ) +
  theme_pursuit() +
  theme(
    axis.title.y.left  = element_text(color = COL_PURSUIT),
    axis.title.y.right = element_text(color = COL_COLLISION)
  )

print(p_b2)
save_plot(p_b2, "B2_pursuit_vs_collision_overlay")


# B3: Vehicle characteristics
p_b3a <- collisions %>%
  filter(!is.na(vehicle_type), vehicle_type != "UNKNOWN") %>%
  count(vehicle_type, sort = TRUE) %>%
  slice_head(n = 12) %>%
  mutate(vehicle_type = fct_reorder(vehicle_type, n)) %>%
  ggplot(aes(n, vehicle_type)) +
  geom_col(fill = COL_COLLISION, alpha = 0.8) +
  labs(title = "Top Vehicle Types in Pursuit Collisions",
       x = "Count", y = NULL) +
  theme_pursuit()

p_b3b <- collisions %>%
  filter(!is.na(pre_crash)) %>%
  count(pre_crash, sort = TRUE) %>%
  slice_head(n = 10) %>%
  mutate(pre_crash = fct_reorder(pre_crash, n)) %>%
  ggplot(aes(n, pre_crash)) +
  geom_col(fill = COL_COLLISION, alpha = 0.8) +
  labs(title = "Pre-Crash Actions",
       x = "Count", y = NULL) +
  theme_pursuit()

p_b3 <- p_b3a / p_b3b +
  plot_annotation(
    title   = "Pursuit Collision Characteristics",
    caption = "Source: NYC MV Collisions (Vehicles)"
  )

print(p_b3)
save_plot(p_b3, "B3_collision_characteristics", w = 10, h = 9)


# C. Robbery trends =============================================================

cat("\n--- C. Robbery trends ---\n\n")

robbery_mo <- robberies %>%
  count(month_date, name = "n") %>%
  arrange(month_date)

robbery_mo_window <- robbery_mo %>%
  filter(month_date >= as.Date("2016-01-01"))

robbery_annual <- robberies %>%
  filter(year >= 2016) %>%
  count(year, name = "n") %>%
  mutate(pct_change = round((n / lag(n) - 1) * 100, 1))

cat("Annual robberies (2016+):\n")
print(robbery_annual, n = Inf)
cat("\n")

p_c1 <- ggplot(robbery_mo_window, aes(month_date, n)) +
  geom_line(color = COL_ROBBERY, linewidth = 0.6, alpha = 0.6) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE,
              color = COL_ROBBERY, linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "NYC Monthly Robbery Complaints (2016-Present)",
    subtitle = "NYPD Complaint Data, Offense: ROBBERY",
    x = NULL, y = "Monthly Robberies",
    caption  = "Source: NYPD Complaint Data (Historic + YTD)"
  ) +
  theme_pursuit()

print(p_c1)
save_plot(p_c1, "C1_robbery_monthly")

robbery_boro <- robberies %>%
  filter(year >= 2016) %>%
  count(month_date, boro, name = "n") %>%
  filter(!is.na(boro))

p_c2 <- ggplot(robbery_boro, aes(month_date, n)) +
  geom_line(color = COL_ROBBERY, alpha = 0.4, linewidth = 0.4) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE,
              color = COL_ROBBERY, linewidth = 0.8) +
  facet_wrap(~boro, scales = "free_y", ncol = 2) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title = "Monthly Robberies by Borough (2016-Present)",
    x = NULL, y = "Monthly Robberies",
    caption = "Source: NYPD Complaint Data"
  ) +
  theme_pursuit()

print(p_c2)
save_plot(p_c2, "C2_robbery_by_borough", w = 11, h = 8)


# D. Shooting / gun violence ===================================================

cat("\n--- D. Shooting & gun violence ---\n\n")

shooting_mo <- shootings %>%
  distinct(incident_key, .keep_all = TRUE) %>%
  count(month_date, name = "n") %>%
  filter(month_date >= as.Date("2016-01-01"))

shooting_annual <- shootings %>%
  distinct(incident_key, .keep_all = TRUE) %>%
  filter(year >= 2016) %>%
  count(year, name = "n") %>%
  mutate(pct_change = round((n / lag(n) - 1) * 100, 1))

cat("Annual shooting incidents (2016+, deduplicated):\n")
print(shooting_annual, n = Inf)
cat("\n")

p_d1 <- ggplot(shooting_mo, aes(month_date, n)) +
  geom_line(color = COL_SHOOTING, linewidth = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE,
              color = COL_SHOOTING, linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "NYC Monthly Shooting Incidents (2016-Present)",
    subtitle = "NYPD Shooting Incident Data, unique incidents",
    x = NULL, y = "Monthly Incidents",
    caption  = "Source: NYPD Shooting Incident Data (Historic + YTD)"
  ) +
  theme_pursuit()

print(p_d1)
save_plot(p_d1, "D1_shooting_monthly")


# D2: Combined gun-violence series (within-NYC)
gv_window <- gun_violence %>%
  filter(month_date >= as.Date("2016-01-01"))

gv_long <- gv_window %>%
  pivot_longer(cols = c(shooting_incidents, shots_fired_incidents),
               names_to = "type", values_to = "count") %>%
  mutate(type = case_match(type,
                           "shooting_incidents"    ~ "Shooting Incidents",
                           "shots_fired_incidents" ~ "Shots Fired Reports"
  ))

p_d2 <- ggplot(gv_long, aes(month_date, count, color = type)) +
  geom_line(alpha = 0.35, linewidth = 0.4) +
  geom_smooth(method = "loess", span = 0.15, se = FALSE, linewidth = 1) +
  scale_color_manual(values = c(
    "Shooting Incidents"  = COL_SHOOTING,
    "Shots Fired Reports" = COL_SHOTS_FIRED
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Gun Violence Components: Shootings + Shots Fired",
    subtitle = "Within-NYC measure, monthly counts",
    x = NULL, y = "Monthly Events",
    caption  = "Sources: NYPD Shooting Data; NYPD CFS Shots Fired"
  ) +
  theme_pursuit()

print(p_d2)
save_plot(p_d2, "D2_gun_violence_components")


# E. NYC vs. non-NYC GIVE comparison ==========================================

cat("\n--- E. GIVE shooting comparison ---\n\n")

nyc_shooting_mo <- shootings %>%
  distinct(incident_key, .keep_all = TRUE) %>%
  count(month_date, name = "nyc_shootings")

give_comparison <- give %>%
  select(month_date, give_shootings = count) %>%
  left_join(nyc_shooting_mo, by = "month_date") %>%
  replace_na(list(nyc_shootings = 0)) %>%
  filter(month_date >= as.Date("2006-04-01"))

cat("GIVE comparison (full series):\n")
cat("  NYC monthly shootings  -- mean:", round(mean(give_comparison$nyc_shootings), 1), "\n")
cat("  GIVE non-NYC monthly   -- mean:", round(mean(give_comparison$give_shootings), 1), "\n\n")

# E1: Levels
give_long <- give_comparison %>%
  pivot_longer(cols = c(nyc_shootings, give_shootings),
               names_to = "region", values_to = "count") %>%
  mutate(region = case_match(region,
                             "nyc_shootings"  ~ "NYC",
                             "give_shootings" ~ "Non-NYC GIVE Jurisdictions"
  ))

p_e1 <- ggplot(give_long, aes(month_date, count, color = region)) +
  geom_line(alpha = 0.35, linewidth = 0.4) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, linewidth = 1) +
  scale_color_manual(values = c(
    "NYC" = COL_NYC,
    "Non-NYC GIVE Jurisdictions" = COL_GIVE
  )) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "Shooting Incidents: NYC vs. Non-NYC GIVE Jurisdictions",
    subtitle = "Monthly counts, same definition (incidents involving injury)",
    x = NULL, y = "Monthly Shooting Incidents",
    caption  = "Sources: NYPD Shooting Data; NYS DCJS GIVE Data"
  ) +
  theme_pursuit()

print(p_e1)
save_plot(p_e1, "E1_give_nyc_vs_nonnyc")


# E2: Indexed trends for parallel-trends check
# 12-month rolling mean smooths seasonal noise before indexing
give_indexed <- give_comparison %>%
  filter(month_date >= as.Date("2016-01-01")) %>%
  arrange(month_date) %>%
  mutate(
    nyc_roll12  = zoo::rollmean(nyc_shootings, k = 12, fill = NA, align = "right"),
    give_roll12 = zoo::rollmean(give_shootings, k = 12, fill = NA, align = "right")
  ) %>%
  filter(!is.na(nyc_roll12), !is.na(give_roll12))

base_nyc  <- give_indexed$nyc_roll12[1]
base_give <- give_indexed$give_roll12[1]

give_indexed <- give_indexed %>%
  mutate(
    nyc_idx  = nyc_roll12 / base_nyc * 100,
    give_idx = give_roll12 / base_give * 100
  )

give_idx_long <- give_indexed %>%
  select(month_date, nyc_idx, give_idx) %>%
  pivot_longer(-month_date, names_to = "region", values_to = "index") %>%
  mutate(region = case_match(region,
                             "nyc_idx"  ~ "NYC",
                             "give_idx" ~ "Non-NYC GIVE Jurisdictions"
  ))

p_e2 <- ggplot(give_idx_long, aes(month_date, index, color = region)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  scale_color_manual(values = c(
    "NYC" = COL_NYC,
    "Non-NYC GIVE Jurisdictions" = COL_GIVE
  )) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Indexed Shooting Trends: NYC vs. Non-NYC GIVE Jurisdictions",
    subtitle = "12-month rolling mean, indexed to first value = 100 | Parallel-trends assessment",
    x = NULL, y = "Index (baseline = 100)",
    caption  = "Sources: NYPD Shooting Data; NYS DCJS GIVE Data"
  ) +
  theme_pursuit()

print(p_e2)
save_plot(p_e2, "E2_give_indexed_trends")


# E3: Faceted to show scale difference
p_e3 <- ggplot(give_long, aes(month_date, count)) +
  geom_line(color = "grey60", alpha = 0.4, linewidth = 0.4) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE,
              color = COL_NYC, linewidth = 1) +
  facet_wrap(~region, scales = "free_y", ncol = 1) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(
    title    = "Shooting Incidents by Region (Free Y-Axis)",
    subtitle = "NYC vs. non-NYC GIVE jurisdictions, same outcome definition",
    x = NULL, y = "Monthly Shooting Incidents",
    caption  = "Sources: NYPD Shooting Data; NYS DCJS GIVE Data"
  ) +
  theme_pursuit()

print(p_e3)
save_plot(p_e3, "E3_give_faceted", w = 10, h = 8)


# F. Multi-series overlay — the core picture ===================================

cat("\n--- F. Multi-series overlay ---\n\n")

# F1: Z-scored overlay, all four series
panel_z <- monthly_panel %>%
  filter(month_date >= as.Date("2018-01-01")) %>%
  mutate(across(c(pursuit_events, pursuit_crashes, robbery_count,
                  shooting_incidents),
                ~ (. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
                .names = "z_{.col}"))

f1_long <- panel_z %>%
  select(month_date, starts_with("z_")) %>%
  pivot_longer(-month_date, names_to = "series", values_to = "z") %>%
  mutate(series = case_match(series,
                             "z_pursuit_events"     ~ "Pursuit Broadcasts",
                             "z_pursuit_crashes"    ~ "Pursuit Collisions",
                             "z_robbery_count"      ~ "Robberies",
                             "z_shooting_incidents" ~ "Shooting Incidents"
  ))

p_f1 <- ggplot(f1_long, aes(month_date, z, color = series)) +
  geom_line(alpha = 0.35, linewidth = 0.4) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, linewidth = 1.1) +
  scale_color_manual(values = c(
    "Pursuit Broadcasts" = COL_PURSUIT,
    "Pursuit Collisions" = COL_COLLISION,
    "Robberies"          = COL_ROBBERY,
    "Shooting Incidents"  = COL_SHOOTING
  )) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Normalized Trends: Pursuits, Collisions, Robberies, and Shootings",
    subtitle = "Z-scored monthly counts (mean = 0) | Common window",
    x = NULL, y = "Standard Deviations from Mean",
    caption  = "Sources: NYPD CFS; NYC MV Collisions; NYPD Complaint Data; NYPD Shooting Data"
  ) +
  theme_pursuit()

print(p_f1)
save_plot(p_f1, "F1_normalized_overlay", w = 12, h = 6.5)


# F2: Pursuits vs. robberies (dual axis)
pr_rob <- monthly_panel %>%
  filter(month_date >= as.Date("2018-01-01"))

sf2 <- max(pr_rob$robbery_count, na.rm = TRUE) /
  max(pr_rob$pursuit_events, na.rm = TRUE)

p_f2 <- ggplot(pr_rob, aes(month_date)) +
  geom_line(aes(y = robbery_count), color = COL_ROBBERY,
            linewidth = 0.5, alpha = 0.4) +
  geom_smooth(aes(y = robbery_count), method = "loess", span = 0.2,
              se = FALSE, color = COL_ROBBERY, linewidth = 1) +
  geom_line(aes(y = pursuit_events * sf2), color = COL_PURSUIT,
            linewidth = 0.5, alpha = 0.4) +
  geom_smooth(aes(y = pursuit_events * sf2), method = "loess", span = 0.2,
              se = FALSE, color = COL_PURSUIT, linewidth = 1) +
  scale_y_continuous(
    name = "Monthly Robberies",
    labels = comma,
    sec.axis = sec_axis(~./sf2, name = "Monthly Pursuit Events")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Pursuit Broadcasts vs. Robberies",
    subtitle = "Blue = Robberies (left axis) | Red = Pursuits (right axis)",
    x = NULL,
    caption  = "Sources: NYPD CFS; NYPD Complaint Data"
  ) +
  theme_pursuit() +
  theme(
    axis.title.y.left  = element_text(color = COL_ROBBERY),
    axis.title.y.right = element_text(color = COL_PURSUIT)
  )

print(p_f2)
save_plot(p_f2, "F2_pursuit_vs_robbery")


# F3: Pursuits vs. shootings
sf3 <- max(pr_rob$shooting_incidents, na.rm = TRUE) /
  max(pr_rob$pursuit_events, na.rm = TRUE)

p_f3 <- ggplot(pr_rob, aes(month_date)) +
  geom_line(aes(y = shooting_incidents), color = COL_SHOOTING,
            linewidth = 0.5, alpha = 0.4) +
  geom_smooth(aes(y = shooting_incidents), method = "loess", span = 0.2,
              se = FALSE, color = COL_SHOOTING, linewidth = 1) +
  geom_line(aes(y = pursuit_events * sf3), color = COL_PURSUIT,
            linewidth = 0.5, alpha = 0.4) +
  geom_smooth(aes(y = pursuit_events * sf3), method = "loess", span = 0.2,
              se = FALSE, color = COL_PURSUIT, linewidth = 1) +
  scale_y_continuous(
    name = "Monthly Shooting Incidents",
    sec.axis = sec_axis(~./sf3, name = "Monthly Pursuit Events")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title    = "Pursuit Broadcasts vs. Shooting Incidents",
    subtitle = "Purple = Shootings (left axis) | Red = Pursuits (right axis)",
    x = NULL,
    caption  = "Sources: NYPD CFS; NYPD Shooting Data"
  ) +
  theme_pursuit() +
  theme(
    axis.title.y.left  = element_text(color = COL_SHOOTING),
    axis.title.y.right = element_text(color = COL_PURSUIT)
  )

print(p_f3)
save_plot(p_f3, "F3_pursuit_vs_shooting")


# G. County-level data (for SCM / DiD) ========================================

cat("\n--- G. County-level index crimes ---\n\n")

nyc_counties <- c("New York", "Kings", "Queens", "Bronx", "Richmond")

county_annual <- index_county %>%
  filter(months_reported == 12 | (year == max(year))) %>%
  group_by(county, year) %>%
  summarise(across(c(robbery, violent_total, index_total, property_total,
                     murder, aggravated_assault, burglary, larceny,
                     motor_vehicle_theft),
                   sum, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(nyc = ifelse(county %in% nyc_counties, "NYC", "Rest of NYS"))

region_annual <- county_annual %>%
  group_by(nyc, year) %>%
  summarise(across(c(robbery, violent_total, index_total), sum, na.rm = TRUE),
            .groups = "drop")

cat("NYC vs Rest-of-State robbery counts (annual):\n")
print(
  region_annual %>%
    select(nyc, year, robbery) %>%
    pivot_wider(names_from = nyc, values_from = robbery) %>%
    arrange(year) %>%
    tail(15),
  n = Inf
)
cat("\n")

p_g1 <- ggplot(region_annual %>% filter(year >= 2010),
               aes(year, robbery, color = nyc)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_color_manual(values = c("NYC" = COL_NYC, "Rest of NYS" = COL_GIVE)) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2010, 2026, 2)) +
  labs(
    title    = "Annual Robberies: NYC vs. Rest of NYS",
    subtitle = "DCJS Index Crimes by County, agencies with 12 months reported",
    x = NULL, y = "Annual Robberies",
    caption  = "Source: NYS DCJS Index Crimes by County and Agency"
  ) +
  theme_pursuit()

print(p_g1)
save_plot(p_g1, "G1_county_robbery_nyc_vs_rest")


# G2: Top non-NYC counties (SCM donor pool)
top_donors <- county_annual %>%
  filter(nyc == "Rest of NYS", year >= 2015) %>%
  group_by(county) %>%
  summarise(mean_robbery = mean(robbery, na.rm = TRUE), .groups = "drop") %>%
  slice_max(mean_robbery, n = 12)

p_g2 <- county_annual %>%
  filter(county %in% top_donors$county, year >= 2010) %>%
  ggplot(aes(year, robbery, color = county)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  scale_x_continuous(breaks = seq(2010, 2026, 2)) +
  labs(
    title    = "Top Non-NYC Counties: Annual Robberies",
    subtitle = "Potential synthetic control donor pool",
    x = NULL, y = "Annual Robberies",
    caption  = "Source: NYS DCJS Index Crimes by County and Agency"
  ) +
  theme_pursuit() +
  theme(legend.position = "right")

print(p_g2)
save_plot(p_g2, "G2_scm_donor_counties", w = 12, h = 7)


# H. Break-point diagnostics ===================================================

cat("\n--- H. Break-point analysis ---\n\n")

pursuit_mo_diag <- pursuit_mo %>%
  arrange(month_date) %>%
  mutate(
    roll_mean_6  = zoo::rollmean(n, k = 6, fill = NA, align = "right"),
    roll_mean_12 = zoo::rollmean(n, k = 12, fill = NA, align = "right"),
    roll_sd_6    = zoo::rollapply(n, width = 6, FUN = sd, fill = NA, align = "right"),
    mom_change   = n - lag(n),
    yoy_change   = n - lag(n, 12),
    yoy_pct      = round((n / lag(n, 12) - 1) * 100, 1)
  )

cat("Largest YoY increases:\n")
print(
  pursuit_mo_diag %>%
    filter(!is.na(yoy_change)) %>%
    slice_max(yoy_change, n = 10) %>%
    select(month_date, n, roll_mean_6, yoy_change, yoy_pct),
  n = Inf
)
cat("\n")

cat("Largest MoM increases:\n")
print(
  pursuit_mo_diag %>%
    filter(!is.na(mom_change)) %>%
    slice_max(mom_change, n = 10) %>%
    select(month_date, n, mom_change),
  n = Inf
)
cat("\n")

p_h1 <- ggplot(pursuit_mo_diag, aes(month_date)) +
  geom_col(aes(y = n), fill = COL_PURSUIT, alpha = 0.3, width = 25) +
  geom_line(aes(y = roll_mean_6, color = "6-month rolling mean"),
            linewidth = 1) +
  geom_line(aes(y = roll_mean_12, color = "12-month rolling mean"),
            linewidth = 1) +
  scale_color_manual(values = c(
    "6-month rolling mean"  = COL_HIGHLIGHT,
    "12-month rolling mean" = "#1D3557"
  )) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Pursuit Broadcasts: Rolling Means for Break-Point Detection",
    subtitle = "Monthly bars with 6- and 12-month trailing averages",
    x = NULL, y = "Monthly Pursuit Events",
    caption  = "Source: NYPD Calls for Service"
  ) +
  theme_pursuit()

print(p_h1)
save_plot(p_h1, "H1_pursuit_rolling_means")


# H2: Year-over-year change
yoy_data <- pursuit_mo_diag %>%
  filter(!is.na(yoy_change)) %>%
  mutate(direction = ifelse(yoy_change > 0, "increase", "decrease"))

p_h2 <- ggplot(yoy_data, aes(month_date, yoy_change, fill = direction)) +
  geom_col(alpha = 0.7, width = 25, show.legend = FALSE) +
  scale_fill_manual(values = c("increase" = COL_HIGHLIGHT, "decrease" = COL_GIVE)) +
  geom_hline(yintercept = 0, color = "grey30") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  labs(
    title    = "Year-over-Year Change in Monthly Pursuit Broadcasts",
    subtitle = "Red = YoY increase | Blue = YoY decrease",
    x = NULL, y = "Change vs. Same Month Prior Year",
    caption  = "Source: NYPD Calls for Service"
  ) +
  theme_pursuit()

print(p_h2)
save_plot(p_h2, "H2_pursuit_yoy_change")


# I. Export tables =============================================================

cat("\n--- Exporting summary tables ---\n\n")

write_csv(monthly_panel, file.path(table_dir, "monthly_panel.csv"))
cat("  Saved: monthly_panel.csv\n")

write_csv(pursuit_annual, file.path(table_dir, "pursuit_annual.csv"))
cat("  Saved: pursuit_annual.csv\n")

write_csv(collision_annual, file.path(table_dir, "collision_annual.csv"))
cat("  Saved: collision_annual.csv\n")

write_csv(robbery_annual, file.path(table_dir, "robbery_annual.csv"))
cat("  Saved: robbery_annual.csv\n")

write_csv(shooting_annual, file.path(table_dir, "shooting_annual.csv"))
cat("  Saved: shooting_annual.csv\n")

write_csv(give_comparison, file.path(table_dir, "give_nyc_vs_nonnyc.csv"))
cat("  Saved: give_nyc_vs_nonnyc.csv\n")

write_csv(county_annual, file.path(table_dir, "county_annual_crimes.csv"))
cat("  Saved: county_annual_crimes.csv\n")

write_csv(region_annual, file.path(table_dir, "region_annual_comparison.csv"))
cat("  Saved: region_annual_comparison.csv\n")

write_csv(pursuit_mo_diag, file.path(table_dir, "pursuit_monthly_diagnostics.csv"))
cat("  Saved: pursuit_monthly_diagnostics.csv\n")

write_csv(pursuit_qtr, file.path(table_dir, "pursuit_quarterly.csv"))
cat("  Saved: pursuit_quarterly.csv\n")

cat("\nEDA complete.\n")
cat("Plots:", plot_dir, "\n")
cat("Tables:", table_dir, "\n\n")
