# The Effects of Lifting Vehicle Pursuit Restrictions: Evidence from New York City

**Authors:** John Hall & Justin Nix

**Status:** Accepted at *Journal of Quantitative Criminology* (2026-09-15)

This repository contains the analysis pipeline underlying the paper — everything needed to go from raw NYC Open Data to the tables, figures, and model objects reported in the published findings. It does **not** contain the manuscript text itself (see [Citation](#citation) below for how to cite the published article).

## Abstract

**Objectives:** This study examines the consequences of the New York Police Department's late-2022 operational shift that dramatically expanded vehicle pursuits without a formal policy change. We evaluate whether the surge in pursuits affected motor vehicle collisions, robberies, and shootings, and whether any crime-prevention benefits offset the associated collision costs.

**Methods:** For collision outcomes, interrupted time series (ITS) segmented regression provides direct causal estimates. For crime outcomes, where within-city trends may reflect national forces, we triangulate across a city-level ITS, a generalized synthetic control comparing New York with large U.S. cities (shootings and robbery), a test of whether crime rose after the February 2025 re-restriction, and descriptive borough- and precinct-level comparisons of where any decline was concentrated. A cost-benefit analysis estimates the threshold of prevented shootings needed for pursuit escalation to break even.

**Results:** Monthly pursuits rose from roughly eight to a peak above 200. Pursuit-related collisions increased proportionally with pursuit volume across all four policy regimes and declined sharply following the February 2025 policy re-restriction. The collision result is the paper's one causally identified estimate. Crime outcomes provide no evidence of deterrence. The generalized synthetic control finds NYC's post-2022 shooting trajectory statistically indistinguishable from the national counterfactual, and robbery, if anything, rose relative to comparable cities. The February 2025 reduction in pursuits was not followed by any increase in crime. Descriptive within-city comparisons align: the shooting DiD rests on non-parallel pre-trends, and the robbery DiD runs opposite to deterrence.

**Conclusions:** Pursuit escalation produced clear and attributable collision harms while providing no credible evidence of crime reduction. Cost-benefit estimates suggest the threshold of prevented shootings required to offset these harms was not achieved. Agencies should require affirmative evidence of crime-prevention benefits before expanding pursuit authority.

## Citation

```
Hall, J., & Nix, J. (2026). The effects of lifting vehicle pursuit restrictions:
Evidence from New York City. Journal of Quantitative Criminology. Advance
online publication. DOI: forthcoming
```

Update the DOI above once JQC assigns one; a BibTeX entry will be added when available.

## Project Structure

```text
fast_and_spurious/
├── README.md
├── LICENSE                    # MIT (applies to the code in this repository)
├── .gitignore
├── docs/                      # Background policy documents
│   ├── pg212-39-vehicle-pursuits.pdf          # NYPD Patrol Guide on vehicle pursuits
│   └── NYPD_Vehicle_Pursuit_Timeline.docx     # Timeline of policy changes (authors' compilation)
├── data/                      # Raw CSV/geojson/RDS source files — NOT included; see Data Sources below
├── scripts/                   # 31 analysis scripts; see "Scripts" below
├── output/
│   ├── plots/                 # Visualizations (PNG + PDF)
│   ├── tables/                # Summary and diagnostic tables
│   ├── its_results/           # ITS regression coefficients and Newey-West SEs
│   ├── event_results/         # Event study outputs and regime comparisons
│   ├── national_did/          # Borough monthly panels and legacy national DiD results
│   ├── national_scm/          # National SCM (per-borough) results, incl. robbery
│   ├── did_scm_results/       # Legacy within-NYS DiD/SCM results
│   ├── precinct_did/          # Precinct-level panels and model objects
│   ├── cba_results/           # Cost-benefit analysis outputs
│   ├── gsynth_results/        # Generalized synthetic control outputs (ATT estimates,
│   │                          # figures, placebo-in-space permutation results)
│   ├── synthdid_results/      # Synthetic DiD estimates, unit/time weights, trajectories
│   ├── detrend_results/       # Group-detrending and pre-trend sensitivity outputs
│   ├── extension/             # Precinct dose-response DiD model objects
│   └── publication_tables/    # Manuscript-ready tables (CSV)
└── renv.lock                  # Locked package versions
```

**Scripts** (in `scripts/`), in the order the master runner executes them:

| Script | Purpose |
| --- | --- |
| `00_run_all.R` | Master runner — executes the active pipeline in order |
| `01_load_clean.R` | Data loading and monthly/precinct panel construction |
| `02_eda.R` | Exploratory data analysis (optional; not part of `00_run_all.R`) |
| `03-its_analysis.R` | City-level ITS (primary method) |
| `06_event_study.R` | Multi-period regimes + Tisch falsification |
| `07_cba.R` | Cost-benefit analysis |
| `08_national_data_prep.R` | Borough monthly panels (required by 14) + national comparison data prep |
| `14_borough_its.R` / `14_borough_dose_response.R` | Borough-level ITS and dose-response robustness |
| `10_national_scm.R` | Borough-level synthetic control; robbery result reported in the published paper |
| `17_precinct_ddd.R` | Precinct triple-difference (must run before 15) |
| `15_precinct_twfe.R` | Precinct TWFE + map (reads `17`'s output) |
| `16_precinct_staggered.R` | Sun-Abraham staggered-adoption robustness |
| `00_regenerate_pub_tables.R` | Refreshes manuscript-ready tables from other scripts' outputs |
| `20_tisch_its.R` | Tisch re-restriction ITS ("reverse experiment") |
| `21_pursuit_dose_did.R` | Precinct pursuit-intensity dose-response DiD |
| `22_enforcement_controls.R` | Precinct B-summons/arrest enforcement-posture controls |
| `23_synthdid.R` | Synthetic DiD (Arkhangelsky et al. 2021), precinct panel |
| `24_collision_elasticity.R` | Marginal collision probability per additional pursuit |
| `25_shooting_detrend.R` | Group-specific detrending robustness, shooting DiD |
| `26_robbery_sensitivity.R` | Linear pre-trend extrapolation sensitivity, robbery DiD |
| `19_gsynth_permutation.R` | gsynth placebo-in-space permutation inference (needs `18`'s cached output — see Replication) |
| `29_gsynth_annual.R` | Annual gsynth for shootings + robbery (national donor panels) |
| `28_national_mde.R` | National comparison estimates expressed as % of NYC's base rate |
| `18_gsynth.R` | Generalized synthetic control (monthly shootings). Run manually first — see Replication. |

**Retained but not part of the active pipeline (`[legacy]`)** — identification failed or the design was superseded, kept for transparency:

| Script | Why it's legacy |
| --- | --- |
| `04_did_analysis.R` | Within-NYS DiD — parallel trends violated |
| `05_scm_analysis.R` | Within-NYS SCM — degenerate (one donor gets ~100% weight) |
| `09_national_did.R` | National borough-level DiD — superseded by the generalized synthetic control |
| `11_av_data_prep.R` / `12_av_did.R` / `13_av_scm.R` | AmericanViolence.org-based DiD/SCM — parallel trends violated / not statistically significant |

An earlier, undocumented crash-severity exploration (once numbered `27`) is not included in this package — its empirical fatality-rate estimate was judged unreliable (likely administrative undercount) and was never used in any reported result.

## Data Sources

Raw data is **not tracked in git** (several files exceed 100MB) and must be downloaded separately and placed in a `data/` directory at the repository root. All primary NYC data comes from [NYC Open Data](https://data.cityofnewyork.us); search by the exact dataset title shown below if a direct link isn't given.

### Required to run the full pipeline (`00_run_all.R`)

| File | Source | Coverage / notes |
| --- | --- | --- |
| `NYPD_Calls_for_Service_(Historic)_*.csv` | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Calls-for-Service-Historic-/d6zx-ckhd) | Pursuit broadcasts, historic |
| `NYPD_Calls_for_Service_(Year_to_Date)_*.csv` | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Calls-for-Service-Year-to-Date-/n2zq-pubd) | Pursuit broadcasts, current year |
| `Motor_Vehicle_Collisions_-_Vehicles_*.csv` | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Vehicles/bm4k-52h4) | Vehicle-level collision records |
| `Motor_Vehicle_Collisions_-_Crashes_*.csv` | [NYC Open Data — search "Motor Vehicle Collisions - Crashes"](https://data.cityofnewyork.us/browse?q=Motor%20Vehicle%20Collisions%20-%20Crashes) | Crash-level records (distinct dataset from Vehicles, above) |
| `NYPD_Complaint_Data_Historic_*.csv` | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i) | Robbery complaints, historic (683MB unfiltered — consider pre-filtering to `OFNS_DESC == "ROBBERY"`) |
| `NYPD_Complaint_Data_Current_(Year_To_Date)_*.csv` | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Current-Year-To-Date-/5uac-w243) | Robbery complaints, current year |
| `Shootings_(2006-Present)_*.csv` | [NYC Open Data — search "Shootings (2006-Present)"](https://data.cityofnewyork.us/browse?q=Shootings%20(2006-Present)) | Incident-level shooting data. **Note:** NYC Open Data split the old combined "NYPD Shooting Incident Data" file into this incident-level file and a separate victim-level file in 2026; the pipeline uses only the incident-level file. |
| `sf_since_2017.csv`, `shots_fired_new.csv` | [NYC Open Data — search "NYPD Shots Fired Incidents"](https://data.cityofnewyork.us/browse?q=NYPD%20Shots%20Fired%20Incidents) | Shots-fired reports, spliced from two source files at a temporal cutoff |
| `Police_Precincts_*.geojson` | [NYC Open Data — search "Police Precincts"](https://data.cityofnewyork.us/browse?q=Police%20Precincts) | Precinct boundary file, used for the precinct-level spatial join |
| `Index_Crimes_by_County_and_Agency__Beginning_1990_*.csv` | [NYS DCJS via data.ny.gov — search "Index Crimes by County and Agency"](https://data.ny.gov) | County-level crime counts (feeds the legacy within-NYS comparison; loaded unconditionally by `01_load_clean.R`) |
| `top50_cities.csv` | **Not an external download — you build this file.** | A reference table with columns `city, state, ori, pop_2020, treated` (one row per comparison city; `treated = 1` for the 5 NYC boroughs, `0` for donor cities). Used by `08_national_data_prep.R` to join Census population and mark treatment status. |
| `offenses_known_yearly_1960_2024.rds` | [Kaplan, J. (2021). Uniform Crime Reporting Program Data: Offenses Known and Clearances by Arrest. OpenICPSR project 100707](https://www.openicpsr.org/openicpsr/project/100707) | Annual FBI UCR Return A, used for the national robbery synthetic control (feeds `10_national_scm.R` and `29_gsynth_annual.R`) |
| `american_violence/*.csv` (lowercase directory name) | [AmericanViolence.org](https://americanviolence.org) | Annual Gun Violence Archive-derived shooting counts, 100 largest US cities, 2016–2025. Used only by `18_gsynth.R` and `29_gsynth_annual.R`. Place files directly in `data/american_violence/`. |
| `Moving_Violation_B_Summons_(Historic)_*.csv` | [NYC Open Data — search "Moving Violation (Parking) B Summons"](https://data.cityofnewyork.us/browse?q=Moving%20Violation%20B%20Summons) | Traffic-enforcement intensity control for `22_enforcement_controls.R` |
| `NYPD_Arrests_Data_(Historic)_*.csv` | [NYC Open Data — search "NYPD Arrests Data (Historic)"](https://data.cityofnewyork.us/browse?q=NYPD%20Arrests%20Data%20Historic) | Enforcement-intensity control for `22_enforcement_controls.R` |

### Optional (only needed for the `[legacy]` scripts, not for `00_run_all.R`)

| File | Source | Used by |
| --- | --- | --- |
| `give_data4.csv` | GIVE (Gun Involved Violence Elimination) statewide data | `01_load_clean.R`'s gated legacy block (near-zero for non-NYC jurisdictions after 2018 — not usable for DiD, kept for transparency only) |
| `baltimore_shootings.csv`, `boston_shootings.csv`, `chicago_shooting.csv`, `cincinatti_shooting.csv`, `philly_shootings.csv`, plus Buffalo/Rochester/Syracuse GIVE monthly aggregates | City-specific police-department open data portals | `08_national_data_prep.R`'s legacy branch, feeding `09_national_did.R`, `12_av_did.R`, `13_av_scm.R` |

**Analysis window:** right-censored at a uniform **2025-12-01** endpoint, matching the pursuit CFS data's actual maximum date as of this project's last data pull.

## Intervention

- **Date:** October 2022
- **Policy:** Operational shift under Chief of Patrol John Chell lifted prior restrictions on vehicle pursuit initiation
- **Subsequent changes:** Commissioner Maddrey compliance memo (Aug 2023); Commissioner Tisch partial re-restriction (Feb 2025)
- **Magnitude (post-dedup):** Pre-escalation ~8/month; escalation ~88/month; post-Maddrey ~159/month; post-Tisch ~34/month

## Methods

### Active Pipeline

1. **Interrupted Time Series (ITS):** Primary method. Segmented regression with Newey-West HAC standard errors (lag = 6), month fixed effects for seasonality, and a COVID indicator (Mar 2020–Jun 2021). Sensitivity analysis over intervention date. Within-NYC temporal placebo tests and cross-city placebo tests (Baltimore, Boston, Philadelphia) were conducted as robustness checks.

2. **National Comparison — Generalized Synthetic Control:** gsynth (Xu 2017) with NYC as the single treated unit against a donor pool of large U.S. cities. Shootings use monthly Gun Violence Archive counts (AmericanViolence.org / Sharkey); robbery uses an annual FBI UCR panel (34 cities, 2014–2024). Cross-validation selects the number of latent factors — under the currently pinned `gsynth` version, zero, so the estimator reduces to two-way fixed effects. Placebo-in-space permutation inference (`19_gsynth_permutation.R`) and an effect-size-in-percentage-terms table (`28_national_mde.R`, `29_gsynth_annual.R`) support the null. Both outcome comparisons are null-or-positive: NYC did not outperform comparable cities.

3. **Re-restriction ITS (`20_tisch_its.R`):** Four-regime segmented model around the February 2025 partial re-restriction ("reverse experiment"). Crime did not rise after pursuits fell.

4. **Community-Level Heterogeneity (descriptive):** Borough-level ITS and a precinct-level heterogeneous-effects DiD (77 precincts × 96 months) grouping precincts by pre-treatment crime level. Event-study diagnostics show non-parallel pre-trends for the crime outcomes, so these are reported as descriptive evidence on where the crime decline was concentrated, not as causal estimates. Precinct TWFE and Sun-Abraham staggered event study serve as supporting checks.

5. **Cost-Benefit Analysis:** Pursuit crash costs (well-identified via collision data) vs. shooting-prevention benefits (require a causal attribution assumption). Break-even analysis: minimum causal share of the ITS-estimated shooting decline at which benefits exceed collision costs. Sensitivity grid over VSL, fatality rate, and causal attribution.

### R&R Robustness Battery

Added in response to peer review for a more direct precinct-level test and additional robustness checks:

- **Precinct pursuit-intensity regression** (`21_pursuit_dose_did.R`): regresses each outcome on precinct-month pursuit counts with precinct + month fixed effects, then adds precinct-specific linear time trends and precinct × year fixed effects as increasingly demanding controls for area-level secular trends. Crime associations (shootings, robbery) attenuate and lose significance under the most demanding spec; the collision association holds across all three.
- **Tisch re-restriction ITS** (`20_tisch_its.R`): extends the city-level ITS to a four-regime segmented model around the Feb 2025 partial re-restriction.
- **gsynth placebo-in-space permutation** (`19_gsynth_permutation.R`): re-estimates gsynth's ATT for each donor city as if it were the treated unit, ranking NYC's actual ATT against the resulting null distribution.
- **Supporting robustness checks** (`22`–`26`): enforcement-intensity controls (B-summons/arrest volume), synthetic DiD (Arkhangelsky et al. 2021) on the precinct panel, collision-pursuit elasticity, group-specific detrending for the shooting DiD, and a linear pre-trend extrapolation sensitivity check for the robbery DiD.

### Legacy Methods

The following designs were estimated but did not survive into the published results, due to identification failures:

- **Within-NYS DiD** (`04_did_analysis.R`): Parallel trends violated — every pre-treatment NYC × year interaction significant (p < .001).
- **Within-NYS SCM** (`05_scm_analysis.R`): Degenerate — one donor county receives ~100% weight.
- **National DiD — Borough-Level** (`09_national_did.R`): Superseded by the generalized synthetic control.
- **AV-Based DiD/SCM** (`12_av_did.R`, `13_av_scm.R`): Parallel trends violated (DiD); not statistically significant (SCM).

## Requirements

- R >= 4.3 (tested on R 4.4.1 — see `renv.lock` for the exact pinned version)
- Key packages: `tidyverse`, `lubridate`, `sandwich`, `lmtest`, `zoo`, `scales`, `janitor`, `here`, `broom`, `fixest`, `tidysynth`, `fwildclusterboot`, `sf`, `gsynth`, `synthdid`
- `synthdid` is not on CRAN and is not captured by `renv.lock` — install it separately: `remotes::install_github("synth-inference/synthdid")`
- Package versions are locked in `renv.lock`. Restore the rest of the environment with `renv::restore()`.

## Replication

1. Download the raw files listed under Data Sources above and place them in a `data/` directory at the repository root (create `data/american_violence/` for the AmericanViolence.org files, using that exact lowercase name).
2. Run script 18 once manually first (produces the gsynth model object that script 19 depends on; not part of the automated runner — see the header comment in `00_run_all.R`):

   ```r
   source("scripts/18_gsynth.R")
   ```

3. Run the active pipeline via the master runner (~30–60 min depending on hardware):

   ```r
   source("scripts/00_run_all.R")
   ```

   Or run scripts individually in dependency order:
   `01 → 03 → 06 → 07 → 08 → 14 (ITS + dose-response) → 10 → 17 → 15 → 16 → 00_regenerate_pub_tables → 20 → 21 → 22 → 23 → 24 → 25 → 26 → 19 → 29 → 28`

   **Critical ordering constraints:** script 17 must precede script 15 (15 reads `pct_ddd_models.rds` produced by 17). Script 08 must precede script 14. Scripts 19 and 28 require script 18's outputs (`gsynth_robustness.rds` and the bootstrap CI columns in `gsynth_att.csv`) — run script 18 first (step 2 above). Script 28 also reads script 29's output.

All tables, figures, and model objects land in `output/`.

## License

- **Code** (`scripts/`) and this documentation: [MIT License](LICENSE)

The manuscript text itself is not distributed in this repository; see [Citation](#citation) for how to cite the published article.
