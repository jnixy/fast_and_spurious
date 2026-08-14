# The Effects of Lifting Vehicle Pursuit Restrictions: Evidence from New York City

**Authors:** John Hall & Justin Nix

**Status:** Manuscript resubmitted to *Journal of Quantitative Criminology* (R&R round 2, 2026-08-14)

**Target Journal:** *Journal of Quantitative Criminology*

## Abstract

**Objectives:** This study examines the consequences of the New York Police Department's late-2022 operational shift that dramatically expanded vehicle pursuits without a formal policy change. We evaluate whether the surge in pursuits affected motor vehicle collisions, robberies, and shootings, and whether any crime-prevention benefits offset the associated collision costs.

**Methods:** ITS segmented regression estimates changes in pursuit-related collisions following the October 2022 escalation; no external comparison group is required because collisions are mechanically linked to pursuit volume. For crime outcomes, a precinct-level triple-difference (DDD) design exploits pre-treatment crime-level variation across all 77 NYPD precincts, with event-study diagnostics assessing the parallel trends assumption. A cost–benefit analysis estimates the threshold of prevented crimes needed for pursuit escalation to break even.

**Results:** Monthly pursuits surged from roughly 8 to over 160 events — a rise exceeding 2,000%. Pursuit-related collisions increased proportionally with pursuit volume and declined sharply following the February 2025 partial re-restriction that reduced pursuits by approximately three-quarters. Crime outcomes provide no evidence of deterrence: the robbery DDD estimate is positive and significant, while the shooting DDD estimate, though nominally negative, rests on non-parallel pre-trends. The February 2025 reduction in pursuits was not followed by any increase in crime.

**Conclusions:** Pursuit escalation produced clear and attributable collision harms while providing no credible evidence of crime reduction. Cost–benefit estimates suggest the threshold of prevented shootings required to offset these harms was not achieved. Agencies should require affirmative evidence of crime-prevention benefits before expanding pursuit authority.

## Project Structure

```text
vehicle_pursuits/
├── README.md                  # This file
├── .gitignore
├── vehicle_pursuits.Rproj
├── docs/                      # Background documents & policy PDFs
│   ├── pg212-39-vehicle-pursuits.pdf
│   └── NYPD_Vehicle_Pursuit_Timeline.docx
├── data/                      # CSV source files (not tracked in git)
├── literature/                # PDF papers and synthesis notes
│   ├── notes.md               # Annotated bibliography
│   └── [~30 PDFs]
├── scripts/
│   ├── 00_run_all.R             # Master runner — active pipeline in order
│   ├── 00_regenerate_pub_tables.R # Refresh manuscript-ready tables from outputs
│   ├── 01_load_clean.R          # Data loading and monthly panel construction
│   ├── 02_eda.R                 # Exploratory data analysis (optional; ~10 min)
│   ├── 03-its_analysis.R        # City-level ITS (primary method)
│   ├── 04_did_analysis.R        # [legacy] Within-NYS DiD (parallel trends violated)
│   ├── 05_scm_analysis.R        # [legacy] Within-NYS SCM (degenerate)
│   ├── 06_event_study.R         # Multi-period regimes + Tisch falsification
│   ├── 07_cba.R                 # Cost-benefit analysis
│   ├── 08_national_data_prep.R  # Borough monthly panels (required by 14)
│   ├── 09_national_did.R        # [legacy] National DiD + wild cluster bootstrap
│   ├── 10_national_scm.R        # [legacy] National SCM (per-borough)
│   ├── 11_av_data_prep.R        # [legacy] AmericanViolence.org panel prep
│   ├── 12_av_did.R              # [legacy] AV-based DiD
│   ├── 13_av_scm.R              # [legacy] AV-based SCM
│   ├── 14_borough_its.R         # Borough-level ITS
│   ├── 14_borough_dose_response.R # Borough dose-response robustness
│   ├── 15_precinct_twfe.R       # Precinct TWFE + map
│   ├── 16_precinct_staggered.R  # Sun-Abraham staggered adoption robustness
│   ├── 17_precinct_ddd.R        # Precinct DDD (must run before 15)
│   ├── 18_gsynth.R              # Generalized synthetic control (Appendix B)
│   ├── 19_gsynth_permutation.R  # gsynth placebo-in-space permutation inference
│   ├── 20_tisch_its.R           # Tisch re-restriction ITS ("reverse experiment")
│   ├── 21_pursuit_dose_did.R    # Precinct pursuit-intensity regression (editor/R1 request)
│   ├── 22_enforcement_controls.R # Precinct B-summons/arrest enforcement-posture controls
│   ├── 23_synthdid.R            # Synthetic DiD (Arkhangelsky et al. 2021), precinct panel
│   ├── 24_collision_elasticity.R # Marginal collision probability per additional pursuit
│   ├── 25_shooting_detrend.R    # Group-detrending robustness for the precinct shooting DiD
│   └── 26_robbery_sensitivity.R # Linear pre-trend extrapolation sensitivity, robbery DiD
├── output/
│   ├── plots/                # Visualizations (PNG + PDF)
│   ├── tables/                # Summary and diagnostic tables
│   ├── its_results/           # ITS regression coefficients and Newey-West SEs
│   ├── event_results/         # Event study outputs and regime comparisons
│   ├── national_did/          # Borough monthly panels and legacy DiD results
│   ├── national_scm/          # Legacy national SCM results
│   ├── precinct_did/          # Precinct-level model objects (RDS) + enforcement controls
│   ├── cba_results/           # Cost-benefit analysis outputs
│   ├── gsynth_results/        # Generalized synthetic control outputs (ATT estimates,
│   │                          # figures, placebo-in-space permutation results)
│   ├── synthdid_results/      # Synthetic DiD estimates, unit/time weights, trajectories
│   ├── detrend_results/       # Group-detrending and pre-trend sensitivity outputs
│   ├── extension/             # Precinct dose-response DiD model objects
│   └── publication_tables/    # Manuscript-ready tables (CSV)
├── quality_reports/
│   ├── plans/             # Implementation plans (one per session)
│   └── session_logs/      # Running log of decisions and findings
└── manuscript/
    ├── references.bib
    ├── criminology-and-public-policy.csl
    └── R&R/
        ├── revised-manuscript.qmd  # Current manuscript source (renders to .pdf/.docx)
        └── response-memo.md        # Point-by-point response to editor/reviewers
```

**Scripts marked `[legacy]`** are retained for reproducibility but are not part of the active publication pipeline. **Scripts 19-26** are the R&R round-2 robustness battery (Phase 6 of `00_run_all.R`): 20 and 21 feed numbers directly into the manuscript (the Tisch section and the precinct×time-trend footnote, respectively); 22-26 are supporting robustness checks not yet cited in manuscript prose.

## Data Sources

All primary data sourced from NYC Open Data.

| Dataset | Direct Download | Coverage |
| ------- | --------------- | -------- |
| NYPD Calls for Service (Historic) | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Calls-for-Service-Historic-/d6zx-ckhd) | Oct 2018 – Dec 2024 |
| NYPD Calls for Service (Year to Date) | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Calls-for-Service-Year-to-Date-/n2zq-pubd) | Jan 2025 – present |
| Motor Vehicle Collisions – Vehicles | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Vehicles/bm4k-52h4) | 2016 – present |
| NYPD Complaint Data (Historic) | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Historic/qgea-i56i) | Jan 2014 – Dec 2024 |
| NYPD Complaint Data (Year to Date) | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Complaint-Data-Current-Year-To-Date-/5uac-w243) | Jan 2025 – present |
| NYPD Shooting Incident Data (Historic) | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Historic-/833y-fsy8) | Through Dec 2024 |
| NYPD Shooting Incident Data (Year to Date) | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Shooting-Incident-Data-Year-To-Date-/5ucz-vwe8) | 2025 – present |
| NYPD Shots Fired Incidents | [NYC Open Data](https://data.cityofnewyork.us/Public-Safety/NYPD-Shots-Fired-Incidents/XXXX-XXXX) | 2017 – present |
| AmericanViolence.org city-month panel | [americanviolence.org](https://americanviolence.org) | 2016–2025, 100 largest US cities |

**Note:** Raw data files are not tracked in git due to size (complaint data alone is over 100MB). Download each file as CSV and place in `data/`. AmericanViolence.org data (used only in the generalized synthetic control, Appendix B; script `18_gsynth.R`) should be placed in `data/American_violence/`. The analysis window is right-censored at a uniform **2025-12-01** endpoint, matching the pursuit CFS data's actual maximum date as of the 2026-07-27 extension.

## Intervention

- **Date:** October 2022
- **Policy:** Operational shift under Chief of Patrol John Chell lifted prior restrictions on vehicle pursuit initiation
- **Subsequent changes:** Commissioner Maddrey compliance memo (Aug 2023); Commissioner Tisch partial re-restriction (Feb 2025)
- **Magnitude (post-dedup):** Pre-escalation ~8/month; escalation ~88/month; post-Maddrey ~159/month; post-Tisch ~34/month

## Methods

### Active Pipeline

1. **Interrupted Time Series (ITS):** Primary method. Segmented regression with Newey-West HAC standard errors (lag = 6), month fixed effects for seasonality, and a COVID indicator (Mar 2020–Jun 2021). Sensitivity analysis over intervention date reported in Appendix A. Within-NYC temporal placebo tests and cross-city placebo tests (Baltimore, Boston, Philadelphia) were conducted as robustness checks but are not shown in the appendix.

2. **Borough-Level ITS:** ITS estimated separately for each of the five NYC boroughs. Dose-response robustness check relates borough-level pursuit intensity to observed crime changes.

3. **Precinct-Level Panel Analysis:** Three complementary designs using 78 precincts × 93 months:
   - TWFE (`feols(y ~ pursuit_events | pct + month_date, cluster = ~pct)`)
   - Difference-in-differences-in-differences (DDD) contrasting precincts by pre-treatment crime level quartile
   - Sun-Abraham staggered adoption event study (`sunab()` via `fixest`)

4. **Event Study / Multi-Period:** Four-regime segmented regression (pre-escalation Jan 2018–Sep 2022; escalation Oct 2022–Jul 2023; post-Maddrey Aug 2023–Jan 2025; post-Tisch Feb 2025–Sep 2025). The Tisch reversal serves as a built-in falsification test for the deterrence hypothesis.

5. **Cost-Benefit Analysis:** Pursuit crash costs (well-identified via collision data) vs. crime reduction benefits (require causal attribution assumption). Breakeven analysis: minimum causal share at which benefits exceed costs (~4.5%). Sensitivity grid over VSL, fatality rate, shooting reduction, and causal attribution.

### R&R Round-2 Robustness Battery (Phase 6)

Added in response to the JQC editor's and Reviewer 1's requests for a more direct precinct-level test and additional robustness checks:

- **Precinct pursuit-intensity regression** (script 21): regresses each outcome on precinct-month pursuit counts with precinct + month fixed effects, then adds precinct-specific linear time trends and precinct × year fixed effects as increasingly demanding controls for area-level secular trends. Crime associations (shootings, robbery) attenuate and lose significance under the most demanding spec; the collision association holds across all three. Reported in a manuscript footnote (Precinct-Level Analysis/Limitations).
- **Tisch re-restriction ITS** (script 20): extends the city-level ITS to a four-regime segmented model around the Feb 2025 partial re-restriction ("reverse experiment"). Feeds the manuscript's Re-restriction section directly.
- **gsynth placebo-in-space permutation** (script 19): re-estimates gsynth's ATT for each of 76 donor cities as if it were the treated unit, ranking NYC's actual ATT against the resulting null distribution. Corroborates the gsynth null result (NYC ATT rank 73/76, two-sided *p* = .961).
- **Supporting robustness checks not yet cited in manuscript prose** (scripts 22-26): enforcement-intensity controls (B-summons/arrest volume), synthetic DiD (Arkhangelsky et al. 2021) on the precinct panel, collision-pursuit elasticity, group-specific detrending for the shooting DiD, and a linear pre-trend extrapolation sensitivity check for the robbery DiD (in the spirit of Rambachan & Roth 2023's breakdown-value concept, but not their constrained-optimization method).

### Legacy Methods (not in manuscript)

The following designs were estimated but are not included in the published manuscript due to identification failures:

- **Within-NYS DiD** (script 04): Parallel trends violated — every pre-treatment NYC × year interaction significant (p < .001).
- **Within-NYS SCM** (script 05): Degenerate — Erie County receives ~100% donor weight.
- **National DiD/SCM — Borough-Level** (scripts 09–10): Retained for robustness reference; removed from manuscript during restructuring.
- **AV-Based DiD/SCM** (scripts 12–13): Parallel trends violated (DiD); Fisher's p = .398 (SCM). Removed from manuscript.

## Requirements

- R >= 4.3 (tested on R 4.6.0)
- Key packages: `tidyverse`, `lubridate`, `sandwich`, `lmtest`, `zoo`, `scales`, `janitor`, `here`, `broom`, `fixest`, `tidysynth`, `fwildclusterboot`, `sf`, `gsynth`, `synthdid` (via `remotes::install_github("synth-inference/synthdid")`)
- Quarto (for manuscript rendering)
- Package versions locked in `renv.lock`. Restore the exact environment with: `renv::restore()`

## Replication

1. Place raw CSV files in `data/` (see Data Sources above)
2. Run script 18 once manually first (produces the gsynth model object script 19 depends on; not part of the automated runner — see `00_run_all.R`'s header):

   ```r
   source("scripts/18_gsynth.R")
   ```

3. Run the active pipeline via the master runner (~15-40 min depending on hardware):

   ```r
   source("scripts/00_run_all.R")
   ```

   Or run scripts individually in dependency order:
   `01 → 03 → 06 → 07 → 08 → 14 (ITS + dose-response) → 10 → 17 → 15 → 16 → 00_regenerate → 20 → 21 → 22 → 23 → 24 → 25 → 26 → 19`

   **Critical:** script 17 must precede script 15 (script 15 reads `pct_ddd_models.rds` produced by 17). Script 08 must precede script 14. Script 19 requires script 18's cached `output/gsynth_results/gsynth_robustness.rds` (step 2 above).

4. Render manuscript: `quarto render "manuscript/R&R/revised-manuscript.qmd" --to pdf` (or `--to docx`)

## License

- **Code** (`scripts/`): [MIT License](https://opensource.org/licenses/MIT)
- **Documentation and manuscript** (`manuscript/`, `README.md`): [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/)
