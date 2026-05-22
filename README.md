![Logo](https://github.com/user-attachments/files/28163686/d4a56cd962e5b9bb48d3f3dca25af851d7b1c8d0e033782b39445c37bbd3534e.tiff)

# MMMSociability

Multiscale behavioral analysis pipeline for automated homecage RFID tracking data in mouse social behavior and social instability stress experiments.

Maintainer: [Tobias Pohl](https://github.com/topohl)  
Institutional context: Max Delbrück Center for Molecular Medicine (MDC), Berlin; Hörnberg Lab.

---

## Overview

`MMMSociability` contains an R-based analysis workflow for longitudinal homecage behavioral tracking data. The repository was developed for RFID/position-based analysis of mouse social behavior, with a particular focus on social instability stress (SIS), cage-change aligned dynamics, early behavioral predictors, and systems-level behavioral phenotyping.

The pipeline is designed to move from raw or preprocessed RFID position data to manuscript-facing behavioral metrics, statistics, prediction models, QC summaries, and publication-style figures.

Core analytical themes include:

- tracking integrity and RFID/position QC
- multiscale movement, entropy, and proximity metrics
- cage-change and circadian phase annotation
- dyadic contact and social network features
- temporal instability and behavioral state-space analyses
- GAMM-derived trajectory features
- optional HMM behavioral state modeling
- early behavioral prediction of later stress burden
- systems-level feature integration
- optional behavior-proteomics integration

---

## Biological Use Case

The main use case is longitudinal analysis of mouse behavior during social instability stress and related homecage paradigms.

Typical experimental groups:

| Group | Meaning |
|---|---|
| `CON` | Control |
| `RES` | Resilient |
| `SUS` | Susceptible |

Typical behavioral domains:

| Domain | Examples |
|---|---|
| Psychomotor activity | movement, movement trajectories, raw movement phase statistics |
| Spatial organization | occupancy, entropy, spatial dynamics |
| Social behavior | proximity, dyadic RFID contacts, dynamic social networks |
| Temporal structure | instability, burstiness, autocorrelation-like dynamics, adaptation kinetics |
| Behavioral states | state-space summaries, optional HMM states |
| Systems-level integration | feature ladders, dashboards, behavior-physiology/proteomics bridges |

The repository is not limited to SIS, but the active scripts and naming conventions are currently optimized around this experimental design.

---

## Repository Structure

```text
MMMSociability/
├── Analysis/        # Active staged analysis pipeline and archived legacy scripts
├── Functions/       # Reusable helper functions for behavior metrics, statistics, plotting, and manifests
├── Formatting/      # Older/raw formatting and preprocessing scripts
├── Testing/         # Development, testing, and legacy exploratory scripts
├── docs/            # Manuscript/analysis strategy notes
└── README.md        # Top-level project documentation
```

The active manuscript-facing workflow is in `Analysis/`.

For the detailed stage-by-stage pipeline documentation, see:

```text
Analysis/README_pipeline.md
```

---

## Active Analysis Pipeline

The active staged pipeline is defined in:

```text
Analysis/run_all_analysis.R
```

The runner sources the shared setup script:

```text
Analysis/_pipeline_setup.R
```

and then executes the active stage scripts in order.

| Stage | Script | Role |
|---:|---|---|
| 00 | `00_qc_tracking_integrity.R` | Non-destructive RFID/tracking integrity QC |
| 01 | `01_build_multiscale_behavior_metrics.R` | Canonical multiscale behavior metrics |
| 02 | `02_build_dyadic_rfid_contacts.R` | Dyadic RFID contact table and network-ready edge data |
| 03 | `03_primary_raw_movement_phase_stats.R` | Primary raw movement broad-phase statistics |
| 04 | `04_temporal_instability.R` | Temporal instability and burstiness features |
| 05 | `05_behavioral_state_space.R` | Behavioral state-space features |
| 06 | `06_dynamic_social_networks.R` | Dynamic social network features |
| 07 | `07_gamm_trajectory_features.R` | GAMM trajectory-derived features |
| 08 | `08_hmm_behavioral_states_optional.R` | Optional HMM behavioral state modeling |
| 09 | `09_early_prediction_model_ladder.R` | Primary early prediction model ladder |
| 10 | `10_systems_feature_prediction_ladder.R` | Secondary systems-extension prediction ladder |
| 11 | `11_behavioral_adaptation_kinetics.R` | Adaptation and recovery kinetics |
| 12 | `12_sleep_like_quiescence_metrics.R` | Sleep-like quiescence and inactivity metrics |
| 13 | `13_ethological_phase_organization.R` | Ethological phase organization |
| 14 | `14_systems_neuroscience_summary_dashboard.R` | Integrated systems neuroscience dashboard |
| 15 | `15_behavior_proteomics_integration.R` | Optional behavior-proteomics integration |

Archived scripts are retained under `Analysis/_archive/` for provenance but should not be treated as the current primary workflow unless explicitly needed.

---

## Primary vs Secondary Analyses

### Primary behavioral statistics

The main raw movement broad-phase analysis is:

```text
Analysis/03_primary_raw_movement_phase_stats.R
```

This is the active replacement for older archived raw movement scripts.

### Primary early prediction analysis

The main conservative prediction analysis is:

```text
Analysis/09_early_prediction_model_ladder.R
```

This script is intended to test whether early behavior predicts later stress burden without making group labels the primary explanatory variable.

### Secondary systems-extension analysis

The broader systems feature model ladder is:

```text
Analysis/10_systems_feature_prediction_ladder.R
```

This should be interpreted as a secondary extension or sensitivity analysis rather than a replacement for the primary early prediction model.

### Optional analyses

Optional stages include:

```text
Analysis/08_hmm_behavioral_states_optional.R
Analysis/15_behavior_proteomics_integration.R
```

These depend on whether appropriate upstream tables and optional external data are available.

---

## Inputs

The repository expects preprocessed RFID/position data and associated animal metadata. Exact filenames can vary depending on the experiment and local data organization, but active scripts generally expect tables containing animal identity, time, phase/cage-change information, and behavioral measurements or position-derived metrics.

Common fields include:

| Field | Meaning |
|---|---|
| `AnimalNum` | Animal identifier |
| `Group` | Experimental group, e.g. `CON`, `RES`, `SUS` |
| `Sex` | Biological sex |
| `Phase` | Light/dark or active/inactive phase annotation |
| `CageChange` | Cage-change index or cage-change-aligned period |
| `Timestamp` / time column | Time of RFID/position measurement |
| `Movement` | Movement-derived metric |
| `Entropy` | Spatial or behavioral entropy metric |
| `Proximity` | Social proximity metric |
| `X`, `Y` | Position coordinates where available |

Some scripts can use fallback input discovery, but for reproducible analyses it is better to keep inputs in a stable project folder and document the selected input file for each analysis run.

---

## Outputs

Active scripts use a standardized output layout where possible.

Typical output folders include:

```text
tables/
stats_tables/
figures/publication_panels/
figures/supplementary/
figures/qc/
manifest/
logs/
```

The helper infrastructure writes manifest files to improve reproducibility, including input/output manifests where implemented.

Important generated outputs may include:

- multiscale behavioral metric tables
- dyadic contact tables
- animal-level feature matrices
- model comparison tables
- pairwise statistics and planned contrasts
- QC summaries
- publication panels
- supplementary figures
- integrated dashboard outputs

---

## Running the Pipeline

Run from the repository root:

```r
source("Analysis/run_all_analysis.R")
```

or from inside the `Analysis/` folder:

```r
source("run_all_analysis.R")
```

Optional stages are controlled through R options before sourcing the runner:

```r
options(
  mmm.run_optional_hmm = TRUE,
  mmm.run_systems_extension = TRUE,
  mmm.run_behavior_proteomics = FALSE,
  mmm.continue_on_error = FALSE
)

source("Analysis/run_all_analysis.R")
```

Recommended default for manuscript-facing core behavior analyses:

```r
options(
  mmm.run_optional_hmm = FALSE,
  mmm.run_systems_extension = TRUE,
  mmm.run_behavior_proteomics = FALSE,
  mmm.continue_on_error = FALSE
)

source("Analysis/run_all_analysis.R")
```

---

## Dependencies

The codebase is written in R.

Core package families used across the workflow include:

```r
# Data handling
library(tidyverse)
library(data.table)
library(readr)
library(readxl)
library(openxlsx)

# Statistics and modeling
library(lme4)
library(lmerTest)
library(emmeans)
library(mgcv)

# Plotting
library(ggplot2)
library(patchwork)
library(pheatmap)

# Utilities
library(zoo)
```

Not every package is required for every stage. Optional stages may require additional packages, depending on the enabled analysis modules.

A future improvement would be to add an `renv.lock` file for exact environment reconstruction.

---

## Reproducibility Notes

The current active pipeline already separates staged scripts and uses shared helper infrastructure. To keep analyses reproducible:

1. Run scripts from the repository root when possible.
2. Keep raw data separate from derived outputs.
3. Do not manually edit intermediate result tables.
4. Preserve `manifest/` and `logs/` outputs for each run.
5. Treat archived scripts as historical provenance, not as primary analysis scripts.
6. Document any non-default R options used for a run.
7. Prefer vector output formats such as SVG or PDF for manuscript figures.

---

## Interpretation Philosophy

The pipeline is built around the idea that homecage behavior should not be reduced only to simple means.

The analysis therefore emphasizes:

- magnitude of behavior
- temporal organization
- circadian/phase structure
- cage-change adaptation
- social network dynamics
- behavioral state transitions
- early predictors of later physiological or behavioral stress outcomes

This allows testing whether stress alters not only how much animals move or interact, but also how behavior is structured over time.

---

## Legacy and Archived Scripts

Older exploratory or deprecated scripts are retained in:

```text
Analysis/_archive/
Testing/
Formatting/
```

These files are useful for provenance and method development history. For new analyses, prefer the active staged scripts listed above and the run order in `Analysis/README_pipeline.md`.

---

## Suggested Future Improvements

Useful repository-level improvements would include:

- adding `renv.lock` for package version control
- adding a central configuration file, e.g. `config.yaml`
- adding a formal workflow engine such as `targets`
- adding a machine-readable input/output manifest for every script
- adding small example/demo data for testing pipeline integrity
- adding GitHub Actions checks for script parsing and basic linting
- adding a dependency graph showing which outputs feed into which scripts

---

## Citation

If this code is used in a manuscript, cite the associated publication or preprint and reference this repository.

Suggested repository citation format:

```text
Pohl T. MMMSociability: multiscale behavioral analysis of automated homecage RFID tracking data. GitHub repository: https://github.com/topohl/MMMSociability
```

---

## License

No explicit license is currently declared in this README.

Before reuse by external users, add a repository license file, for example:

- MIT License
- GNU GPLv3
- Apache License 2.0

Choose the license according to the intended level of openness and reuse.
