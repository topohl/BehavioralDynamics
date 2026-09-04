![Logo](https://github.com/user-attachments/files/28163686/d4a56cd962e5b9bb48d3f3dca25af851d7b1c8d0e033782b39445c37bbd3534e.tiff)

# BEHAVIORAL DYNAMICS

Multiscale behavioral analysis pipeline for automated homecage RFID tracking data in mouse social behavior and social instability stress experiments.

Maintainer: [Tobias Pohl](https://github.com/topohl)
Institutional context: Max Delbrück Center for Molecular Medicine (MDC), Berlin; Hörnberg Lab.

Repository: <https://github.com/topohl/BehavioralDynamics>

---

## Overview

`BehavioralDynamics` contains an R analysis workflow for longitudinal homecage behavioral tracking data, moving from preprocessed RFID position data to manuscript-facing metrics, statistics, prediction models and QC summaries.

**Scope, honestly stated.** This repository was developed for one experiment: the E9 social instability stress (SIS) RFID study. The staged pipeline, the phase and cage-change conventions, the group vocabulary and the endpoint definitions are all specific to that design. Several components are reusable — the preprocessing helpers, the phase classifier, the window selectors, the output-path and provenance infrastructure, the identity contract — but the repository as a whole is not a general-purpose package and is not currently packaged, parameterised or tested for other datasets.

Core analytical themes:

- tracking integrity and RFID/position QC
- multiscale movement, entropy and proximity metrics
- cage-change and circadian phase annotation
- dyadic contact and social network features
- temporal instability and behavioral state-space analyses
- GAMM-derived trajectory features
- optional HMM behavioral state modeling
- early behavioral prediction of later stress burden
- systems-level feature integration
- optional behavior-proteomics integration

---

## Start here

| If you want to | Read |
|---|---|
| Understand the manuscript claims | [`manuscript/README.md`](manuscript/README.md) |
| See the machine-readable analysis map | [`docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv`](docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv) |
| Re-run the pipeline | [`docs/REPRODUCIBILITY.md`](docs/REPRODUCIBILITY.md) |
| Know what the caveats are | [`docs/KNOWN_LIMITATIONS.md`](docs/KNOWN_LIMITATIONS.md) |
| Find the data and outputs | [`docs/DATA_AND_OUTPUTS.md`](docs/DATA_AND_OUTPUTS.md) |
| Freeze a release | [`docs/PUBLICATION_RELEASE.md`](docs/PUBLICATION_RELEASE.md) |
| Understand the stage-by-stage pipeline | [`Analysis/README_pipeline.md`](Analysis/README_pipeline.md) |
| Know what every file is for | [`docs/REPOSITORY_FILE_CLASSIFICATION.csv`](docs/REPOSITORY_FILE_CLASSIFICATION.csv) |

---

## Biological use case

Longitudinal analysis of mouse behavior during social instability stress and related homecage paradigms.

| Group | Meaning |
|---|---|
| `CON` | Control |
| `RES` | Resilient |
| `SUS` | Susceptible |

> `RES` and `SUS` are **derived from the downstream `CombZ` composite**, not independently assigned. Every group comparison is therefore phenotype characterization rather than independent validation. See [`docs/KNOWN_LIMITATIONS.md`](docs/KNOWN_LIMITATIONS.md).

| Domain | Examples |
|---|---|
| Psychomotor activity | movement, movement trajectories, raw movement phase statistics |
| Spatial organization | occupancy, entropy, spatial dynamics |
| Social behavior | proximity, dyadic RFID contacts, dynamic social networks |
| Temporal structure | instability, burstiness, autocorrelation-like dynamics, adaptation kinetics |
| Behavioral states | state-space summaries, optional HMM states |
| Systems-level integration | feature ladders, dashboards, behavior-physiology/proteomics bridges |

---

## Repository structure

```text
BehavioralDynamics/
├── Analysis/          Active staged pipeline (Stages 00-16, 19), runners, and _archive/
├── Functions/         Shared helpers: identity, preprocessing, windows, first-night, stats, HMM, output
├── Formatting/        Raw preprocessing and raw-level QC, upstream of the pipeline, plus _archive/
├── Testing/
│   ├── tests/         17 portable regression/contract checks (CI-eligible)
│   ├── audits/        42 data-dependent scientific validations
│   └── legacy/        11 historical development scripts
├── manuscript/
│   ├── README.md      Authoritative current publication architecture
│   ├── Fig1_behavior_candidates/
│   └── archive/       Historical/forensic provenance, explicitly not current
├── docs/              Reproducibility, data/outputs, limitations, release process, registries
├── .github/workflows/ CI for the portable suite only
├── CITATION.cff
└── README.md
```

Each of `Analysis/`, `Functions/`, `Formatting/`, `Testing/` and `manuscript/` has its own README.

---

## Active analysis pipeline

```r
source("Analysis/run_all_analysis.R")
```

The runner sources `Analysis/_pipeline_setup.R` and executes Stages 00–15:

| Stage | Script | Role |
|---:|---|---|
| 00 | `00_qc_tracking_integrity.R` | Non-destructive RFID/tracking integrity QC |
| 01 | `01_build_multiscale_behavior_metrics.R` | Canonical multiscale behavior metrics |
| 02 | `02_build_dyadic_rfid_contacts.R` | Dyadic RFID contact and network-ready edge data |
| 03 | `03_primary_raw_movement_phase_stats.R` | Secondary phenotype characterization: raw movement by phase |
| 04 | `04_temporal_instability.R` | Temporal instability and burstiness features |
| 05 | `05_behavioral_state_space.R` | Behavioral state-space features |
| 06 | `06_dynamic_social_networks.R` | Dynamic social network features |
| 07 | `07_gamm_trajectory_features.R` | GAMM trajectory-derived features |
| 08 | `08_hmm_behavioral_states_optional.R` | Optional gap-aware HMM behavioral state modeling |
| 09 | `09_early_prediction_model_ladder.R` | **Primary** prospective early prediction ladder |
| 10 | `10_systems_feature_prediction_ladder.R` | Secondary systems-extension prediction ladder |
| 11 | `11_behavioral_adaptation_kinetics.R` | Adaptation and recovery kinetics |
| 12 | `12_sleep_like_quiescence_metrics.R` | Sleep-like quiescence and inactivity metrics |
| 13 | `13_ethological_phase_organization.R` | Ethological phase organization |
| 14 | `14_systems_neuroscience_summary_dashboard.R` | Systems dashboard; canonical first-night five-domain analysis |
| 15 | `15_behavior_proteomics_integration.R` | Optional behavior-proteomics integration |

**Run outside the runner**, deliberately:

| Stage | Script | Why separate |
|---:|---|---|
| 16 | `16_manuscript_behavior_report.R` | Manuscript export layer; must run after canonical Stage 03/09 outputs exist |
| 19 | `19_spatial_occupancy_maps.R` | Secondary/spatial; not part of the manuscript package |
| — | `Formatting/E9_SIS_AnimalPos-preprocessing_parallell.r` | Rewrites Stage 01's canonical input; must be deliberate |

Optional stages are controlled by options before sourcing the runner:

```r
options(
  mmm.run_optional_hmm        = FALSE,
  mmm.run_systems_extension   = TRUE,
  mmm.run_behavior_proteomics = FALSE,
  mmm.continue_on_error       = FALSE
)
source("Analysis/run_all_analysis.R")
```

---

## Manuscript architecture

The authoritative statement is [`manuscript/README.md`](manuscript/README.md). In brief:

| Tier | Content |
|---|---|
| **Primary** | Stage 09 prospective prediction — does behavior in the first active 12 h after the first cage change predict later `CombZ`? Fixed a priori features `Movement_mean`, `Movement_rmssd`, `Entropy_acf1`; `Group` excluded. |
| **Secondary** | Stage 03 raw longitudinal movement; the first-night five-domain panel. |
| **Supplementary / conditional** | Active longitudinal HMM persistence, conditional on stating the identifiability caveats. |
| **Not promoted** | Inactive HMM / rest interpretation (measurement validity); occupancy entropy (sign-unstable); first-night HMM persistence (unstable across refits); Stage 10/14 systems, nonlinear and behavior-proteomics layers. |
| **Export layer** | Stage 16 writes the canonical manuscript package and source data. |

---

## Inputs

The repository expects preprocessed RFID position data and animal metadata. Common fields:

| Field | Meaning |
|---|---|
| `AnimalNum` | Animal identifier (canonicalized; zero-padding stripped) |
| `Group` | `CON`, `RES`, `SUS` |
| `Sex` | Biological sex |
| `Phase` | Active/Inactive annotation, matched by **exact membership** |
| `CageChange` | Cage-change index |
| `BinStart` | Bin start timestamp |
| `Movement` | PositionID transitions |
| `Entropy` | Occupancy entropy |
| `Proximity` | Same-position dyadic seconds / dyadic observation seconds (a **co-location proxy**, not direct sociability) |

---

## Outputs

Outputs are written to the local project root, never into the repository. Canonical migrated stages use:

```text
analysis_ready/pipeline/<stage_id>_<stage_name>/<resolution>/{tables,figures,audit}/
```

The manuscript entry point is:

```text
analysis_ready/manuscript/behavior/Behavioral_Source_Data.xlsx
```

Readers resolve canonical paths first and a single documented legacy path second; any fallback is warned and recorded in provenance. See [`docs/DATA_AND_OUTPUTS.md`](docs/DATA_AND_OUTPUTS.md).

---

## Verification

```bash
# portable suite (no experimental data required) - this is what CI runs
for f in Testing/tests/test_*.R; do Rscript "$f" || echo "FAILED: $f"; done
```

Data-dependent scientific audits live in `Testing/audits/` and require the E9 dataset. See [`Testing/README.md`](Testing/README.md).

---

## Dependencies

R 4.5.1 was used for validation. Declared dependencies and the versions present in the validated environment are in [`docs/package_versions.csv`](docs/package_versions.csv); the full session is in [`docs/sessionInfo.txt`](docs/sessionInfo.txt).

Principal packages: `tidyverse`, `data.table`, `readr`, `readxl`, `openxlsx`, `lme4`, `lmerTest`, `emmeans`, `mgcv`, `glmnet`, `depmixS4`, `igraph`, `ggplot2`, `patchwork`, `pheatmap`, `zoo`, `digest`.

`renv` is **not yet in use** and no `renv.lock` exists. This is stated rather than papered over — see [`docs/REPRODUCIBILITY.md`](docs/REPRODUCIBILITY.md) for why and for the recommended adoption path.

---

## Citation

Citation metadata is in [`CITATION.cff`](CITATION.cff).

```text
Pohl T. BehavioralDynamics: multiscale behavioral analysis of automated homecage
RFID tracking data. GitHub repository: https://github.com/topohl/BehavioralDynamics
```

No DOI or associated publication is recorded yet; those will be added when they exist.

---

## License

**No license file is currently present, so default copyright applies and external reuse is not yet permitted.**

A license must be chosen before the repository is shared for reuse. Options are laid out neutrally in [`docs/LICENSE_DECISION_REQUIRED.md`](docs/LICENSE_DECISION_REQUIRED.md); the choice is deliberately left to the maintainer.

---

## Interpretation philosophy

The pipeline is built on the premise that homecage behavior should not be reduced to simple means. It emphasizes magnitude, temporal organization, circadian and phase structure, cage-change adaptation, social spatial organization, state transitions, and early predictors of later stress burden — so that one can ask whether stress alters not only how much animals move, but how their behavior is organized over time.

That ambition is deliberately paired with conservative reporting: several analytically interesting layers in this repository are explicitly **not** promoted to manuscript claims because they do not currently meet the robustness or measurement-validity bar. Those decisions, and their evidence, are recorded in [`docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv`](docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv) and [`docs/KNOWN_LIMITATIONS.md`](docs/KNOWN_LIMITATIONS.md).
