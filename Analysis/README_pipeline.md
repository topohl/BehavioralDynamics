# MMMSociability Analysis Pipeline

This folder is organized as a staged, reviewer-safe pipeline. The scripts remain modular; no scientific models were merged. The staged order below is the intended run order for manuscript-facing analyses.

## Run Order

| Stage | Script | Role | Inputs | Main outputs |
|---:|---|---|---|---|
| 00 | `00_qc_tracking_integrity.R` | Non-destructive RFID/tracking integrity QC | Preprocessed or derived movement/entropy/proximity files | QC tables, Excel report, QC figures |
| 01 | `01_build_multiscale_behavior_metrics.R` | Canonical multiscale behavior metrics | Preprocessed RFID position data | `all_behavior_metrics.csv` at multiple bin levels |
| 02 | `02_build_dyadic_rfid_contacts.R` | Dyadic RFID contact table | Preprocessed position data | Dyadic contact tables and network-ready edge table |
| 03 | `03_primary_raw_movement_phase_stats.R` | Secondary phenotype/group characterization using broad raw movement | Stage 01 metrics | Raw movement endpoints, planned pairwise statistics, publication panels |
| 04 | `04_temporal_instability.R` | Temporal instability and burstiness features | Stage 01 metrics | Per-animal instability tables and figures |
| 05 | `05_behavioral_state_space.R` | Behavioral state-space features | Stage 01 metrics | State diversity and switching tables |
| 06 | `06_dynamic_social_networks.R` | Dynamic social network features | Stage 02 dyadic contacts, with metric fallback | Animal-level social dynamics and network summaries |
| 07 | `07_gamm_trajectory_features.R` | GAMM trajectory-derived features | Stage 01 metrics | Trajectory feature tables |
| 08 | `08_hmm_behavioral_states_optional.R` | Optional HMM state model with canonical identity and explicit 10-min primary / 5-min sensitivity | Stage 01 metrics plus the current canonical Stage 01 roster | HMM state assignments, transitions, occupancy, identity/sequence-quality/model-fit provenance |
| 09 | `09_early_prediction_model_ladder.R` | Primary early prediction model ladder: first active 12 h after the first cage change, using 10-min bins | Stage 01 metrics plus endpoint table | Fixed a priori early behavior prediction tables, permutation tests, and figures |
| 10 | `10_systems_feature_prediction_ladder.R` | Secondary systems-extension prediction ladder | Stage 09 plus optional downstream features | Domain-wise systems prediction comparison |
| 11 | `11_behavioral_adaptation_kinetics.R` | Adaptation/recovery kinetics | Stage 01 metrics | Recovery and stabilization feature tables |
| 12 | `12_sleep_like_quiescence_metrics.R` | Sleep-like quiescence metrics | Stage 01 metrics | Inactivity bout and quiescence summaries |
| 13 | `13_ethological_phase_organization.R` | Ethological phase organization | Stage 01 metrics | Phase contrast, timing, fragmentation, and recovery features |
| 14 | `14_systems_neuroscience_summary_dashboard.R` | Integrated systems neuroscience dashboard; HMM-domain heatmap uses animal-level g and repeated-measures contrasts | Stages 01, 04-13, optional proteomics | Feature matrix, audits, scorecards, dashboard panels, HMM-resolution sensitivity |
| 15 | `15_behavior_proteomics_integration.R` | Optional behavior-proteomics integration | Behavioral feature tables plus proteomics module data | Behavior-proteomics bridge tables and figures |
| 16 | `16_manuscript_behavior_report.R` | Export-only manuscript reporting layer | Canonical Stage 09 tables plus selected Stage 03/QC tables | Typed results, animal/prediction/movement source data, provenance, validation, and one source-data workbook |

## Primary vs Secondary

`09_early_prediction_model_ladder.R` is the primary early prediction analysis. It asks whether behavior during the first active 12 h after the first cage change predicts later CombZ. The canonical prediction resolution is 10-min bins; 5-min binning is a predefined resolution sensitivity. Stage 14 may retain its own 5-min integration backbone.

The fixed a priori behavior-only models are the mean-only intercept baseline, `Movement_mean`, and `Movement_mean + Movement_rmssd + Entropy_acf1`. Corresponding Sex-adjusted models are sensitivity analyses. RES/SUS `Group` is endpoint-derived and is excluded from all canonical primary models; larger Group-adjusted ladders remain supplementary/contextual compatibility outputs.

`10_systems_feature_prediction_ladder.R` is secondary. It extends the primary model ladder with broader systems-level feature domains and should be framed as an extension/sensitivity analysis rather than a replacement.

`03_primary_raw_movement_phase_stats.R` is the active secondary phenotype/group-characterization script for broad raw movement. Its displayed CON/RES/SUS pairwise comparisons are Holm-adjusted within each prespecified three-contrast panel. With `export_global_family_corrections = FALSE`, no wider global family-wise correction is exported or claimed. The wider Stage 03 scan is secondary/descriptive and does not replace the Stage 09 prospective analysis. The older `18_raw_movement_publication_trajectory.R` and `18b_raw_movement_broad_phase_stats.R` are archived.

`16_manuscript_behavior_report.R` is a thin assembly layer. It reads existing Stage 09, Stage 03, and QC tables and writes the canonical manuscript package to `analysis_ready/manuscript/behavior/`. The entry point is `Behavioral_Source_Data.xlsx`; compact CSV companions provide primary results, supplementary results, animal-level source data, held-out prediction source data, movement-phase source data, provenance, validation, and a SHA-256 manifest. It does not fit models or recalculate statistics. Stage 10/14 predictive claims, HMM/state, manifold, nonlinear, systems-composite, and behavior-proteomics outputs remain exploratory and are not promoted to the primary registry.

## Stage 08 HMM and Stage 14 state-architecture contract

Stage 08 reads the current Stage 01 `all_behavior_metrics.csv`, canonicalizes
`AnimalNum` with the shared `canonical_animal_id()` helper before sequence
construction, and inherits Group/Sex only from the current canonical Stage 01
roster. Alias phenotype conflicts, roster mismatches, and unknown animals fail
closed. The configured primary HMM is `10min_based`; `5min_based` is always run
as the required sensitivity. Exact inputs, code hashes, fit starts, identity
aliases/conflicts, sequence-quality exclusions, and resolution roles are
written beside each HMM output.

Stage 14 imports only those exact configured artifacts. Its ordered semantic
mapping is audited and remains operational rather than ethologically validated.
The historical state-architecture composite is computed after z-scaling within
`Sex x PhaseClass x CageChangeIndex`. If no social state is identified, the
social fraction is reported as zero and the composite reduction is stated
explicitly; no social state is forced.

For `Fig_sis_active_inactive_domain_heatmap`, heatmap color is animal-level
Hedges g after averaging each animal across included cage changes. Significance
comes from `DomainScore ~ Group * Sex + factor(CageChangeIndex) + (1 | AnimalNum)`
and `emmeans` contrasts within Sex. BH families comprise all estimable displayed
Domain x three Group contrasts within Sex x Phase x resolution. The sensitivity
table/figure compares 5- and 10-min model estimates, uncertainty, animal-level
effect sizes, and FDR results.

## Output Layout

The bounded Stage 03/09/10 migration uses:

- `tables/`
- `figures/`
- `audit/`

Canonical migrated roots are `analysis_ready/pipeline/03_movement_phase_stats/10min/`, `analysis_ready/pipeline/09_early_prediction/10min/`, and `analysis_ready/pipeline/10_systems_prediction/10min/`. Resolution tokens use `10min`, not `10min_based`. Future writes go only to the canonical location. Readers resolve the canonical path first and one documented legacy path second; fallback use warns and is recorded in provenance. Historical legacy directories are retained but are not rewritten by Stage 16.

`analysis_ready/README.md` and `analysis_ready/output_index.csv` are the human and machine-readable navigation entry points. Stages not listed above retain their current layout until a later migration.

Stage 09 and Stage 10 canonical writers flatten old category subfolders and suppress repeated writes of the same object to the same canonical filename. A conflicting attempt to write different objects to one canonical filename fails clearly.

## Running Everything

Use `run_all_analysis.R` from the repo root or the `Analysis/` folder. Optional stages are controlled through R options:

```r
options(
  mmm.run_optional_hmm = TRUE,
  mmm.run_systems_extension = TRUE,
  mmm.run_behavior_proteomics = FALSE,
  mmm.continue_on_error = FALSE
)
source("Analysis/run_all_analysis.R")
```

After the required canonical Stage 09 and selected Stage 03 outputs have been generated, assemble the manuscript report separately with:

```r
source("Analysis/16_manuscript_behavior_report.R")
```

Stage 16 may read existing legacy Stage 03/09 outputs during the transition, but it always writes the manuscript package only to `analysis_ready/manuscript/behavior/`.

## Old-to-New Filename Map

| Old filename | New filename / location |
|---|---|
| `00_tracking_qc_rfid_loss.R` | `00_qc_tracking_integrity.R` |
| `03_build_multiscale_behavior_metrics.R` | `01_build_multiscale_behavior_metrics.R` |
| `05_build_dyadic_rfid_contacts.R` | `02_build_dyadic_rfid_contacts.R` |
| `18c_raw_movement_broad_phase_stats_corrected.R` | `03_primary_raw_movement_phase_stats.R` |
| `06_burstiness_temporal_instability.R` | `04_temporal_instability.R` |
| `07_behavioral_state_space.R` | `05_behavioral_state_space.R` |
| `09_dynamic_social_networks.R` | `06_dynamic_social_networks.R` |
| `11_gamm_trajectory_features.R` | `07_gamm_trajectory_features.R` |
| `10_hmm_behavioral_states.R` | `08_hmm_behavioral_states_optional.R` |
| `08b_early_prediction_model_ladder.R` | `09_early_prediction_model_ladder.R` |
| `08c_systems_feature_prediction_ladder.R` | `10_systems_feature_prediction_ladder.R` |
| `15_behavioral_adaptation_kinetics.R` | `11_behavioral_adaptation_kinetics.R` |
| `16_sleep_like_inactivity_metrics.R` | `12_sleep_like_quiescence_metrics.R` |
| `17_ethological_phase_organization.R` | `13_ethological_phase_organization.R` |
| `12_systems_neuroscience_summary.R` | `14_systems_neuroscience_summary_dashboard.R` |
| `12_behavior_proteomics_integration.R` | `15_behavior_proteomics_integration.R` |
| `04_gamm_movement_proximity_phase_and_early_window.R` | `_archive/04_gamm_movement_proximity_phase_and_early_window.R` |
| `08_early_prediction_models.R` | `_archive/08_early_prediction_models.R` |
| `13_nonlinear_systems_dynamics.R` | `_archive/13_nonlinear_systems_dynamics.R` |
| `14_nextgen_behavioral_phenotyping.R` | `_archive/14_nextgen_behavioral_phenotyping.R` |
| `18_raw_movement_publication_trajectory.R` | `_archive/18_raw_movement_publication_trajectory.R` |
| `18b_raw_movement_broad_phase_stats.R` | `_archive/18b_raw_movement_broad_phase_stats.R` |
