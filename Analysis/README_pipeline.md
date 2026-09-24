# MMMSociability Analysis Pipeline

This folder is organized as a staged, reviewer-safe pipeline. The scripts remain modular; no scientific models were merged. The staged order below is the intended run order for manuscript-facing analyses.

## Run Order

| Stage | Script | Role | Inputs | Main outputs |
|---:|---|---|---|---|
| 00 (manual diagnostic) | `00_qc_tracking_integrity.R` | Provisional RFID/tracking integrity QC; requires an explicit single resolution and new run ID | Stage 01 10-second metrics | QC tables, Excel report, QC figures; no exclusion decision |
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

Canonical migrated roots are `analysis_ready/pipeline/03_movement_phase_stats/10min/`, `analysis_ready/pipeline/09_early_prediction/10min/`, and `analysis_ready/pipeline/10_systems_prediction/10min/`. Resolution tokens use `10min`, not `10min_based`. Future writes go only to the canonical location. Stage 16 reads Stage 03 from the canonical path only: its eight historical counterparts differ and cannot safely substitute for missing current files. The documented Stage 09 fallback still warns and is recorded in provenance. Historical legacy directories are retained but are not rewritten by Stage 16.

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

Stage 16 may read documented legacy Stage 09 outputs during the transition. Stage 03 reporting requires the current canonical files. The manuscript package is written only to `analysis_ready/manuscript/behavior/`.

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
| `13_nonlinear_systems_dynamics.R` | `_supporting/13_nonlinear_systems_dynamics.R` |
| `14_nextgen_behavioral_phenotyping.R` | `_supporting/14_nextgen_behavioral_phenotyping.R` |
| `18_raw_movement_publication_trajectory.R` | `_archive/18_raw_movement_publication_trajectory.R` |
| `18b_raw_movement_broad_phase_stats.R` | `_archive/18b_raw_movement_broad_phase_stats.R` |

---

# Stages 19-27, the endpoint producer, and what the runner does not cover

The Run Order table above stops at Stage 16. It predates the stages that now own
most manuscript claims. This section covers the rest.

## Two layers, deliberately separate

**Scientific producers** fit models and own claims. **Manuscript assemblers**
read already-validated outputs and fit nothing; two of them are test-enforced to
contain no model call at all.

| Stage | Script | Layer | Owns |
|---:|---|---|---|
| — | `build_later_outcome_combz.R` | canonical endpoint | CombZ and the CON/RES/SUS assignment. Hard-stops unless it reproduces the upstream workbook exactly. |
| 19 | `19_spatial_occupancy_maps.R` | producer | Reader-occupancy maps. Effect sizes only, no p/q. |
| 20 | `20_first_night_gamm.R` | producer | First-night Active trajectory (`CLAIM_GAMM_01`, `_02`). |
| 21 | `21_cc1_active_longitudinal_gamm.R` | producer | Nights within CC1. |
| 22 | `22_repeated_cagechange_acute_gamm.R` | producer | CC1→CC4 acute adaptation (`CLAIM_GAMM_04`, `_05`). |
| 23 | `23_first_inactive_gamm.R` | producer | First Inactive Markov activation (`CLAIM_GAMM_06`). |
| 24 | `24_cc1_inactive_longitudinal_gamm.R` | producer | CC1 Inactive nights. All contrasts null. |
| 25 | `25_repeated_cagechange_inactive_gamm.R` | producer | Repeated Inactive. No supported adaptation. |
| 26 | `26_build_gamm_manuscript_outputs.R` | assembler | GAMM manuscript/Extended Data products. |
| 27 | `27_build_behavior_main_figure.R` | assembler | The four-panel behavior main figure. |

## Stage 28 — four core raw-RFID behavioural domains

`28_rfid_behavioral_domains.R` is a **secondary, descriptive** characterisation
and is run deliberately and individually, like 16 and 19-27. It is additive: it
neither modifies nor re-runs Stage 01 or Stage 09, and it does not touch the
legacy five-domain first-night producer
(`Functions/first_night_domain_driver.R`), whose artifacts remain the
conservative flat-family sensitivity.

It produces two analyses over four CORE domains — spatial entropy dynamics,
social-spatial organization, cross-channel behavioral volatility, and movement
output:

1. **First night (CC1)** — `lmer(~ Group + Batch + (1|CageEpochID))` within Sex,
   with legacy-LM, batch-LM and CR2 cluster-robust variants reported alongside.
   Multiplicity is hierarchical: BH over exactly four domain omnibus Group tests
   per Sex, then Holm over three pairwise contrasts inside a *supported* domain.
2. **Longitudinal (CC1-CC4)** — equivalent acute 12 h windows after each cage
   change, scored with scaling parameters **fixed** from the CC1 within-Sex
   distribution. The primary adaptation test is the joint `Group x CageChange`
   interaction.

The window rule has exactly one implementation in the repository:
`Functions/rfid_acute_window_helpers.R` calls
`mmm_select_first_night_window()` once per cage change rather than
reimplementing it, so the Stage 09 parity gate still applies unchanged.

Supporting audits: `Testing/audits/audit_rfid_domain_construct_blind.R`
(phenotype-blind construct audit, run before any group inference is
interpreted), `audit_rfid_legacy_vs_new_domains.R`, and
`audit_rfid_leading_bin_seed_sensitivity.R`. Contract:
`Testing/tests/test_rfid_domain_contract.R`. See
`docs/RFID_FOUR_DOMAIN_RECONCILIATION.md`.

## The runner covers stages 01-15 only

`run_all_analysis.R` registers stages 01-15. Stage 00 is an unvalidated
manual diagnostic; its row-count thresholds are not chip-loss or exclusion
criteria. It now requires a single resolution and a fresh run ID, for example:

```powershell
Rscript Analysis/00_qc_tracking_integrity.R --input-scale=10sec_based --run-id=review_YYYYMMDD
```

Stages 16, 19-27 and 28 and the endpoint producer are run **deliberately and individually**.

This is a declaration, not a backlog. Stage 27's absence is asserted by
`Testing/tests/test_behavior_main_figure_contracts.R`: an assembler must not be
swept into a bulk re-run of the scientific pipeline, because re-running it is a
publication act, not an analysis act. Treat 26 and the endpoint producer the
same way.

`analysis_ready/output_index.csv` — generated by Stage 16 — is the
machine-readable version of this table, including a `runner_registration`
column. Prefer it over this README when the two disagree.

## Logical stage number ≠ output directory number

Several scripts were renumbered without moving their output directory, so the
number in the path is a historical accident. **The directory number does not
identify the stage.**

| Script | Writes into |
|---|---|
| `00_qc_tracking_integrity.R` | `analysis_ready/quality_control/tracking_integrity/runs/<new_run_id>/` for provisional single-resolution diagnostics; the May 2026 snapshot stays under `00_qc_tracking_integrity/` as an optional historical source |
| `01_build_multiscale_behavior_metrics.R` | `analysis_ready/foundations/behavior_metrics/` (numbered originals retained) |
| `02_build_dyadic_rfid_contacts.R` | `analysis_ready/analyses/dyadic_contacts/` (historical source retained) |
| `06_dynamic_social_networks.R` | `analysis_ready/analyses/dynamic_social_networks/5min/` (older resolutions retained under `06_behavioral_dynamics/`) |
| `07_gamm_trajectory_features.R` | `analysis_ready/analyses/gamm_trajectory_features/10min/` (historical 30-minute input retained) |
| `05_behavioral_state_space.R` | `analysis_ready/analyses/behavioral_state_space/5min/` (older resolutions retained) |
| `08_hmm_behavioral_states_optional.R` | `analysis_ready/analyses/hmm_states/{10min,5min}/` (originals retained for historical audits) |
| `04_temporal_instability.R` | `analysis_ready/analyses/temporal_instability/10sec/` (older resolutions retained) |
| `15_behavior_proteomics_integration.R` | `analysis_ready/analyses/behavior_proteomics/proteomics_mnn_{primary,sensitivity}/` (old map and outputs retained) |
| `10` | other children of `analysis_ready/06_behavioral_dynamics/` |
| `11_behavioral_adaptation_kinetics.R` | `analysis_ready/analyses/adaptation_kinetics/10min/` (older five-minute tree retained) |
| `12_sleep_like_quiescence_metrics.R` | `analysis_ready/analyses/sleep_like_inactivity/10min/` (older five-minute tree retained) |
| `13_ethological_phase_organization.R` | `analysis_ready/analyses/phase_organization/10min/` (older five-minute tree retained) |
| `14_systems_neuroscience_summary_dashboard.R` | `analysis_ready/analyses/systems_dashboard/5min/` (numbered dashboard and separate audit originals retained) |
| `_supporting/13_nonlinear_systems_dynamics.R` | `analysis_ready/analyses/nonlinear_dynamics/5min/` (numbered original retained) |
| `_supporting/14_nextgen_behavioral_phenotyping.R` | `analysis_ready/analyses/systems_phenotyping/5min/` (numbered original retained) |
| `Testing/audits/audit_inactive_phase_qc_redesign.R` | `analysis_ready/analyses/inactive_phase_qc_audit/` (manual QC proposal; numbered original retained) |

Stages 03, 09 and 20-27 write under `analysis_ready/pipeline/<stage>_<name>/`.
The remaining numbered output roots need separate dependency review; see
`docs/BEHAVIOR_OUTPUT_MIGRATION.md`.

## Resolutions are declared, not discovered

Stages 11, 12 and 13 declare `10min_based` and write nothing else. Older
`5min_based` trees for them still exist on disk but **no current producer can
regenerate them**, and they predate the 2026-09-03 exact-phase-classifier fix
(commit `12f3e76`), so they contain Inactive epochs mislabelled Active.

Stage 14 previously preferred some of those trees because it selected inputs by
first-path-that-exists. It now prefers each producer's declared resolution and
hard-stops on a pre-fix phase-dependent input.
`Testing/tests/test_stage14_input_resolution_contract.R` locks both halves.

**Never point a stage at a different resolution to make a run succeed.** Re-run
the owning producer at its declared resolution instead.

## Supporting and archived

`_supporting/` holds `13_nonlinear_systems_dynamics.R` and
`14_nextgen_behavioral_phenotyping.R`. Their filename numbers correspond to no
current logical stage. They are exploratory and unregistered in the runner.
Their active five-minute outputs are under `analysis_ready/analyses/` with
semantic names; the numbered originals remain available for provenance.

`_archive/` holds superseded producers, including the `18*` movement lineage
that `03_primary_raw_movement_phase_stats.R` replaced. Their output trees are
retained but read by nothing.
