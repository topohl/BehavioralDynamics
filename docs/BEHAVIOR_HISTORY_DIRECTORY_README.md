# Historical RFID analysis copies

These directories hold hash-verified copies of older runs. They are named by
scientific family and time resolution so readers can find them without knowing
the numbered output layout. An activated migration receipt under
`../_migration_control/` selects each copy for current optional readers;
`../output_index.csv` records its producer role and status. The originals are
retained unchanged under `original_layout/`.

`original_layout/` is different: it holds the top-level trees of the original
output layout, moved unchanged rather than copied, each under its own archive
receipt in `../_migration_control/numbered_root_archive/`. Current analyses
read their semantic copies under `../foundations/`, `../analyses/` and this
directory, but their receipt checks require this archive, so never move,
rename, or edit it. Historical replays read it through the receipts, as does
the SLEAPanalyzer BORIS metadata script for `03_derived_metrics/`.

`retired/` holds the outputs of producers retired on 2026-10-05, moved
unchanged (not copied), each with a manifest and a receipt in
`../_migration_control/retired_outputs/`. No current code reads them, and the
repository's path registry refuses any path inside them. The reasons are in the
repository's `docs/LEGACY_AND_GAMM_RETIREMENT_2026-10-05.md`.

Forty-two archived files have full paths of 260 to 270 characters: 27 in
`06_behavioral_dynamics/` and `12_systems_neuroscience_summary/`, 3 in
`13_nonlinear_systems_dynamics/`, 9 in `14_nextgen_behavioral_phenotyping/`
and 3 in `18c_raw_movement_broad_phase_stats_corrected/`. On Windows hosts
without long-path support, R and File Explorer cannot open them here.
Thirty-three have hash-identical copies in active groups. No code reads the other nine: six
historical HMM audit CSVs in
`original_layout/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture/first_night_domain_heatmap/`
and three superseded Stage 18c figures. Open those with PowerShell 7 or on a
host with long paths enabled.

| Folder | Historical role |
| --- | --- |
| `social_networks/{10sec,1min,10min,30min}/` | Older dynamic social-network runs, separate from the current five-minute analysis under `../analyses/`. |
| `state_space/{1min,10min}/` | Older behavioral state-space runs, separate from the current five-minute analysis. |
| `temporal_instability/{1min,5min}/` | Older temporal-instability runs. The five-minute branch is a historical optional Stage 15 input. |
| `gamm_features/30min/` | Older GAMM trajectory features used as a historical optional Stage 15 input. |
| `tracking_integrity/10sec/` | The May 2026 Stage 00 tracking-integrity QC snapshot (8 files). The retired Stage 16 package and release builder read two of its tables; their lineage to the current Stage 01 is unverified. |
| `retired/21_cc1_active_longitudinal_gamm/`, `23_first_inactive_gamm/`, `24_cc1_inactive_longitudinal_gamm/`, `25_repeated_cagechange_inactive_gamm/`, `26_gamm_manuscript_outputs/` | The GAMM Stages 21 and 23-26 (2026-09-22 outputs and older leftovers), formerly under `../pipeline/`. The `audit/gaussian_log1p_invalid/` folders of 23-25 are invalid inference; never quote them. |
| `retired/27_behavior_main_figure/` | The Stage 27 main-figure and candidate trees, formerly `../pipeline/27_behavior_main_figure/`. |
| `retired/manuscript_behavior/` | The 2026-09-22 Stage 16 package (`Behavioral_Source_Data.xlsx` and CSV companions), formerly `../manuscript/behavior/`. |
| `retired/20_first_night_gamm_20260907/`, `retired/22_repeated_cagechange_acute_gamm_20260907/` | The 2026-09-07 outputs of superseded Stage 20 and 22 runs (old classification), moved out of the live `../pipeline/20_*` and `22_*` folders. |
| `original_layout/03_derived_metrics/` | Complete numbered Stage 01 and Stage 19 root, moved unchanged under its archive receipt; not a copy. |
| `original_layout/06_behavioral_dynamics/` | Complete numbered root of the Stage 02, 04–08 and Stage 15 originals, including those of the copies above; moved unchanged under its archive receipt. |
| `original_layout/12_systems_neuroscience_summary/` | Complete numbered Stage 14 root: the original dashboard, first-night outputs, RFID audit families and the 183-file HMM audit tree; moved unchanged under its archive receipt. |
| `original_layout/00_qc_tracking_integrity/` | Original of `tracking_integrity/10sec/`. |
| `original_layout/03_primary_raw_movement_phase_stats/` | Pre-migration Stage 03 outputs; the current Stage 03 writes `../pipeline/03_movement_phase_stats/`. |
| `original_layout/04_model_outputs/`, `original_layout/05_figures/` | Stage 19 model and figure originals of `../analyses/spatial_occupancy/models/` and `figures/`. |
| `original_layout/13_nonlinear_systems_dynamics/`, `original_layout/14_nextgen_behavioral_phenotyping/` | Originals of `../analyses/nonlinear_dynamics/5min/` and `../analyses/systems_phenotyping/5min/`. |
| `original_layout/15_behavioral_adaptation_kinetics/`, `16_sleep_like_inactivity_metrics/`, `17_ethological_phase_organization/` | Stage 11–13 roots: the ten-minute originals of the `../analyses/` copies, and the older five-minute branches, which exist only here and predate the exact-phase-classifier fix. |
| `original_layout/16_manuscript_behavior_report/` | Superseded August 2026 Stage 16 export; the later 2026-09-22 package is under `retired/manuscript_behavior/`. |
| `original_layout/18_raw_movement_publication_trajectory/`, `18b_raw_movement_broad_phase_stats/`, `18c_raw_movement_broad_phase_stats_corrected/` | Superseded raw-movement runs that Stage 03 replaced; nothing reads them. |
| `original_layout/proteomics/` | May 2026 proteomics module-score inputs; Stage 15 reads their copy in `../foundations/proteomics_module_scores/`. |
| `original_layout/_archive_stale_stage10_outputs/`, `_archive_stale_stage27_candidates/`, `_quarantine_legacy_s09/` | Retired trees kept as evidence and never read. The quarantined Stage 09 trees are not to be restored. |

Stage 10 discovers candidate files from the receipt-selected analysis and
history groups. The copy process preserved each file's SHA-256, but did not
rerun the analyses or resolve scientific source-validity questions. In
particular, the older temporal and GAMM inputs do not validate the existing
Stage 15 integrations. Manuscript authority is recorded in the frozen bundles
under `../canonical/` and in the repository's manuscript registry, read with its
addendum. `../../publication_ready/` holds spatial-occupancy panels and tables.
