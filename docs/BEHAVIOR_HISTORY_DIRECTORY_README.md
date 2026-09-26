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
`14_nextgen_behavioral_phenotyping/` has not moved yet and is still at the top
level of `analysis_ready/`.

Thirty-three archived files have full paths of 260 to 266 characters: 27 in
`06_behavioral_dynamics/` and `12_systems_neuroscience_summary/`, 3 in
`13_nonlinear_systems_dynamics/` and 3 in
`18c_raw_movement_broad_phase_stats_corrected/`. On Windows hosts without
long-path support, R and File Explorer cannot open them here. Twenty-four have
hash-identical copies in active groups. No code reads the other nine: six
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
| `tracking_integrity/10sec/` | The May 2026 Stage 00 tracking-integrity QC snapshot (8 files). Two tables are optional Stage 16 and release inputs; their lineage to the current Stage 01 is unverified. |
| `original_layout/03_derived_metrics/` | Complete numbered Stage 01 and Stage 19 root, moved unchanged under its archive receipt; not a copy. |
| `original_layout/06_behavioral_dynamics/` | Complete numbered root of the Stage 02, 04–08 and Stage 15 originals, including those of the copies above; moved unchanged under its archive receipt. |
| `original_layout/12_systems_neuroscience_summary/` | Complete numbered Stage 14 root: the original dashboard, first-night outputs, RFID audit families and the 183-file HMM audit tree; moved unchanged under its archive receipt. |
| `original_layout/00_qc_tracking_integrity/` | Original of `tracking_integrity/10sec/`. |
| `original_layout/03_primary_raw_movement_phase_stats/` | Pre-migration Stage 03 outputs; the current Stage 03 writes `../pipeline/03_movement_phase_stats/`. |
| `original_layout/04_model_outputs/`, `original_layout/05_figures/` | Stage 19 model and figure originals of `../analyses/spatial_occupancy/models/` and `figures/`. |
| `original_layout/13_nonlinear_systems_dynamics/` | Original of `../analyses/nonlinear_dynamics/5min/`. |
| `original_layout/15_behavioral_adaptation_kinetics/`, `16_sleep_like_inactivity_metrics/`, `17_ethological_phase_organization/` | Stage 11–13 roots: the ten-minute originals of the `../analyses/` copies, and the older five-minute branches, which exist only here and predate the exact-phase-classifier fix. |
| `original_layout/16_manuscript_behavior_report/` | Superseded August 2026 Stage 16 export; the current package is `../manuscript/behavior/`. |
| `original_layout/18_raw_movement_publication_trajectory/`, `18b_raw_movement_broad_phase_stats/`, `18c_raw_movement_broad_phase_stats_corrected/` | Superseded raw-movement runs that Stage 03 replaced; nothing reads them. |
| `original_layout/proteomics/` | May 2026 proteomics module-score inputs; Stage 15 reads their copy in `../foundations/proteomics_module_scores/`. |
| `original_layout/_archive_stale_stage10_outputs/`, `_archive_stale_stage27_candidates/`, `_quarantine_legacy_s09/` | Retired trees kept as evidence and never read. The quarantined Stage 09 trees are not to be restored. |

Stage 10 discovers candidate files from the receipt-selected analysis and
history groups. The copy process preserved each file's SHA-256, but did not
rerun the analyses or resolve scientific source-validity questions. In
particular, the older temporal and GAMM inputs do not validate the existing
Stage 15 integrations. Manuscript and
release authority is recorded separately under `../manuscript/`,
`../../publication_ready/`, and the repository's manuscript registry.
