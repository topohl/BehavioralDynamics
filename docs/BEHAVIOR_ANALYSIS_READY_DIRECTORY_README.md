# Behavioral analysis outputs

**Start here.** The manuscript-facing behavioural results are frozen, hash-gated exports under `canonical/`:
- `canonical/behavior_bundle/ebb_v101_20260929_b2ce507/` is the Stage 29 v1.0.1 release bundle that Exp9_manuscript imports and renders Figure 1 from. `canonical/figure_support_bundle/` and `canonical/stage30_figure_bundle/` hold the other imported bundles; each folder's `BUNDLE_REGISTRY.csv` lists its bundles.
- `canonical/later_outcome_combz/tables/` holds the later outcome CombZ and the RES/SUS classification.
- The registered runs are each written once behind identity gates: `pipeline/29_canonical_behavior_releases/`, `pipeline/29b_posthoc_con_contrasts/`, `pipeline/30_exploratory_screen/` and `pipeline/32_behavior_exposure_adaptation/`. Their registries are under `canonical/*_registry/`, the frozen configuration under `canonical/behavior_config/`.

`output_index.csv` maps every output group to its producer, role, status and runner; the repository's `Analysis/STAGE_INVENTORY.csv` lists every script. Both this README and the index are written by `Maintenance/Refresh-BehaviorOutputIndex.R` (backup first), never by hand.

**Pinned inputs.** `foundations/behavior_metrics/10min_based/all_behavior_metrics.csv` (Stage 01), `pipeline/09_early_prediction/10min/tables/model_ladder_input.csv` (Stage 09) and the `acute_window_*` tables under `pipeline/28_rfid_behavioral_domains/` (Stage 28) are inputs that the registered runs pinned by sha256. Never rewrite them in place; the pipeline's frozen-input guard refuses to.

**Descriptive and exploratory layers.**
- `analyses/systems_dashboard/5min/`: the Stage 14 dashboard and domain heatmaps, rebuilt in a guarded sandbox and promoted with a producer-rerun record.
- `analyses/cc4_grid_exposure/<run-id>/`: Stage 31 CC4 runs.
- `pipeline/20_first_night_gamm/` and `pipeline/22_repeated_cagechange_acute_gamm/`: within-night GAMM profiles, kept as descriptive Extended Data candidates without group inference.
- The other semantically named analyses live under `analyses/`; see its `README.md` for the active groups.

**Foundations and history.** Current Stage 01 metric/QC foundations live under `foundations/behavior_metrics/`; see `foundations/README.md`. Their numbered `03_derived_metrics/` original, including the Stage 19 table and audit originals, is retained unchanged under `history/original_layout/03_derived_metrics/`; the Stage 19 model and figure originals are under `history/original_layout/04_model_outputs/` and `05_figures/`. The Stage 15 proteomics inputs are under `foundations/proteomics_module_scores/`.
Older resolution copies are organized by scientific family under `history/`; see its `README.md`. Every other top-level tree of the original numbered layout, including retired and quarantined trees, is retained unchanged under `history/original_layout/` with its own archive receipt.
`history/retired/` holds the outputs of retired producers (GAMM Stages 21 and 23-26, the Stage 27 figure trees, the Stage 16 report package and the 2026-09-07 leftovers of Stages 20 and 22), moved unchanged with a manifest and a receipt under `_migration_control/retired_outputs/`.
Stage-addressed outputs live under `pipeline/` and use `tables/`, `figures/`, and `audit/`.
Stage 00 diagnostic runs live under `quality_control/tracking_integrity/`. The 2026-09-24 pooled-resolution run at that root and the isolated `runs/ten_second_review_20260924/` run are provisional: their row-based zero-movement thresholds flag all 111 animals and have not been validated for exclusions. Read `quality_control/tracking_integrity/REVIEW_STATUS.md` before using either. The May 2026 historical snapshot from the numbered `00_qc_tracking_integrity/` tree is under `history/tracking_integrity/10sec/`.

Readers resolve canonical paths first and a single documented legacy path second. Any legacy fallback is warned; no newest-file guessing is allowed. Historical output folders are retained and never rewritten.

Stage 29 (with 29b, 30 and 32) is the registered layer the manuscript uses, and Stage 09 its prospective prediction input. Stage 03, Stage 10, Stage 14, HMM/state, nonlinear, systems-composite, spatial and behaviour-proteomics analyses remain exploratory or separate unless a later reporting decision promotes a specific result.
