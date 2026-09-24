# Behavioral analysis outputs

Start manuscript reporting at `manuscript/behavior/Behavioral_Source_Data.xlsx`.
Machine-readable result, source-data, provenance, validation, and manifest CSVs are beside the workbook.

Current Stage 01 metric/QC foundations live under `foundations/behavior_metrics/`; see `foundations/README.md`.
Current semantically named behavioral analyses live under `analyses/`; see its `README.md` for the active groups.
Stage-addressed migrated outputs live under `pipeline/` and use `tables/`, `figures/`, and `audit/`.
`output_index.csv` maps active and historical output groups to their producers and roles.
Stage 00 diagnostic runs live under `quality_control/tracking_integrity/`. The 2026-09-24 pooled-resolution run at that root and the isolated `runs/ten_second_review_20260924/` run are provisional: their row-based zero-movement thresholds flag all 111 animals and have not been validated for exclusions. Read `quality_control/tracking_integrity/REVIEW_STATUS.md` before using either. The numbered `00_qc_tracking_integrity/` tree is a May 2026 historical snapshot; Stage 16 and the release builder still read its optional tables.

Readers resolve canonical paths first and a single documented legacy path second. Any legacy fallback is warned and recorded in manuscript provenance; no newest-file guessing is allowed.
Historical output folders are retained and are not rewritten by Stage 16.

Stage 09 is the primary prospective layer. Stage 03 is secondary phenotype/group characterization. Stage 10/14, HMM/state, nonlinear, systems-composite, spatial, and behavior-proteomics analyses remain exploratory or separate unless a later reporting decision promotes a specific result.
