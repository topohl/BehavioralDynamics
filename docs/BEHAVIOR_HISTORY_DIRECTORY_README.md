# Historical RFID analysis copies

These directories hold hash-verified copies of older resolution runs. They are
named by scientific family and time resolution so readers can find them without
knowing the numbered output layout. The original files remain under
`../06_behavioral_dynamics/` as recorded provenance. An activated migration
receipt under `../_migration_control/` selects each copy for current optional
readers; `../output_index.csv` records its producer role and status.

`original_layout/` is different: it holds complete numbered output roots
moved unchanged, not copies, each under a root archive receipt in
`../_migration_control/numbered_root_archive/`; currently only
`03_derived_metrics/` (53 files). Current analyses read
`../foundations/behavior_metrics/` and `../analyses/spatial_occupancy/`, but
their receipt checks require this archive, so never move, rename, or edit it.
Historical replays and the SLEAPanalyzer BORIS metadata script read it
through the receipt.

| Folder | Historical role |
| --- | --- |
| `social_networks/{10sec,1min,10min,30min}/` | Older dynamic social-network runs, separate from the current five-minute analysis under `../analyses/`. |
| `state_space/{1min,10min}/` | Older behavioral state-space runs, separate from the current five-minute analysis. |
| `temporal_instability/{1min,5min}/` | Older temporal-instability runs. The five-minute branch is a historical optional Stage 15 input. |
| `gamm_features/30min/` | Older GAMM trajectory features used as a historical optional Stage 15 input. |
| `original_layout/03_derived_metrics/` | Complete numbered Stage 01 and Stage 19 root, moved unchanged under its archive receipt; not a copy. |

Stage 10 discovers candidate files from the receipt-selected analysis and
history groups. The copy process preserved each file's SHA-256, but did not
rerun the analyses or resolve scientific source-validity questions. In
particular, the older temporal and GAMM inputs do not validate the existing
Stage 15 integrations. Manuscript and
release authority is recorded separately under `../manuscript/`,
`../../publication_ready/`, and the repository's manuscript registry.
