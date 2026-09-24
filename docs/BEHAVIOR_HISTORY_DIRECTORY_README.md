# Historical RFID analysis copies

These directories hold hash-verified copies of older resolution runs. They are
named by scientific family and time resolution so readers can find them without
knowing the numbered output layout. The original files remain under
`../06_behavioral_dynamics/` as recorded provenance. An activated migration
receipt under `../_migration_control/` selects each copy for current optional
readers; `../output_index.csv` records its producer role and status.

| Folder | Historical role |
| --- | --- |
| `social_networks/{10sec,1min,10min,30min}/` | Older dynamic social-network runs, separate from the current five-minute analysis under `../analyses/`. |
| `state_space/{1min,10min}/` | Older behavioral state-space runs, separate from the current five-minute analysis. |
| `temporal_instability/{1min,5min}/` | Older temporal-instability runs. The five-minute branch is a historical optional Stage 15 input. |
| `gamm_features/30min/` | Older GAMM trajectory features used as a historical optional Stage 15 input. |

Stage 10 discovers candidate files from the receipt-selected analysis and
history groups. The copy process preserved each file's SHA-256, but did not
rerun the analyses or resolve scientific source-validity questions. In
particular, the older temporal and GAMM inputs do not validate the existing
Stage 15 integrations. Manuscript and
release authority is recorded separately under `../manuscript/`,
`../../publication_ready/`, and the repository's manuscript registry.
