# Historical RFID analysis copies

These directories hold hash-verified copies of older resolution runs. They are
named by scientific family and time resolution so readers can find them without
knowing the numbered output layout. The original files are retained unchanged
under `original_layout/06_behavioral_dynamics/` as recorded provenance. An activated migration
receipt under `../_migration_control/` selects each copy for current optional
readers; `../output_index.csv` records its producer role and status.

`original_layout/` is different: it holds complete numbered output roots
moved unchanged, not copies, each under a root archive receipt in
`../_migration_control/numbered_root_archive/`: `03_derived_metrics/`
(53 files), `06_behavioral_dynamics/` (1,469 files) and
`12_systems_neuroscience_summary/` (702 files). Current analyses read their
semantic copies under `../foundations/`, `../analyses/` and this directory,
but their receipt checks require this archive, so never move, rename, or edit
it. Historical replays read it through the receipts, as does the SLEAPanalyzer
BORIS metadata script for `03_derived_metrics/`.

Twenty-seven archived files have full paths of 260 to 265 characters. On
Windows hosts without long-path support, R and File Explorer cannot open them
here. Twenty-one have hash-identical copies in active groups. The other six
are historical HMM audit CSVs in
`original_layout/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture/first_night_domain_heatmap/`
that no code reads. Open those with PowerShell 7 or on a host with long paths
enabled.

| Folder | Historical role |
| --- | --- |
| `social_networks/{10sec,1min,10min,30min}/` | Older dynamic social-network runs, separate from the current five-minute analysis under `../analyses/`. |
| `state_space/{1min,10min}/` | Older behavioral state-space runs, separate from the current five-minute analysis. |
| `temporal_instability/{1min,5min}/` | Older temporal-instability runs. The five-minute branch is a historical optional Stage 15 input. |
| `gamm_features/30min/` | Older GAMM trajectory features used as a historical optional Stage 15 input. |
| `original_layout/03_derived_metrics/` | Complete numbered Stage 01 and Stage 19 root, moved unchanged under its archive receipt; not a copy. |
| `original_layout/06_behavioral_dynamics/` | Complete numbered root of the Stage 02, 04–08 and Stage 15 originals, including those of the copies above; moved unchanged under its archive receipt. |
| `original_layout/12_systems_neuroscience_summary/` | Complete numbered Stage 14 root: the original dashboard, first-night outputs, RFID audit families and the 183-file HMM audit tree; moved unchanged under its archive receipt. |

Stage 10 discovers candidate files from the receipt-selected analysis and
history groups. The copy process preserved each file's SHA-256, but did not
rerun the analyses or resolve scientific source-validity questions. In
particular, the older temporal and GAMM inputs do not validate the existing
Stage 15 integrations. Manuscript and
release authority is recorded separately under `../manuscript/`,
`../../publication_ready/`, and the repository's manuscript registry.
