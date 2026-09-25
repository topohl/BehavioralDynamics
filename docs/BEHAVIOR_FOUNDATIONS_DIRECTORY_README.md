# Behavioral foundations

`behavior_metrics/` contains the receipt-selected Stage 01 metric and QC
products used by current analyses. Its six resolution directories contain
`all_behavior_metrics.csv` and a duration-QC table; `qc/` contains eight
shared Stage 01 QC tables. See `../output_index.csv` for the producer and
retained source path.

These 20 files are hash-verified copies of the September 22 Stage 01 run.
The activation receipt is
`../_migration_control/behavior_metrics_foundation.json`. The numbered
original is retained unchanged at
`../history/original_layout/03_derived_metrics/`; its archive receipt is
`../_migration_control/numbered_root_archive/03_derived_metrics.json`. Its
older cross-scale identity reports, Stage 19 spatial outputs, original run
manifests, and the unpromoted 2026-09-24 `qc/first_night_seed_provenance.csv`
were not copied into `behavior_metrics/`.

The manual cross-scale identity validator writes any new report to
`../analyses/cross_scale_identity_validation/`; it has not been rerun as
part of this folder migration.
