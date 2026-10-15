# Protected outputs: the guard of isolated runs and the round-level check

## What is protected
`mmm_protected_output_locations()` in `Functions/protected_outputs.R` returns `data.table(location, class, note)`. The classes are:

- **live_output**, under `<project root>/analysis_ready/`:
  - `analyses/` folders: `spatial_occupancy`, `cc4_grid_exposure`, `hmm_states`, `posthoc_cohort_followups`
  - `pipeline/` folders: `29_canonical_behavior_releases`, `29b_posthoc_con_contrasts`, `29c_posthoc_movement_combz_fit`, `30_exploratory_screen`, `32_behavior_exposure_adaptation`, `09_early_prediction`, `28_rfid_behavioral_domains`
  - `canonical`, `quality_control/{raw_acquisition,tracking_integrity}`, `history/tracking_integrity/10sec`, `foundations`
- **publication_copy**: `publication_ready/{tables,figures/single_panels}/spatial_occupancy`
- **input_raw_logs**: the raw antenna logs (`mmm_raw_antenna_root()`)
- **input**: `MMMSociability/raw_data` and `MMMSociability/data_versions`
- **isolated_runs**: the whole `<Behavior>/RANM` tree. Earlier runs of every tool, including the same tool, are protected. Only the current run's `runs/.tmp_<id>` and `runs/<id>` are left out.
- **manuscript**: `Exp9_manuscript/source_data` and `Exp9_manuscript/provenance`
- **repo_pin**:
  - `docs/spatial_exclusions/`
  - `Functions/behavior_analysis_config.R`
  - `docs/frozen_output_pins/`
  - `docs/behavior_output_producer_reruns/stage19_*.csv`
  - `docs/rfid_calibration/`
- **write_refused**: the project root, the Exp9_manuscript checkout and the MMMSociability repository. No isolated run may write into these. They are not snapshotted beyond the folders listed above.

## Snapshots
Every protected file is recorded by size and time stamp. sha256 is taken for:

- every repository pin file;
- every file whose name matches `manifest|ledger|registry|output_files_sha256|input_hashes|run[.]csv|BUNDLE_REGISTRY|00_manifest`.

On the live root this is about 5,200 files, of which about 170 are hashed (3.7 MB).

Windows thumbnail files (`Thumbs.db`, `desktop.ini`, `.DS_Store`) are not listed.

## Wiring an isolated builder (main block only)
```r
source_mmm_helper("protected_outputs.R")          # also sources qc_run_protection.R and frozen_input_guard.R if needed
base  <- mmm_isolated_output_root_check(<output root>, project_root)
guard <- mmm_protection_guard_begin(base, run_id, "<tool>", project_root)   # before any write
tmp   <- file.path(base, "runs", paste0(".tmp_", run_id))
# ... build everything in tmp ...
mmm_protection_guard_end(guard, file.path(tmp, "manifest"))   # writes protection_snapshot_before.csv, protection_check.csv
# then write manifest/output_files_sha256.csv (it lists both protection files) and rename tmp to runs/<run_id>
```

`begin()` refuses an output base that lies in, equals or contains a protected location:

- In RANM, the run's folders may not lie in, equal or contain an earlier run's folder or any other RANM entry.
- So a base inside an earlier run, or an existing run id, is refused.
- A base inside a `runs` folder (`<tool>/runs`, `<tool>/runs/x`) is refused, and so is an existing `runs/.tmp_<run_id>` unless the caller passes `resume = TRUE`.
- On a sandbox project root (`MMM_BEHAVIOR_PROJECT_ROOT`), the live `<Behavior>/RANM` runs and the live `Exp9_manuscript` are still refused as output bases (registry classes `isolated_runs_live` and `write_refused`); they are not snapshotted then.
- A new `RANM/<tool>`, or a new run of an existing tool, is accepted.

`begin()` also calls `mmm_frozen_guard_before()`; on a sandbox root it does nothing.

`end()` stops when a protected file was added or removed, or when its size or time stamp changed and its bytes cannot be confirmed. Specifically:

- An unhashed file whose size or time stamp changed counts as changed.
- A hashed file rewritten with identical bytes passes.
- A changed frozen pin also stops the run.

After a stop, nothing may be published from the `.tmp_` folder.

## Limits
- **Concurrent writes:** a file that another session writes into a protected location during a run also stops it. Examples are a concurrent RANM run, a manuscript import or a calibration document. The guard cannot tell such a write from a change made by the run itself. Run isolated builders one at a time, and do not write run logs into protected folders.
- **Path spelling:** use the same spelling (drive letter or UNC) for the output base and the project root.

## Round-level check
```
Rscript Testing/audits/verify_protected_outputs.R --tier=1 [--run-id=<id>] [--output-root=<absolute>] (--first-run | --baseline=<earlier run folder>)
```
Run it once at the start of a round with `--first-run` and once at the end with `--baseline=<the start run folder>`, with no
builder running. Without either flag, `baseline_diff` is `not_run` and the check FAILS: a run without a baseline compares only
the files that have a self-manifest or record, and `tables/unchecked_files.csv` lists the rest (their count is in `run.csv`).

- **Default output root:** `<Behavior>/RANM/protected_output_checks/runs/<id>`.
- **Run ids:** the default id needs committed code; `dev*` ids may run on uncommitted code.
- **Tier 1:** existence and recorded sizes, plus the hashes of the small manifests that the chain checks compare. About 12 s.
- **Tier 2:** sha256 of every listed and every protected file except the raw logs. About 5 GB; ask before running it.
- **Exit status:** 1 when any check fails.

| check | compared with |
|---|---|
| frozen_pins | the registered runs' pin lists and `docs/frozen_output_pins/` (118 files) |
| stage29_v101_release | its `audit/output_manifest.csv` |
| registered_runs | `audit/output_manifest.csv` of Stage 29 v1.0.0, 29b, 29c, 30, 32 v1.1 and the Stage 33 post hoc runs |
| stage19_record | `stage19_20261012.csv` overlaid by `stage19_20261012b.csv` (38 live files, the overlay's prior hashes, the 19 publication copies pinned by name in `VPO_STAGE19_COPY_FILES`: a missing copy fails) |
| stage31_runs | each of the seven Stage 31–31d folders' `manifest.csv` or `audit/output_manifest.csv`; no other folder |
| stage31_censored | `audit/output_manifest.csv` of the censored run(s) in `RANM/cc4_grid_exposure_censored` |
| ranm_runs | every RANM run's `manifest/output_files_sha256.csv`, with no unlisted file; every run of `VPO_RANM_RUNS` must exist (later runs: through `--baseline`) |
| stage00b_ledger_chain | `runs_ledger.csv` → each run's `output_files_sha256.csv` → its files; ledger rows = run folders; ledger prefix (with `--baseline`) |
| canonical_bundles | `BUNDLE_REGISTRY.csv` → `00_manifest.csv` → files |
| manuscript_imports | `Exp9_manuscript/provenance/source_manifests/*_manifest.csv` |
| recorded_inputs | the input hashes recorded by the pin lists and by `srs_v1.1_9b18624` |
| repo_pins | `SXM_RELEASED`, the S13 digest and the configuration byte pin (read from the tests), the calibration criteria and schedule pins |
| baseline_diff | the earlier run's `snapshot/baseline_snapshot.csv`; only new RANM run folders may appear (`--first-run`: recorded as the round's first snapshot; neither flag: `not_run`, a failure) |

Outputs:

- `tables/summary.csv`: PASS/FAIL per check and overall.
- `tables/check_<name>.csv`: one row per file.
- `snapshot/baseline_snapshot.csv`: the reference for a later `--baseline` run.

Files that have no self-manifest or record are covered only by the baseline snapshot; `tables/unchecked_files.csv` lists them
per run. They include `RANM/measurement_validation`, RANM tool-level files, the bundle `logs/` folders, Stage 29 `audit/` and
`ARCHIVE_*` files, Stage 00b `rec/` files, unpinned Stage 09/28 outputs, `canonical/behavior_config` and
`canonical/later_outcome_combz`, tracking-integrity and foundations files, Exp9_manuscript provenance and non-imported source
data, the raw logs (size and time stamp only) and repository pin files without a content pin.

Reference runs of the third round (commit 4aebc90): `vpo_t1_20261014_2327_4aebc90` (first run: 13 of 13 checks PASS) and
`vpo_t1_20261014_2333_4aebc90` (baseline comparison: 14 of 14 PASS; the only differences are the five new isolated runs:
s19c_v0.1_4aebc90, ckr_v1.0_4aebc90, cc4e_v1.0_4aebc90, cc4e_v1.0_4aebc90_isens and the first check run).

Reference runs of the fourth round (2026-10-15): `vpo_t1_20261015_0016_28c1d6f` (first run, PASS) and
`vpo_t1_20261015_0308_1a55644` (baseline comparison): every protected output unchanged; the differences are the new isolated
runs (Stage 31b-31d QC reruns, their comparison, the Stage 19 candidate v0.2) and three repository files added by the
approved calibration amendment (`acp_criteria_v1.1.csv`, `schedule_one_day_v2.csv`, `make_schedule_one_day_v2.R`), which the
check reports as added for review. Stage 31b-31d runs in RANM are checked against their root `manifest.csv`.
