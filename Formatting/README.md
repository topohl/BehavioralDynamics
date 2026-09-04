# Formatting

Raw-data preprocessing and QC that runs **upstream of** the staged analysis
pipeline in `Analysis/`.

## Why this is not part of `run_all_analysis.R`

`Analysis/run_all_analysis.R` starts at Stage 00 and assumes preprocessed
position data already exist. Raw preprocessing is deliberately kept outside the
runner because:

- it reads the raw `AnimalPos` exports, which live with the experiment rather
  than with the derived analysis tree;
- it is expensive and parallel, and is re-run only when the raw export or the
  preprocessing contract changes — not on every analysis run;
- re-running it rewrites the canonical input of Stage 01, so it must be an
  explicit, deliberate act rather than a side effect of running the pipeline.

The boundary is therefore: **Formatting produces `preprocessed_data/`; Stage 01
consumes it.**

---

## Active entry points

### `E9_SIS_AnimalPos-preprocessing_parallell.r`

The active E9 main-dataset preprocessing entry point. Reads
`raw_data/B*/E9_SIS_B*_CC*_AnimalPos.csv`, applies the position map and epoch
rules, and writes

```text
<MMM_DATA_DIR>/preprocessed_data/
```

which is exactly the `existing_default_input_dir` that
`Analysis/01_build_multiscale_behavior_metrics.R` and
`Analysis/14_systems_neuroscience_summary_dashboard.R` read.

Paths are overridable without editing the script:

```text
MMM_REPO_DIR   default C:/Users/topohl/Documents/GitHub/MMMSociability
MMM_DATA_DIR   default S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/MMMSociability
```

> The `MMMSociability` inside `MMM_DATA_DIR` is a **local data path component**,
> not the repository name. It must not be renamed when the GitHub repository is
> referred to as `BehavioralDynamics`.

It sources two helper files and hard-asserts their presence:

```r
function_files <- file.path(repo_dir, "Functions",
                            c("E9_SIS_AnimalPos-functions.R",
                              "animalpos_preprocessing_helpers.R"))
stopifnot(all(file.exists(function_files)))
```

### `01_preprocess_cookiehab_animalpos.R`

Cookiehab preprocessing, sourced by
`Analysis/run_cookiehab_preprocessing_and_metrics.R`, which then runs Stages 01
and 02 on that dataset. Separate experiment, same metric machinery.

### `00a_raw_tracking_qc_rfid_loss.R`

Raw-level RFID chip-loss QC, run **before** any metric is derived — the
rationale being that a shed chip still generating reads produces pseudo-data
rather than missing data. Writes `raw_tracking_qc_rfid_loss/`, whose
`tables/raw_tracking_qc_by_animal.csv` is read by
`Analysis/14_systems_neuroscience_summary_dashboard.R:1967`. It is an active
upstream producer, not a one-off script.

---

## `_archive/` — historical preprocessing variants

Retained for provenance; not runnable as written and not part of any current
workflow.

| File | Status |
|---|---|
| `E9_SIS_AnimalPos-preprocessing.R` | Serial predecessor of the parallel entry point. Superseded by the 2026-08-27 refactor that separated measurement events from aggregation boundaries. Its `source()` on line 22 points at a repository-root path where the functions file does not exist, so it cannot run today. Running the code path it calls would reintroduce synthetic boundary rows that the current contract removed. |
| `E9_SIS_lme_data_formatting.r` | Built the old `data_lme_format/data_filtered_agg.csv` for the archived LME scripts. Both ends of its data flow are archived; it has no references anywhere in the repository. |

---

## Relationship to `Functions/`

`Functions/E9_SIS_AnimalPos-functions.R` looks historical but is **not**
archivable: `E9_SIS_AnimalPos-preprocessing_parallell.r` sources it under a
`stopifnot()` guard, and the current helper
`Functions/animalpos_preprocessing_helpers.R` declares

```r
preprocess_animalpos_file <- function(..., remove_phases_fn = remove_phases, ...)
```

where `remove_phases()` is defined only in that legacy file. Because the default
is evaluated lazily, removing it would fail only during an actual preprocessing
run — not during any analysis stage or test. See `Functions/README.md`.

---

## Scope note

Nothing in this directory was modified during the publication restructuring
beyond moving the two archived files with `git mv`. No preprocessing calculation
was changed.
