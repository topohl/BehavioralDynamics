# Reproducibility

How to re-run the BehavioralDynamics analysis, what can be verified without the
experimental data, and how the manuscript package is produced.

---

## Environment

| | |
|---|---|
| Language | R |
| Developed and validated on | R 4.5.1 (2025-06-13 ucrt), Windows 11 |
| Dependency manager | `renv` lockfile (record only; the project library is not activated) |

`renv.lock` (renv 1.3.0, implicit snapshot of 2026-10-05) records R 4.5.1 and
the version of every package the code uses plus its dependencies (240 packages,
237 from CRAN, 3 from Bioconductor), read from the validated analysis library.
It pins the versions that the frozen stages gate on (S32_PACKAGES in
`Functions/stage32_run.R`): lme4 2.0-1, lmerTest 3.2-1, pbkrtest 0.5.5,
clubSandwich 0.7.0, Matrix 1.7-5, reformulas 0.4.4. renv was installed into the
analysis library for the snapshot; no other package was installed or changed
(the six gated versions were checked before and after). The snapshot created
only `renv.lock`: there is no `renv/` folder and no `.Rprofile`, so R starts in
this repository exactly as before.

`docs/package_versions.csv` lists the 79 non-base packages the code names
(`renv::dependencies()`), whether they are installed and the installed version.
Six are not installed. destiny, ranger and randomForest are used only behind
`requireNamespace()` (supporting Stage 13 and Stage 10 skip those branches);
GGally, kableExtra and plotrix appear only in archived scripts under
`Analysis/_archive/`.
`docs/sessionInfo.txt` is the earlier `sessionInfo()` record.

**Policy.** Never upgrade R 4.5.1 or the gated packages in place, and never run
`renv::restore()` against the analysis library: the frozen Stage 29, 29b, 30 and
32 runs check their package versions, and a restore or an upgrade would move
them. To reproduce elsewhere, restore the lockfile into a new, separate library.

---

## Run order

All commands are run **from the repository root**.

### 0. Raw preprocessing (outside the pipeline runner, run deliberately)

```r
# E9 main dataset: raw AnimalPos -> preprocessed_data/
source("Formatting/E9_SIS_AnimalPos-preprocessing_parallell.r")

# optional raw-level RFID chip-loss QC (feeds Stage 14)
source("Formatting/00a_raw_tracking_qc_rfid_loss.R")
```

This is separate from `run_all_analysis.R` on purpose: it rewrites the canonical
input of Stage 01. See `Formatting/README.md`.

### 1. Staged pipeline, Stages 00–15

```r
options(
  mmm.pipeline_profile        = "legacy_systems",  # or "heatmap_inputs", "early_prediction"
  mmm.run_systems_extension   = TRUE,
  mmm.run_behavior_proteomics = FALSE,
  mmm.continue_on_error       = FALSE
)
source("Analysis/run_all_analysis.R")
```

The runner runs nothing by default. It needs one profile or an explicit list
(`options(mmm.pipeline_stages = c("04", "05"))`), sources
`Analysis/_pipeline_setup.R`, then executes the selected stages in order:

| Profile | Stages |
|---|---|
| `legacy_systems` | 02-07, 10, 11, 13, 15 |
| `heatmap_inputs` | 01, 08, 12, 14 (the Stage 14 heatmap inputs, then Stage 14) |
| `early_prediction` | 09 |

Two stages are option-gated within a profile:

| Stage | Option | Default |
|---|---|---|
| 10 systems extension | `mmm.run_systems_extension` | `TRUE` |
| 15 behaviour-proteomics | `mmm.run_behavior_proteomics` | `FALSE` |

Stages 01, 09 and 28 rewrite inputs that the frozen runs pinned by sha256.
`Functions/frozen_input_guard.R` refuses to start them on the live root unless
`options(mmm.allow_pinned_overwrite = TRUE)` is set (it then backs the pinned
files up first), and after each stage it stops if a pinned file changed. A
sandbox root is not guarded. See `Analysis/README_pipeline.md`.

### 2. Stages outside the runner

Stages 19, 20 and 22 are **not** in `run_all_analysis.R` and are run explicitly:

```r
source("Analysis/19_spatial_occupancy_maps.R")       # secondary/spatial
```

Stages 20 and 22 (descriptive within-night GAMM profiles) are re-run in a
sandbox, never on the live root:

1. set `MMM_BEHAVIOR_PROJECT_ROOT` (or `options(mmm.project_root = ...)`) to a C:
   root and `options(mmm.derived_metrics_dir = ...)` to a copy of
   `analysis_ready/foundations/behavior_metrics/` whose 10-min table matches the
   sha256 the Stage 29 v1.0.1 release pins;
2. run Stage 20, then Stage 22 under the same root (Stage 22's CC1 cross-model
   audit reads Stage 20's trajectory table and skips silently without it);
3. compare against the live tables: estimates, SE, CI, p, q and n_obs must be
   identical; only manifests and figure bytes may differ;
4. promote with a producer-rerun record in `docs/behavior_output_producer_reruns/`.

Use the package versions in `docs/package_versions.csv` (R 4.5.1, mgcv 1.9-4).
The frozen Stages 29-32 ran once each and are never re-run.

### 3. Manuscript figures and releases

The manuscript is rendered in Exp9_manuscript from the frozen bundles. This
repository no longer stages figures or builds release bundles: the Figure 1
candidate builder, the Stage 16 package and the release builder were retired on
2026-10-05 (`docs/LEGACY_AND_GAMM_RETIREMENT_2026-10-05.md`).

---

## Local-data requirements

The repository contains **code, not experimental data**. Analysis stages read the
E9 project root:

```text
S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID
```

Overridable per component:

| Variable / option | Used by |
|---|---|
| `MMM_DATA_DIR` | `Formatting/E9_SIS_AnimalPos-preprocessing_parallell.r` |
| `MMM_REPO_DIR` | preprocessing and several audits, to locate the checkout |
| `MMM_BEHAVIOR_PROJECT_ROOT` | `mmm_project_root()` (every stage script), Stage 09 and the sandbox reruns |
| `getOption("mmm.project_root")` | several portable tests |

> The `MMMSociability` component inside the data path is a **local directory
> name**, not the repository name. Renaming the GitHub repository to
> `BehavioralDynamics` does not change it.

---

## Portable vs data-dependent verification

This split is the core of the verification story.

### Portable — runs with no experimental data

```bash
for f in Testing/tests/test_*.R; do Rscript "$f" || echo "FAILED: $f"; done
```

78 scripts. Every fixture is in memory or under `tempdir()`. Four of them
(`test_first_night_window_parity.R`, `test_hmm_stage14_contract.R`,
`test_output_path_length.R`, `test_stage19_identity_and_stage06_schema.R`)
*opportunistically* read the canonical tables when `S:` happens to be mounted,
but every such read is guarded by `file.exists()` / `dir.exists()` and the script
passes without them. This is what CI runs.

### Data-dependent — requires the E9 dataset

51 scripts in `Testing/audits/`. They will fail without `S:`, by design. Three of
them are named `test_*` — `test_animal_identity_contract.R` and the two
acute-window parity checks — because they are contract checks, but they read
canonical tables unconditionally and so are not portable. They live in `audits/`
for that reason. See `Testing/README.md`.

> **Caution when interpreting a green run on a machine with `S:` mounted.** Every
> `test_*.R` passes there, including the non-portable ones. Portability must
> be judged from the guards, not from the exit code.

---

## Canonical artifact resolution

Readers never guess and never pick the newest file. `Analysis/_pipeline_setup.R`
provides the resolution contract:

- `behavior_stage_dir()` / `behavior_stage_tables()` build canonical paths under
  `analysis_ready/pipeline/<stage>_<name>/<resolution>/{tables,figures,audit}/`.
- `resolve_behavior_artifact()` tries the canonical path first, then a single
  documented legacy path.
- `resolve_stage09_early_prediction_artifact()` and
  `resolve_stage04_temporal_instability_artifact()` apply **global source-class
  precedence**: any canonical path at any acceptable resolution beats any legacy
  path at any resolution. Resolution preference only breaks ties within a class.
  A resolution-by-resolution loop returning the first hit of either class would
  be wrong, and the code says so explicitly.
- Any legacy fallback actually used raises a warning and is recorded in the
  manuscript provenance table.

`analysis_ready/output_index.csv` is the machine-readable map of which stages are
migrated to the canonical layout and which still write to historical locations.
`Maintenance/Refresh-BehaviorOutputIndex.R` writes it and `analysis_ready/README.md`
from `Functions/behavior_output_index.R`: a dry run first, then `--write` with a
backup.

---

## The retired Stage 16 package

`Analysis/16_manuscript_behavior_report.R` assembled Stage 03, Stage 09 and QC
tables into `Behavioral_Source_Data.xlsx` and CSV companions, with a provenance
table and 16 validation checks. It was retired on 2026-10-05 and its 2026-09-22
package moved unchanged to `analysis_ready/history/retired/manuscript_behavior/`.
At the last check its provenance verified 18 of 22 files (two Stage 09 5-min
tables were rewritten on 2026-09-27 and two QC files archived), so the
"16 of 16" and "19 of 19" figures quoted for it are historical.

---

## What restructuring must never change

Repository reorganisation must leave every canonical scientific output
byte-identical. The checks are the frozen runs' input pins (re-hashed by
`Functions/frozen_input_guard.R` after every runner stage),
`Testing/tests/test_frozen_code_identity.R` for the frozen code, and a sha256
receipt for every archive move. If a hash changes as a result of moving files,
stop and investigate. Until 2026-10-05 the check was the Stage 16 provenance
comparison, which reported 19/19 before and after the publication restructuring.
