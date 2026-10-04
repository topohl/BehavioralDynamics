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

Stages 16 and 19 are **not** in `run_all_analysis.R` and are run explicitly:

```r
source("Analysis/16_manuscript_behavior_report.R")   # manuscript export layer
source("Analysis/19_spatial_occupancy_maps.R")       # secondary/spatial
```

Stage 16 must run *after* the canonical Stage 03 and Stage 09 outputs exist,
because it only reads and assembles them.

### 3. Manuscript figure staging

```r
source("manuscript/Fig1_behavior_candidates/build_fig1_candidates.R")
```

### 4. Release bundle

```r
Rscript Analysis/build_publication_release.R --dry-run
Rscript Analysis/build_publication_release.R --release-id=rc1
```

See `docs/PUBLICATION_RELEASE.md`.

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
| `MMM_BEHAVIOR_PROJECT_ROOT` | `build_fig1_candidates.R` |
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

17 scripts. Every fixture is in memory or under `tempdir()`. Four of them
(`test_first_night_window_parity.R`, `test_hmm_stage14_contract.R`,
`test_output_path_length.R`, `test_stage19_identity_and_stage06_schema.R`)
*opportunistically* read the canonical tables when `S:` happens to be mounted,
but every such read is guarded by `file.exists()` / `dir.exists()` and the script
passes without them. This is what CI runs.

### Data-dependent — requires the E9 dataset

42 scripts in `Testing/audits/`. They will fail without `S:`, by design. Two of
them are named `test_*` — `test_animal_identity_contract.R` and
`test_reporting_architecture.R` — because they are contract checks, but they read
canonical tables unconditionally and so are not portable. They live in `audits/`
for that reason. See `Testing/README.md`.

> **Caution when interpreting a green run on a machine with `S:` mounted.** Every
> `test_*.R` passes there, including the two non-portable ones. Portability must
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

---

## How Stage 16 is built

`Analysis/16_manuscript_behavior_report.R` is an assembly layer with no
statistics of its own. It:

1. resolves the required canonical Stage 03, Stage 09 and QC artifacts, recording
   each path, its role, and its SHA-256 in `provenance.csv`;
2. selects typed result rows into `primary_results.csv` and
   `supplementary_results.csv` without refitting anything;
3. emits three source-data tables (animal level, held-out predictions,
   movement-phase);
4. writes `Behavioral_Source_Data.xlsx` and then **re-reads it**, aborting if any
   workbook cell disagrees with its CSV counterpart, and validating OOXML
   integrity (no formulas, error cells, external links, drawings or VML);
5. re-hashes every upstream source after assembly and aborts if any hash moved;
6. writes `validation.csv` with one row per check.

Current state: **16 of 16 validation checks PASS**, and all **19** provenance
artifacts match their recorded SHA-256.

Reproduce that check independently:

```r
rfid <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
prov <- read.csv(file.path(rfid, "analysis_ready/manuscript/behavior/provenance.csv"))
live <- vapply(file.path(rfid, prov$path), digest::digest,
               character(1), algo = "sha256", file = TRUE)
stopifnot(all(tolower(live) == tolower(prov$sha256)))
```

---

## What restructuring must never change

Repository reorganisation must leave every canonical scientific output
byte-identical. The invariance check is the hash comparison above: if any
canonical artifact hash changes as a result of moving files, stop and
investigate. It was run before and after the publication restructuring and both
times reported 19/19 matching.
