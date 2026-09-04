# Reproducibility

How to re-run the BehavioralDynamics analysis, what can be verified without the
experimental data, and how the manuscript package is produced.

---

## Environment

| | |
|---|---|
| Language | R |
| Developed and validated on | R 4.5.1 (2025-06-13 ucrt), Windows 11 |
| Dependency manager | **none yet** — see "Environment capture" below |

Package inventory: `docs/package_versions.csv` lists the 41 non-base packages
the active code loads, with the version present in the validated environment.

### Environment capture status

`renv` is **not** in use and `renv.lock` does **not** exist. It was deliberately
not created during the publication restructuring: `renv` was not installed in the
validated environment, and installing it in order to snapshot would have modified
the very environment being captured. A fabricated lockfile would be worse than
none.

What exists instead is an honest inventory:

- `docs/package_versions.csv` — package, whether installed, resolved version
- `docs/sessionInfo.txt` — full `sessionInfo()` from the validated environment

**`renv` adoption is pending** and is the recommended next reproducibility step.
The correct sequence is: install `renv` in a scratch library, `renv::init()` with
`bare = TRUE`, `renv::snapshot()` against the inventory above, then verify the
portable suite still passes. Do not run `renv::restore()` against the current
library.

One dependency is genuinely optional: `randomForest` is absent from the
validated environment. Stage 10 guards it with `requireNamespace()` and skips its
non-linear sensitivity branch when missing, so this does not block any canonical
result.

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
  mmm.run_optional_hmm       = FALSE,
  mmm.run_systems_extension  = TRUE,
  mmm.run_behavior_proteomics = FALSE,
  mmm.continue_on_error      = FALSE
)
source("Analysis/run_all_analysis.R")
```

The runner sources `Analysis/_pipeline_setup.R`, then executes Stages 00–15 in
order. Three stages are option-gated:

| Stage | Option | Default |
|---|---|---|
| 08 optional HMM | `mmm.run_optional_hmm` | `TRUE` |
| 10 systems extension | `mmm.run_systems_extension` | `TRUE` |
| 15 behaviour-proteomics | `mmm.run_behavior_proteomics` | `FALSE` |

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
