# Functions

Shared helper modules sourced by the staged pipeline in `Analysis/`, by the
preprocessing entry points in `Formatting/`, and by the checks in `Testing/`.

Helpers are loaded through `Analysis/_pipeline_setup.R`:

```r
source_mmm_helper("first_night_window_helpers.R")
```

which resolves against `MMM_REPO_ROOT` (found by walking up until
`Functions/behavioral_dynamics_helpers.R` and `Analysis/` are both present).
`behavioral_dynamics_helpers.R` is sourced unconditionally by
`_pipeline_setup.R` itself, so it is available to every stage.

No helper was renamed, split or refactored during the publication
restructuring. This file documents the existing layout; it does not propose one.

---

## Grouped by concept

### Identity

| File | Role |
|---|---|
| `animal_identity_invariants_helpers.R` | Pure validation engine for cross-scale animal-identity invariants after Stage 01. Deliberately free of file I/O so it can be unit-tested; `Testing/audits/validate_cross_scale_animal_identity.R` supplies the data, `Testing/tests/test_animal_identity_invariants_engine.R` tests the engine. |
| `identity_correction_comparison_helpers.R` | Before/after comparison engine plus the Stage 03 / Stage 09 table registry used by `Testing/audits/compare_identity_correction_before_after.R`. Performs no work at source time. |

The canonical `canonical_animal_id()` itself lives in
`behavioral_dynamics_helpers.R` and is the fail-closed identity contract for the
whole pipeline.

### Preprocessing

| File | Role |
|---|---|
| `animalpos_preprocessing_helpers.R` | The **current** preprocessing contract: `parse_animalpos_datetime()`, position mapping, phase/epoch counters, `preprocess_animalpos_file()`. Separates measurement events from aggregation boundaries — no synthetic boundary rows. |
| `cookiehab_preprocessing_helpers.R` | Cookiehab-specific readers used by `Formatting/01_preprocess_cookiehab_animalpos.R`. |
| `E9_SIS_AnimalPos-functions.R` | Historical E9 function library. **Retained in place — see below.** |

### Temporal / window

| File | Role |
|---|---|
| `phase_classification_helpers.R` | The exact Active/Inactive phase classifier. The smallest file here and the highest-leverage scientific invariant: it replaced permissive substring matching (`"active|dark|night"`, which matched `"inactive"`) with exact membership. Locked by `Testing/tests/test_phase_classification.R`. |
| `duration_normalization_helpers.R` | Epoch-duration QC and normalization; the most widely sourced domain helper. |

### First-night analysis

| File | Role |
|---|---|
| `first_night_window_helpers.R` | The single source of truth for the prespecified first-night window (first Active phase block after the first cage change, fixed clock window 18:30 → 06:30, 12 h). Parity with Stage 09 is asserted by `Testing/tests/test_first_night_window_parity.R`. |
| `first_night_domain_helpers.R` | The one canonical implementation of the audited five-domain first-night panel: formulas, standardization, completeness and multiplicity contracts. |
| `first_night_domain_driver.R` | Stage 14's single entry point for both first-night resolutions. Pure orchestration — it holds no formulas of its own and never rebins one resolution from another. |

### Statistics

| File | Role |
|---|---|
| `behavioral_dynamics_stats_helpers.R` | CON/RES/SUS group summaries and pairwise contrasts: `cohens_d_pooled()`, `safe_welch_test()`, normality/variance handling. |

### HMM

| File | Role |
|---|---|
| `hmm_stage14_helpers.R` | HMM identity auditing, semantic state labelling, epoch scoring and the gap-aware sequence contract shared by Stage 08, Stage 14 and the HMM audits. |

### Output / provenance

| File | Role |
|---|---|
| `behavioral_dynamics_helpers.R` | The foundation module sourced by every stage: canonical identity, the house colour palette, output-path construction, the write registry, and manifest/provenance emission. Path-length behaviour is asserted by `Testing/tests/test_output_path_length.R`. |

---

## Why `E9_SIS_AnimalPos-functions.R` is not archived

It looks like legacy code and most of it is: roughly 28 of its ~30 functions are
superseded, including the synthetic-row-inserting routines that the current
preprocessing contract deliberately removed. No `Analysis/` stage, no runner, no
manuscript builder and no portable test sources it.

It is nevertheless a **live runtime dependency**, for two independent reasons:

1. `Formatting/E9_SIS_AnimalPos-preprocessing_parallell.r` — the active E9
   preprocessing entry point — sources it in the main process and again inside
   every parallel worker, under
   `stopifnot(all(file.exists(function_files)))`. Moving the file makes
   preprocessing abort at startup.

2. More subtly, the *current* helper declares

   ```r
   preprocess_animalpos_file <- function(..., remove_phases_fn = remove_phases, ...)
   ```

   and `remove_phases()` (line 194) together with `count_half_hours_elapsed()`
   (line 231) are defined **only** in this file. Because that default argument is
   evaluated lazily, dropping the `source()` would fail only at call time during
   a real preprocessing run — not during any analysis stage and not during any
   test in this repository.

The header of `animalpos_preprocessing_helpers.R` records the intent: those two
functions are "reused unchanged so epoch inclusion semantics are preserved".

If this is cleaned up after the manuscript freeze, the correct sequence is to
lift `remove_phases()` and `count_half_hours_elapsed()` verbatim into
`animalpos_preprocessing_helpers.R`, verify a full preprocessing run reproduces
the current `preprocessed_data/` byte-for-byte, and only then archive the rest.
That is a code change and was deliberately out of scope for a restructuring pass
that must not alter scientific output.
