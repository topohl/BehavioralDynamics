# Testing

Verification code for the BehavioralDynamics analysis pipeline, separated by what
each script needs in order to run and by what role it plays in the manuscript
release.

All scripts in this directory are invoked **from the repository root**, e.g.

```bash
Rscript Testing/tests/test_phase_classification.R
```

Most of them resolve helpers through `source("Analysis/_pipeline_setup.R")`, which
is a working-directory-relative path. Running them from inside `Testing/` will not
work.

---

## `tests/` — portable regression and contract checks

Deterministic checks that run **without the E9 local dataset**. Every fixture is
built in memory or under `tempdir()`. These are the checks that can run in CI.

Seventeen scripts, covering:

| Area | Scripts |
|---|---|
| Animal identity | `test_canonical_animal_id.R`, `test_animal_identity_invariants_engine.R`, `test_identity_comparison_engine.R`, `test_compare_identity_correction_driver.R` |
| Phase classification | `test_phase_classification.R` |
| First-night window/domain contracts | `test_first_night_window_parity.R`, `test_first_night_domain_contract.R` |
| Stage 09 contracts | `test_stage09_endpoint_identity.R`, `test_stage09_primary_window.R` |
| Stage 14 / HMM contracts | `test_stage14_upstream_registry.R`, `test_hmm_stage14_contract.R` |
| Stage 19 / Stage 06 | `test_stage19_group_sex_labels.R`, `test_stage19_identity_and_stage06_schema.R` |
| Preprocessing / boundaries | `test_animalpos_preprocessing_helpers.R`, `test_downstream_boundary_and_gap_contract.R` |
| Output/write infrastructure | `test_output_path_length.R`, `test_write_registry_rerun_behavior.R` |

Several of these scripts *opportunistically* read the canonical E9 tables when
the `S:` project root happens to be mounted, but every such read is guarded by
`file.exists()` / `dir.exists()` and the script still passes when it is absent.
That guard is what makes them portable — see `docs/REPRODUCIBILITY.md`.

Run the whole portable suite:

```bash
for f in Testing/tests/test_*.R; do Rscript "$f" || echo "FAILED: $f"; done
```

---

## `audits/` — data-dependent scientific validation

Forensic and validation analyses that **require the E9 local dataset** under

```text
S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID
```

Forty-two scripts. They read canonical pipeline outputs, and many of them also
*write* audit tables back into the local analysis tree (typically under
`analysis_ready/12_systems_neuroscience_summary/5min_based/audit_*/`). They are
**not** part of CI and will fail without the dataset.

Grouped by subject:

- **First-night five-domain analysis** — `audit_first_night_*.R` (14 scripts):
  candidate-set construction and pruning, domain scores, dwell partition
  stability, window provenance and sensitivity, production parity, the CC1
  heatmap.
- **HMM state architecture** — `audit_hmm_*.R` (19 scripts) and
  `audit_step6_longitudinal_gapaware_robustness.R`: component decomposition,
  identifiability, gap-aware fitting, partition robustness, cross-optimum
  robustness, semantic erasure, read-density sensitivity.
- **Phase classification impact** — `audit_phase_bug_impact.R`.
- **Inactive-phase QC redesign** — `audit_inactive_phase_qc_redesign.R`.
- **Stage 09 artifact hygiene** — `audit_stage09_stale_artifacts.R`.
- **Identity validation** — `compare_identity_correction_before_after.R`,
  `validate_cross_scale_animal_identity.R`,
  `repair_existing_metrics_identity_utility.R`.
- **Data-dependent contract checks** — `test_animal_identity_contract.R` and
  `test_reporting_architecture.R`.

> **Why two `test_*.R` files live here rather than in `tests/`:** both read
> `analysis_ready/03_derived_metrics/.../all_behavior_metrics.csv` (and, for the
> reporting check, a list of required canonical artifacts) *unconditionally* —
> there is no `file.exists()` guard. They are genuine data-dependent contract
> checks despite the `test_` prefix, and placing them in `tests/` would break CI.
> Classification here follows what the code actually does, not the filename.

Several audits are chained: they consume a table written by an earlier audit
rather than by a production stage. Notable chains are recorded in each script's
header (`UPSTREAM <- ...`). Where a `_v2` script exists it supersedes the v1 of
the same name; the v1 is retained because the published provenance tables cite it.

---

## `legacy/` — historical development provenance

Eleven scripts kept for method-development history. They are **not** part of the
production pipeline, are not maintained, and generally hard-code old absolute
paths that no longer exist.

- Early E9 AnimalPos analysis and function variants
  (`E9_SIS_AnimalPos-analyzing*`, `E9_SIS_AnimalPos-functions v.2.0.0.R`,
  `E9_SIS_AnimalPos-analyzing-shannon v.1.0.2.r`)
- Early LME / sociability modelling (`lme v.3.1.2.r`,
  `sociability_lme v.1.0.2.r`)
- Early prediction and physiology exploration (`prediction_analysis.r`,
  `correlate_activity_physiology.R`)
- Superseded pipeline runners (`run_behavioral_dynamics_pipeline.R`,
  `run_full_systems_behavior_pipeline.R`) and the structure check they call
  (`check_behavioral_dynamics_structure.R`). These predate
  `Analysis/run_all_analysis.R`, which is the current runner.

Do not use anything in `legacy/` for new analyses.

---

## Relationship to the rest of the repository

| Directory | Role |
|---|---|
| `Analysis/` | Active staged production pipeline (Stages 00–16, 19) |
| `Functions/` | Shared helpers the tests and audits exercise |
| `Testing/tests/` | Portable contract/regression checks (CI-eligible) |
| `Testing/audits/` | Data-dependent scientific validation |
| `Testing/legacy/` | Historical provenance only |

The authoritative per-file classification, including active consumers and
move-safety notes, is in `docs/REPOSITORY_FILE_CLASSIFICATION.csv`.
