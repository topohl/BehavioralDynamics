# Figure-support bundle v2 (source data for manuscript Figure 1, options 3 and 3b)

## Purpose

Figure 1 option 3b (panel c) shows the post hoc three-group comparison in the first active phase after CC1. For each measure and sex it shows the batch-balanced CON, RES and SUS means and the RES − CON, SUS − CON and SUS − RES contrasts, with Holm-adjusted P. Exp9_manuscript computes nothing statistical, so these values reach the manuscript through this bundle.

v2 contains every v1 table unchanged in definition (F1, F1b, F2, F2b). It adds three verbatim copies of the post hoc run.

- **Writer.** `Analysis/16c_figure_support_bundle.R` with `Functions/figure_support_bundle.R`.
- **Test.** `Testing/tests/test_figure_support_bundle.R`, synthetic data only.
- **v1 is unchanged.** `fsb_v1_20260930_f06552c` stays FROZEN and untouched, and was written by commit f06552c (record: `FIGURE_SUPPORT_BUNDLE_v1.md`).
- **Bundle id.** `fsb_v2_<YYYYMMDD>_<commit7>`, written to the same folder, `analysis_ready/canonical/figure_support_bundle/`, with its own row in `BUNDLE_REGISTRY.csv` (same columns as v1).

## The post hoc source (fitted in MMM, once)

**Registry.** `docs/POSTHOC_CON_CONTRASTS_REGISTRY_v1.0.md`
- Frozen 2026-09-30 at commit 322be31, sha256 `a6435d8d...`.
- A read-only copy is on S: at `analysis_ready/canonical/posthoc_con_contrasts_registry/v1.0/`.
- Tier: POST HOC exploratory. Decision basis: USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION.

**Run.** `Analysis/29b_posthoc_con_contrasts.R` with `Functions/posthoc_con_contrasts.R`, on the frozen engine `Functions/rfid_canonical_inference.R`.
- It was run once as REAL at commit 7f1da1f. The runner refuses any further run while a `v1.0_*` or `.tmp_v1.0_*` folder exists.
- Output: `analysis_ready/pipeline/29b_posthoc_con_contrasts/v1.0_7f1da1f/`. Its `audit/output_manifest.csv` has sha256 `4c079ecf...` and lists 7 files. Every file is read-only.

**Gates in 16c before the copy.** Any failure stops the export:
- the folder name;
- the output-manifest sha256;
- every file's bytes and sha256, and no extra file;
- the files are read-only;
- the README's commit and registry-sha lines;
- `run_manifest`: REAL, the run commit, the registry sha and version, the status counts (`OK=4` for both models), and `n_failed` = 0.

## Added tables (verbatim copies; nothing computed)

| Table | Source (post hoc run) | Rows |
|---|---|---|
| `P1_posthoc_con_contrasts` | `tables/estimates.csv` | 24 = 2 measures × 2 sexes × (3 batch-balanced means + 3 contrasts) |
| `P1b_posthoc_sensitivity` | `tables/sensitivity_common_cage_variance.csv` | 24, with the agreement columns |
| `P1c_posthoc_diagnostics` | `tables/diagnostics.csv` | 8 (4 primary + 4 sensitivity fits) |

- **What is copied.**
  - Every cell is read as text and written back unchanged.
  - A blank cell stays NA and an empty string is kept.
  - Three columns are appended: `source_run`, `source_file` and `source_sha256`. The last must equal the output-manifest entry at the time of the read.
- **Gates before the write.**
  - The row counts.
  - Every source cell equals an independent re-read of the source.
  - The registry sha, version and tier are on every row.
  - There are six estimands per measure × sex, and Holm m = 3 on the tested rows.
  - P1 holds only primary-model rows and P1b only sensitivity rows.
  - The display text contains no banned wording.
- **Gate after the write, before the registry row.** Every line of each written P table must be the source line followed by the appended provenance cells, which makes the copy verbatim at byte level.
- **Display columns** (all in P1):
  - `measure_label`, `unit`, `estimand_label` (with U+2212 minus), `estimate`, `ci_low`, `ci_high`, `df` (Kenward–Roger), `p_holm`, `holm_m` and `status`;
  - `display_note` ("Post hoc, cage-aware; CON = 3 cages/sex"), `caveats`, `registered_rs_note` and `tier`.
- **FAILED rows.** A row whose `status` is FAILED has no estimate and must not be displayed.

## Provenance

`H_provenance.csv` adds `posthoc_run_dir`, `posthoc_run_commit`, `posthoc_output_manifest_sha256`, `posthoc_registry_sha256`, `posthoc_registry_version`, `posthoc_tier` and `posthoc_tables`.

`scientific_recomputation` keeps the v1 opening words. It then states that P1, P1b and P1c are verbatim copies of the hash-gated post hoc run, whose model was fitted there once, and that nothing is fitted, tested or adjusted in this bundle. `H2_inputs.csv` lists every post hoc run file with its sha256.
