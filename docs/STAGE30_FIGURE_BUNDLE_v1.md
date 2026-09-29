# Stage 30 figure bundle v1 (canonical source-data export for manuscript figures)

## Purpose

Exp9_manuscript computes nothing statistical. Every Stage 30 number it plots or prints must therefore come from a frozen, hash-gated MMMSociability export. This bundle is that export for the Stage 30 exploratory screen (registry v1.0, REAL run `v1.0_be71e2f`).

- Writer: `Analysis/30b_stage30_figure_bundle.R` (thin runner) with `Functions/stage30_figure_bundle.R` (pure functions).
- Test: `Testing/tests/test_stage30_figure_bundle.R` (synthetic inputs only; run `Rscript Testing/tests/test_stage30_figure_bundle.R` from the repo root).
- Design: `manuscript_integration_2026-09-29/DESIGN.md` section 2 (audit folder).

## No inference

- Nothing is fitted, tested, refitted, adjusted, smoothed or predicted, and no p or q value is produced.
- Estimates, CIs, p, q, classes, sensitivities and LOBO rows are copies of the frozen Stage 30 tables.
- The static test forbids model, test and adjustment calls in the helper and the runner.
- `H_provenance.csv` states `scientific_recomputation = none; copies of frozen values; descriptive summaries (counts, medians, quartiles, centroid means) and unit rescaling of stored estimates only`.

## Inputs (all hash-gated; any mismatch stops the run)

| Input | Gate |
|---|---|
| Stage 30 run `analysis_ready/pipeline/30_exploratory_screen/v1.0_be71e2f/` | `audit/output_manifest.csv` sha256 `77f271ad...` (pinned); its 27 files by bytes and sha256 (CR stripped); no extra file; README `commit be71e2f07bfe...` and `registry sha256 5c252bf6...`; run_manifest REAL, `OK=48`, no run failures |
| Registry `analysis_ready/canonical/stage30_registry/v1.0/stage30_registry_v1.0.json` | sha256 `5c252bf6...`, status FROZEN, version 1.0; equal to the Stage 30 `input_hashes` entry |
| Stage 29 bundle `analysis_ready/canonical/behavior_bundle/ebb_v101_20260929_b2ce507/` | `00_manifest.csv` sha256 `ec59aa33...`; every file; no extra file; FROZEN row in `BUNDLE_REGISTRY.csv`; Stage 29 folder = Stage 30 README |
| `Analysis/sus_animals.csv`, `Analysis/con_animals.csv`, `later_outcome_combz_animal_level.csv` | on-disk sha256 = Stage 30 `audit/input_hashes.csv` = pinned (`dea3804b...`, `eddd2ee9...`, `1f6a2a69...`) |
| `evidence/wfD/X4_critic/x4_01_inactivity_animal_window.csv` | sha256 `a533b239...` = Stage 30 `input_hashes` (role `inactivity_reference`) |
| `evidence/wfD/W1_canonical_inactivity_gate/w1_04_gate_values.csv` | sha256 `ae80b7e1...` = `w1_99_output_sha256.txt`; `round(full, 2)` equals the registry `group_blind_qc` ρ and ICC |

Group follows the Stage 30 driver rule: CON if on the con list, else SUS if on the sus list, else RES. It must equal the ebb_v101 A1 / A4 Group and the CombZ-table `outcome_group` of every SIS animal, and the registered RES/SUS counts. CombZ comes from the canonical CombZ table and must equal ebb_v101 A1 / A4 CombZ (1e-12).

## Output

- REAL: `analysis_ready/canonical/stage30_figure_bundle/<bundle_id>/`, with `BUNDLE_REGISTRY.csv` beside it (`bundle_id, status, manifest_sha256, stage30_run_commit, mmm_git_commit, created_at`).
  - `bundle_id = s30b_v10_<YYYYMMDD>_<commit7>`.
  - The folder is written once through a staging folder and refused if it, its staging folder, its registry row or its gate log exists. Files are 0444.
  - `--real` is refused unless the MMM files used are committed and clean and the committed runner is executing.
- Gate order and gate records:
  1. Every pre-write gate (hash gates, labels, tables). H_provenance `pre_write_gates_passed` counts exactly these, and the writer refuses unless it equals the gate table it writes.
  2. The staging folder receives the 16 tables, `H_provenance.csv`, `H2_inputs.csv`, `H3_gate_results.csv` (the pre-write gate table) and `00_manifest.csv`. It is renamed into place and made read-only.
  3. Post-write gates: `00_manifest.csv` read back equals the manifest written; every file matches it and nothing else is present; the manifest lists exactly the declared files; every file is read-only.
  4. Only if all post-write gates pass is the `BUNDLE_REGISTRY.csv` row appended, then checked (registration gate).
  5. The complete gate table (pre-write, post-write, registration) is written to `logs/<bundle_id>_gate_results.csv` beside the registry, outside the immutable bundle, and made read-only. If a post-write gate fails, the log records the failure, the bundle is not registered and the run stops.
- DRY (`--dry-run`): `<dry root>/<YYYYMMDD_HHMMSS>/<bundle_id>/`, status `DRY_RUN_NOT_FOR_USE`, with its `BUNDLE_REGISTRY.csv` and `logs/` beside it. It is never written to S:.
- Doubles are written as the shortest decimal (15-17 significant digits) that reads back to the identical double. Every CSV is re-read and compared; embedded double quotes are refused.

## Tables (one CSV each; `run_mode` dropped)

| File | Rows | Content |
|---|---|---|
| `S0_master_hypotheses.csv` | 48 | discovery rows (DESIGN column list), standardized CI, registry display labels, `row_order` |
| `S0b_pool_estimates.csv` | 16 | POOL rows, `flag = ESTIMATION_ONLY`, standardized CI |
| `S0c_l_components.csv` | 84 | copy of `l_components.csv` |
| `S0d_sensitivities.csv` | 128 | copy of `sensitivities.csv` |
| `S0e_lobo.csv` | 534 | copy of `lobo.csv` |
| `S1_light_animals_cc1.csv` | 87 | SIS animals at CC1 (full-precision rds) with Group and CombZ |
| `S1b_light_estimates.csv` | 4 | SLEEP-CAT-IA40L F / M / INT and POOL, with the ≥60-s and Movement-adjusted estimates / CIs |
| `S1c_light_descriptives.csv` | 10 | n, median, q25, q75, min, max by Sex × Group and overall; count ≥ 0.99 for posinact40_light |
| `S2_rate_inactivity_windows.csv` | 444 | group-blind light-phase animal-windows from x4_01 (X = 40) |
| `S2b_rate_inactivity_relationship.csv` | 2 | active / light: registry-rounded and w1_04 full-precision ρ and ICC, window / animal counts, basis, source and sha |
| `S3_cookie_animals.csv` | 77 | cookie SIS animals (full-precision rds) with Group, CombZ, OR646 flag and the registry EPM+1 date |
| `S3b_cookie_descriptives.csv` | 14 | n, median, q25, q75 by Sex, pooled, Sex × Group; `n_increased`; the registry all-97 QC row (copied) |
| `S3c_cookie_estimates.csv` | 8 | COOKIE-CAT and COOKIE-CONT × F / M / INT + POOL, with the Δ45 and Movement-adjusted estimates / CIs |
| `S3d_cookie_cont_lines.csv` | 2 | COOKIE-CONT display line per sex: centroid, frozen slope and CI, end points |
| `S4_screen_matrix.csv` | 48 | layout keys (16 metric rows × Female / Male / Female − male) plus plotting fields from S0 |
| `S5_display_labels.csv` | 15 | measure_col → display label and unit, each with its source |
| `H_provenance.csv`, `H2_inputs.csv`, `00_manifest.csv` | | provenance keys; every input with bytes, sha256, role; every file with bytes, sha256, schema_version 1 |
| `H3_gate_results.csv` | | the pre-write gate table (stage, gate, passed, hard, detail); the complete table is `logs/<bundle_id>_gate_results.csv` |

## Descriptive computations (the only numbers not copied)

1. **Standardized CI.** The stored `ci_low` / `ci_high` are rescaled with the stored `standardization_rule`: CONT × `standardizer_sd` (SD_x), CAT ÷ `standardizer_sd` (SD_y), L none. The stored estimate rescaled the same way must reproduce the stored `standardized_estimate` within 1e-12.
2. **S1c and S3b.** Counts, medians and quartiles (R `quantile` type 7), plus minima and maxima in S1c, over the exported animals. The all-97 cookie row is copied from the registry.
3. **S2b counts.** The window and animal counts are taken from the x4_01 rows. ρ and ICC are copies.
4. **S3d.** `x_mean` and `y_mean` are the arithmetic means of dcookie60 and CombZ over exactly the model's animals; n and the batch count must equal the frozen `n_animals` / `n_batches`.
   - The line is `y = y_mean + slope·(x − x_mean)`, with the frozen slope: it passes through the sex-resolved centroid.
   - For the registered `CombZ ~ Batch + x`, the OLS normal equations give batch intercepts ȳ_b − slope·x̄_b. Their n-weighted mean is ȳ − slope·x̄, so this is the batch-averaged fitted line. No model is refitted.
5. **S4 layout keys.** Ordinal positions only.

## Labels

- Display labels are the registry `measures` `display` strings. The only transform is typesetting `>= ` as `≥`.
- Occupancy dispersion and fragmentation have no registry display field. For these two, the ebb_v101 configuration labels and units are used, and `label_source` says so.
- Block, question and sex-column labels follow DESIGN section 2 (S4).
- A banned-wording guard stops the run if any display label (S0, S4, S5) or the S3d `derivation` text contains antenna, crossing, sleep, confirmatory, preregistered, approach, investigation, consumption, habituation, time near, or female-/sex-specific wording. Legacy identifiers (for example `crossing_rate`) appear only as column values, never as labels.
