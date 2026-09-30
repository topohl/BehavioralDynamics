# Figure-support bundle v1 (descriptive source-data export for manuscript Figure 1, option 3)

## Purpose

Exp9_manuscript computes nothing statistical. Figure 1 option 3 needs two quantities that no frozen bundle stores: the CON cage means per sex and cage change (panels c and d), and the six CombZ component values as they enter CombZ (panel b heatmap). This bundle is the descriptive MMM export for both (spec: `manuscript_integration_2026-09-29/OPTION3_SPEC.md` section 1, audit folder).

- Writer: `Analysis/16c_figure_support_bundle.R` (thin runner) with `Functions/figure_support_bundle.R` (pure functions; it reuses the hash-gate and writer utilities of `Functions/stage30_figure_bundle.R`).
- Test: `Testing/tests/test_figure_support_bundle.R` (synthetic inputs only). Run `Rscript Testing/tests/test_figure_support_bundle.R` from the repo root.

## No inference

- Nothing is fitted, tested, refitted, adjusted, smoothed or predicted, and no p or q value is produced. The static test forbids model, test and adjustment calls in the helper and the runner.
- The only computations are the arithmetic mean of each CON cage's 4 animals, a sign flip (`z = direction x signed_z`), a within-sex CombZ rank used as a layout key, and a comparison of CombZ with the stored threshold. Two further means are gates only and are not exported: the mean of the 3 cage means, and CombZ recomposed from the components.
- `H_provenance.csv` `scientific_recomputation` starts with "none; descriptive cage means and the frozen CombZ standardisation reproduced exactly; no model". It then says precisely what is reproduced: the stored component z-scores exactly as they enter CombZ, and CombZ recomposed from them. The upstream raw-to-z standardisation is not re-derived (`docs/COMBZ_CANONICAL_DEFINITION.md` section 4a).

## Inputs (all hash-gated; any mismatch stops the run)

| Input | Gate |
|---|---|
| ebb_v101 `analysis_ready/canonical/behavior_bundle/ebb_v101_20260929_b2ce507/` | `00_manifest.csv` sha256 `ec59aa33...`; every file by bytes and sha256; no extra file; FROZEN row in `BUNDLE_REGISTRY.csv`. Used: B1, B2, A4, A2b and `I_analysis_config.json` (metric labels) |
| `canonical/later_outcome_combz/tables/later_outcome_combz_animal_level.csv` | sha256 `1f6a2a69...` (the Stage 30 input hash) |
| `combz_component_definition.csv`, `combz_classification_thresholds.csv` | sha256 `af3c2734...`, `e1536f0c...` |
| `Analysis/sus_animals.csv`, `Analysis/con_animals.csv` | sha256 `dea3804b...`, `eddd2ee9...` |
| Frozen CombZ definition code `Analysis/build_later_outcome_combz.R` | `git hash-object` and the HEAD blob both equal `d850859e...`; committed and clean for `--real` |

**The frozen CombZ definition is parsed, never run.**
- The producer is a runner that writes the canonical CombZ tables, so it is never sourced.
- `fsb_read_combz_definition()` parses the file and evaluates only:
  - the seven definition constants, one top-level assignment each, whose right-hand sides may contain only literals, `c()` and `tibble::tribble()`;
  - the four consecutive composite-rule lines, `comp_mat <- ...`, `n_components_present <- rowSums(is.finite(...))`, `CombZ <- rowMeans(..., na.rm = TRUE)` and the all-missing NA line. These are evaluated verbatim, in an empty environment holding only the component matrix.
- The parsed constants are also checked against `combz_component_definition.csv`: order, `sign_inverted`, column letters, weights 1/6 and ids.

**Cross-checks.**
- The CombZ table equals ebb_v101 A4 for all 117 animals: Sex, Batch and Group identical, CombZ within 1e-12.
- The CombZ table `outcome_group` equals the list rule: CON if on the con list, else SUS if on the sus list, else RES.
- Every B1 animal's Sex, Batch, Group and CombZ equal the CombZ table's.
- The thresholds equal ebb_v101 A2b.

## Tables

| Table | Rows | Content |
|---|---|---|
| `F1_con_cage_means.csv` | 96 | {crossing_rate, shared_zone_use, occupancy_dispersion, fragmentation} x Sex x CC1-CC4 x 3 CON cages. Columns: measure, measure_label, unit, tier (from the ebb_v101 config), Sex, CC, Batch, CageEpisodeID, Group, n_animals, n_in_cage, animals, cage_mean |
| `F1b_con_reference_means.csv` | 32 | The CON n and mean per measure x Sex x CC, copied from ebb_v101 B2 |
| `F2_combz_components.csv` | 702 | 117 animals x 6 components. Columns: AnimalNum, Sex, Batch, Group, component, component_order, raw_value, reference_mean, reference_sd, z, direction, signed_z, present; then n_components_present, CombZ, susceptibility_threshold, below_threshold, heatmap_order_within_sex |
| `F2b_combz_definition.csv` | 6 | Per component: the source table and column, the upstream workbook column, the raw measure and derivation, the standardisation and reference population as implemented, the SD convention, whether it is reproducible, the direction and why, the weight, the relationship to CombZ, any upstream correction, and n present / missing |
| `H_provenance.csv`, `H2_inputs.csv`, `H3_gate_results.csv`, `00_manifest.csv` | | As in the Stage 30 figure bundle |

### F1 gates

- Exactly 3 CON cage epochs per sex per CC, one per batch: Female B3/B4/B6, Male B1/B2/B5.
- Each cage has 4 CON animals with a value, and `n_in_cage` is 4.
- The cage sizes sum to the B2 CON n (12).
- CON cage epochs hold only CON animals, and each CON group keeps the same 4 animals at CC1-CC4.
- The mean of the 3 cage means equals the B2 CON mean within 1e-12. This holds because the cage sizes are equal.

### F2 semantics and gates

- `signed_z` is the stored canonical component: the value as it enters CombZ, identical double for double.
- `direction` is -1 for delta_cort, adrenal_weight and spleen_weight. The frozen definition inverts these after standardisation, so that higher always means more resilient-like.
- `z = direction x signed_z` is the standardised value before the inversion.
- `raw_value`, `reference_mean` and `reference_sd` are NA.
  - The canonical producer carries the upstream z-scores verbatim, and the upstream standardisation is written as positional per-sex formula blocks.
  - Re-deriving raw to z would change the endpoint (`docs/COMBZ_CANONICAL_DEFINITION.md` section 4a). F2b states this per component.
- **Recomposition gate:** CombZ recomposed from `signed_z` by the producer's own rule equals the stored CombZ within 1e-12 for all 117 animals, and `n_components_present` matches. Two animals have 5 of 6 components.
- **Classification check:** for every SIS animal, `below_threshold` (CombZ < the stored within-sex threshold) holds exactly when Group is SUS.

## Output and modes

- REAL output goes to `analysis_ready/canonical/figure_support_bundle/<bundle_id>/`, with `BUNDLE_REGISTRY.csv` beside it (`bundle_id, status, manifest_sha256, stage29_bundle_id, combz_table_sha256, mmm_git_commit, created_at`) and `logs/<bundle_id>_gate_results.csv`.
  - `bundle_id = fsb_v1_<YYYYMMDD>_<commit7>`.
  - The folder is written once, through a staging folder, and files are 0444.
  - The post-write gates run before the registry row is appended.
- `--dry-run` writes to `MMM_FSB_DRY_ROOT`, or to the default `evidence/fsb_dry`, which must be on C: and outside analysis_ready and the repo. Its status is `DRY_RUN_NOT_FOR_USE`.
- `--real` is refused unless the MMM files it uses are committed and clean and the committed runner is executing.
- The runner refuses without a mode.
