# Errata and provenance notes for the frozen behaviour configuration v1.0.0

The frozen configuration is `Functions/behavior_analysis_config.R` v1.0.0: JSON SHA-256 `33d22430b0e6d3a45a28bea8546a185c523aedf72055ddf5ae208cff51f8950e`, frozen 2026-09-27 at commit `a9b7a2c`. The immutable record is `analysis_ready/canonical/behavior_config/v1.0.0/`.

**This file does not change v1.0.0.** It records where the frozen text is imprecise or incomplete relative to what the code and data actually do. None of the points below changes a model, estimate, test, multiplicity family or decision of the frozen analysis. The points were found in the 2026-09-27/28 independent verification audit, whose evidence is in `e9_behavior_rationalization_audit_2026-09-27/evidence/final_audit/`.

A documentation-only revision, v1.0.1, is **proposed** at the end. It is not applied. If it is adopted, it must be frozen as a new version under `meta$change_rule`; v1.0.0 stays preserved and remains the version the Stage 29 run and bundle `ebb_v100_20260927_95e5dc8` used.

## E1. Seed rule: "on-grid reads only" vs preprocessing that snaps off-grid reads

**Frozen text** (`event_stream$seed`): the seed is "the last on-grid raw read before the file's first timestamp t0". The Stage 01 rule is implemented literally in `Functions/rfid_event_stream.R`.

**Actual preprocessing** (`Functions/animalpos_preprocessing_helpers.R`, `animalpos_position_id`): off-grid reads are floor-snapped to a PositionID and kept in the window stream. The state at t0 is therefore not always the position the same read would receive one second later.

**Extent:** the "snapped" seed rule would pick a different read for 34 of 420 seeds, and a different position for 27 of them. Evidence: `V2_outcomes/v2_variant_diff_snapped_any.csv`, `D3_seed_rule/`.

- 69 animal-windows change in at least one metric.
- Only 8 windows change their event count, by at most 1 event, i.e. at most 0.083 crossings/h.
- **At CC1:**
  - no SIS animal's event count or crossing rate changes;
  - the only CC1 event-count change is in a CON female (OR415, +1 event);
  - two SIS females in B3|sys.1 (OR428 and OR432, both SUS) change only position-based metrics: shared zone use +0.0006, and occupancy dispersion -0.031 for OR428;
  - four B2|sys.5 males change shared zone use by at most 0.013.
- Across all CCs the maximum changes are:

  | Metric | Maximum change |
  |---|---|
  | shared_zone_use | 0.020 |
  | occupancy_dispersion | 0.133 bits (OR426, CC3) |
  | fragmentation | 0.009 |

- **Effect on the frozen CC1 estimates** (refitted with the variant values; `D3_seed_rule/d3b_all_constructs.csv`):

  | Construct | Effect |
  |---|---|
  | crossing rate (Q1, female and male RES-SUS) | identical to 7e-9 |
  | fragmentation | identical |
  | shared zone use | Q1 0.01281 -> 0.01038 (0.08 SE); p 0.657 -> 0.719 |
  | occupancy dispersion | Q1 -0.1341 -> -0.1321; p 0.244 -> 0.251 |

  No Holm decision changes.

**Correction to an earlier statement.** The 2026-09-27 audit summary said "no female CC1 window is affected". That is imprecise. The corrected statement is that no CC1 crossing rate of any SIS animal is affected. Position-based metrics of two SIS female CC1 windows change slightly, and one CON female CC1 window gains one event.

## E1b. The first reader of each file is never seeded

`mmm_evs_seeds` seeds only "late" animals, i.e. those whose first read is after t0. The single animal whose first read **defines** t0 in each SourceFile is never seeded. For that animal:
- its first read counts as the start of observation;
- the change from its last pre-t0 raw read is not counted.

This affects 24 of the 444 active windows, each by -1 event (-0.083/h). At CC1 the affected animals are 13857, 675, OQ762, OR140, OR420 and OR550.

Stage 01 uses the same rule, so the Stage 01 event gate cannot detect it. The historical half-hour pipeline counted this crossing, through its 18:30 carry-in.

Correcting E1 and E1b together (`B1_main/b1_09`) changes the frozen female CC1 RES-SUS from -0.7079 (p 0.7257) to -0.7123 (p 0.7242), and the male from +2.2887 to +2.3020. No decision changes.

## E2. `cadjust = TRUE` has no effect with HC3

**Frozen text** (`sensitivities$SHARED_ZONE$D1$longitudinal`): `sandwich::vcovCL(..., type = 'HC3', cadjust = TRUE, multi0 = FALSE)`.

`sandwich::vcovCL` applies the G/(G-1) cluster adjustment only for HC0/HC1. For HC2/HC3 the bias adjustment replaces it, and `cadjust` is ignored. The effective D1 longitudinal estimator is therefore two-way HC3 without a G/(G-1) factor.

This is exactly what was null-calibrated before the freeze: HC3 rejected 3.0% at nominal 5%. The results are correct; the argument is inert.

## E3. Standardizer definition

**Frozen text** (`metrics$<construct>$standardizer_definition`): "CC1 SIS pooled within-batch SD".

**Actual computation** (pre-freeze, `evidence/revision_2026-09-27/rev_01_design_rank_precision.R:40`): `sd(resid(lm(y ~ Batch)))` on the 87 (or 85) CC1 SIS animals. That uses an n-1 divisor, not the pooled within-batch n-6 divisor, so the frozen constants are the pooled SD multiplied by sqrt((n-6)/(n-1)):

| Construct | Frozen constant | Pooled within-batch SD |
|---|---|---|
| crossing rate | 5.1698 | 5.3270 |

This affects only the supplementary standardized values (by about 3%). Raw-unit results are unaffected, and the constants are used as frozen.

## E4. Random-slope evidence numbers are REML likelihood ratios

**Frozen text** (`models$TR_POOLED$random_slope$evidence`): "LR 21.7, dBIC -15.8 (F -7.1, M -3.2); covariance dBIC +5.7".

These are REML comparisons. That is valid, because the fixed parts are identical. Under ML the support for the uncorrelated cc1 slope is stronger (`V5_het_slope`):

- LR 23.63, boundary p 5.8e-7, dBIC -17.78 (F -8.57, M -3.43);
- covariance still not supported (dBIC +5.57).

## E5. Cohort description

**Frozen text** (`population$rfid_cohort`): "6 B1 males never tracked (0001, 0002, OQ750-OQ753)".

- 0001, 0002, OQ750 and OQ753 appear in no raw file.
- OQ751 (B1 CC1 sys.2, 223 reads) and OQ752 (B1 CC1 sys.5, 603 reads) **were** tracked at CC1 and were removed by `raw_data/excluded_animals.csv`. Their absence from CC2-CC4 is in the raw data.
- The `excluded_animals.csv` entry `HKH0-OR567` matches no raw label; OR567 itself is excluded.
- The analysis population is unaffected.

## E6. Items that the text implies but the code does not do (none fired in the v1.0.0 run)

- **`diagnostics$zone_variance`.** If triggered, comparator B becomes co-primary for shared zone use. The code only records `triggered`. It did not fire: CC1 LR 1.42 (dBIC +3.02); CC1-CC4 LR 3.21 (dBIC +2.63).
- **`models$CONTINUOUS`.** "CR2 SE as robustness" is filled only for the pooled slope rows, not for the by-sex rows.
- **D2 longitudinal implied magnitude.** The code uses beta_k x f_k with the CC-k factor. The exact animal-level change of DiD from CC1 to CCk would be (beta_sex + beta_k) f_k - beta_sex f_1. Both forms lie inside the primary CIs, and no assessment changes.
- **`hardware_flag` column.** It is defined as n_positions_occupied < 8 and flags 7 windows, including 692 CC1. S13 correctly uses the frozen hard-coded list of 6 and retains 692. The column is descriptive only.
- **Kenward-Roger on singular fits.** KR retains the zero variance component and is mildly conservative: SE up to 4.1% larger than the fit without it. Five focal fits are singular, and no Holm decision changes (`D2_singular/d2_singular_holm.csv`).
- **Pre-freeze longitudinal dyad-robust null calibration.** The 14-17% rates quoted in `d2_calibration/RESULTS.txt` cannot be regenerated, because the pre-freeze `mmm_ci_d2` version was never committed. The CC1 calibrations and all CR2/HC3 calibrations reproduce exactly.

## E7. Implemented in code after the run (commit f3a25da); output and policy only

- **`sensitivities$p_value_policy`.** The D2 dyadic joint row and the D3 row no longer export a p-value. F and df are kept. The duplicated `estimand` column of the D3 row is removed.
- **`fitting$failure_rule`.** An erroring fit becomes an explicit FAILED model, with the error kept in `model_registry.csv`, instead of aborting the run.
  - Every helper returns FAILED rows for it.
  - glmmTMB convergence problems get an nlminb-vs-optim(BFGS) agreement check.
  - Unexpected errors in the robustness, diagnostic, secondary and Stage 09 blocks are recorded in `audit/run_failures.csv`.
  - The v1.0.0 run had 0 failures, and the re-run at f3a25da (2026-09-28, still config v1.0.0) also has 0: 233 models, 0 FAILED, empty `run_failures.csv`.
- **Re-run check** (`evidence/final_audit/E1_stage29_rerun_f3a25da/`):
  - 20 of 23 Stage 29 tables are byte-identical to the 0555c90 run.
  - `diagnostics.csv` gains `n_failed`, and `model_registry.csv` gains `error`.
  - `shared_zone_robustness.csv` has p_raw withheld in exactly the D2 joint row (was 0.2645) and the D3 row (was 0.7442), and the duplicated column is removed.
  - Every shared column is identical to within 1e-9.
- **Dry-run bundle check:** a bundle built from the re-run passes every 16b check. 17 of 19 bundle files are byte-identical to `ebb_v100_20260927_95e5dc8`, including P, C2, D, E and F. The remaining two show only the same added columns and withheld p-values.
- **No new FROZEN bundle has been created.** `ebb_v100_20260927_95e5dc8` remains the canonical bundle.

## Proposed v1.0.1 (documentation only; not applied)

| Field | Proposed text change |
|---|---|
| `event_stream$seed` | State that the seed is the last **on-grid** raw read before t0 (Stage 01 rule), and that preprocessing floor-snaps off-grid reads **inside** the window. State that the animal whose first read defines t0 is not seeded (E1b). Quote the extents in E1 and E1b. |
| `sensitivities$SHARED_ZONE$D1$longitudinal` | Drop `cadjust = TRUE` or annotate "inert for HC3". |
| `metrics$*$standardizer_definition` | "SD of CC1 SIS residuals from lm(y ~ Batch), n - 1 divisor". Values unchanged. |
| `models$TR_POOLED$random_slope$evidence` | Label as REML LR; add the ML values from E4. |
| `population$rfid_cohort` | Replace "never tracked" with the E5 description. |
| `fitting$failure_rule`, `fitting$optimizer_check` | Describe the FAILED-row mechanism, the glmmTMB optimizer check and the block-level `run_failures.csv`. |
| `diagnostics$zone_variance` | State that the co-primary consequence is applied by the analyst (not programmatically) if triggered. |
| `meta$change_log` | "v1.0.1: documentation only; no analytic change; changed after outcome inspection (errata E1-E7)". |
