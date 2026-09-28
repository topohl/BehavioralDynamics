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

## E8. The Stage 29 movement variable is an RFID position-change rate, not an antenna-crossing rate (terminology only)

**Frozen text.**
- `metrics$crossing_rate`: label "antenna-crossing rate", interpretation "overall locomotor activity", unit "crossings/hour".
- `event_stream$source`: "change-only RFID reads", and the input rows are called "reads" throughout.
- The same wording appears in:
  - the fragmentation and descriptive definitions and `figure1`;
  - `behavior_analysis_config.json`, which equals the bundle's `I_analysis_config.json`;
  - two strings hard-coded in `Analysis/16b_canonical_behavior_bundle.R` (:89, :130-132), which reached bundle `ebb_v100_20260927_95e5dc8`: `A0_design_timeline.csv` line 3 and the `unit` column of `C2_estimates.csv` (122 rows).

**What the variable counts.** Evidence: Stage 30 audit V1, all 24 Stage 29 source files compared with their raw PhenoSoft logs (`stage30_sleep_cookie/evidence/wfB/V1_vendor_filter/`).
- **How a record is written.** The input rows are vendor AnimalPos position records. The vendor writes a record only when the tag's estimated position is at least 200 grid units from the last record. The estimated position is the read-weighted mean antenna position over 0.5 s; this reproduces 99.91% of 646,817 records.
- **Step size.** The smallest step is exactly 200 units in all 24 files. Every step moves at least 165 units along the long axis, i.e. about two antenna spacings.
- **Correspondence with Stage 29 events.** A Stage 29 event is one of these records, 1:1: 592,609 rows, 0 synthetic, and n_events recounted in 444/444 windows.
- **What the antennas actually log.** They read continuously. The raw logs contain 7.5–12.0 times more antenna changes than there are records (pooled 8.75×), and 92% of those changes are neighbour moves that are never registered.
- **So the variable is not:** an antenna-crossing count, a distance, or a validated measure of overall locomotor activity. A gap between records is not stillness.

**Corrected wording.**
- **Names and unit.** Use "RFID position-change rate", unit "position changes/h", and "light-phase RFID position-change rate".
- **First use.** "vendor-defined RFID position-change rate (a position change is registered only when the tag's estimated position moves at least 200 grid units, about two antenna spacings)".
- **Unchanged.** `crossing_rate` and the related ids stay as legacy identifiers. No number, model, family or decision changes.

**Where the wording occurs.** Every occurrence is in `stage30_sleep_cookie/evidence/wfC/R7_terminology_audit/r7_06_occurrence_table_v2.csv` (111 rows): frozen config, code comments, 16b export strings, bundle, MMM docs, the Figure 1 branch (legend, contract, panel labels, annotation map, rendered SVG) and master Methods.
- The frozen config v1.0.0 and bundle `ebb_v100_20260927_95e5dc8` are not edited; this erratum corrects them.
- The two 16b strings change only together with a future bundle.
- **Manuscript text is not edited here.** The Figure 1 branch needs the relabel and a re-render. The master Methods needs the unit definition.

## E9. Cage labels of four animal-files were wrong; data version v2 and a data-integrity re-run (2026-09-28)

**Finding.** In 4 of 448 animal-files the cage label (the system suffix of the vendor label) disagrees with the antenna board that read the tag. The evidence establishes that the label is wrong and the board right in all four cases. It includes the planned round cages (GroupComposition), initScan placements, the vendor placeholder-label lineage, an experimenter's note and 100% board reads. Evidence folder: `stage30_sleep_cookie/evidence/wfC/R5_cage_metadata/`.

| File | Animal | Label | Correct | Correct CageEpisodeID | Basis |
|---|---|---|---|---|---|
| B1 CC2 | OQ764 | sys.5 | sys.2 | B1\|sys.2\|CC2 | 30-s mistaken first placement in cage 5; board 2 for 4 days (99.94%); the board partition equals the planned round-2 cage |
| B1 CC2 | OQ770 | sys.2 | sys.5 | B1\|sys.5\|CC2 | 77-s mistaken first placement in cage 2; board 5 (99.89%); the board partition equals the plan |
| B1 CC2 | OQ772 | sys.3 | sys.4 | B1\|sys.4\|CC2 | never read on board 3 (100% board 4); the Dec-2022 renamed vendor outputs already had sys.4; so OQ755 was not alone |
| B6 CC4 | OR646 | sys.2 | sys.5 | B6\|sys.5\|CC4 | initScan places her directly on board 5; 100% board 5 in CC4 and all later sessions; the experimenter's sheet says "OR646_sys2 in cage 5". The label was copied from the plan. **Protocol deviation:** re-housed with former mates 00690 (CC1) and 00694 (CC3), and cage 2 ran with 3 animals |

**Data version v2.**
- **Location:** `S:/.../Analysis/Behavior/RFID/MMMSociability/data_versions/v2_cage_label_correction_2026-09-28/`, with `cage_label_corrections.csv`, `MANIFEST_SHA256.csv` and `README.txt`.
- **Contents:** identical to the original `preprocessed_data/` except the System field of these animals' rows. That is 3,961 rows in B1 CC2 and 1,982 in B6 CC4; the other 22 files are byte-identical.
- **The original is unchanged** and remains the frozen v1.0.0 input.

**Re-run under the unchanged frozen models.** Evidence: `stage30_sleep_cookie/evidence/cage_fix_rerun/`.
- **The patched copy.** Stage 29 at commit 8497516 was run as a sandbox copy with 5 documented patches (`29_CAGEFIX.diff`):
  - the input directory;
  - the output directory;
  - the run record;
  - two data-integrity gates set to the corrected expectations: 346 instead of 345 shared-zone windows, and missing-window set {OQ770, OQ771} instead of {OQ755, OQ770, OQ771}.

  No model, contrast, family, estimator or threshold was changed.
- **Control.** The same sandbox on the original data reproduces all 23 frozen tables byte for byte.
- **Result.**
  - **CC1:** every CC1 analysis is identical, including P-CC1, S-CC1-ORG, FU-CC1, the continuous and cumulative estimates, the lag block and Stage 09.
  - **Longitudinal estimates:** they change by at most 0.47 SE (shared-zone use, TR_BY_SEX) and by at most 0.07 SE for the other constructs; SEs change by at most 7%.
  - **Q2b joint tests:**

    | Construct | p (frozen) | p (corrected) |
    |---|---|---|
    | shared-zone use | 0.826 | 0.755 |
    | position-change rate (crossing_rate) | 0.954 | 0.954 |
    | fragmentation | 0.388 | 0.372 |
    | occupancy dispersion | 0.478 | 0.470 |

  - **No multiplicity decision changes.** 24/24 family members are still non-rejecting.
  - **Other decisions:** no robustness label, heteroscedasticity materiality flag, diagnostic trigger or shared-zone D1/D2 decision changes.
  - **LOBO:** shared-zone Q2b component c2 becomes robust to single-batch removal (5/6 → 6/6).
- The frozen outputs and bundle are not replaced. Whether the corrected results become the reported ones is the user's decision.

## E10. Group guard of the canonical reader (commit 8497516)

`mmm_evs_read_preprocessed` checked for a Group column after `fread(select = ...)` had already dropped it, so the guard could never fire. It now checks the file header first.
- On the 24 inputs the reader returns an identical table (592,609 rows), and none of them has a Group column.
- The sandbox control run at 8497516 reproduces all 23 frozen tables byte for byte.
- A synthetic contract test (`Testing/tests/test_rfid_event_stream_group_guard.R`) fails on the pre-fix code.
- Because canonical code has changed since the Stage 29 run commit, a future bundle needs a Stage 29 re-run at the new commit.

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
| `event_stream$source`, `$seed`, `$carry_forward`, `$event` | "vendor AnimalPos position records, written only when the estimated tag position has moved >= 200 grid units"; "position record" instead of "read"; carry-forward is "an assignment rule, not an observed location" (E8). |
| `metrics$crossing_rate` label, interpretation, unit (+ display_name) | "RFID position-change rate"; "vendor-registered position changes (>= 200 grid units, about two antenna spacings) per observed hour; not an antenna-crossing count, a distance or a validated locomotor measure"; "position changes/hour"; `crossing_rate` kept as legacy identifier (E8). |
| `metrics$light_phase_crossing_rate`, `windows$light_phase$role` | "light-phase RFID position-change rate", "position changes/hour"; never "sleep" or "rest" (E8). |
| `metrics$fragmentation$definition`, `metrics$descriptive` | "position change(s)" instead of "crossing(s)"; 1341.67 s is "3-process long-gap boundary of vendor inter-record intervals; not a rest or sleep criterion" (E8; provenance audited in Stage 30 R4). |
| `figure1` panels, units | "RFID position-change rate", "position changes/h" (E8). |
| `population$expected_counts$shared_zone_use$windows` and the Stage 29 complete-case missing set | For data version v2 only: 346 and {OQ770, OQ771} (E9). The frozen v1.0.0 values stay with data version v1. |
| `meta$change_log` | "v1.0.1: documentation only; no analytic change; changed after outcome inspection (errata E1-E10)". |

The full terminology row list is `stage30_sleep_cookie/evidence/wfC/R7_terminology_audit/r7_08_v101_rows.csv`.
