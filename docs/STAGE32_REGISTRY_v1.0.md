# Stage 32 registry v1.0: SIS exposure, within-episode adaptation, and early prediction beyond Batch

**Status.** FROZEN 2026-09-30T22:41:10+0200, before any Stage 32 metric table joined to Group/CombZ was built and before any model was fitted. Committed alone in MMMSociability (together with its §6 table, `docs/STAGE32_HYPOTHESES_v1.0.csv`), hashed, and copied read-only to `<AR>/canonical/stage32_registry/v1.0/`. The runner refuses to fit unless the frozen sha256 matches.

**Tier.** POST HOC relative to the original experiment and the earlier analysis plans. These results had already been seen:
- the Stage 29 SIS−CON estimates;
- the legacy Stages 21–25 within-episode results (v1 data);
- the Stage 30 screen;
- the 29b post hoc contrasts;
- the Figure 1 candidate plots.

Stage 32 is **prospectively registered before its own fitting**.
- **Decision basis:** POST_HOC_CONTEXT.
- **Scope:** the frozen Stage 29 and Stage 30 results and families are unchanged. No word used for Stage 32 may be "confirmatory" or "preregistered".

## 1. Data and windows

**Stream and inputs**
- **Event stream:** the canonical Stage 29 stream (`Functions/rfid_event_stream.R`), on data version `v2_cage_label_correction_2026-09-28` (manifest bb33a111) with raw-data seeds.
- **Window metrics:** `s30mv_window_metrics(full = TRUE, na_if_file_ends_early = FALSE)` (`Functions/stage30_movement.R`, unchanged), one window at a time, combined with the explicit coverage rule in §2.
- **Labels and outcome:**
  - Group from the canonical lists (con → CON, else sus → SUS, else RES), checked against ebb_v101;
  - CombZ from `canonical/later_outcome_combz/.../later_outcome_combz_animal_level.csv` (sha 1f6a2a69);
  - CombZ_wb = CombZ minus its batch mean, within the analysed population (SIS for within-SIS models).
- **CageEpisodeID (CEID):** Batch|System|CC (v2 board-based System).

**Phases** (logger clock; anchor = 18:30 on the calendar day of each file's first timestamp):
- Active phase k: [anchor + 24(k−1) h, anchor + 24(k−1) h + 12 h).
- Light phase k: [anchor + 24(k−1) h + 12 h, anchor + 24k h).

**Clean phase set**
- CC1–CC3: A1–A4 and L1–L3.
- CC4: **A1, L1 and A2 only.** CC4 L2 onward coincides with the scheduled sucrose-habituation / grid-exposure days.
- CC4 A3–A4 are not imputed.

**Metrics**

| Tier | Measure |
|---|---|
| PRIMARY | position-change rate (`crossing_rate`, position changes h⁻¹); social-spatial overlap = shared RFID-position occupancy (`shared_zone_use`) |
| SECONDARY (estimation only) | occupancy dispersion (`occupancy_dispersion`); fragmentation (`fragmentation`) |
| LIGHT (estimation only; Module E) | light-phase position-change rate per light phase |

No new rest variables. The ≥40-s inactivity measure stays a frozen Stage 30 measure.

## 2. Coverage rule (declared before outcome contact)

- **Board observation interval.** For each file × System, the interval runs from the first to the last raw record of any animal on that board. Carry-forward beyond the board's last raw record is **not** coverage.
- **When a phase is complete.** A phase is complete for an animal only if all of these hold:
  - (a) the phase block is kept in v2 preprocessing;
  - (b) the board observation interval starts at or before the phase start (for A1: at or before t0, as in Stage 29);
  - (c) the board observation interval ends at or after the phase end − 10 min;
  - (d) the phase is in the clean phase set.
- **Explicit exclusion:** `B5|sys.1|CC4` from L2 onward, for animals 314, 318, OR620 and OR630. These fall outside the clean set anyway.
- **Primary analyses** use the ≤10-min tolerance.
- **Strict sensitivity** (for H07–H10 only): tolerance 0, i.e. the board ends at or after the phase end. This removes every A4 whose recording ends before 06:30.
- **Output:** a per-animal × phase coverage manifest.

## 3. Coding and engine

- **Coding:**
  - g_SIS = +½ SIS / −½ CON; sex_c = +½ F / −½ M; Batch treatment-coded.
  - CC as a factor (c2–c4 contrasts to CC1).
  - phase = 0, 1, 2, 3 for A1–A4 (linear, primary); phase_f = a factor (sensitivity).
  - conCage/sisCage per CEID; isCON per animal.
- **Engine:** the frozen engine (`Functions/rfid_canonical_inference.R`): lmerTest, REML, bobyqa, Kenward–Roger (KR) through `contest`, and Holm over the declared m.
  - If KR fails numerically for a Stage 32 model, the pre-declared fallback is Satterthwaite. It is flagged `ddf_fallback` and reported.
  - A fit error or non-convergence makes the row FAILED, with no estimate displayed.
  - Singular fits are kept and flagged.
- **OLS models** (H13, and Module F prediction fits) use `lm`. H13 uses CR2 by CC1 CEID with Satterthwaite df (`clubSandwich`), as in Stage 29 CONTINUOUS.

## 4. Models

**A-exp: first active phase after CC1 (A1 of CC1), all 111 animals.** This is the frozen EXPOSURE_CC1 formula:
`y ~ Batch + g_SIS + g_SIS:sex_c + (0 + conCage | CEID) + (0 + sisCage | CEID)`

**A-dec: estimation only.** A three-level decomposition: `y ~ Batch + group3 + group3:sex_c + (0 + conCage | CEID) + (0 + sisCage | CEID)`, with group3 treatment-coded against CON. It gives batch-balanced sex-averaged RES−CON, SUS−CON and RES−SUS.

**A-cz: A1 of CC1, SIS only (87; overlap 85):**
`y ~ Batch + CombZ_wb + CombZ_wb:sex_c + (1 | CEID)`

**B-exp: A1 of each of CC1–CC4, all 111 animals.** The frozen EXPOSURE_TR formula. It includes `(1 | AnimalNum)` (rate: `(1 + cc1 || AnimalNum)`), `(0 + conCage | CEID) + (0 + sisCage | CEID) + (0 + isCON | Batch)`, and the fixed part `Batch + g_SIS + c2 + c3 + c4 + g_SIS:(c2+c3+c4) + g_SIS:sex_c + (c2+c3+c4):sex_c + g_SIS:(c2+c3+c4):sex_c`.

**B-cz: carried over.** The Stage 30 SCREEN-L results (CombZ_wb × CC) are not re-tested.

**C-exp: within-episode adaptation, all 111 animals.** Rows are active phases A1–A4 at CC1–CC3 and A1–A2 at CC4:
`y ~ Batch + CC + g_SIS + phase + g_SIS:phase + g_SIS:CC + (1 + phase || AnimalNum) + (0 + conCage | CEID) + (0 + sisCage | CEID) + (0 + phase | CEID)`
- `(0 + phase | CEID)` is a cage-episode random phase slope. The cage group, not the animal, replicates the exposure slope.

**C-cz: within-episode adaptation, SIS only:**
`y ~ Batch + CC + CombZ_wb + phase + CombZ_wb:phase + CombZ_wb:CC + (1 + phase || AnimalNum) + (1 + phase || CEID)`

**C secondary (estimation only, only if the primary model is interpretable).** Each adds one of these to C-exp or C-cz:
- `phase:CC` and `g_SIS:phase:CC` (or `CombZ_wb:phase:CC`);
- `g_SIS:phase:sex_c` (or `CombZ_wb:phase:sex_c`).

**D: later-phase behavioural level (estimation only).**
- Model: the categorical-phase version of C-exp and C-cz (phase_f in place of phase, with the g_SIS or CombZ_wb interactions by phase_f), restricted to CC1–CC3 rows.
- Estimand: an L-vector giving the mean of A2, A3 and A4, averaged over CC1–CC3:
  - SIS − CON at that level;
  - the CombZ_wb slope at that level.
- The term "later-phase behavioural level" is used throughout, never "settled state". The categorical phase means are reported to show whether A2–A4 plateau.

**E: light-phase position-change rate (estimation only).** L1–L3 at CC1–CC3 and L1 at CC4, with light index 0, 1, 2:
- the C-exp and C-cz structures, with the light rate as the outcome;
- SIS − CON at L1 of CC1.

**Secondary metrics (estimation only).** A-exp, A-cz, B-exp and C-exp / C-cz for occupancy dispersion and fragmentation. Report estimates and CIs, with no p.

## 5. Module F: early prediction beyond Batch

**Data**
- The frozen Stage 09 input `pipeline/09_early_prediction/10min/tables/model_ladder_input.csv` (sha da78f80e). Its early window is A1 of CC1 (18:30–06:30, 72 × 10-min bins).
- Main population: SIS (n = 87). The all-animal version (111) is kept for continuity, estimation only.
- Stage 09 is not re-run.

**Models** (OLS; fitted inside each training fold; no standardisation; median imputation from the training fold if any NA)

| Label | Formula |
|---|---|
| (A) Batch-only | `CombZ ~ Batch` |
| (B) behaviour-only | `CombZ ~ Movement_mean` (movement_mean); `CombZ ~ Movement_mean + Movement_rmssd + Entropy_acf1` (primary_behavior_family) |
| (C) Batch + behaviour | `CombZ ~ Batch + <behaviour set>` |

**Validation**
- **Primary:** leave-one-cage-out (LOCO), with the CC1 CEID as the grouping unit. It is deterministic.
- **R² definition:** R²_cv = 1 − SSE / SST, with SST about the full-sample mean.
- **Primary quantity:** **ΔR²_LOCO = R²_LOCO(C) − R²_LOCO(A)**, for each behaviour set.

**Tests (H11, H12)**
- 1000 **within-Batch** permutations of CombZ (seed 20260811). Each permutation refits both A and C.
- One-sided p = (#{ΔR²_null ≥ ΔR²_obs} + 1) / 1001.

**Also reported** (sensitivity / continuity):
- R²_LOCO for A, B and C;
- repeated cage-grouped 5-fold CV (100 repeats, seed 521);
- LOAO;
- leave-one-batch-out (LOBO). For batch models, the held-out batch's intercept is the mean of the training batch effects. LOBO is robustness only, given only 6 batches.
- the unrestricted permutation null.

**H13: secondary, single prespecified test, not a discovery claim.**
- Model: `CombZ ~ Batch + Movement_mean + Movement_mean:sex_c`, SIS only, OLS, CR2 by CC1 CEID, Satterthwaite df.
- Estimand: the Movement_mean:sex_c coefficient.
- No sex difference is inferred from separate female/male slopes.

## 6. Hypothesis table

This is the only set of new inferential tests: 12 tests in 6 Holm families, one family per prespecified scientific question, plus H13, a secondary single test.

| HypothesisID | FamilyID | Module | Metric | Population | Window | Model | Estimand / test | Status | Multiplicity |
|---|---|---|---|---|---|---|---|---|---|
| H01 | F32-A1-EXP | A | position-change rate | all 111 | A1 of CC1 | A-exp | g_SIS (SIS − CON, sex-averaged), KR t | primary | Holm, m = 2 (H01, H02) |
| H02 | F32-A1-EXP | A | social-spatial overlap | all 109 (non-NA) | A1 of CC1 | A-exp | g_SIS, KR t | primary | Holm, m = 2 |
| H03 | F32-A1-CZ | A | position-change rate | SIS 87 | A1 of CC1 | A-cz | CombZ_wb slope (sex-averaged), KR t | primary | Holm, m = 2 (H03, H04) |
| H04 | F32-A1-CZ | A | social-spatial overlap | SIS 85 | A1 of CC1 | A-cz | CombZ_wb slope, KR t | primary | Holm, m = 2 |
| H05 | F32-CC-EXP | B | position-change rate | all 111 | A1 of CC1–CC4 | B-exp | joint KR F(3) of g_SIS:(c2 + c3 + c4) | primary | Holm, m = 2 (H05, H06) |
| H06 | F32-CC-EXP | B | social-spatial overlap | all 111 (non-NA) | A1 of CC1–CC4 | B-exp | joint KR F(3) | primary | Holm, m = 2 |
| H07 | F32-AD-EXP | C | position-change rate | all 111 | A1–A4 CC1–CC3; A1–A2 CC4 | C-exp | g_SIS:phase, KR t | primary | Holm, m = 2 (H07, H08) |
| H08 | F32-AD-EXP | C | social-spatial overlap | all 111 (non-NA) | same | C-exp | g_SIS:phase, KR t | primary | Holm, m = 2 |
| H09 | F32-AD-CZ | C | position-change rate | SIS 87 | same | C-cz | CombZ_wb:phase, KR t | primary | Holm, m = 2 (H09, H10) |
| H10 | F32-AD-CZ | C | social-spatial overlap | SIS (non-NA) | same | C-cz | CombZ_wb:phase, KR t | primary | Holm, m = 2 |
| H11 | F32-PRED | F | movement_mean model | SIS 87 | Stage 09 early window (A1 of CC1) | F (A vs C) | ΔR²_LOCO, within-Batch permutation p | primary | Holm, m = 2 (H11, H12) |
| H12 | F32-PRED | F | primary_behavior_family model | SIS 87 | same | F (A vs C) | ΔR²_LOCO, within-Batch permutation p | primary | Holm, m = 2 |
| H13 | none | F | Movement_mean | SIS 87 | same | H13 | Movement_mean:sex_c, CR2 t | secondary | none (single, unadjusted; not a discovery claim) |

**Everything else is estimation only (estimate and 95% CI; no p):**
- A-dec;
- g_SIS:sex_c and CombZ_wb:sex_c at A1;
- the Exposure × CC × Sex terms;
- the C secondary terms;
- D and E;
- the secondary metrics;
- the all-animal prediction;
- LOAO, repeated 5-fold, LOBO and the unrestricted null.

**No family may be expanded after results are seen.**

## 7. Sensitivities (not tests)

- **C (H07–H10):**
  - categorical phase (phase_f), including the A1→A2 change and the A2–A4 plateau contrast;
  - the strict-completeness coverage rule;
  - an added `(1 | AnimalNum:CC)`;
  - C-exp only: separate CON and SIS cage phase-slope variances, `(0 + conCage:phase | CEID) + (0 + sisCage:phase | CEID)`.
- **A-exp and B-exp:** one common cage variance, `(1 | CEID)`.
- **Module F:** repeated cage-grouped 5-fold, LOBO, LOAO, the unrestricted null and the all-animal version.

## 8. Gates

**Before fitting**
- The registry sha.
- The input shas: v2 manifest, ebb_v101 manifest, CombZ table, lists, Stage 09 input.
- The package versions.
- The recomputed A1-of-CC1–CC4 metrics must equal ebb_v101 `canonical_window_metrics` / B1 within 1e-9.
- Coverage counts must match the audit: all 111 animals in every clean phase, except B5 sys.1 at CC4, whose later phases are outside the clean set.

**After fitting (reproduction)**
- The A-exp estimates must equal the frozen EXPOSURE_CC1 rows within 1e-6.
- The B-exp estimates must equal the frozen EXPOSURE_TR rows within 1e-6.

**Run once**
- One REAL run to `pipeline/32_behavior_exposure_adaptation/v1.0_<commit7>/`. It refuses if a v1.0_* folder already exists, and all outputs are read-only.

## 9. Interpretation rules (binding for any text)

- **Movement.** A Movement × CC or Movement × phase pattern shared by CON and SIS is not described as adaptation to social instability. That claim needs a supporting Exposure interaction (H05–H08). Relocation, novelty or repeated handling, and development over about P25–P37 are named as alternatives.
- **Social-spatial overlap.** Differences may reflect familiarity (CON stays with its partners) against repeated regrouping (SIS). This mechanism is not asserted from plots; only the prespecified contrasts, with their uncertainty, are reported.
- **Replication.** Every CON-referenced estimate states that CON is 6 intact groups (3 per sex, one per batch) and that the KR df is small. Exposure × Sex is batch-confounded because sex is nested in batch.
- **Prediction.** No individual-level prediction claim is made unless H11/H12 show held-out information beyond Batch.
- **Forbidden words:** acute, immediate response, sleep, rest (for rates), settled state, confirmatory, preregistered, significant, stars.

## 10. Outputs

**Tables**
- `coverage_manifest.csv`
- `window_metrics_long.csv`
- `descriptives.csv` (group × sex × CC × phase n/mean/sd, plus CON cage means)
- `models.csv`
- `estimates.csv` (L-vectors, estimate, SE, df, CI, p)
- `joint_tests.csv`
- `multiplicity.csv`
- `sensitivities.csv`
- `diagnostics.csv`
- `prediction_performance.csv`
- `prediction_permutations.csv`
- `prediction_heldout.csv`

**Audit and plots**
- `audit/{input_hashes, gate_results, output_manifest, run_manifest}.csv` and a README.
- `qc_plots/`: lightweight QC PNGs, e.g. group × phase means with CON cage lines, and prediction A vs C.

**Tests.** Synthetic tests only (`Testing/tests/test_stage32_*.R`). They never source the runner.
