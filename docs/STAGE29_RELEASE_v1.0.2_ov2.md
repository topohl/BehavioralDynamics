# Stage 29 corrected-outcome release: config v1.0.2, outcome version ov2

Status: RELEASE completed on 2026-10-19, run `v102_dv2_a46be6a`, following the 00317 re-track and the authorized local integration. The original analytic definitions were retained; no batch means are removed in the primary prediction.

## What this release is

The frozen Stage 29 characterisation rerun on the corrected outcome (erratum E13). Nothing analytic changes.

- **Configuration.** v1.0.2 is an outcome-version revision of the frozen v1.0.0 (analytic parent, JSON SHA-256 `33d22430...`, commit `a9b7a2c`), like v1.0.1 was a data-version revision. The identity gate passes with 0 disallowed differences and 12 of 12 semantic checks, three of them new: a complete `outcome_versions` binding, a parent outcome version with the frozen RES/SUS counts, and every version keeping the frozen SIS total (87) and sex x group structure.
- **Outcome versions.**

  | Version | Tag | CombZ table | Lists | RFID SIS counts F RES/SUS, M RES/SUS |
  |---|---|---|---|---|
  | `ov1_original` | ov1 | `1f6a2a69...` (producer v1; archived to `analysis_ready/history/retired/later_outcome_combz_v1_1f6a2a69/`) | sus `dea3804b...` (35), con `eddd2ee9...` | 28/18, 25/16 |
  | `ov2_corrected_workbook` | ov2 | `5acb9a25a05cb32e44ce9523f9477a0bc307f52287e6d8cbd8d05110b9a40e10` (producer v2) | sus `65821453e64264b55cccdc86db2b161788e09fd1b5a4ee56e2786b513b00f131` (39), con `eddd2ee9...` | 26/20, 25/16; 00317 remains SUS |

- **Data version.** Unchanged: `v2_cage_label_correction_2026-09-28` (dv2).
- **Stage 29 run.** `analysis_ready/pipeline/29_canonical_behavior_releases/v102_dv2_a46be6a/`.
- **Bundle.** `ebb_v102_20261019_677ed97`; supersedes `ebb_v101_20260929_b2ce507` as the manuscript-facing Stage 29 release.
- **Preserved unchanged as provenance:** configs v1.0.0 and v1.0.1, bundles ebb_v100 and ebb_v101, every earlier Stage 29 run, and the v1 CombZ tables (archived with a receipt).

## Gates added by this release

- **Stage 29:** the CombZ table must hash to the bound outcome version before any row is read; the RES/SUS count gate reads the outcome version; Stage 09's 10-min ladder outcome must equal that CombZ (a release can no longer run on a stale Stage 09). CONTROL and DRY runs can bind `ov1_original` with its archived table (`MMM_STAGE29_OUTCOME_VERSION`, `MMM_STAGE29_OUTCOME_TABLE`). `run_manifest` records the outcome version and the table hash.
- **Stage 16b:** the run must be bound to `outcome_versions$release` and the canonical table must still hash to it; every RES/SUS label in the bundle (Stage 29 windows, Stage 09 predictions) must equal the CombZ table's `outcome_group` (before, OR419 and OR434 could have entered A2 as RES unnoticed); the Stage 09 5-min tables must have been run on the same CombZ.
- **Freeze script:** the release table, its lists and workbook, and the archived ov1 table must hash to their pins.

## Order (Phase B)

1. Producer v2 live (after archiving the v1 tables); config v1.0.2 values; freeze.
2. CONTROL rehearsal: v1.0.2 code, data version dv2, outcome version ov1 (archived table), before Stage 01 and Stage 09 are rerun. PASS: all 23 tables byte-identical to `v101_dv2_b2ce507`.
3. Stage 01 and Stage 09 (10 and 5 min) on the corrected outcome.
4. RELEASE run (ov2), then 16b.

## Verification record

The frozen configuration JSON SHA-256 is `90018f9ca242685e9e59a7bb95a91ae643b5481985698086139590d62e310a5c`. Analytic identity passed 12/12 checks with zero disallowed changes. CONTROL reproduced all 23 preceding tables byte-for-byte. RELEASE at `a46be6a288032d97392a5f16f8eb81d7d1bd2890` verified 444 windows: every non-outcome/non-group field matches CONTROL; only OR419/OR434 labels changed. All 233 models converged, with zero failed fits or block failures. The registered policy retained and flagged 53 singular fits (160 nonsingular; 20 not applicable).

Stage 16b passed release, code-currency, two-resolution outcome and canonical-group gates. The bundle manifest SHA-256 is `41b8ce8170de64b739b4c3517e7280d4c1282332d873b75794e674220c6b7ff9`. The primary uncentred LOAO R² is 0.21775876111961623, independently verified by PRESS and 111 explicit held-out OLS refits. The raw full-refit permutation P is 1/1001 and Holm P is 2/1001 across the two behaviour-only models.

The corrected full cohort is CON 24 / RES 54 / SUS 39. The RFID cohort is CON 24 / RES 51 / SUS 36. Animals with four, five and six valid components number 2, 10 and 105, respectively. Archival and producer replacement records are in `BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md`.

## Provenance notes (added 2026-10-20)

- **Integration.** `638c57d` copies the prepared branch `corrected-labels-v102` (`708e615`..`1bbb804`) instead of merging
  it, because an uncommitted peer edit blocked the merge; its Stage 32/33 files came later as cherry-picks
  (`7622260`..`013e934`). The frozen configuration and `Functions/figure_support_bundle.R` cite `708e615` for CombZ
  producer version 2. That commit and the rest of the prepared branch are kept by the annotated tag
  `archive/corrected-labels-v102`; the producer bytes are also blob `3303304e` at `638c57d`. The copied paths are listed
  in `docs/behavior_output_producer_reruns/corrected_outcome_20261019_integrated_paths.json`.
- **CONTROL evidence** (Order, step 2): `docs/behavior_output_producer_reruns/corrected_outcome_20261019_stage29_control_comparison.csv`
  (23 of 23 tables byte-identical to `v101_dv2_b2ce507`).
- **Run commit `a46be6a`** also carries a broken glyph in `Functions/hmm_stage14_helpers.R` (`dhm_minus`: U+2212 written
  as CP1252 mojibake). `677ed97` repaired it before Stage 14 ran at `5473f94`. The file is not in the Stage 29 code set.
- **Protection snapshot.** The "before" check `vpo_t1_20261019_1624_b494bbe` was taken after `b494bbe` had moved the
  configuration byte pin to the unfrozen draft (`2d46ed44`); `a46be6a` set the frozen `491d4146` four minutes later.
- **29b/29c registries v1.1** kept placeholder wording for the bundle ("filled in at the freeze"). Both ran on
  `ebb_v102_20261019_677ed97` (`PHC_STAGE29`, `PMF_STAGE29` and the run manifests). The frozen texts stay as they are.
- **`RETRACK-PIN` tags** left in frozen files (`Functions/behavior_analysis_config.R`, `stage30_*.R`, `stage32_run.R`,
  `posthoc_*.R`) are inert comments; removing them would change pinned bytes or blobs.
