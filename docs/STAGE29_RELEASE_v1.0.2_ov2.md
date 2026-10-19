# Stage 29 corrected-outcome release: config v1.0.2, outcome version ov2

Status: prepared on branch `corrected-labels-v102`; the release runs once, after the 00317 re-track is in the workbook (user decision 2026-10-17). Values marked "fill in" come from the real run.

## What this release is

The frozen Stage 29 characterisation rerun on the corrected outcome (erratum E13). Nothing analytic changes.

- **Configuration.** v1.0.2 is an outcome-version revision of the frozen v1.0.0 (analytic parent, JSON SHA-256 `33d22430...`, commit `a9b7a2c`), like v1.0.1 was a data-version revision. The identity gate passes with 0 disallowed differences and 12 of 12 semantic checks, three of them new: a complete `outcome_versions` binding, a parent outcome version with the frozen RES/SUS counts, and every version keeping the frozen SIS total (87) and sex x group structure.
- **Outcome versions.**

  | Version | Tag | CombZ table | Lists | RFID SIS counts F RES/SUS, M RES/SUS |
  |---|---|---|---|---|
  | `ov1_original` | ov1 | `1f6a2a69...` (producer v1; archived to `analysis_ready/history/retired/later_outcome_combz_v1_1f6a2a69/`) | sus `dea3804b...` (35), con `eddd2ee9...` | 28/18, 25/16 |
  | `ov2_corrected_workbook` | ov2 | fill in (producer v2, sheet `combz_canonical`) | sus fill in (39 before the re-track), con `eddd2ee9...` | 26/20, 25/16 before the re-track (fill in) |

- **Data version.** Unchanged: `v2_cage_label_correction_2026-09-28` (dv2).
- **Stage 29 run.** `analysis_ready/pipeline/29_canonical_behavior_releases/v102_dv2_<commit7>/` (fill in).
- **Bundle.** `ebb_v102_<date>_<commit7>` (fill in); supersedes `ebb_v101_20260929_b2ce507` as the manuscript-facing Stage 29 release.
- **Preserved unchanged as provenance:** configs v1.0.0 and v1.0.1, bundles ebb_v100 and ebb_v101, every earlier Stage 29 run, and the v1 CombZ tables (archived with a receipt).

## Gates added by this release

- **Stage 29:** the CombZ table must hash to the bound outcome version before any row is read; the RES/SUS count gate reads the outcome version; Stage 09's 10-min ladder outcome must equal that CombZ (a release can no longer run on a stale Stage 09). CONTROL and DRY runs can bind `ov1_original` with its archived table (`MMM_STAGE29_OUTCOME_VERSION`, `MMM_STAGE29_OUTCOME_TABLE`). `run_manifest` records the outcome version and the table hash.
- **Stage 16b:** the run must be bound to `outcome_versions$release` and the canonical table must still hash to it; every RES/SUS label in the bundle (Stage 29 windows, Stage 09 predictions) must equal the CombZ table's `outcome_group` (before, OR419 and OR434 could have entered A2 as RES unnoticed); the Stage 09 5-min tables must have been run on the same CombZ.
- **Freeze script:** the release table, its lists and workbook, and the archived ov1 table must hash to their pins.

## Order (Phase B)

1. Producer v2 live (after archiving the v1 tables); config v1.0.2 values; freeze.
2. CONTROL rehearsal: v1.0.2 code, data version dv2, outcome version ov1 (archived table), before Stage 01 and Stage 09 are rerun. Expected: the 23 tables byte-identical to `v101_dv2_b2ce507` (fill in).
3. Stage 01 and Stage 09 (10 and 5 min) on the corrected outcome.
4. RELEASE run (ov2), then 16b.

## Verification record

Fill in from the real release: commit; freeze record `analytic_identity.csv` sha; CONTROL table hashes; RELEASE tables that differ from v101 and why; 16b gates.
