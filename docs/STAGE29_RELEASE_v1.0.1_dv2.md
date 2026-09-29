# Stage 29 corrected-metadata release: config v1.0.1, data version v2

## What this release is

This release reruns the frozen Stage 29 characterisation on the corrected cage labels (data version v2). Nothing analytic changes.

- **Configuration.** v1.0.1 is a documentation and data-version revision of the frozen v1.0.0.
  - v1.0.0: JSON SHA-256 `33d22430b0e6d3a45a28bea8546a185c523aedf72055ddf5ae208cff51f8950e`, commit `a9b7a2c`.
  - v1.0.1: JSON SHA-256 `236a1b0827ee5bee9f5a42691f976c8279479e93b67ac9208a590e884ce8eaf7`, R file `f34c7624...`, commit `b2ce507`.
  - Frozen record: `analysis_ready/canonical/behavior_config/v1.0.1/`, with `analytic_identity.csv` (sha `c20e99cf...`) and `analytic_identity_checks.csv`.
  - `meta$change_log`: "v1.0.1: documentation only + data-version binding; no analytic change; changed after outcome inspection (errata E1-E10)".
- **Data version.** `data_versions$release` = `v2_cage_label_correction_2026-09-28`.
  - Manifest `MANIFEST_SHA256.csv` sha `bb33a111980913f4ddf97c40872ccdd96dd73accdbdc5c5867954b51aad2c571`.
  - `cage_label_corrections.csv` sha `27512ab9...`; `README.txt` sha `30fe05f0...`.
  - The four corrections change only the System field:

    | File | Animal | Label change | Rows |
    |---|---|---|---|
    | B1 CC2 | OQ764 | sys.5 -> sys.2 | 3,961 rows in total for the three B1 CC2 animals |
    | B1 CC2 | OQ770 | sys.2 -> sys.5 | |
    | B1 CC2 | OQ772 | sys.3 -> sys.4 | |
    | B6 CC4 | OR646 | sys.2 -> sys.5 | 1,982 |

  - 22 of the 24 files are byte-identical to the original.
- **Stage 29 run.** `analysis_ready/pipeline/29_canonical_behavior_releases/v101_dv2_b2ce507/`.
  - Written once and read-only.
  - Run mode RELEASE, 233 models, 0 FAILED, 0 block failures.
- **Bundle.** `analysis_ready/canonical/behavior_bundle/ebb_v101_20260929_b2ce507/`.
  - Status FROZEN; manifest sha `ec59aa337fc63511de4aa2bf3e1ee1f4b34b84d6d671565f74e0bb3939f7226c`.
  - It supersedes `ebb_v100_20260927_95e5dc8` as the manuscript-facing Stage 29 release (user decision 2026-09-29, round 5, item 29).
- **Preserved unchanged as provenance:**
  - config v1.0.0;
  - bundle `ebb_v100_20260927_95e5dc8`;
  - `pipeline/29_canonical_behavior/`, which holds the f3a25da re-run and is never written again;
  - the 0555c90 run behind ebb_v100, archived read-only at `pipeline/29_canonical_behavior_releases/v100_dv1_0555c90/` together with `ARCHIVE_RECEIPT.csv`.

## Why this is not an analytic change

1. **Configuration identity.** `Functions/behavior_config_identity.R` compares v1.0.1 with the frozen v1.0.0 JSON leaf by leaf.
   - The parent has 675 leaves and v1.0.1 has 798. 167 leaves differ, and none is disallowed:

     | Kind of difference | Leaves |
     |---|---|
     | Added under `data_versions`, `meta` and display nodes | 124 |
     | Display texts (labels, units, interpretations, caveats) | 11 |
     | Spec-bearing documentation texts that keep every number, identifier and code token | 28 |
     | Meta fields | 3 |
     | `change_log` (was empty) | 1 |

   - The gate also passes 9 of 9 semantic checks, including that the data version `v1_original` reproduces the v1.0.0 expected counts and complete-case set exactly.
   - No numeric or logical leaf changed. The only exception is `frozen_before_res_sus_outcome_models`, which is now FALSE.
   - The test `Testing/tests/test_behavior_config_identity.R` rejects 26 kinds of analytic edit (12 were required). Among them:
     - formula, rank, standardizer, bout criterion, family m and members, contrast L, window, population, RNG, S11 and new node;
     - HC3 -> HC1 inside a text leaf, and a term dropped from a formula inside a text leaf;
     - a frozen count, leaf removal, a type change, a term array, a test text and a new sensitivity;
     - the parent binding and complete-case set, an undeclared release, the change_log label, a pre-outcome claim and the parent sha.
2. **Code identity (control run).** The release code with config v1.0.1 on data version v1 reproduces the frozen tables.
   - It gives 23 of 23 tables byte-identical to `pipeline/29_canonical_behavior/tables` (f3a25da, config v1.0.0).
   - Evidence: rehearsal E3, `logs/e3_06_table_hashes.csv`.
3. **Release identity.** The release run on data version v2 gives 23 of 23 tables byte-identical to the sandboxed cage-label re-run `stage30_sleep_cookie/evidence/cage_fix_rerun/out/tables`. That re-run used code 8497516 and config v1.0.0, with only the documented input and gate patches.

## What changes with the corrected labels (data version v1 -> v2)

**Tables.** 15 of 23 Stage 29 tables differ and 8 are byte-identical. The 8 identical ones are `continuous_estimates`, `cumulative_window_estimates`, `lag_block_estimates`, both Stage 09 tables and the three validation tables. The earlier statement "17 tables differ" was a misreading; see erratum E12.

**Results.** These come from `cage_fix_rerun/compare_cagefix` and apply unchanged because the tables are byte-identical. They were not re-derived for this record.
- Every CC1 result is identical.
- Longitudinal estimates move by at most 0.47 SE.
- The shared RFID-position occupancy Q2b (P-TR) p changes from about 0.826 to 0.755.
- No inferential or multiplicity conclusion changes: 24 of 24 family members are non-rejecting.
- LOBO: shared-position Q2b component c2 becomes robust to single-batch removal (5/6 -> 6/6).

**Structure** (`wfD/W5a_data_terminology_release/w5a_06_*`):
- 18 shared-position models gain 1 row and 1 cage epoch (TR 345 -> 346 rows, 93 -> 94 cage epochs).
- Complete case: 336 -> 340 windows (CC1 84 -> 85 animals; missing set {OQ755, OQ770, OQ771} -> {OQ770, OQ771}).
- D2 TR: 481 -> 480 dyads, 93 -> 94 cage epochs, rank 101 -> 102.
- The singular flag flips TRUE -> FALSE for TR_POOLED, S2, S3, S12 and EXPOSURE_TR (shared position).
- 0 fit failures.

**Protocol deviation (disclosed, no exclusion).** OR646 (B6 CC4) was re-housed with former cage-mates 00690 (CC1) and 00694 (CC3), and B6 CC4 sys.2 ran with 3 animals.

## Bundle difference ebb_v100 -> ebb_v101, in four steps

The steps were rehearsed in E3 (`logs/e3_08_*`), and every step matched its prediction. Outcome cells were compared only as counts.

| Step | Change | Files differing (of 24) | Content |
|---|---|---|---|
| 1 | 0555c90 -> f3a25da code (E7) | 5 | `C0_models` gains `error`; `R_robustness` gains `n_failed`, and 2 `p_raw` are withheld (D2 joint, D3); plus H, H2 and the manifest |
| 2 | release code on data version v1 | 3 | only H, H2 and the manifest; H2 gains 25 Stage 29 input rows (24 raw_data seed files and the data-version manifest); the Stage 29 output hashes are equal |
| 3 | data version v2 (E9) | 12 | see below |
| 4 | E8 / shared-position terminology, config v1.0.1, release provenance | 8 | see below |

Step 3 changes these files:
- `B1_animal_longitudinal`: CageEpisodeID, n_in_cage, n_tracked_mates, shared_zone_use.
- `B2_descriptive_summaries`, `C0_models`, `C2_estimates`, `C3_joint_tests`, `E_multiplicity`, `G_sample_sizes` (one `n_missing_shared_zone` cell), `P_primary_results` and `R_robustness`.
- H, H2 (+2 supporting data-version files) and the manifest.
- `A1_animal_cc1` is identical.

Step 4 changes these files:
- `A0_design_timeline` detail becomes "bin-free RFID position-change rate and shared RFID-position occupancy".
- The `C2_estimates` unit changes in 204 rows: "crossings/hour" -> "position changes/hour" and "fraction of dyadic observation time" -> "fraction of co-assigned dyadic time".
- `F_resolution_decisions` definition changes in 3 rows.
- `P_primary_results` `config_version` becomes 1.0.1.
- `I_analysis_config.json` and `I_config_sha256.txt` are the v1.0.1 versions.
- `H_provenance` grows from 15 to 32 keys: data_version, manifest sha, analytic parent, identity result, supersedes_bundle, errata doc and release record. Its `stage29_started_at` is now an ISO timestamp (erratum E11).
- The manifest changes. H2 is identical.

## Terminology (E8 and the shared-position rename)

- **RFID position-change rate** (legacy identifier `crossing_rate`), unit "position changes/hour".
  - First use: "vendor-defined RFID position-change rate (a position change is registered only when the tag's estimated position moves at least 200 grid units, about two antenna spacings)".
- **Shared RFID-position occupancy** (legacy identifier `shared_zone_use`), unit "fraction of co-assigned dyadic time (0-1)".
  - The code counts time during which two cage-mates are simultaneously assigned the same carried-forward vendor PositionID (overlap of equal-position runs; `rfid_binfree_metrics.R:57-79`). That is simultaneous occupancy of the same vendor-defined position, so "occupancy" is the exact term.
  - It is not antenna identity, co-detection, proximity, contact or huddling.
  - "Antenna zone" is never displayed.

## Gates added by this release

- **Stage 29:**
  - Data version from the config, with a manifest-hash, file-set and per-file gate before any row is read.
  - Every data-version count read from the config, with hard stops before any model: expected counts, 8 re-seeded windows, 444 windows, 24 anchors, the complete-case set and the D2 dyad and cage-epoch counts.
  - Identity gate against the frozen parent in every mode.
  - A release folder named `v<ver>_<tag>_<commit7>`, written via a `.tmp_` staging folder and refused if it exists. It is not finalised if any block fails, and its files are made read-only.
  - RELEASE, CONTROL and DRY modes.
  - `run_manifest` records the data version, the manifest sha, the analytic parent and the identity result.
  - `run_inputs` records the 24 raw_data seed files and the data-version files.
- **Stage 16b:**
  - Reads the single release run of `data_versions$release`.
  - Refuses anything other than a RELEASE run, a run with block failures, a run bound to another data version, a run whose preprocessed inputs differ from the manifest, a run that did not pass the identity gate, and a config that fails the identity gate.
  - DRY bundles must be written outside `analysis_ready`.
  - The `BUNDLE_REGISTRY` schema is unchanged.
- **Freeze script:** refuses to freeze unless the identity gate passes and every declared data version verifies. It writes `analytic_identity.csv` into the frozen record.

## Verification record

Fill in from the real release:
- commit `b2ce507`;
- freeze record `analytic_identity.csv` sha equal to the rehearsal;
- `e3_12_verify_real_release.R all`: every check TRUE;
- manuscript import of `ebb_v101_20260929_b2ce507` (separate step).
