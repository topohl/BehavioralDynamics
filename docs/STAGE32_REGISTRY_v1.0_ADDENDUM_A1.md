# Stage 32 registry v1.0, addendum A1: the aborted first execution, the corrected v1.1 execution, and binding reporting rules

**Status.** FROZEN 2026-10-01T00:12:25+0200, before the corrected (v1.1) execution and before any code implementing section B was committed. Committed alone in MMMSociability, hashed, and copied read-only to `<AR>/canonical/stage32_registry/v1.0/` with `ADDENDUM_A1_SHA256.txt`. The runner refuses to fit unless the frozen sha256 matches.

**What this addendum is.** A correction record for registry v1.0 (`STAGE32_REGISTRY_v1.0.md`, sha256 cfbd954cde6056c275ba28a9eddcae31b38eff07cec061b8f6a74a703791ee5a, frozen 2026-09-30T22:41:10+0200 at commit 0970d32).
- It changes **no analysis**: no module, model, formula, random structure, window, phase set, coverage rule, population, estimand, family, m, seed, package, gate threshold or test is added, removed or altered. Registry v1.0 stays the analysis registry.
- It (A) records that the one registered REAL execution aborted inside the writer; (B) registers the corrected write step and a second execution into a new run folder, guarded by a determinism gate; (C) adds reporting rules that extend registry section 9. They only restrict how results may be described.
- Tier and decision basis are unchanged: POST HOC relative to the original experiment; POST_HOC_CONTEXT.

## A. The aborted first execution

- **What ran.** Commit 4f9c0169b975d9d1efce49791ff9815b2c6d93a7, `MMM_STAGE32_REAL_RUN=4f9c016 Rscript Analysis/32_behavior_exposure_adaptation.R --real`, 2026-09-30 23:21–23:23.
- **What passed.** Every gate before fitting. All 65 models fitted (0 FAILED, 0 KR→Satterthwaite fallbacks, 20 singular). Every post-fit gate: A-exp = EXPOSURE_CC1 and B-exp = EXPOSURE_TR within 1e-6; Module F continuity within 1e-9; H01–H13 present; no forbidden word.
- **Where it stopped.** In the writer: `Round-trip text differs in gate_results.csv column detail`.
  - The strict-variant coverage gate detail was built by pasting an empty vector and ended with a trailing space: `0 removed; `.
  - `data.table::fread` strips surrounding whitespace, so the frozen round-trip check in `s30fb_write_csv` (`Functions/stage30_figure_bundle.R`) refused the file.
  - It is the only whitespace-edge cell in the written files; none of the 12 tables holds one.
- **What it left on S:.** `pipeline/32_behavior_exposure_adaptation/.tmp_v1.0_4f9c016/` with 14 files: the 12 tables, `audit/input_hashes.csv` and `audit/gate_results.csv`. There is no run manifest, reproduction / reference / coverage audit file, README, output manifest or QC PNG. The folder was never renamed.
- **Seen.** Its tables were read (implementer report; an independent review recomputed parts of them) before this addendum. The Stage 32 results are therefore seen.

**Pinned files of the aborted staging folder (sha256):**

| File | sha256 |
|---|---|
| tables/coverage_manifest.csv | e5352ac5add6a7dc02521952e77c4ce9ed921dbc537026c592bbd363d955e493 |
| tables/window_metrics_long.csv | 7f2bfdcc25a9331dc68bc93eddfc03599908208723b94b16c8e753d03c5b2f93 |
| tables/descriptives.csv | cdb27f2ddc40e987ecac9d8bdb42044275c0146bfaf85e6a65d46a1fa92c1dbc |
| tables/models.csv | b2b1f608cf5bc7bd2dace7e020e7c23c3732d9474fda76f57f2958e5a2adee67 |
| tables/estimates.csv | 76f262ffad6b8f7a184fccc0cbe4852f3bfe59549cf2ad8745a0691ea6dcfc40 |
| tables/joint_tests.csv | 5fd5d9460b66e945189d1e3dc008398bc463be72c5b4499774941056f3fa2411 |
| tables/multiplicity.csv | 3544bcd052e66cef7535eff3ed21d3f7e37e35fe94fd3168bda7c322e0e9b81b |
| tables/sensitivities.csv | c013bf899336666a615ba0a7e7fba7f024d21440eae0ebbeea4ec920fdfcd636 |
| tables/diagnostics.csv | d39ef652675a86d4094ee2593635e059fa0acf8ae1ecd533abff7b98166e8fe4 |
| tables/prediction_performance.csv | c8d627a0bd1fb5688f1ee865f91bfbde6290d4eb871aa7111248fd8d8ffc2846 |
| tables/prediction_permutations.csv | 5c4e36d66b942f812f90adc353b38ada9d0ef79f6cd9e6c66176e04fb9fec8be |
| tables/prediction_heldout.csv | c384ca1188f0412bd2780b09dce8e85df5f32155170c42f026066aad0e5fb664 |
| audit/input_hashes.csv | 46ae956610216aa17e28730790c7df85a2ce0781d59bdc643969e285ee58301d |
| audit/gate_results.csv | 86dd53bf3cdfb44b42a0057fb91ebec2e98eb723f745b42471bd2e1bbc5419dd |

**Disposition.**
- The folder stays where it is, with its content unmodified; its files are set read-only. It is evidence, not a registered output, and must never be cited as one.
- Its name matches the v1.0 run-once pattern, so no v1.0 run folder can ever be written.

## B. The correction and the v1.1 execution

**B1. Code.** One commit after this addendum, touching only Stage 32 files (`Functions/stage32_run.R`, the new `Functions/stage32_reporting.R`, `Analysis/32_behavior_exposure_adaptation.R`, `Testing/tests/test_stage32_run.R`, the new `Testing/tests/test_stage32_reporting.R`):
- The strict-variant gate detail is built without a trailing separator.
- `s32r_prepare` trims leading and trailing whitespace from character cells before the frozen writer. This is a no-op for the 12 tables (0 such cells).
- Every output file is written and round-trip-checked in a **local** staging folder first. Nothing is copied to S: until every local check has passed. The S: copy goes to `.tmp_v1.1_<commit7>`, is byte-verified against the output manifest, renamed, set read-only and verified again. A failure during the copy leaves `.tmp_v1.1_<commit7>`, which blocks any further run.
- The QC PNGs are produced and copied in the same step. Single-phase panels (CC4 light) are drawn as points without a line.
- README, run manifest and three new audit files (B4) carry the disclosures of this addendum. The wording of convention C13 is corrected and convention C17 added (C6 below).
- **Numeric code unchanged.** `Functions/stage32_windows.R`, `Functions/stage32_inference.R` and `Functions/stage32_prediction.R` keep the git blobs of 4f9c016 (runner gate). The runner code that fits the models and assembles the 12 tables is unchanged; only its gates, run manifest, README and write step change.

**B2. Output and run-once.**
- The corrected execution writes `pipeline/32_behavior_exposure_adaptation/v1.1_<commit7>/` once; all files read-only.
- It refuses to start if any `v1.1_*` or `.tmp_v1.1_*` entry exists, if the stage folder holds anything other than `.tmp_v1.0_4f9c016`, or if that folder's files differ from the pinned list above, include any other file, or are writable.
- All registry v1.0 gates apply unchanged, plus: this addendum's sha256 (S: copy, repository copy in LF form, sha list, status FROZEN, read-only), its freeze commit follows 4f9c016 and is an ancestor of HEAD, and the numeric-code identity of B1.

**B3. Determinism gate.** Before anything is copied to S:, the 12 tables of the v1.1 execution must be byte-identical (sha256) to the 12 pinned tables of the aborted folder.
- None of the 12 tables embeds the commit, a path or a time.
- Identity therefore shows that the second execution re-writes the numbers already produced and seen, with no analytic choice exercised.
- If any table differs, nothing is written to S: and the run stops for a user decision.

**B4. Audit additions.**
- `audit/aborted_run_evidence.csv`: the 14 pinned files (bytes, sha256 found and pinned, read-only status).
- `audit/determinism_gate.csv`: the 12 tables, sha256 of this execution against the aborted staging table.
- `audit/reporting_flags.csv`: the section C rules, one row per flagged estimate, with comparators.
- `audit/input_hashes.csv` also lists this addendum, its sha list, the 14 aborted files, and the frozen Stage 29 v1.0.1 `continuous_estimates.csv` (sha256 cc549c248778a735f1e79ba2b33e59443778ef200e480ad7479b4150f2b15200, read-only; used only as the H13 comparator in C2).
- README and `audit/run_manifest.csv` state that the first execution aborted, that v1.1 is the second execution of the registered fits, why, and that its tables are byte-identical to the aborted ones.

## C. Binding reporting rules (extend registry section 9; they only restrict)

**C1. The strict completeness sensitivity is vacuous.**
- Under the registered board observation interval (raw_data records), boards run past every clean phase end. Minimum margins: A4 4.78 h (CC1), 3.49 h (CC2), 3.94 h (CC3); CC4 A2 7.51 h (B5 sys.1).
- Neither the 10-min tolerance nor the 0-s strict rule binds in the clean set. Strict removes 0 animal-phases, and every SENS_C_STRICT_* row equals the primary (shift 0).
- Registry section 2's sentence "This removes every A4 whose recording ends before 06:30" describes the preprocessed-file ends: all 333 used A4 rows at CC1–CC3 end 0.2–66.7 s before 06:30 (status file_ends_before_window_end). It does not describe the registered raw-record interval. This is a registry inconsistency.
- The rule is not changed: that would be a deviation. Under the file-end reading the primary would be unchanged (all within 10 min) and strict would remove all 333 A4 rows at CC1–CC3.
- The strict rows must never be described as a robustness check that passed.

**C2. H13 is a post hoc restatement, not new information.**
- The Stage 29 v1.0.1 CONTINUOUS SIS_ONLY crossing_rate `slope_DiD_F_minus_M` (`continuous_estimates.csv`: −0.0511468, se_cr2 0.0190519, df_cr2 9.386) was stored and seen before registration.
- On the Movement_mean scale (×6; Movement_mean ≈ crossing_rate / 6, max |diff| 0.012, r 0.999997 over the 111 animals) it is −0.3069 (CR2 SE 0.1143, t −2.685, p 0.024). This is numerically near-identical to H13.
- The audit (evidence r01) also quoted the SIS female and male slopes (F −0.031 [−0.061, −0.002]; M +0.020 [−0.026, 0.065]) before the freeze. The registry's tier list named only the Stage 29 SIS−CON estimates as seen.
- Sex is nested in batch (3 female, 3 male batches; 24 CC1 cages; CR2 df about 9.4). H13 is batch-confounded and unadjusted.
- H13 is reported only as a restatement of the already-seen Stage 29 value. It is never a discovery claim and never a new finding.

**C3. CON-referenced intervals from the multi-phase models.**
- Every CON-referenced row of C-exp, the C-exp secondaries, D-exp, E-exp and the C-exp sensitivities has a KR df that is not bounded by the 6 intact CON groups. The 24 CON cage episodes (6 groups × 4 CCs) enter as separate clusters, and in C-exp one cage phase-slope variance is shared by CON and SIS.
- Such an interval is optimistic. It is flagged (singular fits also as singular) and shown next to its comparator where one exists:

| Row | Value | Comparator |
|---|---|---|
| E-exp SIS−CON at L1 of CC1 | 0.919 [0.196, 1.641], KR df 146, singular (conCage variance 0.0066 against sisCage 0.33) | frozen Stage 29 EXPOSURE_CC1 light-phase rate: 0.878 [−0.363, 2.119], KR df 4.0 |
| C-exp SIS−CON at A1 of CC1, rate / overlap | KR df 50 / 37 | A-exp H01 / H02 (KR df about 4) |
| H07 / H08 (C-exp g_SIS:phase) | KR df 108 | separate CON / SIS cage phase-slope variances (section 7): rate 0.084 [−1.04, 1.21], df 27.8; overlap df 41 |
| SENS_C_PHASEF_EXP overlap, A1→A2 and A2→A4 | df 1021 and 224, singular | none; the categorical-phase random part keeps only the linear cage phase slope (C4) |

- No phase-specific or light-phase SIS−CON interval is evidence of an exposure effect: H05–H08 are null.

**C4. Categorical-phase models (convention C7).** D and the phase_f sensitivities keep only the numeric-phase random terms. Non-linear cage-level deviations therefore enter the residual, and the df of phase-specific contrasts is inflated. Flagged.

**C5. Sex terms.**
- Every sex term compares 3 female with 3 male batches, so it is batch-confounded and estimation only. This includes the A-cz CombZ_wb:sex_c rows, whose KR df count animals and cages, not batches.
- The C secondary `g_SIS:phase:sex_c` is fitted without `phase:sex_c` (registry section 4 wording, convention C8). The CON and SIS sex differences in phase slope are therefore forced to be equal and opposite (g_SIS is ±½; SIS outnumbers CON within each sex, 46 / 41 against 12 in females / males), so the term is not a clean Exposure × phase × Sex contrast. It is labelled and not interpreted.
- `CombZ_wb:phase:sex_c` forces the sex difference in the phase slope at CombZ_wb = 0 to be 0. It is less affected, because CombZ_wb has mean 0 within each batch and hence within each sex.
- A future registry version should add `phase:sex_c`.

**C6. Labels and wording.** No number changes.
- `shared_phase_slope` (the C-exp phase coefficient) is the **CON/SIS-average phase slope**: under ±½ coding it is the unweighted average of the CON and SIS slopes, not a slope shared by CON and SIS. The table label is kept; a display label is given in `reporting_flags.csv` and in the README (convention C17).
- `SIS_phase_slope_at_CombZ_wb_0` has no CON reference. Neither phase slope, nor any phase pattern without a supporting Exposure interaction, is adaptation to social instability (section 9).
- Convention C13 is corrected. A batch-specific shift of CombZ_wb is absorbed by Batch only when the CombZ_wb terms are CombZ_wb and CombZ_wb:sex_c. For CombZ_wb:phase and CombZ_wb:CC it would create Batch × phase or Batch × CC terms the models do not contain. There is no effect here, because every C-cz, D-cz and E-cz model uses all 87 SIS animals, which is the centring set.
- The hypothesis table header reads `Estimand_test` where the registry table reads "Estimand / test". This is cosmetic: all 13 × 10 cells are identical, and the frozen CSV is not changed.

**C7. Machine-readable flags.** `audit/reporting_flags.csv` applies C1–C6 row by row. Codes:

| Code | Rule |
|---|---|
| RF1_CON_DF | C3 |
| RF2_SINGULAR | singular fit, among flagged rows |
| RF3_SEX_BATCH | C5, sex terms |
| RF4_C8_EXP / RF4_C8_CZ | C5, the C8 constraint |
| RF5_H13 | C2 |
| RF6_LABEL_AVG / RF6_LABEL_SIS | C6 |
| RF7_STRICT | C1 |
| RF8_C7 | C4 |

Comparators are those of C3, plus the Stage 29 value of C2 (×6, CR2 SE and df as stored). Any text, figure or bundle built from Stage 32 carries these flags.
