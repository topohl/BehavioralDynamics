# Spatial measurement specification v1.5: closeout (2026-10-15)

*Written for: Tobias and reviewers. Status: **current**. This version closes the RFID measurement-validity work on the
existing evidence. It supersedes the status statements of `docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.4.md`; the
definitions of v1.4 (sections 1-6, the constructs S1-S7 and S9 of section 9, and section 10) remain the definitions except
where this version amends them. Earlier versions (v1 to v1.4) are kept as the record of their rounds.*

## 0. Decision (Tobias, 2026-10-15, sixth round)

- **Calibration will not be performed.** No calibration experiment is booked, prepared, scheduled or required, and none is
  pending. Calibration is not a prerequisite for the current pipeline status or for any statement in this document. The
  measurement uncertainties it was designed to address remain **accepted limitations** (section 5).
- The calibration material is kept as historical record and provenance. The three calibration documents (the protocol
  `docs/RFID_CALIBRATION_PROTOCOL.md`, the pilot `docs/RFID_CALIBRATION_PILOT.md` and the logistics
  `docs/RFID_CALIBRATION_LOGISTICS.md`) carry a cancellation banner added at this closeout; their historical bodies below the
  banner are unchanged. The pinned files of `docs/rfid_calibration/` (schedules v1-v3, criteria v1.0-v1.2, report mappings
  v1.0-v1.1 and the schedule generators) and the hash pins in the code are unchanged. The analysis and report code
  (`Functions/antenna_calibration_pilot.R`, `Functions/antenna_calibration_report.R`, their runners) is unexecuted tooling;
  no runner profile calls it, and its portable tests remain regression checks only.
- Stage 19 is closed conservatively (section 3). The Stage 31b-31d QC comparison stays accepted, with no further model rerun
  (section 4).

## 1. What is integrated

"Automatic" means: after an entry point has been deliberately invoked. `Analysis/run_all_analysis.R` runs nothing without a
profile or an explicit stage list.

| component | current behaviour | evidence |
|---|---|---|
| Stage 00b acquisition QC | **Operational, manual.** Writes a new run into the protected, append-only live QC root `analysis_ready/quality_control/raw_acquisition/` (ledger and output protection; committed code required) or into an isolated `--output-root`. Its flags and proposed exclusions are review items: **no downstream stage reads them, and a FAIL does not stop or filter any downstream analysis.** It is not part of `run_all_analysis.R`. | ledger: `raqc_20261014_6165adf` (FAIL, first live run) and `raqc_20261014_25804cb` (FAIL confined to 3 of 120 board-recordings: B2\|sys.2\|CC3, B2\|sys.5\|CC4, B5\|sys.1\|CC4; 24 WARN, 93 PASS); rules `aqc_rules_v0.2_provisional`; `docs/RFID_ACQUISITION_QC.md` |
| Stage 08 HMM states | **Overwrite protection.** The stage calls the frozen-input guard itself (`mmm_frozen_guard_before("08")` / `mmm_frozen_guard_after()`), so direct and runner invocations are guarded; the 72 live outputs are pinned. An explicit, backed-up override (`mmm.allow_pinned_overwrite`) exists and was not used. | `Analysis/08_hmm_behavioral_states_optional.R`; `docs/frozen_output_pins/stage08_hmm_states_20260922.csv` (72 rows) |
| Stage 09 prediction | **Canonical vendor-based analysis retained.** Raw-input comparisons (RANT flip2s, RANU noflip, RANV ge200, RANA avg500) are separate sensitivities in their own roots; no method is chosen by their R². | `Analysis/run_raw_antenna_parallel.R` (header); `Testing/audits/audit_stage09_section7_by_input.R`; `docs/PHENOSYS_ANIMALPOS_RECONSTRUCTION.md` (avg500) |
| Stage 19 live | **Earlier raw event-start analysis retained** (promoted 2026-10-12: event-start occupancy, 2-s flip filter, reader-level maps, per-sex exploratory models). It reads no spatial exclusion manifest and no unknown-time candidate. | `Analysis/19_spatial_occupancy_maps.R`; `docs/behavior_output_producer_reruns/stage19_20261012.csv`, `stage19_20261012b.csv` |
| Stage 19 candidate | **`s19c_v0.2_defd263` built; unregistered and inactive.** Not read by the live stage, any model or the manuscript. | `<Behavior>/RANM/spatial_occupancy_candidate/runs/s19c_v0.2_defd263` (`manifest/run.csv`: version s19c_v0.2, code_clean TRUE, dev_run FALSE, block_sets `stage19_window cc4_grid_period`; `tables/gates.csv`: 25 of 25 gates pass, 2 diagnostics); `docs/STAGE19_CANDIDATE.md` |
| Stage 29 and S13 | **Frozen.** The registered v1.0.0 and v1.0.1 releases (`v100_dv1_0555c90`, `v101_dv2_b2ce507`), the model specification and S13 are unchanged; no registered stage reads the spatial manifest. The raw-input Stage 29 sensitivity is a separate opt-in wrapper whose outputs are labelled UNREGISTERED. | `Testing/tests/test_spatial_exclusion_manifest.R`; `Analysis/raw_antenna_stage29_sensitivity.R` |
| Stage 31 censoring | **Opt-in.** `MMM_GRID31_CENSOR_MANIFEST` (a released manifest version) together with `MMM_GRID31_OUTPUT_ROOT` (an isolated root); without them Stage 31 behaves historically. | isolated run `<Behavior>/RANM/cc4_grid_exposure_censored/whole_inactive_sx04v11_20261014`; `docs/CC4_GRID_EXPOSURE.md` |
| Stage 31b-31d outcome eligibility | **Opt-in.** `MMM_CC4_OUTCOME_ELIGIBILITY` names a verified eligibility file (rule `cc4e_v1.0`); without it the stages behave historically (legacy byte parity tested). The isolated QC reruns exist, and the accepted comparison needs no further model rerun (section 4). | `docs/CC4_OUTCOME_ELIGIBILITY.md`; `docs/CC4_QC_RERUN_COMPARISON.md` |
| Manuscript imports | **Historical.** The frozen bundles and imports are unchanged; they have not been replaced by any candidate or QC rerun. No source-data or provenance file of Exp9_manuscript names `s19c_`, `srs_v1`, `cc4e_v1`, `p31*_cc4e*`, `cqc_v1` or RANM. | read-only search of `Exp9_manuscript/source_data` and `provenance` (2026-10-15) |
| Board and presence audits | Supporting products; no automatic covariate or board adjustment in registered models. | `docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.4.md` section 6 |
| Calibration tooling | Historical, unexecuted (section 0). | `docs/rfid_calibration/` |

**Completed work:** Stage 00b live runs (two); the Stage 08 guard; the Stage 09 raw-input comparisons; the srs_v1.1
reconstruction release; the Stage 19 candidates s19c_v0.1 and s19c_v0.2; the CC4 reader knockout and the cc4e_v1.0
eligibility audits; the Stage 31 censored run; the six Stage 31b-31d QC reruns and their accepted comparison; the round-level
protection checks.

**Inactive candidates:** `s19c_v0.2_defd263` (and the superseded `s19c_v0.1_4aebc90`); the single-read reconstruction
release `srs_v1.1_9b18624`, the candidate's input, which no registered analysis reads.

**Manual operational QC (not part of `run_all_analysis.R`):** Stage 00b. It writes either a new run into its protected,
append-only live QC root (committed code and a fresh run id required) or a run into an isolated `--output-root`.

**Optional execution paths (opt-in; isolated or separate output roots only):** Stage 31 censoring (requires
`MMM_GRID31_OUTPUT_ROOT` through the command-line block); Stage 31b-31d outcome eligibility; the raw-input parallel pipeline
(RANT, RANU, RANV, RANA) and the raw-input Stage 29 sensitivity (outputs labelled UNREGISTERED).

**Possible future changes (each needs a separate instruction):** activating, registering or importing any candidate into
Stage 19, a model or the manuscript; a group-descriptive layer on the candidate; hardening the direct entry points (section
6); a Stage 33 input-pin update after verification (section 6).

## 2. Measurement interpretation (retained)

- **AnimalPos movement** is thresholded displacement-event activity: the vendor emits a position change when the 500-ms
  window mean of the raw reads moves at least 200 grid units from the last written position (an inferred rule,
  `docs/PHENOSYS_ANIMALPOS_RECONSTRUCTION.md`). It is not distance travelled and not a calibrated locomotion measure.
- **Raw reader changes** depend on detection: read density, read competition between tags, board sensitivity and
  carry-forward conventions all change them. They are descriptive and never locomotion.
- **Same-reader or same-column co-assignment** (shared_zone_use, reader co-assignment) is **not validated physical
  proximity, contact or affiliation**; it is not retained as such (construct S6).
- A recently detected (supported) position is not a validated location, and a carried-forward position is not an observed
  location.
- A controlled-tag bench calibration would test hardware and reconstruction performance under bench conditions. It would
  not by itself validate the positions of freely behaving, implanted mice or any social behaviour. This is a limitation of
  what any such calibration could show, recorded here as a limitation and not as a task.

## 3. Stage 19 closure (no calibration)

- `s19c_v0.2_defd263` is **retained as descriptive and sensitivity evidence**, unregistered and inactive. Live Stage 19, its
  38 live outputs and 19 publication copies are unchanged.
- **Both reconstruction bases are kept:** the event-start basis (historical reference) and the single-read basis (an
  alternative definition). Neither is a validated location; their difference is a difference between two definitions, not
  a measured error of either. No basis is chosen.
- **Unknown time is explicit** and never redistributed. The occupancy and share tables (`occupancy_animal_*`,
  `cage_episode_*_column`, `column_shares_by_bound`, `stage19_comparison_animal_window`) carry both denominators: the share
  of observed time (`share_all` or `share_of_observed_time`; units + unknown = 1) and the share of supported time
  (`share_supported` or `share_of_supported_time`). The basis-comparison tables name their denominator in a `denominator`
  column; the support, gap and organisation tables report observed-time quantities.
- **Support conventions:** unrestricted LOCF (historical reference), observation age <= 300 s and <= 60 s are reporting
  conventions, not validated thresholds. The 2-s flip filter is likewise a convention.
- **Periods:** `stage19_window` (the existing Stage 19 blocks; CC4 restricted to A1, I2, A2) is the primary set;
  `cc4_grid_period` (the CC4 blocks after it) is reported separately and never pooled with it. Whole-span pooling is computed
  only in memory, as parity gates against the srs release (G12, G12b, G13); it is not an output.
- **Unknown share** (read basis; the bounded unknown time is identical in the event basis, gate G06), `stage19_window`,
  pooled over 432 animal-recordings of 111 animals: at <= 60 s 19.84% of observed time (active) and 46.85% (light); at
  <= 300 s 5.99% and 21.96% (`tables/unsupported_time_summary.csv`). The 19.5% and 45.1% quoted in v1.4 (section 3 and
  section 11 item 2) refer to the srs_v1.1 reconstruction span, not to the Stage 19 window.
- The open items of v1.4 section 8 ("Remaining revision": freezing the method after the calibration review, a gated
  group-descriptive layer, figure replacement) are **closed without action**: the method is not frozen to one basis, and
  nothing further is built. Any later activation, registration, group-descriptive layer, model or manuscript import needs a
  separate instruction and its own bounded specification (both bases, the support conventions and the released masks).

## 4. Stage 31b-31d QC sensitivity analyses (completed)

- The QC sensitivity models **were refitted** on 2026-10-15 under rule cc4e_v1.0, in an isolated root, with hypotheses,
  models, parameters, seeds, 4,999 bootstrap draws and the Holm families unchanged:
  `<Behavior>/RANM/cc4_qc_reruns/runs/p31b_cc4e10_20261015`, `p31c_cc4e10_20261015`, `p31d_cc4e10_20261015` and the
  `_isens_` variants (inactive sensitivity). Comparison: `<Behavior>/RANM/cc4_qc_comparison/runs/cqc_v1.0_defd263`
  (`docs/CC4_QC_RERUN_COMPARISON.md`).
- The **historical and registered runs are preserved** and remain the record: `phase_groups_20260929_v2`,
  `phase_statistics_20260929_v1`, `phase_phenotypes_20260929_v1` (and the Stage 31 run `whole_inactive_20260929_v1`). The
  QC reruns are post hoc QC-sensitivity analyses and do not replace them. Manuscript imports remain historical; they are
  not "QC-revised".
- The comparison was accepted (2026-10-15); no further model rerun is required for this issue. Its finding: no change in
  biological interpretation at the model level (0 of 4 Stage 31c contexts and 0 of 12 Stage 31d contrasts change their
  interpretation flag); descriptive 31b contrasts change sign in 4 of 108 (QC primary) and 6 of 108 (inactive
  sensitivity), with one unadjusted pointwise interval excluding zero.

## 5. Accepted limitations (calibration not performed)

1. **Basis.** Whether the event-start or the single-read basis localises a tag correctly, at a boundary or in the house, is
   unresolved. Both are retained as sensitivity analyses; neither is ground truth.
2. **Column-level accuracy.** Column occupancy is a descriptive representation with substantial observation uncertainty; its
   accuracy is not established. Column changes with the house in place and house exits are unmeasured.
3. **Unread time.** Read competition, shielding by the house and absence cannot be separated; unread house time is not
   absence.
4. **Support age.** The 60-s and 300-s conventions and the 2-s flip filter are not validated against known positions.
5. **Boards.** Boards 3 and 4 read less; whether this is hardware or cage, rack and occupancy is open. Board stays a design
   covariate or stratum without a hardware claim; board effects estimated from SIS cages assume the same offset in CON cages.
6. **Read competition.** The effect of the number of tags present on read-dependent measures is uncalibrated.
7. **Acquisition rules.** Stage 00b's `aqc_rules_v0.2_provisional` thresholds remain data-derived and provisional.
8. **Co-location.** Co-assignment is not physical proximity, contact or affiliation (section 2).
9. The limitations of v1.4 section 11 items 6-9 and 11-15 (dead-channel bias, the B5 CC4 censored time, untracked animals in
   B1, light-phase measures, active phases A2-A5 of B2|sys.5|CC4, the CC4 grid period, untracked tags, knockout scales,
   protection coverage) stand unchanged. Items 1, 3, 4, 5, 10 and 13 no longer wait for a pilot; they are covered by 1-6
   above.

## 6. Remaining technical issues (reported, not fixed in this closeout)

1. **Stage 09 direct invocation** bypasses the frozen-input guard, which only `run_all_analysis.R` applies (disclosed in
   `Analysis/README_pipeline.md`). A direct `Rscript` run on the live root could rewrite pinned files unguarded.
2. **Stage 19 direct invocation** uses the output-layout and archive routing guards but not the frozen-input or
   protected-output guard, and writes to fixed live and publication paths.
3. **Stage 31 direct function use:** `run_cc4_grid_exposure(..., censor_manifest = ...)` does not itself enforce an isolated
   output root; only the command-line block of `Analysis/31_cc4_grid_exposure.R` requires `MMM_GRID31_OUTPUT_ROOT` for a
   censored run.
4. **Stage 33 workbook pin mismatch.** `Functions/stage33_run.R` pins `SIS_Analysis/E9_Behavior_Data.xlsx` at sha256
   `38d200a2...6e1127`; the current file hashes to `980bc8c6...1b337` (modified 2026-10-15 08:30). A new Stage 33 run would
   stop at its input gate. Existing Stage 33 outputs are not affected. **Do not update the pin blindly:** first verify that
   the workbook values Stage 33 consumes are numerically equivalent to the pinned version, document the input revision, and
   only then authorize a pin and provenance update.
5. **Live QC-root status note.** `analysis_ready/quality_control/raw_acquisition/REVIEW_STATUS.md` still points to manifest
   v1.0 and specification v1. It lies inside the append-only, protected live QC root (`Functions/protected_outputs.R`), so it
   was not edited; the current pointers are this document and `docs/spatial_exclusions/spatial_analysis_exclusions_v1.1.csv`.
   Updating it needs an explicit instruction (and a protection baseline that records the change).
6. **Protection coverage.** The round-level checks are tier 1 (existence, sizes, selected manifests and the baseline
   snapshot); the full-hash tier 2 was not run.

Until a hardening change is authorized, the guarded invocations are operational requirements: run Stage 09 through
`run_all_analysis.R`; run Stage 19 only for a deliberate replacement of its live outputs; use Stage 31 censoring only through
the command-line block with an isolated root.

## 7. Corrections to v1.4

- v1.4 said "no biological model is refitted (Stages 31b-31d included)". **Corrected:** the Stage 31b-31d QC-sensitivity
  models were refitted in isolated roots on 2026-10-15; the historical and registered runs were preserved (section 4).
- v1.4 section 8 said whole_span is "computed and labelled alongside" the primary window. **Corrected:** s19c_v0.2 reports
  `stage19_window` and `cc4_grid_period` separately; whole-span calculations are internal parity checks only (section 3).
- v1.4 quoted 19.5% / 45.1% unknown time without its span; these are reconstruction-span figures (section 3).
- v1.4's calibration status (booking approved, proposed date, Stage 19 held until calibration) is withdrawn (section 0).
- v1.4 construct S8 ("calibration: approved for preparation; not booked") is replaced by: **calibration not performed; its
  questions remain accepted limitations.** S2's "calibrated tau* when available" no longer applies: tau 2 s stays a
  convention.
- v1.4 said a targeted revision of the affected spatial outcomes and models would be proposed after the calibration pilot.
  **Withdrawn:** no such revision is pending; the registered analysis stays as it is.

## 8. Evidence checked for this closeout (read-only, 2026-10-15)

- Repository: branch `refactor/rfid-four-domain-characterization`, HEAD `788af5f` with a clean working tree at the start;
  documentation edits only.
- The run folders named in sections 1, 3 and 4 exist; s19c_v0.2's `manifest/run.csv` and `tables/gates.csv` and
  `tables/unsupported_time_summary.csv` give the figures quoted; the Stage 00b ledger lists the two live runs.
- Code facts: the Stage 08 guard calls; the 31b-31d opt-in (`MMM_CC4_OUTCOME_ELIGIBILITY`); the Stage 31 censoring
  requirement in the command-line block only; Stage 00b absent from `run_all_analysis.R`; the candidate's block sets
  (`SOC_BLOCK_SETS` in `Functions/spatial_occupancy_candidate.R`).
- The Stage 33 workbook hash was computed read-only.
- External input: the integration audit of 2026-10-15 (`RFID_pipeline_integration_audit_2026-10-15.md`, outside the
  repository), whose findings were verified against the current files before these edits.

## 9. Provenance

No code, data, exclusion definition, model specification, registered output, protected file or manuscript figure was
changed by this closeout; no analysis was rerun and no candidate activated. The calibration files of `docs/rfid_calibration/`
and their pins are unchanged.
