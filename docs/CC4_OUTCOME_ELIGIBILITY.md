# CC4 outcome x cage-episode eligibility (rule cc4e_v1.0)

*Written for: Tobias and reviewers. Status (2026-10-15, current): rule fixed and implemented as an opt-in, versioned, isolated mechanism (`MMM_CC4_OUTCOME_ELIGIBILITY`). The rule-applied Stage 31b-31d QC-sensitivity reruns were completed on 2026-10-15 in an isolated root (six runs under `<Behavior>/RANM/cc4_qc_reruns/runs/`; comparison `cqc_v1.0_defd263`, `docs/CC4_QC_RERUN_COMPARISON.md`), and the comparison was accepted with no further model rerun required. S13, Stage 29 and every frozen or registered output, including the historical Stage 31b-31d runs, are unchanged; manuscript imports remain historical. Closeout: `docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.5.md`.*

## 1. Why

Manifest v1.1 records two CC4 hardware episodes:
- **B2|sys.5|CC4:** R3 silent for the whole session (SX02). The movement exclusion SX06 was written for the Stage 29 window, CC4 A1.
- **B5|sys.1|CC4:** R5 silent for the whole session (SX03, SX07), and the cage is censored from 2023-12-04 14:00:42.962 (SX04).

Stages 31b-31d use a complete-span rule. That rule already leaves B5|sys.1|CC4 out of every roster. B2|sys.5|CC4 is in every roster and every model.

The rule below does not drop the episode everywhere. It decides, per animal x cage episode x phase family x phase, which outcome cells are affected, and keeps everything else.

## 2. The rule (fixed before any rule-applied result was seen; group-blind)

All Stage 31-31d outcomes are vendor displacement-event (crossing) counts from the AnimalPos stream. None is an occupancy, co-location or cage-use measure. The exclude_spatial records (SX01-SX03) are therefore not applied as such; the dead reader acts only through the clauses below.

| clause | condition | family / phases | status | use |
|---|---|---|---|---|
| E03 | record of CC1-CC3 (SX01, SX05) | - | unaffected | Stage 31-31d read CC4 only |
| E04 | censor record; phase block ends after the censoring time | any | censored | missing (takes precedence over everything) |
| E05 | whole-session dead reader together with its exclude_movement record | Active A1-A5 | excluded_hardware | out of primary; in the all-episode sensitivity |
| E06 | same episode; CC4 knockout material for any inactive outcome (reference_rate, post_rate, log_rate_ratio) | Inactive I2-I5 | retained_material | primary, flagged; the inactive sensitivity is mandatory; a decision for Tobias (an exclusion needs an approved new rule version) |
| E07 | same episode; CC4 knockout supplied, not material | Inactive I2-I5 | retained_flagged | primary, flagged; left out of the inactive sensitivity |
| E08 | same episode; no CC4 knockout supplied | Inactive I2-I5 | retained_unverified | primary, flagged; left out of the inactive sensitivity |
| E09 | no complete clock and animal span, no record (the frozen rule) | any | incomplete_span | missing |
| E10 | observed cell whose cage x family holds a censored, excluded or incomplete cell | family cells | cage_roster_incomplete | outside the complete-family roster |
| E11 | otherwise | - | included | primary |
| E12 | statuses never remove an animal | - | - | a family's roster drops only that family |

Precedence: E04 > E05 > E09 > E06/E07/E08 > E11. E10 then replaces E06/E07/E08/E11 where the cage's family is incomplete.

**The inactive family is never excluded under cc4e_v1.0.** Manifest v1.1 keeps the light-phase crossing rate of the SX05-SX07
episodes ("evaluated separately and kept"; no other outcome). The CC4 knockout therefore only grades the kept inactive cells
(E06-E08). If it is material, the cells stay in the primary analysis, flagged, the inactive sensitivity becomes mandatory, and
the audit README names it as a decision for Tobias; excluding them would need his approval of a new rule version.

The full table, with rationales, is CC4E_RULES in Functions/cc4_outcome_eligibility.R and tables/rules_used.csv of every audit run.

**Result for the two episodes:**
- **B2|sys.5|CC4:** A1-A5 excluded_hardware (SX02;SX06). I2-I5 retained_flagged: the release CC4 knockout (ckr_v1.0_4aebc90) is not material for R3 on any inactive outcome (without a knockout they would be retained_unverified).
- **B5|sys.1|CC4:** A1-A2 excluded_hardware (SX03;SX07). A3-A5 and I3-I5 censored (SX04). I2 observed but cage_roster_incomplete (SX04), whatever the R5 CC4 knockout says.
- **Every other CC4 cage:** included. All Female batches (B3, B4, B6) have no record.

## 3. Evidence and limits

**Evidence for each clause:**
- **Active family (E05).** The R3 and R5 knockout on the 117 healthy board-recordings is material for the active crossing rate:
  - R3: +1.7/h; median |shift| 0.33 SD, 90th percentile 0.90 SD.
  - R5: -3.0/h; 0.58 / 1.21 SD.

  Both readers were silent from span_start to span_end, so A2-A5 carry the same fault as A1, the Stage 29 window SX06 and SX07 were written for. Applied literally to A1 only, SX06 would change nothing in 31c/31d, which do not use A1.
- **Inactive family (E06-E08).** The light-window knockout was not material (0.12 / 0.35 SD for R3, 0.12 / 0.41 SD for R5). It was measured on non-grid windows; I3-I5 are grid days with rates 2-4x higher, and an activity-dependent bias would load on the I2 -> I3-I5 change. Hence a CC4 reader knockout (docs/CC4_READER_KNOCKOUT.md, tables/materiality_cc4.csv) grades the kept cells. Until it is supplied, the inactive cells are retained_unverified. The inactive sensitivity without them is written by the audit as its own run (--variant=inactive_sensitivity), so Stages 31b-31d can be rerun on it like on the primary file.
- **Censoring (E04).** SX04 is recording-duration censoring: time after the last read is unobserved. It is missing, not excluded.

**Limits:**
- Extending SX06/SX07 from A1 to A1-A5 rests on SX02/SX03 (the reader silent for the whole session) and the CC4 knockout;
  the movement records themselves name the Stage 29 window. Approved by Tobias on 2026-10-15.
- The active exclusion is fixed and does not consult the CC4 knockout; changing that needs a new rule version.
- The inactive family is kept whatever the CC4 knockout shows (E06); a material result is reported, not acted on.
- A knockout models a silent antenna, not other changes on that board.
- The rule is tied to manifest v1.1. Another manifest version needs a new rule version, and cc4e_manifest() refuses it.
- cage_roster_incomplete is labelled on the 31b families (A1-A5, I2-I5). 31c and 31d recompute their roster from the cells over A2-A5, so their roster never depends on that label. No real cage differs between the two definitions.

## 4. Outcome dependencies (tables/affected_analyses.csv)

| stage | output / context | family | depends on | consequence under cc4e_v1.0 |
|---|---|---|---|---|
| 31 | animal/cage clock profiles, active blocks; `<batch>_active_clock_profiles.png` | Active | 5-min crossings: a silent reader removes position changes through it | B2 sys.5 excluded from primary displays; B5 sys.1 excluded/censored (not wired: Stage 31 is descriptive and unchanged) |
| 31 | clock profiles, inactive blocks; candidate windows; whole-inactive tables, bootstrap, population | Inactive | inactive crossings; same-clock difference to I2 | B2 sys.5 retained, flagged; B5 sys.1 censored, already outside the cohort |
| 31 | phase_animal_coverage (bracketing, ReadCount) | QC | needs a read by any reader | unaffected; QC kept |
| 31b | Active family: animal table, cage/batch/group summaries, contrasts | A1-A5 | active rate, change from A2 | B2 sys.5 leaves the active roster only |
| 31b | Inactive family | I2-I5 | inactive rate, change from I2 | B2 sys.5 retained, flagged |
| 31c | Male_Active: primary NB2 RRR, model rates, secondary, LOBO, dispersion, mixed cages, predictive checks | A2-A5 | summed active crossings | B2 sys.5 excluded |
| 31c | Male_Inactive | I2-I5 | summed inactive crossings | retained, flagged |
| 31d | Male_Active: omnibus, 3 pairwise RRRs, intervals, sensitivities | A2-A5 | animal active crossings | B2 sys.5 excluded |
| 31d | Male_Inactive | I2-I5 | animal inactive crossings | retained, flagged |
| 31c/31d | Holm (n=4; n=4 and 12) | both | multiplicity across contexts | a changed Male Active P changes every adjusted P |
| all | Female contexts | both | - | unaffected |

## 5. How to apply it

1. **Run the audit.** It reads the frozen runs and writes runs/<id> under an isolated root:

   `Rscript Testing/audits/audit_cc4_outcome_eligibility.R [--knockout-run=<cc4 knockout run>] [--variant=primary|inactive_sensitivity]`

   - Default output: `<Behavior>/RANM/cc4_outcome_eligibility/runs/cc4e_v1.0_<commit>` (variant inactive_sensitivity: suffix `_isens`). A release id needs committed code; ids starting with "dev" may run on uncommitted code.
   - A release run accepts only a release knockout run (its run record verified: not a dev run, clean code, parity gate passed, all six CC4 recordings). The knockout run id, commit and gate are recorded in run.csv and the README, and the knockout-graded cells cite the run (`SX02;SX06;ckr:<run id>`).
   - The audit verifies every input manifest: 31c and 31d must have read the 31b table; the Stage 31 chain leads to the cage-label registry, which is also checked against the configured hash; srs_v1.1 animals.csv is checked against its manifest.
   - It checks that CageID equals the manifest CageEpisodeID with the record's animals, and that no cage-label correction touches an episode.
   - It wraps its writes in the protection guard and fits no model.
2. **Rerun 31b, 31c and 31d with the rule.** Set `MMM_CC4_OUTCOME_ELIGIBILITY=<audit run>/tables/outcome_episode_eligibility.csv` and give an output directory in an isolated root (mmm_isolated_output_root_check; anything inside the live project root is refused).
   - Before any read or write, the stages verify the file against the run's manifest/output_files_sha256.csv and the rule version; a changed byte is refused.
   - The stages refuse an eligibility file from a dev audit run or uncommitted code unless MMM_CC4_ALLOW_DEV_ELIGIBILITY=TRUE is set (dev reruns only; the protocol then says so).
   - The inactive sensitivity rerun uses the eligibility file of a `--variant=inactive_sensitivity` audit run; the stage protocol names the variant.
   - The eligibility file, its run manifest and Functions/cc4_outcome_eligibility.R join the hashed inputs.
3. **What the rule changes in the stage outputs:**
   - **31b:** builds a complete-cage roster per phase family. B2|sys.5 keeps its inactive family when its active family leaves.
   - **31c/31d:** a complete cage additionally has no excluded_hardware or censored cell in the family.
   - **New columns:** FamilyExcluded, EligibilityStatus, EligibilityRecord and EligibilityVersion are appended last. Rosters and populations gain ExclusionReason, e.g. `excluded_hardware:B2|sys.5|CC4 (SX02;SX06)`.
   - **New files:** 31c writes eligibility_model_input.csv and 31d writes eligibility_input.csv.
   - **Unchanged:** no row of the 31b animal table is dropped and no value changes; the 31c/31d model inputs lose the rows of the excluded family cells (e.g. the 16 active animal-period rows of B2|sys.5).
4. **Without the env var nothing changes in the results.** Every code path and result table is byte-identical to before; only input_manifest.csv and manifest.csv differ, because they record the hashes of the changed code files. This is proven by digests pinned on a fixture with the pre-change code, and by the audit's frozen_parity.csv: the stage functions without the rule reproduce all 14 frozen tables byte for byte.

## 6. Missing versus excluded

- **excluded_hardware** is an observed but compromised measurement. It is out of the primary analysis and in the all-episode sensitivity.
- **censored** and **incomplete_span** are missing: unobserved, NA, never zero, and never "excluded".
- **cage_roster_incomplete** marks an observed value outside the complete-family roster.
- **No animal is removed from the roster.** The models drop only the affected family cells, since each Sex x family is a separate fit. B2|sys.5 therefore contributes to Male Inactive and not to Male Active.
- **The four B5|sys.1 animals** contribute to no CC4 model family because of censoring plus hardware. They are not excluded animals.
- **Earlier cage changes are never affected by a CC4 record.**
  - All 11 animals named in the manifest keep their CC1-CC3 episodes (tables/cross_cc_episodes.csv).
  - B2|sys.2|CC3 (SX01, SX05) affects CC3 analyses only. OR106 is affected twice: CC3 by SX05 and CC4 by SX06.
  - The eligibility table is keyed by AnimalNum, AnimalKey and CageID (= CageEpisodeID), so a cross-CC longitudinal model can join it without dropping animals.

## 7. Sample sizes (release audit cc4e_v1.0_4aebc90, with the release knockout ckr_v1.0_4aebc90)

Frozen -> rule-applied primary -> inactive sensitivity, as animals / cages:

| stage | sex | family | all | RES | SUS | CON | CON cages | batches | mixed RES/SUS cages |
|---|---|---|---|---|---|---|---|---|---|
| 31c, 31d (and 31b) | Male | Active | 49/14 -> 45/13 -> 45/13 | 22/9 -> 19/8 | 15/8 -> 14/7 | 12/3 | 3 | 3 | 6 -> 5 |
| 31, 31b, 31c, 31d | Male | Inactive | 49/14 -> 49/14 -> 45/13 | 22/9 -> 22/9 -> 19/8 | 15/8 -> 15/8 -> 14/7 | 12/3 | 3 | 3 | 6 -> 6 -> 5 |
| all | Female | both | 58/15 unchanged | 28/11 | 18/9 | 12/3 | 3 | 3 | 8 |

- Cells by status (999 = 111 animals x 9 phases): included 927, excluded_hardware 28, censored 24, retained_flagged 16, cage_roster_incomplete 4; no cell is retained_material.
- The inactive sensitivity is the audit run cc4e_v1.0_4aebc90_isens (B2|sys.5 inactive cells left out); the stages read its eligibility file like the primary one.
- Every stage gate (31b group completeness, 31c condition completeness, 31d phenotype completeness) passes on all three rosters.
- The B2 SUS active batch mean in 31b now rests on one animal (B2|sys.3|CC4).

## 8. What the frozen runs mean now

The frozen runs are the all-episode sensitivity analysis. They keep B2|sys.5|CC4 in both families: whole_inactive_20260929_v1, phase_groups_20260929_v2, phase_statistics_20260929_v1 and phase_phenotypes_20260929_v1.
- They are unchanged and referenced by hash.
- Their rosters equal what the legacy code computes today (frozen_parity.csv).
- They are not the primary analysis under cc4e_v1.0 for the Male active contexts.
- For all Female contexts and for Male Inactive, their rosters equal the rule-applied primary rosters. A rerun with the rule changes the Male Active fits and, through Holm and through 31c's secondary status (computed across all contexts), the adjusted P and the secondary label of every context.

Rule-applied 31b-31d runs: **done** (2026-10-15). When this section was first written no rule-applied run existed; the six isolated QC-sensitivity runs (`p31b_cc4e10_20261015`, `p31c_cc4e10_20261015`, `p31d_cc4e10_20261015` and their `_isens_` variants) were then made with the release audits `cc4e_v1.0_4aebc90` and `cc4e_v1.0_4aebc90_isens` (with the CC4 knockout `ckr_v1.0_4aebc90`) and compared with the frozen runs (`docs/CC4_QC_RERUN_COMPARISON.md`). The frozen runs above remain the record; the QC runs do not replace them (section 10).

## 9. Verification

- **Tests:**
  - Testing/tests/test_cc4_outcome_eligibility.R covers the rule table; statuses with and without a knockout; fail-closed checks; legacy byte parity; the per-family roster; eligibility-file refusals; the drivers; sample sizes by hand; missing vs excluded; and the cross-CC table.
  - Added cases in test_cc4_phase_groups.R, test_cc4_phase_statistics.R and test_cc4_phenotype_statistics.R.
  - test_spatial_exclusion_manifest.R allows the helper as a manifest reader and still forbids the registered stages; 31b-31d read only the eligibility file.
- **Protection:** each audit run records manifest/protection_check.csv; in both release runs it is empty (no protected file changed).

## 10. Decisions (Tobias, 2026-10-15)

1. **A1-A5: approved.** B2|sys.5|CC4 leaves the primary Stage 31 active-phase crossing analyses across A1-A5: R3 was silent
   for the whole recording, and the independent CC4 knockout shows material effects on the active crossing outcomes.
2. **Inactive family: kept, flagged**, as the manifest says; the release CC4 knockout is not material for R3's inactive
   outcomes (retained_flagged). The inactive sensitivity without the affected cage is run alongside.
3. **Rule-applied reruns of 31b-31d: approved and done** (2026-10-15) as post hoc QC-sensitivity analyses in an isolated root,
   with the full model specifications unchanged; the comparison with the historical results is
   `docs/CC4_QC_RERUN_COMPARISON.md`. They do not replace the registered outputs. Earlier cage changes, S13, the historical
   registered results and the master animal rosters are unchanged.
