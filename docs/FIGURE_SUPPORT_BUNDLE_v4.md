# Figure-support bundle v4 (source data for the consolidated manuscript Figure 1)

## Purpose

Exp9_manuscript draws its own figures from frozen bundles and computes nothing statistical. On 2026-10-12 Tobias asked
for the code of Figure 1 to be consolidated so that it draws, from the pipelines, the layout he had assembled by hand:
the design timeline with postnatal ages (1a), CombZ and its components (1b), the Stage 14 domain heatmap (1c), CombZ
against early movement by sex with a least-squares line and Spearman rho (1d), and the held-out prediction (1e). Three of
those needed data no bundle carried yet; v4 adds them.

v4 contains every v3 table unchanged and byte-identical (F1, F1b, F2, F2b, P1, P1b, P1c, D1, M1-M4). It adds E1, E2,
E2b, E2c, T1 and T1b.

- **Writer.** `Analysis/16c_figure_support_bundle.R` with `Functions/figure_support_bundle.R`.
- **Test.** `Testing/tests/test_figure_support_bundle.R` (section 8c), synthetic data only.
- **v1 to v3 are unchanged.** `fsb_v1_20260930_f06552c`, `fsb_v2_20260930_a23d699` and `fsb_v3_20261007_a2acd57` stay FROZEN.
- **Bundle id.** `fsb_v4_<YYYYMMDD>_<commit7>`, with its own row in `BUNDLE_REGISTRY.csv`.

## Sources

| Table | Source | Pin |
|---|---|---|
| E1 | Stage 09, `pipeline/09_early_prediction/10min/tables/primary_movement_entropyacf1_correlations_by_sex.csv` | sha256 `aeec1749...` |
| E1 (identity gate only) | Stage 09, `pipeline/09_early_prediction/10min/tables/model_ladder_input.csv` | sha256 `da78f80e...` (= the Stage 29 v1.0.1 release input pin) |
| E2, E2b, E2c | post hoc run 29c, `pipeline/29c_posthoc_movement_combz_fit/v1.0_fd61cd9/` | `audit/output_manifest.csv` sha256 `fc0d13db...`; registry `docs/POSTHOC_MOVEMENT_COMBZ_FIT_REGISTRY_v1.0.md` (`a617bb5b...`) |
| T1, T1b | `Exp9_Social-Stress/Planning/E9_Experimental Timeline.xlsx`, sheet `Animals` | sha256 `098f22db...` |
| T1, T1b | ebb_v101 `V_runtime_gates.csv` (cage-change times), `A4_combz_animals.csv` | the ebb_v101 manifest |

**The planning workbook.** On 2026-10-12 Tobias added the Animals column "SIS start" (saved 11:21 and 11:50). The batch-6
cells read 7 Apr 2023 where the batch's CC1 and its own test dates say 7 Apr 2024. At his request the 20 cells were
corrected to 45389 (7 Apr 2024) by changing only their cached values in the sheet XML. Read back, every other cell of every
sheet is unchanged, including the Blinding sheet's random codes, which re-draw at every save in Excel. The 11:50 file is
kept as `Planning/old/E9_Experimental Timeline_2026-10-12_before_B6_SIS_start_fix.xlsx`. Stage 33 pinned an earlier state
of the workbook (`2b785435...`, before "SIS start"), so a new Stage 33 run needs that pin renewed.

## Added tables

| Table | Content | Rows |
|---|---|---|
| `E1_movement_combz_spearman_by_sex` | verbatim copy of Stage 09's sex-stratified correlations (Pearson and Spearman with p and BH q within sex, per feature); `source_run`, `source_file`, `source_sha256` appended | 6 = 3 features x 2 sexes |
| `E2_movement_combz_fit_lines` | verbatim copy of the 29c line and pointwise 95% band on a 101-point grid over each sex's observed range | 202 |
| `E2b_movement_combz_fit_coefficients` | verbatim copy of the 29c intercept and slope with SE, residual df, residual SD and R squared (no t, no P) | 4 |
| `E2c_movement_combz_fit_diagnostics` | verbatim copy of the 29c diagnostics | 2 |
| `T1_design_ages` | per design step: label, definition, n animals, minimum and maximum postnatal age | 8 |
| `T1b_design_ages_by_animal` | per CombZ animal and design step: date of birth, start and end date, start and end age | 936 = 117 x 8 |

The design steps and their ages (postnatal day = event date minus date of birth; 117 animals each):

| Step | Date | Ages |
|---|---|---|
| Shuffle start | CC1 cage-change time (ebb_v101) | P22-29 |
| Shuffle stop | CC4 cage-change time + 1 day: the end of the active phase after the last regrouping | P35-42 |
| Sucrose preference test | the workbook's window, first to last day | P36-48 |
| Social preference test | workbook SocPref | P42-49 |
| Open field test | workbook OFT | P44-51 |
| Novel object recognition test | workbook NOR | P45-52 |
| Elevated plus maze | workbook EPM | P46-53 |
| Blood and tissue collection | workbook End date | P51-59 |

- **What is computed.** Nothing is fitted, tested or adjusted. E1 and E2-E2c are text copies; T1b subtracts dates; T1
  takes minima and maxima. The sucrose window is parsed from both date forms the workbook uses (`mm/dd/yyyy - mm/dd/yyyy`
  and, for batch 6, `dd-Mon-yy - dd-Mon-yy`), with an English month table rather than the system locale.
- **Gates before the write.**
  - E1: 6 rows, n per sex = the A2 animals, and Stage 09's own input = A2 `Movement_mean` and `observed_CombZ` for all 111 animals within 1e-12. So the copied rho and q describe the plotted points.
  - E2-E2c: the 29c run folder, manifest, read-only files, README and run manifest; row counts; registry identity on every row; both fits OK; each line's n and x range = A2.
  - T1: all 117 animals matched once; workbook Batch = ebb_v101 Batch; every date present; the design order holds for every animal; CC1 ages 22-29 (Stage 33 GC-6l); the workbook's SIS start = the CC1 date for every animal.
  - T1, against the workbook's own records: every age the workbook records itself equals the computed age. That is SocPref, OFT, NOR and EPM for 117 animals, the sucrose window for 98, and the end-date age for 117.
- **Gate after the write, before the registry row.** Every line of E1, E2, E2b and E2c is the source line followed by the
  appended provenance cells.
- **Display wording.** T1 labels are the names of behavioural tests in the design (e.g. the social preference test), not
  labels of RFID measures; they are fixed constants and the RFID display-wording rule does not apply to them.
- **Claims.** E1 is Stage 09's descriptive sex-stratified estimate ("not an interaction test"); the manuscript prints its
  BH q within sex. E2 is a post hoc display line that adds no test (registry 29c section 6 governs its legend wording).

## Real run

- Commit f4c2ba6; bundle `fsb_v4_20261012_f4c2ba6`, FROZEN; `00_manifest.csv` sha256
  `c826dab157a50e7dc1acf7431201faf62675c6f0c2df9d55f6f53dfc5f16363c`; 22 files, 1,364,981 bytes.
- 138 of 138 pre-write gates; 151 of 151 gates in `logs/fsb_v4_20261012_f4c2ba6_gate_results.csv` with the post-write
  and registration gates.
- The dry run at the same commit passed 148 of 148 gates and wrote the same tables.

## Provenance

`H_provenance.csv` adds `stage09_table`, `posthoc29c_run_dir`, `posthoc29c_run_commit`, `posthoc29c_output_manifest_sha256`,
`posthoc29c_registry_sha256`, `posthoc29c_tier`, `timeline_workbook`, `timeline_workbook_sha256`, `design_age_unit` and
`design_steps`. `scientific_recomputation` keeps the earlier wording and adds that E1 and E2-E2c are verbatim copies and
T1 / T1b date arithmetic.
