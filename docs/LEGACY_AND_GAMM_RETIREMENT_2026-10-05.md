# Retirement of the legacy products and GAMM Stages 21 and 23-26 (2026-10-05)

On 2026-10-05 Tobias asked for a decision on the legacy layer and the GAMM Stages 20-26: keep what makes sense, retire
the rest. This file records what was kept, what was retired and why, and where everything went. The review behind it
(2026-10-04/05) read every stage, its live outputs and its readers. It changed no frozen run folder, no pinned input and
no frozen code.

## Kept

- **Stages 20 and 22, as descriptive Extended Data candidates without group inference.** They are the only source of the
  within-night 10-min profile of the RFID position-change stream; the frozen Stages 29 and 32 summarise whole 12-h
  windows. Stage 20 shows the first active phase after CC1 by sex and group. Stage 22 shows where in the night the
  CC1-to-CC4 change happens: in the live fits most of the rise falls in the first hour after 18:30.
  - Use: display trajectories and difference curves only. Do not show their p-values beside the frozen results; that
    would be a second, unregistered test of registered questions, and the results were seen before the freeze.
  - Their group contrasts carry no cage term, while CON is 3 intact cages per sex. Every SUS−RES contrast is null and
    agrees in direction with the frozen results.
  - The CC4−CC1 p-values of Stage 22 treat cohort-specific change as residual. The change is positive in every cohort
    but varies between them, and CON shares it, so under Stage 32 registry section 9 it is not adaptation.
  - If an Extended Data panel is wanted, export it through a new figure-support bundle version written by Stage 16c,
    never through the retired Stage 26. If the panel is declined, retire Stages 20 and 22 with their helpers.
  - Their 2026-09-07 leftovers (old classification) were moved to `history/retired/` (see Outputs).
  - Kept with them: `Functions/gamm_auc_helpers.R`, `gamm_diagnostics_helpers.R`, `gamm_group_inference_helpers.R`,
    `single_window_stage_runner.R`, `stratified_gamm_driver.R`, `stratified_stage_runner.R` and
    `acute_active_window_helpers.R`, and the path keys `behavior.first_active_trajectory` and
    `behavior.repeated_acute_movement`.
- **`docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv`**, byte-unchanged, because the frozen configurations cite it. Its stale rows
  are flagged in `docs/MANUSCRIPT_ANALYSIS_REGISTRY_ADDENDUM.md`.
- **The Stage 14 first-night five-domain section** (`Functions/first_night_domain_driver.R`), the conservative
  flat-family sensitivity beside Stage 28. Removing it would also make the Stage 14 promotion refuse.
- **The tracked `results/manuscript_bridge/` files**, unchanged, until Extended Data 5 and 9 in Exp9_manuscript move to
  `ebb_v101`; then archive them. They predate the 2026-09-20 CombZ correction, so nothing new may import them.

## Retired

The code is removed from the tree. Every file is recoverable with `git show 1a332a4:<path>` (the last commit that
contains all of them); the column "last change" gives the commit that last edited the file.

| File | Lines | Last change | Why |
|---|---:|---|---|
| `Analysis/16_manuscript_behavior_report.R` | 2342 | 0555c90 | Its package is superseded by 16b and `ebb_v101`; its provenance verified only 18 of 22 files. Its only live role, writing `output_index.csv` and the analysis_ready README, moved to `Functions/behavior_output_index.R` and `Maintenance/Refresh-BehaviorOutputIndex.R`. |
| `Analysis/21_cc1_active_longitudinal_gamm.R` | 118 | eeb373d | All contrasts null (min q 0.734). Its one GAMM-specific result, a male rise from night 1 to night 4, is shared by CON and coincides with the cohort split (sex is nested in cohort). Stage 32 gives the A1-A4 rates. |
| `Analysis/23_first_inactive_gamm.R` | 150 | eeb373d | Its SIS-vs-CON contrasts have no cage term and lost support in unregistered cage-aware checks (male p .0024/.0076 to about .04-.07). The light-phase measure cannot be separated from rest or chip loss (KNOWN_LIMITATIONS items 3 and 12). The frozen configuration already says Stages 23/25 feed no product. |
| `Analysis/24_cc1_inactive_longitudinal_gamm.R` | 124 | eeb373d | All contrasts null at the declared family (min q 0.053); same measure and limits as Stage 23. |
| `Analysis/25_repeated_cagechange_inactive_gamm.R` | 130 | eeb373d | No supported change. Its five CON-referenced contrasts at q 0.047 lost support once CON cages were the unit (for example female CC4 SUS−CON p .0053 to .23). Do not cite them. |
| `Analysis/26_build_gamm_manuscript_outputs.R` | 485 | eeb373d | Assembly only; nothing in either manuscript repository reads it. Its claim strings, N and READY labels are hard-coded. |
| `Analysis/27_build_behavior_main_figure.R`, `27_candidate_recompose_behavior_main_figure.R` | 1583, 967 | 0555c90 | Superseded by the Exp9_manuscript Figure 1 rendered from `ebb_v101`; their group tests duplicate the frozen inference. |
| `Analysis/_supporting/build_figure1_export_bundle.R`, `build_figure1_manuscript_bridge.R`, `build_figure1_panel_source_data.R` | 286, 343, 338 | 0555c90 | They cannot reproduce the bridge bytes Exp9_manuscript imported (edited since, and the inputs changed with the CombZ correction and the leading-bin fix). The manuscript's provenance is its commit pins and sha256 import manifest. |
| `Analysis/_supporting/audit_figure1_bundle_diff.R` | 149 | a1042e2 | Audits the retired bridge; its numeric guard still carries the old 49/38 split. |
| `Analysis/build_publication_release.R`, `verify_publication_release.R` | 510, 147 | c426e16 | The only release (`releases/E9_behavior_manuscript_rc1`, 2026-09-04) predates the corrections and nothing reads `releases/`. The builder had no gate. |
| `manuscript/Fig1_behavior_candidates/` (`build_fig1_candidates.R`, `README.md`, `figure_manifest.csv`) | 306, 118, 12 | 3e724a5, 07a6214 | Staged candidates for the old Figure 1 from Stage 16 tables. Its gitignored `rendered/` folder stays local. |
| `Functions/behavior_main_figure_helpers.R`, `behavior_main_figure_docs.R` | 998, 279 | 073bb17, b56b3a1 | Stage 27 only. |
| `Functions/inactive_markov_gamm_helpers.R`, `inactive_markov_stage_runner.R` | 416, 281 | 967fcb1 | Stages 23-25 only. |
| `Functions/gamm_manuscript_docs.R`, `gamm_publication_style_helpers.R` | 182, 123 | 967fcb1, b56b3a1 | Stages 26 and 27 only. |
| `Testing/tests/test_behavior_main_figure_contracts.R` | 1237 | eeb373d | Stage 27 contract; its three checks that outlive Stage 27 were lifted first (below). |
| `Testing/tests/test_behavior_main_figure_candidate_recomposition.R` | 297 | cce855e | Stage 27 candidate only. |
| `Testing/tests/test_stage16_stage03_canonical_only.R`, `test_stage16_stage09_canonical_only.R` | 37, 33 | f584306, 0f0abc9 | Stage 16 only. |
| `Testing/tests/test_release_builder_upstream_archived_sources.R`, `test_release_verifier_archived_sources.R` | 38, 47 | c426e16 | Release builder and verifier only. |
| `Testing/tests/test_inactive_markov_state.R`, `Testing/audits/test_gamm_manuscript_consistency.R` | 122, 172 | dbcf172 | Stages 23-26 only. |
| `Testing/tests/test_forbidden_segments_cover_archive.R` | 23 | 0f28060 | All four lists it checked belonged to retired scripts. |
| `Testing/audits/test_reporting_architecture.R` | 169 | d30b7a4 | Contract audit of the Stage 16 package. |

Also removed: the path keys `behavior.combz_definition` and `behavior.early_prediction_heldout` (Stage 16 package) and
`gamm.manuscript_outputs` (Stage 26), the publication-root helpers `mmm_publication_root()`, `MMM_PUBLICATION_SUBDIRS`,
`mmm_publication_dir()` and `mmm_assert_publication_path_budget()` (Stage 27), and the GAMM claim ids
`CLAIM_GAMM_01`-`_06`. The Stage 27 reason for keeping `systems_sis_domain_effect_summary.csv` byte-identical across
Stage 14 reruns retires with it; the file stays a live Stage 14 output, and its column contract
(`test_hmm_stage14_contract.R`) and the per-file sha256 in the producer-rerun records still apply.

## Outputs

On 2026-10-05 every output tree of the retired producers moved unchanged to `analysis_ready/history/retired/` with
`Maintenance/Invoke-BehaviorOutputRetirement.ps1`: one manifest and one receipt per move under
`_migration_control/retired_outputs/`, 537 files in all, each verified by SHA-256. The table of moves and the
verification are in `docs/BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md` (2026-10-05).

| Former location | Now under `history/retired/` |
|---|---|
| `pipeline/21_cc1_active_longitudinal_gamm/`, `23_first_inactive_gamm/`, `24_cc1_inactive_longitudinal_gamm/`, `25_repeated_cagechange_inactive_gamm/`, `26_gamm_manuscript_outputs/` | the same names |
| `pipeline/27_behavior_main_figure/` | `27_behavior_main_figure/` |
| `manuscript/behavior/` (the Stage 16 package) | `manuscript_behavior/` |
| the 2026-09-07 leftovers in `pipeline/20_first_night_gamm/10min/` (11 files) and `pipeline/22_repeated_cagechange_acute_gamm/10min/` (15 files) | `20_first_night_gamm_20260907/`, `22_repeated_cagechange_acute_gamm_20260907/` |

`analysis_ready/output_index.csv`, `analysis_ready/README.md` and `history/README.md` were refreshed the same day,
each after a backup.

## Checks that outlive the retired tests

- `Testing/tests/test_data_root_literals.R`: the data-root literal appears once, in `Functions/project_paths.R`; every
  other stage script takes the root from `mmm_project_root()` (exceptions: Stage 09, the cookie runner, the Stage 09
  LOAO re-renderer).
- `Testing/tests/test_path_registry_segments.R`: no path key resolves into a snapshot, quarantine, archive, release,
  `history/original_layout/` or `history/retired/` tree.
- `Testing/tests/test_first_night_domain_contract.R`, section G: the displayed first-night domains exclude the barred
  constructs (state architecture, inactive-phase rest, latent-state, dwell, occupancy entropy, invalid model labels).
- The palette pin of the Stage 27 test was already superseded by `Testing/tests/test_manuscript_palette.R`.
- `Testing/tests/test_stage_inventory.R` now refuses any legacy-product row and any script behind the retired
  `MMM_ALLOW_LEGACY_BEHAVIOR_PRODUCTS` opt-in.

## Open

- Stage 03 has lost its last product readers (16, 27 and the release builder). Its status stays "proposed: secondary"
  until decided.
- Whether Extended Data gets a within-night panel from Stages 20/22.
- Extended Data 5 and 9 and the draft text in Exp9_manuscript still quote bridge-era values; that is a manuscript-side
  change.
- `releases/E9_behavior_manuscript_rc1` on S: stays in place as a dated, pre-correction snapshot.
