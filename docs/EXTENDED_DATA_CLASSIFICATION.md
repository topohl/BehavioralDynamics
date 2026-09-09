# Manuscript figure tiers — main, Extended Data, diagnostic

Scope: the behavior manuscript figures produced by Stages 20–27 plus the
canonical CombZ endpoint. This file assigns a **tier**, not a figure number.
Extended Data numbering is deliberately deferred to whole-manuscript assembly,
because the number depends on the other display items and on the journal.

Nothing listed here is deleted. A DIAGNOSTIC tier means "not a display item",
not "worthless" — several of these artifacts are the evidence that a main or
Extended Data claim is admissible at all.

| tier | meaning |
|---|---|
| **MAIN** | goes in the main text |
| **EXTENDED DATA** | a display item, but supporting rather than headline |
| **DIAGNOSTIC** | audit/QC evidence; cite in Methods or Supplementary text, do not render as a numbered figure |

---

## MAIN

| item | producer | claim | source data |
|---|---|---|---|
| `behavior_early_signal_and_prediction_main` (panels a–d) | `Analysis/27_build_behavior_main_figure.R` | CLAIM_BEHAV_01…05 | `source_panel_{a,b,c,d1,d2}_*.csv` |

183 × 162 mm. Panels: **a** framework + CombZ definition, **b** first-night
Movement (descriptive), **c** early Movement vs later CombZ (inferential),
**d** LOAO prediction + real permutation null (inferential).

This is the only MAIN behavior figure. Stage 26 writes a `figures/main_candidates/`
directory; despite the name, **every file in it is a candidate**, and none is
currently promoted to the main text. They are listed under EXTENDED DATA below.

---

## EXTENDED DATA

### Temporal GAMM series (Stage 26 renders; Stages 20–25 produce)

| candidate stem | producer stage | content | status |
|---|---|---|---|
| `ed1_cc1_active_trajectories_by_night` | 21 | CC1 Active trajectory across nights | READY |
| `ed2_allcc_active_trajectories` | 22 | Active trajectories, CC1–CC4 | READY |
| `ed3_active_pairwise_difference_curves` | 22 | pairwise difference curves | READY |
| `ed6_cc1_inactive_probability_by_block` | 24 | CC1 Inactive probability | READY |
| `ed7_allcc_inactive_probability` | 25 | Inactive probability, CC1–CC4 | READY |
| `ed8_inactive_transition_probabilities` | 23 | Markov transition probabilities | READY |
| `first_active_trajectory_main`, `first_active_auc_contrasts_main` | 20 | first Active response | READY; **all six group contrasts null** (min q = 0.796) |
| `first_inactive_probability_main`, `first_inactive_auc_contrasts_main` | 23 | first Inactive activation | READY; Male RES-CON q = 0.014, SUS-CON q = 0.038, SUS-RES null |
| `active_auc_by_cagechange_main`, `active_cc4_minus_cc1_main` | 22 | repeated Active adaptation | READY; overall CC4−CC1 supported in both sexes, 0/6 phenotype-dependent |

The Stage 25 repeated-Inactive series carries **no supported adaptation
contrast** (min BH q = 0.0699) and must be captioned as such.

### Alternate views of the main association (Stage 27 candidates)

| candidate stem | content | caveat that must travel with it |
|---|---|---|
| `candidate_c_movement_vs_combz_rank_space` | rank-space view; reference line slope = canonical ρ | line is drawn analytically from ρ, not fitted |
| `candidate_c_movement_vs_combz_by_sex_descriptive` | sex-facetted scatter | descriptive only; the feature-by-sex interaction is null (q = 0.895) |
| `candidate_combz_distribution_by_group` | CombZ by later outcome group | definitional, not a test: the groups are *defined* by thresholding CombZ |
| `candidate_d2_null_summary_intervals` | compact null as published quantile interval | space-constrained alternative to the real distribution in main panel d |

### Broad domain maps (descriptive, heavily caveated)

| candidate stem | content | binding caveat |
|---|---|---|
| `ed_candidate_first_night_domain_map` | first-night domain contrasts, 5×3 per sex | first night and Active phase ONLY — never label "longitudinal". 1 of 30 cells FDR-supported. |
| `ed_candidate_broad_domain_map_phase_resolved` | 7 domains × phase × sex | **all 8** FDR-supported cells are Inactive-phase, which `KNOWN_LIMITATIONS.md` item 3 forbids reading as biology; scores use `na.rm = TRUE` + coalesce-to-zero; in no registry row |
| `ed_candidate_longitudinal_movement_by_cage_change` | movement across CC1–CC4 × light phase | descriptive framework; no contrast drawn |

Neither domain map may be described as a "multi-domain signature". Both are
descriptive inventories. See `BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.md`
(`B3_NOT_JUSTIFIED__USE_FRAMEWORK_SCHEMATIC`).

---

## DIAGNOSTIC (not display items)

| artifact | why it is diagnostic-only |
|---|---|
| `ed4_stage20_vs_stage22_cc1_audit` | cross-model agreement audit for the same CC1 window; evidence, not a result |
| `ed5_active_ci_width_sensitivity` | CI-width sensitivity of the Active models |
| `ed9_inactive_markov_calibration` | calibration evidence for the Stage 23 Markov gate |
| `*/audit/gaussian_log1p_invalid/` (Stages 23, 24, 25) | **invalid inference, retained as a negative record.** Gaussian-log1p Inactive models are superseded by the Markov/binomial formulation. Never quote a number from these. |
| `*_sensitivity_no_batch.csv`, `*_sensitivity_k8.csv`, `*_sensitivity_full_shape.csv`, `*_sensitivity_raw_scale.csv` (Stages 21–25) | declared sensitivity companions to the canonical fits |
| `*_ar1_sequence_proof.csv`, `*_residual_acf.csv`, `*_residual_qq.csv`, `*_residual_vs_fitted.csv` | AR1 / residual-structure diagnostics |
| `cc1_cross_model_predictions.svg`, `cc1_cross_model_pointwise.csv` (Stage 22) | Stage 20 vs Stage 22 CC1 prediction audit |
| `figures/superseded_candidates/` (Stage 27) | the retired five-panel composition and its orphaned panel/Source Data files, moved with provenance rather than deleted |
| `pipeline/20_first_night_gamm/10min/tables/first_night_*.csv` (10 files) | **stale output of a superseded Stage 20 run**, sitting beside the current `first_active_*.csv`. Verified unread: no active script, and not Stage 26, resolves any of them. Scheduled for cleanup in a later pass. |
| `analysis_ready/_quarantine_legacy_s09/` | quarantined legacy Stage 09 tree |

---

## Rules that apply across tiers

1. **Latent/HMM state architecture** may be named as a measured domain. No
   claim about state count, stability or transitions is displayable
   (`KNOWN_LIMITATIONS.md` items 1–2).
2. **Inactive-phase** results are displayable as Extended Data with the Markov
   framing only. The Gaussian-log1p variants are invalid.
3. **Proximity** is "social-spatial / co-location organization", never
   "sociability" or "direct social interaction".
4. **CON/RES/SUS** are the *later outcome group*, assigned after the endpoint
   battery. Never a baseline or treatment group.
5. **CombZ direction**: lower CombZ = greater later stress burden.
6. Every Extended Data candidate must carry its own multiplicity family and
   `n animals`; these are already in
   `26_gamm_manuscript_outputs/10min/tables/extended_data/extended_data_gamm_full_results.csv`.
