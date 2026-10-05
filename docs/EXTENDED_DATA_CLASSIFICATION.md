# Manuscript figure tiers — main, Extended Data, diagnostic

Scope (2026-10-05): the behavior manuscript is assembled in Exp9_manuscript from the frozen bundles
(`ebb_v101` from Stage 16b, the figure-support bundles from 16c, the Stage 30 bundle from 30b). This file assigns a
**tier** to the display material this repository still produces outside those bundles. It assigns no figure number;
Extended Data numbering belongs to whole-manuscript assembly.

Earlier versions covered the Stage 26 and 27 renderings (the four-panel main figure, the `ed1`-`ed9` GAMM series and
the Stage 27 candidates). Those producers were retired on 2026-10-05
(`docs/LEGACY_AND_GAMM_RETIREMENT_2026-10-05.md`); their outputs are under `analysis_ready/history/retired/` and are
not display items.

Nothing listed here is deleted. A DIAGNOSTIC tier means "not a display item", not "worthless": several of these
artifacts are the evidence that a candidate is admissible at all.

| tier | meaning |
|---|---|
| **MAIN** | goes in the main text |
| **EXTENDED DATA** | a display item, but supporting rather than headline |
| **DIAGNOSTIC** | audit/QC evidence; cite in Methods or Supplementary text, do not render as a numbered figure |

---

## MAIN

None from this repository. Figure 1 is rendered in Exp9_manuscript from `ebb_v101`.

---

## EXTENDED DATA candidates

### Within-night GAMM profiles (Stages 20 and 22; descriptive, no group inference)

| output | producer | content | binding caveat |
|---|---|---|---|
| `pipeline/20_first_night_gamm/10min/tables/first_active_*` | 20 | within-night 10-min profile of the first active phase after CC1, by sex and group | display trajectories only; all six group contrasts are null (min q 0.730) and have no cage term (CON is 3 intact cages per sex); quote no GAMM p-value |
| `pipeline/22_repeated_cagechange_acute_gamm/10min/tables/allcc_active_*` | 22 | within-night profile of the active phase after each of CC1-CC4; where in the night the CC1-to-CC4 change happens | display only; the CC4−CC1 p-values ignore cohort variation and CON shares the change, so it is not adaptation (Stage 32 registry section 9); the phenotype-dependent change is null (q 0.877-0.892) |

Export them, if wanted, through a new figure-support bundle version written by Stage 16c, with CON shown as reference
only.

### Domain maps (Stage 14; descriptive, heavily caveated)

| output | content | binding caveat |
|---|---|---|
| first-night domain contrasts (`analyses/first_night_five_domain_characterization/10min/`) | first-night domain contrasts, 5×3 per sex | first night and Active phase ONLY — never label "longitudinal". **0 of 30** cells FDR-supported (min q = 0.0893); the conservative flat-family sensitivity beside Stage 28 |
| broad phase-resolved domain map (`analyses/systems_dashboard/5min/stats_tables/systems_sis_domain_effect_summary.csv`) | 7 domains × phase × sex | **all 8** FDR-supported cells are Inactive-phase, which `KNOWN_LIMITATIONS.md` item 3 forbids reading as biology; scores use `na.rm = TRUE` + coalesce-to-zero; in no registry row |

Neither domain map may be described as a "multi-domain signature". Both are descriptive inventories.

---

## DIAGNOSTIC (not display items)

| artifact | why it is diagnostic-only |
|---|---|
| `*_sensitivity_no_batch.csv`, `*_sensitivity_k8.csv`, `*_sensitivity_full_shape.csv`, `*_sensitivity_raw_scale.csv` (Stages 20 and 22) | declared sensitivity companions to the fits |
| `*_ar1_sequence_proof.csv`, `*_residual_acf.csv`, `*_residual_qq.csv`, `*_residual_vs_fitted.csv` | AR1 / residual-structure diagnostics |
| `cc1_cross_model_predictions.svg`, `cc1_cross_model_pointwise.csv` (Stage 22) | Stage 20 vs Stage 22 CC1 prediction audit |
| `history/retired/23_first_inactive_gamm/`, `24_*`, `25_*`: `audit/gaussian_log1p_invalid/` | **invalid inference, retained as a negative record.** Never quote a number from these. |
| `history/retired/20_first_night_gamm_20260907/`, `22_repeated_cagechange_acute_gamm_20260907/` | stale 2026-09-07 output of superseded Stage 20/22 runs (old classification), moved out of the live folders |
| `history/original_layout/_quarantine_legacy_s09/` | quarantined legacy Stage 09 tree |

---

## Rules that apply across tiers

1. **Latent/HMM state architecture** may be named as a measured domain. No
   claim about state count, stability or transitions is displayable
   (`KNOWN_LIMITATIONS.md` items 1–2).
2. **Inactive-phase GAMM and Markov results** (the retired Stages 23-25) are not
   displayable; the frozen configuration excludes them from every product.
   Light-phase results come from the frozen Stages 29, 30 and 32.
3. **Proximity** is "social-spatial / co-location organization", never
   "sociability" or "direct social interaction".
4. **CON/RES/SUS** are the *later outcome group*, assigned after the endpoint
   battery. Never a baseline or treatment group.
5. **CombZ direction**: lower CombZ = greater later stress burden.
6. Every inferential Extended Data item carries its own multiplicity family and
   `n animals`. A display-only candidate states `n animals` and that it shows no
   test.
