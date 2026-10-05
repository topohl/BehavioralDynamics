# Addendum to MANUSCRIPT_ANALYSIS_REGISTRY.csv (2026-10-05)

`docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv` stays byte-unchanged (git blob `679d97ec`). The frozen configuration
(`Functions/behavior_analysis_config.R`) and the imported `ebb_v101` configuration (`I_analysis_config.json`) both cite it
as Stage 09's registry entry, "unchanged". Editing it would contradict them, and moving it would leave them pointing at
a missing path.

Several rows describe the state before the 2026-09-20 CombZ correction, the 2026-09-22 leading-bin fix and the
2026-10-05 retirements. Read these rows through the table below. Current values come from the frozen Stage 29 v1.0.1
bundle (`canonical/behavior_bundle/ebb_v101_20260929_b2ce507/`) and the live Stage 09 tables, which agree.

| Registry row | What the row says | What holds now |
|---|---|---|
| `S09_ASSOC_MOVEMENT_MEAN` | bridge-era values | Spearman rho −0.408 [−0.561, −0.226], BH q 2.6e-5 (was −0.390, q 6.9e-5). Still FDR-supported. |
| `S09_ASSOC_MOVEMENT_RMSSD` | bridge-era values | rho −0.237 [−0.417, −0.037], q 0.018 (was −0.226, q 0.026). Still FDR-supported. |
| `S09_ASSOC_ENTROPY_ACF1` | "NOT BH-supported; bootstrap CI includes zero"; only a qualified negative result may be reported | rho −0.188 [−0.365, −0.0001], q 0.048 (was −0.175 [−0.351, 0.017], q 0.067). It is now BH-supported and the interval excludes zero by 0.0001, so the row's wording rule no longer matches the result. How the manuscript reports it is a separate decision. |
| `S09_PRED_MOVEMENT_MEAN` | bridge-era values | LOAO R² 0.173 (was 0.159); repeated-CV mean 0.168 [0.125, 0.193]; permutation p 0.001. |
| `S09_PRED_FIXED_3FEATURE` | 0.1524 vs 0.1594 | 0.161 vs 0.173. Unchanged conclusion: the three-feature model does not improve on Movement_mean alone. |
| `S09_SEX_INTERACTIONS` | no sex difference (bridge q 0.895) | Feature × Sex q 0.951 (Movement_mean), 0.749 and 0.749. Unchanged conclusion. |
| `S09_SENS_5MIN_RESOLUTION` | bridge-era 5-min values | The 5-min tables were rewritten on 2026-09-27: Movement_mean rho −0.408, LOAO R² 0.173. Its notes name Stage 16 exports, which no longer exist. |
| `S03_RAW_LONGITUDINAL_MOVEMENT` | "Canonical and exported" | Its exporters (Stages 16 and 27, the release builder) were retired on 2026-10-05; Stage 03 has no product reader and its status is open (`Analysis/STAGE_INVENTORY.csv`). |
| `FIRSTNIGHT_5DOMAIN_PANEL` | "Main Figure / Supplementary" | The conservative flat-family sensitivity beside Stage 28 (`FIRSTNIGHT_4DOMAIN_CORE`); not a main-figure panel. Its heatmaps are captioned superseded. |
| `S16_MANUSCRIPT_PACKAGE` | "Canonical; 16/16 validation PASS; 19 of 19 provenance artifacts verified" | Stage 16 was retired on 2026-10-05 and its package moved to `analysis_ready/history/retired/manuscript_behavior/`. At the last check its provenance verified 18 of 22 files: two Stage 09 5-min tables had been rewritten on 2026-09-27 and two QC files archived. The manuscript renders from `ebb_v101` instead. |

The retirements are recorded in `docs/LEGACY_AND_GAMM_RETIREMENT_2026-10-05.md`.
