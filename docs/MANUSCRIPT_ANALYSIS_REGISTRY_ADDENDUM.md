# Addendum to MANUSCRIPT_ANALYSIS_REGISTRY.csv (2026-10-05)

`docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv` stays byte-unchanged (git blob `679d97ec`). The frozen configuration
(`Functions/behavior_analysis_config.R`) and the imported `ebb_v101`/`ebb_v102` configurations (`I_analysis_config.json`) cite it
as Stage 09's registry entry, "unchanged". Editing it would contradict them, and moving it would leave them pointing at
a missing path.

Several rows describe the state before the 2026-09-20 CombZ correction, the 2026-09-22 leading-bin fix and the
2026-10-05 retirements. Read these rows through the table below. Current values are those of the corrected outcome
(2026-10-19): the frozen Stage 29 v102 bundle (`canonical/behavior_bundle/ebb_v102_20261019_677ed97/`) and the live
Stage 09 tables, which agree; the values in brackets marked v101 are those of the 2026-09-29 release.

| Registry row | What the row says | What holds now |
|---|---|---|
| `S09_ASSOC_MOVEMENT_MEAN` | bridge-era values | Spearman rho −0.443 [−0.590, −0.270], BH q 3.3e-6 (v101: −0.408, q 2.6e-5). Still FDR-supported. |
| `S09_ASSOC_MOVEMENT_RMSSD` | bridge-era values | rho −0.265 [−0.441, −0.070], q 0.0074 (v101: −0.237, q 0.018). Still FDR-supported. |
| `S09_ASSOC_ENTROPY_ACF1` | "NOT BH-supported; bootstrap CI includes zero"; only a qualified negative result may be reported | rho −0.192 [−0.364, −0.008], q 0.044 (v101: −0.188 [−0.365, −0.0001], q 0.048). It is BH-supported and the interval excludes zero, so the row's wording rule no longer matches the result. How the manuscript reports it is a separate decision. |
| `S09_PRED_MOVEMENT_MEAN` | bridge-era values | LOAO R² 0.218 (v101: 0.173); repeated-CV mean 0.214 [0.174, 0.238]; permutation p 0.001. |
| `S09_PRED_FIXED_3FEATURE` | 0.1524 vs 0.1594 | 0.211 vs 0.218 (v101: 0.161 vs 0.173). Unchanged conclusion: the three-feature model does not improve on Movement_mean alone. |
| `S09_SEX_INTERACTIONS` | no sex difference (bridge q 0.895) | Feature × Sex q 0.777 (Movement_mean), 0.595 and 0.595 (v101: 0.951, 0.749, 0.749). Unchanged conclusion. |
| `S09_SENS_5MIN_RESOLUTION` | bridge-era 5-min values | The 5-min tables were last rewritten on 2026-10-19 (corrected outcome): Movement_mean rho −0.443, LOAO R² 0.218. Its notes name Stage 16 exports, which no longer exist. |
| `S03_RAW_LONGITUDINAL_MOVEMENT` | "Canonical and exported" | Its exporters (Stages 16 and 27, the release builder) were retired on 2026-10-05; Stage 03 has no product reader and its status is open (`Analysis/STAGE_INVENTORY.csv`). |
| `FIRSTNIGHT_5DOMAIN_PANEL` | "Main Figure / Supplementary" | The conservative flat-family sensitivity beside Stage 28 (`FIRSTNIGHT_4DOMAIN_CORE`); not a main-figure panel. Its heatmaps are captioned superseded. |
| `S16_MANUSCRIPT_PACKAGE` | "Canonical; 16/16 validation PASS; 19 of 19 provenance artifacts verified" | Stage 16 was retired on 2026-10-05 and its package moved to `analysis_ready/history/retired/manuscript_behavior/`. At the last check its provenance verified 18 of 22 files: two Stage 09 5-min tables had been rewritten on 2026-09-27 and two QC files archived. The manuscript renders from the frozen bundles instead (now `ebb_v102`). |

The retirements are recorded in `docs/LEGACY_AND_GAMM_RETIREMENT_2026-10-05.md`.
