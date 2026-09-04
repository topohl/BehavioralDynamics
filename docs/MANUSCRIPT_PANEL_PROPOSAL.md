# Proposed manuscript panel architecture

A **proposal**, not a decision. It is derived from conceptual necessity and
robustness as recorded in `docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv`, not from
counting significant results. Nothing here was chosen because it had a small
p-value, and two of the excluded items are excluded *despite* being nominally
significant.

The selection principle used: **each main panel must answer a question the
previous panel raises, and no panel may depend on a measurement whose validity is
unresolved.**

---

## Recommended MAIN figure: five panels

One coherent narrative — *define the outcome, show the first response, show that
early behaviour predicts the outcome prospectively, validate that prediction out
of sample, then characterise the phenotype longitudinally.*

| Panel | Content | Registry row | Why it is necessary |
|---|---|---|---|
| **A** | Outcome definition: later `CombZ` with CON/RES/SUS shown descriptively | `S16_MANUSCRIPT_PACKAGE` (`animal_level_source_data.csv`) | Without it the reader cannot know what is being predicted, and cannot see that RES/SUS are outcome-derived rather than assigned. Honesty requirement, not a result. |
| **B** | First perturbation response: first active 12 h movement trajectory after CC1 | Stage 14 first-active trajectory | Establishes that the window used for prediction is a real behavioural event, not an arbitrary slice. |
| **C** | Prospective feature associations: the three fixed a priori features vs `CombZ` | `S09_ASSOC_MOVEMENT_MEAN`, `S09_ASSOC_MOVEMENT_RMSSD`, `S09_ASSOC_ENTROPY_ACF1` | The primary claim in its simplest form. Must show all three features including the one that is **not** FDR-supported, otherwise the fixed a priori family is misrepresented. |
| **D** | Prospective prediction: fixed model registry, cross-validated | `S09_PRED_MOVEMENT_MEAN`, `S09_PRED_FIXED_3FEATURE` | Converts association into out-of-sample prediction with a permutation reference. This is what makes the claim prospective rather than correlational. |
| **E** | Longitudinal phenotype: raw movement by cage change × phase | `S03_RAW_LONGITUDINAL_MOVEMENT` | Places the early window in the context of the whole experiment and shows the conservative, model-light characterization. |

Panels D and E could be merged if the journal demands four panels; **A–D are the
irreducible core**. Dropping A would hide the outcome-derived nature of the group
labels, and dropping C or D would leave either the association or the validation
unsupported.

### Explicitly recommended against for the main figure

Adding the first-night five-domain heatmap as a sixth main panel. It is a
legitimate secondary result but it yields **1 FDR-supported cell of 30**, and
placing it in the main figure would invite it to be read as a multi-domain
signature.

---

## Recommended SUPPLEMENT

| Item | Registry row | Role |
|---|---|---|
| **S1** Stage 09 5-min resolution sensitivity | `S09_SENS_5MIN_RESOLUTION` | Shows the headline is not a bin-width artifact (`Movement_mean` LOAO R² 0.1594 → 0.1600). Must carry the lag-1-bin caveat. |
| **S2** First-night five-domain panel | `FIRSTNIGHT_5DOMAIN_PANEL` | The multiscale characterization, reported with its single surviving cell stated as such. |
| **S3** Feature-by-Sex interaction tests | `S09_SEX_INTERACTIONS` | Pre-empts the reviewer question; reports no evidence of sex differences, with the power caveat. |
| **S4** Stage 03 full statistical tables | `S03_RAW_LONGITUDINAL_MOVEMENT` | The 214 supplementary rows behind main panel E. |
| **S5** Window and QC contracts | window contracts, `qc/` | Coverage, completeness, tracking integrity. Lets a reviewer check the window was not fitted. |
| **S6** *Conditional:* active longitudinal HMM persistence | `HMM_ACTIVE_LONGITUDINAL_PERSISTENCE` | Include **only** if the identifiability caveat is stated in the legend, and **only** as one finding, not four. |

**S6 is genuinely optional.** The manuscript is complete without it. Include it
only if a reviewer asks for latent-state evidence, because it costs a paragraph
of caveats (logLik spans 69,078–414,114 across seeds) for one directional result.

---

## Explicitly EXCLUDED as claims

| Excluded | Why — and note that significance is *not* the reason |
|---|---|
| First-night HMM persistence | Animal-level dwell correlates at **ρ = 0.015** between the shipped fit and a refit. Not reproducible within the same data. |
| Occupancy-entropy phenotype | Active `occupancy_entropy` SUS–RES **flips sign** across gap-aware optima; the audit verdict is literally "do not report as a finding". |
| Inactive / rest HMM interpretation | Statistically robust across all five optima, and excluded anyway: inactive read density is not separable from genuine rest, so the contrasts cannot be attributed to biology. **This is the clearest case of excluding a significant result on measurement-validity grounds.** |
| Stage 15 behaviour–proteomics | n = 0–9 per model; 0 of 40 reach FDR < 0.05; the producing script marks every model `StableForMainText = FALSE`. |
| Stage 10 systems-extension ladder, nonlinear and manifold analyses | Exploratory; also depend on output trees that `run_all_analysis.R` does not regenerate (see `docs/ARCHIVE_DEPENDENCY_AUDIT.csv`). |
| Any 5-min `Entropy_acf1` promotion | It *is* BH-supported at 5 min (q = 0.0012) but **not** at the primary 10-min resolution (q = 0.0667). Because it is a lag-1-bin statistic the 5-min version is a different quantity, so using it to promote the primary would be resolution shopping. |

---

## What still requires your decision

1. Whether main panels D and E are separate or merged.
2. Whether S6 (active HMM persistence) is included at all.
3. Whether panel B uses the Stage 14 trajectory as staged or a redrawn version.

None of these is answerable from the data; all three are editorial.
