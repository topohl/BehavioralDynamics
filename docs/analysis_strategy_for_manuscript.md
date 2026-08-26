# BehavioralDynamics Manuscript Analysis Strategy

## Purpose of This Document

This document defines the manuscript-facing scientific logic of the active BehavioralDynamics analysis pipeline.

The goal is to:

- distinguish primary vs secondary analyses
- align behavioral statistics with biologically defensible interpretation
- prevent analytical overreach
- standardize manuscript language across figures and scripts
- define which outputs support central claims versus exploratory extensions

This document should be interpreted together with:

```text
Analysis/README_pipeline.md
README.md
Analysis/run_all_analysis.R
```

---

# Central Biological Hypothesis

Early behavioral adaptation after the first social instability exposure predicts later stress burden and aligns with hippocampal molecular adaptation programs.

The central claim is not that resilience is simply “more” or “less” movement.

The working biological model is that resilience reflects organized adaptation after social perturbation:

- controlled locomotor output
- altered temporal structure
- preserved or restored active/inactive organization
- social reorganization dynamics
- stabilization or recovery trajectories
- coordinated systems-level behavioral organization

The manuscript should therefore emphasize behavioral organization and adaptation rather than isolated endpoint means.

---

# Conceptual Framework

The repository is built around the idea that homecage behavior contains multiple partially independent information layers:

| Layer | Examples |
|---|---|
| Magnitude | movement means, path length, occupancy |
| Temporal dynamics | RMSSD, instability, autocorrelation-like structure |
| Circadian/phase organization | active vs inactive organization |
| Social organization | proximity, dyadic contact structure |
| State organization | HMM states, state-space occupancy |
| Recovery/adaptation | cage-change adaptation kinetics |
| Systems integration | behavior-proteomics alignment |

The manuscript should progressively move from conservative/high-confidence findings toward more mechanistic and exploratory systems analyses.

---

# Core Manuscript Domains

The core manuscript domains are:

1. Early behavioral adaptation
2. Temporal instability and behavioral organization
3. Ethological active/inactive phase organization
4. Social reorganization after regrouping
5. Behavioral-proteomic systems alignment

The strongest manuscript claims should remain centered on these domains.

---

# Primary Prediction Window

The primary prospective behavioral window is:

| Variable | Primary definition |
|---|---|
| Cage change | First cage change |
| Phase | First active phase |
| Duration | First 12 h |
| Resolution | 10-min bins for the primary Stage 09 prediction analysis |

Five-minute binning is a predefined resolution sensitivity for the early-prediction question, not the canonical primary prediction resolution. Stage 14 may retain its own 5-min integration backbone; this does not redefine the Stage 09 primary analysis.

Primary endpoint:

```text
CombZ
```

or an equivalent later composite stress-burden metric.

---

# Primary Feature Family

The primary feature family intentionally separates behavioral magnitude, short-timescale volatility, and temporal persistence:

| Feature | Intended interpretation |
|---|---|
| `Movement_mean` | Locomotor magnitude |
| `Movement_rmssd` | Short-timescale volatility/adaptation instability |
| `Entropy_acf1` | Persistence of spatial organization |

The manuscript should emphasize that these features represent complementary organizational domains rather than redundant movement metrics.

---

# Primary Prediction Strategy

The active primary prediction script is:

```text
Analysis/09_early_prediction_model_ladder.R
```

The manuscript emphasis should remain:

```text
early behavior -> later stress burden
```

rather than:

```text
group labels -> stress phenotype
```

because RES/SUS groups are partially derived from downstream stress-burden measures.

Sex-adjusted models are sensitivity analyses. Models containing RES/SUS `Group` are supplementary/contextual because the grouping is derived from downstream phenotype structure related to CombZ; they must not enter the clean primary prospective summary.

The fixed a priori manuscript-facing behavior-only registry is:

1. Mean-only intercept baseline
2. `Movement_mean`
3. `Movement_mean + Movement_rmssd + Entropy_acf1`

The corresponding Sex-adjusted sensitivity models add `Sex` to models 2 and 3. Models are not selected or labelled as best according to observed cross-validation performance.

---

# Recommended Primary Reporting

Primary reporting should include:

- the fixed a priori behavior-only model registry and its Sex-adjusted sensitivities
- leave-one-animal-out cross-validation
- repeated grouped 5-fold CV as a resampling-robustness companion
- outcome-permutation testing with full refitting for the two non-null primary behavior-only models
- bootstrap confidence intervals
- duration sensitivity analyses

Quantiles across repeated CV splits are resampling-distribution quantiles, not ordinary confidence intervals. Legacy prediction-vector permutations may be retained for compatibility but must be labelled as post-hoc prediction-vector association permutations rather than full-pipeline tests. Larger behavior-only, Sex-adjusted, and Group-adjusted ladders are supplementary compatibility outputs.

Cross-validation results should be emphasized more strongly than single-fit in-sample performance.

---

# Secondary Stage 03 Characterization

`Analysis/03_primary_raw_movement_phase_stats.R` provides secondary phenotype/group characterization; it is not the primary prospective analysis. Its displayed CON/RES/SUS pairwise comparisons use Holm adjustment within each prespecified three-contrast panel. When `export_global_family_corrections = FALSE`, no wider global family-wise correction is exported or claimed. The wider cage-change/phase scan is secondary/descriptive, and p < 0.10 dagger or trend annotations are not used in publication-facing figures.

---

# Manuscript Report Export Layer

`Analysis/16_manuscript_behavior_report.R` is an export/assembly layer only. It reads canonical Stage 09 results plus selected Stage 03 and QC outputs without refitting models or recalculating statistics. The manuscript entry point is `analysis_ready/manuscript/behavior/Behavioral_Source_Data.xlsx`, accompanied by typed result/source-data CSVs, provenance, validation, and a SHA-256 manifest. Its primary registry is limited to the three canonical Stage 09 feature associations, fixed prospective prediction models and Sex-adjusted sensitivities, and formal feature-by-Sex interaction tests. Stage 10/14 predictive claims, HMM/state models, manifold and nonlinear analyses, high-dimensional systems prediction, systems composites, and behavior-proteomics integration remain exploratory unless explicitly enabled in a later reporting decision.

The workbook retains Group only as descriptive animal metadata and for descriptive plotting. Group remains excluded from primary prospective prediction. Individual-level source sheets provide one row per animal for the primary feature/outcome data, held-out LOAO prediction rows for the headline and plotted canonical models, and tidy Stage 03 movement-phase observations underlying manuscript-facing panels.

Repeated grouped-CV intervals in this report are labelled 2.5-97.5% resampling quantiles, never confidence intervals. Full-refit empirical permutation results retain the exact numerator and denominator (for example, 1/1001) and are not displayed as P < 0.001. Internal cross-validation is not external validation.

---

# Duration and CC4 Robustness

Cage change 4 has shorter observation duration than earlier cage changes.

The goal is not to remove CC4 biologically, but to prevent observation-duration artifacts from being interpreted as behavioral phenotypes.

All duration-sensitive analyses should therefore report:

1. full-data analyses
2. excluding-short-duration sensitivity analyses

Main-text claims should only be considered stable when:

```text
- effect direction is unchanged
- abs(delta_cohen_d) < 0.30
```

Metrics that should not be directly compared across unequal duration without normalization include:

- raw counts
- cumulative AUCs
- path lengths
- transition counts

Preferred normalization:

- per-hour
- per-epoch
- per-observation-window

---

# Phase Organization Interpretation

Mice are nocturnal, therefore active/inactive organization is biologically meaningful.

However, RFID homecage metrics alone do not independently validate:

- circadian disruption
- sleep disruption

unless external physiological validation exists.

Preferred language:

- active/inactive organization
- day/night behavioral structure
- phase-specific adaptation
- dark-phase adaptation
- light-phase rest-like organization

Avoid:

- circadian disruption
- sleep disruption
- insomnia-like behavior

unless independently validated.

---

# Social Organization Interpretation

Social analyses should distinguish between:

| Domain | Interpretation |
|---|---|
| Proximity/co-occupancy | Social engagement proxy |
| True dyadic identity | Stronger social network inference |
| Partner instability | Social reorganization |
| Preferred-partner persistence | Social stability |
| Social fragmentation | Disrupted social structure |
| Phase-specific social organization | Circadian social adaptation |

If dyadic identity is unavailable or uncertain, avoid strong graph-theory language.

Preferred phrasing:

- social engagement proxy
- co-occupancy dynamics
- proximity-derived social structure

rather than:

- definitive social interaction network
- affiliative bond mapping

unless identity-resolved interaction data are available.

---

# HMM and State-Space Analyses

The active optional HMM script is:

```text
Analysis/08_hmm_behavioral_states_optional.R
```

HMM, manifold, recurrence, attractor, energy-landscape, and advanced nonlinear analyses should currently be treated as:

```text
secondary or exploratory mechanistic decomposition
```

unless directly supporting one of the primary manuscript domains.

The manuscript should avoid making the entire paper dependent on high-complexity latent-state interpretation.

Preferred role:

- mechanistic decomposition
- organizational interpretation
- secondary support
- systems-level context

Avoid:

- making HMM states the primary biological claim
- overinterpreting latent-state identity
- anthropomorphic state labels

---

# Inactivity / Sleep-Like Metrics

The active inactivity script is:

```text
Analysis/12_sleep_like_quiescence_metrics.R
```

These metrics estimate:

- sleep-like inactivity
- rest-like inactivity
- quiescence fragmentation
- inactivity continuity

They do NOT independently validate EEG-defined sleep.

Avoid:

- REM/NREM terminology
- sleep architecture claims
- definitive sleep disruption claims

unless independently validated.

---

# Proteomics Integration Logic

The active integration script is:

```text
Analysis/15_behavior_proteomics_integration.R
```

The manuscript should avoid unrestricted feature-explosion correlation mining as the primary story.

Preferred logic:

```text
low-dimensional behavioral adaptation axes
        ↕
curated hippocampal molecular adaptation modules
```

Preferred behavioral axes:

- locomotor/adaptation axis
- temporal organization axis
- phase organization axis
- social organization axis
- inactivity/quiescence axis

Preferred proteomic axes:

- RNA/RNP/splicing
- translation/ribosome
- mitochondrial/OXPHOS
- proteostasis/endolysosomal
- synaptic/plasticity

Small-n behavior-proteomics findings should be framed as:

- exploratory
- effect-size focused
- hypothesis-generating

rather than definitive mechanistic proof.

---

# Evidence Tier Framework

## Tier 1 — Primary manuscript evidence

Highest-confidence findings:

- first active phase after first cage change
- early adaptation prediction
- conservative cross-validated models
- phase-aware behavioral organization
- robust movement/organization relationships

These findings should drive:

- title
- abstract
- main figures
- discussion framing

---

## Tier 2 — Mechanistic decomposition

Includes:

- instability
- state switching
- phase organization
- social organization
- adaptation kinetics
- inactivity structure
- GAMM-derived trajectories

These analyses explain:

```text
how behavioral organization differs
```

rather than only:

```text
whether groups differ
```

---

## Tier 3 — Exploratory nonlinear systems analyses

Includes:

- recurrence maps
- attractor landscapes
- manifold embeddings
- energy landscapes
- high-dimensional latent-state optimization

These analyses should currently remain exploratory and hypothesis-generating.

---

# Statistical Philosophy

The manuscript should prioritize:

- effect sizes
- cross-validated prediction
- robustness analyses
- trajectory structure
- biologically interpretable organization

rather than maximizing the number of nominal p-values.

Recommended emphasis hierarchy:

1. Cross-validated predictive performance
2. Effect-size stability
3. Directional consistency
4. Biological interpretability
5. Nominal significance

---

# Recommended Figure Logic

## Main Figures

Main figures should prioritize:

- behavioral trajectories
- early prediction logic
- adaptation dynamics
- systems-level organization
- interpretable effect sizes

Avoid overcrowding main figures with:

- very high-dimensional embeddings
- excessive supplementary metrics
- unstable exploratory latent-state outputs

---

## Supplementary Figures

Supplementary space is appropriate for:

- sensitivity analyses
- robustness checks
- duration normalization
- alternate binning resolutions
- exploratory manifold outputs
- alternate model specifications
- sex-stratified sensitivity analyses

---

# Recommended Active Run Order

The recommended active manuscript-facing order is:

1. `Analysis/00_qc_tracking_integrity.R`
2. `Analysis/01_build_multiscale_behavior_metrics.R`
3. `Analysis/02_build_dyadic_rfid_contacts.R`
4. `Analysis/03_primary_raw_movement_phase_stats.R`
5. `Analysis/04_temporal_instability.R`
6. `Analysis/05_behavioral_state_space.R`
7. `Analysis/06_dynamic_social_networks.R`
8. `Analysis/07_gamm_trajectory_features.R`
9. `Analysis/09_early_prediction_model_ladder.R`
10. `Analysis/10_systems_feature_prediction_ladder.R`
11. `Analysis/11_behavioral_adaptation_kinetics.R`
12. `Analysis/12_sleep_like_quiescence_metrics.R`
13. `Analysis/13_ethological_phase_organization.R`
14. `Analysis/14_systems_neuroscience_summary_dashboard.R`
15. `Analysis/15_behavior_proteomics_integration.R`
16. `Analysis/16_manuscript_behavior_report.R` (export/assembly after canonical upstream outputs exist)

Optional analyses:

- `Analysis/08_hmm_behavioral_states_optional.R`

Archived or legacy scripts in `Analysis/_archive/` should not be treated as the primary manuscript workflow unless explicitly required for historical provenance.

---

# Manuscript Framing Summary

The strongest framing is:

```text
Stress susceptibility and resilience emerge through differences in behavioral organization and adaptation dynamics following social perturbation.
```

not simply:

```text
stress changes movement magnitude.
```

The repository and manuscript should therefore consistently emphasize:

- adaptation
- organization
- dynamics
- multiscale structure
- systems-level integration

rather than isolated endpoint means alone.
