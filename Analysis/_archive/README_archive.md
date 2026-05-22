# Analysis Archive

These scripts were moved out of the active pipeline to make the reviewer-facing workflow unambiguous. They are retained for provenance and backwards comparison.

| Archived file | Reason |
|---|---|
| `03b_multiscale_metric_validation_notes.md` | Development note for metric validation; useful context but not an executable pipeline stage. |
| `04_gamm_movement_proximity_phase_and_early_window.R` | Large exploratory/superseded GAMM/LME analysis layer; active trajectory feature extraction is now `07_gamm_trajectory_features.R`. |
| `08_early_prediction_models.R` | Superseded by the primary, reviewer-safe `09_early_prediction_model_ladder.R`. |
| `13_nonlinear_systems_dynamics.R` | Exploratory nonlinear dynamics layer not included in the staged active pipeline. |
| `14_nextgen_behavioral_phenotyping.R` | Exploratory next-generation phenotyping layer not included in the staged active pipeline. |
| `18_raw_movement_publication_trajectory.R` | Superseded raw movement trajectory script; broad raw movement phase statistics now use `03_primary_raw_movement_phase_stats.R`. |
| `18b_raw_movement_broad_phase_stats.R` | Superseded by corrected raw movement statistics in `03_primary_raw_movement_phase_stats.R`. |
| `legacy_deprecated/E9_SIS_AnimalPos*` | Legacy AnimalPos analysis/comparison scripts from the deprecated folder. |
| `legacy_deprecated/lme v.*` | Legacy LME scripts from the deprecated folder. |
| `legacy_deprecated/sociability_lme v.*` | Legacy sociability LME scripts from the deprecated folder. |

Archived scripts are not part of `run_all_analysis.R`. They may contain older hard-coded paths or output conventions and should be used only for provenance checks.
