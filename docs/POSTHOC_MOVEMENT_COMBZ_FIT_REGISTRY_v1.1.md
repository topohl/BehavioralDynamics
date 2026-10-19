# Post hoc registry v1.1: per-sex least-squares display line of CombZ on early movement, on the corrected outcome

**Status.** DRAFT. It is frozen before the v1.1 run: committed alone in MMMSociability, hashed, and copied read-only to S: at analysis_ready/canonical/posthoc_movement_combz_fit_registry/v1.1/. The runner refuses to fit unless the frozen sha256 matches.

**What this version is.** A re-registration of v1.0 (FROZEN 2026-10-12T11:30:00+0200, sha256 a617bb5b..., commit 2be501d; run `v1.0_fd61cd9`) on the corrected outcome (erratum E13 of `docs/BEHAVIOR_CONFIG_v1.0.0_ERRATA.md`), by user decision of 2026-10-17 to rerun every registered analysis on the corrected workbook. Version 1.0, its frozen copy and its run stay unchanged as provenance.

**Binding.** Sections 1-7 of v1.0 (`docs/POSTHOC_MOVEMENT_COMBZ_FIT_REGISTRY_v1.0.md`) are binding for v1.1 without change: the data roles, the model, the outputs, the absence of multiplicity, the failure rules, the labelling rules and the run rules. Only the items below differ.

## Differences from v1.0 (exhaustive)

1. **Data source.** Stage 29 bundle `ebb_v102_<date>_<commit7>` (config v1.0.2, outcome version `ov2_corrected_workbook`), table `A2_prediction_animals`; the gates pin its manifest and the A2 sha256 (filled in at the freeze). The population is unchanged: 111 animals, 58 female and 53 male.
2. **Output folder** `pipeline/29c_posthoc_movement_combz_fit/v1.1_<commit7>/`. Run once: an earlier `v1.1_*` or `.tmp_v1.1_*` entry blocks the run; the v1.0 folder does not.
3. **Downstream.** The figure-support export `fsb_v5` copies these rows.
