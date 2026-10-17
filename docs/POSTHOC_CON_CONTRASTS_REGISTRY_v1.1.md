# Post hoc registry v1.1: three-group (CON / RES / SUS) contrasts in the first active phase after CC1, on the corrected outcome

**Status.** DRAFT. It is frozen before the v1.1 run: committed alone in MMMSociability, hashed, and copied read-only to S: at analysis_ready/canonical/posthoc_con_contrasts_registry/v1.1/. The runner refuses to fit unless the frozen sha256 matches.

**What this version is.** A re-registration of v1.0 (FROZEN 2026-09-30T15:40:15+0200, sha256 a6435d8d..., commit 322be31; run `v1.0_7f1da1f`) on the corrected outcome (erratum E13 of `docs/BEHAVIOR_CONFIG_v1.0.0_ERRATA.md`), by user decision of 2026-10-17 to rerun every registered analysis on the corrected workbook. Version 1.0, its frozen copy and its run stay unchanged as provenance.

**Binding.** Sections 1-7 of v1.0 (`docs/POSTHOC_CON_CONTRASTS_REGISTRY_v1.0.md`) are binding for v1.1 without change: the measures, the model, the estimands, the multiplicity, the diagnostics and failure rules, the labelling rules and the run rules. Only the items below differ.

## Differences from v1.0 (exhaustive)

1. **Data source.** Stage 29 bundle `ebb_v102_<date>_<commit7>` (config v1.0.2, data version dv2, outcome version `ov2_corrected_workbook`), table `A1_animal_cc1`; the gates pin its manifest and the A1 sha256 (filled in at the freeze).
2. **Group counts** of the 111 RFID animals (population gate): Female CON 12, RES 26, SUS 20; Male CON 12, RES 25, SUS 16 (v1.0: Female 12 / 28 / 18). The count is filled in at the freeze, after the 00317 re-track.
3. **Output folder** `pipeline/29b_posthoc_con_contrasts/v1.1_<commit7>/`. Run once: an earlier `v1.1_*` or `.tmp_v1.1_*` entry blocks the run; the v1.0 folder does not.
4. **Downstream.** The figure-support export `fsb_v5` copies these rows.
