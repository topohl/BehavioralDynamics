# Post hoc registry v1.0: three-group (CON / RES / SUS) contrasts in the first active phase after CC1

**Status.** FROZEN 2026-09-30T15:40:15+0200, before any model was fitted. Committed alone in MMMSociability, hashed, and copied read-only to S: at analysis_ready/canonical/posthoc_con_contrasts_registry/v1.0/. The runner refuses to fit unless the frozen sha256 matches.

**Tier and decision basis**

| Field | Value |
|---|---|
| Tier | POST HOC exploratory |
| Decision basis | `USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION` (2026-09-30), made after CON/RES/SUS descriptive plots and the registered Stage 29 results had been seen |
| Relation to Stage 29 | Not part of the frozen Stage 29 families. The Stage 29 config (v1.0.1) states "RES-CON and SUS-CON are not tested". This analysis is an explicit, labelled post hoc addition and changes none of the frozen results or families. |
| Scope | Estimates, CIs and Holm-adjusted P for display in Figure 1c and the source data. No other use. |

## 1. Data (frozen, not recomputed)

- **Source.** Stage 29 bundle `ebb_v101_20260929_b2ce507`, table `A1_animal_cc1` (manifest ec59aa33…). This is the first full active phase after CC1 (18:30–06:30), all 111 RFID animals.
- **Measures (outcome y).** The two Stage 29 primary constructs:
  - `crossing_rate`: RFID position-change rate, position changes h⁻¹;
  - `shared_zone_use`: shared RFID-position occupancy, "social-spatial overlap", fraction of co-assigned dyadic time. Rows with NA are excluded as in Stage 29 (OQ770 and OQ771, both SIS males).
- **Covariates and structure.** `Group` (CON, RES, SUS; from A1), `Sex`, `Batch`, `CageEpisodeID`.
  - `conCage` = 1 if every animal in the cage epoch is CON, else 0; `sisCage` = 1 − conCage.
  - Every CON cage epoch is CON-only (verified: 1 per batch).

## 2. Model

- **Separate fits.** One fit per measure × sex: 4 fits, each within a sex.
- **Formula.** `y ~ Batch + group + (0 + conCage | CageEpisodeID) + (0 + sisCage | CageEpisodeID)`
  - `group` is a factor with levels CON (reference), RES, SUS.
  - The model allows separate cage-epoch variances for CON-only and SIS cages, as in the frozen Stage 29 EXPOSURE_CC1 model.
- **Fitting.**
  - Engine: `lme4::lmer` / `lmerTest`, REML, bobyqa, reusing the frozen Stage 29 engine (`Functions/rfid_canonical_inference.R`) where its functions accept this formula.
  - Kenward–Roger df and tests via `lmerTest::contest(..., ddf = "Kenward-Roger")`.
- **Unit and replication.** The unit is the animal, with cage-epoch random effects. CON information comes from 3 CON cages per sex (one per batch), so the CON contrasts have very few effective df (≈ 2 expected). That is reported, not remedied.

## 3. Estimands (L-vectors on the fixed effects)

- **Group means, batch-balanced (estimation only):** intercept + ⅓ × Σ(within-sex Batch dummies) + group coefficient, for CON, RES and SUS. Report the 95% KR CI.
- **Contrasts (tested):**
  - RES − CON = β_RES;
  - SUS − CON = β_SUS;
  - SUS − RES = β_SUS − β_RES.
- **Per contrast, report:** estimate, SE, KR df, 95% CI, t, two-sided p.

## 4. Multiplicity

- Holm within each measure × sex over its 3 contrasts (m = 3; 4 families).
- No global correction.
- The registered Stage 29 inference remains authoritative for RES vs SUS: follow-up FU-CC1 (RES − SUS within sex, Holm over sexes) and primary P-CC1 (sex difference in RES − SUS). The post hoc SUS − RES is shown only so that all three pairwise contrasts come from one model. Legends must also give the registered FU-CC1 result.

## 5. Diagnostics and failure rules

- **Singular fits** (e.g. a zero CON-cage variance): keep and flag, as the Stage 29 rule does.
- **Failures.** If a fit fails or does not converge, the row is FAILED and no estimate is displayed. There is no silent fallback.
- **Sensitivity** (reported, not used for display decisions): the same model with one common cage variance, `(1 | CageEpisodeID)`, with the same estimands.
- **Diagnostics recorded:** n animals per group, n CON cages and n SIS cages, variance components, KR df, singularity, convergence.

## 6. Labelling rules (binding for figures and legends)

- Call these "post hoc" in every legend or caption that shows them, and say the analysis is not preregistered.
- **Required caveats:**
  - CON = 3 cages per sex, one per batch;
  - the CON vs SIS comparisons confound social instability with regrouping itself, and with the platform assignment at CC1;
  - sex is nested in batch.
- **Forbidden wording:** "confirmatory", "preregistered", "significant", and stars.
- Display the Holm-adjusted P (m = 3) with the 95% CI.

## 7. Outputs and run rules

- **Runner.** A new MMM runner (e.g. `Analysis/29b_posthoc_con_contrasts.R` plus `Functions/posthoc_con_contrasts.R`). Do not use the number 31, which a peer session uses.
- **Gates** before fitting:
  - the registry sha256;
  - the ebb_v101 manifest;
  - the A1 sha256.
- **Run once** (REAL) to `<analysis_ready>/pipeline/29b_posthoc_con_contrasts/v1.0_<commit7>/`, with `tables/estimates.csv`, `tables/sensitivity_common_cage_variance.csv`, `tables/diagnostics.csv`, `audit/{output_manifest,input_hashes,run_manifest}.csv` and a README. Outputs are read-only.
- **Tests.** Synthetic tests only; never source the runner.
- **Downstream.** The figure-support export (a new version, `fsb_v2`) copies these rows verbatim, with provenance, for the manuscript.
