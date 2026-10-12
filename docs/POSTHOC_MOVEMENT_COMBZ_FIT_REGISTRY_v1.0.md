# Post hoc registry v1.0: per-sex least-squares display line of CombZ on early movement

**Status.** FROZEN 2026-10-12T11:30:00+0200, before this runner fitted any line. Committed alone in MMMSociability, hashed, and copied read-only to S: at analysis_ready/canonical/posthoc_movement_combz_fit_registry/v1.0/. The runner refuses to fit unless the frozen sha256 matches.

**Tier and decision basis**

| Field | Value |
|---|---|
| Tier | POST HOC descriptive display |
| Decision basis | `USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION` (2026-10-12). Requested for manuscript Figure 1d after the author's manual Figure 1 draft, which showed per-sex scatter plots with a least-squares line and band. Stage 09's own figure (`primary_movement_entropyacf1_vs_combz`) draws the same line at render time. The line was therefore seen before this registry was written. |
| Relation to registered results | Adds no test and changes no registered family or result. The statistic shown with Figure 1d is Stage 09's descriptive sex-stratified Spearman ρ with its BH q within sex (`pipeline/09_early_prediction`, table `primary_movement_entropyacf1_correlations_by_sex.csv`, rows `Movement_mean`). |
| Scope | The fitted line and its 95% confidence band, for display in Figure 1d and its source data. No other use. |

## 1. Data (frozen, not recomputed)

- **Source.** Stage 29 bundle `ebb_v101_20260929_b2ce507`, table `A2_prediction_animals` (sha256 5b8ee3fb…, listed in manifest ec59aa33…). One row per RFID-tracked animal: 111 animals, 58 female and 53 male.
- **Outcome y.** `observed_CombZ`, the later composite outcome (displayed as Integrated Resilience).
- **Predictor x.** `Movement_mean`, the predictor of the Stage 29 held-out movement-mean model. Its values are those of Stage 09's `Movement_mean`.
- **Rows.** Per sex, every animal with finite x and y. Expected: 58 female, 53 male, none excluded. CON, RES and SUS are pooled within sex, as in Stage 09's sex-stratified correlation.

## 2. Model

- **Fits.** One ordinary least-squares fit per sex: `observed_CombZ ~ Movement_mean` (`stats::lm`). Two fits.
- **Display line.** Fitted values on a grid of 101 equally spaced x values from that sex's minimum to its maximum `Movement_mean`. No extrapolation.
- **Band.** Pointwise 95% confidence interval of the mean: fit ± t(0.975, n − 2) × se.fit, from `stats::predict(..., se.fit = TRUE)`. This is the band that `ggplot2::geom_smooth(method = "lm", level = 0.95)` draws.

## 3. Outputs per fit

- `tables/fit_lines.csv`: Sex, grid_index (1–101), x, fit, se_fit, df_residual, t_crit, ci_low, ci_high, level, n.
- `tables/coefficients.csv`: Sex, term (intercept, slope), estimate, se, df_residual, n, residual_sd, r_squared. No t statistic and no P: this registry tests nothing.
- `tables/diagnostics.csv`: Sex, n, n_excluded_nonfinite, x_min, x_max, rank, status, failure_reason.

## 4. Multiplicity

- None, because nothing is tested.
- The q printed in Figure 1d comes from Stage 09 (BH within sex over its 3 features).

## 5. Failure rules

- A fit that errors, is rank-deficient (rank < 2) or has fewer than 10 animals is FAILED. It gets no line and no band, and there is no fallback.

## 6. Labelling rules (binding for figures and legends)

- Call the line a "least-squares line with 95% confidence band (post hoc, display only)" in every legend or caption that shows it. Say that it is not preregistered and adds no test.
- Report Stage 09's Spearman ρ and BH q as the statistic for the panel.
- **Forbidden:** the words "confirmatory", "preregistered" and "significant", stars, and any slope P.

## 7. Outputs and run rules

- **Runner.** `Analysis/29c_posthoc_movement_combz_fit.R` plus `Functions/posthoc_movement_combz_fit.R`.
- **Gates** before fitting:
  - the registry sha256 (S: copy, S: `REGISTRY_SHA256.txt`, repository copy);
  - code committed and clean, and the committed runner is the one running;
  - the ebb_v101 manifest, and the A2 sha256 = pinned = manifest entry;
  - the population: 111 unique animals, 58 female and 53 male, finite x and y.
- **Run once** (REAL), with the opt-in `MMM_POSTHOC29C_REAL_RUN` = HEAD commit7. Output goes to `<analysis_ready>/pipeline/29c_posthoc_movement_combz_fit/v1.0_<commit7>/`:
  - `tables/{fit_lines,coefficients,diagnostics}.csv`;
  - `audit/{output_manifest,input_hashes,run_manifest,gate_results}.csv`;
  - `README.txt`.

  Outputs are read-only. There is no dry run on project data.
- **Tests.** Synthetic only (`Testing/tests/test_posthoc_movement_combz_fit.R`); never source the runner.
- **Downstream.** The figure-support export `fsb_v4` copies these rows verbatim, with provenance, for the manuscript.
