# Stage 31b-31d: historical against QC-revised results (rule cc4e_v1.0)

*Written for: Tobias and reviewers. Status: post hoc QC-sensitivity analysis, unregistered. The frozen registered runs stay
the record and are not replaced; no manuscript figure was changed.*

## 1. What was run

Tobias approved on 2026-10-15 the revised active-phase eligibility (manifest SX06 applied to every active phase A1-A5 of
B2|sys.5|CC4, whose R3 antenna was silent for the whole recording) and isolated rule-applied reruns of the full model
specifications.

| version | what it keeps | runs |
|---|---|---|
| historical | every cage episode (the frozen registered runs; also the sensitivity "retaining the affected observations") | `phase_groups_20260929_v2`, `phase_statistics_20260929_v1`, `phase_phenotypes_20260929_v1` |
| QC primary | B2\|sys.5\|CC4 active family A1-A5 excluded; its inactive family kept, flagged (eligibility `cc4e_v1.0_4aebc90`) | `p31b_cc4e10_20261015`, `p31c_cc4e10_20261015`, `p31d_cc4e10_20261015` |
| QC inactive sensitivity | as QC primary, and the B2\|sys.5\|CC4 inactive family left out (eligibility `cc4e_v1.0_4aebc90_isens`) | `p31b_cc4e10_isens_20261015`, `p31c_cc4e10_isens_20261015`, `p31d_cc4e10_isens_20261015` |

The QC runs are in `<Behavior>/RANM/cc4_qc_reruns/runs/`; the comparison is `<Behavior>/RANM/cc4_qc_comparison/runs/cqc_v1.0_defd263`
(`Testing/audits/compare_cc4_qc_reruns.R`; every run's manifest verified before it was read).

**Dependencies verified before running.** Stage 31b reads the Stage 31 bins (`whole_inactive_20260929_v1`) and builds the
per-family rosters with the eligibility file; 31c and 31d read the 31b animal table and rebuild their rosters with
`grid31_stats_data(a, eligibility)`. Every downstream step of 31c and 31d (the primary and secondary fits, the
leave-one-batch-out, dispersion and mixed-cage sensitivities, the predictive checks and the parametric bootstraps) uses
that one filtered input, so the exclusion is the same in fitting and in resampling. The bootstrap simulates all its draws
in the main process from a fixed seed and only refits them in parallel, so the worker count (8 for 31c as before; 12
instead of 24 for 31d, to limit the load on this PC) does not change a result: the Female contexts, which the rule does not
touch, are reproduced to the last digit.

**Unchanged:** hypotheses, models, parameters, outcome definitions, seeds, 4,999 draws, the bootstrap gates and the Holm
families (31c: 4 primary tests; 31d: 4 omnibus and 12 pairwise tests).

## 2. Included animals and independent cages (Male; every Female context unchanged: 58 animals, 15 cages, 3 CON)

| stage | family | historical | QC primary | QC inactive sensitivity |
|---|---|---|---|---|
| 31c | Active | 49 animals, 14 cages (3 CON) | 45, 13 (3 CON) | 45, 13 |
| 31c | Inactive | 49, 14 | 49, 14 | 45, 13 |
| 31d | Active: CON / RES / SUS | 12/3, 22/9, 15/8 | 12/3, 19/8, 14/7 | 12/3, 19/8, 14/7 |
| 31d | Inactive: CON / RES / SUS | 12/3, 22/9, 15/8 | 12/3, 22/9, 15/8 | 12/3, 19/8, 14/7 |
| 31b | Active / Inactive | as 31d | as 31d | as 31d |

No animal was removed from a roster; the B2|sys.5 animals leave only the affected family. The B2 SUS active mean of 31b now
rests on one animal (B2|sys.3|CC4). The six independent CON cages are untouched.

## 3. Stage 31c: SIS/CON ratio of rate ratios (primary)

| context | historical | QC primary | QC inactive sensitivity |
|---|---|---|---|
| Female Active | 0.942 [0.811, 1.088], P 0.518, Holm 1 | identical | identical |
| Female Inactive | 0.836 [0.487, 1.410], P 0.594, Holm 1 | identical | identical |
| Male Active | 1.012 [0.920, 1.114], P 0.839, Holm 1 | 1.013 [0.915, 1.122], P 0.840, Holm 1 | 1.013 [0.915, 1.122], P 0.840, Holm 1 |
| Male Inactive | 0.873 [0.698, 1.073], P 0.315, Holm 1 | identical | 0.893 [0.729, 1.105], P 0.391, Holm 1 |

Model-standardised rates move by less than 0.1 crossings per animal-hour (e.g. Male Active change difference 0.69 -> 0.72).
Diagnostics: every observed model valid, no boundary in the primary models (the secondary family stays withheld, as
before); every bootstrap PASS (4,999 draws used; failed refits Male Active 42/69 -> 43/56, Male Inactive in the sensitivity
106/114 -> 142/186, all below the 5% gate). Sensitivity fits: Male Active leave-one-batch-out 0.946 / 1.120 / 0.980 -> 0.945
/ 1.120 / 0.979; the Male Inactive dispersion model, invalid in the historical run, converges in the inactive sensitivity
(0.873).

## 4. Stage 31d: phenotype contrasts (CON / RES / SUS)

| context | omnibus: historical | QC primary | QC inactive sensitivity |
|---|---|---|---|
| Male Active | LR 4.01, P 0.154, Holm 0.618 | LR 3.75, P 0.176, Holm 0.703 | as QC primary |
| Male Inactive | LR 2.48, P 0.322, Holm 0.967 | identical | LR 2.72, P 0.297, Holm 0.890 |
| Female contexts | P 0.555 / 0.888, Holm 1 | identical | identical |

| Male contrast (ratio of rate ratios) | historical | QC primary | QC inactive sensitivity |
|---|---|---|---|
| Active RES-CON | 0.966 [0.876, 1.069], P 0.515 | 0.967 [0.871, 1.068], P 0.539 | as QC primary |
| Active SUS-CON | 1.072 [0.965, 1.195], P 0.217 | 1.076 [0.965, 1.199], P 0.210 | as QC primary |
| Active SUS-RES | 1.109 [1.005, 1.231], P 0.062, Holm 0.746 | 1.113 [1.002, 1.239], P 0.073, Holm 0.871 | as QC primary |
| Inactive RES-CON | 0.923 [0.710, 1.192], P 0.562 | identical | 0.968 [0.739, 1.265], P 0.826 |
| Inactive SUS-CON | 0.806 [0.612, 1.052], P 0.149 | identical | 0.804 [0.608, 1.058], P 0.145 |
| Inactive SUS-RES | 0.873 [0.675, 1.126], P 0.325 | identical | 0.830 [0.636, 1.085], P 0.205 |

Every Holm-adjusted P other than Male Active SUS-RES is 1 in all three versions. Diagnostics: all fits valid; the boundary
flag (a random-effect SD near 0) is set in every 31d model in all three versions, as before (a flag, not a gate); every
bootstrap PASS (27-64 failed refits per bootstrap, within the 5% gate).

## 5. Stage 31b: descriptive group summaries

Of 108 descriptive contrasts (pointwise t intervals across three batches, 2 df), 4 change sign in the QC primary run and 6
in the inactive sensitivity, all with intervals including 0 except one: in the inactive sensitivity, the Male RES-CON rate
difference at I5 is 1.20 [0.18, 2.22] crossings per animal-hour (historical 1.38 [-0.40, 3.16]). These 31b intervals are
pointwise and not adjusted for multiplicity; the model-based 31c and 31d results above show no corresponding change.

## 6. Interpretation

- **No change in biological interpretation.** No primary SIS/CON effect is detected in any context in any version (all
  intervals include 1, Holm P = 1); no omnibus phenotype test is significant; the one nominal contrast, Male active SUS-RES
  (about 11% higher post/reference crossing ratio in SUS than RES), keeps its estimate and an interval that just excludes 1,
  with slightly weaker evidence (bootstrap P 0.062 -> 0.073; Holm 0.746 -> 0.871), as the earlier eligibility audit
  anticipated through the shared Holm family.
- **The affected cage did not drive any result.** Leaving its active family out, or its inactive family as well, moves
  every estimate within its interval; Female results are unchanged by construction.
- **Status of the analyses.** The QC-revised runs are post hoc QC-sensitivity analyses. The registered outputs remain the
  record; the frozen runs also serve as the sensitivity analysis that retains the affected observations.

## 7. Verification

- Inputs: the eligibility runs `cc4e_v1.0_4aebc90` (primary) and `cc4e_v1.0_4aebc90_isens` (with the release CC4 knockout
  `ckr_v1.0_4aebc90`), verified against their manifests by the stages before any read.
- Each QC run lists its files in `manifest.csv` (sha256) and records its inputs in `input_manifest.csv`; the comparison
  verified all nine run folders before reading them.
- Protection: the round started with `vpo_t1_20261015_0016_28c1d6f` (first run). The end-of-round check against it
  (`vpo_t1_20261015_0308_1a55644`) shows every frozen pin, registered run, Stage 19 output and publication copy, Stage 31-31d
  run, ledger, bundle, manuscript import, recorded input and repository pin unchanged; the only differences are the new
  isolated runs and three new repository files of the approved calibration amendment (`acp_criteria_v1.1.csv`,
  `schedule_one_day_v2.csv`, `make_schedule_one_day_v2.R`), which the check reports for review by design.
