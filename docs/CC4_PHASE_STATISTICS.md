# Stage 31c: CC4 whole-phase count analysis

`Analysis/31c_cc4_phase_statistics.R` consumes the SHA256-verified Stage31b
animal-phase table. It does not read or alter raw/preprocessed recordings and
does not use activity-inferred two-hour windows. CON also received grid sessions.
These are associations with protocol period and stress history, not causal grid
effects. The plan was developed after descriptive inspection, not preregistered.

Each sex is analysed separately with its three batches. Primary inactive
comparison: I2 versus pooled I3-I5; active: A2 versus pooled A3-A5. A1 is context,
not a primary inclusion criterion. Eligibility is recomputed per phase family;
one incomplete animal excludes its whole cage from that family's primary cohort.
Counts are summed, with 12-hour and 36-hour animal-exposure offsets. Leading-zero
IDs and fixed cage rosters are preserved. Missing recording never becomes zero.

Primary NB2 model: `Count ~ Condition*Period + Batch*Period +
offset(log(Hours)) + (1|CageID)`. Cage is the observed unit and Condition is
CON/SIS. The effect is `(SIS post/reference)/(CON post/reference)`.
Model-standardized absolute rates average batches equally and integrate over
the fitted normal cage-intercept distribution; they differ from descriptive
equal-cage means. Pointwise bootstrap intervals are not simultaneous intervals.

Numerical acceptance requires convergence code0, positive Hessian, finite
likelihood/coefficients, and maximum absolute gradient <=0.01. Invalid default
fits receive a BFGS retry. Any observed random-effect SD<0.0001 is flagged as a
boundary estimate and gates inference. This operational boundary threshold does
not prove absence of biological cage effects. A boundary simulation refit is
accepted only when the numerical checks pass; counts are explicitly reported.

For each of four primary contrasts, simulate under the no-interaction null,
draw fresh random effects, and refit full/null models. Use the likelihood-ratio
statistic and `(1 + exceedances)/(1 + successful draws)`. Full-model simulation
and refitting supplies percentile intervals. Default4999 successful replicates
per procedure; maximum attempts ceiling(draws/0.94). At200-draw checkpoints,
more than5% numerical failures withholds inference, including after optimizer
retry. Preserve every attempted result. Apply Holm with family size4, including
when some contexts are unavailable. Export Monte Carlo standard error.

Secondary CON/RES/SUS model retains animal intercept, cage intercept and an
independent cage post-period deviation. Labels are observational phenotype
associations. The 12 secondary tests are held as a family when any observed
covariance fit is invalid or on the boundary; do not drop shared response terms
or substitute Wald p-values. The current implementation exports diagnostic
point estimates only; a future validated secondary bootstrap is required even
if a different dataset passes all four covariance checks. Mixed-cage SUS/RES
fits are explicitly diagnostic sensitivity estimates, not tested discoveries.

Other sensitivities: omit each batch; allow NB2 dispersion to depend on condition
and period. Export observable predictive checks by condition/period/batch, plus
paired cage rate correlation and extremes, against999 unconditional simulations.
These checks are descriptive predictive envelopes, not a multiple-testing family.
They do not constitute proof of model adequacy. No inference about a sex
interaction follows from separate sex-specific results.

Run from the repository in PowerShell:

```powershell
Rscript --vanilla 'Testing/tests/test_cc4_phase_statistics.R'
$env:MMM_TEST_CC4_MODELS='1'
Rscript --vanilla 'Testing/tests/test_cc4_phase_statistics.R'
Remove-Item Env:MMM_TEST_CC4_MODELS
Rscript --vanilla 'Analysis/31c_cc4_phase_statistics.R' '<Stage31b run>' '<new output directory>' 4999 8
```

The opt-in integration test uses synthetic data, two bootstrap draws, two
workers and validation-only mode, which cannot emit inferential p-values or
intervals. Portable tests do not require the fitting package. Production uses
installed glmmTMB; no additional package installation is performed. Outputs
include inputs, frozen protocol, models, refit checkpoints, diagnostics,
sensitivities, estimates, session information and hashes. Existing directories
are refused. Manuscript rendering must consume frozen outputs only.
