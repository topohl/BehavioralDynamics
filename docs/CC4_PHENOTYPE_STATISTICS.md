# Stage31d: CON/RES/SUS whole-phase responses

Scientific role: exploratory phenotype-associated characterization during the
late CC4 assay period. This module complements the early CC1 prospective result;
it is not a second prospective predictor or an independent validation of RES/SUS.
Sucrose preference contributes to the canonical outcome. All groups received
grid sessions, whose insertion/removal times are unrecorded. Whole-phase counts
do not identify a causal grid effect, sleep, or recovery across sessions.

The analysis plan was revised after reviewing descriptive and pooled-CON/SIS
results. Four main tests within this module assess Group-by-Period interaction
for females/males and inactive/active phases. Inactive pools I3-I5 versus I2;
active pools A3-A5 versus A2. Each period has a fixed complete cage roster, with
12h versus36h exposure offsets. A1 is not an inclusion requirement. Original
recordings, Stage31b summaries and Stage31c condition analyses are unchanged.

Animal NB2 model:

```
Count ~ Group*Period + Batch*Period + offset(log(Hours)) +
        (1|AnimalKey) + (1|CageID) + (0+Post|CageID)
```

Animal and cage random intercepts account for repeated records and cage sharing;
the independent cage post-period deviation allows shared cage responses. Batch
is a fixed block (three per sex). The omnibus null removes the two group-period
interaction coefficients. Pairwise nulls retain group baseline differences and
the third group's free response: RES-CON permits a SUS-specific post term;
SUS-CON permits a RES-specific term; SUS-RES gives RES and SUS a common post term.
The random-effects structure is identical in all fits. All12 planned pairwise
contrasts are retained regardless of omnibus significance.

Each test uses its own constrained-null parametric bootstrap LR distribution,
resampling animal/cage/post random effects and counts. Four omnibus P values
receive Holm correction; the12 planned pairwise P values form a separate Holm
family. A shared full-model bootstrap per context provides percentile95% intervals
for all three ratios of rate ratios. Intervals are pointwise, not simultaneous.
Pairwise P values are not Wald approximations or tests under an inappropriate
all-groups-equal null. The default is4999 successful draws for every procedure.

Stage31c automatically withheld the phenotype family when any observed random
variance approached zero. Stage31d corrects that overly restrictive rule: SD<1e-4
is recorded as a boundary flag, while finite likelihood/coefficients, convergence0,
positive Hessian and maximum gradient<=0.01 determine numerical acceptance. No
random terms are dropped or variances manually fixed to zero. Failed initial
fits receive a BFGS retry. All observed fits, boundary counts, retries and failed
draws are exported. More than5% failed draws at a240-attempt checkpoint withholds
that procedure. Failed procedures remain visible; they are not replaced by Wald
P values. First requested successful refits are used within a fixed maximum
attempt budget ceiling(draws/0.94). Seeds and RNG kind are deterministic.

Diagnostic sensitivities omit each batch, allow dispersion~Group+Period and
restrict SUS/RES to mixed cages. These export point estimates and fit flags, not
additional discoveries. Observable predictive checks use999 simulations and are
descriptive envelopes, not independent tests. Three CON cages per sex remain a
major limitation; parametric bootstrapping cannot create independent replication.

Producer: `Analysis/31d_cc4_phenotype_statistics.R`, reading only the verified
Stage31b `animal_phase_activity.csv` and canonical metadata already joined there.
Source tables, all simulation results, models, diagnostics, protocol, package
versions and hashes are saved to a new directory. Stage31d is manually invoked
and does not alter the default pipeline. Rendering belongs to Exp9_manuscript.

```powershell
Rscript --vanilla 'Testing/tests/test_cc4_phenotype_statistics.R'
$env:MMM_TEST_CC4_MODELS='1'
Rscript --vanilla 'Testing/tests/test_cc4_phenotype_statistics.R'
Remove-Item Env:MMM_TEST_CC4_MODELS
Rscript --vanilla 'Analysis/31d_cc4_phenotype_statistics.R' '<Stage31b run>' '<new output directory>' 4999 24
```

Short integration runs set `validation_only=TRUE` through the function API and
cannot emit inferential P values or intervals. The CLI requires final draw counts.
Existing output directories are refused. No reclassification, new package
installation, source-data overwrite or canonical manuscript promotion occurs.
