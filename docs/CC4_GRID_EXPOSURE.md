# CC4 grid-associated period (Stage 31)

## Placement and scope

`Analysis/31_cc4_grid_exposure.R` is a manually invoked exploratory analysis,
outside `run_all_analysis.R`, the frozen Stage 29/30 analyses and manuscript
assembly. It belongs beside other explicit analyses in `Analysis/`, not in
`_archive/` or `_supporting/` (which contains older producers with live consumers).
Reusable calculations live in `Functions/cc4_grid_exposure_helpers.R`.

Outputs use the semantic directory
`analysis_ready/analyses/cc4_grid_exposure/<run-id>/`. Every run requires a new
directory; no existing input, run, canonical metric or release is overwritten.
`full_recording/` contains the run's eligible genuine CC4 records, `audit/`
contains coverage and provenance, `tables/` contains descriptive results and
`figures/` contains diagnostic PNGs. This is not a publication bundle.

## Established facts and timing limitation

The September 29, 2026 audit found the following in all B1-B6 CC4 raw CSVs:
partial I1, A1, I2, A2, I3, A3, I4, A4, I5, A5, partial I6. Full-clock A5
exists in every batch. Legacy preprocessing removes I1 and the maximum
inactive/active indices when greater than four: for these recordings I6 and
A5 are removed, while I5 remains. Stage 01 subsequently excludes CC4 phase
numbers above two. Its old comment implying a universal four-phase cap was
incorrect. These filters do not measure recording completeness.

The user reports grid exposure for two hours during inactive phases, with
variable start times, and confirms that insertion/removal times were **not
recorded**. The earlier schedule associates I3-I5 with SuPH days. Whole I3-I5
phases must not be labelled grid-present, nor should all subsequent nights be
treated as physically exposed. Actual grid exposure per cage remains unknown.

An activity increase can propose a time for review, but selecting it and then
testing that increase as an exposure effect is circular. Stage 31 therefore
does **not** estimate grid effects, run significance tests or assign exposure
labels. Activity candidates remain `activity_inferred_unverified`; the separate
timing-review CSV leaves insertion/removal fields blank and sets
`ExposureAnalysisAllowed = FALSE`. Visual agreement cannot make these times
independent evidence. No missing time defaults to a usual start or inferred end.

## Retention and identity

`preprocess_animalpos_file(..., phase_policy = "full_recording", write_output = FALSE)`
retains eligible genuine observations, including edge phases, without synthetic
rows. It validates timestamps, animal/system labels and positions. Its default
`legacy` policy preserves existing producer behavior and schemas. Full-mode
writes must go through the separate driver, so they cannot accidentally replace
`preprocessed_data/`. Canonical exclusions still apply; this is not an untouched
copy of the raw input.

Stage 31 applies the current configured data version's SHA256-checked cage-label
correction registry, keeping `System_raw`. This includes B6 CC4 OR646's documented
sys.2-to-sys.5 correction. Animal IDs are retained as strings and canonical IDs
use the existing shared helper. Stage 29/30 config is read, not changed. The
original input files and code are hashed in each run.

## Descriptive calculations

### Main comparison: complete inactive phases (I2-I5)

The main descriptive comparison uses the full **06:30-18:30** period, regardless
of any inferred grid timing. `Functions/cc4_whole_inactive_helpers.R` sums the
144 unique five-minute bins per animal/phase and divides crossing counts by 12
hours. Missing/duplicate bins fail validation. A partial session or animal span
gets a missing rate, not a partial count divided by 12. A fully bracketed phase
with zero crossings remains zero; bracketing still does not prove sensor health.

All available animal and cage phases remain in the audit tables. The primary
comparison uses only cages whose **entire roster** has complete I2/I3/I4/I5 spans,
holding the animals and cages constant even at the I2 baseline. In the current
data this removes B5 sys.1 from the paired comparison on all four days. Healthy
animals in a partially observed cage would remain in the descriptive audit but
would not replace missing cage-mates in the primary comparison.

Compute animal rates, then equal-animal cage means, then equal-cage batch means,
then equal-batch sex-specific means. Report I3-I2, I4-I2 and I5-I2 separately;
the mean of I3-I5 minus I2 is secondary. Females and males are separate, with all
their batches retained. No phenotype labels or outcome variables are used; this
is an all-recorded-cages phase comparison, not a phenotype-specific analysis.

Uncertainty is a **descriptive pointwise 95% percentile interval**, using 2,000
whole-cage bootstrap draws within each fixed batch (seed 20260929). Each sampled
cage brings all four days, preserving repeated measurements and animal nesting.
This accounts for within-cage dependence and fixed batch differences without
treating animals as independently exposed replicates. At least two batches and
two complete cages per batch are required; otherwise status is NOT_ESTIMABLE
and intervals are missing. Intervals are conditional on the observed batches,
not a claim of generalisation to new batches, and are not multiplicity-adjusted.
No p-values, significance stars or causal grid-effect claims are produced.

Outputs: `whole_inactive_animal_activity.csv`, `whole_inactive_cage_activity.csv`,
animal/cage `*_changes_from_I2.csv`, `whole_inactive_sex_summary.csv`, and the
population, bootstrap-status and bootstrap-draw audit tables. Diagnostic figures
show individual trajectories with cage means and paired changes with intervals.
The candidate-window scan remains a separate exploratory timing aid and is never
an input to these whole-phase calculations.

### Supporting clock profiles and candidate windows

- Clock: logger timestamps parsed as UTC without local/DST conversion; inactive
  06:30 inclusive to 18:30 exclusive; active 18:30 to 06:30. Original phase
  numbers are never recomputed after trimming. Full recording half-hour indices
  start at its initial I1 clock block; they are not the legacy A1-based indices.
- Coverage: report every animal x scheduled phase, including zero-read phases.
  A session spanning a whole 12-hour block is distinct from an animal's first/last
  reads bracketing it. Neither establishes uninterrupted sensor health.
- Activity: genuine consecutive PositionID changes, attributed to the later read.
  First reads are not crossings; a prior read from the previous bin/phase can
  define a crossing. Conflicting positions at the same animal/time fail closed.
- Profiles: five-minute counts divided by expected cage animals x 5/60 hours.
  The roster is fixed within each corrected cage/session; disappearing animals
  are not silently removed from the denominator. Plots leave gaps where any
  animal's first/last timestamps do not bracket the bin. Counts remain in tables
  with explicit coverage flags. No position imputation or sensor-gap exclusion
  threshold is introduced. Movement is not physical distance, sleep or sociability.
- Candidate localisation: scan all 121 possible five-minute starts for a two-hour
  window fully inside each I3-I5. Score its crossings minus the same clock-time
  I2 crossings, divided by expected animals x two hours. Eligibility requires
  whole-session and whole-animal bracketing for the target and I2 reference phase.
  Retain at most three positive non-overlapping candidates, ranked by score;
  tied scores use the earlier start. Export every score, not only the maxima.
  There is no fitted threshold, phenotype input or p-value. An empty candidate
  table means no eligible positive candidate under this rule, not no exposure.
- Batches/cages remain separate and Sex is retained (B1/B2/B5 male; B3/B4/B6
  female). Subsequent A3-A5 nights appear alongside A1/A2 in their own profiles.

I2 is a descriptive reference, not a matched experimental control. Handling,
SuPH, clock time and time since cage change can all influence activity. Sensor
read patterns may also change during exposure. B5 sys.1 loses all four animals'
records during I3; later zeros are not interpretable as inactivity, and those
phases fail candidate eligibility. Social/proximity and occupancy-based effects
need a separate measurement-validity decision before analysis.

## Run and test (PowerShell, repository root)

```powershell
$env:MMM_GRID31_RUN_ID = 'cc4_grid_timing_review_v1'
Rscript --vanilla 'Analysis/31_cc4_grid_exposure.R'
Remove-Item Env:MMM_GRID31_RUN_ID

Rscript --vanilla 'Testing/tests/test_cc4_grid_exposure.R'
Rscript --vanilla 'Testing/tests/test_cc4_whole_inactive.R'
Rscript --vanilla 'Testing/tests/test_animalpos_preprocessing_helpers.R'
Rscript --vanilla 'Testing/tests/test_preprocessing_function_ownership.R'
```

Test record: on 2026-10-05, at commit 5a3e4e9 with R 4.5.1, `test_cc4_grid_exposure.R`,
`test_cc4_whole_inactive.R` and `test_cc4_phase_groups.R` passed. They had not been run
when Stage 31 was committed (383b25c). `Testing/tests/test_sourced_scripts_guarded.R`
checks that sourcing the Stage 31 scripts only defines functions.

Use a **new** run ID for a later run. `mmm_project_root()` supplies the existing
root configuration (`MMM_BEHAVIOR_PROJECT_ROOT` / `MMM_PROJECT_ROOT` or the
documented S: default). Dependencies are existing R packages, including digest
for provenance hashes and ggplot2 for diagnostic plots; nothing is installed by
the driver. The portable test exercises the actual entry function on fixtures,
including variable activity timing, incomplete tracking, corrections, retained
A5, blank exposure times and refusal to overwrite outputs.

If independent insertion/removal evidence is later recovered, add a separately
validated event table and prespecified relative-time windows before attempting
exposure comparisons. Keep activity-inferred timing distinct, even after review.

## Stage 31b: CON/RES/SUS and subsequent active phases

`Analysis/31b_cc4_phase_groups.R` is an explicit downstream extension. It verifies
the complete output manifest of a prior Stage 31 run and reads its five-minute
animal bins, avoiding changes to raw or canonical preprocessed data. It joins
`canonical/later_outcome_combz/tables/later_outcome_combz_animal_level.csv` by
string animal identity and verifies sex and batch. Duplicate, missing or invalid
group assignments fail; an unmatched animal is never defaulted to RES.

The reusable `grid31_whole_phase()` aggregator checks fixed 12-hour inactive
I2-I5 and active A1-A5 windows. Its existing inactive wrapper preserves the old
schema. Stage 31b uses a common complete cage roster across all nine phases.
Inactive changes reference I2; active changes reference A2, the preceding night.
A1 is retained as earlier context. This is an analysis choice fixed before
examining group contrasts, not a recovered exposure time.

Within each cage/group, animal rates are averaged equally. Cage/group means are
averaged equally within each batch/group, and the three batches are averaged
equally within each sex/group. RES-CON, SUS-CON and SUS-RES rate differences and
differences in change are calculated within batch before combining. The target
is an equal-batch average of cage/group means, not a pooled-animal average.

There is only one CON cage per batch. Thus the group extension does not reuse
the within-batch cage bootstrap of the pooled analysis: that would give no
within-batch control variation. Instead it exports each batch estimate and
descriptive pointwise 95% Student t intervals across the three independent
batch estimates per sex (df=2). These assume approximately normal batch
estimates and have limited precision with n=3. They are not multiplicity
adjusted. Fewer than three batches gives a NOT_ESTIMABLE interval status;
missing groups within a batch fail rather than changing the target weights.
No p-values, significance stars or equivalence claims are produced.

RES/SUS are outcome-defined associations, not randomized groups. Sucrose
preference contributes to the canonical outcome classification. Phase changes
cannot identify a specific timed or causal grid effect, compensatory sleep,
or physical distance. Whole-phase bracketing is not a sensor-health guarantee.

The versioned output directory contains animal and cage/group rates, batch
estimates, group summaries, paired contrasts, population audit, the canonical
outcome snapshot, input hashes and an output manifest. Exp9_manuscript imports
this bundle byte-exact and renders candidate figures; it does not calculate
new estimates or intervals. The behavioural palette is CON #3E3C6F, RES #C6C3BB,
SUS #E63A48, matching `mmm_publication_theme.R` and the manuscript's vendored
`plotting_nature.R`. A conflicting manuscript-wide YAML palette is not used by
these behavioural candidates.

```powershell
Rscript --vanilla 'Testing/tests/test_cc4_phase_groups.R'
Rscript --vanilla 'Analysis/31b_cc4_phase_groups.R' '<existing Stage31 run>' '<canonical outcome CSV>' '<new output directory>'
```
