# Supporting producers

Scripts that are **not executed by `Analysis/run_all_analysis.R`** but whose
output trees **are read by active pipeline stages**. They were previously filed
under `Analysis/_archive/`, which was misleading: nothing archived should have
live consumers.

| Script | Produces | Read by |
|---|---|---|
| `13_nonlinear_systems_dynamics.R` | `analysis_ready/13_nonlinear_systems_dynamics/<bin>/derived_data/` | `Analysis/10_systems_feature_prediction_ladder.R`, `Analysis/14_systems_neuroscience_summary_dashboard.R` |
| `14_nextgen_behavioral_phenotyping.R` | `analysis_ready/14_nextgen_behavioral_phenotyping/<bin>/tables/` | `Analysis/10_systems_feature_prediction_ladder.R`, `Analysis/14_systems_neuroscience_summary_dashboard.R` |

Neither is `source()`d anywhere; the dependency is on their **artifacts**, not
their code.

> **Numbering caution.** These are *superseded* Stage 13/14 scripts. The current
> Stage 13 and Stage 14 in the runner are
> `13_ethological_phase_organization.R` and
> `14_systems_neuroscience_summary_dashboard.R` — different analyses that happen
> to share the numbers. The output directory names still carry the old meaning.

## The reproducibility gap

`run_all_analysis.R` does **not** regenerate these output trees. A clean rebuild
from raw data therefore reproduces every runner stage but leaves the nonlinear
and next-generation phenotyping trees as they were last generated. Stage 10 and
the broader Stage 14 dashboard consume whatever is on disk.

**No manuscript-facing analysis is affected.** Specifically:

- Stage 09 (primary prospective prediction) reads only Stage 01 metrics and the
  endpoint table.
- Stage 03 (secondary characterization) reads only Stage 01 metrics.
- The canonical first-night five-domain panel — the one manuscript-facing Stage 14
  product — is built by `Functions/first_night_domain_driver.R`, whose only data
  input is `analysis_ready/03_derived_metrics/<bin>/all_behavior_metrics.csv`.
- Stage 16 assembles only Stage 03, Stage 09 and QC artifacts.
- `Analysis/build_publication_release.R` resolves only Stage 03, Stage 09,
  first-night, Stage 16 and QC artifacts.

The exposure is confined to the Stage 10 systems-extension ladder and the wider
Stage 14 dashboard domains, both of which are explicitly **not promoted** to
manuscript claims (see `manuscript/README.md`).

## Status and recommendation

These scripts were left functionally untouched. They were moved so that the
repository does not describe live-dependency code as archived, and so the gap is
recorded rather than discovered later.

Two clean resolutions exist, both **deliberately deferred until after the
manuscript freeze**:

1. Wire them into `run_all_analysis.R` as option-gated supporting stages
   (`mmm.run_nonlinear_systems`, `mmm.run_nextgen_phenotyping`, default `FALSE`),
   so a full rebuild can regenerate everything the active stages read.
2. Remove the dependency from Stage 10 and Stage 14 entirely, on the grounds that
   both consuming analyses are exploratory and unpromoted.

Either is a behavioural change to active pipeline stages and was therefore out of
scope for a release-candidate pass. The audit backing this is
`docs/ARCHIVE_DEPENDENCY_AUDIT.csv`.
