# Supporting producers

Scripts that are **not executed by `Analysis/run_all_analysis.R`** but whose
output trees **are read by active pipeline stages**. They were previously filed
under `Analysis/_archive/`, which was misleading: nothing archived should have
live consumers.

| Script | Produces | Read by |
|---|---|---|
| `13_nonlinear_systems_dynamics.R` | `analysis_ready/analyses/nonlinear_dynamics/5min/` | Stages 10, 14, and 15 |
| `14_nextgen_behavioral_phenotyping.R` | `analysis_ready/analyses/systems_phenotyping/5min/` | Stages 10 and 14 |

Neither is `source()`d anywhere; the dependency is on their **artifacts**, not
their code.

> **Numbering caution.** These are *superseded* Stage 13/14 scripts. The current
> Stage 13 and Stage 14 in the runner are
> `13_ethological_phase_organization.R` and
> `14_systems_neuroscience_summary_dashboard.R` — different analyses that happen
> to share the numbers. The numbered output originals remain for provenance;
> activated receipts select the semantic five-minute locations above.

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
  input is the receipt-selected Stage 01 metric at
  `analysis_ready/foundations/behavior_metrics/<bin>/all_behavior_metrics.csv`.
- The Stage 16 package and the publication release builder, which assembled
  Stage 03, Stage 09, first-night and QC artifacts, were retired on 2026-10-05.

The supporting artifacts also feed optional Stage 15 behavior-proteomics
integration. These exploratory results are not promoted to primary manuscript
claims (see `manuscript/README.md`). The existing Stage 15 integrations have a
separate source-validity issue for older five-minute Stage 11–13 inputs; see
`docs/BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md` before reuse.

## Status and recommendation

The migration changed only their output-path selection and readers. It did not
run the scripts or change their analysis logic. The reproducibility gap remains
because the main runner does not execute these supporting producers.

Two clean resolutions exist, both **deliberately deferred until after the
manuscript freeze**:

1. Wire them into `run_all_analysis.R` as option-gated supporting stages
   (`mmm.run_nonlinear_systems`, `mmm.run_nextgen_phenotyping`, default `FALSE`),
   so a full rebuild can regenerate everything the active stages read.
2. Review the supporting-feature dependencies in Stages 10, 14, and 15 and
   remove any that are scientifically unnecessary.

Either is a behavioural change to active pipeline stages and was therefore out of
scope for a release-candidate pass. The audit backing this is
`docs/ARCHIVE_DEPENDENCY_AUDIT.csv`.
