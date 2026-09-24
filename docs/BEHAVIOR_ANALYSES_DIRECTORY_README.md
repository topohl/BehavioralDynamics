# RFID analyses

This directory holds active, semantically named outputs from the staged
behavioral-output migration. For the complete producer, manuscript role, and
historical path of each group, see `../output_index.csv`.

- `first_night_five_domain_characterization/10min/`: Stage 14 primary
  first-night five-domain results.
- `first_night_five_domain_characterization/5min/`: Stage 14 sensitivity
  results. This resolution does not replace the 10-minute primary.
- `spatial_occupancy/tables/`: Stage 19 spatial result tables.
- `spatial_occupancy/audit/`: Stage 19 QC and provenance files.
- `spatial_occupancy/models/`: Stage 19 model and contrast files.
- `spatial_occupancy/figures/`: Stage 19 figure files.
- `dyadic_contacts/`: current Stage 02 dyadic RFID contacts, including the
  network-ready table used by Stage 06.
- `dynamic_social_networks/5min/`: current Stage 06 five-minute outputs.
  The older 10-second, 1-minute, 10-minute, and 30-minute runs remain in
  `../06_behavioral_dynamics/social_networks/` under their historical names.
- `gamm_trajectory_features/10min/`: current Stage 07 trajectory features.
  The separate 30-minute tree remains under
  `../06_behavioral_dynamics/gamm_features/` as a historical Stage 15 input.
- `behavioral_state_space/5min/`: current Stage 05 state-space outputs.
  Older 1-minute and 10-minute branches remain under
  `../06_behavioral_dynamics/state_space/`.
- `hmm_states/10min/`: Stage 08's declared HMM primary output.
- `hmm_states/5min/`: Stage 08's HMM sensitivity output. Historical audit
  scripts can still read the retained originals in
  `../06_behavioral_dynamics/hmm_states/`.
- `hmm_revalidation_runs/current_stage08_review_20260924/`: five current-input
  gap-aware audit tables. These are unpromoted review evidence; see the
  `hmm_revalidation_runs/README.md`. The older 183-file HMM audit tree and
  manuscript registry links remain under the numbered Stage 14 root.
- `temporal_instability/10sec/`: current Stage 04 temporal-instability outputs,
  including large rolling-metric tables. Older 1-minute and 5-minute branches
  remain under `../06_behavioral_dynamics/temporal_instability/`.
- `adaptation_kinetics/10min/`: current Stage 11 adaptation and recovery
  outputs. Its older five-minute branch remains under
  `../15_behavioral_adaptation_kinetics/`.
- `sleep_like_inactivity/10min/`: current Stage 12 rest-like inactivity
  outputs. These are not EEG-validated sleep measures. The older five-minute
  branch remains under `../16_sleep_like_inactivity_metrics/`.
- `phase_organization/10min/`: current Stage 13 active/inactive phase
  outputs. The older five-minute branch remains under
  `../17_ethological_phase_organization/`.
- `nonlinear_dynamics/5min/`: manually produced supporting nonlinear
  features used by Stages 10, 14, and 15.
- `systems_phenotyping/5min/`: manually produced supporting phenotype
  features used by Stages 10 and 14.
- `inactive_phase_qc_audit/`: three tables from a manual audit of inactive
  RFID QC. Its proposed rule is unpromoted and has not changed production QC.
- `systems_dashboard/5min/`: current Stage 14 dashboard tables, statistical
  tables, authored figures, and interactive assets. Figure indexes and folder
  guides were regenerated for this semantic copy. First-night results and
  independently produced audit families have separate owners.
- `rfid_domain_comparison_audit/`: four supporting tables comparing the
  retained five-domain first-night analysis with the local four-domain
  analysis. This path change does not promote a manuscript claim.
- `rfid_leading_bin_seed_audit/`: 12 supporting tables testing sensitivity
  to seeded leading bins in the 10-minute RFID metric series. The primary
  analysis still uses the full window.
- `rfid_construct_audit/`: phenotype-blind checks of four-domain overlap,
  redundancy, and resolution stability. Flags do not change domain formulas.
- `rfid_conservatism_audit/`: exploratory precision and power diagnostics.
  Its p-values do not select the primary analysis.
- `rfid_alternative_inference_audit/`: exploratory alternative-statistics
  diagnostics. These do not change Stage 28 decisions.
- `rfid_reliability_audit/`: diagnostics separating within-occasion
  consistency from cross-occasion behavioral stability.

The older five-minute Stage 11–13 branches predate the exact-phase-classifier
fix. Both retained Stage 15 integration inventories record loading eight tables
from them. Those existing exploratory integrations need a separate scientific
source-validity review before they are reused or rebuilt.
- `behavior_proteomics/proteomics_mnn_primary/` and
  `behavior_proteomics/proteomics_mnn_sensitivity/`: Stage 15 exploratory
  primary and flagged-replicate sensitivity outputs. The two-row
  `behavior_proteomics/proteomics_integration_output_dir_map.csv` connects the
  short directory slugs to their full proteomics input labels. The original
  map remains under `../06_behavioral_dynamics/` as run provenance.

The numbered original folders remain intact for provenance and historical
readers. The active path for each group is recorded by its `activated` receipt
in `../_migration_control/`. Scientific stages and release builders were not
rerun as part of the folder migration. Publication copies remain under
`../../publication_ready/`.
The manifests copied with each analysis record the original run path and time;
they are historical provenance, not instructions to write back there.
