# Behavioral output migration

The separate Stage 14 five-minute dashboard cutover adds 293 hash-verified
scientific/artifact copies and seven regenerated metadata files under
`analysis_ready/analyses/systems_dashboard/5min/`. Its own activated receipt
is outside the 938-file base plan described below.
The four-file RFID legacy-versus-current comparison audit has a second
separate activated receipt under `analyses/rfid_domain_comparison_audit/`.
Its numbered originals remain, and no scientific audit was rerun.
The 12-file leading-bin seed sensitivity audit has another separate activated
receipt under `analyses/rfid_leading_bin_seed_audit/`; its two direct contract
test inputs now resolve there. The numbered originals remain.
The other four RFID audit families were subsequently activated as four
independent groups: 39 hash-matched copies under `analyses/`, with all
numbered originals retained. Their approved plan and cutover checks are
recorded in `docs/BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md`. The 183-file HMM
audit tree remains in its numbered historical location. Two audits were
subsequently rerun against current inputs in an isolated semantic directory;
the older tree is still pending a separate provenance and reader review
before any migration.

The bounded Stage 14/19, Stage 02/06, Stage 07, Stage 05, Stage 08, Stage 04,
Stage 15, Stage 11–13, two manual supporting analyses, and one manual
inactive-phase QC audit were activated on 2026-09-23. Twenty-one groups and
938 files now have
semantic copies under `analysis_ready/analyses/`. All original files remain in
their historical folders. Activation receipts select the new
paths for the participating producers and readers. See
`docs/BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md` for checks and limitations.

The approved human-facing layout is `analysis_ready/foundations/` for shared
derived inputs, `analysis_ready/analyses/` for scientific outputs,
`analysis_ready/manuscript/` for assembled products, and separate `history/`
and `quarantine/` trees. Scientific stage IDs remain in code and registry
metadata. New paths use `10min/` and `5min/`; the primary/sensitivity role must
stay explicit in metadata.

`BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv` currently enumerates eleven resolved
sets: Stage 14's 20 first-night five-domain files (10 per resolution),
Stage 19's 25 spatial occupancy files (tables, audit, models, figures),
Stage 02/06's 110 current dyadic and five-minute social-network files,
Stage 07's 18 current ten-minute GAMM-feature files, Stage 05's 97 current
five-minute state-space files, Stage 08's 72 primary and sensitivity HMM
files, Stage 04's 212 current ten-second temporal-instability files, and
Stage 15's 86 behavior-proteomics files, Stages 11–13's 60 current
ten-minute files, 235 supporting-analysis files, and three inactive-phase QC
audit files. Every
file has an approved source path, proposed destination, and current SHA-256.
All twenty-one groups were changed to `gate=ready` after their producer, reader,
release, registry, and test contracts passed review. The plan pins separate
code-contract CSVs with SHA-256 hashes. The activated receipts record those
contracts as they existed at activation time.
Stage 10 no longer scans the Stage 19 model/figure result trees. Its recorded
10-minute feature-source audit showed four model CSV candidates, all rejected,
and zero loaded spatial figures. The read-only audit
`Testing/audits/audit_stage10_spatial_feature_boundary.R` checks that evidence.
This establishes the spatial boundary for the recorded run; it is not a new
Stage 10 scientific rerun.

The Stage 14 first-night producer, Stage 19 spatial producer, Stage 27 input
registry, release builder, and Stage 16 index definition now call shared
output-group accessors. Each accessor uses the current path until the migration
tool records a verified `activated` receipt for that group. It rejects a
semantic directory without its receipt and a moved directory whose receipt is
still `prepared`. Stage 14 refuses mixed states across its two resolutions;
Stage 19 refuses mixed states across its four groups. Stage 08 refuses a
mixed state across its primary and sensitivity groups. Stage 15 refuses a
mixed state across its primary and sensitivity groups. All twenty-one live receipts
are now `activated`, so these callers resolve to the semantic paths. The live
`output_index.csv` was refreshed from the current Stage 16 source definition
without running the full Stage 16 exporter; its prior version is retained in
`analysis_ready/_migration_control/`.

Stage 02 now writes to the active dyadic group by default; its explicit
`mmm.dyadic_contacts_dir` override still supports the separate cookie-habituation
dataset. Stage 06 reads that active dyadic group and writes the active five-minute
social group. It refuses a partial activation of the pair. Stages 14 and 15
resolve the current five-minute social tables through the same receipt. The four
older social-network resolutions remain in the historical tree; their May
manifests name an earlier producer and are not part of this cutover.
Stage 07 writes to its active ten-minute semantic group. Stage 14 follows
the receipt for ten-minute features while retaining the original resolution
preference order. Stage 15 still reads the historical 30-minute feature tree;
that tree was not copied or reclassified by the Stage 07 cutover.
Stage 05 writes to its active five-minute semantic group. Stages 14 and 15
read that group through the receipt, while older one- and ten-minute branches
remain historical. Stage 14 retains its ordered resolution preferences.
Stage 08 writes its 10-minute primary and 5-minute sensitivity outputs through
the paired receipts. Its Stage 14 artifact resolver, Stage 14 dashboard, and
Stage 15 optional reader follow those paths. Historical audits retain the old
directories and were not rewritten.
Stage 04 writes to its active ten-second semantic group. Stage 14's temporal
resolver follows that receipt, while Stage 15's optional 5-minute and 1-minute
preferences stay on the historical tree. The large rolling-metric tables were
copied and fully hashed; no Stage 04 analysis was rerun.
Stage 15's two active output directories and future output-directory map now
live under `analyses/behavior_proteomics/`. A new two-row location map was
derived from the historical one after activation; the historical map was not
rewritten. The full proteomics labels remain in both maps. Stage 15's models
were not rerun.
Stages 11–13 now write their declared ten-minute outputs under
`analyses/adaptation_kinetics/10min/`, `analyses/sleep_like_inactivity/10min/`,
and `analyses/phase_organization/10min/`. Stage 14 follows the receipts for
its ten-minute input preferences and applies its phase-classifier staleness
guard to the semantic paths. Both existing Stage 15 outputs recorded eight
loaded tables from the pre-fix five-minute branches. Future Stage 15 code now
selects the activated, producer-declared ten-minute outputs for those eight
phase sources; its feature loader also refuses an explicitly requested
five-minute phase table without verified lineage. The existing integrations
label a canonical ten-minute Stage 09 input as five-minute; that future source
specification was corrected as well. None of these code changes rewrote the
historical integrations or ran a scientific analysis. The five-minute branches
remain retained in their numbered locations.
The existing Stage 15 integrations also let HMM features into their primary
behavior matrix despite the default exclusion option. Future code now enforces
that option for both direct five-minute HMM tables and ten-minute HMM summary
features; the historical integration files remain unchanged and require a
reviewed rerun before scientific reuse.

The manually run nonlinear and systems-phenotyping producers now write to
receipt-selected five-minute semantic roots. Stage 10 selects exactly one root
for each recursive feature scan; its candidate sets remained 26 and 55 CSVs
after activation. Stages 14 and 15 resolve their supporting feature paths
through the same helper. Neither supporting producer nor any scientific reader
was rerun for this cutover.

## Cutover record

For a source parent shared by multiple scientific owners, the migration tool
supports `-OwnershipManifest` with `-OwnershipManifestSha256`. The manifest
must enumerate every file under the selected source root, give each file an
`owner_group` and `state`, and mark every file selected by the requested plan
as `ready`. The exact manifest hash is bound to the prepared receipt and must
be supplied unchanged for verification and activation. Unexpected files,
wrong ownership, and files still awaiting review fail closed. The Stage 14
700-file ownership manifest and 293-file hash plan are retained under
`docs/behavior_output_activated_plans/`, along with the four activated RFID
audit plans. Their selected rows and gate were set to
`ready` for the separate dashboard activation. Its 293
authored files have verified semantic copies, and seven derived metadata files
were regenerated. The numbered originals remain. See the Stage 14 entry in
`BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md`. The 938-file base plan and its tests
remain a separate, completed batch.
Read-only `Inspect` checks the complete inventory and selected source hashes
even while ownership is awaiting review, returning `ownership_ready=False`;
`Prepare` and `Activate` still require `ready` ownership.
For the Stage 14 dashboard group, `Activate` regenerates seven derived figure
metadata files from the copied authored figures before it promotes the
prepared receipt. It verifies the full destination inventory and records the
seven metadata hashes in the receipt. An interrupted move or partial metadata
write can be retried while the receipt remains `prepared`; readers fail closed
until the metadata step succeeds. The scientific Stage 14 script is not run.

All 938 files in the earlier activated groups matched their respective plans;
the subsequent Stage 01 foundation cutover added 20 hash-matched copies.
There are no group staging directories left.

The remaining numbered roots are separate migration candidates. Stage 01's
20 current metric/QC products now resolve at
`foundations/behavior_metrics/`; all 52 files under `03_derived_metrics/`
remain for historical paths and provenance. `12_systems_neuroscience_summary/` retains the original
Stage 14 dashboard, historical manifests, figure mirrors, and independently
produced audit families. The current dashboard, first-night groups, and
inactive-QC audit have separate semantic paths. The supporting nonlinear and systems-
phenotyping trees are now active semantic groups; their numbered originals
remain in place. The older resolution branches under
`06_behavioral_dynamics/` retain their historical provenance;
receipt-selected copies under `history/` serve optional readers and Stage 10
feature discovery.
The read-only live-tree and code-dependency snapshot for the remaining roots
is `docs/BEHAVIOR_OUTPUT_REMAINING_AUDIT.md`.

| Group | Files | Producer and active path users | Post-cutover state |
| --- | ---: | --- | --- |
| `first_night_10min` | 10 | Stage 14 writer; manuscript registry; release builder; Stage 28 comparison | Active semantic path; eight release inputs resolve there. |
| `first_night_5min` | 10 | Stage 14 sensitivity writer; Stage 16 index | Active semantic path; remains labeled sensitivity. |
| `spatial_tables` | 6 | Stage 19 writer; Group/Sex output test | Active semantic path; live output test passes against the corrected Stage 01 roster. |
| `spatial_audit` | 8 | Stage 19 QC/provenance writer | Active semantic path; original shared source directory retained. |
| `spatial_models` | 6 | Stage 19 writer | Active semantic path; Stage 10 excludes this result tree from feature discovery. |
| `spatial_figures` | 5 | Stage 19 writer | Active semantic path; `publication_ready` copies remain separate. |
| `dyadic_contacts` | 16 | Stage 02 writer; Stage 06 reader | Active semantic path; explicit cookie-habituation override retained. |
| `social_networks_5min` | 94 | Stage 06 writer; Stages 14 and 15 readers | Active semantic path; four older resolution runs remain historical. |
| `gamm_features_10min` | 18 | Stage 07 writer; Stage 14 reader | Active semantic path; Stage 15's 30-minute historical input remains in place. |
| `state_space_5min` | 97 | Stage 05 writer; Stages 14 and 15 readers | Active semantic path; old 1- and 10-minute branches remain historical. |
| `hmm_states_10min` | 36 | Stage 08 primary writer; Stage 14/15 readers | Active semantic path; declared HMM primary. |
| `hmm_states_5min` | 36 | Stage 08 sensitivity writer; Stage 14/15 readers | Active semantic path; separate sensitivity. |
| `temporal_instability_10sec` | 212 | Stage 04 writer; Stage 14 reader | Active semantic path; old 1- and 5-minute branches remain historical. |
| `proteomics_mnn_primary` | 43 | Optional Stage 15 primary writer; manuscript registry | Active semantic path; exploratory small-n association output. |
| `proteomics_mnn_sensitivity` | 43 | Optional Stage 15 sensitivity writer | Active semantic path; flagged-replicate sensitivity output. |
| `adaptation_kinetics_10min` | 19 | Stage 11 writer; Stage 14 ten-minute reader | Active semantic path; older five-minute branch retained. |
| `sleep_like_inactivity_10min` | 19 | Stage 12 writer; Stage 14 ten-minute reader | Active semantic path; no sleep validation claim. |
| `phase_organization_10min` | 22 | Stage 13 writer; Stage 14 ten-minute reader | Active semantic path; older five-minute branch retained. |
| `nonlinear_dynamics_5min` | 97 | Manual supporting writer; Stages 10, 14, and 15 readers | Active semantic path; numbered original retained. |
| `systems_phenotyping_5min` | 138 | Manual supporting writer; Stages 10 and 14 readers | Active semantic path; numbered original retained. |
| `inactive_phase_qc_audit` | 3 | Manual inactive-QC audit writer; manuscript registry evidence path | Active semantic path; proposed QC rule remains unadopted. |

Stage 16's index now has distinct `19-tables` and `19-audit` rows. Historical manuscript
notes and audit outputs that record the original path remain provenance; they
must not be rewritten as though the historical run used the new location.
The Stage 28 comparison check and its audit follow the activated receipt for
the five-domain group.

The per-group activation code snapshots remain under
`docs/behavior_output_code_contracts/`. A post-activation correction to the
Stage 19 live-output test updated its expected roster from 24/49/38 to the
canonical 24/53/34; the original activation snapshot and receipt remain
immutable. The activation record documents both test hashes. Old files remain
in place for provenance and historical readers. No scientific stage or release
builder was rerun during this migration.

The PowerShell tool is
`Maintenance/Invoke-BehaviorOutputMigration.ps1`. Its `Inspect` action reads
and hashes sources without writing. `Prepare` copies a ready group into
`analysis_ready/_migration_incoming/<group>/`, verifies the file inventory and
hashes, checks that originals stayed unchanged, and writes a plan-bound receipt
under `analysis_ready/_migration_control/`. The receipt binds to the approved
rows for its own group, so later review of a different group does not invalidate
an already prepared one. `Verify` repeats staging
checks. `Activate` moves that verified staging directory to its semantic
destination, checks hashes, and promotes the receipt to `activated`. If
interrupted after the move, a retry accepts only an exact destination inventory
with matching hashes before promoting the receipt. It never deletes an original
or replaces an existing destination. A failed or interrupted preparation leaves
staging visible for inspection; a retry verifies existing files and copies only
missing ones. Mismatched files are never overwritten and no automatic cleanup
occurs.

Example read-only inspection, from the repository root:

```powershell
& .\Maintenance\Invoke-BehaviorOutputMigration.ps1 `
  -Action Inspect -Group first_night_10min `
  -AnalysisReadyRoot 'S:\Lab_Member\Tobi\Experiments\Exp9_Social-Stress\Analysis\Behavior\RFID\analysis_ready'
```

This tool does not edit R code, choose canonical artifacts, rerun science,
rebuild a release, retire fallbacks, or move `publication_ready` copies. The
receipt is the explicit path switch for prepared repository code; consumer and
release-path validation was completed before cutover. Old absolute paths in
historical manifests remain historical evidence. The exact old-to-new mapping
and hashes are recorded in the plan.

The data-independent fixture test is
`Testing/tests/test_behavior_output_migration.ps1`. It uses only a temporary
directory, checks the blocked gate and change detection, and exercises the
prepare/verify/activate flow on flat and nested synthetic files. It does not
touch `S:`. The complete-tree check ignores Windows `Thumbs.db` caches only;
any other unplanned source file blocks migration.
The index and path-source test is
`Testing/tests/test_behavior_output_index_source.R`; run both from the
repository root when reviewing a change to this plan.

Historical-resolution maps use a separate read-only checker:
`Maintenance/Test-BehaviorHistoricalOutputMap.ps1`. The activation tool above
intentionally rejects `history/` destinations. The historical checker accepts
the draft file-map columns `source_rel`, `proposed_target_rel`, `bytes`,
`last_write_utc`, and `sha256`; it verifies allowed numbered source roots,
exact `history/<family>/<resolution>/...` counterparts, complete source
inventories, unchanged metadata and SHA-256, and absent destination roots.
It ignores only Windows `Thumbs.db` cache files when checking completeness,
as the activation tool does. It has no copy or activation action. Run its
synthetic fixture with
`Testing/tests/test_behavior_historical_output_map.ps1` before relying on a
new version of the checker.

The migration tool also supports `-HistoricalDestination` for a reviewed
historical plan. Without that switch it still rejects `history/`. With the
switch it accepts only the nine named historical resolution groups, their
exact numbered source roots, matching relative file paths, and the matching
`history/<family>/<resolution>/` destination. `gate=ready`, the code
contract, complete-tree and hash checks, retained originals, and receipt
requirements remain mandatory. A synthetic blocked-to-activated history
fixture is included in `Testing/tests/test_behavior_output_migration.ps1`.
Live historical activation is recorded in `BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md`.

The separate `docs/BEHAVIOR_HISTORICAL_MIGRATION_PLAN.csv` records 864 exact
files across nine older resolution groups. Every row is `ready`; the nine
groups were activated on 2026-09-24 after a fresh read-only inspection.
Its shared code contract is
`docs/behavior_output_code_contracts/historical_resolution_readers.csv`.
The live `Inspect` action passed for all nine groups with matching source
hashes and no staged or destination directory before activation. For one
read-only group check:

```powershell
& .\Maintenance\Invoke-BehaviorOutputMigration.ps1 `
  -Action Inspect -Group history_social_networks_10min `
  -HistoricalDestination `
  -Plan .\docs\BEHAVIOR_HISTORICAL_MIGRATION_PLAN.csv `
  -AnalysisReadyRoot 'S:\Lab_Member\Tobi\Experiments\Exp9_Social-Stress\Analysis\Behavior\RFID\analysis_ready'
```

Stage 16 has nine receipt-aware history detail rows alongside its four
aggregate family rows. The live navigation index now selects their semantic
paths, and its previous version is backed up under `_migration_control/`.
Stage 10's read-only candidate check found 607 filtered paths, including 330
history routes and 85 eligible `AnimalNum` files; their order, basenames, and
sizes were preserved. No Stage 10 model or manuscript pipeline was rerun.
The originals and their scientific provenance remain unchanged. A semantic
copy does not make an older run current scientific evidence.
