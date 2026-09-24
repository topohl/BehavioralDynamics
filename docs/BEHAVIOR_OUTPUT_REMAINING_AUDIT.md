# Remaining numbered behavioral outputs: dependency audit

Read-only snapshot before the ninth cutover: 2026-09-23. This follows the 18 receipt-activated groups
and 700 copied files in `BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv`. File counts and
sizes below were checked on the live `S:` tree at that time; code relationships were checked
against the repository. The two supporting roots in this snapshot have since
been copied and activated as `nonlinear_dynamics/5min/` and
`systems_phenotyping/5min/`; their originals remain. See the activation record
for the current state. This document authorizes no further cutover.

Current-state update, 2026-09-24: the nine historical-resolution groups
described below were subsequently activated as verified copies under
`analysis_ready/history/`. All 864 numbered originals remain, and the
original/destination hashes match the approved plan. The plan gates are now
`ready`, all nine receipts are `activated`, and the live output index selects
the semantic paths. Stage 10 subsequently switched to receipt-selected group
discovery, as documented below. The sections below preserve the
pre-activation audit sequence; their earlier references to blocked gates,
absent destinations, and zero historical rewrites describe that prior state.
See `BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md` for the cutover checks and hashes.

### Stage 10 semantic-discovery readiness after the history cutover

A fresh inventory found 1,460 files in `06_behavioral_dynamics/`: 1,459
originate from the 18 activated groups in the two migration plans, and the
only unplanned file is the 780-byte
`proteomics_integration_output_dir_map.csv`. Every semantic group has exactly
its planned file inventory. The old map records numbered Stage 15 output
paths; its current semantic counterpart records semantic paths, so the two
files must not be treated as byte-identical copies. The saved Stage 10 source
audit marks the old map `loaded_as_feature_table=FALSE`, and its header has
no `AnimalNum` column.

`Testing/audits/audit_stage10_semantic_discovery_parity.R` now repeats the
read-only comparison. It scans the 18 semantic roots, restores their virtual
numbered-root sort order, and applies Stage 10's current self-ingestion
filters. The resulting 606 filtered candidate paths are identical and in the
same order as the former receipt-routed Stage 10 scan after removing the one
non-feature map row from its 607 paths. Stage 10 now scans the 18
receipt-selected groups directly and sorts their discovered files by virtual
numbered path, so new files written to a semantic group can be discovered
without scanning the old root. The 606-path comparison is read-only; no model
was run. The numbered root remains required by activated receipts and older
audit scripts, so this does not authorize renaming or deleting it. The
contracts bound to the activated receipts remain historical code snapshots;
the post-activation Stage 10 code and its focused checks are pinned in
`docs/behavior_output_code_contracts/stage10_semantic_discovery_20260924.csv`.

From the repository root, rerun the check with:

```powershell
Rscript Testing/audits/audit_stage10_semantic_discovery_parity.R `
  'S:\Lab_Member\Tobi\Experiments\Exp9_Social-Stress\Analysis\Behavior\RFID'
```

### Stage 14 residual inventory after activated cutovers

The exact activated dashboard and RFID-audit plans, plus the 700-row Stage 14
ownership snapshot, are now retained in
`docs/behavior_output_activated_plans/`. They are byte-identical to the
reviewed task artifacts. `Maintenance/Test-BehaviorStage14ResidualInventory.ps1`
checks their group hashes against the activation receipts, rehashes every
planned original and copy, checks each semantic target inventory, and
classifies the files still present only in the numbered Stage 14 tree.

The live check found 700 numbered files. Activated plans cover 371 originals
across ten groups. The 329 retained-only files comprise 183 older HMM audit
files, 132 byte-identical QC mirrors of planned authored figures, one QC
README, and 13 manifests or other records. The 183 older audit hashes match
the separately retained per-file writer map. This is an inventory and
provenance boundary, not a decision to promote the old HMM results or copy
the remaining files. The HMM audit has a separate current revalidation run;
the 13 other records include historical manifests and pre-fix evidence that
must retain their original lineage.

```powershell
& .\Maintenance\Test-BehaviorStage14ResidualInventory.ps1 `
  -AnalysisReadyRoot 'S:\Lab_Member\Tobi\Experiments\Exp9_Social-Stress\Analysis\Behavior\RFID\analysis_ready'
```

### Stage 01 foundation residue

The reviewed 20-file Stage 01 plan and 52-row ownership snapshot are now
retained in `docs/behavior_output_activated_plans/`. The live receipt and an
independent source/destination hash check passed. The numbered
`03_derived_metrics/` tree still has 52 files: 20 activated Stage 01
originals and 32 retained-only files, split into eight cross-scale identity
reports, fourteen Stage 19 spatial originals, and ten Stage 01 metadata files.
`Maintenance/Test-BehaviorFoundationResidualInventory.ps1` repeats the
read-only hash, receipt, and ownership checks:

```powershell
& .\Maintenance\Test-BehaviorFoundationResidualInventory.ps1 `
  -AnalysisReadyRoot 'S:\Lab_Member\Tobi\Experiments\Exp9_Social-Stress\Analysis\Behavior\RFID\analysis_ready'
```

For the three largest numbered behavioral roots, current plan coverage is:

| Numbered root | Total files | Activated copies | Retained-only files |
| --- | ---: | ---: | ---: |
| `03_derived_metrics/` | 52 | 20 | 32 |
| `06_behavioral_dynamics/` | 1,460 | 1,459 | 1 historical Stage 15 map |
| `12_systems_neuroscience_summary/` | 700 | 371 | 329 |

The numbered originals are still required by activation receipts and some
historical readers. This inventory does not authorize moving or deleting
them.

### Gate before removing numbered folders from the top level

The current receipt resolver checks that both the semantic directory and the
retained source directory still exist. A separate archive receipt can now
select an exact `history/original_layout/` source location, but no such receipt
exists on the live tree. The separate transaction tool is fixture-tested and
requires a reviewed audit-reader gate that is not yet satisfied. An archive
move without those gates would still break active path resolution. A
coarse repository text search
also finds numbered-path references in 16 audit scripts for
`06_behavioral_dynamics/`, 34 for `12_systems_neuroscience_summary/`, and 28
for `03_derived_metrics/`. These counts include historical replay scripts;
they are not a count of active scientific consumers. Each script needs an
explicit current-reader versus historical-replay decision before its path is
changed.

The executable references have different roles, so a blanket text replacement
would be unsafe:

| Reference | Current role | Archive implication |
| --- | --- | --- |
| `Functions/project_paths.R::mmm_behavior_output_layout_state()` | An activated group requires both its semantic target and retained source. An exact-root archive receipt can select a retained source under `history/original_layout/`. | No live archive receipt exists; moving a root before one is prepared and activated would fail closed. The separate archive transaction and reader checks remain mandatory. |
| `Analysis/10_systems_feature_prediction_ladder.R::stage10_scan_feature_paths()` | Stage 10 scans 18 receipt-selected semantic groups and sorts by virtual numbered path to preserve candidate precedence. The compatibility router remains in the script, but the group scan no longer needs the numbered `06_behavioral_dynamics/` root. | The 606-path parity check covers the current live discovery set; Stage 10 discovery is **not** a blocker for that root's retirement. Preserve the compatibility router until remaining fallback inputs are reviewed. |
| `Analysis/14_systems_neuroscience_summary_dashboard.R` | The five-minute writer uses the activated semantic root; its other-resolution branch still constructs `12_systems_neuroscience_summary/<resolution>`. | Preserve the numbered fallback for any non-primary resolution until that branch has an explicit policy and test. |
| `Analysis/_pipeline_setup.R::resolve_stage09_early_prediction_artifact()` | An explicit canonical-then-legacy lookup for Stage 09; the old candidate is under `06_behavioral_dynamics`. | Retain the fallback contract or explicitly retire it after verifying that no supported replay needs it. |
| `Analysis/run_cookiehab_preprocessing_and_metrics.R` | Explicit output overrides under the separate `cookiehab/analysis_ready/` tree. | Do not rewrite these to main-experiment semantic paths; audit the cookie-habituation tree separately. |
| `Testing/audits/audit_first_night_*.R` and `audit_hmm_state_architecture_*.R` | Many scripts construct numbered input paths, and some create output directories inside the old Stage 14 HMM audit tree. | Treat as historical replay scripts until each script's input lineage and write destination are reviewed. Running one after a root move could recreate the old root or write a scientifically mixed audit. |
| `Analysis/16_manuscript_behavior_report.R` | Numbered paths in the output registry are explicit historical provenance fields. | Keep these historical strings as provenance; update only the active canonical path when its own registry contract changes. |
| `Analysis/27_build_behavior_main_figure.R`, `Analysis/02_build_dyadic_rfid_contacts.R`, `Analysis/10_systems_feature_prediction_ladder.R`, and `Analysis/15_behavior_proteomics_integration.R` | The matching numbered strings are source labels or comments, not live writes to those roots. | Review them as documentation/provenance, not migration blockers by themselves. |

This classification is based on the executable code as checked on 2026-09-24;
it is not a claim that every manual audit script is safe to rerun. In
particular, retaining the old Stage 14 HMM audit tree is still necessary for
historical replay, and the current HMM revalidation has its own separate
semantic output directory.

The safe archive sequence is: (1) classify those readers and preserve the
ones intentionally tied to the original path; (2) add a versioned archive
manifest and an independent receipt state that can attest to a relocated
source without rewriting the activation receipt; (3) test failed and
interrupted archive transitions with missing, extra, and changed files; (4)
verify source and archive hashes and all reader paths on the live tree; and
(5) only then consider moving a numbered root. This is a separate migration
from the completed semantic-copy cutovers. None of the numbered roots has
been moved or hidden. The proposed destination, separate receipt states,
interruption checks, and reader/writer gates are specified in
`BEHAVIOR_NUMBERED_ROOT_RETIREMENT_DESIGN.md`.

On 2026-09-24, two HMM audits were rerun against current inputs in the
separate `analyses/hmm_revalidation_runs/current_stage08_review_20260924/`
directory. The old 183-file HMM audit tree remains in its numbered location
as historical evidence. Navigation and output-index entries now distinguish
the two lineages; neither is a migration receipt for the old tree.

| Current `analysis_ready/` root | Live contents | Producer and active consumers | Role and classification | Number and next action |
| --- | --- | --- | --- | --- |
| `03_derived_metrics/` | 52 retained originals, 3.16 GB; 20 current Stage 01 files copied to `foundations/behavior_metrics/` | Current stages read the semantic copy; historical manual audits retain the numbered paths | Canonical **foundations** copy plus historical audit/spatial provenance | `03` is historical, not Stage 01's identity. Keep the entire numbered root; identity and spatial outputs have separate owners. |
| `06_behavioral_dynamics/` | 1,460 files, 18.19 GB, including all retained originals of activated groups | Stages 02, 04–08, and 15 produced various branches; older resolution branches still have fallback or optional readers | Mixed active historical inputs and retained provenance | `06` is a historical namespace. Do not rename the root while old Stage 15 temporal/GAMM inputs and resolution fallbacks still point inside it. |
| `12_systems_neuroscience_summary/` | 700 files, 208.75 MB; 20 first-night files already copied, 183 HMM audit files, 301 figure files, 114 table files, other audit families | Stage 14 writes the dashboard; manuscript registry and many audit scripts read specific tables and audit outputs | Exploratory systems output, manuscript candidates, and audit evidence | `12` does not identify Stage 14. Partition by actual producer and reader before a cutover; do not move the entire root as one group. |
| `00_qc_tracking_integrity/` | 8 historical files, 1.15 MB, dated 2026-05-22 | New provisional Stage 00 diagnostics are under `quality_control/tracking_integrity/`; Stage 16 and release still read two optional May tables | Historical technical QC and optional release source | `00` matches the stage but is not useful to a reader. The May set lacks current producer outputs and proved lineage to September Stage 01; do not label it a complete current run. |
| `13_nonlinear_systems_dynamics/` | 97 files, 39.25 MB; run manifest dated 2026-09-21 | Manual `_supporting/13_nonlinear_systems_dynamics.R`; Stages 10, 14, and 15 reference its features; Stage 15 records loading its feature matrix | Supporting exploratory input | `13` names an older supporting script, not current Stage 13. A bounded copy is plausible after testing Stage 10's single-root feature scan and the Stage 14/15 readers. |
| `14_nextgen_behavioral_phenotyping/` | 138 files, 78.79 MB; run manifest dated 2026-09-21 | Manual `_supporting/14_nextgen_behavioral_phenotyping.R`; Stages 10 and 14 reference its features | Supporting exploratory input | `14` names an older supporting script, not the current Stage 14 dashboard. A bounded copy is plausible after the same feature-scan and reader checks. |
| `15_behavioral_adaptation_kinetics/`, `16_sleep_like_inactivity_metrics/`, `17_ethological_phase_organization/` | Only the older five-minute branches remain uncopied: 10, 10, and 13 files | Current Stage 11–13 producers declare ten minutes; Stage 15 integrations loaded eight five-minute tables | Historical, scientifically suspect Stage 15 inputs | Prefixes `15–17` are historical. Keep in place; resolve the pre-fix phase-classifier validity issue before treating them as current inputs. |
| `03_primary_raw_movement_phase_stats/` | 31 files, 2.92 MB | Historical Stage 03 output; comparison helpers retain explicit old-baseline access, while Stage 16 selects canonical files only | Superseded, retained comparison provenance | `03` reflects the stage but this is no longer its write location. Keep the historical set for comparison; current reporting must not fall back to it. |
| `04_model_outputs/`, `05_figures/` | 6 model files and 5 figure files, all spatial occupancy originals already copied | Stage 19 owns them; Stage 10 boundary audit checks they are excluded from feature discovery | Retained provenance | Numbers are historical storage labels. Keep source paths as the activation receipts require. |
| `16_manuscript_behavior_report/` | 5 files, 0.44 MB | Old Stage 16 exporter; current manuscript entry point and path-length check use `manuscript/behavior/` | Superseded report | `16` matches the former stage but is redundant in navigation. Retain as comparison evidence; no current write should target it. |
| `18_raw_movement_publication_trajectory/`, `18b_raw_movement_broad_phase_stats/`, `18c_raw_movement_broad_phase_stats_corrected/` | 57, 8, and 21 files | Archived or absent producers; no active scientific reader found in `Analysis/`, `Functions/`, `Testing/`, or `Maintenance/` | Superseded movement lineage | `18*` is historical. Retain unchanged until an archival action is explicitly designed with a manifest and no-read check. |

## Audit findings and remaining migration order

The Stage 14 output manifest is not a complete inventory: it declares 49
distinct table/figure files, one of which is absent
(`figures/publication_panels/Fig_behavior_proteomics_bridge.svg`); 652 of the
700 live files are not listed in that manifest. Its
`audit_hmm_state_architecture/` subtree alone has 183 files and references
in 34 audit scripts, several with hard-coded output paths. The manifest
cannot be used as an automatic approval list for that whole root. Stage 00
likewise has no live input/output manifest, and the current producer names
additional products absent from its eight-file May tree.

The two supporting trees have recent run manifests, but each output manifest
has only nine summary rows while the live trees contain 97 and 138 files.
Their scripts run manually outside `run_all_analysis.R` and currently set a
machine-specific repository path. Stage 10 recursively scans each old root
for feature files: 26 CSV candidates in the nonlinear tree and 55 in the
next-generation tree. Adding a semantic copy as an additional scan root
would duplicate candidates. A future cutover must replace its selected root
through one receipt-aware accessor, validate the candidate-file set before
and after, and update Stage 14 and Stage 15 readers. No such change was made
in this read-only audit.

1. Resolve the Stage 15 five-minute source-validity choice separately from
   folder work. Keep existing integrations frozen; do not regenerate them as
   part of a path migration.
2. Audit Stage 14 into independently reproducible dashboard tables, figures,
   and named audit families. Inventory exact writers and all manuscript/audit
   readers before proposing any one group for the migration tool.
3. Audit Stage 00 QC producer parity and release-reader contracts. Its small
   directory is not enough evidence for a safe automatic cutover.
4. The two manual supporting analyses have completed this step: the 26/55
   Stage 10 candidate sets and Stage 14/15 reader paths were checked before
   and after the receipt-controlled activation. The nine-row output manifests
   were not used as complete file inventories.
5. Stage 01's 20 current metric/QC files were copied after active readers
   resolved a receipt-controlled semantic path. Keep the old files available
   for historical runs throughout.

The `analyses/` directory is the active human-facing navigation layer today.
The numbered source folders remain in place for verified historical paths,
fallback readers, and provenance. No root-level rename or deletion is implied
by the semantic copies.

The 2026-09-24 Stage 01 foundations pass confirmed six resolution-specific
metric tables and six duration-QC companions, eight Stage 01 QC products,
ten generated metadata/folder-guide files, eight separate cross-scale
identity-audit outputs, and fourteen retained Stage 19 spatial originals in
`03_derived_metrics/`. A targeted code search captured 98 matching
path/configuration lines across current stages, manual audits, tests, and
historical code. The cookie-habituation runner sets a separate Stage 01
output override. A semantic `foundations/behavior_metrics/<resolution>/`
layout now uses the `foundations/behavior_metrics/` root selected by its
activation receipt. Stage 01's default writer and current readers resolve
through that accessor; the explicit cookie-habituation override remains
separate. Current Stage
03–09, 11–15, 20–25, 28, two supporting analyses, Stage 00 QC, the
first-night driver, four current audits, and live-data regression tests now
resolve Stage 01 inputs via that accessor. Historical audit scripts retain
their old paths. The August
26 cross-scale identity audit files predate the September 22 metrics and
must not be promoted as part of the current Stage 01 copy. A reviewed
52-file ownership manifest now separates the 20 Stage 01 scientific/QC files
from ten generated navigation/manifest files, eight older identity-audit
files, and fourteen Stage 19 spatial originals. The ten generated files stay
in the retained numbered tree: their run provenance and folder guides describe
that original output, and the migration receipt describes the verified
copy. A genuine later Stage 01 run can write fresh semantic-root manifests.
A 20-row, hash-pinned ready plan and 43-file code contract are recorded in
the task's foundation boundary report. Inspect, Prepare, Verify, and Activate
passed. A subsequent audit pass classified 30 direct numbered-path
manual forensic scripts as historical, with no pipeline caller found. Five
current audit readers already use the receipt-aware root. The optional
cross-scale identity validator was the remaining current reader/writer: it
now reads that root and writes future reports to the separate
`analyses/cross_scale_identity_validation/` directory, preserving its eight
August reports under the old root. The foundation copy was activated without
moving or rerunning historical audits. Historical audit lineage still blocks
presenting those old results as current-run evidence.

## Historical movement and report roots — 2026-09-24 follow-up

A fresh hash inventory covers the 86 files across the three `18*` movement
roots and the 36 files across the old Stage 03 and Stage 16 roots. The two
archived `18`/`18b` producers and the absent `18c` producer support a
historical classification; no explicit active code reader of those three
roots was found in the searched analysis, function, test, maintenance, or
manuscript code. That bounded search does not exclude external links. Stage
16 previously had executable per-artifact fallbacks into the old Stage 03 tree,
but those have now been retired after the eight source pairs were checked.
All eight canonical and historical tables exist and all eight pairs differ by
SHA-256; the current manuscript provenance selected all eight canonical files.
A missing current Stage 03 source now fails if required or remains explicitly
missing if optional. Historical comparison helpers still read the old baseline
by an explicit path. A path-length regression test still scans the old Stage 16 tree. The
task's blocked candidate maps propose semantic `history/` paths; all 122
numbered originals remain and no copy or receipt was created.

Stage 16's nine Stage 09 manuscript sources also selected canonical paths in
the current provenance. Their declared fallback directory,
`06_behavioral_dynamics/early_prediction_model_ladder/10min_based/`, is absent
from the live tree. Stage 16 no longer offers those nonfunctional fallbacks;
required Stage 09 sources fail closed when a canonical file is missing. This
change leaves the separate Stage 09 resolver used by other analyses intact.

The path-length regression now resolves the active Stage 15 semantic base and
the current `manuscript/behavior/` directory through their writer helpers. Its
overlong old Stage 15 path remains as a synthetic regression example only;
it no longer uses numbered report files to judge today's output budget.

The live 54-row `output_index.csv` was refreshed as navigation metadata after
the Stage 03/09 reader and Stage 15 source-contract corrections. A read-only
comparison found exactly eight changed cells: the Stage 09 status and notes
for Stage 03, Stage 09, three historical phase branches, and two Stage 15
integration rows. No row identity or path changed. The prior file is retained
under `_migration_control/` with SHA-256
`6D9EFE910ADE4C35FC9A37BC94183B1B15E94998E21DC6B628B0B03464361AF1`;
the refreshed index is
`EE7DC09EE2F0E03ECF700F77613575082AE71D4B488AE2A854BBF2190A4340FF`.
The post-refresh comparison reports zero drift from the repository definition.
No Stage 16 manuscript product or scientific output was regenerated.

A later direct-path sweep found that the literal
`06_behavioral_dynamics/gamm_trajectory_features/` branch does not exist.
Stage 14 now resolves its GAMM reader and provenance paths through the
receipt-aware Stage 07 group; its ten-minute source matches the numbered
original by SHA-256. Stage 15 no longer lists the absent branch as a fallback,
but its separate thirty-minute GAMM input still deliberately resolves within
the retained numbered `gamm_features/` tree. Stage 10's required Stage 09
model input now accepts only its canonical pipeline path; the removed
`early_prediction_model_ladder/` fallback is absent on disk. These code
edits have not rerun Stages 10, 14, or 15.

Stage 10 still discovers optional files in the broader numbered
`06_behavioral_dynamics/` root. Its September 22 source audit listed 536
candidate files there, of which 205 have activated file-level migration-plan
targets. Every mapped target exists and matches its retained original in size;
55 smaller files marked loaded by that audit also passed fresh source, plan,
and target SHA-256 checks. The other large loaded files were not rehashed in
this pass. The audit marked 85 unmapped files as loaded across historical
30-minute GAMM, older social-network and state-space, and 1-/5-minute
temporal-instability branches. Its former loaded flag used only a basename
match, so those counts are labels from that historical audit, not an exact
read log. The task deliverable
`stage10_numbered_06_source_reconciliation_20260924.csv` records each path.

Stage 10 now replaces each activated source path in place after discovery and
self-ingestion filtering, using the checked file-level migration plan and
receipt; unmapped historical paths stay in order. A missing or renamed
activated target stops the run. On the September 22 audit's 611 candidate
paths, this rewrites 280 paths in total, including 205 inside numbered `06`,
without changing order, basenames, or file sizes. Its future source audit now
tracks exact candidate paths instead of treating every file with a shared
basename as loaded. Stage 10 models have not been rerun or compared numerically
after this routing change, and the numbered root remains necessary for the
unmapped historical branches.

A read-only scan on 2026-09-24 found 602 CSV/TSV/Excel files under the
numbered `06_behavioral_dynamics/` search root, compared with 536 paths in
the September 22 Stage 10 source audit. All 66 additional paths have one of
four basenames: `input_output_manifest.csv`, `output_figure_inventory.csv`,
`output_folder_summary.csv`, or `output_manifest.csv`. Stage 10's existing
self-ingestion guard excludes all four before feature routing. The 66 paths
include 45 with migration-plan targets and 21 retained numbered-only metadata
files. A scan of the semantic group roots found no CSV/TSV/Excel file outside
the reviewed migration plan. This checks current discovery and metadata
exclusion, not the numerical equivalence of a Stage 10 model rerun.

The current scan also sees the expected 26 nonlinear-dynamics and 55
systems-phenotyping candidates through their activated semantic roots.

A header-only follow-up screened all 331 unmapped numbered `06` CSVs without
reading their approximately 3 GB of table bodies; no header read failed.
Exactly 85 have the `AnimalNum` column required by Stage 10's feature reader,
and the other 246 cannot pass that first check. The 85 divide into 36 older
social-network files (nine each at 10 seconds, 1 minute, 10 minutes, and
30 minutes), 26 temporal-instability files (13 each at 1 and 5 minutes),
18 state-space files (nine each at 1 and 10 minutes), and five 30-minute
GAMM-feature files. All 85 coincide with paths labeled loaded in the saved
September 22 source audit. This schema check supports that audit label but
does not prove which numeric columns survived its full read, aggregation,
and leakage filters. These historical resolutions therefore remain explicit
Stage 10 dependencies until their file-level migration and model-input parity
can be validated; the numbered root cannot yet be removed from discovery.

The nine source resolution folders holding those 85 potential inputs contain
864 files (3.55 GB) in total, dated May 18–19, 2026. Each has an old-format
`output_manifest.csv` naming a predecessor script, but together the manifests
declare only 59 file rows and 15 of those declared files are absent. These
manifests therefore cannot serve as complete migration inventories or current
producer proof. The task report
`stage10_historical_resolution_inventory_20260924.csv` records each folder's
actual count, size, dates, manifest producer, missing declarations, and
`AnimalNum` candidate count. Any historical-resolution copy needs a fresh
file enumeration and hashes, a named provenance role, and a Stage 10
candidate-set check before its receipt is activated.

A read-only draft file map now covers all 864 files with source SHA-256,
byte size, UTC modification time, and distinct proposed targets under
`analysis_ready/history/<family>/<resolution>/...` (`10sec`, `1min`,
`5min`, `10min`, or `30min` as applicable). Its task artifact is
`stage10_historical_resolution_file_map_20260924.csv` (SHA-256
`009EAB5881C050C2BE04307A3C4441F6A9F87067A4900E0F6C5329B5D41EB2C1`).
It includes all 85 `AnimalNum` candidates; no proposed target exists yet.
This is a migration proposal, not an activated plan or proof that the older
outputs are reproducible with the current scripts. The next gate is a
consumer and candidate-set parity test using explicit historical-root
selection, followed by a copy/hash check. No historical file was moved or
copied during this inventory.

The consumer boundary extends beyond Stage 10: Stage 14's multi-resolution
optional imports can resolve older state-space, social-network, temporal, and
GAMM paths through `Functions/project_paths.R`. Stage 15's current source
specification explicitly prefers the retained five-minute temporal table and
the 30-minute GAMM table. Stage 16 records these resolution families as
historical groups. Current Stage 04–07 producer defaults select 10-second,
5-minute, 5-minute, and 10-minute outputs respectively, while the May
manifests name predecessor scripts. An eventual cutover must select the same
historical receipts for both the resolution helpers and Stage 10 candidates;
changing only Stage 10's scan would leave optional readers on numbered paths.

The draft 864-file history map passed the separate read-only
`Maintenance/Test-BehaviorHistoricalOutputMap.ps1` check on 2026-09-24:
nine complete source roots, 3,546,084,818 source bytes with matching SHA-256,
unique proposed paths, and no existing destination root. The checker does not
activate the map; the existing activation tool deliberately rejects `history/`
destinations. Its synthetic fixture also passed the changed-source,
unplanned-file, wrong-resolution, and existing-destination cases.

The nine proposed historical roots now have receipt-aware entries in
`Functions/project_paths.R`. The social-network, state-space,
temporal-instability, and GAMM resolution helpers follow those entries, and
Stage 10 rewrites any activated historical candidate in place after its
current-group routing. Its live read-only scan still found 683 candidate
paths and zero historical rewrites because no history receipt or destination
exists. Temporary-folder tests cover the unchanged current path, an
unreceipted semantic copy, activated selection, and missing or size-mismatched
copies. Stage 10 still discovers candidates from the retained numbered root;
this change prepares reader cutover but does not permit that root to be
renamed or removed.

The migration engine now has an explicit `-HistoricalDestination` mode for
the same nine source/destination roots. The normal mode still rejects
`history/`, and the historical mode retains the ready gate, code contract,
source enumeration, SHA-256, and receipt checks. Its temporary blocked-to-
activated fixture passed. The live draft map has not been submitted to
`Prepare`, `Verify`, or `Activate`.

The reviewed draft is also encoded as a separate blocked repository plan:
`docs/BEHAVIOR_HISTORICAL_MIGRATION_PLAN.csv` has 864 rows in nine groups,
all `gate=blocked_review`, with no duplicate source or target. Its eleven-file
code contract has matching live hashes. Read-only `Inspect` passed for every
group: 92, 92, 92, 88 social-network files; 61 and 95 state-space files;
125 and 210 temporal-instability files; and nine GAMM files. No staging or
destination directory exists. The plan is not yet eligible for `Prepare`.
The shared code contract was repinned after the Stage 16 index change; all
864 plan references match it.

Stage 16 and the live `output_index.csv` now retain the four aggregate
historical family rows and add nine receipt-aware resolution detail rows.
The live index changed from 54 to 63 rows: nine additions and four overview
note edits, with no other previous cell changed. A hash-checked backup is
`_migration_control/output_index_before_history_detail_20260924.csv`
(SHA-256 `EE7DC09EE2F0E03ECF700F77613575082AE71D4B488AE2A854BBF2190A4340FF`);
the refreshed index is
`5C438F0F041A17087C5758E89B351B62CC19398E006D8166FAA78F51F33E0546`.
A post-refresh definition comparison found zero drift. The release builder
has no direct read of those nine old resolution paths. This was a navigation
metadata refresh, not a scientific or manuscript rerun.

A read-only Stage 10 candidate-order simulation applied the blocked history
plan after the script's actual discovery and self-ingestion filters. The
current filtered list has 607 paths; 330 are CSVs in the nine proposed
historical roots, including all 85 with `AnimalNum`. In-place replacement
preserves every basename and ordinal position, has no duplicate destination,
and matches the pinned source sizes. The task report
`stage10_history_candidate_path_parity_20260924.csv` records those 330
positions (SHA-256
`57C289E2186186BB683F1F26C0E0B434996913857916C656246A6F5FA21342DC`).
The 331st unmapped numbered CSV is the root-level
`proteomics_integration_output_dir_map.csv`, outside all nine resolution
groups. This simulation verifies path-set shape only; content parity of
future copies and Stage 10's numerical outputs remains untested.

## Stage 00 QC follow-up and writer safeguard

The live folder has eight files, all dated 2026-05-22: three tables, one XLSX
report, and four SVG figures. Both tables used by Stage 16 and the release
builder are present. The current `Analysis/00_qc_tracking_integrity.R` writes
two further products unconditionally: `rfid_tracking_qc_methods_note.txt` and
`figures/qc/qc_suggested_decision_summary.svg`. Neither is present in this
tree. It also conditionally writes a rolling-QC figure and a skipped-input
table, and calls `write_output_manifest` when available; no manifest exists
in the live tree. The folder therefore cannot be represented as a complete
run of the current producer. Its release-table role is real, but producer
parity remains unresolved. Keep its path unchanged until a versioned run
lineage and reader contract can be established without overwriting these
May outputs.

The current Stage 00 writer is now routed to
`analysis_ready/quality_control/tracking_integrity/` for any future run, so
it cannot overwrite the retained eight-file May snapshot. At the time of that
writer safeguard, no new run existed; see the 2026-09-24 update below. Stage 16 and the release builder still read the two
optional May tables from the numbered root. Their source code now labels
those inputs as historical with unverified current-input lineage; neither
promotes them to automatic exclusions. The existing September 22 Stage 16
manuscript products were not regenerated and do not acquire those revised
labels from this code edit. A read-only release dry run resolved 45 artifacts, with 32
required and 13 optional, and passed its 22-file Stage 16 hash cross-check.
The live 51-row output index was changed only in the Stage 00 notes field,
after backing up its predecessor. This is a writer safeguard and provenance
label, not a scientific QC refresh or path activation for the old files.

The current Stage 01 5- and 10-minute metric files have September 22
modification dates, after all eight May 22 QC files. No input hash or run
manifest accompanies the May QC snapshot. Stage 16 and the release builder
still read two of its tables as optional diagnostic sources. Their role is
real, but the eight files cannot be described as a complete current run or
as verified against the present Stage 01 inputs. The task deliverables now
include an eight-file hash inventory and blocked candidate historical map.

## HMM registry publication boundary

The conditional active HMM registry row has a separate publication boundary:
it cites two September 3 audit files while the current 10- and 5-minute HMM
input-provenance manifests were generated September 22. The release builder's
dry run copies the registry but does not include these HMM audit files in its
artifact plan. Its successful file-hash checks therefore do not establish
that the conditional HMM claim was revalidated against the current Stage 08
run. The registry claim and audit results were left unchanged; publication
use needs recovered exact input lineage or a reviewed scientific rerun.
The two registry-linked audit scripts now have a guarded code-only rerun
route: they require an explicit new run folder under
`analyses/hmm_revalidation_runs/`, read the active Stage 01/08 inputs, and
reject output filename collisions. A fixture verified these path and
overwrite guards. The scripts have not been run, and no revalidation folder
or scientific output was created. Their future results cannot be promoted
without statistical and input-provenance review.

## Stage 14 tree follow-up, read-only

The 700-file `12_systems_neuroscience_summary/5min_based/` tree contains 301
figures, 114 table files, 21 statistical-table files, 20 first-night files,
183 HMM-architecture audit files, and seven other named audit families.
The first-night files already have separate activated semantic copies. The
HMM audit subtree has independent writer scripts in `Testing/audits/`, some
with hard-coded paths to this tree; manuscript registry rows also point to
specific audit files. These families cannot inherit one Stage 14 receipt or
be treated as one regenerable dashboard output. The next safe unit of work is
a file-level producer/reader map for the dashboard tables and figures, with
each audit family inventoried separately. No Stage 14 file was moved here.

The follow-up file inventory and 947 code-reference lines are saved as
`stage14_file_inventory.csv` and `stage14_code_reference_evidence.csv` in the
task's deliverables. The current figure-classification helper now ignores
ancestor directory names and preserves an existing figure category; its
focused fixture and path-length test pass. The live September figure index
remains historical and has not been regenerated. The three files under
`audit_inactive_phase_qc/` match one manual audit's three write sites. This
family has now been activated as `analyses/inactive_phase_qc_audit/` after
receipt-aware writer, registry, and exact hash-plan checks. The numbered
originals remain and the proposed QC rule remains unadopted.
At that checkpoint, the six `audit_rfid_*` families still needed separate
writer and cross-reader review, so they were excluded from the dashboard
batch. All six were later activated independently as described below.

The five-minute Stage 14 dashboard now has a separate receipt-aware path
contract, `systems_dashboard_5min`, targeting `analyses/systems_dashboard/5min/`.
Its producer, Figure 1 candidate builder, two registered source-data readers,
and Stage 16 output-index row follow that contract. The activated receipt now
resolves the live producer and current readers to the semantic directory.
The dashboard group must exclude the independently routed first-night files
and all manual audit families. Its table, statistical-table, figure, and
manifest inventory was reviewed before the bounded plan was marked ready.
Stage 14 itself has not been
rerun; in particular, no claim is made that the existing five-minute tables
reflect the corrected Stage 11-13 inputs.
The migration tool now accepts a reviewed, hash-pinned ownership manifest for
a shared source root. It still compares every file under that root with the
manifest and requires each selected source to have a `ready` owner matching
the requested group. The 700-row Stage 14 ownership draft covers the live
tree exactly. Its narrowed dashboard candidate set is 293 files: 109 tables,
20 statistical tables, 147 authored plot files, and 17 interactive assets.
Another 132 figure files are byte-identical mirrors and remain in the numbered
tree. Four scientific files are historical evidence, seven index/README files
were regenerated in the semantic copy, and three manifests record the old root.
The 293-file hash plan passed read-only inspection, including full source-tree
coverage. The selected ownership rows and plan gate were set to `ready` for
activation. The
separate first-night and audit families retain their own ownership.
The old pre-HMM-identity-fix effects file is now read explicitly from the
retained numbered root; Stage 14 no longer creates a supposed pre-fix file
from a corrected current result. Stage 27's optional broad-domain Source Data
label uses the resolved input path. Its full read-only contract test passed.
The exact-basename consumer scan is lower-bound evidence: eleven historical
manual audit scripts still reference selected dashboard artifacts. Ten also
write outputs into the numbered first-night or HMM-audit trees; the remaining
`audit_first_night_window_provenance.R` prints a cross-source comparison
without writing a file. Their source hashes, dashboard basenames, and matching
reference lines are in the task deliverable
`stage14_historical_manual_audit_boundary_20260924.csv`. One further script,
`audit_inactive_phase_qc_redesign.R`, already uses its separate activated audit
root. The eleven historical scripts mix retained Stage 14 paths with older HMM
and audit inputs, so pointing only their dashboard reads at the semantic copy
would create an unreviewed mixed-lineage result. Keep them pinned for replay;
any current-input rerun needs a separate reader, writer, and scientific review.

The September 23 file inventory's manuscript/release flags were based on
basenames and are a historical lower bound, not proof that the current readers
use the numbered copies. A fresh path check found that the release builder
resolves its three first-night audit-basename matches through the activated
`first_night_10min` group, while the two formerly flagged longitudinal HMM
registry artifacts now point to the separate current-input revalidation run.
The two first-night dwell stability files remain referenced by an `EXCLUDED`
registry row as retained historical evidence. The registry's five-domain
panel row also now names its activated first-night contrast path; its numbered
original and semantic copy have the same SHA-256. No claim or statistic was
changed by that metadata correction.
The migration tool now regenerates the seven derived figure-index and
folder-guide files after copying the authored dashboard products and before
activating the receipt. An isolated fixture covered an interrupted move with
a partially written index and verified recovery, source retention, the
complete destination inventory, and that every indexed figure exists.
This uses `copy_figures = FALSE`, so the old duplicate mirrors do not reappear.
The 293-file dashboard copy was activated after the reviewed gate was set to
`ready`. The live semantic directory contains the 293 hash-matched files and
seven regenerated metadata files. The activated receipt selects it for current
readers; all 700 non-cache source files remain in the numbered tree. The old
manifests, duplicate figure mirrors, and independently written audit families
were not copied. The eleven historical manual audit scripts still require
separate reader and writer migration before a new run can be described as a
current-dashboard audit.

## Separate RFID audit families

All six RFID audit groups are now activated as independent semantic copies:
4 comparison, 12 leading-bin, 11 construct, 11 conservatism, 10 alternative
inference, and 7 reliability files. All 55 numbered originals remain. This
path cutover did not rerun any scientific audit or promote an exploratory
result to a manuscript claim. The live index had 51 rows at that cutover; it
was subsequently refreshed to 54 rows. See the activation
record and separate plans for exact hashes and receipts. The paragraphs below
also preserve the sequence of the earlier, separate cutovers.

The six `audit_rfid_*` folders contain 55 original files in the numbered
root. They are separate from the Stage 14 dashboard and from one another.
Their scripts write into distinct directories; no manuscript release builder
was found reading those directories. The leading-bin audit has a direct
contract-test reader for two files and a documented interpretation role.
The other directory-name matches are predominantly writer paths or generic
basenames and do not establish active downstream consumption.

The four-file legacy-versus-four-domain comparison was activated as a separate
bounded group. Its one audit script names all four writes and reads the
activated first-night tables plus four local Stage 28 tables. A receipt-aware
writer, Stage 16 index row, path fixture, five-file code contract, and exact
source-hash plan are in place. Inspect, Prepare, Verify, and Activate passed;
independent rehashing found four matching originals and copies. The receipt
selects `analyses/rfid_domain_comparison_audit/`, the live index has 46 rows,
and no staging directory remains. The scientific audit was not rerun. Its
scientific comparison remains supporting and does not promote a manuscript
claim. The other five families were reviewed and activated in later separate
cutovers; the preceding 46-row index describes this comparison cutover only.

The 12-file leading-bin seed sensitivity audit was activated separately under
`analyses/rfid_leading_bin_seed_audit/`. Its writer and the Stage 28 contract
test use the receipt-aware path, and Stage 16 has a supporting-audit index
row. All 12 copied hashes match their retained originals. The Stage 28
contract test passed against the semantic copies. The four other RFID audit
families were activated afterward.

Those four independent families now have separate receipt-aware writers and
Stage 16 rows: `rfid_construct_audit` (11 files),
`rfid_conservatism_audit` (11), `rfid_alternative_inference_audit` (10),
and `rfid_reliability_audit` (7). Each live basename is present in its
identified writer. Four distinct code contracts and a 39-file hash plan
passed read-only Inspect, with no destination or staging directory at that
time. The user approved all four; each then passed Inspect, Prepare, Verify,
and Activate. All 39 originals and copies were rehashed against the approved
plan, with exact destination counts, activated receipts, and no staging. No
scientific audit was rerun. The separate HMM audit tree still requires a
file-level scientific and path review.

The HMM follow-up inventories 183 retained files: 86 under
`first_night_domain_heatmap/` and 97 at the audit root. Exact-basename
search found 402 reference lines across 43 code files; direct writer
matching remains incomplete, particularly for dynamic or multiline writes.
At the original snapshot, four files were named in the manuscript registry
across conditional active, unpromoted inactive, and excluded first-night or
occupancy claims. The current registry retains old paths only for the two
excluded first-night dwell files; the HMM revalidation rows use new paths. Neither
partition is yet a migration group. No HMM destination or receipt was created.

The entire HMM audit tree has September 2–4 modification dates, whereas the
current Stage 01 metrics and Stage 08 input-provenance manifests date to
September 22. The old and semantic Stage 08 assignment copies match each
other, but they are the newer run. Thus the existing audits cannot be labeled
as validated against the current HMM inputs from the observed files alone.
The four registry-linked files form four producer units totaling eight files
when their companion outputs are included; a blocked candidate map is in the
task's HMM boundary report. A scientific provenance review is required before
any HMM copy is presented as current analysis evidence or its registry path
is changed.

A further file-level writer review covers all 183 retained files in the task
deliverable `stage14_hmm_audit_writer_map_20260924.csv` (SHA-256
`2F87CCAE5536F57F7D760D74A88BDB68FACF8A473B01C443F415FFA8556CE29D`).
There are 144 single-writer same-line matches and 32 writer candidates
supported by a nearby multiline call, assigned output variable, or figure
helper. Seven filenames have two explicit writer scripts. Six collisions are in the
first-night audit generations; the seventh is
`phaseA_issue4_gapaware_contrast_comparison.csv`. The map records code
references as well as the previous source hashes. Rehashing every live source
found 183/183 unchanged. These historical files still have no migration
receipt: copying them into a current-looking `analyses/` folder would obscure
their older input lineage and the order-dependent overwrites.

## 2026-09-24 scientific revalidation update

Two HMM audits were rerun against the activated current Stage 01/08 inputs in
`analysis_ready/analyses/hmm_revalidation_runs/current_stage08_review_20260924/`.
Their nine pinned input/code files remained hash-identical after execution.
The gap-aware comparison preserved 13/13 compared directions. All 20
manuscript-relevant cross-optimum cells had stable signs across five seeded
fits, which reached three log-likelihood levels at printed precision. The
current registry still cites the older audit and should not be read as having
been refreshed by this run. The old Active SUS-RES occupancy-entropy sign-flip
rationale is stale, but that contrast remained nominally significant in 0/5
current fits and stays excluded.

Stage 00 completed a pooled-resolution diagnostic at
`quality_control/tracking_integrity/` and a separately guarded 10-second-only
diagnostic under `quality_control/tracking_integrity/runs/`. Both flagged all
111 animals; the 10-second run flagged all 888 windows on both row-count
zero-run checks. These thresholds are not validated chip-loss or exclusion
criteria. The eight historical May QC files remained hash-identical and the
Stage 16/release readers were not switched. See the run-local `REVIEW_STATUS.md`
and the task's `scientific_revalidation_result_20260924.md` for details.

## 2026-09-24 source-registry correction

The three HMM registry rows now cite the isolated current-input revalidation
tables. The active persistence row retains its conditional scientific role
but is not publication-ready until the manuscript and release products are
refreshed and reviewed. The inactive row remains unpromoted because tracking
measurement validity is unresolved. The Active SUS-RES occupancy-entropy row
remains excluded: the current five fits have same-sign estimates but 0/5
nominally significant fits, replacing the older sign-flip rationale. The
183-file numbered audit tree remains unchanged as historical evidence.

## 2026-09-24 numbered Stage 01 residue check

The retained `03_derived_metrics/` root still has 52 files. Twenty Stage 01
metric and QC files have receipt-selected copies under
`foundations/behavior_metrics/`; the other files include eight older
cross-scale identity reports, fourteen Stage 19 spatial originals, and ten
original run metadata/folder-guide files. Current Stage 01 readers select the
semantic copy, while historical manual audits still name the numbered tree.
The machine-readable output index now has a separate
`01-identity-history` entry for the eight August identity reports. Its
canonical path is deliberately empty; future manual reports have a separate
semantic write location. No residual file was copied, moved, or rerun.

## 2026-09-24 Stage 15 historical phase-source guard

Stage 15 now resolves its eight adaptation, sleep-like inactivity, and
phase-organization tables at the activated ten-minute outputs declared by
Stages 11–13. All eight files are present with September 22 timestamps and
111 animal IDs; the tables with a `PhaseClass` field contain both Active and
Inactive. A guard still refuses an explicitly requested five-minute table
from those families before reading it, identifying the path that needs
source-validity review. This does not reclassify the historical five-minute
files or revise the existing Stage 15 results. A portable contract test checks
the eight source path/scale pairs and the guarded loader.

The same Stage 15 source review found that its two existing integration
inventories label the Stage 09 early-prediction source `5min_based` while their
recorded path is the canonical ten-minute Stage 09 file. The future source
specification now uses `early_prediction_bin_level` for that row and its
already-required resolved path directly; the unreachable old five-minute
fallback candidates were removed. This corrects future feature names and
inventory labels only. Each existing integration contains nine such feature
names across 993 animal-feature rows; both retain their historical five-minute
labels and need a reviewed rerun before being represented as corrected
ten-minute results.

The existing Stage 15 feature export has a separate HMM inclusion defect.
Although `mmm.stage15.include_hmm_in_primary_axes` defaults to false, the
matrix builder previously ignored `in_primary_axes`, and the three directly
loaded five-minute HMM tables were marked `stable_source`. The saved primary
integration has 5,680 direct HMM feature rows marked stable and 888 ten-minute
HMM summary rows marked unstable but still present in the matrix. Its male
axis inventory contains 67 HMM feature-to-axis matches; the sensitivity
integration has the same count. Future code now classifies both HMM families
as unstable and builds the default matrix only from rows allowed in primary
axes. The full feature export retains excluded rows with their role flags.
These code corrections do not validate or alter either existing integration;
both need a reviewed rerun before scientific reuse. The live output index has
since been refreshed as navigation metadata, and the Stage 15 manuscript
registry row now identifies the saved integration as historical exploratory
evidence. Neither metadata change promotes the old results.
