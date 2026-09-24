# Behavioral output cutover record — 2026-09-23

## Scientific revalidation and provisional QC — later 2026-09-24 update

The Stage 00 writer made one pooled-resolution diagnostic run at
`quality_control/tracking_integrity/` before its input-selection defect was
identified. A guarded single-resolution run followed at
`quality_control/tracking_integrity/runs/ten_second_review_20260924/`.
Both flagged all 111 animals; neither is an exclusion or release source.
Stage 00 now requires `--input-scale=10sec_based` and a fresh `--run-id`, and
is not registered in the bulk runner. The May historical eight-file source
remains hash-identical; Stage 16 and the release builder continue to read it
as optional historical diagnostic material. No QC output was activated.

The two HMM audits were rerun against current Stage 01/08 inputs in
`analyses/hmm_revalidation_runs/current_stage08_review_20260924/`. The five
new tables are isolated and have not replaced the registry-linked older
audits. The active registry wording remains a review item before any
manuscript refresh.

At this point, the live 51-row `output_index.csv` labeled the new QC diagnostics as
unpromoted in its Stage 00 note, while retaining `canonical_path = NA` for
that historical row. Its SHA-256 at that point was
`34a520077e10144fad291d4a4f36e367d150ec2cde5ef941fdb1d7d629207143`;
the immediate predecessor is backed up at
`_migration_control/output_index_before_qc_revalidation_20260924.csv`,
SHA-256 `17a677f72c8bb03d7be4426eabe4a6111c6ef7db97874d1bf21be4862928f053`.
The live `analysis_ready/README.md` was refreshed from the repository source,
SHA-256 `afc52d5c89d4a4b1a053af6a4f36cc120424105fbdcf5b09e935bb73c1b16696`;
its immediate predecessor is backed up at
`_migration_control/README_before_qc_revalidation_20260924.md`, SHA-256
`137e2a32d3bdd8c0fe783f6528df4559c8797e29182f341ee937d2eddb2ca415`.

## Navigation and Stage 00 safeguard — 2026-09-24 follow-up

Stage 00's future write path was changed to
`analysis_ready/quality_control/tracking_integrity/`. No run or directory was
created. The two optional Stage 16/release readers stay on the eight-file
May snapshot with explicit historical and unverified-current-input labels
in source code. Existing manuscript artifacts were not regenerated. The live
51-row output index changed only its Stage 00 notes field. Its current
SHA-256 is
`17a677f72c8bb03d7be4426eabe4a6111c6ef7db97874d1bf21be4862928f053`;
the previous index was backed up as
`_migration_control/output_index_before_qc_provenance_label.csv`, SHA-256
`b3702f3b5d763f34f9b86534787dc938e36719fce91a7a16084ee2e69842220c`.
The release builder's read-only dry run resolved 45 files, 32 required and
13 optional, and matched all 22 Stage 16 upstream hashes. This does not
establish lineage for the May QC or September 3 HMM audits.

The top-level `analysis_ready/README.md` now identifies the active
`foundations/` and `analyses/` paths and the historical QC tree. Its previous
version is backed up under `_migration_control/`. The current file matches
the repository navigation source with SHA-256
`137e2a32d3bdd8c0fe783f6528df4559c8797e29182f341ee937d2eddb2ca415`.
Eight root-level temporary output-index CSVs were removed only after each
matched an existing current index or preserved backup by SHA-256. The exact
matches are recorded in the task's temporary-index cleanup manifest. No
scientific file or numbered folder was removed.

## Stage 01 behavioral foundations — 2026-09-24

After reviewing 36 manual Stage 01 audit readers, the current optional
cross-scale identity validator was routed to the receipt-selected input root
and a separate future-report directory. Thirty first-night, HMM, phase, and
Stage 09 forensic scripts retain numbered inputs; they have no pipeline
caller and their old results are not promoted to current-run evidence. Five
other current audits already use the accessor. No audit or Stage 01 producer
was rerun.

The 52-file ownership manifest assigned 20 current Stage 01 metric/QC
files to `behavior_metrics_foundation` and retained ten generated guides and
manifests, eight August identity-audit reports, and fourteen Stage 19 spatial
originals. The 20-row ready plan had distinct source and destination paths;
its 43-file code contract matched the checkout. Inspect, Prepare, Verify,
and Activate passed. A separate post-cutover check matched the SHA-256 of
each selected original and semantic copy to the plan. The destination
contains exactly 20 files, all 52 numbered originals remain, the receipt is
`activated`, and no staging directory remains. The live path helper and
data-dependent animal-identity and first-night parity checks pass against
`foundations/behavior_metrics/`.

The plan SHA-256 is
`847f4d09de6744f24518603d2db740a6e678999cb172bb359b217595d3512368`;
the full ownership-manifest SHA-256 is
`e71bf1fc8969b798fa76a242abb4db9d96380f2351bb08215592f99ab7e2bdb6`.
The activated receipt SHA-256 is
`3512af062154df25edd72255d581f68ccbf88037112c77df29dafbade13de19a`.
The previous 51-row index was backed up as
`_migration_control/output_index_before_foundation_cutover.csv`, SHA-256
`fc3be4416b39d3f4e42f2290e3559e5140fba1fe809e333624763c88feac27e4`.
Only its Stage 01 row changed. The current 51-row index SHA-256 is
`b3702f3b5d763f34f9b86534787dc938e36719fce91a7a16084ee2e69842220c`.
`foundations/README.md` is a hash-matched copy of the repository navigation
source, SHA-256
`33c98645981c297477c21f21dd6b5f555288d768a3430b1a1944331ce5672abb`.
The full manuscript exporter and publication release were not run.

## Four remaining RFID audit groups — 2026-09-24

The separately approved `rfid_construct_audit` (11 files),
`rfid_conservatism_audit` (11), `rfid_alternative_inference_audit` (10),
and `rfid_reliability_audit` (7) were activated under their matching
`analysis_ready/analyses/` directories. Each group's Inspect, Prepare,
Verify, and Activate actions passed. Independent post-cutover SHA-256 checks
matched all 39 copies to both the approved plan and their numbered originals.
The destinations have exactly 11, 11, 10, and 7 files; all four receipts are
`activated`, all originals remain, and no group staging remains. The four
scientific audit scripts were not rerun.

The approved 39-row plan is retained as
`outputs/rfid_remaining_four_activated_plan.csv` with SHA-256
`61d158f22f30de45523b27a62acf05703148dc02e7ac1fddb35eca2bd8c130a7`.
The blocked version remains as a pre-activation record. The current path and
index fixture and the RFID domain contract test passed after activation.
Their prior receipts and code-contract hashes are snapshots from their own
activation times; later changes to shared path and index code do not rewrite
those records.

The live output index added only these four Stage 16 rows and now has 51
rows, SHA-256
`fc3be4416b39d3f4e42f2290e3559e5140fba1fe809e333624763c88feac27e4`.
The 47-row predecessor is retained at
`_migration_control/output_index_before_rfid_remaining_four_cutover.csv`,
SHA-256
`cce046feb95186019e411b3668d5598f59806f26ea4f50999109dc681225c9b5`.
The live analyses README was refreshed from its repository source and has
SHA-256
`059ff9a78e9ff042fb5f35c1f3c4570a214f20feba8d911ae201b284eb04ee12`;
its predecessor was backed up. The full Stage 16 exporter, manuscript
assembly, and publication release were not run. The HMM audit tree remains
outside this cutover.

## RFID leading-bin seed audit cutover — 2026-09-24

The 12-file `rfid_leading_bin_seed_audit` group was activated under
`analysis_ready/analyses/rfid_leading_bin_seed_audit/`. Inspect, Prepare,
Verify, and Activate passed. A separate check matched all 12 original and
destination SHA-256 hashes to the plan. The originals remain, the target
contains exactly 12 files, the receipt is `activated`, and staging is absent.
The audit itself was not rerun. The Stage 28 RFID domain contract test passed
against the activated path and read its two required burden tables there.

The live output index was updated with only this Stage 16 source row. It now
contains 47 rows and has SHA-256
`cce046feb95186019e411b3668d5598f59806f26ea4f50999109dc681225c9b5`.
The prior 46-row version is backed up as
`_migration_control/output_index_before_rfid_leading_bin_seed_cutover.csv`
with SHA-256 `9a3655d249dbd0900fe37710a4409f48977fad59f88472c241a7706dd776c172`.
The live analyses README was refreshed from the repository template, with
SHA-256 `f6c5afb88a1bba5f99b4bb2fe9499eb2792da2ec8db9c3e148833f4f8909e7c0`;
the previous version was backed up in `_migration_control/`. Stage 16's full
exporter and publication assembly were not run.
Later preparation of the four remaining RFID audit families changed the
shared path registry, Stage 16 source, and index-source test. The leading-bin
receipt and its code-contract hash remain the snapshot of activation time;
the 12 copied files were not changed.

## RFID domain comparison audit cutover

The separately reviewed four-file `rfid_domain_comparison_audit` group was
activated at `analysis_ready/analyses/rfid_domain_comparison_audit/`.
Inspect, Prepare, Verify, and Activate passed. A separate post-activation
check rehashed all four originals and copies against the approved plan with
zero mismatches. The four originals remain in the numbered Stage 14 audit
folder; the semantic destination contains exactly four files, the receipt is
`activated`, and no group staging directory remains. The scientific audit
was not rerun.

The live output index was updated from the current Stage 16 index expression
by adding only this audit row. It now contains 46 rows, gives this group
`migrated_source_retained` status, and has SHA-256
`9a3655d249dbd0900fe37710a4409f48977fad59f88472c241a7706dd776c172`.
The prior 45-row index was backed up as
`_migration_control/output_index_before_rfid_domain_comparison_cutover.csv`
with SHA-256 `06e1bd0ceb3565dbaba95380def196eb23465c6fcf9f59b0906e68a8b73cd169`.
The live analyses README was refreshed from the repository template with
SHA-256 `e578230f34b6662e69e8a41eb6087b573b0559ef6e08f5734ccef2a40eeba7af`;
its previous version is backed up in `_migration_control/`.

The Stage 16 index/path test and migration fixture passed against the active
layout. The prior first-night activation receipt and its code-contract hash
remain an immutable record of that earlier code snapshot; this later
path-only change to the comparison writer is pinned in the separate
comparison contract. The other five RFID audit families and HMM audit tree
have not been migrated.
Subsequent preparation of the leading-bin audit changed the shared path
registry, Stage 16 source, and index-source test again. Their current hashes
therefore differ from this comparison receipt's activation snapshot; the
receipt and four scientific copies remain unchanged. The leading-bin audit
was subsequently activated under its own receipt, as recorded above.

## Stage 14 five-minute dashboard cutover

The separately reviewed `systems_dashboard_5min` group was activated at
`analysis_ready/analyses/systems_dashboard/5min/`. Prepare, Verify, and
Activate each passed. The receipt records 293 copied files, the 14-file code
contract SHA-256 `9e4afc691c443fc9c24f87f5e677b61f99bc25fac9ae0e32fbaa576a7d038f91`,
and the 700-row ownership-manifest SHA-256
`25362d32e3f716b235b41c2a6fcb9b58da9207d169acb83354e0bb49f0d21239d`.
A separate post-activation check rehashed all 293 originals and copies against
the plan with zero mismatches. The target contains exactly 300 files: those
293 products and seven regenerated figure index and folder-guide files. The
figure inventory lists 148 existing paths. All 700 non-cache files remain in
the numbered Stage 14 root, and the group staging directory is absent.

The live 45-row `output_index.csv` was updated from the current Stage 16
source expression for only the Stage 14 dashboard row. It now names the
semantic path with status `migrated_source_retained` and has SHA-256
`06e1bd0ceb3565dbaba95380def196eb23465c6fcf9f59b0906e68a8b73cd169`.
Its exact prior version is retained under `_migration_control/` as
`output_index_before_systems_dashboard_cutover.csv` with SHA-256
`215f154b12ff0a85629e6a8933143d0973da66cc63f3bf1c3e8da59d779bccab`.
The live `analyses/README.md` was refreshed from the repository template and
has SHA-256 `562cf20702673393bfad073e632e239c207553bdb71003d87ad638201e357d63`;
its prior version was backed up in `_migration_control/`.

The Stage 16 output-index, HMM/Stage 14, main-figure, and figure-metadata
contract tests passed against the active path. Stage 14, Stage 16's full
exporter, manual audit producers, and publication assembly were not run.
The historical audit families and their readers still require separate
ownership and path review before new audit runs.

## Scope and result

The bounded migration in `BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv` was activated
for twenty-one groups and 938 files across ten cutovers. The first cutover
covered six groups and 45 files. `first_night_10min` and `first_night_5min`
now resolve under
`analysis_ready/analyses/first_night_five_domain_characterization/`.
The four spatial groups resolve under
`analysis_ready/analyses/spatial_occupancy/{tables,audit,models,figures}/`.
All original files remain in their historical directories; no original was
renamed or deleted. The twenty-one receipts under
`analysis_ready/_migration_control/` have state `activated`. No group staging
directory remains.

The migration tool verified the exact planned source inventory and SHA-256
before staging, after staging, and after activation. A separate post-cutover
check rehashed every original and destination file against the 45 plan rows
and confirmed that each destination contains exactly its planned file count:
10, 10, 6, 8, 6, and 5 respectively.

## Reader and metadata checks

The live path registry resolves all six groups to their semantic directories.
Stage 14's two-group and Stage 19's four-group layout guards pass. Stage 27's
`behavior.rfid_domain_summary` key resolves to the new first-night contrasts,
and all eight first-night inputs named by the release builder exist at the new
path. Stage 16's current index source resolves the six corresponding rows to
the new paths with status `migrated_source_retained`. The main-figure and RFID
domain contract tests passed against the activated layout.

The live `analysis_ready/output_index.csv` was refreshed **only** from the
current Stage 16 index expression; the full Stage 16 exporter was not run. The
old 27-row file is retained at
`analysis_ready/_migration_control/output_index_before_semantic_cutover.csv`
with SHA-256
`2655a0b42c9b336ec0f1940eb75d54cd43c865cace5ebb80be2abdce14296333`.
The new 36-row index has SHA-256
`25fad882a26714ab163cf51ff9b27ee4f68e813d78517ce06d81e05390084f79`.
Every nonmissing canonical directory named by the new index was checked to
exist before the replacement.

## Stage 19 test correction after activation

The first live run of `test_stage19_group_sex_labels.R` failed because its
expected roster was still 24 CON / 49 RES / 38 SUS. The migrated spatial table
has the **same hash as the original** and matches the canonical Stage 01
animal, Group, and Sex roster exactly: 111 animals, 24 CON / 53 RES / 34 SUS,
58 Female / 53 Male, with zero missing or extra animal-phenotype rows. OR424,
OR430, OR434, and OR554 are Female RES in both tables, consistent with the
corrected endpoint recorded in `test_stage19_identity_and_stage06_schema.R`.
Only the stale test expectation was updated; no scientific table was changed.
The live Stage 19 output test then passed against the semantic path.

The `spatial_tables` activation code contract is retained as the immutable
snapshot used at activation. It records SHA-256
`fdd43d471852adfbc13f61b530033d501c547e76b6e7615820c4e34574409ab6`
for the pre-correction test. The post-activation test has SHA-256
`acf0785afb457c3d0d964b12aca187d06595d581fd146788e733179d87952239`.
This later test change does not rewrite the receipt or pretend the activation
used different code.

## Boundaries

Stages 14 and 19 were not rerun. The release builder, full Stage 16 exporter,
and publication-figure builders were not run. `publication_ready` was not
modified. Historical audit paths and old output directories remain for
provenance and compatibility; they need a separate dependency review before
any archival cleanup. Repository changes remain uncommitted, and no GitHub
publication action was taken.

## Second cutover: current dyadic and five-minute social outputs

Later on 2026-09-23, `dyadic_contacts` (16 files) and
`social_networks_5min` (94 files) were prepared, verified, and activated.
Their semantic destinations are `analysis_ready/analyses/dyadic_contacts/`
and `analysis_ready/analyses/dynamic_social_networks/5min/`. Each of the 110
originals and destinations matched its plan SHA-256 after activation; each
destination had exactly its planned file count; both receipts are `activated`;
and neither staging directory remains. All originals remain in place.

The current Stage 02 and Stage 06 manifests record September 22 runs. Four
other social-network resolutions have May manifests naming an older producer;
they remain in `06_behavioral_dynamics/social_networks/` and were excluded
from this cutover. A hidden Windows `figures/Thumbs.db` cache was excluded from
the 94-file scientific plan; the migration tool still rejects any other
unplanned source file. Copied manifests retain their original run paths as
historical provenance.

The live receipt-aware path checks resolve Stage 02's default dyadic output,
Stage 06's dyadic input and five-minute output, and the five-minute social
tables read by Stages 14 and 15 to the new directories. Stage 06 rejects a
mixed activation state for its input and output groups. Its separate
cookie-habituation output override remains available. The old resolutions
continue to resolve to their historical paths. Neither scientific producer nor
the Stage 14/15 consumers were rerun.

The live index was refreshed from the Stage 16 index expression, without
running the full exporter. Its previous 36-row copy is retained at
`analysis_ready/_migration_control/output_index_before_dyadic_social_cutover.csv`
with SHA-256
`25fad882a26714ab163cf51ff9b27ee4f68e813d78517ce06d81e05390084f79`.
The new 37-row index has SHA-256
`17132510bdd39f57e78b07d8f7d35148fb058de44a8243575666bebfa034e859`.
Every nonmissing canonical directory in the candidate index existed before
the replacement. The new index separately records the historical social
resolutions. No publication bundle was rebuilt or modified.

## Third cutover: current ten-minute GAMM features

The 18 files under `06_behavioral_dynamics/gamm_features/10min_based/` were
prepared, verified, and activated at
`analysis_ready/analyses/gamm_trajectory_features/10min/` on 2026-09-23.
Every original and destination matched the plan SHA-256 after activation;
the destination has exactly 18 files, the receipt is `activated`, and its
staging directory is absent. The original files remain. The current Stage 07
manifest records a September 22 ten-minute run. The separate 30-minute tree
has no matching current input/output manifest and remains at its historical
path for Stage 15's optional trajectory input.

Stage 07's writer and Stage 14's ten-minute reader now follow the receipt.
The resolution helper preserves Stage 14's ordered fallback vector; it was
corrected and tested after the second cutover but before this third activation.
The second cutover's immutable code receipts therefore predate that helper
correction. Stage 15's 30-minute path still resolves to the original tree.
No Stage 07, 14, or 15 scientific run was performed.

The 37-row pre-cutover index is retained at
`analysis_ready/_migration_control/output_index_before_gamm_cutover.csv`
with SHA-256
`17132510bdd39f57e78b07d8f7d35148fb058de44a8243575666bebfa034e859`.
The refreshed 38-row index has SHA-256
`227a240ab51f929f14da6f2f19601ff05ab5dc5edd783ccf49d143bbde82f4b6`.
The index was generated from the current Stage 16 definition without running
the exporter. Every nonmissing canonical directory existed before the index
replacement. Publication files were not changed.

## Fourth cutover: current five-minute state space

The 97 files under `06_behavioral_dynamics/state_space/5min_based/` were
prepared, verified, and activated at
`analysis_ready/analyses/behavioral_state_space/5min/` on 2026-09-23.
Every original and destination matched the plan SHA-256; the destination has
exactly 97 files, the receipt is `activated`, and staging is absent. A hidden
Windows `Thumbs.db` cache stayed in the old figures folder. All scientific
originals remain. The current Stage 05 manifest records a September 22
five-minute run. The older one- and ten-minute branches lack a matching
current manifest and remain at their historical paths.

Stage 05's writer and the five-minute Stage 14/15 readers now resolve through
the receipt. Stage 14's ordered resolution alternatives remain intact. No
scientific stage was rerun. The prior 38-row index was backed up at
`analysis_ready/_migration_control/output_index_before_state_space_cutover.csv`
with SHA-256
`227a240ab51f929f14da6f2f19601ff05ab5dc5edd783ccf49d143bbde82f4b6`.
The refreshed 39-row index has SHA-256
`25c79eb73eabd17fbac95182f2a397ff2d4a54c5705d188830ac1536d7c690d4`.
It was generated from the current Stage 16 index definition without running
the exporter. No publication output was changed.

## Fifth cutover: HMM primary and sensitivity

The current Stage 08 HMM outputs were activated as two linked groups on
2026-09-23: 36 files from `hmm_states/10min_based/` to
`analysis_ready/analyses/hmm_states/10min/`, and 36 files from
`hmm_states/5min_based/` to `analysis_ready/analyses/hmm_states/5min/`.
Their September 22 manifests name the current Stage 08 producer. Every
original and destination matched the plan SHA-256 after activation. Both
receipts are `activated`, each destination has exactly 36 files, no group
staging remains, and both original trees remain for historical audits.

Stage 08 refuses a mixed activation state for its declared pair. The Stage 14
artifact resolver, Stage 14 dashboard, and Stage 15 optional HMM reader now
follow the receipts. The live HMM identity and inference contract test passed
against the semantic paths. No HMM model or downstream scientific stage was
rerun; the test's small statistical fixture reported a singular-fit warning
but passed its checks.

The prior 39-row index was retained at
`analysis_ready/_migration_control/output_index_before_hmm_cutover.csv`
with SHA-256
`25c79eb73eabd17fbac95182f2a397ff2d4a54c5705d188830ac1536d7c690d4`.
The refreshed 40-row index has SHA-256
`55d8008a4e792ac3d850a46dfe664dc911bdb75fe62494776661e4741d98e34a`.
It was built from the current Stage 16 index definition without running the
exporter. No publication output was changed.

## Sixth cutover: current ten-second temporal instability

The 212 files in the current Stage 04 ten-second branch, totaling
13,549,791,082 bytes, were prepared, verified, and activated at
`analysis_ready/analyses/temporal_instability/10sec/` on 2026-09-23.
The September 22 input/output manifest names the current Stage 04 producer.
The migration tool checked every source hash during preparation, every staged
hash during preparation and verification, and every destination and retained
source hash during activation. It reported `hashes=PASS` and
`originals_retained=True`; the activated receipt records 212 files. The final
destination inventory has exactly 212 files and no staging directory remains.
A hidden `Thumbs.db` cache stayed in the old figures folder. The older
one- and five-minute branches also remain under the numbered tree.

Stage 04's writer and Stage 14's temporal resolver now follow the ten-second
receipt. The live resolver selected and found the semantic
`temporal_instability_metrics_per_animal_all_metrics.csv`. Stage 15's optional
one- and five-minute preferences still point to the historical branches.
No Stage 04 or downstream scientific analysis was rerun.

The previous 40-row index is retained at
`analysis_ready/_migration_control/output_index_before_temporal_cutover.csv`
with SHA-256
`55d8008a4e792ac3d850a46dfe664dc911bdb75fe62494776661e4741d98e34a`.
The refreshed 41-row index has SHA-256
`23d71d4ee0fbcf55d5f670082e081bb9b6215a80a255f17f0b150ad7a3f81814`.
It came from the current Stage 16 index expression without running the full
exporter; every nonmissing canonical directory existed before replacement.
No publication output was changed.

## Seventh cutover: behavior-proteomics primary and sensitivity

The current Stage 15 primary and flagged-replicate sensitivity trees were
activated together on 2026-09-23. Each has 43 planned files. Their semantic
destinations are `analysis_ready/analyses/behavior_proteomics/` followed by
`proteomics_mnn_primary/` or `proteomics_mnn_sensitivity/`. The migration tool
verified the SHA-256 of all 86 originals and destinations against the plan,
recorded both receipts as `activated`, and left no group staging directory.
Both numbered source trees remain intact. The longest destination path was
228 characters, below the tool's 240-character guard.

Stage 15 now chooses the semantic base only when both receipts are active;
it refuses a mixed pair. Its live input check found the two expected module
score CSVs. A new two-row
`analysis_ready/analyses/behavior_proteomics/proteomics_integration_output_dir_map.csv`
points to the semantic output directories. The historical map remains in place
as provenance. Its SHA-256 is
`77ca14d0c16d341b40489cdfd8bc8f1ddd109923f97cd23f91d2c0dc21171bb7`;
the semantic map's SHA-256 is
`63c6107628e9ed412fab68bafa042cc636ec39d714d2ae386fe869a1857c18b9`.
The manuscript registry's exploratory Stage 15 source path now names the
semantic primary table. No Stage 15 model or manuscript exporter was rerun.

The previous 41-row index is retained at
`analysis_ready/_migration_control/output_index_before_proteomics_cutover.csv`
with SHA-256
`23d71d4ee0fbcf55d5f670082e081bb9b6215a80a255f17f0b150ad7a3f81814`.
The refreshed 41-row index has SHA-256
`1b9f807373c1600f2e717b37585d9f7b5c57db6cc4f1bf4542e2ac05ce3be90b`.
It was generated from the current Stage 16 index expression after checking
every nonmissing canonical directory. No publication output was changed.

## Navigation files after the seventh cutover

The live `analysis_ready/analyses/README.md` was refreshed from
`docs/BEHAVIOR_ANALYSES_DIRECTORY_README.md`; both have SHA-256
`4121caf763d65712f924b0f481adb6f3f6e32ce4d2811ebf7c8377d4abbe444d`.
The root `analysis_ready/README.md` now points readers to `analyses/`, the
stage-addressed `pipeline/`, and `output_index.csv`. Its earlier copy is
`analysis_ready/_migration_control/README_before_semantic_navigation.md`
with SHA-256
`91c483e3b519ad251e1e8ab00e28bf0552ce5ba8460cb37f6634cb658b6f7b07`.
The refreshed root README has SHA-256
`a382e9b6f9be41d4029f67594979040fae863a7ab1a1c19ac9d1184d9c4e343b`.

## Eighth cutover: current Stage 11–13 ten-minute outputs

On 2026-09-23, the declared ten-minute runs from Stages 11, 12, and 13 were
activated as three independent output groups: 19 adaptation-kinetics files,
19 sleep-like inactivity files, and 22 phase-organization files. Their
September 22 input/output manifests name the respective current producers.
The migration tool reported `hashes=PASS` for Prepare, Verify, and Activate
for each group, with `originals_retained=True`. A separate post-cutover check
rehashes all 60 source and destination files against the plan. Each
destination has its exact planned file count, all three receipts are
`activated`, and no group staging remains. The older five-minute branches
remain at their historical paths.

The three producers and Stage 14's ten-minute input preferences now use the
receipt-aware resolver. Stage 14's phase-classifier staleness guard recognizes
the semantic paths and its test rejects a stale semantic fixture. Stage 15's
fixed five-minute behavior scale still resolves to the historical five-minute
trees. No Stage 11–15 scientific analysis or manuscript exporter was rerun.

The prior 41-row index is retained at
`analysis_ready/_migration_control/output_index_before_phase_analysis_cutover.csv`
with SHA-256
`1b9f807373c1600f2e717b37585d9f7b5c57db6cc4f1bf4542e2ac05ce3be90b`.
The refreshed 44-row index has SHA-256
`2428246fd34c4e9d5bc740967093a2a1be49b36a6329c7a5430c05a8ce3ba208`.
It came from the Stage 16 index expression without running the exporter; every
nonmissing canonical directory existed before replacement. The live
`analysis_ready/analyses/README.md` was refreshed from the repository
navigation source and has SHA-256
`50fecf4d91ab8d2b19e6cc75be1b5d3b721b094dd675748196bb2afde3d728b5`
after adding the Stage 15 source-validity note.
No publication output was changed.

## Stage 15 source-validity boundary found during the eighth cutover

The copied Stage 15 primary and sensitivity
`tables/integrated_behavior_feature_sources.csv` each record eight
`loaded_curated` sources from the old five-minute Stage 11–13 trees: two
adaptation, one sleep-like inactivity, and five phase-organization tables.
The sampled source files have 2026-05-18 write times; the Stage 14 guard
defines the exact-phase-classifier fix as 2026-09-03 15:24:07 +0200.
The repository already records that the five-minute branches predate that
fix and can mislabel Inactive epochs as Active. This is a source-validity
issue in the existing Stage 15 products, not a changed result of this path
cutover. The eight sources remain at their historical paths and Stage 15
was not rerun. A scientific decision is needed before using or rebuilding
those integrations: regenerate validated five-minute inputs or change the
integration's declared behavior scale with a new analysis and validation.

## Ninth cutover: manual supporting five-minute outputs

On 2026-09-23, 97 files from `13_nonlinear_systems_dynamics/5min_based/`
and 138 files from `14_nextgen_behavioral_phenotyping/5min_based/` were
activated as `analyses/nonlinear_dynamics/5min/` and
`analyses/systems_phenotyping/5min/`. Each Prepare, Verify, and Activate action
reported `hashes=PASS` and `originals_retained=True`. An independent check
rehashes all 235 planned files in both original and destination locations,
confirmed both receipts are `activated`, and found no staging directory.

The two manual producers, Stage 10 feature scan, Stage 14 dashboard, Stage 15
optional input, and Stage 16 index definition now use the receipt-aware paths.
Stage 10 selects one location per supporting tree; its candidate sets remain
exactly 26 and 55 CSV files. The read-only reader check found the Stage 14/15
feature paths at the semantic locations. No scientific analysis or manuscript
exporter was rerun. The Stage 15 source-validity issue above remains open.

The previous 44-row index was retained at
`analysis_ready/_migration_control/output_index_before_supporting_cutover.csv`
with SHA-256
`2428246fd34c4e9d5bc740967093a2a1be49b36a6329c7a5430c05a8ce3ba208`.
The refreshed 44-row index has SHA-256
`b7bcb345c3446ec15249c1a53b9e3c8b8a101094a4727c8c6436eb46f1cc5901`.
Every nonmissing canonical directory was checked before replacement.
The live `analysis_ready/analyses/README.md` was refreshed from the repository
navigation source and has SHA-256
`f02f9ecd342f7c616edb15287b8779cda65d42ff7fe696a1cf8f4c6e04c10ac4`.

## Tenth cutover: inactive-phase QC audit

On 2026-09-23, the three CSVs written by the manual
`Testing/audits/audit_inactive_phase_qc_redesign.R` were copied from
`12_systems_neuroscience_summary/5min_based/audit_inactive_phase_qc/` to
`analyses/inactive_phase_qc_audit/`. The producer now chooses its write root
from the activation receipt. The manuscript registry points to the semantic
proposed-specification file while retaining the separate HMM verdict path.
The audit's proposal remains unadopted in production QC.

Prepare, Verify, and Activate each reported `hashes=PASS`. A separate check
matched the hashes of all three originals and copies to the plan; the receipt
is `activated`, the originals remain, and no staging directory remains. The
manual audit, Stage 14, manuscript exporter, and release builder were not run.

The 44-row previous index was backed up as
`analysis_ready/_migration_control/output_index_before_inactive_qc_audit_cutover.csv`
with SHA-256
`b7bcb345c3446ec15249c1a53b9e3c8b8a101094a4727c8c6436eb46f1cc5901`.
The 45-row current index has SHA-256
`215f154b12ff0a85629e6a8933143d0973da66cc63f3bf1c3e8da59d779bccab`.
Every nonmissing canonical directory was checked before replacement. The live
`analysis_ready/analyses/README.md` was refreshed and has SHA-256
`79be93fe78524dd446423e8c6eb702a90cb4f708d4cb000c3f8be2784ff8f535`.

## 2026-09-24 HMM navigation and QC lineage follow-up

The current five-file HMM revalidation run has a semantic README under
`analyses/hmm_revalidation_runs/`. The older 183-file HMM audit tree remains
untouched at `12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture/`.
The analysis navigation and Stage 16 output-index source identify the current
run as unpromoted and the older tree as retained historical evidence. No HMM
audit tree was copied or activated, and no manuscript registry entry was
switched. The live index was updated from 51 to 53 rows; a comparison against
the Stage 16 source using live activation receipts passed for all 53 rows.
Its SHA-256 is
`DD2C8D158F980536F97BC58CC3FEA63EAC94869A6BF77A1C0D35BEE267342455`.
The predecessor and an intermediate index are retained under
`analysis_ready/_migration_control/`.

The May raw QC roster has 115 unique canonical IDs in 117 rows; two pairs
are ID aliases. All 111 current behavior animals occur in that roster. Its
four other canonical IDs are all on `raw_data/excluded_animals.csv`, which
preprocessing applies before Stage 01. In current 10-second metrics, zero
movement occurs in 93.79% of Active and 99.35% of Inactive bins. Stage 01
carries the last observed position forward, so positive derived occupancy
time in those bins does not establish a fresh RFID read. Stage 00's zero-run
flags remain provisional and are not additional exclusion decisions.

## 2026-09-24 HMM source-registry correction

The three HMM rows in `docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv` now cite the
isolated current-input run and state its five-fit results. The active claim
remains conditional and is marked not publication-ready until existing
manuscript and release products are refreshed and reviewed. Inactive claims
remain unpromoted; occupancy entropy remains excluded. This changes source
metadata only. The historical HMM audit files, Stage 16 products, and release
products were not changed.
