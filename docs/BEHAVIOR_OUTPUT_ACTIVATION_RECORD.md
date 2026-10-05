# Behavioral output cutover record — 2026-09-23

## 2026-09-24 Stage 01 plan preservation

The reviewed 20-file foundation plan and 52-row ownership snapshot were
copied byte-for-byte into `docs/behavior_output_activated_plans/`. The live
`behavior_metrics_foundation` receipt matches the plan and ownership hash.
A read-only independent check matched all 20 numbered originals and all 20
semantic copies to their pinned SHA-256 values. The other 32 numbered files
remain assigned to eight identity reports, fourteen Stage 19 spatial
originals, and ten Stage 01 metadata files. No metric was regenerated or
source file changed.

## 2026-09-24 navigation and Stage 14 residual check

The root and analyses folder guides were refreshed from the repository
templates, and `analysis_ready/history/README.md` was added to explain the
nine older-resolution groups. The two replaced live guides were backed up
under `_migration_control/` as
`analysis_ready_README_before_history_navigation_20260924.md` (SHA-256
`afc52d5c89d4a4b1a053af6a4f36cc120424105fbdcf5b09e935bb73c1b16696`)
and `analyses_README_before_history_navigation_20260924.md` (SHA-256
`665265014589dedbdc9ba04c83558035fd1b5da312c1d235e3328ff8abb11f46`).
The new live guide hashes are respectively
`903bb02c117574135465fb91ae293035c39da5aef7416ac65b28ff002336a409`,
`5d3a4d5406ddedcd175e35c38d82522432db20a7a767bf966a414c5ae07f8902`,
and `ff9e8a418d041eff4a535de2d45c391ff9b388aa3af381eb218027263889e17e`
for root, analyses, and history. No scientific file was changed.

The activated Stage 14 dashboard and RFID audit plans and 700-row ownership
snapshot were copied byte-for-byte into
`docs/behavior_output_activated_plans/`. A repository read-only check matched
the ten relevant activation receipts, all 371 planned source and destination
hashes, and the generated dashboard metadata. Of 700 numbered Stage 14
files, 329 remain only there: 183 historical HMM audit files with hashes
matching the retained writer map, 132 byte-identical QC mirrors of authored
figures, one QC README, and 13 other historical records. This check did not
rerun an audit or promote the older HMM results.

## 2026-09-24 Stage 10 discovery follow-up

After the history copies were activated, Stage 10 feature discovery was
changed to scan the 18 receipt-selected output groups directly. A read-only
comparison found the same 606 filtered paths in the same order as the former
numbered-root scan after omitting its one non-feature metadata map. A focused
fixture also confirmed that a newly written semantic-group feature is
discovered. The Stage 10 code and its checks are pinned separately in
`docs/behavior_output_code_contracts/stage10_semantic_discovery_20260924.csv`;
the code contracts recorded in activation receipts remain unchanged. Stage 10
models were not rerun, and the numbered originals remain in place.

## 2026-09-24 historical resolution copy activation

The nine groups in `BEHAVIOR_HISTORICAL_MIGRATION_PLAN.csv` were opened from
`blocked_review` to `ready` after a fresh read-only `Inspect` found matching
source hashes, no destination directories, and no staging directories. Each
group then passed `Prepare`, `Verify`, and `Activate` with `hashes=PASS` and
`originals_retained=true`:

| Semantic group | Files |
| --- | ---: |
| `history/social_networks/10sec/` | 92 |
| `history/social_networks/1min/` | 92 |
| `history/social_networks/10min/` | 92 |
| `history/social_networks/30min/` | 88 |
| `history/state_space/1min/` | 61 |
| `history/state_space/10min/` | 95 |
| `history/temporal_instability/1min/` | 125 |
| `history/temporal_instability/5min/` | 210 |
| `history/gamm_features/30min/` | 9 |

All nine plan-bound receipts under `analysis_ready/_migration_control/` have
`state=activated`; their file counts match the plan, the numbered originals
remain, and no group staging directory remains. An independent read-only pass
matched all 864 source hashes and all 864 destination hashes against the plan.

The Stage 10 read-only candidate check retained 607 filtered paths in order.
It routed 330 historical paths to the semantic copies, including 85
`AnimalNum` candidates, with unchanged basenames, unique paths, and equal
file sizes. All nine direct historical-resolution helpers select their
semantic directories. Stage 10 models and Stages 14–16 scientific analyses
were not rerun.

The live 63-row `output_index.csv` was refreshed from the Stage 16 definition.
Exactly 18 cells changed: `canonical_path` and `status` in the nine history
detail rows. All canonical directories exist, and a subsequent comparison
found the live index identical to the Stage 16 definition. Its previous
SHA-256 was
`5c438f0f041a17087c5758e89b351b62cc19398e006d8166faa78f51f33e0546`;
the backup is
`analysis_ready/_migration_control/output_index_before_history_activation_20260924.csv`.
The refreshed index SHA-256 is
`d5088becd4ab42a19ce01b893e92a3a0c6dc4a90a387b33fe560062972321eb6`.
These copies improve navigation and reader routing; they do not revalidate
the older scientific runs or change manuscript authority.

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

## 2026-09-24 historical Stage 01 identity-audit index entry

A read-only residue check confirmed 52 files in the retained numbered
`03_derived_metrics/` root: 20 Stage 01 originals, eight August cross-scale
identity reports, fourteen Stage 19 spatial originals, and ten original
metadata/folder-guide files. The Stage 16 source definition and live output
index now include a separate `01-identity-history` row with no canonical
path and `historical_source_retained` status. The 54-row live index matches
the Stage 16 source definition exactly, SHA-256
`6D9EFE910ADE4C35FC9A37BC94183B1B15E94998E21DC6B628B0B03464361AF1`.
The prior 53-row index is backed up under `_migration_control/` with SHA-256
`38BD52B718D7D32D0F84CBADB47CE3AF8CAF8FAA1EA100F6651EE485C9F3955E`.
No residual file was copied, moved, or rerun.

## 2026-09-25 numbered-root archive: 03_derived_metrics

With the maintainer's approval, `Invoke-BehaviorNumberedRootArchive.ps1
-Action Activate` moved the complete `03_derived_metrics/` root unchanged to
`history/original_layout/03_derived_metrics/` (activated 2026-09-25 13:45:50
UTC, in 60 seconds). The tool rehashed all 53 files (3,162,803,458 bytes)
against manifest `53265050...` before and after the same-volume rename. The
reviewed `ArchivePath` gate is
`behavior_output_archive_gate_03_derived_metrics.csv` (`c01aa48a...`). The
activated receipt `_migration_control/numbered_root_archive/03_derived_metrics.json`
has SHA-256 `929598917B0DCBE4380AD437905F1B5BBF104456D566D06558A34D259675CD3B`.
Before the move no writer process, scheduled task, or peer session was
active, and the full test suite passed (54 R, 6 PowerShell).

After the move, R resolves the retained source to the archive and opens all
53 files there. The foundation copy and the spatial table and audit copies
stay the active roots, and writes into either location are refused by code
on this branch. The unpromoted `qc/first_night_seed_provenance.csv` moved
with the root and was not promoted. Four live READMEs (root, `foundations/`,
`history/`, `analyses/`) were replaced by their repository sources after
backups to `_migration_control/`:

| Backup | SHA-256 |
| --- | --- |
| `analysis_ready_README_before_03_archive_20260925.md` | `903BB02C117574135465FB91AE293035C39DA5AEF7416AC65B28FF002336A409` |
| `foundations_README_before_03_archive_20260925.md` | `33C98645981C297477C21F21DD6B5F555288D768A3430B1A1944331CE5672ABB` |
| `history_README_before_03_archive_20260925.md` | `FF9E8A418D041EFF4A535DE2D45C391FF9B388AA3AF381EB218027263889E17E` |
| `analyses_README_before_03_archive_20260925.md` | `5D3A4D5406DDEDCD175E35C38D82522432DB20A7A767BF966A414C5AE07F8902` |

`Maintenance/Refresh-BehaviorOutputIndex.R` refreshed the 63-row
`output_index.csv` from the Stage 16 definition. Five notes changed: four
`03` rows gained their retained-original location, and `08-audit-history`
took the wording already committed in `c8c0a0a`. The new index has SHA-256
`AAAE32CFDB71EC6FF8EA3AE233707DD9AA5092DC4946C35D8B8E3FDF54135914`; its
predecessor is `_migration_control/output_index_before_03_archive_20260925.csv`
(`D5088BECD4AB42A19CE01B893E92A3A0C6DC4A90A387B33FE560062972321EB6`). No
scientific output was changed or rerun.

## 2026-09-25 numbered-root archive: 06_behavioral_dynamics and 12_systems_neuroscience_summary

With the maintainer's approval, `Invoke-BehaviorNumberedRootArchive.ps1
-Action Activate` moved `12_systems_neuroscience_summary/` (activated
14:10:25 UTC, 8 s) and then `06_behavioral_dynamics/` (14:16:32 UTC, 367 s)
unchanged to `history/original_layout/`. The tool rehashed every file
before and after each rename, against the manifests shown below. Each run
also accepted the reviewed list of archived paths of 260 or more characters
by its SHA-256.

| Root | Files | Bytes | Manifest | Gate | Accepted long paths | Receipt SHA-256 |
| --- | ---: | ---: | --- | --- | --- | --- |
| `12_systems_neuroscience_summary` | 702 | 209,524,954 | `a1b551f0...` | `c658d44a...` | 7, `ea855b79b75fe887...` | `0E483FE17566240135F032D6A2C59DC06E1E5C7A02553F5F9617A027CF960DE1` |
| `06_behavioral_dynamics` | 1,469 | 18,194,653,380 | `17777e9c...` | `d717ec86...` | 20, `0841a9fbdc78fd29...` | `671B5A34159B9534B3042BFD18ADC8AC79E6EE51EBF974546F35345272266B31` |

The receipts are in `_migration_control/numbered_root_archive/`. Both gates
are the reviewed `ArchivePath` gates committed in `2611ee6`. Every active
group sourced from either root still resolves to its semantic copy.

Four live READMEs (root, `history/`, `analyses/`,
`analyses/hmm_revalidation_runs/`) were replaced by their repository sources
from `2232017` after backups to `_migration_control/`. `foundations/README.md`
did not change.

| Backup | SHA-256 |
| --- | --- |
| `analysis_ready_README_before_0612_archive_20260925.md` | `2B4E4DD8FD06E3FE12A50C2E630F3247BF000C2A0699383FFAAE8C2240F12638` |
| `history_README_before_0612_archive_20260925.md` | `79D5DBC0A817F4901BF2FA0B294C5170052BEC132D695668FB06361E89148640` |
| `analyses_README_before_0612_archive_20260925.md` | `9758175D66ECB2D68D69E71CFC3AE18D80B2E3FD64F81AC65745FC50DE56295C` |
| `hmm_revalidation_README_before_0612_archive_20260925.md` | `5B9CCB3714DC7F64326D485797C0E61453EFF7A6547C713B3FCBE9F5AC600516` |

`Maintenance/Refresh-BehaviorOutputIndex.R` then refreshed
`output_index.csv`, using the rule from `cc47627`: a note names a retained
original only if that folder moved with its root. Thirty-four notes gained
the location of their original under `history/original_layout/`. The
Stage 09 row did not, because its legacy folder had been quarantined before
the move. No other cell changed. The new index has SHA-256
`783BDFB7A7FE5316ABA04F416FC80AD62D6C9475518E08D00D27AA0298C08AB9`; its
predecessor is
`_migration_control/output_index_before_0612_archive_20260925.csv`
(`AAAE32CFDB71EC6FF8EA3AE233707DD9AA5092DC4946C35D8B8E3FDF54135914`).

At 14:17:17 UTC Windows Explorer added a 20,480-byte hidden
`5min_based/figures/Thumbs.db` to the archived `12` root. All 702 recorded
files still matched their hashes, and `Verify` for `12` reported only a
file-count difference. After a copy was kept outside the tree
(`rfid_numbered_root_archive_review_20260924/thumbs_db_12_figures_20260925T141717Z.db`,
SHA-256 `F13E3D06BF4374F1396DFE4176081EAD421DE97AC791C12AF957594CDA052A9E`),
the maintainer removed the file. `Verify` then passed for `12` (702 files), as
it does for `03` and `06`. A second Explorer cache, created at 14:32:03 UTC in
the unarchived `14_nextgen_behavioral_phenotyping/` root, was left in place;
the inventory tools skip `Thumbs.db`. No Exp9 scientific output was changed
or rerun.

## 2026-09-25 cookiehab preprocessing and Stage 01/02 outputs regenerated by a test (kept)

This did not involve the Exp9 tree or any archive. A first version of
`Testing/tests/test_cookiehab_runner_restores_options.R` wrapped
`source()` of the runner in `suppressMessages()`, so the runner's
`sys.frame(1)$ofile` lookup failed. It then fell back to `getwd()/Analysis`,
the real repository, and ran the real cookie-habituation preprocessing,
Stage 01 and Stage 02 against `RFID/cookiehab/`.

- The first run completed; its surviving writes span 17:41:21-17:42:18 CEST.
- A second run, from a mutation check, was stopped partway. By then it had
  rewritten only the preprocessing outputs, the two Stage 01 manifests (three
  files, since `output_manifest.csv` is mirrored at the root) and three
  group/sex QC tables. It left 11 of the 63 files; the first run left 52.
- Both runs used `Formatting/01_preprocess_cookiehab_animalpos.R`,
  `Analysis/01_build_multiscale_behavior_metrics.R` and
  `Analysis/02_build_dyadic_rfid_contacts.R` at `e0be716`. The first run
  used the runner with the `local()` change later committed in `bd9c2f0`.
  The second ran a copy of the `e0be716` runner, which differs only in how
  it restores options and the working directory.
- 63 files were written under `cookiehab/`: 58 existing files were
  overwritten, and 5 QC files are new. The overwritten content was last
  modified on 2026-06-30 according to `LOCAL_OUTPUT_TREE_AUDIT.csv`; the
  files were copied to the share on 2026-08-31. Their paths, sizes, run and
  SHA-256 are in `docs/cookiehab_regeneration_20260925_files.csv`.
- `cookiehab/preprocessed_data/` (1 file, 3,645,554 bytes) and
  `cookiehab/qc/` (4 files, 19,117 bytes) have the same byte totals as in
  `docs/LOCAL_OUTPUT_TREE_AUDIT.csv`, so they were most likely regenerated
  unchanged. The 53 pre-existing files under `cookiehab/analysis_ready/` total
  825,092 bytes less than recorded there. The earlier Stage 01/02 content
  changed, and no other copy of it was found on the Exp9 share or the local
  profile.

The maintainer chose to keep the regenerated outputs, since the
cookie-habituation analysis may be revisited. The committed test never
executes the runner. It parses the runner and evaluates only its `local()`
block, with `source()` replaced by a stub.

## 2026-09-26 remaining top-level trees archived

The maintainer asked on 2026-09-25 to move every remaining tree into the new
structure and not to restore the quarantined Stage 09 trees. Commit `c426e16`
extended the receipt-based archive from three roots to every top-level tree
of the original layout, and `69d5183` added a reviewed 37-row `ArchivePath`
gate for each of the 17 remaining trees. The full suite passed before any
live action (67 tests, no writes on `S:`).

**Copies first.** The tool now stages under `_migration_control/incoming/`.
The empty top-level `_migration_incoming/` was moved there beforehand. Two
groups were then copied under `docs/BEHAVIOR_REMAINING_MIGRATION_PLAN.csv`
(`3041c5d`), so that their readers keep a location outside the archive:

| Group | Files | Destination | Activated (UTC) | Receipt SHA-256 |
| --- | ---: | --- | --- | --- |
| `history_tracking_integrity_10sec` | 8 | `history/tracking_integrity/10sec/` | 08:52:35 | `D3B264B66E8A6ABF6F5E3F0F7E79737F6A1AED11CE8E9B87E924EC7E40BDED0A` |
| `proteomics_module_scores` | 3 | `foundations/proteomics_module_scores/` | 08:52:36 | `A0D683F789527847D533A6CDDF326FA86269D957D469B922C956084365D82BF6` |

Stage 16 and the release builder now read the May 2026 QC tables from the
first copy, and Stage 15 reads its module scores from the second.

**Explorer caches.** Before the manifests were built, 16 hidden `Thumbs.db`
files in five trees were moved, not deleted, to
`_migration_control/thumbs_db_removed_20260926/`, keeping their relative
paths. Their list and hashes are in `docs/thumbs_db_removed_20260926.csv`.

**Archives.** `Invoke-BehaviorNumberedRootArchive.ps1` rehashed every file
against its manifest before and after each same-volume rename. The manifests
are in `docs/behavior_output_archive_manifests/<root>.csv`. Long archived
paths were accepted by the SHA-256 of the list that `Inspect` reported:

| Root | Files | Bytes | Accepted long paths | Activated (UTC) | Receipt SHA-256 |
| --- | ---: | ---: | --- | --- | --- |
| `16_manuscript_behavior_report` | 5 | 439,390 | 0 | 09:01:13 | `25B1E1EB077A521298C641B85C676FC302ECFC831D4250B17761F55B9CD58105` |
| `18_raw_movement_publication_trajectory` | 57 | 20,881,627 | 0 | 09:01:35 | `2DAC255EE6C431F92AC504443E2B6CA0C3456FF963DB07B92DB5FA23EA1C606B` |
| `18b_raw_movement_broad_phase_stats` | 8 | 124,592 | 0 | 09:01:36 | `D4F8DE3FB76414AE53BBB7654CB41817D398430AB658CF2992502844FA4770A7` |
| `18c_raw_movement_broad_phase_stats_corrected` | 21 | 839,708 | 3 (`76012d6b...`) | 09:01:38 | `4DFFEE2968BA4F46CF29627C0A6BF268175EE094411D0F2BA195AC28626F225E` |
| `_archive_stale_stage27_candidates` | 34 | 2,258,003 | 0 | 09:01:39 | `65B588F60F125629F7231A99D89795D6D806709CE1C240F0DA729618237B1BEB` |
| `_archive_stale_stage10_outputs` | 178 | 2,417,421,704 | 0 | 09:03:34 | `ACEEB09112673F6695F5FAF587DCB5846D1A0334F0107561659CCF0C72A7B9BE` |
| `_quarantine_legacy_s09` | 393 | 178,345,880 | 0 | 09:04:06 | `6589FAD2592AB4A83709340852E0CF3E2A79AF0B76735E43F2891E02EBC026EC` |
| `00_qc_tracking_integrity` | 8 | 1,152,201 | 0 | 09:04:19 | `FAC5E00258DD31D7C4CF62F9565768C102A70906E6A91F0D006B13B48D0E654B` |
| `proteomics` | 3 | 31,357 | 0 | 09:04:20 | `FBA51C85476A7F0EEF27CB71EC6B7FD62008AC979DCABF102E0ADB7A937925EC` |
| `03_primary_raw_movement_phase_stats` | 31 | 2,918,806 | 0 | 09:04:21 | `4A30BB285B9CC18DCE09B9C33A8E836A51940E1ED0C43B30B174BBAFEB778892` |
| `04_model_outputs` | 6 | 6,721,792 | 0 | 09:04:23 | `67F541BDD19F8B2BD101FB0B682A630BFD6CC9D3F5E5F765CF73E439E1283A2A` |
| `05_figures` | 5 | 433,892 | 0 | 09:04:24 | `1C8EC19AEDE8542CC03DD52DF9119AC76A8CD11EEC404FC6820611EB32985EA6` |
| `15_behavioral_adaptation_kinetics` | 29 | 7,419,605 | 0 | 09:04:25 | `23FBF542472ED74D06C3DB0A0C3706E9ADCCCAF6ADEB0E8C63808F7261F36C5E` |
| `16_sleep_like_inactivity_metrics` | 29 | 6,084,057 | 0 | 09:04:27 | `6474FDDA936883992B0244D62AD27D7774D0800CBE08513E252E4A9A8D514EAB` |
| `17_ethological_phase_organization` | 35 | 5,077,325 | 0 | 09:04:29 | `65A3E112642D5FF67BF7198B21CEA83D65B3EDC536F09C8BC46A856342696DD2` |
| `13_nonlinear_systems_dynamics` | 97 | 39,252,540 | 3 (`0b58a542...`) | 09:04:31 | `5B1D6DC0FCE176E97AFF1C6F9E99AFB48AFF81BEF5465F7FEAE83EFD588897CE` |

Together these hold 939 files and 2,689,402,479 bytes. After the moves:

- Every receipt is `activated`, and every old path is gone.
- R opens every archived file except the six accepted long paths.
- The writer guard refuses both the old and the archived paths.
- All 40 output groups resolve their active root.
- The foundation and Stage 14 residual inventories pass.

`14_nextgen_behavioral_phenotyping` waited at first: a File Explorer window
was open inside it, which could hold a handle during the rename or write a new
thumbnail cache. Once no Explorer window was open, it went through the same
transaction:

| Root | Files | Bytes | Accepted long paths | Activated (UTC) | Receipt SHA-256 |
| --- | ---: | ---: | --- | --- | --- |
| `14_nextgen_behavioral_phenotyping` | 138 | 78,788,782 | 9 (`a5bfb08c...`) | 09:16:09 | `FFB834833AD112E9E9BB5CD5861ECAB466FAA6472095A395F49725C36297AADA` |

R opens 129 of its 138 archived files. Each of the nine long paths (up to 270
characters) has a hash-identical copy in `analyses/systems_phenotyping/5min/`.
All 40 output groups still resolve. The refresh added the retained-original
note to `support-14` (index SHA-256
`5C1C45DFAC5EE661235BCD2DF2A94FEFF538E9926146B5B855C7554C10D5460E`; its
predecessor is `_migration_control/output_index_before_14_archive_20260926.csv`,
`A1CB1E2B...`). No top-level tree of the old numbered layout remains in
`analysis_ready/`. The live root, `history/` and `analyses/` READMEs were
replaced by their sources at `ae4d41f` after backups:
`analysis_ready_README_before_14_archive_20260926.md` (`2312CBAF...`),
`history_README_before_14_archive_20260926.md` (`D27391D8...`) and
`analyses_README_before_14_archive_20260926.md` (`61DA642F...`).

**Live files.** `Maintenance/Refresh-BehaviorOutputIndex.R` added two rows to
`output_index.csv` (`00-history`, `15-inputs`), and 12 notes gained the
retained-original location. The new index has SHA-256
`A1CB1E2B643B2DADE2F4926400DDFF6761B94A654BCF1B317BE2C0CB195961BF`; its
predecessor is
`_migration_control/output_index_before_remaining_roots_20260926.csv`
(`783BDFB7A7FE5316ABA04F416FC80AD62D6C9475518E08D00D27AA0298C08AB9`).
Four live READMEs (root, `history/`, `analyses/`, `foundations/`) were
replaced by their sources at `8868f1d` after backups to `_migration_control/`:

| Backup | SHA-256 |
| --- | --- |
| `analysis_ready_README_before_remaining_roots_20260926.md` | `B6382FE2EDD60B0B8D06A7F48FC83A43326CFC239186C80DA82182E3BC4E8045` |
| `history_README_before_remaining_roots_20260926.md` | `EDE9A1196E93EC76CE02DD93CE8BF303DA829A5614C5C5A1EF28A69F7704BF91` |
| `analyses_README_before_remaining_roots_20260926.md` | `FFE05B008DF7B6F4A85DC5915E9CD9F1496F533C0D65615AF6D992A0DD35BD39` |
| `foundations_README_before_remaining_roots_20260926.md` | `CA4ED10C04643CBF6FEA3FE141917A5682AC5A41AA0F358E177CCD7C55188A25` |

Earlier the same day:

- `QUARANTINE_MANIFEST.csv` was edited to `reversible = FALSE` with a
  not-to-be-restored note (now SHA-256 `7C3719B4...`). The original is at
  `_migration_control/QUARANTINE_MANIFEST_before_no_restore_20260925.csv`
  (`42CF893E...`).
- The two superseded `output_index_contract_*.csv` copies moved to
  `_migration_control/superseded_index_copies_20260924/`.

No scientific output was changed or rerun.

## 2026-09-26 copy parity check after the archive

Until now no check compared copy and original for the groups whose originals
were archived without a residual inventory: the `06`-sourced groups, the
spatial groups from `03` and the two copies made on 2026-09-26.
`Maintenance/Test-BehaviorCopyParityInventory.ps1` now covers all 40 activated
groups at once; it is read-only. Each receipt matched its rows in exactly one
reviewed plan: 36 in the order `Prepare` uses today, three
(`adaptation_kinetics_10min`, `sleep_like_inactivity_10min`,
`phase_organization_10min`) in ordinal order and `systems_phenotyping_5min` in
plan-file order. All 2,181 planned files match their plan hash, both in the
semantic copy and in the retained original under `history/original_layout/`.
The seven dashboard metadata files pinned by the `systems_dashboard_5min`
receipt match that receipt. No copy root holds an unplanned file. The per-row
report (2,181 rows, SHA-256
`C4A5ED16339E9E222DC6200D12141DFA25CF0954B350871584451D72B500E467`) is kept
outside the repository as
`rfid_numbered_root_archive_review_20260924/copy_parity_20260926.csv`.

## 2026-09-26 Stage 14 rerun on the new layout

Stage 14 had last run on 2026-09-22, on the old layout. It now reads its
endpoint from the canonical CombZ table (`c82eca5`); the restructured workbook
no longer has the `zScore` sheet it read before. It was first run in guarded
sandboxes under `C:\Users\topohl\Documents\s14_sandbox_20260926_*`, with the
wrapper and launcher from two adversarial reviews (scripts in
`rfid_numbered_root_archive_review_20260924/stage14_sandbox_scripts/`). The
wrapper redirects the three Stage 14 output groups into the sandbox and stops
the process before any write, delete or device opens outside it. Every run's
post-scan of the live RFID tree, `SIS_Analysis`, the repository and the
neighbouring folders found no change.

- **Validation run** (the 2026-09-22 endpoint input: the pre-restructure
  workbook's `zScore` sheet, SHA-256 `BF257C2C...`, the file the restructure was
  built from). The first attempt crashed natively (0xC0000005) inside
  `cairo_pdf` while writing `Fig_social_reorganization_dynamics.pdf`, after
  162 s. The crash did not reproduce in isolation, and a rerun of the same
  configuration completed. Compared with the live outputs:
  - every live file was reproduced, and every table matches except for these:
    - recorded paths now name the semantic layout;
    - the GAMM trajectory features resolve, where the 2026-09-22 run had used a
      wrong path (`gamm_trajectory_features/5min_based`) and kept a placeholder
      domain;
    - three mirrored `Fig_integrated_systems_dashboard` copies and three run
      manifests are new;
    - one documentation sentence was reworded;
  - figures differ only by PDF creation dates and unseeded jitter.
- **Corrected run** (canonical CombZ). Compared with the validation run, the
  first-night outputs are identical and 32 CombZ-dependent dashboard tables
  change. The dashboard's group labels already matched the corrected outcome
  groups (0 of 111 differ), so the live dashboard had combined corrected labels
  with the uncorrected CombZ.
  - Movement-only prediction: cv R² 0.130 -> 0.149.
  - Integrated model: cv R² 0.209 -> 0.212.
  - Female PC1-CombZ association: rho -0.33 (q 0.013) -> -0.29 (q 0.030).
  - No sign changes, and no result crossed 0.05.

The comparison reports are in the review folder:
`stage14_live_vs_asrecorded_files.csv` (SHA-256 `51B187B7...`),
`stage14_live_vs_canonical_files.csv` (`0EE3C29A...`) and
`stage14_asrecorded_vs_canonical_files.csv`.

**Promotion (approved).** The corrected sandbox outputs replaced the live
contents of the three semantic folders, in these steps:
1. All 320 live files were backed up to
   `_migration_control/stage14_before_rerun_20260926/` and hash-verified
   (`backup_manifest.csv`, SHA-256 `D08D51E9...`). The longest backup path has
   279 characters, so a restore must use PowerShell 7.
2. The reviewed files were staged locally. The sandbox root was rewritten to
   the live root in the two tables that recorded it:
   `manifest/input_output_manifest.csv` and
   `tables/systems_robustness_audit.csv`.
3. The staged files were copied into place, and every one of the 326 live files
   was verified against its staged hash (`promote_manifest.csv`, `95F21AB4...`).

The three folders changed as follows:

| Group | Unchanged | Rewritten | Added |
| --- | ---: | ---: | ---: |
| `systems_dashboard_5min` | 182 | 118 | 6 |
| `first_night_10min` | 8 | 2 | 0 |
| `first_night_5min` | 8 | 2 | 0 |

The rerun is recorded in
`docs/behavior_output_producer_reruns/stage14_20260926.csv` (SHA-256
`3FCE6FFC...`). The copy-parity check and the Stage 14 residual inventory accept
a changed copy only through that record: its prior hash must be the activation
hash, and the retained original must still match the plan. After the promotion
all three read-only checks pass: the Stage 14 residual inventory, the
foundation inventory, and the copy-parity check. In the copy-parity check all
2,181 originals match their plans; 2,061 copies match the plan unchanged, and
120 match the recorded rerun (116 dashboard files, the two changed dashboard
metadata files, and two per first-night folder). The 6 added files match the
record, and no copy root holds an unplanned file. Stage 16's recorded
hashes, Stage 27 and the release bundle predate this rerun and were not
regenerated.

## 2026-10-02 Stage 14 domain heatmaps promoted

Commit `d2b4704` (Stage 14 domain heatmaps for the CC1 first dark phase, the
CC1 first light phase and all phase blocks) ran in a guarded sandbox
(`stage_sandbox_20261002_dhmfinal`, a clean checkout of the commit, with the
wrapper and launcher of the 2026-09-26 rerun). Compared with the live outputs:
- 88 files changed as the redesign intended (47 added, 41 rewritten);
- 66 dashboard files changed through upstream drift since 2026-09-26. The
  Stage 09 5-min tables were rerun on 2026-09-27, and commits `bac46bf` and
  `1999d07` changed two provenance rows;
- nothing else changed: the engine effect summary and the Stage 27 inputs are
  identical.

**Promotion (approved).** Script `promote_stage14_rerun_20261002.ps1` in the
workspace `e9_domain_heatmap_redesign_2026-10-01`:
1. All 326 live files were backed up to
   `_migration_control/stage14_before_rerun_20261002/` and hash-verified.
2. The files were staged with the sandbox root rewritten to the live root in
   `manifest/input_output_manifest.csv`, `tables/systems_robustness_audit.csv`
   and `tables/systems_computation_integration_audit.csv`.
3. The staged files were copied into place, and every one of the 373 live
   files was verified against its staged hash.

**Record.** The rerun is recorded in
`docs/behavior_output_producer_reruns/stage14_20261002.csv` (SHA-256
`4D137131...`; 373 rows: 47 added, 141 rewritten, 185 unchanged; byte-level,
so files whose only change is a PDF date, a timestamp or unseeded jitter count
as rewritten).
- Its prior hashes are those of the 2026-09-26 record. A read-only check before
  the promotion had found the live Stage 14 files byte-identical to that record.
- Its hashes are the live files as they stood before the next promotion.
- It was written on 2026-10-03, together with the next record.

## 2026-10-03 Stage 14 domain heatmaps v3.1 promoted

**What ran.** Commits `d112d48` (CON contrasts with the animals as units, the
manuscript figure format, review fixes) and `b56b3a1` (a neutral figure-theme
name, and the manuscript's blue/orange scale for every diverging scale) ran in
a guarded sandbox (`stage_sandbox_20261003_rc2patch`: `d112d48` plus the
`b56b3a1` change set as a patch).

**Comparisons.**
- `d112d48` against the live outputs: 82 expected changes and none
  unexpected. The engine effect summary and the Stage 27 inputs are
  identical, and an independent validator passes 32 of 32 checks.
- `b56b3a1` on top of it: only 10 figures change colour (30 files and the
  figure inventory); every table is identical.
- The diverging scale (#4C566A / #D8D2C7 / #D98B3A) is the manuscript
  palette, now on the Exp9_manuscript master as `dae6676`.

**Promotion.** Script `promote_stage14_rerun_20261003.ps1`, same steps:
1. All 373 live files were backed up to
   `_migration_control/stage14_before_rerun_20261003/`.
2. 374 files were promoted; the one added file is
   `tables/systems_sis_zero_event_light_blocks.csv`.
3. Every live file was verified against its staged hash.

**Record.** `docs/behavior_output_producer_reruns/stage14_20261003.csv`
(SHA-256 `1C601438...`; 374 rows: 1 added, 143 rewritten, 230 unchanged).

**Checks after the promotion.** All three read-only checks pass: the Stage 14
residual inventory, the foundation inventory, and the copy-parity check.
- 2,181 originals match their plans.
- 1,997 copies match the plan, and 184 match a recorded rerun.
- The 54 files added by recorded reruns match their records.
- No copy root holds an unplanned file.

Stage 16's recorded hashes, Stage 27 and the release bundle were not
regenerated.

## 2026-10-05 Stage 14 promoted with the manuscript palette v2

**What ran.** Commit `9f45a71` (one colour source, `Functions/manuscript_palette.R`:
navy/beige/red groups, diverging high end `#96460A`) ran in a guarded sandbox
from a clean checkout of that commit (`s14src_20261005_9f45a71_palv2`, sandbox
`stage_sandbox_20261005_palv2b`). A first attempt (`stage_sandbox_20261005_palv2`)
ended in a native R crash (0xC0000005) three minutes in, while writing early
figures, as on 2026-10-03; its post-scan found nothing. The retry passed: R exit
0, no post-scan finding.

**Checkout.** Since `d2df1df`, `.gitattributes` gives three files CRLF on
checkout (`Functions/rfid_canonical_inference.R` and the two `docs/stage30`
registry copies), and `git archive` applies it. `make_stage14_checkout_dhm.ps1`
now restores their committed LF bytes before its blob check; the checkout record
lists them as `eol_restored_to_lf`.

**Comparisons.** Against the live outputs, with the expected-change manifest
`expected_changes_palette_v2.csv` (figures only):
- 374 files: 205 identical or known run-to-run noise, 169 expected changes,
  none unexpected.
- The engine effect summary `systems_sis_domain_effect_summary.csv` is
  identical (84 rows).
- Of the tables, only the manifest timestamp and `output_figure_inventory.csv`
  changed.
- The independent validator passes 32 of 32 checks.

**Promotion.** Script `promote_stage14_rerun_20261005.ps1`, same steps as on
2026-10-03:
1. All 374 live files were backed up to
   `_migration_control/stage14_before_rerun_20261005/`.
2. 374 files were promoted; none was added.
3. Every live file was verified against its staged hash.

**Record.** `docs/behavior_output_producer_reruns/stage14_20261005.csv`
(SHA-256 `8AB89CBE...`; 374 rows: 177 rewritten, 197 unchanged; the 20
first-night files are unchanged).

**Checks after the promotion.** All three read-only checks pass: the Stage 14
residual inventory, the foundation inventory, and the copy-parity check.
- 2,181 originals match their plans.
- 1,969 copies match the plan, and 212 match a recorded rerun.
- The 54 files added by recorded reruns match their records.
- No copy root holds an unplanned file.

## 2026-10-05 Retired outputs moved to history/retired/; index and READMEs refreshed

**Decision.** The legacy products and the GAMM Stages 21 and 23-26 were retired, and Stages 20 and 22 kept as
descriptive Extended Data candidates (`docs/LEGACY_AND_GAMM_RETIREMENT_2026-10-05.md`). Their outputs moved unchanged
to `analysis_ready/history/retired/` with `Maintenance/Invoke-BehaviorOutputRetirement.ps1`. For each move the tool
lists every file with its SHA-256, writes the manifest, copies, verifies each copy against the manifest, and only then
removes the original. It writes a receipt (`state = archived`) under `_migration_control/retired_outputs/`. Before the
moves, no frozen pin list, bundle provenance table or migration receipt named any of these folders; only old index
backups did.

| Label | Source | Target | Files | Bytes | Manifest SHA-256 |
|---|---|---|---:|---:|---|
| `s21_gamm_20261005` | `pipeline/21_cc1_active_longitudinal_gamm` | `history/retired/21_cc1_active_longitudinal_gamm` | 45 | 6723909 | `3d29f0d820d3...` |
| `s23_gamm_20261005` | `pipeline/23_first_inactive_gamm` | `history/retired/23_first_inactive_gamm` | 49 | 2776891 | `57e89c778d33...` |
| `s24_gamm_20261005` | `pipeline/24_cc1_inactive_longitudinal_gamm` | `history/retired/24_cc1_inactive_longitudinal_gamm` | 50 | 7996890 | `8f048d9a564e...` |
| `s25_gamm_20261005` | `pipeline/25_repeated_cagechange_inactive_gamm` | `history/retired/25_repeated_cagechange_inactive_gamm` | 51 | 10670194 | `b8765ad5ff4a...` |
| `s26_gamm_20261005` | `pipeline/26_gamm_manuscript_outputs` | `history/retired/26_gamm_manuscript_outputs` | 69 | 7169298 | `f80c95f22335...` |
| `s27_main_figure_20261005` | `pipeline/27_behavior_main_figure` | `history/retired/27_behavior_main_figure` | 238 | 14260256 | `dfb0f3d74eda...` |
| `s16_manuscript_behavior_20261005` | `manuscript/behavior` | `history/retired/manuscript_behavior` | 9 | 461239 | `7ac101b80f25...` |
| `s20_leftovers_20260907` | `pipeline/20_first_night_gamm/10min` (11 listed files) | `history/retired/20_first_night_gamm_20260907` | 11 | 573765 | `ed5f7eba7a4c...` |
| `s22_leftovers_20260907` | `pipeline/22_repeated_cagechange_acute_gamm/10min` (15 listed files) | `history/retired/22_repeated_cagechange_acute_gamm_20260907` | 15 | 2344186 | `7088c1c10fd0...` |

- The longest target path has 241 characters (Stage 27 candidates).
- The Stage 20 and 22 leftovers are the 2026-09-07 files of superseded runs (old classification): ten
  `tables/first_night_*.csv` and `audit/first_night_ar1_sequence_proof.csv` for Stage 20, and fourteen
  `tables/allcc_*.csv` and `audit/allcc_ar1_sequence_proof.csv` for Stage 22. No code reads those names. The current
  files stay in place (32 and 39).
- `analysis_ready/manuscript/` is now empty.

**A failed first attempt at Stage 27.** The first `s27` run stopped after 117 of 238 copies. Explorer's `Thumbs.db`
caches are hidden system files, and `Get-Item` without `-Force` cannot see them. Nothing had been removed. All 238
originals were checked against the manifest (no difference), and the 117 partial copies were checked against it too.
The partial copies and the orphan manifest were then deleted. The tool now passes `-Force` wherever it touches a file,
its fixture test (`Testing/tests/test_behavior_output_retirement.ps1`) includes a hidden system file, and the retry
completed.

**Verification after all moves.** Every target file was re-hashed against its manifest and every manifest against its
receipt: 537 files, 0 mismatches, no original left behind.

**Navigation files.**
- `history/README.md`: the old copy was backed up to `_migration_control/history_README_before_retirement_20261005.md`
  (SHA-256 `de43f938...`). The new copy is `docs/BEHAVIOR_HISTORY_DIRECTORY_README.md` (`92f324ec...`), which adds
  `retired/`.
- `output_index.csv` and `README.md`: `Maintenance/Refresh-BehaviorOutputIndex.R --write
  --backup=output_index_before_retirement_20261005.csv`. The backups are `output_index_before_retirement_20261005.csv`
  (`5c1c45df...`) and `README_before_retirement_20261005.md` (`ffc7c192...`).
  - The index went from 65 to 77 rows. It adds 16b, 16c, 29, 29b, 30, 30b, 31, 32, the frozen configuration and the
    three registry copies, and changes 94 cells in 31 rows: runner profiles, the retired rows and the new roles of 20
    and 22 (`descriptive_candidate`) and 28 (`active_producer_pinned`). New SHA-256 `7fa3362e...`.
  - A second dry run reports no difference. All 67 canonical paths exist.
  - `README.md` (`cffcc3a3...`) now starts at the frozen bundles.

**Rationale retired.** The Stage 27 reason for keeping `analyses/systems_dashboard/5min/stats_tables/
systems_sis_domain_effect_summary.csv` byte-identical across Stage 14 reruns (its candidate B-alt asserted the
contrast orientation) retired with Stage 27. The file stays a live Stage 14 output. Its column contract and the
per-file SHA-256 in the producer-rerun records still apply, and any change in its p or q values needs a rerun record.

**Checks after the moves.** All three read-only checks pass, with the counts of the morning's Stage 14
promotion: the Stage 14 residual inventory (700 numbered files, 371 activated copies), the foundation inventory,
and the copy-parity check (2,181 originals and 1,969 copies match the plans, 212 match a recorded rerun, 54 files
added by recorded reruns, no unplanned file).
