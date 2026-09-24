# Remaining numbered behavioral outputs: dependency audit

Read-only snapshot before the ninth cutover: 2026-09-23. This follows the 18 receipt-activated groups
and 700 copied files in `BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv`. File counts and
sizes below were checked on the live `S:` tree at that time; code relationships were checked
against the repository. The two supporting roots in this snapshot have since
been copied and activated as `nonlinear_dynamics/5min/` and
`systems_phenotyping/5min/`; their originals remain. See the activation record
for the current state. This document authorizes no further cutover.

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
| `03_primary_raw_movement_phase_stats/` | 31 files, 2.92 MB | Historical Stage 03 output; Stage 16 and comparison helpers retain a documented fallback | Superseded, retained compatibility | `03` reflects the stage but this is no longer its write location. Keep until fallback retirement is tested. |
| `04_model_outputs/`, `05_figures/` | 6 model files and 5 figure files, all spatial occupancy originals already copied | Stage 19 owns them; Stage 10 boundary audit checks they are excluded from feature discovery | Retained provenance | Numbers are historical storage labels. Keep source paths as the activation receipts require. |
| `16_manuscript_behavior_report/` | 5 files, 0.44 MB | Old Stage 16 exporter; current manuscript entry point is `manuscript/behavior/`; a path-length test still names the old root | Superseded report | `16` matches the former stage but is redundant in navigation. Retain as comparison evidence; no current write should target it. |
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
16 still has executable per-artifact fallbacks into the old Stage 03 tree,
and a path-length regression test still scans the old Stage 16 tree. The
task's blocked candidate maps propose semantic `history/` paths; all 122
numbered originals remain and no copy or receipt was created.

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
The six `audit_rfid_*` families depend partly on untracked scripts and
cross-read one another; they remain outside the next automatic batch.

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
The exact-basename consumer scan is lower-bound evidence: eleven manual audit
scripts still reference selected dashboard artifacts while writing into
historical audit trees. Their reruns must be routed and revalidated before
being presented as current-dashboard audits.
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
result to a manuscript claim. The live index has 51 rows. See the activation
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
Four files are named in the manuscript registry across conditional active,
unpromoted inactive, and excluded first-night/occupancy claims. Neither
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
