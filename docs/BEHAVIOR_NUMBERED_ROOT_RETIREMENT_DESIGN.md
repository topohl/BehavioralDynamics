# Retiring numbered behavioral output roots: proposed transaction

Status: all three numbered roots were activated under approved `ArchivePath`
gates on 2026-09-25 (`03_derived_metrics` 13:45:50, `12_systems_neuroscience_summary`
14:10:25, `06_behavioral_dynamics` 14:16:32 UTC). Each sits unchanged under
`history/original_layout/<root>/` with an activated receipt, and nothing was
deleted. Current readers select the receipt-activated semantic copies. The
sections up to "Readiness review, 2026-09-25" describe the design and its
state before the moves; the dated sections after it record what was done.

## Scope and destination

The first candidates are the three large, mixed-layout roots. Move each whole
root, without changing its internal filenames, to a clearly labeled provenance
area under the same `analysis_ready/` volume:

| Present path under `analysis_ready/` | Proposed retained-original path | Live inventory at review |
| --- | --- | ---: |
| `03_derived_metrics/` | `history/original_layout/03_derived_metrics/` | 53 files (52 at initial review) |
| `06_behavioral_dynamics/` | `history/original_layout/06_behavioral_dynamics/` | 1,469 files, including 9 hidden `Thumbs.db` |
| `12_systems_neuroscience_summary/` | `history/original_layout/12_systems_neuroscience_summary/` | 702 files, including 2 hidden `Thumbs.db` |

The prefix is preserved *inside* the provenance area because it identifies the
original layout and gives historical paths an unambiguous mapping. It disappears
from the human-facing top level. `history/original_layout/` must not be treated
as a current analysis input or mixed with the receipt-selected resolution
copies already under `history/social_networks/`, `history/state_space/`, etc.
The current 2,224-file complete inventory includes 2,213 ordinary files
(2,212 at the initial audit, plus the new Stage 01 QC file) and 11 hidden
Windows thumbnail caches. It includes
retained-only records as well as originals with active semantic copies; an
archive manifest must cover every file, not just the activation plans.

## Separate manifest and receipt

Do not edit or replace the existing per-group activation receipts. A versioned
archive manifest should record, for every original file, its relative path,
length, SHA-256, and assigned root, plus the hash of the manifest itself. A
separate root-level archive receipt should record the old and archived roots,
manifest hash, file count, byte count, state, and timestamps. Its states should
be `prepared`, `transferring`, and `activated`; an unknown, partial, or mismatched
state must stop affected readers. The original `source_retained=true` promise
remains true after relocation, but the resolver needs an explicit archived
source-location rule before any source is moved.

The transaction tool now has independent `Inspect`, `Prepare`, `Verify`,
`Activate`, `Rollback`, and `Abandon` actions. `Inspect` is read-only. `Prepare` pins the
reviewed manifest and reader gate, rehashes the complete source inventory, and
checks for a vacant archive target.
`Verify` repeats the hashes and checks the receipt and all paths. `Activate`
marks the root `transferring`, moves the directory on the same volume, verifies
the archived inventory, then marks it `activated`. An interruption must leave
the resolver failing closed; resuming or rolling back must inspect both path
locations and their exact contents before changing state. Never infer the
active source from directory existence alone, and never delete a directory as
an automatic rollback shortcut.

The manifest component is now implemented separately as
`Maintenance/Invoke-BehaviorNumberedRootArchiveManifest.ps1`. `Build` writes a
new UTF-8 CSV outside the numbered source and refuses to overwrite one;
`Verify` rejects changed, missing, extra, duplicate, and unsafe paths after
checking size and SHA-256. `Verify -Location Archived` checks the same manifest
at the proposed retained-original path and rejects a recreated original root.
Its temporary-fixture move and drift tests passed. It only builds and
verifies manifests and never moves a root or writes a receipt; the
2026-09-25 moves were made by `Invoke-BehaviorNumberedRootArchive.ps1`,
which calls its `Verify`. Three live read-only
source hash passes wrote these versioned manifests in the repository:

| Source root | Complete files | Manifest SHA-256 |
| --- | ---: | --- |
| `03_derived_metrics/` | 52 | `601a7a1ad84720dd8049a8faf9211f0facc75679688dbf47709cb3fae1d69f4b` |
| `03_derived_metrics/` (2026-09-24 candidate) | 53 | `53265050b732fd4e14ebef273e608453ec551e4362c292b4e5589320e811c5dd` |
| `06_behavioral_dynamics/` | 1,469 | `17777e9cdf5c4176671748faf6d5bf6caa394fbd359b615fc1fd0f4b941364c0` |
| `12_systems_neuroscience_summary/` | 702 | `a1b551f0da80fa39b62a72c74c6c02e65539f507f746e1f60a52ed519ed4fc5c` |

All three manifests passed a separate live `Verify` pass after `Build`.
They remain snapshots. A future `Activate` must rerun `Verify`; even a Windows
thumbnail-cache change will invalidate its snapshot until reviewed.
On 2026-09-24, Stage 01 wrote a new `qc/first_night_seed_provenance.csv`
(58,105 bytes) under `03_derived_metrics/`. The original 52-file manifest was
preserved and now fails `Inspect` on the file count. The versioned 53-file
candidate was built and independently verified against the live source. All
52 old entries retain identical paths, sizes, and SHA-256 values; the new QC
file is the sole addition. Read-only `Inspect` still passed for the unchanged
1,469-file `06` and 702-file `12` manifests. At that point no root archive
receipt had been prepared and no source had moved.
The new QC file is absent from the activated `foundations/behavior_metrics/`
copy; it is retained in the numbered original and has not been promoted or
interpreted as a new canonical product. The current Stage 01 default selects
the semantic foundation through its activation receipt, but an explicit
output override could still have targeted the old path. Stage 01 now guards
its resolved write path, rejecting the numbered or archived root after archive
control begins while allowing the separate cookie-habituation output root.
The source of this particular old-root write was not established from the file
alone.
`Maintenance/Invoke-BehaviorNumberedRootArchive.ps1` implements those actions
against the exact paths above. Temporary fixtures passed activation, repeat
activation refusal, changed-source refusal, rollback from activated state,
and rollback after interruptions on either side of the directory move. The
same transaction fixture now checks that the R pipeline resolver reads the
prepared, activated, and rolled-back receipt states correctly. It
requires a SHA-pinned reader gate with one row for every script in the
queue before `Prepare` or `Activate`. The default `ScientificReplay` gate
requires `ready`; the explicit `ArchivePath` gate requires
`archive_path_ready` and both evidence fields described below. Each reviewed
row must also pin the
current audit script with `script_sha256`; a changed script closes the gate even
when the gate CSV itself still has its reviewed hash. The gate hash and queue
hash are retained in the archive receipt. This verifies review integrity, not
the scientific result of a replay. A live read-only `Inspect` of the Stage 14
root passed with 702 manifest files; `Prepare` using the current unresolved
queue was refused and created no receipt. At that point the transaction tool
had not moved or prepared any live source.

## Reader and writer gate before `Activate`

1. `mmm_behavior_output_layout_state()` now accepts a separate, exact-root
   archive receipt and verifies that the retained source is in the selected
   location. Until an archive receipt is activated, the old path is required
   exactly as today. Temporary-fixture tests cover prepared, transferring,
   activated, invalid, and interrupted locations. Before 2026-09-25 no receipt
   existed on the live tree, and a live move required manual audit
   reader/writer review and a separately approved live `Activate`. Both were
   done for all three roots; see the dated sections below.
2. Keep Stage 10's current 18-group semantic discovery and its 606-path parity
   check. Its virtual numbered-path sort key preserves input precedence; the
   current group scan does not require the numbered `06` root.
3. Review executable fallback branches separately. Stage 09's explicit legacy
   candidate and Stage 14's non-primary-resolution output branch still name old
   roots. Preserve a supported replay path or change the branch under a focused
   test; do not silently select an archived file as current evidence.
4. Inventory each `Testing/audits/` path that names one of the three roots. In
   particular, 34 audit scripts name the old Stage 14 root, and a baseline
   static scan found direct write calls in 32 of them after including the
   repository's `write_table()` helper. That scan is a review queue, not proof
   that all 32 write into that root or that the other two are read-only.
   Historical replay must use a named output location and must
   not recreate a numbered top-level root after archive activation.
   The user chose to keep these audits replayable with explicit **new** output
   folders. Replay inputs must resolve the retained original lineage through
   the archive receipt; a rerun must never write into the archived originals
   or reuse an existing replay output folder.
   `behavior_output_archive_audit_script_queue.csv` lists all 37 audit scripts
   naming at least one of the three roots, with separate reference and common
   write-call flags from the baseline scan. The flags are deliberately not a
   live-consumer classification and are not regenerated as scripts are edited.
   `audit_first_night_candidate_set_scores.R` now resolves its three historical
   input roots through the archive-aware accessor, reads its prerequisite
   anchor from retained audit provenance, and requires
   `MMM_BEHAVIOR_AUDIT_REPLAY_ID` to create a new output directory under
   `analyses/historical_audit_replays/<run-id>/first_night_candidate_set_scores/`.
   `audit_first_night_candidate_set_effects.R` reads those new scores from the
   *same* replay id and writes to its own unused folder.
   `audit_first_night_candidate_set_decision.R` reads both new prerequisite
   folders from that id and writes into a third unused folder. These three
   queue rows are `path_prepared_unvalidated`: the scripts were parsed and
   their path contracts tested, but no scientific replay was run or validated.
   The console-only `audit_first_night_candidate_set_algebra_crosscheck.R`
   now reads the retained Stage 01 original through the same archive accessor;
   it creates no output directory. The console-only
   `audit_first_night_window_provenance.R` now reads retained Stage 08 and 14
   originals through that accessor. Their paths are prepared but their
   calculations were not rerun. `audit_first_night_production_parity.R` is an
   indirect writer: its producer now receives an explicit new replay folder,
   while the comparison audit and roster read retained originals. Its path is
   prepared but the numerical parity calculation was not rerun. The
   `audit_first_night_time_anchor.R` replay now produces the canonical window
   audit in its own new folder, and `audit_first_night_domain_scores_v2.R`
   consumes that file through the same replay id while reading the retained
   Stage 01, 08, and 14 originals. The dwell partition-stability audit reads
   retained Stage 01 and writes its own replay folder; the shipped-versus-refit
   audit reads that same-run prerequisite plus retained Stage 01 and 08, then
   writes a separate folder. These four scripts are path-prepared only; their
   scientific calculations have not been rerun. The v2 heatmap and window
   sensitivity audits now consume the same-run v2 scores and component tables
   from their replay folder, keep Stage 14 comparisons on retained originals,
   and write to distinct replay folders. Their calculations and figures have
   not been rerun. The superseded v1 domain-scores audit and exploratory HMM
   component audit now read the retained originals and write to independent
   replay folders, removing their former shared-writer destination. Their
   scientific calculations have not been rerun. The Stage 09 stale-artifact
   audit now reads the retained Stage 01 and legacy Stage 09 families through
   archive-aware roots, retains its snapshot comparison, and writes its report
   to a separate replay folder. It was not rerun. The read-only Stage 10 discovery parity audit now
   resolves the retained original through the archive receipt and constructs
   its expected semantic list from the reviewed source-to-target plans. It
   passed on the live pre-archive tree (1,459 mapped originals, 606 ordered
   candidates); a synthetic archived-location fixture passed, while the live
   post-move run remains pending. The independent phase-classification bug
   counterfactual now reads the retained Stage 01 original and writes to its
   own replay folder; it was not rerun. The temporal HMM component audit now
   reads retained Stage 01 and Stage 08 originals and writes its epoch metrics
   to a fresh replay folder. The gap-aware, QC sensitivity, and locomotion
   dominance audits consume those same-run metrics; their other Stage 14 inputs
   resolve to the retained original, and each has its own output folder. None
   of these calculations were rerun. The standalone HMM identifiability,
   partition robustness, and semantic erasure probes now read retained Stage
   01/08 inputs and write to separate replay folders. They were not rerun.
   The HMM component foundation audit now resolves all Stage 01, 08, and 14
   original inputs through the archive receipt and writes to its own replay
   folder. Its heavy calculations have not been rerun; downstream audits still
   need to read that replay output. The independent Stage 14 provenance audit
   and its addendum now read retained Stage 01 and Stage 08 inputs, then write
   to separate replay folders. Their numerical checks were not rerun. That
   The three Phase A follow-ups now consume the same-run HMM component
   foundation and write to distinct replay folders, with their additional
   Stage 01, 08, and 14 inputs resolved from retained originals. They were
   not rerun. The component redundancy audit now reads both foundation epoch
   metrics and its length-bias check from the same replay id. The HMM profile
   audit reads retained Stage 01/08 originals. Both write distinct new folders
   and were not rerun. The component-model and longitudinal audits now consume
   the same-run foundation; the construct comparison also consumes the same-run
   redundancy proposal and retained Stage 14 comparison table. Each writes a
   separate replay folder. None of their models were rerun. The identity
   correction baseline comparison now uses an explicit new replay folder when
   outputs are requested. Its Stage 09 legacy fallback resolves an archived
   numbered root while preserving its explicit baseline provenance. Synthetic
   driver tests passed, including an activated archive receipt; no live
   comparison was rerun. The final special-case row, the active RFID domain
   comparison producer, now reads the receipt-selected five-domain first-
   night results and Stage 28 tables, then writes any rerun into a fresh
   `rfid_legacy_vs_new_domains` replay folder. The four saved supporting
   tables under `analyses/rfid_domain_comparison_audit/` remain the activated
   copy and are not overwritten. The new replay has not been run or promoted.
   All 37 queue rows are path-prepared, but none is `ready` for the live
   numbered-root archive gate. The default replay gate needs scientific
   replay; the separate path gate needs recorded path and writer review.
5. Preserve numbered strings that are historical provenance in the Stage 16
   registry. Resolve live reads through current path helpers; do not rewrite
   provenance labels to make past runs appear to have used semantic paths. The
   historical HMM audit note now says reruns use fresh replay folders while
   its legacy path remains the provenance of the saved files.
6. The wider code scan found archive-sensitive writers outside the 37-audit
   queue. The optional non-primary Stage 14 output branch, archived Stage 08
   producer, and two stale legacy pipeline runners now call
   `mmm_behavior_numbered_writer_root()`. It refuses any write into a numbered
   top-level root when an archive receipt exists, including `prepared` or
   `activated`, or an unreceipted archive directory is present. A fixture
   tested these refusals. The legacy runners and archived producer were not
   executed. Stages 04-08 now also check their resolved output directory before
   writing. Their default semantic destinations remain available; an optional
   resolution inside a numbered root stops after archive control begins, and
   writing inside `history/original_layout/` is always rejected. Synthetic
   prepared and activated receipt checks passed. These producers were parsed,
   but their scientific calculations were not rerun. The Stage 09 legacy
   fallback used by the Stage 14 upstream registry now resolves the retained
   original through the root archive receipt; a synthetic archived fallback
   still carries the explicit legacy warning. Two archived raw-movement
   analyses now resolve their complete Stage 03 input root through the archive
   receipt; their code parsed and a synthetic archived Stage 03 root resolved,
   but the analyses were not rerun. The stale legacy structure check also
   resolves its Stage 03 input through the receipt; it was parsed but not run.
   Any remaining archived readers require review before live activation.

`Rscript Maintenance/Get-BehaviorAuditReplayPlan.R` prints a static replay
order from the 37-script queue. It parses each script without sourcing or
running it, extracts literal same-run replay input/output IDs, rejects unknown
producers, duplicate outputs, and cycles, then orders producers before their
consumers. The current plan has 34 scripts with replay output IDs, three
without them, and 16 with same-run prerequisites. It is an execution order,
not a scientific validation result or permission to run the audits. The
`invocation` column marks the Stage 10 parity check's required RFID-root
argument and the identity comparison's separate baseline and provenance
requirement. Those two cannot be treated as ordinary unattended Rscript calls.
The identity comparison must not infer a pristine pre-correction baseline.
Either reviewed gate must pin script hashes. Only the default
`ScientificReplay` gate requires validated replays. The queue has 34
`path_prepared_unvalidated` rows, two
`live_read_only_checked` rows, and one `live_numbered_only_checked` row; none
is `ready` for the archive gate.
`Maintenance/New-BehaviorArchiveReaderGateTemplate.ps1 -Output <new CSV path>`
creates a separate, non-overwriting 37-row review template with each current
script hash and blank path and writer evidence columns. Every new template row
resets to `needs_reader_writer_review`;
the prior queue state is retained only as context. Its fixture confirmed zero
`ready` rows. Reviewing that template and changing states is a separate decision; the
tool does not grant archive readiness.
Five first-night audits previously printed failed assertion rows while exiting
successfully. They now print the same diagnostics and then stop on any `FAIL`.
A focused synthetic test exercised the actual assertion branches with passing
and failing registers. This verifies exit behavior only; it does not establish
that any scientific replay passed.

`Maintenance/Preflight-BehaviorAuditReplay.R` now checks the 37-script plan,
receipt-selected retained roots, unused replay ID, and a separate identity
baseline with status and source note without creating an output folder. Its
synthetic fixture passed. A read-only live preflight on 2026-09-24 found all
three retained roots and a fresh proposed replay ID; it reported
`mechanically_ready: FALSE` because no independent identity baseline was
supplied. An incomplete preflight exits with an error, so it cannot be
mistaken for approval to run all scripts. The nearby `.old/` tree contains
older activity materials, not an `analysis_ready/` comparison baseline; no
baseline was inferred from it. This is a concrete blocker for an unattended
all-script replay.
The two other console-only checks were run read-only: the candidate-set
algebra crosscheck exited successfully, with three formula deviations at or
below `4.441e-16` (109-111 finite animals), while reporting two non-finite
proximity values; the first-night window-provenance check exited successfully
and found 12-hour first blocks at both HMM resolutions for 109 animals. The
Stage 10 discovery parity check had previously passed on the live numbered
tree. These results do not mark the 37-script queue `ready`, and the
live post-move Stage 10 run remains untested.
The queue records the two console checks as `live_read_only_checked` and Stage
10 as `live_numbered_only_checked`; both states remain below `ready`.

## Archive gate scope still to settle

The default transaction gate requires every queued audit row to say `ready`
before it can prepare a numbered-root move. Its script-hash check, path fixtures,
writer guards, complete source manifests, and live Stage 10 discovery parity
establish mechanical path safety. They do not establish that a full scientific
rerun reproduces every old audit result. A full 37-script replay is currently
impossible because the identity comparison has no verified independent
pre-correction baseline. The user confirmed that no such baseline is available.

The transaction now has two explicit gate kinds. `ScientificReplay` is the
default and requires each gate row to be `ready`. `ArchivePath` must be
selected explicitly at both `Prepare` and `Activate`; it requires each row to
be `archive_path_ready` with nonempty `path_review_evidence` and
`writer_review_evidence`. The gate file, queue file, script hashes, and gate
kind are pinned in the prepared receipt. These evidence fields are review
records, not proof that numerical results reproduce. The template initializes
every row as `needs_reader_writer_review` with blank evidence, even when the
queue carries a prior `ready` value. The live queue has no `ready` rows. Until
the reviewed `ArchivePath` gates were committed (`03` in `3aa33e6`, `06` and
`12` in `2611ee6`), live `Prepare` was closed. Those gates record a path and
writer review for the move only. A full scientific replay has not been run,
and the identity comparison remains unvalidated without a verified baseline.

On 2026-09-24, the read-only replay planner still listed 37 scripts, 34 fresh
replay outputs, three console-only checks, and 16 same-run prerequisites. The
historical replay path fixture covered 36 queued scripts; the separate identity
comparison driver fixture covered the remaining script. Both passed. The
numbered-root receipt resolver and Stage 16 output-index source tests also
passed. These fixtures exercise routing and output placement, not the saved
scientific results. The identity driver deliberately labels unknown and mixed
baselines as unsuitable for a clean before/after comparison. R reported locale
startup warnings and package build-version warnings; none caused these path
tests to fail. A separate fixture populated empty files at the 1,459 mapped
original and semantic paths, plus the two supporting feature groups, then
activated a synthetic root archive receipt. The actual Stage 10 read-only
parity audit passed against that archived layout, including the 606-path order
check. This tests discovery after routing without comparing scientific table
contents; the live post-move parity run remains outstanding.
All three live numbered roots were still present and there were zero numbered-
root archive receipts at that check. No reviewed live gate was created.

## Readiness review, 2026-09-25

Evidence (read-only live checks and temporary fixtures; no scientific rerun):

- Live state: all three numbered roots present, `history/original_layout/`
  absent, no `_migration_control/numbered_root_archive/` receipt. Manifest
  `Verify` passed for `03` (53-file manifest, SHA `53265050...`), `06`
  (1,469 files, 18,194,653,380 bytes, `17777e9c...`) and `12` (702 files,
  `a1b551f0...`). The only change since the 52-file `03` manifest is
  `qc/first_night_seed_provenance.csv`. It is recorded as retained only and
  unpromoted in
  `behavior_output_activated_plans/derived_metrics_post_activation_additions.csv`;
  its writer is still not established.
- The foundation and Stage 14 residual inventories are now archive-aware and
  both passed read-only against the live tree. Before the addendum the
  foundation inventory failed on the 53rd file.
- Transaction defects fixed and covered by fixtures: `Move-Item` split a root
  across both locations when a directory rename failed (PowerShell 7.6.6,
  locked file on NTFS); `Move-Item -Force` deleted the receipt when its
  replacement failed; manifests were compared by culture-sorted position
  (638 `06` and 94 `12` rows are not in ordinal order, so Windows PowerShell
  5.1 would report false drift). The tools now use `Directory.Move`,
  `File.Move` with overwrite, and path-keyed verification, and require
  PowerShell 7.2. A live gate must use the repository queue. Each gate row
  pins the queue hash and a hash of the shared path code; an `ArchivePath`
  gate is scoped to one root. Changing actions hold a lock, `Abandon` retires
  a prepared receipt, and `Rollback -AcceptInventoryDrift` renames a drifted
  archive back without trusting it.
- On the lab share a same-share directory rename succeeded while a file
  below it was open (temporary probe under `S:\Lab_Member\Tobi\`, removed).
  An open file therefore may not block `Activate`; writers must be stopped,
  and the post-move hash check fails closed on any change.
- Reader and writer fixes outside the queue are listed in
  `behavior_output_archive_out_of_queue_review.csv`.
- An independent reviewer and an adversarial verifier inventoried every read
  and write of the 37 queued scripts. 34 are `path_ok_test_gap`,
  `audit_stage09_stale_artifacts.R` and `audit_stage10_semantic_discovery_parity.R`
  are `path_gap`, and the identity comparison is `special_invocation`.
  `test_historical_audit_replay_io_contract.R` now parses all 37 scripts:
  every write is under its own replay root, no read uses a numbered-root
  literal outside the resolvers, and the 18 same-run edges match file for
  file. Three mutations were detected.

Path length (blocks `06` and `12` at the proposed destination on this host):
`LongPathsEnabled` is 0, and R 4.5.1 here cannot open a path of 260 or more
characters (`file.exists` FALSE, `file.info` NA) although `list.files` lists
it; PowerShell 7 can. Under `history/original_layout/` the longest `03`
path is 198 characters, but 20 `06` paths reach 260-264 and 7 `12` paths
reach 260-265. The archive tool would verify those files while R readers,
including the Stage 10 discovery parity audit, could not read them. `03` is
not affected. `Prepare` and `Activate` now refuse a root whose archived
paths would reach 260 characters while long paths are disabled.

External consumers: SLEAPanalyzer (`exp9-validation-study`) scripts 01, 07
and 09, and its bundle builder through 09, read `03_derived_metrics/qc`
directly. They now read the hash-identical foundation copy, or find the
retained-only phenotype table through the `03` receipt (SLEAPanalyzer
`817de68`, local, not pushed). Unversioned iCloud copies of those scripts
were left unchanged at the maintainer's request and will stop after the
`03` move. `Analysis/verify_publication_release.R` re-hashes rc1 sources
recorded under the numbered `12` path. Live navigation files name the
numbered roots and need updating with each root's `Activate`.

Per-root scope: `docs/behavior_output_archive_gate_03_derived_metrics_draft.csv`
proposes `archive_path_ready` for all 37 scripts for the `03` move only,
with path and writer evidence per row. Every `review_state` remains
`needs_reader_writer_review`, so it cannot open the gate. A temporary-fixture
`Prepare` accepted it after the proposed states were applied, and refused it
for `06`. Its hashes pin the current scripts, queue and shared code; any
later edit to those files requires regenerating it.

Open, not archive-blocking for `03`: the scientific replay remains blocked by
the missing identity baseline; replay folders have no completion marker, so
a consumer can read a failed producer's partial output; the replay output
check is not atomic across concurrent runs; several same-run reads are
optional; the Stage 09 stale audit's five `06` families were quarantined in
2026-09 and are absent before and after any move.

## 03 prepared, 2026-09-25

The maintainer approved the `ArchivePath` standard for the `03` move only.
The reviewed gate `behavior_output_archive_gate_03_derived_metrics.csv`
(commit `3aa33e6`) marks all 37 queued audits `archive_path_ready` for that
root; its SHA-256 with normalized line endings is
`c01aa48a7e437a27595162d9a92022f97371d6c040cc0b817ea31f42cc3cbf61`.
Before `Prepare`, no process, scheduled task, or peer session was writing to
the RFID tree, and the newest `03` file was still the 2026-09-24 QC table.

Live `Prepare` (2026-09-25 12:31 UTC) rehashed all 53 files against the
53-file manifest and wrote
`_migration_control/numbered_root_archive/03_derived_metrics.json`
(state `prepared`, 53 files, 3,162,803,458 bytes, manifest `53265050...`,
gate `c01aa48a...`, queue `31131d89...`; receipt SHA-256 `a4e21a7b...`).
Nothing moved; `history/original_layout/` was not created and no lock or
temporary file remained. Tool `Verify` and `Inspect` then passed with 53
files. R readers still resolve the original `03` and the foundation copy.
Code on this branch refuses writes into `03`, including through `ensure_dir()`,
while `06`, `12`, the foundation copy, and the cookie-habituation tree stay
writable. This branch is unpushed. On `main` and the release branches,
Stages 01 and 19 still write into `03` by default without any receipt check,
so none of them may run against this RFID tree.
The foundation residual inventory passed, and SLEAPanalyzer's resolver reads
the original. `qc/first_night_seed_provenance.csv` stays retained and
unpromoted. `Abandon` would release the receipt without moving anything.

## 03 activated, 2026-09-25

Following `BEHAVIOR_03_ARCHIVE_ACTIVATION_CHECKLIST.md`, the repository,
gate and writer checks passed, and the full test suite passed. `Activate`
then moved the root (13:45:50 UTC, 60 s; 53 files rehashed before and after
the rename). R now resolves the retained 03 source to the archive and opens
all 53 files there (longest path 198 characters). The foundation and spatial
copies remain the active roots. The live navigation files and
`output_index.csv` were updated after backups; see
`BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md`.

Before any live activation, test each root on synthetic interrupted states:
missing or extra files, changed hashes, pre-existing destination, a destination
inside a source, failed move, stale manifest, invalid receipt, and an old path
recreated by a writer. Then run the read-only live inventory and existing
foundation, Stage 14, Stage 10, and output-index checks. Freeze writers during
the short `transferring` interval. Archive one root at a time, beginning with
the root whose reader and writer inventory is fully resolved; do not choose by
size alone. Obtain a separate approval for `Activate` after the manifest,
reader changes, and test results are reviewable.

## 06 and 12 activated, 2026-09-25

The maintainer approved moving `06` and `12` if the path-length finding that
had held them back was not a real blocker. On review it limits reading, not
correctness. On this host (`LongPathsEnabled=0`), R 4.5.1 cannot open
archived paths of 260 or more characters; PowerShell 7 and the transaction
tool can. All 20 such paths in `06`, and one plotly stylesheet in `12`, have
hash-identical copies in activated semantic groups, of at most 220
characters for the `06` files and 230 for the stylesheet. The other six are historical HMM audit CSVs in
`12/5min_based/audit_hmm_state_architecture/first_night_domain_heatmap/`
with no copy and no code reader. The two queued audits that read that folder
open files of 242 and 249 characters. The only queued input that failed at
live path lengths was the Stage 10 parity audit's listing, fixed in
`039f51b`.

`Prepare` and `Activate` now refuse over-long archived paths unless given
the SHA-256 of the reviewed list (`fd1df88`). The receipt records the count
and hash, and `Activate` refuses a list that changed after `Prepare`. The
reviewed gates (`2611ee6`) mark all 37 queued audits `archive_path_ready`
for each root: `06` `d717ec86...`, `12` `c658d44a...`. Before `Prepare`:

- the full suite passed (55 R, 6 PowerShell);
- no Explorer window was open on either root;
- R and Python sessions used no CPU over 10 s;
- the newest files dated from 2026-09-22 (`06`) and 2026-09-23 (`12`).

| Root | Files | Bytes | Accepted long paths | Prepared (UTC) | Activated (UTC) | Receipt SHA-256 |
| --- | ---: | ---: | --- | --- | --- | --- |
| `12_systems_neuroscience_summary` | 702 | 209,524,954 | 7 (`ea855b79...`) | 14:07:08, 4 s | 14:10:25, 8 s | `0E483FE1...` |
| `06_behavioral_dynamics` | 1,469 | 18,194,653,380 | 20 (`0841a9fb...`) | 14:10:01, 173 s | 14:16:32, 367 s | `671B5A34...` |

After the moves:

- R opens 1,449 of 1,469 archived `06` files and 695 of 702 archived `12`
  files; the longest paths are 264 and 265 characters.
- The 18 and 10 active roots still resolve to their semantic copies, and
  code on this branch refuses writes into either archive.
- The foundation and Stage 14 residual inventories pass.
- The index note now names a retained original only if its folder moved
  with the root (`cc47627`). The Stage 09 legacy folder had been
  quarantined before the move and is absent.

Drift after activation: at 14:17:17 UTC a 20,480-byte hidden
`5min_based/figures/Thumbs.db` appeared in the archived `12` root. It is a
Windows Explorer thumbnail cache. All 702 manifest files still matched their
hashes, and `Verify` failed only on the file count. A copy was kept in the
review work folder (SHA-256 `F13E3D06...`), the maintainer removed the file,
and `Verify` then passed for `12` (702 files). It also passes for `03` and
`06`. A second Explorer cache, created at 14:32:03 UTC in the unarchived
`14_nextgen_behavioral_phenotyping/` root, has no effect: the inventory
tools skip `Thumbs.db`. Avoid thumbnail views in archived folders.

Open risks:

- On `main`, the release branches, `exp9-upstream-endpoint-corrections`
  (`35cda80`) and the local `audit/hmm-state-architecture` (`2ffa556`), code
  reads and writes the three numbered roots by fixed path without receipt
  checks. Both named branches are ancestors of this branch. None of these
  branches may run against this RFID tree. At 14:32:55 UTC GitHub Desktop
  switched the shared worktree to `exp9-upstream-endpoint-corrections` for
  32 seconds, leaving a stash. Nothing was written to `analysis_ready/` in that window.
- The six uncopied HMM audit CSVs are readable only with PowerShell 7 or on
  a long-path host.

## Post-activation dependency sweep, 2026-09-25

A read-only sweep with adversarial verification covered code outside the 37
queued audits, the open review rows, documentation and live navigation, and
consumers outside this repository. It found no current reader of a missing
path and no writer into the archives. Commit `bd9c2f0` closed the defensive
gaps it confirmed:

- the release builder, verifier check 4 and both Stage 27 builders refuse
  `/history/original_layout/`;
- the dashboard metadata refresher and the identity repair utility guard
  their targets;
- the cookiehab runner restores its options;
- Stage 27 labels its first-night source data with the file it reads;
- the residual inventories require PowerShell 7;
- CI installs `jsonlite`.

The per-surface status is in
`behavior_output_archive_out_of_queue_review.csv`. Three items remain open:
the PowerShell fixtures are not run in CI; the live quarantine manifest
still names `06_behavioral_dynamics/` as its restore target; and two
superseded `output_index_contract_*.csv` copies sit beside the live index.
The last two are live files and need the maintainer's approval to change.
A first version of the cookiehab runner test regenerated the cookiehab
preprocessing and Stage 01/02 outputs; see
`BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md`.

## Every remaining tree, 2026-09-26

All three open items above are now closed:

- The quarantine manifest says the trees are not to be restored.
- The two index copies moved into `_migration_control/`.
- A Windows CI job runs the PowerShell fixtures (`decf77c`, not yet run on
  GitHub).

The maintainer then asked to move every remaining tree into the new
structure. A read-only dependency map with adversarial verification covered
all 18 remaining top-level folders (`wf_21124ce5-f95`). It proposed moving 17
of them unchanged under their own receipts and leaving `_migration_incoming/`,
a tool working folder, out of the archive.

The archive is no longer tied to three roots (`c426e16`):

- `MMM_NUMBERED_BEHAVIOR_ROOTS` and the shared PowerShell list name all 20
  trees; `test_numbered_root_lists_agree.R` checks that they match.
- One root-level resolver serves whole trees and output groups.
- The archive tools validate `RootName` against the shared list.
- Readers that built old paths literally now use the resolver: the Stage 11–13
  five-minute branches, the non-five-minute supporting branches, the Stage 15
  proteomics folder, one queued audit and the identity comparison.
- The release builder and verifier map a recorded path through any root's
  receipt.

Two copies were activated first, so that no current reader depends on the
archive: `history/tracking_integrity/10sec/` for the May 2026 QC snapshot (the
release builder refuses `history/original_layout/`), and
`foundations/proteomics_module_scores/` for the Stage 15 inputs. All 17 trees
were then archived, `14_nextgen_behavioral_phenotyping` last, once no File
Explorer window was open inside it; see the activation record. No top-level
tree of the old numbered layout remains in `analysis_ready/`. Forty-two
archived files have paths of 260 to 270 characters; nine of them have no copy
and no reader.

Open items:

- Stage 14 has not been run on the new layout. Its pre-existing resolver
  defect is fixed (c518e96): `mmm_phase_analysis_resolution_root()` rejected
  the multi-resolution preference vectors Stage 14 passes, so Stage 14 could
  not start. The resolver now accepts the same five resolutions as the other
  resolution helpers; only 10min reads a copy, the rest read the retained
  original. Stages 11-13 and the two supporting producers now guard their
  output root. The fix is covered by fixture tests only.
- Branches other than this one (`main`, the release branches,
  `exp9-upstream-endpoint-corrections`) read and write every one of these
  trees by fixed path and must not run against this RFID tree.
