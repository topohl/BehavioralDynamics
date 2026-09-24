# Retiring numbered behavioral output roots: proposed transaction

Status: prepared tooling and unresolved reader gate, 2026-09-24. No numbered source directory has been
moved, renamed, hidden, or deleted. The receipt-activated semantic copies are
already selected by current readers. This design is a separate operation from
the completed copy activations.

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
`Activate`, and `Rollback` actions. `Inspect` is read-only. `Prepare` pins the
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
Its temporary-fixture move and drift tests passed. It has not been used to
move a live root or create an archive receipt. Three live read-only
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
1,469-file `06` and 702-file `12` manifests. No root archive receipt was
prepared and no source was moved.
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
queue was refused and created no receipt. The transaction tool has not moved
or prepared any live source.

## Reader and writer gate before `Activate`

1. `mmm_behavior_output_layout_state()` now accepts a separate, exact-root
   archive receipt and verifies that the retained source is in the selected
   location. Until an archive receipt is activated, the old path is required
   exactly as today. Temporary-fixture tests cover prepared, transferring,
   activated, invalid, and interrupted locations; no receipt was created on
   the live tree. Manual audit reader/writer review and a separately approved
   live `Activate` are still required before a live move.
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
   candidates); the archived-location run remains untested. The independent phase-classification bug
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
archived-location Stage 10 run remains untested.
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
queue carries a prior `ready` value. The live queue has no `ready` rows and no
reviewed `ArchivePath` gate exists, so live `Prepare` remains closed. The
choice between an archive-path review and a full scientific replay remains
open; the identity comparison remains unvalidated without a verified baseline.

On 2026-09-24, the read-only replay planner still listed 37 scripts, 34 fresh
replay outputs, three console-only checks, and 16 same-run prerequisites. The
historical replay path fixture covered 36 queued scripts; the separate identity
comparison driver fixture covered the remaining script. Both passed. The
numbered-root receipt resolver and Stage 16 output-index source tests also
passed. These fixtures exercise routing and output placement, not the saved
scientific results. The identity driver deliberately labels unknown and mixed
baselines as unsuitable for a clean before/after comparison. R reported locale
startup warnings and package build-version warnings; none caused these path
tests to fail. The Stage 10 archived-location parity run remains outstanding.
All three live numbered roots were still present and there were zero numbered-
root archive receipts at that check. No reviewed live gate was created.

Before any live activation, test each root on synthetic interrupted states:
missing or extra files, changed hashes, pre-existing destination, a destination
inside a source, failed move, stale manifest, invalid receipt, and an old path
recreated by a writer. Then run the read-only live inventory and existing
foundation, Stage 14, Stage 10, and output-index checks. Freeze writers during
the short `transferring` interval. Archive one root at a time, beginning with
the root whose reader and writer inventory is fully resolved; do not choose by
size alone. Obtain a separate approval for `Activate` after the manifest,
reader changes, and test results are reviewable.
