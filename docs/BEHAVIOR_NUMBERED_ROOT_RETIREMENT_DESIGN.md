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
| `03_derived_metrics/` | `history/original_layout/03_derived_metrics/` | 52 files |
| `06_behavioral_dynamics/` | `history/original_layout/06_behavioral_dynamics/` | 1,469 files, including 9 hidden `Thumbs.db` |
| `12_systems_neuroscience_summary/` | `history/original_layout/12_systems_neuroscience_summary/` | 702 files, including 2 hidden `Thumbs.db` |

The prefix is preserved *inside* the provenance area because it identifies the
original layout and gives historical paths an unambiguous mapping. It disappears
from the human-facing top level. `history/original_layout/` must not be treated
as a current analysis input or mixed with the receipt-selected resolution
copies already under `history/social_networks/`, `history/state_space/`, etc.
The 2,223-file complete inventory includes 2,212 ordinary files counted in
the scientific audits and 11 hidden Windows thumbnail caches. It includes
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
| `06_behavioral_dynamics/` | 1,469 | `17777e9cdf5c4176671748faf6d5bf6caa394fbd359b615fc1fd0f4b941364c0` |
| `12_systems_neuroscience_summary/` | 702 | `a1b551f0da80fa39b62a72c74c6c02e65539f507f746e1f60a52ed519ed4fc5c` |

All three manifests passed a separate live `Verify` pass after `Build`.
They remain snapshots. A future `Activate` must rerun `Verify`; even a Windows
thumbnail-cache change will invalidate its snapshot until reviewed.
`Maintenance/Invoke-BehaviorNumberedRootArchive.ps1` implements those actions
against the exact paths above. Temporary fixtures passed activation, repeat
activation refusal, changed-source refusal, rollback from activated state,
and rollback after interruptions on either side of the directory move. The
same transaction fixture now checks that the R pipeline resolver reads the
prepared, activated, and rolled-back receipt states correctly. It
requires a SHA-pinned reader gate with one `ready` row for every script in the
queue before `Prepare` or `Activate`. A live read-only `Inspect` of the Stage 14
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
   scientific calculations have not been rerun. The other 27 rows still need
   reader/writer review.
5. Preserve numbered strings that are historical provenance in the Stage 16
   registry. Resolve live reads through current path helpers; do not rewrite
   provenance labels to make past runs appear to have used semantic paths.

Before any live activation, test each root on synthetic interrupted states:
missing or extra files, changed hashes, pre-existing destination, a destination
inside a source, failed move, stale manifest, invalid receipt, and an old path
recreated by a writer. Then run the read-only live inventory and existing
foundation, Stage 14, Stage 10, and output-index checks. Freeze writers during
the short `transferring` interval. Archive one root at a time, beginning with
the root whose reader and writer inventory is fully resolved; do not choose by
size alone. Obtain a separate approval for `Activate` after the manifest,
reader changes, and test results are reviewable.
