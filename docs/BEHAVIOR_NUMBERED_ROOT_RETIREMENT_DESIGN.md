# Retiring numbered behavioral output roots: proposed transaction

Status: design and resolver foundation for review, 2026-09-24. No numbered source directory has been
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
| `06_behavioral_dynamics/` | `history/original_layout/06_behavioral_dynamics/` | 1,460 files |
| `12_systems_neuroscience_summary/` | `history/original_layout/12_systems_neuroscience_summary/` | 700 files |

The prefix is preserved *inside* the provenance area because it identifies the
original layout and gives historical paths an unambiguous mapping. It disappears
from the human-facing top level. `history/original_layout/` must not be treated
as a current analysis input or mixed with the receipt-selected resolution
copies already under `history/social_networks/`, `history/state_space/`, etc.
The 2,212 files include retained-only records as well as originals with active
semantic copies; an archive manifest must cover every file, not just the
activation plans.

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

The tool interface should have independent `Inspect`, `Prepare`, `Verify`,
`Activate`, and `Rollback` actions. `Inspect` is read-only. `Prepare` freezes and
hashes the complete source inventory and checks for a vacant archive target.
`Verify` repeats the hashes and checks the receipt and all paths. `Activate`
marks the root `transferring`, moves the directory on the same volume, verifies
the archived inventory, then marks it `activated`. An interruption must leave
the resolver failing closed; resuming or rolling back must inspect both path
locations and their exact contents before changing state. Never infer the
active source from directory existence alone, and never delete a directory as
an automatic rollback shortcut.

## Reader and writer gate before `Activate`

1. `mmm_behavior_output_layout_state()` now accepts a separate, exact-root
   archive receipt and verifies that the retained source is in the selected
   location. Until an archive receipt is activated, the old path is required
   exactly as today. Temporary-fixture tests cover prepared, transferring,
   activated, invalid, and interrupted locations; no receipt was created on
   the live tree. The archive manifest builder, transaction tool, and manual
   audit reader changes are still required before a live move.
2. Keep Stage 10's current 18-group semantic discovery and its 606-path parity
   check. Its virtual numbered-path sort key preserves input precedence; the
   current group scan does not require the numbered `06` root.
3. Review executable fallback branches separately. Stage 09's explicit legacy
   candidate and Stage 14's non-primary-resolution output branch still name old
   roots. Preserve a supported replay path or change the branch under a focused
   test; do not silently select an archived file as current evidence.
4. Inventory each `Testing/audits/` path that names one of the three roots. In
   particular, 34 audit scripts name the old Stage 14 root, and a static scan
   found common write or directory-creation calls in 27 of them. That scan is
   a review queue, not proof that all 27 write there or that the other seven
   are read-only. Historical replay must use a named output location and must
   not recreate a numbered top-level root after archive activation.
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
