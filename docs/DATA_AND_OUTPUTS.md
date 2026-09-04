# Data and outputs

Where the data live, what this repository does and does not contain, and what can
be regenerated without the raw experimental files.

---

## This repository contains code, not data

No raw experimental data, no derived metric tables and no rendered figures are
version-controlled here. The tracked content is analysis code, helper modules,
verification code, documentation, and a small number of manuscript provenance
tables retained as forensic artifacts under
`manuscript/archive/BehavioralDynamics_schema_preproduction_audit/data/`.

`.gitignore` excludes the rendered figure outputs of both manuscript builders,
which are regenerated from tracked source.

---

## Local data and output root

Everything the pipeline reads and writes lives under the E9 project root:

```text
S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID
```

Approximately 11.8 GB across ~20,200 files. Top level:

| Path | Size | Role |
|---|---|---|
| `analysis_ready/` | 10.2 GB | All pipeline outputs. The only tree the current code writes to. |
| `MMMSociability/` | 0.78 GB | Raw data, preprocessed data and historical outputs. **Local directory name, not the repository name.** |
| `statistics/` | 0.51 GB | Pre-pipeline statistical outputs (legacy) |
| `cookiehab/` | 0.13 GB | Separate cookiehab experiment |
| `publication_ready/` | 0.08 GB | Older hand-assembled publication staging |
| `.old/`, `Sleep/`, `lme_sis_activity/` | 0.11 GB | Historical |

Within `MMMSociability/`:

- `raw_data/` — the raw `E9_SIS_B*_CC*_AnimalPos.csv` exports. **The only
  irreplaceable input.**
- `preprocessed_data/` — produced by
  `Formatting/E9_SIS_AnimalPos-preprocessing_parallell.r`; the canonical input of
  Stage 01.
- `raw_tracking_qc_rfid_loss/` — produced by
  `Formatting/00a_raw_tracking_qc_rfid_loss.R`; read by Stage 14.

---

## The `analysis_ready` role

`analysis_ready/` is the single output root. Its own `README.md` states the
entry point, and `output_index.csv` is the machine-readable map of every stage:
canonical path, producer script, manuscript role, migration status and legacy
path.

### Canonical (migrated) layout

```text
analysis_ready/pipeline/<stage_id>_<stage_name>/<resolution>/{tables,figures,audit}/
```

Only two stages are migrated so far:

```text
analysis_ready/pipeline/03_movement_phase_stats/10min/
analysis_ready/pipeline/09_early_prediction/10min/
```

These are exactly the two stages that carry manuscript claims. Migrated trees use
only `tables/`, `figures/` and `audit/` — note there is no `publication_panels/`
level, unlike the pre-migration trees.

### Manuscript package

```text
analysis_ready/manuscript/behavior/
```

The recommended entry point for anyone reading the results:
`Behavioral_Source_Data.xlsx` plus `primary_results.csv`,
`supplementary_results.csv`, three source-data tables, `provenance.csv`,
`validation.csv` and `manifest.csv`.

### Not-yet-migrated stages

Stages 00, 01, 02, 04–08 and 10–15 still write to historical locations such as
`analysis_ready/03_derived_metrics/`, `analysis_ready/06_behavioral_dynamics/`
and `analysis_ready/12_systems_neuroscience_summary/`. This is recorded per stage
in `output_index.csv` as `legacy_pending_migration`. Migration was deliberately
out of scope for the publication restructuring.

### Quarantine

```text
analysis_ready/_quarantine_legacy_s09/     402 files, 167 MB
```

Stale Stage 09 trees that violate the current contract — 113 animals instead of
the canonical 111, zero-padded non-canonical `AnimalNum`, both phases present
where the analysis is Active-only. `QUARANTINE_MANIFEST.csv` records, per tree,
the generating script, the artifact date, the specific contract violations, what
replaced it, and that the action is reversible. Quarantined data must never be
resolved by any reader, and the release builder refuses to touch this tree.

---

## Known duplication in the output tree

Several canonical figures also exist in pre-migration trees — for example
`Fig18c_cage_change_phase_mean_movement_corrected_stats.svg` exists in the
canonical Stage 03 tree and in two legacy trees, with different hashes and dates.
Similarly, Stage 09 panels such as `behavior_only_repeated_cv_ladder.svg` have up
to four legacy copies.

This is not corruption: the legacy copies are earlier generations retained for
provenance. It matters only because a human browsing the tree can pick the wrong
one. The mitigation is that code never browses — it resolves through the
canonical-first contract — and the release bundle copies only resolved canonical
artifacts and records their hashes.

Figures that appear 5 or 10 times across `analysis_ready/` are usually the same
panel rendered at each bin resolution, which is expected fan-out rather than
duplication.

---

## Manuscript release bundle

`Analysis/build_publication_release.R` assembles a self-contained, hash-verified
bundle:

```text
<RFID_ROOT>/releases/E9_behavior_manuscript_<release_id>/
```

It is strictly copy-only: it never moves, deletes or rewrites a source artifact,
and it refuses to read from quarantined or legacy trees. Contents, guarantees and
failure modes are documented in `docs/PUBLICATION_RELEASE.md`.

The live `analysis_ready/` tree is **not** reorganised to build a release.
Filesystem migration is a separate, later decision — see the end of this file.

---

## What can and cannot be regenerated

| Artifact | Regenerable without raw data? |
|---|---|
| `raw_data/` AnimalPos exports | **No.** Irreplaceable experimental measurement. |
| `preprocessed_data/` | No — requires `raw_data/`. |
| Stage 01 metric tables | No — requires `preprocessed_data/`. |
| All downstream stage outputs (03–16, 19) | No — require Stage 01. |
| Manuscript package | No — requires Stages 03 and 09. |
| Release bundle | No — copies resolved canonical artifacts. |
| Figure 1 candidate staging | No — copies from the analysis tree. |
| The archived provenance schema figures | **Yes** — its `data/` tables are tracked in git, so `R/10`, `R/20` and `R/30` re-render from the repository alone. |
| Portable test suite | **Yes** — 17 scripts, entirely self-contained. |

In short: **everything scientific depends on `raw_data/`, and `raw_data/` cannot
be reconstructed.** It should be treated as the primary preservation target,
independent of any git or release process.

---

## Backup and preservation priority

1. `MMMSociability/raw_data/` — irreplaceable.
2. `analysis_ready/manuscript/behavior/` — the manuscript package, tiny (~0.5 MB).
3. `analysis_ready/pipeline/` — canonical migrated outputs backing every claim
   (~54 MB).
4. This repository at the release commit.
5. Everything else — large and regenerable given 1.

---

## Future local filesystem migration (not now)

The output tree carries real historical debt: stages writing to numbering that no
longer matches their stage ID (`12_systems_neuroscience_summary` is Stage 14,
`15_behavioral_adaptation_kinetics` is Stage 11, `16_sleep_like_inactivity_metrics`
is Stage 12), three superseded raw-movement trees with no active reader, and the
duplication noted above.

A future canonical layout would migrate every stage into
`analysis_ready/pipeline/<stage_id>_<stage_name>/<resolution>/` and move
superseded trees to `analysis_ready/_archive/`.

**This must not be done before the manuscript is frozen.** Release packaging and
filesystem migration are separate operations: packaging copies and verifies
without touching sources, whereas migration rewrites paths that current code
resolves and would invalidate every recorded provenance path. The audit
supporting a future migration is `docs/LOCAL_OUTPUT_TREE_AUDIT.csv`, which
proposes an action per tree but performs none of them.
