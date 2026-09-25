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
entry point. Stage 16 generates `output_index.csv`, a stage and output-group
navigation map: path, producer, manuscript role, migration status and historical
location. It is not the file-level migration plan; see
`docs/BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv`. The live index was refreshed from
the current Stage 16 source definition after the bounded Stage 14/19,
Stage 02/06, Stage 07, Stage 05, Stage 08, Stage 04, Stage 15, Stage 11–13,
two manual supporting-analysis cutovers, and the inactive-phase QC audit cutover;
prior copies are
preserved under
`analysis_ready/_migration_control/`.
For the activated first-night, spatial, dyadic, five-minute social, and
ten-minute trajectory-feature, five-minute state-space, HMM, and ten-second
temporal-instability, behavior-proteomics, and current ten-minute adaptation,
sleep-like inactivity, phase-organization, nonlinear-dynamics, and
systems-phenotyping, and inactive-phase QC audit groups,
the source definition reads
the migration receipt for each group: an activated receipt records the semantic
path, while a blocked or prepared group remains at its current path. The full
Stage 16 exporter was not rerun for the cutover.

### Active semantic analysis outputs

The twenty-one activated groups from Stages 02, 04–08, 11–15, and 19, two
manual supporting analyses, and one manual QC audit are
under `analysis_ready/analyses/`. See its `README.md` and the live
`output_index.csv` for their semantic names and retained source paths.

### Stage-addressed pipeline layout

```text
analysis_ready/pipeline/<stage_id>_<stage_name>/<resolution>/{tables,figures,audit}/
```

Current migrated writer roots include:

```text
analysis_ready/pipeline/03_movement_phase_stats/10min/
analysis_ready/pipeline/09_early_prediction/10min/
analysis_ready/pipeline/10_systems_prediction/10min/
```

Stages 03 and 09 supply selected manuscript claims; Stage 10 is an exploratory
systems extension. Manually run scientific and manuscript stages 20–27 also use
`pipeline/`. Local Stage 28 outputs exist under `pipeline/28_rfid_behavioral_domains/`,
but its producer is currently untracked work and is not a release contract.
Migrated trees use `tables/`, `figures/` and `audit/` rather than the older
`publication_panels/` level.

### Manuscript package

```text
analysis_ready/manuscript/behavior/
```

The recommended entry point for anyone reading the results:
`Behavioral_Source_Data.xlsx` plus `primary_results.csv`,
`supplementary_results.csv`, three source-data tables, `provenance.csv`,
`validation.csv` and `manifest.csv`.

### Remaining historical output branches

Some active or historical groups retain numbered originals. The complete
`03_derived_metrics/`, `06_behavioral_dynamics/` and
`12_systems_neuroscience_summary/` roots are retained unchanged under
`analysis_ready/history/original_layout/`, each under an archive receipt in
`analysis_ready/_migration_control/numbered_root_archive/`. Older resolutions
from `06_behavioral_dynamics/` have receipt-selected copies under
`analysis_ready/history/`. Their status is recorded per output group in
`output_index.csv`. Stage 01 now reads and writes through
`analysis_ready/foundations/behavior_metrics/`; its 20 current metric/QC
files were copied from `03_derived_metrics/` with identical hashes, and the
archived root keeps all 53 originals, including separate identity-audit and
spatial outputs. Activated groups from other numbered roots retain their
originals in place for provenance. Remaining branches need separate
dependency review before any further migration.

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
