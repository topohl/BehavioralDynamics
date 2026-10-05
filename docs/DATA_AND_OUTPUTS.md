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

Approximately 11.8 GB across ~20,200 files in the 2026-09-03/04 snapshot below.
`analysis_ready/` has grown since; the archived `06_behavioral_dynamics` root
alone holds 18,194,653,380 bytes. Top level:

| Path | Size | Role |
|---|---|---|
| `analysis_ready/` | 10.2 GB | All pipeline outputs. The only tree the current code writes to. |
| `MMMSociability/` | 0.78 GB | Raw data, preprocessed data and historical outputs. **Local directory name, not the repository name.** |
| `statistics/` | 0.51 GB | Pre-pipeline statistical outputs (legacy) |
| `cookiehab/` | 0.13 GB | Separate cookiehab experiment |
| `publication_ready/` | 0.08 GB | Older hand-assembled publication staging |
| `.old/`, `Sleep/`, `lme_sis_activity/` | 0.11 GB | Historical |

On 2026-09-25 the cookie-habituation preprocessing outputs
(`cookiehab/preprocessed_data/`, `cookiehab/qc/`) and the Stage 01 and Stage 02
outputs under `cookiehab/analysis_ready/` were regenerated unintentionally,
with the stage code at `e0be716`. The maintainer chose to keep them; see
`docs/BEHAVIOR_OUTPUT_ACTIVATION_RECORD.md`.

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
entry point. `Maintenance/Refresh-BehaviorOutputIndex.R` writes `output_index.csv`
from `Functions/behavior_output_index.R` (the definition lived in Stage 16 until
its retirement on 2026-10-05): a stage and output-group navigation map with path,
producer, manuscript role, status, runner and historical location. It is not the
file-level migration plan; see `docs/BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv`. The live
index was refreshed from the source definition after the bounded Stage 14/19,
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
Stage 16 exporter was never rerun for these cutovers.

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

Stage 09 is the prospective prediction input of the frozen release; Stage 03 has
no product reader since 2026-10-05; Stage 10 is an exploratory systems extension.
The descriptive GAMM Stages 20 and 22, Stage 28 and the frozen Stages 29, 29b, 30
and 32 also use `pipeline/`; the outputs of the retired Stages 21 and 23-27 are
under `history/retired/`.
Migrated trees use `tables/`, `figures/` and `audit/` rather than the older
`publication_panels/` level.

### Manuscript bundles

```text
analysis_ready/canonical/behavior_bundle/ebb_v101_20260929_b2ce507/
```

The recommended entry point for anyone reading the results is the frozen Stage 29
v1.0.1 bundle, which Exp9_manuscript imports; `canonical/figure_support_bundle/`
and `canonical/stage30_figure_bundle/` hold the other imported bundles. The
2026-09-22 Stage 16 package (`Behavioral_Source_Data.xlsx` and its CSV
companions) was retired on 2026-10-05 and is under
`analysis_ready/history/retired/manuscript_behavior/`.

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
analysis_ready/history/original_layout/_quarantine_legacy_s09/     393 files, 170 MB
```

Stale Stage 09 trees that violate the current contract — 113 animals instead of
the canonical 111, zero-padded non-canonical `AnimalNum`, both phases present
where the analysis is Active-only. `QUARANTINE_MANIFEST.csv` records, per tree,
the generating script, the artifact date, the specific contract violations and
what replaced it. Quarantined data must never be resolved by any reader, and
the retired release builder refused to touch this tree. The maintainer decided on
2026-09-25 that these trees are not to be restored, and the manifest now says
so. On 2026-09-26 the whole quarantine moved unchanged from
`analysis_ready/_quarantine_legacy_s09/` to its place under
`history/original_layout/`, under its own archive receipt (nine Explorer
thumbnail caches were set aside first). It must never be moved into
`history/original_layout/06_behavioral_dynamics/`, where it would reactivate the
Stage 09 legacy fallback.

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
canonical-first contract — and the frozen bundles copy only resolved,
hash-gated artifacts.

Figures that appear 5 or 10 times across `analysis_ready/` are usually the same
panel rendered at each bin resolution, which is expected fan-out rather than
duplication.

---

## Manuscript release bundle (retired)

`Analysis/build_publication_release.R` assembled a copy-only, hash-verified
bundle under `<RFID_ROOT>/releases/E9_behavior_manuscript_<release_id>/`
(`docs/PUBLICATION_RELEASE.md`). It was retired on 2026-10-05. The only release,
`releases/E9_behavior_manuscript_rc1` (2026-09-04), predates the CombZ correction
and the leading-bin fix and stays in place as a dated snapshot. The manuscript
imports the frozen bundles under `analysis_ready/canonical/` instead.

---

## What can and cannot be regenerated

| Artifact | Regenerable without raw data? |
|---|---|
| `raw_data/` AnimalPos exports | **No.** Irreplaceable experimental measurement. |
| `preprocessed_data/` | No — requires `raw_data/`. |
| Stage 01 metric tables | No — requires `preprocessed_data/`. |
| All downstream stage outputs (03–15, 19, 20, 22, 28–32) | No — require Stage 01. |
| Frozen bundles | No — copies of hash-gated outputs of the frozen runs. |
| The archived provenance schema figures | **Yes** — its `data/` tables are tracked in git, so `R/10`, `R/20` and `R/30` re-render from the repository alone. |
| Portable test suite | **Yes** — 78 scripts, entirely self-contained. |

In short: **everything scientific depends on `raw_data/`, and `raw_data/` cannot
be reconstructed.** It should be treated as the primary preservation target,
independent of any git or release process.

---

## Backup and preservation priority

1. `MMMSociability/raw_data/` — irreplaceable.
2. `analysis_ready/canonical/` — the frozen bundles, configuration and registries
   the manuscript imports.
3. `analysis_ready/pipeline/` — canonical migrated outputs backing every claim
   (~54 MB).
4. This repository at the commits the frozen runs record.
5. Everything else — large and regenerable given 1.

---

## Future local filesystem migration (not now)

Update 2026-09-25: part of this has since been done by receipt-activated
copies rather than by rewriting paths. The current Stage 11–13 ten-minute
outputs and the Stage 14 outputs are copied under `analysis_ready/analyses/`;
the older Stage 11–13 five-minute branches exist only in their archived
roots. The `03`, `06` and `12` numbered roots moved unchanged to
`analysis_ready/history/original_layout/` on 2026-09-25, and on 2026-09-26
every other top-level tree of the original layout followed, including the
superseded, retired and quarantined trees. The May 2026 QC
snapshot and the Stage 15 proteomics inputs were copied first, to
`history/tracking_integrity/10sec/` and `foundations/proteomics_module_scores/`.
The rest of this section, and `LOCAL_OUTPUT_TREE_AUDIT.csv` (a 2026-09-03
snapshot), describe the earlier state.

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
