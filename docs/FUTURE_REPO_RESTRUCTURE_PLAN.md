# Future repository restructure — plan only

**Nothing in this document has been executed.** It records a possible eventual
layout, maps current locations onto it, and states for each move what the
benefit is, what the risk is, what depends on it, and whether the path registry
introduced in `Functions/project_paths.R` already isolates the change.

The reason to write it now rather than later: the behavior main figure
(Stage 27) was the first consumer built against a semantic path registry instead
of hard-coded directories, which makes it the reference for what "already
isolated" means.

---

## Proposed eventual layout

```
R/                     reusable scientific helpers (currently Functions/)
analysis/              statistical producer scripts (currently Analysis/NN_*.R)
manuscript/            figure and table assemblers (currently Analysis/26, 27)
tests/                 portable contract tests (currently Testing/tests/)
audits/                data-dependent scientific audits (currently Testing/audits/)
outputs/               canonical analysis outputs (currently analysis_ready/pipeline/)
publication/           manuscript figures, source data, tables, manifests
docs/                  unchanged
```

The three conceptual boundaries that matter, and which new code already
respects:

```
statistical producers  ≠  manuscript assemblers  ≠  publication files
```

- A **producer** may fit models and own p-values. It must not know about panel
  letters, physical figure sizes or file formats.
- An **assembler** may know panel letters, millimetres and formats. It must not
  fit anything. Enforced for Stage 27 by
  `Testing/tests/test_behavior_main_figure_contracts.R` §3.
- **Publication files** are products. Nothing reads them back as analysis input.

---

## Current → future map

| # | Current | Future | Benefit | Risk | Dependencies | Isolated by registry? | Order |
|---|---|---|---|---|---|---|---|
| 1 | 9 stages hard-coding `project_root <- "S:/…"` | all call `mmm_project_root()` | one configurable data root instead of three parallel mechanisms; the repo becomes runnable on a machine that mounts the share elsewhere | low per stage, but touches validated stages 14, 15, 20–26, so each edit must be followed by a re-run byte-compare | none | **Yes** — `mmm_project_root()` already honours `getOption("mmm.project_root")`, `MMM_BEHAVIOR_PROJECT_ROOT` and `MMM_PROJECT_ROOT` | **1st** |
| 2 | `analysis_ready/pipeline/NN_*/<res>/` | `outputs/NN_*/<res>/` | separates canonical analysis outputs from the historical `analysis_ready` grab-bag | high: every legacy consumer path breaks at once | needs #1 first, plus a legacy-fallback pass in `resolve_behavior_artifact()` | **Partly** — Stage 27's inputs move by editing the `dir` closures in `.mmm_path_specs()`; every *other* consumer still constructs paths itself | 4th |
| 3 | `analysis_ready/pipeline/27_behavior_main_figure/` | `publication/behavior_main_figure/` | publication products stop living inside the analysis tree | very low | none | **Yes** — set `options(mmm.publication_root=)` or `MMM_PUBLICATION_ROOT`; asserted by test §12 | **2nd** |
| 4 | `Analysis/26_*.R`, `Analysis/27_*.R` | `manuscript/` | makes the producer/assembler boundary structural, not conventional | low; both are already fitting-free and unregistered in `run_all_analysis.R` | update `.github/workflows/portable-tests.yml` parse globs | n/a (source location, not data) | **3rd** |
| 5 | `Functions/` | `R/` | conventional R layout | medium: `source_mmm_helper()` and `find_mmm_repo_root()` both hard-code `Functions/` as the repo sentinel | one edit in `Analysis/_pipeline_setup.R`, plus the sentinel filename in `find_mmm_repo_root()` | n/a | 5th |
| 6 | `Testing/tests/` + `Testing/audits/` | `tests/` + `audits/` | separates portable contract tests from data-dependent audits at the top level | low, but the CI glob `Testing/tests/test_*.R` is the only test discovery mechanism | update the CI workflow in the same commit | n/a | 6th |
| 7 | `analysis_ready/12_systems_neuroscience_summary/…`, `…/16_manuscript_behavior_report/…`, and 13 other unmigrated stage trees | `outputs/NN_*/` | ends the two-layout split where only stages 03, 09 and 20–26 use the canonical pipeline layout | high: these are the trees with the most undocumented consumers | needs #1, #2 | **Partly** — the two Panel B keys and the Stage 16 keys are already registry-resolved, so Stage 27 survives; Stage 14's own internals do not | 7th |

---

## Recommended order, and why

1. **Unify the data root** (#1). Everything else is easier once one accessor
   answers "where is the data". Do it stage by stage, re-running each stage and
   byte-comparing its outputs, because several of these stages are validated and
   must not change a single number.
2. **Move the publication tree** (#3). Zero-risk, already fully isolated, and it
   demonstrates the pattern.
3. **Move the two assemblers** (#4). Cheap, and it makes the boundary visible.
4. **Move the canonical pipeline outputs** (#2), with legacy fallbacks retained
   for one release cycle.
5. **`Functions/` → `R/`** (#5), which is a single-commit mechanical change once
   the sentinel is parameterised.
6. **Test directories** (#6).
7. **The remaining unmigrated stage trees** (#7) — last, because it has the most
   unknown consumers.

Steps 1–3 are independently useful and individually revertible. Step 4 onward
should not start until step 1 is complete for every stage.

---

## What is already isolated, concretely

Stage 27 reads eleven canonical inputs and writes nine output subdirectories,
and contains **zero** path literals. Its test asserts this: no user-home path,
no UNC path, no `S:/` literal, no mention of `analysis_ready`, and no direct
call to `behavior_stage_dir()`.

So relocating either tree requires editing:

- `MMM_PROJECT_ROOT_DEFAULT` and/or the `dir = function(root) …` closures in
  `.mmm_path_specs()` — for inputs;
- `MMM_PUBLICATION_SUBDIRS` or the configured publication root — for outputs.

That is one file. The 26 other stages do not yet have this property.

---

## Cleanups this pass surfaced but did not perform

These are producer-side, not figure-side, and are recorded so they are not lost:

1. **Stale Stage 20 artifacts.** Ten `first_night_*.csv` files from a superseded
   run sit beside the current `first_active_*.csv` files in
   `pipeline/20_first_night_gamm/10min/tables/`, plus
   `audit/first_night_ar1_sequence_proof.csv`. Stage 20 does not clean them, so a
   reader picking the wrong prefix would quote a dead run.
2. **Misleading column names.** `early_observation_hours` and
   `early_observed_bins` in the Stage 09 and Stage 16 animal tables describe the
   whole ≈48 h CC1 Active epoch, not the 12 h window. They invite exactly the
   wrong `n`. Renaming them, or adding true window-coverage columns, would
   remove a live foot-gun.
3. **Missing producer contract.** Stage 09 does not export the 1000 permutation
   null statistics, only their median and 2.5/97.5% quantiles. Panel E therefore
   cannot draw a null density. If that display is wanted, Stage 09 must write
   the draws.
4. **Unmet declared robustness requirement.**
   `prediction_interpretation_constraints.csv` requires main-text prediction
   claims to have consistent full-data *and* excluding-short-duration
   performance, but `duration_analysis_set` is hard-coded to `"full"` for all
   five canonical rows.
5. **Domain-name vocabulary drift.** The two domain tables disagree on spacing
   around the slash (`Active-phase adaptation/exploration` versus
   `… / exploration`), so any join across them fails silently. Stage 27
   normalises on read; the producers should agree instead.
6. **Mislabelled resolution.** The broad domain table's `resolution` column reads
   `10min_based` for all 84 rows but describes only the HMM contributor; six of
   seven domains come from the 5-min backbone.
7. **CombZ is not reproducible from the repository.** The composite rule was
   recovered and verified, but the component z-scoring reference and the RES/SUS
   threshold live in a hand-maintained workbook. See
   `docs/BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.md` for the two options.

---

## Explicitly not in scope

No existing `Analysis/` script is moved or renamed, no stage is registered in
`run_all_analysis.R`, `analysis_ready/` is not reorganised, `Functions/` is not
migrated, the release architecture is untouched, and no historical path is
broken. This document is planning only.
