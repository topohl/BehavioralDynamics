# Manuscript Figure 1 reconstruction — progress

## Run identity

- **Starting HEAD:** `4b0f90f` (Add Stage 27 behavior figure candidate assembly)
- **Branch:** `main`
- **Worktree at start:** clean
- **Date:** 2026-09-15
- **Purpose:** reconstruct the authoritative Figure 1 behavioural analysis and
  export a frozen evidence bundle that the proteomics manuscript repository can
  consume without copying any behavioural code.

## Scope

**May write:** `results/manuscript_bridge/figure1/**`,
`results/reports/manuscript_bridge/**`, this progress file.

**May NOT write:** any analysis script, any `analysis_ready/` output, any
existing figure, any registry. No new exploratory modelling. No re-specification
of an existing model. Reruns only to verify current output, never to change it.

## Headline finding, established early

**The proteomics repository was right that it could not reconstruct Figure 1, and
wrong to imply the analysis does not exist.** It exists here, and it is more
rigorous than the historical narrative suggested. Two things in particular differ
from what the proteomics-side notes assumed:

1. **A valid out-of-sample prediction analysis does exist** — leave-one-animal-out
   plus repeated grouped 5-fold cross-validation with full-refit outcome
   permutation.
2. **A formal sex-by-feature interaction test exists and is negative**
   (all three q = 0.895). The historical "stronger in females" reading is
   **not** supported by the current canonical analysis.

## Authoritative sources consulted

| Source | Established |
|---|---|
| `docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv` | 15 analyses with manuscript role, status, effect sizes and caveats |
| `docs/COMBZ_CANONICAL_DEFINITION.md` | CombZ formula, z-score reference, RES/SUS rule, temporal ordering |
| `Analysis/build_later_outcome_combz.R` | canonical CombZ producer |
| `Analysis/09_early_prediction_model_ladder.R` | the canonical association + prediction stage |
| `docs/BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.csv` | Figure 1 panel provenance |
| `docs/KNOWN_LIMITATIONS.md` | standing caveats |

## Findings ledger

Findings are numbered BH-001 onward and are never renumbered. Full table:
`results/manuscript_bridge/figure1/figure1_findings_ledger.csv`.

| ID | Severity | Finding | Status |
|---|---|---|---|
| BH-001 | MATERIAL | The historical "stronger in females" reading is **not supported**. Formal feature-by-sex interactions all *q* = 0.895; stratified ρ is −0.41 female vs −0.42 male for the headline feature — near identical. | RESOLVED |
| BH-002 | MATERIAL | Cage identity is absent from the Stage 09 design (`cage_col <- NULL`), so cage-level dependence is neither modelled nor assessable. A generalisation concern, not outcome leakage. | DISCLOSED |
| BH-003 | MATERIAL | CombZ component z-scores are not reproducible under one uniform rule (positional per-sex workbook blocks). Downstream reproducibility *is* exact. | DISCLOSED |
| BH-004 | MINOR | `Entropy_acf1` is not FDR-supported at the primary resolution and adds nothing beyond mean movement (partial *r* = −0.06). | RESOLVED |
| BH-005 | MINOR | The three-feature model does not improve on movement alone, and no model-comparison test was performed. | RESOLVED |
| BH-006 | MATERIAL | Behaviour–proteomics integration reaches no FDR < 0.05 and supports no main-text claim in **either** repository. | DISCLOSED |

## Verdicts

- **PREDICTS_ALLOWED = YES**, for the fixed a priori behaviour-only models in
  `Analysis/09_early_prediction_model_ladder.R`. All six §19 conditions are met:
  true temporal ordering, genuine held-out animals, no outcome leakage, no
  preprocessing to leak (none is applied at all), performance computed from
  held-out predictions, and current canonical code and outputs exist.
- **Sex:** `FORMAL_INTERACTION_NOT_SUPPORTED`. "Female-specific", "sex-specific"
  and "stronger in females" are all prohibited.
- **HMM:** not promoted. The registry marks one HMM analysis
  `SUPPLEMENTARY_CONDITIONAL`, one `NOT_PROMOTED` and two `EXCLUDED`.
- **GAMM:** plays **no role** in the canonical Figure 1 primary analysis. The
  Stage 09 features are raw window summaries. This matters because the AUC
  visible from the proteomics side is a *GAMM trajectory* AUC — a different
  quantity from the cross-validated *R*² reported here.

## Decisions

- **DEC-B01:** The registry (`MANUSCRIPT_ANALYSIS_REGISTRY.csv`) is treated as
  the authoritative statement of manuscript role and status, because it carries
  per-analysis effect sizes, publication-readiness and explicit caveats. Claims
  are still checked against the artefact files it names.
- **DEC-B02:** CombZ components are carried through verbatim. The documentation
  establishes that the upstream per-sex positional formula blocks cannot be
  re-derived under one uniform rule, and re-deriving them would change the
  primary endpoint. This is recorded as a limitation, not repaired.
- **DEC-B03:** No behavioural code is copied into the proteomics repository. The
  bridge is a frozen export bundle with commit and file hashes.

## Outputs

All under `results/manuscript_bridge/figure1/` unless noted. 22 of 24 checkpoint
sections COMPLETE; 2 DEFERRED (GAMM reconstruction and HMM role — neither is
required for the licensed story, and the registry already excludes the HMM
analyses).

Contracts: entrypoint inventory · timeline · outcome score · RES/SUS
classification · circularity audit · early feature · association · leakage audit
· LOAO · model ladder · sex · findings ledger · red-team review.

Drafts: `results/reports/manuscript_bridge/figure1_results_draft.md` (≈760
words) and `figure1_methods_draft.md`.

Export bundle for the proteomics repository, `export/`: claim contract (10
claims), methods contract (12 rows), timeline, source-data manifest (8 files,
hashed) and repo provenance (commit, branch, worktree state).

## Resume point

**LAST COMPLETED:** the whole pass. Figure 1 provenance reconstructed, CombZ and
RES/SUS definitions recovered exactly, predictor defined, prediction adjudicated,
sex adjudicated, drafts written, export bundle frozen.

**NEXT EXACT ACTION:** in the *proteomics* repository, import
`results/manuscript_bridge/figure1/export/` and complete the Results §1 and the
behavioural half of Methods from it. Do not copy behavioural code — cite the
export bundle and its commit.

**DO NOT REPEAT:** the registry survey, the CombZ audit, or the Stage 09 number
verification (all numbers are in the contract tables with source hashes).

**CARRY INTO THE MANUSCRIPT:** BH-001 (no sex difference — this contradicts the
historical narrative), BH-002 (cage structure not modelled), BH-003 (do not claim
CombZ is regenerated from raw), BH-006 (no behaviour–proteomics main-text claim).

## Verification state

- **HEAD at export:** `4b0f90f` (the analysis code state the numbers come from)
- **HEAD after bundle commit:** `53bc7e9`
- **Branch:** `main` · **worktree_clean:** TRUE · **index_clean:** TRUE ·
  **index_differs_from_head:** FALSE · **tested_state:** HEAD
- **Tests:** 30 of 30 verification scripts pass, run the intended way
  (`Rscript Testing/tests/<script>.R` from the repository root).
  `test_combz_canonical_definition.R` independently confirms the thresholds
  (male −0.436641698, female −0.222390844), 12 controls per sex, population SD,
  0 of 117 label mismatches, and direct workbook parity at
  max |repo − workbook| = 4.44e-16.

Note: `testthat::test_dir()` is the wrong harness for this suite — the scripts
resolve paths from the repository root and error under `test_dir`. That is an
invocation artefact, not a defect.
