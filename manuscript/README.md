# Manuscript

The E9 SIS behavioural manuscript is built in the Exp9_manuscript repository. It imports frozen, hash-gated bundles
that this repository wrote once each and renders every figure from them; it fits nothing. This file states what this
repository supplies. Until 2026-10-05 it also described an in-repository export layer and Figure 1 staging; both were
retired (`docs/LEGACY_AND_GAMM_RETIREMENT_2026-10-05.md`).

```text
manuscript/
├── README.md     this file
└── archive/      historical/forensic provenance, not current
```

The stage-by-stage layers are in `Analysis/README_pipeline.md` and `Analysis/STAGE_INVENTORY.csv`. The
machine-readable map of manuscript-facing analyses is `docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv`, kept byte-unchanged
because the frozen configurations cite it; read it with `docs/MANUSCRIPT_ANALYSIS_REGISTRY_ADDENDUM.md`.

---

## What the manuscript imports

Under `analysis_ready/canonical/` on the project root:

- `behavior_bundle/ebb_v101_20260929_b2ce507/`: the Stage 29 v1.0.1 release bundle (writer: Stage 16b). Figure 1 is
  rendered from it.
- `figure_support_bundle/` (writer: Stage 16c) and `stage30_figure_bundle/` (writer: Stage 30b).
- `later_outcome_combz/tables/`: the later outcome CombZ and the CON/RES/SUS classification
  (`Analysis/build_later_outcome_combz.R`, `docs/COMBZ_CANONICAL_DEFINITION.md`).

## The registered runs behind them

Each ran once, behind identity gates, into its own run folder:

- Stage 29 v1.0.1 (`docs/STAGE29_RELEASE_v1.0.1_dv2.md`), with Stage 09 as its prospective prediction input: the
  fixed a priori features `Movement_mean`, `Movement_rmssd` and `Entropy_acf1` in the first active 12 h after the first
  cage change; `Group` is endpoint-derived and excluded from every prediction model.
- Stage 29b, post hoc CON contrasts (`docs/POSTHOC_CON_CONTRASTS_REGISTRY_v1.0.md`).
- Stage 30, the exploratory screen (`docs/stage30/STAGE30_REGISTRY_v1.0.md`).
- Stage 32, exposure and adaptation (`docs/STAGE32_REGISTRY_v1.0.md` and addendum A1).

Descriptive layers that are not in a bundle: Stage 28 (four core domains; `docs/RFID_FOUR_DOMAIN_RECONCILIATION.md`),
the Stage 14 first-night five-domain panel as its conservative sensitivity, the CC4 Stages 31-31d
(`docs/CC4_GRID_EXPOSURE.md`) and the within-night GAMM profiles of Stages 20 and 22 (Extended Data candidates without
group inference; `docs/EXTENDED_DATA_CLASSIFICATION.md`).

### NOT CURRENTLY PROMOTED

None of the following may carry a manuscript claim as the repository stands:

- **Inactive HMM / rest interpretation** — pending measurement validity.
  Inactive-phase read density is not separable from genuine rest, so the
  inactive-phase contrasts cannot be attributed to biology rather than to
  detection. The phase-aware inactive-QC redesign is specified but **not
  implemented** (`chip_loss_qc_mode` remains `annotate_only`).
- **Occupancy-entropy phenotype** — unstable. Active `occupancy_entropy`
  SUS–RES flips sign across gap-aware optima and is explicitly recorded as
  `NOT robust: sign flips across optima; do not report as a finding`.
- **First-night HMM persistence** — instability at the first-night window.
  Active longitudinal HMM persistence is at most supplementary and conditional
  on the identifiability caveats in `docs/KNOWN_LIMITATIONS.md`.
- **Stage 10 / Stage 14 systems predictive claims, nonlinear and manifold
  analyses, and behaviour-proteomics integration** — exploratory.
- **GAMM group contrasts** (Stages 20 and 22, and the retired 21 and 23-26) —
  no cage term while CON is 3 intact cages per sex (`docs/KNOWN_LIMITATIONS.md`
  item 13).

---

## `archive/`

`archive/BehavioralDynamics_schema_preproduction_audit/` is the provenance
schema that documented the pipeline state **before** the phase-classifier,
Stage 11/12, Stage 14 and Stage 09 corrections. It is a genuine forensic
artifact — its numerical verification was real — but several defects it
describes as live have since been fixed, so it is not a description of current
production. Its README carries a prominent historical-status banner mapping each
finding to its status today. It is retained, not deleted; nothing in its `data/`
was altered, and its scripts still run from the archived location.

---

## What must not be inferred from this directory

- Panel selection is not a function of p-values.
- A rendered candidate is not a promoted result.
- Anything under `archive/` describes the past, not the present.
