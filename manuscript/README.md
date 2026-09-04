# Manuscript

Publication-facing assembly for the E9 SIS behavioural manuscript. This file is
the **authoritative statement of the current publication architecture**. Where
any other document in this repository disagrees with it, this file wins — with
the single exception of the machine-readable registry it points to, which wins
over prose.

```text
manuscript/
├── README.md                    this file — current publication architecture
├── Fig1_behavior_candidates/    candidate panel staging for Figure 1
└── archive/                     historical/forensic provenance, not current
```

The authoritative machine-readable map of every manuscript-facing analysis is:

```text
docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv
```

---

## Current publication architecture

### PRIMARY — Stage 09 prospective prediction

`Analysis/09_early_prediction_model_ladder.R`

Whether behaviour during the **first active 12 h after the first cage change**
predicts **later CombZ**. Canonical resolution 10-min bins, n = 111 animals.

The canonical feature set is fixed a priori and is exactly three features in
order: `Movement_mean`, `Movement_rmssd`, `Entropy_acf1`. The headline model is
`Movement_mean` alone. `Group` is endpoint-derived and is excluded from every
canonical primary model.

This is the only layer that carries a primary manuscript claim.

### SECONDARY — phenotype characterization

- **Stage 03** raw longitudinal movement
  (`Analysis/03_primary_raw_movement_phase_stats.R`) — cage-change × phase
  characterization. Displayed CON/RES/SUS pairwise comparisons are Holm-adjusted
  within each prespecified three-contrast panel; no wider global family is
  claimed.
- **First-night five-domain panel** (Stage 14, via
  `Functions/first_night_domain_driver.R`) — the canonical first-night analysis
  on the same clock window Stage 09 owns, primary at 10 min with 5 min as a
  declared resolution sensitivity. Five displayed domains, BH-corrected within
  Sex over a declared 5 × 3 family.

### SUPPLEMENTARY / CONDITIONAL

- **Active longitudinal HMM persistence.** The active-phase persistence family
  (mean dwell, self-transition probability, state-switch rate, transition
  entropy) is sign-stable across all five distinct gap-aware optima for
  SUS–CON. Conditional on the identifiability caveats in
  `docs/KNOWN_LIMITATIONS.md` being stated alongside it.

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
- **First-night HMM persistence** — instability at the first-night window;
  distinct from the longitudinal active persistence above.
- **Stage 10 / Stage 14 systems predictive claims, nonlinear and manifold
  analyses, and behaviour-proteomics integration** — exploratory.

### Stage 16 — canonical manuscript behaviour / source-data export layer

`Analysis/16_manuscript_behavior_report.R`

The canonical manuscript export layer. It **reads** existing Stage 09, Stage 03
and QC tables and writes the manuscript package to
`analysis_ready/manuscript/behavior/`. It fits no models and recalculates no
statistics; it is assembly only, and it verifies that the upstream SHA-256
hashes are unchanged across its own run.

Entry point: `Behavioral_Source_Data.xlsx`, with typed CSV companions
(`primary_results.csv`, `supplementary_results.csv`, three source-data tables,
`provenance.csv`, `validation.csv`, `manifest.csv`).

Stage 16 is **not** part of `Analysis/run_all_analysis.R`; it is run explicitly
after the canonical upstream outputs exist.

---

## `Fig1_behavior_candidates/`

Candidate panel staging for Figure 1 — deliberately a candidate set, not a
frozen figure. `figure_manifest.csv` declares each candidate's status;
`build_fig1_candidates.R` is the source of truth for the status vocabulary and
records what actually resolved into `rendered/core/staging_status.csv`.

**The final panel selection is not made by this repository and must not be
inferred from significance.** It is an explicit editorial decision recorded in
`docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv`.

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
- The presence of a rendered candidate does not make it a promoted result.
- Anything under `archive/` describes the past, not the present.
