# Known limitations

Current, real caveats on the BehavioralDynamics analyses as the repository
stands. Each entry states the limitation, the evidence for it, and what it
forbids being claimed.

Issues that were found and **fixed** are listed separately at the end. They are
not current limitations and must not be presented as such.

---

## 1. HMM local optima and identifiability

The hidden Markov models do not converge to a single solution. Across seeds at
the 5-min bin level the log-likelihood spans roughly **69,078 to 414,114** — not
small numerical differences but genuinely distinct optima. Several emission
standard deviations sit at the **0.05 floor**, indicating partially degenerate
components.

*Evidence:* `hmm_architecture_identifiability_probe.csv` (16 fits across seeds
and bin levels).

**Forbids:** treating any single HMM fit as *the* state model, or reporting a
latent-state result without cross-optimum robustness evidence.

**Mitigation in place:** the cross-optimum audit refits across five distinct
gap-aware optima and reports sign stability per contrast
(`hmm_cross_optimum_gapaware_claim_verdicts.csv`). Only contrasts that are
sign-stable across all optima may be reported, and even those carry this caveat.

---

## 2. First-night HMM persistence is not stable

Animal-level first-night dwell time from the shipped fit and from a seed-7 refit
correlate at **Spearman rho = 0.0154** — effectively zero — and differ roughly
twofold in mean (32.06 vs 16.51 minutes) over 109 animals. Group contrast
estimates and nominal p-values do not survive the refit.

*Evidence:* `first_night_dwell_shipped_vs_refit.csv`,
`first_night_dwell_partition_stability_contrasts.csv`.

**Forbids:** any claim that first-night HMM persistence is robust, or any
first-night latent-state finding at all.

A 12-hour window does not contain enough transitions to identify the state model
stably. This is **distinct from** the longitudinal active-phase persistence
family, which *is* cross-optimum stable — the two must not be conflated.

---

## 3. Inactive-phase read density is not separable from rest

During the inactive phase, low RFID read density is ambiguous between an animal
that is genuinely resting and an animal whose chip is failing. Inactive-phase
`observed_fraction` has median **0.149** versus **1.000** in the active phase,
and **95.7 %** of inactive epochs fall below 0.50. Inactive-phase domain scores
correlate with mean movement at **Spearman rho 0.64–0.87**.

Consequently `hard_dropout_signature` cannot distinguish chip loss from rest
during the inactive phase.

*Evidence:* `audit_inactive_phase_qc/inactive_qc_epoch_evidence.csv`,
`inactive_qc_threshold_calibration.csv`.

**Forbids:** interpreting inactive-phase latent states, rest architecture, or
quiescence domains as biology. The inactive HMM contrasts are statistically
robust across optima but are not attributable to behaviour rather than detection.

**Status:** a redesign exists and is specified — a relative read-density
classification against same-`Batch × System × CageChange × Phase` cage-mates,
with classes A (RFID-loss evidence, exclude), B (low density consistent with
shared quiet, retain as rest-like biology) and C (uncertain, retain with a
sensitivity flag), with the cut calibrated on known positives rather than
assumed. It is **SPECIFIED, NOT IMPLEMENTED**: `chip_loss_qc_mode` remains
`annotate_only`. Adopting it in production is an open decision.

---

## 4. RES/SUS labels are derived from the downstream outcome

`Group ∈ {CON, RES, SUS}` is not an independent experimental assignment. RES and
SUS are defined by thresholding the downstream composite `CombZ`.

**Forbids:**

- treating any CON/RES/SUS comparison as independent validation of the endpoint —
  it is phenotype characterization by construction;
- using `Group` as a predictor in a prospective model. The canonical Stage 09
  models exclude it, and Stage 16 asserts this
  (`group_excluded_primary_prediction` PASS).

Every group-contrast analysis in this repository — Stage 03, the first-night
panel, all HMM contrasts — inherits this limitation.

---

## 5. Social proximity is a co-location proxy

`Proximity` is computed as same-position dyadic seconds divided by dyadic
observation seconds from RFID position identities. It measures **co-location in
the same cage position**, not social interaction. The system reads position
identity, not behaviour, and cannot distinguish an affiliative interaction from
two animals independently occupying the same feeder.

**Forbids:** describing proximity, dyadic contacts or network measures as direct
measures of sociability, social preference or social interaction. The
corresponding domain is labelled "social spatial organization" for this reason
and that framing should be preserved.

---

## 6. Stage 09 is prospective prediction, not causal inference

Stage 09 establishes that behaviour measured in the first active 12 h **precedes
and statistically predicts** later `CombZ`. It does not establish that early
behaviour causes the later outcome. There is no manipulation of the predictor,
and unmeasured common causes (baseline trait activity, health, litter effects)
are not excluded.

Validation is **internal** — leave-one-animal-out and repeated grouped 5-fold CV
within one cohort of 111 animals, with a full-refit outcome permutation. There is
no external cohort.

**Forbids:** causal language, and any claim of external validation or
generalisation beyond this cohort and paradigm.

---

## 7. Resolution sensitivity for the primary model is unresolved

The canonical Stage 09 resolution is 10-min bins, with 5-min declared as a
resolution sensitivity. That sensitivity is **not currently available as a clean
canonical artifact**: Stage 16 records
`resolution_sensitivity_status = "Unavailable for this export"` and deliberately
does not promote the legacy 5-min output, which predates the current identity and
window contracts. Duration sensitivity for the primary model is likewise recorded
as unavailable rather than silently omitted.

**Forbids:** stating that the primary result is robust to bin resolution. It is
currently untested at the canonical standard.

This is an honest gap, explicitly marked in the validation table rather than
hidden. Closing it requires rerunning Stage 09 at 5 min under the current
contract.

---

## 8. First-night window completeness

Only **50 of 111** animals have a complete 72-slot first-night window at 10-min
resolution. The remainder have leading, interior or trailing missing slots. The
analysis is adjacency-aware (RMSSD and ACF1 are computed over adjacent observed
slots, not over naive row order) and completeness is reported per animal, but
incomplete windows still contribute.

**Forbids:** presenting the first-night panel as a complete-case analysis.

Related: the first-night panel yields **1 FDR-supported cell out of 30**
displayed. It should be reported as that single result, not as a multi-domain
signature.

---

## 9. Persistence metrics are not independent findings

`mean_dwell_minutes`, `self_transition_probability`, `state_switch_rate` and
`transition_entropy` are algebraically related — `state_switch_rate` is
essentially `1 − self_transition_probability`, and the audit records their exact
redundancy.

**Forbids:** counting them as four converging pieces of evidence. They are one
finding expressed four ways.

---

## 10. Stage 15 behaviour-proteomics is underpowered

All 40 curated behaviour–proteomics models have **n between 0 and 9** animals,
**none** reaches FDR < 0.05, and the script itself labels every one
`EvidenceUse = exploratory_effect_size_small_n` and
`StableForMainText = FALSE`.

**Forbids:** any main-text behaviour–proteomics claim.

---

## 11. Manuscript figure path resolution caveat

`build_fig1_candidates.R` resolves Stage 03 panels under
`figures/publication_panels/`, but the canonical migrated Stage 03 tree writes
figures directly to `figures/`. That panel therefore resolves through the
documented legacy fallback rather than the canonical path. It is recorded in
`staging_status.csv` rather than failing silently. Left unchanged during the
restructuring because altering artifact resolution is a behavioural change.

---

## Resolved historical issues

These were real defects. They have been **fixed**. They are listed so that older
audit documents — in particular
`manuscript/archive/BehavioralDynamics_schema_preproduction_audit/` — can be read
correctly, and they must **not** be presented as current limitations.

| Issue | Resolution |
|---|---|
| Permissive phase classification: `str_detect(phase, "active\|dark\|night")` is unanchored and `"inactive"` contains `"active"`, so every Inactive epoch was relabelled Active (79,920 of 191,445 rows at 10 min) in Stages 11 and 12 and the shared duration helper. | Replaced by exact membership in `Functions/phase_classification_helpers.R`. Stages 11, 12 and 14 regenerated. Locked by `Testing/tests/test_phase_classification.R`; impact quantified by `Testing/audits/audit_phase_bug_impact.R`. |
| Rest-domain collapse: with no Inactive epoch receiving quiescence features, `safe_scale` turned total absence into an exact `0`, so the "Inactive-phase rest/circadian regulation" row reduced to `(Movement_acf1_z − Movement_rmssd_z)/3` and carried no quiescence information. | Downstream consequence of the phase-classifier defect; resolved with it. The separate *measurement-validity* concern about inactive-phase read density remains open — see item 3. |
| HMM sequence segmentation ignored recording gaps, so sequences spanning a gap were treated as contiguous. | Stage 08 segmentation is gap-aware. Robustness re-established under the gap-aware contract. |
| Stage 09 primary window selected both phases rather than Active only, and endpoint identity matching dropped animals (111 → 88) because only one side of the CombZ join was canonicalised. | Fixed; `Testing/tests/test_stage09_primary_window.R` and `test_stage09_endpoint_identity.R` lock both. |
| Stage 14 used row-count windows rather than a clock window, so it did not match the Stage 09 question. | Stage 14 now uses the canonical first-night analysis on the shared clock-window selector; parity asserted by `Testing/tests/test_first_night_window_parity.R`. |
| Stale Stage 09 output trees (113 animals, zero-padded IDs, both phases) were resolvable by readers. | Quarantined to `analysis_ready/_quarantine_legacy_s09/` with a manifest recording every contract violation. |
| Stage 16 reporting contract described `Entropy_acf1` inconsistently with its actual FDR status. | Corrected; the qualified wording is now enforced by the `entropy_wording` validation check. |
| Stage 15 output paths exceeded the Windows `MAX_PATH` limit. | Fixed; `Testing/tests/test_output_path_length.R` guards it. |
