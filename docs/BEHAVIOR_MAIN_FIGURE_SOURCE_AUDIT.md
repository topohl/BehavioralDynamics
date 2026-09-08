# Behavior main figure — five-source provenance audit

Companion prose for `docs/BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.csv`. The CSV is the
machine-readable authority; this file explains what was found and what still
needs a human decision.

The figure being audited tells one story:

> later behavioural outcome definition → broad RFID characterisation → a specific
> early first-active-phase locomotor signal → its continuous relationship with
> later CombZ → out-of-sample generalisation.

Nothing below was chosen because it had a small p-value. Two panels are
deliberately drawn so that they *cannot* be read as a categorical group finding,
because the repository's own declared canonical contrasts for those questions are
null.

---

## Verdict per panel

| Panel | Status | One-line reason |
|---|---|---|
| **A** CombZ definition | `NEEDS_SCIENTIFIC_DECISION` | The CombZ *value* source is unambiguous, but the component z-scoring reference and the RES/SUS threshold are defined in a hand-maintained Excel workbook, not in this repository. |
| **B** RFID domain map | `READY_AFTER_FORMATTING` (chosen) / `NEEDS_SCIENTIFIC_DECISION` (alternative) | Two live domain tables exist. The registry-canonical one is first-night and Active-only; the only genuinely longitudinal, phase-resolved one is in no registry and carries forbidden cells. |
| **C** First-night Movement | `AMBIGUOUS_SOURCE` | The window and the metric are unambiguous and parity-tested. The *categorical* contrast has three implementations that disagree in verdict. |
| **D** Movement vs CombZ | `READY_AFTER_FORMATTING` | Canonical model is a pooled Spearman rank correlation with a bootstrap CI — not least squares, and not sex-facetted. |
| **E** Out-of-sample prediction | `CANONICAL_READY` | Endpoint, feature set, CV design, metric and permutation reference are all frozen and exported. Four wording constraints apply. |

---

## Panel A — the outcome definition is not reproducible from this repository

**What is unambiguous.** All three in-repo consumers
(`Analysis/09_early_prediction_model_ladder.R:61-63`,
`Analysis/03_primary_raw_movement_phase_stats.R:39-40`,
`Analysis/14_systems_neuroscience_summary_dashboard.R:120`) read the *same* file,
the *same* sheet and the *same* column:
`SIS_Analysis/E9_Behavior_Data.xlsx`, sheet `zScore`, column `CombZ`. There is no
competing in-repo implementation.

**What was recovered and verified.** CombZ is the unweighted arithmetic mean of
six component z-scores — `NOR`, `sucrose_pref`, `weight_dev`, `delta_cort`,
`adrenal_weight`, `spleen_weight`. This was confirmed two independent ways: the
workbook formula is `zScore!K = AVERAGE(E:J)`, and recomputing the row mean over
all 117 workbook rows reproduces the shipped `CombZ` to `2.2e-16`.

Note the composite is **behavioural *and* physiological** — two of the six
components are organ weights and one is a corticosterone change. Calling Panel A
a "behavioural battery" would be wrong.

**Direction is not harmonised to "higher = worse".** Higher CombZ means *more
resilient-like*; lower means *worse depressive-like*
(`Analysis/14:126,128`). Group means: CON `+0.027`, RES `+0.379`, SUS `-0.713`.
Consequently the negative Movement–CombZ correlation means **more early movement
→ worse later outcome**. Every axis label and legend sentence in this figure
states the direction explicitly, because getting it backwards inverts the
biological claim.

**What is *not* in this repository — the decision required.**

1. *The component z-scoring reference.* Each component is z-scored against **12
   male CON animals** (`(x - AVERAGE(<12 male CON cells>)) / STDEVPA(<same>)`).
   That is why the component means are not 0 and the SDs are not 1 in the
   analysed set. This is an editorial choice made in the workbook.
2. *The RES/SUS threshold.* `SUS` if and only if
   `CombZ < mean(CON within sex) - populationSD(CON within sex)`.
   The workbook's own limits are **male `-0.43664`**, **female `-0.22239`**; both
   were reproduced here independently from the shipped values to five decimal
   places, and both fall inside the empirically bracketed cut window. So the rule
   *is* now known — but it is applied to the RFID side by importing two ID lists
   (`sus_animals.csv`, 39 IDs; `con_animals.csv`, 24 IDs) in
   `Analysis/01_build_multiscale_behavior_metrics.R`, not by evaluating the rule
   in code. Re-deriving it would silently change group membership if the CombZ
   variant or the exclusion set ever changed.
3. *Two competing composites live in the same workbook and are read by no repo
   script.* Sheet `CombZScore_noBatch` column J is a **five-domain** mean that
   additionally includes social preference, EPM and OFT, and gives materially
   different values (animal `0001`: `-0.157` versus the canonical `-0.343`). Sheet
   `combZScore` is a within-batch-referenced variant. Neither is wrong; they are
   simply not what the analysis used.

**Consequence for the figure.** Panel A draws only the verified arithmetic and
labels the two external steps as external, in-panel. It does not invent a test
list, and it does not imply RES/SUS were known when the RFID measurement was
taken — the box is labelled *"later outcome group"*.

**Recommendation.** Either (a) freeze the workbook, hash it, and cite it as a
data dependency in Methods, or (b) port the six-component z-score and the
one-SD-below-CON rule into a small producer stage so the outcome definition
becomes reproducible. Option (b) is the only one that makes the manuscript's
outcome definition auditable from the repository, and it would need a parity test
against the shipped 111 labels.

---

## Panel B — two live domain tables, and neither may be picked silently

**Candidate B1 — chosen for the main panel.**
`…/12_systems_neuroscience_summary/5min_based/first_night/10min_based/first_night_group_contrasts.csv`
30 rows = 5 domains × 2 sexes × 3 contrasts. Carries `hedges_g`, `estimate`, `SE`,
`ci_low`, `ci_high`, `q`, and a declared BH family of n = 15 per sex. Model
`DomainScore ~ Group * Sex` (`stats::lm` + `emmeans`), strict-completeness
scoring, **no HMM row by contract**.

Chosen because it is the table named in `docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv`
(`FIRSTNIGHT_5DOMAIN_PANEL`) and staged `required = TRUE` by
`Analysis/build_publication_release.R:168`.

**Its honest limitation, which changes the panel's caption.** It is
**first-night and Active-phase only** — one 12 h window, not a longitudinal
record. The proposed story arc says "broad *longitudinal* characterisation"; this
table cannot support the word *longitudinal*. The panel is therefore labelled
"first night". Additionally, only 50 of 111 animals have a complete window, and
**1 of 30 cells** is FDR-supported (Female RES-CON, behavioural
volatility/fragmentation, `g = -1.009`, `q = 0.0201`).
`docs/MANUSCRIPT_PANEL_PROPOSAL.md` explicitly recommends *against* main-figure
placement on exactly that ground — "it would invite it to be read as a
multi-domain signature".

**Candidate B2 — built as an extended-data candidate only.**
`…/5min_based/stats_tables/systems_sis_domain_effect_summary.csv`
84 rows = 7 domains × Active/Inactive × 2 sexes × 3 contrasts, from
`lmerTest::lmer(DomainScore ~ Group * Sex + factor(CageChangeIndex) + (1|AnimalNum))`.
This is the only artifact that can fill a broad, phase-resolved,
one-row-per-domain heatmap — i.e. it is what the proposed story actually asks
for. It is not adopted for the main figure because:

- it appears in **no** registry row and **no** release-plan entry;
- it still displays the HMM-derived `Behavioral state architecture` domain, which
  the first-night contract removed entirely, and whose first-night sibling is the
  repository's single clearest instability (shipped-vs-refit dwell ρ = 0.015);
- **all 8** of its FDR-supported cells are in the **Inactive** phase, which
  `docs/KNOWN_LIMITATIONS.md` item 3 forbids interpreting as biology (inactive
  read density is not separable from genuine rest);
- its domain scores use `na.rm = TRUE` plus `coalesce(missing, 0)`, so an animal
  missing a contributor is scored on a *different formula* from its peers — a
  practice `Functions/first_night_domain_helpers.R:21-24` explicitly rejects;
- it ships no confidence intervals;
- its `resolution` column reads `10min_based` for all 84 rows, but that describes
  only the HMM contributor — 6 of 7 domains come from the 5-min backbone.

**Decision required.** Keep B1 in the main figure with a "first night" label and
the single-supported-cell caveat, or promote B2 and pay for it with a domain
curation pass (drop the HMM row, drop or heavily caveat the Inactive cells) plus
a port of the strict-completeness scoring contract. This is editorial, not a code
question. Both are built; neither is silently chosen.

Excluded from every version of this panel, on current audit status:
first-night HMM persistence (`EXCLUDED` — not reproducible between fits),
occupancy entropy (`EXCLUDED` — sign flips across optima, audit verdict is
literally "do not report as a finding"), and inactive/rest HMM interpretation
(`NOT_PROMOTED` — statistically robust and excluded anyway, on measurement
validity). None was resurrected because it appeared in an older heatmap.

---

## Panel C — the window is certain, the categorical claim is not

**Unambiguous.** The window is the first Active phase block after the first cage
change, fixed clock **18:30 inclusive to 06:30 exclusive**, exactly 12 h, all
UTC, at 10-min primary resolution (72 expected slots).
`Functions/first_night_window_helpers.R` is a byte-parity-tested
re-implementation and `Testing/tests/test_first_night_window_parity.R` asserts
row-for-row, timestamp, slot and per-animal-coverage identity with Stage 09 at
both resolutions. `Movement_mean` agrees to full double precision across all
three producers that compute it.

`Movement_mean` is the animal-level mean of per-bin Movement, where per-bin
Movement is the **count of RFID position transitions** in that 10-min bin — so
the unit is *mean transitions per 10-min bin*, not a total.

**Ambiguous, and consequentially so.** Three implementations answer "does
first-night Movement differ by later group?" and they disagree:

| Implementation | Design | Verdict |
|---|---|---|
| Stage 09 | `wilcox.test` on animal means, **pooled across sex**, BH within feature | SUS-RES `p = 0.0075`, `q = 0.0225` — the *only* nominally FDR-supported result, and the file name plus its `ReportingRole` column both say **descriptive** |
| Stage 20 (**declared canonical**) | bin-level GAMM within sex, `log1p(Movement)`, Batch-adjusted, animal random effect, AR1; Wald on window-average and integrated AUC; family of 6, BH | **all six null**, min `q = 0.796` |
| Stage 14 | `lm` on within-sex z-scored Movement + `emmeans`, BH over 15 | Female SUS-RES `q = 0.0856` — null |

The pattern is consistent with a pooled-sex effect that dissolves once sex is
stratified or modelled — but **no file in the repository states that**, and
`Analysis/26` records the declared canonical status as
`CLAIM_GAMM_02 = "SUPPORTED - all six contrasts null"`.

**Consequence for the figure.** Panel C shows the animal-level distribution with
a descriptive median and interquartile range, and **annotates no categorical
effect**. Its stated role is to show the biological distribution underlying the
early signal, which is what the brief asked for.

**Two traps recorded so they are not repeated.**
`early_observation_hours` and `early_observed_bins` in the Stage 09 and Stage 16
animal tables describe the whole ≈48 h CC1 Active epoch, **not** the 12 h window;
they must never be quoted as window coverage. The correct per-animal window count
is `n_bins` in `early_behavior_features_long.csv` or `observed_target_slots` in
`early_window_design_by_animal.csv`. Separately, ten stale `first_night_*.csv`
files from a superseded Stage 20 run still sit beside the current
`first_active_*.csv` files; a reader picking the wrong prefix would quote a dead
run.

---

## Panel D — the canonical model is rank-based and pooled

`primary_movement_entropyacf1_associations.csv`, from
`Analysis/09_early_prediction_model_ladder.R:1085-1105`:
**Spearman rank correlation with a 5000-iteration animal-resampling bootstrap
CI**, pooled across sex, BH over the three-feature a priori family.

Headline: `Movement_mean` vs `CombZ`, **ρ = −0.3903**,
95% CI `[−0.5474, −0.2092]`, `p = 2.29e-05`, **BH q = 6.88e-05**, n = 111.

Two consequences the panel respects:

- **No least-squares line.** The panel shows the raw scatter plus a *descriptive
  quantile-bin trend* (median y within five equal-count bins of x), labelled as
  such, and a rank-space companion whose single reference line has slope exactly
  equal to the canonical ρ — drawn analytically from that ρ, not fitted.
- **Pooled, not sex-facetted.** The formal feature-by-Sex interaction is null for
  all three features (all `q = 0.895`), and the repository states that
  sex-stratified estimates are descriptive and are **not** interaction tests. Sex
  is carried redundantly by point shape. A sex-facetted version is exported as a
  clearly-labelled descriptive candidate only. This deviates from the brief's
  "prefer Female/Male facets", deliberately, because the brief also required the
  visual and inferential models to match.

The other two features of the same declared family travel with it in the key
results table: `Movement_rmssd` is FDR-supported but **must not** be described as
an independent predictor, and `Entropy_acf1` is **not** FDR-supported at the
primary resolution (`q = 0.0667`) and must be reported with qualified wording.

---

## Panel E — canonical, with four wording constraints

Endpoint is **continuous CombZ**. There is no classification model anywhere in
Stage 09 — no logistic fit, no ROC, no AUC. `Group` is excluded from every
primary model.

- **Design.** Exhaustive **leave-one-animal-out** ordinary least squares
  (`loo_lm_predict`, `:383-418`): each animal in turn is removed, the model is
  refitted on the remaining 110, and predicts the held-out animal, with
  training-fold-only median imputation.
- **Models.** Headline `Movement_mean` alone (LOAO R² = **0.15939**); supporting
  three-feature family (0.15245); intercept-only baseline (−0.01826). Two
  sex-adjusted variants are declared sensitivities with no permutation test. The
  headline is fixed by a contract gate — `validation.csv` row
  `movement_mean_headline … PASS`.
- **Reference.** 1000 **full-refit outcome permutations**, seed `20260811`, which
  reshuffle CombZ and re-run the entire LOAO loop; `empirical_p = (1 + count)/1001
  = 1/1001` for both non-null models. Null median `−0.0313`, 2.5–97.5%
  quantiles `[−0.0442, 0.0059]`.
- **Companion.** 100 repeats of grouped 5-fold CV, seed `521`, mean R² 0.15582,
  split-to-split 2.5–97.5% quantiles `[0.1159, 0.1790]`.

**The four constraints.**

1. **Grouping unit is `AnimalNum` and nothing else.** Cage, social group and
   batch are not grouping units anywhere in Stage 09, and because the model frame
   is one row per animal, the "grouped" 5-fold is operationally plain
   animal-level 5-fold. The legend must not imply cage- or batch-level
   generalisation.
2. **`cv_r2_q025/q975` are not a bootstrap CI.** They are split-to-split
   resampling quantiles across repeated CV splits; the producing code says so
   explicitly. The only bootstrap CIs in Stage 09 are on the three Spearman
   feature correlations. The panel labels the interval by its exact
   `interval_type` string.
3. **The permutation null distribution is not exported** — only its median and
   2.5/97.5% quantiles. The null is therefore drawn as its published interval
   rather than as a density. *This is a genuine missing producer contract*,
   recorded here rather than worked around: if a null density is wanted in the
   figure, Stage 09 must be extended to write the 1000 null statistics. Stage 27
   does not recompute them.
4. **Internal validation only.** `docs/KNOWN_LIMITATIONS.md` §6 requires the
   words "internal validation, not external validation" and forbids causal
   language.

**One declared robustness requirement is unmet.**
`prediction_interpretation_constraints.csv` states that main-text claims require
consistent full-data *and* excluding-short-duration performance, but
`duration_analysis_set` is hard-coded to `"full"` for all five canonical rows
(`Analysis/09:1787`). The duration sensitivity exists only for the superseded
legacy ladder. This is a producer-side gap, not a figure problem.

**Filename hazard.** At least three same-schema look-alike files named
`primary_prediction_performance.csv` exist elsewhere on the share — an erroneous
24 h Stage 09 snapshot directory, and both the primary and supplementary slots of
the `rc1` release bundle (the latter holding the *5-min* numbers). Stage 27
resolves this input through the semantic path registry and never by filename
search; a test asserts the resolved path is not inside a snapshot, quarantine,
archive or release directory.

---

## What this audit does not do

It does not change any statistic, refit any model, move any existing output,
register a new stage in `run_all_analysis.R`, or touch the release. Where a
producer contract is missing (the permutation null distribution) or unmet (the
duration robustness requirement), it is reported here for the producing stage to
fix.
