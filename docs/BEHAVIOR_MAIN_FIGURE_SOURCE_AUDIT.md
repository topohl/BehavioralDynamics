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
  sex-stratified estimates are descriptive and are **not** interaction tests. The
  later outcome group is carried redundantly by point shape and fill, so the panel
  survives greyscale; sex is not encoded in the pooled panel. A sex-facetted
  version is exported as a
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

---

# Addendum — pre-freeze canonicalization pass

Baseline for this pass: `967fcb11804fa3604a24678e1fc613207d9f0dd1`
("Add Markov inactive GAMMs and manuscript builders"), working tree clean.

Everything above is retained as the record of the earlier audit. This addendum
states what changed, and **corrects one thing the earlier audit got wrong**.

## Correction: the CombZ z-score reference is within-sex, not male-only

The earlier audit reported the component z-score reference as "12 male CON
animals" globally. That was **wrong** — it read only the first formula block.

Reading the workbook XML block by block shows four positional formula blocks per
column. In `organWeight` column `O`, rows 2–41 and 81–99 use the male reference
`I$12,I$13,I$15,I$17,I$30:I$33,I$82:I$85`, while rows 42–80 use the female
reference `I$42:I$45,I$65:I$68,I$100:I$103`. The two reference sets are:

| reference | animals | batches |
|---|---|---|
| male | `OQ760/761/763/765`, `OR126/127/128/129`, `OR621/622/623/624` | 1, 2, 5 |
| female | `413/414/415/416`, `OR537/538/539/540`, `OR639/643/644/645` | 3, 4, 6 |

So the standardization is **within-sex control-referenced**. The earlier
"12 male CON" statement happened to be right for male animals only.

## Panel A — now canonical in-repository

`Analysis/build_later_outcome_combz.R` is a new upstream producer (non-numbered,
following the `build_publication_release.R` convention; deliberately **not**
registered in `run_all_analysis.R`). It reproduces the workbook endpoint and
hard-stops otherwise:

| quantity | result | tolerance |
|---|---|---|
| CombZ vs workbook | **2.220446e-16** | 1e-12 |
| outcome-group labels | **0 of 93 mismatched** | 0 |
| Male threshold | **−0.436641698** | — |
| Female threshold | **−0.222390844** | — |

Population SD is **required**: with sample SD, `OR434`, `OR554`, `OR625` and
`13856` misclassify.

**What is still external, and why it stays that way.** The six component
z-scores are carried through verbatim. The upstream reference is written as
hard-coded absolute row positions, and it is not internally consistent:

- row ranges and sex only approximately coincide — in `organWeight` rows 81–118
  are **mixed sex** (19 F, 19 M) yet use the **male** reference;
- sheets have different row layouts (`bodyWeight` has 250 rows and uses
  `12,13,15,17,31:34,87:90`; the 117-row sheets use `12,13,15,17,30:33,82:85`),
  though both resolve to the same 12 animals;
- `sucrosePreference` labels four reference animals (`OQ760/761/763/765`) as
  `SIS` while every other sheet labels them `CON`.

Recomputing from raw with a single uniform within-sex rule does **not** reproduce
the shipped values (max deviation ≈ 2.0 even block-aware). Since this pass must
not "improve" the primary endpoint, the components are preserved exactly and the
dependency is recorded in `combz_component_definition.csv`
(`reproducible_in_repository = FALSE`).

## Panel B — decision: `B3_NOT_JUSTIFIED__USE_FRAMEWORK_SCHEMATIC`

Adversarially verified. No scientifically clean longitudinal multi-domain
overview can be built from currently validated material:

1. The canonical `analysis_ready/pipeline/` tree contains **no domain-level
   results table at all**.
2. `analysis_ready/output_index.csv` records the Stage 14 tree as
   `canonical_path = NA`, `status = "legacy_pending_migration"`,
   `manuscript_role = "exploratory systems layer"`.
3. The registry references only its `first_night/10min_based/` subtree, and the
   release plan stages **no** `sis_domain` artifact among its 45.
4. The strict-completeness defect **provably bites**: `OQ755`/CC2, `OQ770`/CC1
   and `OQ771`/CC1 carry finite domain scores computed from 2-of-3, 4-of-5 and
   2-of-3 contributors — three named animals scored on a different formula from
   their peers, which `Functions/first_night_domain_helpers.R:21-24` forbids.
5. Every HMM/latent-state domain and every inactive-rest/circadian reading is
   barred by `docs/KNOWN_LIMITATIONS.md` items 1–3.

Panel B is therefore a **descriptive measurement-framework panel** built from
the one registry-cleared longitudinal construct — raw movement across CC1–CC4 ×
light phase (`S03_RAW_LONGITUDINAL_MOVEMENT`, `publication_ready = "yes as
secondary characterization"`) — plus the inventory of validated behavioural
domains. It carries **no** inferential claim.

The choice is **explicit configuration** (`PANEL_B_SOURCE`, default
`longitudinal_framework`), never file availability. `first_night_domains` (B1)
is selectable by configuration; `broad_domain_map` (B2) is refused as a main
panel and exists only as an Extended Data candidate.

The two phases are drawn on **independent y axes** because inactive-phase
movement is an order of magnitude lower; the legend states they must not be
compared by eye.

## Panel E — the permutation null is now real

Stage 09 previously discarded the 1000 per-permutation statistics it computed,
which forced the figure to reconstruct a null from published quantiles. That is
fixed at the producer, **persistence only**:

- new output `early_prediction_permutation_draws.csv`, 2002 rows
  = 2 models × (1000 null + 1 observed);
- the permutation count, seed `20260811`, fold structure, model, metric and
  every published summary are untouched;
- the list-column is stripped before `primary_prediction_permutation_test.csv`
  is written, so that file keeps its original schema;
- the persisted draws reproduce all five published summaries to **1.7e-18**.

Stage 09 pre/post parity: every headline value identical to 10 significant
digits (CV R² `0.1593945586`, permutation p `0.000999000999`, null median
`−0.03131646461`, null quantiles `[−0.04420903341, 0.005863676405]`, baseline
`−0.01826446281`), held-out predictions drift **0**, and
`model_ladder_input.csv`, `early_behavior_features_wide.csv` and both
duration-sensitivity tables are **byte-identical**.

Two non-scientific tables did change, and neither is attributable to the edit:
`early_window_rows_used*.csv` gained a metadata column `AnimalID_raw`, and
`epoch_duration_qc.csv` shifted in `active_duration_hours`. Both come from
shared helpers, the 10-min tree was stale relative to `HEAD` (the untouched
5-min tree already contains `AnimalID_raw`), and `epoch_duration_qc` is written
at Stage 09 line **603** whereas the edit begins at line **1806** — causally
downstream, so it cannot be the cause. Neither table is in the registry or the
release plan.

## Panel D — no fitted line at all

The descriptive quintile-median guide is **non-monotone** in this sample, so it
read as noise rather than as a guide. The main panel now shows the scatter with
the canonical ρ / CI / q annotation and no line; the quintile guide and the
rank-space version (reference-line slope = canonical ρ) are retained as panel
candidates.

## Status after this pass

| Panel | Status | Owner |
|---|---|---|
| A | `CANONICAL_READY` | `Analysis/build_later_outcome_combz.R` |
| B | `READY_AFTER_FORMATTING` (framework; decision recorded) | Stage 03 + validated allowlist |
| C | `AMBIGUOUS_SOURCE` for the categorical contrast; distribution is canonical | Stage 09 |
| D | `READY_AFTER_FORMATTING` | Stage 09 pooled Spearman |
| E | `CANONICAL_READY` | Stage 09 LOAO + persisted draws |

---

# Addendum 2 — final editorial and composition pass

Baseline: `967fcb11804fa3604a24678e1fc613207d9f0dd1`, with the CombZ
canonicalization and Stage 09 permutation-draw persistence present as
uncommitted work. **No scientific value was changed in this pass** — proven by
re-hashing all 14 canonical producer outputs (byte-identical), re-checking 19
headline values (identical), and re-checking all 117 per-animal CombZ values and
outcome-group labels (identical).

## The main figure is now FOUR panels

Panels A and B were merged. The previous pass had already concluded
`B3_NOT_JUSTIFIED__USE_FRAMEWORK_SCHEMATIC`, which left a main-figure slot doing
work no validated analysis could support. Merging removes the slot rather than
filling it weakly.

| panel | content | class |
|---|---|---|
| **a** | experimental design, RFID framework, characterised-domain inventory, first-night window, later battery, CombZ construction, later classification | `OUTCOME_DEFINITION` + `DESCRIPTIVE_FRAMEWORK` |
| **b** | first-night Movement by later outcome group | `DESCRIPTIVE_DISTRIBUTION` |
| **c** | early Movement vs later CombZ | `INFERENTIAL_MAIN` |
| **d** | out-of-sample prediction: d1 held-out predictions, d2 real permutation null | `INFERENTIAL_MAIN` |

**There is no main-figure slot for a domain heatmap, and no configuration can
create one.** The former `PANEL_B_SOURCE` switch is gone; `ED_DOMAIN_OVERVIEWS`
governs Extended Data only, and a test asserts the heatmap builder is only ever
called after the Extended Data marker.

Final size: **183 × 162 mm** (PDF MediaBox 518 × 459 pt), inside the 170 mm
limit and the 165 mm preference. Smallest embedded font **5.00 pt**; panel
letters 8.00 pt bold lowercase a–d.

## Panel a: what it does and does not assert

The domain inventory is a **measurement** statement. Each of the six rows
carries its own `claim_status` in the Source Data:

- `Movement / psychomotor activation` — carries the main-figure early signal
- flexibility, social-spatial, volatility — characterised; no main-figure claim
- `Behavioural state architecture (latent)` — characterised only; latent-state
  identifiability caveats; no main-figure claim
- `Inactivity-related locomotor organization` — characterised only;
  inactive-phase biological interpretation barred by `KNOWN_LIMITATIONS` item 3

Two of these are listed as *measured* despite being barred as *claims*. That is
deliberate and is the reason each row carries an explicit status: the panel
answers "what does the continuous record characterise?", not "where do the
groups differ?". A test asserts the panel builder contains no `q`, `p`, `fdr` or
significance term at all.

Proximity-derived measurement is labelled **social-spatial (co-location)
organization**, never sociability.

## Panel a: CombZ provenance wording

The wording was tightened across the figure, the legend, the Stage 27 README and
`docs/COMBZ_CANONICAL_DEFINITION.md` to say exactly:

> Canonical CombZ and later outcome assignments are reproduced exactly from the
> historically defined component z-scores. The historical upstream
> standardization is preserved as source provenance rather than redefined.

This separates **downstream reproducibility** (component z-scores → CombZ →
outcome group, established exactly: `2.22e-16`, 0 of 93 label mismatches) from
**upstream derivation** (raw measurement → component z-score, *not*
reconstructed under one coherent algorithm). An audit of every
manuscript-facing CombZ reference found no overclaim; the only occurrence of
"regenerated from raw" is the prohibition against writing it.

## Panel c: no fitted line at all

The descriptive quantile-median guide was removed from the main panel — it is
non-monotone in this sample and read as noise. The main panel is the raw scatter
plus the pooled annotation (ρ = −0.390, 95% CI [−0.547, −0.209], BH q = 6.88e-05,
n = 111) and a direction note. Group colour is a visual aid; the legend states
explicitly that three separate within-group correlations are neither implied nor
reported. The quantile guide and the rank-space version remain panel candidates.

## Panel d: real null, and wording

d2 plots the **actual 1000 persisted permutation draws**, verified row-for-row
against the producer's file at tolerance 0. The identity line in d1 is a visual
reference only; no regression is fitted to it and none is reported. Performance
is the canonical `cv_r2_vs_mean` = 0.15939 with permutation p = 1/1001.

Wording is constrained and tested: "internal out-of-sample validation", never
external validation / independent cohort / independently validated, and never an
AUC or classification framing for a continuous endpoint. The negation-aware test
allows those phrases only inside a disclaiming sentence.

## Provenance of the superseded figure

The previous five-panel composition is retained at
`figures/superseded_candidates/behavior_outcome_and_early_prediction_main__five_panel_superseded.{pdf,svg,png}`
with a `README.md` recording why it was superseded. It is not silently
overwritten, and a test asserts the old stem no longer appears in `figures/main`
while the new stem does.

## Extended Data candidates after this pass

- broad phase-resolved domain map (unregistered; all FDR cells Inactive)
- registry-cleared first-night domain map (1 of 30 cells FDR-supported)
- longitudinal movement across CC1–CC4 × phase (registry-cleared, descriptive)
- panel candidates: CombZ distribution, rank-space association, sex-faceted
  association, null summary-interval view
- Stage 20–25 GAMM and Inactive Markov figures remain owned by Stage 26

## Status

| Panel | Status |
|---|---|
| a | `READY_AFTER_VISUAL_REVIEW` |
| b | `READY_MAIN` |
| c | `READY_MAIN` |
| d | `READY_MAIN` |
| combined | `READY_AFTER_VISUAL_REVIEW` |

---

# Addendum 3 — provenance vocabulary and claim/test identifiers

## Scientific owner vs immediate source

Provenance is two facts, and one column cannot carry both. Every Source Data
file, the figure manifest, the Source Data manifest and the claim trace now
record them separately:

| field | meaning |
|---|---|
| `scientific_owner_stage` | the stage whose code DEFINES the quantity — fits the model, chooses the window, runs the test. The stage a claim belongs to. |
| `owner_table` | the canonical table in which that stage defines it |
| `immediate_source_stage` | the stage whose file Stage 27 actually READ |
| `source_table` | the file actually read |

They are equal for most panels. They differ for exactly two reads:

| panel | owner | immediate source |
|---|---|---|
| b, c (animal-level Movement / CombZ) | 09 — `pipeline/09_early_prediction/10min/tables/model_ladder_input.csv` | 16 — `manuscript/behavior/animal_level_source_data.csv` |
| d1 (held-out predictions) | 09 — `pipeline/09_early_prediction/10min/tables/primary_prediction_predictions.csv` | 16 — `manuscript/behavior/prediction_source_data.csv` |

**Stage 16 is a validated manuscript export layer, not a producer.** Verified
from its source, not assumed: it contains no `lm`/`glm`/`bam`/`gam`/`lmer`, no
`predict()`, no `set.seed()` and no `sample()`, and it carries the prediction
columns through by `transmute(observed_CombZ = observed, predicted_CombZ =
predicted)` — a rename, never a derivation. Its only other operations are a
filter to the two canonical behavior-only models and a label recode. The values
are bit-identical to Stage 09's own tables, asserted with `identical()` and
independently re-checked by Stage 27's zero-drift gate.

The earlier schema recorded `canonical_stage = "16"` for panel d1, which made
the export layer read as the producer of the LOAO predictions, while panels b
and c recorded Stage 09's table without disclosing that the values were read
through the same export. Both are now stated exactly.
`Testing/tests/test_behavior_main_figure_contracts.R` asserts that Stage 16
never appears as a `scientific_owner_stage` and that Stage 16 acquires no
model-fitting machinery.

## Claim ids and test ids are different things

The upstream frozen-results table keys rows by an ANALYSIS id
(`S09_ASSOC_Movement_mean`, `S09_PRED_movement_mean`). Those are **test ids**.
The manuscript claims are `CLAIM_BEHAV_01..05`. Writing the former into a column
headed `Claim id` meant `CLAIM_BEHAV_04` — the headline number of the figure —
appeared nowhere in `behavior_main_key_results.csv` and could not be joined to
the claim trace.

The key-results table now carries four separate fields:

| field | example |
|---|---|
| `Claim id` | `CLAIM_BEHAV_04` |
| `Test id` | `S09_ASSOC_Movement_mean` |
| `Model or feature id` | `Movement_mean` |
| `Row role` | `headline_estimate` |

`Row role` distinguishes the headline estimate from the rest of its BH family
(`multiplicity_family_member`), the supporting model (`supporting_model`), the
permutation reference (`permutation_reference`) and the panel b descriptive
rows (`descriptive_context`). Family and supporting rows share the claim id
they serve, because they are what makes that claim's q and null interpretable;
the role column stops them reading as separate claims. `CLAIM_BEHAV_02` is
descriptive and deliberately appears in no key-results row, so it can never
acquire a p or q.

## Legend length

The legend body is capped at 450 words and tested. Provenance machinery (parity
residuals, classification thresholds, window-coverage tallies, the repeated-CV
companion, the rank-model rationale) lives in this document, the claim trace and
the key-results table, not in the legend. A companion test asserts that twelve
required concepts survive the compression, so the cap cannot be met by dropping
substance.

## Panel a provenance-note anchor

The CombZ provenance note is anchored at panel-a x = 46, inside the CombZ box
span (x 46–76), stacked beneath the direction note. It previously sat at x = 1,
under the first-night Movement box (x 1–40), where a column-wise reader would
attach a statement about CombZ reproduction to the movement window. A test
asserts the anchor stays at or right of x = 46.

## Non-blocking

`NONBLOCKING_EXPORT_METADATA_VARIANCE` — the PDF carries `CreationDate` and
`ModDate`, so rebuild hashes differ even when the drawn content is identical.
Scientific reproducibility does not depend on byte-identical PDF metadata, and
the repository has no deterministic-export standard to conform to. Not fixed.
