# Four-domain raw-RFID characterisation — reconciliation record

**Date:** 2026-09-23
**Base commit:** `cce855e` ("Re-freeze the contract constants against the leading-bin rebuild")
**Branch:** `refactor/rfid-four-domain-characterization`
**Producer:** `Analysis/28_rfid_behavioral_domains.R`

This document does two things: it records every manuscript-facing constant that
was stale against the regenerated data and what it was corrected to, and it
states the structural facts about this experiment that any reader of the new
analysis needs in order to interpret it.

Nothing in the legacy five-domain analysis was rewritten. Its artifacts under
`analysis_ready/12_systems_neuroscience_summary/5min_based/first_night/` are
byte-identical to what commit `cce855e` shipped, and the legacy contract test
still asserts five displayed domains and a flat 15-test family.

---

## 1. Stale constants found and corrected

Every value below was checked against the regenerated canonical tables, not
against an older commit.

| Claim | Stale value | Verified current value | Where it was | Status |
|---|---|---|---|---|
| First-night window completeness, 10 min | 50 of 111 | **111 of 111** (72/72 slots) | `KNOWN_LIMITATIONS.md:171`, `BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.md:114`, `MANUSCRIPT_ANALYSIS_REGISTRY.csv` row `FIRSTNIGHT_5DOMAIN_PANEL` | **corrected** |
| First-night window completeness, 5 min | 33 of 111 | **111 of 111** (144/144 slots) | `KNOWN_LIMITATIONS.md:172`, registry row `S09_SENS_5MIN_RESOLUTION` | **corrected** |
| Count-rule vs clock-window agreement | 50/111 at 10 min, 33/111 at 5 min | **111/111 at both** | `Functions/first_night_window_helpers.R:28-29`, `Analysis/14:1067-1068,5846`, six `Testing/audits/` scripts | **NOT corrected — see §5** |
| FDR-supported first-night cells | 1 of 30 | **0 of 30** | `BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.md:115,554`, `EXTENDED_DATA_CLASSIFICATION.md:68`, registry | **corrected** |
| Minimum q, first-night panel | 0.020108 | **0.0893** | registry `FIRSTNIGHT_5DOMAIN_PANEL.effect_size` | **corrected** |
| Female RES−CON volatility effect | g = −1.0093, p = 0.001341, q = 0.020108 | **g = −0.844, raw p = 0.00595, q = 0.0893** | registry | **corrected** |
| First-night family size (legacy) | 15 per Sex | 15 per Sex — **unchanged and still correct** | registry, `first_night_domain_helpers.R:29` | verified, no change |

The registry's `g = -1.0093 / q = 0.020108` was **two** generations stale: it
predates both the 2026-09-20 CombZ correction and the 2026-09-22 leading-bin
rebuild. `KNOWN_LIMITATIONS.md` and `FIRST_NIGHT_LEADING_BIN_GAP.md` already
carried the intermediate post-CombZ value (g = −0.946, q = 0.035) correctly as
history, so only the registry needed the two-step correction.

---

## 2. Structural facts about this experiment

These are properties of the design, verified from the data rather than assumed.
They constrain what any first-night or longitudinal contrast can mean.

**Sex is perfectly nested in Batch.** Male = B1 (14), B2 (20), B5 (19); Female =
B3 (20), B4 (19), B6 (19). No model can separate a Sex main effect from batch
variation across 18 months of running. A `~ Group * Sex + Batch` design is rank
deficient by exactly one column, which `mmm_rfid_aliasing_audit()` reports
explicitly rather than letting R drop it silently. The Group and Group × Sex
terms **are** estimable, because Group is crossed with Batch within each sex.

**The cage-epoch key is `Batch × System × CageChange`.** This partitions the data
into 120 cage epochs (30 per cage change) and is identical to `SourceFile ×
System`. Every animal maps to exactly one cage epoch per cage change, with zero
violations. `System` alone is *not* a cage: 18 animals occupy 2 distinct systems
across CC1–CC4, 58 occupy 3 and 35 occupy 4.

**CON animals are cage-segregated at every cage change.** Each batch has exactly
one CON-only cage holding all 4 of its CON animals, at CC1, CC2, CC3 and CC4. No
cage ever mixes CON with RES or SUS. Consequently RES−CON and SUS−CON are
**between-cage** contrasts whose effective sample size is 6 control cages, not 24
animals. **SUS−RES is the only contrast identified within cages** (19 mixed cages
at CC1).

**CON animals are never regrouped.** All 24 CON animals have exactly one distinct
cage-mate set across CC1–CC4 (Jaccard = 1.000 over all 72 CON transitions), while
every RES and SUS animal has four distinct sets (mean Jaccard 0.002–0.007). A
Group × CageChange interaction therefore contrasts *stable social group* against
*novel social group every four days*. That is the intended social-instability
manipulation, but it means a trajectory difference cannot be attributed to
stress-phenotype adaptation alone.

**Cage ICCs are substantial.** From the shipped `cage_lmer` fits at CC1, 10 min
(batch-adjusted, per Sex):

| Domain | Female | Male |
|---|---|---|
| Movement output | 0.292 | 0.213 |
| Social-spatial organization | 0.193 | 0.297 |
| Cross-channel behavioral volatility | 0.158 | 0.184 |
| Spatial entropy dynamics | 0.108 | 0.000 (singular, recorded not dropped) |

Unconditional (batch-unadjusted) cage ICC reaches 0.56 for Movement output in
males. Treating animals as exchangeable is anticonservative for every domain
except spatial entropy dynamics, which is the one domain with essentially no
cage-level variance.

**Two animals are structurally unscorable on proximity.** OQ770 and OQ771 (both
SUS males, B1, `sys.2` and `sys.5`) were singly housed at CC1, so
`ProximityFraction = ProximitySeconds / dyadic_observation_seconds` is undefined.
They are correctly `NA`, never zero-filled — but the loss is **not** missing at
random, since it removes exactly the isolated animals. Proximity-dependent
domains therefore have n = 109, not 111, and the male cell differs by row.

---

## 3. What changed in the analysis, and why

| Aspect | Legacy | New | Reason |
|---|---|---|---|
| Domains | 5 | 4 core | The fifth ("Active-phase adaptation / exploration") is excluded because a single 12 h level/persistence composite does not operationally measure adaptation. Not a p-value decision; the formula is not claimed to be invalid. |
| Formulas | — | **identical** for all four retained domains | Only labels changed. Verified by contract test against the legacy scorer. |
| Labels | psychomotor / flexibility / fragmentation | movement output / spatial entropy dynamics / cross-channel volatility | RFID position transitions do not establish a psychomotor construct; entropy dispersion is not cognitive flexibility; proximity is co-location, not sociability; the score has no fragmentation variable. |
| Model | `lm(~ Group * Sex)` | `lmer(~ Group + Batch + (1\|CageEpochID))` within Sex, plus legacy-LM, batch-LM and CR2 variants | Cage ICCs 0.03–0.45; proximity is mechanically coupled within a cage. |
| Multiplicity | flat 5 × 3 = 15 per Sex | hierarchical: BH over 4 domain omnibus tests per Sex, then Holm over 3 contrasts inside a supported domain | The flat family answers no question that is actually asked. |
| Adaptation | a static composite | `Group × CageChange` across four equivalent acute windows | Adaptation is change across repeated perturbations. |
| Longitudinal scaling | within `Sex × PhaseClass × CageChangeIndex` | **fixed** from CC1 within Sex | Re-centring each cage change removes the longitudinal signal by construction. |

A second pass added two PREDECLARED changes that increase power without relaxing
the cage adjustment (see §4b): sexes are **pooled** when no Group × Sex
interaction survives, and **focused tests** (1-df ordered trend at first night,
2-df predeclared change contrasts longitudinally) are reported alongside the
diffuse omnibus tests.

**The scheme was still not selected for the number of significant findings.** On
the first-night omnibus family it is strictly more conservative than the legacy
analysis: legacy 0 of 30 cells FDR-supported (min q = 0.0893) versus 0 of 8 domain
omnibus tests (min q = 0.516). The one result that does survive (§4c) emerged from
a predeclared contrast, survives cage adjustment and leave-one-batch-out, and is
reported together with the two confounds that limit it.

---

## 4. Robustness of the new first-night result

The only cell that was nominally significant under the legacy-style model does
not survive cage adjustment:

| Female, Cross-channel behavioral volatility | raw p | denominator df |
|---|---|---|
| legacy LM (no adjustment) | 0.0426 | 55.0 |
| batch-adjusted LM | 0.0354 | 53.0 |
| cage random intercept | 0.129 | 17.1 |
| CR2 cluster-robust on cages | 0.329 | 5.7 |

The denominator df collapses from 55 to 17.1 and then to 5.7 because the
effective unit is cages (15 per sex), and for the CON contrasts specifically only
**6** control cages exist. The same pattern appears longitudinally: male Movement
output goes from raw p = 0.0085 with only an animal random effect to 0.343 once
the cage is modelled.

**Leave-one-batch-out: 14 of 24 first-night pairwise contrasts change sign** when
one batch is dropped. With three batches per sex, one batch is a third of the
data. No first-night effect should be described as stable.

**Leading-bin seed sensitivity.** The 2026-09-22 fix injects a synthetic row
carrying the animal's last pre-window position, which is why coverage is now
111/111. A seeded bin with no further read contains one position, so `Entropy = 0`
exactly. 61 of 111 animals have at least one leading zero-entropy bin (max 4 of
72). The burden is **not** group-differential (Kruskal–Wallis p = 0.752), and
domain scores survive trimming the leading bins with Spearman ρ ≥ 0.927. Bounded
and not a confounder, but a real measurement caveat for the entropy channel.

**5-minute sensitivity** is a *different temporal construct*, not a replication:
RMSSD and ACF1 are lag-in-bin quantities, so at 5 min they measure a 5-min lag.
Female spatial entropy dynamics moves from raw p = 0.713 (10 min) to 0.114
(5 min). This is a change of measurement, not evidence of a hidden effect, and
must not be reported as one.

---

## 4b. Power audit — the first pass was conservative in two fixable ways

A power audit after the first pass found two places where the design was
needlessly conservative, and one place where the conservatism was correct.

**Correctly conservative: the cage term must stay.** The hypothesis that the cage
random intercept over-adjusts the CON arm is **false**. Within-group variance
decomposition for movement output gives cage/(cage+residual) = 0.300 for CON,
0.407 for RES and 0.497 for SUS — the cage effect is real in all three arms and is
*smallest* in CON. Dropping it is not defensible.

**Too conservative #1: splitting by Sex.** The Group × Sex interaction is null for
all four domains (min q = 0.834). Splitting halves n and doubles the family from 4
to 8 tests. The predeclared rule is now: test the interaction first; if no domain
shows one at q < 0.05, the **pooled-sex** model is primary and the within-sex split
is the sensitivity. Batch absorbs Sex, so the pooled model still adjusts for it.

**Too conservative #2: diffuse omnibus tests.** CON < RES < SUS is ordered by
construction, so the 2-df unordered omnibus spends power on an alternative nobody
hypothesises; a 1-df ordered trend is the matched test. Likewise the 6-df
Group × CageChange interaction is far less powerful than the predeclared 2-df
change contrasts that were already in the follow-up list.

**Minimum detectable effect — the null was never uniform.** Because CON animals
occupy CON-only cages, the two control contrasts are *between-cage* on 6 cages,
while SUS−RES is *within-cage*. Cage adjustment therefore **improves** precision
for SUS−RES (SE ratio 0.96, residual df ≈ 46) and degrades it for the control
contrasts (SE ratio ≈ 1.2, df ≈ 12):

| Contrast | identification | median df | median MDE (Hedges g) |
|---|---|---|---|
| RES−CON | between-cage, 6 control cages | 11.5 | **1.20** |
| SUS−CON | between-cage, 6 control cages | 14.1 | **1.26** |
| SUS−RES | within-cage | 45.6 | 0.85 |

A null on a contrast that can only detect g ≥ 1.2 is **not evidence of absence**.
The correct statement is that the design is uninformative below a very large
effect for anything involving CON, and genuinely null for SUS−RES.

## 4c. One result that does survive

With sexes pooled and the predeclared **CC2 − CC1** change contrast, cross-channel
behavioural volatility separates controls from stressed animals:

| Group | n | mean(CC2 − CC1) |
|---|---|---|
| CON | 24 | **−0.268** |
| RES | 53 | +0.406 |
| SUS | 31 | +0.197 |

| Contrast | estimate [95% CI] | p | identification |
|---|---|---|---|
| RES−CON | +0.689 [0.27, 1.11] | 0.0015 | between-cage |
| SUS−CON | +0.584 [0.12, 1.05] | 0.014 | between-cage |
| SUS−RES | −0.105 [−0.50, 0.29] | 0.604 | **within-cage** |

It is robust in a way nothing else here is: the baseline-cage variance is exactly
zero (so cage adjustment changes nothing), a CC2-cage random intercept still gives
p = 0.035, CR2 cluster-robust gives p = 0.048 (cage1) and 0.059 (cage2),
leave-one-batch-out gives p = 0.0027–0.038 with **no sign flips**, both sexes point
the same way (F p = 0.013, M p = 0.105), and the 5-min construct gives p = 0.0012.
CC3 − CC1 and CC4 − CC1 point the same way (p = 0.052, 0.061).

**Two caveats that are binding.** First, the effect is entirely CON-versus-stressed:
**SUS−RES is flat**, so this does not separate the stress phenotypes at all.
Second, and more seriously, **CON is the never-regrouped arm** — from CC1 to CC2,
controls return to the same cage-mates while RES and SUS meet an entirely new
social group. "Stressed animals become more behaviourally volatile at the second
cage change while controls settle" is therefore at least as consistent with the
regrouping manipulation itself as with a stress phenotype.

**Multiplicity depends on the declared family.** Within the predeclared CC2 − CC1
measure, BH over 4 domains gives q = 0.020. Across all 12 pooled change tests
actually run (3 measures × 4 domains), BH gives **min q = 0.060**. The 12-test
family is the more defensible declaration because all three measures were run.

## 4d. The orthogonal decomposition — the analysis that should be primary

CON / RES / SUS is not one three-level factor asking one question. It encodes two
questions that differ in what identifies them and in how far they can be trusted,
and they partition the 2 df of Group exactly:

| | contrast | identified by | confounds |
|---|---|---|---|
| **C1 manipulation** | (RES+SUS)/2 − CON | **between**-cage, 6 control cages | also confounded with regrouping — CON is the only arm that keeps its cage-mates |
| **C2 phenotype** | SUS − RES | **within**-cage, df ≈ 90–110 | none of the above; RES and SUS share cages and are regrouped identically |

Reporting them separately stops a well-identified phenotype contrast being buried
under a poorly-identified control contrast. They are corrected as **separate BH
families of four domains each**, because they answer different questions.

**First active phase (CC1), domain scores, pooled sexes, cage + batch adjusted:**

| Contrast | Domain | estimate [95% CI] | df | p | q | MDE *g* |
|---|---|---|---|---|---|---|
| C1 manipulation | Cross-channel volatility | −0.462 [−0.84, −0.08] | 20.6 | 0.020 | 0.078 | 0.77 |
| C1 manipulation | Spatial entropy dynamics | −0.558 [−1.14, +0.02] | 20.8 | 0.060 | 0.119 | 0.66 |
| C1 manipulation | Social-spatial organization | +0.178 [−0.68, +1.04] | 20.7 | 0.673 | 0.715 | 0.93 |
| C1 manipulation | Movement output | −0.098 [−0.65, +0.45] | 21.4 | 0.715 | 0.715 | 0.78 |
| **C2 phenotype** | Spatial entropy dynamics | −0.088 [−0.65, +0.48] | 99.3 | 0.758 | 0.909 | 0.65 |
| **C2 phenotype** | Social-spatial organization | −0.076 [−0.65, +0.50] | 89.0 | 0.792 | 0.909 | 0.62 |
| **C2 phenotype** | Movement output | −0.021 [−0.38, +0.34] | 91.2 | 0.908 | 0.909 | 0.52 |
| **C2 phenotype** | Cross-channel volatility | −0.016 [−0.30, +0.27] | 91.0 | 0.909 | 0.909 | 0.58 |

At the first night the phenotypes are **indistinguishable with tight bounds** —
every |estimate| < 0.09, and we can exclude effects beyond roughly ±0.3 to ±0.5 z.
That is a genuinely informative null, and it is the baseline that makes the later
separation interesting.

**Across cage changes, the phenotype contrast (SUS − RES, pooled sexes):**

| Domain | CC1 | CC2 | CC3 | CC4 |
|---|---|---|---|---|
| Spatial entropy dynamics | +0.018 | −0.163 | **−0.657** | **−0.630** |
| Cross-channel volatility | +0.027 | −0.102 | −0.111 | −0.041 |
| Movement output | +0.062 | −0.117 | −0.068 | +0.179 |
| Social-spatial organization | −0.033 | +0.432 | +0.014 | +0.063 |

Late-window SUS − RES in spatial entropy dynamics is **−0.644 [−1.12, −0.17],
p = 0.0078**. At CC3+CC4 the group means are CON 0.511, RES 0.402, **SUS −0.150** —
resilient animals track controls, susceptible animals separate from both. It
survives CR2 cluster-robust on cage ([−1.16, −0.03]), leave-one-batch-out gives
−0.36 to −0.74 with **no sign flips**, and the 5-min construct gives −0.51,
p = 0.013.

**It is nevertheless EXPLORATORY and is shipped as such.** The window was chosen
after inspecting the per-cage-change estimates. Both corrections are written to
the output: `q_narrow_within_window` = 0.031 (4 domains within the late window)
and **`q_wide_all_windows` = 0.094** (4 domains × 3 window choices = 12). The wide
value is the defensible one. The formal emergence test (late − early) is
p = 0.071, q_wide = 0.425, so "it emerges" is suggested by the trajectory but not
confirmed. The apparent sex-dependence (F p = 0.0068, M p = 0.86) is **not**
supported: the Group × Sex interaction at CC3/CC4 is p = 0.380, q = 0.625.

No single entropy contributor carries it (mean p = 0.73, rmssd p = 0.11,
acf1 p = 0.10); all three lean the same way and the composite aggregates them.

## 4e. Leading-bin burden is unequal ACROSS cage changes

The artificial first-phase concern is real and is now audited for every cage
change, not just CC1. Animals are introduced during the preceding Inactive phase,
which is not in the recording, so an animal that has not moved by 18:30 yields
seeded bins with `Entropy = 0` and `Movement = 0` exactly:

| Cage change | mean leading zero-entropy bins | % of animals with ≥1 | max |
|---|---|---|---|
| CC1 | 1.018 | 55.0 | 4 |
| CC2 | 0.523 | 33.3 | 3 |
| CC3 | 0.595 | 38.7 | 4 |
| CC4 | 0.450 | 32.4 | 3 |

**CC1 carries roughly twice the burden of CC2**, so a CC2 − CC1 change score on an
RMSSD-based domain could in principle be contaminated. It is not: the burden
*change* does not differ by group (Kruskal p = 0.73; CON −0.50, RES −0.55,
SUS −0.41), adjusting for it moves the volatility result from p = 0.00501 to
0.00520 with the burden term at p = 0.83, and domain scores survive trimming the
leading bins at Spearman ρ ≥ 0.927. `Movement` and `Entropy` have **zero** NAs in
all four windows; the only true NAs are proximity for singly-housed animals.

## 5. Known remaining defects, not fixed here

**Stale count-rule comments in protected files.** `Functions/first_night_window_helpers.R:28-29`
still states that the Stage 14 row-count rule agrees with the clock window for
"only 50/111 animals at 10-min bins and 33/111 at 5-min". Post-fix the count rule
agrees for **111/111 at both resolutions**, because there are no longer any
missing leading bins to push the count past the window end. The same stale
sentence appears in `Analysis/14:1067-1068` and `:5846` and in six
`Testing/audits/audit_first_night_*.R` headers.

These were **deliberately not edited**. `first_night_window_helpers.R` is locked
by `Testing/tests/test_first_night_window_parity.R`, which asserts byte-equivalent
selection against Stage 09's own window function, and `Analysis/14` is the legacy
producer. Correcting the comments is safe in principle — they are comments — but
it belongs in a separate, deliberate commit that re-runs the parity gates, not
inside a refactor whose safety argument is that it touched neither file.

**Seed staleness is unauditable.** `Analysis/01_build_multiscale_behavior_metrics.R`
computes `SeedReadTime` and then drops it (~line 630), and `seed_rows_for_file()`
puts no lower bound on how far before the window the seed read may come from. A
seed recovered from hours or days earlier is indistinguishable from one recovered
72 seconds earlier. Exporting `SeedReadTime` would close this, and would require
re-running Stage 01 — which changes every Stage 09 number and is therefore out of
scope here.

**Stale audit artifacts on disk.** Every output under
`audit_hmm_state_architecture/first_night_domain_heatmap/` carries a 2026-09-03
mtime, predating the leading-bin rebuild. The candidate-set audit chain
(`scores → effects → decision`) must be re-run in order before any of those CSVs
is cited. The decision vector in
`audit_first_night_candidate_set_decision.R:167-179` is a hard-coded `tribble`,
not a derivation, so re-running will not regenerate the decision itself.

---

## 5a. Where each piece lives

| Analysis | Function | Shipped table |
|---|---|---|
| orthogonal C1/C2 decomposition | `mmm_rfid_orthogonal_contrasts`, `mmm_rfid_orthogonal_family` in `rfid_domain_inference.R` | `first_night_orthogonal_contrasts_pooled.csv`, `..._by_sex.csv`, `longitudinal_orthogonal_contrasts.csv` |
| per-cage-change + early/late/emergence | `mmm_rfid_window_contrasts` in `rfid_longitudinal_inference.R` | `longitudinal_window_contrasts_by_cagechange.csv` |
| honest exploratory multiplicity | `mmm_rfid_exploratory_path_accounting` | same table, `q_narrow_within_window` vs `q_wide_all_windows`, `analysis_status` |
| minimum detectable effect | `mmm_rfid_mde` | `first_night_pairwise_contrasts_with_power.csv`, all orthogonal tables |
| sex-pooling gate | `mmm_rfid_pooled_sex_analysis` | `first_night_sex_pooling_gate.csv` |
| ordered trend | `mmm_rfid_ordered_trend` | `first_night_pooled_sex_ordered_trend.csv`, `first_night_ordered_trend_within_sex.csv` |
| predeclared change scores | `mmm_rfid_change_score_analysis` | `longitudinal_change_score_predeclared.csv` |
| leading-bin burden by cage change | `Testing/audits/audit_rfid_leading_bin_seed_sensitivity.R` | `leading_zero_burden_by_cagechange.csv`, `burden_change_by_group.csv` |

Contract tests 23-25 in `Testing/tests/test_rfid_domain_contract.R` lock the
orthogonality of C1/C2, the separate-family declaration, the PLANNED vs
EXPLORATORY status flags, and the all-cage-change burden audit.

## 5b. Corrected headline

The first pass concluded "interpretable null". That was wrong in two directions and
is superseded by sections 4b and 4c:

* For anything involving CON, the design cannot detect below g ~ 1.2, so those
  nulls are UNINFORMATIVE rather than negative.
* SUS-RES is genuinely and informatively null (MDE g ~ 0.85) in every domain,
  at first night and across all four cage changes.
* One predeclared contrast does survive: CC2 - CC1 cross-channel volatility,
  CON versus stressed, q = 0.020 within its measure and 0.060 across all 12
  change tests run. It is confounded with the regrouping manipulation.

## 6. What must travel with any manuscript use

1. The analysis is **descriptive** with respect to CON/RES/SUS. RES and SUS are
   defined by a later CombZ composite; these are animals grouped by their later
   outcome, not a prospective test. Stage 09 owns the prospective question.
2. **No Sex main effect is claimed.** Sex is nested in Batch. A difference between
   female and male significance is not a sex difference; that requires the formal
   Group × Sex interaction, which is null for all four domains.
3. **Proximity is co-location**, not sociability, social motivation, affiliation
   or preference.
4. RES−CON and SUS−CON rest on **6 control cages**; SUS−RES is the only
   within-cage contrast.
5. Any Group × CageChange statement confounds stress phenotype with **repeated
   novel-social-group exposure**, because CON is never regrouped.
6. Inactive-phase behaviour is **not analysed**, per `KNOWN_LIMITATIONS.md` item 3.
7. No HMM quantity enters the core domain set.
