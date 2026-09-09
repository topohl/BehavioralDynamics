# CombZ — the canonical later composite stress-burden score

**Canonical producer:** `Analysis/build_later_outcome_combz.R`
**Canonical output:** `analysis_ready/canonical/later_outcome_combz/tables/`
**Semantic path key:** `behavior.later_outcome_combz`
**Definition id:** `combz_v1_six_component_unweighted_mean`

---

## Provenance statement — use this wording

> Canonical CombZ and later outcome assignments are reproduced exactly from the
> historically defined component z-scores. The historical upstream
> standardization is preserved as source provenance rather than redefined.

This distinguishes two different things, and the distinction matters:

| | what it covers | established here? |
|---|---|---|
| **A. downstream reproducibility** | canonical component z-scores → CombZ → outcome group | **YES, exactly.** `max|dCombZ| = 2.22e-16`; 0 of 93 label mismatches |
| **B. upstream derivation** | raw measurement → component z-score | **NO.** Not reconstructed under a single internally coherent algorithm |

**Do not write** "CombZ is fully regenerated from raw behavioural and
physiological data", or any wording implying B. It is not true: see §4a for the
positional per-sex formula blocks that make a single uniform reconstruction
impossible without changing the endpoint.

Do not obscure this limitation in provenance documents; equally, do not
foreground it in the biological Results text, where the relevant fact is simply
that CombZ is the later composite stress-burden score and that lower CombZ means
greater later stress burden.

Everything below was established by direct audit of the upstream workbook's
**cell formulas** (the `.xlsx` unzipped and its sheet XML read), not from prose
or from a previous summary. Where a previous audit was wrong, that is stated.

---

## 1. What CombZ is, and what to call it

CombZ is the **later composite stress-burden score**: the unweighted mean of six
standardized outcome measures collected *after* the RFID recording period.

It is **not** a behavioural score. Three of its six components are
physiological/somatic/endocrine (corticosterone reactivity, adrenal weight,
spleen weight). Preferred manuscript wording:

> later composite stress-burden score (CombZ)

**Direction — state this everywhere:**

- **higher CombZ = more resilient-like = lower later stress burden**
- **lower CombZ = more susceptible-like = greater later stress burden**

The direction is **not** harmonised to "higher = worse". This matters: the
canonical Movement→CombZ association is **negative** (ρ ≈ −0.39), which means
*greater early locomotor activity → lower CombZ → greater later stress burden*.
Reporting CombZ as a "burden score" without inverting the sign inverts the
biological claim.

Group means over the 117 workbook animals: CON `+0.027`, RES `+0.379`,
SUS `−0.713`.

---

## 2. Source variables and direction conventions

Canonical source: `SIS_Analysis/E9_Behavior_Data.xlsx`, sheet **`zScore`**,
columns **E:J** (components) and **K** (composite), 117 animals.

| zScore col | component | domain | raw measure | raw derivation | sign inverted | higher means |
|---|---|---|---|---|---|---|
| E | `NOR` | cognition / recognition memory | NOR discrimination index | `(contactNov − contactFam)/(contactNov + contactFam)` | no | better recognition memory |
| F | `sucrose_pref` | hedonic / anhedonia | combined sucrose preference (%) | `100 × sucrose / total fluid`, pooled over three tests | no | less anhedonia |
| G | `weight_dev` | somatic growth | body-weight change | difference between two scheduled weighing days | no | better somatic growth |
| H | `delta_cort` | HPA-axis reactivity | corticosterone rise | response − baseline concentration | **yes (×−1)** | smaller stress-induced rise |
| I | `adrenal_weight` | endocrine organ load | adrenal weight ratio | `100 × adrenal / body weight` | **yes (×−1)** | less adrenal hypertrophy |
| J | `spleen_weight` | immune organ load | spleen weight ratio | `100 × spleen / body weight` | **yes (×−1)** | less splenic load |

The three inversions are what make every component point the same way, so that
a higher composite always means a more resilient-like animal. In the upstream
workbook the inverted columns are literally named `OverallInvertA`,
`OverallInvertS`, `invertedD`.

---

## 3. Canonical formula

```
zScore!K<row> = AVERAGE(E<row>:J<row>)
```

Verified across all seven shared-formula blocks and every standalone cell in
column K. Therefore:

```
CombZ = mean(NOR, sucrose_pref, weight_dev, delta_cort,
             adrenal_weight, spleen_weight)
```

- **Equal weighting.** Each component carries weight `1/6`. There is no
  weighting scheme and no domain-level intermediate averaging.
- **Missing values.** Excel `AVERAGE` **ignores blank cells**, so an animal with
  five of six components is averaged over five — *not* missing-as-zero, and
  *not* dropped. In R this is `mean(x, na.rm = TRUE)`.
  Two of 117 animals are affected: **`OQ762`** and **`OR126`** (5 of 6).
- **Reproduction:** the producer recomputes this and agrees with the workbook to
  `2.22e-16`.

---

## 4. The z-score reference population — and a correction

Each component is a z-score of its raw measure:

```
z = (x − mean(x | reference)) / populationSD(x | reference)
```

**SD convention: population SD** (`n` divisor). The workbook uses `STDEVPA` in
959 formulas and `STDEV.P` in 2; **sample SD appears nowhere**.

**Reference population: the within-sex control animals.** Concretely, the
upstream formulas standardize against **12 male CON animals** for male animals
and **12 female CON animals** for female animals:

| reference | animals | batches |
|---|---|---|
| male | `OQ760, OQ761, OQ763, OQ765, OR126, OR127, OR128, OR129, OR621, OR622, OR623, OR624` | 1, 2, 5 |
| female | `413, 414, 415, 416, OR537, OR538, OR539, OR540, OR639, OR643, OR644, OR645` | 3, 4, 6 |

> **Correction to the previous audit.** The earlier pass reported the reference
> as "12 male CON animals" globally. That was **wrong** — it read only the
> first formula block. The reference is **within-sex**. Example, `organWeight`
> column `O`: rows 2–41 and 81–99 use the male list
> (`I$12,I$13,I$15,I$17,I$30:I$33,I$82:I$85`), while rows 42–80 use the female
> list (`I$42:I$45,I$65:I$68,I$100:I$103`).

### 4a. Why the components are not recomputed in the repository

**The reference is encoded as hard-coded absolute row positions, not as a
semantic criterion.** There is no `IF(Sex="m", …)` anywhere; there are four
positional formula blocks per column whose boundaries are row ranges.

Consequences, all verified:

1. Row ranges and sex only *approximately* coincide. In `organWeight`, rows
   2–41 are all male and 42–80 all female, but rows **81–118 are mixed**
   (19 female, 19 male) and use the **male** reference.
2. Sheets have different row layouts, so the same "12 reference rows" are
   written differently per sheet (`bodyWeight`, which has 250 rows, uses
   `12,13,15,17,31:34,87:90`; the 117-row sheets use `12,13,15,17,30:33,82:85`).
   Both resolve to the same 12 animals.
3. `sucrosePreference` labels four of those reference animals (`OQ760/761/763/765`)
   as **`SIS`** while every other sheet labels them **`CON`** — an upstream
   label inconsistency in one sheet.
4. Recomputing from raw with a single uniform within-sex rule does **not**
   reproduce the shipped values (max deviation ≈ 2.0 even block-aware).

Because §5 of this pass forbids "improving" the primary endpoint, the six
components are therefore **carried through verbatim** and their irreducible
external provenance is recorded in `combz_component_definition.csv`
(`reproducible_in_repository = FALSE`). Re-deriving them would change the
primary endpoint and invalidate every downstream validated result.

**Batch:** no batch term enters canonical CombZ. The canonical component values
match the workbook's `*_noBatch` family; the batch-referenced variants are not
used.

---

## 5. Later outcome classification (CON / RES / SUS)

**Rule, reproduced exactly:**

```
CON  : experimental control animals (from con_animals.csv)
SUS  : SIS-exposed AND CombZ < mean(CombZ | CON, same Sex)
                              − populationSD(CombZ | CON, same Sex)
RES  : any other SIS-exposed animal
```

- Computed **within Sex**.
- **Population SD is required.** With sample SD, four animals misclassify
  (`OR434`, `OR554`, `OR625`, `13856`).
- Thresholds, recomputed by the producer:

| Sex | n control | control mean | control population SD | susceptibility threshold |
|---|---|---|---|---|
| Male | 12 | `0.011033945` | `0.447675643` | **`−0.436641698`** |
| Female | 12 | `0.042130160` | `0.264521004` | **`−0.222390844`** |

- **Parity: 0 label mismatches** across all 93 SIS-exposed animals.
- CON animals are retained and are never labelled RES or SUS.
- Group counts (117 workbook animals): Female CON 12 / RES 24 / SUS 22;
  Male CON 12 / RES 30 / SUS 17. The RFID analysis set is the 111 of these
  with a resolvable first-night window (CON 24 / RES 49 / SUS 38).

The repository reaches the same labels by reading `sus_animals.csv` (39 ids)
and `con_animals.csv` (24 ids) in `Analysis/01_build_multiscale_behavior_metrics.R`
(unlisted SIS animals → RES). The producer derives the labels *independently
from CombZ* and gates on exact agreement with those lists, so the rule and the
lists can never silently diverge.

---

## 6. Temporal ordering relative to RFID

All six components are measured **after** the RFID home-cage recording:
organ weights and terminal corticosterone are terminal measures, and the
behavioural components (NOR, sucrose preference) and body-weight change are
collected during and after the repeated social-instability paradigm.

The RFID first-active-phase window analysed by Stage 09 is the **first 12 h
Active block after the first cage change (18:30 inclusive → 06:30 exclusive)** —
i.e. at the very start of the paradigm.

**Therefore the CON/RES/SUS labels did not exist at the time of the RFID
measurement.** Every figure and caption must call them the *later outcome group*
or *subsequent phenotype classification*, never a baseline group.

---

## 7. Canonical vs noncanonical composites

| artifact | kind | status |
|---|---|---|
| `zScore` sheet, column `CombZ` | workbook sheet | **CANONICAL** |
| `CombZScore_noBatch` | workbook sheet | `NONCANONICAL_ALTERNATIVE` |
| `combZScore` | workbook sheet | `NONCANONICAL_ALTERNATIVE` |
| `sus_animals_batchCorrected.csv` | classification list | `NONCANONICAL_ALTERNATIVE` |

- `CombZScore_noBatch` column `J` is a **five-domain** composite that additionally
  includes social preference, EPM and OFT, and averages **per domain** rather
  than per measure. It gives materially different values (animal `0001`:
  `−0.157` vs the canonical `−0.343`).
- `combZScore` is a within-batch-referenced variant.
- `sus_animals_batchCorrected.csv` is a batch-corrected susceptible list that
  `Analysis/01_build_multiscale_behavior_metrics.R` does **not** read.

**No repository script reads any of these.** They are retained in the source
data (nothing is deleted) and are recorded in
`combz_alternative_composite_audit.csv`.
`Testing/tests/test_combz_canonical_definition.R` asserts that neither Stage 09
nor Stage 27 can resolve to them.

---

## 8. Output contract

`analysis_ready/canonical/later_outcome_combz/tables/`

| file | contents |
|---|---|
| `later_outcome_combz_animal_level.csv` | `AnimalNum`, `upstream_id`, `Sex`, `Batch`, `experimental_condition`, the six component z-scores, `n_components_present`, `CombZ`, `outcome_group`, `combz_definition_id`, `reference_population_id`, `classification_rule_id` |
| `combz_component_definition.csv` | one row per component: domain, raw measure, derivation, sign inversion, weight, standardization, reproducibility flag |
| `combz_classification_thresholds.csv` | per Sex: n control, control mean, control population SD, susceptibility threshold, SD convention, rule id |
| `combz_workbook_parity_audit.csv` | per quantity: n compared, max absolute difference, tolerance, pass flag, upstream source |
| `combz_alternative_composite_audit.csv` | the noncanonical alternatives and their status |

`experimental_condition` (CON/SIS, randomised assignment) is deliberately kept
separate from `outcome_group` (CON/RES/SUS, outcome-derived).

---

## 9. Downstream consumers

| consumer | uses |
|---|---|
| `Analysis/09_early_prediction_model_ladder.R` | CombZ as the prediction endpoint (`outcome_col <- "CombZ"`) and the group labels as descriptive metadata only — `Group` is excluded from every primary model |
| `Analysis/03_primary_raw_movement_phase_stats.R` | CombZ for descriptive movement–outcome associations |
| `Analysis/14_systems_neuroscience_summary_dashboard.R` | CombZ as the primary outcome for domain associations |
| `Analysis/16_manuscript_behavior_report.R` | packages CombZ into the manuscript source-data workbook |
| `Analysis/27_build_behavior_main_figure.R` | Panel A outcome definition and distribution |

---

## 10. Parity gate

`Analysis/build_later_outcome_combz.R` **hard-stops** unless:

- `max |recomputed CombZ − workbook CombZ| ≤ 1e-12` (observed: `2.22e-16`), and
- outcome-group label mismatches `== 0` (observed: `0` of 93).

The workbook endpoint is authoritative. If parity ever fails, the correct action
is to investigate the component columns — **never** to redefine the outcome so
the producer passes.
