# First-night leading bins: why 61 animals look "incomplete", and what to do about it

**Status: DIAGNOSED, NOT FIXED.** The pipeline is unchanged. This document exists
so the fix can be done in one deliberate pass rather than rediscovered.

**Date:** 2026-09-22
**Affects:** `Analysis/01_build_multiscale_behavior_metrics.R`, the first-night
window QC, and the Extended Data domain map in
`Analysis/27_build_behavior_main_figure.R`.

---

## 1. The symptom

The first-night window is 12 h in 10-minute bins = 72 slots. Of 111 animals,
**50 have all 72 and 61 do not**. The 61 are missing *only* leading slots:

| slots missing | animals |
|---|---|
| 1 | 31 |
| 2 | 14 |
| 3 | 10 |
| 4 | 6 |

Zero animals miss interior or trailing slots. Coverage runs 94.4%–98.6%.

Until 2026-09-22 the Extended Data caption reported this as
*"Only 50 of 111 animals have a complete window"*, which reads as **n = 50**.
It is not: every animal contributes to the contrasts. That wording has been
removed.

---

## 2. The mechanism, established

Three facts, each verified directly, compose into the explanation.

**(a) RFID reads are emitted only on position CHANGE, not periodically.**

Across 135,541 consecutive read pairs in `raw_data/`, only **98 (0.072%)** have
the same position as their predecessor. A gap in the read stream therefore means
*the animal did not change position*, not *the animal was unobserved*.

**(b) The pipeline already carries position forward — but only after the first read.**

`make_occupancy_intervals_one_system()`, `Analysis/01_build_multiscale_behavior_metrics.R:407`:

```r
current_pos <- rep(NA_integer_, length(animals))    # starts NA
...
current_pos[a] <- updates$PositionID[j]             # updated on each read
valid <- is.finite(current_pos) & current_pos > 0   # only animals with a known position
if (!any(valid)) next
```

`current_pos` is a carry-forward state vector. It is why **56% of all bins
(107,999 of 191,445) legitimately carry `Movement == 0`** with a row present, and
why there are **zero `NA` Movement values**. But before an animal's first read
`current_pos[a]` is `NA`, the animal fails `valid`, no occupancy interval is
produced, and the bin does not exist at all.

**(c) The read that would seed `current_pos` is discarded by preprocessing.**

For batch 5 cage change 1 the raw file spans `2023-11-20 13:46:52` onward; the
preprocessed file begins `18:30:03`, the cage-change time. Roughly 4h43m of
recording is dropped. Animal `00317` has **70 raw reads before 18:30, the last at
18:28:48** — 72 seconds before the window opens. That read is exactly what would
initialise its position, and it is not in the preprocessed input.

**Therefore:** the leading bins are absent because the pipeline cannot establish
where the animal was, not because the animal was absent or unobservable. Under
change-only logging those bins should be `Movement = 0`.

---

## 3. Two wrong turns, recorded so they are not repeated

**"The animals were being handled / transferred before 18:30, so pre-window
position is not carry-forward-able."** Not so — the animals were already in the
cage in all cases (confirmed by the experimenter). The pre-window period is
ordinary home-cage occupancy.

**"53 of 53 late starters resumed at a DIFFERENT position, so they moved during
the gap and carry-forward would fabricate stationarity."** This is circular. Under
change-only logging a read is emitted *only* when position changes, so the next
read after a gap is guaranteed to be at a different position. The observation
carries no information about movement during the gap. It was briefly used to argue
against the fix; it does not.

---

## 4. Quantified impact of the fix

Seeding the leading bins as `Movement = 0`:

| | |
|---|---|
| Bins recovered | **113 of 7,992** (1.4%) |
| Animals affected | 61 |
| Mean movement, affected animals | 3.5629 → **3.4765** (−2.42%) |
| Worst case (4 missing bins) | −5.56% |

Change in mean movement by outcome group: **CON −1.50%, RES −1.47%, SUS −1.27%**.
By sex: Female −0.89%, Male −1.99%.

The correction is **near-identical across outcome groups**, so it is not expected
to move the group contrasts, the one FDR-supported cell (Female RES−CON,
behavioural volatility, g = −0.946, q = 0.035), or anything CombZ-derived. CombZ
comes from the behavioural/physiological workbook, not from RFID, so the animal
lists and outcome groups are stable under this change.

Note the direction: the current pipeline **overestimates** activity for the 61,
because it drops precisely their quietest bins. The bias works against the
existing group differences rather than manufacturing them.

---

## 5. Proposed fix

Seed `current_pos` at the window start from each animal's last read *before* the
window, so the carry-forward state is populated from the first bin onward. Two
routes:

1. **Stop truncating `preprocessed_data/` at the cage change.** Simplest
   conceptually; changes the input to every stage and extends all analyses
   backwards, which is not wanted.
2. **Seed only the state vector** — read the last pre-window position per animal
   and initialise `current_pos` with it, without admitting the pre-window period
   into the analysis window. Targeted, and preferred.

Either way the recovered bins should appear with `Movement = 0`, which is the
same encoding already used for the 56% of detected-but-stationary bins.

---

## 6. What a fix would require re-running

- `Analysis/01_build_multiscale_behavior_metrics.R` (~4 h)
- everything downstream of `03_derived_metrics`: stages 02, 04–08, 11–14, then
  03, 09, 10, 16, 19, 26, 27
- re-freezing the numeric expectations in
  `Testing/tests/test_figure1_prediction_contract.R`,
  `test_stage09_permutation_draws.R` and
  `test_stage19_identity_and_stage06_schema.R`, which were already re-frozen on
  2026-09-21 for the CombZ endpoint correction
- removing the clause *"a bin with no RFID read is absent, not zero"* from the
  Extended Data caption in `Analysis/27_build_behavior_main_figure.R`, which
  documents the present limitation and would no longer be true

Estimated total: 6–8 h of compute plus a verification pass.

---

## 7. Interim position

The pipeline is **correct as it stands for every group comparison**, because the
bias is small and balanced across outcome groups. The defect is a modest upward
bias in absolute activity for 61 animals, and a caption that previously implied
an n it did not use. The caption is fixed; the bias is documented here and not
yet corrected.

If a reviewer asks: every animal contributes; no animal is excluded for window
completeness; bins in which an animal produced no RFID read are absent rather than
zero, which slightly overestimates activity for the least active animals, by 2.4%
on average and near-identically across groups.
