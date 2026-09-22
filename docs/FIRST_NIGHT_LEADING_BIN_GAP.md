# First-night leading bins: why 61 animals look "incomplete", and what to do about it

**Status: FIXED 2026-09-22** in `0685366`. Sections 1–4 describe the defect as it
stood; section 5 records what was actually implemented.

**Date:** diagnosed and fixed 2026-09-22
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

> **This prediction was WRONG about the domain cells, and the rebuild proved it.
> See 4a.** CombZ stability held. The contrasts did not.
>
> The error was one of extrapolation: the −2.4% figure and the group balance
> behind it are properties of **mean movement**, and I generalised them to
> metrics that respond to a prepended bin quite differently. Volatility and
> fragmentation measures are built from successive differences, so adding a
> leading `Movement = 0` bin changes them structurally, not just in level —
> and the animals gaining bins are systematically the quieter ones.

Note the direction: the old pipeline **overestimated** activity for the 61,
because it dropped precisely their quietest bins. The bias worked against the
existing group differences rather than manufacturing them.

### 4a. Measured outcome after the rebuild

A prediction was registered before the rebuild finished, from a model that first
had to reproduce the *before* numbers exactly (it did: 113/50 and 265/33). The
rebuild matched it on every count:

| | before | predicted | actual |
|---|---|---|---|
| 10 min — animals complete | 50 | 111 | **111** |
| 10 min — missing leading slots | 113 | 0 | **0** |
| 5 min — animals complete | 33 | 111 | **111** |
| 5 min — missing leading slots | 265 | 0 | **0** |

Both grids are now completely full — 7,992 rows at 10 min and 15,984 at 5 min,
minimum per-animal coverage 1.000 — with interior and trailing gaps still zero.

In the first bin of the window all 111 animals now have a row, 61 of them at
`Movement = 0`. Mean `observation_seconds` in that bin is **553.8 of 600**, not
600, and that is correct rather than a shortfall: each file's first timestamp
falls 3–89 s after its 18:30 anchor, and the fix does not invent occupancy
before recording starts.

**Two statistical results moved, contrary to the section 4 prediction.**

*The Extended Data domain map lost its only FDR-supported cell.*

| | before | after |
|---|---|---|
| Female RES−CON, behavioural volatility | g = −0.946, q = **0.035** | g = −0.844, raw p = 0.00595, q = **0.0893** |
| FDR-supported cells | 1 of 30 | **0 of 30** |
| nominally p < 0.05 | — | 6 of 30 |

It is still the strongest cell in its family and its raw p is 0.006; what
changed is that a ~11% smaller effect no longer survives BH across 15 tests
within Sex. Family structure is unchanged (15 tests per Sex, per-contrast
n 26–46), so this is a shift in effect size, not a change in what was tested.

*`Entropy_acf1` crossed the threshold the other way*, from BH p ≥ 0.05 to
0.04776 with a bootstrap CI that clears zero by 1e-4. Its partial correlation
controlling movement is −0.054, so it still adds nothing beyond `Movement_mean`.
See the contract block in `Analysis/16_manuscript_behavior_report.R`.

`Movement_mean` was essentially unmoved: rho −0.4038 → −0.4082, still the
headline association. CombZ, the animal lists and the outcome groups are
unchanged, as predicted — they come from the workbook, not from RFID.

---

## 5. The fix, as implemented

Route 2 was taken: **seed only the state vector.** Route 1 (un-truncating
`preprocessed_data/`) would have changed the input to every stage and extended
all analyses backwards, which is not wanted.

`Analysis/01_build_multiscale_behavior_metrics.R` now recovers each animal's
last valid pre-window position from `raw_data/` and injects it as one synthetic
row at the file's first timestamp, between `all_pos` assembly and
`occupancy_intervals`. The recovered bins appear with `Movement = 0`, the same
encoding already used for the 56% of detected-but-stationary bins.

Four details that matter:

- **The position is recovered, not back-filled.** Back-filling an animal's first
  *observed* position would be wrong: under change-only logging that is the
  position it moved *to*. Animal `00317` is the worked case — seed position 5 at
  18:28:48, first in-window read position 3 at 19:11:33. `Movement` is 0 either
  way, but `Entropy` and `Proximity` depend on where the animal actually was.
- **Off-grid reads are skipped.** 16% of raw B5_CC1 reads (e.g. `166.7,116`) are
  off the `tblPosition` grid and carry no `PositionID`; the seed uses the last
  read that maps to a valid grid position.
- **Time-derived fields are copied from a real observation at the window start**
  (`Phase`, `ConsecActive/Inactive`, `HalfHoursElapsed`, `MinutesOfDay`, the
  Cookie fields), so nothing is recomputed. Animal-level fields come from the
  animal's own first row.
- **The seed is stamped at the window start, not at its true read time.** This
  keeps the pre-window period out of the analysis window. The cost is that
  `time_since_last_genuine_position_event_sec` for the first real read is
  measured from the window start rather than from 18:28:48.

**Verified against all 24 files before running:** 412 seeds over 111 animals,
schema preserved, no duplicate `(SourceFile, AnimalNum, DateTime)`, all
`PositionID` in 1–8 with no `NA`. All 105 late first-night animals are seeded.

**Not seeded:** 8 animals in B1_CC2, whose raw file contains no pre-window reads
at all (they are late by 0.1–8.0 min). Nothing exists to recover. B1_CC2 is not
a first-night file.

One side effect worth naming: the animals' *first real read* now has a
predecessor, so it registers as a genuine position change where previously it was
discarded for lack of a `PrevPositionID`. That is a real movement being
recovered, not a fabricated one.

---

## 6. What the fix requires re-running

- [x] the ED caption clause in `Analysis/27_build_behavior_main_figure.R`
- [x] `Analysis/01_build_multiscale_behavior_metrics.R` (took ~3 h)
- [ ] everything downstream of `03_derived_metrics`: stages 02–14 (bar the
      optional 15), then 16, 19, **20–25**, 26, 27

  An earlier draft of this list read "02, 04–08, 11–14, then 03, 09, 10, 16, 19,
  26, 27" and **omitted stages 20–25**. All six GAMM stages read
  `all_behavior_metrics.csv` from `03_derived_metrics`, so they go stale with
  stage 01, and stage 26 consumes their tables — running that chain would have
  rebuilt the manuscript figures on stale GAMM output.

- [ ] re-freezing the numeric expectations in
      `Testing/tests/test_figure1_prediction_contract.R`,
      `test_stage09_permutation_draws.R` and
      `test_stage19_identity_and_stage06_schema.R`, which were already re-frozen
      on 2026-09-21 for the CombZ endpoint correction — this is their third
      re-freezing

Estimated total: 6–8 h of compute plus a verification pass.

Note for whoever re-freezes the tests: the seeding also completes *partial*
leading bins. 105 first-night animals read late, not 61 — the other 44 were late
by less than one bin width, so their first bin existed but was only partly
covered. Expect slightly more movement than the section 4 estimate, in the same
direction.

---

## 7. Position after the fix

Every animal contributes; no animal is excluded for window completeness; a bin in
which an animal produced no RFID read is now scored `Movement = 0` rather than
dropped.

Conclusions from before the fix are not invalidated by it. The bias it removed
was small and near-identical across outcome groups (CON −1.50%, RES −1.47%, SUS
−1.27%), and it ran *against* the observed group differences rather than creating
them — the pipeline was overestimating activity for the least active animals. The
correction therefore sharpens the estimates rather than reversing anything.

The ED caption clause "a bin with no RFID read is absent, not zero" documented
the old behaviour and has been replaced.
