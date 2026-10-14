# CC4 reader knockout (ckr_v1.0)

**Status.** UNREGISTERED, isolated, group-blind audit (2026-10-14).
- Code: `Testing/audits/audit_cc4_reader_knockout.R` (driver; its header holds the pre-declared design) and `Functions/cc4_reader_knockout.R` (pure helpers).
- Test: `Testing/tests/test_cc4_reader_knockout.R`.
- Stages 29 and 31-31d, S13 and every registered or frozen output are unchanged. Nothing is refitted.

## Question

Two CC4 cage episodes lost one antenna for the whole session:
- B2|sys.5|CC4 lost R3;
- B5|sys.1|CC4 lost R5, the house.

srs_v1.1 measured the dependence of the vendor position stream on these antennas only in the Stage 29 windows (CC4: A1 and I2). There the active crossing-rate shift was material and the light one was not.

Stages 31-31d count crossings over the whole CC4 recording, including the grid-associated I3-I5 and A3-A5. This audit measures, on the healthy CC4 board-recordings, how far deleting R3 or R5 moves those crossing outcomes. That bears on decision 1: excluding the dead-channel episodes at the outcome level rather than across the whole roster.

## Pre-declared design (fixed before any run)

**Units**
- Every animal of a healthy CC4 board-recording. "Healthy" means all CC4 Batch x board except manifest v1.1's CC4 `exclude_spatial` episodes, which must be exactly B2|sys.5|CC4 and B5|sys.1|CC4.
- The animals are Stage 31's roster: the vendor export minus the canonical exclusions, with the v2 cage-label corrections applied.
- The board is the corrected System label. It must equal the board holding most of the animal's raw reads.
- An animal-phase is complete when Stage 31's `EligiblePhase` holds on the vendor export: the session's and the animal's first and last rows bracket the whole 12-h block. This is fixed for all variants.

**Variants**
- baseline: the vendor AnimalPos stream emulated from the raw reads with `apr_reads()` and `apr_positions(gate = "vendor")`. This is the reconstructed vendor estimator, unchanged.
- ko_R3 and ko_R5: the same after deleting the reads at PositionID 3 or 5 on the animal's own board. All animals of the recording are knocked out at once, and off-board reads are kept (as srs_v1.1).

**Crossings** (Stage 31's rule)
- A record whose PositionID differs from the animal's previous record, counted in the phase of the later record.
- A previous record before the phase counts; the first record never counts.

**Parity gate**
- The baseline must reproduce Stage 31's crossing counts per animal x phase (A1-A5, I2-I5) on the healthy board-recordings. The Stage 31 counts come from Stage 31's own functions on `raw_data/<B>/E9_SIS_<B>_CC4_AnimalPos.csv`, and they must equal the frozen run `whole_inactive_20260929_v1` exactly.
- A release fails, writing nothing, if fewer than 95% of the animal-phases match exactly, if the pooled count differs by more than 1%, or if the frozen-run comparison is not exact.

**Outcomes**
- Crossings per hour for each of the nine phases.
- Active family: reference_rate = A2; post_rate = A3-A5 pooled per 36 h; log_rate_ratio = log((post + 0.5)/36) - log((ref + 0.5)/12).
- Inactive family: the same with I2 and I3-I5.
- rate_A1 and rate_I2 alone, for continuity with srs_v1.1.
- An incomplete phase gives NA, never 0. The family outcomes need all four family phases complete.

**Materiality**
- shift = knockout - baseline.
- Scale per outcome = sd() of the residuals of lm(baseline ~ Batch) over the healthy animals (group-blind).
- Material when the median |shift| >= 0.2 SD or the 90th percentile >= 0.5 SD (the srs_v1.1 rule).
- The scale uses the formula of srs_v1.1's light-window scale but is computed on the post-SIS CC4 baselines (group-blind, yet including group-related variance). srs_v1.1 used CC1 baseline light windows and, for the active window, the frozen Stage 29 standardizers, so the SD units of the two audits are not comparable; compare the unscaled shifts (srs_continuity.csv) instead.

**Not read**: group labels, CombZ, outcome tables, Stage 29 values.

## Run

```
Rscript Testing/audits/audit_cc4_reader_knockout.R [--run-id=<new id>] [--output-root=<absolute>] [--recordings=<regex>]
```

- Default output: `<Behavior>/RANM/cc4_reader_knockout/runs/ckr_v1.0_<commit7>`.
- The run passes the isolated-root check and the path budget, and runs under the protection guard.
- A release needs committed, clean code. `dev*` run ids may use `--recordings`.
- Runtime is about 1 min per CC4 recording (about 7-8 min in total).

## Outputs

**tables/**
- `parity.csv`: two gated rows, plus context rows for the excluded episodes and the record-by-record stream parity.
- `animal_phase.csv`: variant x animal x phase.
- `animal_family.csv`
- `scales.csv`
- `shifts.csv`
- `materiality_cc4.csv`, with columns reader, family, outcome, n_animals, n_board_recordings, scale_sd, median_shift, median_abs_shift_sd, p90_abs_shift_sd, material, rule.
- `animals.csv`
- `recordings.csv`
- `srs_continuity.csv`

**Other files**
- `README.txt`
- `manifest/`: code_hashes, input_hashes, run.csv, session_info.txt, protection_snapshot_before.csv, protection_check.csv, output_files_sha256.csv.

## Development check (dev_ckr_b1, B1 CC4 only)

- **Parity:** 126/126 animal-phases exact (28,691 crossings both ways), and the frozen Stage 31 run was reproduced exactly. 14/14 animal streams were identical to the vendor export record by record.
- **Continuity:** rate_I2 equals srs's light-window rate. rate_A1 lies within 0.056/h of srs, and its knockout shifts within 0.085/h (R3) and 0.008/h (R5).
- **Runtime:** 62 s for B1 CC4.
- **No verdict yet:** the materiality figures of this dev run use a one-batch fallback scale and are not the declared verdict. The verdict comes from the release run over all six recordings.

## Reading the result

- The rates count thresholded displacement events of the vendor estimate (>= 200 grid units). They are not locomotion, proximity, occupancy or resource engagement.
- The shift distribution on healthy boards estimates how much a whole-session silent reader moves these outcomes in a typical cage. It is not a bound for the affected episode, it does not model effects of a dead antenna beyond its missing reads, and it does not correct that episode's values.
- Under rule cc4e_v1.0 the result grades the kept inactive cells of the affected episodes (retained_flagged or retained_material); it never excludes them (manifest v1.1 keeps the light-phase rate). A material result is a decision for Tobias (docs/CC4_OUTCOME_ELIGIBILITY.md).
- The knockout is never used to choose a reconstruction, and it is never read against group effects.