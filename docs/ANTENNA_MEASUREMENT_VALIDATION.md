# Measurement validation of the RFID spatial and social measures (2026-10-13)

**Status: UNREGISTERED and descriptive.**
- No group-effect test or p-value.
- No threshold selected by any outcome.
- No Stage 29 run and no manuscript figure.

**Nothing frozen was changed:**
- Stage 08 HMM outputs (72 pins);
- Stage 09;
- the Stage 29 v1.0.1 release;
- the live RFID root.

All outputs are in the isolated root `<Behavior>/RANM/measurement_validation/`.

**Full report with figures:** `<Behavior>/RANM/measurement_validation/README.md`.

## Code and configuration

| file | role |
|---|---|
| `Functions/antenna_measurement.R` | definitions: read times from MsgValue1, detection union, support age, gaps, bounded support, runs (flip filter tau, carry-forward), runs cut to support, reader changes with geometry, occupancy, 1-s grid tracks, circular time-shift co-assignment (FFT), independence expectation, concurrent tags, system rows |
| `Testing/tests/test_antenna_measurement.R` | portable, hand-worked checks of every function (shift table against brute force) |
| `Testing/audits/audit_antenna_measurement_validation.R` | per recording into `per_recording/<B>_<CC>/` (resumable through `DONE.txt` code markers), then `tables/` |
| `Testing/audits/report_antenna_measurement_validation.R` | `summary/` and `figures/`; the five data versions through the frozen Stage 29 stream (parity and comparison, cached in `summary/03_data_version_metrics.csv`) |

**Fixed parameters.** These are declared ranges, none tuned:
- flip filter tau: 0, 0.5, 1, 2, 5, 10, 20 s. tau 2 = data version flip2s; 20 s = the frozen S12 window.
- carry-forward bound delta: unbounded, 300, 60, 30, 10, 5 s.
- support-age bounds: 1–1,800 s.
- bout criteria: the frozen 39.4 s and the S11 alternatives.
- time-shift nulls: ±5–60 min in 5-min steps, and 30 min to 11.5 h in 30-min steps.
- cage bootstrap: 2,000 draws, seed 20261013.

**Reconstruction bases:**
- carry-forward from detection-event starts (the data versions' rule);
- carry-forward from single reads (tau 0 and 2).

**Environment variables:**
- `MMM_AMV_OUT` (output root; refused inside the live root or a data-version root);
- `MMM_AMV_ONLY` (test runs on matching recordings);
- `MMM_AMV_RECOMPUTE_DV=1` (recompute the cached data-version metrics).

**Run, from the repository root:**
1. `Rscript Testing/audits/audit_antenna_measurement_validation.R` (about 50 min, one process);
2. `Rscript Testing/audits/report_antenna_measurement_validation.R`.

**Provenance.** `tables/provenance.csv` records the sha256 of every code file used. The run of 2026-10-13 19:44 used the code of commit c742292:
- the new LF files are byte-identical to the committed blobs;
- `Functions/antenna_log_helpers.R` was hashed as its CRLF working copy on this Windows host.

**Parity of the module with the built data versions** (444 windows per phase):
- tau 0 against noflip: crossing rate within 2.3 changes/h, dispersion identical;
- tau 2 against flip2s: within 0.7/h;
- 1-s grid against the exact frozen same_s/obs_s: within 0.002.

## Findings (summary)

1. **The log records only reads.** It has no poll, error or heartbeat rows. A gap is "no recorded detection", never observed absence.
   - Recording-wide polling outages: none in any span. The only stall (B3 CC3, 2.4 s) delivered its reads late.
   - Board-wide silences: 6.4% of board-hours. They cannot be classified.
2. **Detection is sparse.**
   - Detected share is 26% (active) and 12% (light) of time.
   - Carry-forward positions are older than 60 s for 18.5% and 47% of the time.
3. **Read competition.** A reader delivers one tag at a time, so an animal's reads drop roughly as 1/(k+1) with k cage-mates on its reader. Detection density depends on co-location.
4. **Switching.** 52% of reader changes are to the neighbouring column, 25% to the other row, 9% across two or more columns.
   - Returning visits are continuous in duration, with a 1–5-s mode and no antimode, so no flip threshold follows from the data.
   - Crosstalk between neighbouring antennas is documented by static-tag scans.
5. **Metric sensitivity.**
   - Reader-change rate and fragmentation depend strongly on tau: rate 172 → 81 /h; fragmentation ranks collapse beyond 5 s.
   - Occupancy metrics depend on the carry-forward bound, not on tau.
   - Carry-forward from reads instead of event starts reassigns 6.6–7.1% of active seconds.
6. **Co-assignment is below chance.**
   - Same-reader co-assignment lies below its time-shift expectation in 81–92% of animal-windows, even in the light phase.
   - Chance overlap explains 86–97% of its between-animal variance.
   - Raw and vendor shared_zone_use agree at ρ 0.23 (active) and 0.04 (light).
7. **CON.** The raw male CON reader-change deficit coincides with fewer reads, more shared-reader time and the board allocation (5 of 6 CON cages on the two low-read boards at CC1).
   - In the active phase it vanishes per read (ratio 1.02) and when board-matched (0.98).
   - In the light phase it remains.
8. **Hardware.** Dead antennas for whole recordings:
   - B2 CC3 board 2 R3 (in S13);
   - **B2 CC4 board 5 R3 (not in S13)**;
   - B5 CC4 board 1 R5 (in S13).

   B5 CC4 board 1 is silent after 2023-12-04 14:00, most likely because the animals were removed.
9. **Probabilistic HMM (pilot).** It identifies detection probabilities and the share of time undetectable (0.63), but not where animals are while unread. Its added value over carry-forward plus an age bound is at most 5.8 points of time per cell. It was not built.
10. **Ground truth.** None exists. Agreement between RFID reconstructions is not accuracy.

## Proposed final spatial and social specification (for decision, not applied)

| | construct | source and rule | status |
|---|---|---|---|
| S1 | thresholded displacement-event rate (Stage 29 `crossing_rate`, Stage 09 Movement) | vendor AnimalPos, unchanged; ge200 as the raw sensitivity | registered, not reopened |
| S2 | reader-change rate | raw antennas, tau 2 s reference with 0.5/1/5/10 s sensitivity; basis stated; reported with reads/h, shared-reader time and board | descriptive only, never "locomotion" |
| S3 | spatial occupancy | raw antennas at **column** resolution (4 columns); tau 2; carry-forward with the supported share at delta 60 s and a delta 60/300 s sensitivity; dead-antenna cage epochs excluded; B5 CC4 board 1 unobserved after 2023-12-04 14:00 | descriptive; reader/row level only as sensitivity |
| S4 | occupancy organisation | column dispersion and dominant-column share from S3; reader level as sensitivity | descriptive |
| S5 | fragmentation | vendor stream at the frozen 39.4 s only | registered; not ported to raw streams |
| S6 | co-location | **no proximity construct from RFID.** If described at all: column-level "antenna co-assignment" with its time-shift expectation and excess, per cage, with the read-competition bias stated | descriptive, not social |
| S7 | group comparisons | cage as the unit; board recorded and reported; CON contrasts descriptive (3 cages per sex); detection covariates alongside | — |
| S8 | validation before any fine-scale spatial or social claim | calibration on the same boards (static tags at surveyed positions with 1–4 tags present, moving tag, one filmed mouse), or tracking of the existing regroup video (`Y:/Tobi/E9_SIS/B<n>/Regroup/`, 118 of 120 cage × CC cells, first 1–2 h, daylight) | — |

## Decisions for Tobias

1. **Raw carry-forward basis.** Choose between:
   - event starts: the current data versions and Stage 19;
   - single reads: which avoid the mis-timing of overlapping events.

   This affects 6.6–7.1% of active seconds and up to 38 points of one animal's time at a reader.
2. **B2 CC4 board 5 R3.** Should it be added to the dead-antenna exclusions of new analyses? The frozen S13 list cannot change.
3. **shared_zone_use.** It is a registered primary construct. Should a limitation or erratum entry state that it is chance-dominated co-assignment and not proximity (`docs/KNOWN_LIMITATIONS.md` §14 carries the measurement facts now)?
4. **Stage 19 resolution.** Should Stage 19 maps and contrasts move to column resolution, with reader level as a supplement?
5. **Independent validation.** Should one be commissioned (S8)?

**Follow-up (2026-10-13): Stage 00b acquisition QC** (`docs/RFID_ACQUISITION_QC.md`).
- **The three silent antennas.** They are confirmed whole-session channel dropouts: each worked in the placement scans next to the session and in the 23 other recordings. The frozen S13 list covers only OR112 and OR141 of the B2 CC3 board 2 cage, not OR106 and OR131.
- **The single-read basis.** It reassigns 8.5% of active and 4.8% of light seconds, mostly fresh border reads at corridor readers. Few R5 seconds are reassigned away, but R5 gains border time (+7.7% active, +2.6% light), so house occupancy is basis-sensitive.
- **The house end of the layout** (R5 house, R1 food, R8 water as reported by the lab) is supported by light-phase resting. The food and water rows are inferred.
- **Calibration.** A proposed protocol is in `docs/RFID_CALIBRATION_PROTOCOL.md`.
