# Proposed RFID calibration protocol (PhenoSys MultiMouseMonitor, Exp9 set-up)

> **Calibration will not be performed (Tobias, 2026-10-15).** This document is kept unchanged below as a historical record.
> Nothing is booked, prepared or scheduled, no experiment is required, and nothing here is a pending task or a prerequisite
> for the current pipeline status. The pinned schedules, criteria, report mappings and code in `docs/rfid_calibration/`
> stay unchanged for provenance. The questions it was meant to answer remain accepted limitations:
> `docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.5.md` (sections 0, 5).

**Status: proposal only (2026-10-13).** Nothing has been started. It requires access to the same boards (AM1-AM5), the
same PhenoSoft version (1.0.8147.28429) and the configuration used in Exp9. It uses no animals except, optionally, step 6.

**2026-10-14:** the minimal two-day pilot built from steps 1-5, with its endpoints, the precision of each duration and a
resource estimate, is `docs/RFID_CALIBRATION_PILOT.md`. Nothing is booked.

## Why

No independent ground truth exists for the Exp9 RFID positions; see `docs/ANTENNA_MEASUREMENT_VALIDATION.md`.

**What the raw logs cannot tell:**
- where a tag is while it is not read (about 63% of dark-phase time in the HMM pilot cage);
- whether a short A→B→A visit is crosstalk or movement;
- whether low read rates at a reader come from antenna sensitivity, shielding by the house or bedding, or read competition between tags;
- what "assigned to the same reader" means in centimetres.

**What the acquisition QC (Stage 00b) can establish:**
- whole-session channel dropouts;
- stable board-level sensitivity differences (boards 3 and 4 read less and cross-read less in every recording).

It cannot turn those differences into localization error. This calibration measures detection and localization independently of any biological outcome.

## Fixed conditions

Run every step with:
- the **production configuration** (antenna pause 1000, length 10, reader phase1 96, phase2 69, phaseDuration 250 — values from `Raw Data/Behavior/RFID/sis_phenosys/BATCH1_SIS_5cages.xlsx`; the per-batch SIS workbooks should be retrieved from the PhenoSys PC and archived first);
- the **same cage housing, bedding depth and red house, the food hopper and the water bottle** placed as in the home-cage recordings.

Record the physical orientation of the housing on the plate with a photo that shows the antenna plate edge and the resources. This also settles R1 = food, R5 = house, R8 = water, which no record documents today.

Use **spare implantable tags of the type used in Exp9**, mounted on small non-metallic carriers:
- a 1-cm foam cube for floor level;
- a 3-cm block for "climbing" height.

Mark three orientations on each tag: long axis parallel to x, parallel to y, and vertical.

Log a manual time stamp on the PhenoSys PC clock at the start and end of every placement. The log has no event markers, so the protocol sheet is the only time reference.

## Steps

| step | what | design | measures |
|---|---|---|---|
| 1 | Static single tag, surveyed grid | One tag at each antenna centre (8), each midpoint between neighbouring antennas (10: row neighbours, column partners and diagonals), each plate corner and edge midpoint. 3 orientations x 2 heights, 2 min per placement, on 2 boards (one of 1/2/5 and one of 3/4) | Read rate per antenna (reads/s), share of polls read (from the gap lattice), crosstalk to neighbours, the antenna the event-start and single-read reconstructions assign. Gives detection and crosstalk maps per antenna and orientation, the localization confusion matrix at known positions, and the board 3/4 sensitivity difference directly |
| 2 | Resource locations | Tag inside the house on the bedding (centre, corners, against the walls), on top of the house, in the hopper basket, at the spout, 2 min each, 3 orientations | Whether the house, the hopper metal and the bottle shield or detune the field. Tests directly whether low individual reads at R5 can be shielding rather than absence or competition |
| 3 | Read competition | 1, 2, 3 and 4 tags together (a) at one antenna centre, (b) inside the house, (c) split over two neighbouring antennas; 3 min each, orientations mixed, repeated with the order of tags rotated | Reads per tag per second against the number of tags; whether reads alternate between tags or collide; whether a read tag "hides" the others. Calibrates the 1/(k+1) read-rate fall observed in the home cage, which so far rests on inferred co-location |
| 4 | Boundaries and movement | One tag moved on a rail or by hand along straight paths at about 5, 15 and 30 cm/s: along each row, across each column, diagonally, with paced pauses of 1, 5 and 20 s at antenna centres and midpoints. Video from above with a visible clock (or an LED driven from the PC) for alignment | Latency of the first read after entry, duration of reads after exit (the ~1-s event-closing rule), A→B→A flicker while static at a midpoint versus during movement. Gives a physical basis for the flip-filter threshold (tau) and the carry-forward bound (delta), which today are only a sensitivity range |
| 5 | Known-empty control | All tags removed from the plate for 1 h with the software running (as already happened after removal in B5 and B6) | Phantom reads: none were seen in the existing empty intervals; repeat on all 5 boards |
| 6 (optional, animal) | One tagged mouse filmed | Only if the lab decides it is justified: one animal from the colony (no new experiment), top-down video at a known clock offset, 2 h light + 2 h dark with infrared | Ground-truth occupancy of a single animal (identity is trivial); support-age and carry-forward error under real behaviour, including time in the house |

## Analysis

The analysis should use the Stage 00b functions (`Functions/antenna_acquisition_qc.R`) on the calibration logs, with the placement sheet as truth. Report per board and antenna:
- the probability of at least one read per second by position, height and orientation;
- the crosstalk matrix;
- the confusion matrix of the event-start and single-read reconstructions (with the 2-s flip filter) against the true position;
- the read-rate ratio with 2-4 co-located tags;
- the latency and tail of reads around crossings.

**Where the results would go:**
- They would replace the provisional thresholds of `aqc_rules()` (co-read ratio, silent-stretch expectation) with calibrated ones, and turn the tau and delta ranges into justified values.
- They would decide whether a probabilistic location model is worth building. Its emission model needs exactly these detection and crosstalk probabilities, and the probabilistic-model pilot found them, and the location of unread animals, otherwise unidentifiable.

## Limitations

- **Static and phantom tags.** They differ from implanted tags in living animals: body water, posture and movement change coupling. Step 6 is the only check under real behaviour.
- **Bedding and soiling.** Their condition changes over days. Calibrate with fresh bedding and with bedding from a used cage if available.
- **Time alignment.** The software has no event markers, so alignment relies on the protocol sheet (about 1 s) or on video with a visible clock.
- **Boards and antennas tested.** Two boards cover the hardware split; per-antenna differences beyond those boards remain uncalibrated.
- **What the calibration cannot change.**
  - It cannot recover positions in the Exp9 data while animals were unread; it bounds the error of the reconstruction rules.
  - It does not make same-reader co-assignment a measure of physical proximity. It can only say how far apart two tags read at one antenna can be.
