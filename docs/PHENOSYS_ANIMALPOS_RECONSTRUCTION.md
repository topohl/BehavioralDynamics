# PhenoSys AnimalPos: reconstruction of the vendor position stream from the raw RFID reads

Status: reconstructed 2026-10-13 from the Exp9 data. No vendor documentation was used, so this is an inferred algorithm, not a documented one.

- Implementation: `Functions/animalpos_reconstruction.R`.
- Portable test: `Testing/tests/test_animalpos_reconstruction.R`.
- Real-data audit: `Testing/audits/audit_phenosys_reconstruction.R`. Its outputs are in `<Behavior>/RANT/comparison_vendor_raw/phenosys_reconstruction/`.

## Result of the audit (all 24 recordings, B1-B6 x CC1-CC4)

| | records |
|---|---|
| vendor AnimalPos export (`raw_data/B<n>/E9_SIS_B<n>_CC<k>_AnimalPos.csv`) | 658,901 |
| reconstructed from the raw reads | 658,489 |
| identical (same animal, time to the millisecond, x and y at 0.1) | 658,424 (99.93% of vendor records) |
| vendor only / reconstruction only | 477 / 65 |

**Recordings.** 23 of 24 recordings match record for record. All 542 unmatched records are in B1 CC1.

**Cause of the B1 CC1 mismatches: the hour repeated when daylight saving ended.**
- They lie between 02:00:16 and 03:18:08 on 2022-10-30, the hour the logger clock repeated, plus about 18 minutes in which the position state resynchronises.
- They are spread over 12 animals in 5 cages.
- They lie at least 34 h after the first read of the recording and at least 57 h before its last read.
- None has a counterpart at the same coordinates within 1 ms, so they are different records, not timing rounding.
- The pipeline's reader drops the repeated hour: `rfid_read_raw_log()` removes 8,097 events. The vendor processed both passes, interleaved by wall clock.
- With those events kept (`drop_clock_fold = FALSE`) and read by wall-clock time, B1 CC1 matches 20,703 of 20,703.
- In this mode five events span the clock change and list a gap of about -1 h inside the event. Their read times follow the logged wall clock (`apr_reads(allow_negative_gaps = TRUE)`, diagnostics only).

**Conclusion.** Given the vendor's handling of the repeated hour, the reconstruction reproduces every vendor record of all 24 recordings exactly. It also implies that the vendor computed AnimalPos after the recording, from the time-sorted log, not online.

**Effect on analyses.** The repeated hour lies in the second dark phase of B1 CC1, so the Stage 09 window (the first dark phase after CC1) is unaffected.

## Algorithm

### Reads

Each raw event row (`MultiMouseMonitor-*.csv`, UTF-16LE, `;`-separated) holds one tag's consecutive reads at one antenna.
- `DateTime` is the event start, as an Excel serial day read as UTC: `(serial - 25569) * 86400` s.
- `MsgValue1` is the comma-separated list of gaps between the event's reads in milliseconds. The first gap is 0.
- Read *k* lies at `round(start_s * 1000) + gap_1 + ... + gap_k` ms. All times are integer milliseconds.
- In all 24 recordings the number of gaps equals `senseRFIDrecords`, and the gaps of an event sum to its `eventDuration`.

Each read takes the grid coordinate of its antenna from the `#ID-Device` rows: x = 0, 100, 200 or 300 and y = 0 or 116 (grid units). Readers are 100 units apart along the cage and 116 apart across it.

**Per tag:**
- reads on every board are pooled (the vendor does not restrict a tag to one board);
- reads are ordered by time, ties by the event's start, then the event's row in the log, then their order within the event;
- tags missing from the vendor export get no records.

### Window alignment and evaluation

Let *s* be the oldest read still held. When read *j* arrives:

1. **Trigger.** If `t_j - t_s > 500 ms` (strictly), the window of reads *s* .. *j*-1 is evaluated. These are all held reads, i.e. every read with `t` in `[t_s, t_s + 500 ms]`. A read exactly 500 ms after `t_s` belongs to the window and does not trigger it.
2. **Averaging.** The window's position is the unweighted arithmetic mean of the x and y coordinates of its reads. Every read counts once, whatever its event, and the mean is not rounded. Reads at two or more antennas therefore give positions between antennas.
3. **Timestamp.** The evaluation is stamped `t_s`, the time of the window's oldest read. It is never the event end or the arrival time.
4. **Purge.** Every read with `t < t_j - 500 ms` (strictly) is dropped, and *s* moves to the first read kept.

Only one window is evaluated per arriving read. Windows that close at the same arrival are dropped without evaluation. Without a new read nothing is evaluated: a gap in the reads produces no position, and the tag's last window is never evaluated.

### Threshold comparison (the displacement gate)

**Euclidean distance.** An evaluation is written when the Euclidean distance between its mean and the last written position is at least 200 grid units. The comparison is inclusive: exactly 200 is written. The written mean becomes the new reference.

**Identifiability.** The data identify the threshold only within (198, 200].
- Every threshold from 199 to 200.00 reproduces the vendor exactly.
- A threshold strictly above 200 falls to about 0.6 recall, because exact 200-unit moves are about 32% of all vendor moves.
- The data cannot tell whether the decision uses the unrounded mean or the mean rounded to 0.1, because both are exact. The implementation uses the unrounded mean with a tolerance of 1e-6.

### State initialisation and end

**Start.** The reference starts at the origin (0, 0). The first evaluation of each tag is always written:
- its mean, if that lies at least 200 from the origin;
- otherwise the origin itself, which then stays the reference.

This start placeholder is the first record of 309 of 448 animal-recordings. In 246 of these the animal was not read at (0, 0).

**End.** A closing record at the tag's last read repeats the last written position. It is not a move.

### Coordinate handling

- **Export.** The export shows x and y rounded to 0.1.
- **Off-grid records.** 106,368 of 658,901 records (16.1%) are off-grid: the mean of reads at two or more antennas.
  - Same row, adjacent columns: 72% (e.g. 250, 266.7, 33.3).
  - Same column, both rows: 19% (y 38.7, 58, 77.3).
  - Both rows, adjacent columns: 7%.
  - Others: 2%.
  - A mean can also land on an antenna that no read in the window came from (about 0.3% of records), e.g. reads at x 100 and 300 average to 200.
- **Floor-snap in preprocessing.** The project's preprocessing (`animalpos_position_id()`) floor-snaps exported coordinates to readers 1-8. Every off-grid y goes to the y = 0 row (R1-R4), and an off-grid x goes to the lower column. The snapped reader equals the window's majority antenna in only about 45% of off-grid records.
- **Consequences.** Vendor reader identities are biased toward row 0 and lower columns. Vendor data cannot show a row change within a column (116 < 200).

### Evidence that the details matter

The alternatives below were tested in pooled recordings (4 recordings, 1 s tolerance) or in the independent verification (3 further recordings, exact).

| variant | recall | precision |
|---|---|---|
| this rule | exact | exact |
| trigger >= 500 ms instead of > 500 ms | 0.9998-0.99995 | |
| purge <= instead of < | 0.999 | |
| window 450 or 550 ms instead of 500 ms | 0.94 / 0.89 | |
| strict > 200 | 0.60 | 0.72 |
| distance from the last evaluated mean instead of the last written position | 0.37-0.50 | 0.77-0.81 |
| column difference >= 2 on the means | 0.93 | 0.93 |
| Manhattan >= 200 | 0.74 | 0.43 |
| no origin placeholder | 0.89 | 1.00 |
| sliding window (every read evaluated) | 0.78 | 0.86 |
| one detection per event, no averaging, >= 200 | 0.68 | 0.57 |
| stamp at the event end | 0.45 | 0.41 |

## The variant without the gate (data version avg500)

`apr_positions(gate = "none")` keeps the estimator unchanged:
- same reads;
- same window alignment;
- same averaging;
- same timestamps (window start).

It removes only the 200-unit gate.

**Emission.** Without a gate, each evaluation is an estimate. To keep a change-only stream that is not driven by the read rate, a record is written when the floor-snapped reader cell of the mean changes (the first evaluation is always written).
- The cell is the same `animalpos_position_id()` of the 0.1-rounded mean that the vendor arm's preprocessing applies.
- Rounding to 0.1 never crosses a cell boundary for window means, which would need more than 2,000 reads in one window.
- Cells are required because Stage 01 counts changes of reader cells.
- There is no origin placeholder and no closing record. Both exist only because of the gate.

**Diagnostic counts.** The number of evaluations and the number of changes of the unsnapped mean are kept as separate diagnostics. They measure other quantities than cell changes and are reported separately.

Build: `antenna_dv_build_recording_window()` in `Functions/antenna_data_version.R`, root RANA.
- The cell held at 18:30 is the seed. Its window age is `seed_emission_age_s`.
- `seed_detection_age_s` (the age of the last on-board detection) is defined exactly as in the other raw versions.
