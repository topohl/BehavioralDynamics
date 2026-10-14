# RFID calibration pilot (v1.1, 2026-10-14)

**Decision of 2026-10-14: start with the one-day pilot, subject to hardware availability.** Nothing is booked, no tag is
placed and no animal is involved. The pilot estimates location-classification errors, missed reads and detection competition
on the Exp9 hardware. Its decision criteria, the seeded schedule and the analysis code are fixed in the repository before any
measurement (hash-pinned); criteria may not be changed after data are seen. After the day, a pre-specified rule recommends
whether a second rack day would materially change a conclusion; the full two-day programme is not commissioned
automatically.

Static tags and tags moved on a sled test the hardware and the reconstruction rules. They cannot establish the exact
movement or the physical proximity of freely behaving mice. Calibration with static tags bounds bench hardware behaviour
only; its results enter the Exp9 analysis as priors, parameter choices and sensitivity analyses, never as bounds on true
physical occupancy or as validated locations. No group label, CombZ or outcome is involved.

| part | where |
|---|---|
| one-day protocol (this document, section 1) | counterbalanced board order, seeded placement order, repeat reference placements |
| seeded schedule | `docs/rfid_calibration/schedule_one_day_v1.csv` (generator `make_schedule_one_day_v1.R`, seed 20261014) |
| decision criteria | section 2; machine-readable `docs/rfid_calibration/acp_criteria_v1.0.csv` |
| day-2 rule | section 3 |
| pre-registered analysis | `Functions/antenna_calibration_pilot.R`, `Testing/audits/analyse_rfid_calibration_pilot.R`, `Testing/tests/test_antenna_calibration_pilot.R` (section 4) |
| full two-day programme (reference; day 2 only under the day-2 rule) | section 5 |

**Code status (2026-10-14).** The analysis is written and tested on synthetic logs only; nothing has been measured.
- `Functions/antenna_calibration_pilot.R` (prefix `acp_`) holds pure definitions and reuses the reconstruction,
  measurement and QC functions unchanged. Every threshold is read from the criteria file. `acp_criteria()` and
  `acp_read_schedule()` stop unless the content sha256 (line endings normalised to LF) equals the pinned value
  (`ACP_CRITERIA_SHA256` = `c934354377d30c2f413d39b8504ee07f35cec132b47741d5a66f9c6fc2780446`, `ACP_SCHEDULE_SHA256` =
  `2855780e2aa5cc7f04049eeb5b2868d9ba7920369218f1bd3ae105d0c30b549e`; the schedule file hash in section 1 is of its CRLF bytes).
- `Testing/audits/analyse_rfid_calibration_pilot.R --date=<YYYYMMDD>` is the main-guarded runner. It reads the day's
  folder read-only and writes only `<Behavior>/RANM/calibration/<run_id>/` (built in `.tmp_<run_id>`; an existing folder
  is refused). A release run needs committed code, the default raw folder and the pinned Stage 00b board history of
  `raqc_20261014_6165adf`, which is the only Exp9 input and is group-blind.
- `Testing/tests/test_antenna_calibration_pilot.R` covers T1-T16 of the analysis plan (section 4) and runs one synthetic
  pilot day on the pinned schedule end to end, with expected values worked out by hand.
- The criteria are fixed from the commit that adds them. A change before the pilot needs a new version
  (`acp_criteria_v1.1`) with its reason; no change is allowed after data are seen.

## 1. One-day pilot protocol (v2)

**Status.** This revises the minimal one-day version in `docs/RFID_CALIBRATION_PILOT.md` (section Resources). Tobias approved that version in principle, subject to hardware availability.
- Nothing is booked, no tag has been placed and no animal is involved.
- It is group-blind: no Exp9 animal, CON/RES/SUS label, CombZ or outcome is used.
- Static tags and tags moved on a sled test the hardware and the reconstruction rules. They cannot show how freely behaving mice move or how close they are to each other.

**Schedule.**
- Generator: `docs/rfid_calibration/make_schedule_one_day_v1.R`.
- Seed: **20261014**, with `RNGkind("Mersenne-Twister", "Inversion", "Rejection")`, R 4.5.1.
- Output: `docs/rfid_calibration/schedule_one_day_v1.csv`, 106 rows, sha256 `7b7fda397638e69b1866befffb159c741e1b6d6ed69654911bc0928c713328dd` (file as written by R on Windows). A rerun gave the same hash.

### 0. Changes from the minimal version
- **Same programme on both boards.** The minimal version gave board 4 no movement, no split or board test and fewer competition steps. Now every board comparison is paired, placement by placement.
- **Counterbalanced board order.** The fixed order "board 2, break, board 4" is replaced by ABBA by halves inside one PhenoSoft session. Board is therefore no longer confounded with order, time of day or session.
- **REF (repeat) placements.** There are 3 per board, spread over the day. They replace the bridge block, which the minimal version dropped.
- **Boundaries refocused on the house end.** This is where the Exp9 bases disagree most: R6→R5 is 29% of the reassigned active seconds, R2→R1 14% and R1→R5 11%.
  - Quarter points with a unique true reader on R5|R6, R1|R2 and R1|R5.
  - The 5-min R5|R6 static flicker.
  - The within-group R2|R3 midpoint.
  - Dropped: the junction, and the centres R3 and R7 (x and z) and R4 (z).
- **Competition tags all in z orientation.** The mixed x, z, y, x orientations would confound read competition with orientation coupling; the static placements measure orientation separately.
  - The n = 2 step is dropped.
  - Two sentinel tags present during the competition run replace the separate board/group-test (C3) runs.
- **House on production bedding.** The house sits on bedding in a well under its footprint. The roof is added.
- **Duplicated set-up.** Two identical spacer boards, and two cameras with clock displays mounted all day.
- **Duration: 245.7 min on site (4.1 h)** instead of about 226 min. The extra ~20 min pays for the counterbalancing (board switches, house moves) and the REF repeats.

### 1. Design against confounding (fixed before booking)
1. **One session for both boards.**
   - PhenoSoft is started once. The tested boards (2 = reference, 4 = least sensitive) share session start, clock and configuration.
   - Boards 1 and 3 record all day as known-empty controls. Board 5 also does, except for the clapper.
   - Substitutes, unchanged from v1: board 3 for board 4, board 1 for board 2.
2. **ABBA by halves.**
   - The static (P), movement (M) and house-in/out (H) programmes are each split into halves by a seeded split, stratified by class.
   - P and H: half 1 runs board 2 then board 4, half 2 board 4 then board 2 (ABBA). M is counterbalanced the other way round: M1 runs board 4 first (V2, 102.5 min) and board 2 second (V3, 127.8 min); M2 runs board 2 first (V3, 133.1 min) and board 4 second (V4, 161.8 min). The schedule file is correct; the design comment of the pinned generator (make_schedule_one_day_v1.R, line 16) states the P/H order for every programme and is left unchanged.
   - Visit sequence:
     - V1 b2: REF, T0, P1
     - V2 b4: REF, P1, P2, M1, REF
     - V3 b2: REF, P2, M1, M2, H1
     - break
     - V4 b4: M2, H1, C, H2, REF
     - V5 b2: C, H2, REF
3. **The one block that is not counterbalanced: the competition run C.** It is one continuous run per board, board 4 at 175-189 min and board 2 at 199-213 min (fixed, 23 min apart). Each run is bracketed by its own sentinel baselines (C0, CE); the nearest REF placements bracket it only loosely (board 4: 107 and 194 min; board 2: 111 and 218 min), and the last REF of each board is taken with bedding in the R5 well. Its board comparison is reported with this caveat.
4. **Randomised order.** Each of the following is an independent seeded permutation:
   - the placement order within every board-block;
   - the order of the house-on and house-off sets;
   - the T0 tag order;
   - the dwell order of the short visits.
5. **REF placements.**
   - REF-z: T1 at the R6 centre in z orientation, 40 s analysed.
   - REF-x: T1 at the R5|R6 quarter point on the R6 side, (75,116), in x orientation, 60 s analysed.
   - Times: board 2 at 45, 111 and 218 min; board 4 at 70, 107 and 194 min (mean 125.7 vs 124.9 min).
   - They give repeatability within a board, drift, the tolerance for the board ratio, and a pre-specified drift-adjusted board ratio.
6. **Identical everything else.**
   - Tags: T1 for every single-tag placement on both boards. T1-T4 sit in the same slots, and T5/T6 are the same sentinels, on both boards. Each labelled carrier travels with its tag.
   - People: one operator places every tag; the recorder handles sheet, clapper and cameras.
   - House: one physical house, moved between the cages and set on a traced outline.
   - Bedding: one batch, in identical pre-weighed portions.
   - Spacer boards: two, cut from one sheet, with the same printed grid.
   - Cameras: identical mounts on both cages all day.
   - Lid, food hopper and filled bottle: in production position, and closed during every analysed window.
7. **Time-of-day balance.** Planned mean clock minute, board 2 vs board 4:

   | programme | board 2 | board 4 |
   |---|---|---|
   | P (static) | 92.4 | 87.6 |
   | M (movement) | 132.3 | 134.0 |
   | H (house) | 166.5 | 177.8 |
   | REF | 125.7 | 124.9 |
   | C (competition; fixed order) | 205.4 | 181.9 |

   Over the 21 paired static placements (P and H), board-4 time minus board-2 time is −0.3 min on average (SD 24.7, range −33.6 to +28.7). Board 2 comes first in 11 of the 21 pairs.
8. **Not removable.** Each board has its own cage and rack position, so board and cage cannot be separated. The same cage model and production items limit this.

### 2. Fixed conditions
**Unchanged from `docs/RFID_CALIBRATION_PILOT.md` section 2:**
- PhenoSoft 1.0.8147.28429.
- The production workbook: archived with sha256 and diffed against BATCH1.
- Per antenna: pause 1000, length 10, onlyValid false.
- Per board: phase1 96, phase2 69, phaseDuration 250.
- One session start; not a DST-change day; no manual change of the PC clock; no animals on the rack.

**Reader map.** Always use channel numbers at the bench.

| reader | channel | phase group | resource |
|---|---|---|---|
| R1 | 1 | B | food |
| R2 | 3 | A | |
| R3 | 5 | A | |
| R4 | 7 | B | |
| R5 | 2 | A | house |
| R6 | 4 | B | |
| R7 | 6 | B | |
| R8 | 8 | A | water |

**Orientation.**
- x: tag long axis along the rows.
- y: along the columns.
- z: vertical.

Tags sit in 1-cm foam cubes, so the tag centre is 0.5 cm above the surface.

**Heights.**
- floor: on the spacer board. Its top is at the measured production bedding depth.
- bedding: on tamped production bedding in the house well, at the same nominal height.
- roof: on the house roof, at the measured height. Its house-off control is a foam stack of the same height.

### 3. Materials and preparation (about 5 person-hours)
**Tags.** 8 of the Exp9 type: T1-T6, the clapper C and one spare, with their IDs in a tag table. The minimum is 6 (T1-T4, C, spare). With 6:
- the sentinels and rows C0 and CE are dropped (−6.6 min in total);
- board/group-level competition then remains unidentified.

**Two spacer boards.** Rigid foam as thick as the production bedding depth. Each carries:
- a laminated grid with 1-cm squares; centres, quarter points and midpoints marked with grid coordinates, reader and channel;
- the traced house outline, including its entrance direction;
- a well under the house footprint, with a fitting foam insert;
- registration edges for a removable acrylic REF frame, with cut-outs at the R6 centre and at (75,116).

**Bedding.** Two pre-weighed portions of fresh production bedding from one batch, each filling the well level when tamped.

**House and carriers.**
- One red house, of the production model.
- A foam stack of roof height.
- Labelled 1-cm cubes with x/y/z slits.
- Plastic tweezers and non-metallic putty.

**Movement.**
- A plastic rail strip.
- A low plastic sled.
- Nylon string led through the lid bars, so the lid stays closed.
- A metronome app.

**Video.**
- Two top-down cameras (≥ 30 fps, 60 preferred), each with a millisecond clock display in frame.
- The offset between each display and the PhenoSys PC clock is filmed at the start, the break and the end. A PC-driven LED is an alternative.

**Sheets.** Pre-printed from `schedule.csv`.

### 4. Before booking (desk work)
1. **Pre-register.** Commit the schedule and its generator, the criteria file (thresholds; sha256 pinned in a test), and the analysis code with passing synthetic-log tests.
2. **Ask PhenoSys:**
   - what phase1, phase2, phaseDuration and length mean;
   - the antenna loop size;
   - whether the boards are polled independently.
3. **Confirm:**
   - ≥ 8 spare tags;
   - cage, floor, bedding type and production depth;
   - the house model and its outline over R5 (from a production photo);
   - the roof height;
   - the rack positions of the b2 and b4 cages (handling one cage must keep metal ≥ 30 cm from the other).
4. **Book** one animal-free rack day, with an alternative date.

### 5. Session timeline (minutes)
| clock (min) | step | board | min |
|---|---|---|---|
| 0-20 | Set-up: cages on boards 1-5; spacer boards with well inserts in b2 and b4; cameras; photos; start PhenoSoft once; archive the workbook | all | 20 |
| 20-30 | Empty baseline | all | 10 |
| 30-35 | Channel check: T1 for 5 s on each antenna of b2 and b4; clapper on b5 channel 7 | 2, 4, 5 | 5 |
| 35-45 | Survey: T1 (z) slid ~1 cm/s along both rows and the 4 columns, on video; grid offset check | 2, 4 | 10 |
| 45-70 | V1: REF; T0 (T2, T4, T5, T6, T3); P1 (7 placements, including the 5-min flicker); switch | 2 | 24.9 |
| 70-111 | V2: REF; P1 (7); P2 (8); M1; REF; switch | 4 | 41.0 |
| 111-146 | V3: REF; P2 (8); M1; M2; bedding into the well; H1 | 2 | 35.1 |
| 146-161 | Break: no tags anywhere, house out | | 15 |
| 161-199 | V4: M2; bedding into the well; H1; C run; H2; REF; switch (house to b2) | 4 | 37.7 |
| 199-221 | V5: C run; H2; REF | 2 | 22.0 |
| 221-231 | Empty baseline | all | 10 |
| 231-246 | Stop the session; copy everything; sha256 manifest; teardown | | 15 |
| **total** | | | **245.7 min (4.1 h)** |

**Time under test.**
- Board 2: 81.0 min of slot time, 58.1 min recorded (T0 included).
- Board 4: 76.1 min of slot time, 54.0 min recorded.

**Known-empty time.** About 14.4 board-hours:
- boards 1 and 3 all session (7.0);
- board 5 except tag C (3.5);
- b2 and b4 while idle (3.9).

**People.** Two on site for 4.1 h (8.2 person-hours).

### 6. Blocks
**Static placement timing.** 70 s recorded, 60 s analysed (5-s guards), 30 s handling.

**REF** (pair IDs REFz, REFx). REF-z first, then REF-x, in the frame. Recorded 50 and 70 s, handling 25 s each. Never with the house in the cage.

**T0.** Board 2 only. T2-T6 at the REF-z spot, 40 s analysed each; T1's value is the V1 REF-z placement.

**P** (house absent; same placements on both boards; the half is assigned by seed). Midpoints are in walking orientation.

| pair | position (x, y) | orientation | half |
|---|---|---|---|
| P01 | R1 centre (0,0) | z | 1 |
| P02 | R2 centre (100,0) | z | 2 |
| P03 | R8 centre (300,116) | z | 2 |
| P04 | R1 centre | x | 1 |
| P05 | R2 centre | x | 1 |
| P06 | R4 centre (300,0) | x | 1 |
| P07 | R6 centre (100,116) | x | 2 |
| P08 | R8 centre | x | 2 |
| P09 | R5\|R6 quarter, R5 side (25,116) | x | 1 |
| P10 | R1\|R2 quarter, R1 side (25,0) | x | 2 |
| P11 | R1\|R2 quarter, R2 side (75,0) | x | 2 |
| P12 | R1\|R5 quarter, R1 side (0,29) | y | 1 |
| P13 | R1\|R5 quarter, R5 side (0,87) | y | 2 |
| P14 | R5\|R6 midpoint (50,116): **310 s recorded, 300 s analysed**, static flicker, cross-group | x | 1 |
| P15 | R2\|R3 midpoint (150,0), within-group | x | 2 |

Together with REF and H, these give centres for R1, R2, R5, R6 and R8 in z, and for R1, R2, R4, R5, R6 and R8 in x.

**H** (production bedding in the well; house on its outline).

| pair (ON / OFF) | position | orientation | half |
|---|---|---|---|
| H01 / H02 (HCx) | R5 centre inside the house | x | 1 |
| H03 / H04 (HCz) | R5 centre | z | 2 |
| H05 / H06 (roof) | on the roof; off = foam stack | x | 1 |

- Half 1 runs house on, lift (bedding untouched), house off.
- Half 2 runs lift, house off, place house, house on.
- Within each state the order is seeded.

**C** (one continuous run per board; a clapper touch at every change; 5-s guards).
1. **C0** (70 s): sentinels T5 at the R8 centre (group A) and T6 at the R4 centre (group B), z.
2. **C1** (130 s): + T1 in the house, slot 1 at (−10,126) around the R5 centre, z.
3. **C4** (310 s): + T2, T3, T4 in slots (10,126), (−10,106) and (10,106), all z.
4. **CS** (160 s): T3 and T4 moved to the R6 centre, slots (90,116) and (110,116); T1 and T2 stay in the house.
5. **CE** (70 s): T1-T4 removed, sentinels alone. Then the sentinels are removed.

The house is on throughout.

**M** (upper row, house absent; rail laid for the run only; lid closed; string pulled from ≥ 30 cm; T1 in x).
- **Passes:** R5→R8→R5, twice: 4 passes per half, with a 5-s pause at each centre.
  - M1 at about 5 cm/s; M2 at about 20 cm/s.
  - That gives 12 crossings per half: R5|R6 and R7|R8 (cross-group), R6|R7 (within-group).
- **Short visits:** 6 per half, R5→R6→R5, with 10 s at R5 between visits. Seeded dwell orders (s):
  - b4 M1: 1,2,5,1,5,2
  - b2 M1: 5,2,1,5,1,2
  - b2 M2: 1,2,1,2,5,5
  - b4 M2: 5,1,1,2,2,5
- **Per board:** 24 crossings and 12 visits (4 per dwell).

### 7. Bench rules, stop rules and the drop order
**During every analysed window:**
- hands and any metal stay ≥ 30 cm from the cage;
- the lid is closed;
- idle tags are in a closed box ≥ 1 m away.

**Clapper.** The recorder touches tag C to board 5, channel 7 for about 2 s at every placement start and end, and twice at every block start and every board switch. The recorder also notes the PC-clock time of each touch.

**Stop rules:**
1. **Channel check fails** (an antenna silent for 5 s with T1 on it). Restart the session once, before any placement, and repeat the check. If it still fails, run the schedule on the substitute board (3 for 4, 1 for 2) and record a deviation.
2. **Tag QC (T0):**
   - a tag outside ±20% of the tag median is not used in C (replace it with the spare);
   - if T1 is outside ±10%, swap T1 for the tag nearest the median and repeat the V1 REF.
3. **REF-z on board 2 below 1.5 reads/s.** Check plate seating and cage position.
4. **Disturbance** (hands or metal too close, lid open, carrier displaced). Repeat the placement at the end of the same board-block under a new ID with suffix r. Never overwrite a placement. The paired placement on the other board is not repeated.
5. **Unplanned session restart.** Log the time, repeat the channel check and continue. The analysis then stratifies by session.

**Drop order if the day is > 20 min behind at the break** (fixed in advance; always applied to both boards):
1. the roof pairs H05/H06 (−6.7 min);
2. CS shortened to 90 s analysed (−2 min);
3. 4 visits per half instead of 6 (−1.3 min).

Never drop a block on one board only.

### 8. Records
**Placement sheet**, one row per executed placement:
- order, visit, block, board, pair_id, placement_id;
- position, x, y, true reader, orientation, height;
- object, house state, well state, lid closed;
- tags with slots and carrier IDs, n_tags;
- planned seconds, PC-clock start and end, clapper OK;
- operator, deviation.

**Video annotation:**
- the PC-clock time at which the carrier centre crosses each midpoint line;
- arrival and departure at the centres;
- the survey sweeps.

**Storage.**
- Raw data: `Raw Data/Behavior/RFID/calibration/<YYYYMMDD>/` holds the log, workbook, sheets, photos and videos, with a sha256 manifest.
- Analysis output: an isolated root, `<Behavior>/RANM/calibration/<run_id>/`.

### 9. Precision at this size (planning values)
- **E1 three-class call per placement:** all 60 s read gives a lower bound of 0.94; none read gives an upper bound of 0.06.
- **E7 pooled paired log ratio:** about ±0.05 over 21 pairs at saturation (counting only); ±0.07 over 10 informative pairs.
- **E4 at n = 4 for 300 s:** about 184 reads per tag (±15%); throughput ±7%. The sentinel ratio is about ±10%.
- **E5:** 4 inside-house pairs (2 orientations × 2 boards) give about ±0.12 on the log ratio.
- **E6 latency:** 16 crossings per boundary class pooled; the median CI is the 4th to 13th ordered value.
- **E6 genuine visits:** 8 of 8 five-second visits kept gives a lower bound of 0.63.
- **Basis statistic D:** about ±0.06 if the unit-level SD is 0.2.
- **E8:** 0 phantom events in 14.4 board-hours gives ≤ 0.21 events per board-hour.

### 10. What the day answers and what it does not
**It answers, paired on both boards:**
- detection and localisation at 11 centre × orientation combinations and 7 boundary positions;
- event-start against single-read reconstruction at static boundaries, crossings and short visits;
- the board 4/board 2 sensitivity ratio, with repeatability;
- house shielding and the roof;
- competition in the house (z tags), the 2 + 2 split and board-level load (sentinels);
- tau, delta, crosstalk by phase pairing, phantom reads and cadence transfer.

**It drops (candidates for a second day only under the day-2 rule):**
- y orientation at centres; 3-cm height;
- hopper and bottle;
- open-field and mixed-orientation competition; the second rotation;
- column passes; R3/R7 centres; the junction;
- a second session (day-to-day repeatability).

Static and sled-moved tags calibrate bench hardware behaviour only (boards 2 and 4). They do not validate positions of freely moving implanted mice; their results enter the Exp9 analysis only as priors, parameter choices and sensitivity analyses, never as bounds on true physical occupancy.

## 2. Pre-specified decision criteria (acp_criteria_v1.0)

### Rules that govern every criterion
- **Fixed before any measurement.** The criteria are fixed before any measurement and before booking.
  - They are committed together with the seeded schedule (sha256 7b7fda39…28dd) and the analysis code.
  - Their sha256 is pinned in a test, and the analysis refuses to run with a different criteria file.
- **No changes after the data.** Criteria may not be changed after any data are seen.
  - A later change becomes a new criteria version.
  - It is reported next to the v1.0 results and never replaces them.
- **Group-blind.** No group label (CON/RES/SUS), CombZ, outcome or Exp9 animal is involved.
  - The only Exp9 inputs are group-blind acquisition references: Stage 00b `board_history.csv` and `reader_qc.csv`.
- **Bootstrap.** Every CI is a 95% percentile interval from a stratified cluster bootstrap.
  - B = 10,000, seed 20261014.
  - Strata: board × unit type.
  - Clusters: placement, pass or visit.
- **Rate ratios.** These use +0.5/T continuity on reads/s.

### (a) Single-read vs event-based reconstruction (decides the basis)

**Bases.**
- Read basis: srs_v1.1, i.e. single reads with ties resolved by `asr_resolve_ties()` (the single-read rule is unchanged from srs_v1.0).
- Event basis: Stage 19, i.e. detection-event starts.
- Both carry the position forward, both use `rfid_flip_filter()` at tau = 2 s, and both use the same observations.
- **Seeding.** Each placement or run is reconstructed only from its own reads (from the placement start on). Earlier placements therefore cannot seed it. Time before the first read is "unassigned" in both bases.

**Truth.**
- Static placements: the nearest antenna centre measured in the survey. Midpoints get the admissible set.
- Movement: the video annotation, on a 0.1-s grid. ±0.2 s around each change of the true position is excluded.

| item | definition |
|---|---|
| unit-level statistic | For unit u and basis b, c(u,b) is the time-weighted share of u's evaluated time in which b assigns the true column (any admissible column at a midpoint). Then d(u) = c(u, read) − c(u, event). |
| units (column level) | **Static boundary:** P09 (25,116), P10 (25,0), P11 (75,0), and REF-x (75,116) as the mean of its 3 replicates; 4 per board, 8 in all. **Crossings:** each midpoint crossing of the passes, from −3 to +3 s and truncated halfway to the neighbouring crossing; 24 per board in 8 passes, with the pass as cluster. **Short visits:** from 3 s before leaving R5 to 3 s after returning; 12 per board. |
| decision statistic | D_col = (mean d of static units + mean d of crossings + mean d of visits) / 3, pooled over both boards. The equal weights stand for three Exp9 situations: resting near a border, a crossing and a brief excursion. Also reported per board: D_col(b2) and D_col(b4). |
| non-inferiority check at centres | D_ctr = mean over the centre placements (P01-P08, REF-z, H02, H04) of the paired difference in correct-reader share. |
| margin | m = 0.05. In Exp9 the bases disagree in 15.8% of active seconds within 2 s of a read, so this is about a third of that. Non-inferiority margin: 0.02. |
| **READ favoured** | D_col ≥ +0.05, **and** the lower CI bound > 0, **and** D_col(b2) > 0 and D_col(b4) > 0, **and** the lower CI bound of D_ctr ≥ −0.02. |
| **EVENT favoured** | D_col ≤ −0.05, **and** the upper CI bound < 0, **and** both board estimates < 0, **and** the upper CI bound of D_ctr ≤ +0.02. |
| **TIE: equivalent** | The 95% CI of D_col lies inside (−0.05, +0.05). Both bases are retained and reported side by side as sensitivity analyses for every basis-sensitive measure (column occupancy, R5 share, reader changes); their estimates are not bounds on true physical occupancy. No accuracy claim is made for either. (The frozen feeds text of A40/A41 in acp_criteria_v1.0 is unchanged; the post-pilot report prints the corrected consequence from docs/rfid_calibration/acp_report_status_v1.0.csv.) |
| **TIE: inconclusive** | Any other result, including boards with opposite signs. Both bases are retained side by side as sensitivity analyses, as above (not bounds on true physical occupancy); single-read stays the provisional candidate and event-based the historical reference, and no basis is chosen by biological effects. (Frozen A42 feeds text unchanged; see above.) This goes to the day-2 rule (D1). |
| reader level | D_rdr is defined the same way, with P12 and P13 (unique true reader) added to the static units. It decides only which basis the reader-level sensitivity analyses use. |
| secondary, no decision | The tau = 0 pair; the competition and house seconds per tag; the comparison at the calibrated tau*. |

**What would favour each basis.**
- **Read basis:** at static boundary positions, the event basis holds a neighbour's crosstalk event until the true reader's next event start, which is the Exp9 R6→R5 / R2→R1 mechanism. The read basis meanwhile returns to the true reader within a read cycle and does not lose at crossings or visits.
- **Event basis:** single crosstalk reads survive tau = 2 in the read basis, or the read basis lags or flickers at crossings, so that the event basis is right more often in the same units.

### (b) Detection limitations the pilot can calibrate

| # | limitation | pilot data | statistic | pre-specified rule | feeds |
|---|---|---|---|---|---|
| B1 | detection footprint | P, REF, H (single tag) | P1s at the true reader per placement | reliable: P1s ≥ 0.95; border: in between; unread: P1s ≤ 0.05 | what "assigned to reader k" means physically (S3, S6 text) |
| B2 | localisation at known positions | centres, P12, P13 | correct-reader share of the assigned time (adopted basis; both bases on a tie) | **Reader level is primary only if** both of the following hold on both boards: (1) ≥ 0.95 at every tested centre placement assigned for ≥ 50% of its window; (2) correct row ≥ 0.90 at P12 and P13. **Otherwise** column level is primary. | resolution (decision 3; S3, S4) |
| B3 | missed reads, short gaps | reliable single-tag placements | Gmax = longest gap within a placement; q99 gap; share of missed polling windows (lattice k ≥ 6; `aqc_lattice`); event splits per min | **Gmax ≤ 2 s on both boards:** an age ≤ 2 s means "read as continuously as a static reliable tag" on the bench (a detection property: a recently detected position is not a validated location, and the bench does not say what causes longer Exp9 gaps). **Otherwise:** the "fresh" age class becomes the smallest of {2, 10} s above Gmax. | age classes of srs_v1.1; interpretation of the supported share |
| B4 | competition in one field | C1 vs C4 (z tags, house) | TR = throughput C4 / C1; MS = largest tag share in C4; starvation = any tag gap ≥ 60 s; CI by 10-s block bootstrap | time-sharing: TR in [0.85, 1.15] and MS ≤ 0.35. collision: TR < 0.70. capture or starvation: MS ≥ 0.5 or any starvation. Otherwise intermediate. | denominator for read-density measures; if capture, unread house time is not absence; co-assignment stays excluded (S6) |
| B5 | competition across neighbouring fields | CS vs C4 | per-tag rate ratio CS/C4 (2 expected if sharing is per antenna) | ≥ 1.5: sharing is per antenna. ≤ 1.15: sharing spans R5/R6, i.e. phase or board level. | co-location vs tracked-tag covariate (S7) |
| B6 | board/group load | sentinels T5 (R8, group A) and T6 (R4, group B) | SR = sentinel rate in C4 / in C0 + CE | SR ≤ 0.85 with upper CI < 1: board-level competition. CI inside [0.85, 1.15]: none. | the tracked-tags-per-board covariate becomes physically justified (B1 boards held 2.9 tags) |
| B7 | carry-forward age cap delta | C4 on board 4; reliable placements | delta* = the smallest of {10, 30, 60, 300} s above both the per-tag q99 gap in C4 on board 4 and Gmax | as defined; board 2 reported alongside | S3 carry-forward age caps (60/300 s): which cap is consistent with bench detection; an age cap, not a limit on occupancy (frozen B7a feeds text unchanged) |
| B8 | house shielding | pairs H01-H04 | house ratio = exp(mean log(rate on / rate off)) at R5, over HCx and HCz on both boards; pairs unread in both states are excluded | **no shielding:** ratio ≥ 0.90, lower CI ≥ 0.80, and P1s(on, HCz) ≥ 0.95 on both boards. **shielding:** ratio < 0.70 and upper CI < 0.90. Otherwise partial. | R5 read densities (QC D01, W10); bias statement for house occupancy |
| B9 | height (roof) | H05, H06, H02 | roof vs foam stack (roof material); foam stack vs HCx off (height) | classes as in B1 | whether unread house time can be roof time |
| B10 | board sensitivity | 21 static pairs (P, H) plus REF means | R_board = exp(mean log(r4 / r2)) at the true reader over informative pairs (not saturated on both boards, e ≥ 0.9; not unread on both); co-read ratio; REF repeatability (excess SD of log REF-x rates beyond counting noise; late/early ratio) | **hardware difference:** (R_board ≤ 0.90 with upper CI < 1, or co-read ratio ≤ 0.70 with upper CI < 1), **and** REF excess SD ≤ 0.10, **and** the call is unchanged after REF drift adjustment. **No difference on the bench:** R_board CI ⊂ [0.90, 1.11] and co-read CI ⊂ [0.70, 1.43]. **Otherwise** undecided. House-end readers (R1, R2, R5, R6) vs R4, R8 are descriptive. | S7 board main effect; board-specific detection and crosstalk parameters; whether the Exp9 board 3/4 signature is hardware or cage/occupancy |
| B11 | flicker → tau | static single-tag placements; M visits | share of static reader changes removed by `rfid_flip_filter(tau)`; 5-s visits kept (a run at the destination survives the filter) | **tau*** = the smallest of {0.5, 1, 2, 5, 10, 20} s that removes ≥ 0.99 on each board (≥ 50 changes per board) and keeps ≥ 7 of the 8 five-second visits; per basis. tau* > 2: flip2s leaves crosstalk, and S2 stays descriptive at column level. No tau qualifies: tau is reported as a range. | S2, S3 tau |
| B12 | entry latency and exit tail | crossings | median by boundary class × speed × board; order-statistic CI (16 pooled crossings: 4th-13th) | descriptive | timing error of reader changes |
| B13 | crosstalk | all static placements | co-read share within 1 s by geometry × phase pairing × board; share of transitions ≤ 60 ms | no threshold here; feeds a new aqc_rules version (H06 0.25, W09 0.02, W08) | QC; emission matrix |
| B14 | tag variability | T0 | rate relative to the tag median | outside ±20%: not used in C. T1 must lie within ±10%. | tag eligibility; W10 floor |
| B15 | phantom reads | known-empty board time | events of tags not placed, per board-hour | 0 events (≤ 0.21 per board-hour): reads are real tags. Any event: an events-minimum rule is added. | QC |
| B16 | transfer to Exp9 | all | board mean within-event gap and lattice k1, as z against `board_history.csv` | \|z\| > 3: transfer only with a caveat | everything above |

### (c) Uncertainties static or controlled tags cannot identify

| uncertainty | why the pilot cannot identify it |
|---|---|
| positions of freely moving mice while unread (positions are > 60 s old for 19.5% of active and 45.1% of light time) | No read means no truth. The bench only bounds how long a present tag can stay unread in a given configuration. |
| posture and continuous changes of orientation (rearing, curling, grooming) | Only x, y and z at fixed heights are tested. |
| huddles: stacked bodies, nest material, mixed heights | The 4-tag patch is all z on bedding. Real huddles may compete worse. |
| detection that depends on behaviour (speed > 20 cm/s, jumps, climbing hopper, bottle or lid; the dark phase) | The sled moves at constant height and roughly constant speed. |
| coupling of an implanted tag (tissue, body water, implant depth and site) | The carriers are foam. |
| drift over several days; how often channel dropouts occur at session start (3 in 120 board-recordings) | There is one short session. |
| soiled or moist bedding; nest building | Only fresh bedding is used. |
| boards 1, 3 and 5 | They are untested. Board 3 is inferred from board 4. |
| why a particular Exp9 animal-block was read little; same-reader co-assignment as proximity; any CON/RES/SUS difference | No calibration can address these. |

## 3. Day-2 rule (pre-specified)

The rule is fixed before the pilot, together with acp_criteria_v1.0.

**What runs, when.**
- After day 1, `acp_decide_day2()` applies the rule to the decision table.
- It returns a recommendation: the triggered criteria and the day-2 content.

**Not automatic.** Tobias decides. Day 2 is recommended only when it can change a conclusion.

### Projection test (P)

P is used by D1, D4 and D6.

**Inputs.** An undecided statistic with:
- estimate θ;
- 95% CI half-width h;
- a decision threshold c that lies inside its CI.

**When day 2 can resolve it.** Day 2 doubles the units of the triggering type. This plausibly resolves the statistic if |θ − c| > h/√2: with doubled data the CI would no longer contain c, if θ stays where it is.

**When it cannot.** If θ sits within h/√2 of the threshold, more of the same data is unlikely to decide. The result stays a tie or undecided, and no day 2 is triggered for it.

### Triggers

**D1. Basis inconclusive** (TIE: inconclusive). P holds for D_col at the threshold 0 or ±0.05.
- Day 2: both M halves again (passes and visits, both boards, ABBA).
- Plus the static units P09, P10, P11 and REF-x.

**D2. Endpoint not obtained.** For example:
- a board failed the channel check and no substitute was run;
- the camera or clock alignment failed, so E6/E2 have no movement truth;
- a C or H block is incomplete;
- the clapper and sheet times could not be aligned.

Day 2: only the missing blocks, on both boards, with the same design.

**D3. Board ratio not repeatable within tolerance.** Any of:
- the REF-x excess SD exceeds 0.10 on a board;
- late/early REF-x differs by more than 15% on a board;
- the board call changes under REF drift adjustment;
- REF-x is uninformative (saturated or unread on both boards) while the board call is "difference" or "undecided".

Day 2: a bridge block on both boards (REF ×3 per board plus 8 paired static placements, ABBA), in a new session. This also measures the session effect, which one day cannot.

**D4. Board call undecided and P holds** (R_board at 0.90, 1.0 or 1.11; or the co-read ratio at 0.70).
- Day 2: more paired static placements on both boards: y orientation at R1, R2, R5, R6, R4 and R8 centres; 3-cm height at the same centres.

**D5. tau not decidable for lack of data.** Either:
- fewer than 50 static reader changes on a board; or
- the 5-s visits cannot be evaluated because the destination is unread in x orientation (P07 has P1s ≤ 0.05).

Day 2: 5-min static flicker at the R1|R2 and R6|R7 midpoints, plus 12 visits per board with the tag in z.

D5 does not apply if flicker and genuine visits simply overlap in duration. That is a result, and tau stays a range.

**D6. Competition call undecided and P holds** (TR at 0.70, 0.85 or 1.15; MS at 0.35 or 0.5; SR at 0.85), **or** capture/starvation on only one board.
- Day 2: the C run again on both boards, with rotation b (tags in reversed slot order), to separate tag and slot from board.

### Never a reason for day 2
- A decision that has been made, even an unwelcome one.
- Any item in (c) of the criteria, which static tags cannot identify.
- Testing boards 1, 3 or 5.
- Tuning thresholds.
- A CI that misses a threshold only narrowly, if P fails.
- D7: a transfer caveat (E9 |z| > 3) is reported, but day 2 cannot fix it.

### What day 2 then contains
- Only the triggered blocks, combined.
- The day-1 design rules: one session start, both boards, ABBA by halves, seeded order (new seed 20261015, stated in advance), REF at every board visit, channel check, baselines, the same tags, house, bedding, operator and carriers.
- At most about 4 h on site.
- **Always:** a bridge of 4 day-1 pairs (P09, P11, P13, P06) plus REF on both boards.
  - Day-2 data are pooled with day 1 only if the bridge log ratios lie within the day-1 REF tolerance (1.96·√2·excess SD, at least ±0.15).
  - Otherwise day 2 is reported separately as a session effect.
- **New criteria version.** The day-2 criteria are the v1.0 criteria. Only the unit counts change. They are committed before day 2.

## 4. Pre-registered analysis plan

### Files
Implemented before booking (pre-registration); see "Pre-registered analysis code" above.

| role | path |
|---|---|
| functions | `Functions/antenna_calibration_pilot.R` (prefix `acp_`; pure definitions) |
| runner | `Testing/audits/analyse_rfid_calibration_pilot.R` |
| portable tests | `Testing/tests/test_antenna_calibration_pilot.R` |
| criteria | `docs/rfid_calibration/acp_criteria_v1.0.csv` (sha256 pinned in the test) |
| schedule | `docs/rfid_calibration/schedule_one_day_v1.csv` |
| schedule generator | `docs/rfid_calibration/make_schedule_one_day_v1.R` (seed 20261014) |

**Running.**
- One light R process; data.table threads 1.
- Inputs, read-only: `Raw Data/Behavior/RFID/calibration/<YYYYMMDD>/` (log, sheet, annotation).
- Output: the isolated root `<Behavior>/RANM/calibration/<run_id>/`, checked with `mmm_isolated_output_root_check()`. It is built in `.tmp_<run_id>`, renamed at the end and refused if it already exists.
- Manifest: code, input, schedule and criteria hashes, the seed and session info.

**Group-blind.** No group label, CombZ or outcome is read. The only Exp9 input is Stage 00b `tables/board_history.csv` from run `raqc_20261014_6165adf`, which is acquisition-only.

### Functions (inputs → outputs)

#### Ingest and alignment
1. `acp_criteria(path)` → list of thresholds. Stops if the sha256 differs from the pinned value.
2. `acp_read_schedule(path)` → data.table of `schedule.csv`. Hash checked.
3. `acp_read_sheet(path, schedule)` → one row per executed placement:
   - placement_id (order, plus r for repeats), visit, block, board, pair_id;
   - position, x, y, orientation, height, object, house and well state, lid closed;
   - tags with slots and carriers, n_tags;
   - pc_start, pc_end, clapper_ok, deviation.

   It checks that every schedule row was executed or dropped under the fixed drop order, flags order deviations, and checks that no two placements overlap on one board.
4. `acp_read_session(path)` → list with:
   - `events` = `rfid_read_raw_log(path, keep_read_offsets = TRUE)$events`;
   - `reads` = `apr_reads(events)` as data.table (tag, t, board, PositionID, event);
   - `sysmsg` = `amv_system_messages(path)`;
   - `device_check` = `aqc_device_check(devices)`;
   - `n_sessions`.

   It calls `rfid_read_raw_log()` directly, because `rfid_read_recording()` expects the MMMdata names.
5. `acp_clapper_touches(reads, tag_C, board = 5, pid = 4, gap_s = 3)` → touches (on, off, n_reads).
6. `acp_align(sheet, touches, tol_s = 3)` → start and end in log time, source (clapper or sheet), offset; unmatched touches flagged.
7. `acp_windows(aligned, guard_s = 5)` → analysed windows (ws, we, T), including the C sub-periods C0, C1, C4, CS and CE.
8. `acp_survey_centres(reads, survey_annotation)` → measured antenna centres per board (read-zone midpoints); falls back to the printed grid.
9. `acp_truth(windows, centres)` → per window:
   - true reader (nearest centre);
   - admissible set (within 5 grid units of the minimum distance);
   - true column, boundary pair, geometry (`amv_move_geometry`);
   - phase pairing (A = PositionID {2,3,5,8}, B = {1,4,6,7}).
10. `acp_read_annotation(path, schedule)` and `acp_movement_truth(annotation, centres, step = 0.1, guard = 0.2)` → a time grid of true reader and column, with the guard mask and the pass and visit IDs.

#### Endpoints
11. `acp_detection(reads, events, windows, truth, lambda_sat)` → E0/E1 per placement × antenna:
    - reads, r, P1s, P10s, e;
    - `aqc_lattice()` of the MsgValue1 gaps and the share of missed windows (k ≥ 6);
    - longest gap, q99 gap, event splits per min;
    - detection class.
12. `acp_lambda_sat(det)` → board median own-antenna rate over the z centres (P01-P03, REF-z, H04).
13. `acp_reconstruct(events, reads, window, tau = c(0, 2), step = 0.1)` → assigned reader on a grid, per basis × tau:
    - event basis: `amv_runs(event starts, PositionID, ws, we, tau)`;
    - read basis: `asr_resolve_ties(t_ms, pid)`, then `amv_runs()`;
    - grid: `amv_grid_track()`.

    Only observations from the placement's own start are used.
14. `acp_confusion(grid, truth)` → per unit × basis × tau: correct reader, correct column, other row, ±1 column, ≥ 2 columns, unassigned.
15. `acp_coread(reads, windows)` → `aqc_coread_flags()` and `aqc_coread_counts(by = "placement_id")`; share of transitions ≤ 60 ms; by geometry × phase pairing.
16. `acp_competition(reads, cwin, roles)` → per board × period × tag: rate, P1s, P10s, longest gap, gaps ≥ 10/30/60 s, q99 gap. Also TR, MS, Gini, CS/C4 and SR, each with a 10-s block bootstrap.
17. `acp_shielding(det, pairs)` → per pair: log rate ratio on/off and the P1s difference; pooled.
18. `acp_flicker(events, reads, windows, taus = c(0.5, 1, 2, 5, 10, 20))` → per placement and basis: raw reader changes, the A→B→A run lengths, and the share removed by `rfid_flip_filter()` at each tau.
19. `acp_boundary_timing(reads, mtruth)` → per crossing: entry latency and exit tail.
20. `acp_visits(events, reads, mtruth, taus)` → per visit × basis × tau: kept, measured run length vs true dwell.
21. `acp_board_ratio(det, coread, pairs, ref)` → per pair: log ratios and the informative flag; R_board and the co-read ratio with CIs; by antenna group; drift-adjusted version.
22. `acp_ref_repeatability(det_ref)` → per board: excess SD beyond counting noise, late/early ratio, trend, change in REF-z cadence.
23. `acp_known_empty(reads, idle, placed_tags, tag_C)` → E8 events per board-hour, IDs, Poisson upper bound.
24. `acp_cadence(events, board_history)` → E9 z scores.
25. `acp_vendor_emulation(reads)` → `apr_positions(gate = "vendor")` per run. Descriptive only, never a decision.

#### Statistics and decisions
26. `acp_boot(units, fun, strata, cluster, B = 10000, seed = 20261014)` → estimate and percentile CI.
27. `acp_project(est, lo, hi, threshold)` → the projection test P.
28. Decision functions, each returning (decision, statistic, estimate, lo, hi, n_units, category, rule_id, feeds):
    - `acp_decide_basis` (criterion a);
    - `acp_decide_resolution` (B2);
    - `acp_decide_support_age` (B3);
    - `acp_decide_competition` (B4-B6);
    - `acp_decide_delta` (B7);
    - `acp_decide_shielding` (B8);
    - `acp_decide_board` (B10);
    - `acp_decide_tau` (B11);
    - `acp_decide_phantom` (B15);
    - `acp_decide_transfer` (B16);
    - `acp_decide_day2` (D1-D6) → a recommendation only.

### Synthetic-log tests
These are portable: they write UTF-16LE logs under `tempdir()` in the way `test_raw_acquisition_qc.R` section 12 does. They never source a runner and never touch S:.

| test | what it checks |
|---|---|
| T1 | Detection classes and P1s, P10s and e worked out by hand: a tag read every 414.5 ms is reliable; no reads is unread; alternating seconds is border. |
| T2 | Clapper alignment recovers a 2.3-s offset; a missing touch falls back to the sheet with a flag; a spurious touch is ignored. |
| T3 | No carry-over between placements: the previous placement's R1 read does not seed the next R8 placement. |
| T4 | Event vs read basis on a hand-built overlap: a long R5 event plus a single R6 read at 3 s. The event basis holds R6 until the next R5 event start; the read basis returns to R5 at the next read. Shares are computed by hand. |
| T5 | The basis decision returns READ for D = +0.2, TIE-equivalent for ±0.01, EVENT for −0.2, and inconclusive when the boards disagree. |
| T6 | The tau sweep removes 0.3-0.8 s flicker at tau = 1, keeps a 5-s visit at tau = 2 and removes it at tau = 10. |
| T7 | Competition is classified as time-sharing, collision or starvation (a 70-s gap) on built inputs; SR and the CS/C4 ratio are checked. |
| T8 | The board call gives "difference" at ratio 0.8 with 5% repeatability, "near 1" at 1.0, and "undecided" when REF drift flips the call. |
| T9 | Shielding pairs that are unread in both states are excluded. |
| T10 | Known-empty counting and the Poisson bound. |
| T11 | Truth: nearest centre; ties at midpoints give 2 admissible readers and at the junction 4; the phase pairing table. |
| T12 | The schedule hash pin; a swapped order is flagged; the drop order is honoured. |
| T13 | The projection test and the day-2 triggers at hand-chosen numbers. |
| T14 | The criteria hash pin is enforced. |
| T15 | Group-blind: no Group, CombZ, sus_animals or outcome symbol in the parsed code. |
| T16 | The output-root guard refuses Raw Data, MMMdata and RANM/measurement_validation. |

### Repository functions reused unchanged
| file | functions |
|---|---|
| `Functions/antenna_log_helpers.R` | `rfid_read_raw_log(keep_read_offsets = TRUE)`, `rfid_flip_filter()`, `rfid_carry_forward_intervals()`, `rfid_union_seconds()`, `RFID_GRID_X/Y`, `RFID_EXCEL_EPOCH_DAYS` |
| `Functions/animalpos_reconstruction.R` | `apr_reads()` (read times on the millisecond grid), `apr_positions(gate = "vendor")` |
| `Functions/antenna_measurement.R` | `amv_read_times()`, `amv_runs()`, `amv_grid_track()`, `amv_transitions()`, `amv_move_geometry()`, `amv_column()`, `amv_union()`, `amv_gaps()`, `amv_support()`, `amv_concurrency()`, `amv_system_messages()` |
| `Functions/antenna_spatial_reconstruction.R` | `asr_resolve_ties()`, `asr_position_at()`, `asr_unsupported()`, `asr_supported_seconds()`, `ASR_TAU_S`, `ASR_BOUNDS_S` |
| `Functions/antenna_acquisition_qc.R` | `aqc_lattice()`, `aqc_coread_flags()`, `aqc_coread_counts()`, `aqc_neighbours()`, `aqc_age_at()`, `aqc_expected_devices()`, `aqc_device_check()` |
| `Functions/project_paths.R` | `mmm_isolated_output_root_check()` |

New code is limited to the joiner (sheet, clapper, windows, truth, annotation), the endpoint wrappers, the bootstrap and projection, and the decision functions.

### Effort
- Before booking: about 10-12 h (code, tests, criteria).
- Video annotation: about 2 h (48 crossings, 24 visits, survey).
- Analysis after the day: about 12-14 h.
- Review: 2 h.

## 5. Reference: the full two-day programme (v1, 2026-10-14)

The programme below was the first proposal. Its endpoint definitions (E0-E9) are used by the one-day pilot; its second day is run only if the day-2 rule recommends it and Tobias decides so.

**Superseded decision texts (note of 2026-10-14).** Where a decision paragraph below differs from the frozen criteria, acp_criteria_v1.0 (section 2) governs and the text here is superseded, as follows:
- E2 resolution ("at every centre and orientation"): replaced by B2 (every tested centre placement assigned for at least 50% of its window has a correct-reader share of at least 0.95; P12 and P13 have a correct-row share of at least 0.90; on both boards under the adopted basis).
- E4 board/group rule (C3 runs; "T1's rate falls by more than 15%"): replaced by B6 (sentinel ratio SR ≤ 0.85 with upper CI < 1).
- E4 house-occupancy wording: replaced by specification v1.1 section 5 (reported with its unknown share).
- E5 house rule (ratio ≥ 0.9 and P1s ≥ 0.95): replaced by B8 (pooled ratio ≥ 0.90 with lower CI ≥ 0.80, and P1s(on, HCz) ≥ 0.95 on both boards).
- E6 tau rule (11 of 12 visits): replaced by B11 (at least 7 of 8 five-second visits, per basis).
- E6 basis rule: replaced by criterion (a).
- E1 support-age rule ("ever unread for more than 1 s"): replaced by B3 (Gmax, the longest gap within a reliable single-tag placement, <= 2 s on both boards; otherwise the smallest candidate of {2, 10} s above Gmax).
- E7 hardware-difference rule ("any of" reads ratio <= 0.9, co-read ratio <= 0.7 or a smaller footprint): replaced by B10 (R_board <= 0.90 with upper CI < 1, or co-read ratio <= 0.70 with upper CI < 1; REF excess SD <= 0.10 on both boards; the call unchanged after REF drift adjustment; no-difference and undecided as in B10). The footprint comparison is descriptive.

**Status: proposal, not started.** Nothing is booked, no tag is placed and no animal is involved. It turns steps 1-5 of `docs/RFID_CALIBRATION_PROTOCOL.md` into a two-day bench pilot (a one-day minimal version is in Resources) with the calibration endpoints fixed before any hardware time is booked. Statements marked *inferred* come from desk checks of the Exp9 logs and the configuration workbook (2026-10-14) and must be confirmed by PhenoSys or by the pilot itself.

Goal: estimate location-classification errors, missed reads and detection competition on the Exp9 hardware, so that the open choices of the spatial specification (`docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.md`: reconstruction basis, flip-filter tau, carry-forward bound, reader or column resolution, board handling) rest on measured values. A static-tag calibration bounds the hardware; it does not validate positions of freely moving implanted mice (section "Limitations").

### Protocol

- Step 6 of the proposal (a filmed animal) is not part of the pilot.
- The pilot is group-blind: no Exp9 animal, no CON/RES/SUS label and no outcome is used.

#### 1. Design choices compared with the proposal

**Two boards.**
- **Board 4** is the least sensitive board.
  - Incoming crosstalk 0.45 x the homologous median; reads 0.83 x; silent share 11.0%.
  - Its deficit sits at the house end (reader_qc.csv, medians over 24 recordings, recomputed 2026-10-14):
    - crosstalk-in ratio R1 0.14, R2 0.15, R5 0.31, R6 0.14, against R4 1.19 and R8 0.83;
    - reads relative to homologous R2 0.40, R6 0.53.
  - R6 on boards 3 and 4 lies below all of boards 1, 2 and 5 in 24/24 recordings; R4 in 0/24.
- **Board 2** is the closest to the homologous median (0.91 x, 1.00 x, 3.9%).
- **Substitutes:** board 3 for board 4, board 1 for board 2. Board 5 is not a reference: it has the fastest cadence (mean within-event gap 367 ms, against 396-407 ms on boards 1-4; `docs/RFID_ACQUISITION_QC.md`).

**60 analysed seconds per static placement** (70 s recorded, with a 5-s guard at each end).
- Each placement only has to be sorted into one of three detection classes. 60 s is enough for that (see Endpoints).
- The time saved buys more positions.
- Longer durations are kept only where tails matter: competition 2-5 min, static flicker 5 min.

**Orientations.**
- x, y and z at the 8 antenna centres.
- The two horizontal ("walking") orientations at boundaries.
- x and z at the resources and at 3-cm height.

**New stratifier: reader phase group** (section 2).

**Known-empty control** is free: the boards not under test record all day.

**Time marks in the log itself:** a "clapper" tag is touched to an antenna on a board that is not under test.

#### 2. Fixed conditions (production configuration, unchanged)

**Software and configuration.**
- PhenoSoft 1.0.8147.28429.
- Workbook as `Raw Data/Behavior/RFID/sis_phenosys/BATCH1_SIS_5cages.xlsx`:
  - 5 LabNetV2 rfid32 boards AM1-AM5 (192.168.137.11-15);
  - per antenna: pause 1000, length 10, onlyValid false;
  - per board: phase1 96, phase2 69, phaseDuration 250;
  - xPos 0/100/200/300, yPos 0/116, zPos 0.
- Archive the workbook actually loaded (sha256) and diff it against BATCH1. Retrieve the per-batch SIS workbooks from the PhenoSys PC first.
- With onlyValid false, unregistered tags are logged, so pilot tags need no naming. If labels are added, only the Individual sheet may change.
- pause 1000 coincides with the ~1-s event-closing rule (inferred).
- The meaning of length, phase1/phase2 and phaseDuration is undocumented. Ask PhenoSys and change nothing.

**Physical set-up.** Same rack, same board-to-rack position, and the same cage type, floor, lid, red house, food hopper and water bottle as in Exp9. Bedding as in production, with its depth measured.

**Session and clock.**
- Start one PhenoSoft session per day and start it once: all three Exp9 dropout antennas read normally in the placement scan before their session and were silent for the whole session (inferred to be a channel state set at session start; shown to end with the session only for B2 CC3).
- Not on a DST-change day (the B1 CC1 fold).
- No manual change of the PhenoSys PC clock during the day; note any NTP synchronisation.

**No animals on the rack.**

**Channel, reader and phase-group map.** It is identical on all boards. Grid units: 100 ≈ 10 cm, inferred from plate text; to be verified.

| reader (QC label) | grid x, y | channel AM<n>-k | phase group | resource (lab-reported) |
|---|---|---|---|---|
| R1 | 0, 0 | 1 | B | food |
| R2 | 100, 0 | 3 | A | |
| R3 | 200, 0 | 5 | A | |
| R4 | 300, 0 | 7 | B | |
| R5 | 0, 116 | 2 | A | house |
| R6 | 100, 116 | 4 | B | |
| R7 | 200, 116 | 6 | B | |
| R8 | 300, 116 | 8 | A | water |

The configured AM view draws each board as 4 rows x 2 columns (AM-1 | AM-2 on top), rotated against this table. At the bench, always use channel numbers.

**Phase groups (inferred, desk check 2026-10-14 on the B2 CC1 and B6 CC1 raw logs).**
- **The masks.** Read as hexadecimal, phase1 96 and phase2 69 are complementary masks: 0x96 = 10010110, 0x69 = 01101001. They split the channels into {2,3,5,8} = A and {1,4,6,7} = B. The partition is the same under either bit order.
- **What the logs show.** Take the same tag's consecutive reads at two adjacent antennas, and count those less than 60 ms apart:
  - **within-group pairs:** 20-49% of transitions (R2-R3, R6-R7, R1-R6, R2-R5, R3-R8, R4-R7);
  - **cross-group pairs:** 4.4-5.4% of transitions, with medians of 213-248 ms (every column pair, plus R1-R2, R3-R4, R5-R6, R7-R8).
- **Plausible reading.** The two groups are active in alternate 250-ms phases. This also fits the within-antenna gap modes of 114.5, 414.5 and 514.5 ms.
- **Consequences for the pilot.**
  - Every house-end row and column boundary (R1|R2, R5|R6, R1|R5, R2|R6) is cross-group.
  - The corridor pairs R2|R3 and R6|R7 are within-group.
  - Crosstalk, flicker and latency are reported by geometry x phase pairing.

#### 3. Before booking (desk work, no hardware)

1. **Pre-specify the analysis.** Commit the pilot analysis code and the decision thresholds of the Endpoints, tested on a synthetic calibration log (as `Testing/tests/test_raw_acquisition_qc.R` does). Reuse:
   - `rfid_read_raw_log(keep_read_offsets = TRUE)` for read-level times;
   - `aqc_lattice()`;
   - `rfid_flip_filter()`;
   - the carry-forward and support functions of `Functions/antenna_measurement.R`.
2. **Home-cage references.** Extend the phase-pairing desk check to all 24 recordings, to get reference values by geometry x phase pairing.
3. **Ask PhenoSys** what phase1, phase2, phaseDuration and length mean, and for the size of the antenna loops.
4. **Confirm the materials and set-up:**
   - tag model and lot (Exp9 tags and the 2023 naming-test tags share the ID prefix 041AF89) and the number of spare tags;
   - cage type, floor thickness, bedding type and depth;
   - house, hopper and bottle models;
   - the implant site, which sets realistic tag heights.
5. **Book** two consecutive days with the rack free of animals.

#### 4. Materials

- **Tags:** 9 spare tags of the Exp9 type: T1-T8 plus the clapper C. Minimum 6: T1-T4, C and one spare.
- **Non-metallic carriers:**
  - 1-cm foam cubes and 3-cm foam blocks, each with three slits (x, y, z) and the tag ID written on it;
  - a foam stack of house-roof height;
  - non-metallic putty.
- **Spacer board:** rigid foam as thick as the production bedding, cut to the cage floor. It carries a laminated printed grid:
  - 1-cm squares;
  - centres, midpoints, junctions, quarter points and corners marked with grid coordinates, R labels and channel numbers.
- **Movement:**
  - a straight plastic rail fixed along each path;
  - a low plastic sled carrying a 1-cm carrier;
  - nylon string or a 50-cm wooden rod;
  - a metronome app and 5-cm marks.
- **Cage items as in Exp9:** cage, lid, red house, food hopper, filled water bottle, fresh bedding (used bedding optional).
- **Video:** a camera at ≥30 fps (60 preferred) on a top-down mount framing the whole floor. A millisecond clock running on the PhenoSys PC must be visible in the frame (second monitor or mirror); a PC-driven LED is an alternative.
- **Other:** pre-printed placement sheets, tape measure or calliper, plastic tweezers, and a closed box for idle tags.

#### 5. Set-up per board

1. Put cages as in production on all 5 boards, with no tags anywhere.
2. Photograph the board under test: plate edge and cage position, then again with the resources in place. This records R1 food, R5 house and R8 water, which no document records today.
3. Start PhenoSoft with the production workbook. Archive the loaded workbook and the version row. Record a 10-min empty baseline.
4. **Channel check:** hold T1 for 5 s on each of the 8 antennas and on the clapper antenna.
5. **Survey:**
   - measure the physical pitch between antenna centres;
   - slide T1 (z orientation) at about 1 cm/s along both rows and the 4 columns, on video;
   - the centre of each antenna's read zone gives its physical centre;
   - record the grid unit in mm.
6. Insert the spacer board and align the printed grid to the measured centres.

#### 6. Placement list

Each static placement: 70 s recorded, 60 s analysed, about 100 s including handling.

| block | design | positions (grid units) | orientation | height | tags | time per placement | placements (board 2 / board 4) |
|---|---|---|---|---|---|---|---|
| T0 tag QC | every tag at one spot | R3 centre | z | floor | each of 9 | 60 s | 9 / 9 |
| S1 centres | single tag | 8 antenna centres | x, y, z | floor | T1 | 60 s | 24 / 24 |
| S2 boundaries | single tag | 6 row midpoints (50, 150, 250 at y 0 and 116); 4 column midpoints (0, 100, 200, 300 at y 58); 3 junctions (50, 150, 250 at y 58) | x, y | floor | T1 | 60 s | 26 / 26 |
| S2 quarter points | single tag | (25,0), (75,0), (25,116), (75,116), (125,0), (175,0) | x | floor | T1 | 60 s | 6 / 6 |
| S2 quarter points | single tag | (0,29), (0,87) | y | floor | T1 | 60 s | 2 / 2 |
| S2 corners | single tag | 4 cage-floor corners, 2 cm from the walls | x | floor | T1 | 60 s | 4 / 4 |
| S3 height | single tag | R1, R3, R5, R8 centres | x, z | 3 cm | T1 | 60 s | 8 / 8 |
| S4 house | production bedding | inside the house at R5: centre, back corner, entrance; with and without the house (house lifted, bedding untouched) | x, z | on bedding | T1 | 60 s | 12 / 12 |
| S4 roof | | on the house roof; without the house, on a foam stack of roof height | x, z | roof | T1 | 60 s | 4 / 4 |
| S4 hopper | | R1: feeding position (height measured in the cage) and floor under the hopper; with and without the hopper | x, z | measured / floor | T1 | 60 s | 8 / 0 |
| S4 bottle | | R8: drinking position at the spout and floor under the spout; with and without the bottle | x, z | measured / floor | T1 | 60 s | 8 / 0 |
| S5 C1 house competition | tags added one at a time without moving the others; 1-2 cm apart in a 3 x 3 cm patch; orientations x, z, y, x | inside the house at R5 | mixed | on bedding | rotation a: T1-T4; rotation b: T5-T8 in reversed slot order | n = 1, 2, 3, 4 for 2, 3, 3, 5 min (continuous, a clapper touch at each addition) | 8 / 2 (n = 1 and 4, rotation a) |
| S5 C2 open competition | as C1 | R3 centre | mixed | floor | as C1 | as C1 | 8 / 2 |
| S5 C3 board/group test | T1 alone (from C2 n = 1), then with 3 tags at a far antenna | 3 tags at R5 (far, same group A), then at R1 (far, group B) | z | floor | T1 + 3 | 3 min each | 2 / 0 |
| S5 C4 split | 2 + 2 tags on neighbouring cross-group antennas | R5 and R6 centres | mixed | floor | 4 | 3 min | 1 / 1 |
| S6 M1 passes | sled on the rail, 5-s pause at each centre, about 5 and 20 cm/s, 3 passes per direction and speed | lower row R1→R4 and back; upper row R5→R8 and back; columns R1↔R5 and R2↔R6 | x (rows), y (columns) | floor | T1 | — | 48 passes, 96 crossings / 12 passes (upper row), 36 crossings |
| S6 M2 short visits | centre to neighbour centre, dwell 1, 2 or 5 s, return; 10 s at the origin before and after; 4 repetitions each | R5→R6 (cross-group), R2→R3 (within-group), R1→R5 (column) | x, x, y | floor | T1 | — | 36 / 12 (R5→R6 only) |
| S6 M3 static flicker | single tag at a midpoint | R5\|R6 (50,116), R2\|R3 (150,0), R1\|R5 (0,58) | x, x, y | floor | T1 | 5 min | 3 / 2 (R5\|R6, R2\|R3) |
| Bridge (day 2, on board 2) | test-retest | T0 for T1-T3, then the 8 centres | z | floor | T1-T3 | 60 s | 11 |
| S7 known-empty | no tag on the board | untested boards all day; all boards 10 min at the start and end of each day | — | — | none | continuous | about 38 board-hours |

**Movement crossings.**
- **Board 2, per speed:** 12 within-group row crossings (R2\|R3, R6\|R7), 24 cross-group row crossings and 12 column crossings.
- **Board 4, per speed:** 6 within-group and 12 cross-group crossings.

#### 7. Session plan (minutes)

**Day 1: board 2, full programme**

| step | block | min | cumulative |
|---|---|---|---|
| 1 | Rack set-up; start PhenoSoft once; verify and archive the workbook; photos; 10-min empty baseline | 30 | 30 |
| 2 | Channel check; survey sweep; align the spacer board | 15 | 45 |
| 3 | T0 tag QC | 15 | 60 |
| 4 | S1 centres | 40 | 100 |
| 5 | S2 boundaries | 64 | 164 |
| 6 | Break | 15 | 179 |
| 7 | S3 height | 14 | 193 |
| 8 | S4 resources: bedding set-up 10, house and roof 32, hopper 14, bottle 14 | 70 | 263 |
| 9 | S5: C1 in the house; then remove the house and bedding and replace the spacer board; C2, C3, C4 | 75 | 338 |
| 10 | Break | 15 | 353 |
| 11 | S6: camera and clock 10, M1 20, M2 12, M3 18 | 60 | 413 |
| 12 | 10-min empty baseline; stop; back up with sha256 | 25 | 438 |

**Day 2: board 4, matched core, plus the bridge on board 2**

| step | block | min | cumulative |
|---|---|---|---|
| 1 | Set-up, start, baseline | 30 | 30 |
| 2 | Channel check and survey | 15 | 45 |
| 3 | T0 tag QC (same 9 tags) | 15 | 60 |
| 4 | S1 centres | 40 | 100 |
| 5 | S2 boundaries | 64 | 164 |
| 6 | Break | 15 | 179 |
| 7 | S3 height | 14 | 193 |
| 8 | S4 house and roof, with bedding set-up | 37 | 230 |
| 9 | S5 reduced: C1 n = 1 then 4; C2 n = 1 then 4; C4 | 20 | 250 |
| 10 | S6 reduced: camera 10, M1 upper row 6, M2 R5→R6 5, M3 two midpoints 12, handling 2 | 35 | 285 |
| 11 | Bridge block on board 2 | 20 | 305 |
| 12 | Baseline; stop; back up | 25 | 330 |

**Totals:**
- Day 1 is 7.3 h on site and day 2 is 5.5 h.
- Board 2 is under test for 6.2 h (including the bridge) and board 4 for 4.0 h.

#### 8. Bench rules and stop criteria

**During every analysed window:**
- Hands and any metal (watch, ring, phone, steel tools, keys) stay at least 30 cm from the cage.
- Only non-metallic tools are used.
- Idle tags stay in a closed box at least 1 m from every plate.

**Clapper.** Tag C sits on board 5, channel 7 (R4), in the neighbouring cage.
- Touch it for about 2 s at every placement start and end, and twice at each block start.
- The recorder writes the PC-clock time of each touch on the sheet.

**Stop rules:**
1. **Channel check fails** (an antenna is silent for 5 s with T1 on it): restart the session once and repeat the check. If it persists, do not test that board that day and record it as a session-start dropout.
2. **Tag QC:**
   - a tag outside ±20% of the tag median is not used in S5;
   - T1 must lie within ±10% of the median, otherwise replace it with the tag nearest the median.
3. **Low read rate on board 2:** if T1 at a centre in z orientation reads below 1.5 reads/s, check plate seating and cage position before continuing.
4. **Disturbance in an analysed window:** repeat the placement under a new ID. Never overwrite a placement.

#### 9. Records and data handling

**Placement sheet,** one row per placement, with these columns:
- placement_id, day, board, block;
- position label, x and y in grid units, true reader and column;
- boundary pair and phase pairing;
- orientation, height in cm;
- object (none, house, hopper, bottle, roof stack), bedding type and depth;
- tag IDs and slots, number of tags;
- planned seconds, PC-clock start and end, clapper OK;
- operator, deviations.

**Movement annotation table** (from video):
- pass or visit ID;
- the PC-clock time at which the carrier centre crosses each midpoint line;
- arrival and departure times at the centres.

**Storage:**
- **Raw data:** keep the raw logs, the loaded workbook, the sheets (scan and CSV), photos, videos and a sha256 manifest in a new folder outside MMMdata, for example `Raw Data/Behavior/RFID/calibration/<YYYYMMDD>/`.
- **Analysis output:** write it to an isolated root outside Raw Data and outside RANM/measurement_validation, for example `RANM/calibration/<run_id>/`, as the Stage 00b path guard requires.


### Endpoints

#### Conventions

All endpoints are computed from the raw PhenoSoft logs plus the placement sheet. The movement annotation table adds video truth for E6.

**Analysed window.**
- W_p = [start + 5 s, end − 5 s], with start and end taken from the clapper events (sheet times as fallback).
- T_p = 60 s for static placements.

**Read times and bins.**
- Read time = event start + cumulative MsgValue1 offsets.
- 1-s bins are aligned to the window start.

**Truth.**
- True reader = the nearest antenna centre.
- Midpoints and junctions have an admissible set of 2 or 4 readers.
- The true column is defined the same way.

**λ_sat (read opportunities per second at saturation) ≈ 2.45/s.**
- Exp9 within-event read rate: board medians 2.45-2.54 for boards 1-4 and 2.73 for board 5.
- Mean within-event gap 367-408 ms.
- Inferred: two active 250-ms phases per second.
- The pilot re-estimates λ_sat per board from the S1 z placements.

**D = design effect.** Reads come in pairs 114.5 ms apart, and consecutive seconds are dependent.
- The pilot estimates D by a 10-s block bootstrap.
- The arithmetic below shows D = 1 and D = 2.

**The placement is the independent unit** for anything that depends on position, because repeatability is about ±0.5 cm on the printed grid. Within-placement precision only says how well the value at one position is known.

#### E0 Tag variability

**Definition.** Reads/s of each tag at the R3 centre (z, floor); each tag's ratio to the tag median; the CV across tags.

**Precision.** 147 reads in 60 s gives ±16% per tag (Poisson bound; conservative, because the lattice cadence is more regular than Poisson).

**Decision it informs.**
- Which tags enter the competition sets.
- T1 is used for every single-tag placement on both boards, so tag variance stays out of the board ratio.
- It sets the floor for reading per-animal read differences (QC W10).

#### E1 Detection map and missed reads

**Definition.** For placement p and antenna k:
- r_pk = reads at k in W_p / T_p;
- P1s_pk = share of 1-s bins with at least one read at k;
- e_pk = r_pk / λ_sat (read efficiency);
- missed-window share = share of gaps on lattice k ≥ 6 (≥614.5 ms; `aqc_lattice`);
- inter-read gap distribution, event splits per minute (gaps > 1 s), longest gap, and P10s (share of 10-s bins with a read).

Each position x orientation x height x object is classed as:
- **reliable:** P1s ≥ 0.95;
- **border:** between the two limits;
- **unread:** P1s ≤ 0.05.

**Precision at 60 s.**
- All 60 seconds read: Clopper-Pearson lower bound 0.94 (rule of three 0.95).
- No second read: upper bound 0.06.
- P1s = 0.5: ±0.13 (D = 1) to ±0.18 (D = 2).
- e = 0.5, with 147 read opportunities: ±0.08 (D = 1) to ±0.11 (D = 2).

**What longer placements would buy.**

| analysed time | lower bound, all read | upper bound, none read | half-width at 0.5 (D = 1 to 2) |
|---|---|---|---|
| 60 s | 0.94 | 0.06 | 0.13-0.18 |
| 120 s | 0.97 | 0.03 | 0.09-0.13 |
| 300 s | 0.99 | 0.012 | 0.06-0.08 |

The three-class call does not need more than 60 s.

**Decision it informs.**
- The detection footprint per antenna and orientation in cm: what "assigned to reader k" means physically (specification S3/S6).
- *(Superseded by B3 of acp_criteria_v1.0; see the note at the top of section 5.)* Whether a present, uncontested tag in the reliable zone is ever unread for more than 1 s. If not, longer home-cage gaps there are not explained by the bench hardware; posture, height, competition or absence remain possible, and the bench cannot separate them. This sets the shortest meaningful 'recently detected' age class; it does not validate a location.
- An input to delta (E5).

#### E2 Localization confusion

**Definition.** For every analysed second of the static placements, the reconstructed reader under four bases:
- carry-forward (LOCF) from event starts, at tau 0 s and 2 s;
- LOCF from single reads, at tau 0 s and 2 s;
- functions: `Functions/antenna_measurement.R` and `rfid_flip_filter`.

Each is compared with the truth, per board x position class (centre, quarter point, midpoint, junction, corner, resource) x orientation x height. Reported shares:
- correct reader;
- correct column;
- other row;
- ±1 column;
- ≥2 columns;
- unassigned.

**Precision.**
- Per board, 1,440 centre seconds (24 placements): ±0.005 at 0.99 (D = 1).
- At placement level: if all 24 centre placements are ≥0.95 correct, the share of failing placements is ≤ 3/24 = 0.125.

**Decision it informs: column or reader resolution (S3).** *Superseded by B2 of acp_criteria_v1.0 (see the note at the top of section 5).* Reader level only if both hold on both boards:
- correct-reader share ≥ 0.95 at every centre and orientation;
- correct-row share ≥ 0.9 at the column quarter points (0,29) and (0,87).

Otherwise the column-resolution proposal stands.

#### E3 Crosstalk ratios

**Definition.** For own antenna k and neighbour j:
- **co-read share:** share of the seconds with a read at k that also have a read at j within 1 s (the Stage 00b co-read definition, at tag level);
- **simultaneous share:** share of k→j transitions ≤ 60 ms;
- **double-read zone:** positions where the co-read share is ≥ 0.5 in both directions.

Reported by geometry (row, column, diagonal) x phase pairing (within, cross) x position x orientation x board.

**Home-cage references.**
- Co-read: row about 0.14, column 0.08, diagonal 0.01.
- HMM pilot, per second: 0.094 / 0.035 / 0.004.
- Simultaneous share: 0.20-0.49 within-group, 0.044-0.054 cross-group.

**Precision.**
- Per centre placement: ±0.09 at 0.14.
- Pooled per board: 36 row, 24 column and 36 diagonal directed pair-placements give about ±0.03-0.04, if the between-placement SD is 0.1.
- A diagonal probability of 0.01 pooled over 1,440 s gives about 14 co-reads (Clopper-Pearson 0.005-0.016).
- A single 120-s placement cannot separate 0.01 from 0 (P(0 co-reads) = 0.30).

**Decision it informs.**
- Replaces the provisional co-read thresholds of `aqc_rules()` (H06 confirmation 0.25, W09 0.02, the W08 expectation model) with board-specific expectations.
- The emission matrix of any probabilistic location model.
- Whether static crosstalk at a given position can explain a short A→B→A visit.

#### E4 Competition curve

**Definition.** For n = 1-4 co-located tags:
- per-tag reads/s;
- total throughput (the sum) and its ratio to n = 1;
- the most-read tag's share of reads, and Gini;
- per-tag P1s and P10s, longest gap, and the number of gaps ≥ 10, 30 and 60 s.

Plus:
- **board/group test (C3):** T1's rate with 3 tags at a far same-group antenna, and at a far cross-group antenna, relative to T1 alone;
- **split (C4):** reads per tag with 2 + 2 tags at R5/R6.

**Expected under time-sharing.**
- Per tag: 2.45/n reads/s.
- Home-cage reference: 1.21 / 0.62 / 0.35 / 0.23 reads/s with 0-3 mates.

**Precision.** Durations of 2, 3, 3 and 5 min for n = 1-4 give similar precision per tag:

| n | reads per tag | per-tag precision |
|---|---|---|
| 1 | 294 | ±11% |
| 2 | 220 | ±13% |
| 3 | 148 | ±16% |
| 4 | 183 | ±15% |

- Total throughput at n = 4 is about 735 reads, ±7%.
- Under even sharing, the maximum share is about 0.25-0.28. Capture would give ≥ 0.5.

**Decision rules.** *The board- or group-level rule is superseded by B6 of acp_criteria_v1.0, and the house-occupancy wording by specification v1.1 section 5 (see the note at the top of section 5).*
- **Time-sharing:** throughput at n = 4 within ±15% of n = 1 and maximum share ≤ 0.35. Read-density measures then need a co-location denominator.
- **Collision:** throughput below 0.7 x n = 1.
- **Capture or starvation:** maximum share ≥ 0.5, or any present tag unread for ≥ 60 s. Then:
  - unread time at the house is not interpretable as absence;
  - co-assignment stays excluded (S6);
  - house occupancy is reported with its unknown share (specification v1.1 section 5), never as a precise measure of time in the nest.
- **Board- or group-level competition:** T1's rate falls by more than 15% in C3. The number of tags per board then becomes a covariate for read-based measures (B1 boards held 2.9 tags and had more reads per animal).

**Delta (carry-forward bound).** The smallest of {10, 30, 60, 300} s that exceeds both:
- the per-tag q99 gap at n = 4 in the house on board 4;
- the longest gap of any single-tag placement in the reliable zone.

#### E5 Shielding ratios

**Definition.** Paired placements with and without the object, at the same coordinates, tag, orientation and height:
- house: 3 inside positions plus the roof;
- hopper: feeding position and floor;
- bottle: spout and floor.

Reported: the ratio of read rates and the difference in P1s; also C1 against C2 (house against open field under competition).

**Precision.** Per 60-s pair, the paired log-ratio has SE 0.117 from counting alone. Pooled:
- house: 6 pairs, ±0.09;
- hopper and bottle: 4 pairs each, ±0.12.

**Decision it informs.**
- *(Superseded by B8 of acp_criteria_v1.0; see the note at the top of section 5.)* **House ratio ≥ 0.9 and P1s ≥ 0.95:** the house does not shield. Low R5 support in Exp9 is then competition, posture or nest height, which bench data cannot separate further.
- **House ratio < 0.7:** shielding. R5 read densities are then not comparable with other readers, and house occupancy rests on carry-forward with a stated bias.
- Hopper and bottle are read the same way for the R1 and R8 densities (QC D01; W10 interpretation).

#### E6 Boundary behaviour (movement; truth from video)

**Definitions.**
- **Entry latency** = first read at the entered antenna − the time the carrier centre crosses the midpoint line.
- **Exit tail** = last read at the exited antenna − crossing time.
- Both by boundary class (within-group row, cross-group row, column), speed and board.
- **Measured run length against true dwell** for the 1, 2 and 5-s visits.
- **Static flicker:** A↔B switches per minute and the run-length distribution in the 5-min midpoint placements.
- **Under tau 0.5, 1, 2, 5, 10 and 20 s,** for both reconstruction bases:
  - the share of static flicker runs removed;
  - the share of genuine visits kept.
- **Correct-reader share** in the seconds within ±3 s of a crossing, for event-start against single-read LOCF.

**Precision.**
- Board 2, per speed: 12 within-group, 24 cross-group row and 12 column crossings.
- Median CI for 12 crossings: the 3rd to 10th ordered value (96%).
- Timing error is about 0.1 s (video 33 ms, clock display about 50 ms), against a read cadence of 0.11-0.5 s.
- A cross-group midpoint read by both antennas gives about 100 or more runs per 5 min, enough for a q99.
- Genuine visits: 12 per dwell on board 2. If all 12 are kept, the lower bound is 0.74.

**Decision it informs.**
- *(Superseded by B11 of acp_criteria_v1.0: at least 7 of 8 five-second visits, per basis; see the note at the top of section 5.)* **tau** = the smallest value that removes ≥ 99% of static midpoint flicker runs on both boards and keeps at least 11 of 12 genuine 5-s visits. If that value is above 2 s, the frozen flip2s version leaves crosstalk in reader-change counts, and the reader-change rate stays descriptive and at column level.
- *(Superseded by criterion (a) of acp_criteria_v1.0.)* **Basis:** adopt single-read LOCF (QC decision 2) if its correct-reader share around crossings exceeds event-start LOCF beyond the CI, and it does not lose at static placements.

#### E7 Board 3/4 against other boards (sensitivity ratio)

**Definition.** Paired placements on board 4 and board 2 (same T1, position, orientation, height and object):
- ratios of read rate, P1s and co-read share;
- footprint and double-read-zone width per antenna;
- per antenna (R1, R2, R5, R6 against R3, R4, R7, R8) and pooled.

The bridge block gives the day-to-day repeatability of board 2.

**Precision.**
- 24 pooled centre pairs: ±0.047 (about ±5%).
- Per antenna (3 pairs): ±0.13.
- 38 boundary pairs: ±0.037, plus the positional error.
- Bridge (8 pairs): ±0.08.
- Field signature to compare with: reads 0.83-0.87, incoming crosstalk 0.45-0.52.
- At antenna centres the read rate may saturate, so differences are expected at boundaries and at 3-cm height.

**Decision it informs.**
- *(Superseded by B10 of acp_criteria_v1.0; see the note at the top of section 5.)* **A hardware sensitivity difference** shows as any of the following, outside the bridge repeatability:
  - reads ratio ≤ 0.9;
  - co-read ratio ≤ 0.7;
  - a smaller footprint at R1, R2, R5 and R6.
  
  Then:
  - board becomes a covariate (QC decision 3), with board-specific detection and crosstalk parameters;
  - the house-end antenna deficit is documented.
- **Ratios near 1:** the field signature comes from the cage or rack environment, or depends on occupancy. Board stays recorded as a covariate but is not described as sensitivity.

#### E8 Known-empty

**Definition.** Events per board-hour on boards with no tag, with their tag IDs.

**Precision.** Zero events in about 38 board-hours gives ≤ 0.08 events per board-hour (rule of three). That is below 0.005% of the home-cage rate (about 1,670 events per board-hour).

**Decision it informs.**
- **No phantom events:** a read is treated as a real tag within range.
- **Any phantom event:** an events minimum rule and a revision of W08/H06.

#### E9 Session and cadence integrity

**Definition.**
- The channel check passes after every session start.
- Each board's lattice signature (mean gap, k1 share) is compared with its Exp9 history in `board_history.csv`.

**Decision it informs.** If |z| > 3, the bench boards no longer behave as they did in 2022-2024, and the results transfer to the Exp9 data only with a caveat.

#### Summary: what the durations buy

| duration | buys |
|---|---|
| 60 s static | the three-class detection call; ±16% read rate; ±5% pooled board ratio |
| 3-5 min competition | per-tag rates at ±11-16%, and starvation detection |
| 5-min static flicker | a q99 of flicker runs |
| 12 crossings per class | the median latency between the 3rd and 10th ordered value |

None of these estimates concerns CON, RES or SUS. They are technical and group-blind.


### Resources

#### Hardware time

**Rack time.** The rack is blocked, free of animals, on 2 consecutive days: 7.3 h and 5.5 h on site, 12.8 h in total.

| board | role | hours |
|---|---|---|
| board 2 | under test (day 1 programme plus the 20-min bridge on day 2) | 6.2 |
| board 4 | under test | 4.0 |
| boards under test, total | | about 10.2 board-hours |
| boards 1, 3, 4 (day 1); boards 1, 3, 2 (day 2) | known-empty, recorded at the same time, no extra personnel | about 38 board-hours |
| board 5 | clapper board; contributes only the 4 x 10-min baselines | — |

PhenoSys PC and PhenoSoft 1.0.8147.28429 with the production workbook.

#### Personnel hours

Two people on site: an operator who places tags, and a recorder who handles the sheet, timer, clapper and camera.

| task | person-hours |
|---|---|
| Preparation: spacer board and grid 2, carriers 1, sled and rail 1, sheets and camera/clock test 1, tag pre-check and box 1 | 6 |
| On site: 2 x 12.8 h, including set-up, placements, movement runs and teardown/backup | about 26 |
| Video annotation: about 130 crossings and 48 short visits on two boards, entered in PC-clock time | 4-6 |
| Analysis, before booking: endpoint code, synthetic-log tests, the phase-pairing reference over all 24 recordings, pre-specified thresholds | 10-12 |
| Analysis, after the sessions: ingest and placement checks 4, endpoints E0-E9 with tables and figures 8, report and decision table 4-6 | 16-18 |
| Review and decision by Tobias | 2 |
| **Total** | **about 64-70** |

**Analysis reuse.** Only a placement-sheet and clapper joiner and the endpoint functions are new. Everything else is existing code:
- `rfid_read_raw_log(keep_read_offsets = TRUE)`;
- `aqc_lattice()`;
- `rfid_flip_filter()`;
- the carry-forward and support functions of `Functions/antenna_measurement.R`.

#### Tags and materials

- **Tags:** 9 spare tags of the Exp9 type (prefix 041AF89 lot if spares remain): T1-T8 and the clapper C. Minimum 6 (T1-T4, C, one spare); the second competition rotation then rotates slots only. The same tags are used on both days.
- **Cage items, as in Exp9:** cage, lid, red house, food hopper, water bottle, fresh bedding for two set-ups (used bedding optional).
- **Bench materials:**
  - foam cubes (1 cm) and blocks (3 cm), and a roof-height foam stack;
  - a rigid foam spacer board with a laminated printed grid;
  - plastic rail and sled, nylon string, wooden rod;
  - plastic tweezers and non-metallic putty;
  - a closed box for idle tags;
  - tape measure or calliper.
- **Video and timing:**
  - a phone or GoPro at ≥30 fps (60 preferred) with a top-down mount;
  - a monitor or mirror showing a millisecond clock running on the PhenoSys PC, or a PC-driven LED;
  - a metronome app.
- **Records:** printed placement sheets, and storage for logs, video and photos (a few GB).

#### Minimal version (one day, if time is short)

**Hardware.** One PhenoSoft session, both boards: about 3.8 h on site and about 2.6 h under test (board 2 1.7 h, board 4 0.9 h). Personnel: about 7.5 h on site for two people, 3 h preparation and 1.5-2 days of analysis.

**Schedule (minutes).**

| step | min |
|---|---|
| Set-up, channel check, 10-min baseline | 30 |
| Board 2 (detailed below) | 102 |
| Break | 15 |
| Board 4 (detailed below) | 54 |
| Baseline, teardown, backup | 25 |
| **Total** | **about 226** |

**Board 2 (about 102 min):**
- T0 for 5 tags: 8 min.
- S1 centres in x and z (16 placements): 27 min.
- Five boundaries, in walking orientation: midpoints R1|R2, R5|R6, R1|R5, R2|R3 and the junction (50,58). 9 min.
- House at R5: centre inside the house, x and z, with and without the house. 10 min.
- In-house competition: n = 1, 2, 4 for 2, 3, 5 min; plus C3 (2 x 3 min) and C4 (3 min). About 23 min.
- Movement-lite: 25 min.
  - camera 10;
  - 5-min static flicker at R5|R6;
  - R5→R6 visits, dwell 1/2/5 s x 4;
  - upper-row passes, 2 speeds x 3 per direction.

**Board 4 (about 54 min):** S1 x and z (27), the same five boundaries (9), house in and out (10), in-house competition n = 1 and 4 (8).

**What the minimal version still answers:**
- E1 and E2 at the centres, in two orientations;
- E7 at the centres and the house-end boundaries;
- E5 for the house;
- the shape of E4 in the house, including the board/group test;
- a first tau from the R5|R6 boundary;
- E8 over about 10 board-hours (≤ 0.3 events per board-hour).

**What it drops:**
- the y orientation at the centres;
- full boundary and quarter-point maps, and the corners;
- height;
- hopper and bottle;
- open-field competition and the second rotation;
- the within-group movement replicate;
- the bridge block, so board and day are confounded, which must be stated.

#### Assumptions

- About 30 s of handling per static placement with the jig; 70 s recorded for 60 analysed; one session per day with no restarts.
- Hand-paced movement within about 30% of the nominal speed; the true speed comes from the video.
- At least 6 (ideally 9) spare tags of the Exp9 lot or model exist; the boards, rack positions, plates and workbook are unchanged since Exp9.
- The rack holds no animals on the pilot days; both operators are trained on PhenoSoft; the analyst knows the repository.
- Breaks are included in the on-site hours but not in the board-hours under test.
- No group labels, outcomes or Exp9 animals are needed.


### Limitations

#### What static and phantom tags cannot validate for freely moving implanted mice

The pilot measures bench hardware behaviour and the error of the reconstruction rules under controlled conditions (boards 2 and 4; static or sled-moved tags). It does not validate positions or measures of freely moving implanted mice. Results enter the Exp9 analysis only as priors, parameter choices and sensitivity analyses, never as bounds on true physical occupancy and never as corrections of individual animals' positions.

1. **Coupling and height.**
   - An implanted tag sits in tissue at a depth and height set by the implant site, body size and posture: lying, curled, walking, rearing, climbing.
   - The carriers fix the tag at 1 cm, 3 cm, roof height or a measured feeding/drinking height, in three cardinal orientations only.
   - Body water and tissue may detune or attenuate the coupling slightly; the pilot does not measure that.
2. **Orientation dynamics.**
   - In a mouse, the tag orientation changes continuously with grooming, turning, rearing and curling up to sleep.
   - The pilot samples x, y and z only. Intermediate and changing orientations may read differently.
3. **Huddles.**
   - The 4-tag patch mimics tag proximity and read competition only.
   - It does not reproduce stacked bodies, nest material that raises animals above the plate, or tags at mixed heights and orientations in a huddle.
   - So competition in real huddles may be worse than on the bench.
4. **Bedding and the house over days.**
   - Moisture from urine and soiling, nest building inside the house, and uneven bedding depth all develop over the 4-day recordings.
   - The pilot uses fresh bedding at one depth; used bedding is optional.
5. **Dark-phase behaviour.**
   - Mice climb on the hopper, lid, bottle and house roof, run faster than 30 cm/s, jump, and change speed and posture within a step.
   - The sled moves at constant height and roughly constant speed.
   - So entry latency, exit tail and flicker of real movement can differ, especially for brief excursions, which are what tau decides about.
6. **Load across the board.**
   - In the home cage, 4-5 tags move over 8 antennas at once.
   - The pilot tests board- and group-level competition only in two static configurations (C3, C4).
7. **Time and drift.**
   - Each pilot day has one short session. It cannot estimate how often whole-session channel dropouts happen: 3 were confirmed in 120 board-recordings of Exp9.
   - It cannot measure drift over 4 days (temperature, humidity, cables).
   - Hardware and spare tags have aged since 2022-2024; E9 only checks that the cadence signature is unchanged.
8. **Coverage of boards and antennas.**
   - Only boards 2 and 4 are tested. Boards 1, 3 and 5 stay uncalibrated, including board 5's faster cadence.
   - The antenna-specific pattern on board 3 is inferred from board 4.
9. **Timing.**
   - Video and clock alignment is about ±0.1 s.
   - Latency resolution is limited by the read cadence (114.5-514.5 ms).
   - Static placements rely on the clapper and the sheet (about ±0.5 s), absorbed by the 5-s guards.
10. **Inferred configuration semantics.**
    - The phase-group reading of phase1/phase2 (0x96/0x69) and the link between pause 1000 and the 1-s event closing are inferred from the logs.
    - The pilot keeps the configuration fixed and stratifies by phase group, but cannot prove the mechanism. PhenoSys must confirm it.
11. **What no calibration can change.**
    - It cannot recover where Exp9 animals were while they were unread (about 63% of dark-phase time in the HMM pilot cage; 47% of light-phase time has positions older than 60 s).
    - It cannot say why a particular Exp9 animal-block was read little.
    - It cannot make same-reader co-assignment a measure of proximity; it can only state how far apart two tags read at one antenna can be.
    - It cannot validate any CON/RES/SUS difference, and none is estimated.

#### Optional later step: one filmed animal (not to be initiated now)

**Conditions.** Only if the lab decides it is justified, and only within existing licences.

**Design.**
- One tagged colony mouse, not a new experiment, alone in a production cage on one tested board.
- Top-down video with the PhenoSys PC clock in frame, and infrared for the dark phase.
- 2 h of light phase plus 2 h of dark phase.

**What it would add.** With one animal, identity is trivial, so the video gives ground-truth column and reader occupancy and transitions under real gait and posture. That would allow:
- the carry-forward and support-age error under real behaviour;
- real entry latency, exit tail and flicker for a moving animal, to check the tau chosen on the bench;
- detection in the house with nest material, and while feeding or drinking;
- the effect of posture and nest height.

**What it would still not cover.**
- Huddles and competition between animals: identity in multi-animal video would need marking.
- 4-day drift.
- Boards other than the one used.

**An alternative that needs no new animal work.** The existing regroup videos (`Y:/Tobi/E9_SIS/B<n>/Regroup/`, 118 of 120 cage x cage-change cells; first 1-2 h, daylight). They allow count-based or one-visible-animal comparisons, with the clock-alignment and identity limits stated in `docs/ANTENNA_MEASUREMENT_VALIDATION.md`.
