# RFID calibration pilot: logistics (one day, boards 2 and 4)

**Written for:** Tobias and the two on-site operators.
The pre-registered protocol, criteria (`acp_criteria_v1.0`), schedule (`schedule_one_day_v1`) and analysis code are in `docs/RFID_CALIBRATION_PILOT.md` and `docs/rfid_calibration/`. Nothing here changes them.

## 1. Status
- **Approved for preparation** (Tobias, 2026-10-14), subject to hardware and personnel.
- **Nothing is booked, no tag is placed and nothing is acquired** until two things are done:
  - every item of the checklist in section 2 is confirmed;
  - Tobias has decided the open options in section 6.2.
- The pilot is group-blind and involves no animal. It runs on the hash-pinned protocol, analysis code and pre-specified criteria, unchanged. Criteria are never changed after any data are seen; a change is a new version reported next to v1.0.
- **Scope of the results:**
  - They apply to boards 2 and 4 under bench conditions, with static or sled-moved tags.
  - Calibration with static tags bounds bench hardware behaviour only. No Exp9 location is validated by it.
  - A recently detected (supported) position is not a validated location.

## 2. Confirmation checklist (all ticked before booking)

**Hardware and rack**
- [ ] The rack with boards 1-5 (LabNetV2 rfid32 AM1-AM5, 192.168.137.11-15) is free of animals for one full day.
  - Boards 2 (reference) and 4 (least sensitive) work.
  - Boards 1, 3 and 5 record all day as known-empty controls (board 5 apart from the clapper).
- [ ] Five production cages (one per board) with lid, food hopper and filled bottle in production position. Same cage type, floor and lid as Exp9.
- [ ] Rack positions of the b2 and b4 cages confirmed: handling one cage keeps hands and metal at least 30 cm from the other.
- [ ] The clapper position (board 5, channel 7 = R4) can be reached without coming within 30 cm of b2 or b4.
- [ ] Substitute boards (3 for 4, 1 for 2): see section 6.2, item G. The runner cannot use a substitute as it stands.

**Configuration**
- [ ] PhenoSys PC runs PhenoSoft 1.0.8147.28429 with the production workbook:
  - per antenna: pause 1000, length 10, onlyValid false;
  - per board: phase1 96, phase2 69, phaseDuration 250.
- [ ] The per-batch SIS workbooks are retrieved first. The loaded workbook is archived with its sha256 and diffed against BATCH1_SIS_5cages.xlsx; the version row is recorded.
- [ ] Questions sent to PhenoSys (desk work; the analysis does not depend on the answers):
  - what phase1, phase2, phaseDuration and length mean;
  - the antenna loop size;
  - whether the boards are polled independently.

**Tags**
- [ ] 8 spare tags of the Exp9 type (the 041AF89 lot if spares remain): T1-T6, the clapper C and one spare.
  - The minimum is 6. With 6, the sentinels and C0/CE are dropped and board-level load (B6) stays unidentified.
- [ ] Every tag read once on the bench (pre-check). `tags.csv` (columns label, tag) prepared.

**Materials (about 5 person-hours)**
- [ ] Measured before anything is cut: production bedding depth, house outline over R5 (from a production photo), roof height.
- [ ] Two spacer boards cut from one rigid foam sheet, as thick as the bedding depth. Each carries:
  - a laminated 1-cm grid with labelled centres, quarter points and midpoints (coordinates, reader, channel);
  - the traced house outline with its entrance;
  - a well under the house with a foam insert;
  - registration edges for the acrylic REF frame (cut-outs at the R6 centre and at (75,116)).
- [ ] Two pre-weighed portions of fresh production bedding from one batch.
- [ ] One red production-model house; a foam stack of roof height; labelled 1-cm foam cubes with x/y/z slits; plastic tweezers; non-metallic putty.
- [ ] Plastic rail strip, low plastic sled, nylon string through the lid bars (pulled from at least 30 cm), metronome app.
- [ ] A closed box for idle tags (at least 1 m from the cages); non-metallic tools only; tape measure or calliper.

**Cameras and clock**
- [ ] Two top-down cameras (at least 30 fps, 60 preferred) on identical mounts over b2 and b4, recording all day.
- [ ] A millisecond clock display in every frame, or a PC-driven LED.
- [ ] The display-to-PC-clock offset is filmed at the start, the break and the end. A test run against the PC clock is done beforehand.
- [ ] A third camera for photos of the cage position and the plate edge.
- [ ] PC clock: no manual change on the day; any NTP sync is noted; PhenoSoft is started once.

**Storage**
- [ ] Raw folder `Raw Data/Behavior/RFID/calibration/<YYYYMMDD>/` beside MMMdata (it does not exist yet). A few GB free.
- [ ] The analysis outputs are created by the scripts: `<Behavior>/RANM/calibration/` and `<Behavior>/RANM/calibration_reports/`.
- [ ] Placement sheets pre-printed from `schedule_one_day_v1.csv`; digital templates with the required columns (section 8).

**People** (section 3)
- [ ] Operator and recorder, both trained on PhenoSoft, on site for the whole session (about 4.1 h plus arrival).
- [ ] Annotator named (about 2 h; recorder or analyst). Analyst (12-14 h; knows the repository). Tobias's review slot (2 h).

**Date**
- [ ] One animal-free rack day plus an alternative date.
- [ ] Not a daylight-saving change day. The EU change is Sunday 25 Oct 2026.
- [ ] No other session needs the rack or the PhenoSys PC that day.

**Pre-registration intact**
- [ ] Pinned hashes unchanged:
  - `ACP_CRITERIA_SHA256` c9343543...0446;
  - `ACP_SCHEDULE_SHA256` 2855780e...549e;
  - the Stage 00b board history of `raqc_20261014_6165adf`.
- [ ] Pilot code committed without local changes; `Testing/tests/test_antenna_calibration_pilot.R` passes.
- [ ] Tobias's choices on section 6.2 recorded. Any pre-data change is committed and pinned as a new version before booking.

## 3. Roles and hours

| role | who | tasks | hours |
|---|---|---|---|
| Operator | one person, the same all day and on any day 2 | places every tag on both boards; sled runs; house moves | 4.1 h on site |
| Recorder | second person | placement sheet and timer; clapper (touch board 5 channel 7 for about 2 s at every placement start and end, twice at every block start and board switch); writes the PC-clock time of every touch; runs the cameras; backup | 4.1 h on site |
| Preparation | operator and recorder | materials, camera and clock test, tag pre-check | about 5 person-hours |
| Annotator | to be assigned | 48 crossings, 24 visits and the survey sweeps, entered in PC-clock time | about 2 h |
| Analyst | knows the repository | pre-registered analysis, report, decision table | 12-14 h |
| Tobias | | options before booking; review; day-2 decision | 2 h review |

Total: about 29-31 person-hours.

## 4. Preparation timeline (D = session day)

| when | what | who |
|---|---|---|
| D-21 to D-15 | Desk work: PhenoSys questions; confirm rack positions, tags, cage, bedding depth, house outline and roof height. Tobias decides the options of section 6.2. | Tobias, operator |
| D-15 to D-14 | Only if Tobias chose a pre-data change: implement it as a new version (schedule, code or criteria), test, commit and pin. Otherwise confirm the pins. | analyst |
| D-14 | Book the rack day and an alternative date (checklist complete). | Tobias |
| D-10 to D-4 | Materials (spacer boards, grid, wells, REF frame, carriers, sled, rail and string, bedding portions). | operator, recorder |
| D-3 | Camera and clock test against the PC clock; tag pre-check; `tags.csv`; sheets printed; raw folder created; workbooks retrieved and diffed. | recorder, analyst |
| D-1 | Rack checked free of animals; materials packed; PC clock and NTP noted; cameras charged; idle-tag box ready. | operator |
| D | Session 08:30-12:36 (section 5). | operator, recorder |
| D, after | Backup and sha256 manifest. | recorder |
| D+1 to D+3 | Video annotation. | annotator |
| D+3 to D+7 | Pre-registered analysis and report (section 9). | analyst |
| D+7 to D+10 | Review and day-2 decision. | Tobias |

## 5. The day (planned clock times for an 08:30 start)

Times are the planned slot starts of the 106 pinned schedule rows, condensed into blocks and rounded to the minute. The sheet records actual PC-clock times.

| clock | min | block (schedule orders) | board | content |
|---|---|---|---|---|
| 08:30-08:50 | 0-20 | SETUP (1) | all | Cages on boards 1-5; spacer boards with well inserts in b2 and b4; cameras and clock displays; photos; start PhenoSoft once; archive the workbook (sha256) and version row; film the clock offsets |
| 08:50-09:00 | 20-30 | BASELINE (2) | all | No tags on any board (known-empty) |
| 09:00-09:05 | 30-35 | CHECK (3) | 2, 4, 5 | Channel check: T1 5 s on each antenna of b2 and b4; clapper C on b5 channel 7 (stop rule 1) |
| 09:05-09:15 | 35-45 | SURVEY (4) | 2, 4 | T1 (z) slid by hand at about 1 cm/s along both rows and the 4 columns, on video |
| 09:15-09:18 | 45-47.8 | V1 REF-start (5-6) | 2 | REF-z, REF-x (board 2, REF 1) |
| 09:18-09:23 | 47.8-53.3 | V1 T0 (7-11) | 2 | Tag QC: T2, T4, T5, T6, T3 at the REF-z spot |
| 09:23-09:39 | 53.3-68.9 | V1 P half 1 (12-18) | 2 | P09, P06, P12, P14 (5-min R5\|R6 flicker 09:28-09:34), P05, P01, P04. **P half 1: board 2 first** |
| 09:39-09:40 | 68.9-69.9 | SWITCH (19) | 2>4 | Carry T1 to b4; two clapper touches |
| 09:40-09:43 | 69.9-72.8 | V2 REF-start (20-21) | 4 | REF-z, REF-x (board 4, REF 1) |
| 09:43-09:58 | 72.8-88.4 | V2 P half 1 (22-28) | 4 | P06, P04, P12, P09, P01, P14 (flicker 09:51-09:57), P05. P half 1: board 4 second |
| 09:58-10:12 | 88.4-101.8 | V2 P half 2 (29-36) | 4 | P08, P13, P03, P11, P02, P10, P15, P07. **P half 2: board 4 first** |
| 10:12-10:17 | 101.8-107.1 | V2 M1 (37-40) | 4 | Rail on; slow passes R5>R8>R5 twice at about 5 cm/s (10:12); visits R5>R6>R5, dwell 1,2,5,1,5,2 s (10:15); rail off. **M half 1: board 4 first** |
| 10:17-10:20 | 107.1-109.9 | V2 REF-end (41-42) | 4 | REF-z, REF-x (board 4, REF 2) |
| 10:20-10:21 | 109.9-110.9 | SWITCH (43) | 4>2 | Carry T1 to b2; two clapper touches |
| 10:21-10:24 | 110.9-113.7 | V3 REF-start (44-45) | 2 | REF-z, REF-x (board 2, REF 2) |
| 10:24-10:37 | 113.7-127.1 | V3 P half 2 (46-53) | 2 | P11, P02, P13, P03, P15, P08, P07, P10. P half 2: board 2 second |
| 10:37-10:42 | 127.1-132.4 | V3 M1 (54-57) | 2 | Slow passes (10:38); visits, dwell 5,2,1,5,1,2 s (10:40). M half 1: board 2 second |
| 10:42-10:47 | 132.4-137.3 | V3 M2 (58-61) | 2 | Fast passes at about 20 cm/s (10:43); visits, dwell 1,2,1,2,5,5 s (10:45). **M half 2: board 2 first** |
| 10:47-10:56 | 137.3-146 | V3 H half 1 (62-67) | 2 | Insert out, bedding tamped, house on. ON: H01, H05 (roof). Lift. OFF: H06 (foam stack), H02. **H half 1: board 2 first** |
| 10:56-11:11 | 146-161 | BREAK (68) | - | House out of the cages; no tags anywhere; film the clock offsets |
| 11:11-11:16 | 161-166 | V4 M2 (69-72) | 4 | Fast passes; visits, dwell 5,1,1,2,2,5 s. M half 2: board 4 second |
| 11:16-11:25 | 166-174.7 | V4 H half 1 (73-78) | 4 | Bedding into the well, house on. ON: H01, H05. Lift. OFF: H06, H02. H half 1: board 4 second |
| 11:25-11:40 | 174.7-189.5 | V4 C run (79-84) | 4 | House on. C0 sentinels T5 (R8) and T6 (R4) 11:25; C1 +T1 in the house 11:27; C4 +T2-T4 11:29; CS T3, T4 to R6 11:35; CE sentinels alone 11:38. **C: fixed order, board 4 first** |
| 11:40-11:44 | 189.5-193.8 | V4 H half 2 (85-88) | 4 | Lift; H04 (HCz off); place the house; H03 (HCz on). **H half 2: board 4 first** |
| 11:44-11:47 | 193.8-197.2 | V4 REF-end (89-91) | 4 | House out (bedding stays in the well); REF-z, REF-x (board 4, REF 3) |
| 11:47-11:49 | 197.2-198.7 | SWITCH (92) | 4>2 | Carry the tags and the house to b2; house on its outline |
| 11:49-12:03 | 198.7-213 | V5 C run (93-97) | 2 | C0 11:49, C1 11:50, C4 11:53, CS 11:58, CE 12:01. C: board 2 second |
| 12:03-12:07 | 213-217.3 | V5 H half 2 (98-101) | 2 | Lift; H04; place the house; H03. H half 2: board 2 second |
| 12:07-12:11 | 217.3-220.7 | V5 REF-end (102-104) | 2 | House out; REF-z, REF-x (board 2, REF 3) |
| 12:11-12:21 | 220.7-230.7 | BASELINE (105) | all | No tags (known-empty) |
| 12:21-12:36 | 230.7-245.7 | BACKUP (106) | - | Stop the session once; film the clock offsets; copy the logs, workbook, sheets, photos and videos; sha256 manifest |

**Board order by halves.**

| programme | half 1 | half 2 | pattern |
|---|---|---|---|
| P (static) | b2 09:23, then b4 09:43 | b4 09:58, then b2 10:24 | ABBA |
| M (movement) | b4 10:12, then b2 10:37 | b2 10:42, then b4 11:11 | BAAB (balanced; opposite to the documented rule) |
| H (house) | b2 10:47, then b4 11:16 | b4 11:40, then b2 12:03 | ABBA |
| C (competition) | one continuous run per board: b4 11:25, then b2 11:49 | | fixed, not counterbalanced |

**REF times.**
- Board 2: 09:15, 10:21, 12:08.
- Board 4: 09:40, 10:17, 11:44.
- REF-x follows REF-z by 1-2 min.
- The third REF of each board is taken with bedding in the R5 well (section 6.2, item E).

**Totals.**
- Board 2: 81.0 slot-min, 58.1 recorded-min.
- Board 4: 76.1 slot-min, 54.0 recorded-min.
- About 14.4 known-empty board-hours.

## 6. Approved priorities: coverage and gaps

### 6.1 How the priorities are covered

| priority | where in the day | units and criteria |
|---|---|---|
| Single-read vs event reconstruction at known positions | P halves (09:23-10:37); REF-x 3 times per board; M passes and visits (10:12-11:16) | Criterion (a): D_col over P09, P10, P11 and REF-x (4 per board), 24 crossings and 12 short visits per board, tau 2 s. D_ctr non-inferiority at the centres. D_rdr adds P12, P13 (reader level only). |
| Antenna boundaries | Quarter points P09-P13; midpoints P14 (5-min R5\|R6 flicker) and P15; crossings R5\|R6, R6\|R7 and R7\|R8 at two speeds | B11 (tau), B12 (entry latency, exit tail), B13 (crosstalk) |
| The house at R5 | H blocks on both boards; both C runs with the house on | B8 shielding (HCx and HCz, both boards); B9 roof vs foam stack |
| Multiple-tag competition | C runs 11:25 (b4) and 11:49 (b2) | B4 (C1 vs C4), B5 (CS vs C4), B6 (sentinels), B7 (delta*) |
| Boards 2 and 4 under comparable conditions | One session; the same T1, slots, sentinels, operator, house, bedding batch, spacer sheet and cameras; seeded order within blocks; ABBA by halves; REF 3 times per board | B10 board call with REF repeatability and drift adjustment. Over the 21 paired static placements, board-4 time minus board-2 time is -0.3 min on average. |

### 6.2 Where the frozen schedule does not control order or coverage

These are stated now; nothing is changed. Items marked **decide before booking** need Tobias's choice. Any change would be a pre-data new version (e.g. `schedule_one_day_v2`, a code change or `acp_criteria_v1.1`), committed and hash-pinned before booking. Otherwise the item stands as a reported limitation.

**A. Competition run order.**
- C runs board 4 first (11:25), then board 2 (11:49). It is not counterbalanced.
- The nearest REF placements before C are at 10:17 (b4) and 10:21 (b2), so the REF bracketing is loose.
- Competition is tested only at n = 1 vs n = 4: one rotation, all tags in z, house only. Tag and slot cannot be separated from board unless D6 runs rotation b.
- Limitation; reported with the board comparison of B4-B6.
- Option: a second C run per board in ABBA order. That is about +30 min and needs a schedule and code change. Not recommended for day 1.

**B. Movement order.** M is counterbalanced, but in the opposite direction to the documented rule ("half 1 board 2 first"). This is a documentation correction only; no decision is needed.

**C. No column-boundary movement.**
- There are no R1<->R5 or R2<->R6 crossings or visits, although R1->R5 is 11% of the reassigned active seconds.
- D_col's static units are row-boundary quarter points only; P12/P13 enter at reader level only. The junction, the R3/R7 centres and the R1|R5 midpoint are untested.
- Limitation.
- Option (decide before booking): add column passes and visits. This changes the D_col units, so it needs a new criteria version before any data.

**D. House positions are thin.**
- Only the R5 centre inside the house (x, z) plus the roof is tested: no back corner, no entrance, fresh bedding only, no nest material.
- P and M run with the house absent, so exits from a house in place are untested.
- The roof pairs are step 1 of the drop order.
- Limitation.

**E. REF well-state confound (decide before booking).**
- The third REF per board (11:44 b4, 12:08 b2) is taken with bedding in the R5 well; the earlier REFs had the foam insert in place.
- This is balanced across boards, but late/early REF-x (D3b) is confounded with the well state. It could trigger D3 (a day-2 bridge block) for a reason that is not drift.
- Options:
  - (a) accept, and interpret D3b with this caveat;
  - (b) a `schedule_one_day_v2` that restores the insert before REF-end (about +1-2 min per board), with the schedule hash re-pinned in the code before booking.

**F. T0 tag QC runs on board 2 only.** Limitation (the same physical tags are used on both boards).

**G. Substitute boards cannot be set (decide before booking).**
- Stop rule 1 says: use board 3 for 4 or board 1 for 2. But the runner hard-codes boards 2 and 4 (`ACP_BOARDS`) and has no `--boards` argument.
- A substitution would need a code change after the data.
- Options:
  - (a) add a substitute-board argument now, pre-data, tested and pinned;
  - (b) treat a channel check that still fails after one restart as a stop, and move to the alternative date.

**H. Unplanned session restart (decide before booking).**
- The runner needs exactly one log with detections.
- The per-session stratification of stop rule 5 is not implemented; only `n_sessions` is reported.
- Options:
  - (a) implement it pre-data;
  - (b) treat an unplanned restart that writes a second log as a stop, and repeat on the alternative date.

**I. Day-2 bridge pooling (DB1/DB2) is not implemented.** It appears only in the recommendation text. It must be coded and committed with the day-2 criteria before any day 2. This does not block booking day 1.

**J. delta* of 30 s.** 30 s is a B7 candidate but not an srs_v1.1 age class (2, 10, 60, 300 s). That result would need a new reconstruction version. Noted for the conditional decisions (section 10).

## 7. On-day stop rules and bench rules

**During every analysed window:**
- hands and any metal stay at least 30 cm from the cage;
- the lid is closed;
- idle tags are in the closed box at least 1 m away.

**Clapper.** At every placement start and end, and twice at every block start and board switch. The recorder notes each PC-clock time.

**Stop rules:**
1. **Channel check fails** (an antenna silent for 5 s with T1 on it): restart the session once, before any placement, and repeat the check. If it still fails, see 6.2 G (substitute board or alternative date, as decided before booking). Record a deviation.
2. **Tag QC (T0):**
   - a tag outside +-20% of the tag median is not used in C (use the spare; it keeps the label);
   - if T1 is outside +-10%, swap T1 for the tag nearest the median and repeat the V1 REF.
3. **REF-z on board 2 below 1.5 reads/s:** check the plate seating and the cage position.
4. **Disturbance** (hands or metal too close, lid open, carrier displaced):
   - repeat the placement at the end of the same board-block as `<order>r`, once only;
   - never overwrite;
   - the paired placement on the other board is not repeated.
5. **Unplanned session restart:** log the time and repeat the channel check. Then follow the choice made for 6.2 H.

**Drop order if the day is more than 20 min behind at the break** (always both boards):
1. roof pairs H05/H06 (-6.7 min);
2. CS shortened to 90 s analysed (-2 min);
3. 4 visits per half instead of 6 (-1.3 min).

Never drop a block on one board only.

## 8. Data handling
- **Raw folder** `Raw Data/Behavior/RFID/calibration/<YYYYMMDD>/` holds:
  - the MultiMouseMonitor log (exactly one log with detections; initScan files are ignored);
  - the workbook copy and its version row;
  - `placement_sheet.csv`, `annotation_movement.csv`, `annotation_survey.csv`, `tags.csv`;
  - photos and videos;
  - `sha256_manifest.csv` (columns file, sha256). The runner checks every listed file against it.
- **`placement_sheet.csv`:**
  - columns placement_id, order, board, pair_id, status (executed or dropped), pc_start, pc_end, clapper_ok, drop_step, deviation (optional: tags);
  - times on the PC clock as `%Y-%m-%d %H:%M:%OS`;
  - placement_id is the schedule order (plus `r` for the one allowed repeat);
  - every row with a tag in a cage (orders 3-104 with tags, including CHECK and SURVEY) is recorded as executed or dropped;
  - include BASELINE and BREAK rows with their times.
- **`annotation_movement.csv`** (placement_id, kind, t_pc, from_reader, to_reader) and **`annotation_survey.csv`** (board, sweep, t_pc, x, y): entered already on the PC clock, with the filmed display offsets applied while annotating.
- **`tags.csv`:** label (T1-T6, C, spare) and tag. A replacement tag keeps its label.
- Never edit a raw file after the manifest has been written. A correction goes into a new file with a note.
- Analysis outputs are written only to `<Behavior>/RANM/calibration/<run_id>/` (pre-registered runner) and `<Behavior>/RANM/calibration_reports/runs/<report_id>/` (report).

## 9. After the pilot
1. **Same day:** backup, sha256 manifest, deviation notes.
2. **Video annotation** (about 2 h, PC-clock time).
3. **Pre-registered analysis** (committed, unchanged code; from the repository root):
   `Rscript Testing/audits/analyse_rfid_calibration_pilot.R --date=<YYYYMMDD>`
   - Output: `<Behavior>/RANM/calibration/acp_v1.0_<date>_<commit>/`, with decisions.csv, day2_recommendation.csv, tables/ and manifest/.
4. **Report:**
   `Rscript Testing/audits/report_rfid_calibration_pilot.R --acp-run=<absolute path of the run from step 3>`
   - Output: `<Behavior>/RANM/calibration_reports/runs/<report_id>/`:
     - README.txt;
     - status_by_decision.csv;
     - spec_consequences.csv (one row per specification item);
     - day2_trace.csv (the projection test per threshold);
     - day2_trace_check.csv;
     - uncertain_without_day2.csv.
   - It verifies the acp run's manifest and pinned hashes first and never writes into the acp run.
   - It stops if any protected output changes while it runs. Start it when no other RANM run is in progress.
5. **Decision table for Tobias**, taken from spec_consequences.csv and the README:
   - resolved decisions;
   - uncertain decisions, and whether a day-2 trigger applies to them;
   - decisions not obtained;
   - the day-2 verdict as computed by the predefined rule (`acp_decide_day2`). It is a recommendation only; Tobias decides.
   - The frozen criteria are not changed on the basis of results.
6. **If day 2 runs:**
   - only the triggered blocks, under the day-1 rules;
   - seed 20261015, at most 4 h on site;
   - always the bridge P09, P11, P13, P06 plus REF on both boards;
   - the day-2 criteria and the pooling test (DB1/DB2) are committed before day 2.

## 10. Decisions that remain conditional on calibration

Wording rules for every row:
- No reconstruction is chosen by group effects, predictive performance or significance.
- Bases that are not resolved are retained as sensitivity analyses; their estimates are never described as bounds on true physical occupancy.
- Recently detected is not validated location.

| decision (spec v1.1) | criteria | possible outcomes | consequence | if inconclusive |
|---|---|---|---|---|
| Reconstruction basis of the basis-sensitive measures (column occupancy, R5 share, reader changes) | (a) A20-A42; D1 | READ; EVENT; TIE equivalent; TIE inconclusive; not obtained | READ: single-read is primary in the revised Stage 19; event-based stays the historical reference and a sensitivity analysis. EVENT: event-based is primary; single-read is a sensitivity analysis. TIE equivalent: no winner; both reported as sensitivity analyses. | Both are retained side by side as sensitivity analyses: single-read stays provisional, event-based the historical reference. Day 2 (D1) only if the projection test holds at -0.05, 0 or +0.05. Not obtained: D2. |
| Basis of the eight-reader tables | (a) A50 | as above, at reader level | Selects the basis of the reader-level sensitivity, QC and resource-location tables only | Both bases in those tables; no day-2 trigger |
| Resolution | B2 | reader; column | Reader: reopens decision 3, and Tobias decides. Column: decision 3 confirmed (four columns primary, eight readers for QC, sensitivity and resource location). | Four columns stay primary; with a tied basis, reader level must hold under both bases |
| "Fresh" (recently detected) age class | B3 | 2 s; 10 s; no candidate; not obtained | Sets the recently detected class: a bench detection property, not validated location | Gmax reported; 2 s and 10 s stay descriptive; no day 2 (D2 only if not obtained) |
| Carry-forward age cap | B7 | 10, 30, 60 or 300 s; no candidate | 60 s: the 60-s cap is consistent with bench detection. 300 s: only the 300-s cap is consistent with bench detection. 10 s: a shorter cap would also be consistent. Unrestricted LOCF stays the historical reference; the 60-s and 300-s caps stay sensitivity analyses. 30 s: needs a new reconstruction version. | LOCF (historical reference), 60 s and 300 s are all reported with the unknown share, as sensitivity analyses of the age cap; no day-2 trigger of its own |
| Flip-filter tau | B11; D5 | tau* <= 2 s; tau* > 2 s; range; not decidable | <= 2 s: tau 2 s retained. > 2 s: reader-change rates (S2) stay descriptive at column level; secondary statistics at tau*. | Range: tau 2 s stays frozen and the range is reported (no day 2). Not decidable: D5. |
| Competition in one field (house) | B4; D6 | time-sharing; collision; capture or starvation; intermediate | Read-density denominator; unread house time is not absence; co-assignment stays excluded; house occupancy is reported with its unknown share | Intermediate: D6 only if the projection test holds (TR 0.7, 0.85, 1.15; MS 0.35, 0.5); capture on one board only triggers D6 |
| Competition across fields | B5 | per antenna; spans R5/R6; undecided | Co-location count or tags-per-board as the read-competition covariate | n_tags_present stays descriptive; no day-2 trigger |
| Board-level load | B6; D6 | board level; none; undecided | Whether n_tags_present is physically justified for read-dependent measures | D6 only if the projection test holds at SR 0.85; otherwise a design covariate without a calibrated mechanism |
| House shielding | B8 | no shielding; shielding; partial | Whether R5 read densities are comparable; the bias statement for house occupancy | Partial: ratio and CI reported, caveat on R5 densities; no day 2 |
| Roof | B9 | B1 classes per board | Whether unread house time can be roof time | Border or dropped: stays open; no day 2 |
| Board call | B10; D3, D4 | difference; no difference; undecided | Interpretation of the board covariate (hardware vs cage, rack or occupancy); the section 6 adjustment is unchanged | D4 if the projection test holds (R_board 0.9, 1, 1.11; co-read 0.7); D3 if REF is not repeatable |
| Footprint wording | B1 | reliable, border, unread per placement | Physical wording of "assigned to reader k" (S3, S6) | Descriptive; border is reported as border |
| QC rules | B13-B15 | co-read expectations; real tags or an events rule; tag eligibility | Inputs to a new aqc_rules version | QC inputs, not day-2 triggers (D2 if missing) |
| Transfer to Exp9 | B16; D7 | transfer; caveat | A caveat is attached to every conclusion | Never a reason for day 2 |