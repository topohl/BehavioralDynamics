# RFID calibration pilot: logistics (one day, boards 2 and 4)

**Written for:** Tobias and the two on-site operators.
The pre-registered protocol, criteria (`acp_criteria_v1.2`), schedule (`schedule_one_day_v3`) and analysis code are in `docs/RFID_CALIBRATION_PILOT.md` and `docs/rfid_calibration/`. Schedule v3 and criteria v1.2 amend v2 and v1.1 before any data and before booking (Tobias, 2026-10-15, second decision of that day; section 6.2). Schedules v1 and v2 and criteria v1.0 and v1.1 are kept for provenance.

## 1. Status
- **Booking approved** (Tobias, 2026-10-15), subject to hardware and personnel availability, once the amended schedule is practical for one rack day. It is: 5.4 h on site, 08:30-13:54 (section 5).
- **Proposed date:** Tuesday 3 November 2026; alternative Tuesday 10 November 2026 (section 11). The booking request in section 11 has not been sent yet.
- **Nothing is acquired** until every item of section 2 is ticked. Day 2 is never scheduled automatically; Tobias decides after the day-1 report.
- The pilot is group-blind and involves no animal. It runs on the hash-pinned protocol, analysis code and criteria.
  - Criteria v1.2 keep every endpoint definition and decision threshold of v1.1. They add rows (K, KA, HX) for the added measurements and make one technical correction (A04, section 6.2), which changes the values, not the definitions, of the basis-sensitive endpoints.
  - Schedule v3 shortens the v1.1 competition periods (section 6.2): their definitions are unchanged, but they rest on less data than in v2.
  - Criteria are never changed after any data are seen; a later change is a new version reported next to the earlier one.
- **Scope of the results:**
  - They apply to boards 2 and 4 under bench conditions, with static or sled-moved tags.
  - Calibration with static tags bounds bench hardware behaviour only. No Exp9 location is validated by it.
  - Static tags do not establish mouse huddling or physical proximity.
  - A recently detected (supported) position is not a validated location.

## 2. Confirmation checklist (all ticked before the booking is confirmed)

**Hardware and rack**
- [ ] The rack with boards 1-5 (LabNetV2 rfid32 AM1-AM5, 192.168.137.11-15) is free of animals and of any other study for one full day.
  - Boards 2 (reference) and 4 (least sensitive) work.
  - Boards 1, 3 and 5 record all day as known-empty controls (board 5 apart from the clapper).
- [ ] Five production cages (one per board) with lid, food hopper and filled bottle in production position. Same cage type, floor and lid as Exp9.
- [ ] Rack positions of the b2 and b4 cages confirmed: handling one cage keeps hands and metal at least 30 cm from the other.
- [ ] The clapper position (board 5, channel 7 = R4) can be reached without coming within 30 cm of b2 or b4.
- [ ] Substitute boards (3 for 4, 1 for 2): only with an authorization file signed off before use (section 6.2 G). Otherwise a failed board means the alternative date.

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

**Materials (about 6 person-hours)**
- [ ] Measured before anything is cut: production bedding depth, house outline over R5 (from a production photo), roof height.
- [ ] From the traced outline, note whether the house-extension positions (25,116) and (0,87) lie inside the house and (75,116) outside it, or on its wall. The positions are fixed by their coordinates; the note only labels them in the report. If (75,116) turns out to lie inside the house, or (25,116) outside it, tell the analyst before the day: the schedule can still be amended and re-pinned.
- [ ] Two spacer boards cut from one rigid foam sheet, as thick as the bedding depth. Each carries:
  - a laminated 1-cm grid with labelled centres, quarter points and midpoints (coordinates, reader, channel);
  - the traced house outline with its entrance;
  - a well under the house with a foam insert;
  - registration edges for the acrylic REF frame (cut-outs at the R6 centre and at (75,116)).
- [ ] Two pre-weighed portions of fresh production bedding from one batch.
- [ ] One red production-model house; a foam stack of roof height; labelled 1-cm foam cubes with x/y/z slits; plastic tweezers; non-metallic putty.
- [ ] **Rail and strings for both rows** (schedule v3):
  - one plastic rail strip per cage, with a low plastic sled;
  - nylon strings threaded through the lid bars before the day, for the upper row (R5-R8) and the lower row (R1-R4), each pulled from at least 30 cm;
  - centre marks on the rail at x = 0, 100, 200 and 300;
  - marks on the strings at one reader spacing (100 grid units), so that one pull moves the sled from one centre to the next.
- [ ] A metronome app, and a timer that beeps 25 s after the start touch and then every 22 s (the K steps at 25, 47, 69, 91, 113 and 135 s).
- [ ] A closed box for idle tags (at least 1 m from the cages); non-metallic tools only; tape measure or calliper.

**Cameras and clock**
- [ ] Two top-down cameras (at least 30 fps, 60 preferred) on identical mounts over b2 and b4, recording all day.
  - The M passes, the visits and the K steps are annotated from these videos.
- [ ] A millisecond clock display in every frame, or a PC-driven LED.
- [ ] The display-to-PC-clock offset is filmed at the start, the break and the end. A test run against the PC clock is done beforehand.
- [ ] A third camera for photos of the cage position, the plate edge and the house-extension positions.
- [ ] PC clock: no manual change on the day; any NTP sync is noted; PhenoSoft is started once.

**Storage**
- [ ] Raw folder `Raw Data/Behavior/RFID/calibration/<YYYYMMDD>/` beside MMMdata (it does not exist yet). A few GB free.
- [ ] The analysis outputs are created by the scripts: `<Behavior>/RANM/calibration/` and `<Behavior>/RANM/calibration_reports/`.
- [ ] Placement sheets pre-printed from `schedule_one_day_v3.csv` (117 rows with a tag in a cage, plus BASELINE and BREAK); digital templates with the required columns (section 8).

**People** (section 3)
- [ ] Operator and recorder, both trained on PhenoSoft, on site for the whole session (5.4 h plus arrival).
- [ ] Annotator named (about 2.5 h; recorder or analyst). Analyst (14-16 h; knows the repository). Tobias's review slot (2 h).

**Date**
- [ ] The rack day and an alternative date confirmed (section 11).
- [ ] Not a daylight-saving change day. The EU change is Sunday 25 Oct 2026, so the proposed dates are clear of it.
- [ ] No other session needs the rack or the PhenoSys PC that day.

**Pre-registration intact**
- [ ] Pinned hashes unchanged (content sha256, line endings normalised):
  - `ACP_CRITERIA_SHA256` 12c0b2d2...0500 (acp_criteria_v1.2);
  - `ACP_SCHEDULE_SHA256` e61c7d6a...341b (schedule_one_day_v3);
  - `ACR_STATUS_SHA256` 921aeaac...09a5 (report mapping acp_report_status_v1.1);
  - the Stage 00b board history of `raqc_20261014_6165adf`.
  - The previous version of this list still showed the v1/v1.0 pins; corrected here.
- [ ] Pilot code committed without local changes. `Testing/tests/test_antenna_calibration_pilot.R` and `test_antenna_calibration_report.R` pass.
- [ ] Any further pre-data change is committed and pinned as a new version before the day.

## 3. Roles and hours

| role | who | tasks | hours |
|---|---|---|---|
| Operator | one person, the same all day and on any day 2 | places every tag on both boards; sled runs and K steps; house moves | 5.4 h on site |
| Recorder | second person | placement sheet and timer (including the K step calls); clapper (touch board 5 channel 7 for about 2 s at every placement start and end, twice at every block start and board switch); writes the PC-clock time of every touch; runs the cameras; backup | 5.4 h on site |
| Preparation | operator and recorder | materials (rails and strings for both rows), camera and clock test, tag pre-check | about 6 person-hours |
| Annotator | to be assigned | 120 crossings (per board: 24 in the passes, 24 in the 12 visits, 12 in the K steps) and the survey sweeps, entered in PC-clock time | about 2.5 h |
| Analyst | knows the repository | pre-registered analysis, report, decision table | 14-16 h |
| Tobias | | review; day-2 decision | 2 h review |

Total: about 35-38 person-hours.

**Order of placing and touching.** The operator places the tag (or sets the sled at its first centre), retreats at least 30 cm, and only then does the recorder make the start touch. The 5-s guards then cover only the retreat. Criteria v1.2 (A04) make the analysis independent of this order: an event in progress at the start counts as an observation at the start.

## 4. Preparation timeline (proposed D = Tuesday 3 November 2026)

| when | date | what | who |
|---|---|---|---|
| D-19 to D-14 | Thu 15 to Tue 20 Oct | Desk work: send the booking request (section 11); PhenoSys questions; confirm rack positions, tags and cages; measure bedding depth, house outline and roof height. | Tobias, operator |
| D-14 | Tue 20 Oct | Booking confirmed for the day and the alternative date (10 Nov). | Tobias |
| D-8 to D-5 | Mon 26 to Thu 29 Oct | Materials: spacer boards, grid, wells, REF frame, carriers, rails and strings for both rows, sled, bedding portions. | operator, recorder |
| D-4 | Fri 30 Oct | Camera and clock test against the PC clock; K timer test; tag pre-check; `tags.csv`; sheets printed; raw folder created; workbooks retrieved and diffed. | recorder, analyst |
| D-1 | Mon 2 Nov | Rack checked free of animals; materials packed; PC clock and NTP noted; cameras charged; idle-tag box ready. | operator |
| D | Tue 3 Nov | Session 08:30-13:54 (section 5); backup and sha256 manifest. | operator, recorder |
| D+1 to D+3 | Wed 4 to Fri 6 Nov | Video annotation. | annotator |
| D+3 to D+7 | Fri 6 to Tue 10 Nov | Pre-registered analysis and report (section 9). The computation itself takes minutes; a crash of the analysis PC (recurring bluescreens) only costs a rerun. | analyst |
| D+7 to D+10 | Tue 10 to Fri 13 Nov | Review; decisions; any day 2 only on Tobias's decision. | Tobias |

If the 3 November is not available, every date moves by the same number of days. The analysis does not depend on the date.

## 5. The day (planned clock times for an 08:30 start; schedule v3)

Times are the planned slot starts of the 160 rows of `schedule_one_day_v3.csv`, condensed into blocks and rounded to the
minute. The sheet records actual PC-clock times. **New in v3** marks the added content.

| clock | min | visit and block | board | content |
|---|---|---|---|---|
| 08:30-08:50 | 0-20 | SETUP | all | Cages on boards 1-5; spacer boards with well inserts in b2 and b4; cameras and clock displays; photos; start PhenoSoft once; archive the workbook (sha256) and version row; film the clock offsets |
| 08:50-09:00 | 20-30 | BASELINE | all | No tags on any board (known-empty) |
| 09:00-09:05 | 30-35 | CHECK | 2, 4, 5 | Channel check: T1 5 s on each antenna of b2 and b4; clapper C on b5 channel 7 (stop rule 1) |
| 09:05-09:15 | 35-45 | SURVEY | 2, 4 | T1 (z) slid by hand at about 1 cm/s along both rows and the 4 columns, on video |
| 09:15-09:17 | 45-47.8 | V1 REF-start | 4 | REF-z, REF-x (board 4, REF 1) |
| 09:17-09:33 | 47.8-63.5 | V1 P half 1 | 4 | P09, P06, P12, P14 (5-min R5\|R6 flicker), P05, P01, P04. **P half 1: board 4 first** |
| 09:33-09:38 | 63.5-68.3 | V1 M half 1 | 4 | Rail on the upper row; slow passes R5>R8>R5 twice at about 5 cm/s; short visits R5>R6>R5. **M half 1: board 4 first** |
| 09:38-09:42 | 68.3-72.5 | V1 K half 1 | 4 | **New in v3.** Rail to the lower row; step transitions R1>R2>R3>R4>R3>R2>R1 (steps at 25, 47, ... 135 s); rail off. **K half 1: board 4 first** |
| 09:42-09:43 | 72.5-73.5 | SWITCH | 4>2 | Carry T1 to b2; two clapper touches |
| 09:43-09:46 | 73.5-76.3 | V2 REF-start | 2 | REF-z, REF-x (board 2, REF 1) |
| 09:46-09:51 | 76.3-81.7 | V2 T0 | 2 | Tag QC: T2, T4, T5, T6, T3 at the REF-z spot (T1 = this REF-z) |
| 09:51-10:07 | 81.7-97.4 | V2 P half 1 | 2 | P09, P04, P06, P05, P12, P14, P01. P half 1: board 2 second |
| 10:07-10:16 | 97.4-106.3 | V2 M and K half 1 | 2 | Slow passes; visits; rail to the lower row; K-lower; rail off. Board 2 second |
| 10:16-10:29 | 106.3-119.7 | V2 P half 2 | 2 | P08, P15, P02, P13, P03, P11, P07, P10. **P half 2: board 2 first** |
| 10:29-10:37 | 119.7-127.6 | V2 M and K half 2 | 2 | Fast passes at about 20 cm/s; visits; **new in v3:** step transitions R5>R6>R7>R8>R7>R6>R5 on the upper row; rail off. **M and K half 2: board 2 first** |
| 10:37-10:38 | 127.6-128.6 | SWITCH | 2>4 | Carry T1 to b4; two clapper touches |
| 10:38-10:53 | 128.6-143.6 | BREAK | - | No tags anywhere; film the clock offsets. Drop decision (section 7) |
| 10:53-10:56 | 143.6-146.4 | V3 REF-start | 4 | REF-z, REF-x (board 4, REF 2) |
| 10:56-11:09 | 146.4-159.8 | V3 P half 2 | 4 | P03, P13, P11, P15, P08, P02, P07, P10. P half 2: board 4 second |
| 11:09-11:17 | 159.8-167.7 | V3 M and K half 2 | 4 | Fast passes; visits; K-upper; rail off. Board 4 second |
| 11:17-11:31 | 167.7-181.3 | V3 H half 1 | 4 | Insert out, bedding tamped, house on. ON: H11 (HSy, new), H01, H05 (roof), H13 (HOx, new). Lift. OFF: H06 (foam stack), H14 (HOx), H02, H12 (HSy). **H half 1: board 4 first** |
| 11:31-11:49 | 181.3-199.9 | V3 C run a | 4 | House on. C0 sentinels T5 (R8), T6 (R4); lift, T1 into slot 1, set down: C1; **new: lift; C1o; +T2-T4 in slots 2-4; C4o; set down:** C4; lift, T3 and T4 to R6, set down: CS; CE sentinels alone. **Ca: board 4 first** |
| 11:49-11:51 | 199.9-201.4 | SWITCH | 4>2 | Carry the tags and the house to b2 |
| 11:51-11:54 | 201.4-204.3 | V4 REF-start | 2 | REF-z, REF-x (board 2, REF 2), insert still in the well |
| 11:54-12:07 | 204.3-217.9 | V4 H half 1 | 2 | Insert out, bedding tamped, house on. ON: H05, H13, H11, H01. Lift. OFF: H14, H06, H02, H12. H half 1: board 2 second |
| 12:07-12:26 | 217.9-236.5 | V4 C run a | 2 | As on b4. Ca: board 2 second |
| 12:26-12:44 | 236.5-254.6 | V4 C run b | 2 | The same run again. **Cb: board 2 first** |
| 12:44-12:53 | 254.6-263.9 | V4 H half 2 | 2 | Lift; OFF: H04 (HCz), H08 (HEx, new), H10 (HEy, new); place the house; ON: H07 (HEx), H03 (HCz), H09 (HEy). **H half 2: board 2 first** |
| 12:53-12:57 | 263.9-267.2 | V4 REF-end | 2 | House out (bedding stays in the well); REF-z, REF-x (board 2, REF 3) |
| 12:57-12:58 | 267.2-268.7 | SWITCH | 2>4 | Carry the tags and the house to b4; house on its outline |
| 12:58-13:16 | 268.7-286.9 | V5 C run b | 4 | The same run. Cb: board 4 second |
| 13:16-13:26 | 286.9-296.1 | V5 H half 2 | 4 | Lift; OFF: H10, H04, H08; place the house; ON: H07, H09, H03. H half 2: board 4 second |
| 13:26-13:29 | 296.1-299.5 | V5 REF-end | 4 | House out; REF-z, REF-x (board 4, REF 3) |
| 13:29-13:39 | 299.5-309.5 | BASELINE | all | No tags (known-empty) |
| 13:39-13:54 | 309.5-324.5 | BACKUP | - | Stop the session once; film the clock offsets; copy the logs, workbook, sheets, photos and videos; sha256 manifest |

**Board order.** Every programme runs 4-2-2-4 by halves, and the competition run twice per board:

| programme | first half / run | second half / run | pattern |
|---|---|---|---|
| P (static) | b4 09:17, then b2 09:51 | b2 10:16, then b4 10:56 | 4-2-2-4 |
| M (movement) | b4 09:33, then b2 10:07 | b2 10:29, then b4 11:09 | 4-2-2-4 |
| K (column-boundary steps; new) | lower row: b4 09:38, then b2 10:12 | upper row: b2 10:34, then b4 11:14 | 4-2-2-4 |
| H with HX (house; HX new) | b4 11:19, then b2 11:55 | b2 12:45, then b4 13:17 | 4-2-2-4 |
| C with C1o, C4o (competition; house-lifted periods new) | Ca: b4 11:31, then b2 12:08 | Cb: b2 12:26, then b4 12:58 | 4-2-2-4; the two runs of a board are pooled per board |

**Time-of-day balance.** Mean time of day of the measured rows, in minutes from the start (board 2 / board 4):

| programme | P | M | K | H | HX | C1o, C4o | REF |
|---|---|---|---|---|---|---|---|
| board 2 / board 4 | 101.3 / 107.4 | 111.1 / 114.2 | 114.9 / 118.0 | 223.0 / 204.3 | 244.2 / 252.4 | 233.6 / 231.4 | 180.9 / 162.8 |

Over the 29 paired static placements (P, H and HX), board-4 time minus board-2 time is -0.3 min on average; board 2 comes first in 14 of 29.

**REF times.** Board 4: 09:15, 10:53, 13:26. Board 2: 09:43, 11:51, 12:54. REF-x follows REF-z by 1-2 min. The third REF of each board is taken with bedding in the R5 well (6.2 E).

**Totals.** Board 2: 120.0 slot-min, 85.3 recorded-min. Board 4: 114.5 slot-min, 81.1 recorded-min. On site 324.5 min (5.4 h).

**Practical for one rack day.**
- The session fits in a morning: 08:30-13:54, including set-up, two baselines, the break and the backup. The afternoon stays free as a buffer for repeats (stop rule 4) or a late start.
- v3 adds 48 min to v2 (+17%):
  - K: +13.2 min;
  - HX (four pairs): +20.0 min;
  - the house-lifted periods and the same lift and set-down handling before every house-on period with tags: +22.0 min.
- Shortening C1, C4 and CS in each of the four competition runs returns 7.0 min. This reallocates time from the competition repetitions doubled in v2, and the v1.1 competition endpoints rest on less data than in v2:

  | per board, analysed | v1 | v2 | v3 |
  |---|---|---|---|
  | C1 | 120 s | 240 s | 180 s |
  | C4 | 300 s | 600 s | 480 s |
  | CS | 150 s | 300 s | 240 s |
  | 10-s blocks of B4-B6 (C1 / C4 / CS) | 12 / 30 / 15 | 24 / 60 / 30 | 18 / 48 / 24 |

- The second half runs 3 h without a scheduled break. If the operators need one, a pause of up to 10 min is allowed at the V4/V5 board switch (12:57), with no tag in a cage. It is noted in the deviation field of the next placement (not as an extra sheet row); the actual PC times absorb it, and the day ends by 14:05.
- If the day is behind at the break, the drop order (section 7) saves up to 13.7 min, always on both boards.

## 6. Approved priorities: coverage and gaps

### 6.1 How the priorities are covered

| priority | where in the day | units and criteria |
|---|---|---|
| Single-read vs event reconstruction at known positions | P halves; REF-x 3 times per board; M passes and visits | Criterion (a): D_col over P09, P10, P11 and REF-x (4 per board), 24 crossings and 12 short visits per board, tau 2 s. D_ctr non-inferiority at the centres. D_rdr adds P12, P13 (reader level only). |
| **Column-boundary transitions (new)** | K steps: lower row in half 1, upper row in half 2, 4-2-2-4 | 12 held, timed transitions per board across all six column boundaries, both directions. D_K (single-read minus event correct-column share, ±10 s) and the overall basis verdict K20; switching latency K30; hold cores K31; detection age K32; column adequacy KA1-KA8. |
| Antenna boundaries | Quarter points P09-P13; midpoints P14 (5-min R5\|R6 flicker) and P15; pass crossings at two speeds | B11 (tau), B12 (entry latency, exit tail), B13 (crosstalk) |
| The house at R5 | H blocks with the HX pairs on both boards; all four C runs | B8 shielding (HCx, HCz); B9 roof vs foam stack; **new:** HX02 extended house ratio over the five classes inside the house; HX03 per class, with HOx outside the wall (misassignment towards R6 and R1 and into the house column; orientation HEy/HEx); KA6 column accuracy next to R6 with the house on (HEx, HEy, HOx) |
| Multiple-tag competition | Ca and Cb on each board, 4-2-2-4 | B4 (C1 vs C4), B5 (CS vs C4), B6 (sentinels), B7 (delta*), each per board from both runs; **new:** C1o and C4o with the house lifted: HX10 competition without the house, HX11 house ratio under load, HX20-HX24 house x load interaction |
| Boards 2 and 4 under comparable conditions | One session; the same T1, tags, slots, orientations and competitor arrangement on both boards; one operator, house, bedding batch, spacer sheet and camera model; seeded order within blocks; 4-2-2-4 for every programme; REF 3 times per board | B10 board call with REF repeatability and drift adjustment |

### 6.2 What the amended schedules change, and what stays a limitation

**Schedule v2 and criteria v1.1** (2026-10-15, first decision; `schedule_one_day_v1` and `acp_criteria_v1.0` kept) resolved these items of the v1 review:
- **A. Competition run order:** counterbalanced (Ca 4-2, Cb 2-4). The two runs per board are identical; the analysis pools their 10-s blocks per board.
- **B. Movement order:** every programme follows one documented pattern, 4-2-2-4.
- **G. Substitute boards:** only through an explicit authorization, `board_substitution.csv` in the raw day folder (role, physical_board, reason, authorized_by, authorized_on), listed in the sha256 manifest; only board 3 for 4 or board 1 for 2. Without it, any placement on another board stops the analysis: never substituted silently; abort and reschedule.
- **I. Day 2:** a separate confirmation experiment, analysed alone with the same criteria and never pooled with day 1 (DC1-DC3).

**Schedule v3 and criteria v1.2** (2026-10-15, second decision; v2 and v1.1 kept) add:
- **K. Column-boundary transitions.** The sled carries T1 (x orientation) from a known centre to the neighbouring column's centre on the recorder's timed call (one reader spacing in about 2 s, at 25, 47, 69, 91, 113 and 135 s after the start touch), holds 20 s, and steps on. Each placement goes out and back along one row, so every column boundary is crossed in both directions; the video gives the crossing times.
- **HX. House coverage.** Four ON/OFF pairs, 40 s analysed each:
  - HEx (25,116) x and HEy (25,116) y, on the bedding: the house edge towards R6, with radial and tangential orientation, in the same half;
  - HSy (0,87) y, on the bedding: the house side towards R1 (food);
  - HOx (75,116) x, on the floor just outside the house wall towards R6: whether the house draws an outside tag into the house column.
- **House-lifted periods.** In every competition run, after C1 the house is lifted with the tags staying in place: C1o (T1 alone), then T2-T4 are added for C4o, then the house is set down again for C4. With C1 and C4 this gives a 2 x 2 (one or four tags x house on or lifted) inside each run, so reader competition can be separated from house attenuation.
- **The same handling for every house-on period with tags in the house** (C1, C4, CS): lift, tags placed or moved, house set down over them, slots checked on the camera. The cells of the 2 x 2 then do not differ in handling.
- **Reallocated:** C1 (130 -> 100 s), C4 (310 -> 250 s) and CS (160 -> 130 s) in each run (section 5 gives the analysed time per board). C0 and CE are unchanged.
- **Drop step 4:** the HSy pairs, on both boards; it may follow step 2, because step 3 cannot be applied on both boards at the break.
- **Every v2 row outside the competition runs is kept**, with the same content and in the same order (the seeded draws of v1 and v2 are unchanged; the new elements use a second seed). Only start times move.
- **Criteria v1.2:**
  - every v1.1 endpoint definition, decision threshold and the day-2 rule are unchanged; the shorter competition periods change the data of B4-B7, and the longer visit spans the known-empty time of B15 and the isolated events of B16;
  - the added rows (category added_v1.2) are computed apart from the v1.1 endpoints and are outside the day-2 rule; a sheet error on an added placement is reported apart and never triggers D2;
  - READ_K and EVENT_K need both boards in the same direction (the D_K bootstrap has only 4 passes per board);
  - column adequacy KA is limited as soon as any component is not met; KA2 and KA6 use the tested placements only;
  - the overall basis verdict (K20) can only withhold a READ or EVENT call of criterion (a): when the column-boundary transitions point to the other basis (READ with EVENT_K, or EVENT with READ_K), the verdict is discordant, and both bases are retained as sensitivity analyses.
- **A04 seeding (technical correction).** Real Exp9 events have no length limit (B1 CC1: the longest lasts 3,862 s).
  - Before v1.2, a tag placed before the start touch and read continuously had no event start inside its placement, so the event basis stayed unassigned for the whole placement. The basis comparison would have favoured the single-read basis by construction.
  - Now an event in progress at the placement start is an observation at the start, in both bases.
  - Found while testing the v3 additions, before any data; on the synthetic test day the event basis was unassigned for 37% of the centre time before the correction and for 0% after it.
  - It changes the values of the basis-sensitive v1.1 endpoints (D_col, D_rdr, D_ctr, B2, the visits of B11), not their definitions or thresholds. The unseeded rule is not reported, because it measures the timing of the start touch, not localisation.

**Correction of an earlier description.** Limitation C of v2 ("No column-boundary movement") was worded in the reader-neighbour vocabulary, in which R1 and R5 form a "column" pair. It meant: no movement between the two rows of a column (R1<->R5, R2<->R6). Movement across the boundaries of the four occupancy columns was already in v2, as continuous upper-row passes and R5>R6 visits, but not on the lower row and not as held transitions. v3 adds held, timed transitions across all six column boundaries.

These stay limitations, reported with the results:
- **C'. Row-to-row movement within a column** (R1<->R5) is still untested. It does not change the column; diagonal crosstalk is bounded only by the static co-reads.
- **K on a sled.** The K steps move a tag at about 5 cm/s in x orientation. They test reader timing at column boundaries, not gait, posture or rearing.
- **D. House positions.** The HX positions are fixed coordinates. Whether they lie inside the house depends on the measured outline (noted before the day). There is no nest material.
- **Column changes with the house in place and house exits are untested.** K runs with the house absent, so an "adequate" column call (KA) does not cover them; the house-edge and outside positions (HEx, HEy, HOx) are static.
- **Tag configuration.** Only one and four z tags in a 2-cm square at R5 and the 2 + 2 split are measured; other spacings, positions and mixed orientations under load are untested.
- **Detection age.** K32 covers ages up to about 10 s (the tags are read almost continuously in the K and M windows); whether positions 60-300 s old are still valid cannot be tested with static or sled-moved tags.
- **D_K has 4 passes per board.** Its CI is approximate; READ_K and EVENT_K therefore also need both boards in the same direction, and K alone never decides the basis.
- **Board 3 is not measured.** Only boards 2 and 4 carry tags. B10 can show whether board 4 differs from board 2; the Exp9 signature of boards 3 and 4 is tested on board 4 only. Board 3 is tested only if it is an authorized substitute.
- **Static and sled-moved tags** do not establish the movement, huddling or physical proximity of freely behaving mice.
- **E. REF well state.** The third REF of each board is taken with bedding in the R5 well; balanced across boards, but late/early REF-x (D3b) is confounded with the well state and is interpreted with this caveat.
- **F. Tag QC (T0) on board 2 only.** The same physical tags are used on both boards.
- **H. Unplanned session restart.** The runner needs exactly one log with detections; an unplanned restart that writes a second log stops the analysis, and the day is repeated on the alternative date (stop rule 5).
- **J. delta* of 30 s.** A B7 candidate but not an srs_v1.1 age class; that result would need a new reconstruction version.

## 7. On-day stop rules and bench rules

**During every analysed window:**
- hands and any metal stay at least 30 cm from the cage;
- the lid is closed;
- idle tags are in the closed box at least 1 m away.

**Clapper.** At every placement start and end, and twice at every block start and board switch. The recorder notes each PC-clock time. The tag is placed (or the sled set at its first centre) before the start touch.

**K steps.** The recorder calls each step from the timer; the operator pulls the string by one mark from at least 30 cm and lets the sled stop on the next centre mark. A step that overshoots or stalls is not corrected during the placement: note it as a deviation (the video annotation gives the true crossings).

**House lifts in the competition runs.** Lift the house vertically, without touching the tags in their slots; place or move tags with tweezers; set the house down on its outline and check the slots on the camera before the start touch.

**Stop rules:**
1. **Channel check fails** (an antenna silent for 5 s with T1 on it): restart the session once, before any placement, and repeat the check. If it still fails, use a substitute board only if an authorization (6.2 G) has been signed off; otherwise stop and move to the alternative date. Record a deviation.
2. **Tag QC (T0):**
   - a tag outside +-20% of the tag median is not used in C (use the spare; it keeps the label);
   - if T1 is outside +-10%, swap T1 for the tag nearest the median and repeat the V1 REF.
3. **REF-z on board 2 below 1.5 reads/s:** check the plate seating and the cage position.
4. **Disturbance** (hands or metal too close, lid open, carrier displaced):
   - repeat the placement at the end of the same board-block as `<order>r`, once only;
   - inside a competition run, where the next period depends on the tags in place: re-seat the tag, then repeat the disturbed period at once as `<order>r` and continue the run;
   - never overwrite;
   - the paired placement on the other board is not repeated.
5. **Unplanned session restart:** log the time and stop: a second log with detections stops the analysis, and the day is repeated on the alternative date (6.2 H).

**Drop order if the day is more than 20 min behind at the break** (always both boards; steps in this order; decided once, at the break):
1. roof pairs H05/H06 (-6.7 min);
2. CS shortened to 90 s analysed (-2 min);
3. 4 visits per half instead of 6 (-1.3 min). In schedules v2 and v3 three of the four visit rows run before the break, so this step cannot be applied on both boards and is skipped (the analysis refuses it on one board);
4. the HSy pairs H11/H12 (-5.0 min); it may follow step 2.

Steps 1, 2 and 4 save 13.7 min. Never drop a block on one board only. The K steps, the HEx, HEy and HOx pairs and the house-lifted periods are not dropped.

## 8. Data handling
- **Raw folder** `Raw Data/Behavior/RFID/calibration/<YYYYMMDD>/` holds:
  - the MultiMouseMonitor log (exactly one log with detections; initScan files are ignored);
  - the workbook copy and its version row;
  - `placement_sheet.csv`, `annotation_movement.csv`, `annotation_survey.csv`, `tags.csv`;
  - photos (including the house-extension positions against the outline) and videos;
  - `sha256_manifest.csv` (columns file, sha256). The runner checks every listed file against it;
  - only if a substitute board was authorized: `board_substitution.csv` (role, physical_board, reason, authorized_by, authorized_on), listed in the manifest.
- **`placement_sheet.csv`:**
  - columns placement_id, order, board, pair_id, status (executed or dropped), pc_start, pc_end, clapper_ok, drop_step, deviation (optional: tags);
  - times on the PC clock as `%Y-%m-%d %H:%M:%OS`;
  - placement_id is the schedule order (plus `r` for the one allowed repeat);
  - every one of the 117 rows with a tag in a cage (including CHECK and SURVEY) is recorded as executed or dropped;
  - include the scheduled BASELINE and BREAK rows with their times; an unscheduled pause is noted in the deviation field of the next placement, never as an extra row;
  - for each HX placement, the deviation field may note the position's relation to the house outline (inside, wall, outside).
- **`annotation_movement.csv`** (placement_id, kind, t_pc, from_reader, to_reader): one `cross` row per midpoint crossing of the M passes, the visits and the K steps, entered already on the PC clock with the filmed display offsets applied. Every K crossing changes column (R1>R2, ..., R8>R7); the runner refuses a K crossing within a column.
- **`annotation_survey.csv`** (board, sweep, t_pc, x, y).
- **`tags.csv`:** label (T1-T6, C, spare) and tag. A replacement tag keeps its label.
- Never edit a raw file after the manifest has been written. A correction goes into a new file with a note.
- Analysis outputs are written only to `<Behavior>/RANM/calibration/<run_id>/` (pre-registered runner) and `<Behavior>/RANM/calibration_reports/runs/<report_id>/` (report).

## 9. After the pilot
1. **Same day:** backup, sha256 manifest, deviation notes.
2. **Video annotation** (about 2.5 h, PC-clock time).
3. **Pre-registered analysis** (committed, unchanged code; from the repository root):
   `Rscript Testing/audits/analyse_rfid_calibration_pilot.R --date=<YYYYMMDD>`
   - Output: `<Behavior>/RANM/calibration/acp_v1.2_<date>_<commit>/`, with decisions.csv, day2_recommendation.csv, tables/ and manifest/. The v1.2 additions are in decisions.csv (criteria K, KA, HX) and in tables/ (k_*, detection_house_ext, confusion_house_ext, house_extension_*, competition_house_*, age_accuracy, missing_additions).
4. **Report:**
   `Rscript Testing/audits/report_rfid_calibration_pilot.R --acp-run=<absolute path of the run from step 3>`
   - Output: `<Behavior>/RANM/calibration_reports/runs/<report_id>/` (default id `acr_v1.1_...`):
     - README.txt;
     - status_by_decision.csv;
     - spec_consequences.csv (one row per specification item, including the four items added in v1.2);
     - day2_trace.csv (the projection test per threshold);
     - day2_trace_check.csv;
     - uncertain_without_day2.csv.
   - It verifies the acp run's manifest and pinned hashes first and never writes into the acp run.
   - It stops if any protected output changes while it runs. Start it when no other RANM run is in progress.
5. **Decision table for Tobias**, taken from spec_consequences.csv and the README, answering the questions of 2026-10-15 with the pre-specified criteria only:

| question | criteria | possible answers | if not resolved |
|---|---|---|---|
| Is single-read or event-based reconstruction better supported? | (a) A20-A42 (D_col with D_ctr), overall verdict K20 with the column-boundary transitions (K10-K16) | READ; EVENT; TIE equivalent; TIE inconclusive; discordant; not obtained | Both bases are retained side by side as sensitivity analyses; neither is ground truth. |
| Is column-level assignment accurate enough for descriptive occupancy? | B2 (reader or column), KA1-KA8 (centres, boundary quarter points, hold cores, detected transitions, switching latency, house edge) | adequate; limited (the components not met are named); not obtained | Four columns stay primary, with the named limitation and the unknown share. |
| How does detection age affect interpretability? | B3 (fresh age class), B7 (carry-forward cap), K30 (switching latency), K32 (correct-column share by detection age, descriptive) | the fresh class (2 or 10 s); the cap consistent with bench detection; accuracy by age class | LOCF, 60 s and 300 s are reported as sensitivity analyses of the age cap. |
| Do boards 3/4 show measurable hardware differences? | B10 (board 4 against board 2, REF repeatability and drift) | difference; no difference; undecided | Board 3 is not measured: its Exp9 signature is not addressed by the pilot. |
| How strongly do the house and nearby tags affect detection? | B4-B7 (competition, house on), B8 and HX02 (attenuation), B9 (roof), HX03 (misassignment and orientation), HX10-HX24 (competition with the house lifted, house ratio under load, interaction) | ratios and calls with their CIs | Ratios and CIs are reported; no day 2 automatically. |

   - No reconstruction is chosen by biological outcomes, group effects, predictive performance (R²) or significance.
   - Unresolved decisions are listed explicitly.
   - The day-2 verdict is the one computed by the predefined rule (`acp_decide_day2`, unchanged): a recommendation only; Tobias decides. Day 2 is never scheduled automatically.
   - The frozen criteria are not changed on the basis of results.
6. **If day 2 runs** (only on Tobias's decision):
   - a separate confirmation experiment: only the triggered blocks, under the day-1 rules, analysed alone and never pooled with day 1 (DC1-DC3); a statistic is decided from day 2 alone, and if day 1 and day 2 disagree both are reported and the decision stays uncertain;
   - seed 20261015, at most 4 h on site;
   - always the descriptive bridge P09, P11, P13, P06 plus REF on both boards (session effect);
   - the day-2 schedule and its unit counts are committed and hash-pinned before day 2.

## 10. Decisions that remain conditional on calibration

Wording rules for every row:
- No reconstruction is chosen by group effects, predictive performance or significance.
- Bases that are not resolved are retained as sensitivity analyses; their estimates are never described as bounds on true physical occupancy.
- Recently detected is not validated location.

| decision (spec v1.1) | criteria | possible outcomes | consequence | if inconclusive |
|---|---|---|---|---|
| Reconstruction basis of the basis-sensitive measures (column occupancy, R5 share, reader changes) | (a) A20-A42; K20; D1 | READ; EVENT; TIE equivalent; TIE inconclusive; discordant; not obtained | READ: single-read is primary in the revised Stage 19; event-based stays the historical reference and a sensitivity analysis. EVENT: event-based is primary; single-read is a sensitivity analysis. TIE equivalent: no winner; both reported as sensitivity analyses. Discordant: neither is decisively supported; both are retained. | Both are retained side by side as sensitivity analyses: single-read stays provisional, event-based the historical reference. Day 2 (D1) only if the projection test holds at -0.05, 0 or +0.05. Not obtained: D2. |
| Basis of the eight-reader tables | (a) A50 | as above, at reader level | Selects the basis of the reader-level sensitivity, QC and resource-location tables only | Both bases in those tables; no day-2 trigger |
| Resolution | B2 | reader; column | Reader: reopens decision 3, and Tobias decides. Column: decision 3 confirmed (four columns primary, eight readers for QC, sensitivity and resource location). | Four columns stay primary; with a tied basis, reader level must hold under both bases |
| Column-level adequacy (new) | KA1-KA8 | adequate; limited; not obtained | Adequate: four-column occupancy is used descriptively with its unknown share. Limited: each failed component is a stated limitation. | Not obtained: adequacy stays open; no day-2 trigger (G10) |
| "Fresh" (recently detected) age class | B3 | 2 s; 10 s; no candidate; not obtained | Sets the recently detected class: a bench detection property, not validated location | Gmax reported; 2 s and 10 s stay descriptive; no day 2 (D2 only if not obtained) |
| Carry-forward age cap | B7; K32 (descriptive) | 10, 30, 60 or 300 s; no candidate | 60 s: the 60-s cap is consistent with bench detection. 300 s: only the 300-s cap is consistent with bench detection. 10 s: a shorter cap would also be consistent. Unrestricted LOCF stays the historical reference; the 60-s and 300-s caps stay sensitivity analyses. 30 s: needs a new reconstruction version. | LOCF (historical reference), 60 s and 300 s are all reported with the unknown share, as sensitivity analyses of the age cap; no day-2 trigger of its own |
| Flip-filter tau | B11; D5 | tau* <= 2 s; tau* > 2 s; range; not decidable | <= 2 s: tau 2 s retained. > 2 s: reader-change rates (S2) stay descriptive at column level; secondary statistics at tau*. | Range: tau 2 s stays frozen and the range is reported (no day 2). Not decidable: D5. |
| Competition in one field (house) | B4; D6 | time-sharing; collision; capture or starvation; intermediate | Read-density denominator; unread house time is not absence; co-assignment stays excluded; house occupancy is reported with its unknown share | Intermediate: D6 only if the projection test holds (TR 0.7, 0.85, 1.15; MS 0.35, 0.5); capture on one board only triggers D6 |
| Competition with the house lifted (new) | HX10; HX11; HX20-HX24 | the B4 calls; HR4 attenuation calls; interaction or none | Whether competition and house attenuation are separate mechanisms, and whether the single-tag house ratio applies under load | Reported with CIs; no day-2 trigger (G10) |
| Competition across fields | B5 | per antenna; spans R5/R6; undecided | Co-location count or tags-per-board as the read-competition covariate | n_tags_present stays descriptive; no day-2 trigger |
| Board-level load | B6; D6 | board level; none; undecided | Whether n_tags_present is physically justified for read-dependent measures | D6 only if the projection test holds at SR 0.85; otherwise a design covariate without a calibrated mechanism |
| House shielding | B8; HX02 | no shielding; shielding; partial (HX02: over the five classes inside the house) | Whether R5 read densities are comparable; the bias statement for house occupancy | Partial: ratio and CI reported, caveat on R5 densities; no day 2 |
| Roof | B9 | B1 classes per board | Whether unread house time can be roof time | Border or dropped: stays open; no day 2 |
| Board call | B10; D3, D4 | difference; no difference; undecided | Interpretation of the board covariate (hardware vs cage, rack or occupancy); the section 6 adjustment is unchanged | D4 if the projection test holds (R_board 0.9, 1, 1.11; co-read 0.7); D3 if REF is not repeatable |
| Footprint wording | B1 | reliable, border, unread per placement | Physical wording of "assigned to reader k" (S3, S6) | Descriptive; border is reported as border |
| QC rules | B13-B15 | co-read expectations; real tags or an events rule; tag eligibility | Inputs to a new aqc_rules version | QC inputs, not day-2 triggers (D2 if missing) |
| Transfer to Exp9 | B16; D7 | transfer; caveat | A caveat is attached to every conclusion | Never a reason for day 2 |

## 11. Booking: proposed date, resources and constraints

**Proposed date:** Tuesday 3 November 2026, 08:00-14:30 (session 08:30-13:54 plus arrival and packing).
**Alternative:** Tuesday 10 November 2026, same times.

**Resources to book or reserve:**
- The Exp9 RFID rack with boards 1-5 (LabNetV2 rfid32 AM1-AM5) and the PhenoSys PC with PhenoSoft 1.0.8147.28429, for the whole day, free of animals and of any other study.
- Five production cages with lids, hoppers and bottles (no animals); one red production house; one batch of production bedding (two weighed portions).
- 8 spare tags of the Exp9 type (T1-T6, clapper C, spare).
- Two top-down cameras with a millisecond clock display (or a PC-driven LED) and a third camera for photos.
- Two people for 08:00-14:30 (operator and recorder), an annotator for about 2.5 h on 4-6 November, and the analyst for 14-16 h on 6-10 November.

**Scheduling constraints:**
- The rack must be free of animals and of other sessions from 08:00 to 14:30; whether it can be in use the day before is for the facility to decide.
- Not on a daylight-saving change day: both dates are after the EU change on Sunday 25 October 2026.
- No manual change of the PC clock on the day; any NTP sync is noted; PhenoSoft is started once.
- The materials (section 2; about 6 person-hours) and the camera, clock and K-timer test must be done in the week before (26-30 October).
- The same operator for the whole day (and for any day 2).
- Board 3 or board 1 may stand in for board 4 or board 2 only with an authorization signed before use; otherwise a failed board moves the day to the alternative date.

**Request text (to the rack and facility contact; to be sent by Tobias):**

> Request for one animal-free RFID rack day for a technical calibration (no animals involved).
> Equipment: the Exp9 RFID rack (boards 1-5, AM1-AM5) and its PhenoSys PC with PhenoSoft, for the whole day.
> Preferred date: Tuesday 3 November 2026, 08:00-14:30. Alternative: Tuesday 10 November 2026, same times.
> We will place five empty production cages with tags on the boards, film them from above, and remove everything by 14:30. The PhenoSoft session runs once, without changes to the PC clock.
> Please confirm that the rack is free of animals and of other sessions on one of these days.
