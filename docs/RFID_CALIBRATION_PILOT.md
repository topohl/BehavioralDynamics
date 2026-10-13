# RFID calibration pilot: minimal technical protocol, endpoints and resources (v1, 2026-10-14)

**Status: proposal, not started.** Nothing is booked, no tag is placed and no animal is involved. It turns steps 1-5 of `docs/RFID_CALIBRATION_PROTOCOL.md` into a two-day bench pilot (a one-day minimal version is in Resources) with the validation endpoints fixed before any hardware time is booked. Statements marked *inferred* come from desk checks of the Exp9 logs and the configuration workbook (2026-10-14) and must be confirmed by PhenoSys or by the pilot itself.

Goal: estimate location-classification errors, missed reads and detection competition on the Exp9 hardware, so that the open choices of the spatial specification (`docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.md`: reconstruction basis, flip-filter tau, carry-forward bound, reader or column resolution, board handling) rest on measured values. A static-tag calibration bounds the hardware; it does not validate positions of freely moving implanted mice (section "Limitations").

## Protocol

- Step 6 of the proposal (a filmed animal) is not part of the pilot.
- The pilot is group-blind: no Exp9 animal, no CON/RES/SUS label and no outcome is used.

### 1. Design choices compared with the proposal

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

### 2. Fixed conditions (production configuration, unchanged)

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

### 3. Before booking (desk work, no hardware)

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

### 4. Materials

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

### 5. Set-up per board

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

### 6. Placement list

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

### 7. Session plan (minutes)

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

### 8. Bench rules and stop criteria

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

### 9. Records and data handling

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


## Endpoints

### Conventions

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

### E0 Tag variability

**Definition.** Reads/s of each tag at the R3 centre (z, floor); each tag's ratio to the tag median; the CV across tags.

**Precision.** 147 reads in 60 s gives ±16% per tag (Poisson bound; conservative, because the lattice cadence is more regular than Poisson).

**Decision it informs.**
- Which tags enter the competition sets.
- T1 is used for every single-tag placement on both boards, so tag variance stays out of the board ratio.
- It sets the floor for reading per-animal read differences (QC W10).

### E1 Detection map and missed reads

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
- Whether a present, uncontested tag in the reliable zone is ever unread for more than 1 s. If not, home-cage gaps there come from posture, height or competition, not from the hardware. This sets the minimum meaningful support age.
- An input to delta (E5).

### E2 Localization confusion

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

**Decision it informs: column or reader resolution (S3).** Reader level only if both hold on both boards:
- correct-reader share ≥ 0.95 at every centre and orientation;
- correct-row share ≥ 0.9 at the column quarter points (0,29) and (0,87).

Otherwise the column-resolution proposal stands.

### E3 Crosstalk ratios

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

### E4 Competition curve

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

**Decision rules.**
- **Time-sharing:** throughput at n = 4 within ±15% of n = 1 and maximum share ≤ 0.35. Read-density measures then need a co-location denominator.
- **Collision:** throughput below 0.7 x n = 1.
- **Capture or starvation:** maximum share ≥ 0.5, or any present tag unread for ≥ 60 s. Then:
  - unread time at the house is not interpretable as absence;
  - co-assignment stays excluded (S6);
  - house occupancy is reported with its supported share only.
- **Board- or group-level competition:** T1's rate falls by more than 15% in C3. The number of tags per board then becomes a covariate for read-based measures (B1 boards held 2.9 tags and had more reads per animal).

**Delta (carry-forward bound).** The smallest of {10, 30, 60, 300} s that exceeds both:
- the per-tag q99 gap at n = 4 in the house on board 4;
- the longest gap of any single-tag placement in the reliable zone.

### E5 Shielding ratios

**Definition.** Paired placements with and without the object, at the same coordinates, tag, orientation and height:
- house: 3 inside positions plus the roof;
- hopper: feeding position and floor;
- bottle: spout and floor.

Reported: the ratio of read rates and the difference in P1s; also C1 against C2 (house against open field under competition).

**Precision.** Per 60-s pair, the paired log-ratio has SE 0.117 from counting alone. Pooled:
- house: 6 pairs, ±0.09;
- hopper and bottle: 4 pairs each, ±0.12.

**Decision it informs.**
- **House ratio ≥ 0.9 and P1s ≥ 0.95:** the house does not shield. Low R5 support in Exp9 is then competition, posture or nest height, which bench data cannot separate further.
- **House ratio < 0.7:** shielding. R5 read densities are then not comparable with other readers, and house occupancy rests on carry-forward with a stated bias.
- Hopper and bottle are read the same way for the R1 and R8 densities (QC D01; W10 interpretation).

### E6 Boundary behaviour (movement; truth from video)

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
- **tau** = the smallest value that removes ≥ 99% of static midpoint flicker runs on both boards and keeps at least 11 of 12 genuine 5-s visits. If that value is above 2 s, the frozen flip2s version leaves crosstalk in reader-change counts, and the reader-change rate stays descriptive and at column level.
- **Basis:** adopt single-read LOCF (QC decision 2) if its correct-reader share around crossings exceeds event-start LOCF beyond the CI, and it does not lose at static placements.

### E7 Board 3/4 against other boards (sensitivity ratio)

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
- **A hardware sensitivity difference** shows as any of the following, outside the bridge repeatability:
  - reads ratio ≤ 0.9;
  - co-read ratio ≤ 0.7;
  - a smaller footprint at R1, R2, R5 and R6.
  
  Then:
  - board becomes a covariate (QC decision 3), with board-specific detection and crosstalk parameters;
  - the house-end antenna deficit is documented.
- **Ratios near 1:** the field signature comes from the cage or rack environment, or depends on occupancy. Board stays recorded as a covariate but is not described as sensitivity.

### E8 Known-empty

**Definition.** Events per board-hour on boards with no tag, with their tag IDs.

**Precision.** Zero events in about 38 board-hours gives ≤ 0.08 events per board-hour (rule of three). That is below 0.005% of the home-cage rate (about 1,670 events per board-hour).

**Decision it informs.**
- **No phantom events:** a read is treated as a real tag within range.
- **Any phantom event:** an events minimum rule and a revision of W08/H06.

### E9 Session and cadence integrity

**Definition.**
- The channel check passes after every session start.
- Each board's lattice signature (mean gap, k1 share) is compared with its Exp9 history in `board_history.csv`.

**Decision it informs.** If |z| > 3, the bench boards no longer behave as they did in 2022-2024, and the results transfer to the Exp9 data only with a caveat.

### Summary: what the durations buy

| duration | buys |
|---|---|
| 60 s static | the three-class detection call; ±16% read rate; ±5% pooled board ratio |
| 3-5 min competition | per-tag rates at ±11-16%, and starvation detection |
| 5-min static flicker | a q99 of flicker runs |
| 12 crossings per class | the median latency between the 3rd and 10th ordered value |

None of these estimates concerns CON, RES or SUS. They are technical and group-blind.


## Resources

### Hardware time

**Rack time.** The rack is blocked, free of animals, on 2 consecutive days: 7.3 h and 5.5 h on site, 12.8 h in total.

| board | role | hours |
|---|---|---|
| board 2 | under test (day 1 programme plus the 20-min bridge on day 2) | 6.2 |
| board 4 | under test | 4.0 |
| boards under test, total | | about 10.2 board-hours |
| boards 1, 3, 4 (day 1); boards 1, 3, 2 (day 2) | known-empty, recorded at the same time, no extra personnel | about 38 board-hours |
| board 5 | clapper board; contributes only the 4 x 10-min baselines | — |

PhenoSys PC and PhenoSoft 1.0.8147.28429 with the production workbook.

### Personnel hours

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

### Tags and materials

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

### Minimal version (one day, if time is short)

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

### Assumptions

- About 30 s of handling per static placement with the jig; 70 s recorded for 60 analysed; one session per day with no restarts.
- Hand-paced movement within about 30% of the nominal speed; the true speed comes from the video.
- At least 6 (ideally 9) spare tags of the Exp9 lot or model exist; the boards, rack positions, plates and workbook are unchanged since Exp9.
- The rack holds no animals on the pilot days; both operators are trained on PhenoSoft; the analyst knows the repository.
- Breaks are included in the on-site hours but not in the board-hours under test.
- No group labels, outcomes or Exp9 animals are needed.


## Limitations

### What static and phantom tags cannot validate for freely moving implanted mice

The pilot measures the hardware and bounds the error of the reconstruction rules under controlled conditions. It does not validate positions or measures of freely moving implanted mice. Results apply to the Exp9 data only as bounds, priors and sensitivity ranges, never as corrections of individual animals' positions.

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

### Optional later step: one filmed animal (not to be initiated now)

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
