# Spatial measurement specification v1.1 (2026-10-14)

**Superseded by `docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.2.md`** (decisions of 2026-10-14, third round). This file is the
record of the second round.

*Current record: `docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.5.md` (closeout of 2026-10-15: calibration will not be
performed). The status, plans and pending items below are historical and are not current instructions.*

**Status.** The decisions of 2026-10-14 (second round) are incorporated. The single-read reconstruction is the *provisional*
candidate representation; the method and its parameters are frozen only after the one-day calibration pilot has been
assessed. Nothing is applied to a registered analysis:
- Stage 19 and its outputs are unchanged (a revised Stage 19 will be a separately versioned output, section 8);
- the Stage 29 v1.0.1 release, its model specification, S13 and every registered output are unchanged; Stage 29 is not rerun;
- no manuscript figure is regenerated, and no full behavioural-model run is launched before this specification has been
  reviewed after the calibration pilot.

The aim is a defensible, reproducible spatial measurement basis. No method, threshold or parameter here was chosen by RES-SUS
separation, CON differences, predictive R² or p-values.

Supersedes `docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.md` (kept as the record of the first proposal).

## 0. Decision record

| # | decision (Tobias, 2026-10-14) | implemented as | where |
|---|---|---|---|
| 1 | Failed-channel exclusions approved (SX01-SX04); movement: the 12 active animal-windows leave new primary analyses of crossing rate and fragmentation, kept as sensitivity; light-phase rate evaluated separately; censoring applied to Stage 31 where relevant | manifest v1.1 with exclude_movement records SX05-SX07; SX04 states the span end per analysis; Stage 31 opt-in censoring | `docs/spatial_exclusions/spatial_analysis_exclusions_v1.1.csv`; section 2 |
| 2 | Single-read reconstruction approved as provisional; event basis preserved; identical windows and carry-forward definitions; agreement with the latest read is timestamp consistency, not accuracy | srs_v1.1, both bases from one function | `Functions/antenna_spatial_reconstruction.R`; sections 1, 4 |
| 3 | Four columns primary for revised Stage 19; eight readers for QC, sensitivity and resource-location diagnostics; no claim of precise resource engagement or proximity | specification S3, S4 | section 9 |
| 4 | Carry-forward: share of time supported by a read less than 60 s old (implemented as an observation age <= 60 s); unrestricted LOCF, max 60 s, max 300 s compared; unsupported time is "unknown", not redistributed; both denominators | `asr_shares_with_unknown()`; observed time per animal-block | section 3 |
| 5 | Board handling approved with limitations | presence metadata (three counts), board-adjustment audit | section 6 |
| 6 | One-day calibration pilot, decision criteria fixed before the pilot, day-2 rule | protocol v2, criteria v1.0, pre-registered code | `docs/RFID_CALIBRATION_PILOT.md`; section 7 |
| 7 | Stage 19 revised later as a separately versioned output | plan | section 8 |
| 8 | No Stage 29 rerun; shared_zone_use and reader co-assignment not retained as proximity or affiliation | KNOWN_LIMITATIONS §5 | section 9 |
| 9 | Stage 00b live integration accepted; the FAIL status names the affected hardware episodes | status per board-recording | section 10 |

## 1. Observation model (srs_v1.1; provisional candidate)

The single-read reconstruction is the candidate spatial representation; the event-based reconstruction (Stage 19's rule) is
kept as the comparison. Both come from one function (`asr_process_recording()`) with identical inputs, span, phase-block
edges, flip filter (tau 2 s), seeds, masks and carry-forward definitions; only the observations differ.

| element | definition |
|---|---|
| observations | read basis: every single read at its logged time (event start + cumulative MsgValue1 gaps, millisecond grid of `apr_reads()`); event basis: detection-event starts (`rfid_raw_occupancy_intervals()`, Stage 19's function) |
| readers | the antenna's PositionID from the log's own device table (24/24 recordings checked by Stage 00b) |
| simultaneous reads | one tag at two readers in the same millisecond (0.03% of reads, one board): one observation, the reader held before it if it is among them, else the lowest PositionID |
| own board | the board with most of the animal's detection events in the span (Stage 19's rule); off-board reads counted, not used |
| assignment | the reader of the latest observation, border flips < 2 s reassigned (`rfid_flip_filter()`); seeded by the last observation before the span (444/444 animal-recordings) |
| observation age | time since the animal's latest read on its board, the same in both bases |
| bounded carry-forward | an assignment is kept while the age is <= 60 s (or 300 s); older time is **unknown** (section 3) |
| span and blocks | vendor phase calendar of the v2 file, clipped to the raw recording end; every interval cut at the block edges; masks from manifest v1.1 |
| resolution | 4 columns primary; 8 readers for QC, sensitivity and resource-location diagnostics |
| denominator | observed time per animal-block = block time before the censoring time; masked cage episodes left out |

Regression tests (`Testing/tests/test_spatial_reconstruction_regression.R`) run a synthetic raw log end to end: multi-read
events split into reads, an overlapping short event (event basis holds it 99.5 s, read basis does not), a same-millisecond
tie, observed time and unknown time by hand, censoring (no run or observation counted after it), exclusions (computed and
flagged but left out of every summary), and the 1-s grid against the exact computation.

**Agreement with the latest read is not accuracy.** At the moment of a read the read basis holds that read's reader for 94%
of reads and the event basis for 75% (active). This is largely built into the two definitions; it shows timestamp
consistency, not localisation accuracy. Which basis is right at a boundary is decided by the calibration criterion (a).

## 2. Exclusions, censoring and movement (manifest v1.1)

`docs/spatial_exclusions/spatial_analysis_exclusions_v1.1.csv` (v1.0 kept unchanged; both content-hash pinned):

| record | action | cage episode | scope |
|---|---|---|---|
| SX01-SX03 | exclude_spatial | B2\|sys.2\|CC3 (R3), B2\|sys.5\|CC4 (R3), B5\|sys.1\|CC4 (R5) | new spatial analyses that need complete antenna coverage, whole span |
| SX04 | censor | B5\|sys.1\|CC4 | from 2023-12-04 14:00:42.962 to the end of the span each analysis uses (Stage 00b/19/srs: 2023-12-06 18:30, 52.5 h per animal; Stage 31: end of the recording 2023-12-07 13:12:01, 71.2 h) |
| SX05-SX07 | exclude_movement | the three cage episodes above | new primary analyses of the **active-phase** crossing rate and fragmentation; a sensitivity analysis keeps them; the light-phase crossing rate is kept and evaluated separately; no other outcome, cage episode or batch is excluded |

**Movement decision, effect sizes.** Knockout of the dead reader on the 117 healthy board-recordings (432 animal-windows; frozen
standardizer SD in the active window, a group-blind light-window scale otherwise): active crossing rate R3 +1.7/h (median
|shift| 0.33 SD, 90th percentile 0.90), R5 -3.0/h (ratio 0.87; 0.58 / 1.21); active fragmentation 0.91 / 1.83 and
1.04 / 2.05 SD; light-phase crossing rate 0.12 / 0.35 and 0.12 / 0.41 SD (not material). Materiality rule declared before the
knockout: median |shift| >= 0.2 SD or 90th percentile >= 0.5 SD. The 12 excluded animal-windows (first active phase of
B2 CC3 sys.2, B2 CC4 sys.5, B5 CC4 sys.1) are listed in `tables/movement_exclusion_windows.csv` of the release run for any
later analysis to join: `movement_excluded` is TRUE for these 12 active windows only, and the 12 light windows of the same
animals are listed with FALSE (kept). The registered Stage 29 results keep all of them.

**Stage 31.** Stage 31 reads the whole CC4 recording. Its new opt-in censoring option
(`MMM_GRID31_CENSOR_MANIFEST=v1.1`) applies SX04: 918 cage-bins of B5 sys.1 after the last read (855 inside the recording)
get no rate instead of a rate of 0, and the affected rows and phases are labelled; no row is dropped. The run
`whole_inactive_sx04v11_20261014` (isolated root `<Behavior>/RANM/cc4_grid_exposure_censored/`) leaves every other row and all
14 figures identical to `whole_inactive_20260929_v1`. Stages 31b-31d already leave the cage out through their complete-span
rule, so no estimate changes and they were not rerun; the existing run folders are untouched (`docs/CC4_GRID_EXPOSURE.md`).
Open: B2|sys.5|CC4 (dead R3; SX06) is in the Stage 31b-31d rosters. Applying the movement exclusion to their active-phase
family would need a rerun of those stages and is a separate decision.

## 3. Carry-forward, support and the unknown category

**Denominators.** *Observed time* is the block time before the censoring time, summed over the animal-blocks of a phase
(masked cage episodes left out). Under LOCF every observed second is assigned. Under a bound d, a second is *supported*
when its observation age is <= d; unsupported seconds are **unknown** and are not redistributed. Two shares are reported:
- *share of observed time*: the columns plus "unknown" sum to 1 (`share_all`);
- *share of supported time*: the columns sum to 1 (`share_supported`), conditional on a read <= d old.

**Support.** Share of observed time supported by a read <= 60 s old (<= 300 s), read basis (the age is the same in both):

| | all | CC1 | CC2 | CC3 | CC4 | boards 3/4 | boards 1/2/5 | cage-episode median (IQR) | animal min |
|---|---|---|---|---|---|---|---|---|---|
| active | 80.5% (94.2%) | 75.8% | 79.4% | 83.0% | 84.3% | 78.1% / 75.7% | 82.4-83.7% | 81.5% (76.7-85.8) | 47.8% |
| light | 54.9% (79.0%) | 46.4% | 54.2% | 57.5% | 60.5% | 49.8% / 47.8% | 58.8-60.3% | 54.9% (46.6-63.3) | 24.3% |

**Column occupancy, read basis** (pooled; percentages):

| phase | column | LOCF | max 300 s: of observed / of supported | max 60 s: of observed / of supported |
|---|---|---|---|---|
| active | C1 (food R1, house R5) | 65.3 | 61.1 / 64.8 | 51.2 / 63.6 |
| active | C2 | 19.0 | 17.6 / 18.7 | 14.7 / 18.3 |
| active | C3 | 5.6 | 5.5 / 5.8 | 5.0 / 6.2 |
| active | C4 (water R8) | 10.2 | 10.0 / 10.7 | 9.6 / 11.9 |
| active | unknown | 0 | 5.8 / – | 19.5 / – |
| light | C1 | 70.0 | 54.5 / 68.9 | 37.1 / 67.6 |
| light | C2 | 26.7 | 21.5 / 27.2 | 15.1 / 27.4 |
| light | C3 | 1.7 | 1.5 / 1.9 | 1.3 / 2.4 |
| light | C4 | 1.7 | 1.6 / 2.0 | 1.4 / 2.6 |
| light | unknown | 0 | 21.0 / – | 45.1 / – |

Conditional on supported time, the column profile moves little (C1 -1.8 active, -2.4 light at 60 s against LOCF); the
unconditional shares fall by the unknown time, which is not spread over the columns. Per cage episode, the unknown share at 60 s
has median 18.5% (IQR 14.2-23.3%, max 39.4%) active and 45.2% (36.7-53.4%, max 67.9%) light.

The 60-s and 300-s bounds are reporting conventions, not validated biological thresholds; the calibration (criterion B3, B7)
can bound how long a present, uncontested tag stays unread on the bench, not in a nest.

## 4. Event-start against single-read basis (identical windows and definitions)

| | active | light |
|---|---|---|
| seconds assigned a different reader / column (LOCF) | 8.50% / 6.24% (73% persists at column level) | 4.85% / 3.17% (65%) |
| of which within 2 s of a read | 80% | 74% |
| column C1, LOCF: event / read | 62.4% / 65.3% | 69.1% / 70.0% |
| column C1, supported (60 s): event / read | 60.1% / 63.6% | 66.1% / 67.6% |
| R5, LOCF: event / read | 33.6% / 36.2% | 53.1% / 54.5% |
| R5, supported (60 s): event / read | 30.4% / 33.5% | 47.9% / 50.1% |
| R5 read - event per cage episode, median (IQR), LOCF | +2.9 (+0.6, +4.1) points | +1.1 (+0.3, +2.0) |
| R5 read - event, supported 60 s, median | +3.3 points | +1.5 |
| R5 read - event (supported 60 s), boards 1 / 2 / 5 vs 3 / 4 | +5.2 / +4.3 / +4.4 vs +0.8 / +0.5 | +3.3 / +2.4 / +1.5 vs +0.9 / +0.4 |
| column changes per observed hour, event / read (pooled) | 106.8 / 89.4 | 31.2 / 24.4 |

R1 (food) differs by +0.3 (active) and -0.5 (light) points, R8 (water) by +0.5 and +0.1 (LOCF). The basis difference is
concentrated at R5 and R6 and on the high-crosstalk boards; it is a difference between two definitions, not a measured error
of either.

## 5. House-associated missingness and the light-phase unsupported time

| | active | light |
|---|---|---|
| unknown time (60 s) held at R5 / R6 / R1 (last assigned reader) | 47% / 17% / 26% | 60% / 23% / 13% |
| unknown time in read gaps of 1-5 min / 5-30 min / 30-60 min / 1-3 h / > 3 h | 45% / 51% / 3.8% / 0.3% / 0 | 25% / 66% / 8.6% / 0.7% / 0 |
| R5 unknown time with a cage-mate also at R5 / that mate being read (<= 60 s) | 91% / 70% | 95% / 66% |
| R5 supported time with a cage-mate also at R5 / that mate being read | 60% / 45% | 81% / 45% |
| R1 unknown vs supported time with a mate at R1 | 75% vs 54% | 35% vs 32% |
| unknown share by cage change, CC1 to CC4 | 24.2% to 15.7% | 53.6% to 39.5% |
| unknown share, boards 3 / 4 vs 1 / 2 / 5 | 21.9% / 24.3% vs 16.3-17.6% | 50.2% / 52.2% vs 39.7-41.2% |

(1-s grid of the read basis; tables `unknown_by_reader_gap`, `unknown_by_gap`, `colocation_by_reader_state`,
`unknown_by_cc_board`. The grid's unknown time agrees with the exact computation within about 10 s per 12-h block.)

Reading: most unsupported time lies in gaps of minutes to half an hour, not in multi-hour silences. The house holds 47%
(active) and 60% (light) of it, modestly more than its share of the carry-forward time (unknown time is 25% of the R5 time
against 19.5% overall in the active phase; 49.5% against 45.1% in the light phase). The clear difference is the cage-mates:
while an animal's house assignment is unsupported, a cage-mate is assigned to the house and often being read far more often
than while it is supported. This fits read
competition among tags in a shared nest (one tag is read per poll), occlusion or posture in the nest, or both; the data cannot
separate them, and they cannot say whether the animal was in the house while unread. The calibration measures competition
in the house (criterion B4) and shielding (B8) on static tags; a nest of moving animals stays unidentifiable (criterion (c)).
House occupancy is therefore reported with its unknown share, never as a precise measure of time in the nest.

## 6. Board handling

**Metadata kept on every table:** SourceFile, Batch, CC, board, System, CageEpisodeID, AnimalNum, and per cage episode three
counts (`Testing/audits/audit_cage_episode_presence.R`; run `cep_v1_9b18624` in `<Behavior>/RANM/cage_episode_presence/`):
- **n_animals_present**: animals housed, from detection-independent records (regrouping plans, the B1 configuration
  workbook, animal records, post-CC4 kill list and sucrose sheet). 4 in 110 cage episodes, 3 in 10 (B5 one cage per CC; B4
  and B6 CC2-CC4 after one animal each left the study).
- **n_tags_present**: tags with >= 100 in-span reads on the board (roster or not): the read-competition count. It differs from
  n_tracked in 4 cage episodes (B1 CC1 b2, b5: 2 tags, 1 tracked; B4 CC1 b1, B6 CC1 b5: 4 tags, 3 tracked).
- **n_tracked**: roster animals on the board; only for counting observable dyads.

**The tracked-tag count is not presence.** The count used in the identifiability check (and called "tracked tags" in v1) is
the number of configured, non-excluded tags with detections. It equals the animals housed in 104 of 120 cage episodes and
understates them by 1-3 in 16 (14 in B1, where 4 animals carried no working transponder and 2 tags fell silent after CC1;
B4 CC1 b1 and B6 CC1 b5, where an excluded animal was present). It is therefore not used as a presence or crowding covariate.
n_animals_present varies only in 10 regrouped cage episodes of 3 batches, so its estimability must be checked before it enters
a model.

**Identifiability** (design only; `audit_board_group_identifiability.R`, run idf_v1_6165adf): board and group main effects are
identifiable (CON VIF 1.01, effective CON n 23.6 of 24); board x CC leaves the CC1 CON contrast on 3.75 of 6 CON episodes; group
x board x CC is not identifiable. The six persistent CON cages are the limiting independent replication.

**Technical board adjustment of read-dependent measures** (`Testing/audits/audit_board_adjustment_read_dependent.R`; run
`badj_v1_9b18624` in `<Behavior>/RANM/board_adjustment/`, on reconstruction `srs_v1.1_9b18624`): board offsets estimated from
SIS cage episodes within Batch x CC (recording fixed effects + board; rank checked), subtracted with their uncertainty
propagated exactly (a' V a, with a the board weights of the recordings in the estimate); adjusted and unadjusted contrasts
side by side; no p-values. A recording without a CON or a SIS cage episode in the population is left out of that contrast
and listed (`tables/dropped_recordings.csv`).

93 SIS cage episodes per outcome and phase (design rank 28 of 28, 65 residual df). Board offsets relative to board 1:

| | board 2 | board 3 | board 4 | board 5 |
|---|---|---|---|---|
| reads per hour, active | -100 (SE 214) | -912 (210) | -922 (204) | +442 (201) |
| 60-s support, active / light (points) | -1.1 / -1.5 | -5.6 / -10.1 | -6.9 / -13.1 | -0.1 / +0.1 |
| column changes per hour, active / light | -23.4 / -9.6 | -38.6 / -20.6 | -43.6 / -23.7 | -8.2 / -2.1 |

CON - SIS, pooled over cage changes (per batch the mean over CC, then the 6 batches; t interval, 5 df), unadjusted and
board-adjusted:

| outcome | phase | unadjusted | board-adjusted |
|---|---|---|---|
| reads per hour | active | -318 [-1149, 513] | -217 [-1019, 584] |
| reads per hour | light | -553 [-1408, 301] | -515 [-1431, 401] |
| 60-s support (points) | active | -1.2 [-7.6, 5.3] | -0.8 [-6.4, 4.9] |
| 60-s support (points) | light | -7.4 [-13.2, -1.6] | -6.7 [-12.7, -0.7] |
| column changes per hour | active | -1.1 [-24.3, 22.1] | +2.0 [-17.1, 21.1] |
| column changes per hour | light | -9.0 [-16.2, -1.9] | -7.4 [-16.9, 2.2] |

- At CC1 (5 of 6 CON cages on boards 3/4) the adjustment moves the active column-change contrast from -10.2 [-38.8, 18.4] to
  +7.4 [-15.1, 30.0] per hour and the light 60-s support contrast from -7.8 [-20.8, 5.3] to -2.5 [-12.0, 7.1] points.
- Leaving one batch out (offsets re-estimated without it), the pooled light 60-s support contrast ranges over -9.4 to -6.2
  points unadjusted and -8.0 to -4.7 adjusted; the light column-change contrast over -10.8 to -7.2 and -9.3 to -5.6 per hour.
- In cage episodes with 4 animals housed, 4 tags read and 4 tracked (all 6 batches; B1 contributes CC1 only, its CC2-CC4
  recordings have no such CON or SIS cage), the light 60-s support contrast is -3.6 [-10.2, 3.0] points unadjusted and
  -2.1 [-8.4, 4.3] adjusted, and the light column-change contrast -6.6 [-11.5, -1.6] and -3.2 [-9.5, 3.2] per hour.
- SUS - RES (24 outcome x phase x CC cells): the board adjustment moves the estimates by a median 0.2 (at most 0.7) of the
  unadjusted standard error, and closer to 0 in 12 of 24 cells (no systematic shift; board is nearly orthogonal to RES/SUS).
  The within-cage estimates (board- and cage-free by construction) move by a median 0.9 standard errors and lie closer to 0
  in 16 of 24 cells, e.g. active column changes at CC4: +13.8 [0.7, 26.9] unadjusted, +13.1 [0.7, 25.5] board-adjusted,
  +2.9 [-10.1, 15.9] within cages. The difference between the unadjusted and within-cage versions is a between-cage
  component, not a board effect.
- The one-stage model (same board effect in CON and SIS cages, standard errors clustered by batch) gives estimates and
  intervals similar to the batch-level ones (light 60-s support -6.8 [-13.6, 0.1] points); it assumes the CON offset equals
  the SIS offset, so the batch-level intervals remain the reference.
- This is a technical evaluation of the adjustment on read-dependent acquisition measures: estimates with intervals, no
  p-values, no group inference; nothing here selected a method or parameter.

Rules: board main effects only; no group x board (x CC) terms; cage change stays an experimental factor; occupancy fractions are
not adjusted (a board offset cannot recover an unobserved location); adjusted and unadjusted results are always shown
together; the registered Stage 29 models are unchanged.

## 7. Calibration pilot (one day; criteria fixed before measurement)

`docs/RFID_CALIBRATION_PILOT.md` (v1.1): a counterbalanced one-day protocol (boards 4 and 2 in one PhenoSoft session, ABBA by
halves, seeded placement order, repeat reference placements for drift), the seeded schedule, the decision criteria
`docs/rfid_calibration/acp_criteria_v1.0.csv` and the pre-registered analysis code, all hash-pinned before booking.
- **What favours the single-read basis** (criterion a): the paired difference in correct-column share (read minus event) over
  static boundary placements, crossings and short visits, pooled over both boards, D_col >= +0.05 with its lower 95% bound
  > 0, positive on both boards, and not worse at antenna centres (non-inferiority margin 0.02); the reverse favours the event
  basis; otherwise both bases are reported as sensitivity bounds (no winner chosen).
- **What can be calibrated** (criteria B1-B16): detection footprint, localisation at known positions (reader vs column
  resolution), missed reads and short gaps (the age classes), competition in one field and across fields, board-level load,
  the carry-forward bound, house shielding, roof height, board 4/2 sensitivity with repeatability, flicker (tau), boundary
  timing, crosstalk, tag variability, phantom reads, transfer to the Exp9 cadence.
- **What stays unidentifiable** (criterion c): positions of freely moving mice while unread, posture, huddles and nest
  material, behaviour-dependent detection, implanted-tag coupling, multi-day drift, boards 1, 3, 5, and any group difference.
- **Day 2** only if a pre-specified trigger shows that more data could change a conclusion (projection test); Tobias decides.

## 8. Stage 19: versioned revision after review (plan; nothing done now)

After the calibration assessment and the review of this specification:
1. freeze the reconstruction method and parameters (basis by criterion a, or both as bounds; tau; carry-forward bound;
   resolution) in a new reconstruction version;
2. build a new, separately versioned Stage 19 output (new output root and version tag; the current Stage 19 outputs stay);
3. four-column occupancy for the primary biological figures; eight-reader QC plots kept;
4. observation-age and reconstruction-method sensitivity in every comparison (LOCF, 60 s, 300 s; event and read basis);
5. original and revised figures compared side by side; a table of which conclusions are stable and which depend on
   preprocessing;
6. if the calibration is inconclusive, event-based and single-read results are reported as methodological sensitivity
   bounds; no winner is chosen by biological effects;
7. only then decide whether revised Stage 19 figures replace the manuscript figures.

## 9. Revised specification and downstream rules

| | construct | source and rule | status |
|---|---|---|---|
| S1 | displacement-event rate (Stage 29 crossing_rate; Stage 09 Movement) | vendor AnimalPos, unchanged. New primary analyses leave the 12 active windows of SX05-SX07 out and keep them in a sensitivity analysis; the light-phase rate is kept and evaluated separately | registered results unchanged |
| S2 | reader-change rate | raw antennas, column resolution, tau 2 s (calibrated tau* when available), per observed hour, with reads/h, n_tags_present and board | descriptive; never locomotion |
| S3 | spatial occupancy | srs (provisional basis by criterion a), **4 columns primary**; eight readers for QC, sensitivity and resource-location diagnostics; reported as (i) unrestricted LOCF for historical reference, (ii) max 60 s and (iii) max 300 s with the unknown share and the shares of supported time; manifest v1.1 masks | descriptive; candidate for revised Stage 19 |
| S4 | occupancy organisation | column dispersion and dominant-column share from S3 (supported time), with the unknown share | descriptive |
| S5 | fragmentation | vendor stream, frozen 39.4 s criterion; SX05-SX07 as in S1 | registered results unchanged |
| S6 | co-location | none as proximity or affiliation; shared_zone_use and reader co-assignment are not retained as such. A new spatial or social measure needs its own operational interpretation, justified separately | — |
| S7 | comparisons | cage episode as the unit; cage change as a design factor (group x CC); board main effect from SIS cages within Batch x CC for read-dependent measures, adjusted and unadjusted shown together; n_tags_present for read competition; six CON cages as the limiting replication | — |
| S8 | validation | one-day pilot with the pre-specified criteria; day 2 by rule only | not booked |
| S9 | keys and masks | SourceFile, Batch, CC, board, System, CageEpisodeID, AnimalNum, n_animals_present, n_tags_present, n_tracked; masks only from a released manifest version | — |

Interpretation: R1 food, R5 house and R8 water locate the resources and help interpretation; column or reader occupancy is not
precise resource engagement and not physical proximity.

Not done now: no Stage 29 rerun, no new behavioural model, no manuscript figure. After the specification has been reviewed
following the calibration pilot, a targeted revision of the affected spatial outcomes and models will be proposed; the
registered analysis stays as it is. No reconstruction method is selected by RES-SUS separation, CON differences, predictive R²
or p-values.

## 10. Stage 00b status and safety

The run status now has a scope: `manifest/qc_status_by_unit.csv` gives PASS / WARN / FAIL per board-recording, and the README
states e.g. "hard failures confined to 3 of 120 board-recordings (B2|sys.2|CC3, B2|sys.5|CC4, B5|sys.1|CC4) in 3 of 24
recordings; the other 117 board-recordings have no hard failure". A FAIL concerns the listed units only. The second live
run, `analysis_ready/quality_control/raw_acquisition/runs/raqc_20261014_25804cb` (Stage 00b code of 25804cb; `qc_run.csv`
records 79ec773, the HEAD when it finished, whose Stage 00b code is identical), shows it: FAIL for B2|sys.2|CC3,
B2|sys.5|CC4 and B5|sys.1|CC4 (H06; the last also W07, W16), WARN for 24 and PASS for 93 board-recordings; the ledger line
reads "FAIL (3 of 120 board-recordings)". Its protection check covered 1,035 files (including the first run's folder) with
0 changes, and all 118 pinned frozen files match. The findings are those of the first live run.

Safety: every new analysis ran in an isolated root; the append-only live QC root, the Stage 08 pins and the frozen-input guard
are unchanged; no registered stage reads the spatial manifest (test); S13 is unchanged (test pins its text).

## 11. Remaining methodological uncertainties

1. **Basis.** Which of the event and single-read bases localises a tag correctly at a boundary is unknown until the pilot;
   agreement with the latest read is definitional.
2. **Unread time.** 19.5% (active) and 45.1% (light) of observed time has no read in the last 60 s; 47% (active) and 60%
   (light) of it is held at the house, mostly while cage-mates are assigned there too. Read competition, occlusion and absence cannot be separated from these data, and
   static calibration cannot fully resolve a nest of moving animals.
3. **Bounds.** 60 s and 300 s are reporting conventions; the calibration bounds bench gaps, not nest gaps.
4. **Board.** Boards 3 and 4 read less and support less; whether this is hardware (pilot B10) or cage and occupancy is open.
   Board effects estimated from SIS cages assume the same offset in CON cages.
5. **Read competition and presence.** n_tags_present measures tags competing for reads; its effect on read-dependent measures
   is not calibrated (pilot B4-B6).
6. **Dead-channel bias.** The knockout models a silent antenna, not other changes on that board (W16 cadence shift).
7. **B5 CC4 board 1.** What happened after 14:00:42 and where the animals were for 52-71 h is unknown; the time is censored.
8. **Untracked animals in B1.** Six animals without a working tag were present; their positions are unknown, and the tracked
   animals' co-assignment and read competition omit them.
9. **Light-phase measures.** The light-window scales are group-blind but not frozen standardizers; light-phase dispersion,
   fragmentation and shared-position occupancy also depend on the dead channels (0.26-1.01 SD).
10. **Transfer.** Pilot results apply to boards 2 and 4 and to static or sled-moved tags; boards 1, 3, 5, implanted tags and
    multi-day drift remain uncalibrated.

## 12. Provenance

- Reconstruction release `<Behavior>/RANM/single_read_spatial/runs/srs_v1.1_9b18624` (commit 9b18624; manifest v1.1;
  24 recordings; code and input sha256 in `manifest/`). Every table equals that of the first v1.1 run `srs_v1.1_25804cb`
  byte for byte except `movement_exclusion_windows.csv`, where the 12 light windows of the excluded animals are now kept
  (`movement_excluded` FALSE; 25804cb flagged them) and `episode_movement_excluded` is added. 25804cb is kept for the
  record; its README still names specification v1.
- Parity: event basis = Stage 19 (21,312 cells, max 5e-11 s); emulated vendor records = v2 records in 888 of 888 Stage 29
  animal-windows (142,357 records); keys consistent in 444 of 444 animal-recordings.
- Earlier runs kept for the record: srs_v1.0_6165adf (specification v1), srs_v1.1_25804cb, idf_v1_622f5cb, idf_v1_6165adf,
  cep_v1_25804cb (table identical to cep_v1_9b18624), badj_v1_79ec773 (before the review fixes of 9b18624: weights over
  dropped recordings, the SUS - RES adjusted variance, leave-one-batch-out with fixed offsets, a one-stage interval without
  clustering; its CON - SIS batch-level contrasts and board offsets equal those of badj_v1_9b18624).
- Cage-episode presence `cep_v1_9b18624`; board adjustment `badj_v1_9b18624`; Stage 31 censored run
  `whole_inactive_sx04v11_20261014` (code of 79ec773; 9b18624 added input checks to the censoring, and the run was not repeated).
- Calibration pilot: criteria `acp_criteria_v1.0` and schedule `schedule_one_day_v1` hash-pinned in
  `Functions/antenna_calibration_pilot.R`; nothing measured.
