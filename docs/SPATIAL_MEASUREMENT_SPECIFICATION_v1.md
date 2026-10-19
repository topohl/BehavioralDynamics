# Revised spatial measurement specification v1 (for review, 2026-10-14)

**Superseded by `docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.1.md`** (decisions of 2026-10-14, second round). This file is the record of
the first proposal and its numbers (release run srs_v1.0_6165adf).

*Current record: `docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.5.md` (closeout of 2026-10-15: calibration will not be
performed). The status, plans and pending items below are historical and are not current instructions.*

**Status: proposal for review.** It works out Tobias's decisions of 2026-10-14 on the Stage 00b findings. Nothing in it is
applied to a registered analysis:
- Stage 19 and its outputs are unchanged and not replaced;
- the Stage 29 v1.0.1 release, its model specification and the frozen S13 exclusion are unchanged;
- the Stage 08 pins, the data versions and the live RFID files are unchanged;
- no Stage 29 rerun and no manuscript figure.

Everything new was computed in isolated roots and is group-blind: no CON/RES/SUS label, CombZ or outcome was read. Two
exceptions are stated where they occur: the identifiability check (section 6) reads the group *allocation* (labels, no
outcome), and the active-window knockout scales are the frozen Stage 29 standardizers, which the registered release defined
on CC1 SIS animals and which are inherited here, not recomputed (section 3.1).

| item | where |
|---|---|
| spatial exclusion manifest v1.0 | `docs/spatial_exclusions/spatial_analysis_exclusions_v1.0.csv` (loader and release pin: `Functions/spatial_exclusion_manifest.R`) |
| single-read reconstruction srs_v1.0 | `Functions/antenna_spatial_reconstruction.R`; builder `Testing/audits/build_single_read_spatial_reconstruction.R`; release run `<Behavior>/RANM/single_read_spatial/runs/srs_v1.0_6165adf/` |
| board x group identifiability | `Testing/audits/audit_board_group_identifiability.R`; run `<Behavior>/RANM/board_identifiability/runs/idf_v1_6165adf/` |
| calibration pilot | `docs/RFID_CALIBRATION_PILOT.md` |
| Stage 00b in the live QC root | `docs/RFID_ACQUISITION_QC.md`, section "Live QC root and output protection"; run `analysis_ready/quality_control/raw_acquisition/runs/raqc_20261014_6165adf/` |
| tests | `Testing/tests/test_antenna_spatial_reconstruction.R`, `test_spatial_exclusion_manifest.R`, `test_qc_run_protection.R` |

## 1. Summary

1. **Exclusions.** The three confirmed channel-dropout cage episodes are excluded from new spatial analyses that need complete
   antenna coverage, at cage-episode level: B2|sys.2|CC3 (R3), B2|sys.5|CC4 (R3) and B5|sys.1|CC4 (R5, the house). B5|sys.1|CC4 is
   in addition censored from its last read, 2023-12-04 14:00:42.962, to the span end. The records are in a separate,
   versioned manifest; S13 and the registered outputs are unchanged.
2. **Movement outcomes depend on the failed channel.** Deleting R3 or R5 from healthy boards and recomputing the vendor
   AnimalPos stream shifts the active-phase crossing rate (R5: -3.0/h, ratio 0.87; R3: +1.7/h), fragmentation, occupancy
   dispersion and shared-position occupancy by 0.3-1.2 standardizer SD (median |shift|). By the rule declared before the
   knockout this is material for every active-window construct and not for the light-phase crossing rate. Proposal: leave
   the three episodes' active-window crossing rate and fragmentation out of new analyses (decision needed; registered
   results unchanged).
3. **Single-read reconstruction.** The event-based reconstruction reproduces Stage 19 exactly. The single-read basis
   assigns a different reader to 8.5% of active and 4.85% of light seconds; **73% (active) and 65% (light) of that persists
   at column resolution** (6.2% and 3.2% of seconds). 80% (active) and 74% (light) of the difference lies within 2 s of a
   read. At the moment of a read, the event basis holds that read's reader for 75% (active) and 77% (light) of reads, the
   single-read basis for 94% and 95% (the rest are border flips under 2 s, which the flip filter removes). The difference
   depends on the board: on boards 1, 2 and 5 the event basis holds the read's reader for 65-71% of active reads and the
   bases differ in 11-13% of active seconds; on boards 3 and 4 it is 89-90% and 3%. Which basis is right at a boundary is
   not known without calibration.
4. **R1/R5/R8.** The single-read basis gives the house (R5) +2.6 percentage points of active and +1.4 of light time (cage-episode
   median +2.9 / +1.1), food (R1) +0.3 / -0.5 and water (R8) +0.5 / +0.1; column 1 (house and food) +2.9 / +0.9.
5. **Observation gaps.** Positions are more than 60 s old for 19.5% of active and 45.1% of light time; R5 (house) holds 47%
   and 60% of that stale time. Bounding the carry-forward at 60 s moves column shares by 1.8 (active) and 2.0 (light) points at the
   median cage episode (at most 4.0 and 8.5); at 300 s by under 1 point. Where an animal is during a long gap cannot be
   recovered from these data.
6. **Board.** Group and board are identifiable as main effects: CON VIF 1.01, effective CON n 23.6 of 24. The CC1 CON contrast
   becomes weakly identified once board x CC is in the model (3.75 of 6 CON episodes), because 5 of 6 CC1 CON cages sat on
   boards 3/4. Group x board x CC is not identifiable. Recommended: a board main effect estimated from SIS cages within
   Batch x CC, a tracked-tag covariate for read competition, cage change kept as a design factor.
7. **Keys.** Board, System, cage episode, batch and cage change agree in 444 of 444 animal-recordings across Stage 00b, the v2
   labels, Stage 19 and Stage 29.

## 2. Keys and units

Every new table carries `SourceFile`, `Batch`, `CC`, `board`, `System` (= `sys.<board>`), `CageEpisodeID` (= `Batch|System|CC`,
Stage 29's key) and `AnimalNum` (canonical). The board is the physical cage: the antenna board that holds most of an
animal's detection events in the span (Stage 19's rule). Stage 00b now writes `System` and `CageEpisodeID` on every
board-level table as well.

Consistency (`tables/metadata_consistency*.csv` of the release run): in all 444 animal-recordings the board of this build
equals the Stage 00b board, `sys.<board>` equals the v2 System label and Stage 19's System, and the cage episode equals Stage
29's `CageEpisodeID`.

Cage change stays an experimental factor: CON cages are never regrouped, SIS cages are regrouped at every cage change, and
CC1 also has the lowest detection support (section 5.5). It is never pooled as a nuisance term.

## 3. Exclusions and censoring (manifest v1.0)

| record | action | cage episode | reader | scope |
|---|---|---|---|---|
| SX01 | exclude_spatial | B2\|sys.2\|CC3 (OR106, OR112, OR131, OR141) | R3 | whole span; new spatial analyses that need complete antenna coverage |
| SX02 | exclude_spatial | B2\|sys.5\|CC4 (OR106, OR113, OR135, OR138) | R3 | as SX01 |
| SX03 | exclude_spatial | B5\|sys.1\|CC4 (314, 318, OR620, OR630) | R5 (house) | as SX01 |
| SX04 | censor | B5\|sys.1\|CC4 | all | from 2023-12-04 14:00:42.962 (the board's last read) to the span end; every measure |

**Affected spatial measures** (SX01-SX03): reader and column occupancy; occupancy organisation (dispersion, dominant share);
raw reader-change rates; same-reader or same-column co-assignment; both raw reconstructions; cage-use maps and their
contrasts. Other cage episodes of the same recordings are not affected.

**Censoring** (SX04): the four tags stop within 85 s, the board stays silent for 52.5 h while the other boards read to the
end. The time after the last read is unobserved: windows, denominators and carry-forward end there. It lies outside the
Stage 29 windows (2023-12-02 18:30 to 2023-12-03 18:30) and outside Stage 19's CC4 phases 1-2 (to 2023-12-04 06:30). Stage 31
(whole CC4 recording) includes it; no registered output is changed.

**Separation from the registered exclusions.** S13 (Stage 29) excludes 314, 318, OR620, OR630 and OR112, OR141 from occupancy
dispersion and shared-position occupancy only, and stays as it is. The manifest is read only by the isolated builder; no
pipeline stage reads it or the Stage 00b proposed exclusions (`test_spatial_exclusion_manifest.R` checks the code and pins
the S13 text). A released manifest is never edited: its content hash is pinned, and a change is a new version file.

### 3.1 Movement outcomes: dependence on the failed channel

The vendor AnimalPos stream is a function of the reads (the 500-ms mean of a tag's reads, written after a move of 200 grid
units or more; `docs/PHENOSYS_ANIMALPOS_RECONSTRUCTION.md`). On the 117 healthy board-recordings, each animal's vendor stream
was recomputed from all its reads (baseline) and with the reads of R3 or R5 on its board deleted (knockout), and the Stage 29
window metrics were computed from both (`mmm_bf_window_table()`, Stage 29's windows). The shift is knockout minus baseline,
on a scale per window (`tables/knockout_scales.csv`):
- active window: the frozen Stage 29 standardizers (inherited as registered: the CC1 SIS residual SDs, e.g. 5.17/h for the
  crossing rate);
- light window: no frozen standardizer exists, so the residual SD of `lm(y ~ Batch)` over the CC1 baseline light windows of
  all healthy animals, CON and SIS alike (no label read): 1.42/h for the crossing rate, 0.466 bits, 0.203 and 0.175 for
  the other three.

Materiality was declared before the knockout ran: material if the median |shift| reaches 0.2 SD or its 90th percentile
0.5 SD. No group or outcome enters it.

| reader deleted | window | construct | median shift | median \|shift\| (SD) | 90th pct \|shift\| (SD) | material |
|---|---|---|---|---|---|---|
| R3 | active | crossing rate | +1.67/h (ratio 1.07) | 0.33 | 0.90 | yes |
| R3 | active | fragmentation | -0.071 | 0.91 | 1.83 | yes |
| R3 | active | occupancy dispersion | +0.070 bits | 0.45 | 1.40 | yes |
| R3 | active | shared-position occupancy | -0.028 | 0.58 | 1.88 | yes |
| R3 | light | crossing rate | 0 (ratio 1.00) | 0.12 | 0.35 | no |
| R5 | active | crossing rate | -3.01/h (ratio 0.87) | 0.58 | 1.21 | yes |
| R5 | active | fragmentation | +0.081 | 1.04 | 2.05 | yes |
| R5 | active | occupancy dispersion | -0.161 bits | 0.91 | 1.89 | yes |
| R5 | active | shared-position occupancy | +0.059 | 1.16 | 3.02 | yes |
| R5 | light | crossing rate | -0.17/h (ratio 0.95) | 0.12 | 0.41 | no |

419-432 animal-windows per row (shared-position occupancy 430). In the light window, dispersion, fragmentation and
shared-position occupancy, which are not registered light-phase constructs, shift by 0.26-1.01 SD (median |shift|) on their
light-window scales. (The development run had put them on the active-window scales; the review corrected this before the
release run. The rule itself was not changed.)

**What S13's rule catches.** S13 flags an active window in which an animal never occupies one of the 8 vendor positions.
After deleting R5 that is true for 100% of animal-windows; after deleting R3 for only 23% (baseline 0.2%). Position 3 is
presumably still reached through window means of R2, R4 and R7 reads (inferred from the vendor rule, not tabulated). This fits
S13 holding all four B5|sys.1|CC4 animals, two of four B2|sys.2|CC3 animals and none of B2|sys.5|CC4.

**Proposal (decision needed):** for new analyses, leave the active-window crossing rate and fragmentation of the three
cage episodes out (12 animal-windows, all CC3/CC4; the CC1 primary windows are not affected), and keep their light-phase
crossing rate with a flag. The registered Stage 29 results are not changed; a Stage 29 sensitivity without those 12
animal-windows would be a separate decision.

**Limits of the knockout.** A deleted antenna reads nothing, as a dead one does. The knockout cannot reproduce other
changes a dead antenna may bring: Stage 00b found the remaining antennas of the dead-R5 board read at a shifted cadence
(W16; lattice z -3.3, and -2.0 on the B2 CC3 board), so the real effect may differ from the knockout's.

**Parity of the baseline with Stage 29** (`tables/parity_stage29_counts.csv`, `parity_stage29_any.csv`,
`vendor_record_parity_summary.csv`):
- Record for record, the emulated vendor stream equals the v2 records inside every Stage 29 window: 888 of 888
  animal-windows, 142,357 records (time within 1.5 ms, same position).
- Window metrics: the crossing rate is equal in 416 of 432 healthy active windows (the others differ by one position
  change), fragmentation in 420, the light-phase rate in 432 of 432. 59 healthy (63 in all) active windows differ by more
  than 0.01 SD in at least one construct, at most 0.58 SD (dispersion).
- With identical records inside the windows, these differences come from the state at the window start. Stage 29 seeds an
  animal whose first record falls after the file's first record from the last raw-export record that lies exactly on an
  antenna coordinate (`mmm_evs_seeds()`), and starts the others without a prior state; the emulation carries the last
  record before the window. Shared-position occupancy then also changes for the cage-mates.
- Both knockout streams use the same start, so the shifts do not depend on it.

## 4. Reconstruction srs_v1.0 (definition)

**Observations.**
- Event basis: detection-event starts (Stage 19 and the data versions), with Stage 19's own function
  `rfid_raw_occupancy_intervals()`.
- Read basis: every single read at its logged time, the event start plus the cumulative MsgValue1 gaps on the millisecond
  grid of `apr_reads()` (the times that reproduce the vendor records to the millisecond).
- Reader: the antenna's PositionID from the log's own device table (checked in 24 of 24 recordings by Stage 00b).
- Simultaneous reads: reads of one tag at two readers in the same millisecond are one observation (0.03% of reads, always
  on one board). It takes the reader held before it if that reader is among them, otherwise the lowest PositionID.
  Deterministic; it decides 0.03% of observations.
- Own board only (Stage 19's rule); off-board reads are counted, not used.

**Assignment.**
- The animal holds the reader of its latest observation (carry-forward), seeded by the last observation before the span.
  All 444 animal-recordings have a seed in both bases.
- Border flips shorter than 2 s are reassigned to the reader returned to (`rfid_flip_filter()`, tau 2 s), as in Stage 19 and
  data version flip2s.

**Uncertainty.** The observation age of each second is the time since the animal's latest read on its board, in both bases.
Every assigned second carries its age class (<= 2, 2-10, 10-60, 60-300, > 300 s). A bounded reconstruction keeps an
assignment while the age is at most delta (2, 10, 60 or 300 s); LOCF keeps it until the next observation.

**Boundaries and masks.**
- Phase blocks: the vendor calendar of the v2 preprocessed file (`antenna_dv_vendor_calendar()`). Span: from the first block
  start to the end of the last block, clipped to the end of the raw recording (as Stage 19). Every interval is cut at the
  block edges.
- Stage 19's CC4 crop (phases 1-2) is used for the parity check only; the summaries use the whole span.
- The manifest masks are applied identically to both bases: SX01-SX03 left out of the summaries, SX04 cut and counted as
  censored.

**Resolution.** 8 readers and 4 columns (each column pools its two readers).

**Product** (release run, `rec/<B>_<CC>/read_basis_runs.csv.gz`): one row per read-basis run piece (animal, reader, start,
end, block) with its supported seconds at each bound. With `tables/occupancy.csv` (both bases, per animal, block and reader,
LOCF and bounded) and `tables/crosstab.csv` (event-basis reader x read-basis reader seconds, with age), every comparison
below can be recomputed at any of these resolutions and bounds.

## 5. Results (release run; 117 cage episodes after SX01-SX03; whole spans)

### 5.1 Parity

- **Event basis = Stage 19:** 21,312 animal x phase x reader cells over all 24 recordings (Stage 19's phase table, its CC4
  crop); largest difference 5e-11 s.
- **Knockout baseline = Stage 29:** see section 3.1.

### 5.2 Event-start against single-read basis, reader and column resolution (LOCF)

| phase | seconds reassigned, reader | seconds reassigned, column | share that persists at column | cage-episode median (IQR), reader | cage-episode median (IQR), column |
|---|---|---|---|---|---|
| active | 8.50% | 6.24% | 73% | 9.7% (3.3-12.5) | 6.9% (2.0-9.4) |
| light | 4.85% | 3.17% | 65% | 4.5% (1.9-6.5) | 3.1% (0.8-4.1) |

- **By observation age:** 80% (active) and 74% (light) of the reader-level reassignment is in seconds within 2 s of a read; there
  15.8% and 14.3% of seconds are assigned differently. Beyond 300 s it is 1.3%.
- **Agreement with the latest read** (`tables/read_agreement.csv`; at every in-span read): the event basis holds the reader
  just read for 75.1% (active) and 76.6% (light) of reads, its column for 81.4% and 84.6%; the single-read basis for 94.1% and
  95.4% (column 95.7% and 97.0%). Its remaining reads are border flips under 2 s that the flip filter reassigns by design.
- **Main flows (event reader -> read reader), active:** R6 -> R5 29%, R2 -> R1 14%, R1 -> R5 11%, R5 -> R6 8%. Light: R6 -> R5 28%,
  R1 -> R5 19%, R5 -> R6 16%. This is consistent with the event basis holding a corridor reader whose short event started
  while the tag was still being read at the house or food antenna (event overlap was not tabulated per board).
- **By board (active):** boards 1, 2, 5: 13.3%, 11.0%, 13.0% of seconds differ at reader level (9.3%, 8.9%, 9.9% at column
  level), and the event basis holds the read's reader for 64.6%, 70.7%, 66.5% of reads; boards 3, 4: 3.1%, 2.9% (2.1%, 1.6%)
  and 89.4%, 90.3%. The basis difference is therefore board-dependent; boards 3 and 4 have the lowest crosstalk.
- **By cage change (active):** 7.0% (CC1) to 9.3% (CC4).
- **Reader changes per animal-span** (median over the 432 animals of the 117 cage episodes; whole spans, SX04 not cut): event
  raw 10,960; event tau 2 8,309; read tau 0 21,055; read tau 2 6,767.

### 5.3 R1 (food), R5 (house), R8 (water) by phase

Pooled shares of carry-forward time, event -> read basis, and the read minus event difference per cage episode:

| phase | reader | event | read | difference | cage-episode median (IQR) | range |
|---|---|---|---|---|---|---|
| active | R1 food | 28.9% | 29.2% | +0.3 | 0.0 (-0.3, +0.7) | -2.5, +4.0 |
| active | R5 house | 33.6% | 36.2% | +2.6 | +2.9 (+0.6, +4.1) | -5.8, +14.8 |
| active | R8 water | 5.3% | 5.8% | +0.5 | +0.4 (+0.3, +0.7) | +0.1, +2.2 |
| light | R1 food | 16.0% | 15.5% | -0.5 | -0.3 (-0.9, 0.0) | -7.1, +1.2 |
| light | R5 house | 53.1% | 54.5% | +1.4 | +1.1 (+0.3, +2.0) | -14.5, +42.2 |
| light | R8 water | 0.96% | 1.08% | +0.1 | +0.1 (0.0, +0.2) | -0.2, +1.2 |

Percentage points. Of the R5 gain, R6 (next to the house) gives up 1.6 (active) and 0.3 (light) points and R2 1.2 and 0.5;
the rest comes from several readers.

Column occupancy (LOCF), event -> read: active C1 62.4 -> 65.3%, C2 21.7 -> 19.0%, C3 6.0 -> 5.6%, C4 9.8 -> 10.2%; light C1 69.1 ->
70.0%, C2 27.4 -> 26.7%, C3 1.8 -> 1.7%, C4 1.6 -> 1.7%. Per cage episode the column-1 difference has median +2.6 (IQR 0.5 to
4.7) active and +0.6 (0.1 to 1.4) light.

### 5.4 Maximum carry-forward age

Share of LOCF time with an observation age within the bound (identical in both bases, since age is read-based):

| bound | 300 s | 60 s | 10 s | 2 s |
|---|---|---|---|---|
| active | 94.2% | 80.5% | 60.7% | 43.1% |
| light | 79.0% | 54.9% | 36.7% | 25.4% |

Change of column shares against LOCF, read basis, in percentage points (pooled):

| phase | column | <= 300 s | <= 60 s | <= 10 s |
|---|---|---|---|---|
| active | C1 (food, house) | -0.5 | -1.8 | -3.4 |
| active | C4 (water) | +0.5 | +1.7 | +3.7 |
| light | C1 | -1.1 | -2.4 | -3.5 |
| light | C4 | +0.3 | +1.0 | +1.7 |

The house (R5) loses most: -1.3 / -2.6 / -3.3 points (active) and -2.1 / -4.4 / -6.1 (light) at 300 / 60 / 10 s. Per cage
episode, the largest column change at 60 s has median 1.8 points (max 4.0) active and 2.0 (max 8.5) light; at 300 s 0.6 and
0.9. The event basis behaves alike (column-1 change at 60 s: -2.3 active, -3.0 light).

### 5.5 Remaining uncertainty from observation gaps

| phase | age > 60 s | age > 300 s | share of the > 60 s time held at R5 / R6 / R1 |
|---|---|---|---|
| active | 19.5% | 5.8% | 47% / 17% / 26% |
| light | 45.1% | 21.0% | 60% / 23% / 13% |

- By cage change, positions older than 60 s fall from 24.2% (CC1) to 15.7% (CC4) of active time and from 53.6% to 39.5% of
  light time.
- By board: boards 3 and 4 hold 22-24% (active) and 50-52% (light) stale time against 16-18% and 40-41% on boards 1, 2 and 5;
  relative to their recording's median, the 60-s support of boards 3/4 is 0.97/0.94 (active) and 0.93/0.84 (light).
- A long gap is "no recorded read", not stillness: read competition (one tag per poll), posture, nest height and shielding
  are not separable without calibration. The house time is the least supported and the most basis- and bound-sensitive;
  in the active phase water is the best supported (positions older than 60 s: 5%); in the light phase R4 (11%) and R8 (15%).
- Not correctable from these data: where an animal is while unread; whether a stale house assignment is the house. A
  probabilistic location model would identify detection probabilities but not unread positions (pilot of 2026-10-13).

## 6. Board as an acquisition factor, and identifiability

Design-only check (`audit_board_group_identifiability.R`; group allocation read, no outcome; cage episode = Batch x CC x board,
120 = 24 x 5; each recording uses all 5 boards; the CON cage sits on one board per Batch x CC).

**Allocation.**
- CON cage episodes per board 1-5: 4/6/6/5/3 (expected 4.8 each). On boards 3/4: 11 of 24 (45.8%, expected 40%); SIS 37 of 96 (38.5%).
- **At CC1, 5 of 6 CON cages were on boards 3/4** (all 3 male: B1 b3, B2 b4, B5 b3; female B3 b3, B4 b3); CC2 1 of 6, CC3 2, CC4 3.
- Board 5 (fastest) held CON 3 of 24 times, never at CC1 or CC4.
- SIS per board x CC is thinnest at CC1 board 3 (2 cages). Four board x CC cells never held CON (CC1 b1, b5; CC2 b3; CC4 b5).
- RES/SUS: boards 3/4 share 39.2% / 39.0% (CON 45.8%); r(SUS, boards 3/4) = -0.002, within recording 0.009.
- The three dropout episodes are all SIS cages.
- A separate confound: CON cages always hold 4 tracked tags; 26 of 96 SIS episodes hold 1-3 (B1: 14 of 16). Read competition
  depends on the number of co-located tags, so read-dependent measures need it too.

**Identifiability (model matrices, rank, alias, VIF, effective CON n).**

| model for a read-dependent cage-episode measure | rank | CON VIF | effective CON n | verdict |
|---|---|---|---|---|
| group + CC + Batch | full | 1.00 | 24 of 24 | identifiable |
| + board (main effect) | full | 1.01 | 23.6 | identifiable, nearly orthogonal |
| + board x CC | full | 1.20 | 20.0 (CC1 contrast 3.75 of 6) | CC1 weakly identified |
| group x board | full | 6.0 (GVIF 15.9) | 3-6 per board | weakly identified |
| group x board x CC | 5 aliased | — | 2.1 | not identifiable |
| board x recording | saturated | — | — | not identifiable |
| SIS-only board effects within Batch x CC | 28 of 28, 68 residual df | — | 23.5 (CC1 5.3) | identifiable, group-blind |
| group-blind crosstalk covariate (per board-recording) | full | 1.02 | 23.4 | identifiable, but 88% of it is board and its within-board part tracks tag count (r -0.42) |

The binding limit for any CON contrast is the 6 persistent CON cages (effective n about 11 at a cage ICC of 0.5), not board.
RES vs SUS is essentially orthogonal to board, and a within-cage contrast (cage-episode fixed effects, 69 mixed cages) is
board-free by construction.

**Recommendation.**
- Keep the registered Stage 29 model as it is.
- For new read-dependent measures, estimate a board main effect from SIS cages within Batch x CC (recording fixed effects
  plus board) and carry its uncertainty. The one-stage `group x CC + Batch + board` model is the equivalent if the board
  effect is the same in CON and SIS cages.
- Add the number of tracked tags (or a 4-animal-cage restriction) for read competition.
- Never add board x CC, group x board or board x recording to a model that estimates group. A CC-specific board check is a
  sensitivity (boards 3/4 x CC; CC1 CON effective n 4.8), with CC1 reported as weakly identified.
- Do not use the board cadence signature as a sensitivity proxy: it marks board 5, not boards 3/4.

## 7. Revised specification

| | construct | source and rule | status |
|---|---|---|---|
| S1 | thresholded displacement-event rate (Stage 29 crossing_rate, Stage 09 Movement) | vendor AnimalPos, unchanged; ge200 as the raw sensitivity. **New:** the active-window values of the three dead-channel cage episodes are left out of new analyses (proposal, section 3.1) | registered, not reopened |
| S2 | reader-change rate | raw antennas, read basis, tau 2 s, at **column** resolution; tau 0.5/1/5/10 s as sensitivity; reported with reads/h, tracked tags and board | descriptive only, never "locomotion" |
| S3 | spatial occupancy | raw antennas, **read basis** (event basis as the Stage 19-comparable sensitivity), tau 2 s, **4 columns** (readers as sensitivity); LOCF reported with the supported share at 60 s; bounded 60 s and 300 s as sensitivity; manifest v1.0 masks; whole spans, Stage 19's CC4 crop where comparability with Stage 19 matters | descriptive; Stage 19 unchanged until decided |
| S4 | occupancy organisation | column dispersion and dominant-column share from S3; reader level as sensitivity | descriptive |
| S5 | fragmentation | vendor stream at the frozen 39.4 s only; dead-channel episodes as in S1 | registered |
| S6 | co-location | no proximity construct from RFID. If described at all: column-level co-assignment with its time-shift expectation, per cage, with read competition stated; dead-channel episodes excluded | descriptive, not social |
| S7 | comparisons | cage episode as the unit; cage change as a design factor (group x CC); board main effect from SIS cages within Batch x CC and a tracked-tag covariate for read-dependent measures; no board x CC or group x board terms with group; CC1 CON contrasts reported as weakly identified when board-adjusted | — |
| S8 | validation | calibration pilot (`docs/RFID_CALIBRATION_PILOT.md`) before any fine-scale spatial claim; it decides tau, the carry-forward bound, reader vs column resolution and the basis on measured values | proposal |
| S9 | keys and masks | every table: SourceFile, Batch, CC, board, System, CageEpisodeID, AnimalNum; masks only from a released manifest version | — |

**Why the read basis (measurement grounds only).** The observation age is defined by reads. At the moment of a read the
single-read basis holds that read's reader for 94% of reads (the rest are filtered border flips), the event basis for 75%;
within 2 s of a read the two differ in 15.8% of active seconds. The event basis's disagreement with the latest read depends
on the board (65-71% agreement on boards 1, 2, 5 against 89-90% on boards 3, 4), so it would bring a board-dependent term
into any spatial comparison. Agreement with the latest read is a definition, not accuracy: which basis is *correct* at a
boundary is not known without ground truth, and the calibration endpoints E2 and E6 decide it. No group effect or
significance was looked at.

## 8. Calibration pilot

`docs/RFID_CALIBRATION_PILOT.md`: two rack days without animals on one board 3/4 board (board 4) and a reference board (board 2):
- known tag positions and orientations (8 centres x 3 orientations, 13 boundaries, quarter points, corners, 3-cm height);
- the house, hopper and bottle with and without the object;
- 1-4 tags in the house and in the open (competition);
- controlled passes and short visits across adjacent zones, timed on video against the PC clock;
- known-empty boards all day.

Endpoints and the decision each informs:

| endpoint | informs |
|---|---|
| E1 detection map and missed reads | the meaning of "assigned to reader k" and the carry-forward bound |
| E2 localization confusion by basis | reader vs column resolution; read vs event basis |
| E3 crosstalk | QC thresholds and emission probabilities |
| E4 competition curve | read-density denominators; house unread time |
| E5 shielding | R5/R1/R8 read densities |
| E6 boundary behaviour | tau and the basis |
| E7 board ratio | board covariate |
| E8 phantom reads | QC rules |
| E9 cadence | transfer to Exp9 |

60-s static placements suffice for the three-class detection call (lower bound 0.94 when every second is read).

Resources: about 10 board-hours under test (38 known-empty board-hours alongside), 2 people on site for 7.3 h + 5.5 h, 9 spare
tags of the Exp9 type, about 64-70 person-hours in total; a one-day minimal version is specified. Static tags do not validate
freely moving implanted mice (posture, huddles, soiled bedding, dark-phase speed). A single filmed animal is noted as a later
option only. Nothing is booked.

## 9. Stage 00b in the live QC root

Run `raqc_20261014_6165adf` (committed code 6165adf, all 24 recordings) is the first run in the live QC root. Checked after it:
- it wrote one new folder, `analysis_ready/quality_control/raw_acquisition/runs/raqc_20261014_6165adf/`, and one line in
  `runs_ledger.csv`; nothing else under the root;
- protection check: 578 protected files (inputs, Stage 00 roots, Stage 08 and 19 outputs, Stage 29 releases, the registered
  exclusion definitions), 0 changes (`manifest/protection_check.csv`);
- frozen-input guard: passed; an independent re-hash after the run matched all 118 pinned files (72 Stage 08 outputs,
  46 Stage 29-32 inputs);
- QC status FAIL, from the same three hard failures (H06 channel dropouts). The manifest and all 84 flags are identical to
  the review run `raqc_20261013_b1783bf`, which adds only the `System` and `CageEpisodeID` columns;
- the run id is refused if reused (the folder exists and the ledger lists it);
- Stage 00b is still outside `Analysis/run_all_analysis.R`.

The QC manifest's proposed exclusions are review items. The adopted spatial exclusions are the separate manifest v1.0;
S13 is unchanged.

## 10. Decisions for Tobias

1. **Movement outcomes of the three dead-channel episodes** in new analyses: leave the active-window crossing rate and
   fragmentation out (proposal), or keep them flagged? A Stage 29 sensitivity without these 12 animal-windows?
2. **Basis of new spatial analyses:** single reads (proposal) or event starts (Stage 19 continuity)?
3. **Resolution:** 4 columns primary, readers as sensitivity (proposal)?
4. **Carry-forward:** LOCF with the 60-s supported share reported, bounded 60 s and 300 s as sensitivity (proposal), or a
   bounded primary?
5. **Board handling** for read-dependent measures: SIS-estimated board main effect plus tracked-tag covariate (proposal)?
6. **Calibration pilot:** two-day version, one-day minimal version, or none?
7. **Stage 19:** when to move it to the read basis and column resolution (it stays as it is until then).

## 11. Provenance

- Release run `srs_v1.0_6165adf` (commit in its `manifest/run.csv`): code and input sha256 in `manifest/`, the manifest version
  used in `manifest/spatial_exclusions_used.csv`, one process, 24 recordings.
- Inputs read-only: raw MultiMouseMonitor logs, v2 preprocessed files (roster, calendar), vendor AnimalPos exports (tag map),
  Stage 19's phase table, the Stage 29 v1.0.1 window metrics (keys and metric values only), the Stage 00b review run's
  animal table.
- Identifiability: `idf_v1_6165adf` (committed script); an earlier run of the same analysis before the commit,
  `idf_v1_622f5cb`, gave the same numbers.
- A development run of the same reconstruction (scratch, not kept) gave the same numbers in every table except the
  light-window knockout rows: their scale was corrected after the review (section 3.1).
