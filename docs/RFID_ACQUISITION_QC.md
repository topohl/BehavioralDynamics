# Raw RFID acquisition QC (Stage 00b)

**Status:** manual diagnostic, with provisional rules (`aqc_rules_v0.2_provisional`), 2026-10-13; integrated into the live QC
root with append-only runs and output protection on 2026-10-14 (section "Live QC root and output protection").

**Current status (closeout, 2026-10-15):** operational, run manually; it is not part of `run_all_analysis.R`, and its flags
and proposed exclusions do not filter any downstream analysis. The exclusions adopted for new spatial analyses are the
released manifest `docs/spatial_exclusions/spatial_analysis_exclusions_v1.1.csv`; the current specification is
`docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.5.md`. Calibration will not be performed, so the rules stay provisional and
data-derived. The live root's `REVIEW_STATUS.md` still names manifest v1.0 and specification v1; it lies in the protected,
append-only QC root and was left unchanged.

- It is **group-blind**: no CON/RES/SUS label, CombZ or outcome is read, and no pass/fail decision depends on them.
- Flags and proposed exclusions are **never applied**.
- The frozen S13 exclusion list, the Stage 08/09/29 outputs, the data versions and the live RFID files are **unchanged**.

v0.1 was revised to v0.2 after an adversarial review of the code and of the first run's findings. The changes are listed at the end.

## Where it sits

Stage 00b is the raw-log companion of Stage 00 (`Analysis/00_qc_tracking_integrity.R`):
- Stage 00 describes stationary trajectories in the binned Stage 01 metrics.
- Stage 00b checks how the positions were acquired, in the raw MultiMouseMonitor logs.

It follows the Stage 00 conventions:
- a manual diagnostic outside the runner;
- new run-id folders only, under `analysis_ready/quality_control/raw_acquisition/runs/<run_id>/`;
- a hand-written `REVIEW_STATUS.md` at the QC root after review;
- manifests in the existing style.

| file | role |
|---|---|
| `Analysis/00b_qc_raw_acquisition.R` | runner: one recording at a time; resumable (`rec/<B>_<CC>/DONE.txt` holds code and input hashes); built in `.tmp_<run_id>` and renamed at the end; a run into the project root needs committed code |
| `Functions/antenna_acquisition_qc.R` | checks, rules catalogue `aqc_rules()`, evaluation `aqc_evaluate()` |
| `Functions/tracking_qc_run_config.R` | `mmm_raw_acquisition_qc_run_config()`: `--run-id` (1-40 characters), `--output-root` (isolated), `--recordings` (regex, isolated only), `--resume` |
| `Functions/project_paths.R` | `mmm_raw_acquisition_qc_root()`, `mmm_raw_antenna_root()`, `mmm_isolated_output_root_check()` |
| `Functions/qc_run_protection.R` | append-only runs: run ledger, protected-location snapshots, output-root assertion, publish step |
| `Functions/frozen_input_guard.R` | the frozen-input guard (pinned Stage 08 outputs and Stage 29-32 inputs), called before and after the run |
| `Testing/tests/test_raw_acquisition_qc.R`, `test_raw_acquisition_qc_path_boundary.R`, `test_qc_run_protection.R` | portable tests: hand-worked functions, every rule on injected cases, a synthetic log with a dead antenna, run configuration and output boundaries, ledger, protection snapshot and publish refusals. Mutations of the key logic are caught |

It reuses the raw reader (`Functions/antenna_log_helpers.R`), the read-time and reconstruction functions of the measurement validation (`Functions/antenna_measurement.R`) and the v2 roster and phase calendar.

## Running

```bash
Rscript Analysis/00b_qc_raw_acquisition.R --run-id=raqc_YYYYMMDD                         # project root; committed code
Rscript Analysis/00b_qc_raw_acquisition.R --run-id=dev01 --output-root=<absolute folder> \
        --recordings='B2_CC[34]'                                                         # isolated test run
```

Isolated roots are refused in four kinds of place:
- inside the project root, in drive or UNC spelling, or with trailing dots or spaces;
- inside the configured raw-log root, `Raw Data` or `MMMdata`;
- inside RANT, RANU, RANV or RANA;
- inside `RANM/measurement_validation`.

A full run takes about 20-25 minutes (0.5-1.4 min per recording, plus input hashing).

## Live QC root and output protection

Stage 00b writes into the live QC root `analysis_ready/quality_control/raw_acquisition/` like Stage 00 writes into
`quality_control/tracking_integrity/`. It stays a manual diagnostic: it is not in `Analysis/run_all_analysis.R` and is run
by hand. Its runs are append-only:
- **Versioned run folders.** Each run gets a new folder `runs/<run_id>/`, built in `runs/.tmp_<run_id>/`. An existing folder
  is refused at the start and again at publication. A partial `.tmp_` folder is continued only with `--resume`.
- **Run ledger.** `runs_ledger.csv` at the root gets one line per published run: id, time, commit, rules version, status,
  recordings, sha256 of the run's output manifest, and the number of protected files checked. An id in the ledger is
  refused even if its folder was moved. A line is appended only if the ledger still starts with the bytes it had when the
  run started (a peer run's appended line is allowed; a rewritten earlier line stops publication). Publication holds a
  lock folder (`runs_ledger.lock`), checks the ledger before renaming the run folder, and moves the run back to its
  `.tmp_` folder if the ledger line cannot be written.
- **Output root.** The root must not lie in, equal or contain a protected location (`mmm_qc_assert_target()`): the raw
  logs, the v2 data version, the vendor exports, the Stage 00 roots, the Stage 08 and Stage 19 outputs, the Stage 29
  releases, `analysis_ready/pipeline`, `canonical` and `foundations`.
- **Protection check.** At the start, a snapshot records size and time stamp of every file in the protected locations:
  - the inputs (raw logs, v2 preprocessed files, vendor exports);
  - the Stage 00 roots (tracking integrity, new and historical);
  - the Stage 08 outputs (`analyses/hmm_states`) and the Stage 19 outputs (`analyses/spatial_occupancy`);
  - the Stage 29 releases;
  - every earlier run of the same root.

  The registered exclusion definitions are hashed in full (sha256): `raw_data/excluded_animals.csv`,
  `Functions/behavior_analysis_config.R` (the S13 sensitivity) and the spatial exclusion manifests
  (`docs/spatial_exclusions/`). Before publication the snapshot is taken again. Any added, removed or changed file stops
  the run: nothing is published, the ledger is untouched, and the `.tmp_` folder keeps `manifest/protection_check.csv` for
  inspection.
- **Frozen-input guard.** `mmm_frozen_guard_before("00b")` confirms that no pinned file lies under the QC root, and
  `mmm_frozen_guard_after()` re-checks every pinned file (the 72 Stage 08 files and the Stage 29-32 inputs) before
  publication.
- **No silent change to registered exclusions.** The QC manifest and its proposed exclusions are review items. No
  registered stage reads them (`Testing/tests/test_spatial_exclusion_manifest.R` checks the code). Exclusions adopted for
  new spatial analyses are recorded separately in the versioned spatial manifest
  (`docs/spatial_exclusions/spatial_analysis_exclusions_v1.0.csv`, since 2026-10-14 superseded by the released v1.1); the
  frozen S13 list is unchanged.
- **Keys.** Every board-level table carries `System` (sys.<board>) and `CageEpisodeID` (Batch|System|CC), the keys of
  Stage 29 and of the revised measurement tables.

**First live run (2026-10-14):** `runs/raqc_20261014_6165adf/` (commit 6165adf). The protection check covered 578 files with
0 changes, the frozen-input guard passed (an independent re-hash matched all 118 pinned files), and the ledger holds its
line. Its findings equal the review run `raqc_20261013_b1783bf` (identical manifest and 84 flags, plus the new keys). Its
status is FAIL because of the three confirmed channel dropouts.

**Status scope (2026-10-14).** A FAIL concerns the board-recordings that carry the hard failure, not the recording or the
data set. `manifest/qc_status_by_unit.csv` gives PASS / WARN / FAIL per board-recording (cage episode); recording-level checks
(board NA) count for every board of their recording; W14/W15 review ranks set no status. The README and `qc_run.csv`
(`qc_scope`, `board_recordings_fail/warn/pass`) state the scope, and the ledger records e.g. "FAIL (3 of 120
board-recordings)". Second live run `runs/raqc_20261014_25804cb`: FAIL for B2|sys.2|CC3, B2|sys.5|CC4 and B5|sys.1|CC4,
WARN for 24, PASS for 93 board-recordings; protection check 1,035 files, 0 changes; findings identical to the first run.

The protection check covers what a run could plausibly touch, not the whole drive, and compares the end of the run with
its start, not with a registered baseline (the S13 text and the released spatial manifests are pinned by
`Testing/tests/test_spatial_exclusion_manifest.R`). A `--resume` takes its snapshot when it resumes, so a change made while
a run was interrupted is not seen. A peer session that edits a protected file during the 20-25 min of a run makes
publication fail; the run then stays in `.tmp_` and can be published with `--resume`. `manifest/output_files_sha256.csv`
does not list itself or the protection files written at publication (`protection_check.csv`,
`protection_snapshot_after.csv`).

## Outputs (run folder)

- `README.txt`: status (PASS / WARN / FAIL), counts by severity, rules version, commit.
  - FAIL: any hard failure.
  - WARN: any warning except the relative review ranks W14 and W15.
- `manifest/qc_manifest.csv`: one row per check.
  - Columns: rules_version, check_id, severity, level, check, metric, comparator, threshold, unit, affects, rationale, status, n_checked, n_flagged, detail.
  - Status is pass, flagged, undecidable, not_evaluated or info.
- `manifest/qc_flags.csv`: one row per flagged unit.
  - Columns: check_id, severity, level, SourceFile, Batch, CC, board, PositionID, reader, resource, AnimalNum, block, Phase, value, threshold, classification, confidence, evidence, metrics_compromised, proposed_action, in_frozen_S13.
- `manifest/qc_proposed_exclusions.csv`: board-recording, scope, time range, reason, confidence, animals, the animals already in S13, and the status "proposed; not applied".
- `manifest/`: also `qc_rules.csv`, `qc_run.csv`, `code_hashes.csv` (sha256 and git blob), `input_hashes.csv` (v2 file, tag map, raw-log candidates, placement scans), `output_files_sha256.csv`, `session_info.txt`, and the protection files `protection_snapshot_before.csv`, `protection_snapshot_after.csv` and `protection_check.csv` (the last two written at publication).
- `tables/`: the QC tables, with column names matching the rules.
  - recording_qc, board_qc, reader_qc, reader_block_qc, stretches, stretch_calibration, animal_qc, animal_block_qc, animal_reader_block, basis_crosstab, coassign_by_reader, silences, nonroster_tags, system_messages, blocks, inputs.
  - Summaries: reader_summary, reader_spread, column_sensitivity, basis_by_reader, basis_by_recording, basis_flow, coassign_by_reader_summary, switching_summary, spatial_completeness, stretch_calibration_summary, board_history.
  - The summaries leave the dropout board-recordings out, except spatial_completeness, which flags them.
- `figures/qc/`:
  - qc01 labelled 4 x 2 layout;
  - qc02 relative reader co-detection (best-neighbour co-read ratio);
  - qc03 board signatures;
  - qc04 support by reader;
  - qc05 column occupancy against the age of the supporting read;
  - qc06 event-start vs single-read basis by reader;
  - qc07 co-assignment by reader;
  - qc08 silent-stretch scan, with the dropouts on the same scale.

## The cage layout and how QC uses it

| | C1 (x = 0) | C2 (x = 100) | C3 (x = 200) | C4 (x = 300) |
|---|---|---|---|---|
| upper row (y = 116) | **R5 house\*** | R6 | R7 | **R8 water\*** |
| lower row (y = 0) | **R1 food\*** | R2 | R3 | R4 |

\* Lab-reported. No PhenoSys configuration, protocol or note records where the resources were placed or how the housing sat on the antenna plate.

**Grid mapping.** It is identical in every log: antenna k of board AM<n> sits at x = 100·floor((k−1)/2), y = 0 for odd k and y = 116 for even k. This is checked as hard failure H01.

**The house end is supported.**
- In the light phase, animals rest at the C1 end in 120/120 cage x CC.
- R5 is the top light-phase reader in 110 of them, and R6, next to it, in most of the others.
- One video frame (B6 regroup 1, cage 1) shows house and food at one end and the spout at the other, on the house's side.

**Food and water rows are consistent but inferred.**
- R1 is a dark-phase site in 119/120.
- R8 is a dark-only visit site in 120/120 but leads R4 only modestly (57% of column-4 reads).
- The row within a column is the least certain quantity.

**Exceptions** from the scoping table (no check depends on them):
- resting place shifted to column 2: B1 CC1 b5, B2 CC3 b2, B4 CC3 b1, B4 CC4 b2, B6 CC3 b2;
- shifted to the lower row: B1 CC3 b3, B1 CC3 b4, B1 CC4 b1;
- unstable, with far-end resting in the first light block: B4 CC4 b1;
- B5 CC4 b1 (dead R5, then removal).

**How the layout is used.** It labels figures and readers but never decides anything:
- no check assumes uniform occupancy or reads;
- read counts are never equated with occupancy;
- low reads at R5, R1 or R8 are never a failure by themselves (huddling, shelter, climbing on hopper or bottle);
- detection counts are not normalized to make reader occupancy uniform.

## How a reader failure is recognised

A tag is read not only by the antenna under it. Within about a second, its row neighbours and its column partner read it too (crosstalk). This is documented by static-tag scans, and by the 11-26% of events that overlap at neighbouring antennas.

**Co-read counting.** For each reader k and neighbour j, the QC counts the occasions on which the same tag read at j is also read at k within 1 s (co-reads).

**Expected co-reads.** The expectation comes from the same reader pair on the other boards of the same recording and phase (homologous). Boards on which j or k read nothing are left out.

**The unit of evidence is the tag-minute** (a tag read at j in a minute), not the read. A resting tag produces long clusters of reads, so reads are not independent chances.

**What a reader is compared with.** A reader is judged against its neighbours' traffic, never against an assumed uniform occupancy.

- **Whole-session dropout (H06).**
  - **Condition:** no read in the session, while the board reads and the neighbours' traffic predicts ≥ 100 co-read tag-minutes.
  - **Confirmed** when both hold:
    - the antenna read in a placement scan next to the session (the initScan before it, or the next cage change's initScan);
    - its median best co-read ratio in its other recordings is ≥ 0.25 (healthy reader-sessions: 1st percentile 0.27).
  - **Probable** otherwise.
  - **Undecidable** below 100 expected tag-minutes. Such a reader is named in the manifest with status "undecidable", not "pass".
- **Best-neighbour co-read ratio per reader and 12-h block (W09).** A dead antenna is low for every neighbour; an animal resting next to it lowers only the adjacent pair. A ratio below 0.02 is warned in active blocks with ≥ 2 informative pairs; light blocks are only reported.
- **Silent-stretch scan (W08).**
  - Every stretch of ≥ 60 min without a read gets an expected number of active minutes with a co-read, from the tags present at the neighbours in each minute.
  - Only active minutes count, so silences during light-phase rest cannot trigger it.
  - **Calibration** (`stretch_calibration_summary.csv`, healthy readers, all minutes): the model is somewhat optimistic. Predicted co-read probabilities of 0.81 and 0.95 are observed at 0.72 and 0.88; at low predictions the overestimate is larger in relative terms.

## Relative reader co-detection (formerly "reader health")

The qc02 heatmap (and review panel QC01a) shows, per reader and session, observed / expected co-reads for the best informative neighbour pair: how often tags read at a neighbour are also read at this reader within 1 s, relative to the same reader pair on the other boards of the same recording. A value of 1 means "as on the other boards".

- **The normalization removes** reader-pair geometry, the traffic at the neighbour and conditions shared within a recording.
- **It does not remove behaviour or board.** The expectation comes from animals in other cages (one per batch is the CON-only cage), so occupancy, posture, shelter use, huddling and tag competition, and cage density all shift the score, as does board sensitivity (live-run medians 2.3, 1.4, 0.8, 0.8 and 2.0 for boards 1-5). Group-blind does not mean behaviour-independent.
- **Low scores are diagnostic, not failures.** Nine of the ten lowest non-zero scores in `raqc_20261014_25804cb` (0.13-0.27) are food or house readers on boards 3 and 4 with 150,000-340,000 reads each. A hardware failure is only a corroborated whole-session dropout (H06): the three grey cells with an x.
- **Not used for** exclusions, group comparisons or rescaling reads, occupancy or social measures. The QC rules are unchanged.

Legacy names are kept (`qc02_reader_health.png`, `healthy_elsewhere`, `AQC_HEALTHY_ELSEWHERE`). There, and in "healthy reader-sessions" or "healthy board-recordings", "healthy" means only "without a confirmed dropout".

## Rules, thresholds and their basis

No threshold was set from group effects, CombZ, prediction or significance. All of them are acquisition-only. Where no external basis exists, the basis is this run's acquisition distribution (stated), and the threshold is provisional: data-derived and not validated against known tag positions. Calibration will not be performed (decision of 2026-10-15), so these thresholds remain provisional and data-derived; no recalibration is pending (`docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.5.md` section 5). The calibration protocol (`docs/RFID_CALIBRATION_PROTOCOL.md`) is a historical record only.

| check | severity | threshold | basis and uncertainty |
|---|---|---|---|
| H01 antenna table on the documented grid | hard failure | exact | documented geometry; identical in all logs |
| H02 read records well formed | hard failure | 0 | read times are rebuilt from these fields |
| H03 duplicate events | hard failure | 0 | definition |
| H04 roster animals with tag, detection and the v2 board | hard failure | 0 | missing or misassigned data |
| H05 unusable timestamps (non-finite; backward jumps > 1,800 s after the DST fold is removed) | hard failure | 0 | the reader's fold rule uses 1,800 s; smaller late-delivery steps are W02 |
| H06 whole-session channel dropout | hard failure (reader) | ≥ 100 expected co-read tag-minutes; confirmed with scan reads and other-recording median ratio ≥ 0.25 | this run: the three dropouts have 2,417-4,727 expected tag-minutes; healthy reader-sessions have best ratios with minimum 0.127, 1st percentile 0.27, median 1.37 |
| W01 multi-read duration ≠ read span | warning | > 0 | format |
| W02 acquisition stall (within-event gap > 1.1 s or zero gaps) | warning | > 0 | events close after ~1 s without a read; read times near a stall are delivery times |
| W03 DST fold removed | warning | > 0 | the hour is unobserved |
| W04 recording-wide silence ≥ 60 s | warning | > 0 | a possible polling outage that the log cannot confirm |
| W05 unpolled pause between sessions inside the span | warning | > 0 s | explicit stop/start rows |
| W06 untracked tag (≥ 100 events) in a cage | warning | 100 events | an unseen animal shares readers |
| W07 board silent from its last read to the span end ≥ 1 h | warning | 3,600 s | a fault cannot be told from removal; "probable removal" when all tags stop within 10 min |
| W08 intermittent silence | warning | ≥ 200 expected active co-read minutes | this run: non-failure maximum 78, 99th percentile 25; the three dropouts on the same scale 1,047-2,063 |
| W09 low best-neighbour ratio, active block | warning | < 0.02 | this run: healthy active-block minimum 0.056 |
| W10 animal-block reads < 0.25 x cage-mate median | warning | 0.25 | compared within the cage only; a tag problem or a resting place out of range |
| W11-W13 placement-scan board, label vs tag map, reads off the home board | warning | any | identity and cage assignment |
| W14 poor detection support (top 1% of observed animal-blocks, share older than 300 s) | warning (review rank) | q0.99 per phase | a relative rank: always about 1% of units and dependent on the recordings in the run; does not set the run status |
| W15 reconstruction sensitivity (top 1% by event-start vs single-read disagreement) | warning (review rank) | q0.99 per phase | as W14 |
| W16 board acquisition signature against its own history | warning | \|z\| > 3 | boards differ by hardware; a change against the same board's history is unexplained |
| W17 analysis span not covered by the raw log | warning | > 60 s | an early stop or late start would otherwise shorten the span silently |
| D01-D06 resource asymmetries, board levels, support, switching, basis, co-assignment by reader | diagnostic | none | continuous; never flagged |

**Hard failures.** A hard failure is a demonstrable acquisition defect. A hard failure at reader level makes the board-recording unsuitable for spatial measures that need complete reader coverage:
- reader, row and column occupancy;
- dispersion and dominant share;
- co-assignment;
- reader changes (bypass jumps inflate ≥ 2-column and vendor displacement counts).

Non-spatial and detection-independent measures are not affected. The exclusion is only *proposed*.

**Warnings and diagnostics.** Warnings are review items. Where no external threshold exists, the QC reports continuous values (support-age classes, supported shares, co-read ratios) and sensitivity across carry-forward bounds rather than exclusions.

## Findings of the review run (`raqc_20261013_b1783bf`)

**Run.** All 24 recordings, code commit b1783bf, isolated root `<Behavior>/RANM/raw_acquisition_qc/runs/raqc_20261013_b1783bf/`. Review notes are in `RANM/raw_acquisition_qc/REVIEW_STATUS.md`. The first run (`raqc_20261013_9aa84d9`, rules v0.1) is kept for comparison.

**Status: FAIL.** 3 hard failures; 81 warning units in 11 checks, 64 of them the relative review ranks W14/W15.

### Confirmed technical failures (hard)

| recording | board | reader | expected co-read tag-minutes (co-reads) | evidence | animals | in frozen S13 |
|---|---|---|---|---|---|---|
| B2 CC3 | 2 | R3 | 4,727 (34,936) | 0 reads in the session; 309 reads in the placement scan before, last one 94 s before the session start; 760 reads in the next cage change's scan, first about 21 min after the session end; median best co-read ratio 1.78 in the 23 other recordings | OR106, OR112, OR131, OR141 | OR112, OR141 only |
| B2 CC4 | 5 | R3 | 3,981 (16,703) | 0 reads; 524 scan reads, last one 23 s before the session; no later scan (last cage change); median 1.97 elsewhere | OR106, OR113, OR135, OR138 | none |
| B5 CC4 | 1 | R5 (house) | 2,417 (19,748) | 0 reads; 1,790 scan reads, last one 41 s before the session; no later scan; median 3.53 elsewhere | 314, 318, OR620, OR630 | all four |

The scan timings were measured from the raw initScan files: from the antenna's last read in the scan to the session's start row.

**Nature of the dropouts.**
- Each antenna read normally seconds before its session and co-read normally in the other 23 recordings, so each failure is confined to its session.
- That it ended with the session is shown only for B2 CC3, which has a later scan; for the other two it is inferred.
- The cause is not documented. A controller or software channel state at session start is inferred.

**Other hard checks.** Antenna tables, read records, duplicates, timestamps and roster pass in 24/24 recordings.

**Proposed exclusion (not applied).** The three board-recordings, for spatial measures that need complete reader coverage. B2 CC4 board 5, and OR106 and OR131 in B2 CC3, are not covered by the frozen S13 list, which stays unchanged.

### Warnings (review items)

**Acquisition events, all outside the analysed data:**
- **DST fold, B1 CC1:** 8,097 rows dropped. The fold is on 2022-10-30, after that recording's Stage 29 windows (2022-10-28 18:30 to 2022-10-29 18:30).
- **Acquisition stall, B3 CC3, 2023-05-02 12:34:** one within-event gap of 3.1 s, after which buffered reads arrived with zero gaps (5 long gaps, 24 zero gaps).
- **B2 CC1:** 3 zero gaps and 2 multi-read events whose duration differs from their read span.
- Both of these lie before the analysed span (18:30).
- **Recording-wide silence, B6 CC2:** 72 s.

**Untracked tags in a cage (W06).** These animals share readers but are not analysed:
- B1 CC1 boards 2 and 5 (42,162 and 80,604 events);
- B4 CC1 board 1 (29,132);
- B6 CC1 board 5 (15,069).

**Probable removal.** B5 CC4 board 1 after 2023-12-04 14:00: all 4 tags stop within 85 s. The time is unobserved.

**Placement-scan board differs from the recording board (W11).** B5 CC3 animals 314 (scan board 4, recording board 2) and 317 (scan 2, recording 4).

**An animal read < 0.25 x its cage-mates (W10).** All in the light phase:
- B3 CC3 OR414, 2 blocks;
- B6 CC2 676 and 663, 1 block each.

Resting places out of range and tag problems cannot be told apart.

**Board signature against its own history (W16).** B5 CC4 board 1 (z −3.3 on the k1 lattice share). This is the dead-R5 recording, and the board's remaining antennas shifted cadence. B2 CC3 board 2, another dropout board, shows a smaller shift in the same direction (z −2.0, not flagged). B2 CC4 board 5 shows none.

**Review ranks:**
- **W14, poor support:** 32 observed animal-blocks. 28 of them are on the low-sensitivity boards 3 and 4, in both phases; the largest group is B6 CC2 board 4 (11).
- **W15, reconstruction sensitivity:** 32 blocks, 29 in B1.

**Not triggered:**
- **Intermittent silence (W08).** The largest non-dropout stretch reaches 78 expected active minutes: B2 CC3 board 5 R8 at the water, 00:37-02:27 during a dark-phase rest.
- **Low co-read ratio in an active block (W09).**
- **Span coverage (W17)**, unpolled pauses inside spans, label vs tag map, reads off the home board.

**Why W15 concentrates in B1.**
- B1 boards had fewer tags present: 2.9 per board, against about 4 elsewhere. Tracked tags were 2.8, including two boards with one tracked and one untracked animal.
- So animals were read more often: light-phase median 2,490 reads/h, against 1,290-1,630.
- More fresh reads at borders means more seconds where the two reconstructions differ. Event-vs-read disagreement correlates with reads/h at ρ 0.73 (active) and 0.67 (light), and B1's W15 blocks fall on boards with 1-3 tracked animals.
- B1's acquisition signature does not differ from the other batches (mean within-event gap 392 ms, k1 0.243).
- The read-density mechanism is inferred, but consistent with the tables.

### Relative reader co-detection and boards

**Healthy reader-sessions** (957): best co-read ratio minimum 0.127, 1st percentile 0.27, median 1.37. The lowest are R1 (food) on boards 4 and 3.

**Boards 3 and 4 are less sensitive**, most clearly in crosstalk:

| board | incoming crosstalk vs homologous (median) | reads vs homologous | silent share of span (median) |
|---|---|---|---|
| 3 | 0.52 | 0.87 | 8.7% |
| 4 | 0.45 | 0.83 | 11.0% |
| 1, 2, 5 (range) | 0.91-1.39 | 1.00-1.19 | 3.4-5.2% |

How consistent each measure is across the 24 recordings:
- **Incoming crosstalk:** boards 3 and 4 are the two lowest in 24/24 recordings.
- **Reads:** both are below all of boards 1, 2 and 5 in 16/24.
- **Silent share:** both are above all of boards 1, 2 and 5 in 12/24.

Board 5 is the fastest board: mean within-event gap 367 ms vs 396-407 ms, and lattice k1 0.31 vs 0.22-0.23. These cadence and crosstalk signatures are stable from B1 to B6, so they are hardware properties. Board allocation is a recorded covariate.

**Low light-phase support also depends on the cage change.** The median share of time supported within 60 s rises from 0.47 (CC1) to 0.56-0.60 (CC2-CC4); the active phase rises from 0.76 to 0.81-0.85. CC1 is the lowest on every board.

The five least-supported light-phase board-recordings after B5 CC4 board 1 all combine board 3 or 4 with CC1: B4 CC1 b3, B2 CC1 b4, B6 CC1 b4, B4 CC1 b4 and B5 CC1 b4. Lower antenna sensitivity and the first-night cage-change epoch add up there. The CC1 effect is not hardware; its cause is inferred, not measured.

### Detection support and carry-forward, by reader

Share of carry-forward time with a read at most 60 s (10 s) old:

| reader | active | light |
|---|---|---|
| R5 house\* | 0.73 (0.52) | 0.50 (0.31) |
| R6 | 0.78 (0.59) | 0.56 (0.38) |
| R1 food\* | 0.83 (0.60) | 0.62 (0.44) |
| R2 | 0.85 (0.66) | 0.69 (0.49) |
| R3, R4, R7 | 0.91-0.94 (0.74-0.81) | 0.77-0.87 (0.56-0.74) |
| R8 water\* | 0.95 (0.83) | 0.87 (0.74) |

Reads per carry-forward second (active / light):

| reader | active | light |
|---|---|---|
| R5 | 0.92 | 0.46 |
| R1 | 0.85 | 0.53 |
| R2 | 0.55 | 0.47 |
| R6 | 0.62 | 0.44 |
| R8 | 1.44 | 1.47 |

**What it means.**
- The house end carries the least recently supported time. That is expected where animals rest and huddle (shelter, read competition, tags out of range), and it is not a failure.
- The house end is also where carry-forward matters most:
  - column C1 holds 0.62 of active and 0.69 of light-phase carry-forward time, but only 0.58 and 0.64 of the time supported within 10 s;
  - C4, the water end, gains under tighter bounds (active 0.098 unbounded, 0.133 within 10 s).
- **Per board-recording supported share (≤ 60 s):** the median is 0.82 in the active and 0.55 in the light phase.
  - The minima are 0.39 and 0.17, at B5 CC4 board 1, driven by the unobserved time after the removal.
  - Without that board, the minima are 0.61 (active, B6 CC2 b4) and 0.32 (light, B4 CC1 b3).

### Event-start vs single-read reconstruction

**Overall.** With the 2-s flip filter on both, single-read carry-forward assigns a different reader in 8.5% of active and 4.8% of light seconds. In 6.2% and 3.2% of seconds the column differs.

**Where the differences are:**
- **Fresh reads, i.e. overlapping events at borders.** 15.8% (active) and 14.3% (light) of seconds with a read ≤ 2 s old are reassigned, against 1-4% beyond that.
- **Mostly away from the corridor readers.** Seconds reassigned away, active phase:
  - corridors: R2 24%, R7 26%, R6 18%, R3 16%, R4 15%;
  - R5 (house) 2.6% (light 1.8%) and R1 3.7% (light 6.7%).
- **Net flow toward the end readers.** The largest flows:
  - active: R6→R5 (29% of reassigned seconds), R2→R1 (14%), R1→R5 (11%);
  - light: R6→R5 (28%), R1→R5 (19%), R5→R6 (16%), R2→R1 (7%).
- **House occupancy is basis-sensitive.** Few R5 seconds are reassigned away, but R5 gains border time:
  - +7.7% (active) and +2.6% (light);
  - its occupancy share rises from 0.336 to 0.362 and from 0.531 to 0.545;
  - this is the largest absolute change of any reader.
- **Other readers.** R8 gains 9.3% and 12.8% in relative terms; the corridor readers R2, R6 and R7 lose 11-18% (active).
- **Recordings.** B1 holds 7 of the 10 board-recordings with the most reassignment. B4 CC4 board 1 is high in both phases, board 2 in the light phase.

**Transitions per hour** (active / light, animal-block medians):

| basis | flip filter | active | light |
|---|---|---|---|
| raw events | none | 179 | 58 |
| events | 2 s | 127 | 37 |
| single reads | none | 159 | 51 |
| single reads | 2 s | 103 | 27 |

**Status of the read-level basis.** It is the better-founded representation: one read, one time and one antenna, with no event-closing artefact. Its disagreement with the event basis is not a measured error, because neither is validated without calibration. It is not adopted. The live Stage 19 outputs and the data versions keep the event basis until a versioned read-level reconstruction has been reviewed. *(Status 2026-10-15: the versioned read-level reconstruction `srs_v1.1_9b18624` and the Stage 19 candidate `s19c_v0.2_defd263` were built. At the closeout neither basis was adopted: both are retained as sensitivity analyses, the candidate is unregistered and inactive, and the live Stage 19 outputs and the data versions keep the event basis; `docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.5.md` section 3.)*

### Co-assignment by reader (diagnostic only)

Same-reader co-assignment against its independence expectation, with the share of the net deficit (expected minus observed pair-seconds):

| reader | active: obs/exp (supported ≤ 60 s) | active: share of net deficit | light: obs/exp (supported) | light: share of net deficit |
|---|---|---|---|---|
| R1 food\* | 0.93 (0.79) | 72% | 0.78 (0.64) | 19% |
| R6 | 0.82 (0.67) | 54% | 0.76 (0.56) | 45% |
| R5 house\* | 0.99 (0.82) | 12% | 0.96 (0.85) | 37% |
| R2 | 0.99 (0.87) | 1% | 0.71 (0.57) | 4% |

R3, R4, R7 and R8 lie above 1. Their expectations are small, and they offset the net deficit (−39% active).

**Where the deficit comes from:**
- **Active phase:** from R1 and R6.
- **Light phase:** shared by R6, R5 and R1.
- **R5:** its ratio is near 1 only because its expectation is large. In supported time it carries 38% (active) and 41% (light) of the deficit.

**Interpretation.** This fits co-located animals at the house end being split between neighbouring antennas by crosstalk and read competition. Co-located animals are also read less, which removes co-located time from the supported-only values preferentially.

**What it is not.** It is not evidence of avoidance. The expectation is built from inferred occupancy, so it is not independent evidence about separation either way.

### Which limitations are correctable from the available data

*Assessment of 2026-10-13. The entries that need calibration are accepted limitations since 2026-10-15 (note below the table); they are not pending work.*

| limitation | status |
|---|---|
| whole-session channel dropouts (3) | identified and documented; the board-recordings can be excluded from complete-coverage spatial measures; not recoverable |
| event-start mis-timing of overlapping events | addressable by a single-read reconstruction (the bases disagree in 8.5% / 4.8% of seconds, and house occupancy shifts by up to 2.6 points); not validated without calibration |
| board sensitivity differences (boards 3, 4) | identifiable and stable; usable as a covariate or stratum; not convertible to absolute detection without calibration |
| carry-forward uncertainty | quantified per animal, reader and column (support-age classes, bounds); can be reported and bounded, not removed |
| removal tail, DST hour, stall, untracked tags | identified and bounded (unobserved time, cage density) |
| location while unread (light-phase support ≤ 60 s only 0.55) | not correctable from RFID |
| read competition vs shielding vs absence at the house | not separable without calibration (`docs/RFID_CALIBRATION_PROTOCOL.md`, steps 2-3) |
| row within a column (R1 vs R5, R4 vs R8) | not resolvable (crosstalk); the column is the defensible resolution |
| short A→B→A visits: crosstalk or movement | not resolvable without calibration (step 4) |
| co-assignment as physical proximity | not correctable; antenna co-assignment only |
| resource layout and orientation per cage | house end supported; food and water rows inferred; needs a photo record or calibration |

A probabilistic location model would only help after calibration; it was not built (pilot: `docs/ANTENNA_MEASUREMENT_VALIDATION.md`).

Calibration will not be performed (decision of 2026-10-15): the limitations above that need calibration stay accepted
limitations (`docs/SPATIAL_MEASUREMENT_SPECIFICATION_v1.5.md` section 5).

### Decisions for Tobias (all resolved; the questions are kept as asked on 2026-10-13)

1. **Dropout exclusions.** Should the three confirmed dropouts (B2 CC3 board 2, B2 CC4 board 5, B5 CC4 board 1) become exclusions in new spatial analyses? And should S13's partial coverage of B2 CC3 board 2 be noted in the limitations? The frozen S13 list cannot change.
   *Resolved:* adopted as SX01-SX04 in the spatial exclusion manifest (v1.0, 2026-10-14; current v1.1 adds the movement records SX05-SX07); S13 unchanged; its partial coverage is noted in `docs/KNOWN_LIMITATIONS.md` section 14.
2. **Single-read reconstruction.** Should a versioned single-read reconstruction (data version) be built for future spatial summaries?
   *Resolved:* built (srs_v1.0, then srs_v1.1_9b18624) as a provisional, isolated reconstruction; not activated in any registered stage.
3. **Board covariate.** Should board be recorded as a covariate in future read-based measures, together with the cage change?
   *Resolved:* board and presence metadata are recorded on the revised tables; board is a design covariate or stratum without a hardware claim (specification v1.4 section 6, construct S7).
4. **Calibration.** Should the calibration protocol be commissioned?
   *Resolved:* a one-day pilot was prepared and pre-registered (`docs/RFID_CALIBRATION_PILOT.md`), then declined on 2026-10-15: calibration will not be performed.
5. **Live QC root.** Should Stage 00b run into the live QC root (`analysis_ready/quality_control/raw_acquisition/`) after review?
   *Resolved:* yes, on 2026-10-14 (runs `raqc_20261014_6165adf` and `raqc_20261014_25804cb`).

## Changes from v0.1 (adversarial review, 2026-10-13)

- **Silent-reader tests.** They used reads as independent chances. Clustered reads drove the co-read probability of a busy-neighbour minute to about 1, which made W08 a duration test. Now:
  - tag-minutes are the unit of evidence;
  - H06 is testable at ≥ 100 expected tag-minutes;
  - a calibration table is written.
- **W08 light-phase exemption.** It could wrongly exempt a silence that starts at light onset and lasts through dark phases. It is removed; only active minutes count.
- **"Confirmed" dropouts.** v0.1 required the minimum of the antenna's other recordings to be ≥ 0.13, which lies inside the healthy distribution. v0.2 uses the median ≥ 0.25.
- **Undecidable status.** Untestable silent readers now give the manifest status "undecidable" instead of "pass".
- **W14/W15.** They pooled unobserved time (after the B5 CC4 removal), which displaced observed blocks. They now rank observed blocks only and do not set the run status.
- **W17 (new).** A span-coverage warning. v0.1 computed coverage but never evaluated it.
- **Bypass pairs.** They are kept for middle-column readers only.
- **Seed age.** It comes from the last read before the span.
- **Provenance.** Placement scans are hashed, and inputs are part of the resume marker.
- **Summaries.** They leave dropout boards out consistently.
- **Figures.** qc02 shows the best-neighbour ratio; qc08 shows the dropouts on the same scale.
- **Path guard.** It handles trailing dots and spaces and the configured raw-log root.
- **Path budget.** It covers the longest file names actually written.
