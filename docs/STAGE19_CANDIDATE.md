# Stage 19 candidate s19c_v0.2 (revised spatial occupancy, for review)

Written for: Tobias and reviewers.

## Purpose and status

This candidate implements decision 4 of 2026-10-14: a **separate, versioned** revision of Stage 19 built from the pinned single-read spatial reconstruction release.

**Fifth round (2026-10-15).** The candidate stays isolated until the results of the calibration pilot have been evaluated; manuscript figures are not replaced.

**v0.2 (decision of 2026-10-15).** The existing Stage 19 block set is the primary window, for a like-for-like comparison of
the original and the revised occupancy reconstruction. The CC4 blocks after it (I3 onward: the grid-exposure period) are
reported separately as `cc4_grid_period` and are never pooled with the primary window, so no row mixes the intervention
period with the pre-grid phases; the whole-span pooling of v0.1 (`whole_span`) is gone from the outputs and is computed only
in memory for the gates that check against the srs release. Everything else is v0.1.

- **Status:** candidate for review. It is UNREGISTERED, group-blind and isolated.
- **Untouched:** Stage 19 (`Analysis/19_spatial_occupancy_maps.R`), its outputs under `analysis_ready/analyses/spatial_occupancy` and `publication_ready/.../spatial_occupancy`, the manuscript figures, S13 and every registered result. Nothing is replaced, and Stage 29 is not rerun.
- **No new preprocessing:** no raw log is read and nothing is reconstructed again. Every number is a seconds-weighted aggregation of the srs_v1.1 tables.
- **Excluded from the build:** no group label, CombZ or outcome is read, and no model, p-value or effect size is computed.
- **Reconstruction labels:** the single-read reconstruction is the provisional candidate and the event-start reconstruction is the historical reference. Their estimates are sensitivity analyses, **not bounds on true physical occupancy**. The final choice of reconstruction rests on independent measurement validity (calibration criterion a), never on group effects, prediction or significance.

**Code:**
- builder: `Testing/audits/build_spatial_occupancy_candidate.R`
- definitions: `Functions/spatial_occupancy_candidate.R`
- tests: `Testing/tests/test_spatial_occupancy_candidate.R`

**Run** from the repository root (one light process, about 1 min):

```
Rscript Testing/audits/build_spatial_occupancy_candidate.R [--run-id=<new id>] [--output-root=<absolute>]
```

- **Default output:** `<Behavior>/RANM/spatial_occupancy_candidate/runs/s19c_v0.2_<commit7>/`, built in `runs/.tmp_<id>/` and renamed at the end. An existing run or build folder is refused.
- **Run ids:** a release id needs committed, clean code; ids starting with `dev` may run on uncommitted code.
- **Protection:** the protected locations are snapshotted before the first write and compared before the rename (`Functions/protected_outputs.R`).
- **Gates:** the run publishes only if every gate passes. A failed run leaves its `.tmp_` folder for inspection.

## Inputs and pins

Each input is pinned by sha256 in the builder, and each pin is cross-checked against what its producer lists. The run stops on any mismatch.

| input | pin | listed in |
|---|---|---|
| srs release `RANM/single_read_spatial/runs/srs_v1.1_9b18624`, tables occupancy, animal_block, animals, blocks, censored, read_agreement, gaps, colocation, shares_unknown_column_pooled, shares_unknown_reader_pooled, support_by_phase, parity_stage19 | 12 sha256 constants | the release's `manifest/output_files_sha256.csv` (itself pinned, 81739ec8...) |
| srs `manifest/run.csv` (srs_v1.1, code clean, tau 2 s, bounds 2/10/60/300, manifest v1.1 with content sha 3ecd65bc...) | 2ad222a0... | output manifest; content checked by gate G02 |
| cage-episode presence `RANM/cage_episode_presence/runs/cep_v1_9b18624/tables/cage_episode_presence.csv` (only n_animals_present, n_tags_present, n_tracked are read; never fixed_cage) | 3613f86e... | the cep output manifest (pinned) |
| Stage 19 `tables/all_position_occupancy_by_phase.csv`, read only, keys and seconds only (never Group or Sex) | 094bce9b... | srs `input_hashes.csv` and the Stage 19 rerun record |
| Stage 19 `audit/raw_antenna_occupancy_audit.csv`, read only, for raw_log, raw_logs_ignored, clock_fold_events_dropped | be29e97b... | `docs/behavior_output_producer_reruns/stage19_20261012.csv` |
| spatial exclusion manifest v1.1 (SX01-SX07) via `sxm_read("v1.1")` | content sha 3ecd65bc... | `SXM_RELEASED` |

## Representation and denominators

**Units and bases**
- **Units:** four columns are primary: C1 = R1 food + R5 house, C2 = R2 + R6, C3 = R3 + R7, C4 = R4 + R8 water (`amv_column()`). The eight readers are diagnostics. R1 food, R5 house and R8 water are lab-reported resource locations. Column or reader occupancy is **not** resource engagement, proximity or a validated location.
- **Bases:** `event` = event-start (historical reference); `read` = single-read (provisional candidate). Both have tau 2 s and the same span, blocks, seeds, masks and carry-forward definitions.

**Bounds and support**
- `LOCF` (`none (unrestricted LOCF)`) is the historical reference. Under LOCF, unknown time is only the unseeded time before an animal's first read, which is 0 here because all 444 animal-recordings are seeded.
- `max300s` and `max60s` (`observation age <= 300 s`, `<= 60 s`) are sensitivity analyses. A second is **supported** when the animal's latest read is at most d s old. This means "recently detected", not "validated".
- Unsupported time is the category **unknown** and is never redistributed. The 60-s and 300-s bounds are reporting conventions, not validated thresholds.

**Denominators** (both always reported)
- `share_all` = seconds / observed time. The units plus unknown sum to 1.
- `share_supported` = seconds / supported time. The units sum to 1. It is NA when no time is supported, and always NA for unknown.
- Observed time = block time before the censoring time. When observed time is 0, every share is NA, not 0.
- Summary tables use explicit names: `share_of_observed_time`, `share_of_supported_time`, `unknown_share_of_observed_time`.

**Phases and block sets**
- **Phases:** `Phase` = Active | Light, with `Phase_vendor` = Active | Inactive kept. Active and light are never pooled. `phase_index_in_class` gives the k-th active or light phase (vendor light numbering starts at 2).
- **`stage19_window` (primary):** Stage 19's blocks, so CC4 keeps A1, I2 and A2 only.
- **`cc4_grid_period` (separate):** the CC4 blocks after Stage 19's window (I3, A3, I4, A4, I5: the grid-exposure period), descriptive only and never pooled with the primary window. Acquisition quality there: B2|sys.5|CC4 is excluded (R3 silent), B5|sys.1|CC4 is censored from 2023-12-04 14:00:42 (during I3).
- **Per-block rows:** the two sets are disjoint, so every block belongs to exactly one; per-block and pooled rows are never mixed in one table (`*_block` and `*_window` tables).
- **Aggregated rows:** `block_set` names the set the row was pooled over; both sets are computed, each on its own.

**Pooling.** Pooling is always by seconds; shares are never averaged. An equal-weight animal mean is shown beside the pooled share only in `cage_episode_block_column` and `cage_episode_window_column`.

**Exclusions and censoring** (manifest v1.1)
- SX01-SX03 (B2|sys.2|CC3, B2|sys.5|CC4, B5|sys.1|CC4): these episodes stay in the animal tables with `spatial_excluded = TRUE` and are left out of every summary, figure and pooled gate.
- SX04: observed time ends at `censor_from` (2023-12-04 14:00:42.962). The censored 755,828 s all lie outside stage19_window.
- SX05-SX07 (movement) have no effect here: the candidate has no movement outcome.
- `exclusions_censoring.csv` lists every record with its animal-blocks and seconds.

**Metadata on every table**
- **Run and method:** run_id, srs_run_id, reconstruction (srs_v1.1), tau_s, basis, basis_label, bound, support_criterion, denominator where a table has one share column, and block_set.
- **Animal tables add:** SourceFile, raw_log, Batch, Sex (B1/B2/B5 male, B3/B4/B6 female), CC, board, System, CageEpisodeID, AnimalNum, tag, n_animals_present, n_tags_present, n_tracked, spatial_excluded and censor_from.
- **Episode tables:** carry the episode keys.
- **Pooled tables:** carry `level` and the counts n_cage_episodes, n_animals (distinct animals, 111 when all are kept) and n_animal_recordings (animal x cage-change recordings, 432; an animal counts once per CC) instead of per-animal keys. "Spread over animals" in unsupported_time_summary is a spread over animal-recordings.
- **Per-episode rows of pooled tables** (e.g. the cage-episode rows of basis_comparison_column) carry the episode keys SourceFile, raw_log, Batch, Sex, System and the presence counts.

## Tables (`tables/`; grain and contents also in `tables/table_guide.csv`)

| file | grain | contents |
|---|---|---|
| occupancy_animal_phase_column.csv.gz (primary) | animal-recording x phase block x basis x bound x C1-C4 + unknown (96,570 rows, zero-completed) | seconds, share_all, share_supported, observed_s, censored_s, supported_s |
| occupancy_animal_phase_reader.csv.gz | the same at R1-R8 + unknown | reader diagnostics with resource labels |
| occupancy_animal_window_column / _reader.csv.gz | animal-recording x window (phase_class = all phases of a class in the set; cc1_active1 = Stage 19 primary window) x block_set | window shares pooled by seconds |
| support_animal_phase.csv | animal-recording x phase block | supported, unknown and observed time; reads and reconstruction changes per observed hour (descriptive, not locomotion); agreement at reads (timestamp consistency, not accuracy); seeds, ties, raw-log QC |
| unknown_by_last_reader_gap.csv.gz | animal-recording x phase block x bound x reader last assigned x gap class | unknown seconds on the 1-s grid |
| unknown_cagemate_context.csv.gz | pooled and cage episode x reader x state | cage-mate context of unknown time: missingness context, not proximity or affiliation |
| cage_episode_block_column.csv.gz | cage episode x phase block (block_set = the narrowest set holding the block) | seconds-pooled share and equal-weight animal mean |
| cage_episode_window_column.csv.gz | cage episode x phase class x block_set (the set pooled over) | as the block table |
| organisation_animal_block.csv.gz | animal-recording x phase block x basis x bound | S4: column entropy (bits) and dominant-column share of supported time, unknown share beside it |
| organisation_animal_window.csv.gz | animal-recording x phase class x block_set x basis x bound | as the block table |
| basis_bound_sensitivity.csv (+ _by_episode.csv.gz) | comparison x resolution x unit x denominator | read - event and max300s / max60s - LOCF: pooled values and differences, spread over cage episodes, per-episode values |
| basis_comparison_column.csv | pooled, cage-episode spread, by board, by CC, per cage episode | read minus event at column resolution, plus R5 (house) reader rows as a diagnostic |
| unsupported_time_summary.csv | pooled, by CC, by board, by CC x board | unknown and supported share of observed time, spread over cage episodes and animals; light phase included |
| column_shares_by_bound.csv | pooled and by CC x basis x bound x unit | LOCF vs 300 s vs 60 s column shares under both denominators, change against LOCF |
| observation_gap_summary.csv | total, gap class, last reader, last reader x gap class | unknown time by gap class and reader last assigned; house association (unknown share of the reader's LOCF time); grid against exact |
| stage19_comparison_animal_window.csv.gz | animal-recording x window x phase x unit (Stage 19 block set) | Stage 19 share against the candidate's event LOCF (identical), read LOCF and bounded shares, with differences |
| exclusions_censoring.csv | manifest record x cage episode | affected animal-blocks and seconds, and what the record does here |
| gates.csv | gate | pass, value, criterion |

## Figures (`figures/`, group-blind QC, PNG)

- **F1**: column shares with unknown, as a share of ALL observed time (single-read basis; LOCF / 300 s / 60 s), by phase and CC.
- **F2**: event-start against single-read column shares per cage episode (LOCF share of observed time; 60 s share of supported time).
- **F3**: reader cage maps per CC x phase (single-read, 60 s, share of supported time), with R1 food, R5 house and R8 water labels and the unknown share under each cage.
- **F4**: support at 60 s per animal and phase block, by board and CC (stage19_window).
- **F5**: reader profiles of the original Stage 19 (= event LOCF), read LOCF and read 60 s (share of supported time), with the same animals in every series and no group panels.
- **F6**: unknown time by the reader last assigned and the read-gap length.

## Gates (the run publishes only if all pass)

- G01: input sha256 against the pins and the producers' manifests.
- G02: content of the srs run record.
- G03: exclusion and censoring flags of srs equal manifest v1.1 per animal-recording.
- G04: row counts (444 animal-recordings, 432 kept, 3,219 animal-blocks, 3,203 observed).
- G05: LOCF seconds equal observed time per animal-block in both bases.
- G06: supported time equal across bases.
- G07, G08: share_all and share_supported sum to 1, and the NA rules hold.
- G09: column seconds equal reader seconds.
- G10, G10b, G11: event-basis LOCF on stage19_window reproduces Stage 19's phase table (21,312 cells, max 5.5e-11 s), its shares and its observation seconds.
- G12, G12b, G13: whole-span pooling, computed in memory only, reproduces srs `shares_unknown_column_pooled.csv`, `shares_unknown_reader_pooled.csv` and `support_by_phase.csv` (max difference about 6e-16).
- G14: excluded episode ids are absent from the summary tables and figure inputs that carry CageEpisodeID.
- G14b: the observed time of every pooled summary (column shares by bound, unsupported time, observation gaps) and of the F1/F3/F5 inputs equals the observed time of the kept animal-blocks, by block set x phase (x CC, board), so no excluded episode contributes to a table that does not name episodes. A mutation test shows it fails when an excluded episode leaks in.
- G15: no group column in any output.
- G16-G21: layout, presence join, raw logs, censoring arithmetic, no negative unknown, phase labels.
- Diagnostics: LOCF unknown time by basis; the 1-s grid against the exact unknown time (max 13.9 s per animal-block).

**Release runs: `s19c_v0.1_4aebc90` (2026-10-14) and `s19c_v0.2_defd263` (2026-10-15; see specification v1.3): 25 of 25 gates each.** The stage19_window numbers below are identical in v0.1 and v0.2. On stage19_window, single-read (event-start), Active phase:

- LOCF: C1 66.0% (63.1%).
- 60-s cap: C1 51.4% of observed time, unknown 19.8%; C1 64.2% (60.7%) of supported time.

Light phase:

- LOCF: C1 72.5% (71.5%).
- 60-s cap: unknown 46.8%; C1 70.9% (69.2%) of supported time.

## Not derivable from the srs release (left out of v0.1)

These need a new reconstruction version from the raw logs (heavy; ask before running):
- 30-min bins or any sub-phase window with bounded support, for either basis. `asr_process_recording()` has no cut argument, and srs stores no read times.
- Flip-filter sensitivity at a tau other than 2 s.
- The vendor AnimalPos source comparison. Use Stage 19's existing audit tables instead.
- The missingness grid (gaps, cage-mate context) at bounds other than 60 s and 300 s.
- Positions of the six untagged B1 animals.

## Decisions still open

1. **Primary block set: decided** (2026-10-15): the existing Stage 19 window; the CC4 grid period separately, never pooled with it.
2. **Group-descriptive layer:** gated. Side-by-side group means (specification section 8 item 5) would read the group lists behind an explicit flag; no models. Not built until Tobias approves.
3. **Basis choice:** after the calibration pilot, by criterion (a) only. If the pilot is inconclusive, both bases are kept as sensitivity analyses. Bounds (60 s / 300 s) and tau stay reporting conventions until calibrated.
4. **Manuscript figures:** whether revised figures replace the Stage 19 manuscript figures is decided only after these reviews (specification section 8 item 7).
