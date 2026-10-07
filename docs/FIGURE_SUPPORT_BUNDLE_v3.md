# Figure-support bundle v3 (source data for the Stage 14 domain-heatmap and HMM figure candidates)

## Purpose

Exp9_manuscript draws its own figures from frozen bundles and computes nothing statistical. To draw the Stage 14
behavioural domain heatmap and the HMM figures as manuscript candidates (Tobias, 2026-10-07: the all-blocks heatmap in
its three resolution variants, colours only, and candidates of the four HMM figures), their display data reach the
manuscript through this bundle.

v3 contains every v2 table unchanged and byte-identical (F1, F1b, F2, F2b, P1, P1b, P1c). It adds D1 and M1-M4.

- **Writer.** `Analysis/16c_figure_support_bundle.R` with `Functions/figure_support_bundle.R`.
- **Test.** `Testing/tests/test_figure_support_bundle.R` (section 8b), synthetic data only.
- **v1 and v2 are unchanged.** `fsb_v1_20260930_f06552c` and `fsb_v2_20260930_a23d699` stay FROZEN and untouched.
- **Bundle id.** `fsb_v3_<YYYYMMDD>_<commit7>`, in `analysis_ready/canonical/figure_support_bundle/` with its own row in
  `BUNDLE_REGISTRY.csv` (same columns).

## The Stage 14 source (not re-run)

Stage 14 is not run for the bundle. The tables are those of the Stage 14 run of 36f93b2 promoted on 2026-10-06, whose
promotion record `docs/behavior_output_producer_reruns/stage14_20261006c.csv` lists every file with its sha256. The
bundle's tile outlines and colour limits are the manuscript's own, so the code-only outline change of bc116f8 needs no
Stage 14 run here.

| Role | File (under `analysis_ready/analyses/`) | sha256 |
|---|---|---|
| domain heatmap | `systems_dashboard/5min/stats_tables/systems_sis_domain_heatmap_source_data.csv` | `fa4c6132...` |
| transition means | `systems_dashboard/5min/tables/systems_hmm_transition_probability_summary.csv` | `0279d418...` |
| transition differences | `systems_dashboard/5min/tables/systems_hmm_transition_probability_differences.csv` | `222208fb...` |
| resolution sensitivity | `systems_dashboard/5min/stats_tables/systems_sis_hmm_resolution_sensitivity.csv` | `d3a79a1a...` |
| state display labels | `systems_dashboard/5min/tables/systems_hmm_state_display_labels.csv` | `60ec75e6...` |
| dwell times (Stage 08) | `hmm_states/10min/tables/hmm_state_dwell_times.csv` | `be4ba0cf...` |

**Gates in 16c before the copy.** Each Stage 14 file's on-disk sha256 must equal the pinned value and the promotion
record must list the file with that sha256; the Stage 08 dwell-time table must equal its pinned sha256. The record file
is one of the code files that must be committed and clean.

## Added tables

| Table | Content | Rows |
|---|---|---|
| `D1_domain_heatmap_cells` | verbatim copy of the domain-heatmap source table (every cell as text, `source_run`, `source_file`, `source_sha256` appended) | 504 = all_blocks 84, cc1_a1 42, cc1_l1 42 per variant x 3 variants |
| `M1_hmm_transition_difference` | the stored group mean transition probabilities and their differences, with state display labels; `colour_limit` = max abs difference | 96 = 2 sexes x 4 x 4 states x 3 contrasts |
| `M2_hmm_transition_flows` | the stored group mean transition probabilities, with display labels, `rank_within_sex_group` and `shown` (the 8 largest per sex x group, as Stage 14 draws them) | 96 = 2 sexes x 3 groups x 16 |
| `M3_hmm_dwell_distribution` | counts of animal x cage-change x phase x state records of the mean dwell time per bin doubling from one 10-minute HMM bin (10-20, 20-40, ..., 640-1280 min), with fractions and animal counts | 336 = 2 sexes x 2 phases x 3 groups x 4 states x 7 bins (3,346 records) |
| `M4_hmm_resolution_sensitivity` | the stored HMM-resolution sensitivity rows, with `wald_ci_low` / `wald_ci_high` = estimate -/+ 1.96 SE (the interval Stage 14 draws) | 24 = 2 resolutions x 2 sexes x 2 phases x 3 contrasts |

- **What is computed.** Nothing is fitted, tested, adjusted or smoothed. M1 and M2 join display labels to stored values
  by the stored semantic state label; M2 ranks; M4 applies the Wald rule to stored estimates and SEs; M3 counts. Stage 14
  drew the dwell times as kernel density ridges: M3 replaces the smoothing by fixed-bin counts.
- **Gates before the write.** D1 row counts per heatmap x variant (468 estimated, 36 not shown) and display text without
  banned wording; four HMM states with unique labels; M1 differences = comparison mean - reference mean within 1e-12; M2
  16 transitions and 8 shown per sex x group; M3 counts summing to the 3,346 records and fractions to 1; M4 24 rows of
  the state-architecture domain only.
- **Gate after the write, before the registry row.** Every line of D1 must be the source line followed by the appended
  provenance cells, as for P1-P1c.
- **Display labels.** The HMM tables carry the display labels of the 10-minute HMM (Many position changes, No position
  change co-located, Few position changes, No position change apart); the stored semantic labels are kept as
  `source_from_label` / `source_to_label`.
- **Claims.** D1 carries the Stage 14 post hoc tests and markers as stored; the manuscript candidates draw colours only.
  The HMM tables are display data: inactive-phase HMM interpretation and the Stage 14 systems layer may not carry a
  manuscript claim (`manuscript/README.md`, `docs/KNOWN_LIMITATIONS.md` items 1-3).

## Real run

- Commit a2acd57; bundle `fsb_v3_20261007_a2acd57`, FROZEN; `00_manifest.csv` sha256
  `bbf2de27282be4a8448dea020b6872e698e2cbf53a1811432118f5b58c6a098c`; 15 files, 1,180,095 bytes.
- 93 of 93 pre-write gates; 102 of 102 gates in `logs/fsb_v3_20261007_a2acd57_gate_results.csv` with the post-write
  and registration gates.
- The dry run (same commit) passed the same gates.

## Provenance

`H_provenance.csv` adds `stage14_source_dir`, `stage14_promotion_record`, `stage14_tables`, `hmm_dwell_source` and
`hmm_resolution`. `scientific_recomputation` keeps the v1 opening words and adds that D1 is a verbatim copy and M1-M4 the
stored Stage 14 HMM figure tables with labels, a rank, the Wald interval and fixed-bin counts.
