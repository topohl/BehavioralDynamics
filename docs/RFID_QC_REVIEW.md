# RFID QC: publication-style review figures and eight-reader spatial overlap

Status: unregistered, isolated review output (`aqr_v0.1`). Requested on 2026-10-15 after the documentation closeout.
This is a new presentation and descriptive analysis layer; it does not activate the spatial candidate, replace any
registered analysis, refit a biological model or change a manuscript import. Calibration remains declined.

## What already exists

| Family | Source under the Behavior folder | Existing material |
|---|---|---|
| Acquisition QC | `RFID/analysis_ready/quality_control/raw_acquisition/runs/raqc_20261014_25804cb` | 8 diagnostic PNGs; status, rules, flags, support, relative reader co-detection, provenance and ledger |
| Measurement validation | `RANM/measurement_validation` | 10 PNGs; event/read reconstruction, detection support, filtering, co-assignment references and board context |
| Single-read reconstruction | `RANM/single_read_spatial/runs/srs_v1.1_9b18624` | 6 PNGs; both bases, observation age, explicit unknown time and reader-knockout effects |
| Spatial candidate | `RANM/spatial_occupancy_candidate/runs/s19c_v0.2_defd263` | 6 PNGs; primary/grid periods separately, four-column shares and eight-reader diagnostics |
| Board adjustment | `RANM/board_adjustment/runs/badj_v1_9b18624` | Saved tables; no figures folder in this run |
| CC4 QC model comparison | `RANM/cc4_qc_comparison/runs/cqc_v1.0_defd263` | Comparison tables; no figures folder |

The builder writes `tables/existing_figure_inventory.csv` with exact existing image paths. Older superseded runs are not
combined with these current source runs.

## What is improved

Three cohesive four-panel review figures replace a long sequence of standalone diagnostic screenshots for review:

- **QC01 acquisition:** relative reader co-detection heatmap, per-board QC status, acquisition cadence and detection support by reader.
- **QC02 uncertainty:** explicit unknown-time composition, basis sensitivity, board-specific missingness and read-gap context.
- **QC03 eight-reader overlap:** observed overlap versus time-shift reference, excess by cage change, reference sensitivity
  and a deterministically chosen example dyad matrix.

Each is 183 mm wide, with consistent Arial typography, panel letters, meaningful phase/basis encodings, existing manuscript
palette aliases, vector PDF/SVG and PNG previews. All quantitative panels have exported source CSVs. Legends distinguish
medians/IQRs from confidence intervals. Figure size and typography follow a common journal specification (183 mm double-column width, Arial 5-8 pt, editable
vector output). These remain review candidates.

## New eight-reader analysis

Implementation: `Functions/acquisition_qc_review.R`; runner: `Testing/audits/render_acquisition_qc_review.R`;
fixture tests: `Testing/tests/test_acquisition_qc_review.R`.

The analysis extends the existing saved eight-reader dyads, rather than rerunning raw reconstruction. It uses **exact
same-reader assignment (R1-R8)** with no column pooling or inferred physical distance. Both event-start and single-read
bases at tau 2 s are retained, each with unrestricted assignment and both tags recently detected (<=60 s).

The population is the first active 12 h and following light 12 h of each cage change (Stage-29 windows), not the wider
Stage-19 window of QC02. Released manifest v1.1 excludes all three failed-reader episodes. The builder checks member
identity against the srs roster, refuses duplicate dyads or incomplete method arms, and refuses any retained window
overlapping censoring because a saved dyad aggregate cannot be clipped. No animal is globally dropped from a roster.

Cage-episode overlap is sum same-reader dyad-seconds / sum joint dyad-seconds. At the 60-s convention both tags must be
supported; unsupported time is outside this conditional denominator and is not interpreted as separation. Animal summaries
are explicitly weighted by partner joint-time. Two retained episodes with only one tracked animal have no dyad measure.

The existing marginal-occupancy and short/long circular-shift references are reused. Their mean fractions are aggregated
with observed joint-time weights, as in the original audit. They are descriptive timing-disrupted references, not a
physical null model, confidence interval or new permutation test. No group label or outcome is read. Repeated cage episodes
are not treated as independent experimental replicates, and no biological significance test is added. A deficit can be
caused by read competition; it does not establish avoidance. An excess does not establish affiliation.

## Reproduce a new review run

From the repository root, using PowerShell:

```powershell
Rscript Testing/audits/render_acquisition_qc_review.R --output-root=<absolute-isolated-folder> --run-id=dev_new_id
```

The isolated output root is required. Existing run IDs/build folders are refused. Inputs match producer manifests except
the legacy measurement dyads, which have an explicit byte pin and saved provenance. The protection guard brackets writes;
inputs are re-hashed before publication. Code snapshots, source/output hashes and session information travel with the run.
Uncommitted development code is disclosed in the run manifest. No output is written into the canonical scientific roots.

## Further improvements worth considering separately

1. A full eight-reader occupancy/contact matrix would require a new saved joint-position product; do not derive different-
   reader distances or adjacent-reader overlap from the existing same-reader dyad aggregate.
2. A support-age curve beyond 60 s would require new dyad products (300 s is not present for both bases in this table).
3. Group/sex contrasts, network inference and manuscript imports remain outside this review; each needs a scoped analysis
   specification and must retain cage/batch dependence and measurement limitations.
4. A small historical-versus-QC forest plot could be rendered from the accepted CC4 comparison tables without refitting.
5. Publication assembly can select these panels after review. Existing exported figures and historical records remain intact.
