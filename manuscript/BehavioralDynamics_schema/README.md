# BehavioralDynamics — explanatory provenance schema

Two figures that explain, end to end, how the panel
`Fig_sis_active_inactive_domain_heatmap` is produced:

> *Continuous home-cage behavioural phenotype: multiscale behavioural dimensions,
> split by active/inactive phase and sex, expressed as pairwise contrasts.*

Everything shown was traced to the actual implementation in
`MMMSociability/Analysis`. No transformation was invented. Numbers in the worked
example are real exported values.

## Figures

| File | Size | Purpose |
|---|---|---|
| `rendered/BehavioralDynamics_schema_main.{svg,pdf,png}` | 125 × 262 mm (1.5-column, full page) | Main-paper compact schema: the seven-level pipeline plus a worked example |
| `rendered/BehavioralDynamics_schema_supplementary.{svg,pdf,png}` | 297 × 420 mm (A3) | Full provenance: equations, definitions, input tables, source lines, caveats, full numerical worked example |

PNG is 600 dpi. PDF is written through `cairo_pdf` with Arial and Consolas
subset-embedded. Colours follow the house palette in
`Functions/behavioral_dynamics_helpers.R:66` (`CON #3d3b6e`, `SUS #e63947`), and the
tile ramp mirrors Stage 14's `scale_fill_gradient2(low = "#3d3b6e", mid = "white",
high = "#e63947", midpoint = 0)`.

## Scripts

Run in order from this directory.

| Script | What it does |
|---|---|
| `R/schema_toolkit.R` | Drawing primitives on a millimetre canvas; measured text wrapping; the g-tile colour ramp; svg/pdf/png export |
| `R/00_verify_provenance.R` | Establishes the 5-min backbone and **reproduces the exported DomainScore exactly** from the exported raw features (max \|difference\| = 0 over 882 rows) |
| `R/01_verify_rest_domain_collapse.R` | Availability of the quiescence inputs by phase; how many inputs each composite actually uses |
| `R/02_verify_zero_fill_mechanism.R` | Proves the zero-fill mechanism and what the rest-like and volatility rows really compute |
| `R/10_main_compact_schema.R` | Renders the main-paper figure |
| `R/20_supplementary_provenance_schema.R` + `R/21_supp_warnings.R` | Renders the supplementary sheet |
| `R/30_write_number_manifest.R` | Writes `data/figure_number_manifest.csv` — every displayed number, its source file and how it was obtained |

## Data

`data/` holds the exported tables the figures draw on, the worked-example rows,
the tile values, the chain-validation output and the number manifest. The
upstream source is
`…/analysis_ready/12_systems_neuroscience_summary/5min_based/`.

## The pipeline, in one line each

1. **Raw RFID** — timestamped position identities, not x/y tracking. Three streams: `Movement` (PositionID transitions, `01:470`), `Entropy` (`-Σ p log₂ p` over occupancy seconds, `01:182`), `Proximity` (same-position dyadic seconds / dyadic observation seconds, `01:823`).
2. **Binning** — Stage 01 emits 10 s … whole-phase; the backbone here is **5 min** (`14:72`). Stage 12 and Stage 08 import at 10 min.
3. **Context** — Animal × CageChange × PhaseClass epochs (`14:5226`).
4. **Temporal descriptors** — mean, RMSSD, ACF1 per epoch (`14:5229-5237`).
5. **Contextual z** — within Sex × PhaseClass × CageChangeIndex (`14:5039`).
6. **Domains** — seven composites (`14:5259-5290`).
7. **Contrast → tile** — Welch t-test, Hedges' g as colour, BH FDR within Sex × PhaseClass as symbol (`14:5822`, `14:5951`).

## Findings that changed how the figure had to be drawn

These were verified numerically against the exported outputs, not inferred from
reading code. They are documented in the supplementary sheet as W1–W9.

- **W1** `12_sleep_like_quiescence_metrics.R:60-64` tests `"active|dark|night"`
  before `"inactive|light|day"`. `str_detect` is unanchored and `"inactive"`
  contains `"active"`, so every Inactive epoch is relabelled Active. The same
  defect is live in `11_behavioral_adaptation_kinetics.R:86-90`;
  `13_ethological_phase_organization.R:63-67` is the correct fix template.
- **W2** Consequently no Inactive epoch receives the quiescence features (0 of
  444), and `safe_scale` turns that total absence into an exact **0**, not `NA`.
  The row labelled *Inactive-phase rest/circadian regulation* therefore equals
  `(Movement_acf1_z − Movement_rmssd_z) / 3` — verified to 1.1e-16 over 444 rows,
  correlation 1.0000000000. **It carries no quiescence information at all.**
- **W3** The volatility row runs at three different effective scalings
  (5-of-5, 3-of-5 dropped, and 3-of-5 zero-filled = exactly 0.6×).
- **W4** Cage-change observations are pooled as independent: the exported
  `n_ref`/`n_comp` are row counts at exactly 4× the animal counts.
- **W5** The BH family is cross-domain within Sex × PhaseClass.
- **W6** Colour (pooled-SD Hedges' g) and symbol (Welch) assume different
  variance models.
- **W7** The code label says "circadian" although nothing in the formula
  estimates period, amplitude or phase, and
  `docs/analysis_strategy_for_manuscript.md:215-238` forbids the term.
- **W8** The 20th-percentile inactivity threshold is inert: 56.4 % of Movement
  values are exactly 0 and the per-animal 20th percentile is 0 for **all 111
  animals**, so `inactive_like` is exactly `Movement == 0`.
- **W9** *Behavioral state architecture* falls back to
  `mean(flexibility, volatility)` when HMM output is absent, and where the HMM
  path is used it is the only row standardized with the sexes pooled.

`manuscript/Fig1_behavior_candidates/figure_manifest.csv` independently marks this
panel `blocked_pending_phase_classifier_fix_and_rerun`. Stage 12 must be fixed and
Stages 12 and 14 rerun before the inactive/rest branch is frozen.

## Recommended manuscript inference

The heatmap's symbols currently come from a Welch test that treats CC1–CC4 as
independent. For the manuscript, fit

```r
DomainScore ~ Group * Sex + CageChange + (1 | Animal)
```

separately within Domain × Phase, then take model-based pairwise group contrasts.
Keep a standardized effect size as the tile colour, but source significance from
the repeated-measures model.

Stage 14 already fits a repeated-measures model — `extract_lmm_stats`
(`14:5064-5126`, called at `14:5367-5376`) — exported to
`stats_tables/systems_sis_domain_mixed_model_stats.csv`. It is **not** the source
of the heatmap symbols.
