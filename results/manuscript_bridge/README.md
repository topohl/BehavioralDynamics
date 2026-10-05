# The 2026-09 Figure 1 bridge (frozen, historical)

These tracked tables are the Figure 1 bridge export of September 2026. Exp9_manuscript imported them at its commits
5460cd4 and 30a2666; its own sha256 import manifest is their provenance.

- They predate the 2026-09-20 CombZ correction and the 2026-09-22 leading-bin fix (for example 49 RES and 38 SUS
  instead of 53 and 34). The tree is also mixed: three files were regenerated later.
- Their builders were retired on 2026-10-05 (`docs/LEGACY_AND_GAMM_RETIREMENT_2026-10-05.md`) and cannot reproduce
  these bytes.
- Nothing new may import them. Figure 1 is rendered from the frozen `ebb_v101` bundle.
- They stay unchanged until Extended Data 5 and 9 in Exp9_manuscript move to `ebb_v101`; then they are archived.

`Testing/tests/test_figure1_panel_source_data.R` keeps checking `figure1/figure_source_data/` against the contract it
was frozen with.
