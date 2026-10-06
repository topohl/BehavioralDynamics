# ================================================================
# Manuscript palette: the single colour source of MMMSociability
# MMMSociability
# ================================================================
# Group and diverging colours of every MMMSociability figure, pinned to the
# manuscript's palette file (Exp9_manuscript config/manuscript_palette.yml, v3)
# so the analysis figures and the manuscript's own renderers draw the same
# colours without reading another repository at run time. mmm_group_colors,
# mmm_pair_colors, mmm_diverging_colors (behavioral_dynamics_helpers.R),
# MMM_GROUP_COLOURS, MMM_DIVERGING_COLOURS (mmm_publication_theme.R) and
# MMM_DHM_PALETTE (hmm_stage14_helpers.R) are aliases of these values;
# Testing/tests/test_manuscript_palette.R keeps them so.
#
# Group: slate blue CON, warm grey RES, red SUS (v3, 2026-10-06). RES is light
# and nearly grey, so group must never be shown by colour alone: shape, line
# type or position carry it too. CON and SUS stay apart under protan and deutan
# simulation (OKLab dE 9.9) and for normal vision (18.0).
# Diverging (signed effects such as Hedges g): blue below zero, white at zero,
# yellow above zero. The ends separate from each other and from white under
# deutan, protan and tritan simulation. The yellow end is the lighter one, so
# its arm is weaker (CIEDE2000 from white 28.4 vs 41.5), and a missing value
# must take a fill other than white (a light grey).
#
# Pure constants: no packages, no file access. Frozen Stage 30/32 code keeps its
# own colours.
# ================================================================

MMM_PALETTE_VERSION <- "manuscript_palette_v3"
MMM_PALETTE_SOURCE <- list(
  repository = "Exp9_manuscript",
  file = "config/manuscript_palette.yml",
  commit = "92a0adda81ab1f0974d05f1ce2121693f95f656f",          # master, 2026-10-06
  git_blob = "5db78b9c18be2884f4145d324f64544b3ef6edc0",        # line-ending independent identity
  sha256 = "fd2ec24c2dcca8372387d73b27ede2a56834e753bf44f8dd50f4ddc39ee754bf")   # bytes of the CRLF working copy

MMM_PALETTE_GROUP <- c(CON = "#6B7296", RES = "#BFBCB4", SUS = "#C74C56")
MMM_PALETTE_DIVERGING <- c(low = "#6679D9", mid = "#FFFFFF", high = "#F2CA4E")
