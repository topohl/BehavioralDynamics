# ================================================================
# Manuscript palette: the single colour source of MMMSociability
# MMMSociability
# ================================================================
# Group and diverging colours of every MMMSociability figure, pinned to the
# manuscript's palette file (Exp9_manuscript config/manuscript_palette.yml, v2)
# so the analysis figures and the manuscript's own renderers draw the same
# colours without reading another repository at run time. mmm_group_colors,
# mmm_pair_colors, mmm_diverging_colors (behavioral_dynamics_helpers.R),
# MMM_GROUP_COLOURS, MMM_DIVERGING_COLOURS (mmm_publication_theme.R) and
# MMM_DHM_PALETTE (hmm_stage14_helpers.R) are aliases of these values;
# Testing/tests/test_manuscript_palette.R keeps them so.
#
# Group: navy CON, beige RES, red SUS (as in manuscript Figure 1). RES is light,
# so group must never be shown by colour alone: shape, line type or position
# carry it too.
# Diverging (signed effects such as Hedges g): blue-grey below zero, warm grey
# at zero, dark orange above zero. Both ends lie at the same perceptual distance
# from the midpoint (CIEDE2000 44.8 and 44.6), so effects of equal size look
# equally strong; the scale stays separable under deutan, protan and tritan
# simulation.
#
# Pure constants: no packages, no file access. Frozen Stage 30/32 code keeps its
# own colours.
# ================================================================

MMM_PALETTE_VERSION <- "manuscript_palette_v2"
MMM_PALETTE_SOURCE <- list(
  repository = "Exp9_manuscript",
  file = "config/manuscript_palette.yml",
  commit = "176fc7e9973ac92cf6a02a694ae708b862b711cd",          # master, 2026-10-05
  git_blob = "1f1c4e39e23dff1c05dcf701b91ff916a6d8318c",        # line-ending independent identity
  sha256 = "18d071236c5fdb3c90d723911420ec81db5b920f12eb3e8940249921a5bee246")   # bytes of the CRLF working copy

MMM_PALETTE_GROUP <- c(CON = "#3E3C6F", RES = "#C6C3BB", SUS = "#E63A48")
MMM_PALETTE_DIVERGING <- c(low = "#4C566A", mid = "#D8D2C7", high = "#96460A")
