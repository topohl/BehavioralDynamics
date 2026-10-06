# ================================================================
# Manuscript palette: the single colour source of MMMSociability
# MMMSociability
# ================================================================
# Group and diverging colours of every MMMSociability figure, pinned to the
# manuscript's palette file (Exp9_manuscript config/manuscript_palette.yml, v3.1)
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
# Limits (v3.1): each measure has one fixed colour limit (MMM_DIVERGING_LIMITS),
# applied by mmm_scale_fill_diverging(); bars take a flat fill by sign
# (mmm_scale_fill_sign()), both in behavioral_dynamics_helpers.R.
#
# Pure constants: no packages, no file access. Frozen Stage 30/32 code keeps its
# own colours.
# ================================================================

MMM_PALETTE_VERSION <- "manuscript_palette_v3.1"
MMM_PALETTE_SOURCE <- list(
  repository = "Exp9_manuscript",
  file = "config/manuscript_palette.yml",
  commit = "b455a937ddd07c42a218b1560578964ec71ccadc",          # master, 2026-10-06
  git_blob = "86989712638f91858e0b98019a0e31addf220f02",        # line-ending independent identity
  sha256 = "733472e50b1292341506ca2fde3c8b94c40d3d5f4849bc01192c3935e5ed9d7f")   # bytes of the CRLF working copy

MMM_PALETTE_GROUP <- c(CON = "#6B7296", RES = "#BFBCB4", SUS = "#C74C56")
MMM_PALETTE_DIVERGING <- c(low = "#6679D9", mid = "#FFFFFF", high = "#F2CA4E")
# Fixed colour limits of the diverging scale, by measure (v3.1, 2026-10-06): every figure of a measure uses the same limit,
# so a colour means the same value everywhere. Full colour is reached just beyond Cohen's "large" (d 0.8, r 0.5);
# values beyond the limit are drawn at full colour and the legend ends read <= / >=. A measure that no other figure
# shares may take its own symmetric limit.
MMM_DIVERGING_LIMITS <- c(smd = 1, correlation = 0.6)   # smd = standardized mean difference (Hedges g, Cohen's d)
