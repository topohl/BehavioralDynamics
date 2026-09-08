# ================================================================
# Restrained publication figure theme (Nature-like)
# MMMSociability
# ================================================================
# One theme and one palette for every manuscript-facing GAMM figure, so the
# Active and Inactive panels are directly comparable by eye.
#
# Palette is colour-vision-deficiency safe (Okabe-Ito blue/vermillion against a
# neutral charcoal control) and prints legibly in greyscale because the three
# lightnesses differ.
# ================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
})

# FIXED manuscript identity colours. Do not substitute, lighten or darken.
MMM_GROUP_COLOURS <- c(CON = "#3E3C6F", RES = "#C6C3BB", SUS = "#E63A48")
MMM_GROUP_LEVELS <- c("CON", "RES", "SUS")

# RES (#C6C3BB) is deliberately light, so colour must never be the ONLY group
# encoding. Line type and point shape carry the same information redundantly,
# which also keeps the figures readable in greyscale.
MMM_GROUP_LINETYPES <- c(CON = "solid", RES = "longdash", SUS = "dotdash")
MMM_GROUP_SHAPES <- c(CON = 21L, RES = 24L, SUS = 22L)   # circle, triangle, square

# Pairwise contrasts are drawn in neutral charcoal: inventing a third colour
# family would compete with the fixed group identity, and contrast panels carry
# their identity in the row label, not in hue.
MMM_CONTRAST_COLOUR <- "#2B2B2B"

# Nature body text is 5-7 pt at final size; 7 pt base keeps axis text at 6.5 pt.
MMM_BASE_PT <- 7
MMM_PANEL_LABEL_PT <- 8

# Nature column widths in mm.
MMM_WIDTH_SINGLE_MM <- 89
MMM_WIDTH_MEDIUM_MM <- 120
MMM_WIDTH_DOUBLE_MM <- 183
MMM_MAX_HEIGHT_MM <- 170

#' Minimal publication theme: white, gridless, borderless, thin rules.
theme_mmm_pub <- function(base_size = MMM_BASE_PT, base_family = "sans") {
  half <- base_size / 2
  theme_classic(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black", linewidth = 0.3),
      axis.ticks = element_line(colour = "black", linewidth = 0.3),
      axis.ticks.length = unit(2, "pt"),
      axis.text = element_text(colour = "black", size = base_size - 1),
      axis.title = element_text(colour = "black", size = base_size),
      strip.background = element_blank(),
      strip.text = element_text(colour = "black", size = base_size, face = "plain",
                                margin = margin(b = half / 2, t = half / 2)),
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_blank(),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, -half, 0),
      legend.key.height = unit(base_size, "pt"),
      legend.key.width = unit(base_size * 1.6, "pt"),
      legend.text = element_text(size = base_size - 1, colour = "black"),
      plot.title = element_text(size = base_size, hjust = 0, face = "plain",
                                margin = margin(b = half)),
      plot.subtitle = element_text(size = base_size - 1, hjust = 0, colour = "grey25",
                                   margin = margin(b = half)),
      plot.caption = element_text(size = base_size - 2, hjust = 0, colour = "grey30"),
      plot.margin = margin(half, half, half, half),
      complete = TRUE
    )
}

mmm_scale_colour_group <- function(...) {
  scale_colour_manual(values = MMM_GROUP_COLOURS, limits = MMM_GROUP_LEVELS, ...)
}
mmm_scale_fill_group <- function(...) {
  scale_fill_manual(values = MMM_GROUP_COLOURS, limits = MMM_GROUP_LEVELS, ...)
}
mmm_scale_linetype_group <- function(...) {
  scale_linetype_manual(values = MMM_GROUP_LINETYPES, limits = MMM_GROUP_LEVELS, ...)
}
mmm_scale_shape_group <- function(...) {
  scale_shape_manual(values = MMM_GROUP_SHAPES, limits = MMM_GROUP_LEVELS, ...)
}
#' Deprecated colour scale kept so older stage figures still render; contrast
#' panels should use the single neutral MMM_CONTRAST_COLOUR instead.
mmm_scale_colour_contrast <- function(...) {
  scale_colour_manual(values = stats::setNames(
    rep(MMM_CONTRAST_COLOUR, 3), c("RES-CON", "SUS-CON", "SUS-RES")), ...)
}

#' y axis for a log1p-fitted curve, labelled in response units.
#'
#' Breaks are chosen on the RESPONSE scale and placed at log1p positions, so
#' the axis reads in RFID transitions while the curve stays on the fitted
#' model scale. This is a relabelling, not a back-transformation of the
#' estimate: expm1(linear predictor) is a median-like fitted value, not an
#' unbiased arithmetic mean, and captions must say so.
mmm_scale_y_log1p <- function(response_breaks = c(0, 1, 2, 5, 10, 20, 50),
                              name = "Movement (RFID transitions / 10 min)") {
  scale_y_continuous(
    name = name,
    breaks = log1p(response_breaks),
    labels = response_breaks
  )
}

MMM_X_LAB_ACTIVE <- "Time after regrouping (h)"
MMM_X_LAB_INACTIVE <- "Time in subsequent inactive phase (h)"

MMM_CAPTION_LOG1P <- paste(
  "GAMMs were fitted to log1p-transformed Movement.",
  "Curves are batch-marginal fitted values with the animal random effect excluded;",
  "bands are 95% CIs from the smoothing-parameter-corrected Bayesian covariance."
)

#' Consistent save wrapper. Dimensions are in mm at final print size.
mmm_save_pub <- function(plot, path, width_mm, height_mm, dpi = 600) {
  ggsave(path, plot, width = width_mm, height = height_mm, units = "mm",
         dpi = dpi, bg = "white")
  invisible(tibble::tibble(file = basename(path), path = path,
                           width_mm = width_mm, height_mm = height_mm))
}

# ================================================================
# Generic additions used by manuscript assemblers (append-only)
# ================================================================
# Nothing above this line is modified. These are generic helpers that were
# missing when the behavior main figure was assembled; they are defined here
# rather than in an assembler so a stage figure and a manuscript figure can
# never disagree about encoding.

#' Diverging fill scale for a standardized effect size (e.g. Hedges g).
#'
#' Anchored on the two extreme FIXED identity colours with white at zero, which
#' is the diverging convention already used elsewhere in the repository. It
#' encodes DIRECTION AND MAGNITUDE of a contrast, never significance: cells are
#' never recoloured by a p or q value.
#'
#' `limits` is symmetric by construction so that equal and opposite effects are
#' equally salient, and values outside it are squished rather than dropped, so a
#' large effect can never render as missing data.
mmm_scale_fill_effect <- function(limits = c(-1.2, 1.2),
                                  name = "Hedges g",
                                  ...) {
  limits <- c(-max(abs(limits)), max(abs(limits)))
  scale_fill_gradient2(
    low = MMM_GROUP_COLOURS[["CON"]], mid = "white",
    high = MMM_GROUP_COLOURS[["SUS"]], midpoint = 0,
    limits = limits, oob = scales::squish, name = name, ...
  )
}

#' Minimal theme for a schematic panel: no axes, no grid, no background.
#'
#' A schematic carries meaning in its boxes and arrows, so every axis element is
#' removed rather than merely blanked, and the plot margin is kept identical to
#' theme_mmm_pub() so a schematic composes flush with a data panel.
theme_mmm_schematic <- function(base_size = MMM_BASE_PT, base_family = "sans") {
  half <- base_size / 2
  theme_void(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      legend.position = "none",
      plot.title = element_text(size = base_size, hjust = 0, face = "plain",
                                margin = margin(b = half)),
      plot.margin = margin(half, half, half, half),
      complete = TRUE
    )
}

#' Neutral greys for schematic assay/measurement boxes.
#'
#' Assay names must NOT be drawn in the group identity colours: at the moment an
#' assay is run the later phenotype label does not exist yet, and colouring an
#' assay box would imply it does.
MMM_SCHEMATIC_INK <- c(
  box_fill    = "#F2F2F2",
  box_border  = "#7A7A7A",
  stage_fill  = "#FFFFFF",
  stage_border= "#2B2B2B",
  arrow       = "#4D4D4D",
  text        = "#1A1A1A",
  caveat      = "#8A6D3B"
)

#' Canonical wording for the direction of the CombZ endpoint.
#'
#' Copied verbatim from Analysis/14_systems_neuroscience_summary_dashboard.R so
#' that every figure describes the sign of the outcome identically.
MMM_COMBZ_DIRECTION_LABEL <-
  "CombZ (lower = worse depressive-like endpoint; higher = more resilient-like endpoint)"
