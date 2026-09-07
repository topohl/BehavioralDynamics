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

MMM_GROUP_COLOURS <- c(CON = "#4D4D4D", RES = "#0072B2", SUS = "#D55E00")
MMM_GROUP_LEVELS <- c("CON", "RES", "SUS")

# Contrast colours reuse the comparison group's hue so a contrast panel reads
# against the trajectory panel without a second legend.
MMM_CONTRAST_COLOURS <- c(
  "RES-CON" = "#0072B2",
  "SUS-CON" = "#D55E00",
  "SUS-RES" = "#009E73"
)

MMM_BASE_PT <- 8

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
mmm_scale_colour_contrast <- function(...) {
  scale_colour_manual(values = MMM_CONTRAST_COLOURS, ...)
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
