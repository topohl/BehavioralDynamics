# ================================================================
# Publication assembly helpers (Nature-compatible) for the GAMM outputs
# MMMSociability
# ================================================================
# Composition/export utilities used ONLY by the manuscript assembly stage.
# Statistical stages must not depend on this file: it knows about panel
# letters, physical sizes and file formats, not about models.
#
# The palette, theme and axis helpers live in mmm_publication_theme.R and are
# shared, so a stage-level figure and a manuscript figure can never disagree
# about group identity.
# ================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tibble); library(readr)
})

if (!exists("theme_mmm_pub", inherits = TRUE)) {
  if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
    source_mmm_helper("mmm_publication_theme.R")
  } else stop("gamm_publication_style_helpers.R requires mmm_publication_theme.R", call. = FALSE)
}

#' Lowercase bold panel label, positioned top-left outside the plot area.
mmm_panel_label <- function(letter) {
  labs(tag = letter)
}
mmm_tag_theme <- function() {
  theme(plot.tag = element_text(size = MMM_PANEL_LABEL_PT, face = "bold", hjust = 0, vjust = 1),
        plot.tag.position = c(0.005, 0.995))
}

#' Export one figure as editable vector PDF + SVG (and optional PNG preview).
#'
#' The default pdf device keeps text as text objects referencing base-14
#' Helvetica, so labels stay editable in Illustrator/Inkscape. Curves are never
#' rasterized.
mmm_export_figure <- function(plot, dir, stem, width_mm, height_mm,
                              png_preview = TRUE, dpi = 600) {
  stopifnot(width_mm <= MMM_WIDTH_DOUBLE_MM, height_mm <= MMM_MAX_HEIGHT_MM)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  pdf_path <- file.path(dir, paste0(stem, ".pdf"))
  svg_path <- file.path(dir, paste0(stem, ".svg"))
  ggsave(pdf_path, plot, width = width_mm, height = height_mm, units = "mm",
         device = "pdf", bg = "white")
  ggsave(svg_path, plot, width = width_mm, height = height_mm, units = "mm",
         device = "svg", bg = "white")
  png_path <- NA_character_
  if (isTRUE(png_preview)) {
    png_path <- file.path(dir, paste0(stem, ".png"))
    ggsave(png_path, plot, width = width_mm, height = height_mm, units = "mm",
           dpi = dpi, bg = "white")
  }
  tibble(stem = stem, pdf = basename(pdf_path), svg = basename(svg_path),
         png = if (is.na(png_path)) NA_character_ else basename(png_path),
         width_mm = width_mm, height_mm = height_mm,
         vector_editable_text = TRUE, colour_space = "RGB",
         base_font = "sans (Helvetica/Arial)", body_text_pt = MMM_BASE_PT,
         panel_label_pt = MMM_PANEL_LABEL_PT, dir = dir)
}

#' Trajectory panel: fitted line + CI ribbon, redundant colour/linetype coding.
mmm_traj_panel <- function(df, x, y, lo, hi, xlab, ylab, facet = "Sex",
                           xbreaks = seq(0, 12, 3), tag = NULL) {
  p <- ggplot(df, aes(.data[[x]], .data[[y]], colour = .data$Group,
                      fill = .data$Group, linetype = .data$Group)) +
    geom_ribbon(aes(ymin = .data[[lo]], ymax = .data[[hi]]), alpha = 0.10,
                colour = NA, show.legend = FALSE) +
    geom_line(linewidth = 0.7) +
    mmm_scale_colour_group() + mmm_scale_fill_group() + mmm_scale_linetype_group() +
    scale_x_continuous(breaks = xbreaks, limits = range(xbreaks), expand = c(0.01, 0)) +
    labs(x = xlab, y = ylab) +
    theme_mmm_pub()
  if (!is.null(facet)) p <- p + facet_wrap(stats::as.formula(paste("~", facet)))
  if (!is.null(tag)) p <- p + mmm_panel_label(tag) + mmm_tag_theme()
  p
}

#' Forest panel: neutral charcoal point + CI, group shape where meaningful.
mmm_forest_panel <- function(df, x, y, lo, hi, xlab, ylab = NULL, facet = "Sex",
                             shape_by = NULL, tag = NULL) {
  p <- ggplot(df, aes(.data[[x]], .data[[y]])) +
    geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey60") +
    geom_errorbarh(aes(xmin = .data[[lo]], xmax = .data[[hi]]), height = 0,
                   linewidth = 0.4, colour = MMM_CONTRAST_COLOUR)
  p <- if (is.null(shape_by)) {
    p + geom_point(size = 1.5, colour = MMM_CONTRAST_COLOUR)
  } else {
    p + geom_point(aes(shape = .data[[shape_by]], fill = .data[[shape_by]]),
                   size = 1.7, colour = MMM_CONTRAST_COLOUR, stroke = 0.35) +
      mmm_scale_shape_group() + mmm_scale_fill_group()
  }
  p <- p + labs(x = xlab, y = ylab) + theme_mmm_pub()
  if (!is.null(facet)) p <- p + facet_wrap(stats::as.formula(paste("~", facet)))
  if (!is.null(tag)) p <- p + mmm_panel_label(tag) + mmm_tag_theme()
  p
}

#' Format a numeric p/q for display without ever printing 0.000.
mmm_fmt_p <- function(p) {
  ifelse(is.na(p), NA_character_,
    ifelse(p < 1e-3, formatC(p, format = "e", digits = 2), formatC(p, format = "g", digits = 3)))
}
mmm_fmt_est <- function(x, digits = 2) formatC(x, format = "f", digits = digits)
mmm_fmt_ci <- function(lo, hi, digits = 2) {
  paste0("[", mmm_fmt_est(lo, digits), ", ", mmm_fmt_est(hi, digits), "]")
}

#' Standard Source Data frame for one plotted panel.
mmm_source_data <- function(df, panel_id, analysis_stage, model_id, cols) {
  df %>% dplyr::select(any_of(cols)) %>%
    mutate(panel_id = panel_id, analysis_stage = analysis_stage, model_id = model_id,
           .before = 1)
}
