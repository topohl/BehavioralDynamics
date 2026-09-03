# ==============================================================================
# schema_toolkit.R
# Drawing primitives for the BehavioralDynamics provenance schema figures.
#
# The schema figures are diagrams, not data plots. We therefore draw on a fixed
# normalized canvas (x, y in [0, 100]) using ggplot2 annotation layers, so the
# result still exports through the same svg/pdf/png device stack the rest of the
# pipeline uses and so panel geometry stays reproducible in mm.
# ==============================================================================

suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)
  library(dplyr)
  library(tibble)
})

# House palette, taken from Functions/behavioral_dynamics_helpers.R:66 so the
# schema reads as the same figure family as the panel it explains.
SCHEMA_PAL <- list(
  con        = "#3d3b6e",  # mmm_group_colors["CON"]; also the heatmap low end
  res        = "#C6C3BB",  # mmm_group_colors["RES"]
  sus        = "#e63947",  # mmm_group_colors["SUS"]; also the heatmap high end
  ink        = "#1a1a1a",
  ink_soft   = "#4d4d4d",
  ink_faint  = "#7a7a7a",
  rule       = "#c9c9c9",
  panel      = "#f7f7f5",
  panel_alt  = "#eef1f6",
  movement   = "#2f6f9f",
  entropy    = "#6a8f3c",
  proximity  = "#b3762b",
  warn_bg    = "#fdf0ef",
  warn_edge  = "#e63947",
  note_bg    = "#f2f4f8",
  note_edge  = "#9aa7bd",
  ok_bg      = "#f1f7f2",
  ok_edge    = "#5aa469"
)

# geom_text sizes are in mm; authoring in pt is easier to reason about.
pt2mm <- function(pt) pt / .pt

schema_theme <- function() {
  theme_void(base_family = "Arial") +
    theme(
      plot.margin = margin(0, 0, 0, 0),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA)
    )
}

schema_canvas <- function(xlim = c(0, 100), ylim = c(0, 100)) {
  ggplot() +
    coord_cartesian(xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    schema_theme()
}

# --- primitives ---------------------------------------------------------------

sbox <- function(x, y, w, h, fill = "white", colour = SCHEMA_PAL$rule,
                 linewidth = 0.28, alpha = 1, linetype = "solid") {
  annotate("rect", xmin = x, xmax = x + w, ymin = y, ymax = y + h,
           fill = fill, colour = colour, linewidth = linewidth,
           alpha = alpha, linetype = linetype)
}

stext <- function(x, y, label, size = 6, hjust = 0, vjust = 1,
                  colour = SCHEMA_PAL$ink, fontface = "plain", lineheight = 1.12,
                  angle = 0, family = "Arial") {
  annotate("text", x = x, y = y, label = label, size = pt2mm(size),
           hjust = hjust, vjust = vjust, colour = colour, fontface = fontface,
           lineheight = lineheight, angle = angle, family = family)
}

# Monospaced, so verbatim R reads as verbatim R.
scode <- function(x, y, label, size = 5.4, hjust = 0, vjust = 1,
                  colour = SCHEMA_PAL$ink_soft, lineheight = 1.18) {
  annotate("text", x = x, y = y, label = label, size = pt2mm(size),
           hjust = hjust, vjust = vjust, colour = colour,
           family = "Consolas", lineheight = lineheight)
}

sarrow <- function(x1, y1, x2, y2, colour = SCHEMA_PAL$ink_soft,
                   linewidth = 0.3, len = 1.6, type = "closed", linetype = "solid") {
  annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
           colour = colour, linewidth = linewidth, linetype = linetype,
           arrow = arrow(length = unit(len, "mm"), type = type))
}

sline <- function(x1, y1, x2, y2, colour = SCHEMA_PAL$rule,
                  linewidth = 0.25, linetype = "solid") {
  annotate("segment", x = x1, y = y1, xend = x2, yend = y2,
           colour = colour, linewidth = linewidth, linetype = linetype)
}

# The numbered stripe that opens each level of the pipeline.
level_banner <- function(x, y, w, h, n, title, accent = SCHEMA_PAL$con,
                         size = 7, tab = 5.5, subtitle = NULL) {
  out <- list(
    sbox(x, y, w, h, fill = "white", colour = accent, linewidth = 0.4),
    sbox(x, y, tab, h, fill = accent, colour = accent, linewidth = 0.4),
    stext(x + tab / 2, y + h / 2, n, size = size + 0.6, hjust = 0.5, vjust = 0.5,
          colour = "white", fontface = "bold")
  )
  if (is.null(subtitle)) {
    out <- c(out, list(stext(x + tab + 1.6, y + h / 2, title, size = size,
                             hjust = 0, vjust = 0.5, fontface = "bold")))
  } else {
    out <- c(out, list(
      stext(x + tab + 1.6, y + h * 0.68, title, size = size, hjust = 0, vjust = 0.5,
            fontface = "bold"),
      stext(x + tab + 1.6, y + h * 0.26, subtitle, size = size - 1.4, hjust = 0,
            vjust = 0.5, colour = SCHEMA_PAL$ink_faint)
    ))
  }
  out
}

callout <- function(x, y, w, h, label, kind = c("warn", "note", "ok"), size = 5.4,
                    pad = 1.4, fontface = "plain", title = NULL) {
  kind <- match.arg(kind)
  bg   <- switch(kind, warn = SCHEMA_PAL$warn_bg, note = SCHEMA_PAL$note_bg, ok = SCHEMA_PAL$ok_bg)
  edge <- switch(kind, warn = SCHEMA_PAL$warn_edge, note = SCHEMA_PAL$note_edge, ok = SCHEMA_PAL$ok_edge)
  out <- list(
    sbox(x, y, w, h, fill = bg, colour = edge, linewidth = 0.3),
    sbox(x, y, 0.8, h, fill = edge, colour = edge, linewidth = 0.3)
  )
  ytop <- y + h - pad * 0.5
  if (!is.null(title)) {
    out <- c(out, list(stext(x + pad + 0.8, ytop, title, size = size, hjust = 0,
                             vjust = 1, colour = edge, fontface = "bold")))
    ytop <- ytop - (size * 0.052 * 100 / 100) - 2.1
  }
  out <- c(out, list(stext(x + pad + 0.8, ytop, label, size = size, hjust = 0,
                           vjust = 1, fontface = fontface)))
  out
}

kv_row <- function(x, y, key, value, key_w = 12, size = 5.2,
                   key_colour = SCHEMA_PAL$ink_faint, value_colour = SCHEMA_PAL$ink) {
  list(
    stext(x, y, key, size = size, hjust = 0, vjust = 1, colour = key_colour,
          fontface = "bold"),
    stext(x + key_w, y, value, size = size, hjust = 0, vjust = 1, colour = value_colour)
  )
}

# Inline heatmap tile mirroring Stage 14's
# scale_fill_gradient2(low = "#3d3b6e", mid = "white", high = "#e63947", midpoint = 0).
tile_colour <- function(g, limit = 1.0) {
  f <- max(-1, min(1, g / limit))
  if (f >= 0) {
    grDevices::colorRampPalette(c("white", SCHEMA_PAL$sus))(101)[round(f * 100) + 1]
  } else {
    grDevices::colorRampPalette(c("white", SCHEMA_PAL$con))(101)[round(-f * 100) + 1]
  }
}

gtile <- function(x, y, w, h, g, sig = "", limit = 1.0, size = 5.4,
                  show_value = TRUE, na = FALSE) {
  col <- if (na) "grey90" else tile_colour(g, limit)
  txt <- if (!na && abs(max(-1, min(1, g / limit))) > 0.55) "white" else SCHEMA_PAL$ink
  out <- list(sbox(x, y, w, h, fill = col, colour = "white", linewidth = 0.5))
  if (na) {
    return(c(out, list(stext(x + w / 2, y + h / 2, "NA", size = size, hjust = 0.5,
                             vjust = 0.5, colour = SCHEMA_PAL$ink_faint))))
  }
  if (show_value) {
    out <- c(out, list(
      stext(x + w / 2, y + h / 2 + 0.9, sprintf("%+.2f", g), size = size,
            hjust = 0.5, vjust = 0.5, colour = txt, fontface = "bold"),
      stext(x + w / 2, y + h / 2 - 1.6, sig, size = size, hjust = 0.5, vjust = 0.5,
            colour = txt)
    ))
  }
  out
}

# Continuous colour key for the g scale.
gtile_legend <- function(x, y, w, h, limit = 1.0, n = 120, size = 5,
                         labels = TRUE, title = NULL) {
  gs <- seq(-limit, limit, length.out = n)
  step <- w / n
  out <- lapply(seq_len(n), function(i) {
    annotate("rect", xmin = x + (i - 1) * step, xmax = x + i * step,
             ymin = y, ymax = y + h, fill = tile_colour(gs[i], limit), colour = NA)
  })
  out <- c(out, list(sbox(x, y, w, h, fill = NA, colour = SCHEMA_PAL$ink_faint,
                          linewidth = 0.25)))
  if (labels) {
    out <- c(out, list(
      stext(x, y - 0.8, sprintf("%.1f", -limit), size = size, hjust = 0.5, vjust = 1,
            colour = SCHEMA_PAL$ink_soft),
      stext(x + w / 2, y - 0.8, "0", size = size, hjust = 0.5, vjust = 1,
            colour = SCHEMA_PAL$ink_soft),
      stext(x + w, y - 0.8, sprintf("+%.1f", limit), size = size, hjust = 0.5,
            vjust = 1, colour = SCHEMA_PAL$ink_soft)
    ))
  }
  if (!is.null(title)) {
    out <- c(out, list(stext(x + w / 2, y + h + 0.7, title, size = size, hjust = 0.5,
                             vjust = 0, colour = SCHEMA_PAL$ink_soft)))
  }
  out
}

# Export through the same device stack the pipeline uses.
save_schema <- function(plot, base, width, height, units = "mm", dpi = 600) {
  dir.create(dirname(base), recursive = TRUE, showWarnings = FALSE)
  ggsave(paste0(base, ".svg"), plot, width = width, height = height, units = units)
  dev_pdf <- if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else "pdf"
  ggsave(paste0(base, ".pdf"), plot, width = width, height = height, units = units,
         device = dev_pdf)
  ggsave(paste0(base, ".png"), plot, width = width, height = height, units = units,
         dpi = dpi, device = ragg::agg_png)
  message("wrote: ", base, ".{svg,pdf,png}")
  invisible(base)
}

# --- text metrics -------------------------------------------------------------
# Approximate advance width per character, in mm, at a given pt size.
# Arial averages ~0.50 em over mixed-case prose; Consolas is a 0.55 em monospace.
# Calibrated against rendered output, so wrapping can be enforced before drawing
# rather than discovered by eye afterwards.
char_mm <- function(size, family = "Arial") {
  em <- size / 2.845                      # pt -> mm
  em * if (identical(family, "Consolas")) 0.55 else 0.50
}

fits <- function(label, size, width_mm, family = "Arial") {
  w <- max(nchar(strsplit(label, "\n", fixed = TRUE)[[1]]), 0) * char_mm(size, family)
  w <= width_mm
}

# Hard-wrap prose to a measured width. Returns a single \n-joined string.
# Explicit newlines in the input are PRESERVED as hard breaks and each segment
# is wrapped independently, so hand-laid-out code keeps its line structure.
wrp <- function(txt, size, width_mm, family = "Arial") {
  n <- max(8L, floor(width_mm / char_mm(size, family)))
  segs <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (!length(segs)) return("")
  # Monospaced text is hand-laid-out: keep the lines and their indentation
  # verbatim, and only report (not silently reflow) anything too wide.
  if (identical(family, "Consolas")) return(paste(segs, collapse = "\n"))
  paste(vapply(segs, function(s) {
    if (!nzchar(trimws(s))) return("")
    paste(strwrap(s, width = n), collapse = "\n")
  }, character(1)), collapse = "\n")
}

# Assert that nothing silently overflows the content column.
check_fit <- function(label, size, width_mm, family = "Arial", where = "") {
  bad <- Filter(function(l) nchar(l) * char_mm(size, family) > width_mm,
                strsplit(label, "\n", fixed = TRUE)[[1]])
  if (length(bad)) warning(sprintf("OVERFLOW [%s] %.1fmm avail: %s", where, width_mm,
                                   substr(bad[1], 1, 60)), call. = FALSE)
  invisible(length(bad) == 0)
}
