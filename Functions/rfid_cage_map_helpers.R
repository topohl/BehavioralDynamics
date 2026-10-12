# ================================================================
# RFID cage maps: reader occupancy on the 4 x 2 reader grid of the SIS home cage
# MMMSociability
# ================================================================
# Used by Analysis/19_spatial_occupancy_maps.R. Pure definitions: sourcing this file reads no data and writes no
# files, so a re-plot harness or a test can use the same summaries and plots as the stage.
#
# Geometry. Eight readers in two rows along the long axis of the cage, at the vendor tblPosition coordinates
# (x = 0/100/200/300, y = 0/116); the same grid as animalpos_position_id(). PositionID 1-4 is the y = 0 row and
# 5-8 the y = 116 row, both left to right. Maps draw the y = 0 row on top and keep the true reader spacing.
#
# Occupancy is the share of an animal's reconstructed time assigned to a reader: the vendor position carried forward
# to the next vendor record. It is a position-record map, not continuous tracking (see the Stage 19 header).
#
# Resolution. The vendor stream writes a record only after the tag's position estimate moves >= 200 grid units, so a
# change of row within a column (116 units) is never recorded: 0 of 592,161 position changes in the 24 preprocessed
# files, against 17,177 R1<->R5 changes in the raw antenna logs of the CC1 first active phase alone (check of
# 2026-10-12). From vendor records the row of a column is therefore only the row on which the animal arrived, and
# the interpretable map is the long-axis profile: four columns (R1+R5, R2+R6, R3+R7, R4+R8). `resolution = "column"`
# draws that profile in the same cage outline; "reader" draws the full 4 x 2 grid, for occupancy built from raw
# antenna reads.
# ================================================================

RFID_READER_MAP <- tibble::tibble(
  PositionID = 1:8,
  ReaderX = c(0, 100, 200, 300, 0, 100, 200, 300),
  ReaderY = c(0, 0, 0, 0, 116, 116, 116, 116),
  ReaderLabel = paste0("R", 1:8),
  ColumnID = c(1:4, 1:4)
)

RFID_COLUMN_MAP <- tibble::tibble(
  ColumnID = 1:4,
  ColumnX = c(0, 100, 200, 300),
  ColumnLabel = c("R1/R5", "R2/R6", "R3/R7", "R4/R8")
)

# Share of time per reader if an animal used the cage uniformly.
RFID_UNIFORM_OCCUPANCY <- 1 / 8

# Occupancy colour scale: one hue, light to dark, with one fixed top for every cage map, so a colour is the same
# share of time in every figure. Animal maps reach higher shares than group means; values above the top are drawn at
# full colour and the legend end reads ">=".
RFID_OCCUPANCY_RAMP <- c("#F7FAFE", "#CDE2FB", "#86B6EF", "#3987E5", "#1C5CAB", "#0D366B")
RFID_OCCUPANCY_LIMIT <- 0.6

# Drawing geometry in reader units: tiles fill the reader spacing (100 x 116) up to a small gap.
RFID_TILE_W <- 92
RFID_TILE_H <- 108
RFID_CAGE_X <- c(-54, 354)
RFID_CAGE_Y <- c(-174, 58)       # plotted y is -ReaderY, so the y = 0 row is on top
RFID_NOTE_ROOM <- 34             # room for one line of text above or below the cage

# Typographic minus and >= for text drawn in the artwork, kept as code points so this file stays ASCII.
.rfid_minus <- intToUtf8(0x2212)
.rfid_geq <- intToUtf8(0x2265)

# ---------------------------------------------------------------
# Summaries
# ---------------------------------------------------------------

# Animal-level occupancy, time-weighted: an animal's seconds at a reader over its observed seconds, pooled over all
# bins or phases of one window. `dat` has one row per animal x unit (bin or phase) x reader, carrying PositionSeconds
# and the unit's observation_seconds (repeated on its eight reader rows). Averaging the per-unit fractions instead
# would give a sparsely observed unit the weight of a fully observed one, and keeping units as rows would count one
# animal several times in every group summary.
rfid_animal_reader_occupancy <- function(dat, by) {
  missing <- setdiff(c(by, "PositionID", "PositionSeconds", "observation_seconds"), names(dat))
  if (length(missing) > 0) {
    stop("rfid_animal_reader_occupancy(): missing column(s) ", paste(missing, collapse = ", "), call. = FALSE)
  }
  dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "PositionID")))) %>%
    dplyr::summarise(
      PositionSeconds = sum(PositionSeconds, na.rm = TRUE),
      observation_seconds = sum(observation_seconds, na.rm = TRUE),
      n_units = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(occupancy_fraction = ifelse(observation_seconds > 0,
                                              PositionSeconds / observation_seconds, NA_real_))
}

# Long-axis profile: the two readers of each column pooled, from the output of rfid_animal_reader_occupancy() (which
# must carry ColumnID). `by` identifies the animal x window and must not contain reader-level columns.
rfid_animal_column_occupancy <- function(animal_reader_dat, by) {
  missing <- setdiff(c(by, "ColumnID", "PositionSeconds", "observation_seconds"), names(animal_reader_dat))
  if (length(missing) > 0) {
    stop("rfid_animal_column_occupancy(): missing column(s) ", paste(missing, collapse = ", "), call. = FALSE)
  }
  out <- animal_reader_dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, "ColumnID")))) %>%
    dplyr::summarise(
      PositionSeconds = sum(PositionSeconds, na.rm = TRUE),
      observation_spread = max(observation_seconds) - min(observation_seconds),
      observation_seconds = dplyr::first(observation_seconds),
      n_readers = dplyr::n(),
      .groups = "drop"
    )
  if (any(out$observation_spread > 1e-6) || any(out$n_readers != 2L)) {
    stop("rfid_animal_column_occupancy(): every animal x window must have both readers of a column with one ",
         "observed duration.", call. = FALSE)
  }
  out %>%
    dplyr::select(-observation_spread) %>%
    dplyr::left_join(RFID_COLUMN_MAP, by = "ColumnID") %>%
    dplyr::mutate(occupancy_fraction = ifelse(observation_seconds > 0,
                                              PositionSeconds / observation_seconds, NA_real_))
}

# Fails unless `dat` holds at most one row per animal within every combination of `by` and spatial unit (`unit`:
# PositionID or ColumnID), i.e. unless the animal is the unit of every mean, SD and effect size computed from it.
rfid_assert_one_row_per_animal <- function(dat, by, label, unit = "PositionID") {
  dup <- dat %>%
    dplyr::count(dplyr::across(dplyr::all_of(c(by, unit, "AnimalNum")))) %>%
    dplyr::filter(n > 1)
  if (nrow(dup) > 0) {
    stop(label, ": ", nrow(dup), " animal x ", unit, " cell(s) have more than one row; pool them to one value per ",
         "animal before summarising.", call. = FALSE)
  }
  invisible(TRUE)
}

# Group means with the animal as the unit.
rfid_group_reader_occupancy <- function(animal_dat, by, unit = "PositionID") {
  rfid_assert_one_row_per_animal(animal_dat, by, "rfid_group_reader_occupancy()", unit)
  animal_dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, unit)))) %>%
    dplyr::summarise(
      n_animals = dplyr::n_distinct(AnimalNum),
      mean_occupancy = mean(occupancy_fraction, na.rm = TRUE),
      sd_occupancy = stats::sd(occupancy_fraction, na.rm = TRUE),
      se_occupancy = sd_occupancy / sqrt(n_animals),
      median_occupancy = stats::median(occupancy_fraction, na.rm = TRUE),
      .groups = "drop"
    )
}

# Cohen's d with the pooled SD; NA when either group has fewer than two values or no spread.
rfid_cohens_d <- function(xa, xb) {
  xa <- xa[is.finite(xa)]
  xb <- xb[is.finite(xb)]
  if (length(xa) < 2 || length(xb) < 2) return(NA_real_)
  s_pool <- sqrt(((length(xa) - 1) * stats::var(xa) + (length(xb) - 1) * stats::var(xb)) /
                   (length(xa) + length(xb) - 2))
  if (!is.finite(s_pool) || s_pool == 0) return(NA_real_)
  (mean(xa) - mean(xb)) / s_pool
}

# Pairwise group differences per spatial unit, one value per animal. `specs` has columns contrast, group_a, group_b.
rfid_reader_occupancy_contrasts <- function(animal_dat, by, specs, unit = "PositionID") {
  rfid_assert_one_row_per_animal(animal_dat, by, "rfid_reader_occupancy_contrasts()", unit)
  animal_dat %>%
    dplyr::mutate(Group_chr = as.character(Group)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(by, unit)))) %>%
    dplyr::group_modify(~ {
      d <- .x
      purrr::pmap_dfr(specs, function(contrast, group_a, group_b) {
        xa <- d$occupancy_fraction[d$Group_chr == group_a]
        xb <- d$occupancy_fraction[d$Group_chr == group_b]
        tibble::tibble(
          contrast = contrast,
          group_a = group_a,
          group_b = group_b,
          n_a = sum(is.finite(xa)),
          n_b = sum(is.finite(xb)),
          mean_a = if (any(is.finite(xa))) mean(xa, na.rm = TRUE) else NA_real_,
          mean_b = if (any(is.finite(xb))) mean(xb, na.rm = TRUE) else NA_real_,
          delta_occupancy = mean_a - mean_b,
          cohens_d = rfid_cohens_d(xa, xb)
        )
      })
    }) %>%
    dplyr::ungroup()
}

# ---------------------------------------------------------------
# Labels
# ---------------------------------------------------------------

rfid_format_percent <- function(x, suffix = "%") {
  ifelse(is.finite(x), paste0(formatC(100 * x, format = "f", digits = 0), suffix), "")
}

rfid_format_signed <- function(x, digits = 1) {
  out <- formatC(abs(x), format = "f", digits = digits)
  zero <- out == formatC(0, format = "f", digits = digits)
  out <- ifelse(zero, out, paste0(ifelse(x < 0, .rfid_minus, "+"), out))
  ifelse(is.finite(x), out, "")
}

rfid_contrast_label <- function(contrast) {
  out <- sub("_minus_", paste0(" ", .rfid_minus, " "), as.character(contrast), fixed = TRUE)
  factor(out, levels = unique(out))
}

# Text ink on a fill: white on dark fills, black otherwise (WCAG relative luminance).
rfid_ink_for_fill <- function(fill) {
  rgb <- grDevices::col2rgb(fill) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- 0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  ifelse((1.05 / (lum + 0.05)) > ((lum + 0.05) / 0.05), "white", "black")
}

# The fill the occupancy scale gives a value, for choosing the label ink.
rfid_occupancy_fill <- function(x, limit = RFID_OCCUPANCY_LIMIT) {
  pal <- scales::col_numeric(RFID_OCCUPANCY_RAMP, domain = c(0, limit), na.color = "grey90")
  pal(pmin(pmax(x, 0), limit))
}

# ---------------------------------------------------------------
# Scales, theme and the map itself
# ---------------------------------------------------------------

rfid_scale_fill_occupancy <- function(name = "Share of time", limit = RFID_OCCUPANCY_LIMIT) {
  breaks <- seq(0, limit, by = 0.1)
  labels <- paste0(formatC(100 * breaks, format = "f", digits = 0), "%")
  labels[length(labels)] <- paste0(.rfid_geq, labels[length(labels)])
  ggplot2::scale_fill_gradientn(
    colours = RFID_OCCUPANCY_RAMP, limits = c(0, limit), breaks = breaks, labels = labels,
    oob = scales::squish, na.value = "grey90", name = name,
    guide = ggplot2::guide_colourbar(barheight = grid::unit(28, "mm"), barwidth = grid::unit(2.6, "mm"))
  )
}

rfid_cage_map_theme <- function(base_size = 7, base_family = "Arial") {
  ggplot2::theme_void(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 0.5, margin = ggplot2::margin(1, 1, 2, 1)),
      strip.text.y = ggplot2::element_text(face = "bold", size = base_size - 0.5, angle = 0, hjust = 0,
                                           margin = ggplot2::margin(1, 1, 1, 3)),
      legend.title = ggplot2::element_text(size = base_size - 1, margin = ggplot2::margin(b = 3)),
      legend.text = ggplot2::element_text(size = base_size - 1.5),
      legend.margin = ggplot2::margin(0, 0, 0, 6),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1, margin = ggplot2::margin(b = 2)),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, colour = "grey25", margin = ggplot2::margin(b = 4)),
      plot.caption = ggplot2::element_text(size = base_size - 1.5, colour = "grey35", hjust = 0,
                                           margin = ggplot2::margin(t = 4)),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.margin = ggplot2::margin(4, 4, 4, 4),
      panel.spacing.x = grid::unit(4, "mm"),
      panel.spacing.y = grid::unit(2.5, "mm")
    )
}

# One cage per facet panel; facets and the fill scale are added by the caller.
#   resolution = "reader": `dat` has one row per panel x reader (ReaderX, ReaderY, ReaderLabel), drawn as the 4 x 2 grid.
#   resolution = "column": `dat` has one row per panel x column (ColumnX, ColumnLabel), drawn as four tiles spanning
#                          both rows (the long-axis profile; see the header for why vendor records need it).
# Optional extras:
#   label_col        text drawn in each tile (already formatted); its ink follows `ink_col` or defaults to black
#   reader_ids       draw the reader (R1) or column (R1/R5) name in the tile corner
#   notes            one row per panel (facet columns + `note`), drawn above or below the cage (`note_side`)
#   outlines         one row per panel (facet columns + `outline`), the cage outline colour per panel
#   tile_border      tile outline colour (MMM_TILE_BORDER for diverging maps, so a zero tile stays visible)
rfid_cage_map <- function(dat, fill_col, resolution = c("reader", "column"), label_col = NULL, ink_col = NULL,
                          reader_ids = FALSE, notes = NULL, note_side = c("bottom", "top"), outlines = NULL,
                          tile_border = "grey85", label_pt = 6, note_pt = 5.5) {
  resolution <- match.arg(resolution)
  note_side <- match.arg(note_side)
  y_lim <- RFID_CAGE_Y + c(-6, 6)
  if (!is.null(notes)) {
    if (note_side == "bottom") y_lim[1] <- y_lim[1] - RFID_NOTE_ROOM else y_lim[2] <- y_lim[2] + RFID_NOTE_ROOM
  }
  if (resolution == "reader") {
    dat$.plot_x <- dat$ReaderX
    dat$.plot_y <- -dat$ReaderY
    dat$.unit_id <- dat$ReaderLabel
    tile_h <- RFID_TILE_H
  } else {
    dat$.plot_x <- dat$ColumnX
    dat$.plot_y <- -58
    dat$.unit_id <- dat$ColumnLabel
    tile_h <- 116 + RFID_TILE_H
  }

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .plot_x, y = .plot_y))
  if (is.null(outlines)) {
    p <- p + ggplot2::annotate("rect", xmin = RFID_CAGE_X[1], xmax = RFID_CAGE_X[2],
                               ymin = RFID_CAGE_Y[1], ymax = RFID_CAGE_Y[2],
                               fill = NA, colour = "grey35", linewidth = 0.4)
  } else {
    p <- p + ggplot2::geom_rect(data = outlines, inherit.aes = FALSE,
                                ggplot2::aes(colour = outline),
                                xmin = RFID_CAGE_X[1], xmax = RFID_CAGE_X[2],
                                ymin = RFID_CAGE_Y[1], ymax = RFID_CAGE_Y[2],
                                fill = NA, linewidth = 0.55)
  }
  p <- p + ggplot2::geom_tile(ggplot2::aes(fill = .data[[fill_col]]), width = RFID_TILE_W, height = tile_h,
                              colour = tile_border, linewidth = 0.25)
  if (isTRUE(reader_ids)) {
    # Same ink as the tile value, so the name stays legible on dark fills.
    dat$.id_ink <- if (is.null(ink_col)) "black" else dat[[ink_col]]
    p <- p + ggplot2::geom_text(data = dat,
                                ggplot2::aes(x = .plot_x - RFID_TILE_W / 2 + 6, y = .plot_y + tile_h / 2 - 6,
                                             label = .unit_id, colour = .id_ink),
                                hjust = 0, vjust = 1, size = 4.5 / ggplot2::.pt, alpha = 0.75)
  }
  if (!is.null(label_col)) {
    if (is.null(ink_col)) {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = .data[[label_col]]), size = label_pt / ggplot2::.pt,
                                  colour = "black")
    } else {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = .data[[label_col]], colour = .data[[ink_col]]),
                                  size = label_pt / ggplot2::.pt)
    }
  }
  # Outline and ink colours are literal colour values, so one identity scale serves them all.
  if (!is.null(outlines) || isTRUE(reader_ids) || (!is.null(label_col) && !is.null(ink_col))) {
    p <- p + ggplot2::scale_colour_identity()
  }
  if (!is.null(notes)) {
    note_y <- if (note_side == "bottom") RFID_CAGE_Y[1] - RFID_NOTE_ROOM / 2 else RFID_CAGE_Y[2] + RFID_NOTE_ROOM / 2
    note_x <- if (note_side == "bottom") RFID_CAGE_X[2] else RFID_CAGE_X[1]
    p <- p + ggplot2::geom_text(data = notes, inherit.aes = FALSE,
                                ggplot2::aes(label = note), x = note_x, y = note_y,
                                hjust = if (note_side == "bottom") 1 else 0, vjust = 0.5,
                                size = note_pt / ggplot2::.pt, colour = "grey20")
  }
  p +
    ggplot2::coord_fixed(xlim = RFID_CAGE_X + c(-6, 6), ylim = y_lim, expand = FALSE, clip = "off") +
    rfid_cage_map_theme()
}

# Device size (inches) that fits n_row x n_col cage panels of a facet_grid without blank bands: the panel height
# follows the fixed aspect of the cage. Column strips are drawn once, above the top row. `extra_w` covers the row
# strips and the legend; `head_h` the title and subtitle; each caption line takes `line_h`.
rfid_cage_map_size <- function(n_col, n_row, panel_w = 1.6, notes = TRUE, extra_w = 0.9, head_h = 0.42,
                               strip_h = 0.17, caption_lines = 0, line_h = 0.1, space_x = 4 / 25.4,
                               space_y = 2.5 / 25.4) {
  y_span <- diff(RFID_CAGE_Y) + 12 + if (isTRUE(notes)) RFID_NOTE_ROOM else 0
  x_span <- diff(RFID_CAGE_X) + 12
  panel_h <- panel_w * y_span / x_span
  c(width = n_col * panel_w + (n_col - 1) * space_x + extra_w,
    height = head_h + strip_h + n_row * panel_h + (n_row - 1) * space_y + caption_lines * line_h + 0.14)
}

# Wraps a caption to the figure width (about 23 characters per inch at the 5.5-pt caption size).
rfid_wrap_caption <- function(text, width_in, chars_per_in = 23) {
  paste(strwrap(text, width = floor((width_in - 0.15) * chars_per_in)), collapse = "\n")
}

# Size and wrapped caption of a cage-map figure together: the caption's line count sets part of the height.
rfid_cage_map_layout <- function(caption, n_col, n_row, ...) {
  size <- rfid_cage_map_size(n_col, n_row, ...)
  wrapped <- rfid_wrap_caption(caption, size[["width"]])
  n_lines <- length(strsplit(wrapped, "\n", fixed = TRUE)[[1]])
  list(size = rfid_cage_map_size(n_col, n_row, caption_lines = n_lines, ...), caption = wrapped)
}
