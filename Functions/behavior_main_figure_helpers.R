# ================================================================
# Panel builders for the behavior main figure
# MMMSociability
# ================================================================
# Pure presentation layer. Every function here takes an ALREADY LOADED canonical
# data frame and returns a ggplot object or a small descriptive summary of the
# points it is about to draw.
#
# HARD RULES, enforced by Testing/tests/test_behavior_main_figure_contracts.R:
#   * no file is read or written here;
#   * no statistical model is fitted, no p-value or q-value is computed or
#     adjusted, no confidence interval is estimated from data;
#   * the only arithmetic permitted is descriptive summarisation of the values
#     that are being plotted (median, quartiles, mean, count) and unit
#     relabelling. Every such summary is returned in a tidy frame so it can be
#     written to Source Data and checked against the canonical tables.
#
# Anything that looks like inference must already exist in an upstream canonical
# table and be passed in as a pre-computed annotation string or column.
# ================================================================

suppressPackageStartupMessages({
  library(ggplot2); library(dplyr); library(tibble)
})

if (!exists("theme_mmm_pub", inherits = TRUE)) {
  if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
    source_mmm_helper("mmm_publication_theme.R")
  } else {
    stop("behavior_main_figure_helpers.R requires mmm_publication_theme.R",
         call. = FALSE)
  }
}

# ------------------------------------------------------------------ contracts

#' Minimal schema contract for a canonical input.
#'
#' Deliberately order-independent and filename-independent: a future restructure
#' may rename directories and reorder columns, and neither should break the
#' assembler. What must not change silently is the SEMANTICS -- which columns
#' exist, what identifies a row, and which label vocabularies are allowed.
#'
#' @param df data frame to validate
#' @param label human-readable name used in error messages
#' @param required character vector of required column names
#' @param keys columns that must jointly identify a row uniquely
#' @param allowed named list: column name -> permitted value set
#' @param finite numeric columns that must be entirely finite
#' @param n_rows optional exact expected row count
bmf_assert_schema <- function(df, label, required = character(), keys = NULL,
                              allowed = list(), finite = character(),
                              n_rows = NULL) {
  if (!is.data.frame(df)) {
    stop("Contract violation [", label, "]: not a data frame.", call. = FALSE)
  }
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0L) {
    stop("Contract violation [", label, "]: missing required column(s): ",
         paste(missing_cols, collapse = ", "),
         "\nPresent columns: ", paste(names(df), collapse = ", "), call. = FALSE)
  }
  if (!is.null(n_rows) && nrow(df) != n_rows) {
    stop("Contract violation [", label, "]: expected ", n_rows, " rows, got ",
         nrow(df), ".", call. = FALSE)
  }
  if (!is.null(keys)) {
    key_missing <- setdiff(keys, names(df))
    if (length(key_missing) > 0L) {
      stop("Contract violation [", label, "]: key column(s) absent: ",
           paste(key_missing, collapse = ", "), call. = FALSE)
    }
    dup <- duplicated(df[, keys, drop = FALSE])
    if (any(dup)) {
      stop("Contract violation [", label, "]: key (",
           paste(keys, collapse = " + "), ") is not unique; ", sum(dup),
           " duplicated row(s).", call. = FALSE)
    }
  }
  for (col in names(allowed)) {
    if (!col %in% names(df)) {
      stop("Contract violation [", label, "]: vocabulary column '", col,
           "' absent.", call. = FALSE)
    }
    seen <- unique(as.character(df[[col]]))
    seen <- seen[!is.na(seen)]
    extra <- setdiff(seen, allowed[[col]])
    if (length(extra) > 0L) {
      stop("Contract violation [", label, "]: column '", col,
           "' contains unexpected value(s): ", paste(extra, collapse = ", "),
           "\nPermitted: ", paste(allowed[[col]], collapse = ", "), call. = FALSE)
    }
  }
  for (col in finite) {
    if (!col %in% names(df)) {
      stop("Contract violation [", label, "]: finite-check column '", col,
           "' absent.", call. = FALSE)
    }
    bad <- !is.finite(suppressWarnings(as.numeric(df[[col]])))
    if (any(bad)) {
      stop("Contract violation [", label, "]: column '", col, "' has ",
           sum(bad), " non-finite value(s).", call. = FALSE)
    }
  }
  invisible(TRUE)
}

BMF_SEX_LEVELS <- c("Female", "Male")
BMF_GROUP_LEVELS <- MMM_GROUP_LEVELS
BMF_CONTRAST_LEVELS <- c("RES-CON", "SUS-CON", "SUS-RES")

#' Put Group and Sex on their canonical display order without changing values.
bmf_order_factors <- function(df) {
  if ("Group" %in% names(df)) {
    df$Group <- factor(as.character(df$Group), levels = BMF_GROUP_LEVELS)
  }
  if ("Sex" %in% names(df)) {
    df$Sex <- factor(as.character(df$Sex), levels = BMF_SEX_LEVELS)
  }
  if ("contrast" %in% names(df)) {
    df$contrast <- factor(as.character(df$contrast), levels = BMF_CONTRAST_LEVELS)
  }
  df
}

#' Descriptive location and spread of the values that are being plotted.
#'
#' This is NOT an inferential interval. It summarises the displayed points and
#' nothing else, which is why it returns the n it was computed from and a fixed
#' `summary_type` string that the legend and the manifest both quote.
bmf_descriptive_summary <- function(df, value, by) {
  stopifnot(value %in% names(df), all(by %in% names(df)))
  out <- df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      n_animals = sum(is.finite(.data[[value]])),
      median = stats::median(.data[[value]], na.rm = TRUE),
      q25 = stats::quantile(.data[[value]], 0.25, na.rm = TRUE, names = FALSE),
      q75 = stats::quantile(.data[[value]], 0.75, na.rm = TRUE, names = FALSE),
      mean = mean(.data[[value]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      summary_type = "descriptive median and interquartile range of the plotted animal values",
      value_column = value
    )
  bmf_order_factors(as.data.frame(out))
}

# ------------------------------------------------------- panel A: schematic
#
# A scientific schematic, not an infographic: plain boxes, plain arrows, one
# left-to-right time axis. Group identity colour appears ONLY in the final
# classification box, because at the time of the RFID measurement the later
# phenotype label does not exist.

.bmf_box <- function(id, x, y, w, h, label, role) {
  data.frame(id = id, x = x, y = y, w = w, h = h, label = label, role = role,
             stringsAsFactors = FALSE)
}

#' Panel A schematic: temporal order, CombZ construction, later labelling.
#'
#' @param components character vector of the CombZ component names, in the order
#'   they appear in the canonical endpoint source. Passed in rather than hard
#'   coded so the schematic can never drift from the source table.
#' @param combine_rule short string describing the verified combination rule
#' @param window_label short string describing the first-night window identity
#' @param external_note wording for the steps that are defined OUTSIDE this
#'   repository. Must not be empty while that remains true.
bmf_panel_a_schematic <- function(components,
                                  combine_rule,
                                  window_label,
                                  external_note,
                                  n_animals,
                                  group_counts,
                                  n_comp_cols = 3L) {
  stopifnot(length(components) >= 1L, nzchar(combine_rule), nzchar(external_note))
  ink <- MMM_SCHEMATIC_INK
  txt <- (MMM_BASE_PT - 2.1) / .pt
  txt_small <- (MMM_BASE_PT - 2.8) / .pt

  # Coordinates are a plain 0-10 x 0-6 drawing grid, so box geometry can be read
  # off the numbers below rather than inferred from ggplot defaults.
  early <- .bmf_box("early", 2.15, 5.05, 4.05, 1.45,
                    paste0("EARLIER\nRFID home-cage recording\n", window_label),
                    "stage")
  later <- .bmf_box("later", 7.35, 5.05, 4.05, 1.45,
                    paste0("LATER\nendpoint battery and physiology\n",
                           "(n = ", n_animals, " animals)"),
                    "stage")

  # --- component grid, sized to the number of components ------------------
  n_comp <- length(components)
  n_rows <- ceiling(n_comp / n_comp_cols)
  box_w <- 1.62; box_h <- 0.82
  col_x <- 0.95 + (seq_len(n_comp_cols) - 1L) * (box_w + 0.16)
  row_y <- 3.30 - (seq_len(n_rows) - 1L) * (box_h + 0.16)
  comps <- do.call(rbind, lapply(seq_len(n_comp), function(i) {
    cc <- ((i - 1L) %% n_comp_cols) + 1L
    rr <- ((i - 1L) %/% n_comp_cols) + 1L
    .bmf_box(paste0("comp", i), col_x[cc], row_y[rr], box_w, box_h,
             components[i], "assay")
  }))
  comp_right <- max(comps$x) + box_w / 2
  comp_mid_y <- mean(range(comps$y))

  combz <- .bmf_box("combz", 6.88, comp_mid_y, 2.60, 1.45,
                    paste0("CombZ\n", combine_rule), "stage")
  label_box <- .bmf_box("label", 9.15, comp_mid_y, 1.58, 1.45,
                        "later\noutcome group", "identity")

  boxes <- rbind(early, later, comps, combz, label_box)
  boxes$xmin <- boxes$x - boxes$w / 2; boxes$xmax <- boxes$x + boxes$w / 2
  boxes$ymin <- boxes$y - boxes$h / 2; boxes$ymax <- boxes$y + boxes$h / 2
  boxes$fill <- ifelse(boxes$role == "assay", ink[["box_fill"]], ink[["stage_fill"]])
  boxes$border <- ifelse(boxes$role == "assay", ink[["box_border"]],
                         ink[["stage_border"]])
  boxes$size <- ifelse(boxes$role == "assay", txt_small, txt)

  arrows <- rbind(
    # the time arrow: the single most important statement in this panel
    data.frame(x = early$xmax <- early$x + early$w / 2,
               xend = later$x - later$w / 2, y = early$y, yend = early$y),
    # the endpoint battery feeds the components
    data.frame(x = later$x - 1.4, xend = max(col_x) - 0.2,
               y = later$y - later$h / 2, yend = max(row_y) + box_h / 2 + 0.06),
    # components -> composite -> later label
    data.frame(x = comp_right + 0.10, xend = combz$x - combz$w / 2 - 0.04,
               y = comp_mid_y, yend = comp_mid_y),
    data.frame(x = combz$x + combz$w / 2, xend = label_box$x - label_box$w / 2 - 0.04,
               y = comp_mid_y, yend = comp_mid_y))

  # Group identity chips: the ONLY place colour appears in this schematic,
  # placed under the label box so no assay name is ever drawn in a group colour.
  chips <- data.frame(
    Group = factor(BMF_GROUP_LEVELS, levels = BMF_GROUP_LEVELS),
    x = label_box$x + c(-0.55, 0, 0.55),
    y = label_box$y - label_box$h / 2 - 0.34,
    n = as.integer(group_counts[BMF_GROUP_LEVELS]),
    stringsAsFactors = FALSE)
  chips$chip_label <- paste0(chips$Group, "\nn=", chips$n)

  ggplot() +
    geom_segment(data = arrows,
                 aes(x = .data$x, xend = .data$xend, y = .data$y, yend = .data$yend),
                 linewidth = 0.33, colour = ink[["arrow"]],
                 arrow = grid::arrow(length = unit(1.3, "mm"), type = "closed")) +
    geom_rect(data = boxes,
              aes(xmin = .data$xmin, xmax = .data$xmax, ymin = .data$ymin,
                  ymax = .data$ymax),
              fill = boxes$fill, colour = boxes$border, linewidth = 0.28) +
    geom_text(data = boxes,
              aes(x = .data$x, y = .data$y, label = .data$label),
              size = boxes$size, colour = ink[["text"]], lineheight = 1.04) +
    geom_point(data = chips,
               aes(x = .data$x, y = .data$y, fill = .data$Group,
                   shape = .data$Group),
               size = 1.7, colour = "black", stroke = 0.3, show.legend = FALSE) +
    geom_text(data = chips,
              aes(x = .data$x, y = .data$y - 0.46, label = .data$chip_label),
              size = txt_small, colour = ink[["text"]], lineheight = 1.0) +
    annotate("text", x = (early$x + later$x) / 2, y = early$y + 0.92,
             label = "time", hjust = 0.5, size = txt_small,
             colour = ink[["arrow"]]) +
    annotate("text", x = 0.12, y = 1.42, hjust = 0, vjust = 1,
             label = external_note, size = txt_small,
             colour = ink[["caveat"]], lineheight = 1.10) +
    mmm_scale_fill_group() + mmm_scale_shape_group() +
    scale_x_continuous(limits = c(0, 10.0), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 6.0), expand = c(0, 0)) +
    theme_mmm_schematic()
}

#' Small CombZ distribution strip that grounds the panel A schematic in data.
bmf_panel_a_distribution <- function(animal_df, summary_df,
                                     y_lab = "CombZ") {
  bmf_assert_schema(animal_df, "panel A distribution",
                    required = c("AnimalID", "Sex", "Group", "CombZ"),
                    keys = "AnimalID",
                    allowed = list(Group = BMF_GROUP_LEVELS, Sex = BMF_SEX_LEVELS),
                    finite = "CombZ")
  animal_df <- bmf_order_factors(animal_df)
  summary_df <- bmf_order_factors(summary_df)

  ggplot(animal_df, aes(x = .data$Group, y = .data$CombZ)) +
    geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey75") +
    geom_linerange(data = summary_df,
                   aes(x = .data$Group, ymin = .data$q25, ymax = .data$q75),
                   inherit.aes = FALSE, linewidth = 0.45,
                   colour = MMM_CONTRAST_COLOUR) +
    geom_point(aes(fill = .data$Group, shape = .data$Group),
               position = position_jitter(width = 0.16, height = 0, seed = 27L),
               size = 0.72, colour = "grey25", stroke = 0.18, alpha = 0.9,
               show.legend = FALSE) +
    geom_point(data = summary_df,
               aes(x = .data$Group, y = .data$median),
               inherit.aes = FALSE, shape = 95L, size = 3.4,
               colour = MMM_CONTRAST_COLOUR) +
    mmm_scale_fill_group() + mmm_scale_shape_group() +
    facet_wrap(~Sex) +
    labs(x = NULL, y = y_lab) +
    theme_mmm_pub()
}

# ------------------------------------------------- panel B: domain heatmap

#' Compact domain-level effect-size heatmap.
#'
#' Fill encodes the standardized contrast only. Statistical support is shown by
#' a single thin outline on cells that pass the DECLARED family threshold, which
#' is carried in from the canonical table rather than recomputed, and which is
#' named in the legend. No stars, no per-cell numbers.
bmf_panel_b_domain_heatmap <- function(contrast_df,
                                       fdr_threshold = 0.05,
                                       effect_col = "hedges_g",
                                       domain_order = NULL,
                                       fill_limits = NULL,
                                       key_cols = c("Sex", "contrast", "Domain"),
                                       q_col = "q") {
  df0 <- contrast_df
  if (!identical(q_col, "q")) df0$q <- df0[[q_col]]
  bmf_assert_schema(df0, "panel B domain contrasts",
                    required = unique(c(key_cols, effect_col, "q")),
                    keys = key_cols,
                    allowed = list(Sex = BMF_SEX_LEVELS,
                                   contrast = BMF_CONTRAST_LEVELS),
                    finite = effect_col)
  contrast_df <- df0
  df <- bmf_order_factors(contrast_df)
  if (is.null(domain_order)) {
    domain_order <- df %>%
      dplyr::group_by(.data$Domain) %>%
      dplyr::summarise(m = max(abs(.data[[effect_col]]), na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(.data$m) %>% dplyr::pull(.data$Domain)
  }
  df$Domain <- factor(as.character(df$Domain), levels = domain_order)
  df$effect <- df[[effect_col]]
  df$supported <- !is.na(df$q) & df$q < fdr_threshold

  if (is.null(fill_limits)) {
    fill_limits <- c(-1, 1) * max(abs(df$effect), na.rm = TRUE)
  }

  ggplot(df, aes(x = .data$contrast, y = .data$Domain, fill = .data$effect)) +
    geom_tile(colour = "white", linewidth = 0.7) +
    geom_tile(data = dplyr::filter(df, .data$supported),
              colour = "black", linewidth = 0.5, fill = NA) +
    mmm_scale_fill_effect(
      limits = fill_limits, name = "Hedges g",
      breaks = scales::pretty_breaks(3),
      guide = guide_colourbar(title.position = "top", title.hjust = 0,
                              ticks.colour = "grey30", frame.colour = "grey30",
                              frame.linewidth = 0.2)) +
    facet_wrap(~Sex) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    labs(x = NULL, y = NULL) +
    theme_mmm_pub() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.justification = "right",
          legend.title = element_text(size = MMM_BASE_PT - 1.5),
          legend.text = element_text(size = MMM_BASE_PT - 1.8),
          legend.key.width = unit(MMM_BASE_PT * 2.2, "pt"),
          legend.key.height = unit(MMM_BASE_PT * 0.5, "pt"),
          legend.box.margin = margin(-2, 0, 0, 0),
          axis.line = element_blank(), axis.ticks = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1,
                                     size = MMM_BASE_PT - 1.6),
          axis.text.y = element_text(size = MMM_BASE_PT - 1.6))
}

# ----------------------------------------- panel C: first-night movement

#' Animal-level first active-phase Movement by later group, faceted by sex.
#'
#' Every animal stays visible. The overlaid marker is a descriptive median with
#' the interquartile range of exactly those points, supplied by the caller from
#' bmf_descriptive_summary() so the plotted summary and the Source Data summary
#' are the same object.
bmf_panel_c_first_night <- function(animal_df, summary_df, y_lab,
                                    jitter_seed = 27L) {
  bmf_assert_schema(animal_df, "panel C animal movement",
                    required = c("AnimalID", "Sex", "Group", "Movement_mean"),
                    keys = "AnimalID",
                    allowed = list(Group = BMF_GROUP_LEVELS, Sex = BMF_SEX_LEVELS),
                    finite = "Movement_mean")
  bmf_assert_schema(summary_df, "panel C descriptive summary",
                    required = c("Sex", "Group", "median", "q25", "q75", "n_animals"),
                    keys = c("Sex", "Group"))
  animal_df <- bmf_order_factors(animal_df)
  summary_df <- bmf_order_factors(summary_df)

  ggplot(animal_df, aes(x = .data$Group, y = .data$Movement_mean)) +
    geom_linerange(data = summary_df,
                   aes(x = .data$Group, ymin = .data$q25, ymax = .data$q75),
                   inherit.aes = FALSE, linewidth = 0.5,
                   colour = MMM_CONTRAST_COLOUR) +
    geom_point(aes(fill = .data$Group, shape = .data$Group),
               position = position_jitter(width = 0.17, height = 0,
                                          seed = jitter_seed),
               size = 1.0, colour = "grey20", stroke = 0.2, alpha = 0.9) +
    geom_point(data = summary_df,
               aes(x = .data$Group, y = .data$median),
               inherit.aes = FALSE, shape = 95L, size = 4.2,
               colour = MMM_CONTRAST_COLOUR) +
    mmm_scale_fill_group() + mmm_scale_shape_group() +
    facet_wrap(~Sex) +
    labs(x = NULL, y = y_lab) +
    theme_mmm_pub() +
    theme(legend.position = "none")
}

# ----------------------------------- panel D: early movement vs later CombZ

#' Descriptive quantile-bin trend of the plotted points.
#'
#' The canonical inferential model is a Spearman rank correlation, so no
#' least-squares line may be drawn. This returns the median y within equal-count
#' bins of x -- a monotone-friendly, purely descriptive summary of the same
#' points, which is labelled as such wherever it is drawn.
bmf_quantile_trend <- function(df, x, y, n_bins = 5L) {
  stopifnot(x %in% names(df), y %in% names(df))
  keep <- is.finite(df[[x]]) & is.finite(df[[y]])
  d <- df[keep, c(x, y), drop = FALSE]
  breaks <- stats::quantile(d[[x]], probs = seq(0, 1, length.out = n_bins + 1L),
                            na.rm = TRUE, names = FALSE)
  breaks[1] <- -Inf; breaks[length(breaks)] <- Inf
  d$bin <- cut(d[[x]], breaks = unique(breaks), include.lowest = TRUE,
               labels = FALSE)
  out <- d %>%
    dplyr::group_by(.data$bin) %>%
    dplyr::summarise(x_median = stats::median(.data[[x]]),
                     y_median = stats::median(.data[[y]]),
                     n_animals = dplyr::n(), .groups = "drop") %>%
    dplyr::arrange(.data$x_median)
  out$trend_type <- paste0("descriptive median of y within ", n_bins,
                           " equal-count bins of x; not a fitted model")
  as.data.frame(out)
}

#' Early Movement versus later CombZ, one point per animal.
#'
#' Pooled across sex because the canonical Stage 09 association is pooled and
#' the formal feature-by-Sex interaction is null; sex is carried redundantly by
#' point shape so the reader can still see it. `annotation` is a pre-formatted
#' string built from canonical numbers -- nothing statistical is computed here.
bmf_panel_d_scatter <- function(animal_df, trend_df, annotation,
                                x_lab, y_lab, show_trend = TRUE) {
  bmf_assert_schema(animal_df, "panel D animal association",
                    required = c("AnimalID", "Sex", "Group", "Movement_mean",
                                 "CombZ"),
                    keys = "AnimalID",
                    allowed = list(Group = BMF_GROUP_LEVELS, Sex = BMF_SEX_LEVELS),
                    finite = c("Movement_mean", "CombZ"))
  animal_df <- bmf_order_factors(animal_df)

  p <- ggplot(animal_df, aes(x = .data$Movement_mean, y = .data$CombZ)) +
    geom_hline(yintercept = 0, linewidth = 0.25, colour = "grey80")
  if (isTRUE(show_trend)) {
    p <- p + geom_line(data = trend_df,
                       aes(x = .data$x_median, y = .data$y_median),
                       inherit.aes = FALSE, linewidth = 0.45,
                       linetype = "22", colour = MMM_CONTRAST_COLOUR) +
      geom_point(data = trend_df,
                 aes(x = .data$x_median, y = .data$y_median),
                 inherit.aes = FALSE, size = 0.9,
                 colour = MMM_CONTRAST_COLOUR)
  }
  p +
    geom_point(aes(fill = .data$Group, shape = .data$Group),
               size = 1.15, colour = "grey20", stroke = 0.2, alpha = 0.92) +
    mmm_scale_fill_group() + mmm_scale_shape_group() +
    annotate("text", x = -Inf, y = -Inf, hjust = -0.05, vjust = -0.45,
             label = annotation, size = (MMM_BASE_PT - 1.6) / .pt,
             colour = MMM_CONTRAST_COLOUR, lineheight = 1.05) +
    labs(x = x_lab, y = y_lab) +
    theme_mmm_pub() +
    theme(legend.position = "top")
}

#' Rank-space companion for panel D.
#'
#' Spearman rho IS the Pearson correlation of the ranks, so the only line whose
#' slope is licensed by the canonical model is the one with slope rho through the
#' rank centroid. It is drawn analytically from the canonical rho -- no
#' regression is fitted.
bmf_panel_d_rank_space <- function(animal_df, rho, annotation, x_lab, y_lab) {
  stopifnot(is.finite(rho))
  d <- animal_df
  d$rank_x <- rank(d$Movement_mean, ties.method = "average")
  d$rank_y <- rank(d$CombZ, ties.method = "average")
  d <- bmf_order_factors(d)
  centre <- c(mean(d$rank_x), mean(d$rank_y))
  ggplot(d, aes(x = .data$rank_x, y = .data$rank_y)) +
    geom_abline(slope = rho, intercept = centre[2] - rho * centre[1],
                linewidth = 0.45, colour = MMM_CONTRAST_COLOUR,
                linetype = "solid") +
    geom_point(aes(fill = .data$Group, shape = .data$Group),
               size = 1.05, colour = "grey20", stroke = 0.2, alpha = 0.92,
               show.legend = FALSE) +
    mmm_scale_fill_group() + mmm_scale_shape_group() +
    annotate("text", x = -Inf, y = -Inf, hjust = -0.06, vjust = -0.5,
             label = annotation, size = (MMM_BASE_PT - 1.6) / .pt,
             colour = MMM_CONTRAST_COLOUR, lineheight = 1.05) +
    labs(x = x_lab, y = y_lab) +
    theme_mmm_pub()
}

# -------------------------------------- panel E: out-of-sample prediction

#' Held-out observed versus predicted CombZ.
#'
#' Every predicted value must have been produced with that animal absent from
#' the training data; the caller asserts that from the canonical validation
#' scheme label, and this function refuses to plot a frame whose scheme column
#' does not say so.
bmf_panel_e_heldout <- function(pred_df, scheme_pattern = "one-animal-out",
                                x_lab, y_lab, annotation = NULL) {
  bmf_assert_schema(pred_df, "panel E held-out predictions",
                    required = c("AnimalID", "observed_CombZ", "predicted_CombZ",
                                 "model_id", "validation_scheme", "Sex", "Group"),
                    keys = "AnimalID",
                    allowed = list(Group = BMF_GROUP_LEVELS, Sex = BMF_SEX_LEVELS),
                    finite = c("observed_CombZ", "predicted_CombZ"))
  schemes <- unique(as.character(pred_df$validation_scheme))
  bad <- schemes[!grepl(scheme_pattern, schemes, ignore.case = TRUE)]
  if (length(bad) > 0L) {
    stop("Panel E refuses to plot predictions whose validation scheme is not ",
         "held out. Offending scheme(s): ", paste(bad, collapse = "; "),
         call. = FALSE)
  }
  d <- bmf_order_factors(pred_df)
  lim <- range(c(d$observed_CombZ, d$predicted_CombZ), finite = TRUE)

  p <- ggplot(d, aes(x = .data$observed_CombZ, y = .data$predicted_CombZ)) +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.3,
                colour = "grey65", linetype = "22") +
    geom_point(aes(fill = .data$Group, shape = .data$Group),
               size = 1.15, colour = "grey20", stroke = 0.2, alpha = 0.92,
               show.legend = FALSE) +
    mmm_scale_fill_group() + mmm_scale_shape_group() +
    coord_fixed(xlim = lim, ylim = lim) +
    labs(x = x_lab, y = y_lab) +
    theme_mmm_pub()
  if (!is.null(annotation)) {
    p <- p + annotate("text", x = -Inf, y = Inf, hjust = -0.05, vjust = 1.3,
                      label = annotation, size = (MMM_BASE_PT - 1.6) / .pt,
                      colour = MMM_CONTRAST_COLOUR, lineheight = 1.05)
  }
  p
}

#' Observed cross-validated performance against its permutation reference.
#'
#' Everything drawn here is read from the canonical performance and permutation
#' tables: the observed statistic, the permutation null median and its 2.5-97.5%
#' quantiles, and the repeated-CV resampling interval. The null distribution
#' itself is not exported by the producing stage, so the null is drawn as its
#' published quantile interval rather than as a density; the legend says so.
bmf_panel_e_performance <- function(perf_df, baseline = NULL,
                                    x_lab = "Out-of-sample R2",
                                    annotation = NULL,
                                    label_col = "model_label") {
  bmf_assert_schema(perf_df, "panel E performance",
                    required = unique(c(label_col, "model_label",
                                 "observed_statistic",
                                 "null_median", "null_q025", "null_q975",
                                 "cv_r2_q025", "cv_r2_q975", "empirical_p",
                                 "n_permutations", "reporting_role")),
                    keys = label_col,
                    finite = c("observed_statistic", "null_median",
                               "null_q025", "null_q975"))
  d <- perf_df
  d$.row_label <- factor(as.character(d[[label_col]]),
                         levels = rev(as.character(d[[label_col]])))

  p <- ggplot(d, aes(y = .data$.row_label))
  if (!is.null(baseline) && is.finite(baseline)) {
    p <- p + geom_vline(xintercept = baseline, linewidth = 0.3,
                        colour = "grey70", linetype = "22")
  }
  p +
    geom_vline(xintercept = 0, linewidth = 0.25, colour = "grey85") +
    geom_linerange(aes(xmin = .data$null_q025, xmax = .data$null_q975),
                   linewidth = 2.6, colour = "grey82") +
    geom_point(aes(x = .data$null_median), shape = 124L, size = 2.6,
               colour = "grey45") +
    geom_linerange(aes(xmin = .data$cv_r2_q025, xmax = .data$cv_r2_q975),
                   linewidth = 0.5, colour = MMM_CONTRAST_COLOUR) +
    geom_point(aes(x = .data$observed_statistic), size = 1.9, shape = 21L,
               fill = MMM_GROUP_COLOURS[["SUS"]], colour = "black",
               stroke = 0.35) +
    labs(x = x_lab, y = NULL, caption = annotation) +
    theme_mmm_pub() +
    theme(legend.position = "none",
          plot.caption = element_text(size = MMM_BASE_PT - 2, hjust = 0,
                                      colour = "grey30", lineheight = 1.08),
          axis.text.y = element_text(hjust = 1, size = MMM_BASE_PT - 1.4))
}

# ------------------------------------------------------------------ export

#' Standard Source Data frame for one behavior-figure panel.
#'
#' Mirrors mmm_source_data() but keeps the panel-id vocabulary of this figure and
#' records the producing stage and the canonical table the values came from, so
#' a Source Data file always names its own provenance.
bmf_source_data <- function(df, panel_id, canonical_stage, canonical_table,
                            cols) {
  keep <- intersect(cols, names(df))
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0L) {
    stop("Source Data for panel ", panel_id, " is missing column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  out <- df[, keep, drop = FALSE]
  tibble::as_tibble(cbind(
    data.frame(panel_id = panel_id, canonical_stage = canonical_stage,
               canonical_table = canonical_table, stringsAsFactors = FALSE),
    out
  ))
}
