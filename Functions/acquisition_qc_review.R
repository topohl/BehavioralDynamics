# RFID acquisition QC review figures (house style) and eight-reader spatial-overlap summaries.
# Pure definitions. New, unregistered, group-blind downstream analysis of saved tables.
# No raw reconstruction, model fit, group contrast, manuscript import or activation.
# Needs data.table; plot functions need ggplot2, patchwork and the manuscript palette.

AQR_VERSION <- "aqr_v0.1"
AQR_READER_ORDER <- c(5L, 6L, 7L, 8L, 1L, 2L, 3L, 4L)
aqr_colours <- function() c(Event = unname(MMM_PALETTE_DIVERGING[["low"]]), Read = "#202020",
                            Active = unname(MMM_PALETTE_DIVERGING[["low"]]), Light = "#A47A22")

aqr_require <- function(d, cols, label) {
  miss <- setdiff(cols, names(d))
  if (length(miss)) stop(label, " lacks: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# Validate and retain only the eight-reader fields of the saved measurement audit.
# Masks are explicit inputs from the released manifest. Each row is an unordered
# dyad x first active/following light window x method; no column field is consumed.
aqr_overlap <- function(dyads, animals, blocks, manifest) {
  d <- data.table::copy(data.table::as.data.table(dyads))
  cols <- c("SourceFile", "Batch", "CC", "window", "System", "A", "B", "basis", "tau", "delta", "joint_s", "same_s",
            "indep_same", "short_null_same_mean", "long_null_same_mean")
  aqr_require(d, cols, "Saved dyads")
  d <- d[tau == 2 & basis %in% c("event_start", "read") & (is.infinite(delta) | delta == 60), ..cols]
  if (!nrow(d)) stop("No declared eight-reader method arms.", call. = FALSE)
  if (anyNA(d) || any(!d$window %in% c("active", "light")) || any(d$A == d$B) ||
      any(!is.finite(d$joint_s) | d$joint_s <= 0 | d$joint_s > 43200) ||
      any(!is.finite(d$same_s) | d$same_s < 0 | d$same_s > d$joint_s)) stop("Invalid dyad identity, window or seconds.", call. = FALSE)
  for (k in c("indep_same", "short_null_same_mean", "long_null_same_mean"))
    if (any(!is.finite(d[[k]]) | d[[k]] < 0 | d[[k]] > 1)) stop("Invalid reference probability: ", k, call. = FALSE)
  d[, `:=`(A = as.character(A), B = as.character(B), pair_lo = pmin(as.character(A), as.character(B)),
           pair_hi = pmax(as.character(A), as.character(B)))]
  key <- c("SourceFile", "System", "window", "pair_lo", "pair_hi", "basis", "delta")
  if (anyDuplicated(d[, ..key])) stop("Duplicate unordered dyad/method rows.", call. = FALSE)
  a <- data.table::as.data.table(animals)
  aqr_require(a, c("SourceFile", "AnimalNum", "Batch", "CC", "System", "CageEpisodeID"), "Srs roster")
  if (anyDuplicated(a[, .(SourceFile, AnimalNum)])) stop("Ambiguous srs roster.", call. = FALSE)
  for (who in c("A", "B")) {
    i <- match(paste(d$SourceFile, d[[who]]), paste(a$SourceFile, a$AnimalNum))
    if (anyNA(i) || any(d$Batch != a$Batch[i] | d$CC != a$CC[i] | d$System != a$System[i]))
      stop("Dyad member differs from the srs cage roster: ", who, call. = FALSE)
  }
  d[, CageEpisodeID := paste(Batch, System, CC, sep = "|")]
  m <- data.table::as.data.table(manifest)
  aqr_require(m, c("action", "CageEpisodeID", "from"), "Released manifest")
  d[, excluded := CageEpisodeID %in% m[action == "exclude_spatial"]$CageEpisodeID]
  audit <- d[, .(rows = .N, dyads = data.table::uniqueN(paste(pair_lo, pair_hi)), excluded = unique(excluded)), by = CageEpisodeID]
  d <- d[excluded == FALSE]
  if (!nrow(d)) stop("No retained dyad windows after the released exclusions.", call. = FALSE)
  # The legacy dyad table holds Stage-29 first-active/following-light windows only.
  # It has no timestamps for clipping: refuse, rather than guess, if a retained
  # cage's released censor time overlaps either window.
  bl <- data.table::as.data.table(blocks)
  aqr_require(bl, c("SourceFile", "Phase", "PhaseNumber", "be"), "Srs blocks")
  ends <- bl[(Phase == "Active" & PhaseNumber == 1) | (Phase == "Inactive" & PhaseNumber == 2),
             .(window = ifelse(Phase == "Active", "active", "light"), window_end = be), by = SourceFile]
  if (anyDuplicated(ends[, .(SourceFile, window)])) stop("Ambiguous first active/light block.", call. = FALSE)
  j <- match(paste(d$SourceFile, d$window), paste(ends$SourceFile, ends$window))
  if (anyNA(j)) stop("Missing first active/light window endpoint.", call. = FALSE)
  ce <- m[action == "censor"]
  ct <- as.numeric(as.POSIXct(ce$from, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"))
  if (anyNA(ct)) stop("Invalid released censor time.", call. = FALSE)
  ci <- match(d$CageEpisodeID, ce$CageEpisodeID)
  if (any(!is.na(ci) & ends$window_end[j] > ct[ci])) stop("Censor overlaps a retained dyad window; saved dyads cannot be clipped.", call. = FALSE)
  # All retained pair-window keys must carry the same four arms; do not fill a
  # missing method with zeros or compare different populations across methods.
  d[, arm := paste(basis, ifelse(is.infinite(delta), "LOCF", "max60s"), sep = "_")]
  arms <- c("event_start_LOCF", "event_start_max60s", "read_LOCF", "read_max60s")
  complete <- d[, .(complete = setequal(arm, arms) && .N == 4L), by = .(SourceFile, System, window, pair_lo, pair_hi)]
  if (any(!complete$complete)) stop("Missing or extra method arm for a dyad-window.", call. = FALSE)
  d[, `:=`(observed = same_s / joint_s, basis_label = ifelse(basis == "read", "Read", "Event"),
           support = ifelse(is.infinite(delta), "LOCF", "Both tags <=60 s"), Phase = ifelse(window == "active", "Active", "Light"))]
  d[, `:=`(excess_short = observed - short_null_same_mean, excess_long = observed - long_null_same_mean,
           excess_independence = observed - indep_same)]
  by <- c("SourceFile", "Batch", "CC", "System", "CageEpisodeID", "window", "Phase", "basis", "basis_label", "delta", "support")
  aggregate <- function(z, by) z[, .(n_dyads = .N, joint_s = sum(joint_s), same_s = sum(same_s),
    observed = sum(same_s) / sum(joint_s), expected_short = sum(short_null_same_mean * joint_s) / sum(joint_s),
    expected_long = sum(long_null_same_mean * joint_s) / sum(joint_s),
    expected_independence = sum(indep_same * joint_s) / sum(joint_s)), by = by]
  episodes <- aggregate(d, by)
  episodes[, `:=`(excess_short = observed - expected_short, excess_long = observed - expected_long,
                  excess_independence = observed - expected_independence)]
  # Animal rows are explicitly partner-weighted: a dyad appears once per member.
  long <- data.table::rbindlist(list(data.table::copy(d)[, AnimalNum := A], data.table::copy(d)[, AnimalNum := B]))
  animal <- aggregate(long, c(by, "AnimalNum"))
  animal[, excess_short := observed - expected_short]
  paired <- data.table::dcast(episodes, SourceFile + CageEpisodeID + Batch + CC + Phase + support ~ basis_label,
                             value.var = "excess_short")
  paired[, read_minus_event_pp := 100 * (Read - Event)]
  coverage <- data.table::dcast(d, SourceFile + CageEpisodeID + pair_lo + pair_hi + Phase + basis_label ~ support, value.var = "joint_s")
  coverage[, supported_pair_share := get("Both tags <=60 s") / LOCF]
  if (any(coverage$supported_pair_share < 0 | coverage$supported_pair_share > 1)) stop("Invalid supported pair share.", call. = FALSE)
  summary <- episodes[, .(n_cage_episodes = .N, n_batches = data.table::uniqueN(Batch), n_dyad_windows = sum(n_dyads),
    joint_s = sum(joint_s), observed_pooled = sum(same_s) / sum(joint_s),
    excess_short_median = median(excess_short), excess_short_q25 = unname(quantile(excess_short, .25)),
    excess_short_q75 = unname(quantile(excess_short, .75)), fraction_below_short_reference = mean(excess_short < 0)),
    by = .(Phase, basis_label, support)]
  list(dyads = d, animals = animal, episodes = episodes, paired = paired, coverage = coverage, summary = summary, exclusion_audit = audit)
}

aqr_theme <- function() ggplot2::theme_classic(base_size = 6.5, base_family = "Arial") +
  ggplot2::theme(plot.title = ggplot2::element_text(size = 7, face = "bold", margin = ggplot2::margin(b = 4)),
    axis.title = ggplot2::element_text(size = 6.5), axis.text = ggplot2::element_text(size = 5.5, colour = "#222222"),
    axis.line = ggplot2::element_line(linewidth = .25), axis.ticks = ggplot2::element_line(linewidth = .25),
    axis.ticks.length = grid::unit(1.2, "mm"), legend.title = ggplot2::element_text(size = 6),
    legend.text = ggplot2::element_text(size = 5.5), legend.key.size = grid::unit(2.4, "mm"),
    legend.position = "bottom", strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_text(size = 6.5, face = "bold"),
    panel.spacing = grid::unit(2.5, "mm"), plot.margin = ggplot2::margin(4, 4, 4, 4),
    plot.tag = ggplot2::element_text(size = 8, face = "bold"))

aqr_point_spread <- function(d, by, value) {
  d[, .(median = median(get(value)), q25 = unname(quantile(get(value), .25)), q75 = unname(quantile(get(value), .75))), by = by]
}

aqr_figures <- function(x, overlap) {
  th <- aqr_theme(); colours <- aqr_colours(); pc <- colours[c("Active", "Light")]
  sc <- function() ggplot2::scale_colour_manual(values = pc)
  rec_levels <- rev(as.vector(outer(paste0("B", 1:6), paste0("CC", 1:4), paste, sep = " ")))
  # Explicit batch-major ordering, rather than alphabetical sorting across facets.
  rec_levels <- rev(paste(rep(paste0("B", 1:6), each = 4), rep(paste0("CC", 1:4), 6)))
  health <- data.table::copy(x$reader)
  health[, `:=`(reader = factor(paste0("R", PositionID), levels = paste0("R", AQR_READER_ORDER)),
                recording = factor(paste(Batch, CC), levels = rec_levels), log_ratio = log2(session_best_ratio))]
  health[!is.finite(log_ratio), log_ratio := NA_real_]
  p1 <- ggplot2::ggplot(health, ggplot2::aes(reader, recording, fill = log_ratio)) +
    ggplot2::geom_tile(colour = MMM_TILE_BORDER, linewidth = .1) +
    ggplot2::geom_point(data = health[zero_read_session == TRUE], shape = 4, size = 1.5, stroke = .5) +
    ggplot2::facet_wrap(~board, nrow = 1, labeller = ggplot2::label_both) +
    mmm_scale_fill_diverging("own", limit = 6, na.value = "#D1D1D1", name = "Relative reader\nco-detection (log2)") +
    ggplot2::guides(fill = ggplot2::guide_colourbar(display = "rectangles", nbin = 64)) +
    ggplot2::labs(title = "Relative reader co-detection across all recordings", x = NULL, y = NULL) + th +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(), legend.position = "right")
  stat <- x$status[, .N, by = .(board, status)]
  stat[, status := factor(status, levels = c("PASS", "WARN", "FAIL"))]
  p2 <- ggplot2::ggplot(stat, ggplot2::aes(factor(board), N, fill = status)) + ggplot2::geom_col(width = .65, colour = "white", linewidth = .15) +
    ggplot2::geom_text(ggplot2::aes(label = N), position = ggplot2::position_stack(vjust = .5), size = 2) +
    ggplot2::scale_fill_manual(values = c(PASS = "#D5D5D5", WARN = unname(MMM_PALETTE_DIVERGING[["high"]]), FAIL = unname(MMM_PALETTE_GROUP[["SUS"]])), name = NULL, drop = FALSE) +
    ggplot2::scale_y_continuous(breaks = c(0, 12, 24), expand = ggplot2::expansion(mult = c(0, .03))) +
    ggplot2::labs(title = "QC status by board", x = "Board", y = "Cage episodes") + th
  cad <- x$cadence
  s <- aqr_point_spread(cad, "board", "mean_gap_ms")
  p3 <- ggplot2::ggplot(cad, ggplot2::aes(factor(board), mean_gap_ms)) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = .12, seed = 16), size = .7, alpha = .4) +
    ggplot2::geom_linerange(data = s, ggplot2::aes(y = median, ymin = q25, ymax = q75), linewidth = .55) +
    ggplot2::geom_point(data = s, ggplot2::aes(y = median), shape = 23, size = 1.6, fill = "white") +
    ggplot2::labs(title = "Acquisition cadence", x = "Board", y = "Within-event read gap (ms)") + th
  r <- data.table::copy(x$reader_spread)[, reader := factor(paste0("R", PositionID), levels = paste0("R", AQR_READER_ORDER))]
  r[, Phase := factor(ifelse(Phase == "Inactive", "Light", Phase), levels = c("Active", "Light"))]
  p4 <- ggplot2::ggplot(r, ggplot2::aes(reader, 100 * supported_le60_median, colour = Phase, group = Phase)) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = 100 * supported_le60_q25, ymax = 100 * supported_le60_q75), position = ggplot2::position_dodge(.35), linewidth = .4) +
    ggplot2::geom_point(ggplot2::aes(shape = Phase), position = ggplot2::position_dodge(.35), size = 1.2) + sc() +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
    ggplot2::labs(title = "Recent detection by reader", x = NULL, y = "Time with a read <=60 s old (%)") + th
  f1 <- patchwork::wrap_plots(p1, patchwork::wrap_plots(p2, p3, p4, nrow = 1), ncol = 1, heights = c(1.45, 1)) +
    patchwork::plot_annotation(tag_levels = "a", theme = th)

  shares <- data.table::copy(x$shares)[block_set == "stage19_window" & level == "pooled" & CC == "all" & basis == "read"]
  shares[, `:=`(bound = factor(bound, levels = c("LOCF", "max300s", "max60s"), labels = c("LOCF", "<=300 s", "<=60 s")),
                 unit = factor(unit, levels = c("C1", "C2", "C3", "C4", "unknown")))]
  p5 <- ggplot2::ggplot(shares, ggplot2::aes(bound, 100 * share_of_observed_time, fill = unit)) +
    ggplot2::geom_col(width = .65, position = ggplot2::position_stack(reverse = TRUE), colour = "white", linewidth = .15) +
    ggplot2::facet_wrap(~Phase, nrow = 1) +
    ggplot2::scale_fill_manual(values = c(C1 = unname(MMM_PALETTE_GROUP[["CON"]]), C2 = "#A7ACC2", C3 = unname(MMM_PALETTE_DIVERGING[["high"]]), C4 = "#8E7747", unknown = "#D0D0D0"), name = NULL) +
    ggplot2::scale_y_continuous(breaks = c(0, 50, 100), expand = ggplot2::expansion(mult = c(0, .01))) + ggplot2::coord_cartesian(ylim = c(0, 100)) +
    ggplot2::labs(title = "Occupancy with unknown time retained", x = "Read-age convention", y = "Observed time (%)") + th
  diff <- x$basis[block_set == "stage19_window" & resolution == "column" & unit %in% paste0("C", 1:4) & bound == "LOCF" & denominator == "observed time (share_all)" &
                    level %in% c("pooled over animals (seconds)", "cage-episode spread")]
  diffs <- merge(diff[level == "pooled over animals (seconds)", .(Phase, unit, pooled = read_minus_event)],
                 diff[level == "cage-episode spread", .(Phase, unit, episode_median, episode_q25, episode_q75)], by = c("Phase", "unit"))
  diffs[, reader := factor(unit, levels = paste0("C", 1:4))]
  p6 <- ggplot2::ggplot(diffs, ggplot2::aes(reader, 100 * episode_median, colour = Phase)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = .25, colour = "grey60") +
    ggplot2::geom_linerange(ggplot2::aes(ymin = 100 * episode_q25, ymax = 100 * episode_q75), position = ggplot2::position_dodge(.4), linewidth = .4) +
    ggplot2::geom_point(ggplot2::aes(shape = Phase), position = ggplot2::position_dodge(.4), size = 1.1) + sc() +
    ggplot2::labs(title = "Column assignment depends on reconstruction", x = NULL, y = "Read - event occupancy (pp)") + th
  un <- x$unknown[block_set == "stage19_window" & level == "by board" & bound == "max60s" & CC == "all"]
  p7 <- ggplot2::ggplot(un, ggplot2::aes(factor(board), 100 * unknown_share_of_observed_time, colour = Phase, shape = Phase)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(.3), size = 1.6) + sc() +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
    ggplot2::labs(title = "Unknown time by board", x = "Board", y = "Observed time without a recent read (%)") + th
  gaps <- x$gaps[block_set == "stage19_window" & level == "gap class" & bound == "max60s"]
  gaps[, gap := factor(gap_class, levels = c("60-300s", "300-1800s", "1800-3600s", "3600-10800s"), labels = c("1-5 min", "5-30 min", "30-60 min", "1-3 h"))]
  p8 <- ggplot2::ggplot(gaps, ggplot2::aes(gap, 100 * share_of_phase_unknown_time, fill = Phase)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(.75), width = .65) + ggplot2::scale_fill_manual(values = pc) +
    ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
    ggplot2::labs(title = "Read gaps containing unknown time", x = NULL, y = "Unknown time (%)") + th
  f2 <- patchwork::wrap_plots(p5, p6, p7, p8, ncol = 2) + patchwork::plot_annotation(tag_levels = "a", theme = th)

  e <- data.table::copy(overlap$episodes)
  e[, support := factor(support, levels = c("LOCF", "Both tags <=60 s"))]
  p9 <- ggplot2::ggplot(e, ggplot2::aes(100 * expected_short, 100 * observed, colour = basis_label, shape = basis_label)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linewidth = .25, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_point(size = .65, alpha = .45) + ggplot2::facet_grid(Phase ~ support) +
    ggplot2::scale_colour_manual(values = colours[c("Event", "Read")], name = "Basis") + ggplot2::scale_shape_manual(values = c(Event = 1, Read = 16), name = "Basis") +
    ggplot2::scale_x_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) + ggplot2::scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100)) +
    ggplot2::labs(title = "Eight-reader overlap against time-shift reference", x = "Time-shift reference (%)", y = "Observed same-reader time (%)") + th
  z <- e[basis == "read" & support == "Both tags <=60 s"]
  ss <- aqr_point_spread(z, c("CC", "Phase"), "excess_short")
  p10 <- ggplot2::ggplot(z, ggplot2::aes(CC, 100 * excess_short, colour = Phase)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = .25) +
    ggplot2::geom_point(position = ggplot2::position_jitterdodge(jitter.width = .12, dodge.width = .4, seed = 16), size = .6, alpha = .3) +
    ggplot2::geom_linerange(data = ss, ggplot2::aes(y = 100 * median, ymin = 100 * q25, ymax = 100 * q75), position = ggplot2::position_dodge(.4), linewidth = .5) +
    ggplot2::geom_point(data = ss, ggplot2::aes(y = 100 * median, shape = Phase), position = ggplot2::position_dodge(.4), size = 1.4) + sc() +
    ggplot2::labs(title = "Excess overlap by cage change", x = NULL, y = "Observed - shifted reference (pp)") + th
  nl <- data.table::melt(z, id.vars = c("CageEpisodeID", "Phase"), measure.vars = c("excess_independence", "excess_short", "excess_long"),
                        variable.name = "reference", value.name = "excess")
  ns <- aqr_point_spread(nl, c("reference", "Phase"), "excess")
  ns[, reference := factor(reference, levels = c("excess_independence", "excess_short", "excess_long"), labels = c("Marginal\noccupancy", "Short\nshifts", "Long\nshifts"))]
  p11 <- ggplot2::ggplot(ns, ggplot2::aes(reference, 100 * median, colour = Phase, shape = Phase)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60", linewidth = .25) +
    ggplot2::geom_linerange(ggplot2::aes(ymin = 100 * q25, ymax = 100 * q75), position = ggplot2::position_dodge(.3), linewidth = .5) +
    ggplot2::geom_point(position = ggplot2::position_dodge(.3), size = 1.3) + sc() +
    ggplot2::labs(title = "Dependence on reference construction", x = NULL, y = "Observed - reference (pp)") + th
  ex <- overlap$dyads[basis == "read" & delta == 60 & window == "active"]
  choice <- ex[, .(pairs = .N, animals = data.table::uniqueN(c(A, B))), by = CageEpisodeID][pairs == 6 & animals == 4][order(CageEpisodeID)][1, CageEpisodeID]
  if (length(choice) != 1 || is.na(choice)) stop("No complete four-tag episode for the prespecified example.", call. = FALSE)
  ex <- ex[CageEpisodeID == choice]
  mat <- data.table::rbindlist(list(ex[, .(A, B, excess_short)], ex[, .(A = B, B = A, excess_short)]))
  ids <- sort(unique(c(mat$A, mat$B)))
  mat[, `:=`(A = factor(A, levels = ids), B = factor(B, levels = rev(ids)))]
  lim <- max(5, ceiling(max(abs(mat$excess_short)) * 100 / 5) * 5)
  p12 <- ggplot2::ggplot(mat, ggplot2::aes(A, B, fill = 100 * excess_short)) + ggplot2::geom_tile(colour = MMM_TILE_BORDER, linewidth = .3) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", 100 * excess_short)), size = 1.8) + ggplot2::coord_equal() +
    mmm_scale_fill_diverging("own", limit = lim, name = "Excess (pp)") +
    ggplot2::guides(fill = ggplot2::guide_colourbar(display = "rectangles", nbin = 64)) +
    ggplot2::labs(title = paste("Dyad example:", choice), x = "Animal", y = "Animal") + th +
    ggplot2::theme(axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
  f3 <- patchwork::wrap_plots(p9, p10, p11, p12, ncol = 2) + patchwork::plot_annotation(tag_levels = "a", theme = th)
  list(figures = list(QC01_acquisition = f1, QC02_uncertainty = f2, QC03_reader_overlap = f3),
       plot_data = list(QC01a_reader_health = health, QC01b_status = stat, QC01c_cadence = cad, QC01d_support = r,
         QC02a_shares = shares, QC02b_basis = diffs, QC02c_board_unknown = un, QC02d_gaps = gaps,
         QC03a_episodes = e, QC03b_change = z, QC03c_references = ns, QC03d_matrix = mat))
}
