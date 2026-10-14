# ================================================================
# One-day RFID calibration pilot: the post-pilot report layer (status mapping acp_report_status_v1.0)
# MMMSociability -- Functions/antenna_calibration_report.R
# ================================================================
# Used by Testing/audits/report_rfid_calibration_pilot.R (isolated output <Behavior>/RANM/calibration_reports/runs/<report_id>/),
# after the pre-registered analysis Testing/audits/analyse_rfid_calibration_pilot.R. Pure definitions: sourcing this file
# reads no data and writes no files. Needs data.table and digest and, at call time, the pre-registered module
# Functions/antenna_calibration_pilot.R (acp_content_sha256, acp_rule, acp_th, acp_set, acp_project and the ACP_* pins) and
# Functions/project_paths.R (mmm_isolated_output_root_check). Nothing of the pre-registered module is changed or
# reimplemented: thresholds come from the pinned criteria, the projection test from acp_project().
#
# What the report adds to a finished acp run (decisions.csv, day2_recommendation.csv, tables/, manifest/):
#  * a status per decision row: resolved, uncertain, not_obtained, or supporting (a statistic without a decision, or a
#    component of a governing decision), with the specification v1.1 item it informs and its consequence, from the
#    hash-pinned mapping docs/rfid_calibration/acp_report_status_v1.0.csv (statistic, decision_value, status, spec_item,
#    consequence, day2_path, rule_ids, superseded_feeds). A decision value or rule id the mapping does not list stops the
#    report: the mapping was written before any data and, like the criteria, a change is a new version;
#  * the projection test of the day-2 rule per threshold (D1, D4, D6: c inside the CI, h, |theta - c|, h/sqrt(2),
#    resolvable), recomputed from decisions.csv. It explains the recorded day2_recommendation.csv and never replaces it: a
#    disagreement is flagged and the recorded verdict stands;
#  * the uncertain results without a day-2 path, and one row per specification item.
# Wording (Tobias, 2026-10-14): a basis that is not resolved is retained as a sensitivity analysis, and its estimates are
# never described as bounds on true physical occupancy; recently detected (supported) is not validated location;
# calibration with static tags concerns bench hardware behaviour only. The frozen feeds text of A40-A42 and B7a in
# acp_criteria_v1.0 and v1.1 (v1.1 changes only G07, G08 and the day-2 handling) speaks of bounds; it stays unchanged
# (pinned), and the report prints the mapping's consequence instead.
#
# Choices where the brief leaves a detail open:
#  * statistic names with a run-time part are mapped by family: rate_rel_median_<tag> (any tag label) and
#    five_s_visits_kept_<basis>_tau<tau>; every other statistic by its exact name; a decision NA is the value "NA";
#  * status of a specification item: not_obtained if any governing row is, else uncertain if any is, else resolved;
#  * a day-2 trigger applies to a resolved, uncertain or not-obtained row when the recorded day2_recommendation.csv has it
#    triggered and, for the rows governing D1, D4 and D6 (D_col, board_call, competition_call, SR), when the trace has a
#    counting projection (or the D6 capture rule) for that row's statistic and board;
#  * decisions.csv numbers are read back as written by fwrite (15 significant digits); the recomputed projection can only
#    differ from the recorded one when |theta - c| lies within that rounding of h/sqrt(2), which is then flagged.
# ================================================================

ACR_VERSION <- "acr_v1.0"
ACR_STATUS_VERSION <- "acp_report_status_v1.0"
ACR_STATUS_FILE <- file.path("docs", "rfid_calibration", "acp_report_status_v1.0.csv")
# Content sha256 with line endings normalised to LF (acp_content_sha256()).
ACR_STATUS_SHA256 <- "8400d348a3a609a53f31adc09601bf2f75d4be81899e92840af2c10b2c3cabe4"
ACR_STATUS_COLUMNS <- c("statistic", "decision_value", "status", "spec_item", "consequence", "day2_path", "rule_ids", "superseded_feeds")
ACR_STATUSES <- c("resolved", "uncertain", "not_obtained", "supporting")
ACR_NA <- "NA"                                   # decision_value of a row whose decision is NA
ACR_TRIGGERS <- paste0("D", 1:7)
ACR_DECISION_COLUMNS <- c("criterion", "rule_id", "statistic", "board", "estimate", "lo", "hi", "n_units", "decision", "category", "feeds")
ACR_DAY2_COLUMNS <- c("trigger", "triggered", "reason", "day2_content")
# Statistics of the trace that decide whether D1, D4 or D6 applies to a governing row.
ACR_TRACE_LINK <- list(D_col = "D_col", board_call = c("R_board", "coread_ratio"), competition_call = c("TR", "MS", "competition_call"),
                       SR = "SR")
ACR_SCOPE <- paste("These results apply to boards 2 and 4 under bench conditions (one PhenoSoft session, production cages, fresh",
                   "bedding, no animal), with static or sled-moved tags.")
ACR_BENCH_SENTENCE <- paste("Calibration with static tags bounds bench hardware behaviour only: no Exp9 location is validated, and a",
                            "recently detected (supported) position is not a validated location.")
ACR_BASES_SENTENCE <- paste("A basis that is not resolved is retained as a sensitivity analysis: its estimates are not limits that",
                            "bracket true physical occupancy, and no basis is chosen by group effects, predictive performance or",
                            "significance.")
ACR_FOOTPRINT_CONSEQUENCE <- paste(
  "Descriptive, no decision: the reliable, border and unread classes of the single-tag placements (P1s at the true reader) give",
  "the physical wording of 'assigned to reader k' (S3, S6 text) for boards 2 and 4 on the bench; border placements are reported",
  "as border. Not a day-2 reason.")
# Specification v1.1 items the pilot informs; governing = the decision statistics that carry the item's status.
ACR_SPEC_ITEMS <- data.frame(
  spec_item = c("basis", "reader-level basis", "resolution", "fresh age class", "carry-forward age cap", "tau",
                "competition in one field", "competition across fields", "board-level load", "house shielding", "roof",
                "board call", "footprint wording", "QC rules", "transfer"),
  spec_reference = c("decision 2; sections 1, 4, 7, 8 (items 1, 6), 11.1; S3", "decision 3; S3", "decision 3; section 1; S3, S4",
                     "section 3; 11.3", "decision 4; section 3; S3 (ii), (iii); section 8 item 1", "section 1; S2, S3",
                     "sections 5, 6; S6, S7; 11.2, 11.5", "section 6; S7; 11.5", "section 6; S7, S9; 11.5", "section 5; 11.2",
                     "section 5", "section 6; 11.4; S7", "section 9 (S3, S6 text)", "section 10", "section 11.10"),
  criteria = c("(a) A20-A42", "(a) A50", "B2", "B3", "B7", "B11; D5", "B4; D6", "B5", "B6; D6", "B8", "B9", "B10; D3, D4", "B1",
               "B13-B15", "B16; D7"),
  governing = c("D_col", "D_rdr", "resolution", "fresh_age_s", "delta_s", "tau_star_event|tau_star_read", "competition_call", "CS_C4",
                "SR", "house_ratio", "roof_vs_foam_H05_H06", "board_call", "", "phantom_events|tag_qc", "transfer"),
  stringsAsFactors = FALSE)

# ---------------------------------------------------------------- the status mapping (pinned)

#' The status mapping (all columns character; attribute sha256). Stops unless its content hash is ACR_STATUS_SHA256 and it
#' is well formed: known statuses, specification items and day-2 triggers; one row per statistic x decision value; every
#' decision NA supporting.
acr_status_map <- function(path = ACR_STATUS_FILE) {
  if (!file.exists(path)) stop("No status mapping: ", path, call. = FALSE)
  h <- acp_content_sha256(path)
  if (!identical(h, ACR_STATUS_SHA256)) {
    stop("The status mapping differs from the pinned ", ACR_STATUS_VERSION, " (content sha256 ", h, "); the report refuses to run. ",
         "A change is a new mapping version.", call. = FALSE)
  }
  m <- data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
  if (!identical(names(m), ACR_STATUS_COLUMNS)) stop("Status mapping columns differ from ACR_STATUS_COLUMNS.", call. = FALSE)
  if (any(!m$status %in% ACR_STATUSES)) stop("Unknown status in the mapping.", call. = FALSE)
  if (any(!m$spec_item %in% ACR_SPEC_ITEMS$spec_item)) stop("Unknown specification item in the mapping.", call. = FALSE)
  paths <- strsplit(m$day2_path, "|", fixed = TRUE)
  if (!all(vapply(paths, function(p) length(p) > 0 && all(p %in% c("none", ACR_TRIGGERS)), logical(1)))) {
    stop("Unknown day-2 path in the mapping.", call. = FALSE)
  }
  if (anyDuplicated(m[, c("statistic", "decision_value")])) stop("Duplicate statistic x decision value in the mapping.", call. = FALSE)
  if (any(m$decision_value == ACR_NA & m$status != "supporting")) stop("A decision NA must be supporting.", call. = FALSE)
  if (any(!nzchar(m$consequence) | !nzchar(m$rule_ids))) stop("Every mapping row needs a consequence and its rule ids.", call. = FALSE)
  attr(m, "sha256") <- h
  attr(m, "path") <- path
  m[]
}

#' Mapping key of a statistic name: the family of names with a run-time part, otherwise the name itself.
acr_statistic_key <- function(statistic) {
  s <- sub("^rate_rel_median_.*$", "rate_rel_median_<tag>", as.character(statistic))
  sub("^five_s_visits_kept_(event|read)_tau.*$", "five_s_visits_kept_<basis>_tau<tau>", s)
}

#' Mapping key of a decision: the value, or "NA" for a missing or empty one.
acr_decision_value <- function(decision) {
  d <- as.character(decision)
  ifelse(is.na(d) | !nzchar(d), ACR_NA, d)
}

# ---------------------------------------------------------------- classification

#' One row per decision row (order kept): the decision columns without feeds, then status, spec_item, consequence,
#' day2_path and feeds_note (non-empty where the frozen feeds text of the row's rule is superseded in reporting). Stops on a
#' decision value or a rule id the mapping does not list.
acr_classify <- function(decisions, map) {
  d <- data.table::copy(data.table::as.data.table(decisions))
  miss <- setdiff(ACR_DECISION_COLUMNS, names(d))
  if (length(miss)) stop("The decision table lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  if (!nrow(d)) stop("The decision table is empty.", call. = FALSE)
  d[, `:=`(row_ = .I, statistic_key = acr_statistic_key(statistic), decision_value = acr_decision_value(decision),
           board = as.character(board))]
  m <- data.table::as.data.table(map)[, .(statistic_key = statistic, decision_value, status, spec_item, consequence, day2_path, rule_ids,
                                          superseded_feeds)]
  x <- merge(d, m, by = c("statistic_key", "decision_value"), all.x = TRUE, sort = FALSE)
  un <- x[is.na(status)]
  if (nrow(un)) {
    stop("Decision value(s) not in the status mapping ", ACR_STATUS_VERSION, ": ",
         paste(unique(sprintf("%s = %s", un$statistic, un$decision_value)), collapse = "; "), ". A new value needs a new mapping version.",
         call. = FALSE)
  }
  ok <- mapply(function(r, ids) r %in% strsplit(ids, "|", fixed = TRUE)[[1]], x$rule_id, x$rule_ids)
  if (!all(ok)) {
    stop("The status mapping does not describe these decision rows (rule id): ",
         paste(unique(sprintf("%s = %s (%s)", x$statistic[!ok], x$decision_value[!ok], x$rule_id[!ok])), collapse = "; "), call. = FALSE)
  }
  x[, feeds_note := ifelse(nzchar(superseded_feeds), sprintf("frozen feeds text of %s in %s is unchanged; this report prints the consequence instead",
                                                             rule_id, ACP_VERSION), "")]
  x <- x[order(row_)]
  x[, .(criterion, rule_id, statistic, board, estimate, lo, hi, n_units, decision, category, status, spec_item, consequence, day2_path,
        feeds_note)]
}

# ---------------------------------------------------------------- day-2 trace

#' The projection test of the day-2 rule per threshold, recomputed from the decision table with acp_project(), the P1 factor
#' and the D1a, D4a, D4b, D6a-D6c threshold sets of the criteria; plus the D6 capture rule per board. One row per trigger x
#' statistic x board x threshold: governing_decision (the decision the trigger's precondition looks at), precondition, the
#' estimate and CI, inside, h (CI half-width), distance (|theta - c|), h_over_sqrt2 (P1 x h), resolvable and counts
#' (precondition and resolvable; for D6d: capture or starvation on this board and on no other).
acr_day2_trace <- function(decisions, crit) {
  fac <- acp_th(crit, "P1")
  d <- data.table::copy(data.table::as.data.table(decisions))[, board := as.character(board)]
  one <- function(stat, b = "pooled") d[d$statistic == stat & d$board == b]
  dec_of <- function(x) if (nrow(x) != 1L) "missing" else if (is.na(x$decision)) NA_character_ else x$decision
  proj <- function(trigger, rule, stat, b, governing, pre) {
    x <- one(stat, b); th <- acp_set(crit, rule)
    if (nrow(x) != 1L) {
      return(data.table::data.table(trigger = trigger, rule_id = rule, statistic = stat, board = b, governing_decision = governing,
                                    precondition = pre, threshold = th, estimate = NA_real_, lo = NA_real_, hi = NA_real_, inside = FALSE,
                                    h = NA_real_, distance = NA_real_, h_over_sqrt2 = NA_real_, resolvable = FALSE, counts = FALSE))
    }
    p <- acp_project(x$estimate, x$lo, x$hi, th, fac)
    data.table::data.table(trigger = trigger, rule_id = rule, statistic = stat, board = b, governing_decision = governing, precondition = pre,
                           threshold = p$threshold, estimate = p$estimate, lo = p$lo, hi = p$hi, inside = p$inside, h = p$h,
                           distance = p$distance, h_over_sqrt2 = fac * p$h, resolvable = p$resolvable, counts = pre & p$resolvable)
  }
  rows <- list()
  g <- dec_of(one("D_col"))
  rows[[1]] <- proj("D1", "D1a", "D_col", "pooled", g, identical(g, "TIE_inconclusive"))
  g <- dec_of(one("board_call"))
  rows[[2]] <- proj("D4", "D4a", "R_board", "pooled", g, identical(g, "undecided"))
  rows[[3]] <- proj("D4", "D4b", "coread_ratio", "pooled", g, identical(g, "undecided"))
  n_cap <- nrow(d[d$statistic == "competition_call" & d$decision %in% "capture_or_starvation"])
  for (bb in c("2", "4")) {
    g <- dec_of(one("competition_call", bb))
    rows[[length(rows) + 1L]] <- proj("D6", "D6a", "TR", bb, g, identical(g, "intermediate"))
    rows[[length(rows) + 1L]] <- proj("D6", "D6b", "MS", bb, g, identical(g, "intermediate"))
    gs <- dec_of(one("SR", bb))
    rows[[length(rows) + 1L]] <- proj("D6", "D6c", "SR", bb, gs, identical(gs, "undecided"))
    cap <- identical(g, "capture_or_starvation")
    rows[[length(rows) + 1L]] <- data.table::data.table(trigger = "D6", rule_id = "D6d", statistic = "competition_call", board = bb,
                                                        governing_decision = g, precondition = cap, threshold = NA_real_, estimate = NA_real_,
                                                        lo = NA_real_, hi = NA_real_, inside = NA, h = NA_real_, distance = NA_real_,
                                                        h_over_sqrt2 = NA_real_, resolvable = NA, counts = cap && n_cap == 1L)
  }
  data.table::rbindlist(rows)
}

#' The recorded triggers next to what the decision table implies: D1, D4 and D6 recomputed from the trace; D5 from the tau
#' decisions; D2 only in one direction (a not_obtained decision implies D2; its other reasons are not in decisions.csv); D3
#' recorded only; D7 never a trigger; the recommendation row against the recorded triggers. flag: non-empty where the
#' recorded verdict and the decision table disagree (the recorded verdict stands).
acr_day2_check <- function(trace, day2, classified) {
  y <- data.table::as.data.table(day2)
  rec <- function(t) { x <- y[y$trigger == t]; if (nrow(x) == 1L) isTRUE(as.logical(x$triggered)) else NA }
  cl <- data.table::as.data.table(classified)
  tz <- cl[grepl("^tau_star_", cl$statistic)]
  rows <- list(
    list("D1", any(trace$counts[trace$trigger == "D1"] %in% TRUE), "projection test (P1) over day2_trace.csv"),
    list("D2", if (any(acr_decision_value(cl$decision) == "not_obtained")) TRUE else NA,
         "a not_obtained decision implies D2 (its other reasons, e.g. sheet or alignment problems, are in the acp run's tables/missing.csv)"),
    list("D3", NA, "recorded only (REF repeatability; not recomputed)"),
    list("D4", any(trace$counts[trace$trigger == "D4"] %in% TRUE), "projection test (P1) over day2_trace.csv"),
    list("D5", any(tz$decision %in% "not_decidable"), "a tau decision not_decidable implies D5"),
    list("D6", any(trace$counts[trace$trigger == "D6"] %in% TRUE), "projection test (P1) and the one-board capture rule over day2_trace.csv"),
    list("D7", FALSE, "never a trigger"))
  out <- data.table::rbindlist(lapply(rows, function(r) {
    recorded <- rec(r[[1]]); recomputed <- r[[2]]
    agree <- if (is.na(recorded) || is.na(recomputed)) NA else identical(recorded, recomputed)
    data.table::data.table(trigger = r[[1]], recorded = recorded, recomputed = recomputed, check = r[[3]], agree = agree,
                           flag = if (isFALSE(agree)) sprintf("%s: recorded %s, the decision table gives %s; the recorded verdict stands",
                                                              r[[1]], recorded, recomputed) else "")
  }))
  rr <- rec("recommendation"); any_rec <- any(out$recorded %in% TRUE)
  agree <- if (is.na(rr)) NA else identical(rr, any_rec)
  rbind(out, data.table::data.table(trigger = "recommendation", recorded = rr, recomputed = any_rec, check = "any recorded trigger D1-D7",
                                    agree = agree, flag = if (isFALSE(agree)) "recommendation: the recorded line disagrees with the recorded triggers" else ""))
}

#' Adds day2_applies and day2_note to classified rows (supporting rows: FALSE, ""). A trigger of the row's day2_path applies
#' when it is recorded as triggered and, for the statistics in ACR_TRACE_LINK, the trace counts for that statistic family on
#' the row's board.
acr_attach_day2 <- function(classified, trace, day2) {
  x <- data.table::copy(data.table::as.data.table(classified))
  y <- data.table::as.data.table(day2)
  rec <- function(t) { r <- y[y$trigger == t]; nrow(r) == 1L && isTRUE(as.logical(r$triggered)) }
  one <- function(path, stat, b, status) {
    if (status == "supporting") return(list(FALSE, ""))
    ts <- setdiff(strsplit(path, "|", fixed = TRUE)[[1]], "none")
    if (!length(ts)) return(list(FALSE, "no day-2 path"))
    res <- vapply(ts, function(t) {
      r <- rec(t)
      if (!r) return(c("FALSE", paste(t, "not recorded")))
      if (t %in% c("D1", "D4", "D6") && stat %in% names(ACR_TRACE_LINK)) {
        hit <- any(trace$counts[trace$trigger == t & trace$board == b & trace$statistic %in% ACR_TRACE_LINK[[stat]]] %in% TRUE)
        return(c(as.character(hit), sprintf("%s recorded; %s", t, if (hit) "it applies to this decision" else
          "triggered by another decision; neither the projection test nor the one-board capture rule holds for this one")))
      }
      c("TRUE", paste(t, "recorded"))
    }, character(2))
    list(any(res[1, ] == "TRUE"), paste(res[2, ], collapse = "; "))
  }
  r <- Map(one, x$day2_path, x$statistic, x$board, x$status)
  x[, `:=`(day2_applies = vapply(r, `[[`, logical(1), 1), day2_note = vapply(r, `[[`, character(1), 2))]
  x[]
}

#' Uncertain rows without an applicable day-2 trigger (adds the day-2 columns when missing).
acr_uncertain_without_day2 <- function(classified, trace, day2) {
  x <- data.table::as.data.table(classified)
  if (!"day2_applies" %in% names(x)) x <- acr_attach_day2(x, trace, day2)
  x[x$status == "uncertain" & !x$day2_applies, .(spec_item, statistic, board, decision, estimate, lo, hi, day2_path, day2_note, consequence)]
}

# ---------------------------------------------------------------- summaries

#' "estimate [lo, hi]" (4 significant digits), the estimate alone, or "".
acr_fmt <- function(estimate, lo = NA_real_, hi = NA_real_) {
  f <- function(v) format(signif(v, 4), scientific = FALSE, trim = TRUE)
  ifelse(is.na(estimate), "", ifelse(is.na(lo) | is.na(hi), vapply(estimate, f, ""),
                                     sprintf("%s [%s, %s]", vapply(estimate, f, ""), vapply(lo, f, ""), vapply(hi, f, ""))))
}

#' Label of a decision row: "statistic (board b): decision = estimate [lo, hi]".
acr_label <- function(x) {
  e <- acr_fmt(x$estimate, x$lo, x$hi)
  sprintf("%s%s: %s%s", x$statistic, ifelse(x$board == "pooled", "", paste0(" (board ", x$board, ")")), acr_decision_value(x$decision),
          ifelse(nzchar(e), paste0(" = ", e), ""))
}

#' B1 classes at the true reader per board, from the acp run's tables/detection.csv (columns board, PositionID, class).
acr_footprint <- function(det) {
  x <- data.table::as.data.table(det)[PositionID == 0L & !is.na(class)]
  if (!nrow(x)) return("no single-tag placement in tables/detection.csv")
  n <- x[, .N, by = .(board, class)]
  paste(vapply(sort(unique(n$board)), function(b) {
    z <- n[n$board == b]
    sprintf("board %s: %s", b, paste(sprintf("%d %s", z$N, z$class)[order(match(z$class, c("reliable", "border", "unread")))], collapse = ", "))
  }, character(1)), collapse = "; ")
}

#' One row per specification item (ACR_SPEC_ITEMS): the governing decisions, status, consequence (one per distinct
#' consequence, prefixed with its decisions when the item has more than one governing row), the day-2 triggers that apply
#' (with the decision they apply to) and the feeds notes.
acr_spec_consequences <- function(classified, footprint = NULL) {
  cl <- data.table::as.data.table(classified)
  if (!"day2_applies" %in% names(cl)) cl[, `:=`(day2_applies = FALSE, day2_note = "")]
  si <- data.table::as.data.table(ACR_SPEC_ITEMS)
  data.table::rbindlist(lapply(seq_len(nrow(si)), function(i) {
    s <- si[i]
    base <- data.table::data.table(spec_item = s$spec_item, spec_reference = s$spec_reference, criteria = s$criteria)
    if (!nzchar(s$governing)) {
      return(cbind(base, governing = "B1 classes per placement (acp tables/detection.csv)",
                   decisions = if (is.null(footprint) || is.na(footprint)) "see the acp run's tables/detection.csv" else footprint,
                   status = "supporting", consequence = ACR_FOOTPRINT_CONSEQUENCE, day2 = "none", feeds_note = ""))
    }
    gov <- strsplit(s$governing, "|", fixed = TRUE)[[1]]
    x <- cl[cl$statistic %in% gov]
    if (!nrow(x)) {
      return(cbind(base, governing = gsub("|", ", ", s$governing, fixed = TRUE), decisions = "no decision row", status = "not_obtained",
                   consequence = "The governing decision is missing from decisions.csv; the item stays open.",
                   day2 = "see day2_recommendation.csv", feeds_note = ""))
    }
    st <- if (any(x$status == "not_obtained")) "not_obtained" else if (any(x$status == "uncertain")) "uncertain" else "resolved"
    lab <- acr_label(x)
    cons <- if (nrow(x) == 1L) x$consequence else
      paste(vapply(unique(x$consequence), function(cc) sprintf("[%s] %s", paste(lab[x$consequence == cc], collapse = "; "), cc), ""), collapse = " ")
    d2 <- if (any(x$day2_applies)) paste(sprintf("%s: %s", lab[x$day2_applies], x$day2_note[x$day2_applies]), collapse = "; ") else "none applies"
    cbind(base, governing = gsub("|", ", ", s$governing, fixed = TRUE), decisions = paste(lab, collapse = "; "), status = st, consequence = cons,
          day2 = d2,
          feeds_note = paste(unique(x$feeds_note[nzchar(x$feeds_note)]), collapse = "; "))
  }))
}

#' The report of one acp run (pure): classified rows with their day-2 columns, the trace and its check against the
#' recorded verdict, the resolved / uncertain / not-obtained lists, the uncertain rows without a day-2 path, the
#' specification items, and the recorded verdict and triggers (never recomputed; flags lists every disagreement).
acr_summary <- function(decisions, map, crit, day2, footprint = NULL) {
  y <- data.table::as.data.table(day2)
  cl <- acr_classify(decisions, map)
  tr <- acr_day2_trace(decisions, crit)
  cl <- acr_attach_day2(cl, tr, y)
  ck <- acr_day2_check(tr, y, cl)
  list(classified = cl, trace = tr, check = ck,
       resolved = cl[cl$status == "resolved"], uncertain = cl[cl$status == "uncertain"], not_obtained = cl[cl$status == "not_obtained"],
       uncertain_without_day2 = acr_uncertain_without_day2(cl, tr, y), spec = acr_spec_consequences(cl, footprint),
       verdict = y[y$trigger == "recommendation"], triggers = y[y$trigger != "recommendation"], flags = ck$flag[nzchar(ck$flag)])
}

# ---------------------------------------------------------------- the acp run (read-only) and the report folder

#' Verifies a finished acp run folder before anything is read from it: every file listed in manifest/output_files_sha256.csv
#' has its sha256, no unlisted file is present, decisions.csv, day2_recommendation.csv and manifest/run.csv are listed,
#' run.csv names this folder and records the pinned criteria and schedule (version and content sha256, actual and pinned).
#' Returns list(dir, run = named run.csv values, files = data.table(file, sha256), release = dev_run FALSE, code clean and
#' board history pinned).
acr_verify_acp_run <- function(acp_run) {
  if (!is.character(acp_run) || length(acp_run) != 1L || is.na(acp_run) || !grepl("^([A-Za-z]:[/\\\\]|//|\\\\\\\\)", acp_run)) {
    stop("The acp run must be given as one absolute path.", call. = FALSE)
  }
  dir <- normalizePath(gsub("\\\\", "/", acp_run), winslash = "/", mustWork = FALSE)
  if (!dir.exists(dir)) stop("No acp run folder: ", dir, call. = FALSE)
  if (startsWith(basename(dir), ".tmp_")) stop("A build folder (.tmp_) is not a finished acp run: ", dir, call. = FALSE)
  man <- file.path(dir, "manifest", "output_files_sha256.csv")
  if (!file.exists(man)) stop("The acp run has no manifest/output_files_sha256.csv: ", dir, call. = FALSE)
  m <- data.table::fread(man, colClasses = "character", na.strings = NULL)
  if (!all(c("file", "sha256") %in% names(m)) || anyDuplicated(m$file)) stop("Unreadable acp output manifest: ", man, call. = FALSE)
  need <- c("decisions.csv", "day2_recommendation.csv", "manifest/run.csv")
  if (!all(need %in% m$file)) stop("The acp output manifest does not list: ", paste(setdiff(need, m$file), collapse = ", "), call. = FALSE)
  actual <- vapply(file.path(dir, m$file), function(p) if (file.exists(p)) digest::digest(p, algo = "sha256", file = TRUE) else NA_character_,
                   character(1), USE.NAMES = FALSE)
  bad <- m$file[is.na(actual) | tolower(actual) != tolower(m$sha256)]
  if (length(bad)) stop("The acp run differs from its output manifest (missing or changed): ", paste(bad, collapse = ", "), call. = FALSE)
  extra <- setdiff(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE), c(m$file, "manifest/output_files_sha256.csv"))
  if (length(extra)) stop("The acp run folder holds files its output manifest does not list: ", paste(extra, collapse = ", "), call. = FALSE)
  run <- data.table::fread(file.path(dir, "manifest", "run.csv"), colClasses = "character", na.strings = NULL)
  if (!identical(names(run), c("item", "value")) || anyDuplicated(run$item)) stop("Unreadable acp manifest/run.csv.", call. = FALSE)
  v <- stats::setNames(run$value, run$item)
  pins <- c(criteria_version = ACP_VERSION, criteria_content_sha256 = ACP_CRITERIA_SHA256, criteria_pinned_sha256 = ACP_CRITERIA_SHA256,
            schedule_version = ACP_SCHEDULE_VERSION, schedule_content_sha256 = ACP_SCHEDULE_SHA256, schedule_pinned_sha256 = ACP_SCHEDULE_SHA256)
  items <- c("run_id", "git_commit", "code_clean", "dev_run", "board_history_pinned", names(pins))
  if (!all(items %in% names(v))) stop("acp manifest/run.csv lacks: ", paste(setdiff(items, names(v)), collapse = ", "), call. = FALSE)
  off <- names(pins)[v[names(pins)] != pins]
  if (length(off)) stop("The acp run did not use the pinned criteria and schedule (", ACP_VERSION, ", ", ACP_SCHEDULE_VERSION, "): ",
                        paste(sprintf("%s = %s", off, v[off]), collapse = "; "), call. = FALSE)
  if (!identical(v[["run_id"]], basename(dir))) stop("run.csv names run ", v[["run_id"]], " but the folder is ", basename(dir), call. = FALSE)
  list(dir = dir, run = v, files = data.table::data.table(file = m$file, sha256 = tolower(m$sha256)),
       release = identical(v[["dev_run"]], "FALSE") && identical(v[["code_clean"]], "TRUE") && identical(v[["board_history_pinned"]], "TRUE"))
}

#' decisions.csv (typed: estimate, lo, hi numeric, n_units integer, an empty decision NA) and day2_recommendation.csv
#' (triggered logical; rows D1-D7 and recommendation, in that order) of a verified acp run folder.
acr_read_acp <- function(dir) {
  d <- data.table::fread(file.path(dir, "decisions.csv"), colClasses = "character", na.strings = "", encoding = "UTF-8")
  if (!identical(names(d), ACR_DECISION_COLUMNS)) stop("decisions.csv columns differ from ACR_DECISION_COLUMNS.", call. = FALSE)
  for (cc in c("estimate", "lo", "hi")) data.table::set(d, j = cc, value = as.numeric(d[[cc]]))
  d[, n_units := as.integer(n_units)]
  y <- data.table::fread(file.path(dir, "day2_recommendation.csv"), colClasses = "character", na.strings = NULL, encoding = "UTF-8")
  if (!identical(names(y), ACR_DAY2_COLUMNS)) stop("day2_recommendation.csv columns differ from ACR_DAY2_COLUMNS.", call. = FALSE)
  if (!identical(y$trigger, c(ACR_TRIGGERS, "recommendation")) || !all(y$triggered %in% c("TRUE", "FALSE"))) {
    stop("day2_recommendation.csv must hold D1-D7 and the recommendation, triggered TRUE or FALSE.", call. = FALSE)
  }
  y[, triggered := triggered == "TRUE"]
  list(decisions = d[], day2 = y[])
}

#' The report folder <base>/runs/<report_id> and its build folder <base>/runs/.tmp_<report_id> (the two folders the
#' protection guard of Functions/protected_outputs.R leaves out of its snapshots). The base passes
#' mmm_isolated_output_root_check(), must not lie in (or be) the acp run folder and must not contain it (reports are kept
#' apart from the acp runs); an existing report or build folder is refused.
acr_report_dirs <- function(acp_run, base, report_id, project_root = mmm_project_root()) {
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,59}$", report_id)) stop("Bad report id: ", report_id, call. = FALSE)
  b <- mmm_isolated_output_root_check(base, project_root)
  key <- function(p) tolower(paste(sub("[. ]+$", "", strsplit(normalizePath(gsub("\\\\", "/", p), winslash = "/", mustWork = FALSE), "/",
                                                              fixed = TRUE)[[1]]), collapse = "/"))
  within <- function(p, root) { p <- key(p); r <- sub("/+$", "", key(root)); identical(p, r) || startsWith(p, paste0(r, "/")) }
  if (within(b, acp_run)) stop("Refusing to write inside the acp run folder: ", b, call. = FALSE)
  if (within(acp_run, b)) stop("The report base contains the acp run folder; reports are kept apart from the acp runs: ", b, call. = FALSE)
  out <- file.path(b, "runs", report_id); tmp <- file.path(b, "runs", paste0(".tmp_", report_id))
  if (file.exists(out)) stop("Report folder exists; choose a new report id: ", out, call. = FALSE)
  if (file.exists(tmp)) stop("A partial report folder exists; inspect and remove it first: ", tmp, call. = FALSE)
  list(base = b, out = out, tmp = tmp)
}

#' README lines of a report. info: report_id, commit, acp_dir, acp_run (named run.csv values), release (acp run),
#' map_sha256.
acr_readme_lines <- function(rep, info) {
  wrap <- function(txt) strwrap(txt, width = 118, indent = 2, exdent = 6)
  # a resolved decision without a day-2 path gets no day-2 clause
  item <- function(x) {
    if (!nrow(x)) return("  (none)")
    unlist(lapply(seq_len(nrow(x)), function(i) {
      r <- x[i]
      d2 <- if (r$status == "resolved" && r$day2_path == "none") "" else
        paste0(" Day 2: ", if (r$day2_applies) "applies (" else "no applicable trigger (", r$day2_note, ").")
      wrap(sprintf("- [%s] %s. %s%s", r$spec_item, acr_label(r), r$consequence, d2))
    }))
  }
  v <- info$acp_run
  ck <- rep$check
  c(paste("RFID CALIBRATION PILOT: post-pilot report", ACR_VERSION, "on the pre-registered analysis", basename(info$acp_dir),
          sprintf("(%s / %s)", ACP_VERSION, ACP_SCHEDULE_VERSION)), "",
    paste("report:", info$report_id, "| commit:", info$commit, "| status mapping:", ACR_STATUS_VERSION, "(content sha256", paste0(info$map_sha256, ")")),
    paste("acp run:", info$acp_dir), paste("acp run commit:", v[["git_commit"]], "| dev run:", v[["dev_run"]], "| code clean:", v[["code_clean"]],
                                           "| board history pinned:", v[["board_history_pinned"]]),
    if (!isTRUE(info$release)) "NOT A RELEASE: the acp run is a dev run, ran on uncommitted code or used another board history; for checking only.",
    "", "SCOPE", ACR_SCOPE, ACR_BENCH_SENTENCE, wrap(ACR_BASES_SENTENCE), "",
    "DAY-2 VERDICT (as recorded in the acp run's day2_recommendation.csv; a recommendation only, Tobias decides; not recomputed)",
    wrap(rep$verdict$reason),
    vapply(seq_len(nrow(rep$triggers)), function(i) sprintf("  %s %s: %s", rep$triggers$trigger[i],
                                                            if (isTRUE(rep$triggers$triggered[i])) "TRIGGERED" else "not triggered",
                                                            rep$triggers$reason[i]), character(1)),
    if (length(rep$flags)) paste("  FLAG:", rep$flags) else
      "  The projection-test trace (day2_trace.csv) and the decision table agree with the recorded D1, D4, D5 and D6.",
    "", sprintf("RESOLVED (%d)", nrow(rep$resolved)), item(rep$resolved),
    "", sprintf("UNCERTAIN (%d; %d without an applicable day-2 trigger)", nrow(rep$uncertain), nrow(rep$uncertain_without_day2)),
    item(rep$uncertain),
    "", sprintf("NOT OBTAINED (%d)", nrow(rep$not_obtained)), item(rep$not_obtained),
    "", "FROZEN CRITERIA TEXT",
    wrap(paste("The feeds text of A40-A42 and B7a in", ACP_VERSION, "is unchanged (content-hash pinned) and still appears in the acp run's",
               "decisions.csv; this report prints the corrected consequences of", ACR_STATUS_VERSION, "instead (Tobias, 2026-10-14).",
               "The pre-registered criteria, schedule and analysis code are not changed by this report.")),
    "", "FILES",
    "status_by_decision.csv      every decision row: status, specification item, consequence, day-2 path and whether it applies",
    "spec_consequences.csv       one row per specification v1.1 item: governing decisions, status, consequence",
    "day2_trace.csv              projection test per threshold (D1, D4, D6): inside, h, |theta - c|, h/sqrt(2), resolvable",
    "day2_trace_check.csv        recorded triggers against the decision table (flags only; the recorded verdict stands)",
    "uncertain_without_day2.csv  uncertain results that no day-2 trigger addresses",
    "manifest/                   acp run files verified, code hashes, run parameters, protection check, output hashes")
}
