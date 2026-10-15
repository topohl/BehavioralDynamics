# ================================================================
# One-day RFID calibration pilot: the pre-registered analysis (decision criteria acp_criteria_v1.2, schedule schedule_one_day_v3)
# MMMSociability -- Functions/antenna_calibration_pilot.R
# ================================================================
# Used by Testing/audits/analyse_rfid_calibration_pilot.R (PRE-REGISTERED before any measurement; isolated output root
# <Behavior>/RANM/calibration/<run_id>/). Pure definitions: sourcing this file reads no data and writes no files. Needs
# data.table and digest and, at call time, Functions/antenna_log_helpers.R (rfid_read_raw_log, rfid_flip_filter,
# RFID_GRID_X/Y; itself needs dplyr, tibble and animalpos_position_id() of Functions/animalpos_preprocessing_helpers.R),
# Functions/animalpos_reconstruction.R (apr_reads, apr_positions), Functions/antenna_measurement.R (amv_runs,
# amv_grid_track, amv_move_geometry, amv_column, amv_union, amv_system_messages), Functions/antenna_spatial_reconstruction.R
# (asr_resolve_ties) and Functions/antenna_acquisition_qc.R (aqc_lattice, aqc_coread_flags, aqc_coread_counts,
# aqc_neighbours, aqc_device_check, aqc_board_overlap_counts). Those functions are reused unchanged.
# Protocol, criteria, day-2 rule and plan: docs/RFID_CALIBRATION_PILOT.md sections 1-4.
#
# Amendment of 2026-10-15 (before any acquisition; Tobias's decision): schedule v2 runs every programme 4-2-2-4 by halves and
# the competition run twice per board (the 10-s blocks of both runs are pooled per board); criteria v1.1 change only G07
# and the day-2 handling (a separate confirmation experiment, never pooled); a substitute board needs an explicit
# authorization (acp_read_substitution()). acp_criteria_v1.0.csv and schedule_one_day_v1.csv stay for provenance.
# Amendment v3 of 2026-10-15 (before any acquisition and booking; Tobias's decision): schedule v3 adds column-boundary step
# transitions (K, both rows, 4-2-2-4 within the M visits), three house-extension ON/OFF pairs (HX: HEx, HEy, HSy) and
# house-lifted periods C1o and C4o in every competition run (C4, CS and C1 shortened); criteria v1.2 change G07 and add the
# rows of category added_v1.2 (G10, K, KA, HX), and correct the seeding rule A04 (an event in progress at the placement
# start seeds both bases; before, a tag placed before the start touch and read continuously left the event basis
# unassigned for its whole placement). The added endpoints are computed apart from the v1.1 endpoints (no v1.1
# endpoint gains or loses a unit, its bootstrap draws are unchanged), and they are outside the day-2 rule. A fourth drop
# step drops the HSy pairs on both boards. Schedule v2 and criteria v1.1 stay for provenance.
# Pre-registration. The criteria file docs/rfid_calibration/acp_criteria_v1.2.csv (one row per rule: rule_id, criterion,
# outcome, statistic, comparator, threshold, threshold_hi, category, feeds, source) and the seeded schedule
# docs/rfid_calibration/schedule_one_day_v3.csv are pinned by their content sha256 (line endings normalised to LF, as
# sxm_content_sha256()); acp_criteria() and acp_read_schedule() stop on any other content. Every decision threshold is read
# from the criteria file (acp_th(), acp_set(), acp_test(): the comparator of the row is applied, with 1e-9 slack for
# floating point); a statistic that is NA never satisfies a condition. Comparators: = (a parameter), >=, >, <=, <, ==,
# in (closed interval [threshold, threshold_hi]), set (candidates a|b|c), info (documentation only). The file holds the
# decision_criteria section and, as category day2, the thresholds of the day-2 rule fixed with it.
#
# Inputs of the day (Raw Data/Behavior/RFID/calibration/<YYYYMMDD>/; formats fixed here):
#   the MultiMouseMonitor log (UTF-16LE): events and single reads (apr_reads(): event start + cumulative MsgValue1 gaps);
#   tags.csv: label (T1-T6, C, spare), tag (IdRFID). A replaced tag keeps its label (the label is its role/slot).
#   placement_sheet.csv: ACP_SHEET_COLUMNS, one row per schedule row with a tag in a cage (n_tags > 0), executed or dropped,
#     plus repeats (placement_id <order>r); optional BASELINE/BREAK rows mark idle time; optional column tags. Board = the
#     physical board (a substitute board is mapped by `boards`). Times: PC clock, "%Y-%m-%d %H:%M:%OS", labelled UTC.
#   annotation_movement.csv: ACP_ANNOTATION_COLUMNS, the video times (PC clock) at which the carrier centre crosses each
#     midpoint line (kind "cross", from_reader, to_reader as PositionID or R<k>); arrive/depart rows are kept, not used.
#   annotation_survey.csv: board, sweep, t_pc, x, y: carrier positions along each survey sweep (a row sweep has constant y,
#     a column sweep constant x).
#   Stage 00b tables/board_history.csv of run raqc_20261014_6165adf (content hash pinned): the only Exp9 input.
#
# Analysis. Clapper touches (tag C on board 5, channel 7 = R4) give each placement start and end in log time (acp_align():
# median clapper-minus-sheet offset over matches within 3 s, then one touch per sheet time within 3 s, closest first; a
# time without a touch falls back to the sheet time plus that offset, flagged). Analysed window [start + 5 s, end - 5 s].
# Truth: the nearest measured antenna centre (survey read-zone midpoints, grid fallback); the admissible set = readers
# within 5 grid units of the minimum distance; reader "0" in detection tables is that set (the true reader). Bases: event
# starts and single reads (asr_resolve_ties()), both from the placement's own start (acp_observations(); v1.2: an event in
# progress at the start is an observation at the start in both bases, criteria A04), flip filter over
# the placement's own observations [start, end), carry-forward amv_runs(), grid amv_grid_track() at 0.1 s; unassigned
# before the first observation. Every bootstrap: acp_boot(), stratified cluster resampling, B and seed from the criteria,
# percentile CI; C statistics: 10-s block bootstrap within each period. Group-blind throughout.
#
# Choices where the plan leaves a detail open (each the simplest rule consistent with it; no new threshold):
#  * criteria: decision thresholds and the day-2 thresholds in one pinned file (categories governing/basis/limitation/
#    descriptive/day2); analysis-plan parameters (guards, tolerances) are the ACP_* constants below.
#  * sheet: placement_id = schedule order (+ "r" for a repeat, which supersedes its original); dropped rows are allowed
#    only as drop step 1 (H05/H06 on both boards) or, from schedule v3, drop step 4 (the HSy pairs H11/H12 on both boards);
#    step 2 marks CS rows, step 3 M-visits rows; steps are used in order.
#    An order deviation = an executed first-time row whose rank by start differs from its schedule rank (flag only).
#  * time base: the PC clock is the logger clock; the annotation is already on the PC clock (filmed display offsets
#    applied when annotating), so it is not shifted by the clapper offset.
#  * truth at the C slots from the protocol coordinates (ACP_C_ROLES); house and roof placements at (0, 116) -> R5.
#  * detection: P1s/P10s over the full 1-s/10-s bins of the window; gaps include the window edges (an unread antenna has
#    Gmax = T); q99 over the same gaps (type 7); event splits per min = inner read gaps > 1 s per minute; missed share =
#    within-event gaps on lattice k >= 6. lambda_sat = board median rate at the true reader over ACP_LAMBDA_UNITS.
#  * single-tag static placements of B3 and B11 are those of T1 (P, REF-z, REF-x, H); T0 serves the tag QC (B14).
#  * movement: passes = maximal runs of crossings in one column direction; visit v = the v-th crossing out of the origin
#    reader and the next crossing back; true dwell = time between the two crossings; a visit is kept when a run at the
#    destination overlaps its unit window; latency/tail from the reads inside the crossing's unit window.
#  * basis units: REF-x (and REF-z in D_ctr) = the mean of its replicates per board; strata board x unit type (static,
#    crossing, visit); D_col, D_col(b2), D_col(b4) from one bootstrap; the board ratios resample pairs within unit type
#    (P, H, REF), because a pair spans both boards.
#  * rates in ratios: (reads + 0.5) / T at the admissible set; TR uses the summed reads of the four load tags; MS and Gini
#    use reads without continuity; CS/C4 = exp(mean log per-tag ratio) over T1-T4; SR pools T5 and T6.
#  * B4 precedence: capture/starvation, then collision, then time-sharing, else intermediate. B5 between its thresholds:
#    undecided. B2 on a tie or an undecided basis: reader level only if it holds under both bases.
#  * B10: REF excess SD = sqrt(max(0, var(log REF-x rates) - mean(1 / (n + 0.5)))) per board; drift adjustment subtracts
#    the per-board slope of log REF-x rate on time; only a "difference" call is required to survive it (else undecided).
#    Co-read rate = reads at the true reader with a neighbour co-read per second; informative unless 0 on both boards.
#  * B15 idle time: boards 1, 3 and 5 the whole session (tag C excluded on board 5); a tested board while the other one is
#    in a visit (first start to last end) and during BASELINE/BREAK rows of the sheet. Poisson upper bound at the CI level.
#  * B16: board signature over the session's isolated events, z against the same board's rows of board_history.csv.
#  * day 2: "undecided" competition = B4 intermediate or B6 undecided; P is applied to the thresholds inside the CI only.
#  * v1.2 additions (category added_v1.2): K units are the K crossings (K01) and never enter `units` (D_col, B11, B12); the
#    v1.1 competition statistics use the v1.1 periods only (C0, C1, C4, CS, CE), and the house-lifted periods have their
#    own bootstrap (acp_competition_house(), periods C1, C1o, C4, C4o in that order); HX placements are not in
#    ACP_SINGLE_TYPES (B1, B3, B11, B13 and B10 keep their units). Latency: cell midpoints (A06 grid) at or after the
#    crossing and inside the unit window; a missed transition counts as an infinite latency in the KA5 median. Detection
#    age: from the tag's single reads at any reader of the tested board. KA2 per unit: correct_column / assigned of each
#    placement, REF-x replicates averaged; a placement never assigned gives no value (KA component not obtained if none).
#    The overall verdict K20 does not feed B2 (the resolution keeps the criterion (a) decision).
# ================================================================

ACP_VERSION <- "acp_criteria_v1.2"
ACP_SCHEDULE_VERSION <- "schedule_one_day_v3"
ACP_DIR <- file.path("docs", "rfid_calibration")
ACP_CRITERIA_FILE <- file.path(ACP_DIR, "acp_criteria_v1.2.csv")
ACP_SCHEDULE_FILE <- file.path(ACP_DIR, "schedule_one_day_v3.csv")
# Content sha256 with line endings normalised to LF (acp_content_sha256()): the same in every checkout.
ACP_CRITERIA_SHA256 <- "12c0b2d2e3b0cd61f1b7ea0be64e0e8c1752e167ffb8b69b7c08c400d1aa0500"
ACP_SCHEDULE_SHA256 <- "e61c7d6ad9fd40886f806cf895f640c6bcdceab0aec013b5246f7062bf0f341b"
# The schedule as written by R 4.5.1 on Windows (CRLF bytes; the hash recorded in the protocol).
ACP_SCHEDULE_FILE_SHA256_CRLF <- "a733033215aa43542412d9cd8991c41c0ff4251c77939f237d64337cda02b739"
# Category of the rows added in criteria v1.2 (outside the day-2 rule; criteria G10).
ACP_ADDED_CATEGORY <- "added_v1.2"
# The one Exp9 input (group-blind acquisition reference): Stage 00b board history of run raqc_20261014_6165adf.
ACP_BOARD_HISTORY_RUN <- "raqc_20261014_6165adf"
ACP_BOARD_HISTORY_SHA256 <- "fe743f4d5a8c9a2a147420052675ba484a765e873484d2511d935bab9c463cc0"

ACP_CRITERIA_COLUMNS <- c("rule_id", "criterion", "outcome", "statistic", "comparator", "threshold", "threshold_hi", "category",
                          "feeds", "source")
ACP_COMPARATORS <- c("=", ">=", ">", "<=", "<", "==", "in", "set", "info")
ACP_SCHEDULE_COLUMNS <- c("order", "visit", "block", "board", "position", "x", "y", "orientation", "height", "object", "n_tags",
                          "tags", "seconds", "slot_s", "start_min", "pair_id", "purpose")
ACP_SHEET_COLUMNS <- c("placement_id", "order", "board", "pair_id", "status", "pc_start", "pc_end", "clapper_ok", "drop_step",
                       "deviation")
ACP_ANNOTATION_COLUMNS <- c("placement_id", "kind", "t_pc", "from_reader", "to_reader")
ACP_TIME_FORMAT <- "%Y-%m-%d %H:%M:%OS"   # PC (logger) clock, labelled UTC as everywhere in the pipeline

# Analysis-plan parameters (docs/RFID_CALIBRATION_PILOT.md section 4, function list); not decision thresholds.
ACP_BOARDS <- c(`2` = 2L, `4` = 4L)       # tested boards by role (2 reference, 4 least sensitive) -> physical board
# The only substitutes the protocol allows (role -> physical board): board 3 for board 4 (least sensitive pair), board 1
# for board 2. A substitute is used only through an authorization file (acp_read_substitution()); never silently.
ACP_ALLOWED_SUBSTITUTES <- c(`4` = 3L, `2` = 1L)
ACP_SUBSTITUTION_FILE <- "board_substitution.csv"
ACP_SUBSTITUTION_COLUMNS <- c("role", "physical_board", "reason", "authorized_by", "authorized_on")
ACP_PHASE_A <- c(2L, 3L, 5L, 8L)          # phase group A (R2, R3, R5, R8); group B = R1, R4, R6, R7
ACP_CLAPPER_TAG <- "C"
ACP_CLAPPER_BOARD <- 5L
ACP_CLAPPER_PID <- 4L                     # board 5, channel 7 (antenna 7 = R4)
ACP_TOUCH_GAP_S <- 3                      # clapper reads more than 3 s apart are separate touches
ACP_ALIGN_TOL_S <- 3                      # a sheet time matches a touch within 3 s (after the median offset)
ACP_GUARD_S <- 5                          # analysed window = [start + 5 s, end - 5 s]
ACP_ADMISSIBLE_UNITS <- 5                 # admissible readers: within 5 grid units of the minimum distance
ACP_SPLIT_GAP_S <- 1                      # event splits: inter-read gaps > 1 s (an event closes after ~1 s)
ACP_GAP_COUNTS_S <- c(10, 30, 60)         # per-tag gaps counted in the C periods
ACP_LAMBDA_UNITS <- c("P01", "P02", "P03", "REFz", "H04")   # z centres of lambda_sat
ACP_SINGLE_TYPES <- c("P", "REFz", "REFx", "H", "T0")       # single-tag static placements
ACP_T1_STATIC_TYPES <- c("P", "REFz", "REFx", "H")          # the T1 placements of B3 and B11
ACP_C_PERIODS <- c("C0", "C1", "C4", "CS", "CE")   # the v1.1 periods: B4-B7 use these only
ACP_C_PERIODS_HOUSE_OFF <- c("C1o", "C4o")         # v1.2: the house-lifted periods of every competition run (HX10, HX11)
ACP_BOARD_PAIRS <- c(sprintf("P%02d", 1:15), sprintf("H%02d", 1:6))   # the 21 paired static placements of B10
ACP_HOUSE_PAIRS <- data.frame(pair = c("HCx", "HCz"), on = c("H01", "H03"), off = c("H02", "H04"), stringsAsFactors = FALSE)
# v1.2 house-extension pairs (HX01): house edge towards R6 in x and y, house side towards R1 in y (inside the house, on the
# bedding), and HOx on the floor just outside the house wall towards R6.
ACP_HOUSE_EXT_PAIRS <- data.frame(pair = c("HEx", "HEy", "HSy", "HOx"), on = c("H07", "H09", "H11", "H13"), off = c("H08", "H10", "H12", "H14"),
                                  stringsAsFactors = FALSE)
ACP_HOUSE_RATIO_EXT_CLASSES <- c("HCx", "HCz", "HEx", "HEy", "HSy")   # HX02: the classes inside the house
ACP_HOUSE_EDGE_ON <- c("H07", "H09", "H13")        # KA6: the house placements next to R6 with the house on
ACP_K_PAIRS <- c("K-lower", "K-upper")
# Tags and slots of the competition run (protocol section 6, block C; v1.2 adds the house-lifted periods C1o and C4o, whose
# tags lie in the same slots as in C1 and C4).
ACP_C_ROLES <- data.frame(
  pair_id = c("C0", "C0", "C1", "C1", "C1", rep("C4", 6), rep("CS", 6), "CE", "CE", rep("C1o", 3), rep("C4o", 6)),
  tag_label = c("T5", "T6", "T1", "T5", "T6", "T1", "T2", "T3", "T4", "T5", "T6", "T1", "T2", "T3", "T4", "T5", "T6", "T5", "T6",
                "T1", "T5", "T6", "T1", "T2", "T3", "T4", "T5", "T6"),
  x = c(300, 300, -10, 300, 300, -10, 10, -10, 10, 300, 300, -10, 10, 90, 110, 300, 300, 300, 300, -10, 300, 300, -10, 10, -10, 10, 300, 300),
  y = c(116, 0, 126, 116, 0, 126, 126, 106, 106, 116, 0, 126, 126, 116, 116, 116, 0, 116, 0, 126, 116, 0, 126, 126, 106, 106, 116, 0),
  role = c("sentinel", "sentinel", "load", "sentinel", "sentinel", rep("load", 4), "sentinel", "sentinel", rep("load", 4),
           "sentinel", "sentinel", "sentinel", "sentinel", "load", "sentinel", "sentinel", rep("load", 4), "sentinel", "sentinel"),
  stringsAsFactors = FALSE)
ACP_EPS <- 1e-9

# ---------------------------------------------------------------- criteria and schedule (pinned)

#' sha256 of a text file's content with CRLF/CR normalised to LF (as sxm_content_sha256()).
acp_content_sha256 <- function(path) {
  txt <- paste(sub("\r$", "", readLines(path, warn = FALSE, encoding = "UTF-8")), collapse = "\n")
  digest::digest(txt, algo = "sha256", serialize = FALSE)
}

#' The criteria table (all columns character; attribute sha256). Stops unless the content hash is ACP_CRITERIA_SHA256.
acp_criteria <- function(path = ACP_CRITERIA_FILE) {
  if (!file.exists(path)) stop("No criteria file: ", path, call. = FALSE)
  h <- acp_content_sha256(path)
  if (!identical(h, ACP_CRITERIA_SHA256)) {
    stop("The criteria file differs from the pinned ", ACP_VERSION, " (content sha256 ", h, "); the analysis refuses to run. ",
         "A change is a new criteria version.", call. = FALSE)
  }
  crit <- data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
  if (!identical(names(crit), ACP_CRITERIA_COLUMNS)) stop("Criteria columns differ from ACP_CRITERIA_COLUMNS.", call. = FALSE)
  if (anyDuplicated(crit$rule_id)) stop("Duplicate rule_id in the criteria.", call. = FALSE)
  if (any(!crit$comparator %in% ACP_COMPARATORS)) stop("Unknown comparator in the criteria.", call. = FALSE)
  num <- crit$comparator %in% c("=", ">=", ">", "<=", "<", "==", "in")
  if (anyNA(suppressWarnings(as.numeric(crit$threshold[num]))) ||
      anyNA(suppressWarnings(as.numeric(crit$threshold_hi[crit$comparator == "in"])))) {
    stop("A numeric criteria row has no numeric threshold.", call. = FALSE)
  }
  attr(crit, "sha256") <- h
  attr(crit, "path") <- path
  crit
}

#' One criteria row; stops if the rule does not exist.
acp_rule <- function(crit, id) {
  r <- crit[crit$rule_id == id, ]
  if (nrow(r) != 1L) stop("No criteria rule ", id, call. = FALSE)
  r
}

#' Numeric threshold of a rule.
acp_th <- function(crit, id) {
  v <- suppressWarnings(as.numeric(acp_rule(crit, id)$threshold))
  if (is.na(v)) stop("Rule ", id, " has no numeric threshold.", call. = FALSE)
  v
}

#' Candidate set of a "set" rule (numeric when every member is a number).
acp_set <- function(crit, id) {
  r <- acp_rule(crit, id)
  if (r$comparator != "set") stop("Rule ", id, " is not a set.", call. = FALSE)
  v <- strsplit(r$threshold, "|", fixed = TRUE)[[1]]
  nv <- suppressWarnings(as.numeric(v))
  if (anyNA(nv)) v else nv
}

#' Applies the comparator of rule `id` to x (vectorised); NA never satisfies a condition.
acp_test <- function(crit, id, x) {
  r <- acp_rule(crit, id)
  a <- as.numeric(r$threshold)
  x <- as.numeric(x)
  out <- switch(r$comparator,
                ">=" = x >= a - ACP_EPS, ">" = x > a + ACP_EPS, "<=" = x <= a + ACP_EPS, "<" = x < a - ACP_EPS,
                "==" = abs(x - a) <= ACP_EPS, "in" = x >= a - ACP_EPS & x <= as.numeric(r$threshold_hi) + ACP_EPS,
                stop("Rule ", id, " is not a comparison.", call. = FALSE))
  out & !is.na(out)
}

#' Type of a schedule pair_id: REFz, REFx, T0, P, H, HX (v1.2 house extension H07-H12), M_pass, M_visits, K (v1.2
#' column-boundary steps), C (including the house-lifted periods C1o, C4o), SURVEY or "".
acp_pair_type <- function(pair_id) {
  out <- rep("", length(pair_id))
  out[pair_id == "REFz"] <- "REFz"; out[pair_id == "REFx"] <- "REFx"
  out[grepl("^T0-", pair_id)] <- "T0"; out[grepl("^P[0-9]{2}$", pair_id)] <- "P"; out[grepl("^H0[1-6]$", pair_id)] <- "H"
  out[pair_id %in% c(ACP_HOUSE_EXT_PAIRS$on, ACP_HOUSE_EXT_PAIRS$off)] <- "HX"
  out[grepl("^M-pass", pair_id)] <- "M_pass"; out[grepl("^M-visits", pair_id)] <- "M_visits"; out[pair_id %in% ACP_K_PAIRS] <- "K"
  out[pair_id %in% c(ACP_C_PERIODS, ACP_C_PERIODS_HOUSE_OFF)] <- "C"; out[pair_id == "SURVEY"] <- "SURVEY"
  out
}

#' The seeded schedule (typed; unit_type, analysed, required). Stops unless the content hash is ACP_SCHEDULE_SHA256.
acp_read_schedule <- function(path = ACP_SCHEDULE_FILE) {
  if (!file.exists(path)) stop("No schedule file: ", path, call. = FALSE)
  h <- acp_content_sha256(path)
  if (!identical(h, ACP_SCHEDULE_SHA256)) {
    stop("The schedule differs from the pinned ", ACP_SCHEDULE_VERSION, " (content sha256 ", h, "); the analysis refuses to run.",
         call. = FALSE)
  }
  s <- data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
  if (!identical(names(s), ACP_SCHEDULE_COLUMNS)) stop("Schedule columns differ from ACP_SCHEDULE_COLUMNS.", call. = FALSE)
  num <- function(v) suppressWarnings(as.numeric(ifelse(nzchar(v), v, NA_character_)))
  s[, `:=`(order = as.integer(order), x = num(x), y = num(y), n_tags = as.integer(n_tags), seconds = num(seconds),
           slot_s = num(slot_s), start_min = num(start_min))]
  s[, unit_type := acp_pair_type(pair_id)]
  s[, analysed := board %in% names(ACP_BOARDS) & unit_type %in% c(ACP_SINGLE_TYPES, "HX", "M_pass", "M_visits", "K", "C")]
  s[, required := n_tags > 0L]   # every row with a tag in a cage is executed or dropped
  attr(s, "sha256") <- h
  s[]
}

#' Nominal dwell times (s) of a short-visit row, from its position text ("dwell (s) in order 1,2,5,1,5,2").
acp_visit_dwells <- function(position) {
  m <- regmatches(position, regexpr("in order [0-9,]+", position))
  if (!length(m)) return(numeric())
  as.numeric(strsplit(sub("in order ", "", m), ",", fixed = TRUE)[[1]])
}

#' PC-clock time strings -> POSIX seconds (NA if unreadable).
acp_time <- function(x) as.numeric(as.POSIXct(as.character(x), format = ACP_TIME_FORMAT, tz = "UTC"))

#' Reader labels "R5" or "5" -> PositionID.
acp_reader <- function(x) as.integer(sub("^R", "", toupper(trimws(as.character(x)))))

# ---------------------------------------------------------------- sheet, session, clapper, windows

#' Reads and checks a placement sheet (acp_check_sheet()).
#' An explicit board-substitution authorization (raw day folder, ACP_SUBSTITUTION_FILE; columns ACP_SUBSTITUTION_COLUMNS,
#' one row per substituted role). Only the substitutes of ACP_ALLOWED_SUBSTITUTES are accepted, each with a reason, the
#' person who authorized it and the date; anything else stops the analysis. Returns the board mapping (ACP_BOARDS with the
#' substitutions applied) with the authorization as attribute "substitution".
acp_read_substitution <- function(path) {
  s <- data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
  if (!identical(names(s), ACP_SUBSTITUTION_COLUMNS)) stop("The board substitution file must have exactly the columns: ",
                                                          paste(ACP_SUBSTITUTION_COLUMNS, collapse = ", "), call. = FALSE)
  s[, (names(s)) := lapply(.SD, trimws)]
  ok_role <- s$role %in% names(ACP_ALLOWED_SUBSTITUTES)
  ok_board <- ok_role & suppressWarnings(as.integer(s$physical_board)) == unname(ACP_ALLOWED_SUBSTITUTES[s$role])
  ok_date <- !is.na(as.Date(s$authorized_on, format = "%Y-%m-%d"))
  if (!nrow(s) || anyDuplicated(s$role) || !all(ok_role) || !all(ok_board %in% TRUE) || !all(nzchar(s$reason)) ||
      !all(nzchar(s$authorized_by)) || !all(ok_date)) {
    stop("Board substitution not authorized: only ", paste(sprintf("board %d for role %s", ACP_ALLOWED_SUBSTITUTES, names(ACP_ALLOWED_SUBSTITUTES)),
         collapse = " and "), " are allowed, each with a reason, authorized_by and authorized_on (YYYY-MM-DD). Abort and reschedule.", call. = FALSE)
  }
  boards <- ACP_BOARDS
  boards[s$role] <- as.integer(s$physical_board)
  attr(boards, "substitution") <- s
  boards
}

acp_read_sheet <- function(path, schedule, boards = ACP_BOARDS) {
  acp_check_sheet(data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8"), schedule, boards)
}

#' Checks a placement sheet against the schedule. sheet: ACP_SHEET_COLUMNS (character); boards: tested roles -> physical
#' boards. Returns list(sheet, problems (check, severity error/flag, placement_id, detail), ok = no error). The sheet gains
#' the schedule attributes, numeric times (pc_start_t, pc_end_t), board (role), phys_board, repeat_of, superseded, use.
acp_check_sheet <- function(sheet, schedule, boards = ACP_BOARDS) {
  sh <- data.table::copy(data.table::as.data.table(sheet))
  miss <- setdiff(ACP_SHEET_COLUMNS, names(sh))
  if (length(miss)) stop("The placement sheet lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  for (cc in names(sh)) data.table::set(sh, j = cc, value = as.character(sh[[cc]]))
  prob <- list()
  add <- function(check, severity, id, detail) if (length(id)) prob[[length(prob) + 1L]] <<- data.table::data.table(
    check = check, severity = severity, placement_id = as.character(id), detail = detail)
  if (!"tags" %in% names(sh)) sh[, tags := ""]
  sh[, `:=`(order = suppressWarnings(as.integer(order)), drop_step = suppressWarnings(as.integer(ifelse(nzchar(drop_step), drop_step, "0"))),
            pc_start_t = acp_time(pc_start), pc_end_t = acp_time(pc_end))]
  sh[, repeat_of := ifelse(grepl("r$", placement_id), sub("r$", "", placement_id), NA_character_)]
  add("duplicate_id", "error", unique(sh$placement_id[duplicated(sh$placement_id)]), "placement_id used twice")
  add("bad_id", "error", sh[is.na(repeat_of) & placement_id != as.character(order)]$placement_id,
      "placement_id must be the schedule order (plus r for a repeat)")
  add("bad_id", "error", sh[!is.na(repeat_of) & repeat_of != as.character(order)]$placement_id, "a repeat keeps the order of its original")
  add("unknown_order", "error", sh[!order %in% schedule$order]$placement_id, "order not in the schedule")
  add("bad_status", "error", sh[!status %in% c("executed", "dropped")]$placement_id, "status must be executed or dropped")
  sc <- schedule[, .(order, visit, block, sched_board = board, position, x, y, orientation, height, object, n_tags,
                     sched_tags = tags, seconds, slot_s, start_min, sched_pair = pair_id, unit_type, analysed, required)]
  sh <- merge(sh, sc, by = "order", all.x = TRUE, sort = FALSE)
  add("pair_mismatch", "error", sh[!is.na(sched_pair) & pair_id != sched_pair]$placement_id, "pair_id differs from the schedule")
  sh[, board_role := ifelse(sched_board %in% names(boards), suppressWarnings(as.integer(sched_board)), NA_integer_)]
  sh[, phys_board := ifelse(sched_board %in% names(boards), unname(boards[sched_board]), NA_integer_)]
  sh[, board_n := suppressWarnings(as.integer(board))]
  add("board_mismatch", "error", sh[!is.na(phys_board) & (is.na(board_n) | board_n != phys_board)]$placement_id,
      "board differs from the (substituted) schedule board")
  sh[, tags := ifelse(nzchar(tags), tags, sched_tags)]
  sh[, tag_label := ifelse(grepl("^T[0-9]+$|^C$", tags), tags, NA_character_)]
  ex <- sh$status == "executed"
  add("bad_times", "error", sh[ex & !(is.finite(pc_start_t) & is.finite(pc_end_t) & pc_end_t > pc_start_t)]$placement_id,
      "unreadable or reversed PC-clock times")
  # every required schedule row executed or dropped
  done <- sh[is.na(repeat_of)]$order
  add("missing", "error", as.character(setdiff(schedule[required == TRUE]$order, done)), "neither executed nor dropped")
  # dropped rows and the fixed drop order (always applied to both boards); step 4 (schedule v3) drops the HSy pairs
  add("drop_outside_order", "error", sh[status == "dropped" & !(sched_pair %in% c("H05", "H06") & drop_step == 1L) &
                                          !(sched_pair %in% c("H11", "H12") & drop_step == 4L)]$placement_id,
      "only the roof pairs H05/H06 (drop step 1) and the HSy pairs H11/H12 (drop step 4) may be dropped")
  add("drop_outside_order", "error", sh[status == "executed" & drop_step %in% c(1L, 4L)]$placement_id, "drop steps 1 and 4 drop rows")
  add("drop_outside_order", "error", sh[drop_step == 2L & sched_pair != "CS"]$placement_id, "drop step 2 shortens CS only")
  add("drop_outside_order", "error", sh[drop_step == 3L & !grepl("^M-visits", sched_pair)]$placement_id, "drop step 3 shortens visits only")
  add("drop_outside_order", "error", sh[drop_step > 4L | drop_step < 0L]$placement_id, "unknown drop step")
  steps <- sort(unique(sh$drop_step[sh$drop_step > 0L]))
  # steps in order; step 3 may be skipped (at the break of schedules v2 and v3 only board 4's second visits remain, so step
  # 3 cannot be applied on both boards), so step 4 may follow step 2
  if (length(steps) && !(identical(as.integer(steps), seq_len(max(steps))) || identical(as.integer(steps), c(1L, 2L, 4L)))) {
    add("drop_order", "error", "all", "drop steps must be used in order (1, then 2, then 3 if it can be applied on both boards, then 4)")
  }
  if (3L %in% steps) {
    vh <- unique(sh[drop_step == 3L]$sched_pair)
    add("drop_one_board", "error", as.character(setdiff(schedule[pair_id %in% vh]$order, sh[drop_step == 3L]$order)),
        "drop step 3 shortens a visits half on both boards")
  }
  if (1L %in% steps) {
    roof <- schedule[pair_id %in% c("H05", "H06")]$order
    add("drop_one_board", "error", as.character(setdiff(roof, sh[status == "dropped" & drop_step == 1L]$order)),
        "drop step 1 drops H05 and H06 on both boards")
  }
  if (4L %in% steps) {
    hsy <- schedule[pair_id %in% c("H11", "H12")]$order
    add("drop_one_board", "error", as.character(setdiff(hsy, sh[status == "dropped" & drop_step == 4L]$order)),
        "drop step 4 drops H11 and H12 on both boards")
  }
  if (2L %in% steps) add("drop_one_board", "error", as.character(setdiff(schedule[pair_id == "CS"]$order, sh[drop_step == 2L]$order)),
                         "drop step 2 shortens CS on both boards")
  # repeats supersede their originals
  orig <- sh[!is.na(repeat_of)]$repeat_of
  add("repeat_without_original", "error", sh[!is.na(repeat_of) & !repeat_of %in% sh$placement_id]$placement_id, "no original row")
  sh[, superseded := placement_id %in% orig[orig %in% sh[!is.na(repeat_of) & status == "executed"]$repeat_of]]
  sh[, use := status == "executed" & !superseded]
  # order deviations among first-time executed rows
  x <- sh[ex & is.na(repeat_of) & is.finite(pc_start_t)][order(pc_start_t)]
  add("order_deviation", "flag", x$placement_id[x$order != sort(x$order)], "executed out of schedule order")
  # no two placements overlap on one board
  for (b in unname(boards)) {
    on_b <- vapply(strsplit(sh$sched_board, ",", fixed = TRUE), function(z) any(z %in% names(boards)[boards == b]), logical(1))
    y <- sh[ex & on_b & is.finite(pc_start_t)][order(pc_start_t)]
    if (nrow(y) > 1L) {
      k <- which(y$pc_start_t[-1] < y$pc_end_t[-nrow(y)])
      add("overlap", "error", y$placement_id[k + 1L], paste("overlaps the previous placement on board", b))
    }
  }
  problems <- if (length(prob)) data.table::rbindlist(prob) else
    data.table::data.table(check = character(), severity = character(), placement_id = character(), detail = character())
  list(sheet = sh[], problems = problems, ok = !any(problems$severity == "error"))
}

#' Tag table: label (T1-T6, C, spare), tag (IdRFID); both unique.
acp_read_tags <- function(path) {
  tm <- data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
  if (!all(c("label", "tag") %in% names(tm))) stop("tags.csv needs columns label and tag.", call. = FALSE)
  if (anyDuplicated(tm$label) || anyDuplicated(tm$tag)) stop("tags.csv maps a label or a tag twice.", call. = FALSE)
  tm[, .(label, tag)]
}

#' One raw log of the day: events (rfid_read_raw_log(keep_read_offsets = TRUE), row order kept: reads$event indexes it),
#' reads (apr_reads(): tag, t_ms, t (s), board, PositionID, gx, gy, event, read), tag_label on both (from tag_map; unmapped
#' tags "unknown_<id>"), sysmsg (amv_system_messages()), device_check (aqc_device_check()), n_sessions (start messages).
acp_read_session <- function(path, tag_map = NULL) {
  lg <- rfid_read_raw_log(path, keep_read_offsets = TRUE)
  ev <- data.table::as.data.table(lg$events)
  rd <- data.table::as.data.table(apr_reads(ev))
  rd[, t := t_ms / 1000]
  lab <- function(tag) {
    l <- if (is.null(tag_map)) rep(NA_character_, length(tag)) else tag_map$label[match(tag, tag_map$tag)]
    ifelse(is.na(l), paste0("unknown_", tag), l)
  }
  ev[, tag_label := lab(tag)]
  rd[, tag_label := lab(tag)]
  sm <- amv_system_messages(path)
  list(path = path, events = ev, reads = rd, sysmsg = sm, devices = lg$devices, device_check = aqc_device_check(lg$devices),
       n_sessions = sum(sm$SystemMsg == "start"), clock_fold_rows_dropped = lg$clock_fold_rows_dropped)
}

#' Clapper touches: reads of tag_C on `board` at `pid`, grouped where consecutive reads lie at most gap_s apart.
#' Returns data.table(touch, on, off, n_reads).
acp_clapper_touches <- function(reads, tag_C = ACP_CLAPPER_TAG, board = ACP_CLAPPER_BOARD, pid = ACP_CLAPPER_PID, gap_s = ACP_TOUCH_GAP_S) {
  tc <- tag_C; bd <- board; pp <- pid
  t <- sort(reads[tag_label == tc & reads$board == bd & PositionID == pp]$t)
  if (!length(t)) return(data.table::data.table(touch = integer(), on = numeric(), off = numeric(), n_reads = integer()))
  grp <- cumsum(c(TRUE, diff(t) > gap_s))
  data.table::data.table(touch = seq_len(max(grp)), on = as.numeric(tapply(t, grp, min)), off = as.numeric(tapply(t, grp, max)),
                         n_reads = tabulate(grp))
}

#' Placement start and end in log time. The median clapper-minus-sheet offset is taken over sheet times with a touch
#' within tol_s; each sheet time (executed rows) then gets the closest unused touch within tol_s of sheet + offset, or
#' falls back to sheet + offset (source "sheet"). Returns list(placements: sheet rows with start, end, start_source,
#' end_source, start_offset_s, end_offset_s; touches: with matched; offset_s; n_matched).
acp_align <- function(sheet, touches, tol_s = ACP_ALIGN_TOL_S) {
  s <- data.table::copy(data.table::as.data.table(sheet))[status == "executed"]
  tm <- rbind(s[, .(placement_id, side = "start", pc = pc_start_t)], s[, .(placement_id, side = "end", pc = pc_end_t)])
  on <- sort(touches$on)
  d0 <- rep(NA_real_, nrow(tm))
  if (length(on)) {
    i <- findInterval(tm$pc, on)
    lo <- on[pmax(i, 1L)] - tm$pc; hi <- on[pmin(i + 1L, length(on))] - tm$pc
    d0 <- ifelse(abs(lo) <= abs(hi), lo, hi)
  }
  ok0 <- !is.na(d0) & abs(d0) <= tol_s
  offset <- if (any(ok0)) stats::median(d0[ok0]) else 0
  cand <- data.table::rbindlist(lapply(seq_len(nrow(tm)), function(i) {
    j <- which(abs(touches$on - (tm$pc[i] + offset)) <= tol_s)
    if (length(j)) data.table::data.table(i = i, j = j, d = abs(touches$on[j] - (tm$pc[i] + offset)))
  }))
  match_j <- rep(NA_integer_, nrow(tm)); used <- logical(nrow(touches))
  if (nrow(cand)) {
    data.table::setorder(cand, d, i)
    for (k in seq_len(nrow(cand))) if (is.na(match_j[cand$i[k]]) && !used[cand$j[k]]) { match_j[cand$i[k]] <- cand$j[k]; used[cand$j[k]] <- TRUE }
  }
  tm[, `:=`(log_t = ifelse(is.na(match_j), pc + offset, touches$on[match_j]), source = ifelse(is.na(match_j), "sheet", "clapper"),
            offset_s = touches$on[match_j] - pc)]
  st <- tm[side == "start", .(placement_id, start = log_t, start_source = source, start_offset_s = offset_s)]
  en <- tm[side == "end", .(placement_id, end = log_t, end_source = source, end_offset_s = offset_s)]
  s <- merge(merge(s, st, by = "placement_id", sort = FALSE), en, by = "placement_id", sort = FALSE)
  tt <- data.table::copy(data.table::as.data.table(touches))[, matched := used]
  list(placements = s[order(start)], touches = tt, offset_s = offset, n_matched = sum(!is.na(match_j)))
}

#' Analysed windows: ws = start + guard_s, we = end - guard_s, T = we - ws (seconds); board = the role (2/4).
acp_windows <- function(aligned, guard_s = ACP_GUARD_S) {
  w <- data.table::copy(data.table::as.data.table(aligned))
  if ("board" %in% names(w)) data.table::setnames(w, "board", "sheet_board")
  w[, `:=`(board = board_role, ws = start + guard_s, we = end - guard_s)]
  w[, T := we - ws]
  w[]
}

#' Reads of the survey sweeps -> measured antenna centres: per board and reader, the midpoint of the read zone of `tag`
#' along the row sweep through the reader (cx) and along the column sweep (cy); the printed grid where there is none.
#' survey: data.table(board (physical), sweep, t (log s), x, y).
acp_survey_centres <- function(reads, survey, boards = unname(ACP_BOARDS), tag = "T1") {
  out <- data.table::CJ(board = as.integer(boards), PositionID = 1:8)
  out[, `:=`(cx = RFID_GRID_X[PositionID], cy = RFID_GRID_Y[PositionID], source_x = "grid", source_y = "grid", reads_x = 0L, reads_y = 0L)]
  if (is.null(survey) || !nrow(survey)) return(out[])
  sv <- data.table::as.data.table(survey)[order(board, sweep, t)]
  sw <- sv[, .(t0 = min(t), t1 = max(t), x0 = min(x), x1 = max(x), y0 = min(y), y1 = max(y)), by = .(board, sweep)]
  sw[, axis := ifelse(y0 == y1 & x0 != x1, "x", ifelse(x0 == x1 & y0 != y1, "y", NA_character_))]
  tg <- tag
  zone <- function(b, p, ax) {
    s <- if (ax == "x") sw[board == b & axis == "x" & y0 == RFID_GRID_Y[p]] else sw[board == b & axis == "y" & x0 == RFID_GRID_X[p]]
    pos <- unlist(lapply(seq_len(nrow(s)), function(k) {
      r <- reads[tag_label == tg & reads$board == b & PositionID == p & t >= s$t0[k] & t <= s$t1[k]]$t
      a <- sv[board == b & sweep == s$sweep[k]]
      if (length(r)) stats::approx(a$t, a[[ax]], xout = r, ties = "ordered")$y else numeric()
    }))
    pos[is.finite(pos)]
  }
  for (k in seq_len(nrow(out))) {
    zx <- zone(out$board[k], out$PositionID[k], "x"); zy <- zone(out$board[k], out$PositionID[k], "y")
    if (length(zx)) data.table::set(out, k, c("cx", "source_x", "reads_x"), list((min(zx) + max(zx)) / 2, "survey", length(zx)))
    if (length(zy)) data.table::set(out, k, c("cy", "source_y", "reads_y"), list((min(zy) + max(zy)) / 2, "survey", length(zy)))
  }
  out[]
}

# ---------------------------------------------------------------- truth

#' Bitmask of a set of readers (PositionID 1-8).
acp_mask <- function(pids) { p <- unique(as.integer(pids[!is.na(pids)])); if (length(p)) as.integer(sum(2^(p - 1L))) else 0L }
#' Readers of a bitmask.
acp_mask_readers <- function(mask) which(bitwAnd(as.integer(mask), as.integer(2^(0:7))) > 0L)
#' Whether reader pos (0 = none) is in mask (vectorised over both).
acp_in_mask <- function(mask, pos) pos > 0L & bitwAnd(as.integer(mask), as.integer(2^(pmax(pos, 1L) - 1L))) > 0L
#' Column distance of pos to the nearest column of the readers in mask (NA for pos 0).
acp_col_distance <- function(mask, pos) {
  cp <- amv_column(pmax(pos, 1L)); d <- rep(Inf, length(pos))
  for (cc in 1:4) {
    has <- bitwAnd(as.integer(mask), as.integer(2^(cc - 1L) + 2^(cc + 3L))) > 0L
    d <- ifelse(has, pmin(d, abs(cp - cc)), d)
  }
  d[pos == 0L] <- NA_real_
  d
}
#' Whether reader pos lies in a row (y) of the readers in mask.
acp_row_ok <- function(mask, pos) pos > 0L & bitwAnd(as.integer(mask), ifelse(pos <= 4L, 15L, 240L)) > 0L

#' Phase pairing of two readers: "within" (same phase group) or "cross".
acp_phase_pairing <- function(p, q) ifelse((p %in% ACP_PHASE_A) == (q %in% ACP_PHASE_A), "within", "cross")

#' Truth of positions on a board's measured centres: true_pid (nearest), admissible set (within ACP_ADMISSIBLE_UNITS of the
#' minimum distance; mask adm), its size, readers, true column, boundary ("R5|R6" for two readers), geometry and phase
#' pairing of the two. pos: data.table(phys_board, x, y); centres: acp_survey_centres().
acp_position_truth <- function(pos, centres) {
  out <- data.table::rbindlist(lapply(seq_len(nrow(pos)), function(i) {
    cen <- centres[centres$board == pos$phys_board[i]][order(PositionID)]
    if (nrow(cen) != 8L || !is.finite(pos$x[i]) || !is.finite(pos$y[i])) {
      return(data.table::data.table(true_pid = NA_integer_, adm = NA_integer_, n_adm = NA_integer_, adm_readers = NA_character_))
    }
    d <- sqrt((cen$cx - pos$x[i])^2 + (cen$cy - pos$y[i])^2)
    a <- cen$PositionID[d <= min(d) + ACP_ADMISSIBLE_UNITS]
    data.table::data.table(true_pid = cen$PositionID[which.min(d)], adm = acp_mask(a), n_adm = length(a), adm_readers = paste(a, collapse = "|"))
  }))
  out[, true_col := amv_column(pmax(true_pid, 1L))]
  two <- !is.na(out$n_adm) & out$n_adm == 2L
  p <- q <- rep(NA_integer_, nrow(out))
  if (any(two)) { rr <- lapply(out$adm[two], acp_mask_readers); p[two] <- vapply(rr, `[`, 0L, 1L); q[two] <- vapply(rr, `[`, 0L, 2L) }
  out[, `:=`(boundary = ifelse(two, paste0("R", p, "|R", q), ""),
             geometry = ifelse(two, as.character(amv_move_geometry(pmax(p, 1L), pmax(q, 1L))), NA_character_),
             phase_pairing = ifelse(two, acp_phase_pairing(p, q), NA_character_))]
  out[]
}

#' Truth per static placement x tag: the single-tag placements (and the v1.2 house-extension placements HX) at their
#' schedule position, the C periods per tag at the protocol slots (roles). windows: acp_windows() rows; centres:
#' acp_survey_centres().
acp_truth <- function(windows, centres, roles = ACP_C_ROLES) {
  w <- windows[use == TRUE & unit_type %in% c(ACP_SINGLE_TYPES, "HX", "C")]
  single <- w[unit_type != "C", .(placement_id, board, phys_board, pair_id, unit_type, tag_label, x, y, role = "single")]
  multi <- merge(w[unit_type == "C", .(placement_id, board, phys_board, pair_id, unit_type)], data.table::as.data.table(roles),
                 by = "pair_id", allow.cartesian = TRUE)
  tr <- rbind(single, multi, use.names = TRUE)
  if (!nrow(tr)) return(cbind(tr, acp_position_truth(tr[, .(phys_board, x, y)], centres)))
  cbind(tr, acp_position_truth(tr[, .(phys_board, x, y)], centres))
}

#' Movement annotation: crossings (and arrive/depart rows) with log times t; from/to as PositionID. Returns list(annotation,
#' problems). Checks: kind, readable times, neighbouring readers at a crossing, continuity (each crossing starts where the
#' previous one ended), increasing times within a placement, that every movement placement of the sheet (M and, from
#' schedule v3, K) has crossings, and that every crossing of a K placement changes column.
acp_check_annotation <- function(annotation, sheet = NULL) {
  a <- data.table::copy(data.table::as.data.table(annotation))
  miss <- setdiff(ACP_ANNOTATION_COLUMNS, names(a))
  if (length(miss)) stop("The movement annotation lacks column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  a[, `:=`(placement_id = as.character(placement_id), t = acp_time(t_pc), from_reader = acp_reader(from_reader), to_reader = acp_reader(to_reader))]
  prob <- list()
  add <- function(check, id, detail) if (length(id)) prob[[length(prob) + 1L]] <<- data.table::data.table(check = check, severity = "error",
                                                                                                        placement_id = as.character(id), detail = detail)
  add("bad_kind", unique(a[!kind %in% c("cross", "arrive", "depart")]$placement_id), "kind must be cross, arrive or depart")
  add("bad_time", unique(a[!is.finite(t)]$placement_id), "unreadable time")
  cr <- a[kind == "cross"]
  nb <- aqc_neighbours()
  add("not_neighbours", unique(cr[!paste(from_reader, to_reader) %in% paste(nb$k, nb$j)]$placement_id), "a crossing between non-neighbours")
  for (p in unique(cr$placement_id)) {
    x <- cr[placement_id == p][order(t)]
    if (nrow(x) > 1L && any(x$to_reader[-nrow(x)] != x$from_reader[-1])) add("discontinuous", p, "a crossing does not start where the previous one ended")
    if (anyDuplicated(x$t)) add("duplicate_time", p, "two crossings at one time")
  }
  if (!is.null(sheet)) {
    mv <- sheet[use == TRUE & unit_type %in% c("M_pass", "M_visits", "K")]$placement_id
    add("no_crossings", setdiff(mv, cr$placement_id), "movement placement without annotated crossings")
    kp <- sheet[use == TRUE & unit_type == "K"]$placement_id
    kc <- cr[placement_id %in% kp & is.finite(from_reader) & is.finite(to_reader)]
    add("k_within_column", unique(kc[amv_column(pmax(from_reader, 1L)) == amv_column(pmax(to_reader, 1L))]$placement_id),
        "a crossing of a K placement does not change column")
  }
  problems <- if (length(prob)) data.table::rbindlist(prob) else
    data.table::data.table(check = character(), severity = character(), placement_id = character(), detail = character())
  list(annotation = a[], problems = problems, ok = !nrow(problems))
}

#' Reads and checks a movement annotation file.
acp_read_annotation <- function(path, sheet = NULL) {
  acp_check_annotation(data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8"), sheet)
}

#' Survey annotation file -> data.table(board, sweep, t, x, y).
acp_read_survey <- function(path) {
  s <- data.table::fread(path, colClasses = "character", na.strings = NULL, encoding = "UTF-8")
  need <- c("board", "sweep", "t_pc", "x", "y")
  if (!all(need %in% names(s))) stop("The survey annotation needs columns ", paste(need, collapse = ", "), call. = FALSE)
  s[, .(board = as.integer(board), sweep = as.character(sweep), t = acp_time(t_pc), x = as.numeric(x), y = as.numeric(y))]
}

#' True reader of each movement placement on a grid of `step` s over its analysed window (cells as amv_grid_track()):
#' the reader before the first crossing is its from_reader, after crossing k its to_reader; cells within `guard` s of a
#' crossing are not evaluated. Returns data.table(placement_id, tag_label, cell, u (log s), true_pid, adm, eval).
acp_movement_truth <- function(annotation, windows, step = 0.1, guard = 0.2) {
  cr <- annotation[kind == "cross"]
  data.table::rbindlist(lapply(unique(cr$placement_id), function(p) {
    w <- windows[placement_id == p & use == TRUE]
    if (nrow(w) != 1L) return(NULL)
    x <- cr[placement_id == p][order(t)]
    u <- w$ws + seq(step / 2, w$we - w$ws, by = step)
    k <- findInterval(u, x$t)
    tp <- ifelse(k == 0L, x$from_reader[1], x$to_reader[pmax(k, 1L)])
    dprev <- ifelse(k > 0L, u - x$t[pmax(k, 1L)], Inf); dnext <- ifelse(k < nrow(x), x$t[pmin(k + 1L, nrow(x))] - u, Inf)
    data.table::data.table(placement_id = p, tag_label = w$tag_label, cell = seq_along(u), u = u, true_pid = as.integer(tp),
                           adm = as.integer(2^(tp - 1L)), eval = pmin(dprev, dnext) > guard)
  }))
}

#' Basis units of the movement placements. Crossings of M-pass rows: [t - half_s, t + half_s] truncated halfway to the
#' neighbouring crossings, cluster = pass (a maximal run of crossings in one column direction). Short visits of M-visits
#' rows: the v-th crossing out of the origin reader and the next crossing back, [out - margin_s, back + margin_s],
#' cluster = the visit. Unit windows are clipped to the analysed window. Returns data.table(unit_id, placement_id, board,
#' unit_type (crossing/visit), cluster, us, ue, from, to, t_cross, t_back, speed, visit, dwell_nominal, boundary_class).
acp_movement_units <- function(annotation, windows, half_s = 3, margin_s = 3) {
  cr <- annotation[kind == "cross"]
  data.table::rbindlist(lapply(unique(cr$placement_id), function(p) {
    w <- windows[placement_id == p & use == TRUE]
    if (nrow(w) != 1L || !w$unit_type %in% c("M_pass", "M_visits")) return(NULL)
    x <- cr[placement_id == p][order(t)]
    n <- nrow(x)
    if (w$unit_type == "M_pass") {
      dirn <- sign(amv_column(x$to_reader) - amv_column(x$from_reader))
      pass <- cumsum(c(TRUE, dirn[-1] != dirn[-n]))
      prev_mid <- c(-Inf, (x$t[-n] + x$t[-1]) / 2); next_mid <- c((x$t[-n] + x$t[-1]) / 2, Inf)
      return(data.table::data.table(unit_id = paste0(p, "_c", seq_len(n)), placement_id = p, board = w$board, unit_type = "crossing",
                                    cluster = paste0(p, "_pass", pass), us = pmax(x$t - half_s, prev_mid, w$ws), ue = pmin(x$t + half_s, next_mid, w$we),
                                    from = x$from_reader, to = x$to_reader, t_cross = x$t, t_back = NA_real_,
                                    speed = if (grepl("slow", w$pair_id)) "slow" else "fast", visit = NA_integer_, dwell_nominal = NA_real_,
                                    boundary_class = acp_phase_pairing(x$from_reader, x$to_reader)))
    }
    origin <- x$from_reader[1]
    out_i <- which(x$from_reader == origin)
    dw <- acp_visit_dwells(w$position)
    data.table::rbindlist(lapply(seq_along(out_i), function(v) {
      i <- out_i[v]
      if (i == n || x$to_reader[i + 1L] != origin) return(NULL)   # incomplete visit: no crossing back
      data.table::data.table(unit_id = paste0(p, "_v", v), placement_id = p, board = w$board, unit_type = "visit", cluster = paste0(p, "_v", v),
                             us = max(x$t[i] - margin_s, w$ws), ue = min(x$t[i + 1L] + margin_s, w$we), from = origin, to = x$to_reader[i],
                             t_cross = x$t[i], t_back = x$t[i + 1L], speed = "visit", visit = v,
                             dwell_nominal = if (v <= length(dw)) dw[v] else NA_real_, boundary_class = acp_phase_pairing(origin, x$to_reader[i]))
    }))
  }), fill = TRUE)
}

# ---------------------------------------------------------------- detection and reconstruction

#' Detection quantities of one tag at one reader in [ws, we): reads, r, P1s, P10s, within-event lattice, gaps.
acp_detect_one <- function(t, t_ms, event, read, ws, we, missed_k, split_gap_s = ACP_SPLIT_GAP_S) {
  T <- we - ws; n <- length(t)
  o <- order(t); t <- t[o]; t_ms <- t_ms[o]; event <- event[o]; read <- read[o]
  nb1 <- floor(T + 1e-6); nb10 <- floor(T / 10 + 1e-6)
  b1 <- floor(t - ws) + 1; b10 <- floor((t - ws) / 10) + 1
  g <- diff(c(ws, t, we))
  inner <- diff(t)
  oe <- order(event, read)
  same <- if (n > 1) event[oe][-1] == event[oe][-n] & read[oe][-1] == read[oe][-n] + 1L else logical(0)
  wg <- if (n > 1) diff(t_ms[oe])[same] else numeric(0)
  la <- aqc_lattice(wg)
  data.table::data.table(T = T, reads = n, r = n / T, P1s = if (nb1 > 0) length(unique(b1[b1 <= nb1])) / nb1 else NA_real_,
                         P10s = if (nb10 > 0) length(unique(b10[b10 <= nb10])) / nb10 else NA_real_,
                         gaps_n = la$gaps, mean_gap_ms = la$mean_gap_ms, lattice_k1 = la$lattice_k1,
                         missed_share = if (la$gaps > 0) sum(unlist(la[paste0("lattice_k", missed_k:9)])) else NA_real_,
                         gmax = max(g), q99_gap = stats::quantile(g, 0.99, names = FALSE, type = 7),
                         splits_per_min = sum(inner > split_gap_s) / (T / 60))
}

#' E0/E1 per placement x tag x reader (PositionID 1-8, and 0 = the admissible set, the true reader) for the single-tag
#' static placements: reads, r, P1s, P10s, e (= r / lambda_sat of the board, when given), within-event lattice and missed
#' share (lattice k >= B3c), longest and q99 gap, event splits per min; class (B1) at reader 0. types: the unit types
#' (ACP_SINGLE_TYPES; the v1.2 house-extension placements are computed apart with types = "HX").
acp_detection <- function(reads, windows, truth, crit, lambda_sat = NULL, types = ACP_SINGLE_TYPES) {
  missed_k <- acp_th(crit, "B3c")
  tr <- merge(truth[unit_type %in% types], windows[, .(placement_id, ws, we, start)], by = "placement_id")
  out <- data.table::rbindlist(lapply(seq_len(nrow(tr)), function(i) {
    x <- tr[i]; tg <- x$tag_label
    r <- reads[tag_label == tg & board == x$phys_board & t >= x$ws & t < x$we]
    adm <- acp_mask_readers(x$adm)
    rows <- lapply(0:8 * 1L, function(k) {
      rk <- if (k == 0L) r[PositionID %in% adm] else r[PositionID == k]
      cbind(data.table::data.table(PositionID = k), acp_detect_one(rk$t, rk$t_ms, rk$event, rk$read, x$ws, x$we, missed_k))
    })
    cbind(x[, .(placement_id, board, phys_board, pair_id, unit_type, tag_label, true_pid, adm, n_adm, adm_readers, boundary,
                geometry, phase_pairing, ws, we, start)], data.table::rbindlist(rows))
  }))
  if (!nrow(out)) return(out)
  out[, is_true := PositionID == 0L]
  out[, class := ifelse(PositionID != 0L, NA_character_, ifelse(acp_test(crit, "B1a", P1s), "reliable",
                                                              ifelse(acp_test(crit, "B1b", P1s), "unread", "border")))]
  if (!is.null(lambda_sat)) out <- merge(out, lambda_sat[, .(board, lambda_sat)], by = "board", all.x = TRUE)[, e := r / lambda_sat]
  out[]
}

#' lambda_sat per board: the median own-antenna (true reader) rate over the z centres ACP_LAMBDA_UNITS.
acp_lambda_sat <- function(det) det[PositionID == 0L & pair_id %in% ACP_LAMBDA_UNITS, .(lambda_sat = stats::median(r), n = .N), by = board]

#' Observations of one tag on one (physical) board in [from, to): event starts (basis "event") or single reads, one per
#' millisecond (asr_resolve_ties(); basis "read"). Seeding (criteria v1.2, A04): an event of the tag on the board that
#' started before `from` and still has reads at or after it (in progress at the placement start) is an observation at
#' `from` in both bases (the most recent such event; ties by file order). Without it a tag placed before the start touch
#' and read continuously has no event start in its placement, and the event basis stays unassigned for the whole
#' placement. Returns data.table(t, pid) in time order.
acp_observations <- function(events, reads, tag, board, from, to, basis = c("event", "read")) {
  basis <- match.arg(basis); tg <- tag; bd <- board
  rr <- reads[tag_label == tg & reads$board == bd & t_ms >= round(from * 1000) & t_ms < round(to * 1000)]
  ip <- unique(rr$event)
  ip <- ip[events$t[ip] < from]
  seed <- if (length(ip)) {
    e <- events[ip][order(t, file_order)]
    data.table::data.table(t = from, pid = as.integer(e$PositionID[nrow(e)]))
  } else data.table::data.table(t = numeric(), pid = integer())
  if (basis == "event") {
    x <- events[tag_label == tg & events$board == bd & t >= from & t < to]
    x <- x[order(t, file_order)]
    return(rbind(seed, data.table::data.table(t = x$t, pid = as.integer(x$PositionID))))
  }
  x <- rr[order(t_ms)]
  o <- asr_resolve_ties(x$t_ms, x$PositionID)
  rbind(seed, data.table::data.table(t = o$t_ms / 1000, pid = as.integer(o$pid)))
}

#' Assigned reader of one window on a grid of `step` s, per basis x tau: observations from the placement's own start
#' (window$start) to its end, the flip filter over them, carry-forward (amv_runs()), amv_grid_track(); 0 = unassigned.
#' window: one row with placement_id, tag_label, phys_board, start, end, ws, we.
acp_reconstruct <- function(events, reads, window, tau = c(0, 2), step = 0.1) {
  len <- window$we - window$ws
  none <- data.table::data.table(s = numeric(), e = numeric())
  data.table::rbindlist(lapply(c("event", "read"), function(b) {
    o <- acp_observations(events, reads, window$tag_label, window$phys_board, window$start, window$end, b)
    data.table::rbindlist(lapply(tau, function(tt) {
      g <- amv_grid_track(amv_runs(o$t, o$pid, window$ws, window$we, tt), none, len, step)$pos
      data.table::data.table(placement_id = window$placement_id, tag_label = window$tag_label, basis = b, tau = tt, cell = seq_along(g), pos = g)
    }))
  }))
}

#' Confusion shares per group: grid (placement_id, tag_label, basis, tau, cell, pos) joined with truth on the shared key
#' columns (placement_id, tag_label, cell); truth carries adm (bitmask) and optionally eval and unit_id. Over the evaluated
#' cells: unassigned, correct_reader, correct_column, other_row, col_pm1, col_ge2 (mutually exclusive except
#' correct_column = correct_reader + other_row), assigned, and over the assigned cells correct_reader_assigned and
#' correct_row_assigned.
acp_confusion <- function(grid, truth, by = c("placement_id", "tag_label", "basis", "tau")) {
  key <- intersect(c("placement_id", "tag_label", "cell"), intersect(names(grid), names(truth)))
  x <- merge(grid, truth, by = key)
  if (!"eval" %in% names(x)) x[, eval := TRUE]
  x <- x[eval == TRUE]
  x[, `:=`(cr = acp_in_mask(adm, pos), cd = acp_col_distance(adm, pos), ro = acp_row_ok(adm, pos))]
  x[, .(n_cells = .N, unassigned = mean(pos == 0L), correct_reader = mean(cr), correct_column = mean(!is.na(cd) & cd == 0),
        other_row = mean(!cr & !is.na(cd) & cd == 0), col_pm1 = mean(!is.na(cd) & cd == 1), col_ge2 = mean(!is.na(cd) & cd >= 2),
        assigned = mean(pos > 0L),
        correct_reader_assigned = if (any(pos > 0L)) mean(cr[pos > 0L]) else NA_real_,
        correct_row_assigned = if (any(pos > 0L)) mean(ro[pos > 0L]) else NA_real_), by = by]
}

#' Cells of the movement grid assigned to their basis unit (crossing or visit window); cells outside every unit dropped.
acp_unit_cells <- function(mtruth, units) {
  data.table::rbindlist(lapply(unique(units$placement_id), function(p) {
    m <- mtruth[placement_id == p]; u <- units[placement_id == p][order(us)]
    k <- findInterval(m$u, u$us)
    ok <- k > 0L
    ok[ok] <- m$u[ok] < u$ue[k[ok]]
    cbind(m[ok], u[k[ok], .(unit_id, unit_type, cluster, board)])
  }))
}

# ---------------------------------------------------------------- bootstrap and projection

#' Evaluates `code` with the pinned RNG (Mersenne-Twister, Inversion, Rejection; set.seed(seed)) and restores the caller's
#' RNG state afterwards.
acp_with_seed <- function(seed, code) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv()) else NULL
  old_kind <- RNGkind()
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had) assign(".Random.seed", old, envir = globalenv()) else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  code
}

#' Stratified cluster bootstrap: clusters resampled with replacement within each stratum (as many as the stratum has),
#' fun(units) -> named numeric vector; percentile CI at `level`. Returns data.table(statistic, estimate, lo, hi, n_units,
#' n_clusters, B).
acp_boot <- function(units, fun, strata, cluster, B = 10000L, seed = 20261014L, level = 0.95) {
  u <- data.table::as.data.table(units)
  est <- fun(u)
  nm <- if (is.null(names(est))) paste0("stat", seq_along(est)) else names(est)
  if (!nrow(u)) return(data.table::data.table(statistic = nm, estimate = NA_real_, lo = NA_real_, hi = NA_real_, n_units = 0L,
                                              n_clusters = 0L, B = as.integer(B)))
  sk <- if (length(strata)) do.call(paste, c(as.list(u[, strata, with = FALSE]), sep = "|")) else rep("all", nrow(u))
  ck <- paste(sk, u[[cluster]], sep = "\r")
  rows <- split(seq_len(nrow(u)), ck)
  by_stratum <- split(seq_along(rows), sub("\r.*$", "", names(rows)))
  reps <- acp_with_seed(seed, {
    m <- matrix(NA_real_, B, length(est))
    for (b in seq_len(B)) {
      pick <- unlist(lapply(by_stratum, function(cl) cl[sample.int(length(cl), length(cl), replace = TRUE)]), use.names = FALSE)
      m[b, ] <- fun(u[unlist(rows[pick], use.names = FALSE)])
    }
    m
  })
  a <- (1 - level) / 2
  q <- vapply(seq_along(est), function(j) {
    v <- reps[, j]
    if (all(is.na(v))) c(NA_real_, NA_real_) else stats::quantile(v, c(a, 1 - a), na.rm = TRUE, names = FALSE, type = 7)
  }, numeric(2))
  data.table::data.table(statistic = nm, estimate = unname(as.numeric(est)), lo = q[1, ], hi = q[2, ], n_units = nrow(u),
                         n_clusters = length(rows), B = as.integer(B))
}

#' Projection test P of the day-2 rule: for each threshold c inside the CI [lo, hi] of an estimate, day 2 (doubled units)
#' plausibly resolves it when |est - c| > factor * h, h the CI half-width (factor = 1/sqrt(2), criteria rule P1).
acp_project <- function(est, lo, hi, threshold, factor) {
  h <- (hi - lo) / 2
  inside <- !is.na(lo) & !is.na(hi) & threshold >= lo & threshold <= hi
  data.table::data.table(threshold = threshold, estimate = est, lo = lo, hi = hi, h = h, inside = inside,
                         distance = abs(est - threshold), resolvable = inside & !is.na(est) & abs(est - threshold) > factor * h)
}

#' Median and its order-statistic CI: the k-th to (n-k+1)-th ordered values with the largest k whose binomial coverage
#' 1 - 2 pbinom(k - 1, n, 0.5) is at least `level` (n = 16: 4th to 13th). NA bounds when no k qualifies.
acp_median_ci <- function(x, level = 0.95) {
  x <- sort(x[is.finite(x)]); n <- length(x)
  if (!n) return(data.table::data.table(n = 0L, median = NA_real_, lo = NA_real_, hi = NA_real_, coverage = NA_real_))
  k <- 0L
  for (kk in seq_len(floor(n / 2))) if (1 - 2 * stats::pbinom(kk - 1L, n, 0.5) >= level) k <- kk else break
  data.table::data.table(n = n, median = stats::median(x), lo = if (k) x[k] else NA_real_, hi = if (k) x[n - k + 1L] else NA_real_,
                         coverage = if (k) 1 - 2 * stats::pbinom(k - 1L, n, 0.5) else NA_real_)
}

# ---------------------------------------------------------------- basis decision statistics

#' Basis units at column or reader level at tau: static boundary units (A09; reader level adds A50) from the static
#' confusion (REF-x the mean of its replicates per board), crossings and visits from the movement confusion. c = correct
#' column (column level) or correct reader share; d = c(read) - c(event). conf_static needs board and pair_id; conf_move
#' needs unit_id, unit_type, cluster, board.
acp_basis_units <- function(conf_static, conf_move, crit, level = c("column", "reader"), tau = acp_th(crit, "A03")) {
  level <- match.arg(level)
  val <- if (level == "column") "correct_column" else "correct_reader"
  ids <- acp_set(crit, "A09"); if (level == "reader") ids <- c(ids, acp_set(crit, "A50"))
  tt <- tau
  cs <- conf_static[abs(tau - tt) < 1e-9 & pair_id %in% ids]
  st <- if (nrow(cs)) {
    w <- data.table::dcast(cs, board + pair_id + placement_id ~ basis, value.var = val)
    w[, .(c_read = mean(read), c_event = mean(event)), by = .(board, pair_id)][
      , `:=`(unit_id = paste0(pair_id, "_b", board), unit_type = "static", cluster = paste0(pair_id, "_b", board))][]
  } else data.table::data.table(board = integer(), pair_id = character(), c_read = numeric(), c_event = numeric(), unit_id = character(),
                                unit_type = character(), cluster = character())
  cm <- conf_move[abs(tau - tt) < 1e-9]
  mv <- if (nrow(cm)) {
    data.table::dcast(cm, board + unit_id + unit_type + cluster ~ basis, value.var = val)[, .(board, unit_id, unit_type, cluster, c_read = read, c_event = event)]
  } else NULL
  u <- rbind(st[, .(board, unit_id, unit_type, cluster, c_read, c_event)], mv)
  u[, d := c_read - c_event]
  u[]
}

#' Centre units of D_ctr at tau: placements A13 (REF-z the mean of its replicates per board); d = correct-reader share
#' read - event.
acp_centre_units <- function(conf_static, crit, tau = acp_th(crit, "A03")) {
  tt <- tau
  cs <- conf_static[abs(tau - tt) < 1e-9 & pair_id %in% acp_set(crit, "A13")]
  if (!nrow(cs)) return(data.table::data.table(board = integer(), pair_id = character(), unit_id = character(), cluster = character(), d = numeric()))
  w <- data.table::dcast(cs, board + pair_id + placement_id ~ basis, value.var = "correct_reader")
  w[, .(d = mean(read - event)), by = .(board, pair_id)][, `:=`(unit_id = paste0(pair_id, "_b", board), cluster = paste0(pair_id, "_b", board))][]
}

#' D (pooled), D_b2, D_b4 and D_ctr with bootstrap CIs (strata board x unit type; clusters as in the units).
acp_basis_statistics <- function(units, centre, crit, B = NULL) {
  B <- if (is.null(B)) as.integer(acp_th(crit, "G02")) else as.integer(B)
  seed <- acp_th(crit, "G03"); level <- acp_th(crit, "G01")
  dfun <- function(u) mean(tapply(u$d, factor(u$unit_type, levels = c("static", "crossing", "visit")), mean))
  f <- function(u) c(D = dfun(u), D_b2 = dfun(u[u$board == 2L]), D_b4 = dfun(u[u$board == 4L]))
  rbind(acp_boot(units, f, c("board", "unit_type"), "cluster", B, seed, level),
        acp_boot(centre, function(u) c(D_ctr = if (nrow(u)) mean(u$d) else NA_real_), "board", "cluster", B, seed, level))
}

# ---------------------------------------------------------------- endpoints

#' Co-reads of the single-tag static placements: aqc_coread_flags() of each placement's reads in its window and
#' aqc_coread_counts() by placement; per placement the reads at the true reader with a co-read at any neighbour
#' (coread_true); transitions between consecutive reads at different readers with their length, the simultaneous share
#' (<= B13b) and the geometry x phase pairing summary. Returns list(counts, placement, transitions, by_geometry).
acp_coread <- function(reads, windows, truth, crit) {
  tr <- merge(truth[unit_type %in% ACP_SINGLE_TYPES], windows[, .(placement_id, ws, we)], by = "placement_id")
  r <- data.table::rbindlist(lapply(seq_len(nrow(tr)), function(i) {
    x <- tr[i]; tg <- x$tag_label
    reads[tag_label == tg & board == x$phys_board & t >= x$ws & t < x$we, .(tag = tag_label, board, pid = PositionID, t, placement_id = x$placement_id)]
  }))
  if (!nrow(r)) return(list(counts = data.table::data.table(), placement = data.table::data.table(), transitions = data.table::data.table(),
                            by_geometry = data.table::data.table()))
  fl <- aqc_coread_flags(r, window = acp_th(crit, "B13a"))
  co_any <- rowSums(as.matrix(fl[, paste0("co_R", 1:8), with = FALSE])) > 0
  fl[, co_any := co_any]
  fl <- merge(fl, tr[, .(placement_id, adm, role_board = board)], by = "placement_id")
  pl <- fl[, .(reads = .N, reads_true = sum(acp_in_mask(adm, pid)), coread_true = sum(co_any & acp_in_mask(adm, pid))), by = placement_id]
  cnt <- aqc_coread_counts(fl, by = c("board", "placement_id"))
  nb <- aqc_neighbours()
  cnt[, phase_pairing := acp_phase_pairing(j, k)]
  tz <- fl[order(placement_id, t)][, .(from = pid[-.N], to = pid[-1], dt = diff(t)), by = placement_id][from != to]
  tz <- merge(tz, nb[, .(from = k, to = j, type)], by = c("from", "to"), all.x = TRUE)
  tz[is.na(type), type := "far"]
  tz[, `:=`(phase_pairing = acp_phase_pairing(from, to), simultaneous = acp_test(crit, "B13b", dt))]
  tz <- merge(tz, tr[, .(placement_id, board)], by = "placement_id")
  bg <- merge(cnt[, .(O = sum(O), N = sum(N)), by = .(placement_id, type, phase_pairing)], tr[, .(placement_id, board)], by = "placement_id")[
    , .(O = sum(O), N = sum(N)), by = .(board, type, phase_pairing)][, coread_share := O / N][]
  ts <- tz[, .(transitions = .N, simultaneous_share = mean(simultaneous)), by = .(board, type, phase_pairing)]
  list(counts = cnt[], placement = pl[], transitions = tz[], by_geometry = merge(bg, ts, by = c("board", "type", "phase_pairing"), all = TRUE))
}

#' Competition run (C0, C1, C4, CS, CE) per board: per period x tag at its admissible set: reads, rate, P1s, P10s, longest
#' gap, gaps >= 10/30/60 s, q99 gap. A board's runs of the same period (schedule v2: two C runs per board) are pooled: their
#' 10-s blocks (ids unique per placement) enter one count matrix. Statistics per board with a 10-s block bootstrap (B4f)
#' within each period: TR, MS,
#' Gini (descriptive), CS_C4, SR; the longest gap of a load tag in C4 (starvation) and the per-tag q99 gaps in C4. Only
#' the v1.1 periods (ACP_C_PERIODS) enter; the house-lifted periods of schedule v3 are analysed by acp_competition_house().
acp_competition <- function(reads, windows, truth, crit, B = NULL) {
  B <- if (is.null(B)) as.integer(acp_th(crit, "G02")) else as.integer(B)
  L <- acp_th(crit, "B4f"); k0 <- acp_th(crit, "G06"); level <- acp_th(crit, "G01"); seed <- acp_th(crit, "G03")
  tr <- merge(truth[unit_type == "C" & pair_id %in% ACP_C_PERIODS], windows[, .(placement_id, ws, we)], by = "placement_id")
  if (!nrow(tr)) return(list(per_tag = data.table::data.table(), stats = data.table::data.table(), gaps = data.table::data.table()))
  load <- unique(ACP_C_ROLES$tag_label[ACP_C_ROLES$role == "load"]); sent <- unique(ACP_C_ROLES$tag_label[ACP_C_ROLES$role == "sentinel"])
  per_tag <- list(); blocks <- list()
  for (i in seq_len(nrow(tr))) {
    x <- tr[i]; tg <- x$tag_label
    rt <- sort(reads[tag_label == tg & board == x$phys_board & t >= x$ws & t < x$we & PositionID %in% acp_mask_readers(x$adm)]$t)
    T <- x$we - x$ws; g <- diff(c(x$ws, rt, x$we))
    nb1 <- floor(T + 1e-6); nb10 <- floor(T / 10 + 1e-6)
    per_tag[[i]] <- data.table::data.table(board = x$board, period = x$pair_id, tag_label = tg, role = x$role, T = T, reads = length(rt),
      rate = length(rt) / T, P1s = length(unique(floor(rt - x$ws)[floor(rt - x$ws) < nb1])) / nb1,
      P10s = length(unique(floor((rt - x$ws) / 10)[floor((rt - x$ws) / 10) < nb10])) / nb10, longest_gap = max(g),
      gaps_ge10 = sum(g >= ACP_GAP_COUNTS_S[1]), gaps_ge30 = sum(g >= ACP_GAP_COUNTS_S[2]), gaps_ge60 = sum(g >= ACP_GAP_COUNTS_S[3]),
      q99_gap = stats::quantile(g, 0.99, names = FALSE, type = 7))
    nb <- max(1L, ceiling(T / L - 1e-9))
    blocks[[i]] <- data.table::data.table(board = x$board, period = x$pair_id, tag_label = tg, block = paste0(x$placement_id, "_", seq_len(nb)),
      reads = tabulate(pmin(floor((rt - x$ws) / L) + 1L, nb), nbins = nb), dur = pmin(L, T - (seq_len(nb) - 1L) * L))
  }
  per_tag <- data.table::rbindlist(per_tag); blocks <- data.table::rbindlist(blocks)
  # statistics from per-period count matrices N[[p]] (replicates x tags) and durations D[[p]] (replicates)
  stat_fn <- function(N, D) {
    has <- function(p, tg) !is.null(N[[p]]) && all(tg %in% colnames(N[[p]]))
    nr <- length(D[[1]])
    na <- rep(NA_real_, nr)
    tr_ <- if (has("C4", load) && has("C1", "T1")) ((rowSums(N$C4[, load, drop = FALSE]) + k0) / D$C4) / ((N$C1[, "T1"] + k0) / D$C1) else na
    tot <- if (has("C4", load)) rowSums(N$C4[, load, drop = FALSE]) else na
    ms <- if (has("C4", load)) ifelse(tot > 0, apply(N$C4[, load, drop = FALSE], 1, max) / tot, NA_real_) else na
    gini <- if (has("C4", load)) apply(N$C4[, load, drop = FALSE], 1, function(v) if (sum(v) > 0) sum(abs(outer(v, v, "-"))) / (2 * length(v)^2 * mean(v)) else NA_real_) else na
    cs <- if (has("CS", load) && has("C4", load)) exp(rowMeans(log(((N$CS[, load, drop = FALSE] + k0) / D$CS) / ((N$C4[, load, drop = FALSE] + k0) / D$C4)))) else na
    sr <- if (has("C4", sent) && has("C0", sent) && has("CE", sent)) {
      ((rowSums(N$C4[, sent, drop = FALSE]) + k0) / D$C4) / ((rowSums(N$C0[, sent, drop = FALSE]) + rowSums(N$CE[, sent, drop = FALSE]) + k0) / (D$C0 + D$CE))
    } else na
    cbind(TR = tr_, MS = ms, Gini = gini, CS_C4 = cs, SR = sr)
  }
  stats <- data.table::rbindlist(lapply(sort(unique(blocks$board)), function(b) {
    bl <- blocks[board == b]
    per <- unique(bl$period)
    mats <- lapply(stats::setNames(per, per), function(p) {
      m <- data.table::dcast(bl[period == p], block ~ tag_label, value.var = "reads", fill = 0L)
      list(cnt = as.matrix(m[, -1, with = FALSE]), dur = bl[period == p, .(dur = dur[1]), by = block]$dur)
    })
    pt <- stat_fn(lapply(mats, function(z) matrix(colSums(z$cnt), 1, dimnames = list(NULL, colnames(z$cnt)))),
                  lapply(mats, function(z) sum(z$dur)))
    reps <- acp_with_seed(seed, {
      N <- list(); D <- list()
      for (p in per) {
        z <- mats[[p]]; nb <- nrow(z$cnt)
        idx <- matrix(sample.int(nb, B * nb, replace = TRUE), B, nb)
        N[[p]] <- vapply(colnames(z$cnt), function(tg) rowSums(matrix(z$cnt[idx, tg], B, nb)), numeric(B))
        if (B == 1L) N[[p]] <- matrix(N[[p]], 1, dimnames = list(NULL, colnames(z$cnt)))
        D[[p]] <- rowSums(matrix(z$dur[idx], B, nb))
      }
      stat_fn(N, D)
    })
    a <- (1 - level) / 2
    data.table::data.table(board = b, statistic = colnames(pt), estimate = as.numeric(pt[1, ]),
      lo = apply(reps, 2, function(v) if (all(is.na(v))) NA_real_ else stats::quantile(v, a, na.rm = TRUE, names = FALSE)),
      hi = apply(reps, 2, function(v) if (all(is.na(v))) NA_real_ else stats::quantile(v, 1 - a, na.rm = TRUE, names = FALSE)),
      n_units = nrow(bl[tag_label == bl$tag_label[1]]), B = B)
  }))
  gaps <- per_tag[period == "C4" & role == "load", .(max_gap_C4 = max(longest_gap), max_q99_gap_C4 = max(q99_gap)), by = board]
  list(per_tag = per_tag[], stats = stats[], gaps = gaps[], blocks = blocks[])
}

#' House shielding pairs HCx (H01 on / H02 off) and HCz (H03 / H04) per board at the true reader: log rate ratio on/off
#' (continuity G06), P1s difference; pairs unread (B1b) in both states excluded; pooled house ratio exp(mean log ratio)
#' with a bootstrap over pairs (strata board). Returns list(pairs, stats, p1s_on_hcz).
acp_shielding <- function(det, crit, B = NULL) {
  B <- if (is.null(B)) as.integer(acp_th(crit, "G02")) else as.integer(B)
  k0 <- acp_th(crit, "G06")
  d0 <- det[PositionID == 0L & pair_id %in% c(ACP_HOUSE_PAIRS$on, ACP_HOUSE_PAIRS$off)]
  p <- data.table::rbindlist(lapply(seq_len(nrow(ACP_HOUSE_PAIRS)), function(i) {
    on <- d0[pair_id == ACP_HOUSE_PAIRS$on[i], .(board, n_on = reads, T_on = T, P1s_on = P1s)]
    off <- d0[pair_id == ACP_HOUSE_PAIRS$off[i], .(board, n_off = reads, T_off = T, P1s_off = P1s)]
    merge(on, off, by = "board")[, pair := ACP_HOUSE_PAIRS$pair[i]]
  }))
  if (!nrow(p)) return(list(pairs = p, stats = acp_boot(p, function(u) c(house_ratio = NA_real_), "board", "pair", 1L), p1s_on_hcz = p))
  p[, `:=`(log_ratio = log((n_on + k0) / T_on) - log((n_off + k0) / T_off), P1s_diff = P1s_on - P1s_off,
           unread_both = acp_test(crit, "B1b", P1s_on) & acp_test(crit, "B1b", P1s_off), unit_id = paste0(pair, "_b", board))]
  p[, included := !unread_both]
  st <- acp_boot(p[included == TRUE], function(u) c(house_ratio = if (nrow(u)) exp(mean(u$log_ratio)) else NA_real_), "board", "unit_id",
                 B, acp_th(crit, "G03"), acp_th(crit, "G01"))
  list(pairs = p[], stats = st, p1s_on_hcz = p[pair == "HCz", .(board, P1s_on)])
}

#' REF repeatability per board from the REF-x replicates at the true reader: log rates l = log((n + 0.5) / T), excess SD
#' beyond counting noise sqrt(max(0, var(l) - mean(1 / (n + 0.5)))), late/early ratio, trend (per hour) and drift slope
#' (per s) with the mean replicate time; REF-z cadence change (mean within-event gap, last minus first replicate, ms);
#' REF-x informative unless saturated (B10k) or unread (B1b) on both boards.
acp_ref_repeatability <- function(det, crit) {
  k0 <- acp_th(crit, "G06")
  x <- det[PositionID == 0L & pair_id == "REFx"][, tm := (ws + we) / 2][order(board, tm)]
  z <- det[PositionID == 0L & pair_id == "REFz"][, tm := (ws + we) / 2][order(board, tm)]
  if (!nrow(x)) return(data.table::data.table(board = integer(), replicates = integer(), excess_sd = numeric()))
  x[, l := log((reads + k0) / T)]
  out <- x[, {
    v <- if (.N > 1) stats::var(l) else NA_real_
    sl <- if (.N > 1 && stats::var(tm) > 0) unname(stats::coef(stats::lm(l ~ tm))[2]) else 0
    .(replicates = .N, excess_sd = if (is.na(v)) NA_real_ else sqrt(max(0, v - mean(1 / (reads + k0)))), sd_log_rate = sqrt(v),
      counting_sd = sqrt(mean(1 / (reads + k0))), late_early = exp(l[.N] - l[1]), trend_per_h = sl * 3600, drift_slope_per_s = sl,
      t_mean = mean(tm), e_mean = if ("e" %in% names(.SD)) mean(e) else NA_real_, P1s_mean = mean(P1s))
  }, by = board]
  zc <- z[, .(refz_cadence_change_ms = if (.N > 1) mean_gap_ms[.N] - mean_gap_ms[1] else NA_real_), by = board]
  out <- merge(out, zc, by = "board", all.x = TRUE)
  sat_both <- nrow(out) == 2L && all(acp_test(crit, "B10k", out$e_mean))
  unread_both <- nrow(out) == 2L && all(acp_test(crit, "B1b", out$P1s_mean))
  out[, refx_informative := !(sat_both || unread_both)]
  out[]
}

#' Board ratio over the 21 static pairs (ACP_BOARD_PAIRS) plus the REF means (REF-z, REF-x: pooled reads and time per
#' board): per pair the log rate ratio board 4 / board 2 at the true reader (continuity G06), informative unless saturated
#' (B10k) on both or unread (B1b) on both; the co-read log ratio (reads at the true reader with a neighbour co-read, per s;
#' informative unless 0 on both); the drift-adjusted log ratio (per-board slope of the REF-x log rate, ref); R_board,
#' R_board_drift_adjusted and the co-read ratio with bootstrap CIs over pairs (strata unit type P/H/REF); descriptive
#' ratios for the house-end readers (R1, R2, R5, R6) and R4/R8. Returns list(pairs, stats, by_group).
acp_board_ratio <- function(det, coread_placement, ref, crit, B = NULL) {
  B <- if (is.null(B)) as.integer(acp_th(crit, "G02")) else as.integer(B)
  k0 <- acp_th(crit, "G06")
  d0 <- det[PositionID == 0L & pair_id %in% c(ACP_BOARD_PAIRS, "REFz", "REFx")][, tm := (ws + we) / 2]
  d0 <- merge(d0, if (nrow(coread_placement)) coread_placement[, .(placement_id, coread_true)] else data.table::data.table(placement_id = character(), coread_true = integer()),
              by = "placement_id", all.x = TRUE)
  d0[is.na(coread_true), coread_true := 0L]
  if (!"e" %in% names(d0)) d0[, e := NA_real_]
  pb <- d0[, .(n = sum(reads), T = sum(T), P1s = mean(P1s), e = mean(e), O = sum(coread_true), tm = mean(tm), adm_readers = adm_readers[1]), by = .(board, pair_id)]
  if (nrow(ref)) pb <- merge(pb, ref[, .(board, slope = drift_slope_per_s, t_ref = t_mean)], by = "board", all.x = TRUE) else pb[, `:=`(slope = 0, t_ref = 0)]
  pb[is.na(slope), `:=`(slope = 0, t_ref = 0)]
  pb[, `:=`(l = log((n + k0) / T), lc = log((O + k0) / T))]
  pb[, l_adj := l - slope * (tm - t_ref)]
  w <- data.table::dcast(pb, pair_id + adm_readers ~ board, value.var = c("n", "T", "P1s", "e", "O", "l", "lc", "l_adj"))
  need <- c("l_2", "l_4")
  if (!all(need %in% names(w))) return(list(pairs = w, stats = data.table::data.table(), by_group = data.table::data.table()))
  w[, `:=`(log_ratio = l_4 - l_2, log_ratio_adj = l_adj_4 - l_adj_2, log_coread_ratio = lc_4 - lc_2,
           saturated_both = acp_test(crit, "B10k", e_2) & acp_test(crit, "B10k", e_4),
           unread_both = acp_test(crit, "B1b", P1s_2) & acp_test(crit, "B1b", P1s_4),
           unit_type = ifelse(grepl("^REF", pair_id), "REF", substr(pair_id, 1, 1)))]
  w[, `:=`(informative = !saturated_both & !unread_both & is.finite(log_ratio), coread_informative = !(O_2 == 0 & O_4 == 0) & is.finite(log_coread_ratio))]
  seed <- acp_th(crit, "G03"); level <- acp_th(crit, "G01")
  st <- rbind(
    acp_boot(w[informative == TRUE], function(u) c(R_board = if (nrow(u)) exp(mean(u$log_ratio)) else NA_real_,
                                                   R_board_drift_adjusted = if (nrow(u)) exp(mean(u$log_ratio_adj)) else NA_real_),
             "unit_type", "pair_id", B, seed, level),
    acp_boot(w[coread_informative == TRUE], function(u) c(coread_ratio = if (nrow(u)) exp(mean(u$log_coread_ratio)) else NA_real_),
             "unit_type", "pair_id", B, seed, level))
  rd <- strsplit(w$adm_readers, "|", fixed = TRUE)
  w[, grp := ifelse(vapply(rd, function(z) all(as.integer(z) %in% c(1L, 2L, 5L, 6L)), logical(1)), "house_end_R1_R2_R5_R6",
                    ifelse(vapply(rd, function(z) all(as.integer(z) %in% c(4L, 8L)), logical(1)), "R4_R8", "other"))]
  bg <- w[informative == TRUE, .(pairs = .N, ratio = exp(mean(log_ratio))), by = grp]
  list(pairs = w[], stats = st, by_group = bg)
}

#' Static flicker per single-tag T1 placement x basis: raw reader changes in the window and the changes left by
#' rfid_flip_filter() at each candidate tau (B11a; the filter runs over the placement's own observations), and the runs
#' at B between two runs at the same A (run length = next run start - run start). Returns list(changes, runs).
acp_flicker <- function(events, reads, windows, crit) {
  taus <- acp_set(crit, "B11a")
  w <- windows[use == TRUE & unit_type %in% ACP_T1_STATIC_TYPES]
  chg <- function(v) if (length(v) < 2) 0L else sum(v[-1] != v[-length(v)])
  ch <- list(); rn <- list()
  for (i in seq_len(nrow(w))) for (b in c("event", "read")) {
    x <- w[i]
    o <- acp_observations(events, reads, x$tag_label, x$phys_board, x$start, x$end, b)
    iw <- o$t >= x$ws & o$t < x$we
    raw <- chg(o$pid[iw])
    ch[[length(ch) + 1L]] <- data.table::data.table(placement_id = x$placement_id, board = x$board, pair_id = x$pair_id, basis = b, tau = taus, changes_raw = raw,
                                                    changes_kept = vapply(taus, function(tt) chg(rfid_flip_filter(o$t, o$pid, tt)[iw]), integer(1)))
    if (sum(iw) >= 3) {
      r <- rle(o$pid[iw]); st <- o$t[iw][cumsum(c(1L, utils::head(r$lengths, -1L)))]; k <- length(r$lengths)
      if (k >= 3) {
        inner <- 2:(k - 1); fl <- inner[r$values[inner - 1L] == r$values[inner + 1L]]
        if (length(fl)) rn[[length(rn) + 1L]] <- data.table::data.table(placement_id = x$placement_id, board = x$board, basis = b,
                                                                          from = r$values[fl - 1L], at = r$values[fl], run_s = st[fl + 1L] - st[fl])
      }
    }
  }
  list(changes = data.table::rbindlist(ch), runs = data.table::rbindlist(rn))
}

#' Short visits per visit x basis x tau (B11a): kept = a run at the destination (after the flip filter over the placement's
#' own observations) overlaps the visit's unit window; measured = seconds of destination runs inside it; true dwell =
#' time between the crossings out and back.
acp_visits <- function(events, reads, windows, units, crit) {
  taus <- acp_set(crit, "B11a")
  vu <- units[unit_type == "visit"]
  data.table::rbindlist(lapply(unique(vu$placement_id), function(p) {
    w <- windows[placement_id == p & use == TRUE]
    v <- vu[placement_id == p]
    data.table::rbindlist(lapply(c("event", "read"), function(b) {
      o <- acp_observations(events, reads, w$tag_label, w$phys_board, w$start, w$end, b)
      data.table::rbindlist(lapply(taus, function(tt) {
        runs <- amv_runs(o$t, o$pid, w$ws, w$we, tt)
        ov <- vapply(seq_len(nrow(v)), function(i) {
          d <- runs[pos == v$to[i]]
          sum(pmax(0, pmin(d$e, v$ue[i] - w$ws) - pmax(d$s, v$us[i] - w$ws)))
        }, numeric(1))
        v[, .(unit_id, placement_id, board, visit, dwell_nominal, true_dwell = t_back - t_cross, basis = b, tau = tt, measured_s = ov, kept = ov > 0)]
      }))
    }))
  }))
}

#' Entry latency (first read at the entered reader minus the crossing time) and exit tail (last read at the exited reader
#' minus the crossing time) of each pass crossing, from the reads inside its unit window; summaries (median and
#' order-statistic CI at the G01 level) by board x boundary class x speed and by boundary class x speed pooled.
acp_boundary_timing <- function(reads, windows, units, crit) {
  cu <- units[unit_type == "crossing"]
  if (!nrow(cu)) return(list(crossings = cu, summary = data.table::data.table()))
  cu <- merge(cu, windows[, .(placement_id, tag_label, phys_board)], by = "placement_id")
  x <- data.table::rbindlist(lapply(seq_len(nrow(cu)), function(i) {
    u <- cu[i]; tg <- u$tag_label
    r <- reads[tag_label == tg & board == u$phys_board & t >= u$us & t < u$ue]
    en <- r[PositionID == u$to]$t; ex <- r[PositionID == u$from]$t
    u[, .(unit_id, placement_id, board, boundary = paste0("R", from, "|R", to), boundary_class, speed, t_cross,
          entry_latency_s = if (length(en)) min(en) - t_cross else NA_real_, exit_tail_s = if (length(ex)) max(ex) - t_cross else NA_real_)]
  }))
  level <- acp_th(crit, "G01")
  sm <- function(d, by) data.table::rbindlist(lapply(c("entry_latency_s", "exit_tail_s"), function(m)
    d[, acp_median_ci(get(m), level), by = by][, measure := m]))
  list(crossings = x[], summary = rbind(sm(x, c("board", "boundary_class", "speed")),
                                       sm(x, c("boundary_class", "speed"))[, board := NA_integer_], use.names = TRUE))
}

#' Tag QC (B14): the T0 rates at the REF-z spot (true reader) of T2-T6 and T1's rate from the first REF-z placement on
#' board 2.
acp_tag_qc <- function(det) {
  t0 <- det[PositionID == 0L & unit_type == "T0", .(tag_label, rate = r, placement_id)]
  t1 <- det[PositionID == 0L & pair_id == "REFz" & board == 2L][order(ws)]
  rbind(t0, if (nrow(t1)) t1[1, .(tag_label, rate = r, placement_id)])
}

#' Channel check: reads of `tag` per board and reader during the CHECK row (stop rule 1); passed = every reader of each
#' tested board read.
acp_channel_check <- function(reads, windows, boards = ACP_BOARDS, tag = "T1") {
  ck <- windows[block == "CHECK" & use == TRUE]
  if (nrow(ck) != 1L) return(data.table::data.table(board = unname(boards), reads_min = NA_integer_, silent_readers = NA_character_, passed = FALSE))
  tg <- tag
  r <- reads[tag_label == tg & t >= ck$start & t < ck$end]
  data.table::rbindlist(lapply(unname(boards), function(b) {
    n <- tabulate(r[board == b]$PositionID, 8)
    data.table::data.table(board = as.integer(names(boards)[boards == b]), phys_board = b, reads_min = min(n),
                           silent_readers = paste(which(n == 0), collapse = "|"), passed = all(n > 0))
  }))
}

#' Known-empty time per physical board: boards outside `boards` the whole session span; a tested board while another
#' tested board is in a visit (first start to last end) and during BASELINE/BREAK rows. Returns data.table(board, s, e).
acp_idle_intervals <- function(windows, span, boards = ACP_BOARDS, all_boards = 1:5) {
  w <- windows[use == TRUE]
  out <- list(data.table::data.table(board = setdiff(all_boards, unname(boards)), s = span[1], e = span[2]))
  base <- w[block %in% c("BASELINE", "BREAK"), .(s = start, e = end)]
  for (b in unname(boards)) {
    other <- w[!is.na(phys_board) & phys_board != b & phys_board %in% boards, .(s = min(start), e = max(end)), by = visit][, .(s, e)]
    u <- rbind(other, base)
    if (nrow(u)) out[[length(out) + 1L]] <- cbind(data.table::data.table(board = b), amv_union(u$s, u$e))
  }
  data.table::rbindlist(out)[e > s]
}

#' Known-empty events (B15): events starting in a board's idle time (tag_C excluded on the clapper board), per board-hour,
#' with their tags and the one-sided Poisson upper bound at `level` (0 events: about 3 / board-hours).
acp_known_empty <- function(events, idle, crit, tag_C = ACP_CLAPPER_TAG, clapper_board = ACP_CLAPPER_BOARD) {
  level <- acp_th(crit, "G01")
  ev <- events[!(tag_label == tag_C & board == clapper_board)]
  per <- data.table::rbindlist(lapply(sort(unique(idle$board)), function(b) {
    iv <- idle[board == b]; e <- ev[board == b]
    k <- findInterval(e$t, iv$s); hit <- k > 0L
    hit[hit] <- e$t[hit] < iv$e[k[hit]]
    data.table::data.table(board = b, board_hours = sum(iv$e - iv$s) / 3600, events = sum(hit), tags = paste(sort(unique(e$tag_label[hit])), collapse = "|"))
  }))
  tot <- per[, .(board = NA_integer_, board_hours = sum(board_hours), events = sum(events), tags = paste(setdiff(unique(unlist(strsplit(tags, "|", fixed = TRUE))), ""), collapse = "|"))]
  out <- rbind(per, tot)
  out[, `:=`(rate_per_h = events / board_hours, upper_per_h = stats::qchisq(level, 2 * (events + 1)) / 2 / board_hours)]
  out[]
}

#' Board cadence signature (B16) over the session: within-event read gaps of the events that overlap no other event on
#' the board (aqc_board_overlap_counts()), aqc_lattice() -> mean gap and lattice k1 per board; z against the same board's
#' rows of board_history (columns board, mean_gap_ms, lattice_k1).
acp_cadence <- function(events, board_history) {
  ev <- data.table::copy(events)[, ov := aqc_board_overlap_counts(board, t, t + dur_sec)]
  iso <- ev[ov == 0L & reads >= 2L]
  gaps <- iso[, .(gap_ms = as.numeric(strsplit(read_offsets_ms, ",", fixed = TRUE)[[1]])[-1]), by = .(board, file_order)]
  sig <- gaps[, { la <- aqc_lattice(gap_ms); .(gaps = la$gaps, mean_gap_ms = la$mean_gap_ms, lattice_k1 = la$lattice_k1) }, by = board]
  bh <- data.table::as.data.table(board_history)[, .(hist_n = .N, hist_mean_gap = mean(mean_gap_ms), hist_sd_gap = stats::sd(mean_gap_ms),
                                                     hist_mean_k1 = mean(lattice_k1), hist_sd_k1 = stats::sd(lattice_k1)), by = board]
  sig <- merge(sig, bh, by = "board", all.x = TRUE)
  sig[, `:=`(z_gap = (mean_gap_ms - hist_mean_gap) / hist_sd_gap, z_k1 = (lattice_k1 - hist_mean_k1) / hist_sd_k1)]
  sig[]
}

#' Vendor position records (apr_positions(gate = "vendor")) of each single-tag placement from its own reads (all boards,
#' as the vendor pools them) in [start, end). Descriptive only (schedule v3: also the HX and K placements).
acp_vendor_emulation <- function(reads, windows) {
  w <- windows[use == TRUE & unit_type %in% c(ACP_SINGLE_TYPES, "HX", "M_pass", "M_visits", "K") & !is.na(tag_label)]
  data.table::rbindlist(lapply(seq_len(nrow(w)), function(i) {
    x <- w[i]; tg <- x$tag_label
    r <- reads[tag_label == tg & t_ms >= round(x$start * 1000) & t_ms < round(x$end * 1000)][order(t_ms)]
    if (nrow(r) < 2L) return(NULL)
    p <- apr_positions(r$t_ms, r$gx, r$gy, gate = "vendor")
    if (!nrow(p)) return(NULL)
    data.table::data.table(placement_id = x$placement_id, board = x$board, pair_id = x$pair_id, p)
  }), fill = TRUE)
}

# ---------------------------------------------------------------- decisions

#' One decision-table row; criterion, category and feeds from the criteria rule.
acp_row <- function(crit, rule_id, statistic, decision = NA_character_, estimate = NA_real_, lo = NA_real_, hi = NA_real_,
                    n_units = NA_integer_, board = "pooled") {
  r <- acp_rule(crit, rule_id)
  data.table::data.table(criterion = r$criterion, rule_id = rule_id, statistic = statistic, board = as.character(board),
                         estimate = as.numeric(estimate), lo = as.numeric(lo), hi = as.numeric(hi), n_units = as.integer(n_units),
                         decision = as.character(decision), category = r$category, feeds = r$feeds)
}

#' Criterion (a): READ / EVENT / TIE_equivalent / TIE_inconclusive (not_obtained when a statistic is missing) from stats
#' rows D, D_b2, D_b4, D_ctr (acp_basis_statistics()). level "reader": the same rules for D_rdr.
acp_decide_basis <- function(stats, crit, level = c("column", "reader")) {
  level <- match.arg(level)
  g <- function(s) { x <- stats[stats$statistic == s]; if (nrow(x) != 1L) data.table::data.table(estimate = NA_real_, lo = NA_real_, hi = NA_real_, n_units = NA_integer_) else x }
  D <- g("D"); b2 <- g("D_b2"); b4 <- g("D_b4"); ct <- g("D_ctr")
  nm <- if (level == "column") "D_col" else "D_rdr"
  t <- function(id, x) acp_test(crit, id, x)
  if (anyNA(c(D$estimate, D$lo, D$hi, b2$estimate, b4$estimate, ct$lo, ct$hi))) { dec <- "not_obtained"; rid <- "A12" }
  else if (t("A20", D$estimate) && t("A21", D$lo) && t("A22", b2$estimate) && t("A23", b4$estimate) && t("A24", ct$lo)) { dec <- "READ"; rid <- "A20" }
  else if (t("A30", D$estimate) && t("A31", D$hi) && t("A32", b2$estimate) && t("A33", b4$estimate) && t("A34", ct$hi)) { dec <- "EVENT"; rid <- "A30" }
  else if (t("A40", D$lo) && t("A41", D$hi)) { dec <- "TIE_equivalent"; rid <- "A40" }
  else { dec <- "TIE_inconclusive"; rid <- "A42" }
  rbind(acp_row(crit, rid, nm, dec, D$estimate, D$lo, D$hi, D$n_units),
        acp_row(crit, "A22", paste0(nm, "_b2"), NA, b2$estimate, b2$lo, b2$hi, b2$n_units, board = "2"),
        acp_row(crit, "A23", paste0(nm, "_b4"), NA, b4$estimate, b4$lo, b4$hi, b4$n_units, board = "4"),
        acp_row(crit, "A24", if (level == "column") "D_ctr" else "D_ctr_reader_level", NA, ct$estimate, ct$lo, ct$hi, ct$n_units))
}

#' B2: reader level is primary only if, on both boards and under the adopted basis (both on a tie or no decision), every
#' tested centre placement (A13, assigned share >= B2b) has a correct-reader share of the assigned time >= B2a and P12 and
#' P13 have a correct-row share >= B2c; otherwise column level. conf: static confusion with board and pair_id.
acp_decide_resolution <- function(conf, basis_decision, crit) {
  bases <- switch(basis_decision, READ = "read", EVENT = "event", c("event", "read"))
  tt <- acp_th(crit, "A03")
  x <- conf[abs(tau - tt) < 1e-9 & basis %in% bases]
  rows <- list(); ok <- TRUE
  for (b in c(2L, 4L)) for (bs in bases) {
    ce <- x[board == b & basis == bs & pair_id %in% acp_set(crit, "A13") & acp_test(crit, "B2b", assigned)]
    q <- x[board == b & basis == bs & pair_id %in% c("P12", "P13")]
    c1 <- nrow(ce) > 0 && all(acp_test(crit, "B2a", ce$correct_reader_assigned))
    c2 <- all(c("P12", "P13") %in% q$pair_id) && all(acp_test(crit, "B2c", q$correct_row_assigned))
    ok <- ok && c1 && c2
    rows[[length(rows) + 1L]] <- acp_row(crit, "B2a", paste0("min_centre_correct_reader_assigned_", bs), if (c1) "met" else "not_met",
                                         if (nrow(ce)) min(ce$correct_reader_assigned) else NA, n_units = nrow(ce), board = b)
    rows[[length(rows) + 1L]] <- acp_row(crit, "B2c", paste0("min_P12_P13_correct_row_assigned_", bs), if (c2) "met" else "not_met",
                                         if (nrow(q)) min(q$correct_row_assigned) else NA, n_units = nrow(q), board = b)
  }
  rbind(acp_row(crit, if (ok) "B2a" else "B2d", "resolution", if (ok) "reader" else "column"), data.table::rbindlist(rows))
}

#' B3: Gmax per board over the reliable T1 placements (class reliable at the true reader); fresh age class = B3a's bound
#' when Gmax <= it on both boards, otherwise the smallest B3b candidate above the larger Gmax.
acp_decide_support_age <- function(det, crit) {
  rel <- det[PositionID == 0L & unit_type %in% ACP_T1_STATIC_TYPES & class == "reliable"]
  g <- rel[, .(gmax = max(gmax), q99 = max(q99_gap), missed_share = mean(missed_share, na.rm = TRUE), splits_per_min = mean(splits_per_min), n = .N), by = board]
  per <- data.table::rbindlist(lapply(seq_len(nrow(g)), function(i) rbind(
    acp_row(crit, "B3a", "Gmax_s", NA, g$gmax[i], n_units = g$n[i], board = g$board[i]),
    acp_row(crit, "B3c", "max_q99_gap_s", NA, g$q99[i], n_units = g$n[i], board = g$board[i]),
    acp_row(crit, "B3c", "mean_missed_share", NA, g$missed_share[i], n_units = g$n[i], board = g$board[i]))))
  if (!all(c(2L, 4L) %in% g$board)) return(rbind(acp_row(crit, "B3a", "fresh_age_s", "not_obtained"), per))
  if (all(acp_test(crit, "B3a", g$gmax))) {
    main <- acp_row(crit, "B3a", "fresh_age_s", "fresh_le_Gmax_bound", acp_th(crit, "B3a"), n_units = sum(g$n))
  } else {
    cand <- acp_set(crit, "B3b"); above <- cand[cand > max(g$gmax) + ACP_EPS]
    main <- acp_row(crit, "B3b", "fresh_age_s", if (length(above)) "smallest_candidate_above_Gmax" else "no_candidate_above_Gmax",
                    if (length(above)) min(above) else NA, n_units = sum(g$n))
  }
  rbind(main, per)
}

#' B4-B6 per board from acp_competition(): B4 call (capture_or_starvation, collision, time_sharing, intermediate), B5
#' (per_antenna, spans_R5_R6, undecided), B6 (board_level, none, undecided), with the statistic rows.
acp_decide_competition <- function(comp, crit) {
  st <- comp$stats
  out <- list()
  for (b in c(2L, 4L)) {
    s <- function(nm) { x <- st[board == b & statistic == nm]; if (nrow(x) != 1L) data.table::data.table(estimate = NA_real_, lo = NA_real_, hi = NA_real_, n_units = NA_integer_) else x }
    TR <- s("TR"); MS <- s("MS"); CS <- s("CS_C4"); SR <- s("SR"); GI <- s("Gini")
    G <- comp$gaps[board == b]$max_gap_C4; G <- if (length(G)) G else NA_real_
    c4 <- if (anyNA(c(TR$estimate, MS$estimate, G))) "not_obtained"
          else if (acp_test(crit, "B4d", MS$estimate) || acp_test(crit, "B4e", G)) "capture_or_starvation"
          else if (acp_test(crit, "B4c", TR$estimate)) "collision"
          else if (acp_test(crit, "B4a", TR$estimate) && acp_test(crit, "B4b", MS$estimate)) "time_sharing" else "intermediate"
    c5 <- if (is.na(CS$estimate)) "not_obtained" else if (acp_test(crit, "B5a", CS$estimate)) "per_antenna"
          else if (acp_test(crit, "B5b", CS$estimate)) "spans_R5_R6" else "undecided"
    c6 <- if (anyNA(c(SR$estimate, SR$lo, SR$hi))) "not_obtained" else if (acp_test(crit, "B6a", SR$estimate) && acp_test(crit, "B6b", SR$hi)) "board_level"
          else if (acp_test(crit, "B6c", SR$lo) && acp_test(crit, "B6d", SR$hi)) "none" else "undecided"
    out[[length(out) + 1L]] <- rbind(
      acp_row(crit, "B4a", "competition_call", c4, board = b),
      acp_row(crit, "B4a", "TR", NA, TR$estimate, TR$lo, TR$hi, TR$n_units, board = b),
      acp_row(crit, "B4b", "MS", NA, MS$estimate, MS$lo, MS$hi, MS$n_units, board = b),
      acp_row(crit, "B4e", "max_gap_C4_s", NA, G, board = b),
      acp_row(crit, "B4g", "Gini", NA, GI$estimate, GI$lo, GI$hi, GI$n_units, board = b),
      acp_row(crit, "B5a", "CS_C4", c5, CS$estimate, CS$lo, CS$hi, CS$n_units, board = b),
      acp_row(crit, "B6a", "SR", c6, SR$estimate, SR$lo, SR$hi, SR$n_units, board = b))
  }
  data.table::rbindlist(out)
}

#' B7: delta* = the smallest B7a candidate above both the largest per-tag q99 gap in C4 on board 4 and Gmax (the larger of
#' the boards' Gmax over reliable T1 placements); board 2's value alongside.
acp_decide_delta <- function(comp, det, crit) {
  gm <- det[PositionID == 0L & unit_type %in% ACP_T1_STATIC_TYPES & class == "reliable"]$gmax
  gmax <- if (length(gm)) max(gm) else NA_real_
  cand <- acp_set(crit, "B7a")
  one <- function(b) {
    q <- comp$gaps[board == b]$max_q99_gap_C4
    if (!length(q) || is.na(gmax)) return(acp_row(crit, "B7a", "delta_s", "not_obtained", board = b))
    above <- cand[cand > max(q, gmax) + ACP_EPS]
    acp_row(crit, "B7a", "delta_s", if (length(above)) "smallest_candidate_above" else "no_candidate_above", if (length(above)) min(above) else NA,
            board = b)
  }
  rbind(one(4L), one(2L)[, statistic := "delta_s_board2_alongside"], acp_row(crit, "B7a", "Gmax_reliable_s", NA, gmax))
}

#' B8 (house ratio, CI and P1s(on, HCz) on both boards) and B9 (roof classes per board: H05 roof, H06 foam stack, H02 HCx
#' off; classes as B1).
acp_decide_shielding <- function(sh, det, crit) {
  hr <- sh$stats[statistic == "house_ratio"]
  p1 <- sh$p1s_on_hcz
  dec <- if (!nrow(hr) || anyNA(c(hr$estimate, hr$lo, hr$hi)) || nrow(p1) < 2L) "not_obtained"
         else if (acp_test(crit, "B8a", hr$estimate) && acp_test(crit, "B8b", hr$lo) && all(acp_test(crit, "B8c", p1$P1s_on))) "no_shielding"
         else if (acp_test(crit, "B8d", hr$estimate) && acp_test(crit, "B8e", hr$hi)) "shielding" else "partial"
  rows <- list(acp_row(crit, "B8a", "house_ratio", dec, hr$estimate[1], hr$lo[1], hr$hi[1], hr$n_units[1]))
  for (i in seq_len(nrow(p1))) rows[[length(rows) + 1L]] <- acp_row(crit, "B8c", "P1s_on_HCz", NA, p1$P1s_on[i], board = p1$board[i])
  cl <- det[PositionID == 0L & pair_id %in% c("H05", "H06", "H02"), .(board, pair_id, class)]
  for (b in sort(unique(cl$board))) {
    f <- function(p) { z <- cl[board == b & pair_id == p]$class; if (length(z)) z[1] else "missing" }
    rows[[length(rows) + 1L]] <- acp_row(crit, "B9a", "roof_vs_foam_H05_H06", paste(f("H05"), f("H06"), sep = " / "), board = b)
    rows[[length(rows) + 1L]] <- acp_row(crit, "B9a", "foam_vs_HCx_off_H06_H02", paste(f("H06"), f("H02"), sep = " / "), board = b)
  }
  data.table::rbindlist(rows)
}

#' B10: "difference" when (R_board <= B10a with upper CI < B10b, or co-read ratio <= B10c with upper CI < B10d), REF excess
#' SD <= B10e on both boards, and the call is unchanged with the drift-adjusted R_board; "no_difference" when both CIs lie
#' inside [B10g, B10h] and [B10i, B10j]; otherwise "undecided". br: acp_board_ratio(); ref: acp_ref_repeatability().
acp_decide_board <- function(br, ref, crit) {
  g <- function(s) { x <- br$stats[statistic == s]; if (nrow(x) != 1L) data.table::data.table(estimate = NA_real_, lo = NA_real_, hi = NA_real_, n_units = NA_integer_) else x }
  R <- g("R_board"); RA <- g("R_board_drift_adjusted"); C <- g("coread_ratio")
  ref_ok <- nrow(ref) == 2L && all(acp_test(crit, "B10e", ref$excess_sd))
  call <- function(R, C) {
    if (anyNA(c(R$estimate, R$lo, R$hi, C$estimate, C$lo, C$hi))) return("not_obtained")
    diff_arm <- (acp_test(crit, "B10a", R$estimate) && acp_test(crit, "B10b", R$hi)) || (acp_test(crit, "B10c", C$estimate) && acp_test(crit, "B10d", C$hi))
    if (diff_arm && ref_ok) return("difference")
    if (acp_test(crit, "B10g", R$lo) && acp_test(crit, "B10h", R$hi) && acp_test(crit, "B10i", C$lo) && acp_test(crit, "B10j", C$hi)) return("no_difference")
    "undecided"
  }
  raw <- call(R, C); adj <- call(data.table::data.table(estimate = RA$estimate, lo = RA$lo, hi = RA$hi), C)
  final <- if (raw == "difference" && adj != "difference") "undecided" else raw
  rows <- list(acp_row(crit, if (final == "difference") "B10a" else if (final == "no_difference") "B10g" else "B10l", "board_call", final),
               acp_row(crit, "B10a", "R_board", NA, R$estimate, R$lo, R$hi, R$n_units),
               acp_row(crit, "B10c", "coread_ratio", NA, C$estimate, C$lo, C$hi, C$n_units),
               acp_row(crit, "B10f", "R_board_drift_adjusted", NA, RA$estimate, RA$lo, RA$hi, RA$n_units),
               acp_row(crit, "B10f", "board_call_drift_adjusted", adj),
               acp_row(crit, "B10e", "REFx_informative", NA, if (nrow(ref)) as.numeric(all(ref$refx_informative)) else NA))
  for (i in seq_len(nrow(ref))) {
    rows[[length(rows) + 1L]] <- acp_row(crit, "B10e", "REF_excess_sd", NA, ref$excess_sd[i], n_units = ref$replicates[i], board = ref$board[i])
    rows[[length(rows) + 1L]] <- acp_row(crit, "B10e", "REF_late_early", NA, ref$late_early[i], n_units = ref$replicates[i], board = ref$board[i])
  }
  if (nrow(br$by_group)) for (i in seq_len(nrow(br$by_group))) rows[[length(rows) + 1L]] <-
    acp_row(crit, "B10l", paste0("ratio_", br$by_group$grp[i]), NA, br$by_group$ratio[i], n_units = br$by_group$pairs[i])
  data.table::rbindlist(rows)
}

#' B11 per basis: tau* = the smallest B11a candidate removing >= B11b of the static reader changes on each board (with >=
#' B11c changes per board) and keeping >= B11d of the five-second visits; "range" when none qualifies (lo = the smallest
#' tau meeting the removal rule, hi = the largest keeping the visits); "not_decidable" for lack of data (fewer changes
#' than B11c on a board, or P07 unread, D5b). flicker: acp_flicker(); visits: acp_visits(); det for P07.
acp_decide_tau <- function(flicker, visits, det, crit) {
  taus <- acp_set(crit, "B11a")
  p07 <- det[PositionID == 0L & pair_id == "P07"]$P1s
  p07_unread <- length(p07) > 0 && any(acp_test(crit, "D5b", p07))
  out <- list()
  for (bs in c("event", "read")) {
    ch <- flicker$changes[basis == bs]
    nb <- ch[tau == taus[1], .(raw = sum(changes_raw)), by = board]
    enough <- all(c(2L, 4L) %in% nb$board) && all(acp_test(crit, "B11c", nb$raw))
    rm_ok <- vapply(taus, function(tt) {
      r <- ch[abs(tau - tt) < 1e-9, .(rem = 1 - sum(changes_kept) / sum(changes_raw)), by = board]
      all(c(2L, 4L) %in% r$board) && all(acp_test(crit, "B11b", r$rem))
    }, logical(1))
    v5 <- if (nrow(visits)) visits[basis == bs & dwell_nominal == 5] else visits
    kept <- vapply(taus, function(tt) if (nrow(v5)) sum(v5[abs(tau - tt) < 1e-9]$kept) else 0L, numeric(1))
    vis_ok <- acp_test(crit, "B11d", kept)
    q <- taus[rm_ok & vis_ok]
    tstar <- if (length(q)) min(q) else NA_real_
    dec <- if (!enough || p07_unread) "not_decidable" else if (!is.na(tstar)) {
      if (acp_test(crit, "B11e", tstar)) "tau_star_above_flip2s" else "tau_star" } else "range"
    lo <- if (any(rm_ok)) min(taus[rm_ok]) else NA_real_; hi <- if (any(vis_ok)) max(taus[vis_ok]) else NA_real_
    out[[length(out) + 1L]] <- rbind(
      acp_row(crit, if (dec == "range") "B11f" else "B11b", paste0("tau_star_", bs), dec, tstar, lo, hi),
      data.table::rbindlist(lapply(seq_len(nrow(nb)), function(i) acp_row(crit, "B11c", paste0("static_changes_", bs), NA, nb$raw[i], board = nb$board[i]))),
      data.table::rbindlist(lapply(seq_along(taus), function(i) acp_row(crit, "B11d", sprintf("five_s_visits_kept_%s_tau%g", bs, taus[i]), NA, kept[i],
                                                                         n_units = if (nrow(v5)) sum(abs(v5$tau - taus[i]) < 1e-9) else 0L))))
  }
  rbind(data.table::rbindlist(out), acp_row(crit, "D5b", "P07_P1s_min", if (p07_unread) "unread" else NA, if (length(p07)) min(p07) else NA))
}

#' B15: real_tags (no event in known-empty time) or events_rule.
acp_decide_phantom <- function(ke, crit) {
  tot <- ke[is.na(board)]
  if (nrow(tot) != 1L) return(acp_row(crit, "B15a", "phantom_events", "not_obtained"))
  dec <- if (acp_test(crit, "B15a", tot$events)) "real_tags" else "events_rule"
  rbind(acp_row(crit, if (dec == "real_tags") "B15a" else "B15b", "phantom_events", dec, tot$events, 0, tot$upper_per_h),
        acp_row(crit, "B15a", "known_empty_board_hours", NA, tot$board_hours))
}

#' B16 per tested board: caveat when |z_gap| or |z_k1| > B16a; pooled caveat when any tested board has one.
acp_decide_transfer <- function(cad, crit, boards = ACP_BOARDS) {
  x <- cad[board %in% unname(boards)]
  if (!nrow(x)) return(acp_row(crit, "B16a", "transfer", "not_obtained"))
  x[, caveat := acp_test(crit, "B16a", abs(z_gap)) | acp_test(crit, "B16a", abs(z_k1))]
  rows <- data.table::rbindlist(lapply(seq_len(nrow(x)), function(i) rbind(
    acp_row(crit, "B16a", "z_gap", if (x$caveat[i]) "caveat" else "transfer", x$z_gap[i], board = x$board[i]),
    acp_row(crit, "B16a", "z_k1", NA, x$z_k1[i], board = x$board[i]))))
  rbind(acp_row(crit, "B16a", "transfer", if (any(x$caveat %in% TRUE)) "caveat" else if (anyNA(x$caveat)) "not_obtained" else "transfer"), rows)
}

#' B14: each tag's rate relative to the median of the tags (T1 from its V1 REF-z); eligible for C inside B14a; T1 inside B14b.
acp_decide_tags <- function(tq, crit) {
  if (!nrow(tq)) return(acp_row(crit, "B14a", "tag_qc", "not_obtained"))
  m <- stats::median(tq$rate)
  rows <- data.table::rbindlist(lapply(seq_len(nrow(tq)), function(i) {
    rel <- tq$rate[i] / m
    acp_row(crit, if (tq$tag_label[i] == "T1") "B14b" else "B14a", paste0("rate_rel_median_", tq$tag_label[i]),
            if (tq$tag_label[i] == "T1") (if (acp_test(crit, "B14b", rel)) "T1_within" else "T1_outside")
            else (if (acp_test(crit, "B14a", rel)) "eligible" else "not_eligible"), rel)
  }))
  rows
}

#' Day-2 rule (D1-D6): a recommendation only (Tobias decides). decisions: the decision table; missing: blocks or endpoints
#' not obtained (D2). P (rule P1) is applied to the thresholds inside each CI. Returns data.table(trigger, triggered,
#' reason, day2_content), with a final row "recommendation". Rows of category ACP_ADDED_CATEGORY (criteria v1.2) are
#' outside the rule (G10): they never trigger D2.
acp_decide_day2 <- function(decisions, crit, missing = character()) {
  fac <- acp_th(crit, "P1")
  d <- data.table::as.data.table(decisions)
  g_ <- function(stat, b = "pooled") d[statistic == stat & board == b]
  P_any <- function(row, th) nrow(row) == 1L && any(acp_project(row$estimate, row$lo, row$hi, th, fac)$resolvable)
  content <- function(id) acp_rule(crit, id)$feeds
  rows <- list()
  add <- function(trigger, triggered, reason, id) rows[[length(rows) + 1L]] <<- data.table::data.table(trigger = trigger, triggered = isTRUE(triggered),
                                                                                                        reason = reason, day2_content = if (isTRUE(triggered)) content(id) else "")
  b <- g_("D_col")
  add("D1", nrow(b) == 1L && identical(b$decision, "TIE_inconclusive") && P_any(b, acp_set(crit, "D1a")),
      sprintf("basis %s; D_col %.3f [%.3f, %.3f]", if (nrow(b)) b$decision else "missing", if (nrow(b)) b$estimate else NA, if (nrow(b)) b$lo else NA, if (nrow(b)) b$hi else NA), "D1a")
  no <- unique(c(d[decision == "not_obtained" & !category %in% ACP_ADDED_CATEGORY, paste(criterion, statistic, board)], missing))
  add("D2", length(no) > 0, if (length(no)) paste("not obtained:", paste(no, collapse = "; ")) else "every endpoint obtained", "D2a")
  sd <- d[statistic == "REF_excess_sd"]; le <- d[statistic == "REF_late_early"]
  bc <- g_("board_call"); ba <- g_("board_call_drift_adjusted"); inf <- g_("R_board")
  rx <- g_("REFx_informative")
  d3 <- any(acp_test(crit, "D3a", sd$estimate)) || any(acp_test(crit, "D3b", abs(le$estimate - 1))) ||
    (nrow(bc) == 1L && nrow(ba) == 1L && bc$decision != "not_obtained" && ba$decision != bc$decision) ||
    (nrow(rx) == 1L && isTRUE(rx$estimate == 0) && nrow(bc) == 1L && bc$decision %in% c("difference", "undecided"))
  add("D3", d3, sprintf("REF-x excess SD %s; late/early %s; board call %s, drift-adjusted %s", paste(signif(sd$estimate, 3), collapse = "/"),
                        paste(signif(le$estimate, 3), collapse = "/"), if (nrow(bc)) bc$decision else "missing", if (nrow(ba)) ba$decision else "missing"), "D3a")
  C <- g_("coread_ratio")
  add("D4", nrow(bc) == 1L && bc$decision == "undecided" && (P_any(inf, acp_set(crit, "D4a")) || P_any(C, acp_set(crit, "D4b"))),
      sprintf("board call %s", if (nrow(bc)) bc$decision else "missing"), "D4a")
  tz <- d[grepl("^tau_star_", statistic)]
  add("D5", any(tz$decision == "not_decidable"), paste("tau:", paste(tz$statistic, tz$decision, collapse = "; ")), "D5a")
  d6 <- FALSE; why <- character()
  for (bb in c("2", "4")) {
    cc <- g_("competition_call", bb); sr <- g_("SR", bb)
    if (nrow(cc) == 1L && cc$decision == "intermediate" && (P_any(g_("TR", bb), acp_set(crit, "D6a")) || P_any(g_("MS", bb), acp_set(crit, "D6b")))) { d6 <- TRUE; why <- c(why, paste0("B4 board ", bb)) }
    if (nrow(sr) == 1L && identical(sr$decision, "undecided") && P_any(sr, acp_set(crit, "D6c"))) { d6 <- TRUE; why <- c(why, paste0("B6 board ", bb)) }
  }
  caps <- d[statistic == "competition_call" & decision == "capture_or_starvation"]
  if (nrow(caps) == 1L) { d6 <- TRUE; why <- c(why, paste0("capture/starvation on board ", caps$board, " only")) }
  add("D6", d6, if (length(why)) paste(why, collapse = "; ") else "competition decided or P fails", "D6a")
  tr <- g_("transfer")
  rows[[length(rows) + 1L]] <- data.table::data.table(trigger = "D7", triggered = FALSE, reason = sprintf("transfer %s (reported; never a reason for day 2)",
                                                                                                          if (nrow(tr)) tr$decision else "missing"), day2_content = "")
  out <- data.table::rbindlist(rows)
  rec <- if (any(out$triggered)) paste0("day 2 recommended (Tobias decides): ", paste(out$trigger[out$triggered], collapse = ", "),
                                         "; a separate confirmation experiment, never pooled with day 1 (DC1, DC2), with the descriptive bridge (",
                                         gsub("\\|", ", ", acp_rule(crit, "DC3")$threshold), " plus REF on both boards), seed ",
                                         format(acp_th(crit, "DS1"), scientific = FALSE), ", at most ", acp_th(crit, "DH1"), " h on site") else "no day 2 recommended"
  rbind(out, data.table::data.table(trigger = "recommendation", triggered = any(out$triggered), reason = rec, day2_content = ""))
}

# ---------------------------------------------------------------- v1.2 additions (criteria category added_v1.2)

#' K units (K01): each crossing of a K placement, [t - half_s, t + half_s] truncated halfway to the neighbouring crossings
#' and clipped to the analysed window; cluster = pass (a maximal run of crossings in one column direction). Returns
#' data.table(unit_id, placement_id, board, unit_type "transition", cluster, us, ue, from, to, t_cross, direction (+1 or -1
#' column), row (lower/upper), boundary).
acp_k_units <- function(annotation, windows, half_s = 10) {
  cr <- annotation[kind == "cross"]
  out <- data.table::rbindlist(lapply(unique(cr$placement_id), function(p) {
    w <- windows[placement_id == p & use == TRUE]
    if (nrow(w) != 1L || !identical(w$unit_type, "K")) return(NULL)
    x <- cr[placement_id == p][order(t)]
    n <- nrow(x)
    dirn <- sign(amv_column(x$to_reader) - amv_column(x$from_reader))
    pass <- cumsum(c(TRUE, dirn[-1] != dirn[-n]))
    prev_mid <- c(-Inf, (x$t[-n] + x$t[-1]) / 2); next_mid <- c((x$t[-n] + x$t[-1]) / 2, Inf)
    data.table::data.table(unit_id = paste0(p, "_k", seq_len(n)), placement_id = p, board = w$board, unit_type = "transition",
                           cluster = paste0(p, "_pass", pass), us = pmax(x$t - half_s, prev_mid, w$ws), ue = pmin(x$t + half_s, next_mid, w$we),
                           from = x$from_reader, to = x$to_reader, t_cross = x$t, direction = as.integer(dirn),
                           row = ifelse(x$from_reader <= 4L, "lower", "upper"),
                           boundary = paste0("R", pmin(x$from_reader, x$to_reader), "|R", pmax(x$from_reader, x$to_reader)))
  }))
  if (!nrow(out)) return(data.table::data.table(unit_id = character(), placement_id = character(), board = integer(), unit_type = character(),
                                                cluster = character(), us = numeric(), ue = numeric(), from = integer(), to = integer(),
                                                t_cross = numeric(), direction = integer(), row = character(), boundary = character()))
  out[ue > us]
}

#' Median of x (NA dropped; Inf kept, so a missed transition counts as an infinite latency) with the order-statistic CI
#' of acp_median_ci().
acp_median_ci_inf <- function(x, level = 0.95) {
  x <- sort(x[!is.na(x)]); n <- length(x)
  if (!n) return(data.table::data.table(n = 0L, median = NA_real_, lo = NA_real_, hi = NA_real_))
  k <- 0L
  for (kk in seq_len(floor(n / 2))) if (1 - 2 * stats::pbinom(kk - 1L, n, 0.5) >= level) k <- kk else break
  data.table::data.table(n = n, median = stats::median(x), lo = if (k) x[k] else NA_real_, hi = if (k) x[n - k + 1L] else NA_real_)
}

#' K statistics at tau A03 (K02, K30, K31). grid: acp_reconstruct() rows of the K placements; mtruth: acp_movement_truth()
#' rows (u, adm, eval); units: acp_k_units(). Per unit the correct-column share per basis and d_K = read - event; D_K,
#' D_K_b2 and D_K_b4 with the bootstrap (strata board, clusters the passes); per transition x basis the switching latency
#' (first cell at or after the crossing, inside the unit window, assigned to the destination column; NA = missed) and its
#' summary per board x basis; the hold-core correct-column share of the assigned time per board x basis. Returns
#' list(unit_d, stats, latency, latency_summary, hold_core).
acp_k_analysis <- function(grid, mtruth, units, crit, B = NULL) {
  B <- if (is.null(B)) as.integer(acp_th(crit, "G02")) else as.integer(B)
  tt <- acp_th(crit, "A03"); level <- acp_th(crit, "G01"); seed <- acp_th(crit, "G03"); core <- acp_th(crit, "K31")
  none <- list(unit_d = data.table::data.table(), stats = data.table::data.table(statistic = c("D_K", "D_K_b2", "D_K_b4"), estimate = NA_real_,
                                                                                  lo = NA_real_, hi = NA_real_, n_units = 0L, n_clusters = 0L, B = B),
               latency = data.table::data.table(), latency_summary = data.table::data.table(), hold_core = data.table::data.table())
  if (is.null(units) || !nrow(units) || is.null(grid) || !nrow(grid) || is.null(mtruth) || !nrow(mtruth)) return(none)
  g <- grid[abs(tau - tt) < 1e-9 & placement_id %in% units$placement_id]
  mt <- mtruth[placement_id %in% units$placement_id]
  cells <- acp_unit_cells(mt, units)
  conf <- acp_confusion(g, cells[, .(placement_id, cell, adm, eval, unit_id, unit_type, cluster, board)], by = c("unit_id", "cluster", "board", "basis", "tau"))
  if (!nrow(conf) || !all(c("event", "read") %in% conf$basis)) return(none)
  w <- data.table::dcast(conf, unit_id + cluster + board ~ basis, value.var = "correct_column")
  ud <- w[, .(unit_id, cluster, board, c_read = read, c_event = event, d = read - event)][is.finite(d)]
  f <- function(u) c(D_K = if (nrow(u)) mean(u$d) else NA_real_, D_K_b2 = if (any(u$board == 2L)) mean(u$d[u$board == 2L]) else NA_real_,
                     D_K_b4 = if (any(u$board == 4L)) mean(u$d[u$board == 4L]) else NA_real_)
  st <- acp_boot(ud, f, "board", "cluster", B, seed, level)
  x <- merge(g, mt[, .(placement_id, cell, u)], by = c("placement_id", "cell"))
  lat <- data.table::rbindlist(lapply(seq_len(nrow(units)), function(i) {
    un <- units[i]; pid <- un$placement_id; t0 <- un$t_cross; t1 <- un$ue; dest <- amv_column(un$to)
    y <- x[placement_id == pid & u >= t0 & u < t1]
    data.table::rbindlist(lapply(c("event", "read"), function(b) {
      z <- y[basis == b][order(u)]
      hit <- z[pos > 0L & amv_column(pmax(pos, 1L)) == dest]
      data.table::data.table(unit_id = un$unit_id, placement_id = pid, board = un$board, basis = b, boundary = un$boundary, row = un$row,
                             direction = un$direction, latency_s = if (nrow(hit)) hit$u[1] - t0 else NA_real_)
    }))
  }))
  lat[, missed := is.na(latency_s)]
  ls <- lat[, c(list(transitions = .N, detected = sum(!missed), detected_share = mean(!missed)),
                as.list(acp_median_ci_inf(ifelse(missed, Inf, latency_s), level)[, .(median_s = median, lo_s = lo, hi_s = hi)])), by = .(board, basis)]
  hm <- data.table::rbindlist(lapply(unique(units$placement_id), function(p) {
    mm <- mt[placement_id == p & eval == TRUE]; tcs <- sort(units[placement_id == p]$t_cross)
    k <- findInterval(mm$u, tcs)
    dd <- pmin(ifelse(k > 0L, mm$u - tcs[pmax(k, 1L)], Inf), ifelse(k < length(tcs), tcs[pmin(k + 1L, length(tcs))] - mm$u, Inf))
    mm[dd > core, .(placement_id, cell, adm)][, board := units[placement_id == p]$board[1]]
  }))
  hc <- merge(g, hm, by = c("placement_id", "cell"))
  hc[, ok := pos > 0L & acp_col_distance(adm, pos) %in% 0]
  hold <- hc[, .(cells = .N, assigned = sum(pos > 0L), correct_column_assigned = if (any(pos > 0L)) sum(ok) / sum(pos > 0L) else NA_real_),
             by = .(board, basis)]
  list(unit_d = ud[], stats = st, latency = lat[], latency_summary = ls[], hold_core = hold[])
}

#' K decision (K10-K16) from the D_K statistics: READ_K (and both boards above 0), EVENT_K (both below 0), EQUIVALENT_K,
#' INCONCLUSIVE_K or not_obtained; with the per-board rows (supporting).
acp_decide_k <- function(stats, crit) {
  g <- function(s) { x <- stats[stats$statistic == s]; if (nrow(x) != 1L) data.table::data.table(estimate = NA_real_, lo = NA_real_, hi = NA_real_, n_units = NA_integer_) else x }
  D <- g("D_K"); b2 <- g("D_K_b2"); b4 <- g("D_K_b4")
  t <- function(id, x) acp_test(crit, id, x)
  if (anyNA(c(D$estimate, D$lo, D$hi, b2$estimate, b4$estimate))) { dec <- "not_obtained"; rid <- "K16" }
  else if (t("K10", D$estimate) && t("K11", D$lo) && t("K11a", b2$estimate) && t("K11b", b4$estimate)) { dec <- "READ_K"; rid <- "K10" }
  else if (t("K12", D$estimate) && t("K13", D$hi) && t("K13a", b2$estimate) && t("K13b", b4$estimate)) { dec <- "EVENT_K"; rid <- "K12" }
  else if (t("K14", D$lo) && t("K15", D$hi)) { dec <- "EQUIVALENT_K"; rid <- "K14" }
  else { dec <- "INCONCLUSIVE_K"; rid <- "K16" }
  rbind(acp_row(crit, rid, "D_K", dec, D$estimate, D$lo, D$hi, D$n_units),
        acp_row(crit, "K02", "D_K_b2", NA, b2$estimate, b2$lo, b2$hi, b2$n_units, board = "2"),
        acp_row(crit, "K02", "D_K_b4", NA, b4$estimate, b4$lo, b4$hi, b4$n_units, board = "4"))
}

#' Overall basis verdict (K20): the criterion (a) decision of D_col, except (a) READ with EVENT_K or (a) EVENT with READ_K
#' -> discordant; (a) missing or not obtained -> not_obtained.
acp_decide_basis_overall <- function(dcol, dk, crit) {
  dcol <- if (length(dcol) == 1L && !is.na(dcol)) dcol else "not_obtained"
  dk <- if (length(dk) == 1L && !is.na(dk)) dk else "not_obtained"
  dec <- if (dcol == "not_obtained") "not_obtained" else
    if ((dcol == "READ" && dk == "EVENT_K") || (dcol == "EVENT" && dk == "READ_K")) "discordant" else dcol
  acp_row(crit, "K20", "basis_overall", dec)
}

#' Switching-latency rows (K30) per board x basis: median (missed = Inf) with its order-statistic CI, and the missed count.
acp_k_latency_rows <- function(k, crit) {
  ls <- k$latency_summary
  if (!nrow(ls)) return(NULL)
  data.table::rbindlist(lapply(seq_len(nrow(ls)), function(i) rbind(
    acp_row(crit, "K30", paste0("k_latency_median_s_", ls$basis[i]), NA, ls$median_s[i], ls$lo_s[i], ls$hi_s[i], ls$transitions[i], board = ls$board[i]),
    acp_row(crit, "K30", paste0("k_transitions_missed_", ls$basis[i]), NA, ls$transitions[i] - ls$detected[i], n_units = ls$transitions[i], board = ls$board[i]))))
}

#' Column-level adequacy (KA1-KA8) under the basis of the overall verdict (READ -> read, EVENT -> event, otherwise both):
#' per board x basis the components KA1-KA6 (met, not_met, not_obtained; KA1, KA2 and KA6 over the tested placements, B2b)
#' and the decision: limited when any component is not met, else not_obtained when any lacks data, else adequate.
#' conf_s: static confusion (board, pair_id, basis, tau, assigned, correct_column); k: acp_k_analysis(); conf_hx: the
#' HX static confusion.
acp_decide_column_adequacy <- function(conf_s, k, conf_hx, overall, crit) {
  ov <- if (length(overall) == 1L && !is.na(overall)) overall else ""
  bases <- switch(ov, READ = "read", EVENT = "event", c("event", "read"))
  tt <- acp_th(crit, "A03")
  cs <- conf_s[abs(tau - tt) < 1e-9]
  ch <- if (!is.null(conf_hx) && nrow(conf_hx)) conf_hx[abs(tau - tt) < 1e-9] else NULL
  cca <- function(a, cc) ifelse(a > 0, cc / a, NA_real_)
  fin <- function(v) if (length(v) && any(is.finite(v))) v[is.finite(v)] else NA_real_
  rows <- list(); status <- character()
  comp <- function(rule, stat, b, bs, v, n) {
    st <- if (is.na(v)) "not_obtained" else if (acp_test(crit, rule, v)) "met" else "not_met"
    status <<- c(status, st)
    rows[[length(rows) + 1L]] <<- acp_row(crit, rule, paste0(stat, "_", bs), st, v, n_units = n, board = b)
  }
  for (b in c(2L, 4L)) for (bs in bases) {
    x <- cs[board == b & basis == bs & pair_id %in% acp_set(crit, "A13") & acp_test(crit, "B2b", assigned)]
    comp("KA1", "ka1_centre_min_correct_column", b, bs, if (nrow(x)) min(fin(cca(x$assigned, x$correct_column))) else NA_real_, nrow(x))
    y <- cs[board == b & basis == bs & pair_id %in% acp_set(crit, "A09") & acp_test(crit, "B2b", assigned)]
    yv <- if (nrow(y)) y[, .(v = mean(cca(assigned, correct_column), na.rm = TRUE)), by = pair_id][is.finite(v)] else data.table::data.table(v = numeric())
    comp("KA2", "ka2_boundary_mean_correct_column", b, bs, if (nrow(yv)) mean(yv$v) else NA_real_, nrow(yv))
    hc <- if (nrow(k$hold_core)) k$hold_core[board == b & basis == bs] else k$hold_core
    comp("KA3", "ka3_hold_core_correct_column", b, bs, if (nrow(hc) == 1L) hc$correct_column_assigned else NA_real_, if (nrow(hc) == 1L) hc$cells else 0L)
    lv <- if (nrow(k$latency)) k$latency[board == b & basis == bs] else k$latency
    comp("KA4", "ka4_transitions_detected", b, bs, if (nrow(lv)) mean(!lv$missed) else NA_real_, nrow(lv))
    comp("KA5", "ka5_latency_median_s", b, bs, if (nrow(lv)) stats::median(ifelse(lv$missed, Inf, lv$latency_s)) else NA_real_, nrow(lv))
    he <- if (!is.null(ch)) ch[board == b & basis == bs & pair_id %in% ACP_HOUSE_EDGE_ON & acp_test(crit, "B2b", assigned)] else NULL
    comp("KA6", "ka6_house_edge_min_correct_column", b, bs, if (!is.null(he) && nrow(he)) min(fin(cca(he$assigned, he$correct_column))) else NA_real_,
         if (is.null(he)) 0L else nrow(he))
  }
  dec <- if (any(status == "not_met")) "limited" else if (any(status == "not_obtained")) "not_obtained" else "adequate"
  rbind(acp_row(crit, "KA8", "column_adequacy", dec, sum(status == "not_met"), n_units = length(status)), data.table::rbindlist(rows))
}

#' House pairs (HX02, HX03) over HCx, HCz (B8) and the v1.2 classes HEx, HEy, HSy and HOx: per class x board the log rate
#' ratio house on / lifted at the true reader (continuity G06), P1s in both states, unread in both (B1b; excluded); the
#' extended house ratio over the classes inside the house (ACP_HOUSE_RATIO_EXT_CLASSES) with the bootstrap over pairs
#' (strata board); the pooled ratio per class (HOx included); per placement x basis (tau A03) the
#' shares of the assigned time at the correct reader, in the correct row and in the correct column; the orientation ratio
#' HEy / HEx per state and board. det: detection rows (reader 0) of H01-H04 and H07-H12; conf: their static confusion.
acp_house_extension <- function(det, conf, crit, B = NULL) {
  B <- if (is.null(B)) as.integer(acp_th(crit, "G02")) else as.integer(B)
  k0 <- acp_th(crit, "G06"); tt <- acp_th(crit, "A03")
  cls <- rbind(ACP_HOUSE_PAIRS, ACP_HOUSE_EXT_PAIRS)
  d0 <- det[PositionID == 0L & pair_id %in% c(cls$on, cls$off)]
  p <- data.table::rbindlist(lapply(seq_len(nrow(cls)), function(i) {
    on <- d0[pair_id == cls$on[i], .(board, n_on = reads, T_on = T, P1s_on = P1s)]
    off <- d0[pair_id == cls$off[i], .(board, n_off = reads, T_off = T, P1s_off = P1s)]
    merge(on, off, by = "board")[, pair := cls$pair[i]]
  }))
  if (!nrow(p)) return(list(pairs = p, stats = data.table::data.table(statistic = "house_ratio_ext", estimate = NA_real_, lo = NA_real_, hi = NA_real_,
                                                                      n_units = 0L, n_clusters = 0L, B = B),
                            per_class = data.table::data.table(), assignment = data.table::data.table(), orientation = data.table::data.table()))
  p[, `:=`(log_ratio = log((n_on + k0) / T_on) - log((n_off + k0) / T_off),
           unread_both = acp_test(crit, "B1b", P1s_on) & acp_test(crit, "B1b", P1s_off), unit_id = paste0(pair, "_b", board))]
  p[, included := !unread_both]
  st <- acp_boot(p[included == TRUE & pair %in% ACP_HOUSE_RATIO_EXT_CLASSES], function(u) c(house_ratio_ext = if (nrow(u)) exp(mean(u$log_ratio)) else NA_real_),
                 "board", "unit_id", B, acp_th(crit, "G03"), acp_th(crit, "G01"))
  pc <- p[included == TRUE, .(pairs = .N, ratio = exp(mean(log_ratio))), by = pair]
  a <- if (!is.null(conf) && nrow(conf)) conf[abs(tau - tt) < 1e-9 & pair_id %in% c(cls$on, cls$off)] else data.table::data.table()
  if (nrow(a)) {
    a[, `:=`(pair = c(cls$pair, cls$pair)[match(pair_id, c(cls$on, cls$off))], state = ifelse(pair_id %in% cls$on, "on", "off"),
             correct_column_assigned = ifelse(assigned > 0, correct_column / assigned, NA_real_))]
    a <- a[, .(board, pair, state, pair_id, placement_id, basis, assigned, unassigned, correct_reader_assigned, correct_row_assigned, correct_column_assigned)]
  }
  rate <- function(id) d0[pair_id == id, .(board, r = (reads + k0) / T)]
  ori <- data.table::rbindlist(lapply(c("on", "off"), function(s) {
    x <- rate(if (s == "on") "H07" else "H08"); y <- rate(if (s == "on") "H09" else "H10")
    if (!nrow(x) || !nrow(y)) return(NULL)
    merge(x, y, by = "board", suffixes = c("_HEx", "_HEy"))[, .(board, state = s, ratio_HEy_HEx = r_HEy / r_HEx)]
  }))
  list(pairs = p[], stats = st, per_class = pc[], assignment = a[], orientation = ori[])
}

#' HX02 (extended house ratio: no_attenuation, attenuation, partial with the B8 thresholds; B8c not applied) and the
#' per-class ratios (HX03, supporting).
acp_decide_house_extension <- function(hx, crit) {
  hr <- hx$stats[statistic == "house_ratio_ext"]
  dec <- if (nrow(hr) != 1L || anyNA(c(hr$estimate, hr$lo, hr$hi))) "not_obtained"
         else if (acp_test(crit, "B8a", hr$estimate) && acp_test(crit, "B8b", hr$lo)) "no_attenuation"
         else if (acp_test(crit, "B8d", hr$estimate) && acp_test(crit, "B8e", hr$hi)) "attenuation" else "partial"
  rows <- list(acp_row(crit, "HX02", "house_ratio_ext", dec, hr$estimate[1], hr$lo[1], hr$hi[1], hr$n_units[1]))
  pc <- hx$per_class
  for (cl in c(ACP_HOUSE_PAIRS$pair, ACP_HOUSE_EXT_PAIRS$pair)) {
    z <- if (nrow(pc)) pc[pair == cl] else pc
    rows[[length(rows) + 1L]] <- acp_row(crit, "HX03", paste0("house_ratio_", cl), NA, if (nrow(z)) z$ratio else NA_real_, n_units = if (nrow(z)) z$pairs else 0L)
  }
  data.table::rbindlist(rows)
}

#' Competition with the house lifted (HX10, HX11, HX20): periods C1, C1o, C4 and C4o of both runs of a board pooled by their
#' 10-s blocks (B4f; ids unique per placement), with a block bootstrap within each period (periods drawn in the fixed order
#' C1, C1o, C4, C4o): TR_off = load throughput C4o / C1o, MS_off, HR1 = rate C1 / C1o (T1), HR4 = throughput C4 / C4o, HI =
#' HR4 / HR1, TR_on = C4 / C1 (descriptive; B4 is the v1.1 call); the longest gap of a load tag in C4o.
acp_competition_house <- function(reads, windows, truth, crit, B = NULL) {
  B <- if (is.null(B)) as.integer(acp_th(crit, "G02")) else as.integer(B)
  L <- acp_th(crit, "B4f"); k0 <- acp_th(crit, "G06"); level <- acp_th(crit, "G01"); seed <- acp_th(crit, "G03")
  per_all <- c("C1", "C1o", "C4", "C4o"); load <- c("T1", "T2", "T3", "T4")
  tr <- merge(truth[unit_type == "C" & pair_id %in% per_all & role == "load"], windows[, .(placement_id, ws, we)], by = "placement_id")
  if (!nrow(tr) || !any(tr$pair_id %in% ACP_C_PERIODS_HOUSE_OFF)) {
    return(list(per_tag = data.table::data.table(), stats = data.table::data.table(), gaps = data.table::data.table(), blocks = data.table::data.table()))
  }
  per_tag <- list(); blocks <- list()
  for (i in seq_len(nrow(tr))) {
    x <- tr[i]; tg <- x$tag_label
    rt <- sort(reads[tag_label == tg & board == x$phys_board & t >= x$ws & t < x$we & PositionID %in% acp_mask_readers(x$adm)]$t)
    T <- x$we - x$ws; g <- diff(c(x$ws, rt, x$we))
    per_tag[[i]] <- data.table::data.table(board = x$board, period = x$pair_id, placement_id = x$placement_id, tag_label = tg, T = T, reads = length(rt),
                                           rate = length(rt) / T, longest_gap = max(g))
    nb <- max(1L, ceiling(T / L - 1e-9))
    blocks[[i]] <- data.table::data.table(board = x$board, period = x$pair_id, tag_label = tg, block = paste0(x$placement_id, "_", seq_len(nb)),
                                          reads = tabulate(pmin(floor((rt - x$ws) / L) + 1L, nb), nbins = nb), dur = pmin(L, T - (seq_len(nb) - 1L) * L))
  }
  per_tag <- data.table::rbindlist(per_tag); blocks <- data.table::rbindlist(blocks)
  stat_fn <- function(N, D) {
    has <- function(p, tg) !is.null(N[[p]]) && all(tg %in% colnames(N[[p]]))
    nr <- length(D[[1]]); na <- rep(NA_real_, nr)
    thr <- function(p, tg) if (has(p, tg)) (rowSums(N[[p]][, tg, drop = FALSE]) + k0) / D[[p]] else na
    tot <- if (has("C4o", load)) rowSums(N$C4o[, load, drop = FALSE]) else na
    ms <- if (has("C4o", load)) ifelse(tot > 0, apply(N$C4o[, load, drop = FALSE], 1, max) / tot, NA_real_) else na
    HR1 <- thr("C1", "T1") / thr("C1o", "T1"); HR4 <- thr("C4", load) / thr("C4o", load)
    cbind(TR_off = thr("C4o", load) / thr("C1o", "T1"), MS_off = ms, HR1 = HR1, HR4 = HR4, HI = HR4 / HR1, TR_on = thr("C4", load) / thr("C1", "T1"))
  }
  stats <- data.table::rbindlist(lapply(sort(unique(blocks$board)), function(b) {
    bl <- blocks[board == b]
    per <- intersect(per_all, unique(bl$period))
    mats <- lapply(stats::setNames(per, per), function(p) {
      m <- data.table::dcast(bl[period == p], block ~ tag_label, value.var = "reads", fill = 0L)
      list(cnt = as.matrix(m[, -1, with = FALSE]), dur = bl[period == p, .(dur = dur[1]), by = block]$dur)
    })
    pt <- stat_fn(lapply(mats, function(z) matrix(colSums(z$cnt), 1, dimnames = list(NULL, colnames(z$cnt)))), lapply(mats, function(z) sum(z$dur)))
    reps <- acp_with_seed(seed, {
      N <- list(); D <- list()
      for (p in per) {
        z <- mats[[p]]; nb <- nrow(z$cnt)
        idx <- matrix(sample.int(nb, B * nb, replace = TRUE), B, nb)
        N[[p]] <- vapply(colnames(z$cnt), function(tg) rowSums(matrix(z$cnt[idx, tg], B, nb)), numeric(B))
        if (B == 1L) N[[p]] <- matrix(N[[p]], 1, dimnames = list(NULL, colnames(z$cnt)))
        D[[p]] <- rowSums(matrix(z$dur[idx], B, nb))
      }
      stat_fn(N, D)
    })
    a <- (1 - level) / 2
    data.table::data.table(board = b, statistic = colnames(pt), estimate = as.numeric(pt[1, ]),
      lo = apply(reps, 2, function(v) if (all(is.na(v))) NA_real_ else stats::quantile(v, a, na.rm = TRUE, names = FALSE)),
      hi = apply(reps, 2, function(v) if (all(is.na(v))) NA_real_ else stats::quantile(v, 1 - a, na.rm = TRUE, names = FALSE)),
      n_units = nrow(bl[tag_label == "T1"]), B = B)
  }))
  gaps <- per_tag[period == "C4o", .(max_gap_C4o = max(longest_gap)), by = board]
  list(per_tag = per_tag[], stats = stats[], gaps = gaps[], blocks = blocks[])
}

#' HX10 (competition call with the house lifted, B4 thresholds and precedence), HX11 (HR4 call with the B8 thresholds; HR1
#' supporting) and HX20-HX24 (interaction HI: the CI excludes 1 -> interaction, else inside [HX21, HX22] -> no_interaction,
#' else undecided), per board.
acp_decide_competition_house <- function(ch, crit) {
  st <- ch$stats
  out <- list()
  for (b in c(2L, 4L)) {
    s <- function(nm) { x <- if (nrow(st)) st[board == b & statistic == nm] else st; if (nrow(x) != 1L) data.table::data.table(estimate = NA_real_, lo = NA_real_, hi = NA_real_, n_units = NA_integer_) else x }
    TRo <- s("TR_off"); MSo <- s("MS_off"); HR1 <- s("HR1"); HR4 <- s("HR4"); HI <- s("HI")
    G <- if (nrow(ch$gaps)) ch$gaps[board == b]$max_gap_C4o else numeric(); G <- if (length(G)) G else NA_real_
    call <- if (anyNA(c(TRo$estimate, MSo$estimate, G))) "not_obtained"
            else if (acp_test(crit, "B4d", MSo$estimate) || acp_test(crit, "B4e", G)) "capture_or_starvation"
            else if (acp_test(crit, "B4c", TRo$estimate)) "collision"
            else if (acp_test(crit, "B4a", TRo$estimate) && acp_test(crit, "B4b", MSo$estimate)) "time_sharing" else "intermediate"
    hr4 <- if (anyNA(c(HR4$estimate, HR4$lo, HR4$hi))) "not_obtained"
           else if (acp_test(crit, "B8a", HR4$estimate) && acp_test(crit, "B8b", HR4$lo)) "no_attenuation"
           else if (acp_test(crit, "B8d", HR4$estimate) && acp_test(crit, "B8e", HR4$hi)) "attenuation" else "partial"
    hi <- if (anyNA(c(HI$estimate, HI$lo, HI$hi))) "not_obtained"
          else if (HI$hi < 1 - ACP_EPS || HI$lo > 1 + ACP_EPS) "interaction"
          else if (acp_test(crit, "HX21", HI$lo) && acp_test(crit, "HX22", HI$hi)) "no_interaction" else "undecided"
    out[[length(out) + 1L]] <- rbind(
      acp_row(crit, "HX10", "competition_call_house_off", call, board = b),
      acp_row(crit, "HX10", "TR_off", NA, TRo$estimate, TRo$lo, TRo$hi, TRo$n_units, board = b),
      acp_row(crit, "HX10", "MS_off", NA, MSo$estimate, MSo$lo, MSo$hi, MSo$n_units, board = b),
      acp_row(crit, "HX10", "max_gap_C4o_s", NA, G, board = b),
      acp_row(crit, "HX11", "HR4", hr4, HR4$estimate, HR4$lo, HR4$hi, HR4$n_units, board = b),
      acp_row(crit, "HX11", "HR1", NA, HR1$estimate, HR1$lo, HR1$hi, HR1$n_units, board = b),
      acp_row(crit, switch(hi, interaction = "HX23", no_interaction = "HX21", "HX24"), "HI", hi, HI$estimate, HI$lo, HI$hi, HI$n_units, board = b))
  }
  data.table::rbindlist(out)
}

#' Accuracy by detection age (K32, descriptive): over the evaluated cells of the K and M placements at tau A03 that a basis
#' assigns, the age = time since the tag's latest single read at any reader of its board (from the placement start); the
#' share in the correct column by age class x basis x board x movement type (K or M).
acp_age_accuracy <- function(reads, windows, grid, mtruth, crit) {
  cls <- acp_set(crit, "K32"); tt <- acp_th(crit, "A03")
  w <- windows[use == TRUE & unit_type %in% c("K", "M_pass", "M_visits")]
  if (is.null(grid) || !nrow(grid) || is.null(mtruth) || !nrow(mtruth) || !nrow(w)) return(data.table::data.table())
  g <- grid[abs(tau - tt) < 1e-9 & placement_id %in% w$placement_id]
  x <- merge(g, mtruth[eval == TRUE, .(placement_id, cell, u, adm)], by = c("placement_id", "cell"))[pos > 0L]
  if (!nrow(x)) return(data.table::data.table())
  x <- merge(x, w[, .(placement_id, board, unit_type, phys_board, start, end, tag = tag_label)], by = "placement_id")
  x[, age := {
    tg <- tag[1]; pb <- phys_board[1]; s0 <- start[1]; s1 <- end[1]
    rt <- sort(reads[tag_label == tg & board == pb & t >= s0 & t < s1]$t)
    k <- findInterval(u, rt)
    ifelse(k > 0L, u - rt[pmax(k, 1L)], NA_real_)
  }, by = placement_id]
  lab <- c(paste0("<=", cls[1]), paste0(cls[-length(cls)], "-", cls[-1]), paste0(">", cls[length(cls)]))
  x[, age_class := cut(age, c(-Inf, cls, Inf), labels = lab, right = TRUE)]
  x[, correct := acp_col_distance(adm, pos) %in% 0]
  x[!is.na(age_class), .(cells = .N, correct_column_assigned = mean(correct)),
    by = .(board, basis, movement = ifelse(unit_type == "K", "K", "M"), age_class)][order(board, basis, movement, age_class)]
}

# ---------------------------------------------------------------- the whole analysis

#' The pre-registered analysis of one pilot day, pure (reads and writes no file). schedule: acp_read_schedule(); sheet:
#' acp_check_sheet(); session: acp_read_session(); annotation: acp_check_annotation() (may be NULL); survey:
#' acp_read_survey() (may be NULL); board_history: Stage 00b board_history table; crit: acp_criteria(); B: bootstrap
#' replicates (default the criteria's G02; smaller only in tests). Returns a named list of tables, with `decisions` and
#' `day2`.
acp_analyse <- function(schedule, sheet, session, annotation, survey, board_history, crit, B = NULL, boards = ACP_BOARDS) {
  B <- if (is.null(B)) as.integer(acp_th(crit, "G02")) else as.integer(B)
  out <- list(); missing <- character(); add_missing <- character()
  # sheet errors on the placements added in schedule v3 (K, HX, C1o, C4o) are reported apart and are not D2 reasons (G10)
  added_orders <- as.character(schedule[unit_type %in% c("K", "HX") | pair_id %in% ACP_C_PERIODS_HOUSE_OFF]$order)
  se <- sheet$problems[severity == "error"]
  se_added <- sub("r$", "", se$placement_id) %in% added_orders
  if (any(!se_added)) missing <- c(missing, paste("sheet:", unique(se[!se_added]$check)))
  if (any(se_added)) add_missing <- c(add_missing, paste0("sheet: ", se[se_added]$check, " (", se[se_added]$placement_id, ")"))
  touches <- acp_clapper_touches(session$reads)
  al <- acp_align(sheet$sheet, touches)
  if (!al$n_matched) missing <- c(missing, "clapper and sheet times could not be aligned")
  win <- acp_windows(al$placements)
  out$touches <- al$touches; out$windows <- win
  out$alignment <- data.table::data.table(offset_s = al$offset_s, matched_times = al$n_matched, sheet_times = 2L * nrow(al$placements),
                                          fallback_times = sum(win$start_source == "sheet") + sum(win$end_source == "sheet"))
  out$channel_check <- acp_channel_check(session$reads, win, boards)
  if (!all(out$channel_check$passed)) missing <- c(missing, "a board failed the channel check")
  reqd <- schedule[((unit_type == "C" & pair_id %in% ACP_C_PERIODS) | unit_type == "H") & board %in% names(boards)]$order
  inc <- setdiff(reqd, win[use == TRUE]$order)
  inc <- setdiff(inc, sheet$sheet[status == "dropped" & drop_step == 1L]$order)
  if (length(inc)) missing <- c(missing, paste("C or H block incomplete: orders", paste(inc, collapse = ",")))
  centres <- acp_survey_centres(session$reads, survey, unname(boards))
  out$centres <- centres
  truth <- acp_truth(win, centres)
  out$truth <- truth
  det <- acp_detection(session$reads, win, truth, crit)
  lam <- acp_lambda_sat(det)
  det <- merge(det, lam[, .(board, lambda_sat)], by = "board", all.x = TRUE)[, e := r / lambda_sat]
  out$lambda_sat <- lam; out$detection <- det
  taus <- sort(unique(c(0, acp_th(crit, "A03"), acp_set(crit, "B11a"))))
  step <- acp_th(crit, "A06")
  st_w <- merge(truth[unit_type %in% ACP_SINGLE_TYPES, .(placement_id, tag_label)], win[, .(placement_id, phys_board, start, end, ws, we)], by = "placement_id")
  grid_s <- data.table::rbindlist(lapply(seq_len(nrow(st_w)), function(i) acp_reconstruct(session$events, session$reads, st_w[i], taus, step)))
  conf_s <- merge(acp_confusion(grid_s, truth[, .(placement_id, tag_label, adm)]), truth[, .(placement_id, tag_label, board, pair_id, unit_type)],
                  by = c("placement_id", "tag_label"))
  # secondary: competition seconds per tag (C periods)
  c_w <- merge(truth[unit_type == "C", .(placement_id, tag_label)], win[, .(placement_id, phys_board, start, end, ws, we)], by = "placement_id")
  grid_c <- data.table::rbindlist(lapply(seq_len(nrow(c_w)), function(i) acp_reconstruct(session$events, session$reads, c_w[i], c(0, acp_th(crit, "A03")), step)))
  out$confusion_competition <- if (nrow(grid_c)) merge(acp_confusion(grid_c, truth[, .(placement_id, tag_label, adm)]),
                                                       truth[, .(placement_id, tag_label, board, pair_id, role)], by = c("placement_id", "tag_label")) else NULL
  out$confusion_static <- conf_s
  units <- NULL; conf_m <- data.table::data.table(); mtruth <- NULL; grid_m <- NULL
  if (!is.null(annotation) && nrow(annotation$annotation)) {
    mtruth <- acp_movement_truth(annotation$annotation, win, step, acp_th(crit, "A07"))
    units <- acp_movement_units(annotation$annotation, win, acp_th(crit, "A10"), acp_th(crit, "A11"))
  }
  mv <- win[use == TRUE & unit_type %in% c("M_pass", "M_visits")]
  if (is.null(units) || !nrow(units)) missing <- c(missing, "no movement truth (camera or clock alignment)")
  else {
    nomv <- setdiff(mv$placement_id, units$placement_id)
    if (length(nomv)) missing <- c(missing, paste("no movement truth for placements", paste(nomv, collapse = ",")))
    m_w <- mv[placement_id %in% units$placement_id, .(placement_id, tag_label, phys_board, start, end, ws, we)]
    grid_m <- data.table::rbindlist(lapply(seq_len(nrow(m_w)), function(i) acp_reconstruct(session$events, session$reads, m_w[i], taus, step)))
    cells <- acp_unit_cells(mtruth, units)
    conf_m <- acp_confusion(grid_m, cells[, .(placement_id, cell, adm, eval, unit_id, unit_type, cluster, board)],
                            by = c("unit_id", "unit_type", "cluster", "board", "basis", "tau"))
  }
  out$movement_units <- units; out$confusion_movement <- conf_m
  # criterion (a): column level (decision), reader level, and secondary taus
  tt <- acp_th(crit, "A03")
  bstats <- list(); dec <- list()
  for (lv in c("column", "reader")) {
    u <- acp_basis_units(conf_s, conf_m, crit, lv, tt)
    s <- acp_basis_statistics(u, acp_centre_units(conf_s, crit, tt), crit, B)
    bstats[[lv]] <- cbind(level = lv, tau = tt, s)
    dec[[lv]] <- acp_decide_basis(s, crit, lv)
    out[[paste0("basis_units_", lv)]] <- u
  }
  out$flicker <- acp_flicker(session$events, session$reads, win, crit)
  out$visits <- if (!is.null(units)) acp_visits(session$events, session$reads, win, units, crit) else data.table::data.table()
  dec$tau <- acp_decide_tau(out$flicker, out$visits, det, crit)
  sec_taus <- unique(c(0, dec$tau[grepl("^tau_star_", statistic) & !is.na(estimate)]$estimate))
  for (s_tau in setdiff(sec_taus, tt)) {
    u <- acp_basis_units(conf_s, conf_m, crit, "column", s_tau)
    bstats[[paste0("secondary_tau", s_tau)]] <- cbind(level = "column_secondary", tau = s_tau, acp_basis_statistics(u, acp_centre_units(conf_s, crit, s_tau), crit, B))
  }
  out$basis_statistics <- data.table::rbindlist(bstats)
  dec$resolution <- acp_decide_resolution(conf_s, dec$column[statistic == "D_col"]$decision, crit)
  dec$support <- acp_decide_support_age(det, crit)
  out$coread <- acp_coread(session$reads, win, truth, crit)
  out$competition <- acp_competition(session$reads, win, truth, crit, B)
  dec$competition <- acp_decide_competition(out$competition, crit)
  dec$delta <- acp_decide_delta(out$competition, det, crit)
  out$shielding <- acp_shielding(det, crit, B)
  dec$shielding <- acp_decide_shielding(out$shielding, det, crit)
  out$ref <- acp_ref_repeatability(det, crit)
  out$board_ratio <- acp_board_ratio(det, out$coread$placement, out$ref, crit, B)
  dec$board <- acp_decide_board(out$board_ratio, out$ref, crit)
  out$boundary_timing <- if (!is.null(units)) acp_boundary_timing(session$reads, win, units, crit) else NULL
  out$tag_qc <- acp_tag_qc(det)
  dec$tags <- acp_decide_tags(out$tag_qc, crit)
  sm <- session$sysmsg
  span <- c(if (any(sm$SystemMsg == "start")) min(sm$t[sm$SystemMsg == "start"]) else min(session$events$t),
            if (any(sm$SystemMsg == "end")) max(sm$t[sm$SystemMsg == "end"]) else max(session$events$t + session$events$dur_sec))
  idle <- acp_idle_intervals(win, span, boards)
  out$idle <- idle
  out$known_empty <- acp_known_empty(session$events, idle, crit)
  dec$phantom <- acp_decide_phantom(out$known_empty, crit)
  out$cadence <- acp_cadence(session$events, board_history)
  dec$transfer <- acp_decide_transfer(out$cadence, crit, boards)
  out$vendor_emulation <- acp_vendor_emulation(session$reads, win)
  # ---- v1.2 additions (category added_v1.2): computed apart from the v1.1 endpoints above; outside the day-2 rule (G10)
  # K: column-boundary step transitions
  kw <- win[use == TRUE & unit_type == "K"]
  k_units <- if (!is.null(annotation) && nrow(annotation$annotation)) acp_k_units(annotation$annotation, win, acp_th(crit, "K01")) else NULL
  if (!nrow(kw)) add_missing <- c(add_missing, "no K placement executed")
  else if (is.null(k_units) || !nrow(k_units)) add_missing <- c(add_missing, "no K movement truth (camera or clock alignment)")
  else if (length(setdiff(kw$placement_id, k_units$placement_id))) {
    add_missing <- c(add_missing, paste("no K movement truth for placements", paste(setdiff(kw$placement_id, k_units$placement_id), collapse = ",")))
  }
  grid_k <- if (!is.null(k_units) && nrow(k_units)) {
    k_w <- kw[placement_id %in% k_units$placement_id, .(placement_id, tag_label, phys_board, start, end, ws, we)]
    data.table::rbindlist(lapply(seq_len(nrow(k_w)), function(i) acp_reconstruct(session$events, session$reads, k_w[i], tt, step)))
  } else NULL
  out$k_units <- k_units
  kres <- acp_k_analysis(grid_k, mtruth, k_units, crit, B)
  out$k_unit_d <- kres$unit_d; out$k_statistics <- kres$stats; out$k_latency <- kres$latency
  out$k_latency_summary <- kres$latency_summary; out$k_hold_core <- kres$hold_core
  dec$k <- acp_decide_k(kres$stats, crit)
  dec$basis_overall <- acp_decide_basis_overall(dec$column[statistic == "D_col"]$decision, dec$k[statistic == "D_K"]$decision, crit)
  dec$k_latency <- acp_k_latency_rows(kres, crit)
  # HX: house-extension pairs (static, apart from B1/B3/B8/B10/B11/B13) and the house-lifted competition periods
  det_hx <- acp_detection(session$reads, win, truth, crit, types = "HX")
  if (nrow(det_hx)) det_hx <- merge(det_hx, lam[, .(board, lambda_sat)], by = "board", all.x = TRUE)[, e := r / lambda_sat]
  out$detection_house_ext <- det_hx
  hx_w <- merge(truth[unit_type == "HX", .(placement_id, tag_label)], win[, .(placement_id, phys_board, start, end, ws, we)], by = "placement_id")
  grid_hx <- data.table::rbindlist(lapply(seq_len(nrow(hx_w)), function(i) acp_reconstruct(session$events, session$reads, hx_w[i], c(0, tt), step)))
  conf_hx <- if (nrow(grid_hx)) merge(acp_confusion(grid_hx, truth[, .(placement_id, tag_label, adm)]), truth[, .(placement_id, tag_label, board, pair_id, unit_type)],
                                      by = c("placement_id", "tag_label")) else NULL
  out$confusion_house_ext <- conf_hx
  hx_need <- schedule[unit_type == "HX" & board %in% names(boards)]$order
  hx_inc <- setdiff(setdiff(hx_need, win[use == TRUE]$order), sheet$sheet[status == "dropped" & drop_step == 4L]$order)
  if (length(hx_inc)) add_missing <- c(add_missing, paste("HX pairs incomplete: orders", paste(hx_inc, collapse = ",")))
  out$house_extension <- acp_house_extension(rbind(det[pair_id %in% c(ACP_HOUSE_PAIRS$on, ACP_HOUSE_PAIRS$off)], det_hx, fill = TRUE),
                                             rbind(conf_s[pair_id %in% c(ACP_HOUSE_PAIRS$on, ACP_HOUSE_PAIRS$off)], conf_hx, fill = TRUE), crit, B)
  dec$house_ext <- acp_decide_house_extension(out$house_extension, crit)
  c_need <- schedule[unit_type == "C" & pair_id %in% ACP_C_PERIODS_HOUSE_OFF & board %in% names(boards)]$order
  if (length(setdiff(c_need, win[use == TRUE]$order))) {
    add_missing <- c(add_missing, paste("house-lifted competition periods incomplete: orders", paste(setdiff(c_need, win[use == TRUE]$order), collapse = ",")))
  }
  out$competition_house <- acp_competition_house(session$reads, win, truth, crit, B)
  dec$competition_house <- acp_decide_competition_house(out$competition_house, crit)
  # KA: column-level adequacy under the overall basis verdict
  dec$column_adequacy <- acp_decide_column_adequacy(conf_s, kres, conf_hx, dec$basis_overall$decision, crit)
  out$age_accuracy <- acp_age_accuracy(session$reads, win, rbind(grid_m, grid_k), mtruth, crit)
  out$decisions <- data.table::rbindlist(dec, use.names = TRUE)
  out$missing <- data.table::data.table(item = unique(missing))
  out$missing_additions <- data.table::data.table(item = unique(add_missing))
  out$day2 <- acp_decide_day2(out$decisions, crit, unique(missing))
  out$session <- data.table::data.table(n_sessions = session$n_sessions, devices_expected = session$device_check$devices_expected,
                                        devices_on_grid = session$device_check$devices_on_grid, clock_fold_rows_dropped = session$clock_fold_rows_dropped,
                                        span_start = span[1], span_end = span[2], events = nrow(session$events), reads = nrow(session$reads))
  out
}

#' The isolated run folder <base>/<run_id> and its build folder <base>/.tmp_<run_id>: the base passes
#' mmm_isolated_output_root_check(); refuses a run folder or a build folder that exists.
acp_run_dirs <- function(base, run_id, project_root = mmm_project_root()) {
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,59}$", run_id)) stop("Bad run id: ", run_id, call. = FALSE)
  b <- mmm_isolated_output_root_check(base, project_root)
  out <- file.path(b, run_id); tmp <- file.path(b, paste0(".tmp_", run_id))
  if (file.exists(out)) stop("Run folder exists; choose a new run id: ", out, call. = FALSE)
  if (file.exists(tmp)) stop("A partial run folder exists; inspect and remove it first: ", tmp, call. = FALSE)
  list(base = b, out = out, tmp = tmp)
}
