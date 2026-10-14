# ===========================================================================
# Post-pilot report layer of the one-day RFID calibration pilot: Functions/antenna_calibration_report.R, the status mapping
# docs/rfid_calibration/acp_report_status_v1.0.csv and the writer of Testing/audits/report_rfid_calibration_pilot.R (sourced
# for its definitions; its main block is never run). Portable: synthetic acp run folders under tempdir(), decision rows
# built with the pre-registered acp_row() and acp_decide_*() on the real pinned criteria; nothing on the lab drive.
# Expected values are worked out by hand in the comments.
#
#  1  mapping pin and form: content hash, a tampered copy refused, CRLF accepted, statuses; the superseded feeds are the
#     frozen A40 (= A41), A42 and B7a text, and every criteria row whose feeds speak of bounds is marked; no 'bound'
#     wording in the consequences; unresolved bases are retained as sensitivity analyses
#  2  decision vocabulary from the source: every decision literal of acp_decide_*() and every B9 class pair is mapped
#  3  every decision row the pilot code can emit is mapped: each branch of each decision function, in both directions
#  4  classification by hand on a representative day; an unmapped value and a wrong rule id stop the report
#  5  day-2 trace by hand: D1 (D_col 0.03 [-0.01, 0.07]) and D4 (R_board 0.93 [0.85, 1.01]); agreement with
#     acp_decide_day2(); a tampered recorded trigger is flagged and the recorded verdict stands
#  6  a synthetic acp run folder: verified, read back, reported; README and consequences give the verdict as recorded and
#     no 'bound' wording except the bench-hardware sentence; a changed or unlisted file, an unpinned criteria hash and a
#     renamed folder are refused
#  7  report folder: refused inside the acp run folder, at a base that contains it, in the live root and on a relative
#     path; an existing report or build folder and a bad id refused; the isolated report root accepted (runs/<id>); the
#     protection guard (Functions/protected_outputs.R) around a sandbox build: nothing protected changes, and a protected
#     acp file touched during a build stops it
#  8  group-blind: no group label, CombZ or outcome symbol in the parsed code
# ===========================================================================
suppressPackageStartupMessages(library(data.table))
data.table::setDTthreads(1L)
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "antenna_calibration_report.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
near <- function(a, b, tol = 1e-9) isTRUE(all.equal(as.numeric(a), as.numeric(b), tolerance = tol, scale = 1, check.attributes = FALSE))
fails <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
options(mmm.project_root = file.path(normalizePath(tempdir(), winslash = "/"), "Exp", "Analysis", "Behavior", "RFID"))
for (f in c("project_paths.R", "antenna_calibration_pilot.R", "antenna_calibration_report.R")) source(file.path("Functions", f))
source("Testing/audits/report_rfid_calibration_pilot.R")   # definitions only (main block guarded)
crit <- acp_criteria()
map <- acr_status_map()
tmp <- tempfile("acr_"); dir.create(tmp)
# 'bound' may appear only in the bench-hardware sentence; code identifiers (snake_case, e.g. fresh_le_Gmax_bound) are not wording
no_bound <- function(txt) {
  t <- gsub(ACR_BENCH_SENTENCE, " ", paste(txt, collapse = " "), fixed = TRUE)
  !grepl("bound", gsub("[A-Za-z0-9]+(_[A-Za-z0-9]+)+", " ", t), ignore.case = TRUE)
}

# 1 ----------------------------------------------------------------------------------------------------------------------
check(identical(attr(map, "sha256"), ACR_STATUS_SHA256) && identical(acp_content_sha256(ACR_STATUS_FILE), ACR_STATUS_SHA256),
      "1: the mapping has the pinned content hash")
txt <- readLines(ACR_STATUS_FILE, warn = FALSE, encoding = "UTF-8")
bad <- txt; i1 <- grep("^house_ratio,partial,", bad); bad[i1] <- sub(",uncertain,", ",resolved,", bad[i1], fixed = TRUE)
bad_f <- file.path(tmp, "status_bad.csv"); writeLines(bad, bad_f)
check(length(i1) == 1L && fails(acr_status_map(bad_f)), "1: a mapping with a changed status is refused (tampered hash)")
crlf_f <- file.path(tmp, "status_crlf.csv"); writeBin(charToRaw(paste0(paste(txt, collapse = "\r\n"), "\r\n")), crlf_f)
check(identical(attr(acr_status_map(crlf_f), "sha256"), ACR_STATUS_SHA256), "1: the pin ignores line endings")
check(all(map$status %in% ACR_STATUSES) && all(map[decision_value == ACR_NA]$status == "supporting") &&
        !anyDuplicated(map[, .(statistic, decision_value)]) && all(map$spec_item %in% ACR_SPEC_ITEMS$spec_item), "1: mapping form")
sup <- map[nzchar(superseded_feeds)]
frozen <- function(ids) crit$feeds[match(strsplit(ids, "|", fixed = TRUE)[[1]], crit$rule_id)]
check(nrow(sup) > 0 && all(mapply(function(ids, f) all(frozen(ids) == f), sup$rule_ids, sup$superseded_feeds)),
      "1: superseded_feeds is the frozen feeds text of the row's rule")
check(setequal(unique(unlist(strsplit(sup$rule_ids, "|", fixed = TRUE))), c("A40", "A42", "B7a")) &&
        identical(acp_rule(crit, "A41")$feeds, acp_rule(crit, "A40")$feeds) &&
        setequal(crit[grepl("bound", feeds, ignore.case = TRUE)]$rule_id, c("A40", "A41", "A42", "B7a")) &&
        all(nzchar(map[grepl("A40|A42|B7a", rule_ids)]$superseded_feeds)),
      "1: every frozen feeds text speaking of bounds (A40 = A41, A42, B7a) is marked on every row of those rules")
check(no_bound(map$consequence), "1: no 'bound' wording in the mapping's consequences")
check(all(grepl("sensitivity analys", map[statistic %in% c("D_col", "D_rdr") & decision_value %in% c("TIE_equivalent", "TIE_inconclusive", "not_obtained")]$consequence)) &&
        all(grepl("not limits that bracket true physical occupancy", map[statistic == "D_col" & decision_value %in% c("TIE_equivalent", "TIE_inconclusive")]$consequence, fixed = TRUE)),
      "1: unresolved bases are retained as sensitivity analyses, not as limits on true occupancy")
check(all(grepl("not validated location", map[statistic == "D_col" & decision_value %in% c("READ", "EVENT")]$consequence, fixed = TRUE)) &&
        all(grepl("not a validated location", map[statistic == "fresh_age_s" & decision_value %in% c("fresh_le_Gmax_bound", "smallest_candidate_above_Gmax")]$consequence, fixed = TRUE)),
      "1: recently detected is kept apart from validated location")

# 2 ----------------------------------------------------------------------------------------------------------------------
# String literals of each decision function, minus rule and pair ids, statistic names (and their prefixes) and a short list
# of non-decision literals; what is left must be a decision value of the mapping.
ex <- parse("Functions/antenna_calibration_pilot.R", keep.source = TRUE)
pd <- utils::getParseData(ex)
lits_of <- function(fn) {
  i <- which(vapply(ex, function(e) is.call(e) && identical(as.character(e[[1]]), "<-") && identical(as.character(e[[2]]), fn), logical(1)))
  sr <- attr(ex, "srcref")[[i]]
  unique(gsub('^"|"$', "", pd$text[pd$line1 >= sr[1] & pd$line2 <= sr[3] & pd$token == "STR_CONST"]))
}
DEC_FNS <- c("acp_decide_basis", "acp_decide_resolution", "acp_decide_support_age", "acp_decide_competition", "acp_decide_delta",
             "acp_decide_shielding", "acp_decide_board", "acp_decide_tau", "acp_decide_phantom", "acp_decide_transfer", "acp_decide_tags")
lits <- unique(unlist(lapply(DEC_FNS, lits_of)))
NON_DECISION <- c("D_b2", "D_b4", "_b2", "_b4", "2", "4", "read", "event", "reliable", "missing", " / ", "five_s_visits_kept_%s_tau%g")
is_id <- grepl("^[A-Z][0-9]+[a-z]?$", lits)
is_stat <- vapply(lits, function(l) any(startsWith(unique(map$statistic), l)), logical(1))
left <- lits[!is_id & !is_stat & !lits %in% NON_DECISION]
check(length(left) >= 35 && all(left %in% map$decision_value),
      paste("2: decision literals of the pilot code missing from the mapping:", paste(setdiff(left, map$decision_value), collapse = ", ")))
b1 <- intersect(lits_of("acp_detection"), c("reliable", "border", "unread"))
pairs <- as.vector(outer(c(b1, "missing"), c(b1, "missing"), paste, sep = " / "))
check(length(b1) == 3L && all(pairs %in% map[statistic == "roof_vs_foam_H05_H06"]$decision_value) &&
        all(pairs %in% map[statistic == "foam_vs_HCx_off_H06_H02"]$decision_value), "2: every B9 class pair (B1 classes and missing) is mapped")

# 3 ----------------------------------------------------------------------------------------------------------------------
emit <- list(); E <- function(x) emit[[length(emit) + 1L]] <<- x
# criterion (a): stats D, D_b2, D_b4, D_ctr
bs <- function(D, lo, hi, b2, b4, clo, chi) data.table(statistic = c("D", "D_b2", "D_b4", "D_ctr"), estimate = c(D, b2, b4, 0),
                                                         lo = c(lo, NA, NA, clo), hi = c(hi, NA, NA, chi), n_units = 8L)
for (lv in c("column", "reader")) {
  E(acp_decide_basis(bs(0.2, 0.1, 0.3, 0.2, 0.2, 0, 0.05), crit, lv))            # READ
  E(acp_decide_basis(bs(-0.2, -0.3, -0.1, -0.2, -0.2, -0.05, 0), crit, lv))      # EVENT
  E(acp_decide_basis(bs(0.01, -0.02, 0.04, 0.01, 0.01, -0.01, 0.01), crit, lv))  # TIE_equivalent: CI inside (-0.05, 0.05)
  E(acp_decide_basis(bs(0.06, -0.01, 0.12, 0.1, -0.02, -0.01, 0.01), crit, lv))  # TIE_inconclusive
  E(acp_decide_basis(bs(NA, NA, NA, 0, 0, 0, 0), crit, lv))                      # not_obtained
}
# B2: one centre (P01) and P12/P13 per board and basis
cf <- function(cra, cro) rbindlist(lapply(c(2L, 4L), function(b) rbindlist(lapply(c("event", "read"), function(bb) data.table(
  tau = 2, basis = bb, board = b, pair_id = c("P01", "P12", "P13"), assigned = 0.9, correct_reader_assigned = c(cra, 1, 1),
  correct_row_assigned = c(1, cro, cro))))))
E(acp_decide_resolution(cf(0.99, 0.95), "TIE_equivalent", crit))   # reader; met under both bases
E(acp_decide_resolution(cf(0.5, 0.5), "TIE_inconclusive", crit))   # column; not_met
# B3
dt3 <- function(g2, g4) data.table(PositionID = 0L, unit_type = "P", class = "reliable", board = c(2L, 4L), gmax = c(g2, g4), q99_gap = 1,
                                   missed_share = 0, splits_per_min = 0)
E(acp_decide_support_age(dt3(1.5, 2), crit)); E(acp_decide_support_age(dt3(1.5, 5), crit)); E(acp_decide_support_age(dt3(1.5, 12), crit))
E(acp_decide_support_age(dt3(1.5, 2)[board == 2L], crit))
# B4-B6 (CI half-widths 0.05, 0.1 and as given for SR)
cst <- function(b, TR, MS, CS, SR, SRlo, SRhi) data.table(board = b, statistic = c("TR", "MS", "CS_C4", "SR", "Gini"),
  estimate = c(TR, MS, CS, SR, 0.1), lo = round(c(TR - 0.05, MS - 0.05, CS - 0.1, SRlo, 0.05), 10),
  hi = round(c(TR + 0.05, MS + 0.05, CS + 0.1, SRhi, 0.15), 10), n_units = 30L)
gaps <- function(g, q) data.table(board = c(2L, 4L), max_gap_C4 = g, max_q99_gap_C4 = q)
comp1 <- list(stats = rbind(cst(2L, 1.0, 0.3, 2.0, 0.8, 0.7, 0.9), cst(4L, 0.6, 0.3, 1.0, 1.0, 0.9, 1.1)), gaps = gaps(5, 3))
comp2 <- list(stats = rbind(cst(2L, 0.8, 0.6, 1.3, 0.9, 0.8, 1.05), cst(4L, 0.8, 0.3, 1.3, 0.9, 0.8, 1.05)), gaps = gaps(5, 3))
comp0 <- list(stats = cst(4L, 1, 0.3, 1, 1, 0.9, 1.1)[0], gaps = gaps(5, 3)[0])
E(acp_decide_competition(comp1, crit))   # b2 time_sharing, per_antenna, board_level; b4 collision, spans_R5_R6, none
E(acp_decide_competition(comp2, crit))   # b2 capture_or_starvation; b4 intermediate; CS and SR undecided on both
E(acp_decide_competition(comp0, crit))   # not_obtained everywhere
# B7
E(acp_decide_delta(comp1, dt3(1.5, 2), crit)); E(acp_decide_delta(list(gaps = gaps(500, 400)), dt3(1.5, 2), crit))
E(acp_decide_delta(comp0, dt3(1.5, 2), crit))
# B8, B9: sixteen pseudo-boards give every (H05, H06) pair and, with H02 = H05's class, every (H06, H02) pair
shd <- function(r, lo, hi) list(stats = data.table(statistic = "house_ratio", estimate = r, lo = lo, hi = hi, n_units = 4L),
                                p1s_on_hcz = data.table(board = c(2L, 4L), P1s_on = 0.99))
cls <- c("reliable", "border", "unread", "missing"); cmb <- CJ(a = cls, b = cls)
roof16 <- rbindlist(lapply(seq_len(nrow(cmb)), function(k) data.table(PositionID = 0L, board = k, pair_id = c("H05", "H06", "H02"),
                                                                      class = c(cmb$a[k], cmb$b[k], cmb$a[k]))))[class != "missing"]
# "missing / missing" needs the third placement present: roof pairs dropped (drop step 1) with H02 run, and H05 alone
roof16 <- rbind(roof16, data.table(PositionID = 0L, board = c(17L, 18L), pair_id = c("H02", "H05"), class = "reliable"))
E(acp_decide_shielding(shd(0.95, 0.85, 1.05), roof16, crit))   # no_shielding
E(acp_decide_shielding(shd(0.6, 0.5, 0.8), roof16[0], crit))   # shielding
E(acp_decide_shielding(shd(0.8, 0.7, 0.9), roof16[0], crit))   # partial
E(acp_decide_shielding(shd(NA, NA, NA), roof16[0], crit))      # not_obtained
# B10
brs <- function(R, Rlo, Rhi, RA, RAlo, RAhi, C, Clo, Chi) list(
  stats = data.table(statistic = c("R_board", "R_board_drift_adjusted", "coread_ratio"), estimate = c(R, RA, C), lo = c(Rlo, RAlo, Clo),
                     hi = c(Rhi, RAhi, Chi), n_units = 23L),
  by_group = data.table(grp = c("house_end_R1_R2_R5_R6", "R4_R8", "other"), ratio = c(0.8, 0.9, 1), pairs = c(10L, 6L, 5L)))
ref_ok <- data.table(board = c(2L, 4L), excess_sd = 0.05, late_early = 1.02, replicates = 3L, refx_informative = TRUE)
E(acp_decide_board(brs(0.8, 0.7, 0.9, 0.8, 0.7, 0.9, 0.9, 0.8, 1.0), ref_ok, crit))         # difference
E(acp_decide_board(brs(1.0, 0.95, 1.05, 1.0, 0.95, 1.05, 1.0, 0.8, 1.2), ref_ok, crit))     # no_difference
E(acp_decide_board(brs(0.93, 0.85, 1.01, 0.93, 0.85, 1.01, 0.9, 0.8, 1.0), ref_ok, crit))   # undecided
E(acp_decide_board(brs(1.0, 0.95, 1.05, NA, NA, NA, 1.0, 0.8, 1.2), ref_ok, crit))          # no_difference; adjusted not_obtained
E(acp_decide_board(brs(NA, NA, NA, NA, NA, NA, NA, NA, NA), ref_ok, crit))                  # not_obtained
# B11: removal >= 0.99 from tau `from` on; 8 five-second visits kept up to tau `upto`; P07 for D5b
taus <- acp_set(crit, "B11a")
fl <- function(bb, from, raw = 100L) data.table(basis = bb, tau = rep(taus, 2), board = rep(c(2L, 4L), each = length(taus)), changes_raw = raw,
                                                changes_kept = ifelse(rep(taus, 2) >= from, 0L, 50L))
vis <- function(bb, upto) data.table(basis = bb, dwell_nominal = 5, tau = rep(taus, each = 8), kept = rep(taus, each = 8) <= upto)
p07 <- function(p) data.table(PositionID = 0L, pair_id = "P07", P1s = p)
tau_case <- function(fe, ue, fr, ur, p = 0.9, raw_r = 100L) acp_decide_tau(list(changes = rbind(fl("event", fe), fl("read", fr, raw_r))),
                                                                          rbind(vis("event", ue), vis("read", ur)), p07(p), crit)
E(tau_case(1, 2, 5, 20))             # event tau_star (1), read tau_star_above_flip2s (5)
E(tau_case(5, 20, 10, 2))            # event above, read range
E(tau_case(10, 2, 1, 2))             # event range, read tau_star
E(tau_case(1, 2, 1, 2, raw_r = 10L)) # read not_decidable (10 changes per board)
E(tau_case(1, 2, 1, 2, p = 0))       # P07 unread: both not_decidable
# B14-B16
E(acp_decide_tags(data.table(tag_label = paste0("T", 1:6), rate = c(2.44, 2.5, 2.45, 3.5, 2.4, 2.45)), crit))   # T1_within, eligible, not_eligible
E(acp_decide_tags(data.table(tag_label = paste0("T", 1:6), rate = c(2.0, 2.5, 2.45, 2.45, 2.4, 2.45)), crit))   # T1_outside
E(acp_decide_tags(data.table(tag_label = character(), rate = numeric()), crit))                                  # not_obtained
ke <- function(ev) data.table(board = c(1L, 3L, 5L, NA), board_hours = c(3.5, 3.5, 3.5, 10.5), events = c(ev, 0L, 0L, ev), upper_per_h = 0.3)
E(acp_decide_phantom(ke(0L), crit)); E(acp_decide_phantom(ke(2L), crit)); E(acp_decide_phantom(ke(0L)[!is.na(board)], crit))
E(acp_decide_transfer(data.table(board = c(2L, 4L), z_gap = c(0.5, 4), z_k1 = c(0.2, 0.1)), crit))   # caveat (board 4)
E(acp_decide_transfer(data.table(board = c(2L, 4L), z_gap = c(0.5, 1), z_k1 = c(0.2, 0.1)), crit))   # transfer
E(acp_decide_transfer(data.table(board = c(1L, 3L), z_gap = 0, z_k1 = 0), crit))                     # not_obtained
all_rows <- rbindlist(emit)
cl_all <- acr_classify(all_rows, map)
check(nrow(cl_all) == nrow(all_rows) && !anyNA(cl_all$status), "3: every emitted decision row is mapped and classified")
produced <- unique(paste(acr_statistic_key(all_rows$statistic), acr_decision_value(all_rows$decision), sep = " = "))
mapped <- paste(map$statistic, map$decision_value, sep = " = ")
check(setequal(produced, mapped), paste("3: mapping rows and emitted rows differ:", paste(c(setdiff(mapped, produced), setdiff(produced, mapped)), collapse = "; ")))

# 4 ----------------------------------------------------------------------------------------------------------------------
# A representative day: basis TIE_inconclusive at both levels (D1 case), column resolution, fresh 2 s, competition comp2
# (board 2 capture -> D6 by the one-board rule; board 4 intermediate with TR 0.8 [0.75, 0.85]), delta* 10 s, partial
# shielding, roof reliable on board 2 and unread with the foam stack unread on board 4, board call undecided (D4 case),
# tau resolved for both bases, tags, phantom and transfer resolved.
roof_day <- data.table(PositionID = 0L, board = rep(c(2L, 4L), each = 3), pair_id = rep(c("H05", "H06", "H02"), 2),
                       class = c("reliable", "reliable", "reliable", "unread", "unread", "reliable"))
day <- rbind(acp_decide_basis(bs(0.03, -0.01, 0.07, 0.05, 0.01, -0.01, 0.01), crit, "column"),
             acp_decide_basis(bs(0.03, -0.01, 0.07, 0.05, 0.01, -0.01, 0.01), crit, "reader"),
             acp_decide_resolution(cf(0.5, 0.5), "TIE_inconclusive", crit), acp_decide_support_age(dt3(1.5, 2), crit),
             acp_decide_competition(comp2, crit), acp_decide_delta(comp2, dt3(1.5, 2), crit),
             acp_decide_shielding(shd(0.8, 0.7, 0.9), roof_day, crit),
             acp_decide_board(brs(0.93, 0.85, 1.01, 0.93, 0.85, 1.01, 0.9, 0.8, 1.0), ref_ok, crit), tau_case(1, 2, 5, 20),
             acp_decide_tags(data.table(tag_label = paste0("T", c(2, 4, 5, 6, 3, 1)), rate = c(2.5, 2.45, 2.4, 2.45, 2.42, 2.44)), crit),
             acp_decide_phantom(ke(0L), crit), acp_decide_transfer(data.table(board = c(2L, 4L), z_gap = c(0.4, -1.2), z_k1 = c(0.3, 0.8)), crit))
day2 <- acp_decide_day2(day, crit)   # the pre-registered rule: the recorded verdict of this day
check(identical(stats::setNames(day2$triggered, day2$trigger), c(D1 = TRUE, D2 = FALSE, D3 = FALSE, D4 = TRUE, D5 = FALSE, D6 = TRUE, D7 = FALSE,
                                                                 recommendation = TRUE)), "4: the representative day triggers D1, D4 and D6")
rep4 <- acr_summary(day, map, crit, day2)
cl <- rep4$classified
row4 <- function(stat, b = "pooled") cl[statistic == stat & board == b]
check(nrow(cl) == nrow(day) && identical(cl$statistic, day$statistic), "4: one classified row per decision row, in order")
x <- row4("D_col")
check(x$status == "uncertain" && x$spec_item == "basis" && x$day2_path == "D1" && x$day2_applies && grepl("A42", x$feeds_note) &&
        grepl("sensitivity analyses", x$consequence), "4: D_col TIE_inconclusive is uncertain, D1 applies, A42's frozen text is superseded")
x <- row4("D_rdr")
check(x$status == "uncertain" && x$spec_item == "reader-level basis" && !x$day2_applies && x$day2_note == "no day-2 path",
      "4: D_rdr TIE_inconclusive has no day-2 path")
check(row4("competition_call", "2")$status == "resolved" && row4("competition_call", "2")$day2_applies &&
        row4("competition_call", "4")$status == "uncertain" && row4("competition_call", "4")$day2_applies,
      "4: capture on board 2 only and the intermediate board 4 (TR at 0.85 resolvable) are both addressed by D6")
check(row4("SR", "2")$status == "uncertain" && !row4("SR", "2")$day2_applies && grepl("neither the projection test nor the one-board capture rule holds", row4("SR", "4")$day2_note),
      "4: SR undecided (0.9 [0.8, 1.05]; |0.9 - 0.85| = 0.05 < 0.0884) is not addressed although D6 is recorded")
check(row4("board_call")$status == "uncertain" && row4("board_call")$day2_applies && row4("resolution")$status == "resolved" &&
        row4("Gmax_s", "2")$status == "supporting" && row4("fresh_age_s")$decision == "fresh_le_Gmax_bound" && row4("fresh_age_s")$status == "resolved",
      "4: board call uncertain with D4; column resolution resolved; Gmax supporting; fresh 2 s resolved")
x <- row4("delta_s", "4")
check(x$status == "resolved" && near(x$estimate, 10) && grepl("B7a", x$feeds_note) && nzchar(row4("Gmax_reliable_s")$feeds_note),
      "4: delta* = 10 s resolved; B7a's frozen text superseded on every B7a row")
check(row4("roof_vs_foam_H05_H06", "2")$decision == "reliable / reliable" && row4("roof_vs_foam_H05_H06", "2")$status == "resolved" &&
        grepl("height rather than the roof material", row4("roof_vs_foam_H05_H06", "4")$consequence) && row4("house_ratio")$status == "uncertain",
      "4: roof classes by board; partial shielding uncertain")
check(setequal(paste(rep4$uncertain_without_day2$statistic, rep4$uncertain_without_day2$board),
               c("D_rdr pooled", "CS_C4 2", "CS_C4 4", "SR 2", "SR 4", "house_ratio pooled")), "4: uncertain results without a day-2 path")
check(nrow(rep4$not_obtained) == 0L && nrow(rep4$resolved) + nrow(rep4$uncertain) + sum(cl$status == "supporting") == nrow(cl), "4: the three lists")
sp <- rep4$spec
check(nrow(sp) == nrow(ACR_SPEC_ITEMS) && sp[spec_item == "basis"]$status == "uncertain" && sp[spec_item == "competition in one field"]$status == "uncertain" &&
        sp[spec_item == "tau"]$status == "resolved" && sp[spec_item == "footprint wording"]$status == "supporting" &&
        sp[spec_item == "roof"]$status == "resolved" && grepl("[roof_vs_foam_H05_H06 (board 2)", sp[spec_item == "roof"]$consequence, fixed = TRUE),
      "4: one row per specification item with the worst status of its governing rows")
check(fails(acr_classify(copy(day)[statistic == "board_call", decision := "MAYBE"], map)), "4: an unmapped decision value stops the report")
check(fails(acr_classify(copy(day)[statistic == "D_col", rule_id := "A20"], map)), "4: a rule id the mapping does not list stops the report")

# 5 ----------------------------------------------------------------------------------------------------------------------
# D1: D_col 0.03 [-0.01, 0.07], h = 0.04, h / sqrt(2) = 0.028284; c = -0.05 outside; c = 0 inside, |0.03 - 0| = 0.03 > 0.028
# (resolvable); c = 0.05 inside, 0.02 < 0.028 (not).
# D4: R_board 0.93 [0.85, 1.01], h = 0.08, h / sqrt(2) = 0.056569; c = 0.9 inside, 0.03 (not); c = 1 inside, 0.07 (resolvable);
# c = 1.11 outside; co-read 0.9 [0.8, 1.0]: c = 0.7 outside.
tr <- rep4$trace
d1 <- tr[trigger == "D1"][order(threshold)]
check(nrow(d1) == 3L && near(d1$threshold, c(-0.05, 0, 0.05)) && identical(d1$inside, c(FALSE, TRUE, TRUE)) && near(d1$h, rep(0.04, 3)) &&
        near(d1$distance, c(0.08, 0.03, 0.02)) && near(d1$h_over_sqrt2, rep(0.04 / sqrt(2), 3), 1e-12) && identical(d1$resolvable, c(FALSE, TRUE, FALSE)) &&
        identical(d1$counts, c(FALSE, TRUE, FALSE)) && all(d1$precondition) && all(d1$governing_decision == "TIE_inconclusive"), "5: D1 trace by hand")
d4 <- tr[trigger == "D4"][order(threshold)]
check(nrow(d4) == 4L && sum(d4$statistic == "R_board") == 3L && sum(d4$statistic == "coread_ratio") == 1L &&
        near(d4[statistic == "R_board"]$threshold, c(0.9, 1, 1.11)) && identical(d4[statistic == "R_board"]$inside, c(TRUE, TRUE, FALSE)) &&
        near(d4[statistic == "R_board"]$h, rep(0.08, 3)) && near(d4[statistic == "R_board"]$distance, c(0.03, 0.07, 0.18)) &&
        near(d4[statistic == "R_board"]$h_over_sqrt2, rep(0.08 / sqrt(2), 3), 1e-12) && identical(d4[statistic == "R_board"]$resolvable, c(FALSE, TRUE, FALSE)) &&
        identical(d4[statistic == "coread_ratio"]$inside, FALSE) && sum(d4$counts) == 1L, "5: D4 trace by hand")
d6 <- tr[trigger == "D6"]
check(isTRUE(d6[rule_id == "D6d" & board == "2"]$counts) && !isTRUE(d6[rule_id == "D6d" & board == "4"]$counts) &&
        isTRUE(d6[rule_id == "D6a" & board == "4" & abs(threshold - 0.85) < 1e-9]$counts) && !any(d6[rule_id == "D6c"]$counts),
      "5: D6 trace: capture on board 2 only; TR at 0.85 on board 4 (|0.8 - 0.85| = 0.05 > 0.035); SR fails")
ck <- rep4$check
check(all(ck[trigger %in% c("D1", "D4", "D5", "D6", "D7", "recommendation")]$agree) && !length(rep4$flags) &&
        identical(ck[trigger %in% c("D1", "D4", "D6")]$recomputed, ck[trigger %in% c("D1", "D4", "D6")]$recorded), "5: the trace agrees with acp_decide_day2()")
day2_bad <- copy(day2)[trigger == "D1", triggered := FALSE]
rep5 <- acr_summary(day, map, crit, day2_bad)
check(length(rep5$flags) == 1L && grepl("^D1: recorded FALSE", rep5$flags) && identical(rep5$verdict$reason, day2[trigger == "recommendation"]$reason) &&
        !rep5$classified[statistic == "D_col"]$day2_applies && "D_col" %in% rep5$uncertain_without_day2$statistic,
      "5: a recorded trigger the decision table contradicts is flagged; the recorded verdict stands")
check(any(grepl("FLAG: D1: recorded FALSE", acr_readme_lines(rep5, list(report_id = "dev_x", commit = "c", acp_dir = "x", release = TRUE, map_sha256 = "m",
                                                                      acp_run = c(git_commit = "g", dev_run = "FALSE", code_clean = "TRUE", board_history_pinned = "TRUE"))),
                fixed = TRUE)), "5: the README prints the flag")
day_no <- rbind(day[statistic != "house_ratio"], acp_decide_shielding(shd(NA, NA, NA), roof16[0], crit)[statistic == "house_ratio"])
rep5b <- acr_summary(day_no, map, crit, acp_decide_day2(day_no, crit))
check(isTRUE(rep5b$check[trigger == "D2"]$agree) && rep5b$not_obtained$statistic == "house_ratio" && rep5b$not_obtained$day2_applies,
      "5: a not_obtained decision implies the recorded D2")

# 6 ----------------------------------------------------------------------------------------------------------------------
pr <- getOption("mmm.project_root"); beh <- dirname(pr)
det_tab <- data.table(board = c(2L, 2L, 2L, 4L, 4L, 4L, 2L), PositionID = c(0L, 0L, 0L, 0L, 0L, 0L, 5L),
                      class = c("reliable", "reliable", "border", "reliable", "unread", "border", NA))
run_items <- function(id, crit_sha = ACP_CRITERIA_SHA256) data.table(
  item = c("run_id", "criteria_version", "criteria_content_sha256", "criteria_pinned_sha256", "schedule_version", "schedule_content_sha256",
           "schedule_pinned_sha256", "board_history_run", "board_history_content_sha256", "board_history_pinned", "bootstrap_B", "bootstrap_seed",
           "git_commit", "code_clean", "dev_run", "raw_dir", "raw_sha256_manifest", "started", "finished", "R"),
  value = c(id, ACP_VERSION, crit_sha, ACP_CRITERIA_SHA256, ACP_SCHEDULE_VERSION, ACP_SCHEDULE_SHA256, ACP_SCHEDULE_SHA256, ACP_BOARD_HISTORY_RUN,
            ACP_BOARD_HISTORY_SHA256, "TRUE", "10000", "20261014", strrep("a", 40), "TRUE", "FALSE", "raw", "TRUE", "2026-11-03 17:00:00",
            "2026-11-03 17:20:00", R.version.string))
#' A finished acp run folder as the runner leaves it (decisions, day 2, a table, manifest/run.csv, output hashes).
make_acp <- function(id, crit_sha = ACP_CRITERIA_SHA256, run_id = id) {
  d <- file.path(beh, "RANM", "calibration", id)
  dir.create(file.path(d, "tables"), recursive = TRUE); dir.create(file.path(d, "manifest"))
  fwrite(day, file.path(d, "decisions.csv")); fwrite(day2, file.path(d, "day2_recommendation.csv"))
  fwrite(det_tab, file.path(d, "tables", "detection.csv")); fwrite(run_items(run_id, crit_sha), file.path(d, "manifest", "run.csv"))
  outs <- list.files(d, recursive = TRUE)
  fwrite(data.table(file = outs, sha256 = mmm_file_sha256(file.path(d, outs)), bytes = file.size(file.path(d, outs))),
         file.path(d, "manifest", "output_files_sha256.csv"))
  d
}
acp_dir <- make_acp("acp_v1.0_20261103_abcdef0")
v <- acr_verify_acp_run(acp_dir)
check(v$release && identical(v$dir, normalizePath(acp_dir, winslash = "/")) && nrow(v$files) == 4L, "6: a finished release acp run is verified")
rd <- acr_read_acp(v$dir)
check(identical(rd$decisions$decision, day$decision) && identical(rd$decisions$board, day$board) && near(rd$decisions$estimate, day$estimate) &&
        near(rd$decisions$lo, day$lo) && identical(rd$day2$triggered, day2$triggered) && identical(rd$day2$reason, day2$reason),
      "6: decisions and the recorded day-2 verdict read back as written")
fp <- acr_footprint(fread(file.path(v$dir, "tables", "detection.csv"), select = c("board", "PositionID", "class")))
check(identical(fp, "board 2: 2 reliable, 1 border; board 4: 1 reliable, 1 border, 1 unread"), "6: footprint classes at the true reader by board")
rep6 <- acr_summary(rd$decisions, map, crit, rd$day2, fp)
check(identical(rep6$classified$status, rep4$classified$status) && identical(rep6$classified$day2_applies, rep4$classified$day2_applies) &&
        identical(rep6$trace$counts, rep4$trace$counts) && !length(rep6$flags), "6: the report from the files equals the report in memory")
out6 <- file.path(tmp, "report")
info <- list(report_id = "dev_acr_test", commit = strrep("b", 40), acp_dir = v$dir, acp_run = v$run, release = v$release, map_sha256 = attr(map, "sha256"))
acr_write_report(rep6, out6, info)
check(all(file.exists(file.path(out6, ACR_REPORT_FILES))), "6: every report file is written")
readme <- readLines(file.path(out6, "README.txt"), warn = FALSE)
flat <- gsub("\\s+", " ", paste(readme, collapse = " "))
check(sum(readme == ACR_BENCH_SENTENCE) == 1L && any(readme == ACR_SCOPE) &&
        sum(grepl("bound", gsub("[A-Za-z0-9]+(_[A-Za-z0-9]+)+", " ", readme), ignore.case = TRUE)) == 1L &&
        grepl("boards 2 and 4 under bench conditions", flat, fixed = TRUE) && grepl("no Exp9 location is validated", flat, fixed = TRUE),
      "6: the README states the bench scope; 'bound' appears only in the bench-hardware sentence")
check(no_bound(readme), "6: no 'bound' wording for the bases in the README")
check(grepl(day2[trigger == "recommendation"]$reason, flat, fixed = TRUE) && grepl("D1 TRIGGERED", flat, fixed = TRUE) &&
        grepl("D2 not triggered", flat, fixed = TRUE) && grepl("(as recorded in the acp run's day2_recommendation.csv", flat, fixed = TRUE),
      "6: the day-2 verdict is printed as recorded")
check(grepl(sprintf("RESOLVED (%d)", nrow(rep6$resolved)), flat, fixed = TRUE) && grepl(sprintf("UNCERTAIN (%d; 6 without", nrow(rep6$uncertain)), flat, fixed = TRUE) &&
        grepl("NOT OBTAINED (0)", flat, fixed = TRUE) && grepl(paste("A40-A42 and B7a in", ACP_VERSION, "is unchanged"), flat, fixed = TRUE),
      "6: resolved, uncertain and not-obtained lists; the frozen criteria text is declared unchanged")
sc <- fread(file.path(out6, "spec_consequences.csv"))
sb <- fread(file.path(out6, "status_by_decision.csv"))
check(nrow(sc) == nrow(ACR_SPEC_ITEMS) && no_bound(unlist(sc)) && no_bound(sb$consequence) && no_bound(sb$feeds_note) &&
        identical(sc[spec_item == "footprint wording"]$decisions, fp), "6: no 'bound' wording in the consequences")
tr6 <- fread(file.path(out6, "day2_trace.csv"))
check(nrow(tr6) == nrow(rep6$trace) && near(tr6[trigger == "D1" & threshold == 0]$h_over_sqrt2, 0.04 / sqrt(2), 1e-12), "6: the trace is written")
# refusals of a changed, extended, unpinned or renamed acp run
a2 <- make_acp("acp_v1.0_20261103_changed0"); write("x", file.path(a2, "decisions.csv"), append = TRUE)
check(fails(acr_verify_acp_run(a2)), "6: a changed decisions.csv is refused")
a3 <- make_acp("acp_v1.0_20261103_extra00"); writeLines("x", file.path(a3, "tables", "added.csv"))
check(fails(acr_verify_acp_run(a3)), "6: an unlisted file is refused")
a4 <- make_acp("acp_v1.0_20261103_crit000", crit_sha = strrep("0", 64))
check(fails(acr_verify_acp_run(a4)), "6: a run that recorded other criteria is refused")
a5 <- make_acp("acp_v1.0_20261103_renamed", run_id = "acp_v1.0_20261103_other00")
check(fails(acr_verify_acp_run(a5)), "6: a folder whose run.csv names another run is refused")
check(fails(acr_verify_acp_run("relative/acp_run")) && fails(acr_verify_acp_run(file.path(beh, "RANM", "calibration", "missing"))),
      "6: a relative or missing acp run is refused")
a6 <- make_acp("acp_v1.0_20261103_dev0000"); r6 <- run_items("acp_v1.0_20261103_dev0000")[item == "dev_run", value := "TRUE"]
fwrite(r6, file.path(a6, "manifest", "run.csv")); outs6 <- setdiff(list.files(a6, recursive = TRUE), "manifest/output_files_sha256.csv")
fwrite(data.table(file = outs6, sha256 = mmm_file_sha256(file.path(a6, outs6))), file.path(a6, "manifest", "output_files_sha256.csv"))
check(!acr_verify_acp_run(a6)$release, "6: a dev acp run is verified but is not a release")

# 7 ----------------------------------------------------------------------------------------------------------------------
rep_base <- file.path(beh, "RANM", "calibration_reports")
refuse <- function(base, id = "acr_test") fails(acr_report_dirs(acp_dir, base, id))
check(refuse(acp_dir) && refuse(file.path(acp_dir, "reports")) && refuse(file.path(acp_dir, "tables")), "7: refused inside the acp run folder")
check(refuse(dirname(acp_dir)) && refuse(file.path(beh, "RANM")), "7: refused at a base that contains the acp run folder")
check(refuse(file.path(pr, "analysis_ready", "x")) && refuse("relative/RANM/calibration_reports") &&
        refuse(file.path(beh, "RANM", "measurement_validation")), "7: the live root, a relative path and measurement validation are refused")
rd7 <- acr_report_dirs(acp_dir, rep_base, "acr_test")
check(identical(rd7$out, file.path(normalizePath(rep_base, winslash = "/", mustWork = FALSE), "runs", "acr_test")) &&
        identical(rd7$tmp, file.path(normalizePath(rep_base, winslash = "/", mustWork = FALSE), "runs", ".tmp_acr_test")),
      "7: the isolated report root is accepted")
dir.create(rd7$out, recursive = TRUE); check(refuse(rep_base), "7: an existing report folder is refused"); unlink(rd7$out, recursive = TRUE)
dir.create(rd7$tmp, recursive = TRUE); check(refuse(rep_base), "7: an existing build folder is refused"); unlink(rd7$tmp, recursive = TRUE)
check(refuse(rep_base, "bad id") && refuse(rep_base, ".hidden"), "7: a bad report id is refused")
check(identical(acr_report_args(c("--acp-run=C:/x/acp", "--report-id=dev_1"))$report_id, "dev_1") && fails(acr_report_args(character())) &&
        fails(acr_report_args(c("--acp-run=C:/x", "--acp-run=C:/y"))) && fails(acr_report_args("--date=20261103")), "7: arguments")
# The protection guard of the main block around a report build, on this sandbox root (the frozen-input guard does not
# apply there): the acp runs (in the RANM tree) and the repository pin files, docs/rfid_calibration included, are
# snapshotted; a report build leaves them unchanged, and a protected acp file touched during a build stops it.
for (f in c("qc_run_protection.R", "frozen_input_guard.R", "protected_outputs.R")) source(file.path("Functions", f))
d7 <- acr_report_dirs(acp_dir, rep_base, "dev_acr_guard")
g7 <- suppressMessages(mmm_protection_guard_begin(d7$base, "dev_acr_guard", "calibration report", pr))
acr_write_report(rep6, d7$tmp, info)
ch7 <- suppressMessages(mmm_protection_guard_end(g7, file.path(d7$tmp, "manifest")))
snap <- g7$before[exists == TRUE]
check(!nrow(ch7[verdict == "changed"]) && file.exists(file.path(d7$tmp, "manifest", "protection_check.csv")) &&
        any(endsWith(snap$path, "acp_v1.0_20261103_abcdef0/decisions.csv")) &&
        !is.na(snap[endsWith(path, "rfid_calibration/acp_report_status_v1.0.csv")]$sha256) &&
        !is.na(snap[endsWith(path, "rfid_calibration/acp_criteria_v1.0.csv")]$sha256) && !any(grepl("calibration_reports/runs", snap$path)),
      "7: a report build leaves every protected file unchanged (acp runs, docs/rfid_calibration and the other pins snapshotted)")
d7b <- acr_report_dirs(acp_dir, rep_base, "dev_acr_guard_b")
g7b <- suppressMessages(mmm_protection_guard_begin(d7b$base, "dev_acr_guard_b", "calibration report", pr))
acr_write_report(rep6, d7b$tmp, info)
write("x", file.path(acp_dir, "tables", "detection.csv"), append = TRUE)   # a size change
check(fails(suppressMessages(mmm_protection_guard_end(g7b, file.path(d7b$tmp, "manifest")))) && fails(acr_verify_acp_run(acp_dir)),
      "7: a protected acp file changed during a build stops the report")

# 8 ----------------------------------------------------------------------------------------------------------------------
for (p in c("Functions/antenna_calibration_report.R", "Testing/audits/report_rfid_calibration_pilot.R")) {
  pd8 <- utils::getParseData(parse(p, keep.source = TRUE))
  sym <- pd8$text[pd8$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_SUB")]
  str <- pd8$text[pd8$token == "STR_CONST"]
  check(!any(sym %in% c("Group", "outcome_group", "GroupSex", "CombZ", "Outcome", "sus_animals", "con_animals")) &&
          !any(grepl("sus_animals|con_animals|later_outcome_combz|outcome_group|CombZ_|GroupSex", str)), paste("8: group-blind", p))
}

unlink(tmp, recursive = TRUE); unlink(file.path(beh, "RANM"), recursive = TRUE)
cat("test_antenna_calibration_report: all checks passed\n")
