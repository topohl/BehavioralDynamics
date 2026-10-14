# One-day RFID calibration pilot: seeded, counterbalanced placement schedule v1 (pre-registered with the criteria
# acp_criteria_v1.0 and the analysis code; docs/RFID_CALIBRATION_PILOT.md section 1). Copy of the generator
# scratchpad/calib_pilot/one_day_v2/make_schedule.R of 2026-10-14; the only changes are this header, the main guard, the
# default output folder, the file name and two design comments corrected to match the code. The random calls and their
# order are unchanged, so the schedule is identical.
# Writes schedule_one_day_v1.csv and prints the timeline, the totals and the time-of-day balance.
# Pure R (no packages). Run from the repository root:
#   Rscript docs/rfid_calibration/make_schedule_one_day_v1.R [<out_dir>]      (default docs/rfid_calibration)
# The file written by R on Windows (CRLF) has sha256 7b7fda397638e69b1866befffb159c741e1b6d6ed69654911bc0928c713328dd; its
# content hash with line endings normalised to LF is pinned as ACP_SCHEDULE_SHA256 in Functions/antenna_calibration_pilot.R.
#
# Design (fixed before any measurement):
#   boards: 2 (reference) and 4 (least sensitive); one PhenoSoft session; one operator; tag T1 for every single-tag
#   placement; one physical house moved between the two cages; two identical spacer boards (one per tested cage).
#   Programmes P (static), M (movement), H (house in/out) are split into halves 1 and 2 by a seeded, stratified split;
#   half 1 runs board 2 then board 4, half 2 board 4 then board 2 (ABBA). The competition run C is one continuous run
#   per board (board 4 first, then board 2, about 20 min apart). REF (repeat) placements open the visits V1-V3 and close
#   V2, V4 and V5 (3 per board).  [comment corrected in this copy to match the code]
#   Visits: V1 b2 [REF, T0, P1] | V2 b4 [REF, P1, P2, M1, REF] | V3 b2 [REF, P2, M1, M2, H1] | break |
#           V4 b4 [M2, H1, C, H2, REF] | V5 b2 [C, H2, REF]   [comment corrected in this copy to match the code]
if (sys.nframe() == 0L) {
  args <- commandArgs(TRUE)
  out_dir <- if (length(args)) args[1] else file.path("docs", "rfid_calibration")
  SEED <- 20261014L
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(SEED)

  # slot lengths (s): recorded seconds + handling
  STATIC_REC <- 70; STATIC_HANDLE <- 30          # 60 s analysed (5-s guards)
  FLICKER_REC <- 310                              # 300 s analysed
  REF_REC <- 50; REFX_REC <- 70; REF_HANDLE <- 25 # REF-z 40 s analysed, REF-x 60 s analysed
  T0_REC <- 50; T0_HANDLE <- 15                   # 40 s analysed
  HOUSE_STEP <- 30; WELL_FILL <- 60; SWITCH <- 60

  rows <- list()
  add <- function(visit, block, board, position, x = NA, y = NA, orientation = "", height_cm = NA, object = "", n_tags = 0L,
                  tags = "", seconds = 0, slot_s = seconds, pair_id = "", purpose = "") {
    rows[[length(rows) + 1L]] <<- data.frame(visit = visit, block = block, board = board, position = position, x = x, y = y,
      orientation = orientation, height = height_cm, object = object, n_tags = as.integer(n_tags), tags = tags,
      seconds = seconds, slot_s = slot_s, pair_id = pair_id, purpose = purpose, stringsAsFactors = FALSE)
  }
  H_FLOOR <- "floor"   # on the spacer board (production bedding depth); 1-cm cube centre

  # ---------------------------------------------------------------- placement sets
  # P (static, house absent): 15 per board. class: cz centre z, cx centre x, q quarter point, m midpoint
  P <- data.frame(
    pair_id = c("P01", "P02", "P03", "P04", "P05", "P06", "P07", "P08", "P09", "P10", "P11", "P12", "P13", "P14", "P15"),
    position = c("R1 centre", "R2 centre", "R8 centre", "R1 centre", "R2 centre", "R4 centre", "R6 centre", "R8 centre",
                 "R5|R6 quarter, R5 side", "R1|R2 quarter, R1 side", "R1|R2 quarter, R2 side", "R1|R5 quarter, R1 side",
                 "R1|R5 quarter, R5 side", "R5|R6 midpoint (static flicker, cross-group)", "R2|R3 midpoint (within-group)"),
    x = c(0, 100, 300, 0, 100, 300, 100, 300, 25, 25, 75, 0, 0, 50, 150),
    y = c(0, 0, 116, 0, 0, 0, 116, 116, 116, 0, 0, 29, 87, 116, 0),
    orientation = c("z", "z", "z", "x", "x", "x", "x", "x", "x", "x", "x", "y", "y", "x", "x"),
    class = c(rep("cz", 3), rep("cx", 5), rep("q", 5), "m", "m"),
    stringsAsFactors = FALSE)
  P$seconds <- ifelse(P$pair_id == "P14", FLICKER_REC, STATIC_REC)
  P$purpose <- c(rep("E1/E7 centre, z (lambda_sat, reliable zone)", 3), rep("E1/E2/E7 centre, walking orientation", 5),
                 rep("E2 basis and resolution at a boundary (unique true reader); E7", 5),
                 "E6 static flicker -> tau; E3 cross-group co-read; E7", "E6/E3 within-group flicker and simultaneous reads; E7")
  # seeded stratified split into halves 1/2 (counts per class: cz 2/1 or 1/2, cx 3/2 or 2/3, q 3/2 or 2/3, m 1/1)
  P$half <- NA_integer_
  extra <- sample(1:2, 1)                          # half that gets the extra placement of the first odd stratum
  for (cl in c("cz", "cx", "q", "m")) {
    i <- which(P$class == cl)
    i <- i[sample.int(length(i))]
    if (length(i) %% 2 == 1) { first <- extra; extra <- 3L - extra } else first <- sample(1:2, 1)
    P$half[i] <- rep(c(first, 3L - first), length.out = length(i))
  }
  # the 5-min flicker placement (P14) goes to the half with fewer placements, so the two halves last about equally long
  small <- as.integer(names(which.min(table(P$half))))
  if (P$half[P$pair_id == "P14"] != small) P$half[P$class == "m"] <- 3L - P$half[P$class == "m"]
  # H (house block, R5 inside the house, production bedding in the well): classes x {ON, OFF}
  H <- data.frame(cls = c("HCx", "HCz", "HRx"), position = c("R5 centre inside house", "R5 centre inside house", "house roof above R5 centre"),
                  x = c(0, 0, 0), y = c(116, 116, 116), orientation = c("x", "z", "x"), height = c("bedding", "bedding", "roof"),
                  stringsAsFactors = FALSE)
  hc_first <- sample(c("HCx", "HCz"), 1)            # which centre orientation goes to H1
  roof_half <- sample(1:2, 1)                      # which half gets the roof pair
  H$half <- ifelse(H$cls == hc_first, 1L, ifelse(H$cls == "HRx", roof_half, 2L))
  H$pair_on <- paste0("H", c("01", "03", "05")); H$pair_off <- paste0("H", c("02", "04", "06"))
  # M (movement on the upper row, house absent): half 1 slow passes + visits set 1; half 2 fast passes + visits set 2
  visit_order <- function() paste(sample(c(1, 1, 2, 2, 5, 5)), collapse = ",")
  # T0 tag order (T2-T6; T1 is the V1 REF-z placement)
  t0_order <- paste0("T", 1 + sample.int(5))

  # ---------------------------------------------------------------- blocks as functions
  ref <- function(v, b, when) {
    add(v, paste0("REF-", when), b, "R6 centre (REF pocket)", 100, 116, "z", H_FLOOR, "", 1, "T1", REF_REC, REF_REC + REF_HANDLE,
        "REFz", "repeat/drift: cadence and lambda_sat (saturated)")
    add(v, paste0("REF-", when), b, "R5|R6 quarter, R6 side (REF pocket)", 75, 116, "x", H_FLOOR, "", 1, "T1", REFX_REC,
        REFX_REC + REF_HANDLE, "REFx", "repeat/drift: sensitivity-limited boundary; E2 unit; E7 repeatability")
  }
  p_block <- function(v, b, half) {
    s <- P[P$half == half, ]
    s <- s[sample.int(nrow(s)), ]
    for (k in seq_len(nrow(s))) add(v, paste0("P", half), b, s$position[k], s$x[k], s$y[k], s$orientation[k], H_FLOOR, "", 1, "T1",
                                    s$seconds[k], s$seconds[k] + STATIC_HANDLE, s$pair_id[k], s$purpose[k])
  }
  h_block <- function(v, b, half) {
    s <- H[H$half == half, ]
    states <- if (half == 1) c("ON", "OFF") else c("OFF", "ON")
    # half 2 follows the competition run, which leaves the house on: lift it before the OFF placements
    if (half == 2) add(v, "H-handling", b, "lift house (bedding untouched)", slot_s = HOUSE_STEP, purpose = "house state change")
    for (st in states) {
      if (st == states[2]) add(v, "H-handling", b, if (st == "OFF") "lift house (bedding untouched)" else "place house on its outline",
                               slot_s = HOUSE_STEP, purpose = "house state change")
      ss <- s[sample.int(nrow(s)), , drop = FALSE]
      for (k in seq_len(nrow(ss))) {
        obj <- if (st == "ON") (if (ss$cls[k] == "HRx") "house (tag on roof)" else "house") else (if (ss$cls[k] == "HRx") "foam stack, roof height" else "none (house lifted)")
        add(v, paste0("H", half, "-", st), b, ss$position[k], ss$x[k], ss$y[k], ss$orientation[k], ss$height[k], obj, 1, "T1",
            STATIC_REC, STATIC_REC + STATIC_HANDLE, if (st == "ON") ss$pair_on[k] else ss$pair_off[k],
            "E5 shielding (paired ON/OFF); E1 house/roof detection; E7")
      }
    }
  }
  m_block <- function(v, b, half) {
    add(v, paste0("M", half), b, "rail and sled on the upper row; lid closed", slot_s = 45, purpose = "handling")
    if (half == 1) add(v, "M1", b, "passes R5>R8>R5 x2, ~5 cm/s, 5-s pause at each centre", NA, 116, "x", H_FLOOR, "", 1, "T1", 115, 125,
                       "M-pass-slow", "E6 latency/tail; E2 basis +-3 s around 12 crossings (R5|R6, R6|R7, R7|R8)")
    else add(v, "M2", b, "passes R5>R8>R5 x2, ~20 cm/s, 5-s pause at each centre", NA, 116, "x", H_FLOOR, "", 1, "T1", 96, 106,
             "M-pass-fast", "E6 latency/tail; E2 basis +-3 s around 12 crossings (R5|R6, R6|R7, R7|R8)")
    add(v, paste0("M", half), b, paste0("short visits R5>R6>R5, dwell (s) in order ", visit_order(), "; 10 s at R5 between"), NA, 116,
        "x", H_FLOOR, "", 1, "T1", 108, 118, paste0("M-visits-", half), "E6 genuine visits kept under tau; E2 basis per visit")
    add(v, paste0("M", half), b, "remove rail and sled", slot_s = 30, purpose = "handling")
  }
  c_run <- function(v, b) {
    add(v, "C", b, "sentinels: T5 at R8 centre, T6 at R4 centre", NA, NA, "z", H_FLOOR, "house", 2, "T5,T6", 70, 90, "C0",
        "E4 sentinel baseline (no load at R5)")
    add(v, "C", b, "+T1 in house, slot 1 (R5 centre -10,+10)", -10, 126, "z", "bedding", "house", 3, "T1,T5,T6", 130, 145, "C1",
        "E4 n = 1 in the house")
    add(v, "C", b, "+T2, T3, T4 in house, slots 2-4 (2-cm square on R5)", NA, NA, "z", "bedding", "house", 6, "T1-T6", 310, 340, "C4",
        "E4 n = 4: time-sharing/collision/capture; q99 gap -> delta; sentinels under load")
    add(v, "C", b, "T3, T4 moved to R6 centre (slots 90,116 and 110,116); T1, T2 stay in house", NA, NA, "z", "bedding/floor", "house",
        6, "T1-T6", 160, 180, "CS", "E4 split 2 + 2 over neighbouring cross-group fields")
    add(v, "C", b, "T1-T4 removed; sentinels alone", NA, NA, "z", H_FLOOR, "house", 2, "T5,T6", 70, 105, "CE",
        "E4 sentinel baseline after load; then remove sentinels")
  }

  # ---------------------------------------------------------------- the day
  add("S", "SETUP", NA, "cages on boards 1-5 (lid, hopper, filled bottle); spacer boards with foam well inserts in b2, b4; cameras and clock displays on b2, b4; photos", slot_s = 20 * 60, purpose = "set-up; start PhenoSoft once; archive workbook (sha256) and version row")
  add("S", "BASELINE", "all", "no tags on any board", n_tags = 0, seconds = 600, slot_s = 600, purpose = "E8 known-empty; E9 idle signature")
  add("S", "CHECK", "2,4,5", "channel check: T1 5 s on each antenna of b2 and b4; clapper C on b5 channel 7", n_tags = 1, tags = "T1,C", slot_s = 5 * 60, purpose = "stop rule 1; E9")
  add("S", "SURVEY", "2,4", "T1 z slid by hand ~1 cm/s along both rows and the 4 columns, on video; grid offset check", NA, NA, "z", H_FLOOR, "", 1, "T1", 480, 10 * 60, "SURVEY", "measured antenna centres for the truth function")
  # V1 board 2
  ref("V1", 2, "start")
  for (tg in t0_order) add("V1", "T0", 2, "R6 centre (REF pocket)", 100, 116, "z", H_FLOOR, "", 1, tg, T0_REC, T0_REC + T0_HANDLE, paste0("T0-", tg), "E0 tag QC (T1 = V1 REF-z)")
  p_block("V1", 2, 1)
  add("V1", "SWITCH", "2>4", "carry T1 to b4; two clapper touches", slot_s = SWITCH, purpose = "board switch")
  # V2 board 4
  ref("V2", 4, "start"); p_block("V2", 4, 1); p_block("V2", 4, 2); m_block("V2", 4, 1); ref("V2", 4, "end")
  add("V2", "SWITCH", "4>2", "carry T1 to b2; two clapper touches", slot_s = SWITCH, purpose = "board switch")
  # V3 board 2
  ref("V3", 2, "start"); p_block("V3", 2, 2); m_block("V3", 2, 1); m_block("V3", 2, 2)
  add("V3", "H-handling", 2, "well insert out; pre-weighed bedding portion in the well, tamped level; house on its outline", slot_s = WELL_FILL + HOUSE_STEP, purpose = "house block set-up")
  h_block("V3", 2, 1)
  add("B", "BREAK", NA, "break (house out of the cages; no tags on any board)", slot_s = 15 * 60, purpose = "")
  # V4 board 4
  m_block("V4", 4, 2)
  add("V4", "H-handling", 4, "well insert out; pre-weighed bedding portion in the well, tamped level; house on its outline", slot_s = WELL_FILL + HOUSE_STEP, purpose = "house block set-up")
  h_block("V4", 4, 1)
  add("V4", "H-handling", 4, "place house on its outline", slot_s = HOUSE_STEP, purpose = "house on for C")
  c_run("V4", 4)
  h_block("V4", 4, 2)
  add("V4", "H-handling", 4, "house out of the cage (REF without house)", slot_s = HOUSE_STEP, purpose = "REF conditions identical"); ref("V4", 4, "end")
  add("V4", "SWITCH", "4>2", "carry tags and house to b2; house on its outline in b2; two clapper touches", slot_s = SWITCH + HOUSE_STEP, purpose = "board switch")
  # V5 board 2
  c_run("V5", 2); h_block("V5", 2, 2)
  add("V5", "H-handling", 2, "house out of the cage (REF without house)", slot_s = HOUSE_STEP, purpose = "REF conditions identical"); ref("V5", 2, "end")
  add("E", "BASELINE", "all", "no tags on any board", n_tags = 0, seconds = 600, slot_s = 600, purpose = "E8 known-empty; E9")
  add("E", "BACKUP", NA, "stop session once; copy logs, workbook, sheets, photos, videos; sha256 manifest", slot_s = 15 * 60, purpose = "")

  s <- do.call(rbind, rows)
  s$start_min <- round(c(0, cumsum(s$slot_s))[seq_len(nrow(s))] / 60, 1)
  s$order <- seq_len(nrow(s))
  s <- s[, c("order", "visit", "block", "board", "position", "x", "y", "orientation", "height", "object", "n_tags", "tags",
             "seconds", "slot_s", "start_min", "pair_id", "purpose")]
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(s, file.path(out_dir, "schedule_one_day_v1.csv"), row.names = FALSE, na = "")

  # ---------------------------------------------------------------- summaries
  tot <- sum(s$slot_s) / 60
  cat(sprintf("seed %d; rows %d; total on site %.1f min (%.2f h)\n", SEED, nrow(s), tot, tot / 60))
  vis <- aggregate(slot_s ~ visit, s, function(z) round(sum(z) / 60, 1)); print(vis)
  meas <- s[s$board %in% c("2", "4") & s$seconds > 0, ]
  cat("\nrecorded minutes under test by board:\n"); print(aggregate(seconds ~ board, meas, function(z) round(sum(z) / 60, 1)))
  cat("\nslot minutes (incl. handling) by board:\n"); print(aggregate(slot_s ~ board, s[s$board %in% c("2", "4"), ], function(z) round(sum(z) / 60, 1)))
  meas$prog <- sub("[0-9].*$", "", sub("-.*$", "", meas$block))
  meas$mid <- meas$start_min + meas$seconds / 120
  cat("\nmean clock minute of measured rows by programme x board (time-of-day balance):\n")
  print(reshape(aggregate(mid ~ prog + board, meas, function(z) round(mean(z), 1)), idvar = "prog", timevar = "board", direction = "wide"))
  pp <- meas[grepl("^P[0-9]{2}$|^H0", meas$pair_id), ]
  w <- reshape(pp[, c("pair_id", "board", "mid")], idvar = "pair_id", timevar = "board", direction = "wide")
  w$lag <- w$mid.4 - w$mid.2
  cat("\npaired static placements: board-4 minus board-2 time (min): mean", round(mean(w$lag), 1), " SD", round(sd(w$lag), 1),
      " range", paste(round(range(w$lag), 1), collapse = " to "), " n", nrow(w), " b2-first", sum(w$lag > 0), "\n")
  cat("P halves:\n"); print(P[, c("pair_id", "position", "orientation", "class", "half")])
  cat("H halves:\n"); print(H[, c("cls", "half")])
  cat("T0 order:", t0_order, "\n")
  cat("REF times (min):\n"); print(meas[meas$pair_id %in% c("REFz", "REFx"), c("board", "pair_id", "start_min")])
}
