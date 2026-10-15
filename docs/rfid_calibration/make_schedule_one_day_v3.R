# One-day RFID calibration pilot: seeded, counterbalanced placement schedule v3 (amended 2026-10-15, before any
# acquisition and before booking, on Tobias's decision; docs/RFID_CALIBRATION_PILOT.md, "Amendment v3"). Schedules v1 and
# v2 and their generators stay unchanged for provenance.
# What changed from v2, and why (Tobias, 2026-10-15: four-column occupancy is the primary spatial representation, and the
# house at R5 needs more coverage):
#   * column-boundary step transitions (programme K, new): the sled carries T1 (x) from a known centre to the neighbouring
#     column's centre on a timed call, holds 20 s, and steps on, out and back along a whole row (6 transitions, 3 column
#     boundaries x 2 directions). K half 1 on the lower row (R1-R4) in the M half 1 visits, K half 2 on the upper row
#     (R5-R8) in the M half 2 visits, so K runs 4-2-2-4 like M. v2 had column crossings only as continuous upper-row passes
#     (5-s pauses), and no lower-row crossing;
#   * house coverage (programme HX, new): four more ON/OFF pairs at the house with production bedding in the well, T1, 40 s
#     analysed: HEx and HEy at the R5|R6 quarter point on the R5 side (25,116; x radial and y tangential to the R6 side:
#     misassignment towards the neighbouring column, and orientation at the house edge), HSy at the R1|R5 quarter point on
#     the R5 side (0,87; y radial: misassignment towards R1, the food reader) and HOx at the R5|R6 quarter point on the R6
#     side (75,116; x radial; on the floor just outside the house wall towards R6: whether the house draws an outside tag
#     into the house column). HEx and HEy go together to the H half with fewer classes, HSy and HOx to the other; their
#     positions inside each state are drawn from a second seed;
#   * house-lifted periods in every competition run (new): after C1 the house is lifted (tags stay), C1o (T1 alone) and C4o
#     (T1-T4 in slots 1-4) are recorded with the house lifted, then the house is set down again for C4. With C1 and C4 this
#     is a 2 x 2 (one or four tags x house on or lifted) inside one run: reader competition apart from house attenuation.
#     Every house-on period with tags in the house now starts the same way (lift, tags placed or moved, house set down
#     over them, slots checked on camera): C1, C4 and CS, so the handling does not differ between the cells;
#   * reallocated from the competition repetitions (doubled in v2, Ca and Cb): each run's C1 (130 -> 100 s), C4 (310 ->
#     250 s) and CS (160 -> 130 s) are shortened. Per board the analysed time falls from v2's 240 / 600 / 300 s (C1 / C4 /
#     CS) to 180 / 480 / 240 s (v1: 120 / 300 / 150 s), the 10-s blocks of B4-B6 from 24 / 60 / 30 to 18 / 48 / 24.
#     C0 and CE are unchanged;
#   * every v2 row other than the competition-run rows is kept with the same content and in the same relative order (the
#     main seed and its random calls are those of v1 and v2; the new elements use SEED2), so the content per board is
#     unchanged; only start times move.
# Writes schedule_one_day_v3.csv and prints the timeline, the totals and the time-of-day balance.
# Pure R (no packages). Run from the repository root:
#   Rscript docs/rfid_calibration/make_schedule_one_day_v3.R [<out_dir>]      (default docs/rfid_calibration)
# Its content hash with line endings normalised to LF is pinned as ACP_SCHEDULE_SHA256 in Functions/antenna_calibration_pilot.R.
#
# Design (fixed before any measurement):
#   boards: 2 (reference) and 4 (least sensitive); one PhenoSoft session; one operator; tag T1 for every single-tag
#   placement; one physical house moved between the two cages; two identical spacer boards (one per tested cage).
#   Programmes P (static), M (movement) with K (column-boundary steps), H (house in/out) with HX are split into halves 1 and
#   2; each programme runs half 1 on board 4 then board 2 and half 2 on board 2 then board 4 (4-2-2-4). The competition run
#   C runs twice per board: Ca on board 4 then board 2, Cb on board 2 then board 4. REF (repeat) placements: 3 per board.
#   Visits: V1 b4 [REF, P1, M1+K1] | V2 b2 [REF, T0, P1, M1+K1, P2, M2+K2] | break | V3 b4 [REF, P2, M2+K2, H1, Ca] |
#           V4 b2 [REF, H1, Ca, Cb, H2, REF] | V5 b4 [Cb, H2, REF]
if (sys.nframe() == 0L) {
  args <- commandArgs(TRUE)
  out_dir <- if (length(args)) args[1] else file.path("docs", "rfid_calibration")
  SEED <- 20261014L    # as v1 and v2: the halves of P and H, the visit dwell orders and the T0 order are drawn by the same calls
  SEED2 <- 20261016L   # v3 only: the half and the within-state position of the HX classes
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(SEED2); S2 <- .Random.seed
  set.seed(SEED)
  # draws from the second stream without touching the main one
  draw2 <- function(f) {
    main <- get(".Random.seed", envir = globalenv())
    assign(".Random.seed", S2, envir = globalenv())
    out <- f()
    S2 <<- get(".Random.seed", envir = globalenv())
    assign(".Random.seed", main, envir = globalenv())
    out
  }

  # slot lengths (s): recorded seconds + handling
  STATIC_REC <- 70; STATIC_HANDLE <- 30          # 60 s analysed (5-s guards)
  FLICKER_REC <- 310                              # 300 s analysed
  REF_REC <- 50; REFX_REC <- 70; REF_HANDLE <- 25 # REF-z 40 s analysed, REF-x 60 s analysed
  T0_REC <- 50; T0_HANDLE <- 15                   # 40 s analysed
  HOUSE_STEP <- 30; WELL_FILL <- 60; SWITCH <- 60
  HX_REC <- 50; HX_HANDLE <- 25                   # v3: 40 s analysed per house-extension placement
  K_REC <- 165; K_HANDLE <- 10; K_RAIL_MOVE <- 45 # v3: steps at 25, 47, 69, 91, 113, 135 s; holds of 20 s; 155 s analysed
  C1_REC <- 100; C1O_REC <- 70; C4O_REC <- 130; C4_REC <- 250; CS_REC <- 130   # v3 competition-run periods

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
  # HX (v3; house extension, ON/OFF pairs as H, 40 s analysed): to the halves so that each half holds three H/HX classes
  HX <- data.frame(cls = c("HEx", "HEy", "HSy", "HOx"),
                   position = c("R5|R6 quarter, R5 side (house edge towards R6), x radial", "R5|R6 quarter, R5 side (house edge towards R6), y tangential",
                                "R1|R5 quarter, R5 side (house side towards R1), y radial",
                                "R5|R6 quarter, R6 side (outside the house wall towards R6), x radial"),
                   x = c(25, 25, 0, 75), y = c(116, 116, 87, 116), orientation = c("x", "y", "y", "x"),
                   height = c("bedding", "bedding", "bedding", "floor"),
                   pair_on = c("H07", "H09", "H11", "H13"), pair_off = c("H08", "H10", "H12", "H14"), stringsAsFactors = FALSE)
  # the orientation pair HEx/HEy goes to the half with fewer H classes (v2 split: H1 HCx + HRx, H2 HCz -> H2), so that the
  # two orientations at the house edge are measured in one block; HSy and HOx (outside the wall) to the other half
  he_half <- which.min(tabulate(H$half, 2))
  HX$half <- ifelse(HX$cls %in% c("HSy", "HOx"), 3L - he_half, he_half)
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
    sx <- HX[HX$half == half, ]
    states <- if (half == 1) c("ON", "OFF") else c("OFF", "ON")
    # half 2 follows the competition run, which leaves the house on: lift it before the OFF placements
    if (half == 2) add(v, "H-handling", b, "lift house (bedding untouched)", slot_s = HOUSE_STEP, purpose = "house state change")
    for (st in states) {
      if (st == states[2]) add(v, "H-handling", b, if (st == "OFF") "lift house (bedding untouched)" else "place house on its outline",
                               slot_s = HOUSE_STEP, purpose = "house state change")
      ss <- s[sample.int(nrow(s)), , drop = FALSE]                 # main stream, as v2
      n <- nrow(ss) + nrow(sx)
      at <- draw2(function() sort(sample.int(n, nrow(sx))))       # positions of the HX classes in this state
      ox <- draw2(function() sample.int(nrow(sx)))                 # their order
      seq_old <- seq_len(nrow(ss)); kx <- 0L; ko <- 0L
      for (j in seq_len(n)) {
        if (j %in% at) {
          kx <- kx + 1L; x <- sx[ox[kx], ]
          add(v, paste0("H", half, "-", st), b, x$position, x$x, x$y, x$orientation, x$height,
              if (st == "ON") "house" else "none (house lifted)", 1, "T1", HX_REC, HX_REC + HX_HANDLE,
              if (st == "ON") x$pair_on else x$pair_off,
              "HX house extension (paired ON/OFF): attenuation, misassignment to the neighbouring reader, orientation; KA6")
        } else {
          ko <- ko + 1L; k <- seq_old[ko]
          obj <- if (st == "ON") (if (ss$cls[k] == "HRx") "house (tag on roof)" else "house") else (if (ss$cls[k] == "HRx") "foam stack, roof height" else "none (house lifted)")
          add(v, paste0("H", half, "-", st), b, ss$position[k], ss$x[k], ss$y[k], ss$orientation[k], ss$height[k], obj, 1, "T1",
              STATIC_REC, STATIC_REC + STATIC_HANDLE, if (st == "ON") ss$pair_on[k] else ss$pair_off[k],
              "E5 shielding (paired ON/OFF); E1 house/roof detection; E7")
        }
      }
    }
  }
  k_row <- function(v, b, half) {   # v3: column-boundary step transitions, lower row in half 1, upper row in half 2
    lower <- half == 1
    if (lower) add(v, paste0("K", half), b, "move rail and sled to the lower row (R1-R4); lid closed", slot_s = K_RAIL_MOVE, purpose = "handling")
    add(v, paste0("K", half), b,
        paste0("step transitions ", if (lower) "R1>R2>R3>R4>R3>R2>R1" else "R5>R6>R7>R8>R7>R6>R5",
               ": sled at the first centre; one reader spacing in about 2 s at 25, 47, 69, 91, 113 and 135 s after the start touch",
               " (timer, recorder's call); hold at each centre in between"),
        NA, if (lower) 0 else 116, "x", H_FLOOR, "", 1, "T1", K_REC, K_REC + K_HANDLE, if (lower) "K-lower" else "K-upper",
        "K column-boundary transitions (known start and end, timed, both directions): column accuracy, switching latency per basis; KA")
  }
  m_block <- function(v, b, half) {
    add(v, paste0("M", half), b, "rail and sled on the upper row; lid closed", slot_s = 45, purpose = "handling")
    if (half == 1) add(v, "M1", b, "passes R5>R8>R5 x2, ~5 cm/s, 5-s pause at each centre", NA, 116, "x", H_FLOOR, "", 1, "T1", 115, 125,
                       "M-pass-slow", "E6 latency/tail; E2 basis +-3 s around 12 crossings (R5|R6, R6|R7, R7|R8)")
    else add(v, "M2", b, "passes R5>R8>R5 x2, ~20 cm/s, 5-s pause at each centre", NA, 116, "x", H_FLOOR, "", 1, "T1", 96, 106,
             "M-pass-fast", "E6 latency/tail; E2 basis +-3 s around 12 crossings (R5|R6, R6|R7, R7|R8)")
    add(v, paste0("M", half), b, paste0("short visits R5>R6>R5, dwell (s) in order ", visit_order(), "; 10 s at R5 between"), NA, 116,
        "x", H_FLOOR, "", 1, "T1", 108, 118, paste0("M-visits-", half), "E6 genuine visits kept under tau; E2 basis per visit")
    k_row(v, b, half)
    add(v, paste0("M", half), b, "remove rail and sled", slot_s = 30, purpose = "handling")
  }
  c_run <- function(v, b, run) {   # run "a" or "b": two identical runs per board (block Ca, Cb); pair ids C0..CE, C1o, C4o
    blk <- paste0("C", run)
    add(v, blk, b, "sentinels: T5 at R8 centre, T6 at R4 centre", NA, NA, "z", H_FLOOR, "house", 2, "T5,T6", 70, 90, "C0",
        "E4 sentinel baseline (no load at R5)")
    # every house-on period with tags in the house starts the same way: lift, tags placed or moved, house set down over them
    add(v, blk, b, "lift house vertically (sentinels stay; bedding untouched)", slot_s = HOUSE_STEP, purpose = "house state change")
    add(v, blk, b, "+T1 in slot 1 (R5 centre -10,+10); house set down over it (slot checked on camera)", -10, 126, "z", "bedding", "house", 3,
        "T1,T5,T6", C1_REC, C1_REC + 30, "C1", "E4 n = 1 in the house")
    add(v, blk, b, "lift house vertically (T1 and sentinels stay; bedding untouched)", slot_s = HOUSE_STEP, purpose = "house state change (HX10)")
    add(v, blk, b, "T1 alone in slot 1, house lifted", -10, 126, "z", "bedding", "none (house lifted)", 3, "T1,T5,T6", C1O_REC, C1O_REC + 10, "C1o",
        "HX10/HX11 n = 1, house lifted")
    add(v, blk, b, "+T2, T3, T4 in slots 2-4 (2-cm square on R5), house lifted", NA, NA, "z", "bedding", "none (house lifted)", 6, "T1-T6",
        C4O_REC, C4O_REC + 30, "C4o", "HX10/HX11 n = 4, house lifted: competition without the house")
    add(v, blk, b, "set house down on its outline over T1-T4 (slots checked on camera)", slot_s = HOUSE_STEP, purpose = "house state change (HX10)")
    add(v, blk, b, "T1-T4 in house, slots 1-4 (house set down over them)", NA, NA, "z", "bedding", "house", 6, "T1-T6", C4_REC, C4_REC + 10, "C4",
        "E4 n = 4: time-sharing/collision/capture; q99 gap -> delta; sentinels under load")
    add(v, blk, b, "lift house; T3, T4 moved to R6 centre (slots 90,116 and 110,116); house set down over T1, T2 (slots checked on camera)",
        NA, NA, "z", "bedding/floor", "house", 6, "T1-T6", CS_REC, CS_REC + 40, "CS", "E4 split 2 + 2 over neighbouring cross-group fields")
    add(v, blk, b, "T1-T4 removed; sentinels alone", NA, NA, "z", H_FLOOR, "house", 2, "T5,T6", 70, 105, "CE",
        "E4 sentinel baseline after load; then remove sentinels")
  }

  # ---------------------------------------------------------------- the day
  add("S", "SETUP", NA, "cages on boards 1-5 (lid, hopper, filled bottle); spacer boards with foam well inserts in b2, b4; cameras and clock displays on b2, b4; photos", slot_s = 20 * 60, purpose = "set-up; start PhenoSoft once; archive workbook (sha256) and version row")
  add("S", "BASELINE", "all", "no tags on any board", n_tags = 0, seconds = 600, slot_s = 600, purpose = "E8 known-empty; E9 idle signature")
  add("S", "CHECK", "2,4,5", "channel check: T1 5 s on each antenna of b2 and b4; clapper C on b5 channel 7", n_tags = 1, tags = "T1,C", slot_s = 5 * 60, purpose = "stop rule 1; E9")
  add("S", "SURVEY", "2,4", "T1 z slid by hand ~1 cm/s along both rows and the 4 columns, on video; grid offset check", NA, NA, "z", H_FLOOR, "", 1, "T1", 480, 10 * 60, "SURVEY", "measured antenna centres for the truth function")
  # V1 board 4
  ref("V1", 4, "start"); p_block("V1", 4, 1); m_block("V1", 4, 1)
  add("V1", "SWITCH", "4>2", "carry T1 to b2; two clapper touches", slot_s = SWITCH, purpose = "board switch")
  # V2 board 2
  ref("V2", 2, "start")
  for (tg in t0_order) add("V2", "T0", 2, "R6 centre (REF pocket)", 100, 116, "z", H_FLOOR, "", 1, tg, T0_REC, T0_REC + T0_HANDLE, paste0("T0-", tg), "E0 tag QC (T1 = V2 REF-z)")
  p_block("V2", 2, 1); m_block("V2", 2, 1); p_block("V2", 2, 2); m_block("V2", 2, 2)
  add("V2", "SWITCH", "2>4", "carry T1 to b4; two clapper touches", slot_s = SWITCH, purpose = "board switch")
  add("B", "BREAK", NA, "break (no tags on any board; film the clock offsets)", slot_s = 15 * 60, purpose = "")
  # V3 board 4
  ref("V3", 4, "start"); p_block("V3", 4, 2); m_block("V3", 4, 2)
  add("V3", "H-handling", 4, "well insert out; pre-weighed bedding portion in the well, tamped level; house on its outline", slot_s = WELL_FILL + HOUSE_STEP, purpose = "house block set-up")
  h_block("V3", 4, 1)
  add("V3", "H-handling", 4, "place house on its outline", slot_s = HOUSE_STEP, purpose = "house on for C")
  c_run("V3", 4, "a")
  add("V3", "SWITCH", "4>2", "carry tags and house to b2; two clapper touches", slot_s = SWITCH + HOUSE_STEP, purpose = "board switch")
  # V4 board 2
  ref("V4", 2, "start")
  add("V4", "H-handling", 2, "well insert out; pre-weighed bedding portion in the well, tamped level; house on its outline", slot_s = WELL_FILL + HOUSE_STEP, purpose = "house block set-up")
  h_block("V4", 2, 1)
  add("V4", "H-handling", 2, "place house on its outline", slot_s = HOUSE_STEP, purpose = "house on for C")
  c_run("V4", 2, "a"); c_run("V4", 2, "b"); h_block("V4", 2, 2)
  add("V4", "H-handling", 2, "house out of the cage (REF without house)", slot_s = HOUSE_STEP, purpose = "REF conditions identical"); ref("V4", 2, "end")
  add("V4", "SWITCH", "2>4", "carry tags and house to b4; house on its outline in b4; two clapper touches", slot_s = SWITCH + HOUSE_STEP, purpose = "board switch")
  # V5 board 4
  c_run("V5", 4, "b"); h_block("V5", 4, 2)
  add("V5", "H-handling", 4, "house out of the cage (REF without house)", slot_s = HOUSE_STEP, purpose = "REF conditions identical"); ref("V5", 4, "end")
  add("E", "BASELINE", "all", "no tags on any board", n_tags = 0, seconds = 600, slot_s = 600, purpose = "E8 known-empty; E9")
  add("E", "BACKUP", NA, "stop session once; copy logs, workbook, sheets, photos, videos; sha256 manifest", slot_s = 15 * 60, purpose = "")

  s <- do.call(rbind, rows)
  s$start_min <- round(c(0, cumsum(s$slot_s))[seq_len(nrow(s))] / 60, 1)
  s$order <- seq_len(nrow(s))
  s <- s[, c("order", "visit", "block", "board", "position", "x", "y", "orientation", "height", "object", "n_tags", "tags",
             "seconds", "slot_s", "start_min", "pair_id", "purpose")]
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(s, file.path(out_dir, "schedule_one_day_v3.csv"), row.names = FALSE, na = "")

  # ---------------------------------------------------------------- summaries
  tot <- sum(s$slot_s) / 60
  cat(sprintf("seeds %d / %d; rows %d; total on site %.1f min (%.2f h)\n", SEED, SEED2, nrow(s), tot, tot / 60))
  vis <- aggregate(slot_s ~ visit, s, function(z) round(sum(z) / 60, 1)); print(vis)
  meas <- s[s$board %in% c("2", "4") & s$seconds > 0, ]
  cat("\nrecorded minutes under test by board:\n"); print(aggregate(seconds ~ board, meas, function(z) round(sum(z) / 60, 1)))
  cat("\nslot minutes (incl. handling) by board:\n"); print(aggregate(slot_s ~ board, s[s$board %in% c("2", "4"), ], function(z) round(sum(z) / 60, 1)))
  meas$prog <- ifelse(grepl("^H(0[7-9]|1[0-2])$", meas$pair_id), "HX", ifelse(meas$pair_id %in% c("C1o", "C4o"), "Coff",
                      sub("[0-9].*$", "", sub("-.*$", "", meas$block))))
  meas$mid <- meas$start_min + meas$seconds / 120
  cat("\nmean clock minute of measured rows by programme x board (time-of-day balance):\n")
  print(reshape(aggregate(mid ~ prog + board, meas, function(z) round(mean(z), 1)), idvar = "prog", timevar = "board", direction = "wide"))
  pp <- meas[grepl("^P[0-9]{2}$|^H[01]", meas$pair_id), ]
  w <- reshape(pp[, c("pair_id", "board", "mid")], idvar = "pair_id", timevar = "board", direction = "wide")
  w$lag <- w$mid.4 - w$mid.2
  cat("\npaired static placements: board-4 minus board-2 time (min): mean", round(mean(w$lag), 1), " SD", round(sd(w$lag), 1),
      " range", paste(round(range(w$lag), 1), collapse = " to "), " n", nrow(w), " b2-first", sum(w$lag > 0), "\n")
  cat("P halves:\n"); print(P[, c("pair_id", "position", "orientation", "class", "half")])
  cat("H halves:\n"); print(H[, c("cls", "half")])
  cat("HX halves:\n"); print(HX[, c("cls", "pair_on", "pair_off", "half")])
  cat("T0 order:", t0_order, "\n")
  cat("REF times (min):\n"); print(meas[meas$pair_id %in% c("REFz", "REFx"), c("board", "pair_id", "start_min")])
}
