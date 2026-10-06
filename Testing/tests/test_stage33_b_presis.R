# Stage 33 module B (Functions/stage33_b_presis.R): synthetic checks only.
#
#   1. covariates: age_c, w_c centred within cohort; w_c = w_within + w_srcmean; singleton source cage w_within = 0;
#      SD_w (n - 1, cohorts in which the covariate varies, source-cage level for w_srcmean); variant rows;
#   2. small estimators: nested moment ICC (= s33_moment_icc with one cohort; signed); CR2-Welch linear combination
#      (= s33_cr2_welch); B2/B6 contrast weights; direction (same / opposite);
#   3. arena parser, three-chamber calibration record, SLEAP ratios; gates GB-13, GB-14a, GB-14b (recorded), GB-15, GB-16
#      with passing and failing fixtures (NA and empty included);
#   4. GB-12 weight_dev coupling: holds on unpermuted structure; a within-cohort x condition permutation of the outcome
#      columns (tp2 left in place, as in the development context) breaks it;
#   5. units: pooled within-(cohort x condition) SD, u values, indices with a minimum of parts re-expressed in their own
#      SD; cohort means, deviations from the own-sex 3-cohort mean, ranks (1 = highest); profile agreement with LOBO;
#   6. cage-resampling stream: one seed, declared cell order, replicate means = (U S) / (U n); the E12 pooled r comes from
#      the same index matrices;
#   7. variance shares: shares and s_cohort identities; parametric bootstrap reproducible; a component at 0 gets the
#      spread label and the profile upper limit; glmmTMB dispformula ~ Batch on known SDs (S11, n-weighted disp^2);
#   8. association rows: KR without p; CR2 by source cage = clubSandwich on the OLS fixed part; E6 group; SD_w; job lists
#      and the twelve sensitivity seeds;
#   9. the whole module on a synthetic design (reduced job lists, small B): declared tables, GC-8, the lint, the fixed
#      selection sentence, no chance or probability column, re-extractable outcome-free products, cross-contract columns
#      (X1, X4, X5 with partner fixtures), X1 identities (o_5 = -sum, layout terms), same-board differences, no interval on
#      the rate contrast or a CON row.
# Portable: no S: drive, no live data, no readxl; sources Functions/ only.

suppressPackageStartupMessages({ library(data.table); library(digest) })
fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")
must_error <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
near <- function(a, b, tol = 1e-10) length(a) == length(b) && isTRUE(all(abs(a - b) <= tol))

for (f in c("Functions/rfid_canonical_inference.R", "Functions/stage32_inference.R", "Functions/stage30_figure_bundle.R",
            "Functions/stage32_run.R", "Functions/stage33_common.R", "Functions/stage33_run.R", "Functions/stage33_b_presis.R")) source(f)
cfg <- new.env(); sys.source("Functions/behavior_analysis_config.R", envir = cfg)
canon <- s33_load_canonical_id("Functions/behavioral_dynamics_helpers.R")$fun
T0 <- Sys.time()

# ---------------------------------------------------------------- synthetic design (used by sections 7-9)
#' Six cohorts (sex nested as in the experiment); per cohort 4 source cages x 3 SIS crossed into 4 CC1 cages of 3, later
#' cages re-mixed, one CON cage of 4 (its own source cage) whose board changes at every cage change; 2 untracked B1 SIS;
#' C57BL/6J source cage 1 in B1, B3, B5; B2 and B6 SIS lowest in rate and highest in arena distance of their sex.
mk_design <- function(seed = 11L) {
  set.seed(seed)
  con_board <- c(B1 = "sys.3", B2 = "sys.4", B3 = "sys.3", B4 = "sys.3", B5 = "sys.3", B6 = "sys.2"); boards <- paste0("sys.", 1:5)
  base_rate <- c(B1 = 24, B2 = 15, B3 = 28, B4 = 26, B5 = 23, B6 = 17)
  arena_shift <- c(B1 = 0, B2 = 500, B3 = 0, B4 = 100, B5 = 50, B6 = 500)
  A <- list(); E <- list(); L <- list()
  for (bi in seq_along(S33_COHORTS)) {
    b <- S33_COHORTS[bi]; sx <- S33_SEX_OF_COHORT[[b]]; cc1d <- as.Date("2023-01-10") + 90L * (bi - 1L)
    conb <- vapply(1:4, function(k) boards[(match(con_board[[b]], boards) + k - 2L) %% 5L + 1L], "")
    src_eff <- stats::rnorm(4, sd = 1.5); cage_eff <- stats::rnorm(4, sd = 1)
    rs <- as.POSIXct(paste(format(cc1d), "14:00:00"), tz = "UTC") + 600 * bi
    for (k in 1:4) { an <- as.POSIXct(paste(format(cc1d + 4L * (k - 1L)), "18:30:00"), tz = "UTC"); st <- an - (2.5 + bi / 3 + k / 7) * 3600
      L[[length(L) + 1L]] <- data.table(Batch = b, CC = paste0("CC", k), rec_start = st, anchor_1830 = an,
                                        lag_h = as.numeric(difftime(an, st, units = "hours")), board_first_delay_h = 0.01) }
    add_animal <- function(id, cond, src, s, cg, untracked) {
      age <- if (b == "B1") 24 else if (cond == "CON") 25 else 24 + (s %% 3)
      tp1 <- 8 + 0.4 * (age - 24) + stats::rnorm(1, sd = 0.8); tp2 <- tp1 + 1.5 + stats::rnorm(1, sd = 0.5)
      sis_boards <- lapply(1:4, function(k) setdiff(boards, conb[k]))
      bd <- if (cond == "CON") conb else vapply(1:4, function(k) sis_boards[[k]][cg[k]], "")
      cages <- paste(b, bd, paste0("CC", 1:4), sep = "|")
      rate <- if (untracked) NA_real_ else max(4, base_rate[[b]] + (if (cond == "SIS") src_eff[s] + cage_eff[cg[1]] else 2) +
                                                 0.8 * (tp1 - 8) + stats::rnorm(1, sd = 3))
      dist <- 3000 + arena_shift[[b]] * (cond == "SIS") + stats::rnorm(6, sd = 300)
      A[[length(A) + 1L]] <<- data.table(AnimalNum = id, Batch = b, Sex = sx, condition = cond, tracked = !untracked, untracked_b1 = untracked,
        xmate_excluded = FALSE, cc1_cage = cages[1], cc2_cage = if (untracked) NA_character_ else cages[2],
        cc3_cage = if (untracked) NA_character_ else cages[3], cc4_cage = cages[4], cc1_board = bd[1], src_cage = src,
        line = if (cond == "SIS" && s == 1L && b %in% c("B1", "B3", "B5")) "C57BL/6J" else "C57BL/6JRj",
        dob = cc1d - as.integer(age), cc1_date = cc1d, age_cc1 = age, tp1 = tp1, tp2 = tp2, tp1_date = cc1d - 3L, tp2_date = cc1d,
        crossing_rate = rate, shared_zone_use = if (untracked) NA_real_ else stats::runif(1, 0.2, 0.6),
        hw_a1 = FALSE, n_in_cage = NA_integer_, n_tracked_mates = NA_integer_, mark_date = cc1d - 7L, rack = "rack 1", method = "m1",
        oft_distance = dist[1], epm_distance = dist[2] / 2, nor_distance = dist[3] / 2, hab_distance = dist[4], s1_distance = dist[5],
        s2_distance = dist[6])
      if (!untracked) for (k in 1:4) E[[length(E) + 1L]] <<- data.table(AnimalNum = id, Batch = b, Sex = sx, condition = cond,
        CC = paste0("CC", k), CageEpisodeID = cages[k], board = bd[k],
        crossing_rate = if (k == 1L) rate else max(4, base_rate[[b]] + stats::rnorm(1, sd = 4)), shared_zone_use = stats::runif(1, 0.2, 0.6),
        n_in_cage = NA_integer_, n_tracked_mates = NA_integer_, hardware_flag = FALSE, in_S13 = FALSE, obs_s = 43200, n_events = 100L)
    }
    for (s in 1:4) for (j in 1:3) add_animal(paste0(b, "S", s, "N", j), "SIS", paste0(b, "-SRC", s), s,
      c((s + j - 2L) %% 4L + 1L, (s + 2L * j) %% 4L + 1L, (s + 3L * j) %% 4L + 1L, (s - j) %% 4L + 1L),
      untracked = b == "B1" && s == 4L && j > 1L)
    for (j in 1:4) add_animal(paste0(b, "CN", j), "CON", paste0(b, "-CONSRC"), 0L, rep(1L, 4), FALSE)
  }
  a <- rbindlist(A); ep <- rbindlist(E)
  a[, `:=`(line_J = as.integer(line == "C57BL/6J"), J7 = FALSE, J9 = FALSE, x_RU = crossing_rate / S33_RU,
           pop_tracked_111 = tracked, pop_sis_87 = tracked & condition == "SIS", pop_focal_85 = tracked & condition == "SIS",
           pop_sis_93 = condition == "SIS", pop_canonical_117 = TRUE, pop_con_24 = condition == "CON")]
  ep[AnimalNum == "B2S1N1" & CC == "CC3", `:=`(in_S13 = TRUE, hardware_flag = TRUE)]
  arena_cols <- c("oft_distance", "epm_distance", "nor_distance", "hab_distance", "s1_distance", "s2_distance")
  arena <- a[, c("AnimalNum", arena_cols), with = FALSE]; a[, (arena_cols) := NULL]
  arena[AnimalNum %in% c("B3CN1", "B4S2N1"), oft_distance := NA_real_]; arena[AnimalNum == "B3CN2", `:=`(hab_distance = NA_real_, s1_distance = NA_real_, s2_distance = NA_real_)]
  cages <- ep[, .(Sex = Sex[1], board = board[1], condition = condition[1], n_tracked = .N, members = paste(sort(AnimalNum), collapse = ";")),
              by = .(CageEpisodeID, Batch, CC)]
  setkey(a, AnimalNum)
  list(animals = a, episodes = ep, cages = cages, lags = rbindlist(L), arena = arena,
       gates = rbind(s33_gate_rows("GC-6g", "Timeline Animals matches once each", TRUE), s33_gate_rows("GC-6p", "source-cage facts", TRUE)),
       sets = list())
}
#' Workbook-like sheet tables for the arena parser (as read by the runner-only reader).
mk_raw_sheets <- function(arena, extra_unmatched = FALSE) {
  nominal <- c(oft = 600, epm = 300, nor = 300, hab = 600, s1 = 600, s2 = 600)
  sp <- function(v, k) v / nominal[[k]]
  ids <- arena$AnimalNum
  tc <- rbindlist(lapply(c("HAB", "S1", "S2"), function(ph) { v <- arena[[paste0(tolower(ph), "_distance")]]
    data.table(animal_id = ids, phase = ph, distance = v, speed_raw = sp(v, tolower(ph))) }))
  list(master_wide = data.table(animal_id = ids, oft_distance = arena$oft_distance),
       oft = data.table(animal_id = c(ids, NA, NA), code = c(rep(NA_character_, length(ids)), "Y8G0", "I1E6"),
                        raw_distance = c(arena$oft_distance, NA, NA), raw_speed = c(sp(arena$oft_distance, "oft"), NA, NA)),
       epm = data.table(animal_id = ids, bodycentre_raw_distance = arena$epm_distance, bodycentre_raw_speed = sp(arena$epm_distance, "epm")),
       nor = data.table(animal_id = c(ids, if (extra_unmatched) "OR999"), distance = c(arena$nor_distance, if (extra_unmatched) 1),
                        speed_raw = c(sp(arena$nor_distance, "nor"), if (extra_unmatched) 1)),
       three_chamber = tc,
       sleap_v2 = data.table(animal_id = ids, sleap_OFT_distance_cm = arena$oft_distance * 0.995,
                             sleap_EPM_distance_cm = arena$epm_distance * 1.002, sleap_NOR_distance_cm = arena$nor_distance))
}
D <- mk_design()

cat("1. covariates, SD_w and variant rows\n")
x <- data.table(AnimalNum = paste0("a", 1:9), Batch = rep(c("B1", "B2", "B3"), each = 3), src_cage = c("s1", "s1", "s2", "s3", "s3", "s4", "s5", "s5", "s5"),
                cc1_cage = paste0("c", c(1, 2, 2, 3, 4, 4, 5, 6, 6)), age_cc1 = c(24, 24, 24, 25, 25, 27, 26, 26, 26),
                tp1 = c(8, 9, 11, 7, 9, 10, 9, 10, 14), tp2 = 10)
cv <- s33b_covariates(x)
check(near(cv[, sum(age_c), by = Batch]$V1, rep(0, 3)) && near(cv[, sum(w_c), by = Batch]$V1, rep(0, 3)), "age_c and w_c centred within cohort")
check(near(cv$w_c, cv$w_within + cv$w_srcmean), "w_c = w_within + w_srcmean exactly")
check(cv[src_cage == "s2", w_within] == 0 && cv[src_cage == "s4", w_within] == 0, "a singleton source cage gets w_within = 0 (OR568 rule)")
check(near(cv[AnimalNum == "a1", w_within], 8 - 8.5) && near(cv[AnimalNum == "a1", w_srcmean], 8.5 - 28 / 3), "w_within and w_srcmean values")
sd_age <- s33b_sd_w(cv, "age_c")
check(near(sd_age, sqrt(sum(cv[Batch == "B2", (age_c)^2]) / (3 - 1))), "SD_w(age) uses only the cohorts in which age varies (n - 1)")
src_lvl <- unique(cv[, .(Batch, src_cage, w_srcmean)])
src_lvl <- src_lvl[Batch != "B3"]                       # B3 has one source cage: w_srcmean does not vary there
check(near(s33b_sd_w(cv, "w_srcmean", "source_cage"), s33_resid_sd(src_lvl$w_srcmean, src_lvl$Batch)), "SD_w(w_srcmean) at source-cage level")
check(near(s33b_sd_w(cv, "w_c"), sqrt(sum(cv$w_c^2) / 8)), "SD_w(w_c) = n - 1 residual SD on cohort")
check(identical(names(s33b_sd_w_set(cv)), c("age_c", "w_c", "w_within", "w_srcmean")), "the SD_w set names its four covariates")
v2 <- s33b_variant_rows(x[, `:=`(Sex = "Male")], list(b2_src_unknown = TRUE))
check(all(grepl("^B2_unknown_", v2[Batch == "B2", src_cage])) && all(v2[Batch == "B2", w_within] == 0), "S2: B2 source cages coded unknown (singletons)")
check(nrow(s33b_variant_rows(x, list(drop_batch = "B1"))) == 6L && nrow(s33b_variant_rows(x, list(drop_animals = "a9"))) == 8L, "S1 / S7 drop a cohort; S9 drops an animal")
check(identical(as.numeric(s33b_variant_rows(x, list(wcol = "tp2"))$w_c), rep(0, 9)), "S6 uses tp2 in place of tp1")
md <- s33b_model_data(cbind(x, y0 = c(1, NA, 3:9)), "y0")
check(nrow(md) == 8L && identical(levels(md$Batch), c("B1", "B2", "B3")) && identical(md$CageEpisodeID, md$cc1_cage) && identical(md$y, c(1, 3:9)),
      "model data: finite outcome rows, y, Batch in cohort order, CageEpisodeID = the CC1 cage")
ok("covariate identities, SD_w, variants")

cat("\n2. ICC, CR2-Welch combination, contrast weights, direction\n")
set.seed(3); y <- rnorm(30); g <- rep(paste0("c", 1:10), each = 3); h <- rep(c("B1", "B2"), each = 15)
check(near(s33b_moment_icc_within(y[1:15], g[1:15], h[1:15]), s33_moment_icc(y[1:15], g[1:15])), "one cohort: nested ICC = s33_moment_icc")
yy <- c(1, 9, 1, 9, 1, 9); check(s33b_moment_icc_within(yy, c(1, 1, 2, 2, 3, 3), rep("B1", 6)) < 0, "the nested ICC is signed")
mi <- tapply(y, g, mean); ni <- tapply(y, g, length); hm <- tapply(y, h, mean); hg <- tapply(h, g, `[`, 1)
msb <- sum(ni * (mi - hm[hg])^2) / (10 - 2); msw <- sum((y - mi[g])^2) / (30 - 10); n0 <- (30 - 2 * (5 * 9) / 15) / 8
check(near(s33b_moment_icc_within(y, g, h), (msb - msw) / (msb + (n0 - 1) * msw)), "two cohorts: (MSB - MSW) / (MSB + (n0 - 1) MSW) with cohort-centred MSB")
check(must_error(s33b_moment_icc_within(y, rep("c1", 30), h)), "a cluster spanning cohorts stops")
ya <- c(10, 12, 14, 20, 22, 24, 30, 31, 33); ca <- rep(c("a", "b", "c"), each = 3); yb <- c(5, 6, 8, 9, 3, 4, 7, 7, 6); cb <- rep(c("d", "e", "f"), each = 3)
ma <- s33_cr2_mean(ya, ca); mb <- s33_cr2_mean(yb, cb); wl <- s33_cr2_welch(ya, ca, yb, cb)
lc <- s33b_lincomb(c(ma$estimate, mb$estimate), c(ma$se, mb$se), c(ma$df, mb$df), c(1, -1))
check(near(lc$estimate, wl$estimate) && near(lc$se, wl$se) && near(lc$df, wl$df) && near(lc$ci_low, wl$ci_low), "lincomb(1, -1) = s33_cr2_welch")
check(is.na(s33b_lincomb(c(1, 2), c(NA, 1), c(NA, 3), c(1, -1))$se) && s33b_lincomb(c(1, 2), c(NA, 1), c(NA, 3), c(1, -1))$estimate == -1,
      "a part without a CR2 se: point value only")
check(near(s33b_lincomb(c(1, NA, 2), c(1, NA, 1), c(3, NA, 3), c(1, 0, -1))$estimate, -1), "a zero-weight part may be missing")
wM <- s33b_contrast_weights("Male"); wF <- s33b_contrast_weights("Female"); wA <- s33b_contrast_weights("average")
check(identical(unname(wM), c(-0.5, 1, 0, 0, -0.5, 0)) && identical(unname(wF), c(0, 0, -0.5, -0.5, 0, 1)) && near(wA, (wM + wF) / 2),
      "B2 - mean(B1, B5); B6 - mean(B3, B4); average = half of each")
sw <- setNames(c(2, 2, 4, 4, 2, 4), S33_COHORTS)
check(near(s33b_contrast_weights("average", sw), wA / sw), "u-scale weights divide by the sex's s_w")
dr <- s33b_direction(c(2, -1, 0, NA), c(-3, -3, -3, -3))
check(identical(dr$direction_vs_rate, c("opposite", "same", NA, NA)) && identical(dr$direction[1:2], c("above the other two of its sex", "below the other two of its sex")),
      "direction: sign of the contrast and agreement with the rate contrast's sign")
ok("ICC, lincomb, weights, direction")

cat("\n3. arena parser, calibration record, ratios; gates GB-13 to GB-16\n")
raw <- mk_raw_sheets(D$arena, extra_unmatched = TRUE)
pa <- s33b_parse_arena(raw, canon, D$animals$AnimalNum)
check(nrow(pa$arena) == nrow(D$animals) && identical(pa$arena$AnimalNum, sort(D$animals$AnimalNum, method = "radix")), "one parsed row per canonical animal")
check(identical(pa$dropped_codes, c("I1E6", "Y8G0")) && pa$audit[item == "nor", unmatched_ids] == "OR999" && !any(pa$duplicates), "dropped codes, unmatched ids, no duplicates")
check(pa$oft_check[["n_both"]] == pa$oft_check[["n_equal"]] && pa$oft_check[["n_both"]] > 0, "master_wide oft_distance = oft raw_distance")
check(near(pa$arena[AnimalNum == "B2S1N1", hab_distance], D$arena[AnimalNum == "B2S1N1", hab_distance]) &&
        near(pa$arena[AnimalNum == "B2S1N1", s2_speed], D$arena[AnimalNum == "B2S1N1", s2_distance] / 600), "three-chamber phases by column")
raw_dup <- raw; raw_dup$epm <- rbind(raw_dup$epm, raw_dup$epm[1])
check(isTRUE(s33b_parse_arena(raw_dup, canon, D$animals$AnimalNum)$duplicates[["epm"]]), "a duplicated animal row is flagged")
pa <- s33b_parse_arena(mk_raw_sheets(D$arena), canon, D$animals$AnimalNum)
geo <- data.table(assay = c("SocP", "SocP", "SocP", "NOR"), width_px = c(650, 724, 690, 780))
cal <- s33b_three_chamber_calibration(c("# config", "phases: [S1, S2]"), geo, c("animal_id", "sleap_OFT_distance_cm"))
check(!cal$documented && all(cal$checks) && identical(cal$phases, c("S1", "S2")) && near(cal$width_ratio, 724 / 650), "calibration not documented")
cal_doc <- s33b_three_chamber_calibration(c("phases: [HAB, S1, S2]"), geo[1], c("animal_id", "sleap_SocP_HAB_distance_cm"))
check(cal_doc$documented && !any(cal_doc$checks), "a documented calibration is recognised")
g14b <- s33b_gate_gb14b(cal); g14b_doc <- s33b_gate_gb14b(cal_doc)
check(isTRUE(g14b$passed) && !g14b$hard && !isTRUE(g14b_doc$passed) && isTRUE(s33_stop_on_gates(g14b_doc)), "GB-14b recorded: never stops")
ratios <- s33b_sleap_ratios(pa$arena, pa$sleap_v2, setNames(D$animals$Batch, D$animals$AnimalNum))
check(nrow(ratios) == 18L && near(ratios[measure == "oft_distance", median_ratio], rep(0.995, 6)), "per-cohort median ratios (3 measures x 6 cohorts)")
check(isTRUE(s33b_gate_gb14a(ratios)$passed), "GB-14a passes in [0.98, 1.02]")
check(!isTRUE(s33b_gate_gb14a(copy(ratios)[1, median_ratio := 1.03])$passed) && !isTRUE(s33b_gate_gb14a(ratios[-1])$passed) &&
        !isTRUE(s33b_gate_gb14a(copy(ratios)[2, median_ratio := NA])$passed), "GB-14a fails out of range, short or NA")
# GB-13 on a fixture with the plan's counts: 117 animals, 111 tracked (87 SIS), missingness as declared
ids117 <- sprintf("A%03d", 1:117); trk <- ids117[1:111]; sis <- ids117[1:87]
ar <- data.table(AnimalNum = ids117)
for (k in seq_len(nrow(S33B_ARENA))) { v <- rep(1000, 117); set(ar, j = S33B_ARENA$measure[k], value = v)
  set(ar, j = S33B_ARENA$speed[k], value = v / S33B_ARENA$nominal_s[k]) }
ar[AnimalNum %in% c("A086", "A087", "A100", "A101"), oft_distance := NA]                    # 2 SIS, 2 CON among the tracked
ar[AnimalNum == "A102", c("hab_distance", "s1_distance", "s2_distance") := NA]              # one tracked CON
g13 <- function(a, codes = c("I1E6", "Y8G0"), dup = c(a = FALSE)) s33b_gate_gb13(list(arena = a, dropped_codes = codes, duplicates = dup), trk, sis)
check(isTRUE(g13(ar)$passed), "GB-13 passes with 113/117/117/116, 107/111/111/110 and SIS 85/87/87/87")
check(!isTRUE(g13(copy(ar)[AnimalNum == "A001", epm_distance := NA])$passed), "GB-13 fails on one more missing value")
check(!isTRUE(g13(ar, codes = "Y8G0")$passed) && !isTRUE(g13(ar, dup = c(a = TRUE))$passed), "GB-13 fails on other dropped codes or a duplicate")
check(!isTRUE(g13(copy(ar)[1:10, epm_speed := epm_distance / 400])$passed), "GB-13 fails when durations are off")
check(!isTRUE(g13(copy(ar)[AnimalNum == "A050", oft_distance := NA][AnimalNum == "A100", oft_distance := 1])$passed),
      "GB-13 fails when the SIS count differs at the same tracked count")
# GB-15: 123 CC1 pairs from cage sizes 1,1,4,4 / 4x4 / 4x4 / 3,4,4,4 / 3,4,4,4 / 3,4,4,4
sizes <- list(c(1, 1, 4, 4), c(4, 4, 4, 4), c(4, 4, 4, 4), c(3, 4, 4, 4), c(3, 4, 4, 4), c(3, 4, 4, 4))
s15 <- rbindlist(lapply(1:6, function(b) rbindlist(lapply(seq_along(sizes[[b]]), function(k)
  data.table(AnimalNum = sprintf("B%dK%dN%d", b, k, seq_len(sizes[[b]][k])), cc1_cage = sprintf("B%d|sys.%d|CC1", b, k))))))
s15[, cc4_cage := paste0("u", .I)]
check(!isTRUE(s33b_gate_gb15(s15)$passed) && nrow(s33b_cc1_pairs(s15)) == 123L, "123 CC1 pairs; GB-15 fails with no shared CC4 cage")
s15b <- copy(s15)[AnimalNum == "B2K1N2", cc4_cage := s15[AnimalNum == "B2K1N1", cc4_cage]]
check(isTRUE(s33b_gate_gb15(s15b)$passed), "GB-15 passes with exactly 1 of 123 pairs sharing a CC4 cage")
s15c <- copy(s15b)[AnimalNum == "B3K1N2", cc4_cage := s15[AnimalNum == "B3K1N1", cc4_cage]]
check(!isTRUE(s33b_gate_gb15(s15c)$passed) && !isTRUE(s33b_gate_gb15(s15b[AnimalNum != "B2K2N4"])$passed), "GB-15 fails with 2 shared pairs or 120 pairs")
check(isTRUE(s33b_gate_gb16(S33_SEEDS)$passed), "GB-16 passes on the declared seeds")
bad_seeds <- S33_SEEDS; bad_seeds[["B_sens_3"]] <- bad_seeds[["B_sens_2"]]
check(!isTRUE(s33b_gate_gb16(bad_seeds)$passed) && !isTRUE(s33b_gate_gb16(S33_SEEDS[!grepl("^B_sens_1$", names(S33_SEEDS))])$passed),
      "GB-16 fails on a duplicated or missing seed index")
far <- S33_SEEDS; far[["B_sens_12"]] <- 33020200L + 150L
check(!isTRUE(s33b_gate_gb16(far)$passed), "GB-16 fails on an index >= 100")
tla <- s33b_timeline_prefix_audit(c(paste0("HKH0-", ids117[1:115]), "MCM0-A116", "LUM0-A117"), canon, ids117)
check(isTRUE(tla$ok[1]) && tla$n_observed[1] == 117L && tla$n_observed[2] == 115L, "Timeline join: four-prefix rule 117/117, two-prefix rule recorded")
ok("parser, calibration, ratios; GB-13, GB-14a, GB-14b, GB-15, GB-16 pass and fail")

cat("\n4. GB-12 weight_dev coupling\n")
mk_wd <- function(seed = 5L) {
  set.seed(seed)
  a <- data.table(AnimalNum = sprintf("W%03d", 1:96), Batch = rep(S33_COHORTS, each = 16), condition = rep(rep(c("CON", "SIS"), c(4, 12)), 6))
  a[, Sex := S33_SEX_OF_COHORT[Batch]][, `:=`(tp2 = rnorm(.N, 10), tp3 = rnorm(.N, 12))][, tp6 := tp3 + rnorm(.N, 6, 1.5)]
  a[, d := ifelse(Batch == "B6", tp6 - tp3, tp6 - tp2)]
  ref <- a[condition == "CON", .(m = mean(d), s = sd(d)), by = Sex]
  a[, weight_dev := (d - ref$m[match(Sex, ref$Sex)]) / ref$s[match(Sex, ref$Sex)]][, d := NULL][]
}
wd <- mk_wd()
check(isTRUE(s33b_gate_gb12(wd)$passed), "unpermuted structure: r > 0.9999 within sex (B6 with tp6 - tp3)")
perm <- copy(wd); set.seed(999L)
perm[, c("weight_dev", "tp3", "tp6") := { i <- sample.int(.N); list(weight_dev[i], tp3[i], tp6[i]) }, by = .(Batch, condition)]
gp <- s33b_gate_gb12(perm)
check(!isTRUE(gp$passed), "a within-cohort x condition permutation of the outcome columns (tp2 left in place) breaks it")
check(isTRUE(s33b_gate_gb12(perm[Batch == "B6"])$n_ok == 1L) && !isTRUE(s33b_gate_gb12(perm[Batch == "B6"])$passed),
      "B6 alone keeps its coupling (tp3 travels with weight_dev) but one sex is not enough")
check(!isTRUE(s33b_gate_gb12(copy(wd)[, weight_dev := NA_real_])$passed), "no finite pair: the gate fails")
ok("passes on structure; fails only through the permutation")

cat("\n5. units, cohort means, ranks, profile agreement\n")
ux <- data.table(AnimalNum = paste0("u", 1:24), Batch = rep(S33_COHORTS, each = 4), condition = rep(c("CON", "SIS"), 12))
ux[, Sex := S33_SEX_OF_COHORT[Batch]]; set.seed(8); for (v in c("oft_distance", "epm_distance", "nor_distance", "hab_distance")) set(ux, j = v, value = rnorm(24, 1000, 100))
ux[AnimalNum == "u1", `:=`(oft_distance = NA, epm_distance = NA)]
us <- s33b_unit_scale(ux, "oft_distance")
z <- ux[Sex == "Male" & is.finite(oft_distance)]; z[, r := oft_distance - mean(oft_distance), by = .(Batch, condition)]
check(near(us[Sex == "Male", s_w], sqrt(sum(z$r^2) / (nrow(z) - uniqueN(z[, .(Batch, condition)])))) &&
        near(us[Sex == "Male", sex_mean], mean(z$oft_distance)), "s_w = pooled within-(cohort x condition) SD of the sex; sex mean")
un <- s33b_add_units(ux, c("oft_distance", "epm_distance", "nor_distance", "hab_distance"), S33B_INDEX[c("arena_index", "arena_index4")])
check(is.na(un$x[AnimalNum == "u1", arena_index]) && is.finite(un$x[AnimalNum == "u1", arena_index4]) == FALSE, "u1 lacks 2 of 3 parts: no index")
check(near(s33b_unit_scale(un$x, "u_arena_index")$s_w, c(1, 1)), "an index is re-expressed in its own pooled SD")
check(near(un$x[AnimalNum == "u2", u_oft_distance], (ux[AnimalNum == "u2", oft_distance] - us[Sex == "Male", sex_mean]) / us[Sex == "Male", s_w]) &&
        near(un$x[AnimalNum == "u9", u_oft_distance], (ux[AnimalNum == "u9", oft_distance] - us[Sex == "Female", sex_mean]) / us[Sex == "Female", s_w]),
      "u = (x - sex mean) / s_w")
cm <- s33b_cohort_means(un$x, c("oft_distance", "arena_index"), un$scales)
check(near(cm[, sum(dev_u_from_own_sex_3cohort_mean), by = .(condition, Sex, measure)]$V1, rep(0, 8)), "deviations from the own-sex 3-cohort mean sum to 0")
check(all(cm[, rank_of_3_within_sex[which.max(mean_raw)] == 1L && rank_of_3_within_sex[which.min(mean_raw)] == 3L, by = .(condition, Sex, measure)]$V1),
      "rank_of_3_within_sex: 1 = highest")
ur <- setNames(c(-1, 2, -0.5, 1, -1, 0.3), S33_COHORTS); um <- setNames(c(0.2, -1, 0.4, 0.1, 0.8, -0.6), S33_COHORTS)
pa2 <- s33b_profile_agreement(ur, um)
ctr <- function(u) u - ave(u, S33_SEX_OF_COHORT[names(u)])
check(near(pa2$pearson, cor(ctr(ur), ctr(um))) && near(pa2$spearman, cor(ctr(ur), ctr(um), method = "spearman")), "Pearson and Spearman over the six sex-centred values")
lo <- vapply(S33_COHORTS, function(b) { k <- setdiff(S33_COHORTS, b); cor(ctr(ur[k]), ctr(um[k])) }, 0)
check(near(pa2$lobo_pearson, range(lo)), "leave-one-cohort-out re-centres that sex on its other two cohorts")
check(is.na(s33b_profile_agreement(ur, um[1:5])$pearson), "fewer than six cohorts: no value")
check(identical(s33b_rank_order(ur), "B1 < B5 < B2; B3 < B6 < B4"), "rank orders within sex")
ok("units, indices, cohort means, ranks and profile agreement")

cat("\n6. cage-resampling stream\n")
cx <- data.table(AnimalNum = paste0("r", 1:20), Batch = rep(c("B1", "B2"), each = 10), cc1_cage = rep(paste0("k", 1:5), each = 4),
                 cc4_cage = paste0("m", c(1, 2, 3, 1, 2, 3, 1, 2, 3, 3, 4, 5, 6, 4, 5, 6, 4, 5, 6, 6)))
set.seed(4); cx[, `:=`(crossing_rate = rnorm(.N, 20, 4), oft = rnorm(.N, 1000, 100), u_crossing_rate = rnorm(.N), u_oft = rnorm(.N))]
sums <- list(B1_cc1 = s33b_cell_sums(cx[Batch == "B1"], "cc1_cage", "crossing_rate"), B2_cc1 = s33b_cell_sums(cx[Batch == "B2"], "cc1_cage", "crossing_rate"),
             B1_cc4 = s33b_cell_sums(cx[Batch == "B1"], "cc4_cage", "oft"), B2_cc4 = s33b_cell_sums(cx[Batch == "B2"], "cc4_cage", "oft"))
cells <- vapply(sums, function(z) length(z$cages), 1L)
check(identical(unname(cells), c(3L, 3L, 3L, 3L)) && identical(names(cells), c("B1_cc1", "B2_cc1", "B1_cc4", "B2_cc4")), "cells = cages per cohort in the declared order (CC1 cells, then CC4 cells)")
pairs <- list(B1_cc4 = list(oft = s33b_cell_pair_sums(cx[Batch == "B1"], "cc4_cage", "u_crossing_rate", "u_oft")),
              B2_cc4 = list(oft = s33b_cell_pair_sums(cx[Batch == "B2"], "cc4_cage", "u_crossing_rate", "u_oft")))
st <- s33b_stream_reps(cells, 25L, 33020301L, sums, pairs)
idx <- s33_resample_index(cells, 25L, 33020301L)
U4 <- s33_multiplicity(idx$B2_cc4, 3L)
check(near(st$means$B2_cc4[, "oft"], as.vector((U4 %*% sums$B2_cc4$S) / (U4 %*% sums$B2_cc4$n))), "replicate means = (U S) / (U n), cells in declared order")
check(identical(st$index_sha256, s33b_sha(idx)), "the index matrices are those of one s33_resample_index() call (sha256 recorded)")
rep_rows <- function(b, i) { cg <- sums[[paste0(b, "_cc4")]]$cages[idx[[paste0(b, "_cc4")]][i, ]]
  rbindlist(lapply(cg, function(k) cx[Batch == b & cc4_cage == k])) }
r1 <- { z <- rbind(rep_rows("B1", 1)[, .(Batch = "B1", a = u_crossing_rate, b = u_oft)], rep_rows("B2", 1)[, .(Batch = "B2", a = u_crossing_rate, b = u_oft)])
  z[, `:=`(a = a - mean(a), b = b - mean(b)), by = Batch]; sum(z$a * z$b) / sqrt(sum(z$a^2) * sum(z$b^2)) }
check(near(st$r$oft[1], r1), "E12 pooled within-cohort r of a replicate = the r of the drawn cages' animals (duplicates kept)")
check(near(s33b_pooled_r(list(s33b_cell_pair_sums(cx[Batch == "B1"], "cc1_cage", "u_crossing_rate", "u_oft"),
                              s33b_cell_pair_sums(cx[Batch == "B2"], "cc1_cage", "u_crossing_rate", "u_oft"))),
           { z <- copy(cx)[, `:=`(a = u_crossing_rate - mean(u_crossing_rate), b = u_oft - mean(u_oft)), by = Batch]; sum(z$a * z$b) / sqrt(sum(z$a^2) * sum(z$b^2)) }),
      "pooled within-cohort r (each cohort centred on its own mean)")
st2 <- s33b_stream_reps(cells, 25L, 33020301L, sums, pairs)
check(identical(st, st2), "the stream is reproducible from its seed")
ok("one seed per stream, declared order, means and r from the same matrices")

cat("\n7. variance shares, parametric bootstrap, boundary labels, glmmTMB dispersion\n")
fr <- copy(D$animals)
set.seed(21); fr[pop_sis_87 == TRUE, crossing_rate := rnorm(.N, 20, 4)]
ctx0 <- list(seeds = S33_SEEDS, B = c(nonparametric = 30L, parametric = 9L, parametric_sensitivity = 7L), checkpoint_dir = NULL, canon = canon)
lg <- new.env(parent = emptyenv())
mdl <- s33b_model_data(s33b_covariates(fr[pop_sis_87 == TRUE]), "crossing_rate")
m <- s33_kr_fit("y ~ Batch + (1 | src_cage) + (1 | CageEpisodeID)", mdl, "t_share", 6L)
fit <- methods::as(m$fit, "lmerMod"); vc <- s33b_varcomp(fit)
xb <- drop(lme4::getME(fit, "X") %*% lme4::fixef(fit))
check(near(vc[["v_fix"]], mean((xb - mean(xb))^2)), "v_fix = n-divisor variance of X b")
sh <- s33b_shares(vc, c("src_cage", "CageEpisodeID", "Residual"))
check(near(sum(sh[1, 1:3]), 1) && near(sh[1, "s_cohort"], vc[["v_fix"]] / (vc[["v_fix"]] + sum(vc[c("src_cage", "CageEpisodeID", "Residual")]))),
      "within-cohort shares sum to 1; s_cohort = v_fix / (v_fix + total)")
rk0 <- RNGkind()
b1 <- s33b_pboot(fit, c("src_cage", "CageEpisodeID", "Residual", "v_fix"), 5L, 33020101L)
b2 <- s33b_pboot(fit, c("src_cage", "CageEpisodeID", "Residual", "v_fix"), 5L, 33020101L)
check(identical(b1, b2) && nrow(b1) == 5L && identical(RNGkind(), rk0), "parametric bootstrap reproducible from its seed; RNG kind restored")
# boundary fixture: every CC1 cage mean equals its cohort mean, so the CC1-cage component is estimated at 0
fb <- copy(D$animals)[pop_sis_87 == TRUE]
fb[, crossing_rate := 20 + as.integer(substr(Batch, 2, 2)) + c(-2, 0, 2)[seq_len(.N)] * (1 + 0.1 * as.integer(factor(cc1_cage))), by = cc1_cage]
fb[, crossing_rate := crossing_rate - mean(crossing_rate) + 20 + as.integer(substr(Batch, 2, 2)), by = cc1_cage]
sj_b <- list(id = "S4b", label = "boundary fixture", outcome = "crossing_rate", population = "sis_87", group = "E1", re = S33B_RE[["S4b"]],
             boot = "S1_rate", cohort_share = TRUE)
rb <- s33b_run_share(fb, sj_b, ctx0, lg, "test")$rows
z0 <- rb[estimand == "variance_share_cc1_cage"]; zc <- rb[estimand == "variance_component_cc1_cage"]
check(z0$interval_method == "parametric_bootstrap_spread" && z0$interval_note == S33B_LABEL$spread && isTRUE(z0$singular),
      "a component at 0: bootstrap limits labelled spread")
check(grepl("^estimated at 0; upper 95% limit .* [(]profile likelihood[)]$", z0$boundary_statement) && zc$interval_method == "profile_likelihood" &&
        is.finite(zc$ci_high), "boundary statement with the profile upper limit")
check(rb[estimand == "variance_share_remaining", interval_method] == "parametric_bootstrap_percentile" && rb[estimand == "variance_share_between_cohort_means", level] == "cohort",
      "the remaining share is a percentile interval; s_cohort is a cohort-level row")
check(all(rb$boot_seed == S33_SEEDS[["B_sens_1"]] & rb$boot_B == 7L, na.rm = TRUE) && any(is.finite(rb$boot_seed)), "S1 rate bootstrap: seed 33020201, B from ctx$B")
# glmmTMB dispersion on known SDs (S11): n-weighted mean of predict(type = 'disp')^2
if (!requireNamespace("glmmTMB", quietly = TRUE)) fail("glmmTMB is needed for the S11 dispersion check (plan section 15)")
set.seed(31); sdb <- setNames(c(1, 2, 3, 1.5, 2.5, 4), S33_COHORTS)
ft <- data.table(AnimalNum = sprintf("T%03d", 1:360), Batch = rep(S33_COHORTS, each = 60), Sex = rep(S33_SEX_OF_COHORT, each = 60), condition = "SIS",
                 pop_sis_87 = TRUE, age_cc1 = 25, tp1 = 9)
ft[, `:=`(src_cage = paste0(Batch, "_s", rep(1:10, 6)), cc1_cage = paste0(Batch, "_c", rep(1:10, each = 6)), cc4_cage = paste0(Batch, "_d", 1:60 %% 7)), by = Batch]
ft[, crossing_rate := 20 + as.integer(substr(Batch, 2, 2)) + rnorm(.N, sd = sdb[Batch])][, crossing_rate := crossing_rate + rnorm(1, sd = 0.3), by = src_cage]
rs11 <- s33b_run_share(ft, s33b_share_jobs("rate")[[which(vapply(s33b_share_jobs("rate"), `[[`, "", "id") == "S11")]], ctx0, lg, "test")$rows
mdt <- s33b_model_data(s33b_covariates(ft), "crossing_rate")
gt <- s33b_fit_tmb("y ~ Batch + (1 | src_cage) + (1 | CageEpisodeID)", mdt, "t_tmb", 6L)
pdisp <- stats::predict(gt$fit, type = "disp")
check(all(abs(tapply(pdisp, mdt$Batch, mean)[S33_COHORTS] / sdb - 1) < 0.2), "glmmTMB predict(type = 'disp') is the residual SD of each cohort (known SDs)")
check(near(rs11[component == "remaining", variance], mean(pdisp^2), 1e-8) && abs(mean(pdisp^2) / mean(sdb[mdt$Batch]^2) - 1) < 0.25,
      "S11 remaining variance = n-weighted mean of disp^2 (about the n-weighted true variance)")
check(all(rs11$interval_method == "none") && grepl("B1 ", rs11[component == "remaining", residual_variance_by_cohort]) && !any(rs11$lead),
      "S11 shares: no interval; per-cohort residual variances recorded")
ok("share identities, bootstrap, boundary labels, glmmTMB dispersion")

cat("\n8. association rows, job lists and seeds\n")
fa <- copy(D$animals); sdw <- s33b_sd_w_set(s33b_covariates(fa[pop_sis_87 == TRUE]))
rj <- s33b_run_assoc(fa, list(outcome = "crossing_rate", population = "sis_87", form = "E5_line", id = "primary", label = "primary"), sdw, "test")
check(nrow(rj) == 2L && identical(rj$interval_method[2], "CR2_Satterthwaite_src_cage") && rj$interval_method[1] %in% c("KR", "Satterthwaite_fallback"),
      "E5 primary: a KR row and a CR2-by-source-cage row")
mdj <- s33b_model_data(s33b_covariates(fa[pop_sis_87 == TRUE]), "crossing_rate")
olsj <- s33_cr2_contrast(lm(y ~ Batch + line_J, mdj), mdj$src_cage, list(c(line_J = 1)))
check(near(rj$estimate[2], olsj$estimate) && near(rj$se[2], olsj$se) && near(rj$df[2], olsj$df), "CR2 by source cage = clubSandwich on the OLS fixed part")
check(!any(c("p_raw", "statistic", "p_value", "test") %in% names(rj)) && all(rj$lead) && all(grepl("3 source cages", rj$note)), "no p column; E5 rows lead; model-based label")
r3 <- s33b_run_assoc(fa, list(outcome = "crossing_rate", population = "sis_87", form = "E3_split", id = "S4c", label = "S4c", engine = "lm"), sdw, "test")
check(all(grepl("^t_[0-9]+$", r3$interval_method)) && identical(r3$estimand, c("age_at_cc1_adjusted_for_tp1", "within_source_cage", "between_source_cages_within_cohort")) &&
        identical(r3$level, c("source_cage_within_cohort", "animal_within_cohort", "source_cage_within_cohort")), "S4c OLS t rows; level-named estimands of the split form")
check(near(r3$estimate_per_sd_w, r3$estimate * sdw[r3$term]) && near(r3$sd_w, unname(sdw[r3$term])), "per SD_w = estimate x SD_w")
fz6 <- copy(fa)[, weight_dev := rnorm(.N)]
r6 <- s33b_run_assoc(fz6, list(outcome = "weight_dev", population = "sis_93", form = "E3_joint", id = "primary", label = "primary"), sdw, "test")
check(all(r6$estimand_group == "E6") && all(r6$mech_coupling == "shared_term") && !any(r6$lead) && all(grepl("^S33B[|]E6[|]", r6$model_id)) &&
        grepl("shared by construction", r6[term == "w_c", note]), "E6 weight_dev rows")
check(near(r6$sd_w, unname(sdw[r6$term])), "CombZ-family rows use the sis_87 SD_w passed in")
s12 <- s33b_s12(fa, "test")
check(startsWith(s12$note, "single CON cage per cohort; anti-conservative") && s12$df == 17 && s12$interval_method == "t_17",
      "S12: CON lm, df 17, flagged 'single CON cage per cohort; anti-conservative'")
sdw2 <- s33b_sd_w_set(s33b_covariates(fa[pop_sis_87 == TRUE], "tp2"))
r6b <- s33b_run_assoc(fa, list(outcome = "crossing_rate", population = "sis_87", form = "E3_split", id = "S6", label = "S6", wcol = "tp2"), sdw2, "test")
check(identical(r6b$term_label, c("age at CC1, adjusted for CC1-day weight (tp2)", "CC1-day weight (tp2) relative to source-cage mates (age-free)",
                                  "source-cage mean CC1-day weight (tp2), adjusted for age")) && near(r6b$sd_w, unname(sdw2[r6b$term])) &&
        identical(r3$term_label, c("age at CC1, adjusted for tp1 weight", "weight relative to source-cage mates (age-free)", "source-cage mean weight, adjusted for age")),
      "plan wording on the tp1 rows; S6 rows name the CC1-day weight and use its SD_w")
ja <- s33b_assoc_jobs("rate"); jc <- s33b_assoc_jobs("cz")
ids_rate <- unique(vapply(ja, `[[`, "", "id"))
check(all(c("primary", "S1", "S2", "S4a", "S4b", "S4c", paste0("S7_without_", S33_COHORTS), "S8_male", "S8_female", "S5", "S6", "S9", "S11") %in% ids_rate),
      "rate jobs: primary and S1, S2, S4a-c, S7 (6), S8 (2), S5, S6, S9, S11")
check(all(vapply(Filter(function(j) j$id == "S6", ja), `[[`, "", "form") %in% c("E3_joint", "E3_split", "E4_weight", "E4_split")), "S6 on the weight forms only")
check(identical(unique(vapply(jc, `[[`, "", "outcome"))[1], "CombZ_noBW") && all(c("CombZ_noWD", "CombZ", "weight_dev") %in% vapply(jc, `[[`, "", "outcome")),
      "CombZ family: CombZ_noBW first; weight_dev (E6)")
sj_all <- c(s33b_share_jobs("rate"), s33b_share_jobs("cz")); bt <- unlist(lapply(sj_all, `[[`, "boot"))
check(setequal(bt[!bt %in% c("E1_rate", "E2_CombZ")], names(S33B_SENS_BOOT)) && !anyDuplicated(bt) && identical(unname(S33B_SENS_BOOT), 1:12),
      "the twelve sensitivity bootstraps each have one stream; E1 and E2 their own")
check(identical(unname(S33_SEEDS[paste0("B_sens_", 1:12)]), 33020200L + 1:12) && S33_SEEDS[["B_e1_rate"]] == 33020101L && S33_SEEDS[["B_e2_cz"]] == 33020102L &&
        S33_SEEDS[["B_e9_arena"]] == 33020301L && S33_SEEDS[["B_s16"]] == 33020302L && S33_SEEDS[["B_s17"]] == 33020303L && S33_SEEDS[["B_s19"]] == 33020304L,
      "seeds 33020101, 33020102, 33020200 + k, 33020301-04")
av <- s33b_arena_variants()
check(identical(vapply(av, `[[`, "", "id"), c("primary", "S16", "S17", "S19")) && identical(vapply(av, `[[`, "", "stream"), c("B_e9_arena", "B_s16", "B_s17", "B_s19")),
      "arena variants and their streams")
ok("association rows, job lists, seeds")

cat("\n9. the whole module on the synthetic design (reduced jobs, small B)\n")
full_assoc_jobs <- s33b_assoc_jobs; full_share_jobs <- s33b_share_jobs
s33b_assoc_jobs <- function(kind = c("rate", "cz")) Filter(function(j) j$id == "primary" || (j$id %in% c("S4c", "S11", "S6") && j$form == "E3_joint") ||
                                                              (j$id == "S7_without_B1" && j$form == "E5_line") || (j$id == "S3" && j$outcome == "weight_dev"), full_assoc_jobs(kind))
s33b_share_jobs <- function(kind = c("rate", "cz")) Filter(function(j) j$id %in% c("primary", "S11", "S13", "S15_sis_93"), full_share_jobs(kind))
design <- D[c("animals", "episodes", "cages", "lags", "gates", "sets")]
pa <- s33b_parse_arena(mk_raw_sheets(D$arena), canon, design$animals$AnimalNum)
tl <- s33b_timeline_prefix_audit(paste0("HKH0-", design$animals$AnimalNum), canon, design$animals$AnimalNum)
ctx <- list(seeds = S33_SEEDS, B = c(S33_B[setdiff(names(S33_B), c("nonparametric", "parametric", "parametric_sensitivity"))],
                                     nonparametric = 40L, parametric = 9L, parametric_sensitivity = 5L), checkpoint_dir = NULL, canon = canon)
p2 <- s33b_phase2_core(design, ctx, pa, cal, tl)
check(identical(names(p2$products), S33B_OUTCOME_FREE_PRODUCTS) && all(vapply(p2$products, function(t) isTRUE(s33_assert_outcome_free(t)), TRUE)),
      "Phase 2: the declared outcome-free products, none with an outcome column")
check(identical(p2$gates$gate_id, c("GB-13", "GB-14a", "GB-14b", "GB-15", "GB-16")) && identical(p2$gates[gate_id == "GB-14b", hard], FALSE), "Phase-2 gates GB-13 to GB-16")
# Phase 3 with synthetic outcomes (weight_dev built from tp6 - tp2 / tp6 - tp3 against the same-sex CON)
dout <- copy(design); ao <- copy(design$animals); set.seed(41)
ao[, `:=`(tp3 = tp2 + rnorm(.N, 1.5, 0.3))][, `:=`(tp4 = tp3 + 1, tp5 = tp3 + 2, tp6 = tp3 + rnorm(.N, 5, 1))]
ao[, dd := ifelse(Batch == "B6", tp6 - tp3, tp6 - tp2)]
ref <- ao[condition == "CON", .(m = mean(dd), s = sd(dd)), by = Sex]
ao[, weight_dev := (dd - ref$m[match(Sex, ref$Sex)]) / ref$s[match(Sex, ref$Sex)]][, dd := NULL]
for (k in setdiff(S33_COMPONENTS, "weight_dev")) set(ao, j = k, value = rnorm(nrow(ao)))
ao[AnimalNum == "B3S1N1", delta_cort := NA_real_]
ao[, n_components_present := rowSums(is.finite(as.matrix(.SD))), .SDcols = S33_COMPONENTS]
ao[, CombZ := rowMeans(as.matrix(.SD), na.rm = TRUE), .SDcols = S33_COMPONENTS]
dout$animals <- ao
out <- s33b_phase3(dout, p2, ctx)
check(identical(names(out$tables), S33_TABLES$B) && identical(names(out$audit), S33_AUDIT_TABLES$B), "15 declared tables and 2 audit tables")
check(identical(out$gates$gate_id, "GB-12") && isTRUE(out$gates$passed), "GB-12 holds on the synthetic structure")
check(isTRUE(s33_table_gates(list(B = out), "B")$passed), "GC-8: tier on every table, section-7 columns and declared levels")
lint <- s33_lint(c(out$tables, out$audit), character(), s33_lint_patterns(cfg$MMM_BEHAVIOR_CONFIG), exempt_columns = s33_lint_exempt_columns("B"))
check(nrow(lint) == 0L, paste("GC-9 lint clean:", paste(lint$where, lint$pattern, collapse = " | ")))
check(all(c("engine_messages", "profile_messages") %in% s33_lint_exempt_columns("B")), "the module's engine-message columns are exempt (S33B_LINT_EXEMPT_COLUMNS)")
check(nrow(s33_lint(list(), paste0("B: ", S33B_DEVIATIONS), s33_lint_patterns(cfg$MMM_BEHAVIOR_CONFIG))) == 0L, "the module's recorded deviations pass the README lint")
b12 <- out$tables$b12_arena_cohort_contrasts
check(all(b12$selection_statement == S33_FIXED_SENTENCES[["b_selection"]]), "E10 rows carry the plan's selection sentence verbatim")
allcols <- unlist(lapply(c(out$tables, out$audit), names))
check(!any(grepl("(?i)chance|probab|p_ref", allcols, perl = TRUE)) && !any(grepl("^(p|pval|p_value|statistic|t|t_value|F|F_value)$|^p_|_p$", allcols)) &&
        !any(c("outcome_group", "Group") %in% allcols), "no chance, probability, p or statistic column; no outcome group")
check(all(is.na(b12[measure == "crossing_rate", ci_low])) && all(is.na(b12[condition == "CON", ci_low])) && all(is.na(out$tables$b10_arena_cohort_means[condition == "CON", ci_low])),
      "no interval on the rate contrast (selection criterion) or on a CON row")
sis_oft <- b12[variant == "primary" & condition == "SIS" & measure == "oft_distance" & scale == "raw" & target %in% c("B2", "B6")]
check(all(sis_oft$rank_of_3_within_sex == 1L) && all(sis_oft$rate_rank_of_3_within_sex == 3L) && all(sis_oft$direction_vs_rate == "opposite") &&
        all(sis_oft$interval_method == "CR2_Welch_cc4_cage") && all(is.finite(sis_oft$range_low)), "B2 / B6 ranks, direction and CR2-Welch with the cage range")
# re-extraction: the Phase-2 rows of the final tables give the same hashed products
h2 <- s33_product_hashes(p2$products, "B"); h3 <- s33_product_hashes(s33b_outcome_free_extract(out), "B")
check(identical(h2, h3), "outcome-free products re-extracted from the final tables (sha256 of the writer serialization)")
check(all(out$tables$b05_associations$computed_in[seq_len(nrow(p2$products$b05_associations))] == "phase2"), "Phase-2 rows first")
b05 <- out$tables$b05_associations
check(all(b05[estimand_group %in% c("E3", "E5") & variant == "primary" & outcome %in% c("crossing_rate", S33B_CZ_OUTCOMES), lead]) &&
        !any(b05[!(estimand_group %in% c("E3", "E5") & variant == "primary"), lead]), "lead rows: E3 and E5 primary rows")
w_sd <- unique(b05[term == "w_c" & !grepl("tp2", estimand), sd_w])
check(length(w_sd) == 1L && near(w_sd, s33b_sd_w(s33b_covariates(design$animals[pop_sis_87 == TRUE]), "w_c")), "one SD_w (sis_87) for the rate and CombZ rows")
check(all(out$tables$b04_variance_shares[variant == "primary" & grepl("^variance_share", estimand), lead]), "E1 / E2 share rows lead")
check(all(grepl("no body-weight term", b05[outcome == "CombZ_noBW" & term %in% c("w_c", "w_within", "w_srcmean"), note])) &&
        all(b05[outcome == "CombZ_noBW", mech_coupling] == "none") && all(grepl("detection geometry", b05[outcome == "crossing_rate" & term == "w_c", note])),
      "coupling notes per outcome (CombZ_noBW first, no body-weight term); detection geometry on the rate rows")
# cross contract (plan section 13): B with partner fixtures for X1, X4, X5
b01 <- out$tables$b01_covariates_animal; b03 <- out$tables$b03_balance_cohort
check(all(c("AnimalNum", "Batch", "tp2", "src_cage", "cc4_cage", "lag_h_cc1") %in% names(b01)) && all(c("Batch", "lag_h_cc1") %in% names(b03)) && nrow(b03) == 12L,
      "b01 and b03 carry the cross-contract columns; b03 has 12 rows")
check(all(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", b01$dob)) && !any(vapply(b01, function(v) inherits(v, "Date"), TRUE)), "b01 dates as ISO text")
lagv <- D$lags[CC == "CC1"]
fakeA <- list(tables = list(), cross = list(lags = lagv[, .(Batch, lag_cc1_h = lag_h)], cc4_cage = b01[, .(AnimalNum, cc4_cage)], tp2 = b01[, .(AnimalNum, tp2, src_cage)]))
fakeD <- list(tables = list(d08_recording_start_lags = D$lags[, .(Batch, CC, lag_h)]))
xg <- s33_cross_gates(list(A = fakeA, B = out, D = fakeD), list(), c("A", "B", "D"))
check(all(xg[gate_id %in% c("X1", "X4", "X5"), passed]), "X1, X4 and X5 hold with consistent partners")
fakeA2 <- fakeA; fakeA2$cross$tp2 <- copy(fakeA$cross$tp2)[1, tp2 := tp2 + 1]
check(!isTRUE(s33_cross_gates(list(A = fakeA2, B = out), list(), c("A", "B"))[gate_id == "X4", passed]), "X4 fails on a changed tp2")
# X1 appendix identities
b15 <- out$tables$b15_appendix_board_offsets
for (sc in c("raw", "log")) for (pp in c("primary", "S20")) {
  o <- setNames(b15[scale == sc & variant == pp & estimand == "board_offset", estimate], b15[scale == sc & variant == pp & estimand == "board_offset", term])
  dl <- b15[scale == sc & variant == pp & term == "D_layout_CON", estimate]
  check(near(o[["o_sys.5"]], -sum(o[paste0("o_sys.", 1:4)]), 1e-8) && near(dl, 0.5 * o[["o_sys.2"]] + 0.5 * o[["o_sys.4"]] - o[["o_sys.3"]], 1e-8),
        paste("X1", sc, pp, ": o_5 = -sum(o_1..o_4); D_layout_CON = (o_4 - o_3 + o_2 - o_3) / 2"))
}
cnt <- design$animals[pop_sis_87 == TRUE, .N, by = .(Batch, cc1_board)]
Lb <- function(b) { o <- setNames(b15[scale == "raw" & variant == "primary" & estimand == "board_offset", estimate], b15[scale == "raw" & variant == "primary" & estimand == "board_offset", term])
  sum(vapply(paste0("sys.", 1:5), function(bd) sum(cnt[Batch == b & cc1_board == bd, N]), 0) * o[paste0("o_", paste0("sys.", 1:5))]) / sum(cnt[Batch == b, N]) }
check(near(b15[scale == "raw" & variant == "primary" & term == "D_layout_SIS", estimate], ((Lb("B2") - (Lb("B1") + Lb("B5")) / 2) + (Lb("B6") - (Lb("B3") + Lb("B4")) / 2)) / 2, 1e-8),
      "D_layout_SIS from the CC1 SIS board counts")
check(b15[variant == "S20", unique(n_windows)] == b15[variant == "primary" & is.finite(n_windows), unique(n_windows)] - 1L &&
        all(b15[estimand %in% c("board_offset", "layout_term"), n_params] == 28L), "S20 drops the S13 window; rank 28")
# same-board table (E8)
b08 <- out$tables$b08_cc1_board_table; cgm <- design$animals[tracked == TRUE & condition == "SIS", .(m = mean(crossing_rate)), by = .(Batch, cc1_board)]
cm1 <- function(b, bd) cgm[Batch == b & cc1_board == bd, m]
check(near(b08[row_type == "same_board_diff" & Batch == "B2" & board == "sys.2", estimate], cm1("B2", "sys.2") - (cm1("B1", "sys.2") + cm1("B5", "sys.2")) / 2) &&
        near(b08[row_type == "same_board_mean_diff" & Batch == "B6", estimate], mean(vapply(c("sys.1", "sys.4", "sys.5"), function(bd) cm1("B6", bd) - (cm1("B3", bd) + cm1("B4", bd)) / 2, 0))),
      "same-board differences B2 - mean(B1, B5) and B6 - mean(B3, B4)")
check(nrow(b08[row_type == "con_context"]) == 6L && all(is.na(b08$ci_low)) && nrow(b08[row_type == "sys3_vs_others"]) == 2L, "CON context rows, sys.3 rows, no interval")
check(all(out$tables$b09_con_board_rotation$changes_at_every_cage_change), "CON board at CC1-CC4 (b09)")
check(all(out$tables$b13_arena_profile_agreement$interval_method == "none") && nrow(out$tables$b13_arena_profile_agreement) == 40L, "E11: no interval; 40 rows")
check(nrow(out$tables$b14_arena_within_cohort_association[estimand == "within_cohort_slope_u" & variant == "primary"]) == 8L &&
        all(grepl("cage_resampling_range_cc4", out$tables$b14_arena_within_cohort_association[estimand == "pooled_within_cohort_r" & condition == "SIS" & variant == "primary", interval_method])),
      "E12: SIS slopes (KR) and r with the CC4-cell range; CON r only")
check(all(out$checkpoints$module == "B") && identical(names(out$checkpoints), c("module", "step", "checkpoint_key", "file", "reused")), "checkpoint rows in the contract form")
# E6 coupling table, E7 balance, E9 CR2 means, E10 contrasts from the E9 means, E5 listing
b07 <- out$tables$b07_weight_component_coupling; zz <- ao[pop_sis_93 == TRUE & is.finite(NOR), .(Batch, a = tp1, b = NOR)]
zz[, `:=`(a = a - mean(a), b = b - mean(b)), by = Batch]
check(nrow(b07) == 18L && near(b07[weight == "tp1" & component == "NOR", estimate], sum(zz$a * zz$b) / sqrt(sum(zz$a^2) * sum(zz$b^2))) &&
        b07[weight == "tp2" & component == "weight_dev", mech_coupling] == "shared_term", "E6: within-cohort r of tp1 / tp2 with the components (93 SIS)")
check(identical(b03[order(Batch, condition), n], design$animals[, .N, keyby = .(Batch, condition)]$N) &&
        near(b03[condition == "SIS" & Batch == "B1", n_tracked], 10), "E7: 12 cohort x condition rows with n and n tracked")
b10 <- out$tables$b10_arena_cohort_means
zb <- p2$state$units_frame[Batch == "B4" & condition == "SIS" & tracked == TRUE]
cr <- s33_cr2_mean(zb$epm_distance, zb$cc4_cage)
check(near(b10[variant == "primary" & condition == "SIS" & Batch == "B4" & measure == "epm_distance", c(estimate, ci_low, ci_high)], c(cr$estimate, cr$ci_low, cr$ci_high)) &&
        b10[variant == "primary" & condition == "SIS" & Batch == "B4" & measure == "crossing_rate", interval_method] == "CR2_Satterthwaite_cc1_cage",
      "E9: SIS cohort means with CR2 by CC4 cage (arena) and by CC1 cage (rate)")
mm10 <- b10[variant == "primary" & condition == "SIS" & measure == "nor_distance", setNames(estimate, Batch)]
check(near(b12[variant == "primary" & condition == "SIS" & measure == "nor_distance" & scale == "raw" & target == "B2", estimate], mm10[["B2"]] - (mm10[["B1"]] + mm10[["B5"]]) / 2),
      "E10: B2 contrast = B2 - mean(B1, B5) of the E9 means")
check(all(b10[variant == "primary" & measure == "arena_index4", sensitivity_id] == "S18") && all(b10[variant == "S16" & measure == "arena_index4", sensitivity_id] == "S16;S18") && all(b10[variant == "S19" & measure == "arena_speed_index4", sensitivity_id] == "S19;S18") &&
        all(is.na(b10[variant == "primary" & measure == "oft_distance", sensitivity_id])) && all(b12[variant == "S17", sensitivity_id] %in% c("S17", "S17;S18")),
      "sensitivity ids: S16, S17, S19 by variant; S18 for the index with HAB")
check(nrow(out$tables$b06_line_J_animals) == 9L && !any(c("outcome_group", "Group") %in% names(out$tables$b06_line_J_animals)), "E5 listing: the 9 C57BL/6J SIS, no outcome group")
# checkpoints in a temporary folder: a stored stream is reused
ckd <- file.path(tempdir(), paste0("s33b_ck_", Sys.getpid())); ctxc <- c(ctx[setdiff(names(ctx), "checkpoint_dir")], list(checkpoint_dir = ckd, checkpoint_log = new.env()))
lgc <- new.env(parent = emptyenv())
v1 <- s33b_ckpt(ctxc, "cage_test", function() s33b_stream_reps(cells, 5L, 33020301L, sums, pairs), list(seed = 33020301L), lgc)
v2 <- s33b_ckpt(ctxc, "cage_test", function() stop("recomputed"), list(seed = 33020301L), lgc)
check(identical(v1, v2) && identical(s33b_log_rows(lgc)$reused, c(FALSE, TRUE)) && length(ls(ctxc$checkpoint_log)) == 2L, "a stored checkpoint is reused and logged")
unlink(ckd, recursive = TRUE)
s33b_assoc_jobs <- full_assoc_jobs; s33b_share_jobs <- full_share_jobs
ok("tables, GC-8, lint, fixed sentence, re-extraction, cross columns, X1 and board identities")

cat(sprintf("\nPASS: stage 33 module B (%.0f s)\n", as.numeric(difftime(Sys.time(), T0, units = "secs"))))
