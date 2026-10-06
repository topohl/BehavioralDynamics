# Stage 33 module A (Functions/stage33_a_components.R): synthetic checks only.
#
#   1. contract: declared tables and audit tables, seeds, the outcome-free product, lint-exempt columns; every label passes
#      the lint
#   2. small estimators: six-point slopes (= lm, t(4) / within sex t(3)), CON t(n - 1), animal Welch, per-cohort CR2
#      (= s33_cr2_mean; a cell without spread gets no interval and raises no warning), exact OLS fits, column order
#   3. E2 share algebra: SCP (never cov()), shares of the six components sum to 100, shared + not shared = total, the
#      delta_cort parts add up, all six = within sex + between sex, slope contributions add to the six-point CombZ slope,
#      explicit six-point slopes = lm
#   4. E1b offsets: O = P + Q; O = 2/3 of the difference from the other two same-sex cohorts; resampled statistics leave
#      out the rate SIS-minus-CON quantities
#   5. animal frame: sum of c_k = CombZ; E1 (mean of present values) and E2 (missing as 0) conventions; r/q/e split exact,
#      e = 0 where the delta_cort map is exact
#   6. resampling: declared cell order, CON animals within the one CON cage, whole cages with duplicates, multiplicity 1 =
#      the observed means, one seed per stream (reproducible), envelope = union of the scheme intervals
#   7. the 36 within-sex CON re-pairings (identity first); 8. reference SDs; centring within cohort among a model's animals
#   9. E5 gains: the interval containing each A1, the weight_dev link (B6 from tp3), the 72-row gain table
#  10. gate helpers: check rows (NA, empty fail), gates without checks fail, target checks, the verbatim copy check
#  11. entry points on a synthetic 117-animal design with the declared structure: Phase-2 gates GA-4a, GA-5, GA-9a, GA-13
#      pass and each fails on a perturbation; Phase 3: declared tables, GC-8, lint (GC-9), Phase-3 gates GA-4b, GA-6, GA-7,
#      GA-8, GA-9b, GA-10, GA-T8 pass and each fails on a perturbation (GA-11 / GA-15 hold plan values and fail here),
#      no interval on rate SIS-minus-CON rows, no warning from an exact fit, re-extraction of the product, cross exports
#      (6 x x_RU rows = the CR2 rate rows module D builds), checkpoints with the contract columns
# Portable: no S: drive, no live data, no readxl; sources Functions/ only; temporary files only.

suppressPackageStartupMessages({ library(data.table); library(digest) })
fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")
must_error <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
near <- function(a, b, tol = 1e-10) length(a) == length(b) && all(is.finite(a) & is.finite(b)) && all(abs(a - b) <= tol)
for (f in c("Functions/rfid_canonical_inference.R", "Functions/stage32_inference.R", "Functions/stage32_run.R", "Functions/stage33_common.R",
            "Functions/stage33_run.R", "Functions/behavior_analysis_config.R", "Functions/stage33_a_components.R")) source(f)
fx <- file.path(tempdir(), paste0("s33a_", Sys.getpid())); dir.create(fx, recursive = TRUE, showWarnings = FALSE)
pats <- s33_lint_patterns(MMM_BEHAVIOR_CONFIG)

# ---------------------------------------------------------------- synthetic design (declared structure, invented values)
mk_design <- function(seed = 7L) {
  set.seed(seed)
  con_board <- c(B1 = "sys.3", B2 = "sys.4", B3 = "sys.3", B4 = "sys.3", B5 = "sys.3", B6 = "sys.2")
  sis_ids <- list(B1 = c("OQ770", "OQ771", "3", "4", sprintf("OQ%03d", 760:765)), B2 = sprintf("OR%03d", 101:116),
                  B3 = c("1545", "1546", "1547", sprintf("OR%03d", 301:313)), B4 = sprintf("OR%03d", 401:415),
                  B5 = c("13856", "13857", sprintf("OR%03d", 501:513)), B6 = c("692", sprintf("OR%03d", 601:614)))
  con_ids <- list(B1 = sprintf("OQ%03d", 780:783), B2 = c("OR126", "OR127", "OR128", "OR129"), B3 = sprintf("OR%03d", 320:323),
                  B4 = sprintf("OR%03d", 420:423), B5 = sprintf("OR%03d", 520:523), B6 = sprintf("OR%03d", 640:643))
  rows <- list()
  for (b in S33_COHORTS) {
    bo <- setdiff(paste0("sys.", 1:5), con_board[[b]])
    c1 <- if (b == "B1") c("sys.2", "sys.5", rep("sys.1", 4), rep("sys.4", 4)) else if (b %in% c("B2", "B3")) rep(bo, each = 4) else
      c(rep(bo[1], 3), rep(bo[2:4], each = 4))
    c4 <- if (b == "B1") c("sys.1", "sys.3", "sys.4", "sys.5", "sys.1", "sys.3", "sys.4", "sys.5", "sys.3", "sys.5") else sample(c1)
    rows[[length(rows) + 1L]] <- data.table(AnimalNum = sis_ids[[b]], Batch = b, condition = "SIS", tracked = TRUE, b1 = c1, b2 = sample(c1),
                                            b3 = sample(c1), b4 = c4)
    cb4 <- if (b == "B1") "sys.2" else con_board[[b]]
    rows[[length(rows) + 1L]] <- data.table(AnimalNum = con_ids[[b]], Batch = b, condition = "CON", tracked = TRUE, b1 = con_board[[b]],
                                            b2 = con_board[[b]], b3 = con_board[[b]], b4 = cb4)
  }
  u <- S33_UNTRACKED_B1
  rows[[length(rows) + 1L]] <- data.table(AnimalNum = u, Batch = "B1", condition = "SIS", tracked = FALSE,
    b1 = sub("^B1[|](sys[.][0-9])[|]CC1$", "\\1", S33A_UNTRACKED_CC1[u]), b2 = NA_character_, b3 = NA_character_,
    b4 = sub("^B1[|](sys[.][0-9])[|]CC4$", "\\1", S33A_UNTRACKED_CC4[u]))
  a <- rbindlist(rows)
  a[, `:=`(Sex = unname(S33_SEX_OF_COHORT[Batch]), cc1_cage = paste(Batch, b1, "CC1", sep = "|"), cc4_cage = paste(Batch, b4, "CC4", sep = "|"),
           cc2_cage = ifelse(tracked, paste(Batch, b2, "CC2", sep = "|"), NA_character_), cc3_cage = ifelse(tracked, paste(Batch, b3, "CC3", sep = "|"), NA_character_),
           cc1_board = b1)]
  a[, src_cage := ifelse(condition == "CON", paste0("SRC-", Batch, "-CON"), paste0("SRC-", Batch, "-", sample(1:5, .N, TRUE))), by = Batch]
  a[, `:=`(line = ifelse(AnimalNum %in% S33_J7, "C57BL/6J", "C57BL/6JRj"), J7 = AnimalNum %in% S33_J7, J9 = AnimalNum %in% S33_J9)]
  a[, line_J := as.integer(line == "C57BL/6J")]
  a[, cc1_date := as.Date(unname(S33_CC1_DATES[Batch]))]
  a[, age_cc1 := if (Batch[1] == "B1") 24 else ifelse(condition == "CON", 25, as.numeric(sample(23:28, .N, TRUE))), by = Batch]
  a[, `:=`(dob = cc1_date - age_cc1, tp1_date = as.Date(unname(S33_TP1_DATES[Batch])), tp2_date = cc1_date)]
  a[, tp1 := round(ifelse(Sex == "Male", 11, 10) + runif(.N, 0, 3), 1)][, tp2 := round(tp1 + runif(.N, 0.3, 1.5), 1)]
  eff <- c(B1 = 3, B2 = -6, B3 = 5, B4 = 1, B5 = 2, B6 = -5)
  a[, crossing_rate := ifelse(tracked, 30 + eff[Batch] + rnorm(.N, 0, 4), NA_real_)]
  a[, `:=`(x_RU = crossing_rate / S33_RU, shared_zone_use = ifelse(tracked, runif(.N, 0.1, 0.4), NA_real_),
           hw_a1 = paste(AnimalNum, "CC1", sep = "|") %in% S33_HW_A1)]
  mk <- c(B1 = 4, B2 = 5, B3 = 5, B4 = 4, B5 = 7, B6 = NA)
  a[, mark_date := cc1_date - mk[Batch]][Batch == "B5" & AnimalNum %in% c("13856", "13857"), mark_date := cc1_date - 11]
  a[, `:=`(rack = ifelse(Batch == "B6", "1.37/04", "1.37/03"), method = ifelse(Batch %in% c("B5", "B6"), "ketamine/xylazine overdose", "perfusion"))]
  a[, `:=`(untracked_b1 = AnimalNum %in% S33_UNTRACKED_B1, xmate_excluded = AnimalNum %in% S33_XMATE, pop_tracked_111 = tracked,
           pop_sis_87 = tracked & condition == "SIS", pop_focal_85 = tracked & condition == "SIS" & !AnimalNum %in% S33_SINGLE_CC1,
           pop_sis_93 = condition == "SIS", pop_canonical_117 = TRUE, pop_con_24 = condition == "CON")]
  # episodes (tracked x CC1-CC4) and cages
  ep <- rbindlist(lapply(1:4, function(k) a[tracked == TRUE, .(AnimalNum, Batch, Sex, condition, CC = paste0("CC", k),
    board = get(paste0("b", k)), r0 = crossing_rate)]))
  ep[, `:=`(CageEpisodeID = paste(Batch, board, CC, sep = "|"), crossing_rate = ifelse(CC == "CC1", r0, r0 + rnorm(.N, 0, 3)))]
  ep[, `:=`(shared_zone_use = runif(.N, 0.1, 0.4), n_in_cage = .N), by = CageEpisodeID]
  ep[, `:=`(n_tracked_mates = n_in_cage - 1L, hardware_flag = paste(AnimalNum, CC, sep = "|") %in% S33_HW_A1, obs_s = 43100,
            n_events = round(crossing_rate * 43100 / 3600))][, in_S13 := hardware_flag & CC %in% c("CC3", "CC4")]
  ep[, r0 := NULL]
  a[, n_in_cage := ep[CC == "CC1"]$n_in_cage[match(AnimalNum, ep[CC == "CC1"]$AnimalNum)]][, n_tracked_mates := n_in_cage - 1L]
  cages <- ep[, .(Sex = Sex[1], board = board[1], condition = paste(sort(unique(condition)), collapse = "/"), n_tracked = .N,
                  members = paste(sort(AnimalNum), collapse = ";")), by = .(CageEpisodeID, Batch, CC)]
  lags <- S33_PLANNED_LAGS[, .(Batch, CC, lag_h)]
  lags[, anchor_1830 := as.POSIXct(paste(as.Date(unname(S33_CC1_DATES[Batch])) + 4 * (as.integer(sub("CC", "", CC)) - 1L), "18:30:00"), tz = "UTC")]
  lags[, `:=`(rec_start = anchor_1830 - round(lag_h * 3600), board_first_delay_h = 0.01)]
  setkey(a, AnimalNum)
  list(animals = a[, c(S33_DESIGN_ANIMAL_COLUMNS, "mark_date", "rack", "method"), with = FALSE], episodes = ep, cages = cages,
       lags = lags[, .(Batch, CC, rec_start, anchor_1830, lag_h, board_first_delay_h)],
       sets = list(HW_A1 = S33_HW_A1, J7 = S33_J7, UNTRACKED_B1 = S33_UNTRACKED_B1), con_board = con_board)
}

#' Phase-2 inputs (the module's restricted reads) consistent with the design.
mk_inputs2 <- function(des) {
  a <- des$animals; lg <- des$lags[CC == "CC1"][match(S33_COHORTS, Batch)]
  a1 <- CJ(Batch = S33_COHORTS, CC = S33_CC)[, a1_date := as.Date(unname(S33_CC1_DATES[Batch])) + 4L * (as.integer(sub("CC", "", CC)) - 1L)]
  lc <- a[tracked == TRUE, .N, by = .(Batch, line)][, .(line_counts = paste(paste0(line, ":", N), collapse = "; ")), by = Batch]
  mkd <- a[, .(v = paste(sort(unique(as.numeric(cc1_date - mark_date))), collapse = ";")), by = Batch][v == "", v := NA_character_]
  tech <- data.table(Batch = S33_COHORTS, CON_system = unname(des$con_board[S33_COHORTS]), raw_recording_start = format(lg$rec_start, "%H:%M:%S"),
    rack = ifelse(S33_COHORTS == "B6", "1.37/04", "1.37/03"), terminal_method = ifelse(S33_COHORTS %in% c("B5", "B6"), "ketamine/xylazine overdose", "perfusion"),
    rfid_mark_to_cc1_days = mkd$v[match(S33_COHORTS, mkd$Batch)], line_counts = lc$line_counts[match(S33_COHORTS, lc$Batch)],
    DST_state_at_A1 = c("summer", "winter", "summer", "summer", "winter", "summer"), placement_median_range = "15:10 (15:00-15:20)",
    pre1830_h = c("2.75", "3.34", "6.26", "5.38", "4.94", "4.41"), pre1830_is_lower_bound = c("TRUE", rep("FALSE", 5)),
    elisa_files = paste0("elisa_", S33_COHORTS, "_basal.xlsx, elisa_", S33_COHORTS, "_response.xlsx"))
  el <- data.table(file = c(paste0("elisa_", S33_COHORTS, "_basal.xlsx"), paste0("elisa_", S33_COHORTS, "_response.xlsx")),
                   reader_run_date = rep(c("2023-01-24", "2023-01-25"), each = 6L))
  list(sucpref = a[, .(AnimalNum, SucPref = S33A_DATES$sucpref_text[match(Batch, S33A_DATES$Batch)])], a1_dates = a1, a4_tech = tech, a4_elisa = el)
}

#' Outcomes (Phase 3) with the declared CON reference, missingness and exact maps; the female delta_cort map carries a small
#' residual orthogonal to the map (as in the canonical table), so the fitted map slope stays the plan value.
mk_outcomes <- function(des, seed = 11L) {
  set.seed(seed)
  a <- copy(des$animals); n <- nrow(a); sis <- a$condition == "SIS"
  sh <- ifelse(sis, c(B1 = 0.3, B2 = -0.5, B3 = 0.6, B4 = 0.1, B5 = -0.2, B6 = -0.4)[a$Batch], 0)
  grp <- list(B1 = c(4, 6), B2 = c(14, 2), B3 = c(7, 9), B4 = c(9, 6), B5 = c(7, 8), B6 = c(12, 3))
  a[, outcome_group := "CON"]
  for (b in names(grp)) { s <- a[Batch == b & condition == "SIS" & tracked == TRUE, which = TRUE]; a[s, outcome_group := rep(c("RES", "SUS"), grp[[b]])] }
  a[untracked_b1 == TRUE, outcome_group := c("RES", "RES", "RES", "RES", "RES", "SUS")]
  # weights and dates (CC1-CC4 four days apart; tp6 5 d after CC4, 8 in B6)
  a[, `:=`(tp3 = round(tp2 + runif(.N, 0.5, 2.5), 1))][, tp4 := round(tp3 + runif(.N, 0.5, 2.5), 1)][, tp5 := round(tp4 + runif(.N, 0, 2), 1)]
  a[, tp6 := round(tp5 + runif(.N, -0.5, 1.5) + 0.3 * sh, 1)]
  a[, `:=`(tp3_date = cc1_date + 4, tp4_date = cc1_date + 8, tp5_date = cc1_date + 12)][, tp6_date := tp5_date + ifelse(Batch == "B6", 8, 5)]
  std <- function(raw, sgn = 1, target = c(Female = 1, Male = 1)) {
    z <- rep(NA_real_, n)
    for (s in c("Female", "Male")) { k <- a$Sex == s; cr <- raw[k & !sis & is.finite(raw)]; m <- mean(cr); sdp <- sqrt(mean((cr - m)^2))
      z[k] <- sgn * target[[s]] * (raw[k] - m) / sdp }
    z
  }
  raw <- list(nor_d2 = 0.25 + 0.06 * sh + rnorm(n, 0, 0.1), sucrose_preference_pct = 75 + 4 * sh + rnorm(n, 0, 6),
              adrenal_ratio_mg_per_100g = 12 - 1.5 * sh + rnorm(n, 0, 2), spleen_ratio_mg_per_100g = 400 + 30 * sh + rnorm(n, 0, 40))
  raw$adrenal_ratio_mg_per_100g[a$AnimalNum == "OR126"] <- NA_real_
  a[, `:=`(NOR = std(raw$nor_d2), sucrose_pref = std(raw$sucrose_preference_pct), adrenal_weight = std(raw$adrenal_ratio_mg_per_100g, -1),
           spleen_weight = std(raw$spleen_ratio_mg_per_100g, -1))]
  a[, weight_dev := std(ifelse(Batch == "B6", tp6 - tp3, tp6 - tp2))]
  # corticosterone: delta_cort = b_s (Delta - centre) with the plan's map slopes; CON SD 1 (male) and 0.8826 (female)
  tsd <- c(Male = 1, Female = 0.8826); u <- rnorm(n) + sh
  for (s in names(tsd)) { k <- a$Sex == s & !sis; u[k] <- (u[k] - mean(u[k])) / sqrt(mean((u[k] - mean(u[k]))^2)) }
  bs <- unname(S33A_DCORT_SLOPE[a$Sex]); delta <- 250 + tsd[a$Sex] / abs(bs) * u
  a[, cort_baseline := ifelse(Batch == "B3", runif(.N, 0.01, 1.5), runif(.N, 5, 40))][, cort_response := cort_baseline + delta]
  dc <- bs * (delta - 250)
  fs <- which(a$Sex == "Female" & sis); X <- cbind(1, delta[fs]); e <- runif(length(fs), -1, 1); e <- e - X %*% solve(crossprod(X), crossprod(X, e))
  dc[fs] <- dc[fs] + as.vector(e) * 0.015 / max(abs(e))
  a[, delta_cort := dc][AnimalNum == "OQ762", `:=`(delta_cort = NA_real_, cort_response = NA_real_)]
  zz <- as.matrix(a[, S33_COMPONENTS, with = FALSE])
  a[, `:=`(CombZ = rowMeans(zz, na.rm = TRUE), n_components_present = rowSums(is.finite(zz)))]
  mw <- a[, .(AnimalNum, nor_d2 = raw$nor_d2, sucrose_preference_pct = raw$sucrose_preference_pct, cort_delta_ng_ml = cort_response - cort_baseline,
              adrenal_ratio_mg_per_100g = raw$adrenal_ratio_mg_per_100g, spleen_ratio_mg_per_100g = raw$spleen_ratio_mg_per_100g,
              mw_nor = NOR, mw_sucrose_pref = sucrose_pref, mw_weight_dev = weight_dev, mw_delta_cort = delta_cort,
              mw_adrenal_weight = adrenal_weight, mw_spleen_weight = spleen_weight)]
  setkey(a, AnimalNum)
  out <- des; out$animals <- a
  list(design_out = out, mw = mw)
}

#' Phase-3 inputs: Movement_mean, registered rows (written to temporary files, read back as text like the module reader),
#' the Stage 29 parent row, a4 tables, master_wide raw measures and the female cycle phase.
mk_inputs3 <- function(dout, mw, dir) {
  a <- dout$animals
  set.seed(3L)
  mult <- data.table(HypothesisID = c("H01", "H02", "H03", "H11", "H13"), FamilyID = "F1", model_id = c("A_EXP|crossing_rate", "A_EXP|shared_zone_use",
    "A_CZ|crossing_rate", "F|SIS|movement_mean", "H13|Movement_mean"), estimand = c("SIS_minus_CON", "SIS_minus_CON", "CombZ_wb_slope", "Delta R2_LOCO",
    "Movement_mean:sex_c"), estimate = c("-0.6394", "0.01", "-1.1156", "-0.0035", "-0.307"), p_holm = c("0.84", "0.9", "0.53", "0.44", ""),
    p_raw = c("0.42", "0.5", "0.27", "0.22", "0.03"), statistic_type = c("KR t", "KR t", "KR t", "within-Batch permutation", "CR2 t"),
    caveat = c("text, with a comma", "", "", "", "secondary"))
  est <- data.table(hypothesis_id = c("H01", "", "H03", "", "H13", "H05"), model_id = c("A_EXP|crossing_rate", "A_EXP|crossing_rate", "A_CZ|crossing_rate",
    "A_CZ|crossing_rate", "H13|Movement_mean", "B_EXP|crossing_rate"), estimand = c("SIS_minus_CON", "SIS_minus_CON_x_sex", "CombZ_wb_slope",
    "CombZ_wb_slope_x_sex", "Movement_mean:sex_c", "Exposure_x_CC_joint"), estimate = c("-0.6394", "1.2", "-1.1156", "0.4", "-0.307", ""),
    se = c("3.02", "6.1", "0.99", "1.8", "0.114", ""), df = c("4.0", "4.0", "72.2", "70.1", "9.4", ""), ci_low = c("-9.0234", "-15.7", "-3.0908", "-3.2", "-0.565", ""),
    ci_high = c("7.7446", "18.1", "0.8596", "4.0", "-0.050", ""), p_raw = c("0.84", "0.86", "0.27", "0.83", "0.03", "0.5"),
    ddf_fallback = c("FALSE", "FALSE", "FALSE", "FALSE", "", ""), test = c("KR t", "KR t", "KR t", "KR t", "OLS; CR2 by CC1 CageEpisodeID", "KR F"))
  fm <- file.path(dir, "multiplicity.csv"); fe <- file.path(dir, "estimates.csv"); fwrite(mult, fm); fwrite(est, fe)
  rd <- function(p) fread(p, colClasses = "character", na.strings = NULL, encoding = "UTF-8", showProgress = FALSE)
  s29 <- data.table(population = "SIS_ONLY", predictor = "crossing_rate", estimand = c("slope_sexavg", "slope_DiD_F_minus_M"),
                    estimate = c("-0.0058", "0.01"), se = c("0.0135", "0.027"), df = c("79", "79"), ci_low = c("-0.0328", "-0.04"),
                    ci_high = c("0.0212", "0.06"), n = "87")
  # a4 means of the synthetic data (so that the a4 part of GA-11 holds) and a yardstick from an independent computation
  tr <- a[tracked == TRUE]; wide <- data.table(Batch = S33_COHORTS, Sex = unname(S33_SEX_OF_COHORT[S33_COHORTS]))
  for (k in c(S33_COMPONENTS, "CombZ")) {
    ms <- vapply(S33_COHORTS, function(b) mean(tr[Batch == b & condition == "SIS"][[k]], na.rm = TRUE), 0)
    mc <- vapply(S33_COHORTS, function(b) mean(tr[Batch == b & condition == "CON"][[k]], na.rm = TRUE), 0)
    wide[, (paste0("mean_SIS_", k)) := ms][, (paste0("mean_CON_", k)) := mc][, (paste0("SIS_minus_CON_", k)) := ms - mc] }
  cm <- tr[condition == "SIS", .(n = .N, mu = mean(CombZ)), by = .(Batch, cc1_cage)][n >= 3L][, dev := mu - mean(mu), by = Batch]
  yard <- data.table(measure = c("Movement_mean", "CombZ"), pooled_within_batch_SD_of_SIS_cage_means = c(NA_real_, sqrt(sum(cm$dev^2) / (nrow(cm) - 6L))))
  cyc <- a[, .(AnimalNum, cycle_phase = ifelse(Batch %in% c("B3", "B4"), sample(c("Follicular", "Luteal"), .N, TRUE), NA_character_))]
  list(movement = a[tracked == TRUE, .(AnimalNum, Movement_mean = x_RU * (1 + runif(.N, -0.002, 0.002)))], s32_multiplicity = rd(fm),
       s32_estimates = rd(fe), s29 = s29, a4_wide = wide, a4_yard = yard, mw = mw, cycle = cyc,
       paths = list(s32_multiplicity = fm, s32_estimates = fe),
       sha = list(s32_multiplicity = digest::digest(file = fm, algo = "sha256"), s32_estimates = digest::digest(file = fe, algo = "sha256")))
}

des <- mk_design(); inp2 <- mk_inputs2(des); oc <- mk_outcomes(des); dout <- oc$design_out; inp3 <- mk_inputs3(dout, oc$mw, fx)
ft <- s33a_frame(dout$animals, S33A_DCORT_SLOPE, inp3$movement)

cat("1. contract and labels\n")
check(identical(S33A_OUTCOME_FREE_PRODUCTS, "a12_cohort_descriptors_phase2"), "one declared outcome-free product")
check(all(unlist(S33A_SEED_NAMES) %in% names(S33_SEEDS)) && !anyDuplicated(S33_SEEDS[unlist(S33A_SEED_NAMES)]) &&
        identical(unname(S33_SEEDS[unlist(S33A_SEED_NAMES)]), c(33010101L, 33010102L, 33010103L, 33010201L, 33010202L, 33010203L)),
      "six streams with the declared seeds 33010101-03 (tracked_111) and 33010201-03 (canonical_117)")
check(all(S33A_LINT_EXEMPT_COLUMNS %in% s33_lint_exempt_columns("A")), "the core reads S33A_LINT_EXEMPT_COLUMNS")
labels <- unique(c(unlist(S33A_LAB), unlist(S33A_U), S33A_PROCEDURAL, S33A_GATE_NAMES, S33A_NO_SPREAD_NOTE, S33A_EXACT_FIT_NOTE, S33A_DEVIATIONS))
lab_hits <- c(unlist(lapply(c(pats$stage, pats$rfid_labels), function(p) labels[grepl(p, labels, ignore.case = TRUE, perl = TRUE)])),
              unlist(lapply(pats$journals, function(p) labels[grepl(p, labels, perl = TRUE)])))
check(!length(lab_hits), paste("labels pass the lint:", paste(unique(lab_hits), collapse = " | ")))
check(grepl("in B2, B5 and B6 that pre-SIS cage also supplied 2 SIS; interval anti-conservative or absent", S33A_LAB$con_phrase, fixed = TRUE) &&
        S33A_LAB$kr_common == "common residual variance across cohorts" && grepl("registered, null; per-cohort rows are not slices of a test", S33A_LAB$h01, fixed = TRUE),
      "CON phrase (17.1 item 9), KR label and H01 label as in the plan")
ok("tables, seeds, product, exempt columns; labels pass the stage, journal and RFID-label patterns")

cat("\n2. small estimators\n")
set.seed(5); x6 <- c(4, 5.5, 6, 3, 7, 5); y6 <- 0.3 * x6 + rnorm(6, 0, 0.2); sx6 <- unname(S33_SEX_OF_COHORT)
f6 <- s33a_scatter_slope(y6, x6); l6 <- lm(y6 ~ x6); ci6 <- confint(l6)["x6", ]
check(near(f6$estimate, unname(coef(l6)["x6"])) && f6$df == 4 && near(c(f6$ci_low, f6$ci_high), unname(ci6)), "six-point slope = lm, t(4)")
f3 <- s33a_scatter_slope(y6, x6, sx6); l3 <- lm(y6 ~ sx6 + x6)
check(near(f3$estimate, unname(coef(l3)["x6"])) && f3$df == 3 && near(c(f3$ci_low, f3$ci_high), unname(confint(l3)["x6", ])), "within-sex slope = lm(y ~ sex + x), t(3)")
check(is.na(s33a_scatter_slope(y6[1:2], x6[1:2])$estimate) && s33a_t_method(NA_real_) == "none" && s33a_t_method(4) == "t_4", "too few points: no slope; t labels")
yc <- c(0.2, -0.4, 0.9, 0.1); mt <- s33a_mean_t(yc)
check(near(mt$se, sd(yc) / 2) && mt$df == 3 && near(mt$ci_high - mt$estimate, qt(0.975, 3) * sd(yc) / 2), "CON mean with t(n - 1)")
check(is.na(s33a_mean_t(c(24, 24, 24, 24))$ci_low) && isTRUE(s33a_mean_t(c(24, 24, 24, 24))$no_spread), "a CON cell without spread: no interval")
ya <- c(1, 2, 4, 3, 5); yb <- c(0, 1, 1.5); w <- s33a_welch(ya, yb); va <- var(ya) / 5; vb <- var(yb) / 3
check(near(w$estimate, mean(ya) - mean(yb)) && near(w$se, sqrt(va + vb)) && near(w$df, (va + vb)^2 / (va^2 / 4 + vb^2 / 2)), "animal Welch interval")
check(is.na(s33a_welch(c(24, 24, 24), c(24, 24))$ci_low), "Welch: two cells without spread give no interval")
set.seed(6); yy <- rnorm(14); cl <- rep(c("c1", "c2", "c3", "c4"), c(4, 4, 3, 3))
cc <- s33a_cr2_cell(yy, cl); ref <- s33_cr2_mean(yy, cl)
check(near(cc$se, ref$se) && near(cc$df, ref$df) && near(cc$ci_low, ref$ci_low) && !cc$no_spread, "per-cohort CR2 cell = s33_cr2_mean")
nw <- 0L; cf <- withCallingHandlers(s33a_cr2_cell(rep(24, 10), rep(c("a", "b", "c", "d"), c(1, 1, 4, 4))), warning = function(w) { nw <<- nw + 1L; invokeRestart("muffleWarning") })
check(nw == 0L && isTRUE(cf$no_spread) && is.na(cf$ci_low) && cf$estimate == 24, "a cell without spread (B1 age): point value, no interval, no 'perfect fit' warning")
dx <- data.table(y = 2 + 3 * (1:8), x = 1:8, g = rep(1:4, 2)); nw <- 0L
ox <- withCallingHandlers(s33a_ols(dx, "x", "x", "g"), warning = function(w) { nw <<- nw + 1L; invokeRestart("muffleWarning") })
check(nw == 0L && isTRUE(ox$exact_fit) && near(ox$estimate, 3) && is.null(ox$cr2$g), "an exact OLS fit: point value, no CR2, no warning")
dx[, y := y + rnorm(8, 0, 0.5)]; ox2 <- s33a_ols(dx, "x", "x", "g")
check(!isTRUE(ox2$exact_fit) && is.finite(ox2$cr2$g$se) && !any(c("p_val", "p_Satt") %in% names(ox2$cr2$g)), "OLS CR2 contrast without p-value")
check(identical(names(s33a_finish(data.table(population = "p", estimand = "e", level = "cohort", estimate = 1, tier = "t"), c("population", "estimand"))),
                c("population", "estimand", "level", "estimate", "tier")), "column order: front, declared columns, tier last (estimand in front is not repeated)")
check(must_error(s33a_finish(setnames(data.table(1, 2), c("a", "a")))), "duplicated column names stop")
ok("slopes, t, Welch, CR2 cells, exact fits, column order")

cat("\n3. E2 share algebra\n")
un <- s33a_units(des$animals, "tracked_111"); sums <- s33a_cell_sums(un$animal, ft[tracked == TRUE], S33A_ARRAY_VARS); Ap <- s33a_means_array(sums)
sr <- s33a_share_rows(Ap); V <- sr$V[1]
xb <- Ap[1, , "SIS", "x_RU"]; czb <- Ap[1, , "SIS", "CombZ"]
check(near(V, sum((xb - mean(xb)) * (czb - mean(czb)))) && near(V / 5, cov(xb, czb)), "V = SCP(xbar, SIS CombZ); V / 5 = the diagnostic covariance")
tot <- sr[estimand == "share_total" & measure %in% S33_COMPONENTS]
check(near(sum(tot$scp), V) && near(sum(100 * tot$scp / V), 100) && near(sr[measure == "CombZ_total" & estimand == "share_total", scp], V),
      "the six total shares sum to 100 (sum of c_k = CombZ)")
w3 <- dcast(sr, measure ~ estimand, value.var = "scp")
check(near(w3$share_shared + w3$share_not_shared, w3$share_total), "shared + not shared = total for every measure")
check(near(sum(w3[measure %in% S33A_DCORT_PARTS, share_total]), w3[measure == "delta_cort", share_total]), "the delta_cort parts add up to its share")
sw <- s33a_share_rows(Ap, sex = TRUE); xw <- xb - ave(xb, sx6); cw <- czb - ave(czb, sx6)
check(near(sw$V[1], sum(xw * cw)) && near(sr$V[1] - sw$V[1], sum((tapply(xb, sx6, mean)[sx6] - mean(xb)) * (tapply(czb, sx6, mean)[sx6] - mean(czb)))),
      "all six = within sex + between sex (SCP decomposition)")
check(near(sum(tot$scp / tot$scp_xx), sr[measure == "CombZ_total" & estimand == "share_total", scp / scp_xx]) &&
        near(sr[measure == "CombZ_total" & estimand == "share_total", scp / scp_xx], unname(coef(lm(czb ~ xb))["xb"])),
      "slope contributions add to the six-point CombZ slope (= lm)")
for (k in c("NOR", "delta_cort")) { zs <- Ap[1, , "SIS", k]
  check(near(sr[measure == k & estimand == "share_total", scp_z / scp_xx], unname(coef(lm(zs ~ xb))["xb"])) &&
          near(sw[measure == k & estimand == "share_total", scp_z / scp_xx], unname(coef(lm(zs ~ sx6 + xb))["xb"])),
        paste("explicit six-point slope of the E1 means of", k, "(all six and within sex)")) }
ok("SCP, shares, shared / not shared, delta_cort parts, within / between sex, slopes")

cat("\n4. E1b offsets and resampled statistics\n")
pv <- s33a_stats(Ap); pv <- setNames(pv[1, ], colnames(pv))
for (k in c("NOR", "CombZ", "tp2", "x_RU")) for (b in S33_COHORTS) {
  O <- pv[[s33a_id("E1b", "-", "offset_total", b, "SIS", k)]]; P <- pv[[s33a_id("E1b", "-", "part_shared_con", b, "CON", k)]]
  Q <- pv[[s33a_id("E1b", "-", "part_not_shared", b, "SIS_minus_CON", k)]]; same <- names(S33_SEX_OF_COHORT)[S33_SEX_OF_COHORT == S33_SEX_OF_COHORT[[b]]]
  oth <- setdiff(same, b); m <- Ap[1, , "SIS", k]
  check(abs(O - P - Q) < 1e-12 && abs(O - 2 / 3 * (m[[b]] - mean(m[oth]))) < 1e-12, paste("O = P + Q and O = 2/3 of the difference from the other two:", b, k)) }
xc6 <- Ap[1, , "CON", "x_RU"]; zc6 <- Ap[1, , "CON", "NOR"]
check(near(pv[[s33a_id("E3d", "con_x_within_sex", "between_cohort_slope", "-", "CON", "NOR")]], unname(coef(lm(zc6 ~ sx6 + xc6))["xc6"])) &&
        near(pv[[s33a_id("E3d", "con_on_sis_x", "between_cohort_slope", "-", "CON", "NOR")]], unname(coef(lm(zc6 ~ xb))["xb"])) &&
        near(pv[[s33a_id("E3c", "all_six", "between_cohort_slope", "-", "SIS", "NOR")]], unname(coef(lm(Ap[1, , "SIS", "NOR"] ~ xb))["xb"])),
      "E3(c) and E3(d) six-point slopes from the means array = lm (CON within sex on the CON rate; CON on the SIS rate)")
pb <- colnames(s33a_stats(Ap, point = FALSE))
check(!any(grepl("sis_minus_con_diff[|][^|]+[|]SIS_minus_CON[|]x_RU$", pb)) && !any(grepl("^E1b[|].*[|]x_RU$", pb)) &&
        any(grepl("sis_minus_con_diff[|]B1[|]SIS_minus_CON[|]NOR$", pb)), "resampled statistics leave out every rate SIS-minus-CON quantity")
ok("offset split identity, 2/3 rule, no resampled rate SIS-minus-CON")

cat("\n5. animal frame\n")
cs <- rowSums(as.matrix(ft[, paste0("c_", S33_COMPONENTS), with = FALSE]))
check(max(abs(cs - ft$CombZ)) < 1e-12 && all(ft$n_present == ft$n_components_present), "sum of c_k = CombZ (117); n_present")
b1s <- ft[Batch == "B1" & condition == "SIS" & tracked == TRUE]
check(near(Ap[1, "B1", "SIS", "delta_cort"], mean(b1s$delta_cort, na.rm = TRUE)) && near(Ap[1, "B1", "SIS", "c_delta_cort"], sum(b1s$c_delta_cort) / nrow(b1s)) &&
        sum(is.finite(b1s$delta_cort)) == 9L && b1s[AnimalNum == "OQ762", c_delta_cort] == 0, "E1 = mean of present values (9 of 10); E2 counts the missing value as 0")
check(max(abs(ft$dcort_response_part + ft$dcort_baseline_part + ft$dcort_residual_part - ft$delta_cort), na.rm = TRUE) < 1e-12 &&
        all(is.na(ft[AnimalNum == "OQ762", c(S33A_DCORT_PARTS), with = FALSE])), "r + q + e = delta_cort; NA where delta_cort is missing")
check(max(abs(ft[Sex == "Male" | condition == "CON", dcort_residual_part]), na.rm = TRUE) < 1e-9 && max(abs(ft[Sex == "Female", dcort_residual_part])) > 1e-6,
      "e = 0 where the map is exact (males, CON); the near-exact female map leaves a residual")
mR <- mean(ft[Sex == "Male" & condition == "CON", cort_response]); i1 <- ft[Sex == "Male" & condition == "SIS" & is.finite(delta_cort), which = TRUE][1]
check(near(ft$dcort_response_part[i1], S33A_DCORT_SLOPE[["Male"]] * (ft$cort_response[i1] - mR)), "r = b_s (R - mR_s) with the same-sex CON mean")
ok("contributions, conventions, delta_cort parts")

cat("\n6. resampling\n")
check(identical(names(un$cc1$G), paste0(rep(S33_COHORTS, each = 2), "_", c("SIS", "CON"))) && all(un$cc4$G[grepl("_CON$", names(un$cc4$G))] == 4L) &&
        un$cc1$G[["B1_SIS"]] == 4L && un$animal$G[["B3_SIS"]] == 16L, "cells in the declared order (B1 SIS, B1 CON, ...); CON animals within the one CON cage")
m1 <- un$cc1$members[cell == "B4_SIS"]
check(identical(sort(unique(m1$unit_id), method = "radix"), unique(m1$unit_id[order(m1$unit)])), "cages sorted C-locale (radix)")
u117 <- s33a_units(des$animals, "canonical_117")
check(u117$cc1$G[["B1_SIS"]] == 4L && u117$animal$G[["B1_SIS"]] == 16L && u117$cc4$G[["B1_SIS"]] == 4L, "canonical_117: B1 SIS 16 in the planned cages")
U1 <- lapply(un$cc1$G, function(g) matrix(1, 1, g)); s1 <- s33a_cell_sums(un$cc1, ft[tracked == TRUE], S33A_ARRAY_VARS)
check(max(abs(s33a_means_array(s1, U1) - Ap), na.rm = TRUE) < 1e-12, "multiplicity 1 reproduces the observed means")
ix <- s33_resample_index(un$cc1$G, 3L, 33010102L); Um <- lapply(names(ix), function(k) s33_multiplicity(ix[[k]], un$cc1$G[[k]])); names(Um) <- names(ix)
Ab <- s33a_means_array(s1, Um); mm <- un$cc1$members[cell == "B2_SIS"]; drawn <- ix$B2_SIS[2, ]
yv <- unlist(lapply(drawn, function(u) ft[AnimalNum %in% mm[unit == u, AnimalNum], NOR]))
check(near(Ab[2, "B2", "SIS", "NOR"], mean(yv)), "whole cages drawn, duplicates kept as separate copies")
bs1 <- s33a_boot_stream(s1, ix, un$cc1$G, c(all_six = V)); bs2 <- s33a_boot_stream(s1, s33_resample_index(un$cc1$G, 3L, 33010102L), un$cc1$G, c(all_six = V))
check(isTRUE(all.equal(bs1, bs2)) && all(bs1$q025 <= bs1$q975, na.rm = TRUE) && all(bs1$n_valid <= 3L), "one seed per stream: the stream is reproducible")
bsx <- data.table(stat_id = rep(c("s1", "s2"), 3), scheme = rep(S33A_SCHEMES, each = 2), q025 = c(1, 5, 0.5, 6, 2, 4), q975 = c(3, 9, 2, 8, 4, 10),
                  n_valid = 10L, sd = 1)
ev <- s33a_envelope(bsx)
check(identical(ev$envelope_low, c(0.5, 4)) && identical(ev$envelope_high, c(4, 10)) && identical(ev$envelope_set_by, c("low:cc1;high:cc4", "low:cc4;high:cc4")),
      "envelope = union of the animal, CC1-cage and CC4-cage intervals")
ok("cell order, units, whole cages, multiplicities, reproducible streams, envelope")

cat("\n7. re-pairings, reference SDs, centring\n")
rp <- s33a_repairings(Ap)
check(nrow(rp) == 720L && data.table::uniqueN(rp$pairing_id) == 36L && identical(rp[identity == TRUE, unique(pairing_id)], 1L), "36 pairings x 10 measures x 2 versions, identity first")
check(all(vapply(strsplit(unique(rp$female_con_cages_from), ","), function(v) setequal(v, c("B3", "B4", "B6")), TRUE)) &&
        all(vapply(strsplit(unique(rp$male_con_cages_from), ","), function(v) setequal(v, c("B1", "B2", "B5")), TRUE)), "re-pairings within sex only")
idn <- rp[identity == TRUE & version == "all_six" & measure == "CombZ_total", share_shared]
check(near(idn, 100 * sr[measure == "CombZ_total" & estimand == "share_shared", scp] / V) &&
        rp[version == "all_six" & measure == "CombZ_total", min(share_shared) <= idn & idn <= max(share_shared)], "identity = the observed shared share, inside min-max")
rs <- s33a_reference_sds(ft, "CombZ")
cmn <- ft[tracked == TRUE & condition == "SIS", .(n = .N, mu = mean(CombZ)), by = .(Batch, cc1_cage)][n >= 3L][, dev := mu - mean(mu), by = Batch]
ccn <- ft[condition == "CON", .(mu = mean(CombZ)), by = .(Batch, Sex)][, dev := mu - mean(mu), by = Sex]
check(near(rs$cage_sd_reference_cc1, sqrt(sum(cmn$dev^2) / (nrow(cmn) - 6))) && rs$cage_sd_reference_cc1_n_cages == 22L && rs$cage_sd_reference_cc4_all117_n_cages == 24L &&
        near(rs$con_cage_mean_sd_within_sex, sqrt(sum(ccn$dev^2) / 4)), "reference SDs: pooled within-cohort cage-mean SD (22 cages, df 16; all 117: 24), CON spread df 4")
md <- s33a_model_data(ft[tracked == TRUE & condition == "SIS"], "delta_cort")
check(nrow(md) == 86L && all(abs(md[, mean(tp2_c), by = Batch]$V1) < 1e-12) && all(abs(md[, sum(x_w), by = Batch]$V1) < 1e-12) &&
        md[, near(mean(x), xbar_b[1]), by = Batch][, all(V1)], "tp2_c, x_w centred within cohort among the model's animals (86 with delta_cort)")
ok("re-pairings, reference SDs, centring")

cat("\n8. E5 gains and a08\n")
g <- s33a_gain_frame(ft, des$episodes)
check(nrow(g) == 444L && near(g[CC == "CC1", gain], g[CC == "CC1", tp3 - tp2]) && near(g[CC == "CC4", gain], g[CC == "CC4", tp6 - tp5]) &&
        all(g[CC == "CC1", start_tp] == "tp2") && all(g[CC %in% c("CC1", "CC2", "CC3"), days] == 4) && all(g[CC == "CC4", days] == ifelse(g[CC == "CC4", Batch] == "B6", 8, 5)),
      "each gain spans the inter-change interval that contains its A1 (4 d; CC4 5 d, 8 in B6)")
gt <- s33a_gain_table(g, ft)
check(nrow(gt) == 72L && all(gt[episode == "weight_dev_interval", identity_abs_error] < 1e-9) && all(gt[episode == "weight_dev_interval" & Batch == "B6", start_tp] == "tp3") &&
        !any(gt[Batch == "B6" & episode == "CC1", in_weight_dev]), "weight_dev link exact (B6 from tp3); 6 x 2 x 6 rows")
ok("gain frame and gain table")

cat("\n9. gate helpers\n")
check(isTRUE(s33a_chk("G", "x", c(TRUE, TRUE))$ok) && !isTRUE(s33a_chk("G", "x", c(TRUE, NA))$ok) && !isTRUE(s33a_chk("G", "x", logical())$ok), "check rows: NA and empty fail")
gg <- s33a_gates(s33a_chk("GA-6", "x", TRUE), c("GA-6", "GA-7"))
check(isTRUE(gg[gate_id == "GA-6", passed]) && !isTRUE(gg[gate_id == "GA-7", passed]) && gg[gate_id == "GA-7", detail] == "no check evaluated", "a gate without checks fails")
tg <- data.table(target_id = c("a", "b", "c"), value = c(1, 2, 3), tolerance = 0.01)
tc <- s33a_target_checks("GA-15", tg, c(a = 1.005, b = 2.5))
check(identical(tc$ok, c(TRUE, FALSE, FALSE)), "target checks: within tolerance, outside, missing")
rc <- s33a_registered_copy(inp3$s32_multiplicity, inp3$s32_estimates, "x", "y")
check(nrow(rc$a10) == 4L && nrow(rc$a11) == 5L && s33a_verbatim_check(rc$a10, inp3$paths$s32_multiplicity, "HypothesisID") &&
        s33a_verbatim_check(rc$a11, inp3$paths$s32_estimates, "model_id"), "verbatim copies of the registered rows (4 and 5)")
bad <- copy(rc$a11); bad[1, estimate := "-0.6395"]
check(!s33a_verbatim_check(bad, inp3$paths$s32_estimates, "model_id") && !s33a_verbatim_check(rc$a10, file.path(fx, "nofile.csv"), "HypothesisID"),
      "a changed cell or a missing source fails the verbatim check")
rv <- s33a_reference_values(rc$a11, inp3$s29)
check(near(rv$H01_RU[["estimate"]], -0.6394 / 6) && near(rv$S29_parent_RU[["estimate"]], -0.0348) && rv$H03[["ddf_fallback"]] == 0, "reference values and the RU scaling")
ok("check rows, gates, targets, verbatim copies, reference values")

cat("\n10. entry points (synthetic 117-animal design)\n")
ctx <- list(seeds = S33_SEEDS, B = c(nonparametric = 30L), canon = function(x) x, checkpoint_dir = file.path(fx, "checkpoints"),
            checkpoint_log = new.env(), run_mode = "test")
p2 <- s33a_phase2(des, ctx, inputs2 = inp2)
check(identical(names(p2$products), S33A_OUTCOME_FREE_PRODUCTS) && isTRUE(s33_assert_outcome_free(p2$products[[1]])), "Phase 2: the declared outcome-free product")
check(identical(p2$gates$gate_id, c("GA-4a", "GA-5", "GA-9a", "GA-13")) && all(p2$gates$passed), "Phase-2 gates GA-4a, GA-5, GA-9a, GA-13 pass on the declared structure")
g2 <- function(d2, i2 = inp2) s33a_gates(s33a_checks_p2(d2, i2), c("GA-4a", "GA-5", "GA-9a", "GA-13"))
dd <- copy(des); dd$animals <- dd$animals[AnimalNum != "OR640"]
check(!isTRUE(g2(dd)[gate_id == "GA-4a", passed]), "GA-4a fails without one CON animal")
dd <- copy(des); dd$cages <- copy(des$cages)[CageEpisodeID == "B2|sys.4|CC1", board := "sys.3"]
check(!isTRUE(g2(dd)[gate_id == "GA-5", passed]), "GA-5 fails on a moved CON board")
dd <- copy(des); dd$animals <- copy(des$animals)[AnimalNum == "OR301", tp2_date := tp2_date + 1]
check(!isTRUE(g2(dd)[gate_id == "GA-9a", passed]), "GA-9a fails on a tp2 date off the CC1 A1 date")
i2b <- copy(inp2); i2b$a4_tech <- copy(inp2$a4_tech)[Batch == "B6", rack := "1.37/03"]
check(!isTRUE(g2(des, i2b)[gate_id == "GA-13", passed]), "GA-13 fails when a descriptor differs from the a4 field")
nw <- character()
t0 <- Sys.time()
out <- withCallingHandlers(s33a_phase3(dout, p2, ctx, inputs3 = inp3), warning = function(w) { nw <<- c(nw, conditionMessage(w)); invokeRestart("muffleWarning") })
cat(sprintf("   (Phase 3 on the synthetic design: %.1f s, B = %d)\n", as.numeric(difftime(Sys.time(), t0, units = "secs")), ctx$B[["nonparametric"]]))
check(!any(grepl("perfect fit", nw)), paste("no 'essentially perfect fit' warning:", paste(unique(nw), collapse = " | ")))
check(identical(names(out$tables), S33_TABLES$A) && identical(names(out$audit), S33_AUDIT_TABLES$A), "the 13 declared tables and 2 audit tables")
check(isTRUE(s33_table_gates(list(A = out), "A")$passed), "GC-8: tier, section-7 columns and levels")
lint <- s33_lint(c(out$tables, out$audit), character(), pats, exempt_columns = s33_lint_exempt_columns("A"))
check(nrow(lint) == 0L, paste("GC-9 lint clean:", paste(utils::head(paste(lint$where, lint$text), 3), collapse = " | ")))
pcol <- unlist(lapply(setdiff(names(out$tables), c("a10_s32_multiplicity_rows", "a11_s32_estimate_rows")), function(nm) grep("(^p_|_p$|^p$|^statistic$)", names(out$tables[[nm]]), value = TRUE)))
check(!length(pcol) && !any(grepl("(?i)res_minus_sus|sus_minus_res", unlist(lapply(out$tables, names)), perl = TRUE)), "no p or statistic column outside a10/a11; no RES-vs-SUS column")
g3 <- out$gates
check(identical(g3$gate_id, c("GA-4b", "GA-6", "GA-7", "GA-8", "GA-9b", "GA-10", "GA-11", "GA-15", "GA-T8")) &&
        all(g3[gate_id %in% c("GA-4b", "GA-6", "GA-7", "GA-8", "GA-9b", "GA-10", "GA-T8"), passed]), paste("Phase-3 gates pass on a consistent design:",
      paste(g3[!(passed %in% TRUE), gate_id], collapse = ",")))
check(!isTRUE(g3[gate_id == "GA-11", passed]) && !isTRUE(g3[gate_id == "GA-15", passed]) && nrow(g3) == 9L, "GA-11 / GA-15 hold the plan values and fail on invented data")
ck <- out$audit$a_data_checks
check(ck[gate_id == "GA-11" & grepl("= a4", check), .N] == 2L && all(ck[gate_id == "GA-11" & grepl("= a4", check), ok]),
      "GA-11: cohort means and the yardstick = the a4 tables (the part that holds on any data)")
# outputs
tb <- out$tables
check(nrow(tb$a03_cohort_covariance_shares) == 390L && nrow(tb$a04_con_repairing_reference) == 20L && nrow(tb$a08_weight_episode_gains) == 72L &&
        nrow(tb$a13_bootstrap_scheme_comparison) == 97L && nrow(tb$a10_s32_multiplicity_rows) == 4L && nrow(tb$a11_s32_estimate_rows) == 5L,
      "row counts: a03 390, a04 20, a08 72, a13 97, a10 4, a11 5")
check(sum(tb$a03_cohort_covariance_shares$lead) == 18L && sum(tb$a05_component_slopes$lead) == 18L, "lead rows: 18 share rows; 18 component slopes (E3(a) unadjusted and tp2_c, E3(c))")
a03 <- tb$a03_cohort_covariance_shares
check(a03[version == "all_six" & estimand == "share_total" & measure %in% S33_COMPONENTS, near(sum(estimate), 100)] &&
        all(a03[, near(estimate[estimand == "share_total"], estimate[estimand == "share_shared"] + estimate[estimand == "share_not_shared"], 1e-9), by = .(version, measure)]$V1),
      "a03: shares sum to 100; shared + not shared = total in every version")
check(a03[version == "omit_B2_B6", any(denom_flag)] == (abs(a03[version == "omit_B2_B6", V][1]) < 0.25 * abs(a03[version == "all_six", V][1])), "the small-V flag")
a01 <- tb$a01_cohort_component_means
check(nrow(a01[arm == "SIS_minus_CON" & measure == "x_RU" & population != "registered_reference" & (is.finite(ci_low) | is.finite(ci_high))]) == 0L &&
        nrow(a01[population == "registered_reference"]) == 2L && nrow(tb$a02_cohort_offset_split[measure == "x_RU" & is.finite(ci_low)]) == 0L,
      "no interval on any rate SIS-minus-CON row or rate offset part; H01 rows copied (rate and RU)")
check(a01[population == "tracked_111" & Batch == "B1" & arm == "SIS" & measure == "age_cc1", interval_method == "none" & interval_note == S33A_NO_SPREAD_NOTE],
      "B1 SIS age (no spread): point value, no interval")
check(nrow(a01[population == "S8_drop_HW_A1"]) == 28L && all(a01[population == "S8_drop_HW_A1", Batch] == "B6") &&
        setequal(a01[population == "S8_drop_J7", unique(Batch)], c("B1", "B3", "B5")), "S8 rows: without 692 (B6) and without J7 (B1, B3, B5)")
check(all(tb$a02_cohort_offset_split$identity_abs_error < 1e-12), "a02: O = P + Q")
a05 <- tb$a05_component_slopes
check(all(a05[model == "E3a_OLS_FE" & variant == "primary" & measure == "CombZ", additivity_abs_error] < 1e-10), "OLS FE contribution slopes add to the CombZ slope")
check(identical(sort(a05[model == "E3a_KR" & variant == "primary" & measure == "NOR", n_params]), c(7L, 8L, 9L)) &&
        all(a05[model == "E3e_contextual_KR" & measure == "NOR", n_params] %in% 3:5), "declared ranks: E3(a) 7/8/9, E3(e) 3/4 (+ sex_c 4/5)")
check(all(a05[model == "E3c_six_point" & variant == "all_six", interval_basis] == "cohort_scatter_model") &&
        all(a05[estimand == "contextual_cohort_minus_within", level] == "cohort") && all(!is.na(a05[adjustment == "tp2_c" & model == "E3a_KR", mech_coupling])),
      "six-point rows on the cohort-scatter basis; contextual rows at cohort level; tp2_c rows carry mech_coupling")
cn <- ft[tracked == TRUE & condition == "CON" & is.finite(NOR)]; lcn <- lm(NOR ~ 0 + factor(Batch) + x_RU, cn)
e3b <- a05[model == "E3b_CON_OLS" & measure == "NOR" & adjustment == "none"]
check(nrow(e3b) == 1L && near(e3b$estimate, unname(coef(lcn)["x_RU"])) && e3b$interval_method == "t_17" && e3b$ci_flag == S33A_LAB$con_flag,
      "E3(b): CON within the one cage, OLS with t(17), flagged")
a09 <- tb$a09_weight_gain_models
check(identical(a09[model == "E5a" & variant == "primary" & arm == "SIS", n_params], c(27L, 28L)) && identical(a09[model == "E5b" & variant == "primary", unique(n_params)], c(28L, 29L)) &&
        identical(a09[model == "E5c" & variant == "primary", unique(n_params)], c(30L, 31L)) && identical(a09[model == "E5d", unique(n_params)], c(26L, 27L)),
      "E5 ranks 27/28, 28/29, 30/31, 26/27")
check(setequal(a09[model == "E5b", unique(estimand)], c("within_animal_across_episodes", "between_animals_within_cohort")) &&
        all(is.finite(a09[model == "E5a" & variant == "primary", cr2_animal_se])) && all(grepl("regression-to-the-mean", a09[adjustment == "ws_c", note])),
      "E5: Mundlak parts, CR2 by animal beside KR, ws_c label")
vocab <- "^(KR|Satterthwaite_fallback|CR2_Satterthwaite_[a-z0-9_]+|Welch|t_[0-9.]+|envelope_bootstrap|none)$"
im <- unique(unlist(lapply(tb, function(x) if ("interval_method" %in% names(x)) x$interval_method)))
check(all(grepl(vocab, im)), paste("interval methods from the declared vocabulary:", paste(im[!grepl(vocab, im)], collapse = ",")))
miss <- vapply(setdiff(names(tb), c("a10_s32_multiplicity_rows", "a11_s32_estimate_rows")), function(nm) { x <- tb[[nm]]
  if (!all(c("estimate", "units", "ci_low", "interval_basis") %in% names(x))) return(0L)
  x[(is.finite(estimate) & (is.na(units) | !nzchar(units))) | (is.finite(ci_low) & is.na(interval_basis)), .N] }, 0L)
check(all(miss == 0L), "every estimate carries units; every interval carries its basis")
ex <- s33a_outcome_free_extract(out)$a12_cohort_descriptors_phase2; pr <- p2$products$a12_cohort_descriptors_phase2
check(identical(names(ex), names(pr)) && isTRUE(all.equal(as.data.frame(ex), as.data.frame(pr), check.attributes = FALSE, tolerance = 0)),
      "the outcome-free product is re-extractable from a12")
a12 <- tb$a12_cohort_descriptors
check(nrow(a12) == 8L && all(a12[row_type == "cohort", n_RES_tracked + n_SUS_tracked] == c(10L, 16L, 16L, 15L, 15L, 15L)) &&
        all(grepl("not attributable", a12[row_type != "cohort", note])), "a12: six cohort rows, two r rows (labelled), n_RES / n_SUS counts only")
coh <- a12[row_type == "cohort"]; cw12 <- function(v) v - ave(v, coh$Sex)
check(near(a12[row_type == "r_with_xbar_all6", tp2_SIS_mean], cor(coh$tp2_SIS_mean, coh$xbar_SIS_RU)) &&
        near(a12[row_type == "r_with_xbar_within_sex", tp6_SIS_mean], cor(cw12(coh$tp6_SIS_mean), cw12(coh$xbar_SIS_RU))),
      "a12: r of a descriptor with xbar over the six cohorts (all six and within sex; Phase-3 tp6 included)")
cr <- out$cross
xr <- rbindlist(lapply(S33_COHORTS, function(b) { e <- des$episodes[CC == "CC1" & condition == "SIS" & Batch == b]; m <- s33_cr2_mean(e$crossing_rate, e$CageEpisodeID)
  data.table(Batch = b, mean = mean(e$crossing_rate), se = m$se, df = m$df, lo = m$ci_low, hi = m$ci_high) }))
xx <- merge(cr$x_RU_sis_cc1_rows, xr, by = "Batch")
check(nrow(xx) == 6L && near(6 * xx$estimate, xx$mean, 1e-9) && near(6 * xx$cr2_cc1_se, xx$se, 1e-9) && near(xx$cr2_cc1_df, xx$df, 1e-9) &&
        near(6 * xx$cr2_cc1_ci_low, xx$lo, 1e-9) && near(6 * xx$cr2_cc1_ci_high, xx$hi, 1e-9), "X2: 6 x A's x_RU rows = the CR2 rate rows by CC1 cage (as module D)")
check(identical(names(cr$lags), c("Batch", "lag_cc1_h")) && nrow(cr$lags) == 6L && length(cr$cage_sd_reference_cc1_cages) == 22L &&
        identical(names(cr$cc4_cage), c("AnimalNum", "cc4_cage")) && nrow(cr$cc4_cage) == 117L && identical(names(cr$tp2), c("AnimalNum", "tp2", "src_cage")),
      "cross exports: lags, 22 CC1 cages, cc4_cage (117), tp2 with source cages")
check(identical(names(out$checkpoints), c("module", "step", "checkpoint_key", "file", "reused")) && nrow(out$checkpoints) == 6L, "checkpoints: one per stream, contract columns")
ok("Phase 2 and 3 entry points, tables, gates, cross exports")

cat("\n11. Phase-3 gates fail on perturbations\n")
g3f <- function(d3, i3 = inp3) { a <- d3$animals; mp <- s33a_maps(a, data.table::as.data.table(i3$mw)[AnimalNum %in% a$AnimalNum])
  fr <- s33a_frame(a, mp$b_dcort, i3$movement); s33a_gates(s33a_checks_p3(fr, d3, inp2, i3, mp), c("GA-4b", "GA-6", "GA-7", "GA-8", "GA-9b", "GA-10")) }
base <- g3f(dout)
check(all(base$passed), "the consistent design passes GA-4b, GA-6, GA-7, GA-8, GA-9b, GA-10")
pert <- function(f) { d <- copy(dout); d$animals <- copy(dout$animals); f(d$animals); d }
check(!isTRUE(g3f(pert(function(a) a[AnimalNum == "OR101", outcome_group := "SUS"]))[gate_id == "GA-4b", passed]), "GA-4b fails on a changed outcome-group count")
check(!isTRUE(g3f(pert(function(a) a[AnimalNum == "OR102", CombZ := CombZ + 1e-6]))[gate_id == "GA-6", passed]), "GA-6 fails when CombZ is not the sum of its contributions")
check(!isTRUE(g3f(pert(function(a) a[AnimalNum == "OR103", crossing_rate := crossing_rate + 1e-6]))[gate_id == "GA-6", passed]), "GA-6 fails when a rate differs from the bundle")
check(!isTRUE(g3f(pert(function(a) a[AnimalNum == "OR104", spleen_weight := NA_real_]))[gate_id == "GA-7", passed]), "GA-7 fails on an undeclared missing value")
check(!isTRUE(g3f(pert(function(a) a[AnimalNum == "OR127", NOR := NOR + 0.5]))[gate_id == "GA-8", passed]), "GA-8 fails when the CON reference is not mean 0 / SD 1")
check(!isTRUE(g3f(pert(function(a) a[AnimalNum == "OR105", tp3_date := tp3_date + 1]))[gate_id == "GA-9b", passed]), "GA-9b fails on a tp3 date off the CC2 A1 date")
i3b <- copy(inp3); i3b$mw <- copy(inp3$mw)[AnimalNum == "OR106", nor_d2 := nor_d2 + 0.05]
check(!isTRUE(g3f(dout, i3b)[gate_id == "GA-10", passed]), "GA-10 fails when a component is not a linear map of its raw measure")
i3c <- copy(inp3); i3c$s29 <- copy(inp3$s29)[estimand == "slope_sexavg", estimate := "-0.0070"]
rv2 <- s33a_reference_values(s33a_registered_copy(i3c$s32_multiplicity, i3c$s32_estimates, "x", "y")$a11, i3c$s29)
rchk <- S33A_REFERENCE[, .(ok = abs(rv2[[ref]][[field]] - value) <= tolerance), by = .(ref, field)]
check(!all(rchk$ok) && all(rchk[ref != "S29_parent" & ref != "S29_parent_RU", ok]), "GA-T8 reference check fails on a reference value off the plan value")
ok("every Phase-3 gate fails on its perturbation")

unlink(fx, recursive = TRUE)
cat("\nPASS: stage 33 module A\n")
