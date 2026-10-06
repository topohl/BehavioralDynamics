# Stage 33 module C (Functions/stage33_c_cage.R): synthetic checks only.
#
#   1. terms: xbar_c, xbar_k, w, b, xbar_loo, CC1-day weight parts, w_Bk, the S2 replacement and the S4 offsets; the
#      GC-C05b sums; cage table, information shares, pi_k and the top-4 share against a base-R recomputation; variant
#      rows and the population table (c01)
#   2. reallocation reference: the vectorised draws = the data.table loop p[, xp := sample(x), by = Batch]
#   3. design sensitivity: minimum detectable r (noncentral t, Fisher) and expected half-width at the plan's df; residual
#      df; declared ranks; the design CR2 df do not depend on the response (outcome-free)
#   4. board offsets: sum to zero over sys.1-sys.5, SE = sqrt(L V L'), ranks 23 / 29, no g_SIS row
#   5. Phase 2 core on a synthetic design: every gate passes (GC-C03a-d, GC-C04a, GC-C05a-g, GC-C06, GC-C07a, GC-C12) with
#      expected files recomputed independently; products are outcome-free; each gate fails on its own perturbation
#   6. bridge: per-sex recombination identity (GC-C13b); the parent rows reproduce a table built with the Stage 29
#      recipe (OLS, vcovCR + coef_test; GC-C13a); both gates fail on a perturbation
#   7. Phase 3 core on non-singular synthetic CombZ (placebo first, then the real path): gates; GC-C08a exact at M1's
#      variance parameters and GC-C08c recorded; quoting rule (KR and CR2 equal standing; wider interval for 'compatible'
#      sentences); M1 sex interactions only as c06 point values (no female-minus-male row); strata mean (Welch); influence
#      summary; partial r = WLS partial correlation (Fisher z); ICC rows; declared tables, GC-8, lint, re-extractable
#      products, checkpoint rows, placebo isolation, cross export (xbar_loo = cage-mate mean)
#   8. singular synthetic CombZ: quoting rule (KR within, cage-aggregate t(G - 8) between, two-part Welch gamma, with the
#      Welch df formula); GC-C08b gated; S3 delta_peer per frozen SD = module E's S15 OLS m1 coefficient (X8); bootstrap
#      spread label; a FAILED fit gives FAILED rows without estimates and an empty bootstrap stream
#   9. gate fixtures: GC-C04b, GC-C08a (fail, not evaluated), GC-C09 structure checks (row count, status, failed fit)
# Portable: no S: drive, no live data, no readxl; sources Functions/ only.

suppressPackageStartupMessages({ library(data.table) })
fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")
must_error <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
for (f in c("Functions/rfid_canonical_inference.R", "Functions/stage32_inference.R", "Functions/stage32_run.R",
            "Functions/stage33_common.R", "Functions/stage33_run.R", "Functions/stage33_c_cage.R")) source(f)
T0 <- Sys.time()
fx <- file.path(tempdir(), paste0("s33c_", Sys.getpid())); dir.create(fx, showWarnings = FALSE)

# ---------------------------------------------------------------- the synthetic design
# Six cohorts (sexes as in Exp9). SIS CC1 cages: B1 4, 4 and a singleton (with one untracked mate, like OQ770); B2 4, 4, 4;
# B3-B6 4, 4, 3. The 3-animal cages of B4 and B6 also housed an excluded animal (XMATE); the S2 animal (lowest rate, the
# only CC1 hardware flag) sits in the B6 one. One CON cage per cohort on sys.3 (B2 sys.4, B6 sys.2). CC2-CC4: the SIS
# animals regrouped over the four other boards.
mk_design <- function(seed = 20261006L) {
  set.seed(seed)
  co <- S33_COHORTS; sx <- S33_SEX_OF_COHORT
  con_board <- c(B1 = "sys.3", B2 = "sys.4", B3 = "sys.3", B4 = "sys.3", B5 = "sys.3", B6 = "sys.2")
  sizes <- list(B1 = c(4L, 4L, 1L), B2 = c(4L, 4L, 4L), B3 = c(4L, 4L, 3L), B4 = c(4L, 4L, 3L), B5 = c(4L, 4L, 3L), B6 = c(4L, 4L, 3L))
  sis_boards <- list(B1 = c("sys.1", "sys.5", "sys.2"), B2 = c("sys.1", "sys.2", "sys.5"), B3 = c("sys.2", "sys.4", "sys.5"),
                     B4 = c("sys.1", "sys.5", "sys.4"), B5 = c("sys.1", "sys.2", "sys.5"), B6 = c("sys.3", "sys.5", "sys.4"))
  off_b <- c(B1 = 4, B2 = -6, B3 = 2, B4 = 0, B5 = 3, B6 = -4)
  theta <- c(sys.1 = 0.6, sys.2 = -0.4, sys.3 = 0.2, sys.4 = -0.9, sys.5 = 0.5)
  cg <- rbindlist(lapply(co, function(b) rbind(data.table(Batch = b, condition = "SIS", cc1_board = sis_boards[[b]], n = sizes[[b]]),
                                              data.table(Batch = b, condition = "CON", cc1_board = con_board[[b]], n = 4L))))
  a <- cg[rep(seq_len(.N), n)][, n := NULL]
  a[, `:=`(AnimalNum = sprintf("A%03d", .I), Sex = unname(sx[Batch]), cc1_cage = paste0(Batch, "|", cc1_board, "|CC1"), tracked = TRUE)]
  ce <- a[, .(ce = stats::rnorm(1L, 0, 2.5), co = stats::rnorm(1L, 0, 0.04)), by = cc1_cage]
  a[, crossing_rate := 20 + unname(off_b[Batch]) + unname(theta[cc1_board]) + ce$ce[match(cc1_cage, ce$cc1_cage)] + stats::rnorm(.N, 0, 4)]
  a[, shared_zone_use := 0.3 + ce$co[match(cc1_cage, ce$cc1_cage)] + stats::rnorm(.N, 0, 0.04)]
  a[, n_cage := .N, by = cc1_cage][n_cage == 1L, shared_zone_use := NA_real_]
  s2 <- a[cc1_cage == "B6|sys.4|CC1"]$AnimalNum[1]
  a[AnimalNum == s2, crossing_rate := min(a$crossing_rate) - 2]
  a[, `:=`(tp2 = as.numeric(sample(8:14, .N, replace = TRUE)), src_cage = paste0(Batch, "_src", (seq_len(.N) %% 5L) + 1L)), by = Batch]
  a[condition == "CON", src_cage := paste0(Batch, "_con")]
  a <- rbind(a, data.table(Batch = "B1", condition = "SIS", cc1_board = "sys.2", AnimalNum = "U001", Sex = "Male", cc1_cage = "B1|sys.2|CC1",
                           tracked = FALSE, crossing_rate = NA_real_, shared_zone_use = NA_real_, n_cage = NA_integer_, tp2 = 11, src_cage = "B1_src9"))
  a[, pop_sis_87 := tracked & condition == "SIS"]
  a[, n_sis_tr := sum(pop_sis_87), by = cc1_cage]
  a[, `:=`(pop_focal_85 = pop_sis_87 & n_sis_tr >= 2L, xmate_excluded = cc1_cage %in% c("B4|sys.4|CC1", "B6|sys.4|CC1"))]
  e1 <- a[tracked == TRUE, .(AnimalNum, Batch, Sex, condition, CC = "CC1", CageEpisodeID = cc1_cage, board = cc1_board, crossing_rate, shared_zone_use)]
  later <- rbindlist(lapply(2:4, function(k) {
    s <- a[tracked == TRUE & condition == "SIS"][, i := seq_len(.N), by = Batch]
    s[, board := setdiff(paste0("sys.", 1:5), con_board[[Batch[1]]])[((i + k) %% 4L) + 1L], by = Batch]
    x <- rbind(s[, .(AnimalNum, Batch, Sex, condition, board)], a[tracked == TRUE & condition == "CON", .(AnimalNum, Batch, Sex, condition, board = cc1_board)])
    x[, `:=`(CC = paste0("CC", k), CageEpisodeID = paste0(Batch, "|", board, "|CC", k))]
    x[, `:=`(crossing_rate = 20 + unname(off_b[Batch]) + unname(theta[board]) + stats::rnorm(.N, 0, 4), shared_zone_use = 0.3 + stats::rnorm(.N, 0, 0.04))]
    x }))
  ep <- rbind(e1, later, use.names = TRUE)
  ep[, n_tracked_mates := .N - 1L, by = CageEpisodeID][n_tracked_mates == 0L, shared_zone_use := NA_real_]
  ep[, `:=`(n_events = as.integer(round(crossing_rate * 10)), hardware_flag = CC == "CC1" & AnimalNum == s2, in_S13 = FALSE, obs_s = 43200)]
  mates <- sort(a[cc1_cage == "B6|sys.4|CC1" & AnimalNum != s2, AnimalNum])
  list(animals = a, episodes = ep, s2 = s2, mates = mates)
}
D <- mk_design()
S2V <- 0.123456789
mk_inputs <- function(D, expected = NULL) {
  ep1 <- D$episodes[CC == "CC1"]
  list(odisp = D$animals[tracked == TRUE, .(AnimalNum, occupancy_dispersion = seq(0.5, 0.9, length.out = .N))],
       s32 = ep1[, .(AnimalNum, CageEpisodeID, crossing_rate, shared_zone_use, n_tracked_mates, complete_primary = TRUE)],
       expected = expected,
       one_file = list(metrics = ep1[Batch == "B6", .(AnimalNum, crossing_rate, shared_zone_use, n_events, n_tracked_mates, hardware_flag)],
                       no_s2_animal = data.table(AnimalNum = D$mates, CC = "CC1", shared_zone_use = S2V, n_tracked_mates = 1L, dyadic_obs_s = 40000),
                       error = NA_character_))
}
FACTS <- list(
  counts = list(
    primary = list(n = 64L, cages = 17L, female = c(33L, 9L), male = c(31L, 8L), by_cohort = c(B1 = 8L, B2 = 12L, B3 = 11L, B4 = 11L, B5 = 11L, B6 = 11L),
                   cages_by_cohort = c(B1 = 2L, B2 = 3L, B3 = 3L, B4 = 3L, B5 = 3L, B6 = 3L), n4 = 13L, n3 = 4L),
    S1_excl_cages = list(n = 58L, cages = 15L, female = c(27L, 7L), male = c(31L, 8L)),
    S2_drop692 = list(n = 63L, cages = 17L, n2 = 1L),
    S8_4tracked = list(n = 52L, cages = 13L, female = c(24L, 6L), male = c(28L, 7L)),
    LOBO = list(B1 = c(56L, 15L), B2 = c(52L, 14L), B3 = c(53L, 14L), B4 = c(53L, 14L), B5 = c(53L, 14L), B6 = c(53L, 14L)),
    leave_one_cage_out = c(60L, 61L),
    parent = list(crossing_rate = c(65L, 18L), shared_zone_use = c(64L, 17L))),
  pi_tolerance = 1e-12, design_df_tolerance = 1e-8,
  s2_animal = D$s2, s2_mates = D$mates, s2_no692 = S2V,
  xmate_unanalysed = c("B4|sys.4|CC1" = "X901", "B6|sys.4|CC1" = "X902"),
  bw_range = c(6, 16), aggregate_df = 9L, dummy_df = 45L, dummy_rank = 19L, one_file_n = 15L)
CTX <- list(B = c(nonparametric = 60L, parametric = 9L, placebo = 3L), seeds = S33_SEEDS, run_mode = "test", checkpoint_dir = NULL)

cat("1. terms, cage table, information\n")
base <- s33c_base(D, mk_inputs(D)$odisp)
f85 <- base[focal == TRUE]
check(nrow(base) == 65L && nrow(f85) == 64L && identical(base$AnimalNum, sort(base$AnimalNum, method = "radix")) && !anyNA(base$sex_c),
      "base: tracked SIS, keyed by AnimalNum (C locale), sex_c coded")
check(all(base$sys_e1 + base$sys_e2 + base$sys_e3 + base$sys_e4 == ifelse(base$System == "sys.5", -4, 1 - (base$System == "sys.5"))) &&
        all(base[System == "sys.5", sys_e1] == -1) && all(base[System == "sys.2", sys_e2] == 1), "board terms sys_ek = 1[sys.k] - 1[sys.5]")
tr <- s33c_terms(f85, "crossing_rate")
man <- data.table::copy(f85)[, x := crossing_rate][, `:=`(xc = mean(x), nc = .N), by = CageEpisodeID][, xk := mean(x), by = Batch]
man[, `:=`(w = x - xc, b = xc - xk, loo = (nc * xc - x) / (nc - 1))][, mates := vapply(seq_len(.N), function(i) mean(x[CageEpisodeID == CageEpisodeID[i]][AnimalNum[CageEpisodeID == CageEpisodeID[i]] != AnimalNum[i]]), 0)]
man[, `:=`(bwc = mean(tp2)), by = CageEpisodeID][, bwk := mean(tp2), by = Batch]
check(isTRUE(all.equal(tr$w, man$w, tolerance = 1e-14)) && isTRUE(all.equal(tr$b, man$b, tolerance = 1e-14)) &&
        isTRUE(all.equal(tr$xbar_loo, man$loo, tolerance = 1e-14)) && max(abs(tr$xbar_loo - man$mates)) < 1e-10,
      "w = x - cage mean, b = cage mean - cohort mean, xbar_loo = (n xbar_c - x)/(n - 1) = mean of the cage-mates")
check(isTRUE(all.equal(tr$bw_w, man$tp2 - man$bwc)) && isTRUE(all.equal(tr$bw_b, man$bwc - man$bwk)), "CC1-day weight split like x")
check(all(abs(tr$w_B3 - tr$w * (tr$Batch == "B3")) == 0) && all(abs(rowSums(as.matrix(tr[, paste0("w_", S33_COHORTS), with = FALSE])) - tr$w) < 1e-12),
      "w_Bk = w x 1[Batch = Bk]; they sum to w")
check(all(abs(s33c_term_sums(tr)) < 1e-9) && !all(abs(s33c_term_sums(data.table::copy(tr)[1, w := w + 1e-6])) < 1e-9), "GC-C05b sums: 0 by construction; a perturbed w fails")
off <- c(sys.1 = 1, sys.2 = -2, sys.3 = 0.5, sys.4 = 0.25, sys.5 = 0.25)
t4 <- s33c_terms(f85, "crossing_rate", offsets = off)
check(isTRUE(all.equal(t4$w, tr$w)) && isTRUE(all.equal(t4$x, tr$x - unname(off[tr$System]))), "S4: x minus the board offset; w unchanged (cage = board)")
ts2 <- s33c_variant_terms(base, "S2_drop692", "shared_zone_use", off, stats::setNames(rep(S2V, 2), D$mates), FACTS)
check(!D$s2 %in% ts2$AnimalNum && all(ts2[AnimalNum %in% D$mates, x] == S2V) && all(ts2[AnimalNum %in% D$mates, w] == 0),
      "S2 (occupancy): the S2 animal dropped, its mates' occupancy replaced (w = 0)")
check(nrow(s33c_variant_rows(base, "S1_excl_cages", FACTS)) == 58L && nrow(s33c_variant_rows(base, "S8_4tracked", FACTS)) == 52L &&
        nrow(s33c_variant_rows(base, "S2_drop692", FACTS)) == 63L && must_error(s33c_variant_rows(base, "S99", FACTS)), "variant rows S1, S8, S2; unknown variant stops")
cgt <- s33c_cage_table(tr, "crossing_rate", "primary", off)
mc <- man[, .(n_c = .N, xbar_c = mean(x), b_c = mean(x) - xk[1], sdw = sd(x), mn = min(x), mx = max(x)), by = CageEpisodeID]
mc[, info := n_c * b_c^2 / sum(n_c * b_c^2)]
j <- merge(cgt, mc, by = "CageEpisodeID", suffixes = c("", ".m"))
check(nrow(j) == 17L && max(abs(j$b_c - j$b_c.m)) < 1e-12 && max(abs(j$info_share - j$info)) < 1e-12 && max(abs(j$sd_within_cage - j$sdw)) < 1e-12 &&
        abs(sum(j$info_share) - 1) < 1e-12 && max(abs(j$b_c_per_frozen_sd - j$b_c / S33_SD_RATE)) < 1e-14, "cage table: b_c, SD, info share n_c b_c^2 / sum, per frozen SD")
adj <- s33c_terms(f85, "crossing_rate", offsets = off)[, .(b = b[1]), by = CageEpisodeID]
check(max(abs(cgt$b_c_board_adj - adj$b[match(cgt$CageEpisodeID, adj$CageEpisodeID)])) < 1e-12, "board-adjusted b_c")
inf <- s33c_information(tr, "crossing_rate", "primary")
mi <- man[, .(SSw = sum(w^2), SSb = sum(b^2)), by = .(Sex, Batch)][, pi := SSb / sum(SSb), by = Sex]
check(max(abs(inf$SS_within - mi$SSw[match(inf$Batch, mi$Batch)])) < 1e-10 && max(abs(inf$pi_between_within_sex - mi$pi[match(inf$Batch, mi$Batch)])) < 1e-14 &&
        all(abs(inf[, sum(pi_between_within_sex), by = Sex]$V1 - 1) < 1e-12) && abs(sum(inf$share_between_all) - 1) < 1e-12,
      "information: SS within / between by cohort, pi_k within sex (sum 1), shares")
t4c <- s33c_top4(cgt)
check(isTRUE(all.equal(t4c$share, sum(sort(cgt$info_share, decreasing = TRUE)[1:4]))) && length(strsplit(t4c$cages, ";")[[1]]) == 4L, "top-4 cage information share")
pop <- s33c_population(base, list(primary = tr, S1_excl_cages = s33c_terms(s33c_variant_rows(base, "S1_excl_cages", FACTS), "crossing_rate"),
                                  S2_drop692 = s33c_terms(s33c_variant_rows(base, "S2_drop692", FACTS), "crossing_rate"),
                                  S8_4tracked = s33c_terms(s33c_variant_rows(base, "S8_4tracked", FACTS), "crossing_rate")))
check(pop[variant == "primary" & Batch == "all", n_animals] == 64L && pop[variant == "parent_crossing_87" & Batch == "all", n_cages] == 18L &&
        pop[variant == "S2_drop692" & Batch == "all", n_cages_n2] == 1L && pop[variant == "LOBO_-B2" & Batch == "all", n_animals] == 52L &&
        pop[variant == "S1_excl_cages" & Batch == "all", dropped_cages] == "B4|sys.4|CC1;B6|sys.4|CC1", "population table (c01)")
ok("terms, sums, cage table, information, pi_k, top-4 share, populations")

cat("\n2. reallocation reference\n")
loop_draws <- function(t, B, seed) {
  p <- data.table::copy(t[, .(AnimalNum, Batch, CageEpisodeID, x)]); data.table::setkeyv(p, "AnimalNum")
  old <- RNGkind(); on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(seed)
  vapply(seq_len(B), function(r) { p[, xp := sample(x), by = Batch]
    cm <- p[, .(xc = mean(xp), n = .N, Batch = Batch[1]), by = CageEpisodeID][, xk := sum(n * xc) / sum(n), by = Batch]
    stats::sd(cm$xc - cm$xk) }, 0)
}
vd <- s33c_reallocation_draws(tr$x, tr$Batch, tr$CageEpisodeID, 60L, S33_SEEDS[["C_realloc"]])
ld <- loop_draws(tr, 60L, S33_SEEDS[["C_realloc"]])
check(max(abs(vd - ld)) < 1e-12, "vectorised reallocation = p[, xp := sample(x), by = Batch] on the AnimalNum-keyed frame (same RNG use)")
check(identical(vd, s33c_reallocation_draws(tr$x, tr$Batch, tr$CageEpisodeID, 60L, S33_SEEDS[["C_realloc"]])) &&
        must_error(s33c_reallocation_draws(1:3, c("B1", "B2", "B2"), c("a", "b", "b"), 5L, 1L)), "reproducible; a one-animal cohort stops")
ok("reallocation draws")

cat("\n3. design sensitivity, ranks, design CR2 df\n")
check(abs(s33c_mde_t(14) - 0.627) < 5e-4 && abs(s33c_mde_fisher(14) - 0.651) < 5e-4 && abs(s33c_mde_t(61) - 0.342) < 5e-4 &&
        abs(s33c_mde_fisher(61) - 0.347) < 5e-4, "minimum detectable r at df 14 (0.627 / 0.651) and 61 (0.342 / 0.347)")
check(isTRUE(all.equal(s33c_halfwidth_fisher(14), tanh(stats::qnorm(0.975) / sqrt(13)))), "expected half-width tanh(1.96 / sqrt(df - 1))")
tv_rate <- lapply(stats::setNames(S33C_VARIANTS, S33C_VARIANTS), function(v) s33c_variant_terms(base, v, "crossing_rate", off, NULL, FACTS))
c13 <- s33c_design_sensitivity(tv_rate)
check(c13[design_row == "cage primary G17", residual_df] == 9L && c13[design_row == "within primary N64", residual_df] == 45L &&
        c13[variant == "S8_4tracked", residual_df] == 5L && c13[variant == "S10_board_terms", residual_df] == 5L && all(c13$level %in% S33_LEVELS) &&
        all(c("level", "units", "lead", "interval_basis", "metric_label") %in% names(c13)) && all(c13$frame == "design property in a two-sided 5% test frame; no test is run"),
      "c13: residual df of the cage-aggregate and within designs; section-7 columns; the plan's label")
rk <- s33c_declared_ranks(base, tv_rate, FACTS)
check(all(rk$n_cols == rk$expected_rank) && all(rk$rank == rk$expected_rank) && rk[model == "cage_aggregate", residual_df] == 9L &&
        nrow(rk[grepl("^leave_one_cage_out", variant)]) == 17L && nrow(rk[grepl("^LOBO", variant)]) == 6L, "declared ranks of every design (incl. LOBO and leave-one-cage-out)")
d1 <- s33c_design_cr2_df(tv_rate$primary)
alt <- data.table::copy(tv_rate$primary); set.seed(3); alt[, tp2 := stats::rnorm(.N)]
check(max(abs(d1 - s33c_design_cr2_df(alt))) < 1e-8 && all(d1 > 0), "design CR2 df depend on the design and the CC1-cage clusters only (any response)")
ok("design sensitivity and ranks")

cat("\n4. board offsets\n")
bo <- s33c_board_offsets(D$episodes)
check(nrow(bo$rows) == 20L && all(abs(bo$rows[, sum(estimate), by = .(metric_id, rows_used)]$V1) < 1e-12) && !any(grepl("g_SIS", bo$rows$System)),
      "offsets sum to 0 over sys.1-sys.5; no g_SIS row")
check(identical(bo$fits[, expected_rank], c(23L, 29L, 23L, 29L)) && all(bo$fits$n_fixed_cols == bo$fits$expected_rank) &&
        all(bo$rows$interval_basis == "none") && all(bo$rows$level == "cage_within_cohort"), "ranks 23 (CC2-CC4) and 29 (all rows); no interval")
dd <- data.table::copy(D$episodes)[CC != "CC1", .(AnimalNum, Batch, CC, CageEpisodeID, System = board, condition, y = crossing_rate)]
dd[, `:=`(Session = paste(Batch, CC, sep = ":"), g_SIS = data.table::fifelse(condition == "CON", -0.5, 0.5))][, condition := NULL]
s33c_sys_cols(dd)
fm <- s33_kr_fit(S33C_FORMULAS[["offsets"]], dd, "t", 23L)
V <- as.matrix(stats::vcov(fm$fit))[paste0("sys_e", 1:4), paste0("sys_e", 1:4)]
check(abs(bo$rows[metric_id == "crossing_rate" & rows_used == "CC2_4" & System == "sys.5", se] - sqrt(sum(V))) < 1e-10 &&
        abs(bo$rows[metric_id == "crossing_rate" & rows_used == "CC2_4" & System == "sys.5", estimate] + sum(lme4::fixef(fm$fit)[paste0("sys_e", 1:4)])) < 1e-10,
      "sys.5 offset = -sum(sys_e1..4) with SE sqrt(1' V 1)")
ok("board offsets")

cat("\n5. Phase 2 core: gates pass, products outcome-free, each gate fails on its perturbation\n")
indep_cage_terms <- function(D, metric, variant, offsets24) {
  a <- D$animals[pop_focal_85 == TRUE]
  od <- mk_inputs(D)$odisp
  if (variant == "S2_drop692") a <- a[AnimalNum != D$s2]
  a[, x := get(metric)]
  if (variant == "S2_drop692" && metric == "shared_zone_use") a[AnimalNum %in% D$mates, x := S2V]
  a[, od := od$occupancy_dispersion[match(AnimalNum, od$AnimalNum)]]
  a[, xk := mean(x), by = Batch][, bwk := mean(tp2), by = Batch]
  cg <- a[, .(Sex = Sex[1], Batch = Batch[1], System = cc1_board[1], n_c = .N, xbar_c = mean(x), xbar_k = xk[1], b_c = mean(x) - xk[1], sd_within = stats::sd(x),
              min_x = min(x), max_x = max(x), bw_cage_mean = mean(tp2), bw_b = mean(tp2) - bwk[1], occdisp_cage_mean = mean(od)), by = .(CageEpisodeID = cc1_cage)]
  cg[, `:=`(board_offset_cc2_4 = unname(offsets24[System]), b_c_per_frozen_sd = b_c / S33C_SD[[metric]], info_share = n_c * b_c^2 / sum(n_c * b_c^2),
            variant = variant, metric = metric)]
  cg[]
}
indep_information <- function(D, metric) {
  a <- D$animals[pop_focal_85 == TRUE][, x := get(metric)]
  a[, `:=`(xc = mean(x)), by = cc1_cage][, xk := mean(x), by = Batch][, `:=`(w = x - xc, b = xc - xk)]
  s <- a[, .(n = .N, n_cages = data.table::uniqueN(cc1_cage), SSw = sum(w^2), SSb = sum(b^2)), by = .(Sex, Batch)]
  s[, `:=`(share_within_all = SSw / sum(SSw), share_between_all = SSb / sum(SSb))]
  s[, `:=`(pi_matched_within_sex = SSb / sum(SSb), between_share_of_within_cohort_ss_sex = sum(SSb) / (sum(SSw) + sum(SSb))), by = Sex]
  cbind(data.table(metric = metric, variant = "primary"), s)
}
pi_of <- function(D, metric) { z <- indep_information(D, metric); stats::setNames(z$pi_matched_within_sex, z$Batch) }
ddf_of <- function(D, metric) { t <- s33c_terms(D$animals[pop_focal_85 == TRUE, .(AnimalNum, Batch, Sex, CageEpisodeID = cc1_cage, System = cc1_board,
                                                                                  crossing_rate, shared_zone_use, tp2, sex_c = ifelse(Sex == "Female", 0.5, -0.5))], metric)
  set.seed(9); t[, tp2 := stats::rnorm(.N)]; s33c_design_cr2_df(t) }
facts <- FACTS
facts$pi <- list(crossing_rate = pi_of(D, "crossing_rate"), shared_zone_use = pi_of(D, "shared_zone_use"))
facts$design_cr2_df <- list(crossing_rate = ddf_of(D, "crossing_rate"), shared_zone_use = ddf_of(D, "shared_zone_use"))
facts$exposure_icc <- c(crossing_rate = 0.5, shared_zone_use = 0.5)
off24 <- bo$offsets
mk_expected <- function(D, facts, off24) {
  p <- data.table::copy(D$animals[pop_focal_85 == TRUE])
  ex_ct <- rbind(indep_cage_terms(D, "crossing_rate", "primary", off24$crossing_rate$CC2_4), indep_cage_terms(D, "shared_zone_use", "primary", off24$shared_zone_use$CC2_4),
                 indep_cage_terms(D, "shared_zone_use", "S2_drop692", off24$shared_zone_use$CC2_4))
  realloc_t <- s33c_terms(base[focal == TRUE], "crossing_rate")
  ld <- loop_draws(realloc_t, CTX$B[["nonparametric"]], S33_SEEDS[["C_realloc"]]); q <- stats::quantile(ld, c(0.025, 0.975), type = 7, names = FALSE)
  list(bw = p[, .(AnimalNum, Sex, Batch, CageEpisodeID = cc1_cage, bw_cc1 = tp2)], cage_terms = ex_ct,
       information = rbind(indep_information(D, "crossing_rate"), indep_information(D, "shared_zone_use")),
       offsets = bo$rows[, .(metric = metric_id, rows = rows_used, n_rows, rank = n_params, singular, System, offset = estimate, se)],
       design_sensitivity = c13[, .(level = design_row, n = n_units, df = residual_df, mde_noncentral_t = round(min_detectable_r_noncentral_t, 3),
                                    mde_fisher = round(min_detectable_r_fisher, 3))],
       s2 = data.table(AnimalNum = D$mates, CC = "CC1", shared_zone_use = S2V, n_tracked_mates = 1L, dyadic_obs_s = 40000),
       reallocation = data.table(metric = "crossing_rate", observed_sd_b_c = stats::sd(unique(realloc_t[, .(CageEpisodeID, b)])$b), B = CTX$B[["nonparametric"]],
                                 seed = S33_SEEDS[["C_realloc"]], ref_median = stats::median(ld), ref_q025 = q[1], ref_q975 = q[2]))
}
EXP <- mk_expected(D, facts, off24)
p2a <- s33c_phase2_core(D, mk_inputs(D, EXP), CTX, facts)
facts$exposure_icc <- vapply(S33C_METRICS, function(m) p2a$products$c10_variance[metric_id == m & estimand == "exposure_icc_cc1_cage", estimate], 0)
p2 <- s33c_phase2_core(D, mk_inputs(D, EXP), CTX, facts)
g2 <- p2$gates
want2 <- c("GC-C06", "GC-C03a", "GC-C03b", "GC-C03c", "GC-C03d", "GC-C04a", "GC-C12", paste0("GC-C05", letters[1:7]), "GC-C07a")
check(setequal(g2$gate_id, want2) && all(g2$passed) && all(g2$evaluated) && identical(g2[gate_id == "GC-C05g", hard], FALSE),
      paste("every Phase-2 gate passes on the synthetic design:", paste(g2[!(passed %in% TRUE), gate_id], collapse = ",")))
check(identical(names(p2$products), S33C_OUTCOME_FREE_PRODUCTS) && isTRUE(all(vapply(names(p2$products), function(nm) s33_assert_outcome_free(p2$products[[nm]], nm), TRUE))),
      "products: names = S33C_OUTCOME_FREE_PRODUCTS; outcome-free")
check(all(vapply(S33C_METRICS, function(m) max(abs(p2$state$pi[[m]][S33_COHORTS] - facts$pi[[m]][S33_COHORTS])) < 1e-12, TRUE)),
      "pi_k of the module = independent pi_k (both metrics)")
check(p2a$gates[gate_id == "GC-C05g", passed] %in% FALSE && isTRUE(s33_stop_on_gates(p2a$gates[gate_id == "GC-C05g"])), "GC-C05g (exposure ICCs) is recorded: a mismatch never stops")
bad_facts <- facts
bad_facts$counts$primary$n <- 65L; bad_facts$counts$S8_4tracked$n <- 51L; bad_facts$counts$LOBO$B2 <- c(51L, 14L)
bad_facts$counts$parent$shared_zone_use <- c(63L, 17L); bad_facts$exposure_icc <- facts$exposure_icc + 0.01
bad_facts$design_cr2_df$crossing_rate[1] <- bad_facts$design_cr2_df$crossing_rate[1] + 1
bad_exp <- EXP
bad_exp$bw <- data.table::copy(EXP$bw)[1, bw_cc1 := bw_cc1 + 1]
bad_exp$cage_terms <- data.table::copy(EXP$cage_terms)[1, b_c := b_c + 1e-7]
bad_exp$information <- data.table::copy(EXP$information)[2, SSw := SSw * (1 + 1e-6)]
bad_exp$offsets <- data.table::copy(EXP$offsets)[3, offset := offset + 1e-7]
bad_exp$design_sensitivity <- data.table::copy(EXP$design_sensitivity)[1, df := df + 1L]
bad_exp$reallocation <- data.table::copy(EXP$reallocation)[, ref_median := ref_median + 1e-6]
bad_inp <- mk_inputs(D, bad_exp)
bad_inp$s32 <- data.table::copy(bad_inp$s32)[5, crossing_rate := crossing_rate + 1e-6]
bad_inp$one_file <- list(metrics = NULL, no_s2_animal = NULL, error = "synthetic one-file failure")
gb <- s33c_phase2_core(D, bad_inp, CTX, bad_facts)$gates
check(setequal(gb$gate_id, want2) && all(gb[gate_id != "GC-C05b", passed] %in% FALSE) && isTRUE(gb[gate_id == "GC-C05b", passed]),
      paste("each Phase-2 gate fails on its own perturbation (GC-C05b by construction above); still passing:",
            paste(gb[gate_id != "GC-C05b" & passed %in% TRUE, gate_id], collapse = ",")))
check(must_error(s33_stop_on_gates(gb)), "a failed hard Phase-2 gate stops")
ok("Phase 2 core: 15 gates pass, products outcome-free; every gate fails on its perturbation")

cat("\n6. bridge (GC-C13)\n")
set.seed(41)
an <- D$animals[pop_sis_87 == TRUE]
ce <- an[, .(u = stats::rnorm(1L, 0, 0.5)), by = cc1_cage]
an[, xmk := mean(crossing_rate), by = Batch]
an[, CombZ := unname(c(B1 = 0.2, B2 = -0.1, B3 = 0.3, B4 = 0, B5 = -0.3, B6 = 0.1)[Batch]) + 0.03 * (crossing_rate - xmk) + ce$u[match(cc1_cage, ce$cc1_cage)] + stats::rnorm(.N, 0, 0.6)]
CZ <- an[, .(AnimalNum, Batch, Sex, CombZ, n_components_present = 6L)]; data.table::setkeyv(CZ, "AnimalNum")
s29_recipe <- function(an, cz) {
  out <- list()
  for (pred in S33C_METRICS) {
    d <- data.table::copy(an)[is.finite(get(pred))][, `:=`(x = get(pred), CombZ = cz$CombZ[match(AnimalNum, cz$AnimalNum)], sex_c = ifelse(Sex == "Female", 0.5, -0.5))]
    fit <- stats::lm(CombZ ~ Batch + x + x:sex_c, d); V <- clubSandwich::vcovCR(fit, cluster = d$cc1_cage, type = "CR2")
    ct <- summary(fit)$coefficients; dfm <- fit$df.residual
    for (cf in c("x", "x:sex_c")) { cr <- clubSandwich::coef_test(fit, vcov = V, test = "Satterthwaite", coefs = cf)
      out[[length(out) + 1L]] <- data.table(population = "SIS_ONLY", predictor = pred, estimand = if (cf == "x") "slope_sexavg" else "slope_DiD_F_minus_M",
        estimate = ct[cf, 1], se = ct[cf, 2], df = dfm, ci_low = ct[cf, 1] - stats::qt(.975, dfm) * ct[cf, 2], ci_high = ct[cf, 1] + stats::qt(.975, dfm) * ct[cf, 2],
        se_cr2 = cr$SE, df_cr2 = cr$df_Satt, n = nrow(d)) }
    for (s in c("Female", "Male")) { fs <- stats::lm(CombZ ~ Batch + x, d[Sex == s]); ct <- summary(fs)$coefficients; dfs <- fs$df.residual
      out[[length(out) + 1L]] <- data.table(population = "SIS_ONLY", predictor = pred, estimand = paste0("slope_", s), estimate = ct["x", 1], se = ct["x", 2], df = dfs,
        ci_low = ct["x", 1] - stats::qt(.975, dfs) * ct["x", 2], ci_high = ct["x", 1] + stats::qt(.975, dfs) * ct["x", 2], se_cr2 = NA_real_, df_cr2 = NA_real_,
        n = nrow(d[Sex == s])) }
  }
  data.table::rbindlist(out)
}
FROZEN <- s29_recipe(an, CZ)
br <- s33c_bridge(p2$state$base, CZ, FROZEN)
gb13 <- s33c_bridge_gates(br, TRUE)
check(all(gb13$passed) && nrow(br$reproduction) == 56L && max(br$recombination$diff) < 1e-10 && nrow(br$rows) == 9L,
      "parent rows = the Stage 29 recipe (56 quantities, 1e-9); recombination (SSw bW + SSb bB) / (SSw + SSb) = parent slope (1e-10); 9 c08 rows")
check(all(br$rows[population != "primary_85", interval_method] != "none") && all(br$rows[population == "primary_85", interval_method] == "none") &&
        !any(grepl("DiD", br$rows$estimand)) && all(br$rows[sex == "sexavg", n_cohorts] == 6L), "c08: F, M and sex-averaged rows; the DiD only in the gate record")
fz_bad <- data.table::copy(FROZEN)[predictor == "shared_zone_use" & estimand == "slope_DiD_F_minus_M", se_cr2 := se_cr2 + 1e-8]
check(!isTRUE(s33c_bridge_gates(s33c_bridge(p2$state$base, CZ, fz_bad), TRUE)[gate_id == "GC-C13a", passed]), "GC-C13a fails when a frozen quantity differs by 1e-8")
br_bad <- br; br_bad$recombination <- data.table::copy(br$recombination)[1, diff := 1e-9]
check(!isTRUE(s33c_bridge_gates(br_bad, TRUE)[gate_id == "GC-C13b", passed]) && !s33c_bridge_gates(br, FALSE)[gate_id == "GC-C13a", evaluated],
      "GC-C13b fails beyond 1e-10; GC-C13a not evaluated on permuted outcomes")
ok("bridge, reproduction and recombination")

cat("\n7. Phase 3 core (non-singular synthetic CombZ)\n")
DO <- D; DO$animals <- data.table::copy(D$animals)[, `:=`(CombZ = CZ$CombZ[match(AnimalNum, CZ$AnimalNum)], n_components_present = 6L)]
ck_dir <- file.path(fx, "checkpoints")
ctx3 <- CTX; ctx3$checkpoint_dir <- ck_dir; ctx3$checkpoint_log <- new.env(); ctx3$head <- "test"; ctx3$input_sha256 <- "test"
out <- s33c_phase3_core(DO, p2, FROZEN, ctx3)
g3 <- out$gates
check(setequal(unique(g3$gate_id), c("GC-C04b", "GC-C09", "GC-C13a", "GC-C13b", "GC-C08a", "GC-C08b", "GC-C08c", "GC-C07b")) &&
        all(g3[hard == TRUE, passed]) && isTRUE(s33_stop_on_gates(g3)), paste("Phase-3 hard gates pass:", paste(g3[hard == TRUE & !(passed %in% TRUE), gate_id], collapse = ",")))
check(isTRUE(g3[gate_id == "GC-C08c", passed]) && identical(g3[gate_id == "GC-C08c", hard], FALSE), "GC-C08c: the separately optimised identity form agrees to the optimiser tolerance (recorded)")
psc <- out$audit$c_placebo_structure_checks
check(nrow(psc) == 15L && all(psc$ok) && psc[check_id == "P10", observed] == "0", "placebo: 15 structure checks pass; no checkpoint read or written")
check(identical(names(out$tables), S33_TABLES$C) && identical(names(out$audit), S33_AUDIT_TABLES$C), "declared tables and audit tables")
tg <- s33_table_gates(list(C = out), "C")
check(isTRUE(tg$passed), paste("GC-8 on module C:", tg$detail))
lint <- s33_lint(c(out$tables, out$audit), character(), s33_lint_patterns(), exempt_columns = s33_lint_exempt_columns("C"))
check(nrow(lint) == 0L, paste("lint (GC-9) clean:", paste(lint$where, lint$pattern, collapse = "; ")))
check(all(S33C_LINT_EXEMPT_COLUMNS %in% s33_lint_exempt_columns("C")), "S33C_LINT_EXEMPT_COLUMNS read by the core")
for (nm in names(p2$products)) { P <- p2$products[[nm]]; Tt <- c(out$tables, out$audit)[[nm]]
  check(isTRUE(all.equal(Tt[seq_len(nrow(P)), names(P), with = FALSE], P, check.attributes = FALSE)), paste("product re-extractable from its table:", nm)) }
check(identical(names(out$checkpoints), c("module", "step", "checkpoint_key", "file", "reused")) && nrow(out$checkpoints) == 3L &&
        all(out$checkpoints$module == "C") && length(ls(ctx3$checkpoint_log)) == 3L && all(file.exists(file.path(ck_dir, out$checkpoints$file))),
      "checkpoint rows in the contract form (3 bootstrap streams); also in ctx$checkpoint_log")
e7 <- out$tables$c07_estimates
check(!any(grepl("sex_c", e7$L)) && !any(grepl("sex_interaction|minus", e7$estimand)), "c07 has no female-minus-male row")
c06 <- out$tables$c06_models
M1r <- s33c_fit("M1", data.table::copy(p2$state$terms$crossing_rate$primary)[, CombZ := CZ$CombZ[match(AnimalNum, CZ$AnimalNum)]], "t", 10L)
fx1 <- lme4::fixef(M1r$fit); r6 <- c06[model_id == "crossing_rate|primary|M1"]
check(nrow(c06[!is.na(sex_interaction_note)]) == 2L && abs(r6$sex_interaction_w - fx1[["w:sex_c"]]) < 1e-12 && abs(r6$sex_interaction_b - fx1[["b:sex_c"]]) < 1e-12 &&
        abs(r6$sex_interaction_gamma - (fx1[["b:sex_c"]] - fx1[["w:sex_c"]])) < 1e-12 && grepl("female-cohort set minus male-cohort set", r6$sex_interaction_note),
      "M1 sex interactions: point values on the M1 rows of c06 only")
sing_rate <- isTRUE(c06[model_id == "crossing_rate|primary|M1", singular]); sing_occ <- isTRUE(c06[model_id == "shared_zone_use|primary|M1", singular])
check(!sing_rate && !sing_occ, "this synthetic CombZ gives non-singular M1 fits (the singular rule is section 8)")
core <- e7[variant == "primary" & model == "M1" & estimand %in% S33C_PRIMARY_ESTIMANDS]
check(all(core[role == "primary", quote_role] == "primary_interval") && all(core[role == "check", quote_role] == "check") &&
        nrow(core[lead == TRUE]) == 12L && all(core[lead == TRUE, scale] == "raw") && all(core[lead == TRUE, interval_method] %in% c("KR", "CR2_Satterthwaite_cc1_cage")),
      "non-singular rule: KR and CR2 by CC1 cage with equal standing; lead = the raw primary rows")
cs <- core[quote_role == "primary_interval", .(n = sum(compatible_sentence_interval), wider = interval_method[which.max(ci_high - ci_low)],
                                               chosen = interval_method[compatible_sentence_interval]), by = .(metric_id, estimand, scale)]
check(all(cs$n == 1L) && all(cs$wider == cs$chosen), "the wider of KR and CR2 is the interval for 'compatible with' sentences")
check(all(is.finite(core[quote_role == "primary_interval", top4_cage_info_share])) && all(is.finite(core[quote_role == "primary_interval", c13_halfwidth_r])) &&
        all(is.finite(core[quote_role == "primary_interval" & estimand != "within_cc1_cage", leave_one_cage_out_min_estimate])) &&
        all(is.na(core[quote_role == "primary_interval" & estimand == "within_cc1_cage", leave_one_cage_out_min_estimate])),
      "quoted with the primary rows: top-4 information share, c13 half-width, leave-one-cage-out range (beta_B, gamma)")
check(all(e7[model == "S9_cc1_weight", quote_role] == "quoted_beside") && all(e7[model == "S11_matched" & estimand == "gamma_matched", quote_role] == "quoted_beside") &&
        all(e7[model == "C_pooled", quote_role] == "table_only") && all(e7[model == "S9_cc1_weight" & scale == "raw", grepl("weight_dev", mech_coupling)]),
      "S9 and S11 gamma quoted beside; C_pooled table only; S9 mech_coupling")
st <- e7[model == "S7_stratum" & scale == "raw"]; sm <- e7[model == "S7_strata_mean" & scale == "raw"]
for (mt in S33C_METRICS) for (e in S33C_PRIMARY_ESTIMANDS) { f <- st[metric_id == mt & variant == "S7_Female" & estimand == e]
  m <- st[metric_id == mt & variant == "S7_Male" & estimand == e]; r <- sm[metric_id == mt & estimand == e]
  check(abs(r$estimate - (f$estimate + m$estimate) / 2) < 1e-12 && abs(r$se - sqrt(f$se^2 + m$se^2) / 2) < 1e-12 &&
          abs(r$df - (f$se^2 + m$se^2)^2 / (f$se^4 / f$df + m$se^4 / m$df)) < 1e-9 && r$interval_method == "Welch", paste("strata mean (Welch):", mt, e)) }
pers <- e7[scale == "per_frozen_sd"]; raws <- e7[scale == "raw" & role != "influence"]
kk <- match(paste(pers$metric_id, pers$variant, pers$model, pers$estimand, pers$interval_method, pers$L), paste(raws$metric_id, raws$variant, raws$model, raws$estimand, raws$interval_method, raws$L))
check(!anyNA(kk) && max(abs(pers$estimate - raws$estimate[kk] * S33C_SD[pers$metric_id]), na.rm = TRUE) < 1e-12 && all(grepl("per frozen SD", pers$units)),
      "per-SD rows = raw rows x the frozen SD (5.1698 / 0.0607)")
c09 <- out$tables$c09_influence_summary
lo <- e7[grepl("^LOBO", variant) & metric_id == "crossing_rate" & estimand == "contextual_peer_composition"]
z9 <- c09[metric_id == "crossing_rate" & estimand == "contextual_peer_composition" & unit_type == "cohort (LOBO)"]
check(nrow(c09) == 12L && z9$n_refits == 6L && abs(z9$min_estimate - min(lo$estimate)) < 1e-12 && abs(z9$max_estimate - max(lo$estimate)) < 1e-12 &&
        all(c09[unit_type == "cage (leave-one-cage-out)", n_refits] == 17L) && grepl("of 6 LOBO refits", z9$sign_stability) && is.logical(c09$sign_same_S9) &&
        all(c09$interval_basis == "none"), "influence summary: LOBO and leave-one-cage-out ranges, sign stability (incl. S9, S11)")
c11 <- out$tables$c11_correlations
tp <- data.table::copy(p2$state$terms$crossing_rate$primary)[, CombZ := CZ$CombZ[match(AnimalNum, CZ$AnimalNum)]]
cgc <- tp[, .(ybar = mean(CombZ), xbar_c = xbar_c[1], n_c = .N, Batch = Batch[1]), by = CageEpisodeID]
rx <- stats::residuals(stats::lm(xbar_c ~ Batch, cgc, weights = n_c)); ry <- stats::residuals(stats::lm(ybar ~ Batch, cgc, weights = n_c))
r_w <- stats::cov.wt(cbind(rx, ry), wt = cgc$n_c / sum(cgc$n_c), cor = TRUE)$cor[1, 2]
rc <- c11[metric_id == "crossing_rate" & variant == "primary" & sex == "pooled" & estimand == "partial_r_cage"]
rw <- c11[metric_id == "crossing_rate" & variant == "primary" & sex == "pooled" & estimand == "partial_r_within_cage"]
tp[, yw := CombZ - mean(CombZ), by = CageEpisodeID]
check(abs(rc$estimate - r_w) < 1e-10 && abs(rw$estimate - stats::cor(tp$w, tp$yw)) < 1e-12 &&
        abs(rc$ci_high - tanh(atanh(rc$estimate) + stats::qnorm(0.975) / sqrt(17 - 5 - 3))) < 1e-12 && rw$n_units == 64L && rw$q == 16L && nrow(c11) == 24L,
      "partial r: n_c-weighted cage r = WLS partial correlation on Batch; within r; Fisher z with q = 5 (cage) and G - 1 (within)")
c10 <- out$tables$c10_variance
ic <- c10[metric_id == "crossing_rate" & model == "M1" & estimand == "icc_cc1_cage"]
check(abs(ic$estimate - ic$var_cage / (ic$var_cage + ic$var_resid)) < 1e-14 && ic$interval_method == "parametric_bootstrap_percentile" && ic$pb_B == 9L &&
        ic$pb_seed == S33_SEEDS[["C_icc_resid1"]] && nrow(c10) == 19L && all(c10[interval_method == "none", interval_basis] == "none") &&
        all(c10[estimand == "cage_variance_cc1", interval_method] == "profile_likelihood"), "ICC rows: var_cage / (var_cage + var_resid), bootstrap stream, profile, moment ICCs")
cx <- out$cross
loo <- tp[, .(AnimalNum, xl = vapply(seq_len(.N), function(i) mean(x[CageEpisodeID == CageEpisodeID[i] & AnimalNum != AnimalNum[i]]), 0))]
check(identical(names(cx), c("focal", "cages", "tp2", "src_cage", "s1", "xbar_loo", "s3_delta_peer_per_sd", "s3_singular")) && length(cx$focal) == 64L &&
        length(cx$cages) == 17L && nrow(cx$tp2) == 65L && identical(cx$tp2$AnimalNum, cx$src_cage$AnimalNum) && cx$s1 == S33_SD_RATE &&
        max(abs(cx$xbar_loo$xbar_loo - loo$xl[match(cx$xbar_loo$AnimalNum, loo$AnimalNum)])) < 1e-10 && length(cx$s3_delta_peer_per_sd) == 1L &&
        is.logical(cx$s3_singular), "cross export: focal, cages, tp2 / src_cage (same animals), s1, xbar_loo = cage-mate mean, one S3 value")
ok("Phase 3 core: gates, placebo, identities, quoting rule, tables, GC-8, lint, products, checkpoints, cross export")

cat("\n8. singular synthetic CombZ\n")
set.seed(43)
an2 <- D$animals[pop_sis_87 == TRUE]
an2[, e := stats::rnorm(.N)][, e := e - mean(e), by = cc1_cage]
cu <- an2[, .(u = stats::rnorm(1L, 0, 0.05)), by = cc1_cage]
an2[, CombZ := unname(c(B1 = 0.2, B2 = -0.1, B3 = 0.3, B4 = 0, B5 = -0.3, B6 = 0.1)[Batch]) + 0.02 * crossing_rate + 1.5 * data.table::fifelse(is.finite(shared_zone_use), shared_zone_use, 0.3) + e + cu$u[match(cc1_cage, cu$cc1_cage)]]
CZ2 <- an2[, .(AnimalNum, Batch, Sex, CombZ, n_components_present = 6L)]; data.table::setkeyv(CZ2, "AnimalNum")
ctx_nc <- CTX; ctx_nc$no_checkpoints <- TRUE
run <- s33c_estimate(p2$state, CZ2, NULL, ctx_nc, vapply(S33C_BOOT_SEEDS, function(s) S33_SEEDS[[s]], 0L), 9L, new.env(), check_frozen = FALSE)
check(all(vapply(run$res, function(z) z$singular, TRUE)) && all(vapply(run$res, function(z) z$s11_both_singular, TRUE)), "this synthetic CombZ gives singular M1 and S11 fits")
e8 <- run$est[variant == "primary" & model == "M1" & estimand %in% S33C_PRIMARY_ESTIMANDS]
pi8 <- e8[quote_role == "primary_interval" & scale == "raw"]
check(nrow(pi8) == 6L && all(pi8[estimand == "within_cc1_cage", interval_method] == "KR") && all(pi8[estimand == "between_cc1_cages_within_cohort", interval_method] == "t_9") &&
        all(pi8[estimand == "contextual_peer_composition", interval_method] == "Welch") && all(e8[interval_method == "CR2_Satterthwaite_cc1_cage", quote_role] == "quoted_beside") &&
        all(pi8$compatible_sentence_interval) && all(pi8$lead), "singular rule: beta_W KR, beta_B cage-aggregate WLS t(G - 8), gamma two-part Welch; CR2 quoted beside")
for (m in S33C_METRICS) { k <- e8[metric_id == m & scale == "raw"]
  kb <- k[estimand == "between_cc1_cages_within_cohort" & interval_method == "KR"]; ab <- k[estimand == "between_cc1_cages_within_cohort" & interval_method == "t_9"]
  wd <- k[estimand == "within_cc1_cage" & interval_method == "t_45"]; wl <- k[estimand == "contextual_peer_composition" & interval_method == "Welch"]
  check(abs(kb$estimate - ab$estimate) < 1e-8 && abs(wl$estimate - (ab$estimate - wd$estimate)) < 1e-12 &&
          abs(wl$df - (ab$se^2 + wd$se^2)^2 / (ab$se^4 / 9 + wd$se^4 / 45)) < 1e-9 && abs(wl$se - sqrt(ab$se^2 + wd$se^2)) < 1e-12,
        paste("singular M1 = OLS: cage aggregate = M1 beta_B; Welch gamma = aggregate - dummy, df (sB^2 + sW^2)^2 / (sB^4/14 + sW^4/61) form:", m)) }
gi <- s33c_identity_gates(run)
check(isTRUE(gi[gate_id == "GC-C08a", passed]) && all(gi[gate_id == "GC-C08b", hard]) && all(gi[gate_id == "GC-C08b", passed]), "GC-C08a holds; GC-C08b gated (both singular) and holds")
s3 <- run$est[model == "S3_peer_loo" & scale == "per_frozen_sd" & metric_id == "crossing_rate"]
e15 <- data.table::copy(p2$state$terms$crossing_rate$primary)[, CombZ := CZ2$CombZ[match(AnimalNum, CZ2$AnimalNum)]]
e15[, `:=`(own = x / S33_SD_RATE, m1 = xbar_loo / S33_SD_RATE)]
b15 <- stats::coef(stats::lm(CombZ ~ Batch + own + m1 + own:sex_c + m1:sex_c, e15))[["m1"]]
check(isTRUE(s3$singular) && abs(s3$estimate - b15) < 1e-6, "S3 delta_peer per frozen SD = E's S15 OLS m1 coefficient when S3 is singular (X8)")
pbr <- run$est[model == "M1" & variant == "primary" & grepl("^parametric_bootstrap", interval_method)]
check(all(pbr$interval_method == "parametric_bootstrap_spread") && all(grepl("not a confidence limit", pbr$note)) &&
        all(grepl("spread", run$variance[model == "M1" & estimand == "icc_cc1_cage", note])), "boundary: bootstrap percentiles labelled spread under the fitted zero component")
bm1 <- s33c_bootmer(s33c_fit("M1", data.table::copy(p2$state$terms$crossing_rate$primary)[, CombZ := CZ2$CombZ[match(AnimalNum, CZ2$AnimalNum)]], "t", 10L), 5L, 33030201L)
bm2 <- s33c_bootmer(s33c_fit("M1", data.table::copy(p2$state$terms$crossing_rate$primary)[, CombZ := CZ2$CombZ[match(AnimalNum, CZ2$AnimalNum)]], "t", 10L), 5L, 33030201L)
check(identical(bm1$t, bm2$t) && identical(dim(bm1$t), c(5L, 6L)), "parametric bootstrap: one seed per stream, reproducible, B rows")
mf <- s33c_fit("M1", data.table::copy(p2$state$terms$crossing_rate$primary)[, CombZ := CZ2$CombZ[match(AnimalNum, CZ2$AnimalNum)]], "t", 9L)
kf <- s33c_kr_rows(mf, S33C_L, "crossing_rate", "primary", "M1", "primary")
check(s32i_failed(mf) && all(kf$status == "FAILED") && all(is.na(kf$estimate)) && is.null(s33c_bootmer(mf, 5L, 1L)$t) &&
        is.na(s33c_profile(mf)$sd_high) && all(is.na(unlist(s33c_sex_interaction_points(mf)[, 1:3]))), "a FAILED fit (rank stop): FAILED rows without estimates, empty bootstrap, no profile")
ok("singular rule, Welch df, GC-C08b gated, S3 = S15, spread label, FAILED fits")

cat("\n9. gate fixtures\n")
cz_bad <- data.table::copy(CZ)[3, CombZ := NA_real_]
check(!isTRUE(s33c_outcome_join_gate(cz_bad, p2$state)$passed) && isTRUE(s33c_outcome_join_gate(CZ, p2$state)$passed), "GC-C04b: a missing CombZ fails")
rb <- run; rb$res$crossing_rate$ident <- data.table::copy(run$res$crossing_rate$ident)[identity == "within_cage_dummy", diff := 1e-7]
check(!isTRUE(s33c_identity_gates(rb)[gate_id == "GC-C08a", passed]), "GC-C08a fails beyond 1e-8")
rn <- run; for (m in S33C_METRICS) rn$res[[m]]$ident <- data.table::copy(run$res[[m]]$ident)[, evaluable := FALSE]
gn <- s33c_identity_gates(rn)
check(!any(gn$evaluated) && isTRUE(s33_stop_on_gates(gn)), "identities needing FAILED fits only: not evaluated, never stopping")
rs <- run; rs$res$shared_zone_use$ident <- data.table::copy(run$res$shared_zone_use$ident)[kind == "s11", diff := 1e-6]
gs <- s33c_identity_gates(rs)[gate_id == "GC-C08b"]
check(!all(gs$passed) && must_error(s33_stop_on_gates(gs)), "GC-C08b fails (and stops) when both fits are singular and S11 differs from M1")
psc0 <- s33c_structure_checks(run, p2$state, 9L)
check(nrow(psc0) == 13L && all(psc0$ok), "structure checks pass on a complete run")
r1 <- run; r1$est <- run$est[-1]
r2 <- run; r2$est <- data.table::copy(run$est)[1, status := "FAILED"]
r3 <- run; r3$res$crossing_rate$boot_dims <- c(NA, NA)
r4 <- run; r4$res$crossing_rate$boot_dims <- c(NA, NA); r4$res$crossing_rate$m1_failed <- TRUE
check(!all(s33c_structure_checks(r1, p2$state, 9L)$ok) && !all(s33c_structure_checks(r2, p2$state, 9L)$ok) && !all(s33c_structure_checks(r3, p2$state, 9L)$ok) &&
        s33c_structure_checks(r4, p2$state, 9L)[check_id == "P7", ok], "structure checks fail on a missing row, a FAILED row with an estimate, an empty stream (not for a FAILED fit)")
check(isTRUE(s33_gate_rows("GC-C09", "x", psc0$ok)$passed) && !isTRUE(s33_gate_rows("GC-C09", "x", s33c_structure_checks(r1, p2$state, 9L)$ok)$passed), "GC-C09 passes and fails with its checks")
pm <- s33c_permute(CZ, S33_SEEDS[["C_placebo"]])
check(identical(pm, s33c_permute(CZ, S33_SEEDS[["C_placebo"]])) && all(pm[, sort(CombZ), by = Batch]$V1 == CZ[, sort(CombZ), by = Batch]$V1) &&
        !identical(pm$CombZ, CZ$CombZ), "placebo permutation: within cohort, reproducible, CombZ only")
ok("GC-C04b, GC-C08a/b, GC-C09 fixtures; permutation")

unlink(fx, recursive = TRUE)
cat(sprintf("\nPASS: stage 33 module C (%.0f s)\n", as.numeric(difftime(Sys.time(), T0, units = "secs"))))
