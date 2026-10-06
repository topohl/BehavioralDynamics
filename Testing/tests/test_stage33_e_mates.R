# Stage 33 module E (Functions/stage33_e_mates.R): synthetic checks only.
#
#   1. classes of cohort-mates on a hand-made cohort: first-met class, dyads, averaging matrices;
#   2. a synthetic six-cohort design on the affine plane of order 4 (CC1-CC4 regroupings and a never co-housed class that
#      closes into groups of 4, as in B2 and B3), with untracked, excluded and single animals, 3-cages, a pre-SIS cage
#      singleton, swapped pre-SIS cages and a CC1 pair meeting again at CC4: classes, class means, version-2, S4, S7 and
#      S10 sets against brute force; the within-cohort identity (GE-14), m1 = xbar_loo / s1 (GE-17), the five-class rank
#      deficiency;
#   3. designs: fixed parts, templates, stacked = separate fits and CR2 blocks (GE-16), VIF and GE-10, absent coefficients;
#   4. CR2: manual = clubSandwich (SE and Satterthwaite df; GE-11) for the single, subset, pre-SIS cage and stacked designs
#      and with a zero eigenvalue (pseudo-inverse); CR1;
#   5. wild cluster bootstrap-t: Webb weights of one seed (RNG kind restored); vectorized = loop with clubSandwich = loop
#      with the manual CR2 (CR2- and CR1-studentized); the interval formula;
#   6. simulation: scenario table and index rule, generator = the plan's model (independent loop), truths, summaries, sizes,
#      reproducibility, coverage (replicates regenerated from the scenario seed, wild weights from the coverage seed)
#      against a loop;
#   7. gates GE-03, GE-04, GE-05, GE-08, GE-09, GE-11, GE-14, GE-15, GE-16, GE-17: passing and failing fixtures;
#   8. the module on the synthetic design: Phase 2 and Phase 3, gates, declared tables, GC-8, lint, outcome-free products,
#      independent refits of e04 rows (single, subset, pre-SIS cage, weight, version 2 per CC and stacked), the wild interval
#      of an e04 row by a loop, per-exposure-SD rows, e06 sensitivities S1-S16 (lm or lmer refits; S8 and e08; S14 labels;
#      S11 interval methods; S16 CR2 by CC4 cage), checkpoints, the cross contract with a mock module C (X3, X4, X6, X7, X8;
#      S15 fill);
#   9. S15 = C's S3 after rescaling (OLS exactly; the frozen-engine lmer within 1e-6 when singular); no p or chance column.
# Portable: no S: drive, no live data, no readxl; sources Functions/ only.

suppressPackageStartupMessages({ library(data.table); library(digest) })
fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")
must_error <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
near <- function(a, b, tol) { a <- as.numeric(a); b <- as.numeric(b)
  length(a) == length(b) && identical(is.na(a), is.na(b)) && all(abs(a - b) <= tol, na.rm = TRUE) }
for (f in c("Functions/rfid_canonical_inference.R", "Functions/stage32_inference.R", "Functions/stage32_run.R", "Functions/stage33_common.R",
            "Functions/stage33_run.R", "Functions/stage33_e_mates.R")) source(f)
cfg <- new.env(); sys.source("Functions/behavior_analysis_config.R", envir = cfg)
fx <- file.path(tempdir(), paste0("s33e_", Sys.getpid())); dir.create(fx, showWarnings = FALSE)
mt_seed <- function(seed, fun) { old <- RNGkind(); on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(seed); fun() }
WEBB <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))
T0 <- Sys.time()

cat("1. classes of cohort-mates (hand-made cohort)\n")
ea <- rbind(c("a", "b", "c", "d"), c("a", "x", "y", "z"), c("p", "q", "r", "s"), c(NA, "b", "c", "d"))
eb <- rbind(c("a", "b", "c", "d"), c("b", "x", "y", "s"), c("p2", "q2", "r2", "s2"), c(NA, "b2", "c", "d"))
check(identical(s33e_first_met(ea, eb), c("CC1", "CC2", "NM", "CC3")), "the first shared CEID gives the class, none gives NM, NA never matches")
nd <- data.table(AnimalNum = c("A", "B", "C", "D", "E"), Batch = "B1",
                 e1 = c("c1", "c1", "c1x", "c1x", "c1y"), e2 = c("c2a", "c2b", "c2a", "c2b", "c2c"),
                 e3 = c("c3a", "c3b", "c3b", "c3c", "c3a"), e4 = c("c4a", "c4a", "c4b", "c4c", "c4b"),
                 src_cage = c("s1", "s2", "s1", "s2", "s1"), dob = as.Date(c("2022-10-01", "2022-10-02", "2022-10-01", "2022-10-02", "2022-10-01")))
dy <- s33e_dyads(nd)
check(identical(dy$first_met_class, c("CC1", "CC2", "NM", "CC3", "CC3", "CC2", "NM", "CC1", "CC4", "NM")) &&
        identical(dy$n_cc_together, c(2L, 1L, 0L, 1L, 1L, 1L, 0L, 1L, 1L, 0L)) && identical(dy$ccs_together[1:3], c("CC1;CC4", "CC2", "none")),
      "dyads: first-met class, number and list of shared CCs (A and B together at CC1 and CC4)")
check(identical(dy[first_met_class == "NM", nm_subclass], c("dc", "dc", "dc")) && identical(dy$same_src_cage[c(2, 4)], c(TRUE, TRUE)) &&
        all(is.na(dy[first_met_class != "NM", nm_subclass])) && identical(dy$same_dob[1:2], c(FALSE, TRUE)),
      "never co-housed subclass by recorded pre-SIS cage; same DoB flag")
od1 <- dy[, .(a = c(ia, ib), b = c(ib, ia), cls = rep(first_met_class, 2L))]
av <- s33e_avg(5L, od1$a, od1$b, od1$cls == "CC1")
check(identical(av$n, c(1L, 1L, 1L, 1L, 0L)) && all(is.na(av$M[5, ])) && isTRUE(all.equal(rowSums(av$M[1:4, ]), rep(1, 4))) && av$M[1, 2] == 1,
      "averaging matrix: rows average over the class; an empty class gives an NA row")
avn <- s33e_avg(5L, od1$a, od1$b, od1$cls == "NM")
check(isTRUE(all.equal(avn$M[4, ], c(0.5, 0, 0, 0, 0.5))) && avn$n[4] == 2L, "D's never co-housed class is A and E, weight 1/2 each")
ok("first-met class, dyads, averaging matrices")

cat("\n2. synthetic design: classes, class means, version-2 sets, identities\n")
gf4 <- matrix(c(0L, 0L, 0L, 0L, 0L, 1L, 2L, 3L, 0L, 2L, 3L, 1L, 0L, 3L, 1L, 2L), 4L, 4L, byrow = TRUE)   # GF(4): 2 = a, 3 = a + 1
P16 <- data.table(x = rep(0:3, each = 4L), y = rep(0:3, times = 4L))
line_of <- function(m) if (is.na(m)) P16$x else bitwXor(P16$y, gf4[cbind(m + 1L, P16$x + 1L)])
LN <- cbind(CC1 = line_of(NA), CC2 = line_of(0L), CC3 = line_of(1L), CC4 = line_of(2L), NM = line_of(3L))
pp <- t(utils::combn(16L, 2L))
check(all(rowSums(LN[pp[, 1], ] == LN[pp[, 2], ]) == 1L) && all(apply(LN, 2, function(v) all(tabulate(v + 1L) == 4L))),
      "fixture: five parallel classes of four lines of 4; every pair shares exactly one")
bf_class <- function(E, i, j) { s <- which(E[i, ] == E[j, ]); if (length(s)) paste0("CC", s[1]) else "NM" }
cmat <- function(d) as.matrix(d[, paste0("cc", 1:4, "_cage"), with = FALSE])
classes_ok <- function(d) {   # brute force: every animal with a CC1 mate has all classes; no CC1 pair at CC2/CC3; no CC2 pair at CC4
  E <- cmat(d); n <- nrow(E); cl <- matrix(NA_character_, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) if (i != j) cl[i, j] <- bf_class(E, i, j)
  tog <- function(i, j, k) isTRUE(E[i, k] == E[j, k])
  foc <- which(rowSums(cl == "CC1", na.rm = TRUE) >= 1L)
  each <- all(vapply(foc, function(i) all(c("NM", "CC2", "CC3", "CC4") %in% cl[i, ]), TRUE))
  bad <- any(vapply(seq_len(n), function(i) any(vapply(setdiff(seq_len(n), i), function(j)
    (cl[i, j] == "CC1" && (tog(i, j, 2) || tog(i, j, 3))) || (cl[i, j] == "CC2" && tog(i, j, 4)), TRUE)), TRUE))
  each && !bad
}
make_design <- function(seed = 33050L) {
  set.seed(seed); sis <- list()
  for (b in S33_COHORTS) {
    d <- data.table(AnimalNum = sprintf("S%s%02d", substr(b, 2, 2), 1:16), Batch = b, Sex = S33_SEX_OF_COHORT[[b]], condition = "SIS",
                    x = P16$x, y = P16$y, nm_line = LN[, "NM"], tracked = TRUE, excluded = FALSE, xmate_excluded = FALSE)
    for (k in 1:4) d[, (paste0("cc", k, "_cage")) := paste0(b, "|sys.", LN[, k] + 1L, "|CC", k)]
    d[, src_cage := paste0(b, "-src", nm_line + 1L)]
    if (b == "B1") d[x %in% 2:3 & y > 0L, tracked := FALSE]                                 # 6 untracked; CC1 cages of 4, 4, 1, 1
    if (b == "B2") { d[, src_cage := paste0(b, "-src", LN[, "CC4"] + 1L)]                     # six pre-SIS cages (two split)
      d[LN[, "CC4"] >= 2L, src_cage := paste0(src_cage, ifelse(x < 2L, "a", "b"))] }
    if (b == "B3") d[, src_cage := paste0(b, "-src", LN[, "CC2"] + 1L)]                     # never co-housed mates from other cages
    if (b %in% c("B4", "B6")) { d[x == 0L & y == 0L, excluded := TRUE]; d[x == 0L & y > 0L, xmate_excluded := TRUE] }
    if (b == "B4") d[x == 1L & y == 0L, src_cage := "B4-solo"]                               # alone in its pre-SIS cage
    if (b == "B5") d[x == 3L & y == 0L, excluded := TRUE]                                   # a 3-cage
    if (b %in% c("B5", "B6")) { i <- which(d$x == (if (b == "B5") 0L else 2L) & d$y %in% 0:1); d[i, src_cage := rev(d$src_cage[i])] }
    sis[[b]] <- d
  }
  # B6: a CC1 pair meets again at CC4 (P moves into Q's CC4 cage), keeping every class non-empty
  k6 <- sis$B6[tracked & !excluded]; mv <- NULL
  for (p in k6$AnimalNum) { if (!is.null(mv)) break
    for (q in setdiff(k6[cc1_cage == k6[AnimalNum == p, cc1_cage], AnimalNum], p)) {
      d2 <- copy(k6); d2[AnimalNum == p, cc4_cage := k6[AnimalNum == q, cc4_cage]]
      if (classes_ok(d2)) { mv <- c(p, q); break } } }
  sis$B6[AnimalNum == mv[1], cc4_cage := sis$B6[AnimalNum == mv[2], cc4_cage]]
  for (b in S33_COHORTS) {
    d <- sis[[b]]; srcs <- sort(unique(d$src_cage)); cc1d <- as.Date(S33_CC1_DATES[[b]])
    age <- stats::setNames(sample(22:29, length(srcs), replace = TRUE), srcs)
    if (b == "B2") age <- stats::setNames(ifelse(grepl("src1$|src2$|src3a$", srcs), 24L, 22L), srcs)   # two DoB groups (10 + 6)
    d[, `:=`(dob = cc1d - age[src_cage], line = if (b %in% c("B1", "B3", "B5")) ifelse(src_cage == srcs[1], "C57BL/6J", "C57BL/6JRj") else "C57BL/6JRj")]
    w0 <- stats::setNames(stats::rnorm(length(srcs), 0, 1.2), srcs)
    d[, tp2 := round((if (Sex[1] == "Female") 17.5 else 21) + w0[src_cage] + stats::rnorm(.N, 0, 0.9), 1)]
    mu <- stats::runif(4L, 22, 38); for (k in 1:4) d[, (paste0("r", k)) := mu[k] + stats::rnorm(.N, 0, 5)]
    sis[[b]] <- d
  }
  con <- rbindlist(lapply(S33_COHORTS, function(b) {
    cd <- data.table(AnimalNum = sprintf("C%s%02d", substr(b, 2, 2), 1:4), Batch = b, Sex = S33_SEX_OF_COHORT[[b]], condition = "CON",
                     tracked = TRUE, excluded = FALSE, xmate_excluded = FALSE, src_cage = paste0(b, "-con"),
                     dob = as.Date(S33_CC1_DATES[[b]]) - 25L, line = "C57BL/6JRj", tp2 = round(20 + stats::rnorm(4L), 1))
    for (k in 1:4) { cd[, (paste0("cc", k, "_cage")) := paste0(b, "|sys.9|CC", k)]; cd[, (paste0("r", k)) := 30 + stats::rnorm(4L, 0, 5)] }
    cd }))
  a <- rbind(rbindlist(sis), con, fill = TRUE)[excluded == FALSE]
  a[, `:=`(line_J = as.integer(line == "C57BL/6J"), age_cc1 = as.numeric(as.Date(S33_CC1_DATES[Batch]) - dob),
           untracked_b1 = condition == "SIS" & !tracked, pop_tracked_111 = tracked, pop_sis_87 = tracked & condition == "SIS",
           pop_sis_93 = condition == "SIS", pop_canonical_117 = TRUE, pop_con_24 = condition == "CON")]
  for (k in 1:4) { a[tracked == FALSE, (paste0("cc", k, "_cage")) := NA_character_]; a[tracked == FALSE, (paste0("r", k)) := NA_real_] }
  single <- a[pop_sis_87 == TRUE, .N, by = cc1_cage][N == 1L, cc1_cage]
  a[, `:=`(pop_focal_85 = pop_sis_87 & !cc1_cage %in% single, crossing_rate = r1)]
  ep <- rbindlist(lapply(1:4, function(k) a[tracked == TRUE, .(AnimalNum, Batch, Sex, condition, CC = paste0("CC", k),
                                                                CageEpisodeID = get(paste0("cc", k, "_cage")), crossing_rate = get(paste0("r", k)))]))
  animals <- a[, .(AnimalNum, Batch, Sex, condition, tracked, untracked_b1, xmate_excluded, pop_tracked_111, pop_sis_87, pop_focal_85,
                   pop_sis_93, pop_canonical_117, pop_con_24, cc1_cage, cc2_cage, cc3_cage, cc4_cage, src_cage, line, line_J, dob, age_cc1,
                   tp2, crossing_rate)]
  setkey(animals, AnimalNum)
  list(design = list(animals = animals, episodes = ep), full = a, moved = mv)
}
mk <- make_design(); des <- mk$design; A <- mk$full
check(nrow(des$animals) == 117L && sum(des$animals$pop_sis_87) == 87L && sum(des$animals$pop_focal_85) == 85L && nrow(des$episodes) == 444L &&
        identical(des$animals[pop_sis_87 == TRUE, .N, keyby = Batch]$N, c(10L, 16L, 16L, 15L, 15L, 15L)) && length(mk$moved) == 2L,
      "fixture: 117 animals, 87 tracked SIS (10/16/16/15/15/15), 85 focal, 444 episodes, one CC1 pair moved together at CC4")
# brute force over the 87 contributors
nod <- A[pop_sis_87 == TRUE][order(AnimalNum)]; n87 <- nrow(nod)
E <- cmat(nod); R <- unname(as.matrix(nod[, paste0("r", 1:4), with = FALSE]))
CL <- matrix(NA_character_, n87, n87)
for (i in seq_len(n87)) for (j in seq_len(n87)) if (i != j && nod$Batch[i] == nod$Batch[j]) CL[i, j] <- bf_class(E, i, j)
SS <- outer(nod$src_cage, nod$src_cage, "==")
mates <- function(i, cls) which(CL[i, ] %in% cls)
mn <- function(v) if (length(v)) mean(v) else NA_real_
s1 <- S33_SD_RATE; sL <- S33E_SL; sW <- S33E_SW
c_of <- c(`2` = "CC3", `3` = "CC4", `4` = "CC2")
bf <- rbindlist(lapply(seq_len(n87), function(i) {
  h <- function(k) which(E[, k] == E[i, k]); c1 <- mates(i, "CC1"); nm <- mates(i, "NM"); dc <- which(!SS[i, ])
  v <- c(own = R[i, 1] / s1, m1 = mn(R[c1, 1]) / s1, m2 = mn(R[mates(i, "CC2"), 1]) / s1, m3 = mn(R[mates(i, "CC3"), 1]) / s1,
         m4 = mn(R[mates(i, "CC4"), 1]) / s1, mNM = mn(R[nm, 1]) / s1, mNMdc = mn(R[intersect(nm, dc), 1]) / s1,
         m2_dc = mn(R[intersect(mates(i, "CC2"), dc), 1]) / s1, m3_dc = mn(R[intersect(mates(i, "CC3"), dc), 1]) / s1,
         m4_dc = mn(R[intersect(mates(i, "CC4"), dc), 1]) / s1,
         ownW = nod$tp2[i] / sW, wm1 = mn(nod$tp2[c1]) / sW, wm2 = mn(nod$tp2[mates(i, "CC2")]) / sW, wm3 = mn(nod$tp2[mates(i, "CC3")]) / sW,
         wm4 = mn(nod$tp2[mates(i, "CC4")]) / sW, wmNM = mn(nod$tp2[nm]) / sW, xbar_loo = mn(R[c1, 1]))
  for (k in 2:4) v <- c(v, stats::setNames(c(R[i, k] / sL, mn(R[setdiff(c1, h(k)), k]) / sL, mn(R[setdiff(mates(i, c_of[[as.character(k)]]), h(k)), k]) / sL,
                                             mn(R[nm, k]) / sL, mn(R[setdiff(setdiff(c1, h(k)), which(nod$AnimalNum == mk$moved[1])), k]) / sL),
                                           paste0(c("own", "v", "l", "u", "v_s10_"), k)))
  data.table(AnimalNum = nod$AnimalNum[i], t(v), n_cc1 = length(c1), n_nm = length(nm))
}))
nod[, sex_c := fifelse(Sex == "Female", 0.5, -0.5)]
focal <- bf$n_cc1 >= 1L
pairs <- which(upper.tri(CL) & !is.na(CL), arr.ind = TRUE)
pn <- function(ij) paste(nod$AnimalNum[ij[, 1]], nod$AnimalNum[ij[, 2]], sep = "|")
ntog <- vapply(seq_len(nrow(pairs)), function(r) sum(E[pairs[r, 1], ] == E[pairs[r, 2], ]), 0L)
pcls <- CL[pairs]
units_f <- nod[focal, .N, by = .(Batch, src_cage)]
fe_units <- units_f[N >= 2L]
# outcomes (Phase 3 fixture): the focal CombZ = an S3-design mean plus a residual orthogonal to the S3 design and to the CC1
# cage indicators, so C's S3 lmer fit is singular (cage variance estimated at 0) and equals its OLS fit
fb <- data.table(nod[focal, .(AnimalNum, Batch, Sex, cc1_cage, sex_c, x = r1)], xbar_loo = bf$xbar_loo[focal])
XS3 <- model.matrix(~ factor(Batch) + x + xbar_loo + x:sex_c + xbar_loo:sex_c, fb)
Zc <- model.matrix(~ 0 + factor(cc1_cage), fb)
set.seed(9L); e0 <- qr.resid(qr(cbind(XS3, Zc)), stats::rnorm(nrow(fb))); e0 <- e0 / stats::sd(e0)
fb[, CombZ := drop(XS3 %*% c(0, 0.3, -0.2, 0.4, 0.1, -0.3, 0.01, 0.03, 0.005, -0.01)) + e0]
thr <- data.table(Sex = c("Female", "Male"), susceptibility_threshold = c(-0.2, -0.1))
oc <- des$animals[, .(AnimalNum, Sex, condition)]
oc[, CombZ := fb$CombZ[match(AnimalNum, fb$AnimalNum)]][is.na(CombZ), CombZ := stats::rnorm(.N)]
oc[, outcome_group := fifelse(condition == "CON", "CON", fifelse(CombZ < thr$susceptibility_threshold[match(Sex, thr$Sex)], "SUS", "RES"))]
for (cm in S33_COMPONENTS) oc[, (cm) := CombZ + stats::rnorm(.N, 0, 0.3)]
oc[AnimalNum == fb$AnimalNum[5], delta_cort := NA_real_]
oc[, n_components_present := rowSums(!is.na(as.matrix(.SD))), .SDcols = S33_COMPONENTS]
des_out <- list(animals = merge(des$animals, oc[, c("AnimalNum", "CombZ", "outcome_group", S33_COMPONENTS, "n_components_present"), with = FALSE],
                                by = "AnimalNum"), episodes = des$episodes, thresholds = thr)
setkey(des_out$animals, AnimalNum)
lab <- des_out$animals[pop_sis_87 == TRUE]
lr <- data.table(x = c(R[, 2], R[, 3], R[, 4]), g = paste(rep(nod$Batch, 3L), rep(2:4, each = n87)))
ex <- list(n_episodes = 444L, sis_by_cohort = stats::setNames(c(10L, 16L, 16L, 15L, 15L, 15L), S33_COHORTS), n_focal = sum(focal),
           n_clusters = data.table::uniqueN(nod$cc1_cage[focal]), non_focal = nod$AnimalNum[!focal],
           dyads = c(CC1 = sum(pcls == "CC1"), CC2 = sum(pcls == "CC2"), CC3 = sum(pcls == "CC3"), CC4 = sum(pcls == "CC4"), NM = sum(pcls == "NM")),
           n_nmdc = sum(focal & is.finite(bf$mNMdc)), nmdc_clusters = data.table::uniqueN(nod$cc1_cage[focal & is.finite(bf$mNMdc)]),
           multi_cc_pairs = pn(pairs[ntog > 1L, , drop = FALSE]), v2_dropped_cc4 = pn(pairs[pcls == "CC1" & E[pairs[, 1], 4] == E[pairs[, 2], 4], , drop = FALSE]),
           closure_cohorts = c("B2", "B3"),
           nm_same_src = vapply(S33_COHORTS, function(b) sum(pcls == "NM" & SS[pairs] & nod$Batch[pairs[, 1]] == b), 0L),
           nm_total = vapply(S33_COHORTS, function(b) sum(pcls == "NM" & nod$Batch[pairs[, 1]] == b), 0L),
           units_by_cohort = stats::setNames(units_f[, .N, keyby = Batch]$N, S33_COHORTS),
           unit_singletons = nod[focal][paste(Batch, src_cage) %in% units_f[N == 1L, paste(Batch, src_cage)], AnimalNum],
           fe_cols = nrow(fe_units) + 5L, fe_df = sum(fe_units$N) - (nrow(fe_units) + 5L),
           s1 = stats::sd(stats::resid(stats::lm(R[, 1] ~ factor(nod$Batch)))), sL = stats::sd(stats::resid(stats::lm(x ~ factor(g), lr))),
           sW = stats::sd(stats::resid(stats::lm(nod$tp2 ~ factor(nod$Batch)))), tol_s1 = 1e-4, tol_sLW = 1e-3, vif_max = 4,
           sis_res = sum(lab$outcome_group == "RES"), sis_sus = sum(lab$outcome_group == "SUS"),
           focal_res = sum(lab[pop_focal_85 == TRUE]$outcome_group == "RES"), focal_sus = sum(lab[pop_focal_85 == TRUE]$outcome_group == "SUS"),
           s13_cohort = "B2", s3_cohort = "B3", s2_pair = c("B1", "B6"), s10_animal = mk$moved[1])
check(ex$n_focal == 85L && ex$n_clusters == 22L && length(ex$multi_cc_pairs) >= 1L && length(ex$v2_dropped_cc4) == 1L && length(ex$unit_singletons) == 1L &&
        ex$n_nmdc > 0L && ex$sis_res > 0L && ex$sis_sus > 0L, "fixture: 85 focal in 22 CC1 clusters, a CC1 pair at CC4, one pre-SIS cage singleton, labels of both kinds")
# the module's structure
st <- s33e_structure(des, ex); nodes <- st$nodes; n <- nrow(nodes)
check(identical(nodes$AnimalNum, nod$AnimalNum) && identical(nodes$focal, focal) && identical(nodes$focal, nodes$pop_focal_85),
      "nodes = the 87 tracked SIS animals in C-locale order; focal = at least one CC1 cage-mate = focal_85")
mcl <- matrix(NA_character_, n87, n87); mcl[cbind(st$dyads$ia, st$dyads$ib)] <- st$dyads$first_met_class
check(nrow(st$dyads) == nrow(pairs) && identical(mcl[pairs], pcls) && identical(st$dyads[order(ia, ib)]$n_cc_together, ntog[order(pairs[, 1], pairs[, 2])]),
      "first-met classes and shared-CC counts of all dyads = brute force")
R0 <- as.matrix(nodes[, paste0("r", 1:4), with = FALSE])
EX_all <- s33e_expo(lapply(st$mats, `[[`, "M"), R0, nodes$tp2, seq_len(n), S33_SD_RATE, S33E_SL, S33E_SW)
cols <- c("own", "m1", "m2", "m3", "m4", "mNM", "mNMdc", "m2_dc", "m3_dc", "m4_dc", "ownW", "wm1", "wm2", "wm3", "wm4", "wmNM",
          paste0(rep(c("own", "v", "l", "u"), each = 3L), 2:4))
check(all(vapply(cols, function(cn) near(EX_all[focal, cn], bf[[cn]][focal], 1e-12), TRUE)),
      paste("class means = brute force: CC1-CC4, never co-housed and its different-cage subset, S4 different-cage later classes, weights;",
            "version 2: C_k and class c(k) without CCk housemates, never co-housed, own CCk rate"))
m10 <- s33e_mats(n, st$od, st$tog, exclude_cc4 = which(nodes$AnimalNum == mk$moved[1]))
check(near(as.vector(m10$V4$M %*% R0[, 4])[focal] / S33E_SL, bf$v_s10_4[focal], 1e-12), "S10: the declared animal removed as a CC4 contributor")
ip <- which(nodes$AnimalNum == mk$moved[1]); iq <- which(nodes$AnimalNum == mk$moved[2])
check(st$mats$V4$M[ip, iq] == 0 && st$mats$K1$M[ip, iq] > 0 && st$mats$V2$M[ip, iq] > 0, "C_4 drops the CC1 mate housed with the animal again at CC4 (C_2 keeps it)")
idn <- EX_all[, "own"] + rowSums(sapply(c("m1", "m2", "m3", "m4", "mNM"), function(cn) { k <- c(m1 = "n_cc1", m2 = "n_cc2", m3 = "n_cc3", m4 = "n_cc4", mNM = "n_nm")[[cn]]
  ifelse(nodes[[k]] > 0L, EX_all[, cn] * nodes[[k]], 0) }))
check(max(abs(idn - stats::ave(R0[, 1], nodes$Batch, FUN = sum) / S33_SD_RATE)[focal]) < 1e-10, "GE-14 identity: own + sum_k n_k m_k + n_NM mNM = cohort sum (classes partition the cohort-mates)")
check(max(abs(S33_SD_RATE * EX_all[focal, "m1"] - bf$xbar_loo[focal])) < 1e-10, "GE-17 identity: s1 m1 = the leave-one-out CC1 cage-mate mean")
f <- which(nodes$focal); fm <- copy(nodes[f])
EXf <- cbind(EX_all[f, , drop = FALSE], sex_c = nodes$sex_c[f], age_cc1 = nodes$age_cc1[f], line_J = as.numeric(nodes$line_J[f]))
EXf <- cbind(EXf, own_sex = EXf[, "own"] * EXf[, "sex_c"], m1_sex = EXf[, "m1"] * EXf[, "sex_c"])
fm[, `:=`(nmdc_ok = is.finite(EXf[, "mNMdc"]), m1_raw_s1 = EXf[, "m1"])]
check(isTRUE(s33e_five_class_deficient(fm, EXf, c("B2", "B3"))) && identical(s33e_five_class_deficient(fm, EXf, "B5"), FALSE) &&
        identical(s33e_five_class_deficient(fm, EXf, character()), logical()), "the five-class model is rank-deficient where class sizes are equal (B2, B3), not in B5")
ok("classes, class means, version-2 / S4 / S10 sets, GE-14 and GE-17 identities, five-class rank deficiency")

cat("\n3. designs\n")
specs <- s33e_model_specs(); cspec <- s33e_contrast_specs()
fx1 <- s33e_fixed(c("B3", "B1", "B2", "B1"), "coh_")
check(identical(colnames(fx1), c("Intercept", "coh_B2", "coh_B3")) && identical(unname(fx1[, "coh_B3"]), c(1, 0, 0, 0)) &&
        identical(colnames(s33e_fixed(c("b", "a", "c"), "u")), c("Intercept", "u02", "u03")) && ncol(s33e_fixed(rep("B3", 4), "coh_")) == 1L,
      "fixed part: intercept and treatment dummies in C-locale radix order (numbered for pre-SIS cages); one level = intercept only")
tC <- s33e_template(specs$C, fm); XC <- s33e_assemble(tC, EXf)
fr <- data.table(fm[, .(AnimalNum, Batch, Sex, cc1_cage = e1, cc2_cage = e2, cc3_cage = e3, cc4_cage = e4, unit, unit_closure_s13, unit_dob_s13,
                        xmate_excluded)], EXf)
fr[, CombZ := des_out$animals$CombZ[match(AnimalNum, des_out$animals$AnimalNum)]]
fr[, `:=`(xbar_loo = bf$xbar_loo[match(AnimalNum, bf$AnimalNum)], x = own * S33_SD_RATE)]
fC <- CombZ ~ factor(Batch) + own + m1 + m2 + m3 + m4
check(dim(XC)[1] == 85L && dim(XC)[2] == 11L && near(stats::coef(s33e_lm(XC, fr$CombZ))[c("own", "m1", "m2", "m3", "m4")],
                                                     stats::coef(stats::lm(fC, fr))[c("own", "m1", "m2", "m3", "m4")], 1e-10),
      "Model C template: 85 x 11; its OLS = lm(CombZ ~ cohort + own + m1..m4)")
tN <- s33e_template(specs$NMdc, fm); tF <- s33e_template(specs$C_FE, fm); tV <- s33e_template(specs$V2L, fm)
check(length(tN$ix) == ex$n_nmdc && ncol(s33e_assemble(tN, EXf)) == 5L + 3L && length(tF$ix) == sum(fe_units$N) && ncol(s33e_assemble(tF, EXf)) == ex$fe_cols &&
        tF$n_units == nrow(fe_units) && tV$nb == 3L && length(tV$rows_all) == 255L && tV$cl$G == 22L,
      "NMdc rows, pre-SIS cage rows (units with >= 2 focal animals), version-2 stack of 255 rows in 22 CC1 clusters")
XV <- s33e_assemble(tV, EXf)
check(dim(XV)[2] == 30L && all(XV[1:85, 11:30] == 0) && all(XV[86:170, c(1:10, 21:30)] == 0) && all(XV[171:255, 1:20] == 0) &&
        identical(colnames(XV)[c(7:10, 27:30)], c("k2_own", "k2_ownk", "k2_v", "k2_l", "k4_own", "k4_ownk", "k4_v", "k4_l")),
      "stacked design is block-diagonal with one copy per CC (k2_, k3_, k4_ columns)")
check(must_error(s33e_L(colnames(XC), list(bad = c(m9 = 1)))) && identical(unname(s33e_L(colnames(XC), list(d = c(m1 = 1, m2 = -1)))[c("m1", "m2"), 1]), c(1, -1)),
      "contrast matrices; a weight on an absent coefficient stops")
check(identical(unname(s33e_v2w("l")), rep(c(1, -1) / 3, 3)) && identical(names(s33e_v2w("u", 3L, 1)), c("k3_v", "k3_u")),
      "version-2 weights: +-1/3 over CC2-CC4, +-1 within one CC")
ss <- s33e_sim_struct(st, fm); sc <- s33e_scenarios(); p1 <- as.list(sc[scenario_id == 1L])
chk16 <- s33e_check_stacked(ss, mt_seed(1L, function() s33e_sim_draw(ss, p1)), fm)
check(length(chk16$ok) == 12L && all(chk16$ok), "GE-16 fit part: stacked OLS = three separate fits; each stacked CR2 block = that fit's CR2")
set.seed(3); Zv <- cbind(Intercept = 1, g = rep(0:1, each = 20)); a1 <- stats::rnorm(40); a2 <- a1 + stats::rnorm(40, 0, 0.1); a3 <- stats::rnorm(40)
vv <- s33e_vif(cbind(Zv, a1 = a1, a2 = a2, a3 = a3), c("a1", "a2", "a3"))
Rz <- stats::resid(stats::lm(cbind(a1, a2, a3) ~ Zv[, "g"]))
vref <- vapply(1:3, function(j) 1 / (1 - summary(stats::lm(Rz[, j] ~ Rz[, -j]))$r.squared), 0)
check(near(vv, vref, 1e-8) && vv[["a2"]] > 4 && vv[["a3"]] < 1.5, "VIF after absorbing the fixed part = 1 / (1 - R^2)")
vr <- s33e_design_vif(fm, EXf, specs)
g10 <- s33e_gate_rank_vif(vr, s33e_five_class_deficient(fm, EXf, ex$closure_cohorts), ex$vif_max)
check(nrow(vr) == 12L && isTRUE(g10$passed), paste("GE-10 passes: planned designs full rank, VIF < 4 (", g10$detail, ")"))
check(!isTRUE(s33e_gate_rank_vif(vr, TRUE, 1)$passed) && !isTRUE(s33e_gate_rank_vif(vr, FALSE, 4)$passed) &&
        !isTRUE(s33e_gate_rank_vif(copy(vr)[1, rank := p - 1L], TRUE, 4)$passed) && !isTRUE(s33e_gate_rank_vif(vr[0], TRUE, 4)$passed) &&
        !isTRUE(s33e_gate_rank_vif(vr, logical(), 4)$passed), "GE-10 fails on a VIF at the bound, an identified five-class model, a rank deficiency, empty input")
ok("fixed parts, templates, stacked = separate fits, VIF, GE-10")

cat("\n4. CR2\n")
set.seed(4); yr <- stats::rnorm(nrow(fm))
for (m in c("C", "NMdc", "C_FE", "V2L", "W_C_FE")) {
  tpl <- s33e_template(specs[[m]], fm); X <- s33e_assemble(tpl, EXf)
  L <- s33e_L(colnames(X), lapply(Filter(function(z) identical(z$model, m), cspec), `[[`, "w"))
  dsg <- s33e_cr2_design(X, tpl$cl, L); r <- s33e_cr2_apply(dsg, X, yr[tpl$rows_all])
  fit <- s33e_lm(X, yr[tpl$rows_all]); club <- s33_cr2_contrast(fit, tpl$cl$levels[tpl$cl$gi], t(L))
  check(near(r$est, club$estimate, 1e-10) && near(r$se, club$se, 1e-10) && near(r$df / club$df, rep(1, length(r$df)), 1e-8),
        paste("manual CR2 = clubSandwich (estimate, SE, Satterthwaite df):", m))
  V1 <- as.matrix(clubSandwich::vcovCR(fit, cluster = tpl$cl$levels[tpl$cl$gi], type = "CR1"))
  check(near(s33e_cr1_se(dsg, r$u), sqrt(diag(t(L) %*% V1 %*% L)), 1e-10), paste("CR1 SE = clubSandwich CR1:", m))
}
set.seed(41); gz <- rep(1:6, times = c(4, 3, 5, 2, 4, 3)); Xz <- cbind(Intercept = 1, d2 = as.numeric(gz == 2), x = stats::rnorm(length(gz)))
yz <- stats::rnorm(length(gz)); clz <- s33e_clusters(sprintf("g%d", gz)); Lz <- s33e_L(colnames(Xz), list(a = c(x = 1)))
dz <- s33e_cr2_design(Xz, clz, Lz); rz <- s33e_cr2_apply(dz, Xz, yz)
cz <- s33_cr2_contrast(s33e_lm(Xz, yz), clz$levels[clz$gi], t(Lz))
check(near(rz$se, cz$se, 1e-10) && near(rz$df, cz$df, 1e-8), "a cluster absorbed by its own dummy (zero eigenvalue): pseudo-inverse as clubSandwich")
ok("CR2 SE and df = clubSandwich in single, subset, pre-SIS cage and stacked designs; CR1")

cat("\n5. wild cluster bootstrap-t\n")
lv <- sprintf("g%02d", 1:7)
ww <- s33e_wild_weights(lv, 50L, 33050101L)
idx <- mt_seed(33050101L, function() matrix(sample.int(6L, 7L * 50L, replace = TRUE), nrow = 7L))
check(identical(ww$idx, idx) && identical(dimnames(ww$w), list(lv, NULL)) && identical(unname(ww$w), matrix(WEBB[idx], nrow = 7L)) &&
        identical(ww$sha256, digest::digest(idx, algo = "sha256")), "Webb weights: one set.seed, matrix(sample.int(6, G * B, replace = TRUE), nrow = G)")
RNGkind("L'Ecuyer-CMRG"); invisible(s33e_wild_weights(lv, 5L, 1L)); kind_after <- RNGkind()[1]; RNGkind("Mersenne-Twister", "Inversion", "Rejection")
check(identical(kind_after, "L'Ecuyer-CMRG"), "the caller's RNG kind is restored")
set.seed(5); gw <- rep(1:7, times = c(5, 4, 1, 3, 5, 2, 4)); nw <- length(gw)
Xw <- cbind(Intercept = 1, x1 = stats::rnorm(nw), x2 = stats::rnorm(nw) + gw / 4)
yw <- drop(Xw %*% c(1, 0.5, -0.2)) + stats::rnorm(nw) + stats::rnorm(7)[gw]
clw <- s33e_clusters(sprintf("g%02d", gw)); Lw <- s33e_L(colnames(Xw), list(a = c(x1 = 1), b = c(x1 = 1, x2 = -1)))
dw <- s33e_cr2_design(Xw, clw, Lw); rw <- s33e_cr2_apply(dw, Xw, yw); yh <- drop(Xw %*% (dw$Bi %*% crossprod(Xw, yw)))
Bw <- 40L; W <- ww$w[clw$levels, seq_len(Bw)]
tv <- s33e_wild_tstar(dw, Xw, rw$u, W); tv1 <- s33e_wild_tstar(dw, Xw, rw$u, W, "CR1")
loop_club <- loop_man <- loop_cr1 <- matrix(NA_real_, 2L, Bw)
for (bb in seq_len(Bw)) {
  ys <- yh + W[clw$gi, bb] * rw$u; f2 <- s33e_lm(Xw, ys)
  cc <- s33_cr2_contrast(f2, clw$levels[clw$gi], t(Lw)); loop_club[, bb] <- (cc$estimate - rw$est) / cc$se
  r2 <- s33e_cr2_apply(dw, Xw, ys); loop_man[, bb] <- (r2$est - rw$est) / r2$se
  V1 <- as.matrix(clubSandwich::vcovCR(f2, cluster = clw$levels[clw$gi], type = "CR1"))
  loop_cr1[, bb] <- (drop(t(Lw) %*% stats::coef(f2)) - rw$est) / sqrt(diag(t(Lw) %*% V1 %*% Lw))
}
check(max(abs(tv - loop_club)) < 1e-8 && max(abs(tv - loop_man)) < 1e-10, "vectorized wild t* = loop refitting with clubSandwich CR2 = loop with the manual CR2")
check(max(abs(tv1 - loop_cr1)) < 1e-8, "CR1-studentized vectorized wild t* = loop with clubSandwich CR1")
ci <- s33e_wild_ci(c(1, 2), c(0.5, 1), rbind(c(-2, -1, 0, 1, 3), c(NA, -1, 1, 2, 4)))
q1 <- stats::quantile(c(-2, -1, 0, 1, 3), c(0.025, 0.975), type = 7); q2 <- stats::quantile(c(-1, 1, 2, 4), c(0.025, 0.975), type = 7)
check(near(ci$lo, c(1 - q1[[2]] * 0.5, 2 - q2[[2]]), 1e-12) && near(ci$hi, c(1 - q1[[1]] * 0.5, 2 - q2[[1]]), 1e-12),
      "interval [est - q.975(t*) se, est - q.025(t*) se], quantile type 7, non-finite draws dropped")
ok("weights, vectorized = loop = manual CR2 (CR2 and CR1), interval")

cat("\n6. simulation\n")
check(nrow(sc) == 24L && identical(sc$scenario_id, 1:24) && !anyDuplicated(sc$scenario), "24 scenarios in index order")
grid <- sc[scenario_id %in% 2:19]
irule <- with(grid, 2L + 6L * (match(ps, c(0.07, 0.14, 0.26)) - 1L) + 2L * (match(gs2, c(0.05, 0.15, 0.25)) - 1L) +
                (match(b2_truth_units, c("recorded", "dob_groups")) - 1L))
check(identical(as.integer(irule), grid$scenario_id) && nrow(unique(grid[, .(ps, gs2, b2_truth_units)])) == 18L && all(abs(grid$pt + grid$ps - 0.48) < 1e-12),
      "N1 grid: i = 2 + 6 (ps - 1) + 2 (gs - 1) + (truth - 1); pt = 0.48 - ps")
pv <- function(i, ...) { z <- sc[scenario_id == i]; a <- list(...); all(vapply(names(a), function(k) isTRUE(all.equal(z[[k]], a[[k]])), TRUE)) }
check(pv(10L, ps = 0.14, gs2 = 0.15, b2_truth_units = "recorded") && pv(1L, pt = 0.48, ps = 0, pg = 0, gs2 = 0, gz2 = 0, gg2 = 0, gh2 = 0, g1 = 0, gw1 = 0) &&
        pv(20L, pt = 0.44, pg = 0.04, gg2 = 0.10) && pv(21L, pt = 0.48, gh2 = 0.05) && pv(22L, pt = 0.35, ps = 0.13, gz2 = 0.15, gs2 = 0) &&
        pv(23L, pt = 0.35, ps = 0.13, gz2 = 0.30) && pv(24L, pt = 0.48, g1 = 0.3, gw1 = 0.3), "base cell 10; N0, N2, N3, N4a/b and P1 parameters")
check(identical(sc[size == "large", scenario_id], c(1L, 10L, 20:24)) && all(sc$pt + sc$ps + sc$pg <= 1) &&
        all(1 - 0.16 - sc$gs2 - sc$gz2 - sc$gg2 - 3 * sc$gh2 >= 0), "5000 replicates for i = 1, 10, 20-24; variance budgets non-negative")
Bd <- S33_B; Bd[["coverage_reps"]] <- 20L
check(identical(s33e_sim_sizes(list(B = S33_B)), c(large = 5000L, small = 2000L)) && identical(s33e_sim_sizes(list(B = Bd)), c(large = 100L, small = 40L)) &&
        identical(s33e_sim_sizes(list(B = c(Bd, sim_large = 7L, sim_small = 3L))), c(large = 7L, small = 3L)),
      "replicates 5000 / 2000 with the run's B; a development B scales them; explicit sizes win")
qq <- c("Delta_NM", "Delta_later", "Delta_NMdc", "Delta_NM_FE", "Delta_later_FE", "Delta_w_NM", "Delta_w_later", "Delta_CC2_vs_NM", "Delta_v2L", "A_noown_m1_minus_mNM")
tP <- s33e_truth(as.list(sc[scenario_id == 24L]), qq); tN <- s33e_truth(as.list(sc[scenario_id == 10L]), qq)
check(all(tP[c("Delta_NM", "Delta_later", "Delta_NMdc", "Delta_NM_FE", "Delta_later_FE", "A_noown_m1_minus_mNM", "Delta_w_NM", "Delta_w_later")] == 0.3) &&
        tP[["Delta_CC2_vs_NM"]] == 0 && is.na(tP[["Delta_v2L"]]) && all(tN == 0), "generating values: P1 0.3 (CC1-class contrasts), 0 (comparators), undefined (version 2); 0 elsewhere")
par <- list(pt = 0.3, ps = 0.2, pg = 0.1, gs2 = 0.1, gz2 = 0.1, gg2 = 0.1, gh2 = 0.05, g1 = 0.3, gw1 = 0.2, b2_truth_units = "dob_groups")
dat <- mt_seed(77L, function() s33e_sim_draw(ss, par))
ref <- mt_seed(77L, function() {   # the plan's model, animal by animal, in the declared draw order
  ui <- match(nodes$unit_dob_s13, sort(unique(nodes$unit_dob_s13), method = "radix")); bi <- match(nodes$Batch, S33_COHORTS)
  c1 <- match(nodes$e1, sort(unique(nodes$e1), method = "radix")); hk <- sort(unique(c(nodes$e2, nodes$e3, nodes$e4)), method = "radix")
  a_r <- matrix(stats::rnorm(24L), 6L, 4L); a_w <- stats::rnorm(6L); a_c <- stats::rnorm(6L); t_ <- stats::rnorm(n); s_ <- stats::rnorm(max(ui))
  z_ <- stats::rnorm(max(ui)); g_ <- stats::rnorm(max(c1)); h_ <- stats::rnorm(length(hk)); eps <- matrix(stats::rnorm(4L * n), n, 4L)
  epw <- stats::rnorm(n); e_ <- stats::rnorm(n); r <- matrix(NA_real_, n, 4L); w <- cz <- numeric(n)
  for (i in seq_len(n)) { for (k in 1:4) { pgk <- if (k == 1L) par$pg else 0
      r[i, k] <- a_r[bi[i], k] + sqrt(par$pt) * t_[i] + sqrt(par$ps) * s_[ui[i]] + sqrt(pgk) * g_[c1[i]] + sqrt(1 - par$pt - par$ps - pgk) * eps[i, k] }
    w[i] <- a_w[bi[i]] + sqrt(0.61) * s_[ui[i]] + sqrt(0.39) * epw[i] }
  for (i in seq_len(n)) { cm <- setdiff(which(nodes$e1 == nodes$e1[i]), i)
    cz[i] <- a_c[bi[i]] + 0.4 * t_[i] + sqrt(par$gs2) * s_[ui[i]] + sqrt(par$gz2) * z_[ui[i]] + sqrt(par$gg2) * g_[c1[i]] +
      sqrt(par$gh2) * sum(h_[match(c(nodes$e2[i], nodes$e3[i], nodes$e4[i]), hk)]) + par$g1 * (if (length(cm)) mean(r[cm, 1]) else 0) +
      par$gw1 * (if (length(cm)) mean(w[cm]) else 0) + sqrt(1 - 0.16 - par$gs2 - par$gz2 - par$gg2 - 3 * par$gh2) * e_[i] }
  list(r = r, w = w, cz = cz) })
check(max(abs(dat$r - ref$r)) < 1e-12 && max(abs(dat$w - ref$w)) < 1e-12 && max(abs(dat$cz - ref$cz)) < 1e-12 &&
        data.table::uniqueN(ss$u_dob[nodes$Batch == "B2"]) == 2L && data.table::uniqueN(ss$u_rec[nodes$Batch == "B2"]) == 6L,
      "generator = the plan's model (rates, weight, CombZ; truth units = DoB groups in B2)")
res <- list(est = matrix(c(1, 2, 3, NA), 4, 1, dimnames = list(NULL, "q")), se = matrix(1, 4, 1, dimnames = list(NULL, "q")),
            df = matrix(10, 4, 1, dimnames = list(NULL, "q")))
sm <- s33e_sim_summary(res, c(q = 0))
check(sm$R_finite == 3L && sm$mean == 2 && sm$sd == 1 && sm$mean_over_sd == 2 && near(sm$mcse_mean, 1 / sqrt(3), 1e-12) &&
        near(sm$mcse_mean_over_sd, sqrt(3 / 3), 1e-12) && near(sm$share_cr2_intervals_excluding_0, 1 / 3, 1e-12) &&
        near(sm$q025, stats::quantile(1:3, 0.025, type = 7), 1e-12) && near(sm$mean_width_cr2, 2 * stats::qt(0.975, 10), 1e-12) && sm$true_value == 0,
      "summary: mean, SD, mean/SD and their MCSE, quantiles, CR2 interval behaviour")
run3 <- s33e_sim_run(ss, p1, 3L, 33050201L)
check(identical(run3, s33e_sim_run(ss, p1, 3L, 33050201L)) && identical(colnames(run3$est), ss$quantities) && length(ss$quantities) == 22L,
      "a scenario is reproducible from its seed; 17 contrasts and 5 demonstrations")
cv <- s33e_sim_coverage(ss, p1, 3L, 9L, 33050201L, 33050401L, n_check = 3L, return_limits = TRUE)
qcv <- S33E_COVERAGE_CONTRASTS; tq <- stats::qt(0.975, run3$df[, qcv])
check(near(cv$limits$lo_cr2, run3$est[, qcv] - tq * run3$se[, qcv], 1e-10) && near(cv$limits$hi_cr2, run3$est[, qcv] + tq * run3$se[, qcv], 1e-10),
      "coverage replicates are the scenario's first replicates, regenerated from its seed")
check(nrow(cv$check) > 0L && all(cv$check$d_se < 1e-8) && all(cv$check$d_df < 1e-8), "manual CR2 = clubSandwich on the coverage replicates")
dats <- mt_seed(33050201L, function() lapply(1:3, function(i) s33e_sim_draw(ss, p1)))
Wst <- mt_seed(33050401L, function() lapply(1:3, function(i) matrix(sample.int(6L, 22L * 9L, replace = TRUE), nrow = 22L)))
for (m in c("C", "NMdc")) for (rp in 1:3) {
  tpl <- ss$tpl[[m]]; EXs <- s33e_expo(ss$Mf, dats[[rp]]$r, dats[[rp]]$w, ss$focal, 1, 1, 1); cn <- tpl$blocks[[1]]$cols
  d <- data.table(y = dats[[rp]]$cz[ss$focal][tpl$ix], Batch = fm$Batch[tpl$ix], cl = fm$e1[tpl$ix], EXs[tpl$ix, cn, drop = FALSE])
  fml <- stats::as.formula(paste("y ~ factor(Batch) +", paste(cn, collapse = " + ")))
  f0 <- stats::lm(fml, d); u0 <- stats::resid(f0); yh0 <- stats::fitted(f0)
  Wr <- matrix(WEBB[Wst[[rp]]], nrow = 22L, dimnames = list(ss$wild_levels, NULL)); gi <- match(d$cl, rownames(Wr))
  for (q in intersect(colnames(ss$L[[m]]), qcv)) {
    Lq <- stats::setNames(numeric(length(stats::coef(f0))), names(stats::coef(f0))); Lq[names(cspec[[q]]$w)] <- cspec[[q]]$w
    se_of <- function(fit) sqrt(drop(t(Lq) %*% as.matrix(clubSandwich::vcovCR(fit, cluster = d$cl, type = "CR2")) %*% Lq))
    est <- sum(Lq * stats::coef(f0)); se <- se_of(f0)
    ts <- vapply(1:9, function(bb) { d2 <- copy(d); d2[, y := yh0 + Wr[gi, bb] * u0]; f2 <- stats::lm(fml, d2); (sum(Lq * stats::coef(f2)) - est) / se_of(f2) }, 0)
    qs <- stats::quantile(ts, c(0.025, 0.975), type = 7, names = FALSE)
    check(near(c(cv$limits$lo_wild[rp, q], cv$limits$hi_wild[rp, q]), c(est - qs[2] * se, est - qs[1] * se), 1e-8),
          paste("coverage wild interval = loop (weights from the coverage seed, one 22 x B matrix per replicate):", q, "replicate", rp))
  }
}
ok("scenarios, generator, truths, summaries, sizes, coverage = loop")

cat("\n7. gates: passing and failing fixtures\n")
scales <- c(s1 = S33_SD_RATE, sL = S33E_SL, sW = S33E_SW)
gsx <- s33e_gates_structure(des, st, fm, EX_all, scales, ex)
check(identical(gsx$rows$gate_id, c("GE-03", "GE-04", "GE-08", "GE-14", "GE-15", "GE-17")) && all(gsx$rows$passed) && all(gsx$ge16_sets),
      paste("structure gates pass on the fixture:", paste(gsx$rows[passed != TRUE, paste(gate_id, detail)], collapse = " | ")))
gbad <- function(id, fe = ex, st_ = st, fm_ = fm, EX_ = EX_all) { g <- s33e_gates_structure(des, st_, fm_, EX_, scales, fe)$rows; !isTRUE(g[gate_id == id, passed]) }
check(gbad("GE-03", modifyList(ex, list(dyads = ex$dyads + c(0L, 0L, 0L, 0L, 1L)))) && gbad("GE-03", modifyList(ex, list(n_nmdc = ex$n_nmdc + 1L))) &&
        gbad("GE-03", modifyList(ex, list(non_focal = ex$non_focal[1]))), "GE-03 fails on a wrong dyad count, NMdc count or non-focal set")
check(gbad("GE-04", modifyList(ex, list(multi_cc_pairs = c(ex$multi_cc_pairs, "S601|S602")))) &&
        gbad("GE-04", modifyList(ex, list(multi_cc_pairs = ex$multi_cc_pairs[-1]))), "GE-04 fails on an undeclared or a missing multi-CC pair")
check(gbad("GE-04", modifyList(ex, list(nm_same_src = ex$nm_same_src + 1L))) && gbad("GE-04", modifyList(ex, list(closure_cohorts = c("B2", "B5")))),
      "GE-04 fails on a wrong source-cage sharing table or a cohort whose never co-housed sets do not close")
check(gbad("GE-08", modifyList(ex, list(s1 = ex$s1 + 0.01))) && !isTRUE(s33e_gates_structure(des, st, fm, EX_all, c(s1 = 5.17, sL = S33E_SL, sW = S33E_SW), ex)$rows[gate_id == "GE-08", passed]),
      "GE-08 fails on a recomputed scale away from the declared one or a scale other than the frozen constant")
EXb <- EX_all; EXb[which(focal)[3], "m2"] <- EXb[which(focal)[3], "m2"] + 1e-6
check(gbad("GE-14", EX_ = EXb), "GE-14 fails when a class mean breaks the within-cohort identity")
check(gbad("GE-15", modifyList(ex, list(unit_singletons = character()))) && gbad("GE-15", modifyList(ex, list(units_by_cohort = ex$units_by_cohort + 1L))),
      "GE-15 fails on a wrong singleton or unit count")
fmb <- copy(fm); fmb[2, m1_raw_s1 := m1_raw_s1 + 1e-9]
check(gbad("GE-17", fm_ = fmb) && gbad("GE-17", modifyList(ex, list(n_focal = 84L))), "GE-17 fails beyond 1e-10 or on a wrong focal count")
g15 <- s33e_gate_units(gsx$rows[gate_id == "GE-15"], vr, ex)
check(isTRUE(g15$passed) && !isTRUE(s33e_gate_units(gsx$rows[gate_id == "GE-15"], vr, modifyList(ex, list(fe_cols = ex$fe_cols + 1L)))$passed) &&
        !isTRUE(s33e_gate_units(gsx$rows[gate_id == "GE-15"], vr[design_id != "W_C_FE"], ex)$passed) &&
        !isTRUE(s33e_gate_units(copy(gsx$rows[gate_id == "GE-15"])[, passed := FALSE], vr, ex)$passed), "GE-15 (with the pre-SIS cage designs): pass and fail")
g16 <- s33e_gate_v2(gsx, chk16)
gsb <- s33e_gates_structure(des, st, fm, EX_all, scales, modifyList(ex, list(v2_dropped_cc4 = "x|y")))
check(isTRUE(g16$passed) && !isTRUE(s33e_gate_v2(gsx, list(ok = c(TRUE, FALSE), detail = ""))$passed) && !isTRUE(s33e_gate_v2(gsb, chk16)$passed),
      "GE-16 passes; fails on a stacked-fit mismatch or a wrong dropped pair")
g5 <- s33e_gate_labels(des_out, ex)
check(isTRUE(g5$passed) && grepl("GC-7a, GC-7b", g5$detail), "GE-05 passes on consistent labels (bundle identity held by GC-7a/GC-7b)")
lb <- function(f) { d2 <- des_out; d2$animals <- copy(des_out$animals); f(d2$animals); !isTRUE(s33e_gate_labels(d2, ex)$passed) }
sis1 <- des_out$animals[pop_sis_87 == TRUE, AnimalNum][1]
check(!isTRUE(s33e_gate_labels(des_out, modifyList(ex, list(focal_sus = ex$focal_sus + 1L)))$passed) &&
        lb(function(a) a[AnimalNum == sis1, outcome_group := fifelse(outcome_group == "SUS", "RES", "SUS")]) &&
        lb(function(a) a[AnimalNum == sis1, CombZ := NA_real_]) && lb(function(a) a[AnimalNum == sis1, n_components_present := 4L]),
      "GE-05 fails on a wrong count, a label against its threshold, a missing CombZ, a wrong component count")
prd <- list(e01_exposures_animal = data.table(a = 1, tier = "t"), e02_dyads = data.table(b = 2, tier = "t"), e05_simulation = data.table(c = 3, tier = "t"),
            e07_age_descriptive = data.table(d = 4, tier = "t"))
p2t <- list(products = prd, state = list(product_sha256 = vapply(prd, s33e_sha, "")))
p2b <- p2t; p2b$products$e02_dyads <- data.table(b = 3, tier = "t")
p2c <- p2t; p2c$products$e01_exposures_animal <- data.table(a = 1, CombZ = 0, tier = "t"); p2c$state$product_sha256 <- vapply(p2c$products, s33e_sha, "")
p2d <- p2t; p2d$products$e07_age_descriptive <- NULL
check(isTRUE(s33e_gate_products(p2t)$passed) && !isTRUE(s33e_gate_products(p2b)$passed) && !isTRUE(s33e_gate_products(p2c)$passed) &&
        !isTRUE(s33e_gate_products(p2d)$passed), "GE-09 passes on unchanged outcome-free products; fails on a change, an outcome column, a missing product")
gx <- data.table(d_se = c(1e-12, 2e-12), d_df = c(0, 1e-12))
check(isTRUE(s33e_gate_cr2(gx, gx)$passed) && !isTRUE(s33e_gate_cr2(copy(gx)[1, d_se := 1e-6], gx)$passed) && !isTRUE(s33e_gate_cr2(gx[0], gx[0])$passed) &&
        !isTRUE(s33e_gate_cr2(copy(gx)[2, d_df := NA_real_], NULL)$passed), "GE-11 passes within 1e-8; fails beyond, on NA and when empty")
ok("GE-03, 04, 05, 08, 09, 10, 11, 14, 15, 16, 17")

cat("\n8. the module on the synthetic design\n")
wmf <- file.path(fx, "window_metrics_long.csv")
set.seed(8); wm <- rbindlist(lapply(c("A1", "A2", "A3", "A4", "A5"), function(ph) nod[, .(AnimalNum, Batch, CC = "CC1", phase = ph,
  crossing_rate = if (ph == "A1") r1 else r1 + stats::rnorm(.N, 0, 3), metrics_used = ph != "A5", Group = "SIS")]))
wm <- rbind(wm, nod[, .(AnimalNum, Batch, CC = "CC2", phase = "A1", crossing_rate = r2, metrics_used = TRUE, Group = "SIS")])
wm[AnimalNum == nod$AnimalNum[3] & phase == "A2", metrics_used := FALSE]
fwrite(wm, wmf)
rb7 <- wm[CC == "CC1" & phase %in% paste0("A", 1:4) & metrics_used == TRUE, .(rbar = mean(crossing_rate)), keyby = AnimalNum][nod$AnimalNum, rbar]
s7r <- stats::sd(stats::resid(stats::lm(rb7 ~ factor(nod$Batch))))
Bt <- S33_B; Bt[c("wild", "coverage_reps", "coverage_wild")] <- c(19L, 3L, 9L); Bt <- c(Bt, sim_large = 4L, sim_small = 2L)
ctx <- list(seeds = S33_SEEDS, B = Bt, checkpoint_dir = NULL, inputs = list(s32_window_metrics = wmf), e_expect = ex, run_mode = "test",
            checkpoint_log = new.env())
check(must_error(s33e_phase2(list(animals = copy(des$animals)[, CombZ := 0], episodes = des$episodes), ctx)), "Phase 2 refuses a design carrying an outcome column")
p2 <- s33e_phase2(des, ctx)
check(setequal(p2$gates$gate_id, c("GE-03", "GE-04", "GE-08", "GE-10", "GE-14", "GE-15", "GE-16", "GE-17")) && all(p2$gates$passed),
      paste("Phase 2 gates pass:", paste(p2$gates[passed != TRUE, paste(gate_id, substr(detail, 1, 120))], collapse = " | ")))
check(identical(names(p2$products), S33E_OUTCOME_FREE_PRODUCTS) && all(vapply(p2$products, function(x) isTRUE(s33_assert_outcome_free(x)), TRUE)) &&
        nrow(p2$products$e05_simulation) == 24L * 22L && nrow(p2$products$e02_dyads) == nrow(pairs) && nrow(p2$products$e07_age_descriptive) == 6L,
      "Phase-2 products as declared and outcome-free (e05 24 x 22, e02 all dyads, e07 6)")
e01 <- p2$products$e01_exposures_animal
check(nrow(e01) == 87L && all(c("AnimalNum", "focal", "tp2", "src_cage", "s1", "m1") %in% names(e01)) && identical(unique(e01$s1), S33_SD_RATE) &&
        near(e01$tp2, nod$tp2, 0) && identical(e01$src_cage, nod$src_cage) && near(e01$own7[focal], rb7[focal] / s7r, 1e-12) &&
        near(e01$m1_7[focal], vapply(which(focal), function(i) mean(rb7[mates(i, "CC1")]), 0) / s7r, 1e-12) && near(e01$v4_s10, bf$v_s10_4, 1e-12),
      "e01: cross columns (AnimalNum, focal, tp2, src_cage, s1, m1); S7 A1-A4 means (metrics_used rows) on their own SD; S10 sets")
e05 <- p2$products$e05_simulation
check(all(e05[scenario_id %in% S33E_COVERAGE_SCENARIOS & quantity %in% S33E_COVERAGE_CONTRASTS, is.finite(coverage_cr2) & is.finite(coverage_wild)]) &&
        all(is.na(e05[!scenario_id %in% S33E_COVERAGE_SCENARIOS, coverage_cr2])) && identical(unique(e05[scenario_id == 3L, seed]), S33_SEEDS[["E_scenario_3"]]) &&
        all(e05$R_planned == ifelse(e05$scenario_id %in% c(1L, 10L, 20:24), 4L, 2L)) && all(e05$model_id == ss$q_model[e05$quantity]),
      "e05: coverage only for the declared scenarios and contrasts; seeds and replicate counts by scenario")
out <- s33e_phase3(des_out, p2, ctx)
check(identical(out$gates$gate_id, c("GE-05", "GE-09", "GE-11")) && all(out$gates$passed), paste("Phase 3 gates pass:", paste(out$gates[passed != TRUE, detail], collapse = " | ")))
Tb <- out$tables
check(identical(names(Tb), S33_TABLES$E) && length(out$audit) == 0L && identical(S33_AUDIT_TABLES$E, character()) && isTRUE(s33_table_gates(list(E = out), "E")$passed),
      "declared tables, no audit table; GC-8 passes")
lint <- s33_lint(Tb, character(), s33_lint_patterns(cfg$MMM_BEHAVIOR_CONFIG), exempt_columns = s33_lint_exempt_columns("E"))
check(nrow(lint) == 0L && "engine_messages" %in% s33_lint_exempt_columns("E"), paste("lint clean (GC-9):", paste(lint$where, lint$pattern, collapse = "; ")))
check(identical(Tb$e01_exposures_animal, p2$products$e01_exposures_animal) && identical(Tb$e05_simulation, p2$products$e05_simulation),
      "Phase-2 products are the final e01, e02, e05, e07 tables")
e04 <- Tb$e04_contrasts
check(identical(e04$estimand, names(cspec)) && nrow(e04) == 17L && all(e04$status == "OK") && all(e04$interval_method == "CR2_Satterthwaite_cc1_cage") &&
        all(e04$interval_basis == "conditional_on_cohorts") && all(e04$level == "animal_within_cohort") &&
        identical(e04[lead == TRUE, estimand], c("Delta_NM", "Delta_NM_FE", "Delta_later", "Delta_later_FE", "Delta_NMdc", "Delta_v2L", "Delta_w_later", "Delta_w_later_FE")),
      "e04: 17 contrasts in the declared order, CR2 by CC1 cage, lead rows")
refit_lm <- function(fml, d, w, cl) { fit <- stats::lm(fml, d); b <- stats::coef(fit); Lv <- stats::setNames(numeric(length(b)), names(b)); Lv[names(w)] <- w
  lc <- clubSandwich::linear_contrast(fit, vcov = clubSandwich::vcovCR(fit, cluster = cl, type = "CR2"),
                                      contrasts = matrix(Lv, 1L, dimnames = list(NULL, names(Lv))), test = "Satterthwaite", p_values = FALSE)
  c(est = lc$Est, se = lc$SE, df = lc$df) }
cmp <- function(id, v) { r <- e04[estimand == id]; near(c(r$estimate, r$se), v[c("est", "se")], 1e-8) && near(r$df / v[["df"]], 1, 1e-6) }
later <- c(m1 = 1, m2 = -1/3, m3 = -1/3, m4 = -1/3)
check(cmp("Delta_NM", refit_lm(fC, fr, c(m1 = 1), fr$cc1_cage)) && cmp("Delta_later", refit_lm(fC, fr, later, fr$cc1_cage)) &&
        cmp("Delta_CC3_vs_NM", refit_lm(fC, fr, c(m3 = 1), fr$cc1_cage)), "Model C contrasts = lm on brute-force class means, clubSandwich CR2 by CC1 cage")
frd <- fr[is.finite(mNMdc)]; fre <- fr[unit %in% fr[, .N, by = unit][N >= 2L, unit]]
check(cmp("Delta_NMdc", refit_lm(CombZ ~ factor(Batch) + own + m1 + mNMdc, frd, c(m1 = 1, mNMdc = -1), frd$cc1_cage)) &&
        cmp("Delta_later_FE", refit_lm(CombZ ~ factor(unit) + own + m1 + m2 + m3 + m4, fre, later, fre$cc1_cage)) &&
        cmp("Delta_w_NM_FE", refit_lm(CombZ ~ factor(unit) + ownW + wm1 + wm2 + wm3 + wm4, fre, c(wm1 = 1), fre$cc1_cage)) &&
        cmp("Delta_w_later", refit_lm(CombZ ~ factor(Batch) + ownW + wm1 + wm2 + wm3 + wm4, fr, c(wm1 = 1, wm2 = -1/3, wm3 = -1/3, wm4 = -1/3), fr$cc1_cage)),
      "NMdc rows, pre-SIS cage intercepts and weight models = lm refits")
check(cmp("Delta_v2L_CC3", refit_lm(CombZ ~ factor(Batch) + own + own3 + v3 + l3, fr, c(v3 = 1, l3 = -1), fr$cc1_cage)), "version 2 within one CC = its separate fit")
stk <- rbindlist(lapply(2:4, function(k) fr[, .(CombZ, Batch, cc1_cage, kf = factor(k, levels = 2:4), own, ownk = get(paste0("own", k)),
                                                v = get(paste0("v", k)), l = get(paste0("l", k)))]))
check(cmp("Delta_v2L", refit_lm(CombZ ~ 0 + kf + kf:factor(Batch) + kf:own + kf:ownk + kf:v + kf:l, stk,
                                stats::setNames(rep(c(1, -1) / 3, each = 3L), c(paste0("kf", 2:4, ":v"), paste0("kf", 2:4, ":l"))), stk$cc1_cage)),
      "Delta_v2L = stacked lm with copy interactions, CR2 by CC1 cage across the copies")
sdm1 <- stats::sd(stats::resid(stats::lm(m1 ~ factor(Batch), fr)))
check(near(e04[estimand == "Delta_NM", estimate_per_exposure_sd], e04[estimand == "Delta_NM", estimate] * sdm1, 1e-10) &&
        near(e04[estimand == "Delta_later", ci_low_per_exposure_sd], e04[estimand == "Delta_later", ci_low] * sdm1, 1e-10),
      "per within-cohort SD of the CC1-class exposure")
lev <- sort(unique(fr$cc1_cage), method = "radix"); Bw2 <- ctx$B[["wild"]]
Ww <- matrix(WEBB[mt_seed(S33_SEEDS[["E_wild"]], function() matrix(sample.int(6L, length(lev) * Bw2, replace = TRUE), nrow = length(lev)))], nrow = length(lev))
gwl <- match(fr$cc1_cage, lev); fC0 <- stats::lm(fC, fr); u0 <- stats::resid(fC0); yh0 <- stats::fitted(fC0)
Ln <- stats::setNames(numeric(length(stats::coef(fC0))), names(stats::coef(fC0))); Ln["m1"] <- 1
se_n <- function(fit, d) sqrt(drop(t(Ln) %*% as.matrix(clubSandwich::vcovCR(fit, cluster = d$cc1_cage, type = "CR2")) %*% Ln))
est0 <- sum(Ln * stats::coef(fC0)); se0 <- se_n(fC0, fr)
tsw <- vapply(seq_len(Bw2), function(bb) { d2 <- copy(fr); d2[, CombZ := yh0 + Ww[gwl, bb] * u0]; f2 <- stats::lm(fC, d2); (sum(Ln * stats::coef(f2)) - est0) / se_n(f2, d2) }, 0)
qw <- stats::quantile(tsw, c(0.025, 0.975), type = 7, names = FALSE)
check(near(unlist(e04[estimand == "Delta_NM", .(wild_ci_low, wild_ci_high)]), c(est0 - qw[2] * se0, est0 - qw[1] * se0), 1e-8) &&
        e04[estimand == "Delta_NM", wild_B] == Bw2 && e04$wild_seed[1] == S33_SEEDS[["E_wild"]],
      "e04 wild interval = a loop refitting with clubSandwich CR2 under the E_wild Webb weights")
e06 <- Tb$e06_sensitivities
check(setequal(unique(e06$sensitivity_id), c(paste0("S", 1:13), "S15", "S16")) && e06[sensitivity_id == "S11", .N] == 4L * 17L &&
        e06[sensitivity_id == "S16", .N] == 3L && e06[sensitivity_id == "S12", .N] == 6L && e06[sensitivity_id == "S13", .N] == 12L,
      "e06: S1-S13, S15, S16 (S14 = the label of every row); S11 four methods x 17, S12 by sex, S13 two unit variants")
lb6 <- e06[is.finite(estimate) & is.finite(primary_estimate) & is.finite(primary_se)]
check(nrow(lb6) > 100L && identical(lb6$robust_label, mmm_ci_robust_label(lb6$primary_estimate, lb6$primary_se, lb6$estimate)) &&
        all(e06$robust_label_rule == S33E_ROBUST_RULE), "S14: every sensitivity row labelled by the frozen rule against its primary")
frx <- fr[xmate_excluded == FALSE]
check(near(e06[sensitivity_id == "S1" & model_id == "S1_C" & estimand == "Delta_NM", estimate], stats::coef(stats::lm(fC, frx))[["m1"]], 1e-10) &&
        near(e06[sensitivity_id == "S3" & estimand == "Delta_later", estimate], sum(later * stats::coef(stats::lm(update(fC, . ~ . - factor(Batch)), fr[Batch == "B3"]))[names(later)]), 1e-10) &&
        all(e06[sensitivity_id == "S3", interval_method] == "none"), "S1 without the cages that held an excluded animal; S3 one cohort, point estimates only")
e08 <- Tb$e08_jackknife_cc1cage; cg <- lev[5]
check(nrow(e08) == 44L && setequal(e08$left_out_cc1_cage, lev) &&
        near(e08[left_out_cc1_cage == cg & estimand == "Delta_NM", estimate], stats::coef(stats::lm(fC, fr[cc1_cage != cg]))[["m1"]], 1e-10) &&
        near(e06[sensitivity_id == "S8" & estimand == "Delta_later", estimate], range(e08[estimand == "Delta_later", estimate]), 1e-12),
      "e08: leave one CC1 cage out (22 x 2); S8 rows = its range")
check(!any(e08$lead) && !any(e06$lead), "sensitivity rows (e06, e08) are never lead rows")
s6r <- function(sid, lab, mid, id) e06[sensitivity_id == sid & sensitivity %like% lab & model_id == mid & estimand == id]
cf <- function(fml, d, w) { b <- stats::coef(stats::lm(fml, d)); sum(w * b[names(w)]) }
fe2 <- function(d, ucol) d[get(ucol) %in% d[, .N, by = ucol][N >= 2L][[ucol]]]
e1f <- Tb$e01_exposures_animal[match(fr$AnimalNum, AnimalNum)]
fr7 <- copy(fr)[, `:=`(own7 = e1f$own7, m1_7 = e1f$m1_7, m2_7 = e1f$m2_7, m3_7 = e1f$m3_7, m4_7 = e1f$m4_7, v4s = e1f$v4_s10, u4s = e1f$u4_s10)]
frP <- fr7[AnimalNum != mk$moved[1]]
v2nm <- mean(c(cf(CombZ ~ factor(Batch) + own + own2 + v2 + u2, frP, c(v2 = 1, u2 = -1)), cf(CombZ ~ factor(Batch) + own + own3 + v3 + u3, frP, c(v3 = 1, u3 = -1)),
               cf(CombZ ~ factor(Batch) + own + own4 + v4s + u4s, frP, c(v4s = 1, u4s = -1))))
fr4 <- fr[is.finite(m2_dc) & is.finite(m3_dc) & is.finite(m4_dc)]
check(near(s6r("S2", "B4$", "S2_C", "Delta_NM")$estimate, cf(fC, fr[Batch != "B4"], c(m1 = 1)), 1e-10) &&
        near(s6r("S4", "", "S4_C", "Delta_later")$estimate, cf(CombZ ~ factor(Batch) + own + m1 + m2_dc + m3_dc + m4_dc, fr4,
                                                                c(m1 = 1, m2_dc = -1/3, m3_dc = -1/3, m4_dc = -1/3)), 1e-10) &&
        near(s6r("S5", "", "S5_C", "Delta_NM")$estimate, cf(update(fC, . ~ . + age_cc1 + line_J), fr, c(m1 = 1)), 1e-10) &&
        near(s6r("S7", "", "S7_C", "Delta_NM")$estimate, cf(CombZ ~ factor(Batch) + own7 + m1_7 + m2_7 + m3_7 + m4_7, fr7, c(m1_7 = 1)), 1e-10) &&
        near(s6r("S9", "", "S9_A", "Delta_NM_A")$estimate, cf(CombZ ~ factor(Batch) + own + m1 + mNM, fr, c(m1 = 1, mNM = -1)), 1e-10) &&
        near(s6r("S10", "", "S10_V2N", "Delta_v2NM")$estimate, v2nm, 1e-10) &&
        near(s6r("S12", "^Female", "S12_C", "Delta_NM")$estimate, cf(fC, fr[Sex == "Female"], c(m1 = 1)), 1e-10) &&
        near(s6r("S13", "closure", "S13_C_FE", "Delta_NM_FE")$estimate,
             cf(CombZ ~ factor(unit_closure_s13) + own + m1 + m2 + m3 + m4, fe2(fr, "unit_closure_s13"), c(m1 = 1)), 1e-10) &&
        near(s6r("S13", "DoB", "S13_W_C_FE", "Delta_w_later_FE")$estimate, cf(CombZ ~ factor(unit_dob_s13) + ownW + wm1 + wm2 + wm3 + wm4,
                                                                             fe2(fr, "unit_dob_s13"), c(wm1 = 1, wm2 = -1/3, wm3 = -1/3, wm4 = -1/3)), 1e-10),
      "sensitivity refits = lm: S2 (leave B4 out), S4, S5, S7 (A1-A4 means), S9 (Model A), S10 (without the moved animal), S12, S13 (closure, DoB)")
fC0 <- stats::lm(fC, fr); se_ols <- sqrt(stats::vcov(fC0)["m1", "m1"])
s11 <- e06[sensitivity_id == "S11" & estimand == "Delta_NM"]
V1 <- as.matrix(clubSandwich::vcovCR(fC0, cluster = fr$cc1_cage, type = "CR1"))
check(near(s11[interval_method == "t_74", c(se, ci_low)], c(se_ols, s11[interval_method == "t_74", estimate] - stats::qt(0.975, 74) * se_ols), 1e-10) &&
        near(s11[interval_method == "CR1_t_G-1", c(se, df)], c(sqrt(V1["m1", "m1"]), 21), 1e-10) &&
        near(s11[interval_method == "CR2_Satterthwaite_src_cage", se], refit_lm(fC, fr, c(m1 = 1), fr$unit)[["se"]], 1e-8) &&
        s11[interval_method == "wild_cluster_bootstrap_t", .N] == 1L && is.finite(s11[interval_method == "wild_cluster_bootstrap_t", ci_low]),
      "S11: OLS t(74), CR1 with t(G - 1), CR1-studentized wild, CR2 by recorded pre-SIS cage")
s16 <- e06[sensitivity_id == "S16" & estimand == "Delta_later"]
check(near(c(s16$se, s16$df), refit_lm(fC, fr, later, fr$cc4_cage)[c("se", "df")], 1e-8) && s16$interval_method == "CR2_Satterthwaite_cc4_cage",
      "S16: CR2 by CC4 cage")
s6d <- data.table(fr[, .(AnimalNum, Batch, CombZ, own, m1, m2, m3, m4, cc1_cage, cc2_cage, cc3_cage, cc4_cage, src_unit = unit)])
lm6 <- suppressMessages(lme4::lmer(CombZ ~ Batch + own + m1 + m2 + m3 + m4 + (1 | cc1_cage) + (1 | cc2_cage) + (1 | cc3_cage) + (1 | cc4_cage) + (1 | src_unit),
                  data = s6d, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5))))
s6x <- e06[sensitivity_id == "S6" & estimand == "Delta_later"]
check(abs(s6x$estimate - sum(later * lme4::fixef(lm6)[names(later)])) < 1e-6 && s6x$interval_method %in% c("KR", "Satterthwaite_fallback") &&
        Tb$e03_models[model_id == "S6_S6_unit", n_params] == 11L, "S6: crossed random intercepts (frozen engine, rank 11) = lmer fixed effects; KR interval")
e03 <- Tb$e03_models
check(all(e03$status == "OK") && !any(e03$adjustment_coefficients_reported) && e03[model_id == "C", n_params] == 11L && e03[model_id == "C_FE", residual_df] == ex$fe_df &&
        e03[engine != "OLS (lm)", .N] == 3L && all(is.finite(e03[engine == "OLS (lm)", max_vif])), "e03: one row per fit; adjustment coefficients never reported; S6 and S13 lmer rows")
ck_dir <- file.path(fx, "ck"); cctx <- list(checkpoint_dir = ck_dir, checkpoint_log = new.env(), head = "h", run_mode = "test", input_sha256 = "x")
k1 <- s33e_ckpt(cctx, "unit", function() list(v = 1:3), key_extra = list(seed = 1L))
k2 <- s33e_ckpt(cctx, "unit", function() stop("must not recompute"), key_extra = list(seed = 1L))
k3c <- s33e_ckpt(cctx, "unit", function() list(v = 4:6), key_extra = list(seed = 2L))
check(!k1$row$reused && k2$row$reused && identical(k1$value, k2$value) && !k3c$row$reused && k3c$row$checkpoint_key != k1$row$checkpoint_key &&
        length(ls(cctx$checkpoint_log)) == 3L, "checkpoints: stored, reused under the same key, recomputed under another key; logged")
# cross contract with a mock module C whose S3 is the frozen-engine fit of C's formula on the same animals
d3 <- fr[, .(AnimalNum, Batch, CageEpisodeID = cc1_cage, CombZ, x, xbar_loo, sex_c)]
m3 <- s33_kr_fit("CombZ ~ Batch + x + xbar_loo + x:sex_c + xbar_loo:sex_c + (1 | CageEpisodeID)", d3, "test_C_S3", 10L)
k3 <- s33_kr_contrast(m3, list(xbar_loo = 1), "delta_peer")
mockC <- list(tables = list(), cross = list(focal = fr$AnimalNum, cages = lev, tp2 = des$animals[pop_focal_85 == TRUE, .(AnimalNum, tp2)],
                                            src_cage = des$animals[pop_focal_85 == TRUE, .(AnimalNum, src_cage)], s1 = S33_SD_RATE,
                                            xbar_loo = fr[, .(AnimalNum, xbar_loo)], s3_delta_peer_per_sd = k3$estimate * S33_SD_RATE,
                                            s3_singular = isTRUE(k3$singular)))
resCE <- list(C = mockC, E = out)
xg <- s33_cross_gates(resCE, list(), c("C", "E"))
check(all(xg[gate_id %in% c("X3", "X4", "X6", "X7"), passed]) && all(!xg[gate_id %in% c("X1", "X2", "X5"), evaluated]),
      paste("cross contract: X3, X4, X6, X7 pass with module C; X1, X2, X5 not evaluated", paste(xg[passed != TRUE, detail], collapse = " | ")))
check(isTRUE(k3$singular) && isTRUE(xg[gate_id == "X8", passed]), "X8: E's S15 = C's S3 per frozen SD within 1e-6 when S3 is singular")
filled <- s33_cross_fill(resCE, c("C", "E"))$E$tables$e06_sensitivities
check(near(filled[sensitivity_id == "S15", c_s3_delta_peer_per_frozen_sd], k3$estimate * S33_SD_RATE, 1e-12) &&
        all(is.na(filled[sensitivity_id != "S15", c_s3_delta_peer_per_frozen_sd])) && all(is.na(e06$c_s3_delta_peer_per_frozen_sd)),
      "S15 fill: C's S3 beside E's S15 only; E leaves the column empty")
ok("Phase 2 and 3, tables, GC-8, lint, refits, wild loop, e06/e08, checkpoints, cross contract")

cat("\n9. S15 = C's S3 after rescaling; no p or chance column\n")
s15 <- e06[sensitivity_id == "S15"]
f15 <- stats::lm(CombZ ~ factor(Batch) + own + m1 + own:sex_c + m1:sex_c, fr)
f3 <- stats::lm(CombZ ~ factor(Batch) + x + xbar_loo + x:sex_c + xbar_loo:sex_c, d3)
check(nrow(s15) == 1L && s15$estimand == "Delta_CC1_vs_all_others" && near(s15$estimate, stats::coef(f15)[["m1"]], 1e-10) &&
        near(s15$estimate, stats::coef(f3)[["xbar_loo"]] * S33_SD_RATE, 1e-10) && s15$interval_method == "CR2_Satterthwaite_cc1_cage",
      "S15 (sex-averaged m1 coefficient, CR2) = C's S3 delta_peer x s1 (OLS, exact)")
check(abs(s15$estimate - k3$estimate * S33_SD_RATE) < 1e-6, "S15 = C's S3 frozen-engine lmer per frozen SD within 1e-6 (S3 singular)")
pcol <- s33_lint_patterns()$p_columns; allc <- unlist(lapply(Tb, names))
check(!any(vapply(pcol, function(p) any(grepl(p, allc, perl = TRUE)), TRUE)) && !any(grepl("(?i)chance|probab|p_val|pval|reject", allc, perl = TRUE)),
      "no p, chance or test column in any table")
ok("S15 bridge and column names")

unlink(fx, recursive = TRUE)
cat(sprintf("\nPASS: stage 33 module E (%.0f s)\n", as.numeric(difftime(Sys.time(), T0, units = "secs"))))
