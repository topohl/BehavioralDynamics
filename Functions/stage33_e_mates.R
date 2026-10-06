# ================================================================
# Stage 33 - module E: classes of cohort-mates (E_cagemates)
# MMMSociability
# ================================================================
# Plan: docs/STAGE33_POSTHOC_COHORT_FOLLOWUPS_PLAN_v1.0.md, section 12 (with the shared sections 0-7 and 13-17).
# Post hoc, descriptive (estimation only). Within cohort, adjusted for the animal's own CC1 A1 RFID position-change rate,
# contrasts between classes of cohort-mates in how CombZ covaries with their mean CC1 A1 rate: CC1 cage-mates; cage-mates
# first met at CC2, CC3 or CC4; cohort-mates never co-housed during CC1-CC4 (NM); never co-housed cohort-mates from a
# different recorded pre-SIS cage (NMdc). Own rate plus the sum over all cohort-mates is fixed within a cohort (GE-14),
# so only class contrasts are identified; own-term coefficients are never reported (they overlap the registered H03
# quantity). E's CC1-class exposure m1 equals C's leave-one-out cage-mate mean / s1 exactly (GE-17, X7): E's CC1-class
# contrasts and C's contextual term are one association, counted once.
#
# Entry points (MODULE CONTRACT in Functions/stage33_common.R):
#   p2  <- s33e_phase2(design, ctx)          classes, dyads, exposures, age table, simulations and coverage (outcome-free)
#   out <- s33e_phase3(design_out, p2, ctx)  OLS fits with CR2 and wild cluster bootstrap-t intervals, sensitivities
# Definitions only: sourcing this file reads and writes nothing. Requires data.table, digest and clubSandwich; S6 needs
# the frozen engine (Functions/stage32_inference.R and its sources) and S14 mmm_ci_robust_label()
# (Functions/rfid_canonical_inference.R). No p-value or test statistic is produced: clubSandwich is called with
# p_values = FALSE (s33_cr2_contrast) and the frozen engine's p and statistic are dropped (s33_kr_contrast).
# Seeds come only from ctx$seeds (E_wild, E_scenario_<i>, E_coverage_<i>), replicate counts only from ctx$B.
#
# Readings of the plan (reported as deviations):
#   - GE-05 checks the labels in design_out; the identity of CombZ and outcome_group with the bundle is GC-7a/GC-7b: the
#     outcomes are read once, by s33_attach_outcomes() (plan section 6), never again by a module.
#   - N1 grid: pt = 0.48 - ps, as in N2 and N4 where the rate components add to 0.48 (source plan E).
#   - Simulation replicates are the plan's 5000 / 2000 in the run; a development ctx with reduced B scales them
#     (s33e_sim_sizes). Coverage contrasts, which the plan does not list, are the source plan's seven.
#   - S14 is the robust_label column of every e06 row (rule text in robust_label_rule), not separate rows.
#   - Level of every E estimate: animal_within_cohort (animal-level regressions within cohort).
#   - Wild weights: one G x B matrix over the 22 focal CC1 cages (C-locale order); a model on fewer cages uses their rows.
# ================================================================

# ---------------------------------------------------------------- constants (plan sections 7 and 12)
S33E_OUTCOME_FREE_PRODUCTS <- c("e01_exposures_animal", "e02_dyads", "e05_simulation", "e07_age_descriptive")
S33E_SL <- 6.2383        # CC2-CC4 A1 rates: n - 1 residual SD on cohort x CC in sis_87 (261 rows); recomputed value is GE-08
S33E_SW <- 1.6143        # CC1-day weight tp2 (g): n - 1 residual SD on cohort in sis_87; recomputed value is GE-08
S33E_CLASSES <- c("CC1", "CC2", "CC3", "CC4", "NM")
S33E_WEBB <- c(-sqrt(1.5), -1, -sqrt(0.5), sqrt(0.5), 1, sqrt(1.5))   # Webb six-point weights, indexed by sample.int(6)
S33E_V2_LATER <- c("2" = 3L, "3" = 4L, "4" = 2L)    # version 2: the comparator of CCk is the class first met at CC c(k)
S33E_SIM_R <- c(large = 5000L, small = 2000L)        # replicates per scenario (plan: 5000 for i = 1, 10, 20-24, else 2000)
S33E_SIM_LARGE <- c(1L, 10L, 20L, 21L, 22L, 23L, 24L)
S33E_COVERAGE_SCENARIOS <- c(1L, 10L, 21L, 22L, 23L)
S33E_COVERAGE_CONTRASTS <- c("Delta_NM", "Delta_later", "Delta_NMdc", "Delta_later_FE", "Delta_v2L", "Delta_w_later",
                             "Delta_w_later_FE")
S33E_CR2_CHECK_REPS <- 20L                            # coverage replicates whose manual CR2 is checked against clubSandwich
S33E_LEVEL <- "animal_within_cohort"
S33E_BASIS <- "conditional_on_cohorts"
S33E_CI_CR2 <- "CR2_Satterthwaite_cc1_cage"
S33E_SIM_NOTE <- paste("simulation summary: expected shift under shared-origin settings and interval behaviour under the assumed",
                       "settings; never a correction")
S33E_ONE_ASSOCIATION <- paste("E's CC1-class contrasts and C's contextual term are one association (CC1 cage-mates' mean",
                              "rate), counted once; same variation as C's S3 (X7, X8)")
# Engine-message columns of the module tables (GC-9 skips them; read by s33_lint_exempt_columns()).
S33E_LINT_EXEMPT_COLUMNS <- c("engine_messages")
# Readings of the plan recorded in the README (s33_deviations()).
S33E_DEVIATIONS <- c(
  paste("GE-05 checks the labels in design_out (CombZ finite; RES/SUS from the within-sex thresholds; components counted;",
        "53/34 in sis_87 and 53/32 in focal_85); their identity with the bundle is GC-7a/GC-7b's, because outcomes are read once."),
  "N1 grid: pt = 0.48 - ps (the plan gives pt for N0, N2 and N4 only; this matches N2, N4 and the source plan).",
  paste("Coverage contrasts (not listed in the frozen plan) are the source plan's seven: Delta_NM, Delta_later, Delta_NMdc,",
        "Delta_later_FE, Delta_v2L, Delta_w_later, Delta_w_later_FE; the first 20 coverage replicates per scenario are",
        "compared with clubSandwich (GE-11)."),
  "S14 is the robust_label column on every e06 row (frozen mmm_ci_robust_label; rule in robust_label_rule), not separate rows.",
  paste("Every E estimate has level animal_within_cohort (animal-level OLS within cohort); module C labels the same",
        "association (its contextual term) cage_within_cohort."),
  paste("Wild weights: one 22 x B Webb matrix over the focal CC1 cages in C-locale order (stream E_wild); a model on fewer",
        "cages uses those cages' rows; stacked copies of a cage share its weight."),
  paste("Per-exposure-SD rows use the within-cohort SD over the model rows; version 2 scales copy k's exposures by sd(v_k),",
        "as the source plan does."),
  "The S15 row's estimand id is Delta_CC1_vs_all_others (the plan names the bridge without an id).",
  paste("Section 12 caveats are carried in an e04 'caveats' column; the caveat on other recorded source cages reads 'may still",
        "be kin (kinship unknown)', because the lint bars the plan's own word."),
  "GE-04 records, without gating, the later-class pre-SIS sharing counts (the source plan's counts predate the recorded B2 cages).")
# S14: the frozen robustness rule of every sensitivity row against its primary row.
S33E_ROBUST_RULE <- paste("S14: frozen mmm_ci_robust_label(primary estimate, primary CR2 SE, sensitivity estimate): SIGN_CHANGE if the",
                          "signs differ and both |estimates| >= 0.5 SE; LARGE_SHIFT if |shift| >= 1 SE; otherwise COMPATIBLE; descriptive")
# Caveats of plan section 12 (e04, every row).
S33E_CAVEATS <- paste(
  "post hoc, descriptive; only contrasts between classes are identified (own rate plus all cohort-mates is fixed within a cohort);",
  "the never co-housed class is mostly former pre-SIS cage-mates, so Delta_NM is origin-shifted; later cage-mates lived with",
  "the animal later; pre-SIS cage intercepts rely on the recorded pre-SIS cages (B2 from a planning file); cohort-mates from",
  "another recorded pre-SIS cage may still be kin (kinship unknown); peer associations share the reflection problem; 22 CC1",
  "clusters; version 2 includes CC1 carry-over; sex is nested in cohort; within-cohort only; 17 contrasts without",
  "multiplicity control;", paste0(S33E_ONE_ASSOCIATION, "; transparency: E scratch c11, k02, k03 printed outcome values,"),
  "no decision used them")
S33E_UNITS <- c(
  s1 = "CombZ units per frozen SD (5.1698 position changes/hour) of the class-mean CC1 A1 rate",
  sL = "CombZ units per SD sL (6.2383 position changes/hour; n - 1 residual SD of CC2-CC4 A1 rates on cohort x CC) of the class-mean CCk A1 rate",
  sW = "CombZ units per SD sW (1.6143 g; n - 1 residual SD of tp2 on cohort) of the class-mean CC1-day weight",
  s7 = "CombZ units per n - 1 residual SD on cohort of the CC1 A1-A4 mean rate (position changes/hour)",
  exposure_sd = "CombZ units per within-cohort SD (n - 1, on cohort, model rows) of the CC1-class exposure")
S33E_TP2_LABEL <- "CC1-day body weight (tp2, g)"
# Structural facts of the real design (plan sections 3 and 12; GE-03, GE-04, GE-08, GE-15, GE-16, GE-05). ctx$e_expect may
# replace this list (synthetic tests); the module reads nothing else from it.
S33E_EXPECT <- list(
  n_episodes = 444L, sis_by_cohort = c(B1 = 10L, B2 = 16L, B3 = 16L, B4 = 15L, B5 = 15L, B6 = 15L),
  n_focal = 85L, n_clusters = 22L, non_focal = c("OQ770", "OQ771"),
  dyads = c(CC1 = 123L, CC2 = 119L, CC3 = 119L, CC4 = 117L, NM = 122L),
  n_nmdc = 56L, nmdc_clusters = 20L,
  multi_cc_pairs = c("690|OR646", "694|OR646"), v2_dropped_cc4 = "690|OR646",
  closure_cohorts = c("B2", "B3"),
  nm_same_src = c(B1 = 9L, B2 = 13L, B3 = 1L, B4 = 18L, B5 = 11L, B6 = 12L),
  nm_total = c(B1 = 9L, B2 = 24L, B3 = 24L, B4 = 21L, B5 = 21L, B6 = 23L),
  units_by_cohort = c(B1 = 4L, B2 = 6L, B3 = 6L, B4 = 5L, B5 = 6L, B6 = 6L), unit_singletons = "OR568",
  fe_cols = 37L, fe_df = 47L,
  s1 = 5.1698, sL = 6.2383, sW = 1.6143, tol_s1 = 1e-4, tol_sLW = 1e-3, vif_max = 4,
  sis_res = 53L, sis_sus = 34L, focal_res = 53L, focal_sus = 32L,
  s13_cohort = "B2", s3_cohort = "B3", s2_pair = c("B1", "B6"), s10_animal = "OR646")

# ---------------------------------------------------------------- classes of cohort-mates (plan section 12)
#' First-met class of animal pairs: the first CC at which the two share a v2 CEID, else 'NM'. ea, eb: n x 4 CEID matrices.
s33e_first_met <- function(ea, eb) {
  ea <- matrix(ea, ncol = 4L); eb <- matrix(eb, ncol = 4L)
  same <- ea == eb; same[is.na(same)] <- FALSE
  k <- max.col(matrix(as.numeric(same), ncol = 4L), ties.method = "first")
  ifelse(rowSums(same) > 0L, paste0("CC", k), "NM")
}

#' Tracked SIS animals (sis_87, the exposure contributors) with CC1-CC4 CEIDs and A1 rates from the bundle episodes, in
#' C-locale radix order of AnimalNum. Outcome-free design columns only.
s33e_nodes <- function(design) {
  a <- data.table::as.data.table(design$animals)
  a <- a[a$pop_sis_87 %in% TRUE, c("AnimalNum", "Batch", "Sex", "src_cage", "line", "line_J", "dob", "age_cc1", "tp2",
                                     "pop_focal_85", "xmate_excluded"), with = FALSE]
  ep <- data.table::as.data.table(design$episodes)
  ep <- ep[ep$AnimalNum %in% a$AnimalNum, c("AnimalNum", "CC", "CageEpisodeID", "crossing_rate"), with = FALSE]
  w <- data.table::dcast(ep, AnimalNum ~ CC, value.var = c("CageEpisodeID", "crossing_rate"))
  for (k in 1:4) {
    if (!paste0("CageEpisodeID_CC", k) %in% names(w)) w[, (paste0("CageEpisodeID_CC", k)) := NA_character_]
    if (!paste0("crossing_rate_CC", k) %in% names(w)) w[, (paste0("crossing_rate_CC", k)) := NA_real_]
  }
  data.table::setnames(w, c(paste0("CageEpisodeID_CC", 1:4), paste0("crossing_rate_CC", 1:4)), c(paste0("e", 1:4), paste0("r", 1:4)))
  n <- merge(a, w[, c("AnimalNum", paste0("e", 1:4), paste0("r", 1:4)), with = FALSE], by = "AnimalNum", all.x = TRUE)
  n <- n[order(n$AnimalNum, method = "radix")]
  n[, `:=`(unit = paste(Batch, src_cage, sep = "|"), sex_c = data.table::fifelse(Sex == "Female", 0.5, -0.5))]
  n[]
}

#' The unordered tracked SIS dyads within cohorts (plan: 600 = CC1 123, CC2 119, CC3 119, CC4 117, NM 122).
s33e_dyads <- function(nodes) {
  E <- as.matrix(nodes[, paste0("e", 1:4), with = FALSE])
  by_b <- split(seq_len(nrow(nodes)), nodes$Batch)
  pr <- data.table::rbindlist(lapply(sort(names(by_b), method = "radix"), function(b) {
    ii <- by_b[[b]]
    if (length(ii) < 2L) return(NULL)
    cb <- utils::combn(length(ii), 2L)
    data.table::data.table(Batch = b, ia = ii[cb[1L, ]], ib = ii[cb[2L, ]]) }))
  ea <- E[pr$ia, , drop = FALSE]; eb <- E[pr$ib, , drop = FALSE]
  same <- ea == eb; same[is.na(same)] <- FALSE
  pr[, `:=`(animal_a = nodes$AnimalNum[pr$ia], animal_b = nodes$AnimalNum[pr$ib], first_met_class = s33e_first_met(ea, eb),
            n_cc_together = as.integer(rowSums(same)),
            ccs_together = apply(same, 1L, function(z) if (any(z)) paste(S33_CC[z], collapse = ";") else "none"),
            same_src_cage = nodes$src_cage[pr$ia] == nodes$src_cage[pr$ib], same_dob = nodes$dob[pr$ia] == nodes$dob[pr$ib])]
  pr[, nm_subclass := data.table::fifelse(first_met_class == "NM", data.table::fifelse(same_src_cage, "sc", "dc"), NA_character_)]
  pr[]
}

#' Row-averaging matrix over the selected ordered pairs (a = focal node, b = contributing node); rows with an empty set NA.
s33e_avg <- function(n, a, b, keep) {
  keep <- keep %in% TRUE
  M <- matrix(0, n, n)
  if (any(keep)) M[cbind(a[keep], b[keep])] <- 1
  k <- rowSums(M)
  M[k > 0, ] <- M[k > 0, , drop = FALSE] / k[k > 0]
  M[k == 0, ] <- NA_real_
  list(M = M, n = as.integer(k))
}

#' All class sets as averaging matrices. K1-K4: first met at CCk; NM; NMdc (NM from a different recorded pre-SIS cage);
#' Kkdc (S4: later class k from a different recorded pre-SIS cage); version 2 (k = 2..4): Vk = CC1 mates not housed with
#' the animal at CCk, Lk = the class first met at CC c(k) minus CCk housemates, Uk = NM. `exclude_cc4`: node indices
#' removed as CC4 contributors (S10).
s33e_mats <- function(n, od, tog, exclude_cc4 = integer()) {
  sel <- function(cond) s33e_avg(n, od$a, od$b, cond)
  m <- list()
  for (k in 1:4) m[[paste0("K", k)]] <- sel(od$cls == paste0("CC", k))
  m$NM <- sel(od$cls == "NM")
  m$NMdc <- sel(od$cls == "NM" & !od$same_src)
  for (k in 2:4) {
    keep_b <- if (k == 4L) !od$b %in% exclude_cc4 else rep(TRUE, nrow(od))
    m[[paste0("K", k, "dc")]] <- sel(od$cls == paste0("CC", k) & !od$same_src)
    m[[paste0("V", k)]] <- sel(od$cls == "CC1" & !tog[, k] & keep_b)
    m[[paste0("L", k)]] <- sel(od$cls == paste0("CC", S33E_V2_LATER[[as.character(k)]]) & !tog[, k] & keep_b)
    m[[paste0("U", k)]] <- sel(od$cls == "NM" & keep_b)
  }
  m
}

#' Exposures (plan section 12): own = r1/s1; m_k, mNM, mNMdc = class means of r1 / s1; S4 m_k_dc; version 2 own_k = r_k/sL,
#' v_k, l_k, u_k = class means of r_k / sL; weight ownW = tp2/sW, wm_k, wmNM. M: averaging matrices whose rows are the
#' output animals (columns = all nodes); r: nodes x 4 A1 rates (CC1-CC4); w: node weights; own_rows: the output animals.
s33e_expo <- function(M, r, w, own_rows, s1, sL, sW) {
  p <- function(nm, x) as.vector(M[[nm]] %*% x)
  cbind(own = r[own_rows, 1] / s1, m1 = p("K1", r[, 1]) / s1, m2 = p("K2", r[, 1]) / s1, m3 = p("K3", r[, 1]) / s1,
        m4 = p("K4", r[, 1]) / s1, mNM = p("NM", r[, 1]) / s1, mNMdc = p("NMdc", r[, 1]) / s1,
        m2_dc = p("K2dc", r[, 1]) / s1, m3_dc = p("K3dc", r[, 1]) / s1, m4_dc = p("K4dc", r[, 1]) / s1,
        own2 = r[own_rows, 2] / sL, own3 = r[own_rows, 3] / sL, own4 = r[own_rows, 4] / sL,
        v2 = p("V2", r[, 2]) / sL, v3 = p("V3", r[, 3]) / sL, v4 = p("V4", r[, 4]) / sL,
        l2 = p("L2", r[, 2]) / sL, l3 = p("L3", r[, 3]) / sL, l4 = p("L4", r[, 4]) / sL,
        u2 = p("U2", r[, 2]) / sL, u3 = p("U3", r[, 3]) / sL, u4 = p("U4", r[, 4]) / sL,
        ownW = w[own_rows] / sW, wm1 = p("K1", w) / sW, wm2 = p("K2", w) / sW, wm3 = p("K3", w) / sW, wm4 = p("K4", w) / sW,
        wmNM = p("NM", w) / sW)
}

#' Never co-housed closure class of every node (sorted ID string of the animal and its NM set).
s33e_closure <- function(nodes, dy) {
  nm <- dy[dy$first_met_class == "NM"]
  vapply(nodes$AnimalNum, function(a) paste(sort(c(a, nm$animal_b[nm$animal_a == a], nm$animal_a[nm$animal_b == a]), method = "radix"),
                                           collapse = ";"), "", USE.NAMES = FALSE)
}

#' The outcome-free structure of module E: nodes, dyads, ordered pairs, class matrices, units and their S13 variants
#' (closure classes and DoB groups of the S13 cohort), focal flags.
s33e_structure <- function(design, expect = S33E_EXPECT) {
  nodes <- s33e_nodes(design); n <- nrow(nodes)
  dy <- s33e_dyads(nodes)
  E <- as.matrix(nodes[, paste0("e", 1:4), with = FALSE])
  od <- data.table::data.table(a = c(dy$ia, dy$ib), b = c(dy$ib, dy$ia), cls = rep(dy$first_met_class, 2L),
                               same_src = rep(dy$same_src_cage, 2L))
  tog <- E[od$a, , drop = FALSE] == E[od$b, , drop = FALSE]; tog[is.na(tog)] <- FALSE
  mats <- s33e_mats(n, od, tog)
  cnt <- vapply(c(paste0("K", 1:4), "NM", "NMdc"), function(k) mats[[k]]$n, integer(n))
  if (is.null(dim(cnt))) cnt <- matrix(cnt, nrow = 1L, dimnames = list(NULL, c(paste0("K", 1:4), "NM", "NMdc")))
  nodes[, `:=`(n_cc1 = cnt[, "K1"], n_cc2 = cnt[, "K2"], n_cc3 = cnt[, "K3"], n_cc4 = cnt[, "K4"], n_nm = cnt[, "NM"],
               n_nm_dc = cnt[, "NMdc"], n_nm_sc = cnt[, "NM"] - cnt[, "NMdc"])]
  nodes[, focal := n_cc1 >= 2L]
  clos <- s33e_closure(nodes, dy)
  s13 <- expect$s13_cohort %s33or% character()
  cid <- match(clos, sort(unique(clos), method = "radix"))
  nodes[, `:=`(unit_closure_s13 = data.table::fifelse(Batch %in% s13, paste0(Batch, "|nm_closure_", cid), unit),
               unit_dob_s13 = data.table::fifelse(Batch %in% s13, paste0(Batch, "|dob_", format(dob)), unit))]
  fu <- nodes[nodes$focal, .N, by = unit]
  nodes[, unit_singleton := focal & unit %in% fu$unit[fu$N == 1L]]
  list(nodes = nodes, dyads = dy, od = od, tog = tog, mats = mats, closure = clos)
}

# ---------------------------------------------------------------- designs, contrasts and model specifications
#' Fixed part: intercept and treatment dummies of a cohort or pre-SIS cage factor (levels in C-locale radix order).
s33e_fixed <- function(f, prefix) {
  f <- as.character(f); lv <- sort(unique(f), method = "radix")
  D <- matrix(1, length(f), 1L, dimnames = list(NULL, "Intercept"))
  if (length(lv) > 1L) {
    Z <- matrix(vapply(lv[-1], function(l) as.numeric(f == l), numeric(length(f))), nrow = length(f))
    colnames(Z) <- if (identical(prefix, "u")) sprintf("u%02d", seq_along(lv)[-1]) else paste0(prefix, lv[-1])
    D <- cbind(D, Z)
  }
  D
}

#' Cluster structure (levels in C-locale radix order, row index per level).
s33e_clusters <- function(cluster) {
  cluster <- as.character(cluster); lv <- sort(unique(cluster), method = "radix"); gi <- match(cluster, lv)
  list(levels = lv, gi = gi, rows = split(seq_along(gi), factor(gi, levels = seq_along(lv))), G = length(lv))
}

#' Model specifications (plan section 12). rows: focal (85), nmdc (focal with >= 1 NMdc member), fe (pre-SIS cages with
#' >= 2 model rows); absorb: cohort (Batch) or pre-SIS cage; stacked: version-2 copies k = 2..4 (every term per copy).
s33e_model_specs <- function() list(
  C = list(rows = "focal", absorb = "Batch", terms = c("own", "m1", "m2", "m3", "m4"),
           formula = "CombZ ~ cohort + own + m1 + m2 + m3 + m4"),
  NMdc = list(rows = "nmdc", absorb = "Batch", terms = c("own", "m1", "mNMdc"), formula = "CombZ ~ cohort + own + m1 + mNMdc"),
  C_FE = list(rows = "fe", absorb = "unit", terms = c("own", "m1", "m2", "m3", "m4"),
              formula = "CombZ ~ pre-SIS cage + own + m1 + m2 + m3 + m4"),
  V2L = list(rows = "focal", absorb = "Batch", stacked = 2:4, terms = c("own", "own_k", "v_k", "l_k"),
             formula = "stacked k = 2..4: CombZ ~ 0 + k + k:cohort + k:own + k:own_k + k:v_k + k:l_k"),
  V2N = list(rows = "focal", absorb = "Batch", stacked = 2:4, terms = c("own", "own_k", "v_k", "u_k"),
             formula = "stacked k = 2..4: CombZ ~ 0 + k + k:cohort + k:own + k:own_k + k:v_k + k:u_k"),
  W_C = list(rows = "focal", absorb = "Batch", terms = c("ownW", "wm1", "wm2", "wm3", "wm4"),
             formula = "CombZ ~ cohort + ownW + wm1 + wm2 + wm3 + wm4"),
  W_C_FE = list(rows = "fe", absorb = "unit", terms = c("ownW", "wm1", "wm2", "wm3", "wm4"),
                formula = "CombZ ~ pre-SIS cage + ownW + wm1 + wm2 + wm3 + wm4"),
  A = list(rows = "focal", absorb = "Batch", terms = c("own", "m1", "mNM"), formula = "CombZ ~ cohort + own + m1 + mNM"),
  A_noown = list(rows = "focal", absorb = "Batch", terms = c("m1", "mNM"), formula = "CombZ ~ cohort + m1 + mNM (simulation only)"),
  V2L_noownk = list(rows = "focal", absorb = "Batch", stacked = 2:4, terms = c("own", "v_k", "l_k"),
                    formula = "stacked k = 2..4 without own_k (simulation only)"),
  S15 = list(rows = "focal", absorb = "Batch", terms = c("own", "m1", "own_sex", "m1_sex"),
             formula = "CombZ ~ cohort + own + m1 + own:sex_c + m1:sex_c"))

#' Version-2 weights: +wt on every k_v, -wt on every k_<cmp>.
s33e_v2w <- function(cmp, ks = 2:4, wt = 1 / length(ks))
  stats::setNames(rep(c(wt, -wt), length(ks)), as.vector(rbind(paste0("k", ks, "_v"), paste0("k", ks, "_", cmp))))

#' The 17 e04 contrasts (plan section 12) and the simulation-only demonstrations, in e04 order (17.2 E).
s33e_contrast_specs <- function() {
  later <- c(m1 = 1, m2 = -1/3, m3 = -1/3, m4 = -1/3); wlater <- c(wm1 = 1, wm2 = -1/3, wm3 = -1/3, wm4 = -1/3)
  nm_lab <- "cohort-mates not housed with the animal during CC1-CC4 (in B1, B2 and B4-B6 mostly its pre-SIS cage-mates)"
  dc_lab <- "never co-housed cohort-mates from a different recorded pre-SIS cage (kinship unknown)"
  later_lab <- "the mean of the three later cage-mate classes (first met at CC2, CC3 or CC4)"
  adj <- "adjusted for own CC1 A1 rate; within cohort"
  fe <- "; with pre-SIS cage intercepts (companion)"
  x <- list(
    list(id = "Delta_NM", model = "C", w = c(m1 = 1), role = "reported first; origin-shifted", lead = TRUE, expo = "m1", scale = "s1",
         label = paste0("CC1 cage-mates minus ", nm_lab, "; origin-shifted; ", adj)),
    list(id = "Delta_NM_FE", model = "C_FE", w = c(m1 = 1), role = "companion", lead = TRUE, expo = "m1", scale = "s1",
         label = paste0("CC1 cage-mates minus ", nm_lab, "; ", adj, fe)),
    list(id = "Delta_later", model = "C", w = later, role = "lead", lead = TRUE, expo = "m1", scale = "s1",
         label = paste0("CC1 cage-mates minus ", later_lab, "; ", adj)),
    list(id = "Delta_later_FE", model = "C_FE", w = later, role = "companion", lead = TRUE, expo = "m1", scale = "s1",
         label = paste0("CC1 cage-mates minus ", later_lab, "; ", adj, fe)),
    list(id = "Delta_NMdc", model = "NMdc", w = c(m1 = 1, mNMdc = -1), role = "lead", lead = TRUE, expo = "m1", scale = "s1",
         label = paste0("CC1 cage-mates minus ", dc_lab, "; ", adj)),
    list(id = "Delta_v2L", model = "V2L", w = s33e_v2w("l"), role = "lead (version 2)", lead = TRUE, expo = "v", scale = "sL",
         label = paste("CC1 cage-mates' A1 rates at CC2-CC4 (recorded in cages without the animal) minus an equal-size later class",
                       "at the same CC, mean over CC2-CC4; adjusted for own CC1 and own CCk A1 rates; within cohort; includes CC1 carry-over")),
    list(id = "Delta_w_later", model = "W_C", w = wlater, role = "lead (weight)", lead = TRUE, expo = "wm1", scale = "sW",
         label = paste0("CC1-day weight of CC1 cage-mates minus ", later_lab, "; adjusted for own CC1-day weight; within cohort")),
    list(id = "Delta_w_later_FE", model = "W_C_FE", w = wlater, role = "companion", lead = TRUE, expo = "wm1", scale = "sW",
         label = paste0("CC1-day weight of CC1 cage-mates minus ", later_lab, "; adjusted for own CC1-day weight; within cohort", fe)),
    list(id = "Delta_CC2_vs_NM", model = "C", w = c(m2 = 1), role = "comparator (later cage-mates lived with the animal later)",
         lead = FALSE, expo = "m1", scale = "s1", label = paste0("cage-mates first met at CC2 minus ", nm_lab, "; ", adj)),
    list(id = "Delta_CC3_vs_NM", model = "C", w = c(m3 = 1), role = "comparator (later cage-mates lived with the animal later)",
         lead = FALSE, expo = "m1", scale = "s1", label = paste0("cage-mates first met at CC3 minus ", nm_lab, "; ", adj)),
    list(id = "Delta_CC4_vs_NM", model = "C", w = c(m4 = 1), role = "comparator (later cage-mates lived with the animal later)",
         lead = FALSE, expo = "m1", scale = "s1", label = paste0("cage-mates first met at CC4 minus ", nm_lab, "; ", adj)),
    list(id = "Delta_v2L_CC2", model = "V2L", w = s33e_v2w("l", 2L, 1), role = "version 2, one CC", lead = FALSE, expo = "v", scale = "sL",
         label = "CC1 cage-mates' A1 rates at CC2 minus the class first met at CC3 (rates at CC2); adjusted for own CC1 and CC2 A1 rates"),
    list(id = "Delta_v2L_CC3", model = "V2L", w = s33e_v2w("l", 3L, 1), role = "version 2, one CC", lead = FALSE, expo = "v", scale = "sL",
         label = "CC1 cage-mates' A1 rates at CC3 minus the class first met at CC4 (rates at CC3); adjusted for own CC1 and CC3 A1 rates"),
    list(id = "Delta_v2L_CC4", model = "V2L", w = s33e_v2w("l", 4L, 1), role = "version 2, one CC", lead = FALSE, expo = "v", scale = "sL",
         label = "CC1 cage-mates' A1 rates at CC4 minus the class first met at CC2 (rates at CC4); adjusted for own CC1 and CC4 A1 rates"),
    list(id = "Delta_v2NM", model = "V2N", w = s33e_v2w("u"), role = "secondary (version 2)", lead = FALSE, expo = "v", scale = "sL",
         label = paste("CC1 cage-mates' A1 rates at CC2-CC4 minus never co-housed cohort-mates at the same CC, mean over CC2-CC4;",
                       "adjusted for own CC1 and own CCk A1 rates; origin-shifted")),
    list(id = "Delta_w_NM", model = "W_C", w = c(wm1 = 1), role = "secondary (weight)", lead = FALSE, expo = "wm1", scale = "sW",
         label = paste0("CC1-day weight of CC1 cage-mates minus ", nm_lab, "; adjusted for own CC1-day weight; origin-shifted")),
    list(id = "Delta_w_NM_FE", model = "W_C_FE", w = c(wm1 = 1), role = "companion", lead = FALSE, expo = "wm1", scale = "sW",
         label = paste0("CC1-day weight of CC1 cage-mates minus ", nm_lab, "; adjusted for own CC1-day weight", fe)))
  stats::setNames(x, vapply(x, `[[`, "", "id"))
}

#' Sensitivity-only contrasts: S9 (Model A, later classes pooled with the never co-housed class) and the S15 one-class
#' bridge (sex-averaged m1 coefficient of CombZ ~ cohort + own + m1 + own:sex_c + m1:sex_c; CC1 class vs all other
#' cohort-mates; per frozen SD, since m1 = xbar_loo / s1).
s33e_extra_specs <- function() list(
  Delta_NM_A = list(id = "Delta_NM_A", model = "A", w = c(m1 = 1, mNM = -1), lead = FALSE, expo = "m1", scale = "s1"),
  Delta_CC1_vs_all_others = list(id = "Delta_CC1_vs_all_others", model = "S15", w = c(m1 = 1), lead = FALSE, expo = "m1", scale = "s1"))

#' Simulation-only demonstrations (plan section 12: 'A_noown' and 'V2L_k without own_k').
s33e_demo_specs <- function() {
  x <- list(list(id = "A_noown_m1_minus_mNM", model = "A_noown", w = c(m1 = 1, mNM = -1)),
            list(id = "Delta_v2L_noownk", model = "V2L_noownk", w = s33e_v2w("l")),
            list(id = "Delta_v2L_noownk_CC2", model = "V2L_noownk", w = s33e_v2w("l", 2L, 1)),
            list(id = "Delta_v2L_noownk_CC3", model = "V2L_noownk", w = s33e_v2w("l", 3L, 1)),
            list(id = "Delta_v2L_noownk_CC4", model = "V2L_noownk", w = s33e_v2w("l", 4L, 1)))
  stats::setNames(x, vapply(x, `[[`, "", "id"))
}

#' Model template: the model rows (indices into the focal frame), fixed part, terms per block and cluster structure.
#' keep: logical row filter on the focal frame; unit_col: the pre-SIS cage column (S13 variants); cluster_col: e1 (CC1 CEID).
s33e_template <- function(spec, fm, keep = NULL, unit_col = "unit", cluster_col = "e1") {
  rows <- if (is.null(keep)) rep(TRUE, nrow(fm)) else keep %in% TRUE
  if (identical(spec$rows, "nmdc")) rows <- rows & fm$nmdc_ok
  is_unit <- identical(spec$absorb, "unit")
  if (is_unit) { u <- fm[[unit_col]]; tb <- table(u[rows]); rows <- rows & u %in% names(tb)[tb >= 2L] }
  ix <- which(rows)
  ab <- if (is_unit) fm[[unit_col]][ix] else fm$Batch[ix]
  fixed <- s33e_fixed(ab, if (is_unit) "u" else "coh_")
  ks <- spec$stacked %s33or% NA_integer_
  blocks <- lapply(ks, function(k) {
    if (is.na(k)) return(list(k = NA_integer_, cols = spec$terms, names = c(colnames(fixed), spec$terms)))
    gen <- ifelse(spec$terms == "own_k", "ownk", sub("_k$", "", spec$terms))
    list(k = k, cols = sub("_k$", as.character(k), spec$terms), names = paste0("k", k, "_", c(colnames(fixed), gen))) })
  nb <- length(blocks)
  list(spec = spec, ix = ix, rows_all = rep(ix, nb), fixed = fixed, blocks = blocks, nb = nb,
       cl = s33e_clusters(rep(fm[[cluster_col]][ix], nb)), n_units = length(unique(ab)), n_cohorts = length(unique(fm$Batch[ix])),
       colnames = unlist(lapply(blocks, `[[`, "names")))
}

#' Design matrix of a template for an exposure matrix EX (rows = focal frame); stacked copies are block-diagonal.
s33e_assemble <- function(tpl, EX) {
  if (tpl$nb == 1L) {
    X <- cbind(tpl$fixed, EX[tpl$ix, tpl$blocks[[1]]$cols, drop = FALSE]); colnames(X) <- tpl$colnames; return(X)
  }
  nr <- length(tpl$ix); pc <- ncol(tpl$fixed) + length(tpl$blocks[[1]]$cols)
  X <- matrix(0, nr * tpl$nb, pc * tpl$nb, dimnames = list(NULL, tpl$colnames))
  for (b in seq_len(tpl$nb)) X[(b - 1L) * nr + seq_len(nr), (b - 1L) * pc + seq_len(pc)] <-
    cbind(tpl$fixed, EX[tpl$ix, tpl$blocks[[b]]$cols, drop = FALSE])
  X
}

#' Contrast matrix (p x m) from a named list of coefficient weights; a weight naming an absent column stops (code error).
s33e_L <- function(cn, w) {
  L <- matrix(0, length(cn), length(w), dimnames = list(cn, names(w)))
  for (j in seq_along(w)) {
    miss <- setdiff(names(w[[j]]), cn)
    if (length(miss)) stop("Contrast ", names(w)[j], " names a coefficient that is not in the design: ", paste(miss, collapse = ", "), call. = FALSE)
    L[names(w[[j]]), j] <- w[[j]]
  }
  L
}

#' Variance inflation factors of the non-absorbed terms after absorbing the fixed part (cohort or pre-SIS cage dummies).
s33e_vif <- function(X, terms) {
  fx <- X[, setdiff(colnames(X), terms), drop = FALSE]; Z <- X[, terms, drop = FALSE]
  R <- Z - fx %*% qr.coef(qr(fx), Z)
  if (length(terms) < 2L) return(stats::setNames(1, terms))
  stats::setNames(vapply(seq_along(terms), function(j) {
    y <- R[, j]; W <- cbind(1, R[, -j, drop = FALSE]); e <- y - W %*% qr.coef(qr(W), y)
    sum((y - mean(y))^2) / sum(e^2) }, 0), terms)
}

# ---------------------------------------------------------------- CR2 (manual) and the wild cluster bootstrap-t
#' Outcome-free CR2 parts of an OLS design (plan: c_g = A_g X_g (X'X)^-1 L with A_g = (I - H_gg)^(-1/2), pseudo-inverse for
#' zero eigenvalues as clubSandwich) and the Satterthwaite df of every contrast: df = tr(O)^2 / sum(O^2), O = Q'Q,
#' Q = (I - H) P, P[i, g(i)] = c_g[i].
s33e_cr2_design <- function(X, cl, L, tol = 1e-12) {
  Bi <- chol2inv(chol(crossprod(X)))
  L <- matrix(L, nrow = ncol(X), dimnames = list(colnames(X), colnames(L)))
  BL <- Bi %*% L; XBL <- X %*% BL; XB <- X %*% Bi
  n <- nrow(X); m <- ncol(L); G <- cl$G
  C <- matrix(0, n, m)
  for (g in seq_len(G)) {
    r <- cl$rows[[g]]
    M <- -tcrossprod(XB[r, , drop = FALSE], X[r, , drop = FALSE]); diag(M) <- diag(M) + 1
    e <- eigen(M, symmetric = TRUE); v <- e$values; d <- numeric(length(v)); pos <- v > tol; d[pos] <- 1 / sqrt(v[pos])
    C[r, ] <- e$vectors %*% (d * crossprod(e$vectors, XBL[r, , drop = FALSE]))
  }
  P <- matrix(0, n, G * m)
  for (j in seq_len(m)) P[cbind(seq_len(n), cl$gi + (j - 1L) * G)] <- C[, j]
  Q <- P - XB %*% crossprod(X, P)
  df <- vapply(seq_len(m), function(j) { O <- crossprod(Q[, (j - 1L) * G + seq_len(G), drop = FALSE]); sum(diag(O))^2 / sum(O^2) }, 0)
  list(Bi = Bi, L = L, BL = BL, XBL = XBL, C = C, cl = cl, df = df)
}

#' Estimates, CR2 standard errors and residuals of a CR2 design for a response y.
s33e_cr2_apply <- function(des, X, y) {
  beta <- des$Bi %*% crossprod(X, y); u <- as.vector(y - X %*% beta)
  list(est = as.vector(crossprod(des$L, beta)), se = sqrt(colSums(rowsum(des$C * u, des$cl$gi, reorder = TRUE)^2)),
       df = des$df, u = u)
}

#' Wild cluster bootstrap-t statistics, vectorized (plan section 12): y* = fit + w_g(i) u_i (unrestricted), t*_b =
#' (L'b* - L'b) / se*_b with se* from the precomputed c_g (CR2) or sqrt(G/(G-1)) X_g B L (CR1). wmat: G x B weights in the
#' design's cluster order. Returns m x B.
s33e_wild_tstar <- function(des, X, u, wmat, studentize = c("CR2", "CR1")) {
  studentize <- match.arg(studentize)
  W <- wmat[des$cl$gi, , drop = FALSE]; WU <- W * u
  num <- crossprod(des$XBL, WU)
  Us <- WU - X %*% (des$Bi %*% crossprod(X, WU))
  Cs <- if (studentize == "CR2") des$C else sqrt(des$cl$G / (des$cl$G - 1)) * des$XBL
  out <- vapply(seq_len(ncol(des$L)), function(j) num[j, ] / sqrt(colSums(rowsum(Cs[, j] * Us, des$cl$gi, reorder = TRUE)^2)),
                numeric(ncol(W)))
  t(matrix(out, nrow = ncol(W)))
}

#' Wild bootstrap-t interval [est - q.975(t*) se, est - q.025(t*) se] (quantile type 7).
s33e_wild_ci <- function(est, se, tstar) {
  q <- vapply(seq_len(nrow(tstar)), function(j) { t <- tstar[j, ]; t <- t[is.finite(t)]
    if (!length(t)) c(NA_real_, NA_real_) else stats::quantile(t, c(0.025, 0.975), type = 7, names = FALSE) }, numeric(2))
  list(lo = est - q[2, ] * se, hi = est - q[1, ] * se)
}

#' CR1 standard errors (clubSandwich 'CR1': G/(G - 1) times CR0) from a CR2 design's parts.
s33e_cr1_se <- function(des, u) sqrt(des$cl$G / (des$cl$G - 1) * colSums(rowsum(des$XBL * u, des$cl$gi, reorder = TRUE)^2))

#' Webb weight matrix of one stream: one set.seed, matrix(sample.int(6, G * B, replace = TRUE), nrow = G) (plan section 12).
s33e_wild_weights <- function(levels, B, seed) {
  idx <- s33e_with_seed(seed, function() matrix(sample.int(6L, length(levels) * B, replace = TRUE), nrow = length(levels)))
  list(idx = idx, w = matrix(S33E_WEBB[idx], nrow = length(levels), dimnames = list(levels, NULL)),
       sha256 = digest::digest(idx, algo = "sha256"))
}

#' Run fun() in one Mersenne-Twister / Inversion / Rejection stream started by set.seed(seed); the RNG kind is restored.
s33e_with_seed <- function(seed, fun) {
  old <- RNGkind(); on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(seed)
  fun()
}

#' OLS fit object for clubSandwich (coefficients named as the design columns).
s33e_lm <- function(X, y) {
  d <- as.data.frame(X); d$.y <- y
  stats::lm(.y ~ 0 + ., data = d)
}

# ---------------------------------------------------------------- simulation (plan section 12; Phase 2, outcome-free)
#' The 24 scenarios: i = 1 N0; 2-19 N1 grid (i = 2 + 6 (ps - 1) + 2 (gs - 1) + (truth - 1) by index; base i = 10);
#' 20 N2; 21 N3; 22-23 N4a/b; 24 P1. N1 uses pt = 0.48 - ps.
s33e_scenarios <- function() {
  one <- function(i, code, pt, ps = 0, pg = 0, gs2 = 0, gz2 = 0, gg2 = 0, gh2 = 0, g1 = 0, gw1 = 0, truth = "recorded", label)
    data.table::data.table(scenario_id = as.integer(i), scenario = code, pt = pt, ps = ps, pg = pg, gs2 = gs2, gz2 = gz2, gg2 = gg2,
                           gh2 = gh2, g1 = g1, gw1 = gw1, b2_truth_units = truth, scenario_label = label)
  rows <- list(one(1L, "N0", 0.48, label = "own trait only: no shared-origin, cage or class component"))
  for (p in 1:3) for (g in 1:3) for (t in 1:2) {
    i <- 2L + 6L * (p - 1L) + 2L * (g - 1L) + (t - 1L); ps <- c(0.07, 0.14, 0.26)[p]; gs2 <- c(0.05, 0.15, 0.25)[g]
    tr <- c("recorded", "dob_groups")[t]
    rows[[length(rows) + 1L]] <- one(i, sprintf("N1_ps%.2f_gs2%.2f_%s", ps, gs2, tr), 0.48 - ps, ps = ps, gs2 = gs2, truth = tr,
      label = paste0("shared origin: pre-SIS cage component in rates (ps) and CC1-day weight, and in CombZ (gs^2); B2 truth units ",
                     tr, if (i == 10L) "; base cell" else ""))
  }
  rows <- c(rows, list(
    one(20L, "N2", 0.44, pg = 0.04, gg2 = 0.10, label = "CC1 cage component in CC1 rates (pg) and in CombZ (gg^2)"),
    one(21L, "N3", 0.48, gh2 = 0.05, label = "later-cage (CC2-CC4) components in CombZ (gh^2)"),
    one(22L, "N4a", 0.35, ps = 0.13, gz2 = 0.15, label = "pre-SIS cage component in rates and weight; separate pre-SIS cage component in CombZ (gz^2 0.15)"),
    one(23L, "N4b", 0.35, ps = 0.13, gz2 = 0.30, label = "pre-SIS cage component in rates and weight; separate pre-SIS cage component in CombZ (gz^2 0.30)"),
    one(24L, "P1", 0.48, g1 = 0.3, gw1 = 0.3, label = "planted class terms: CC1-class mean rate and weight each enter CombZ with 0.3")))
  x <- data.table::rbindlist(rows)[order(scenario_id)]
  x[, size := data.table::fifelse(scenario_id %in% S33E_SIM_LARGE, "large", "small")][]
}

#' Generating value of every simulated quantity (P1: 0.3 for the CC1-class contrasts against NM or the later classes,
#' 0 for the later-class comparators, undefined (NA) for version 2; every other scenario: 0).
s33e_truth <- function(par, quantities) {
  tv <- stats::setNames(rep(0, length(quantities)), quantities)
  if (par$g1 != 0 || par$gw1 != 0) {
    tv[] <- 0
    tv[intersect(quantities, c("Delta_NM", "Delta_later", "Delta_NMdc", "Delta_NM_FE", "Delta_later_FE", "A_noown_m1_minus_mNM"))] <- par$g1
    tv[intersect(quantities, c("Delta_w_NM", "Delta_w_later", "Delta_w_NM_FE", "Delta_w_later_FE"))] <- par$gw1
    tv[grepl("v2", quantities)] <- NA_real_
  }
  tv
}

#' Outcome-free simulation structure: the real nodes, cages, classes and pre-SIS cages; templates of every simulated model.
s33e_sim_struct <- function(st, fm) {
  nodes <- st$nodes; n <- nrow(nodes); f <- which(nodes$focal)
  lv <- function(x) match(x, sort(unique(x), method = "radix"))
  h <- c(nodes$e2, nodes$e3, nodes$e4); hl <- sort(unique(h), method = "radix")
  specs <- s33e_model_specs(); cs <- c(s33e_contrast_specs(), s33e_demo_specs())
  models <- c("C", "NMdc", "C_FE", "V2L", "V2N", "W_C", "W_C_FE", "A_noown", "V2L_noownk")
  tpl <- lapply(stats::setNames(models, models), function(m) s33e_template(specs[[m]], fm))
  L <- lapply(stats::setNames(models, models), function(m) {
    w <- lapply(Filter(function(z) identical(z$model, m), cs), `[[`, "w"); s33e_L(tpl[[m]]$colnames, w) })
  q_model <- unlist(lapply(models, function(m) stats::setNames(rep(m, ncol(L[[m]])), colnames(L[[m]]))))
  list(n = n, focal = f, batch = lv(nodes$Batch), nb = length(unique(nodes$Batch)), u_rec = lv(nodes$unit),
       u_dob = lv(nodes$unit_dob_s13), g1 = lv(nodes$e1), hi = matrix(match(h, hl), n, 3L), nh = length(hl),
       K1all = st$mats$K1$M, Mf = lapply(st$mats, function(x) x$M[f, , drop = FALSE]), tpl = tpl, L = L,
       quantities = names(q_model), q_model = q_model, wild_levels = sort(unique(fm$e1), method = "radix"))
}

#' One simulated replicate (plan section 12; every number of replicate r is drawn before replicate r + 1, in this order:
#' cohort offsets of the four rates, of weight and of CombZ; t_i; s_u; z_u; g_e (CC1 cages); h_e (CC2-CC4 cages);
#' eps_i^k; eps'_i; e_i). Truth units are the recorded pre-SIS cages, or the DoB groups in the S13 cohort.
s33e_sim_draw <- function(ss, par) {
  dob <- identical(par$b2_truth_units, "dob_groups")
  ui <- if (dob) ss$u_dob else ss$u_rec; nu <- max(ui); n <- ss$n; nb <- ss$nb
  a_r <- matrix(stats::rnorm(nb * 4L), nb, 4L); a_w <- stats::rnorm(nb); a_c <- stats::rnorm(nb)
  t <- stats::rnorm(n); s <- stats::rnorm(nu); z <- stats::rnorm(nu)
  g <- stats::rnorm(max(ss$g1)); h <- stats::rnorm(ss$nh)
  eps <- matrix(stats::rnorm(n * 4L), n, 4L); epw <- stats::rnorm(n); e <- stats::rnorm(n)
  r <- matrix(0, n, 4L)
  for (k in 1:4) {
    pg <- if (k == 1L) par$pg else 0
    r[, k] <- a_r[ss$batch, k] + sqrt(par$pt) * t + sqrt(par$ps) * s[ui] + sqrt(pg) * g[ss$g1] + sqrt(1 - par$pt - par$ps - pg) * eps[, k]
  }
  w <- a_w[ss$batch] + sqrt(0.61) * s[ui] + sqrt(0.39) * epw
  m1t <- as.vector(ss$K1all %*% r[, 1]); wm1t <- as.vector(ss$K1all %*% w)
  m1t[!is.finite(m1t)] <- 0; wm1t[!is.finite(wm1t)] <- 0
  cz <- a_c[ss$batch] + 0.4 * t + sqrt(par$gs2) * s[ui] + sqrt(par$gz2) * z[ui] + sqrt(par$gg2) * g[ss$g1] +
    sqrt(par$gh2) * (h[ss$hi[, 1]] + h[ss$hi[, 2]] + h[ss$hi[, 3]]) + par$g1 * m1t + par$gw1 * wm1t +
    sqrt(1 - 0.16 - par$gs2 - par$gz2 - par$gg2 - 3 * par$gh2) * e
  list(r = r, w = w, cz = cz)
}

#' Fit the simulated models of one replicate (unscaled exposures); estimates, CR2 SE and df of every quantity.
s33e_sim_fit <- function(ss, dat, models = names(ss$tpl), keep_parts = FALSE) {
  EX <- s33e_expo(ss$Mf, dat$r, dat$w, ss$focal, 1, 1, 1)
  y <- dat$cz[ss$focal]
  est <- se <- df <- stats::setNames(rep(NA_real_, length(ss$quantities)), ss$quantities)
  parts <- list()
  for (m in models) {
    tpl <- ss$tpl[[m]]; X <- s33e_assemble(tpl, EX)
    des <- tryCatch(s33e_cr2_design(X, tpl$cl, ss$L[[m]]), error = function(e) NULL)
    if (is.null(des)) next
    r <- s33e_cr2_apply(des, X, y[tpl$rows_all]); q <- colnames(ss$L[[m]])
    est[q] <- r$est; se[q] <- r$se; df[q] <- r$df
    if (keep_parts) parts[[m]] <- list(des = des, X = X, y = y[tpl$rows_all], u = r$u)
  }
  list(est = est, se = se, df = df, parts = parts)
}

#' One scenario: R replicates from seed 33050200 + i; replicate-level estimates, CR2 SE and df of every quantity.
s33e_sim_run <- function(ss, par, R, seed) {
  Q <- ss$quantities
  est <- se <- df <- matrix(NA_real_, R, length(Q), dimnames = list(NULL, Q))
  s33e_with_seed(seed, function() for (rep in seq_len(R)) {
    v <- s33e_sim_fit(ss, s33e_sim_draw(ss, par))
    est[rep, ] <<- v$est; se[rep, ] <<- v$se; df[rep, ] <<- v$df
  })
  list(est = est, se = se, df = df)
}

#' Summary rows of one scenario (mean, SD, mean/SD with MCSE, quantiles, CR2 interval behaviour).
s33e_sim_summary <- function(res, truth) {
  data.table::rbindlist(lapply(colnames(res$est), function(q) {
    e <- res$est[, q]; s <- res$se[, q]; d <- res$df[, q]; ok <- is.finite(e) & is.finite(s) & is.finite(d) & d > 0
    e <- e[ok]; s <- s[ok]; d <- d[ok]; R <- length(e)
    m <- if (R) mean(e) else NA_real_; sdv <- if (R > 1L) stats::sd(e) else NA_real_
    tq <- stats::qt(0.975, d); lo <- e - tq * s; hi <- e + tq * s
    data.table::data.table(quantity = q, R_finite = R, mean = m, sd = sdv, mean_over_sd = m / sdv,
      mcse_mean = sdv / sqrt(R), mcse_mean_over_sd = sqrt((1 + 0.5 * (m / sdv)^2) / R),
      q025 = if (R) stats::quantile(e, 0.025, type = 7, names = FALSE) else NA_real_,
      q975 = if (R) stats::quantile(e, 0.975, type = 7, names = FALSE) else NA_real_,
      true_value = unname(truth[q]), share_cr2_intervals_excluding_0 = if (R) mean(lo > 0 | hi < 0) else NA_real_,
      mean_se_cr2 = if (R) mean(s) else NA_real_, mean_se_cr2_over_sd = if (R) mean(s) / sdv else NA_real_,
      mean_width_cr2 = if (R) mean(hi - lo) else NA_real_, mean_df_cr2 = if (R) mean(d) else NA_real_)
  }))
}

#' Coverage study of one scenario: replicates 1..R_cov regenerated from the scenario seed, wild weights from the coverage
#' seed (one G x B matrix per replicate, in order), CR2 and wild bootstrap-t intervals of the coverage contrasts; the
#' manual CR2 of the first n_check replicates is compared with clubSandwich. return_limits = TRUE adds the per-replicate
#' interval limits (tests only; the run stores no draws).
s33e_sim_coverage <- function(ss, par, R_cov, B_wild, seed_gen, seed_wild, n_check = S33E_CR2_CHECK_REPS, return_limits = FALSE) {
  dats <- s33e_with_seed(seed_gen, function() lapply(seq_len(R_cov), function(i) s33e_sim_draw(ss, par)))
  qs <- S33E_COVERAGE_CONTRASTS; mods <- unique(unname(ss$q_model[qs])); G <- length(ss$wild_levels)
  lo_c <- hi_c <- lo_w <- hi_w <- matrix(NA_real_, R_cov, length(qs), dimnames = list(NULL, qs))
  chk <- list()
  s33e_with_seed(seed_wild, function() for (rep in seq_len(R_cov)) {
    idx <- matrix(sample.int(6L, G * B_wild, replace = TRUE), nrow = G)
    wm <- matrix(S33E_WEBB[idx], nrow = G, dimnames = list(ss$wild_levels, NULL))
    v <- s33e_sim_fit(ss, dats[[rep]], models = mods, keep_parts = TRUE)
    for (m in mods) {
      pt <- v$parts[[m]]; if (is.null(pt)) next
      q <- intersect(colnames(ss$L[[m]]), qs); j <- match(q, colnames(ss$L[[m]]))
      ts <- s33e_wild_tstar(pt$des, pt$X, pt$u, wm[pt$des$cl$levels, , drop = FALSE])[j, , drop = FALSE]
      ci <- s33e_wild_ci(v$est[q], v$se[q], ts)
      tq <- stats::qt(0.975, v$df[q])
      lo_w[rep, q] <<- ci$lo; hi_w[rep, q] <<- ci$hi; lo_c[rep, q] <<- v$est[q] - tq * v$se[q]; hi_c[rep, q] <<- v$est[q] + tq * v$se[q]
      if (rep <= n_check) {
        fit <- s33e_lm(pt$X, pt$y)
        cs <- s33_cr2_contrast(fit, pt$des$cl$levels[pt$des$cl$gi], t(ss$L[[m]][, j, drop = FALSE]))
        chk[[length(chk) + 1L]] <<- data.table::data.table(rep = rep, model = m, quantity = q, d_se = abs(cs$se - v$se[q]),
                                                           d_df = abs(cs$df - v$df[q]) / pmax(1, abs(cs$df)))
      }
    }
  })
  cov <- data.table::rbindlist(lapply(qs, function(q) {
    okc <- is.finite(lo_c[, q]) & is.finite(hi_c[, q]); okw <- is.finite(lo_w[, q]) & is.finite(hi_w[, q])
    cc <- mean(lo_c[okc, q] <= 0 & hi_c[okc, q] >= 0); cw <- mean(lo_w[okw, q] <= 0 & hi_w[okw, q] >= 0)
    data.table::data.table(quantity = q, R_coverage = sum(okc), B_wild_coverage = as.integer(B_wild), coverage_cr2 = cc,
                           mcse_coverage_cr2 = sqrt(cc * (1 - cc) / sum(okc)), coverage_wild = cw,
                           mcse_coverage_wild = sqrt(cw * (1 - cw) / sum(okw)),
                           mean_width_wild = mean(hi_w[okw, q] - lo_w[okw, q]))
  }))
  out <- list(coverage = cov, check = data.table::rbindlist(chk))
  if (isTRUE(return_limits)) out$limits <- list(lo_cr2 = lo_c, hi_cr2 = hi_c, lo_wild = lo_w, hi_wild = hi_w)
  out
}

#' Simulation replicate counts: the plan's 5000 / 2000, scaled by ctx$B['coverage_reps'] / 1000 (= 1 in the real run) so a
#' development ctx with reduced B also reduces them; ctx$B['sim_large'] / ['sim_small'] take precedence when present.
s33e_sim_sizes <- function(ctx) {
  B <- ctx$B
  if (all(c("sim_large", "sim_small") %in% names(B))) return(c(large = as.integer(B[["sim_large"]]), small = as.integer(B[["sim_small"]])))
  f <- as.numeric(B[["coverage_reps"]]) / 1000
  c(large = max(as.integer(B[["coverage_reps"]]), as.integer(ceiling(S33E_SIM_R[["large"]] * f))),
    small = max(2L, as.integer(ceiling(S33E_SIM_R[["small"]] * f))))
}

#' Checkpointed computation (s33_checkpoint, which also logs to ctx$checkpoint_log) with a row in the contract's
#' checkpoint columns (module, step, checkpoint_key, file, reused).
s33e_ckpt <- function(ctx, step, fun, key_extra) {
  key <- s33_checkpoint_key(ctx, "E", step, key_extra)
  f <- if (is.null(ctx$checkpoint_dir) || isTRUE(ctx$no_checkpoints)) NA_character_ else
    file.path(ctx$checkpoint_dir, paste0("E_", step, "_", substr(key, 1L, 16L), ".rds"))
  side <- paste0(f, ".sha256")
  reused <- !is.na(f) && file.exists(f) && file.exists(side) &&
    identical(readLines(side, warn = FALSE)[1], digest::digest(file = f, algo = "sha256"))
  v <- s33_checkpoint(ctx, "E", step, fun, key_extra)
  list(value = v, row = data.table::data.table(module = "E", step = step, checkpoint_key = key,
                                               file = if (is.na(f)) NA_character_ else basename(f), reused = reused))
}

# ---------------------------------------------------------------- Phase 2 gates (outcome-free)
#' GE-03 counts, GE-04 invariants, GE-08 scales, GE-14 within-cohort identity, GE-15 pre-SIS cages, GE-16 version-2 sets
#' (sets part), GE-17 m1 = xbar_loo / s1. Returns gate rows.
s33e_gates_structure <- function(design, st, fm, EX_all, scales, expect) {
  G <- list(); gate <- function(id, name, ok, detail = "") G[[length(G) + 1L]] <<- s33_gate_rows(id, name, ok, detail = detail)
  nodes <- st$nodes; dy <- st$dyads
  # GE-03 counts
  sis_b <- table(nodes$Batch); nf <- nodes[nodes$focal]
  dcount <- table(factor(dy$first_met_class, levels = S33E_CLASSES))
  nmdc <- fm[fm$nmdc_ok]
  gate("GE-03", "counts: bundle episodes; sis_87 per cohort; focal_85 in 22 CC1 clusters (non-focal OQ770, OQ771); dyads by class; NMdc 56 in 20",
       c(nrow(design$episodes) == expect$n_episodes, identical(as.integer(sis_b[names(expect$sis_by_cohort)]), unname(expect$sis_by_cohort)),
         sum(sis_b) == sum(expect$sis_by_cohort), nrow(nf) == expect$n_focal, data.table::uniqueN(nf$e1) == expect$n_clusters,
         setequal(nodes$AnimalNum[!nodes$focal], expect$non_focal), identical(nodes$focal, nodes$pop_focal_85 %in% TRUE),
         identical(as.integer(dcount[names(expect$dyads)]), unname(expect$dyads)), nrow(nmdc) == expect$n_nmdc,
         data.table::uniqueN(nmdc$e1) == expect$nmdc_clusters),
       sprintf("episodes %d; sis %d; focal %d in %d clusters; dyads %s; NMdc %d in %d (%s)", nrow(design$episodes), nrow(nodes), nrow(nf),
               data.table::uniqueN(nf$e1), paste(names(dcount), as.integer(dcount), collapse = " "), nrow(nmdc), data.table::uniqueN(nmdc$e1),
               paste(nmdc[, .N, keyby = Batch][, paste(Batch, N)], collapse = ", ")))
  # GE-04 invariants
  ep <- data.table::as.data.table(design$episodes)
  mixed <- ep[, .(k = data.table::uniqueN(condition)), by = CageEpisodeID][k > 1L]
  cc1p <- dy[dy$first_met_class == "CC1"]
  later_with_cc1 <- cc1p[grepl("CC2|CC3", cc1p$ccs_together)]
  multi <- dy[dy$n_cc_together > 1L, paste(animal_a, animal_b, sep = "|")]
  each_class <- nf$n_nm >= 1L & nf$n_cc2 >= 1L & nf$n_cc3 >= 1L & nf$n_cc4 >= 1L
  clos_ok <- vapply(expect$closure_cohorts, function(b) { i <- which(nodes$Batch == b)
    cl <- st$closure[i]; parts <- unique(cl); sizes <- lengths(strsplit(parts, ";", fixed = TRUE))
    length(i) > 0L && all(sizes == 4L) && sum(sizes) == length(i) && length(parts) == length(i) / 4 }, TRUE)
  nm <- dy[dy$first_met_class == "NM"]
  sh <- nm[, .(same = sum(same_src_cage %in% TRUE), total = .N), keyby = Batch]
  bb <- names(expect$nm_same_src)
  # later classes sharing a recorded pre-SIS cage (recorded in the detail, not gated)
  lsh <- dy[dy$first_met_class %in% c("CC2", "CC3", "CC4"), .(same = sum(same_src_cage %in% TRUE)), keyby = .(Batch, first_met_class)]
  lsh <- lsh[, .(txt = paste(same, collapse = "/")), keyby = Batch]
  gate("GE-04", paste("invariants: no SIS-CON shared CEID; no CC1 pair together at CC2 or CC3; only the declared multi-CC pairs;",
                      "every focal animal has NM, CC2, CC3, CC4 members; NM closure in the declared cohorts; source-cage sharing table"),
       c(nrow(mixed) == 0L, nrow(later_with_cc1) == 0L, setequal(multi, expect$multi_cc_pairs), each_class, clos_ok,
         identical(sh[match(bb, sh$Batch), same], unname(expect$nm_same_src)),
         identical(sh[match(bb, sh$Batch), total], unname(expect$nm_total)), sum(cc1p$same_src_cage %in% TRUE) == 0L),
       sprintf("multi-CC pairs %s; NM same source %s; CC1 pairs sharing a source cage %d; later classes CC2/CC3/CC4 sharing a source cage (recorded) %s",
               paste(multi, collapse = ","), paste(sh$Batch, paste0(sh$same, "/", sh$total), collapse = " "), sum(cc1p$same_src_cage %in% TRUE),
               paste(lsh$Batch, lsh$txt, collapse = " ")))
  # GE-08 scales (s1, sL, sW recomputed; the constants are used)
  lr <- data.table::data.table(x = c(nodes$r2, nodes$r3, nodes$r4), g = paste(rep(nodes$Batch, 3L), rep(c("CC2", "CC3", "CC4"), each = nrow(nodes))))
  s1r <- s33_resid_sd(nodes$r1, nodes$Batch); sLr <- s33_resid_sd(lr$x, lr$g); sWr <- s33_resid_sd(nodes$tp2, nodes$Batch)
  gate("GE-08", "scales: s1 recomputed within 1e-4 of 5.1698 and identical to S33_SD_RATE; sL, sW recomputed within 1e-3 of 6.2383, 1.6143",
       c(abs(s1r - expect$s1) < expect$tol_s1, identical(scales[["s1"]], S33_SD_RATE), abs(sLr - expect$sL) < expect$tol_sLW,
         abs(sWr - expect$sW) < expect$tol_sLW, identical(scales[["sL"]], S33E_SL), identical(scales[["sW"]], S33E_SW)),
       sprintf("recomputed s1 %.6f sL %.6f sW %.6f", s1r, sLr, sWr))
  # GE-14 within-cohort identity: own + sum_k n_k m_k + n_NM mNM = cohort sum (CC1 rates and tp2)
  idn <- function(own, mcols, ncols) own + rowSums(sapply(seq_along(mcols), function(i) { v <- EX_all[, mcols[i]] * nodes[[ncols[i]]]; v[nodes[[ncols[i]]] == 0L] <- 0; v }))
  tot_r <- stats::ave(EX_all[, "own"], nodes$Batch, FUN = sum); tot_w <- stats::ave(EX_all[, "ownW"], nodes$Batch, FUN = sum)
  nc <- c("n_cc1", "n_cc2", "n_cc3", "n_cc4", "n_nm")
  d_r <- abs(idn(EX_all[, "own"], c("m1", "m2", "m3", "m4", "mNM"), nc) - tot_r)[nodes$focal]
  d_w <- abs(idn(EX_all[, "ownW"], c("wm1", "wm2", "wm3", "wm4", "wmNM"), nc) - tot_w)[nodes$focal]
  gate("GE-14", "within-cohort identity: own + sum_k n_k m_k + n_NM mNM = cohort sum (< 1e-10), CC1 rates and tp2 weights",
       c(d_r < 1e-10, d_w < 1e-10), sprintf("max %.3g (rates), %.3g (tp2)", max(d_r), max(d_w)))
  # GE-15 pre-SIS cages
  ub <- nf[, .(u = data.table::uniqueN(unit)), keyby = Batch]
  gate("GE-15", "pre-SIS cages: recorded for every contributor; units among focal per cohort as declared; the declared singleton alone; FE designs columns, rank, residual df",
       c(!is.na(nodes$src_cage) & nzchar(nodes$src_cage),
         identical(ub[match(names(expect$units_by_cohort), ub$Batch), u], unname(expect$units_by_cohort)),
         setequal(nodes$AnimalNum[nodes$unit_singleton], expect$unit_singletons)),
       sprintf("units %s; singletons %s", paste(ub$Batch, ub$u, collapse = " "), paste(nodes$AnimalNum[nodes$unit_singleton], collapse = ",")))
  # GE-16 version-2 sets (no comparator housed with the animal at CCk; C_k drops only the declared pair at CC4)
  od <- st$od; tg <- st$tog
  comp_with <- vapply(2:4, function(k) sum(od$cls == paste0("CC", S33E_V2_LATER[[as.character(k)]]) & tg[, k] |
                                            (od$cls == "NM" & tg[, k])), 0L)
  drops <- unique(unlist(lapply(2:4, function(k) { x <- od[od$cls == "CC1" & tg[, k]]
    if (!nrow(x)) character() else paste0("CC", k, ":", vapply(seq_len(nrow(x)), function(i)
      paste(sort(nodes$AnimalNum[c(x$a[i], x$b[i])], method = "radix"), collapse = "|"), "")) })))
  G16 <- c(comp_with == 0L, setequal(unique(sub("^CC4:", "", drops)), expect$v2_dropped_cc4), all(grepl("^CC4:", drops)))
  # GE-17 m1 = xbar_loo / s1 (xbar_loo from the shared design: tracked SIS CC1 cage-mates)
  a <- data.table::as.data.table(design$animals)[pop_sis_87 %in% TRUE]
  a[, `:=`(n_c = .N, sum_c = sum(crossing_rate)), by = cc1_cage]
  a[, xbar_loo := (sum_c - crossing_rate) / (n_c - 1)]
  j <- match(fm$AnimalNum, a$AnimalNum)
  d17 <- abs(scales[["s1"]] * fm$m1_raw_s1 - a$xbar_loo[j])
  gate("GE-17", "|s1 m1 - xbar_loo| < 1e-10 for every focal animal (xbar_loo from the shared design)",
       c(d17 < 1e-10, length(d17) == expect$n_focal), sprintf("max %.3g over %d", max(d17), length(d17)))
  list(rows = data.table::rbindlist(G), ge16_sets = G16, drops = drops)
}

# ---------------------------------------------------------------- Phase 2
#' Outcome-free part of module E (plan sections 6 and 12). Returns list(products, gates, state).
s33e_phase2 <- function(design, ctx) {
  s33_assert_outcome_free(design$animals, "design (module E)"); s33_assert_outcome_free(design$episodes, "design episodes (module E)")
  expect <- ctx$e_expect %s33or% S33E_EXPECT
  t0 <- Sys.time()
  scales <- c(s1 = S33_SD_RATE, sL = S33E_SL, sW = S33E_SW)
  st <- s33e_structure(design, expect)
  nodes <- st$nodes; n <- nrow(nodes)
  r <- as.matrix(nodes[, paste0("r", 1:4), with = FALSE])
  EX_all <- s33e_expo(lapply(st$mats, `[[`, "M"), r, nodes$tp2, seq_len(n), scales[["s1"]], scales[["sL"]], scales[["sW"]])
  # S7: CC1 A1-A4 mean rate from the Stage 32 window metrics (rate columns only; Group and CombZ unread)
  wm <- data.table::fread(ctx$inputs$s32_window_metrics, select = c("AnimalNum", "Batch", "CC", "phase", "crossing_rate", "metrics_used"),
                          colClasses = list(character = "AnimalNum"), showProgress = FALSE)
  s33_assert_outcome_free(wm, "Stage 32 window metrics read (module E)")
  wm <- wm[wm$CC == "CC1" & wm$phase %in% paste0("A", 1:4) & wm$metrics_used %in% TRUE & wm$AnimalNum %in% nodes$AnimalNum]
  r7 <- wm[, .(rbar = mean(crossing_rate), n_ph = .N), by = AnimalNum]
  rbar <- r7$rbar[match(nodes$AnimalNum, r7$AnimalNum)]
  s7 <- s33_resid_sd(rbar, nodes$Batch)
  M <- lapply(st$mats, `[[`, "M")
  EX7 <- cbind(own7 = rbar / s7, m1_7 = as.vector(M$K1 %*% rbar) / s7, m2_7 = as.vector(M$K2 %*% rbar) / s7,
               m3_7 = as.vector(M$K3 %*% rbar) / s7, m4_7 = as.vector(M$K4 %*% rbar) / s7)
  # S10: version-2 exposures without the declared animal as a CC4 contributor
  s10 <- which(nodes$AnimalNum %in% expect$s10_animal)
  m10 <- s33e_mats(n, st$od, st$tog, exclude_cc4 = s10)
  EX10 <- cbind(v4_s10 = as.vector(m10$V4$M %*% r[, 4]) / scales[["sL"]], l4_s10 = as.vector(m10$L4$M %*% r[, 4]) / scales[["sL"]],
                u4_s10 = as.vector(m10$U4$M %*% r[, 4]) / scales[["sL"]])
  # focal frame (model rows) with every exposure and covariate
  f <- which(nodes$focal)
  fm <- data.table::copy(nodes[f])
  EXf <- cbind(EX_all[f, , drop = FALSE], EX7[f, , drop = FALSE], EX10[f, , drop = FALSE],
               age_cc1 = nodes$age_cc1[f], line_J = as.numeric(nodes$line_J[f]), sex_c = nodes$sex_c[f])
  EXf <- cbind(EXf, own_sex = EXf[, "own"] * EXf[, "sex_c"], m1_sex = EXf[, "m1"] * EXf[, "sex_c"])
  fm[, `:=`(nmdc_ok = is.finite(EXf[, "mNMdc"]), m1_raw_s1 = EXf[, "m1"])]
  # gates on the structure
  gs <- s33e_gates_structure(design, st, fm, EX_all, scales, expect)
  specs <- s33e_model_specs()
  # GE-10 rank and VIF of the planned designs (the five-class model never fitted); GE-15 FE design columns, rank and df
  vif_rows <- s33e_design_vif(fm, EXf, specs)
  gate10 <- s33e_gate_rank_vif(vif_rows, s33e_five_class_deficient(fm, EXf, expect$closure_cohorts), expect$vif_max)
  ge15_new <- s33e_gate_units(gs$rows[gate_id == "GE-15"], vif_rows, expect)
  # simulation structure; GE-16 stacked = separate fits and CR2 blocks on one simulated outcome (scenario 1, replicate 1)
  ss <- s33e_sim_struct(st, fm)
  scen <- s33e_scenarios()
  par1 <- as.list(scen[scenario_id == 1L])
  dat1 <- s33e_with_seed(ctx$seeds[["E_scenario_1"]], function() s33e_sim_draw(ss, par1))
  ge16 <- s33e_gate_v2(gs, s33e_check_stacked(ss, dat1, fm))
  gates <- rbind(gs$rows[gate_id != "GE-15"], ge15_new, gate10, ge16)
  # simulations and coverage (checkpointed per scenario and per coverage study)
  sizes <- s33e_sim_sizes(ctx)
  R_cov <- as.integer(ctx$B[["coverage_reps"]]); B_cov <- as.integer(ctx$B[["coverage_wild"]])
  frame_sha <- digest::digest(list(ss$Mf, ss$u_rec, ss$u_dob, ss$g1, ss$hi, lapply(ss$tpl, `[`, c("ix", "fixed", "colnames")), ss$L), algo = "sha256")
  ck <- list(); sims <- list(); covs <- list(); checks <- list()
  for (i in scen$scenario_id) {
    par <- as.list(scen[scenario_id == i]); R <- sizes[[par$size]]; seed <- ctx$seeds[[paste0("E_scenario_", i)]]
    x <- s33e_ckpt(ctx, sprintf("sim_%02d", i), function() {
      res <- s33e_sim_run(ss, par, R, seed)
      list(summary = s33e_sim_summary(res, s33e_truth(par, ss$quantities)), replicate_sha256 = digest::digest(res, algo = "sha256"))
    }, key_extra = list(frame = frame_sha, seed = seed, R = R, scenario = par))
    ck[[length(ck) + 1L]] <- x$row
    sm <- data.table::copy(x$value$summary)
    sm[, `:=`(scenario_id = as.integer(i), seed = seed, R_planned = as.integer(R), replicate_sha256 = x$value$replicate_sha256)]
    sims[[length(sims) + 1L]] <- sm
    if (i %in% S33E_COVERAGE_SCENARIOS) {
      seed_w <- ctx$seeds[[paste0("E_coverage_", i)]]
      y <- s33e_ckpt(ctx, sprintf("cov_%02d", i), function() s33e_sim_coverage(ss, par, R_cov, B_cov, seed, seed_w),
                     key_extra = list(frame = frame_sha, seed = seed, seed_wild = seed_w, R = R_cov, B = B_cov, scenario = par))
      ck[[length(ck) + 1L]] <- y$row
      cv <- data.table::copy(y$value$coverage); cv[, `:=`(scenario_id = as.integer(i), coverage_seed_wild = seed_w)]
      covs[[length(covs) + 1L]] <- cv
      checks[[length(checks) + 1L]] <- y$value$check
    }
  }
  e05 <- merge(data.table::rbindlist(sims), data.table::rbindlist(covs), by = c("scenario_id", "quantity"), all.x = TRUE, sort = FALSE)
  e05 <- merge(scen, e05, by = "scenario_id", sort = FALSE)
  e05 <- e05[order(e05$scenario_id, match(e05$quantity, ss$quantities))]
  e05[, `:=`(model_id = unname(ss$q_model[quantity]),
             quantity_role = data.table::fifelse(quantity %in% names(s33e_contrast_specs()), "e04 contrast", "simulation-only demonstration"),
             note = S33E_SIM_NOTE, tier = S33_TIER)]
  # products
  e01 <- s33e_e01(nodes, EX_all, EX7, EX10, s7, scales, design)
  e02 <- st$dyads[, .(Batch, animal_a, animal_b, first_met_class, n_cc_together, ccs_together, same_src_cage, same_dob, nm_subclass,
                      animal_a_focal = nodes$focal[ia], animal_b_focal = nodes$focal[ib])]
  e02[, tier := S33_TIER]
  e07 <- s33e_age_table(nodes, st$mats)
  products <- list(e01_exposures_animal = e01, e02_dyads = e02, e05_simulation = e05, e07_age_descriptive = e07)
  for (k in names(products)) s33_assert_outcome_free(products[[k]], paste("module E product", k))
  stopifnot(identical(names(products), S33E_OUTCOME_FREE_PRODUCTS))
  state <- list(expect = expect, scales = scales, s7 = s7, fm = fm, EXf = EXf, sizes = sizes, sim_check = data.table::rbindlist(checks),
                checkpoints = data.table::rbindlist(ck), product_sha256 = vapply(products, s33e_sha, ""),
                vif = vif_rows, frame_sha256 = frame_sha, phase2_seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
  list(products = products, gates = gates, state = state)
}

#' Rank and maximum VIF of every planned design (stacked designs per copy k), after absorbing cohort or pre-SIS cage dummies.
s33e_design_vif <- function(fm, EXf, specs = s33e_model_specs(), models = c("C", "C_FE", "NMdc", "A", "V2L", "V2N", "W_C", "W_C_FE")) {
  data.table::rbindlist(lapply(models, function(m) {
    tpl <- s33e_template(specs[[m]], fm); X <- s33e_assemble(tpl, EXf)
    blocks <- if (tpl$nb == 1L) list(list(X = X, terms = tpl$blocks[[1]]$cols, name = m)) else lapply(seq_len(tpl$nb), function(b) {
      nr <- length(tpl$ix); pc <- ncol(tpl$fixed) + length(tpl$blocks[[b]]$cols)
      Xb <- X[(b - 1L) * nr + seq_len(nr), (b - 1L) * pc + seq_len(pc), drop = FALSE]
      colnames(Xb) <- c(colnames(tpl$fixed), tpl$blocks[[b]]$cols)
      list(X = Xb, terms = tpl$blocks[[b]]$cols, name = paste0(m, "_k", tpl$blocks[[b]]$k)) })
    data.table::rbindlist(lapply(blocks, function(bk) data.table::data.table(design_id = bk$name, n = nrow(bk$X), p = ncol(bk$X),
      rank = qr(bk$X)$rank, max_vif = max(s33e_vif(bk$X, bk$terms)))))
  }))
}

#' The five-class model (all classes, never co-housed included) in the closure cohorts: TRUE where it is rank-deficient
#' (equal class sizes make own + sum of n_k m_k + n_NM mNM the cohort total). Empty when no closure cohort is declared.
s33e_five_class_deficient <- function(fm, EXf, closure_cohorts) {
  k5 <- fm$Batch %in% closure_cohorts
  if (!any(k5)) return(logical())
  X5 <- cbind(s33e_fixed(fm$Batch[k5], "coh_"), EXf[k5, c("own", "m1", "m2", "m3", "m4", "mNM"), drop = FALSE])
  qr(X5)$rank < ncol(X5)
}

#' GE-10: planned designs full rank, max VIF below the declared bound; the five-class model rank-deficient (never fitted).
s33e_gate_rank_vif <- function(vif_rows, five, vif_max) {
  s33_gate_rows("GE-10", "planned designs full rank with max VIF < 4 after absorbing cohort or pre-SIS cage dummies; the five-class model is rank-deficient in the closure cohorts and never fitted",
                c(nrow(vif_rows) > 0L, vif_rows$rank == vif_rows$p, vif_rows$max_vif < vif_max, length(five) > 0L, five),
                detail = paste(sprintf("%s n %d p %d rank %d VIF %.2f", vif_rows$design_id, vif_rows$n, vif_rows$p, vif_rows$rank,
                                       vif_rows$max_vif), collapse = "; "))
}

#' GE-15: the pre-SIS cage row of s33e_gates_structure() plus the two pre-SIS cage designs (columns, rank, residual df).
s33e_gate_units <- function(ge15, vif_rows, expect) {
  fe_d <- vif_rows[design_id %in% c("C_FE", "W_C_FE")]
  if (nrow(ge15) != 1L || nrow(fe_d) != 2L) return(s33_gate_rows("GE-15", "pre-SIS cages and their designs", FALSE, detail = "FE designs or unit row missing"))
  s33_gate_rows("GE-15", ge15$gate, c(isTRUE(ge15$passed), fe_d$p == expect$fe_cols, fe_d$rank == expect$fe_cols, fe_d$n - fe_d$p == expect$fe_df),
                detail = paste0(ge15$detail, sprintf("; FE n %s p %s df %s", paste(fe_d$n, collapse = "/"), paste(fe_d$p, collapse = "/"),
                                                     paste(fe_d$n - fe_d$p, collapse = "/"))))
}

#' GE-16: version-2 sets (from s33e_gates_structure) and the stacked = separate fits check (s33e_check_stacked).
s33e_gate_v2 <- function(gs, fit_check) {
  s33_gate_rows("GE-16", "version-2 sets: no comparator housed with the animal at CCk; C_k drops only the declared pair at CC4; stacked = separate fits (< 1e-10) and CR2 blocks",
                c(gs$ge16_sets, fit_check$ok), detail = paste0("drops ", paste(gs$drops, collapse = ","), "; ", fit_check$detail))
}

#' GE-16 fit part: the stacked version-2 OLS equals the three per-CC fits (coefficients < 1e-10) and each stacked CR2
#' block equals that fit's CR2 (clubSandwich, < 1e-10), on one simulated outcome.
s33e_check_stacked <- function(ss, dat, fm) {
  EX <- s33e_expo(ss$Mf, dat$r, dat$w, ss$focal, 1, 1, 1); y <- dat$cz[ss$focal]
  ok <- logical(); det <- character()
  for (m in c("V2L", "V2N")) {
    tpl <- ss$tpl[[m]]; X <- s33e_assemble(tpl, EX); yy <- y[tpl$rows_all]; cl <- fm$e1[tpl$rows_all]
    fs <- s33e_lm(X, yy); Vs <- as.matrix(clubSandwich::vcovCR(fs, cluster = cl, type = "CR2"))
    nr <- length(tpl$ix); pc <- ncol(X) / tpl$nb
    for (b in seq_len(tpl$nb)) {
      cols <- (b - 1L) * pc + seq_len(pc); rows <- (b - 1L) * nr + seq_len(nr)
      fb <- s33e_lm(X[rows, cols, drop = FALSE], yy[rows]); Vb <- as.matrix(clubSandwich::vcovCR(fb, cluster = cl[rows], type = "CR2"))
      dc <- max(abs(stats::coef(fs)[cols] - stats::coef(fb))); dv <- max(abs(Vs[cols, cols] - Vb))
      ok <- c(ok, dc < 1e-10, dv < 1e-10); det <- c(det, sprintf("%s k%d coef %.2g CR2 %.2g", m, tpl$blocks[[b]]$k, dc, dv))
    }
  }
  list(ok = ok, detail = paste(det, collapse = "; "))
}

#' e01: one row per tracked SIS animal (87): CEIDs, pre-SIS cage and its S13 variants, CC1-day weight (tp2, g; read by the
#' cross-module gate X4), class sizes, every exposure, flags.
s33e_e01 <- function(nodes, EX_all, EX7, EX10, s7, scales, design) {
  x <- nodes[, .(AnimalNum, Batch, Sex, focal, cc1_cage = e1, cc2_cage = e2, cc3_cage = e3, cc4_cage = e4, src_cage, unit,
                 unit_singleton, unit_closure_s13, unit_dob_s13, line, line_J, dob = format(dob, "%Y-%m-%d"), age_cc1, tp2, n_cc1, n_cc2,
                 n_cc3, n_cc4, n_nm, n_nm_sc, n_nm_dc)]
  x <- cbind(x, data.table::as.data.table(EX_all), data.table::as.data.table(EX7), data.table::as.data.table(EX10))
  excl_cages <- unique(nodes$e1[nodes$xmate_excluded %in% TRUE])
  x[, `:=`(flag_cc1_cage_had_excluded_animal = cc1_cage %in% excl_cages,
           flag_cohort_has_untracked_sis = Batch %in% unique(design$animals$Batch[design$animals$untracked_b1 %in% TRUE]),
           s1 = scales[["s1"]], sL = scales[["sL"]], sW = scales[["sW"]], s7 = s7, tier = S33_TIER)]
  x[]
}

#' e07: age at CC1 by cohort (descriptive only; own age and line enter only as S5 covariates).
s33e_age_table <- function(nodes, mats) {
  mate_age <- as.vector(mats$K1$M %*% nodes$age_cc1)
  data.table::rbindlist(lapply(sort(unique(nodes$Batch), method = "radix"), function(b) {
    i <- which(nodes$Batch == b & nodes$focal); own <- nodes$age_cc1[i]; mate <- mate_age[i]
    res <- if (length(unique(own)) > 1L) stats::resid(stats::lm(mate ~ own)) else mate - mean(mate)
    sdr <- if (length(i) > 1L) sqrt(sum(res^2) / (length(i) - 1L)) else NA_real_
    src <- nodes[nodes$Batch == b, .(k = data.table::uniqueN(age_cc1)), by = src_cage]
    data.table::data.table(Batch = b, n_focal = length(i), n_distinct_dob = data.table::uniqueN(nodes$dob[i]),
                           own_age_min_d = min(own), own_age_max_d = max(own), cc1_mate_mean_age_min_d = min(mate),
                           cc1_mate_mean_age_max_d = max(mate), sd_cc1_mate_age_resid_after_own_d = sdr,
                           age_constant_within_src_cage = all(src$k == 1L), mate_age_varies_after_own_age = is.finite(sdr) && sdr > 1e-8,
                           tier = S33_TIER)
  }))
}

# ---------------------------------------------------------------- Phase 3 helpers
#' Fit one OLS model (template + exposures) and return its pieces; FAILED (rank or error) fits are kept as such.
s33e_fit <- function(tpl, EX, y, contrasts, cluster = NULL) {
  X <- s33e_assemble(tpl, EX); yy <- y[tpl$rows_all]
  cl <- if (is.null(cluster)) tpl$cl else s33e_clusters(cluster)
  ok <- all(is.finite(X)) && all(is.finite(yy)) && qr(X)$rank == ncol(X) && nrow(X) > ncol(X)
  if (!ok) return(list(ok = FALSE, X = X, y = yy, tpl = tpl, reason = "rank-deficient design or missing values"))
  w <- lapply(contrasts, `[[`, "w"); L <- s33e_L(colnames(X), w)
  des <- s33e_cr2_design(X, cl, L); r <- s33e_cr2_apply(des, X, yy)
  fit <- s33e_lm(X, yy)
  list(ok = TRUE, X = X, y = yy, tpl = tpl, des = des, res = r, fit = fit, L = L, cl = cl)
}

#' clubSandwich CR2 contrast rows of a fit for its contrast matrix (cluster = the design's cluster structure).
s33e_cs_rows <- function(fo, L = fo$L, cl = fo$cl) {
  s33_cr2_contrast(fo$fit, cl$levels[cl$gi], t(L))
}

#' Estimate rows (s33_row) of module E with the declared descriptive columns.
s33e_rows <- function(ids, est, se, df, lo, hi, method, specs, n_animals, n_cages, n_cohorts, n_params, status = "OK", ...) {
  sp <- specs[ids]
  units <- vapply(sp, function(z) S33E_UNITS[[z$scale]], "")
  mid <- vapply(sp, function(z) if (identical(z$scale, "sW")) "tp2_weight" else "crossing_rate", "")
  x <- s33_row(estimand = ids, level = S33E_LEVEL, estimate = est, se = se, df = df, ci_low = lo, ci_high = hi,
               interval_method = method, interval_basis = S33E_BASIS, units = units, metric_id = mid, status = status,
               lead = vapply(sp, function(z) isTRUE(z$lead), TRUE), n_animals = n_animals, n_cages = n_cages, n_cohorts = n_cohorts,
               n_params = n_params, ...)
  x[metric_id == "tp2_weight", metric_label := S33E_TP2_LABEL]
  x[]
}

#' sha256 of a table's column values (independent of data.table internals).
s33e_sha <- function(x) digest::digest(lapply(x, function(v) v), algo = "sha256")

#' Within-cohort SD (n - 1, on cohort, model rows) of exposure columns of a template.
s33e_expo_sd <- function(tpl, EX, fm, cols) {
  stats::setNames(vapply(cols, function(cn) s33_resid_sd(EX[tpl$ix, cn], fm$Batch[tpl$ix]), 0), cols)
}

#' Maximum VIF of a design built from a template (per stacked copy).
s33e_vif_template <- function(tpl, X) {
  nr <- length(tpl$ix)
  max(vapply(seq_len(tpl$nb), function(b) {
    pc <- ncol(tpl$fixed) + length(tpl$blocks[[b]]$cols)
    Xb <- X[(b - 1L) * nr + seq_len(nr), (b - 1L) * pc + seq_len(pc), drop = FALSE]
    colnames(Xb) <- c(colnames(tpl$fixed), tpl$blocks[[b]]$cols)
    max(s33e_vif(Xb, tpl$blocks[[b]]$cols)) }, 0))
}

S33E_ADJUSTMENT_TERMS <- c("own", "own_k", "ownW", "age_cc1", "line_J", "own_sex")

#' One e03 row for a fitted OLS model (design facts; adjustment coefficients are never reported).
s33e_model_row <- function(model_id, sid, spec, fo, population, EX, fm, note = "", unit_col = "unit") {
  tpl <- fo$tpl; X <- fo$X
  adj <- intersect(spec$terms, S33E_ADJUSTMENT_TERMS); expo <- setdiff(spec$terms, S33E_ADJUSTMENT_TERMS)
  cols <- unique(unlist(lapply(tpl$blocks, function(b) b$cols[match(expo, spec$terms)])))
  sds <- if (length(tpl$ix) > 1L) s33e_expo_sd(tpl, EX, fm, cols) else stats::setNames(rep(NA_real_, length(cols)), cols)
  is_unit <- identical(spec$absorb, "unit")
  sdu <- if (is_unit && length(tpl$ix) > 1L) stats::setNames(vapply(cols, function(cn) s33_resid_sd(EX[tpl$ix, cn], fm[[unit_col]][tpl$ix]), 0), cols) else NULL
  data.table::data.table(model_id = model_id, sensitivity_id = sid, formula = spec$formula, population = population, engine = "OLS (lm)",
    n_obs = nrow(X), n_animals = length(tpl$ix), n_clusters_cc1 = data.table::uniqueN(fm$e1[tpl$ix]), n_cohorts = tpl$n_cohorts,
    n_pre_sis_cages = if (is_unit) tpl$n_units else NA_integer_, n_params = ncol(X),
    rank = if (nrow(X) && all(is.finite(X))) qr(X)$rank else NA_integer_, residual_df = nrow(X) - ncol(X),
    max_vif = if (isTRUE(fo$ok)) s33e_vif_template(tpl, X) else NA_real_,
    adjustment_terms = paste(c(adj, if (is_unit) "pre-SIS cage intercepts" else "cohort intercepts"), collapse = ", "),
    adjustment_coefficients_reported = FALSE, exposure_terms = paste(expo, collapse = ", "),
    exposure_sd_within_cohort = paste(sprintf("%s %.4f", names(sds), sds), collapse = "; "),
    exposure_sd_within_pre_sis_cage = if (is.null(sdu)) NA_character_ else paste(sprintf("%s %.4f", names(sdu), sdu), collapse = "; "),
    status = if (isTRUE(fo$ok)) "OK" else "FAILED", singular = NA, converged = NA, engine_messages = NA_character_, note = note, tier = S33_TIER)
}

#' e06 / e08 rows of a fit for the given contrast ids (CR2 by the fit's clusters unless interval = 'none' or < 3 clusters).
s33e_fit_rows <- function(fo, ids, specs, interval = c("CR2", "none"), method = S33E_CI_CR2, min_clusters = 3L) {
  interval <- match.arg(interval)
  tpl <- fo$tpl; nA <- length(tpl$ix); nG <- data.table::uniqueN(fo$tpl$cl$levels); np <- ncol(fo$X)
  if (!isTRUE(fo$ok)) return(s33e_rows(ids, NA, NA, NA, NA, NA, "none", specs, nA, nG, tpl$n_cohorts, np, status = "FAILED"))
  j <- match(ids, colnames(fo$L))
  if (interval == "none" || fo$cl$G < min_clusters)
    return(s33e_rows(ids, fo$res$est[j], NA, NA, NA, NA, "none", specs, nA, nG, tpl$n_cohorts, np))
  cs <- s33e_cs_rows(fo, fo$L[, j, drop = FALSE])
  s33e_rows(ids, cs$estimate, cs$se, cs$df, cs$ci_low, cs$ci_high, method, specs, nA, nG, tpl$n_cohorts, np)
}

#' GE-05 (Phase 3): labels of the tracked SIS animals in design_out. CombZ finite; RES/SUS reproduce from the within-sex
#' thresholds (SUS below the threshold); n_components_present = the number of present components; RES/SUS counts as
#' declared (sis_87 and focal_85). The identity of CombZ and outcome_group with the bundle is GC-7a/GC-7b (core, Phase 3):
#' the outcomes are read once, by s33_attach_outcomes(), and never again by a module.
s33e_gate_labels <- function(design_out, expect) {
  an <- data.table::as.data.table(design_out$animals)
  sis <- an[an$pop_sis_87 %in% TRUE]; foc <- an[an$pop_focal_85 %in% TRUE]
  thr <- data.table::as.data.table(design_out$thresholds)
  tv <- thr$susceptibility_threshold[match(sis$Sex, thr$Sex)]
  rep_ok <- sis$outcome_group == data.table::fifelse(sis$CombZ < tv, "SUS", "RES")
  npres <- rowSums(is.finite(as.matrix(sis[, S33_COMPONENTS, with = FALSE])))
  np_ok <- sis$n_components_present == npres
  cnt <- c(sum(sis$outcome_group == "RES", na.rm = TRUE) == expect$sis_res, sum(sis$outcome_group == "SUS", na.rm = TRUE) == expect$sis_sus,
           sum(foc$outcome_group == "RES", na.rm = TRUE) == expect$focal_res, sum(foc$outcome_group == "SUS", na.rm = TRUE) == expect$focal_sus)
  ok <- c(nrow(sis) == sum(expect$sis_by_cohort), is.finite(sis$CombZ), rep_ok %in% TRUE, np_ok %in% TRUE, cnt)
  det <- sprintf(paste("CombZ finite %d/%d; thresholds reproduce RES/SUS %d/%d; n_components_present = present components %d/%d (%d with fewer",
                       "than 6); label counts as declared: %s; CombZ and outcome_group = bundle: GC-7a, GC-7b (outcomes read once)"),
                 sum(is.finite(sis$CombZ)), nrow(sis), sum(rep_ok %in% TRUE), nrow(sis), sum(np_ok %in% TRUE), nrow(sis),
                 sum(npres < length(S33_COMPONENTS)), all(cnt))
  s33_gate_rows("GE-05", "labels: CombZ finite; RES/SUS reproduce from the within-sex thresholds; components present counted; RES/SUS counts as declared",
                ok, detail = det)
}

#' GE-09 (Phase 3): the Phase-2 products (e01, e02, e05, e07) are outcome-free and unchanged since their Phase-2 hash.
s33e_gate_products <- function(p2) {
  ref <- p2$state$product_sha256
  pr_ok <- vapply(S33E_OUTCOME_FREE_PRODUCTS, function(k) !is.null(p2$products[[k]]) && identical(s33e_sha(p2$products[[k]]), unname(ref[k])), TRUE)
  of_ok <- vapply(S33E_OUTCOME_FREE_PRODUCTS, function(k) !inherits(tryCatch(s33_assert_outcome_free(p2$products[[k]]), error = function(e) e), "error"), TRUE)
  s33_gate_rows("GE-09", "Phase-2 products (e01, e02, e05, e07) outcome-free, hashed in Phase 2 and unchanged in the final tables",
                c(length(ref) == length(S33E_OUTCOME_FREE_PRODUCTS), pr_ok, of_ok),
                detail = paste(sprintf("%s %s", names(ref), substr(ref, 1L, 12L)), collapse = "; "))
}

#' GE-11 (Phase 3): manual CR2 = clubSandwich on the e04 fits (g11) and on the first simulated coverage replicates (sc):
#' SE within 1e-8, Satterthwaite df within 1e-8 (relative). Both tables: d_se, d_df.
s33e_gate_cr2 <- function(g11, sc) {
  if (is.null(sc)) sc <- data.table::data.table(d_se = numeric(), d_df = numeric())
  ok <- c(g11$d_se < 1e-8, g11$d_df < 1e-8, sc$d_se < 1e-8, sc$d_df < 1e-8)
  s33_gate_rows("GE-11", "manual CR2 = clubSandwich (SE within 1e-8; Satterthwaite df within 1e-8 relative): e04 contrasts and the first simulated coverage replicates",
    ok, n_expected = 2L * (nrow(g11) + nrow(sc)),
    detail = sprintf("e04 rows %d, max |dSE| %.2g, max rel |ddf| %.2g; simulation rows %d, max |dSE| %.2g, max rel |ddf| %.2g", nrow(g11),
                     if (nrow(g11)) max(g11$d_se) else NA_real_, if (nrow(g11)) max(g11$d_df) else NA_real_, nrow(sc),
                     if (nrow(sc)) max(sc$d_se) else NA_real_, if (nrow(sc)) max(sc$d_df) else NA_real_))
}

#' The frozen engine's messages of an S6 fit and its KR contrasts (engine_messages; exempt from the lint).
s33e_engine_messages <- function(mm, kr) {
  info <- mm$info
  v <- c(if (!is.null(info$messages)) info$messages, if (!is.null(info$optimizer_check_messages)) info$optimizer_check_messages,
         if (!is.null(info$error)) info$error, mm$failure_reason, if ("kr_messages" %in% names(kr)) kr$kr_messages,
         if ("failure_reason" %in% names(kr)) kr$failure_reason)
  v <- unique(trimws(as.character(v))); v <- v[!is.na(v) & nzchar(v)]
  if (length(v)) paste(v, collapse = " | ") else NA_character_
}

#' Notes of the e04 rows (caveats of plan section 12 in the declared wording).
s33e_e04_notes <- function(expect) {
  sh <- paste(sprintf("%s %d/%d", names(expect$nm_same_src), expect$nm_same_src, expect$nm_total), collapse = ", ")
  nm <- paste0("the never co-housed class is mostly former pre-SIS cage-mates (never co-housed pairs sharing a recorded pre-SIS cage: ", sh,
               "), so this contrast is origin-shifted; read it against the later-class comparators and the expected shift in e05")
  fe <- "pre-SIS cage intercepts rely on the recorded pre-SIS cages (B2 from a planning file; S13 alternatives)"
  cmp <- "comparator: later cage-mates lived with the animal later"
  v2 <- "version 2 includes carry-over from CC1 co-housing"
  c(Delta_NM = paste0(nm, "; ", S33E_ONE_ASSOCIATION), Delta_NM_FE = fe,
    Delta_later = paste0("later classes match the CC1 class in the meeting design, but later cage-mates also lived with the animal later; ",
                         S33E_ONE_ASSOCIATION),
    Delta_later_FE = fe, Delta_NMdc = paste0("kinship of cohort-mates from a different recorded pre-SIS cage is unknown; ", S33E_ONE_ASSOCIATION),
    Delta_v2L = v2, Delta_w_later = "CC1-day weight is mostly pre-SIS cage variance", Delta_w_later_FE = fe,
    Delta_CC2_vs_NM = cmp, Delta_CC3_vs_NM = cmp, Delta_CC4_vs_NM = cmp, Delta_v2L_CC2 = v2, Delta_v2L_CC3 = v2, Delta_v2L_CC4 = v2,
    Delta_v2NM = "never co-housed comparator: origin-shifted", Delta_w_NM = "never co-housed comparator: origin-shifted", Delta_w_NM_FE = fe)
}

# ---------------------------------------------------------------- Phase 3
#' Module E, Phase 3 (outcomes): primary fits (e04), sensitivities S1-S16 (e06), leave one CC1 cage out (e08), models (e03).
s33e_phase3 <- function(design_out, p2, ctx) {
  st <- p2$state; expect <- st$expect; fm <- st$fm; EXf <- st$EXf; scales <- st$scales
  specs <- s33e_model_specs(); cs <- s33e_contrast_specs()
  csx <- c(cs, s33e_extra_specs())
  an <- data.table::as.data.table(design_out$animals)
  y <- an$CombZ[match(fm$AnimalNum, an$AnimalNum)]
  gates <- list(s33e_gate_labels(design_out, expect))
  # ---- primary fits: CR2 (clubSandwich), manual CR2 (GE-11; wild studentization), wild bootstrap-t, S11 and S16 intervals
  prim <- c("C", "C_FE", "NMdc", "V2L", "V2N", "W_C", "W_C_FE")
  ww <- s33e_wild_weights(sort(unique(fm$e1), method = "radix"), as.integer(ctx$B[["wild"]]), ctx$seeds[["E_wild"]])
  prow <- list(); g11 <- list(); s11 <- list(); s16 <- list(); e03 <- list()
  for (m in prim) {
    ids_m <- names(Filter(function(z) identical(z$model, m), cs))
    fo <- s33e_fit(s33e_template(specs[[m]], fm), EXf, y, cs[ids_m])
    pop <- switch(m, NMdc = "focal animals with >= 1 never co-housed cohort-mate from a different recorded pre-SIS cage",
                  C_FE = , W_C_FE = "focal_85 without animals alone in their pre-SIS cage", "focal_85")
    e03[[length(e03) + 1L]] <- s33e_model_row(m, NA_character_, specs[[m]], fo, pop, EXf, fm)
    if (!isTRUE(fo$ok)) { prow[[m]] <- data.table::data.table(id = ids_m, status = "FAILED", n_animals = length(fo$tpl$ix), n_cages = fo$tpl$cl$G,
                                                              n_cohorts = fo$tpl$n_cohorts, n_params = ncol(fo$X)); next }
    clv <- fo$cl$levels[fo$cl$gi]
    csr <- s33e_cs_rows(fo)
    g11[[m]] <- data.table::data.table(model = m, quantity = colnames(fo$L), d_se = abs(csr$se - fo$res$se),
                                       d_df = abs(csr$df - fo$des$df) / pmax(1, abs(csr$df)))
    wci <- s33e_wild_ci(csr$estimate, csr$se, s33e_wild_tstar(fo$des, fo$X, fo$res$u, ww$w[fo$cl$levels, , drop = FALSE]))
    # per within-cohort SD of the CC1-class exposure (version 2: weights sd(v_k) on the copy-k terms)
    sdx <- lapply(ids_m, function(id) { z <- cs[[id]]
      if (identical(z$expo, "v")) {
        ks <- as.integer(sub("^k([0-9])_v$", "\\1", grep("_v$", names(z$w), value = TRUE)))
        sdk <- s33e_expo_sd(fo$tpl, EXf, fm, paste0("v", ks)); w <- z$w
        for (k in ks) { s <- startsWith(names(w), paste0("k", k, "_")); w[s] <- w[s] * sdk[[paste0("v", k)]] }
        list(w = w, detail = paste(sprintf("%s %.4f sL (%.3f position changes/hour)", names(sdk), sdk, sdk * scales[["sL"]]), collapse = "; "))
      } else {
        sd1 <- s33e_expo_sd(fo$tpl, EXf, fm, z$expo)[[1]]
        list(w = z$w * sd1, detail = sprintf("%s %.4f (%.3f %s)", z$expo, sd1, sd1 * scales[[z$scale]],
                                             if (identical(z$scale, "sW")) "g" else "position changes/hour")) } })
    sdr <- data.table::rbindlist(lapply(sdx, function(s) s33_cr2_contrast(fo$fit, clv, list(s$w))))
    # S11: OLS t; CR1 with t(G - 1); CR1-studentized wild (same weights); CR2 by pre-SIS cage
    se_o <- sqrt(diag(t(fo$L) %*% stats::vcov(fo$fit) %*% fo$L))
    V1 <- as.matrix(clubSandwich::vcovCR(fo$fit, cluster = clv, type = "CR1")); se_1 <- sqrt(diag(t(fo$L) %*% V1 %*% fo$L))
    to <- s33_t_interval(csr$estimate, se_o, fo$fit$df.residual); t1 <- s33_t_interval(csr$estimate, se_1, fo$cl$G - 1)
    w1 <- s33e_wild_ci(csr$estimate, s33e_cr1_se(fo$des, fo$res$u),
                       s33e_wild_tstar(fo$des, fo$X, fo$res$u, ww$w[fo$cl$levels, , drop = FALSE], "CR1"))
    ucl <- fm$unit[fo$tpl$rows_all]
    cu <- s33_cr2_contrast(fo$fit, ucl, t(fo$L))
    s11[[m]] <- data.table::data.table(id = colnames(fo$L), est = csr$estimate, se_o = se_o, df_o = fo$fit$df.residual, lo_o = to$ci_low,
      hi_o = to$ci_high, se_1 = se_1, df_1 = fo$cl$G - 1, lo_1 = t1$ci_low, hi_1 = t1$ci_high, lo_w1 = w1$lo, hi_w1 = w1$hi,
      se_u = cu$se, df_u = cu$df, lo_u = cu$ci_low, hi_u = cu$ci_high, n_units = data.table::uniqueN(ucl), n_cl = fo$cl$G,
      n_animals = length(fo$tpl$ix), n_cohorts = fo$tpl$n_cohorts, n_params = ncol(fo$X))
    # S16: CR2 by CC4 cage
    j16 <- match(intersect(colnames(fo$L), c("Delta_CC4_vs_NM", "Delta_later", "Delta_w_later")), colnames(fo$L))
    if (length(j16)) { c4 <- fm$e4[fo$tpl$rows_all]
      s16[[m]] <- cbind(data.table::data.table(id = colnames(fo$L)[j16], n_animals = length(fo$tpl$ix), n_cohorts = fo$tpl$n_cohorts,
                                               n_params = ncol(fo$X)),
                        s33_cr2_contrast(fo$fit, c4, t(fo$L[, j16, drop = FALSE]))) }
    prow[[m]] <- data.table::data.table(id = colnames(fo$L), status = "OK", estimate = csr$estimate, se = csr$se, df = csr$df,
      ci_low = csr$ci_low, ci_high = csr$ci_high, wild_ci_low = wci$lo, wild_ci_high = wci$hi, n_animals = length(fo$tpl$ix),
      n_cages = fo$cl$G, n_cohorts = fo$tpl$n_cohorts, n_params = ncol(fo$X), exposure_sd_detail = vapply(sdx, `[[`, "", "detail"),
      estimate_per_exposure_sd = sdr$estimate, se_per_exposure_sd = sdr$se, df_per_exposure_sd = sdr$df,
      ci_low_per_exposure_sd = sdr$ci_low, ci_high_per_exposure_sd = sdr$ci_high)
  }
  # ---- e04: 17 rows in the order of 17.2 E, with the simulation summaries copied as expected behaviour
  ids <- names(cs)
  pr <- data.table::rbindlist(prow, fill = TRUE)
  for (cn in setdiff(c("estimate", "se", "df", "ci_low", "ci_high", "wild_ci_low", "wild_ci_high", "estimate_per_exposure_sd",
                       "se_per_exposure_sd", "df_per_exposure_sd", "ci_low_per_exposure_sd", "ci_high_per_exposure_sd"), names(pr)))
    pr[, (cn) := NA_real_]
  if (!"exposure_sd_detail" %in% names(pr)) pr[, exposure_sd_detail := NA_character_]
  pr <- pr[match(ids, pr$id)]
  e05 <- p2$products$e05_simulation
  simv <- function(qid, sid, col) { v <- e05[e05$scenario_id == sid & e05$quantity == qid][[col]]; if (length(v)) as.numeric(v[1]) else NA_real_ }
  gmm <- function(qid, f) { v <- e05[e05$scenario_id %in% 2:19 & e05$quantity == qid, mean_over_sd]; v <- v[is.finite(v)]
    if (length(v)) f(v) else NA_real_ }
  e04 <- s33e_rows(ids, pr$estimate, pr$se, pr$df, pr$ci_low, pr$ci_high, S33E_CI_CR2, cs, pr$n_animals, pr$n_cages, pr$n_cohorts,
                   pr$n_params, status = data.table::fifelse(is.na(pr$status), "FAILED", pr$status))
  notes <- s33e_e04_notes(expect)
  e04[, `:=`(row_order = seq_along(ids), role = vapply(cs, `[[`, "", "role"), model_id = vapply(cs, `[[`, "", "model"),
             contrast_weights = vapply(cs, function(z) paste(sprintf("%s=%g", names(z$w), z$w), collapse = "; "), ""),
             label = vapply(cs, `[[`, "", "label"), wild_interval_method = "wild_cluster_bootstrap_t", wild_ci_low = pr$wild_ci_low,
             wild_ci_high = pr$wild_ci_high, wild_B = as.integer(ctx$B[["wild"]]), wild_seed = as.integer(ctx$seeds[["E_wild"]]),
             wild_weights = "Webb six-point, one weight per CC1 cage (all stacked copies of a cage share it); CR2-studentized",
             wild_weights_sha256 = ww$sha256,
             exposure_sd_detail = pr$exposure_sd_detail, units_per_exposure_sd = S33E_UNITS[["exposure_sd"]],
             estimate_per_exposure_sd = pr$estimate_per_exposure_sd, se_per_exposure_sd = pr$se_per_exposure_sd,
             df_per_exposure_sd = pr$df_per_exposure_sd, ci_low_per_exposure_sd = pr$ci_low_per_exposure_sd,
             ci_high_per_exposure_sd = pr$ci_high_per_exposure_sd,
             sim_N0_mean_over_sd = vapply(ids, simv, 0, sid = 1L, col = "mean_over_sd"),
             expected_shift_N1_base_over_sd = vapply(ids, simv, 0, sid = 10L, col = "mean_over_sd"),
             expected_shift_N1_grid_min_over_sd = vapply(ids, gmm, 0, f = min), expected_shift_N1_grid_max_over_sd = vapply(ids, gmm, 0, f = max),
             sim_P1_mean = vapply(ids, simv, 0, sid = 24L, col = "mean"), sim_P1_true_value = vapply(ids, simv, 0, sid = 24L, col = "true_value"),
             sim_N0_coverage_cr2 = vapply(ids, simv, 0, sid = 1L, col = "coverage_cr2"),
             sim_N0_coverage_wild = vapply(ids, simv, 0, sid = 1L, col = "coverage_wild"), sim_note = S33E_SIM_NOTE,
             note = unname(notes[ids]), caveats = S33E_CAVEATS)]
  prim_est <- stats::setNames(e04$estimate, e04$estimand); prim_se <- stats::setNames(e04$se, e04$estimand)
  # ---- sensitivities (e06), e08 and the e03 rows of every refit
  sens <- list()
  add <- function(sid, label, rows, primary_id = rows$estimand, note = "") {
    pe <- unname(prim_est[primary_id]); ps <- unname(prim_se[primary_id])
    rows[, `:=`(sensitivity_id = sid, sensitivity = label, primary_estimand = primary_id, primary_estimate = pe, primary_se = ps,
                shift_vs_primary_in_se = (estimate - pe) / ps,
                robust_label = data.table::fifelse(is.finite(estimate) & is.finite(pe) & is.finite(ps),
                                                   mmm_ci_robust_label(pe, ps, estimate), NA_character_),
                robust_label_rule = S33E_ROBUST_RULE, note = note)]
    sens[[length(sens) + 1L]] <<- rows
    invisible(rows)
  }
  refit <- function(sid, model, keep, ids_r, EX = EXf, unit_col = "unit", interval = "CR2", population, label, note = "", units = NULL,
                    primary_id = NULL, spec = specs[[model]]) {
    tpl <- s33e_template(spec, fm, keep, unit_col); fo <- s33e_fit(tpl, EX, y, csx[ids_r])
    mid <- paste(sid, model, sep = "_")
    e03[[length(e03) + 1L]] <<- s33e_model_row(mid, sid, spec, fo, population, EX, fm, note = label, unit_col = unit_col)
    rows <- s33e_fit_rows(fo, ids_r, csx, interval)
    if (!is.null(units)) rows[, units := units]
    rows[, model_id := mid]
    add(sid, label, rows, primary_id %s33or% ids_r, note)
  }
  b_all <- sort(unique(fm$Batch), method = "radix")
  lead6 <- list(C = c("Delta_NM", "Delta_later"), C_FE = "Delta_later_FE", V2L = "Delta_v2L", W_C = "Delta_w_later", W_C_FE = "Delta_w_later_FE")
  # S1 without the focal animals of the CC1 cages that held an excluded animal (XMATE)
  k1 <- !(fm$xmate_excluded %in% TRUE)
  for (m in c("C", "NMdc", "C_FE", "V2L", "W_C", "W_C_FE"))
    refit("S1", m, k1, if (m == "NMdc") "Delta_NMdc" else lead6[[m]], population = "focal without the animals of the CC1 cages that held an excluded animal",
          label = "without the focal animals of the CC1 cages that held an excluded animal (XMATE)")
  # S2 leave one cohort out (and the declared pair); NMdc leaves each of its cohorts out in turn
  for (b in b_all) for (m in names(lead6))
    refit("S2", m, fm$Batch != b, lead6[[m]], population = paste("focal without", b), label = paste("leave one cohort out:", b))
  for (b in sort(unique(fm$Batch[fm$nmdc_ok]), method = "radix"))
    refit("S2", "NMdc", fm$Batch != b, "Delta_NMdc", population = paste("NMdc rows without", b), label = paste("leave one cohort out:", b))
  if (length(expect$s2_pair) && all(expect$s2_pair %in% b_all)) for (m in names(lead6))
    refit("S2", m, !fm$Batch %in% expect$s2_pair, lead6[[m]], population = paste("focal without", paste(expect$s2_pair, collapse = " and ")),
          label = paste("leave two cohorts out:", paste(expect$s2_pair, collapse = " and ")))
  # S3 one cohort alone (point estimates only)
  if (expect$s3_cohort %in% b_all)
    refit("S3", "C", fm$Batch == expect$s3_cohort, c("Delta_NM", "Delta_later"), interval = "none", population = paste(expect$s3_cohort, "alone"),
          label = paste(expect$s3_cohort, "alone (point estimates only; four CC1 cages give no usable interval)"))
  # S4 later classes from a different recorded pre-SIS cage
  EX4 <- EXf; EX4[, c("m2", "m3", "m4")] <- EXf[, c("m2_dc", "m3_dc", "m4_dc")]
  refit("S4", "C", rowSums(!is.finite(EX4[, c("m2", "m3", "m4"), drop = FALSE])) == 0L, "Delta_later", EX = EX4,
        population = "focal with a later cage-mate from a different recorded pre-SIS cage in every later class",
        label = "later classes restricted to cage-mates from a different recorded pre-SIS cage")
  # S5 + own age at CC1 and line
  for (m in c("C", "NMdc", "V2L", "W_C")) {
    sp <- specs[[m]]; sp$terms <- c(sp$terms, "age_cc1", "line_J"); sp$formula <- paste(sp$formula, "+ age_cc1 + line_J")
    refit("S5", m, NULL, switch(m, C = c("Delta_NM", "Delta_later"), NMdc = "Delta_NMdc", V2L = "Delta_v2L", W_C = "Delta_w_later"),
          spec = sp, population = "focal_85 (NMdc: its rows)", label = "adding own age at CC1 and line (C57BL/6J vs C57BL/6JRj); coefficients not reported")
  }
  # S6 crossed random intercepts with KR (frozen engine); S13 repeats it with the alternative pre-SIS cages
  s6 <- function(sid, unit_col, label) {
    d <- data.table::data.table(AnimalNum = fm$AnimalNum, Batch = fm$Batch, CombZ = y, own = EXf[, "own"], m1 = EXf[, "m1"], m2 = EXf[, "m2"],
                                m3 = EXf[, "m3"], m4 = EXf[, "m4"], cc1_cage = fm$e1, cc2_cage = fm$e2, cc3_cage = fm$e3, cc4_cage = fm$e4,
                                src_unit = fm[[unit_col]])
    fml <- "CombZ ~ Batch + own + m1 + m2 + m3 + m4 + (1 | cc1_cage) + (1 | cc2_cage) + (1 | cc3_cage) + (1 | cc4_cage) + (1 | src_unit)"
    mid <- paste0(sid, "_S6_", unit_col)
    mm <- s33_kr_fit(fml, d, mid, 11L)
    kr <- data.table::rbindlist(list(s33_kr_contrast(mm, list(m1 = 1), "Delta_NM"),
                                     s33_kr_contrast(mm, list(m1 = 1, m2 = -1/3, m3 = -1/3, m4 = -1/3), "Delta_later")), fill = TRUE)
    info <- mm$info
    rows <- s33e_rows(c("Delta_NM", "Delta_later"), kr$estimate, kr$se, kr$df, kr$ci_low, kr$ci_high,
                      data.table::fifelse(kr$status == "OK", kr$interval_method, "none"), csx, nrow(d), data.table::uniqueN(d$cc1_cage),
                      data.table::uniqueN(d$Batch), 11L, status = kr$status, singular = kr$singular, ddf_fallback = kr$ddf_fallback)
    rows[, model_id := mid]
    add(sid, label, rows)
    e03[[length(e03) + 1L]] <<- data.table::data.table(model_id = mid, sensitivity_id = sid, formula = fml, population = "focal_85",
      engine = "lmer (frozen engine; KR)", n_obs = nrow(d), n_animals = nrow(d), n_clusters_cc1 = data.table::uniqueN(d$cc1_cage),
      n_cohorts = data.table::uniqueN(d$Batch), n_pre_sis_cages = data.table::uniqueN(d$src_unit), n_params = 11L,
      rank = if (is.null(info$rank)) NA_integer_ else as.integer(info$rank[1]), residual_df = NA_integer_, max_vif = NA_real_,
      adjustment_terms = "own, cohort intercepts", adjustment_coefficients_reported = FALSE, exposure_terms = "m1, m2, m3, m4",
      exposure_sd_within_cohort = NA_character_, exposure_sd_within_pre_sis_cage = NA_character_,
      status = if (s32i_failed(mm)) "FAILED" else "OK",
      singular = if (is.null(info$singular)) NA else as.logical(info$singular[1]),
      converged = if (is.null(info$converged)) NA else as.logical(info$converged[1]),
      engine_messages = s33e_engine_messages(mm, kr),
      note = paste0(label, "; random intercepts for the CC1-CC4 cages and the pre-SIS cage (", unit_col, ")"), tier = S33_TIER)
  }
  s6("S6", "unit", "crossed random intercepts (CC1-CC4 cages, pre-SIS cage) with Kenward-Roger df")
  # S7 CC1 A1-A4 mean rate (Stage 32) as the exposure, scaled by its own within-cohort SD
  EX7 <- EXf; EX7[, c("own", "m1", "m2", "m3", "m4")] <- EXf[, c("own7", "m1_7", "m2_7", "m3_7", "m4_7")]
  refit("S7", "C", NULL, c("Delta_NM", "Delta_later"), EX = EX7, population = "focal_85", units = S33E_UNITS[["s7"]],
        label = "exposure window: CC1 A1-A4 mean rate (Stage 32), scaled by its own within-cohort SD")
  # S8 leave one CC1 cage out (e08) and its range (e06)
  e08 <- data.table::rbindlist(lapply(sort(unique(fm$e1), method = "radix"), function(cg) {
    tpl <- s33e_template(specs$C, fm, fm$e1 != cg); fo <- s33e_fit(tpl, EXf, y, cs[c("Delta_NM", "Delta_later")])
    cbind(data.table::data.table(left_out_cc1_cage = cg, Batch = fm$Batch[match(cg, fm$e1)]),
          s33e_fit_rows(fo, c("Delta_NM", "Delta_later"), cs)) }))
  e08[, `:=`(model_id = "S8_C", lead = FALSE)]   # sensitivity S8: never a lead row (plan 17.1 item 6)
  for (id in c("Delta_NM", "Delta_later")) {
    v <- e08$estimate[e08$estimand == id & is.finite(e08$estimate)]
    rr <- s33e_rows(rep(id, 2L), if (length(v)) range(v) else c(NA, NA), NA, NA, NA, NA, "none", cs, NA, NA, NA, NA)
    rr[, model_id := "S8_C"]
    add("S8", c("leave one CC1 cage out: minimum over the refits (e08)", "leave one CC1 cage out: maximum over the refits (e08)"), rr)
  }
  # S9 Model A (later classes pooled with the never co-housed class)
  refit("S9", "A", NULL, "Delta_NM_A", population = "focal_85", primary_id = "Delta_NM",
        label = "Model A: later classes not separated; CC1 class minus never co-housed (m1 - mNM)")
  # S10 version 2 without the declared animal (as focal animal and as CC4 contributor)
  EX10 <- EXf; EX10[, c("v4", "l4", "u4")] <- EXf[, c("v4_s10", "l4_s10", "u4_s10")]
  k10 <- !fm$AnimalNum %in% expect$s10_animal
  for (m in c("V2L", "V2N"))
    refit("S10", m, k10, if (m == "V2L") "Delta_v2L" else "Delta_v2NM", EX = EX10, population = paste("focal without", expect$s10_animal),
          label = paste("version 2 without", expect$s10_animal, "as focal animal and as CC4 contributor"))
  # S11 alternative intervals of the e04 contrasts
  s11d <- data.table::rbindlist(s11)
  if (nrow(s11d)) {
    s11d <- s11d[match(intersect(ids, s11d$id), s11d$id)]
    mk <- function(se, df, lo, hi, method, lab) {
      rr <- s33e_rows(s11d$id, s11d$est, se, df, lo, hi, method, cs, s11d$n_animals, s11d$n_cl, s11d$n_cohorts, s11d$n_params)
      rr[, model_id := vapply(cs[s11d$id], `[[`, "", "model")]; add("S11", lab, rr) }
    mk(s11d$se_o, s11d$df_o, s11d$lo_o, s11d$hi_o, paste0("t_", s11d$df_o), "OLS model-based standard error")
    mk(s11d$se_1, s11d$df_1, s11d$lo_1, s11d$hi_1, "CR1_t_G-1", "CR1 by CC1 cage with t(G - 1)")
    mk(NA, NA, s11d$lo_w1, s11d$hi_w1, "wild_cluster_bootstrap_t", "wild cluster bootstrap-t, CR1-studentized (same Webb weights)")
    mk(s11d$se_u, s11d$df_u, s11d$lo_u, s11d$hi_u, "CR2_Satterthwaite_src_cage", "CR2 by recorded pre-SIS cage")
  }
  # S12 sex strata (each sex is a set of cohorts; no female-minus-male row)
  for (sx in c("Female", "Male")) for (m in c("C", "C_FE"))
    refit("S12", m, fm$Sex == sx, if (m == "C") c("Delta_NM", "Delta_later") else "Delta_later_FE", population = paste(sx, "focal animals"),
          label = paste(sx, "cohorts only (a subset of cohorts; no female-minus-male row)"))
  # S13 the declared cohort's pre-SIS cages as never co-housed closure classes or DoB groups (FE companions and S6)
  for (uc in c("unit_closure_s13", "unit_dob_s13")) {
    lab <- paste(paste(expect$s13_cohort, collapse = ","), "pre-SIS cages taken as",
                 if (uc == "unit_closure_s13") "never co-housed closure classes (former assumption)" else "DoB groups")
    refit("S13", "C_FE", NULL, c("Delta_NM_FE", "Delta_later_FE"), unit_col = uc, population = "focal_85 without animals alone in their pre-SIS cage",
          label = lab)
    refit("S13", "W_C_FE", NULL, c("Delta_w_NM_FE", "Delta_w_later_FE"), unit_col = uc,
          population = "focal_85 without animals alone in their pre-SIS cage", label = lab)
    s6("S13", uc, paste(lab, "(S6 form)"))
  }
  # S15 one-class bridge to C's S3 (sex-averaged m1 coefficient)
  refit("S15", "S15", NULL, "Delta_CC1_vs_all_others", population = "focal_85", primary_id = NA_character_,
        label = "CC1 class vs all other cohort-mates; same variation as C's S3; not independent evidence", note = S33E_ONE_ASSOCIATION)
  # S16 CR2 by CC4 cage
  s16d <- data.table::rbindlist(s16)
  if (nrow(s16d)) {
    rr <- s33e_rows(s16d$id, s16d$estimate, s16d$se, s16d$df, s16d$ci_low, s16d$ci_high, "CR2_Satterthwaite_cc4_cage", cs, s16d$n_animals,
                    s16d$n_clusters, s16d$n_cohorts, s16d$n_params)
    rr[, model_id := vapply(cs[s16d$id], `[[`, "", "model")]
    add("S16", "CR2 by CC4 cage", rr)
  }
  e06 <- data.table::rbindlist(sens, fill = TRUE)
  e06[, `:=`(lead = FALSE, c_s3_delta_peer_per_frozen_sd = NA_real_)]
  e06[sensitivity_id == "S15", note := paste0(note, "; C's S3 delta_peer per frozen SD belongs beside this row (cross-module gate X8, Phase 4)")]
  front <- c("sensitivity_id", "sensitivity", "model_id", "estimand", "primary_estimand")
  data.table::setcolorder(e06, c(front, setdiff(names(e06), front)))
  # ---- GE-09 (Phase-2 products unchanged) and GE-11 (manual CR2 = clubSandwich)
  gates[[length(gates) + 1L]] <- s33e_gate_products(p2)
  gates[[length(gates) + 1L]] <- s33e_gate_cr2(data.table::rbindlist(g11), st$sim_check)
  e03 <- data.table::rbindlist(e03, fill = TRUE)
  tables <- list(e01_exposures_animal = p2$products$e01_exposures_animal, e02_dyads = p2$products$e02_dyads, e03_models = e03,
                 e04_contrasts = e04, e05_simulation = p2$products$e05_simulation, e06_sensitivities = e06,
                 e07_age_descriptive = p2$products$e07_age_descriptive, e08_jackknife_cc1cage = e08)
  streams <- data.table::data.table(stream = "E_wild", seed = ctx$seeds[["E_wild"]], B = as.integer(ctx$B[["wild"]]),
    rng_kind = "Mersenne-Twister/Inversion/Rejection", unit_order = "focal CC1 CEIDs, C-locale radix", matrix_sha256 = ww$sha256)
  list(tables = tables[S33_TABLES[["E"]]], audit = stats::setNames(list(), character()), gates = data.table::rbindlist(gates),
       checkpoints = st$checkpoints, streams = streams)
}
