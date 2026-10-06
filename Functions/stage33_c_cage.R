# ================================================================
# Stage 33 - module C: parts of the Stage 29 slope (C_cc1_cage/)
# MMMSociability
# ================================================================
# Post hoc, descriptive (estimation only) module C of the frozen plan docs/STAGE33_POSTHOC_COHORT_FOLLOWUPS_PLAN_v1.0.md
# (section 10, with sections 0-7 and 13-17). Definitions only: sourcing this file reads nothing and writes nothing.
#
# The frozen Stage 29 v1.0.1 within-cohort SIS slope of CombZ on x (x = the CC1 A1 RFID position-change rate,
# identifier crossing_rate, or shared RFID-position occupancy, identifier shared_zone_use) is re-sliced into
#   beta_W  within the CC1 cage            (w = x - xbar_c; xbar_c = cage mean over the analysed animals, own included),
#   beta_B  between CC1 cages in a cohort  (b = xbar_c - xbar_k; own-animal and cage-mate parts together),
#   gamma   = beta_B - beta_W, the contextual (peer-composition) term,
# sex-averaged as the parent (sex_c = +1/2 F, -1/2 M). Cage = RFID board within a cohort. No p-value or test statistic is
# produced: KR rows come from the frozen engine with p and statistic dropped (s33_kr_contrast), CR2 rows from clubSandwich
# with p_values = FALSE (s33_cr2_contrast, s33_kr_fit_cr2), OLS rows from coef() and vcov().
#
# Entry points (module contract: Functions/stage33_common.R):
#   p2  <- s33c_phase2(design, ctx)          outcome-free: populations, terms, cage means, information shares and pi_k,
#                                             CC2-CC4 board offsets, reallocation reference, design sensitivity, exposure
#                                             ICCs, declared ranks and design CR2 df, the B6 CC1 one-file check (S2);
#                                             gates GC-C03, GC-C04a, GC-C05a-g, GC-C06, GC-C07a, GC-C12
#   out <- s33c_phase3(design_out, p2, ctx)  the placebo structure run (GC-C09) first, then the parent reproduction and
#                                             bridge (GC-C13) before any decomposition row, then M1 and its variants,
#                                             influence refits, ICCs and correlations; gates GC-C04b, GC-C07b, GC-C08a-c,
#                                             GC-C09, GC-C13a-b
# GC-C08a checks the identities as exact algebra: the identity form is evaluated at M1's variance parameters (no second
# optimisation), because two separately optimised non-singular REML fits agree only to the optimiser tolerance (KR df
# differ by up to about 1e-6, depending on the outcome vector). The separately fitted identity form (its own c07 rows)
# is compared with M1 in the recorded GC-C08c. The M1 sex-interaction coefficients are point values in c06 (columns
# sex_interaction_*), never c07 rows: c07 has no female-minus-male row.
# Both are thin readers around pure cores (s33c_phase2_core, s33c_phase3_core) that take in-memory inputs, so the whole
# path runs on synthetic data in Testing/tests/test_stage33_c_cage.R.
#
# Outcome-free products (S33C_OUTCOME_FREE_PRODUCTS) are re-extractable from the written tables: for each product P and
# its final table T, T[seq_len(nrow(P)), names(P), with = FALSE] equals P (c02 and c03 gain CombZ columns at the end;
# c06 and c10 gain Phase-3 rows after the Phase-2 rows; the other products are written unchanged).
#
# Seeds (ctx$seeds): C_realloc, C_r2, C_r3, C_r4 = reallocation of primary, S1, S2, S8; C_icc_resid1 / C_icc_resid2 =
# M1 parametric bootstraps (rate / occupancy); C_icc_empty = empty-model CombZ bootstrap; C_placebo = the within-cohort
# CombZ permutation; C_placebo_1..3 replace the three bootstrap streams in the placebo (B = ctx$B[['placebo']]).
#
# Requires data.table, digest, lme4, lmerTest, pbkrtest, clubSandwich; Functions/rfid_canonical_inference.R,
# stage32_inference.R, stage33_common.R and stage33_run.R; the one-file check also needs rfid_event_stream.R,
# rfid_binfree_metrics.R and stage30_movement.R (frozen; sourced by the runner after GC-1).
# ================================================================

# ---------------------------------------------------------------- constants (plan section 10)
S33C_METRICS <- c("crossing_rate", "shared_zone_use")
S33C_SD <- c(crossing_rate = S33_SD_RATE, shared_zone_use = S33_SD_OCC)
S33C_UNITS_RAW <- c(crossing_rate = "per position change/hour", shared_zone_use = "per 1.0 shared occupancy")
S33C_UNITS_SD <- c(crossing_rate = S33_UNITS$per_sd_rate, shared_zone_use = S33_UNITS$per_sd_occ)
S33C_UNITS_X <- c(crossing_rate = S33_UNITS$rate, shared_zone_use = "shared occupancy (fraction of co-assigned dyadic time)")
S33C_VARIANTS <- c("primary", "S1_excl_cages", "S2_drop692", "S8_4tracked", "S4_board_adj")
S33C_REALLOC_VARIANTS <- c(primary = "C_realloc", S1_excl_cages = "C_r2", S2_drop692 = "C_r3", S8_4tracked = "C_r4")
S33C_BOOT_SEEDS <- c(crossing_rate = "C_icc_resid1", shared_zone_use = "C_icc_resid2", empty = "C_icc_empty")
S33C_PLACEBO_SEEDS <- c(crossing_rate = "C_placebo_1", shared_zone_use = "C_placebo_2", empty = "C_placebo_3")
S33C_PRIMARY_ESTIMANDS <- c("within_cc1_cage", "between_cc1_cages_within_cohort", "contextual_peer_composition")
S33C_L <- list(within_cc1_cage = list(w = 1), between_cc1_cages_within_cohort = list(b = 1),
               contextual_peer_composition = list(b = 1, w = -1))
S33C_LEVEL <- c(within_cc1_cage = "animal_within_cohort", between_cc1_cages_within_cohort = "cage_within_cohort",
                contextual_peer_composition = "cage_within_cohort", delta_peer = "cage_within_cohort",
                within_cc1_cage_matched = "animal_within_cohort", gamma_matched = "cage_within_cohort",
                parent_within_cohort_slope = "animal_within_cohort", board_offset = "cage_within_cohort",
                partial_r_cage = "cage_within_cohort", partial_r_within_cage = "animal_within_cohort",
                icc_cc1_cage = "cage_within_cohort", cage_variance_cc1 = "cage_within_cohort",
                moment_icc_cc1_cage = "cage_within_cohort", exposure_icc_cc1_cage = "cage_within_cohort",
                exposure_moment_icc_cc1_cage = "cage_within_cohort")
# Level wording of plan 17.2 C (gamma and delta_peer only are 'the contextual (peer-composition) term').
S33C_ESTIMAND_LABEL <- c(within_cc1_cage = "within the CC1 cage",
                         between_cc1_cages_within_cohort = "between CC1 cages within a cohort, own-animal and cage-mate parts together",
                         contextual_peer_composition = "the contextual (peer-composition) term",
                         gamma_matched = "cohort-matched version of the contextual (peer-composition) term (S11)",
                         delta_peer = "leave-one-out version of the contextual (peer-composition) term (S3)",
                         within_cc1_cage_matched = "within the CC1 cage, cohort-matched weights (S11)")
S33C_FORMULAS <- c(
  M1 = "CombZ ~ Batch + w + b + w:sex_c + b:sex_c + (1 | CageEpisodeID)",
  identity = "CombZ ~ Batch + x + xbar_c + x:sex_c + xbar_c:sex_c + (1 | CageEpisodeID)",
  S3_peer_loo = "CombZ ~ Batch + x + xbar_loo + x:sex_c + xbar_loo:sex_c + (1 | CageEpisodeID)",
  C_pooled = "CombZ ~ Batch + w + b + (1 | CageEpisodeID)",
  S9_cc1_weight = "CombZ ~ Batch + w + b + w:sex_c + b:sex_c + bw_w + bw_b + (1 | CageEpisodeID)",
  S10_board_terms = "CombZ ~ Batch + sys_e1 + sys_e2 + sys_e3 + sys_e4 + w + b + w:sex_c + b:sex_c + (1 | CageEpisodeID)",
  S11_matched = "CombZ ~ Batch + w_B1 + w_B2 + w_B3 + w_B4 + w_B5 + w_B6 + b + b:sex_c + (1 | CageEpisodeID)",
  S7_stratum = "CombZ ~ Batch + w + b + (1 | CageEpisodeID)",
  empty = "CombZ ~ Batch + (1 | CageEpisodeID)",
  exposure = "x ~ Batch + (1 | CageEpisodeID)",
  offsets = "y ~ 0 + Session + g_SIS + sys_e1 + sys_e2 + sys_e3 + sys_e4 + (1 | AnimalNum) + (1 | CageEpisodeID)",
  cage_aggregate = "ybar ~ Batch + xbar_c + xbar_c:sex_c",
  cage_dummy = "CombZ ~ factor(CageEpisodeID) + w + w:sex_c",
  parent_pooled = "CombZ ~ Batch + x + x:sex_c",
  parent_by_sex = "CombZ ~ Batch + x",
  within_between_by_sex = "CombZ ~ Batch + w + b")
S33C_RANKS <- c(M1 = 10L, identity = 10L, S3_peer_loo = 10L, C_pooled = 8L, S9_cc1_weight = 12L, S10_board_terms = 14L,
                S11_matched = 14L, S7_stratum = 5L, LOBO = 9L, empty = 6L, exposure = 6L, offsets_CC2_4 = 23L,
                offsets_all = 29L, cage_aggregate = 8L, cage_dummy = 24L)
# Plan facts used by the gates (sections 3 and 10); the pure cores take them as an argument so that tests can pass
# facts of a synthetic design.
S33C_FACTS <- list(
  counts = list(
    primary = list(n = 85L, cages = 22L, female = c(46L, 12L), male = c(39L, 10L),
                   by_cohort = c(B1 = 8L, B2 = 16L, B3 = 16L, B4 = 15L, B5 = 15L, B6 = 15L),
                   cages_by_cohort = c(B1 = 2L, B2 = 4L, B3 = 4L, B4 = 4L, B5 = 4L, B6 = 4L), n4 = 19L, n3 = 3L),
    S1_excl_cages = list(n = 79L, cages = 20L, female = c(40L, 10L), male = c(39L, 10L)),
    S2_drop692 = list(n = 84L, cages = 22L, n2 = 1L),
    S8_4tracked = list(n = 76L, cages = 19L, female = c(40L, 10L), male = c(36L, 9L)),
    LOBO = list(B1 = c(77L, 20L), B2 = c(69L, 18L), B3 = c(69L, 18L), B4 = c(70L, 18L), B5 = c(70L, 18L), B6 = c(70L, 18L)),
    leave_one_cage_out = c(81L, 82L),
    parent = list(crossing_rate = c(87L, 24L), shared_zone_use = c(85L, 22L))),
  pi = list(crossing_rate = c(B1 = 0.00338, B2 = 0.14003, B3 = 0.72921, B4 = 0.06896, B5 = 0.85658, B6 = 0.20183),
            shared_zone_use = c(B1 = 0.24134, B2 = 0.70216, B3 = 0.03422, B4 = 0.71186, B5 = 0.05649, B6 = 0.25392)),
  pi_tolerance = 5e-6,
  design_cr2_df = list(crossing_rate = c(within_cc1_cage = 6.2, between_cc1_cages_within_cohort = 3.7, contextual_peer_composition = 5.4),
                       shared_zone_use = c(within_cc1_cage = 12.6, between_cc1_cages_within_cohort = 6.0, contextual_peer_composition = 9.3)),
  design_df_tolerance = 0.05,
  exposure_icc = c(crossing_rate = 0.034, shared_zone_use = 0.207),
  s2_animal = "692", s2_mates = c("662", "695"), s2_no692 = 0.145527854729,
  xmate_unanalysed = c("B4|sys.1|CC1" = "OR567", "B6|sys.5|CC1" = "655"),
  bw_range = c(6, 16), aggregate_df = 14L, dummy_df = 61L, dummy_rank = 24L, one_file_n = 19L)
S33C_OUTCOME_FREE_PRODUCTS <- c("c01_population", "c02_animal_terms", "c03_cage_means", "c04_information", "c05_reallocation",
                                "c06_models", "c10_variance", "c12_board_offsets", "c13_design_sensitivity",
                                "c_expected_value_checks")
# Engine-message and path columns of module C (plan section 7; read by s33_lint_exempt_columns()): lme4 / lmerTest messages,
# fit errors and failure reasons in c06, and the expected-value file each check in c_expected_value_checks compares with.
S33C_LINT_EXEMPT_COLUMNS <- c("messages", "error", "failure_reason", "formula", "source")
# Readings of the plan recorded in the README (s33_deviations()).
S33C_DEVIATIONS <- c(
  paste("GC-C08 (PLAN ISSUE): two separately optimised REML fits agree only to the optimiser tolerance, so GC-C08a evaluates",
        "the identity form at M1's variance parameters (no second optimisation; absolute 1e-8). The separately optimised identity",
        "form keeps its c07 rows and is compared with M1 in the recorded GC-C08c (relative 1e-6)."),
  paste("M1's sex-interaction coefficients are point values on the M1 rows of c06 (sex_interaction_* columns with the plan's",
        "label), not c07 rows: 'point values only' and 'c07 without female-minus-male rows' together."),
  "Rows without an interval carry interval_basis 'none', as in module D.",
  "c13 has metric_id and metric_label NA (one CC1-cage design for both metrics), lead FALSE and interval_basis 'none'.",
  paste("GC-C08a leaves out components that need a FAILED fit (recorded as not evaluated if none remain); a parametric",
        "bootstrap error gives FAILED rows without an estimate."),
  paste("The c08 copies of the frozen Stage 29 interval are named parent_frozen_lower / parent_frozen_upper (equal to",
        "ci_low / ci_high within 1e-9), so that the README interval count does not count them twice."),
  paste("The values the plan says are always quoted are columns of the c07 primary rows: top4_cage_info_share,",
        "c13_halfwidth_r, leave_one_cage_out_min/max_estimate (beta_B and gamma) and cr2_design_df_ols."),
  "c10 cage-variance rows give the bootstrap percentiles (or the spread at a boundary) as bootstrap_low / bootstrap_high.",
  "GC-C04b (the outcome join) is checked at the start of Phase 3, before the placebo.")
# GC-C08 tolerances: exact identities (plan 1e-8, absolute); the recorded comparison of the separately optimised identity
# form with M1 (relative to max(1, |value|); the optimiser tolerance).
S33C_IDENTITY_TOL <- 1e-8
S33C_SEPARATE_FIT_TOL <- 1e-6
S33C_NOTE <- c(
  sex_interaction = "female-cohort set minus male-cohort set; sex and cohort not separable; point value only",
  mech_coupling = "CombZ contains weight_dev (tp2 baseline) and two organ ratios to body weight",
  parent = "Stage 29 v1.0.1 row reproduced; not a new estimate",
  parent_did = "Stage 29 v1.0.1 row reproduced (the registered H13 quantity on the per-change/h scale); not a new estimate",
  parent_85 = "parent-form OLS slope on the 85 analysed animals of the 22 CC1 cages; algebraic bridge only",
  bridge = "per sex, parent slope = (SSw beta_W + SSb beta_B) / (SSw + SSb) with OLS within-cage and between-cage slopes; sex-averaged = mean of the sexes; a re-slicing of an already seen association, not new evidence",
  spread = "spread under the fitted zero component, not a confidence limit",
  c13 = "design property in a two-sided 5% test frame; no test is run",
  strata = "estimation only; sex is nested in cohort (3 v 3)",
  strata_mean = "mean of the two sex strata (SE sqrt(seF^2 + seM^2) / 2, Welch df); estimation only; sex is nested in cohort (3 v 3)",
  realloc = "x permuted across the CC1 cages of its cohort with cage sizes kept (p[, xp := sample(x), by = Batch] on the AnimalNum-keyed frame); observed SD of b_c beside the reference median and central 95% range; no tail share",
  board = "model-based SE (descriptive plug-in, not propagated); offsets sum to 0 over sys.1-sys.5; cage = board within a cohort",
  aggregate = "cage-aggregate WLS, one row per CC1 cage (weights n_c), t with the residual df",
  dummy = "cage-dummy OLS (within-cage estimator), t with the residual df",
  welch = "two-part Welch: cage-aggregate beta_B minus cage-dummy beta_W, SE sqrt(sB^2 + sW^2), Welch-Satterthwaite df",
  cr2 = "CR2 by CC1 cage on the lmer working model, Satterthwaite df",
  pb = "parametric bootstrap (bootMer, type parametric, use.u = FALSE), percentiles type 7",
  identity = "identity form: gamma is the xbar_c coefficient and beta_W the x coefficient of the same fit (optimised separately; agrees with M1 to the optimiser tolerance, GC-C08c)",
  leave_one_cage_out_range = "leave-one-cage-out range of the estimate over the refits without one CC1 cage (c09); not an interval",
  s3 = "leave-one-out cage-mate mean (xbar_loo); delta_peer about 0.74 gamma; same variation as module E's CC1-class contrasts (S15), counted once",
  pooled = "comparator without sex terms; tables only; never beside the parent",
  s4 = "x minus the CC2-CC4 board offset of the CC1 board (exposure side only; offset SEs not propagated)",
  s10 = "sum-to-zero coded board terms in the CombZ model (both sides)",
  s11 = "cohort-specific within-cage slopes combined with the fixed between-cage weights pi_k (sex-averaged)",
  s1 = "without the CC1 cages that also housed an excluded, unanalysed animal through A1 (B4|sys.1, B6|sys.5)",
  s2 = "influence check without 692, the animal with the lowest A1 RFID position-change rate (only 7 of 8 RFID positions registered; retained under frozen S13; lightest SIS animal at CC1); dropping it selects on x; occupancy of 662 and 695 recomputed without the 692 dyads (GC-C06)",
  s8 = "only CC1 cages with 4 analysed animals",
  lobo = "LOBO (leave one cohort out); sign stability reported as k of 6",
  locage = "leave-one-cage-out refit",
  icc_empty = "CombZ ~ Batch + (1 | CC1 cage): clustering of later CombZ among CC1 cage-mates beyond the cohort",
  icc_resid = "residual cage share of M1",
  icc_exposure = "x ~ Batch + (1 | CC1 cage); model-based only",
  moment = "signed one-way moment ICC of cohort-centred values by CC1 cage (may be negative)",
  profile = "profile likelihood of the CC1-cage SD (REML), squared",
  icc_upper = "icc_profile_upper: upper 95% profile limit of the CC1-cage SD at the fitted residual variance",
  corr_cage = "n_c-weighted; equals the partial correlation of the cage-aggregate WLS on Batch; the weighted cage-level interval is approximate",
  corr_within = "animals: w against CombZ minus its CC1-cage mean")

# ---------------------------------------------------------------- small helpers
s33c_sys_cols <- function(d, col = "System") {
  for (k in 1:4) data.table::set(d, j = paste0("sys_e", k), value = as.numeric(d[[col]] == paste0("sys.", k)) - as.numeric(d[[col]] == "sys.5"))
  invisible(d)
}

#' Difference relative to max(1, |a|, |b|) (the recorded GC-C08c comparison of separately optimised fits).
s33c_reldiff <- function(a, b) abs(a - b) / pmax(1, abs(a), abs(b))

#' The section-7 metric label of a metric identifier (NA for none, e.g. the CombZ-only models).
s33c_label <- function(metric) { out <- rep(NA_character_, length(metric)); k <- !is.na(metric) & metric %in% names(S33_METRIC_LABEL)
  out[k] <- unname(S33_METRIC_LABEL[metric[k]]); out }

#' interval_basis of a row: no interval, no basis; otherwise conditional on the six cohorts (fixed-Batch models, CC1-cage
#' clustering, within-cohort resampling).
s33c_basis <- function(method) data.table::fifelse(is.na(method) | method == "none", "none", "conditional_on_cohorts")

#' sha256 of the columns of an in-memory frame (checkpoint keys).
s33c_frame_sha <- function(d) digest::digest(lapply(as.list(d), function(v) v), algo = "sha256")

#' Sort cages and animals in C-locale radix order.
s33c_sort <- function(x, ...) data.table::setorderv(x, c(...))

#' The module's checkpoint wrapper: s33_checkpoint() with a local log (returned to the runner as `checkpoints`); rows
#' are also appended to ctx$checkpoint_log when the runner provides one.
s33c_ckpt <- function(ctx, step, fun, key_extra, log) {
  c2 <- ctx; c2$checkpoint_log <- log
  v <- s33_checkpoint(c2, "C", step, fun, key_extra)
  v
}
#' Rows of a local checkpoint log in the module-contract form data.table(module, step, checkpoint_key, file, reused); they
#' are also appended to ctx$checkpoint_log (the runner's record) when the runner provides one.
s33c_ckpt_rows <- function(log, ctx = NULL) {
  nm <- sort(ls(log))
  x <- if (length(nm)) data.table::rbindlist(mget(nm, envir = log)) else
    data.table::data.table(module = character(), step = character(), checkpoint_key = character(), file = character(), reused = logical())
  if (!is.null(ctx) && is.environment(ctx$checkpoint_log) && nrow(x)) for (i in seq_len(nrow(x))) {
    n <- length(ls(ctx$checkpoint_log)) + 1L
    assign(sprintf("%06d", n), x[i], envir = ctx$checkpoint_log)
  }
  x[, .(module, step, checkpoint_key, file, reused)]
}

#' An lmer model evaluated at given variance parameters theta, without optimisation (lme4: optimizer = NULL evaluates the
#' REML criterion at the start values). Used only for the exact GC-C08a identity of the identity form at M1's variance
#' parameters; its values never enter a table. Returns a frozen-engine-like list(fit, info, data) for s33_kr_contrast().
s33c_fit_at_theta <- function(formula, data, theta, model_id) {
  d <- droplevels(data.table::as.data.table(data))
  fit <- tryCatch(suppressMessages(suppressWarnings(lmerTest::lmer(stats::as.formula(formula), data = d, REML = TRUE,
    start = list(theta = unname(theta)), control = lme4::lmerControl(optimizer = NULL, calc.derivs = FALSE, check.rankX = "stop.deficient")))),
    error = function(e) e)
  bad <- inherits(fit, "error")
  list(fit = if (bad) NULL else fit, data = d,
       info = data.table::data.table(model_id = model_id, singular = if (bad) NA else lme4::isSingular(fit), failed = bad,
                                     error = if (bad) conditionMessage(fit) else NA_character_))
}

# ---------------------------------------------------------------- frames and terms (Phase 2)
#' The 87 tracked SIS animals (keyed by AnimalNum, C locale): CC1 cage, board, x, CC1-day weight (tp2), focal flag.
s33c_base <- function(design, odisp) {
  a <- design$animals[pop_sis_87 == TRUE]
  ep1 <- design$episodes[CC == "CC1"]
  d <- a[, .(AnimalNum, Batch, Sex, condition, src_cage, CageEpisodeID = cc1_cage, System = cc1_board, crossing_rate,
             shared_zone_use, tp2, focal = pop_focal_85, xmate = xmate_excluded)]
  d[, n_tr := .N, by = CageEpisodeID]
  d[, hardware_flag := ep1$hardware_flag[match(AnimalNum, ep1$AnimalNum)]]
  d[, occupancy_dispersion := odisp$occupancy_dispersion[match(AnimalNum, odisp$AnimalNum)]]
  d[, sex_c := data.table::fifelse(Sex == "Female", 0.5, -0.5)]
  s33c_sys_cols(d)
  data.table::setkeyv(d, "AnimalNum")
  d[]
}

#' Terms of plan section 10, recomputed on the rows given: x, n_c, xbar_c, xbar_k, w, b, xbar_loo, bw parts, w_Bk.
#' x_fix: named values replacing x (S2 occupancy); offsets: named board offsets subtracted from x (S4).
s33c_terms <- function(d, metric, offsets = NULL, x_fix = NULL) {
  d <- data.table::copy(d)
  d[, x := get(metric)]
  if (length(x_fix)) d[AnimalNum %in% names(x_fix), x := unname(x_fix[AnimalNum])]
  if (!is.null(offsets)) d[, x := x - unname(offsets[System])]
  d[, n_c := .N, by = CageEpisodeID][, xbar_c := mean(x), by = CageEpisodeID][, xbar_k := mean(x), by = Batch]
  d[, `:=`(w = x - xbar_c, b = xbar_c - xbar_k)]
  d[, xbar_loo := data.table::fifelse(n_c > 1L, (n_c * xbar_c - x) / (n_c - 1), NA_real_)]
  d[, bw_c := mean(tp2), by = CageEpisodeID][, bw_k := mean(tp2), by = Batch][, `:=`(bw_w = tp2 - bw_c, bw_b = bw_c - bw_k)]
  for (k in S33_COHORTS) data.table::set(d, j = paste0("w_", k), value = d$w * as.numeric(d$Batch == k))
  d[]
}

#' GC-C05b sums of a terms frame: sum w per cage and sum n_c b_c per cohort (all 0 by construction).
s33c_term_sums <- function(t) c(t[, .(s = sum(w)), by = CageEpisodeID]$s,
                                unique(t[, .(CageEpisodeID, Batch, n_c, b)])[, .(s = sum(n_c * b)), by = Batch]$s)

#' Rows of a variant (plan section 10): S1 without the XMATE animals, S2 without the S2 animal, S8 only 4-animal cages.
s33c_variant_rows <- function(base, variant, facts = S33C_FACTS) {
  f <- base[focal == TRUE]
  switch(variant, primary = f, S4_board_adj = f, S1_excl_cages = f[xmate == FALSE],
         S2_drop692 = f[AnimalNum != facts$s2_animal], S8_4tracked = f[n_tr == 4L],
         stop("Unknown module C variant: ", variant, call. = FALSE))
}
s33c_variant_terms <- function(base, variant, metric, offsets, s2_fix, facts = S33C_FACTS) {
  s33c_terms(s33c_variant_rows(base, variant, facts), metric,
             offsets = if (variant == "S4_board_adj") offsets else NULL,
             x_fix = if (variant == "S2_drop692" && metric == "shared_zone_use") s2_fix else NULL)
}

#' Unanalysed CC1 cage-mates: untracked SIS animals with a (planned) CC1 cage, plus the excluded mates of the XMATE cages.
s33c_unanalysed <- function(design, facts = S33C_FACTS) {
  a <- design$animals
  u <- a[condition == "SIS" & tracked == FALSE & !is.na(cc1_cage), .(AnimalNum, cc1_cage)]
  x <- data.table::data.table(AnimalNum = unname(facts$xmate_unanalysed), cc1_cage = names(facts$xmate_unanalysed))
  rbind(u, x)[, .(ids = paste(sort(AnimalNum, method = "radix"), collapse = ";"), n = .N), by = cc1_cage]
}

# ---------------------------------------------------------------- descriptive tables (Phase 2)
s33c_cage_table <- function(t, metric, variant, offsets_cc24) {
  cg <- t[, .(Sex = Sex[1], Batch = Batch[1], System = System[1], n_c = .N, xbar_c = xbar_c[1], xbar_k = xbar_k[1],
              b_c = xbar_c[1] - xbar_k[1], sd_within_cage = stats::sd(x), min_x = min(x), max_x = max(x),
              bw_cage_mean = bw_c[1], bw_b = bw_c[1] - bw_k[1], occdisp_cage_mean = mean(occupancy_dispersion)), by = CageEpisodeID]
  cg[, `:=`(b_c_per_frozen_sd = b_c / S33C_SD[[metric]], info_share = n_c * b_c^2 / sum(n_c * b_c^2),
            board_offset_cc2_4 = unname(offsets_cc24[System]))]
  if (variant == "S4_board_adj") cg[, b_c_board_adj := b_c] else {
    a <- t[, .(CageEpisodeID, Batch, xa = x - unname(offsets_cc24[System]))]
    a[, xa_c := mean(xa), by = CageEpisodeID][, xa_k := mean(xa), by = Batch]
    u <- a[, .(v = xa_c[1] - xa_k[1]), by = CageEpisodeID]
    cg[, b_c_board_adj := u$v[match(CageEpisodeID, u$CageEpisodeID)]]
  }
  cg[, `:=`(variant = variant, metric_id = metric)]
  cg[]
}

s33c_information <- function(t, metric, variant) {
  s <- t[, .(n_animals = .N, n_cages = data.table::uniqueN(CageEpisodeID), SS_within = sum(w^2), SS_between = sum(b^2)), by = .(Sex, Batch)]
  s[, `:=`(share_within_all = SS_within / sum(SS_within), share_between_all = SS_between / sum(SS_between))]
  s[, `:=`(pi_between_within_sex = SS_between / sum(SS_between),
           between_share_of_within_cohort_ss_sex = sum(SS_between) / (sum(SS_within) + sum(SS_between))), by = Sex]
  s33c_sort(s, "Batch")
  cbind(data.table::data.table(metric_id = metric, metric_label = unname(S33_METRIC_LABEL[[metric]]), variant = variant), s)
}

#' Top-4 cages by information share (n_c b_c^2 / sum) and their summed share.
s33c_top4 <- function(cg) {
  o <- cg[order(-info_share, CageEpisodeID)][seq_len(min(4L, nrow(cg)))]
  list(cages = paste(o$CageEpisodeID, collapse = ";"), share = sum(o$info_share))
}

#' Board offsets (plan section 10, S4): y ~ 0 + Session + g_SIS + sys_e1..4 + (1 | AnimalNum) + (1 | CageEpisodeID) on the
#' CC2-CC4 rows (used) and on all rows (reference); theta_sys.5 = -sum(theta_1..4). The SIS-vs-CON term is never returned.
s33c_board_offsets <- function(episodes) {
  rows <- list(); fits <- list(); offsets <- list()
  for (m in S33C_METRICS) for (rs in c("CC2_4", "all")) {
    dd <- data.table::copy(episodes)[is.finite(get(m))]
    if (rs == "CC2_4") dd <- dd[CC != "CC1"]
    dd <- dd[, .(AnimalNum, Batch, CC, CageEpisodeID, System = board, condition, y = get(m))]
    dd[, `:=`(Session = paste(Batch, CC, sep = ":"), g_SIS = data.table::fifelse(condition == "CON", -0.5, 0.5))]
    dd[, condition := NULL]
    s33c_sys_cols(dd)
    er <- data.table::uniqueN(dd$Session) + 5L
    id <- paste(m, paste0("offsets_", rs), "board_offsets", sep = "|")
    fm <- s33_kr_fit(S33C_FORMULAS[["offsets"]], dd, id, er)
    if (s32i_failed(fm)) { off <- rep(NA_real_, 5L); se <- rep(NA_real_, 5L) } else {
      cf <- paste0("sys_e", 1:4); sy <- lme4::fixef(fm$fit)[cf]; off <- c(unname(sy), -sum(sy))
      Vb <- as.matrix(stats::vcov(fm$fit))[cf, cf]; L <- rbind(diag(4), rep(-1, 4)); se <- sqrt(diag(L %*% Vb %*% t(L)))
    }
    offsets[[m]][[rs]] <- stats::setNames(off, paste0("sys.", 1:5))
    fits[[length(fits) + 1L]] <- s33c_model_row(fm, m, paste0("offsets_", rs), "board_offsets", "descriptive", er)
    rows[[length(rows) + 1L]] <- s33_row(
      estimand = "board_offset", level = "cage_within_cohort", estimate = off, se = se, interval_method = "none",
      interval_basis = "none", units = S33C_UNITS_X[[m]], metric_id = m, status = if (s32i_failed(fm)) "FAILED" else "OK",
      singular = fm$info$singular, ddf_fallback = NA, lead = FALSE, n_animals = data.table::uniqueN(dd$AnimalNum),
      n_cages = data.table::uniqueN(dd$CageEpisodeID), n_cohorts = data.table::uniqueN(dd$Batch), n_params = fm$info$n_fixed_cols,
      rows_used = rs, row_role = if (rs == "CC2_4") "used (S4)" else "reference", System = paste0("sys.", 1:5), n_rows = nrow(dd),
      model_id = id, note = S33C_NOTE[["board"]])
  }
  list(rows = data.table::rbindlist(rows), fits = data.table::rbindlist(fits), offsets = offsets)
}

#' Within-cohort random reallocation of x across the cohort's CC1 cages, cage sizes kept (plan section 10, Phase 2).
#' Vectorized form of replicate(B, { p <- copy(d)[, xp := sample(x), by = Batch]; ...; sd(b_c) }) on the AnimalNum-keyed
#' frame: one set.seed per stream, cohorts in order of first appearance, sample(x) per cohort (identical RNG use).
s33c_reallocation_draws <- function(x, batch, cage, B, seed) {
  grp <- split(seq_along(x), factor(batch, levels = unique(batch)))
  if (any(lengths(grp) < 2L)) stop("Reallocation needs at least 2 animals per cohort.", call. = FALSE)
  old <- RNGkind(); on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(seed)
  cf <- factor(cage, levels = unique(cage)); n_c <- tabulate(cf)
  cage_batch <- batch[match(levels(cf), cage)]
  bf <- factor(batch, levels = unique(batch)); n_k <- tabulate(bf)
  ck <- match(cage_batch, levels(bf))
  out <- numeric(B); xp <- x
  for (r in seq_len(B)) {
    for (g in grp) xp[g] <- sample(x[g])
    xc <- as.vector(rowsum(xp, cf, reorder = TRUE)) / n_c
    xk <- as.vector(rowsum(xp, bf, reorder = TRUE)) / n_k
    out[r] <- stats::sd(xc - xk[ck])
  }
  out
}

#' Minimum detectable partial r (80%, two-sided 5% frame; design property) and the expected 95% half-width at r = 0.
s33c_mde_t <- function(df, tol = .Machine$double.eps^0.25) {
  f <- function(ncp) 1 - stats::pt(stats::qt(0.975, df), df, ncp) + stats::pt(-stats::qt(0.975, df), df, ncp) - 0.8
  n <- stats::uniroot(f, c(0, 30), tol = tol)$root
  n / sqrt(n^2 + df)
}
s33c_mde_fisher <- function(df) tanh((stats::qnorm(0.975) + stats::qnorm(0.8)) / sqrt(df - 1))
s33c_halfwidth_fisher <- function(df) tanh(stats::qnorm(0.975) / sqrt(df - 1))

#' Cage-level frame of a terms frame (one row per CC1 cage).
s33c_cage_frame <- function(t) {
  t[, .(xbar_c = xbar_c[1], n_c = .N, Batch = Batch[1], Sex = Sex[1], sex_c = sex_c[1], bw_c = bw_c[1], sys_e1 = sys_e1[1],
        sys_e2 = sys_e2[1], sys_e3 = sys_e3[1], sys_e4 = sys_e4[1]), by = CageEpisodeID]
}

#' Design sensitivity (c13): residual df of the cage-aggregate designs and of the cage-dummy design.
s33c_design_sensitivity <- function(tv) {
  rk <- function(f, d) qr(stats::model.matrix(stats::as.formula(f), data = d))$rank
  agg <- "~ Batch + xbar_c + xbar_c:sex_c"
  cp <- s33c_cage_frame(tv$primary)
  one <- function(lab, variant, lev, n, df) data.table::data.table(design_row = lab, variant = variant, level = lev, n_units = as.integer(n),
                                                                   residual_df = as.integer(df))
  out <- list(
    one(sprintf("cage primary G%d", nrow(cp)), "primary", "cage_within_cohort", nrow(cp), nrow(cp) - rk(agg, cp)),
    { c1 <- s33c_cage_frame(tv$S1_excl_cages); one(sprintf("cage S1 G%d", nrow(c1)), "S1_excl_cages", "cage_within_cohort", nrow(c1), nrow(c1) - rk(agg, c1)) },
    { c2 <- s33c_cage_frame(tv$S2_drop692); one(sprintf("cage S2 G%d", nrow(c2)), "S2_drop692", "cage_within_cohort", nrow(c2), nrow(c2) - rk(agg, c2)) },
    { c8 <- s33c_cage_frame(tv$S8_4tracked); one(sprintf("cage S8 G%d", nrow(c8)), "S8_4tracked", "cage_within_cohort", nrow(c8), nrow(c8) - rk(agg, c8)) },
    one(sprintf("cage S9 (+bw) G%d", nrow(cp)), "S9_cc1_weight", "cage_within_cohort", nrow(cp), nrow(cp) - rk("~ Batch + bw_c + xbar_c + xbar_c:sex_c", cp)),
    one(sprintf("cage S10 (+board) G%d", nrow(cp)), "S10_board_terms", "cage_within_cohort", nrow(cp),
        nrow(cp) - rk("~ Batch + sys_e1 + sys_e2 + sys_e3 + sys_e4 + xbar_c + xbar_c:sex_c", cp)),
    { cf <- cp[Sex == "Female"]; one(sprintf("cage F G%d", nrow(cf)), "S7_Female", "cage_within_cohort", nrow(cf), nrow(cf) - rk("~ Batch + xbar_c", cf)) },
    { cm <- cp[Sex == "Male"]; one(sprintf("cage M G%d", nrow(cm)), "S7_Male", "cage_within_cohort", nrow(cm), nrow(cm) - rk("~ Batch + xbar_c", cm)) },
    one(sprintf("within primary N%d", nrow(tv$primary)), "primary", "animal_within_cohort", nrow(tv$primary),
        nrow(tv$primary) - rk("~ factor(CageEpisodeID) + w + w:sex_c", tv$primary)))
  x <- data.table::rbindlist(out)
  x[, `:=`(min_detectable_r_noncentral_t = vapply(residual_df, s33c_mde_t, 0, tol = 1e-10),
           min_detectable_r_fisher = s33c_mde_fisher(residual_df), expected_halfwidth_r_fisher = s33c_halfwidth_fisher(residual_df),
           frame = S33C_NOTE[["c13"]])]
  # section-7 columns: a design property of the CC1-cage structure, the same for both metrics (no metric row)
  x[, `:=`(metric_id = NA_character_, metric_label = NA_character_,
           units = "partial correlation r (half-width and minimum detectable r); residual_df in degrees of freedom",
           lead = FALSE, interval_basis = "none", note = "same CC1-cage design for both metrics; computed before any fit", tier = S33_TIER)]
  x[]
}

#' Declared ranks (plan section 10) of every model design, from the model matrices (outcome-free). The cage-dummy rank
#' (number of CC1 cages + 2) is a fact of the design (facts$dummy_rank).
s33c_declared_ranks <- function(base, tv, facts = S33C_FACTS) {
  rk <- function(f, d) { X <- stats::model.matrix(stats::as.formula(f), data = d); c(cols = ncol(X), rank = qr(X)$rank) }
  rhs <- function(f) paste("~", sub("^[^~]*~", "", sub(" \\+ \\(1 \\| [A-Za-z]+\\)", "", f)))
  p <- tv$primary; out <- list()
  add <- function(model, variant, f, d, expected) {
    r <- rk(rhs(f), d)
    out[[length(out) + 1L]] <<- data.table::data.table(model = model, variant = variant, n_cols = r[["cols"]], rank = r[["rank"]], expected_rank = expected)
  }
  for (nm in c("M1", "identity", "S3_peer_loo", "C_pooled", "S9_cc1_weight", "S10_board_terms", "S11_matched"))
    add(nm, "primary", S33C_FORMULAS[[nm]], p, S33C_RANKS[[nm]])
  for (v in c("S1_excl_cages", "S2_drop692", "S8_4tracked", "S4_board_adj")) add("M1", v, S33C_FORMULAS[["M1"]], tv[[v]], S33C_RANKS[["M1"]])
  for (s in c("Female", "Male")) add("S7_stratum", paste0("S7_", s), S33C_FORMULAS[["S7_stratum"]], p[Sex == s], S33C_RANKS[["S7_stratum"]])
  f <- base[focal == TRUE]
  for (k in S33_COHORTS) add("M1", paste0("LOBO_-", k), S33C_FORMULAS[["M1"]], s33c_terms(f[Batch != k], "crossing_rate"), S33C_RANKS[["LOBO"]])
  for (cg in sort(unique(f$CageEpisodeID), method = "radix"))
    add("M1", paste0("leave_one_cage_out_-", cg), S33C_FORMULAS[["M1"]], s33c_terms(f[CageEpisodeID != cg], "crossing_rate"), S33C_RANKS[["M1"]])
  add("empty", "primary", S33C_FORMULAS[["empty"]], p, S33C_RANKS[["empty"]])
  cp <- s33c_cage_frame(p)
  add("cage_aggregate", "primary", S33C_FORMULAS[["cage_aggregate"]], cp, S33C_RANKS[["cage_aggregate"]])
  add("cage_dummy", "primary", S33C_FORMULAS[["cage_dummy"]], p, facts$dummy_rank)
  x <- data.table::rbindlist(out)
  x[, residual_df := NA_integer_]
  x[model == "cage_aggregate", residual_df := nrow(cp) - rank]
  x[model == "cage_dummy", residual_df := nrow(p) - rank]
  x[]
}

#' Design CR2 df (OLS working model; outcome-free: the Satterthwaite df depend only on the design and the CC1-cage
#' clusters, so the CC1-day weight serves as a placeholder response).
s33c_design_cr2_df <- function(t) {
  fo <- stats::lm(tp2 ~ Batch + w + b + w:sex_c + b:sex_c, data = t)
  vapply(S33C_PRIMARY_ESTIMANDS, function(e) s33_cr2_contrast(fo, t$CageEpisodeID, list(unlist(S33C_L[[e]])))$df, 0)
}

#' The B6 CC1 one-file run (plan GC-C06): the frozen window code reproduces A1 for the B6 animals, then the shared
#' occupancy of 662 and 695 is recomputed without the dyads involving 692. canonical_animal_id is bound into copies of the
#' frozen reader and seed functions with the frozen s30mv_with() (their bodies are unchanged).
s33c_one_file_run <- function(ctx, facts = S33C_FACTS) {
  canon <- ctx$canon %s33or% get0("canonical_animal_id", envir = globalenv(), mode = "function")
  if (is.null(canon)) stop("canonical_animal_id is not available for the one-file check.", call. = FALSE)
  pre_file <- ctx$inputs[["v2_B6_CC1"]]
  raw_dir <- ctx$inputs[["raw_dir"]] %s33or% dirname(dirname(ctx$inputs[["raw_B6_CC1"]]))
  bout <- S33_CODE_PINS$bout_criterion_s
  rd <- s30mv_with(mmm_evs_read_preprocessed, canonical_animal_id = canon)
  sd_fun <- s30mv_with(mmm_evs_seeds, canonical_animal_id = canon)
  reader <- s30mv_with(s30mv_read_preprocessed, mmm_evs_read_preprocessed = rd)
  builder <- s30mv_with(s30mv_build_stream, mmm_evs_seeds = sd_fun)
  pre <- reader(pre_file)
  st <- builder(pre, raw_dir)
  W <- mmm_evs_windows(pre)[, .(SourceFile, window_id = "A1", start = active_start, end = active_end)]
  m <- s30mv_window_metrics(st, W, bout_criterion_s = bout, na_if_file_ends_early = FALSE, strict_seed = TRUE, full = TRUE)
  ws <- s30mv_window_streams(st, W)[["A1"]]
  wt <- mmm_bf_window_table(ws, bout)
  a <- facts$s2_animal
  sz <- mmm_bf_shared_zone(wt$dyads[A != a & B != a], wt$animals[AnimalNum != a, .(AnimalNum, CC)])
  list(metrics = m[, .(AnimalNum, crossing_rate, shared_zone_use, n_events, n_tracked_mates, hardware_flag)],
       no_s2_animal = sz[, .(AnimalNum, CC, shared_zone_use, n_tracked_mates, dyadic_obs_s)], error = NA_character_)
}

#' One comparison block of c_expected_value_checks: obs vs exp merged by keys; one row per compared column.
s33c_compare <- function(check_id, item, obs, exp, keys, cols, tol, gated = TRUE, source = "") {
  m <- merge(data.table::as.data.table(exp), data.table::as.data.table(obs), by = keys, suffixes = c(".exp", ".obs"), all = TRUE)
  data.table::rbindlist(lapply(cols, function(cl) {
    e <- m[[paste0(cl, ".exp")]]; o <- m[[paste0(cl, ".obs")]]
    if (is.null(e) || is.null(o)) { okv <- rep(FALSE, max(1L, nrow(m))); d <- NA_real_ } else if (is.numeric(e) && is.numeric(o)) {
      d <- abs(e - o); okv <- (is.na(e) & is.na(o)) | (is.finite(d) & d <= tol)
    } else { d <- NA_real_; okv <- (is.na(e) & is.na(o)) | (!is.na(e) & !is.na(o) & as.character(e) == as.character(o)) }
    data.table::data.table(check_id = check_id, item = item, column = cl, n_expected = nrow(exp), n_compared = nrow(m),
                           n_ok = sum(okv), max_abs_diff = if (all(is.na(d))) NA_real_ else max(d, na.rm = TRUE), tolerance = tol,
                           ok = nrow(m) > 0L && nrow(m) == nrow(exp) && nrow(m) == nrow(obs) && all(okv), gated = gated, source = source)
  }))
}

#' Model registry row (c06) of a frozen-engine fit.
s33c_model_row <- function(m, metric, variant, model, role, expected_rank) {
  vc <- s33c_varcomp(m)
  i <- m$info
  data.table::data.table(model_id = i$model_id, metric_id = metric, metric_label = s33c_label(metric), variant = variant, model = model,
    role = role, engine = i$engine,
    formula = i$formula, n_obs = i$n_obs, n_animals = i$n_animals, n_cage_episodes = i$n_cage_episodes, n_batches = i$n_batches,
    n_fixed_cols = i$n_fixed_cols, rank = i$rank, expected_rank = as.integer(expected_rank), df_residual = NA_real_,
    singular = i$singular, converged = i$converged, optimizer_check_agree = i$optimizer_check_agree, failed = s32i_failed(m),
    var_cage = vc[["var_cage"]], var_resid = vc[["var_resid"]], icc_resid = vc[["var_cage"]] / (vc[["var_cage"]] + vc[["var_resid"]]),
    reml_criterion = if (s32i_failed(m)) NA_real_ else as.numeric(lme4::REMLcrit(m$fit)),
    messages = i$messages, error = i$error %s33or% NA_character_, failure_reason = m$failure_reason %s33or% NA_character_, tier = S33_TIER)
}
#' Model registry row of an lm fit.
s33c_lm_row <- function(fit, data, metric, variant, model, role, expected_rank) {
  data.table::data.table(model_id = paste(metric, variant, model, sep = "|"), metric_id = metric, metric_label = s33c_label(metric),
    variant = variant, model = model, role = role, engine = "lm", formula = paste(deparse(stats::formula(fit), width.cutoff = 500L), collapse = " "), n_obs = nrow(data),
    n_animals = if ("AnimalNum" %in% names(data)) data.table::uniqueN(data$AnimalNum) else NA_integer_,
    n_cage_episodes = data.table::uniqueN(data$CageEpisodeID), n_batches = data.table::uniqueN(data$Batch),
    n_fixed_cols = length(stats::coef(fit)), rank = fit$rank, expected_rank = as.integer(expected_rank), df_residual = fit$df.residual,
    singular = NA, converged = TRUE, optimizer_check_agree = NA, failed = anyNA(stats::coef(fit)), var_cage = NA_real_,
    var_resid = sum(stats::residuals(fit)^2 * (if (is.null(fit$weights)) 1 else fit$weights)) / fit$df.residual, icc_resid = NA_real_,
    reml_criterion = NA_real_, messages = "", error = NA_character_, failure_reason = NA_character_, tier = S33_TIER)
}
s33c_varcomp <- function(m) {
  if (s32i_failed(m)) return(c(var_cage = NA_real_, var_resid = NA_real_))
  v <- as.data.frame(lme4::VarCorr(m$fit))
  c(var_cage = sum(v$vcov[v$grp == "CageEpisodeID" & is.na(v$var2)]), var_resid = v$vcov[v$grp == "Residual"][1])
}

# ---------------------------------------------------------------- Phase 2
#' Read the Phase-2 inputs of module C (outcome-free; only declared columns) and run the one-file check.
s33c_read_phase2_inputs <- function(ctx, facts = S33C_FACTS) {
  p <- ctx$inputs
  odisp <- data.table::fread(p[["bundle_a1"]], select = c("AnimalNum", "occupancy_dispersion"),
                             colClasses = list(character = "AnimalNum"), showProgress = FALSE)
  s32 <- data.table::fread(p[["s32_window_metrics"]], select = c("AnimalNum", "CC", "phase", "CageEpisodeID", "crossing_rate",
                                                                  "shared_zone_use", "n_tracked_mates", "complete_primary"),
                           colClasses = list(character = "AnimalNum"), showProgress = FALSE)[CC == "CC1" & phase == "A1"]
  rd <- function(role, ...) data.table::fread(p[[role]], showProgress = FALSE, ...)
  expected <- list(bw = rd("expected_bw_cc1_primary", colClasses = list(character = "AnimalNum")),
                   cage_terms = rd("expected_cage_terms"), information = rd("expected_information_primary"),
                   offsets = rd("expected_board_offsets"), design_sensitivity = rd("expected_design_sensitivity"),
                   s2 = rd("expected_s2_shared_zone_no692", colClasses = list(character = "AnimalNum")),
                   reallocation = rd("expected_reallocation"))
  for (nm in c("odisp", "s32")) s33_assert_outcome_free(get(nm), paste("module C read", nm))
  for (nm in names(expected)) s33_assert_outcome_free(expected[[nm]], paste("module C expected file", nm))
  one <- tryCatch(s33c_one_file_run(ctx, facts), error = function(e) list(metrics = NULL, no_s2_animal = NULL, error = conditionMessage(e)))
  list(odisp = odisp, s32 = s32, expected = expected, one_file = one)
}

s33c_phase2 <- function(design, ctx) s33c_phase2_core(design, s33c_read_phase2_inputs(ctx), ctx)

#' Phase 2 of module C on in-memory inputs (inp: odisp, s32, expected, one_file).
s33c_phase2_core <- function(design, inp, ctx, facts = S33C_FACTS) {
  G <- list(); gate <- function(id, name, ok, detail = "", hard = TRUE, evaluated = TRUE, n_expected = length(ok))
    G[[length(G) + 1L]] <<- s33_gate_rows(id, name, ok, hard = hard, detail = detail, n_expected = n_expected, evaluated = evaluated)
  log <- new.env()
  base <- s33c_base(design, inp$odisp)
  s33_assert_outcome_free(base, "module C base frame")
  f85 <- base[focal == TRUE]
  # ---- GC-C06 the B6 CC1 one-file check; its no-692 occupancy feeds S2
  of <- inp$one_file; a2 <- facts$s2_animal
  mates <- f85[CageEpisodeID == f85[AnimalNum == a2]$CageEpisodeID[1] & AnimalNum != a2, AnimalNum]
  b6 <- design$episodes[CC == "CC1" & Batch == "B6"]
  if (!is.null(of$metrics) && is.na(of$error)) {
    j <- merge(b6[, .(AnimalNum, cr = crossing_rate, sz = shared_zone_use, ne = n_events, ntm = n_tracked_mates, hw = hardware_flag)],
               of$metrics, by = "AnimalNum", all = TRUE)
    nz <- of$no_s2_animal[AnimalNum %in% mates]
    ex2 <- inp$expected$s2
    s2_fix <- stats::setNames(nz$shared_zone_use, nz$AnimalNum)
    gate("GC-C06", sprintf("B6 CC1 one-file run reproduces A1 (%d/%d; 1e-9; n_events, n_tracked_mates, hardware_flag identical); without %s: occupancy of %s = expected (1e-9), 1 mate each",
                           facts$one_file_n, facts$one_file_n, a2, paste(facts$s2_mates, collapse = ", ")),
         c(nrow(j) == facts$one_file_n, nrow(b6) == facts$one_file_n, abs(j$cr - j$crossing_rate) <= 1e-9,
           abs(j$sz - j$shared_zone_use) <= 1e-9 | (is.na(j$sz) & is.na(j$shared_zone_use)), j$ne == j$n_events, j$ntm == j$n_tracked_mates,
           j$hw == j$hardware_flag, setequal(mates, facts$s2_mates), nrow(nz) == 2L,
           abs(nz$shared_zone_use - ex2$shared_zone_use[match(nz$AnimalNum, ex2$AnimalNum)]) <= 1e-9,
           abs(nz$shared_zone_use - facts$s2_no692) <= 1e-9, nz$n_tracked_mates == 1L),
         detail = sprintf("%d animals compared", nrow(j)))
  } else {
    s2_fix <- stats::setNames(rep(facts$s2_no692, length(mates)), mates)
    gate("GC-C06", "B6 CC1 one-file run (S2 occupancy without the 692 dyads)", FALSE, detail = paste("one-file run failed:", of$error))
  }
  # ---- board offsets, terms, cage means, information
  bo <- s33c_board_offsets(design$episodes)
  tv <- list(); cgl <- list(); inf <- list(); anl <- list()
  un <- s33c_unanalysed(design, facts)
  for (m in S33C_METRICS) {
    off24 <- bo$offsets[[m]][["CC2_4"]]
    for (v in S33C_VARIANTS) {
      t <- s33c_variant_terms(base, v, m, off24, s2_fix, facts)
      tv[[m]][[v]] <- t
      cg <- s33c_cage_table(t, m, v, off24)
      cgl[[length(cgl) + 1L]] <- cg
      if (v != "S4_board_adj") inf[[length(inf) + 1L]] <- s33c_information(t, m, v)
      adj <- if (v == "S4_board_adj") t else s33c_terms(s33c_variant_rows(base, v, facts), m, offsets = off24,
                                                        x_fix = if (v == "S2_drop692" && m == "shared_zone_use") s2_fix else NULL)
      anl[[length(anl) + 1L]] <- t[, .(variant = v, metric_id = m, AnimalNum, Sex, Batch, src_cage, CageEpisodeID, System, n_c, x, xbar_c,
                                       xbar_loo, xbar_k, w, b, board_offset_cc2_4 = unname(off24[System]),
                                       b_board_adj = adj$b[match(AnimalNum, adj$AnimalNum)], bw_cc1 = tp2, bw_cage_mean = bw_c, bw_w, bw_b,
                                       hardware_flag, cage_had_unanalysed_mate = CageEpisodeID %in% un$cc1_cage, in_model = TRUE)]
      if (v == "primary") {
        sg <- base[focal == FALSE]
        anl[[length(anl) + 1L]] <- sg[, .(variant = v, metric_id = m, AnimalNum, Sex, Batch, src_cage, CageEpisodeID, System,
                                          n_c = NA_integer_, x = get(m), xbar_c = NA_real_, xbar_loo = NA_real_, xbar_k = NA_real_, w = NA_real_,
                                          b = NA_real_, board_offset_cc2_4 = unname(off24[System]), b_board_adj = NA_real_, bw_cc1 = tp2,
                                          bw_cage_mean = NA_real_, bw_w = NA_real_, bw_b = NA_real_, hardware_flag,
                                          cage_had_unanalysed_mate = CageEpisodeID %in% un$cc1_cage, in_model = FALSE)]
      }
    }
  }
  # c02
  c02 <- data.table::rbindlist(anl)
  c02[, `:=`(metric_id = factor(metric_id, S33C_METRICS), variant = factor(variant, S33C_VARIANTS))]
  data.table::setorderv(c02, c("metric_id", "variant", "AnimalNum"))
  c02[, `:=`(metric_id = as.character(metric_id), variant = as.character(variant))]
  c02[, `:=`(metric_label = s33c_label(metric_id), tier = S33_TIER)]
  data.table::setcolorder(c02, c("variant", "metric_id", "metric_label"))
  # c03 (usable cages of every variant; the primary also lists the CC1 cages without a second analysed animal)
  cgs <- data.table::rbindlist(cgl)
  ntr <- unique(base[, .(CageEpisodeID, n_tr)])
  cgs[, `:=`(n_tracked = ntr$n_tr[match(CageEpisodeID, ntr$CageEpisodeID)], usable = TRUE)]
  sg_cages <- unique(base[focal == FALSE, .(CageEpisodeID, Sex, Batch, System, n_tracked = n_tr)])
  sg_rows <- data.table::rbindlist(lapply(S33C_METRICS, function(m) sg_cages[, .(CageEpisodeID, Sex, Batch, System, n_c = NA_integer_,
    xbar_c = NA_real_, xbar_k = NA_real_, b_c = NA_real_, sd_within_cage = NA_real_, min_x = NA_real_, max_x = NA_real_,
    bw_cage_mean = NA_real_, bw_b = NA_real_, occdisp_cage_mean = NA_real_, b_c_per_frozen_sd = NA_real_, info_share = NA_real_,
    board_offset_cc2_4 = unname(bo$offsets[[m]][["CC2_4"]][System]), b_c_board_adj = NA_real_, variant = "primary", metric_id = m,
    n_tracked, usable = FALSE)]))
  c03 <- rbind(cgs, sg_rows, use.names = TRUE)
  c03[, `:=`(unanalysed_mate_ids = un$ids[match(CageEpisodeID, un$cc1_cage)], n_unanalysed = un$n[match(CageEpisodeID, un$cc1_cage)])]
  c03[is.na(n_unanalysed), n_unanalysed := 0L]
  c03[, n_physical_A1 := n_tracked + n_unanalysed][, n_unanalysed := NULL]
  c03[, `:=`(metric_id = factor(metric_id, S33C_METRICS), variant = factor(variant, S33C_VARIANTS))]
  data.table::setorderv(c03, c("metric_id", "variant", "CageEpisodeID"))
  c03[, `:=`(metric_id = as.character(metric_id), variant = as.character(variant), tier = S33_TIER)]
  c03[, metric_label := s33c_label(metric_id)]
  data.table::setcolorder(c03, c("variant", "metric_id", "metric_label", "Batch", "Sex", "CageEpisodeID", "System", "n_tracked", "n_physical_A1",
                                 "unanalysed_mate_ids", "usable", "n_c", "xbar_c", "xbar_k", "b_c", "b_c_per_frozen_sd", "sd_within_cage",
                                 "min_x", "max_x", "info_share", "board_offset_cc2_4", "b_c_board_adj", "bw_cage_mean", "bw_b",
                                 "occdisp_cage_mean", "tier"))
  # c04 with the top-4 cage shares of each variant
  c04 <- data.table::rbindlist(inf)
  t4 <- c03[usable == TRUE, { z <- s33c_top4(.SD); .(top4_cages = z$cages, top4_cage_info_share = z$share) }, by = .(metric_id, variant)]
  c04 <- merge(c04, t4, by = c("metric_id", "variant"), sort = FALSE)
  c04[, `:=`(metric_id = factor(metric_id, S33C_METRICS), variant = factor(variant, S33C_VARIANTS))]
  data.table::setorderv(c04, c("metric_id", "variant", "Batch"))
  c04[, `:=`(metric_id = as.character(metric_id), variant = as.character(variant), tier = S33_TIER)]
  pi_k <- lapply(S33C_METRICS, function(m) { z <- c04[metric_id == m & variant == "primary"]; stats::setNames(z$pi_between_within_sex, z$Batch) })
  names(pi_k) <- S33C_METRICS
  # ---- c05 reallocation (crossing rate; primary, S1, S2, S8)
  B <- as.integer(ctx$B[["nonparametric"]])
  c05 <- data.table::rbindlist(lapply(names(S33C_REALLOC_VARIANTS), function(v) {
    t <- tv[["crossing_rate"]][[v]]; seed <- as.integer(ctx$seeds[[S33C_REALLOC_VARIANTS[[v]]]])
    obs <- stats::sd(unique(t[, .(CageEpisodeID, bc = b)])$bc)
    sims <- s33c_ckpt(ctx, paste0("reallocation_", v), function() s33c_reallocation_draws(t$x, t$Batch, t$CageEpisodeID, B, seed),
                      key_extra = list(frame = s33c_frame_sha(t[, .(AnimalNum, Batch, CageEpisodeID, x)]), seed = seed, B = B), log = log)
    q <- s33_percentile(sims)
    data.table::data.table(metric_id = "crossing_rate", metric_label = s33c_label("crossing_rate"), variant = v, n_animals = nrow(t),
      n_cages = data.table::uniqueN(t$CageEpisodeID),
      observed_sd_b_c = obs, B = B, seed = seed, rng_kind = "Mersenne-Twister/Inversion/Rejection", row_order = "AnimalNum (C locale)",
      ref_median = stats::median(sims), ref_q025 = q$lo, ref_q975 = q$hi, units = S33C_UNITS_X[["crossing_rate"]],
      note = S33C_NOTE[["realloc"]], tier = S33_TIER)
  }))
  # ---- c13 design sensitivity; ranks and design CR2 df (GC-C07)
  c13 <- s33c_design_sensitivity(tv[["crossing_rate"]])
  ranks <- s33c_declared_ranks(base, tv[["crossing_rate"]], facts)
  ddf <- lapply(S33C_METRICS, function(m) s33c_design_cr2_df(tv[[m]][["primary"]])); names(ddf) <- S33C_METRICS
  # ---- exposure ICCs (c10 Phase-2 rows; c06 Phase-2 rows)
  exp_fits <- list(); c10p <- list()
  for (m in S33C_METRICS) {
    t <- tv[[m]][["primary"]]
    id <- paste(m, "primary", "exposure_icc", sep = "|")
    fm <- s33_kr_fit(S33C_FORMULAS[["exposure"]], t[, .(AnimalNum, Batch, CageEpisodeID, x)], id, S33C_RANKS[["exposure"]])
    exp_fits[[m]] <- s33c_model_row(fm, m, "primary", "exposure_icc", "descriptive", S33C_RANKS[["exposure"]])
    vc <- s33c_varcomp(fm); icc <- vc[["var_cage"]] / (vc[["var_cage"]] + vc[["var_resid"]])
    mom <- s33_moment_icc(t$x - stats::ave(t$x, t$Batch), t$CageEpisodeID)
    c10p[[length(c10p) + 1L]] <- s33c_var_row("exposure_icc_cc1_cage", m, icc, "none", fm, "primary", "exposure_icc", m, vc,
                                              note = S33C_NOTE[["icc_exposure"]])
    c10p[[length(c10p) + 1L]] <- s33c_var_row("exposure_moment_icc_cc1_cage", m, mom, "none", fm, "primary", "exposure_icc", m, vc,
                                              note = S33C_NOTE[["moment"]], from_model = FALSE)
  }
  c06 <- rbind(data.table::rbindlist(exp_fits), bo$fits)
  c10 <- data.table::rbindlist(c10p)
  c12 <- bo$rows
  # ---- c01 population
  c01 <- s33c_population(base, tv[["crossing_rate"]], facts)
  # ---- gates
  cnt <- facts$counts
  pc <- function(t) c(nrow(t), data.table::uniqueN(t$CageEpisodeID))
  sx <- function(t, s) pc(t[Sex == s])
  p <- tv[["crossing_rate"]]
  sizes <- p$primary[, .N, by = CageEpisodeID]$N
  gate("GC-C03a", "focal_85: 85 animals in 22 CC1 cages (F 46/12, M 39/10); per cohort 8/16/16/15/15/15 in 2/4/4/4/4/4 cages; 19 x 4 + 3 x 3",
       c(pc(p$primary) == c(cnt$primary$n, cnt$primary$cages), sx(p$primary, "Female") == cnt$primary$female,
         sx(p$primary, "Male") == cnt$primary$male,
         identical(p$primary[, .N, keyby = Batch]$N, unname(cnt$primary$by_cohort)),
         identical(p$primary[, data.table::uniqueN(CageEpisodeID), keyby = Batch]$V1, unname(cnt$primary$cages_by_cohort)),
         sum(sizes == 4L) == cnt$primary$n4, sum(sizes == 3L) == cnt$primary$n3, all(sizes %in% 3:4)))
  s2s <- p$S2_drop692[, .N, by = CageEpisodeID]$N
  gate("GC-C03b", "variants: S1 79/20 (F 40/10, M 39/10); S2 84/22 with one cage of 2; S8 76/19 (F 40/10, M 36/9)",
       c(pc(p$S1_excl_cages) == c(cnt$S1_excl_cages$n, cnt$S1_excl_cages$cages), sx(p$S1_excl_cages, "Female") == cnt$S1_excl_cages$female,
         sx(p$S1_excl_cages, "Male") == cnt$S1_excl_cages$male, pc(p$S2_drop692) == c(cnt$S2_drop692$n, cnt$S2_drop692$cages),
         sum(s2s == 2L) == cnt$S2_drop692$n2, pc(p$S8_4tracked) == c(cnt$S8_4tracked$n, cnt$S8_4tracked$cages),
         sx(p$S8_4tracked, "Female") == cnt$S8_4tracked$female, sx(p$S8_4tracked, "Male") == cnt$S8_4tracked$male))
  lobo <- vapply(S33_COHORTS, function(k) pc(f85[Batch != k]), integer(2))
  loc <- vapply(sort(unique(f85$CageEpisodeID), method = "radix"), function(cg) pc(f85[CageEpisodeID != cg]), integer(2))
  gate("GC-C03c", "LOBO sets 77/20, 69/18, 69/18, 70/18, 70/18, 70/18; leave-one-cage-out sets of 81 or 82 animals in 21 cages",
       c(as.vector(lobo) == unlist(cnt$LOBO[S33_COHORTS]), loc[1, ] %in% cnt$leave_one_cage_out, loc[2, ] == cnt$primary$cages - 1L,
         ncol(loc) == cnt$primary$cages))
  par_ok <- vapply(S33C_METRICS, function(m) all(pc(base[is.finite(get(m))]) == cnt$parent[[m]]), TRUE)
  nofin <- unlist(lapply(S33C_METRICS, function(m) vapply(tv[[m]], function(t) all(is.finite(t$x)) && all(is.finite(t$tp2)), TRUE)))
  gate("GC-C03d", "parent populations 87/24 (rate) and 85/22 (occupancy); no missing x or CC1-day weight in any model frame",
       c(par_ok, nofin))
  bw <- inp$expected$bw
  jb <- merge(f85[, .(AnimalNum, Sex, Batch, CageEpisodeID, tp2)], bw, by = "AnimalNum", all = TRUE, suffixes = c("", ".exp"))
  xm <- sort(unique(base[xmate == TRUE, CageEpisodeID]), method = "radix")
  low <- base[is.finite(crossing_rate)][order(crossing_rate)]$AnimalNum[1]
  gate("GC-C04a", "joins: CC1-day weight (tp2) = bw_cc1_primary.csv for the 85 (exact; integer grams 6-16); occupancy dispersion joined 85/85; XMATE cages as declared; the S2 animal is the lowest A1 rate and the only CC1 hardware flag; focal animals are tracked SIS",
       c(nrow(jb) == nrow(f85), jb$tp2 == jb$bw_cc1, jb$Sex == jb$Sex.exp, jb$Batch == jb$Batch.exp, jb$CageEpisodeID == jb$CageEpisodeID.exp,
         f85$tp2 == round(f85$tp2), f85$tp2 >= facts$bw_range[1], f85$tp2 <= facts$bw_range[2], is.finite(f85$occupancy_dispersion),
         identical(xm, sort(names(facts$xmate_unanalysed), method = "radix")), low == facts$s2_animal,
         identical(base[hardware_flag %in% TRUE, AnimalNum], facts$s2_animal), all(f85$condition == "SIS"),
         identical(base$AnimalNum, sort(base$AnimalNum, method = "radix"))))
  wm <- merge(design$episodes[CC == "CC1", .(AnimalNum, CageEpisodeID, cr = crossing_rate, sz = shared_zone_use, ntm = n_tracked_mates)],
              inp$s32[, .(AnimalNum, CEID32 = CageEpisodeID, crossing_rate, shared_zone_use, n_tracked_mates, complete_primary)],
              by = "AnimalNum", all = TRUE)
  gate("GC-C12", "CC1 A1 metrics = Stage 32 window_metrics_long CC1 A1 (111; rate and occupancy 1e-9, missing pattern equal; CC1 cage and mates identical; complete_primary)",
       c(nrow(wm) == data.table::uniqueN(design$episodes$AnimalNum), abs(wm$cr - wm$crossing_rate) <= 1e-9,
         (is.na(wm$sz) & is.na(wm$shared_zone_use)) | abs(wm$sz - wm$shared_zone_use) <= 1e-9, wm$CageEpisodeID == wm$CEID32,
         wm$ntm == wm$n_tracked_mates, wm$complete_primary %in% TRUE))
  # GC-C05: expected values (docs/stage33/expected/)
  ex <- inp$expected
  vmap <- c(primary = "primary", S1_excl_cages = "S1_excl_cages", S2_drop692 = "S2_drop692", S8_4tracked = "S8_4tracked", S4_board_adj = "S4_board_adj")
  ct_cols <- c("Sex", "Batch", "System", "n_c", "xbar_c", "xbar_k", "b_c", "sd_within_cage", "min_x", "max_x", "bw_cage_mean", "bw_b",
               "occdisp_cage_mean", "board_offset_cc2_4", "b_c_per_frozen_sd", "info_share")
  exc <- data.table::copy(ex$cage_terms); data.table::setnames(exc, c("metric", "sd_within"), c("metric_id", "sd_within_cage"), skip_absent = TRUE)
  grp <- unique(exc[, .(variant, metric_id)])
  chk_ct <- data.table::rbindlist(lapply(seq_len(nrow(grp)), function(i) {
    v <- grp$variant[i]; m <- grp$metric_id[i]
    tol <- if (v == "S2_drop692" && m == "shared_zone_use") 1e-6 else 1e-9
    s33c_compare("GC-C05a", paste("cage terms", v, m), c03[usable == TRUE & variant == vmap[[v]] & metric_id == m], exc[variant == v & metric_id == m],
                 "CageEpisodeID", ct_cols, tol, source = "expected_cage_terms.csv")
  }))
  gate("GC-C05a", "cage terms of primary, S1, S2, S8 and S4 for both metrics = expected_cage_terms.csv (1e-9; S2 occupancy 1e-6)",
       chk_ct$ok, n_expected = nrow(grp) * length(ct_cols), detail = sprintf("max |diff| %.3g", max(chk_ct$max_abs_diff, na.rm = TRUE)))
  sums <- unlist(lapply(S33C_METRICS, function(m) lapply(tv[[m]], s33c_term_sums)))
  gate("GC-C05b", "sum n_c b_c = 0 per cohort and sum w = 0 per cage in every variant (1e-9)", abs(sums) <= 1e-9)
  exi <- data.table::copy(ex$information); data.table::setnames(exi, c("metric", "n", "SSw", "SSb", "pi_matched_within_sex"),
                                                              c("metric_id", "n_animals", "SS_within", "SS_between", "pi_between_within_sex"), skip_absent = TRUE)
  ic <- c("Sex", "n_animals", "n_cages", "SS_within", "SS_between", "share_within_all", "share_between_all", "pi_between_within_sex",
          "between_share_of_within_cohort_ss_sex")
  chk_inf <- s33c_compare("GC-C05c", "information shares (primary)", c04[variant == "primary"], exi[variant == "primary"],
                          c("metric_id", "Batch"), ic, 1e-9, source = "expected_information_primary.csv")
  pi_ok <- unlist(lapply(S33C_METRICS, function(m) abs(pi_k[[m]][names(facts$pi[[m]])] - facts$pi[[m]]) <= facts$pi_tolerance))
  chk_pi <- data.table::data.table(check_id = "GC-C05c", item = "pi_k = the plan's constants", column = "pi_between_within_sex",
    n_expected = 12L, n_compared = length(pi_ok), n_ok = sum(pi_ok %in% TRUE),
    max_abs_diff = max(unlist(lapply(S33C_METRICS, function(m) abs(pi_k[[m]][names(facts$pi[[m]])] - facts$pi[[m]])))),
    tolerance = facts$pi_tolerance, ok = length(pi_ok) == 12L && all(pi_ok %in% TRUE), gated = TRUE, source = "plan section 10")
  gate("GC-C05c", "information shares and pi_k = expected_information_primary.csv (1e-9); pi_k = the plan's 5-decimal constants",
       c(chk_inf$ok, chk_pi$ok), n_expected = length(ic) + 1L)
  exo <- data.table::copy(ex$offsets); data.table::setnames(exo, c("metric", "rows"), c("metric_id", "rows_used"), skip_absent = TRUE)
  obs_o <- c12[, .(metric_id, rows_used, System, offset = estimate, se, n_rows, rank = n_params, singular)]
  chk_off <- s33c_compare("GC-C05d", "board offsets (CC2_4 used, all reference)", obs_o, exo, c("metric_id", "rows_used", "System"),
                          c("n_rows", "rank", "singular", "offset", "se"), 1e-9, source = "expected_board_offsets.csv")
  gate("GC-C05d", "board offsets and SEs (both metrics, CC2-CC4 and all rows; n, rank, singular) = expected_board_offsets.csv (1e-9)",
       chk_off$ok, n_expected = 5L)
  exd <- data.table::copy(ex$design_sensitivity)
  obs_d <- c13[, .(level = design_row, n = n_units, df = residual_df, mde_noncentral_t = round(min_detectable_r_noncentral_t, 3),
                   mde_fisher = round(min_detectable_r_fisher, 3))]
  chk_ds <- s33c_compare("GC-C05e", "design sensitivity (3 decimals)", obs_d, exd, "level", c("n", "df", "mde_noncentral_t", "mde_fisher"), 1e-9,
                         source = "expected_design_sensitivity.csv")
  gate("GC-C05e", "design sensitivity = expected_design_sensitivity.csv (3 decimals)", chk_ds$ok, n_expected = 4L)
  exr <- ex$reallocation
  rp <- c05[variant == "primary"]
  same_stream <- nrow(exr) == 1L && isTRUE(exr$B == rp$B) && isTRUE(exr$seed == rp$seed)
  chk_re <- rbind(s33c_compare("GC-C05f", "reallocation observed SD", rp[, .(metric_id, observed_sd_b_c)], exr[, .(metric_id = metric, observed_sd_b_c)],
                               "metric_id", "observed_sd_b_c", 1e-9, source = "expected_reallocation.csv"),
                  s33c_compare("GC-C05f", "reallocation reference (same B and seed)", rp[, .(metric_id, B, seed, ref_median, ref_q025, ref_q975)],
                               exr[, .(metric_id = metric, B, seed, ref_median, ref_q025, ref_q975)], "metric_id",
                               c("B", "seed", "ref_median", "ref_q025", "ref_q975"), 1e-9, gated = same_stream, source = "expected_reallocation.csv"))
  dev_reduced <- identical(ctx$run_mode, "development") && !same_stream
  if (dev_reduced) {
    gate("GC-C05f", "reallocation: observed SD = expected_reallocation.csv (1e-9); reference not evaluated (development B differs from the file's)",
         chk_re[item == "reallocation observed SD"]$ok, detail = sprintf("development B %d", B))
  } else gate("GC-C05f", "reallocation (primary): observed SD, median, 2.5% and 97.5% = expected_reallocation.csv (1e-9; B 4000, seed 33030101)",
              chk_re$ok, n_expected = 6L)
  exp_icc <- vapply(S33C_METRICS, function(m) c10[metric_id == m & estimand == "exposure_icc_cc1_cage"]$estimate, 0)
  chk_icc <- data.table::data.table(check_id = "GC-C05g", item = "exposure ICCs (recorded)", column = S33C_METRICS, n_expected = 1L,
    n_compared = 1L, n_ok = as.integer(abs(exp_icc - facts$exposure_icc) <= 5e-4), max_abs_diff = abs(exp_icc - facts$exposure_icc),
    tolerance = 5e-4, ok = abs(exp_icc - facts$exposure_icc) <= 5e-4, gated = FALSE, source = "plan section 10")
  gate("GC-C05g", "exposure ICCs as in the plan (0.034, 0.207; 3 decimals; recorded)", chk_icc$ok, hard = FALSE)
  # GC-C07a: declared ranks and design df
  ddf_ok <- unlist(lapply(S33C_METRICS, function(m) abs(ddf[[m]][S33C_PRIMARY_ESTIMANDS] - facts$design_cr2_df[[m]][S33C_PRIMARY_ESTIMANDS]) <= facts$design_df_tolerance))
  gate("GC-C07a", "declared ranks of every module C design (M1 10, identity 10, S3 10, C_pooled 8, S9 12, S10 14, S11 14, S1/S2/S8/S4 10, strata 5, LOBO 9, leave-one-cage-out 10, empty 6, exposure 6, offsets 23/29); cage aggregate rank 8 df 14, cage dummy rank 24 df 61; design CR2 df (OLS working model) as planned (0.05)",
       c(ranks$n_cols == ranks$expected_rank, ranks$rank == ranks$expected_rank,
         ranks[model == "cage_aggregate"]$residual_df == facts$aggregate_df, ranks[model == "cage_dummy"]$residual_df == facts$dummy_df,
         c06$n_fixed_cols == c06$expected_rank, ddf_ok),
       detail = paste(sprintf("%s: %s", S33C_METRICS, vapply(ddf, function(z) paste(sprintf("%.2f", z), collapse = "/"), "")), collapse = "; "))
  # ---- audit table and products
  cev <- data.table::rbindlist(list(chk_ct, s33c_compare("GC-C04a", "CC1-day weight = bw_cc1_primary.csv", f85[, .(AnimalNum, bw_cc1 = tp2)],
                                                         bw[, .(AnimalNum, bw_cc1)], "AnimalNum", "bw_cc1", 1e-9, source = "bw_cc1_primary.csv"),
                                    chk_inf, chk_pi, chk_off, chk_ds, chk_re, chk_icc,
                                    if (!is.null(of$no_s2_animal)) s33c_compare("GC-C06", "occupancy without the S2 animal",
                                      of$no_s2_animal[AnimalNum %in% mates, .(AnimalNum, shared_zone_use, n_tracked_mates, dyadic_obs_s)],
                                      inp$expected$s2[, .(AnimalNum, shared_zone_use, n_tracked_mates, dyadic_obs_s)], "AnimalNum",
                                      c("shared_zone_use", "n_tracked_mates", "dyadic_obs_s"), 1e-6, source = "expected_s2_shared_zone_no692.csv")),
                               fill = TRUE)
  cev[, tier := S33_TIER]
  products <- list(c01_population = c01, c02_animal_terms = c02, c03_cage_means = c03, c04_information = c04, c05_reallocation = c05,
                   c06_models = c06, c10_variance = c10, c12_board_offsets = c12, c13_design_sensitivity = c13, c_expected_value_checks = cev)
  for (nm in names(products)) s33_assert_outcome_free(products[[nm]], paste("module C product", nm))
  state <- list(base = base, terms = tv, offsets = bo$offsets, s2_fix = s2_fix, pi = pi_k, unanalysed = un, c13 = c13, ranks = ranks,
                design_cr2_df = ddf, top4 = t4, female_shares = c04[variant == "primary", .(fw = sum(SS_within[Sex == "Female"]) / sum(SS_within),
                                                                                         fb = sum(SS_between[Sex == "Female"]) / sum(SS_between)), by = metric_id],
                facts = facts, checkpoints = s33c_ckpt_rows(log, ctx),
                # tp2 and src_cage for the same 87 tracked SIS animals (X4 merges the two by AnimalNum)
                cross = list(focal = f85$AnimalNum, cages = sort(unique(f85$CageEpisodeID), method = "radix"), tp2 = base[, .(AnimalNum, tp2)],
                             src_cage = base[, .(AnimalNum, src_cage)], s1 = S33_SD_RATE,
                             xbar_loo = tv[["crossing_rate"]][["primary"]][, .(AnimalNum, xbar_loo)]))
  list(products = products, gates = data.table::rbindlist(G), state = state)
}

#' c01 population table: variants by cohort with a total row.
s33c_population <- function(base, tvr, facts = S33C_FACTS) {
  f85 <- base[focal == TRUE]
  one <- function(v, d, ref) {
    cs <- d[, .N, by = .(CageEpisodeID, Batch, Sex)]
    by_k <- function(dd, cc, refk) data.table::data.table(n_animals = nrow(dd), n_cages = nrow(cc), n_cages_n4 = sum(cc$N == 4L),
      n_cages_n3 = sum(cc$N == 3L), n_cages_n2 = sum(cc$N == 2L), n_cages_n1 = sum(cc$N == 1L),
      dropped_animals = paste(sort(setdiff(refk$AnimalNum, dd$AnimalNum), method = "radix"), collapse = ";"),
      dropped_cages = paste(sort(setdiff(unique(refk$CageEpisodeID), unique(dd$CageEpisodeID)), method = "radix"), collapse = ";"))
    ks <- sort(unique(d$Batch), method = "radix")
    rbind(data.table::rbindlist(lapply(ks, function(k) cbind(data.table::data.table(variant = v, Sex = S33_SEX_OF_COHORT[[k]], Batch = k),
                                                             by_k(d[Batch == k], cs[Batch == k], ref[Batch == k])))),
          cbind(data.table::data.table(variant = v, Sex = "all", Batch = "all"), by_k(d, cs, ref)))
  }
  out <- list(one("primary", tvr$primary, f85), one("S1_excl_cages", tvr$S1_excl_cages, f85), one("S2_drop692", tvr$S2_drop692, f85),
              one("S8_4tracked", tvr$S8_4tracked, f85))
  for (k in S33_COHORTS) out[[length(out) + 1L]] <- one(paste0("LOBO_-", k), f85[Batch != k], f85)
  for (s in c("Female", "Male")) out[[length(out) + 1L]] <- one(paste0("sex_", s), f85[Sex == s], f85[Sex == s])
  out[[length(out) + 1L]] <- one("parent_crossing_87", base[is.finite(crossing_rate)], base)
  out[[length(out) + 1L]] <- one("parent_shared_85", base[is.finite(shared_zone_use)], base)
  x <- data.table::rbindlist(out)
  x[, tier := S33_TIER][]
}

#' One c10 row (variance summaries); interval columns filled by the caller where they exist. A moment ICC does not use the
#' fit (m only supplies the counts): its status follows the value (from_model = FALSE).
s33c_var_row <- function(estimand, metric, estimate, method, m, variant, model, response, vc, ci = c(NA_real_, NA_real_), pb = NULL,
                         prof = NULL, note = NA_character_, from_model = TRUE, boot = c(NA_real_, NA_real_)) {
  st <- if (from_model) { if (is.null(m) || !s32i_failed(m)) "OK" else "FAILED" } else if (is.finite(estimate)) "OK" else "FAILED"
  if (st == "FAILED") { estimate <- NA_real_; ci <- c(NA_real_, NA_real_); boot <- c(NA_real_, NA_real_) }
  s33_row(estimand = estimand, level = S33C_LEVEL[[estimand]], estimate = estimate, ci_low = ci[1], ci_high = ci[2],
          interval_method = method, interval_basis = s33c_basis(method), units = if (grepl("variance", estimand)) "variance (squared units of the response)" else "share of variance (signed for moment ICCs)",
          metric_id = if (is.na(metric)) NA_character_ else metric, status = st,
          singular = if (is.null(m)) NA else m$info$singular, ddf_fallback = NA, lead = FALSE,
          n_animals = if (is.null(m)) NA_integer_ else m$info$n_animals, n_cages = if (is.null(m)) NA_integer_ else m$info$n_cage_episodes,
          n_cohorts = if (is.null(m)) NA_integer_ else m$info$n_batches, n_params = if (is.null(m)) NA_integer_ else m$info$n_fixed_cols,
          model_id = if (is.null(m)) NA_character_ else m$info$model_id, variant = variant, model = model, response = response,
          var_cage = vc[["var_cage"]], var_resid = vc[["var_resid"]], pb_B = pb$B %s33or% NA_integer_, pb_seed = pb$seed %s33or% NA_integer_,
          pb_failed = pb$n_fail %s33or% NA_integer_, pb_boundary_share = pb$boundary %s33or% NA_real_,
          bootstrap_low = boot[1], bootstrap_high = boot[2],
          profile_sd_low = prof$sd_low %s33or% NA_real_, profile_sd_high = prof$sd_high %s33or% NA_real_,
          icc_profile_upper = prof$icc_upper %s33or% NA_real_, n_profile_warnings = prof$n_warnings %s33or% NA_integer_, note = note)
}

# ---------------------------------------------------------------- Phase 3 estimators
#' bootMer statistic: M1 (b, w, gamma) and the cage variance share; empty model: the cage variance share only.
s33c_boot_stat <- function(f) {
  fx <- lme4::fixef(f); v <- as.data.frame(lme4::VarCorr(f))
  vc <- sum(v$vcov[v$grp == "CageEpisodeID" & is.na(v$var2)]); vr <- v$vcov[v$grp == "Residual"][1]
  out <- c(vc_cage = vc, vc_res = vr, icc = vc / (vc + vr))
  if (all(c("b", "w") %in% names(fx))) out <- c(b = fx[["b"]], w = fx[["w"]], ctx = fx[["b"]] - fx[["w"]], out)
  out
}

#' Parametric bootstrap of a frozen-engine fit (one set.seed per stream; Mersenne-Twister / Inversion / Rejection). A FAILED
#' fit or a bootMer error gives t = NULL (its rows become FAILED rows; the run continues).
s33c_bootmer <- function(m, B, seed) {
  if (s32i_failed(m)) return(list(t = NULL, t0 = NULL, n_fail = NA_integer_, n_msgs = NA_integer_, B = B, seed = seed, boundary = NA_real_))
  old <- RNGkind(); on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  n_msg <- 0L
  bm <- tryCatch(withCallingHandlers(lme4::bootMer(m$fit, FUN = s33c_boot_stat, nsim = B, seed = seed, type = "parametric", use.u = FALSE),
    message = function(x) { n_msg <<- n_msg + 1L; invokeRestart("muffleMessage") },
    warning = function(x) { n_msg <<- n_msg + 1L; invokeRestart("muffleWarning") }), error = function(e) e)
  if (inherits(bm, "error")) return(list(t = NULL, t0 = NULL, n_fail = as.integer(B), n_msgs = n_msg, B = B, seed = seed, boundary = NA_real_,
                                         error = conditionMessage(bm)))
  tt <- bm$t; colnames(tt) <- names(bm$t0)
  list(t = tt, t0 = bm$t0, n_fail = sum(!stats::complete.cases(tt)), n_msgs = n_msg, B = B, seed = seed,
       boundary = mean(tt[, "vc_cage"] < 1e-10, na.rm = TRUE))
}

#' Profile-likelihood limits of the CC1-cage SD (REML); icc_upper at the fitted residual variance.
s33c_profile <- function(m) {
  na <- list(sd_low = NA_real_, sd_high = NA_real_, icc_upper = NA_real_, n_warnings = NA_integer_)
  if (s32i_failed(m)) return(na)
  nw <- 0L
  ci <- tryCatch(withCallingHandlers(suppressMessages(stats::confint(m$fit, parm = "theta_", method = "profile", quiet = TRUE, signames = FALSE)),
                 warning = function(w) { nw <<- nw + 1L; invokeRestart("muffleWarning") }), error = function(e) e)
  if (inherits(ci, "error")) return(na)
  r <- grep("CageEpisodeID", rownames(ci))[1]
  vr <- s33c_varcomp(m)[["var_resid"]]
  list(sd_low = unname(ci[r, 1]), sd_high = unname(ci[r, 2]), icc_upper = unname(ci[r, 2]^2 / (ci[r, 2]^2 + vr)), n_warnings = nw)
}

#' Fit with the frozen engine on the declared columns only.
s33c_fit <- function(model, t, model_id, rank, formula = S33C_FORMULAS[[model]]) {
  cols <- unique(c("AnimalNum", "Batch", "CageEpisodeID", all.vars(stats::as.formula(formula))))
  s33_kr_fit(formula, t[, cols, with = FALSE], model_id, rank)
}

#' c07 rows from one result table r (estimate, se, df, ci_low, ci_high, interval_method, status, singular, ddf_fallback),
#' raw and (unless raw_only) per frozen SD.
s33c_est_rows <- function(r, metric, variant, model, role, estimand, L, m = NULL, note = NA_character_, mech = NA_character_,
                          raw_only = FALSE, counts = NULL) {
  n <- if (!is.null(m)) list(a = m$info$n_animals, c = m$info$n_cage_episodes, k = m$info$n_batches, p = m$info$n_fixed_cols,
                             id = m$info$model_id) else counts
  sc <- if (raw_only) "raw" else c("raw", "per_frozen_sd")
  data.table::rbindlist(lapply(sc, function(s) {
    f <- if (s == "raw") 1 else S33C_SD[[metric]]
    s33_row(estimand = estimand, level = S33C_LEVEL[[estimand]], estimate = r$estimate * f, se = r$se * f, df = r$df,
            ci_low = r$ci_low * f, ci_high = r$ci_high * f, interval_method = r$interval_method, interval_basis = s33c_basis(r$interval_method),
            units = if (s == "raw") S33C_UNITS_RAW[[metric]] else S33C_UNITS_SD[[metric]], metric_id = metric, status = r$status,
            singular = r$singular, ddf_fallback = r$ddf_fallback, lead = FALSE, n_animals = n$a, n_cages = n$c, n_cohorts = n$k, n_params = n$p,
            variant = variant, model = model, role = role, scale = s, L = L,
            estimand_label = if (estimand %in% names(S33C_ESTIMAND_LABEL)) S33C_ESTIMAND_LABEL[[estimand]] else NA_character_,
            quote_role = "table_only", compatible_sentence_interval = FALSE,
            shift_in_primary_se = NA_real_, robustness_label = NA_character_, sign_same_as_primary = NA, top4_cage_info_share = NA_real_,
            c13_halfwidth_r = NA_real_, leave_one_cage_out_min_estimate = NA_real_, leave_one_cage_out_max_estimate = NA_real_,
            cr2_design_df_ols = NA_real_,
            mech_coupling = mech, model_id = n$id, note = note)
  }))
}

s33c_Ltext <- function(L) { w <- unlist(L); paste(sprintf("%s=%g", names(w), w), collapse = "; ") }

#' KR rows of a frozen-engine fit for named L-vectors.
s33c_kr_rows <- function(m, Ls, metric, variant, model, role, note = NA_character_, mech = NA_character_, raw_only = FALSE) {
  data.table::rbindlist(lapply(names(Ls), function(e) {
    k <- s33_kr_contrast(m, Ls[[e]], e)
    s33c_est_rows(k, metric, variant, model, role, e, s33c_Ltext(Ls[[e]]), m = m, note = note, mech = mech, raw_only = raw_only)
  }))
}

#' CR2 rows (by CC1 cage) of a frozen-engine fit.
s33c_cr2_rows <- function(m, Ls, metric, variant, model, role, note = S33C_NOTE[["cr2"]], mech = NA_character_) {
  data.table::rbindlist(lapply(names(Ls), function(e) {
    r <- s33_kr_fit_cr2(m, Ls[[e]], "CageEpisodeID")
    r[, `:=`(interval_method = "CR2_Satterthwaite_cc1_cage", singular = m$info$singular, ddf_fallback = NA)]
    s33c_est_rows(r, metric, variant, model, role, e, s33c_Ltext(Ls[[e]]), m = m, note = note, mech = mech)
  }))
}

#' Point value (no interval) of a linear combination of the fixed effects of a frozen-engine fit (NA for a FAILED fit).
s33c_point_value <- function(m, L) if (s32i_failed(m)) NA_real_ else sum(mmm_ci_L(names(lme4::fixef(m$fit)), L) * lme4::fixef(m$fit))

#' The M1 sex-interaction coefficients as point values for its c06 row (plan section 10: point values only; female-cohort
#' set minus male-cohort set, sex and cohort not separable; raw units of the metric). Never an estimate row.
s33c_sex_interaction_points <- function(m) {
  data.table::data.table(sex_interaction_w = s33c_point_value(m, list(`w:sex_c` = 1)),
                         sex_interaction_b = s33c_point_value(m, list(`b:sex_c` = 1)),
                         sex_interaction_gamma = s33c_point_value(m, list(`b:sex_c` = 1, `w:sex_c` = -1)),
                         sex_interaction_note = S33C_NOTE[["sex_interaction"]])
}

#' A one-row result from an estimate, SE and df (t interval).
s33c_t_result <- function(est, se, df, method) {
  ci <- s33_t_interval(est, se, df)
  data.table::data.table(estimate = est, se = se, df = df, ci_low = ci$ci_low, ci_high = ci$ci_high, interval_method = method,
                         status = if (is.finite(est)) "OK" else "FAILED", singular = NA, ddf_fallback = NA)
}

#' Parent reproduction and bridge (plan section 10, GC-C13): per population and sex.
s33c_bridge <- function(base, cz, frozen = NULL) {
  pops <- list(crossing_rate = c("parent_87", "primary_85"), shared_zone_use = "parent_85")
  rows <- list(); fits <- list(); rec <- list(); repro <- list()
  for (m in S33C_METRICS) for (pop in pops[[m]]) {
    d <- if (pop == "primary_85") base[focal == TRUE] else base[is.finite(get(m))]
    d <- data.table::copy(d); d[, CombZ := cz$CombZ[match(AnimalNum, cz$AnimalNum)]]
    t <- s33c_terms(d, m)
    fp <- stats::lm(stats::as.formula(S33C_FORMULAS[["parent_pooled"]]), data = t)
    fits[[length(fits) + 1L]] <- s33c_lm_row(fp, t, m, pop, "parent_pooled", "bridge", 8L)
    bp <- stats::coef(fp); Vp <- stats::vcov(fp); dfp <- fp$df.residual
    cr <- s33_cr2_contrast(fp, t$CageEpisodeID, list(c(x = 1), c(`x:sex_c` = 1)))
    sexes <- list()
    for (s in c("Female", "Male")) {
      ts <- t[Sex == s]
      fs <- stats::lm(stats::as.formula(S33C_FORMULAS[["parent_by_sex"]]), data = ts)
      fw <- stats::lm(stats::as.formula(S33C_FORMULAS[["within_between_by_sex"]]), data = ts)
      fits[[length(fits) + 1L]] <- s33c_lm_row(fs, ts, m, pop, paste0("parent_", s), "bridge", 4L)
      fits[[length(fits) + 1L]] <- s33c_lm_row(fw, ts, m, pop, paste0("within_between_", s), "bridge", 5L)
      SSw <- sum(ts$w^2); SSb <- sum(ts$b^2)
      bW <- stats::coef(fw)[["w"]]; bB <- stats::coef(fw)[["b"]]
      recomb <- (SSw * bW + SSb * bB) / (SSw + SSb)
      sexes[[s]] <- list(est = stats::coef(fs)[["x"]], se = sqrt(stats::vcov(fs)["x", "x"]), df = fs$df.residual, n = nrow(ts),
                         nc = data.table::uniqueN(ts$CageEpisodeID), SSw = SSw, SSb = SSb, bW = bW, bB = bB, rec = recomb)
      rec[[length(rec) + 1L]] <- data.table::data.table(metric_id = m, population = pop, sex = s, diff = abs(recomb - sexes[[s]]$est))
    }
    sav <- list(est = bp[["x"]], se = sqrt(Vp["x", "x"]), df = dfp, n = nrow(t), nc = data.table::uniqueN(t$CageEpisodeID),
                bW = mean(c(sexes$Female$bW, sexes$Male$bW)), bB = mean(c(sexes$Female$bB, sexes$Male$bB)),
                rec = mean(c(sexes$Female$rec, sexes$Male$rec)))
    rec[[length(rec) + 1L]] <- data.table::data.table(metric_id = m, population = pop, sex = "sexavg", diff = abs(sav$rec - sav$est))
    is_frozen <- pop != "primary_85"
    fz <- if (is_frozen && !is.null(frozen)) frozen[predictor == m] else NULL
    get_fz <- function(e) if (is.null(fz)) NULL else fz[estimand == e]
    if (is_frozen) {
      # the 4 rows of this predictor: estimate, se, df, ci (all) plus CR2 se and df (sexavg, DiD) and n; one row per quantity
      did <- list(est = bp[["x:sex_c"]], se = sqrt(Vp["x:sex_c", "x:sex_c"]), df = dfp, n = nrow(t))
      for (e in c("slope_sexavg", "slope_DiD_F_minus_M", "slope_Female", "slope_Male")) {
        o <- switch(e, slope_sexavg = sav, slope_DiD_F_minus_M = did, slope_Female = sexes$Female, slope_Male = sexes$Male)
        ci <- s33_t_interval(o$est, o$se, o$df)
        z <- get_fz(e)
        crr <- if (e == "slope_sexavg") cr[1] else if (e == "slope_DiD_F_minus_M") cr[2] else NULL
        vals <- c(estimate = o$est, se = o$se, df = o$df, ci_low = ci$ci_low, ci_high = ci$ci_high, n = o$n,
                  se_cr2 = if (is.null(crr)) NA_real_ else crr$se, df_cr2 = if (is.null(crr)) NA_real_ else crr$df)
        refv <- if (is.null(z) || nrow(z) != 1L) rep(NA_real_, 8L) else c(z$estimate, z$se, z$df, z$ci_low, z$ci_high, z$n, z$se_cr2, z$df_cr2)
        use <- if (is.null(crr)) 1:6 else 1:8
        repro[[length(repro) + 1L]] <- data.table::data.table(metric_id = m, estimand = e, quantity = names(vals)[use],
                                                              diff = abs(vals[use] - refv[use]))
      }
    }
    for (s in c("Female", "Male", "sexavg")) {
      o <- if (s == "sexavg") sav else sexes[[s]]
      z <- get_fz(if (s == "sexavg") "slope_sexavg" else paste0("slope_", s))
      if (is_frozen) { ci <- s33_t_interval(o$est, o$se, o$df); r <- s33c_t_result(o$est, o$se, o$df, paste0("t_", o$df)) } else
        r <- data.table::data.table(estimate = o$est, se = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_, interval_method = "none",
                                    status = "OK", singular = NA, ddf_fallback = NA)
      crr <- if (is_frozen && s == "sexavg") cr[1] else NULL
      rows[[length(rows) + 1L]] <- s33_row(
        estimand = "parent_within_cohort_slope", level = "animal_within_cohort", estimate = r$estimate, se = r$se, df = r$df,
        ci_low = r$ci_low, ci_high = r$ci_high, interval_method = r$interval_method, interval_basis = s33c_basis(r$interval_method),
        units = S33C_UNITS_RAW[[m]], metric_id = m, status = r$status, singular = NA, ddf_fallback = NA, lead = FALSE, n_animals = o$n,
        n_cages = o$nc, n_cohorts = if (s == "sexavg") 6L else 3L, n_params = if (s == "sexavg") 8L else 4L, population = pop, sex = s,
        SS_within = if (s == "sexavg") NA_real_ else o$SSw, SS_between = if (s == "sexavg") NA_real_ else o$SSb,
        between_share = if (s == "sexavg") NA_real_ else o$SSb / (o$SSw + o$SSb), beta_within_ols = o$bW, beta_between_ols = o$bB,
        recombined_slope = o$rec, abs_diff_recombined = abs(o$rec - o$est),
        # copies of the frozen Stage 29 row (= ci_low / ci_high of this row within 1e-9, GC-C13a; named so that the README
        # interval count does not count the parent interval twice)
        parent_frozen_estimate = if (is.null(z) || !nrow(z)) NA_real_ else z$estimate,
        parent_frozen_lower = if (is.null(z) || !nrow(z)) NA_real_ else z$ci_low,
        parent_frozen_upper = if (is.null(z) || !nrow(z)) NA_real_ else z$ci_high,
        cr2_se = if (is.null(crr)) NA_real_ else crr$se, cr2_df = if (is.null(crr)) NA_real_ else crr$df,
        cr2_ci_low = if (is.null(crr)) NA_real_ else crr$ci_low, cr2_ci_high = if (is.null(crr)) NA_real_ else crr$ci_high,
        note = paste(if (is_frozen) S33C_NOTE[["parent"]] else S33C_NOTE[["parent_85"]], S33C_NOTE[["bridge"]], sep = "; "))
    }
  }
  list(rows = data.table::rbindlist(rows), fits = data.table::rbindlist(fits), recombination = data.table::rbindlist(rec),
       reproduction = if (length(repro)) data.table::rbindlist(repro) else
         data.table::data.table(metric_id = character(), estimand = character(), quantity = character(), diff = numeric()))
}

#' Influence summary (c09) from the LOBO and leave-one-cage-out rows of c07.
s33c_influence <- function(est) {
  pr <- est[variant == "primary" & model == "M1" & scale == "raw" & interval_method %in% c("KR", "Satterthwaite_fallback")]
  inf <- est[role == "influence" & scale == "raw"]
  inf[, unit_type := data.table::fifelse(grepl("^LOBO", variant), "cohort (LOBO)", "cage (leave-one-cage-out)")]
  inf[, unit := sub("^(LOBO_-|leave_one_cage_out_-)", "", variant)]
  inf[, `:=`(prim = pr$estimate[match(paste(metric_id, estimand), paste(pr$metric_id, pr$estimand))],
             prim_se = pr$se[match(paste(metric_id, estimand), paste(pr$metric_id, pr$estimand))])]
  inf[, shift := (estimate - prim) / prim_se]
  inf[, lab := mmm_ci_robust_label(prim, prim_se, estimate)]
  x <- inf[, {
    ok <- is.finite(estimate); i <- if (any(ok & is.finite(shift))) which.max(abs(shift) * (ok & is.finite(shift))) else NA_integer_
    lobo <- unit_type[1] == "cohort (LOBO)"
    .(level = level[1], n_refits = .N, n_failed = sum(!ok), min_estimate = if (any(ok)) min(estimate[ok]) else NA_real_,
      max_estimate = if (any(ok)) max(estimate[ok]) else NA_real_, n_sign_same = sum(ok & sign(estimate) == sign(prim)),
      sign_stability = sprintf("same sign in %d of %d %s", sum(ok & sign(estimate) == sign(prim)), .N,
                               if (lobo) "LOBO refits" else "leave-one-cage-out refits"),
      max_abs_shift_in_primary_se = if (is.na(i)) NA_real_ else abs(shift[i]), unit_at_max = if (is.na(i)) NA_character_ else unit[i],
      n_SIGN_CHANGE = if (lobo) NA_integer_ else sum(lab == "SIGN_CHANGE", na.rm = TRUE),
      n_LARGE_SHIFT = if (lobo) NA_integer_ else sum(lab == "LARGE_SHIFT", na.rm = TRUE)) }, by = .(metric_id, estimand, unit_type)]
  x[, `:=`(metric_id = factor(metric_id, S33C_METRICS), estimand = factor(estimand, S33C_PRIMARY_ESTIMANDS))]
  data.table::setorderv(x, c("metric_id", "estimand", "unit_type"), order = c(1L, 1L, -1L))
  x[, `:=`(metric_id = as.character(metric_id), estimand = as.character(estimand))]
  # sign stability beside the refits (plan 17.2 C: 'same sign in S9, S11 and k of 6 LOBO refits'): S9 and S11 KR rows
  # against the primary KR row (S11: beta_W -> within_cc1_cage_matched, gamma -> gamma_matched, beta_B -> b)
  s11_of <- c(within_cc1_cage = "within_cc1_cage_matched", between_cc1_cages_within_cohort = "between_cc1_cages_within_cohort",
              contextual_peer_composition = "gamma_matched")
  kr_raw <- est[scale == "raw" & interval_method %in% c("KR", "Satterthwaite_fallback") & variant == "primary"]
  sgn <- function(mt, mdl, e) { v <- kr_raw$estimate[kr_raw$metric_id == mt & kr_raw$model == mdl & kr_raw$estimand == e]
    if (length(v) == 1L && is.finite(v)) sign(v) else NA_real_ }
  x[, `:=`(sign_same_S9 = mapply(function(mt, e, p) { s <- sgn(mt, "S9_cc1_weight", e); if (is.na(s) || !is.finite(p)) NA else s == sign(p) },
                                 metric_id, estimand, pr$estimate[match(paste(metric_id, estimand), paste(pr$metric_id, pr$estimand))]),
           sign_same_S11 = mapply(function(mt, e, p) { s <- sgn(mt, "S11_matched", s11_of[[e]]); if (is.na(s) || !is.finite(p)) NA else s == sign(p) },
                                  metric_id, estimand, pr$estimate[match(paste(metric_id, estimand), paste(pr$metric_id, pr$estimand))]))]
  # section-7 columns (no interval: ranges of refit estimates)
  x[, `:=`(metric_label = s33c_label(metric_id), units = unname(S33C_UNITS_RAW[metric_id]), lead = FALSE, interval_basis = "none",
           note = S33C_NOTE[["leave_one_cage_out_range"]], tier = S33_TIER)]
  x[unit_type == "cohort (LOBO)", note := "LOBO (leave one cohort out) refits: range of the estimates and sign stability; not an interval"]
  data.table::setcolorder(x, c("metric_id", "metric_label", "estimand", "level", "unit_type"))
  x[]
}

#' Partial correlations (c11): n_c-weighted cage level and within cages, Fisher z intervals.
s33c_corr_rows <- function(t, metric, variant, sex_label) {
  t <- data.table::copy(t)
  cg <- t[, .(n_c = .N, b_c = b[1], ybar = mean(CombZ), Batch = Batch[1]), by = CageEpisodeID]
  cg[, ybar_k := sum(n_c * ybar) / sum(n_c), by = Batch][, yb := ybar - ybar_k]
  r_c <- sum(cg$n_c * cg$b_c * cg$yb) / sqrt(sum(cg$n_c * cg$b_c^2) * sum(cg$n_c * cg$yb^2))
  t[, yw := CombZ - mean(CombZ), by = CageEpisodeID]
  r_w <- sum(t$w * t$yw) / sqrt(sum(t$w^2) * sum(t$yw^2))
  G <- nrow(cg); N <- nrow(t); qk <- data.table::uniqueN(cg$Batch) - 1L
  fz <- function(r, n, q) { k <- n - q - 3; if (!is.finite(r) || k <= 0 || abs(r) >= 1) return(c(NA_real_, NA_real_, NA_real_))
    se <- 1 / sqrt(k); c(se, tanh(atanh(r) + c(-1, 1) * stats::qnorm(0.975) * se)) }
  one <- function(estimand, r, n, q, w, note) { z <- fz(r, n, q)
    s33_row(estimand = estimand, level = S33C_LEVEL[[estimand]], estimate = r, ci_low = z[2], ci_high = z[3], interval_method = "Fisher_z",
            interval_basis = "conditional_on_cohorts", units = "correlation (r)", metric_id = metric, status = if (is.finite(r)) "OK" else "FAILED",
            lead = FALSE, n_animals = N, n_cages = G, n_cohorts = qk + 1L, variant = variant, sex = sex_label, n_units = as.integer(n), q = as.integer(q),
            fisher_se_z = z[1], weighting = w, note = note) }
  rbind(one("partial_r_cage", r_c, G, qk, "n_c", S33C_NOTE[["corr_cage"]]), one("partial_r_within_cage", r_w, N, G - 1L, "none", S33C_NOTE[["corr_within"]]))
}

#' The decomposition of one metric (M1 and its variants, checks, bootstraps, influence refits, ICC and correlation rows).
s33c_decompose_metric <- function(st, cz, m, ctx, seed, B, log) {
  add_cz <- function(t) { t <- data.table::copy(t); t[, CombZ := cz$CombZ[match(AnimalNum, cz$AnimalNum)]]; t }
  tv <- lapply(st$terms[[m]], add_cz)
  tp <- tv$primary
  est <- list(); mods <- list(); vrows <- list()
  addm <- function(mm, variant, model, role, rank) mods[[length(mods) + 1L]] <<- s33c_model_row(mm, m, variant, model, role, rank)
  idf <- function(variant, model) paste(m, variant, model, sep = "|")
  # M1 (primary)
  M1 <- s33c_fit("M1", tp, idf("primary", "M1"), S33C_RANKS[["M1"]]); addm(M1, "primary", "M1", "primary", S33C_RANKS[["M1"]])
  kr <- s33c_kr_rows(M1, S33C_L, m, "primary", "M1", "primary")
  est[[length(est) + 1L]] <- kr
  est[[length(est) + 1L]] <- s33c_cr2_rows(M1, S33C_L, m, "primary", "M1", "primary")
  # sex-interaction coefficients of M1: point values only, on the M1 row of c06 (c07 has no female-minus-male row)
  mods[[length(mods)]] <- cbind(mods[[length(mods)]], s33c_sex_interaction_points(M1))
  # parametric bootstrap of M1 (checkpointed per stream; never in the placebo)
  pb <- s33c_ckpt(ctx, paste0("pb_M1_", m), function() s33c_bootmer(M1, B, seed[[m]]),
                  key_extra = list(frame = s33c_frame_sha(M1$data), formula = M1$info$formula, seed = seed[[m]], B = B), log = log)
  sing <- isTRUE(M1$info$singular)
  pbm <- if (sing) "parametric_bootstrap_spread" else "parametric_bootstrap_percentile"
  pbn <- if (sing) paste(S33C_NOTE[["pb"]], S33C_NOTE[["spread"]], sep = "; ") else S33C_NOTE[["pb"]]
  for (e in S33C_PRIMARY_ESTIMANDS) {
    col <- c(within_cc1_cage = "w", between_cc1_cages_within_cohort = "b", contextual_peer_composition = "ctx")[[e]]
    q <- if (is.null(pb$t)) list(lo = NA_real_, hi = NA_real_) else s33_percentile(pb$t[, col])
    r <- data.table::data.table(estimate = if (is.null(pb$t)) NA_real_ else kr[scale == "raw" & estimand == e]$estimate, se = NA_real_,
                                df = NA_real_, ci_low = q$lo, ci_high = q$hi,
                                interval_method = pbm, status = if (is.null(pb$t)) "FAILED" else "OK", singular = M1$info$singular, ddf_fallback = NA)
    est[[length(est) + 1L]] <- s33c_est_rows(r, m, "primary", "M1", "check", e, s33c_Ltext(S33C_L[[e]]), m = M1,
                                             note = sprintf("%s; B %d, seed %d, failed refits %s", pbn, B, seed[[m]], pb$n_fail))
  }
  # checks: cage aggregate t(df), cage dummy t(df), two-part Welch
  cg <- tp[, .(ybar = mean(CombZ), xbar_c = xbar_c[1], n_c = .N, Batch = Batch[1], sex_c = sex_c[1]), by = CageEpisodeID]
  fa <- stats::lm(stats::as.formula(S33C_FORMULAS[["cage_aggregate"]]), data = cg, weights = n_c)
  fd <- stats::lm(stats::as.formula(S33C_FORMULAS[["cage_dummy"]]), data = tp)
  mods[[length(mods) + 1L]] <- s33c_lm_row(fa, cg, m, "primary", "cage_aggregate", "check", S33C_RANKS[["cage_aggregate"]])
  mods[[length(mods) + 1L]] <- s33c_lm_row(fd, tp, m, "primary", "cage_dummy", "check", st$facts$dummy_rank %s33or% S33C_RANKS[["cage_dummy"]])
  sB <- sqrt(stats::vcov(fa)["xbar_c", "xbar_c"]); bBa <- stats::coef(fa)[["xbar_c"]]; dfa <- fa$df.residual
  sW <- sqrt(stats::vcov(fd)["w", "w"]); bWd <- stats::coef(fd)[["w"]]; dfd <- fd$df.residual
  cnt <- list(a = nrow(tp), c = nrow(cg), k = data.table::uniqueN(tp$Batch), p = fa$rank, id = idf("primary", "cage_aggregate"))
  est[[length(est) + 1L]] <- s33c_est_rows(s33c_t_result(bBa, sB, dfa, paste0("t_", dfa)), m, "primary", "M1", "check",
                                           "between_cc1_cages_within_cohort", "xbar_c=1 (cage aggregate)", note = S33C_NOTE[["aggregate"]], counts = cnt)
  cnt$p <- fd$rank; cnt$id <- idf("primary", "cage_dummy")
  est[[length(est) + 1L]] <- s33c_est_rows(s33c_t_result(bWd, sW, dfd, paste0("t_", dfd)), m, "primary", "M1", "check",
                                           "within_cc1_cage", "w=1 (cage dummy)", note = S33C_NOTE[["dummy"]], counts = cnt)
  wdf <- (sB^2 + sW^2)^2 / (sB^4 / dfa + sW^4 / dfd)
  cnt$p <- NA_integer_; cnt$id <- idf("primary", "two_part_welch")
  est[[length(est) + 1L]] <- s33c_est_rows(s33c_t_result(bBa - bWd, sqrt(sB^2 + sW^2), wdf, "Welch"), m, "primary", "M1", "check",
                                           "contextual_peer_composition", "aggregate xbar_c=1 minus dummy w=1", note = S33C_NOTE[["welch"]], counts = cnt)
  # identity form, S3, C_pooled, S9, S10, S11, S4, S1, S2, S8
  MI <- s33c_fit("identity", tp, idf("primary", "identity"), S33C_RANKS[["identity"]]); addm(MI, "primary", "identity", "identity", S33C_RANKS[["identity"]])
  ki <- s33c_kr_rows(MI, list(contextual_peer_composition = list(xbar_c = 1), within_cc1_cage = list(x = 1)), m, "primary", "identity", "identity",
                     note = S33C_NOTE[["identity"]])
  est[[length(est) + 1L]] <- ki
  M3 <- s33c_fit("S3_peer_loo", tp, idf("primary", "S3_peer_loo"), S33C_RANKS[["S3_peer_loo"]]); addm(M3, "primary", "S3_peer_loo", "sensitivity", S33C_RANKS[["S3_peer_loo"]])
  est[[length(est) + 1L]] <- s33c_kr_rows(M3, list(delta_peer = list(xbar_loo = 1)), m, "primary", "S3_peer_loo", "sensitivity", note = S33C_NOTE[["s3"]])
  MP <- s33c_fit("C_pooled", tp, idf("primary", "C_pooled"), S33C_RANKS[["C_pooled"]]); addm(MP, "primary", "C_pooled", "comparator", S33C_RANKS[["C_pooled"]])
  fs <- st$female_shares[metric_id == m]
  est[[length(est) + 1L]] <- s33c_kr_rows(MP, S33C_L, m, "primary", "C_pooled", "comparator",
                                          note = sprintf("%s; female share of the within-cage / between-cage variation of x %.3f / %.3f", S33C_NOTE[["pooled"]], fs$fw, fs$fb))
  M9 <- s33c_fit("S9_cc1_weight", tp, idf("primary", "S9_cc1_weight"), S33C_RANKS[["S9_cc1_weight"]]); addm(M9, "primary", "S9_cc1_weight", "sensitivity", S33C_RANKS[["S9_cc1_weight"]])
  est[[length(est) + 1L]] <- s33c_kr_rows(M9, S33C_L, m, "primary", "S9_cc1_weight", "sensitivity", mech = S33C_NOTE[["mech_coupling"]])
  est[[length(est) + 1L]] <- s33c_cr2_rows(M9, S33C_L, m, "primary", "S9_cc1_weight", "sensitivity", mech = S33C_NOTE[["mech_coupling"]])
  M10 <- s33c_fit("S10_board_terms", tp, idf("primary", "S10_board_terms"), S33C_RANKS[["S10_board_terms"]]); addm(M10, "primary", "S10_board_terms", "sensitivity", S33C_RANKS[["S10_board_terms"]])
  k10 <- s33c_kr_rows(M10, S33C_L, m, "primary", "S10_board_terms", "sensitivity", note = S33C_NOTE[["s10"]])
  est[[length(est) + 1L]] <- k10
  M11 <- s33c_fit("S11_matched", tp, idf("primary", "S11_matched"), S33C_RANKS[["S11_matched"]]); addm(M11, "primary", "S11_matched", "sensitivity", S33C_RANKS[["S11_matched"]])
  pik <- st$pi[[m]][S33_COHORTS]
  L11 <- list(gamma_matched = c(list(b = 1), stats::setNames(as.list(-0.5 * pik), paste0("w_", S33_COHORTS))),
              within_cc1_cage_matched = stats::setNames(as.list(0.5 * pik), paste0("w_", S33_COHORTS)),
              between_cc1_cages_within_cohort = list(b = 1))
  k11 <- s33c_kr_rows(M11, L11, m, "primary", "S11_matched", "sensitivity", note = S33C_NOTE[["s11"]])
  est[[length(est) + 1L]] <- k11
  vfit <- list()
  for (v in c("S4_board_adj", "S1_excl_cages", "S2_drop692", "S8_4tracked")) {
    mv <- s33c_fit("M1", tv[[v]], idf(v, "M1"), S33C_RANKS[["M1"]]); addm(mv, v, "M1", "sensitivity", S33C_RANKS[["M1"]]); vfit[[v]] <- mv
    est[[length(est) + 1L]] <- s33c_kr_rows(mv, S33C_L, m, v, "M1", "sensitivity",
                                            note = S33C_NOTE[[c(S4_board_adj = "s4", S1_excl_cages = "s1", S2_drop692 = "s2", S8_4tracked = "s8")[[v]]]])
  }
  # S7 strata and their mean (no female-minus-male row)
  stl <- list()
  for (s in c("Female", "Male")) {
    ts <- add_cz(s33c_terms(st$base[focal == TRUE & Sex == s], m))
    ms <- s33c_fit("S7_stratum", ts, idf(paste0("S7_", s), "S7_stratum"), S33C_RANKS[["S7_stratum"]])
    addm(ms, paste0("S7_", s), "S7_stratum", "secondary", S33C_RANKS[["S7_stratum"]])
    stl[[s]] <- s33c_kr_rows(ms, S33C_L, m, paste0("S7_", s), "S7_stratum", "secondary", note = S33C_NOTE[["strata"]])
    est[[length(est) + 1L]] <- stl[[s]]
  }
  for (e in S33C_PRIMARY_ESTIMANDS) {
    f <- stl$Female[scale == "raw" & estimand == e]; g <- stl$Male[scale == "raw" & estimand == e]
    se <- sqrt(f$se^2 + g$se^2) / 2; df <- (f$se^2 + g$se^2)^2 / (f$se^4 / f$df + g$se^4 / g$df)
    r <- s33c_t_result((f$estimate + g$estimate) / 2, se, df, "Welch")
    est[[length(est) + 1L]] <- s33c_est_rows(r, m, "S7_strata_mean", "S7_strata_mean", "secondary", e, "(F + M) / 2 of the strata",
                                             note = S33C_NOTE[["strata_mean"]], counts = list(a = nrow(tp), c = nrow(cg), k = 6L, p = NA_integer_,
                                                                                              id = idf("S7_strata_mean", "S7_strata_mean")))
  }
  # LOBO and leave-one-cage-out (terms recomputed on each set)
  f85 <- st$base[focal == TRUE]
  for (k in S33_COHORTS) {
    tl <- add_cz(s33c_terms(f85[Batch != k], m))
    ml <- s33c_fit("M1", tl, idf(paste0("LOBO_-", k), "M1"), S33C_RANKS[["LOBO"]]); addm(ml, paste0("LOBO_-", k), "M1", "influence", S33C_RANKS[["LOBO"]])
    est[[length(est) + 1L]] <- s33c_kr_rows(ml, S33C_L, m, paste0("LOBO_-", k), "M1", "influence", note = S33C_NOTE[["lobo"]], raw_only = TRUE)
  }
  for (cgid in sort(unique(f85$CageEpisodeID), method = "radix")) {
    tl <- add_cz(s33c_terms(f85[CageEpisodeID != cgid], m))
    v <- paste0("leave_one_cage_out_-", cgid)
    ml <- s33c_fit("M1", tl, idf(v, "M1"), S33C_RANKS[["M1"]]); addm(ml, v, "M1", "influence", S33C_RANKS[["M1"]])
    est[[length(est) + 1L]] <- s33c_kr_rows(ml, S33C_L, m, v, "M1", "influence", note = S33C_NOTE[["locage"]], raw_only = TRUE)
  }
  # M1 residual ICC (c10): bootstrap of the fitted share, profile limits of the cage SD, moment ICC of OLS residuals
  vc <- s33c_varcomp(M1); icc <- vc[["var_cage"]] / (vc[["var_cage"]] + vc[["var_resid"]])
  prof <- s33c_profile(M1)
  q <- if (is.null(pb$t)) list(lo = NA_real_, hi = NA_real_) else s33_percentile(pb$t[, "icc"])
  qv <- if (is.null(pb$t)) list(lo = NA_real_, hi = NA_real_) else s33_percentile(pb$t[, "vc_cage"])
  pbi <- list(B = B, seed = seed[[m]], n_fail = pb$n_fail, boundary = pb$boundary)
  vrows[[1]] <- s33c_var_row("icc_cc1_cage", m, icc, pbm, M1, "primary", "M1", "CombZ", vc, ci = c(q$lo, q$hi), pb = pbi, prof = prof,
                             note = paste(c(S33C_NOTE[["icc_resid"]], if (sing) c(S33C_NOTE[["spread"]], S33C_NOTE[["icc_upper"]])), collapse = "; "))
  vrows[[2]] <- s33c_var_row("cage_variance_cc1", m, vc[["var_cage"]], "profile_likelihood", M1, "primary", "M1", "CombZ", vc,
                             ci = c(prof$sd_low^2, prof$sd_high^2), pb = pbi, prof = prof, boot = c(qv$lo, qv$hi),
                             note = paste(S33C_NOTE[["profile"]], if (sing) paste("bootstrap_low / bootstrap_high:", S33C_NOTE[["spread"]])
                                          else "bootstrap_low / bootstrap_high: parametric bootstrap percentiles of the cage variance", sep = "; "))
  fo <- stats::lm(CombZ ~ Batch + w + b + w:sex_c + b:sex_c, data = tp)
  vrows[[3]] <- s33c_var_row("moment_icc_cc1_cage", m, s33_moment_icc(stats::residuals(fo), tp$CageEpisodeID), "none", M1, "primary", "M1", "CombZ", vc,
                             note = paste(S33C_NOTE[["moment"]], "(residuals of the M1 fixed part, OLS)"), from_model = FALSE)
  # correlations (primary pooled, F, M; S1, S2, S8 pooled)
  corr <- list(s33c_corr_rows(tp, m, "primary", "pooled"), s33c_corr_rows(tp[Sex == "Female"], m, "primary", "Female"),
               s33c_corr_rows(tp[Sex == "Male"], m, "primary", "Male"))
  for (v in c("S1_excl_cages", "S2_drop692", "S8_4tracked")) corr[[length(corr) + 1L]] <- s33c_corr_rows(tv[[v]], m, v, "pooled")
  # identities (GC-C08)
  # PLAN ISSUE: GC-C08 asks the identity form to equal M1 within 1e-8 in estimate, CI and df. Two separately optimised
  # non-singular REML fits of the same model agree only to the optimiser tolerance (on permuted data |theta difference|
  # up to 3e-8 moved the KR df by 4e-7), so the gated identities are evaluated exactly: the identity form at M1's variance
  # parameters (s33c_fit_at_theta), the cage-dummy within estimator, S10 and S4 beta_W (variance-free within estimators)
  # and the per-SD rows (GC-C08a, absolute 1e-8). The separately optimised identity form (its c07 rows) is compared with
  # M1 in the recorded GC-C08c (relative 1e-6).
  MIt <- if (s32i_failed(M1)) NULL else s33c_fit_at_theta(S33C_FORMULAS[["identity"]],
    tp[, unique(c("AnimalNum", "Batch", "CageEpisodeID", all.vars(stats::as.formula(S33C_FORMULAS[["identity"]])))), with = FALSE],
    lme4::getME(M1$fit, "theta"), idf("primary", "identity_at_M1_theta"))
  kt <- if (is.null(MIt)) NULL else data.table::rbindlist(list(s33_kr_contrast(MIt, list(xbar_c = 1), "contextual_peer_composition"),
                                                                s33_kr_contrast(MIt, list(x = 1), "within_cc1_cage")), fill = TRUE)
  pick <- function(tb, e) {
    if (is.null(tb)) return(data.table::data.table(estimate = NA_real_, ci_low = NA_real_, ci_high = NA_real_, df = NA_real_, ddf_fallback = NA))
    r <- tb[tb$estimand == e]
    if ("scale" %in% names(r)) r <- r[r$scale == "raw"]
    r
  }
  g1 <- pick(kr, "contextual_peer_composition"); g2 <- pick(ki, "contextual_peer_composition"); gt <- pick(kt, "contextual_peer_composition")
  w1 <- pick(kr, "within_cc1_cage"); w2 <- pick(ki, "within_cc1_cage"); wt <- pick(kt, "within_cc1_cage")
  w10 <- pick(k10, "within_cc1_cage")
  e_all <- data.table::rbindlist(est)
  w4 <- e_all[variant == "S4_board_adj" & model == "M1" & scale == "raw" & estimand == "within_cc1_cage"]
  b1 <- pick(kr, "between_cc1_cages_within_cohort"); b11 <- pick(k11, "between_cc1_cages_within_cohort")
  raw <- e_all[scale == "raw" & role != "influence"]
  psd <- e_all[scale == "per_frozen_sd"]
  keyc <- function(x) paste(x$variant, x$model, x$estimand, x$interval_method, x$L)
  jj <- match(keyc(psd), keyc(raw))
  ok_fit <- function(...) all(vapply(list(...), function(z) !is.null(z) && !s32i_failed(z), TRUE))
  same_ddf <- function(a, b) identical(as.logical(a$ddf_fallback), as.logical(b$ddf_fallback))
  ident <- rbind(
    s33c_identity_rows(m, "gamma_identity_form_at_M1_theta", c("estimate", "ci_low", "ci_high", "df"),
                       c(g1$estimate, g1$ci_low, g1$ci_high, g1$df), c(gt$estimate, gt$ci_low, gt$ci_high, gt$df), ok_fit(M1, MIt) && same_ddf(g1, gt)),
    s33c_identity_rows(m, "within_identity_form_at_M1_theta", "estimate", w1$estimate, wt$estimate, ok_fit(M1, MIt)),
    s33c_identity_rows(m, "within_cage_dummy", "estimate", w1$estimate, bWd, ok_fit(M1)),
    s33c_identity_rows(m, "within_S10", "estimate", w10$estimate, w1$estimate, ok_fit(M1, M10)),
    s33c_identity_rows(m, "within_S4", "estimate", w4$estimate, w1$estimate, ok_fit(M1, vfit$S4_board_adj)),
    s33c_identity_rows(m, "per_frozen_sd_rows", rep(c("estimate", "ci_low", "ci_high"), each = nrow(psd)),
                       c(psd$estimate, psd$ci_low, psd$ci_high), c(raw$estimate[jj], raw$ci_low[jj], raw$ci_high[jj]) * S33C_SD[[m]], TRUE),
    s33c_identity_rows(m, "per_frozen_sd_rows_matched", "structure", as.numeric(nrow(psd) > 0L && !anyNA(jj) && nrow(psd) == nrow(raw)), 1, TRUE),
    s33c_identity_rows(m, "S11_between_vs_M1", "estimate", b11$estimate, b1$estimate, ok_fit(M1, M11), kind = "s11"),
    s33c_identity_rows(m, "identity_form_separately_optimised", c("gamma_estimate", "gamma_ci_low", "gamma_ci_high", "gamma_df", "within_estimate"),
                       c(g1$estimate, g1$ci_low, g1$ci_high, g1$df, w1$estimate), c(g2$estimate, g2$ci_low, g2$ci_high, g2$df, w2$estimate),
                       ok_fit(M1, MI) && same_ddf(g1, g2), kind = "separate"))
  # quoting rule and labels
  est_dt <- s33c_quote(e_all, m, sing, st)
  list(est = est_dt, models = data.table::rbindlist(mods, fill = TRUE), variance = data.table::rbindlist(vrows), corr = data.table::rbindlist(corr),
       ident = ident, s11_both_singular = isTRUE(M1$info$singular) && isTRUE(M11$info$singular), singular = sing, m1_failed = s32i_failed(M1),
       pb = pb, boot_dims = if (is.null(pb$t)) c(NA, NA) else dim(pb$t))
}

#' Identity comparison rows (GC-C08): one row per compared component. kind 'exact' (GC-C08a: absolute difference),
#' 's11' (GC-C08b: absolute), 'separate' (GC-C08c: relative to max(1, |value|)). A component is evaluable when its fits
#' did not FAIL and at least one side is finite; one finite side against a missing one is a difference of Inf.
s33c_identity_rows <- function(metric, identity, component, a, b, fits_ok, kind = "exact") {
  a <- as.numeric(a); b <- as.numeric(b)
  if (!length(a)) a <- rep(NA_real_, length(component)); if (!length(b)) b <- rep(NA_real_, length(component))
  fa <- is.finite(a); fb <- is.finite(b)
  d <- rep(Inf, length(a)); both <- fa & fb
  d[both] <- if (kind == "separate") s33c_reldiff(a[both], b[both]) else abs(a[both] - b[both])
  data.table::data.table(metric_id = metric, identity = identity, component = component, kind = kind,
                         evaluable = isTRUE(fits_ok) & (fa | fb), diff = d)
}

#' Quoting rule (plan section 10, fixed before the run) and robustness labels on one metric's c07 rows.
s33c_quote <- function(e, m, sing, st) {
  e <- data.table::copy(e)
  prim <- e[variant == "primary" & model == "M1" & role == "primary" & interval_method %in% c("KR", "Satterthwaite_fallback")]
  pk <- function(est, sc) prim[estimand == est & scale == sc]
  # robustness labels against the primary KR row of the same estimand and scale
  map <- c(gamma_matched = "contextual_peer_composition", within_cc1_cage_matched = "within_cc1_cage")
  lab_rows <- which((e$variant %in% c("S1_excl_cages", "S2_drop692", "S8_4tracked", "S4_board_adj") & e$model == "M1") |
                      e$model %in% c("S9_cc1_weight", "S10_board_terms", "S11_matched", "S7_strata_mean") |
                      grepl("^leave_one_cage_out", e$variant))
  lab_rows <- lab_rows[e$interval_method[lab_rows] %in% c("KR", "Satterthwaite_fallback", "Welch")]
  for (i in lab_rows) {
    tgt <- if (e$estimand[i] %in% names(map)) map[[e$estimand[i]]] else e$estimand[i]
    p <- pk(tgt, e$scale[i])
    if (nrow(p) == 1L && is.finite(p$se) && is.finite(e$estimate[i])) data.table::set(e, i, c("shift_in_primary_se", "robustness_label", "sign_same_as_primary"),
      list((e$estimate[i] - p$estimate) / p$se, mmm_ci_robust_label(p$estimate, p$se, e$estimate[i]), sign(e$estimate[i]) == sign(p$estimate)))
  }
  lb <- which(grepl("^LOBO", e$variant))
  for (i in lb) { p <- pk(e$estimand[i], e$scale[i])
    if (nrow(p) == 1L && is.finite(e$estimate[i])) data.table::set(e, i, "sign_same_as_primary", sign(e$estimate[i]) == sign(p$estimate)) }
  # quote roles
  core <- e$variant == "primary" & e$model == "M1" & e$estimand %in% S33C_PRIMARY_ESTIMANDS
  e[core & interval_method == "CR2_Satterthwaite_cc1_cage", quote_role := if (sing) "quoted_beside" else "primary_interval"]
  e[core & role == "primary" & interval_method %in% c("KR", "Satterthwaite_fallback"),
    quote_role := if (sing) data.table::fifelse(estimand == "within_cc1_cage", "primary_interval", "check") else "primary_interval"]
  e[core & role == "check", quote_role := "check"]
  if (sing) e[core & role == "check" & ((estimand == "between_cc1_cages_within_cohort" & grepl("^t_", interval_method)) |
                                          (estimand == "contextual_peer_composition" & interval_method == "Welch")), quote_role := "primary_interval"]
  e[model == "S9_cc1_weight" & variant == "primary", quote_role := "quoted_beside"]
  e[model == "S11_matched" & estimand == "gamma_matched", quote_role := "quoted_beside"]
  e[quote_role == "primary_interval", lead := scale == "raw"]
  # the interval for 'compatible with' sentences: the rule's row, or the wider of KR and CR2 (equal standing)
  for (sc in c("raw", "per_frozen_sd")) for (est in S33C_PRIMARY_ESTIMANDS) {
    ii <- which(core & e$scale == sc & e$estimand == est & e$quote_role == "primary_interval")
    if (length(ii)) { wd <- e$ci_high[ii] - e$ci_low[ii]; j <- if (all(is.na(wd))) ii[1] else ii[which.max(wd)]
      data.table::set(e, j, "compatible_sentence_interval", TRUE) }
  }
  c13 <- st$c13
  hw_cage <- c13[design_row == sprintf("cage primary G%d", data.table::uniqueN(st$base[focal == TRUE]$CageEpisodeID))]$expected_halfwidth_r_fisher
  hw_within <- c13[grepl("^within primary", design_row)]$expected_halfwidth_r_fisher
  t4 <- st$top4[metric_id == m & variant == "primary"]$top4_cage_info_share
  e[quote_role == "primary_interval", `:=`(top4_cage_info_share = t4,
                                           c13_halfwidth_r = data.table::fifelse(estimand == "within_cc1_cage", hw_within, hw_cage))]
  # the CR2 interval is quoted with its design df (OLS working model; plan: rate 6.2 / 3.7 / 5.4, occupancy 12.6 / 6.0 / 9.3)
  ddf <- st$design_cr2_df[[m]]
  ic2 <- which(core & e$interval_method == "CR2_Satterthwaite_cc1_cage")
  if (length(ic2) && length(ddf)) data.table::set(e, ic2, "cr2_design_df_ols", unname(ddf[e$estimand[ic2]]))
  # always quoted with beta_B and gamma: the leave-one-cage-out range of the refit estimates (raw; per-SD rows rescaled)
  lr <- e[grepl("^leave_one_cage_out_-", variant) & model == "M1" & scale == "raw" & status == "OK" & is.finite(estimate),
          .(lo = min(estimate), hi = max(estimate)), by = estimand]
  for (est in intersect(c("between_cc1_cages_within_cohort", "contextual_peer_composition"), lr$estimand)) {
    ii <- which(e$quote_role == "primary_interval" & e$estimand == est)
    f <- ifelse(e$scale[ii] == "raw", 1, S33C_SD[[m]])
    data.table::set(e, ii, c("leave_one_cage_out_min_estimate", "leave_one_cage_out_max_estimate"),
                    list(lr[estimand == est]$lo * f, lr[estimand == est]$hi * f))
  }
  e[]
}

#' Within-cohort permutation of CombZ for the placebo (one set.seed; AnimalNum order).
s33c_permute <- function(cz, seed) {
  old <- RNGkind(); on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(seed)
  p <- data.table::copy(cz)
  p[, CombZ := sample(CombZ), by = Batch]
  p[]
}

#' Expected row counts of the Phase-3 tables (plan section 10 recipe). Per metric, raw rows (each also per frozen SD):
#' M1 12 (KR 3, CR2 3, PB 3, aggregate / dummy / Welch checks 3), identity 2, S3 1, C_pooled 3, S9 6 (KR, CR2), S10 3,
#' S4 3, S11 3, S1 / S2 / S8 9, S7 strata 6 and their mean 3 = 51; LOBO and leave-one-cage-out refits 3 each, raw only.
#' c06: per metric 13 fits (M1, identity, S3, C_pooled, S9, S10, S11, S4, S1, S2, S8, cage aggregate, cage dummy), K LOBO,
#' G leave-one-cage-out, 2 strata; bridge 15 lm fits; empty models 4.
s33c_expected_counts <- function(st) {
  G <- data.table::uniqueN(st$base[focal == TRUE]$CageEpisodeID); K <- length(S33_COHORTS)
  base_rows <- 12L + 2L + 1L + 3L + 6L + 3L + 3L + 3L + 9L + 9L     # M1, identity, S3, C_pooled, S9, S10, S4, S11, S1/S2/S8, S7
  c(c06 = 2L * (13L + K + G + 2L) + 15L + 4L, c07 = 2L * (2L * base_rows + 3L * (K + G)), c08 = 9L, c09 = 12L, c10 = 6L + 9L, c11 = 24L)
}

#' Empty-model CombZ ICC rows (primary with bootstrap and profile; S1, S2, S8 model-based) and their c06 rows.
s33c_empty_models <- function(st, cz, ctx, seed, B, log) {
  tv <- st$terms[["crossing_rate"]]
  rows <- list(); mods <- list(); pbo <- NULL; failed_primary <- NA
  for (v in c("primary", "S1_excl_cages", "S2_drop692", "S8_4tracked")) {
    t <- data.table::copy(tv[[v]])[, CombZ := cz$CombZ[match(AnimalNum, cz$AnimalNum)]]
    me <- s33c_fit("empty", t, paste("CombZ", v, "empty", sep = "|"), S33C_RANKS[["empty"]])
    mods[[length(mods) + 1L]] <- s33c_model_row(me, NA_character_, v, "empty", "descriptive", S33C_RANKS[["empty"]])
    vc <- s33c_varcomp(me); icc <- vc[["var_cage"]] / (vc[["var_cage"]] + vc[["var_resid"]])
    mom <- s33_moment_icc(t$CombZ - stats::ave(t$CombZ, t$Batch), t$CageEpisodeID)
    if (v == "primary") {
      pbo <- s33c_ckpt(ctx, "pb_empty", function() s33c_bootmer(me, B, seed[["empty"]]),
                       key_extra = list(frame = s33c_frame_sha(me$data), formula = me$info$formula, seed = seed[["empty"]], B = B), log = log)
      sing <- isTRUE(me$info$singular); prof <- s33c_profile(me); failed_primary <- s32i_failed(me)
      q <- if (is.null(pbo$t)) list(lo = NA_real_, hi = NA_real_) else s33_percentile(pbo$t[, "icc"])
      qv <- if (is.null(pbo$t)) list(lo = NA_real_, hi = NA_real_) else s33_percentile(pbo$t[, "vc_cage"])
      pbi <- list(B = B, seed = seed[["empty"]], n_fail = pbo$n_fail, boundary = pbo$boundary)
      rows[[length(rows) + 1L]] <- s33c_var_row("icc_cc1_cage", NA, icc, if (sing) "parametric_bootstrap_spread" else "parametric_bootstrap_percentile",
                                                me, v, "empty", "CombZ", vc, ci = c(q$lo, q$hi), pb = pbi, prof = prof,
                                                note = paste(c(S33C_NOTE[["icc_empty"]], if (sing) c(S33C_NOTE[["spread"]], S33C_NOTE[["icc_upper"]])), collapse = "; "))
      rows[[length(rows) + 1L]] <- s33c_var_row("cage_variance_cc1", NA, vc[["var_cage"]], "profile_likelihood", me, v, "empty", "CombZ", vc,
                                                ci = c(prof$sd_low^2, prof$sd_high^2), pb = pbi, prof = prof, boot = c(qv$lo, qv$hi),
                                                note = paste(S33C_NOTE[["profile"]], if (sing) paste("bootstrap_low / bootstrap_high:", S33C_NOTE[["spread"]])
                                                             else "bootstrap_low / bootstrap_high: parametric bootstrap percentiles of the cage variance", sep = "; "))
    } else rows[[length(rows) + 1L]] <- s33c_var_row("icc_cc1_cage", NA, icc, "none", me, v, "empty", "CombZ", vc, note = paste(S33C_NOTE[["icc_empty"]], "model-based only", sep = "; "))
    rows[[length(rows) + 1L]] <- s33c_var_row("moment_icc_cc1_cage", NA, mom, "none", me, v, "empty", "CombZ", vc, note = S33C_NOTE[["moment"]],
                                              from_model = FALSE)
  }
  list(rows = data.table::rbindlist(rows), models = data.table::rbindlist(mods), pb = pbo, failed = failed_primary)
}

#' The full Phase-3 path on a CombZ vector (the real one or the placebo permutation).
s33c_estimate <- function(st, cz, frozen, ctx, seeds, B, log, stop_on_bridge = FALSE, check_frozen = TRUE) {
  # parent reproduction and bridge first (GC-C13), before any decomposition row
  br <- s33c_bridge(st$base, cz, if (check_frozen) frozen else NULL)
  g13 <- s33c_bridge_gates(br, check_frozen)
  if (stop_on_bridge) s33_stop_on_gates(g13)
  res <- lapply(S33C_METRICS, function(m) s33c_decompose_metric(st, cz, m, ctx, seeds, B, log))
  names(res) <- S33C_METRICS
  em <- s33c_empty_models(st, cz, ctx, seeds, B, log)
  est <- data.table::rbindlist(lapply(res, `[[`, "est"))
  list(bridge = br, g13 = g13, res = res, empty = em, est = est, influence = s33c_influence(est),
       models = rbind(data.table::rbindlist(lapply(res, `[[`, "models"), fill = TRUE), br$fits, em$models, fill = TRUE),
       variance = rbind(data.table::rbindlist(lapply(res, `[[`, "variance")), em$rows), corr = data.table::rbindlist(lapply(res, `[[`, "corr")))
}

#' GC-C13 rows from a bridge result: (a) the 8 SIS_ONLY Stage 29 v1.0.1 rows reproduced (2 predictors x (sexavg 8,
#' DiD 8, Female 6, Male 6) = 56 quantities, 1e-9; the parent DiD appears only here), not evaluated on permuted outcomes;
#' (b) the per-sex recombination and the sex average (9 rows, 1e-10).
s33c_bridge_gates <- function(br, check_frozen = TRUE) {
  rp <- br$reproduction; rc <- br$recombination
  rbind(
    if (check_frozen) s33_gate_rows("GC-C13a", "8 SIS_ONLY rows of Stage 29 v1.0.1 continuous_estimates reproduced (estimate, se, df, ci, n; CR2 se and df of sexavg and DiD; 1e-9)",
                                    c(nrow(rp) == 56L, rp$diff <= 1e-9), n_expected = 57L,
                                    detail = sprintf("%s; max |diff| %.3g", S33C_NOTE[["parent_did"]], if (nrow(rp)) suppressWarnings(max(rp$diff, na.rm = TRUE)) else NA_real_)) else
      s33_gate_not_evaluated("GC-C13a", "8 SIS_ONLY rows of Stage 29 v1.0.1 reproduced", "permuted (development or placebo) outcomes"),
    s33_gate_rows("GC-C13b", "per-sex recombination (SSw beta_W + SSb beta_B) / (SSw + SSb) = parent OLS slope; sex average = pooled slope (1e-10)",
                  c(nrow(rc) == 9L, rc$diff <= 1e-10), n_expected = 10L,
                  detail = sprintf("max |diff| %.3g", if (nrow(rc)) max(rc$diff) else NA_real_)))
}

#' Identity gate rows (GC-C08) of one estimate run (from the identity rows of s33c_decompose_metric()):
#'   GC-C08a (hard): exact identities, absolute 1e-8 - M1 gamma = xbar_c coefficient of the identity form at M1's variance
#'           parameters (estimate, CI, df); M1 beta_W = its x coefficient = cage-dummy w; S10 and S4 beta_W = M1 beta_W;
#'           per-SD rows = raw x frozen SD. Components that need a FAILED fit are left out (FAILED fits never stop the
#'           run); none left = not evaluated.
#'   GC-C08b: S11 beta_B = M1 beta_B (1e-8), per metric; hard only when both fits are singular, otherwise recorded.
#'   GC-C08c (recorded): the separately optimised identity form = M1 to the optimiser tolerance (relative 1e-6).
s33c_identity_gates <- function(run, tol = S33C_IDENTITY_TOL, tol_separate = S33C_SEPARATE_FIT_TOL) {
  idt <- data.table::rbindlist(lapply(run$res, `[[`, "ident"))
  both <- vapply(run$res, function(z) isTRUE(z$s11_both_singular), TRUE)
  mx <- function(d) if (length(d) && any(is.finite(d))) max(d[is.finite(d)]) else NA_real_
  ex <- idt[kind == "exact"]; exe <- ex[evaluable == TRUE]
  a_text <- "identities (absolute 1e-8): M1 gamma = xbar_c coefficient of the identity form at M1's variance parameters (estimate, CI, df); M1 beta_W = its x coefficient = cage-dummy w; S10 and S4 beta_W = M1 beta_W; per-SD rows = raw x frozen SD"
  bad_ids <- unique(exe[!(diff <= tol), paste(metric_id, identity)])
  a <- if (nrow(exe)) s33_gate_rows("GC-C08a", a_text, exe$diff <= tol,
                                    detail = sprintf("%d of %d components evaluable (the rest need a FAILED fit); max |diff| %.3g; failing: %s", nrow(exe), nrow(ex),
                                                     mx(exe$diff), if (length(bad_ids)) paste(bad_ids, collapse = ", ") else "none")) else
    s33_gate_not_evaluated("GC-C08a", a_text, "every identity needs a FAILED fit")
  b <- data.table::rbindlist(lapply(names(run$res), function(m) {
    z <- idt[kind == "s11" & metric_id == m & evaluable == TRUE]
    txt <- sprintf("S11 beta_B = M1 beta_B (absolute 1e-8) for %s: gated when both fits are singular, otherwise recorded", m)
    if (!nrow(z)) return(s33_gate_not_evaluated("GC-C08b", txt, "M1 or S11 FAILED"))
    s33_gate_rows("GC-C08b", txt, z$diff <= tol, hard = both[[m]],
                  detail = sprintf("%s; |diff| %.3g", if (both[[m]]) "both singular" else "not both singular (recorded)", mx(z$diff)))
  }))
  sep <- idt[kind == "separate" & evaluable == TRUE]
  c_text <- "recorded: the separately optimised identity form (its c07 rows) = M1 within the optimiser tolerance (relative 1e-6): gamma estimate, CI, df; beta_W"
  cc <- if (nrow(sep)) s33_gate_rows("GC-C08c", c_text, sep$diff <= tol_separate, hard = FALSE, detail = sprintf("max relative |diff| %.3g", mx(sep$diff))) else
    s33_gate_not_evaluated("GC-C08c", c_text, "M1 or the identity form FAILED")
  rbind(a, b, cc)
}

#' GC-C04b: the outcome join of Phase 3 (CombZ present for the 87 tracked SIS animals; AnimalNum, Batch and Sex as in
#' Phase 2; no outcome-group column in the module C frames).
s33c_outcome_join_gate <- function(cz, st) {
  s33_gate_rows("GC-C04b", "outcome join: CombZ present for the 87 tracked SIS animals (85 focal); Batch and Sex as in Phase 2; no outcome-group column in the module C frames",
                c(nrow(cz) == nrow(st$base), identical(cz$AnimalNum, st$base$AnimalNum), is.finite(cz$CombZ), cz$Batch == st$base$Batch,
                  cz$Sex == st$base$Sex, !any(c("Group", "outcome_group") %in% names(st$base))))
}

#' Placebo structure checks (GC-C09): ranks, statuses, identities, row counts and columns; no estimate is kept. A FAILED
#' fit on the permutation is structure, not a defect: its bootstrap stream may be empty and its identities not evaluated.
s33c_structure_checks <- function(run, st, B) {
  exp_n <- s33c_expected_counts(st)
  mods <- run$models
  lm4 <- mods[engine == "lmer"]
  rank_ok <- lm4$failed | (lm4$n_fixed_cols == lm4$expected_rank)
  rank_err <- !grepl("expected rank|Rank-deficient", lm4$error %s33or% "") | is.na(lm4$error)
  est <- run$est
  stat_ok <- est$status %in% c("OK", "FAILED") & !(est$status == "FAILED" & is.finite(est$estimate))
  idg <- s33c_identity_gates(run)[gate_id == "GC-C08a"]
  id_ok <- (idg$evaluated & idg$passed %in% TRUE) | !idg$evaluated
  bad_cols <- function(x) grepl("^(p|pval|p_value|statistic|t|t_value|F|F_value|Pr)$|^p_|_p$|Pr\\(", names(x))
  n_obs <- c(c06 = nrow(mods), c07 = nrow(est), c08 = nrow(run$bridge$rows), c09 = nrow(run$influence), c10 = nrow(run$variance), c11 = nrow(run$corr))
  dims <- c(vapply(run$res, function(z) as.numeric(z$boot_dims[1]), 0), empty = if (is.null(run$empty$pb$t)) NA_real_ else nrow(run$empty$pb$t))
  fit_failed <- c(vapply(run$res, function(z) isTRUE(z$m1_failed), TRUE), empty = isTRUE(run$empty$failed))
  boot_ok <- (is.finite(dims) & dims == B) | (fit_failed & is.na(dims))
  lead_n <- nrow(est[lead == TRUE & model == "M1" & variant == "primary", .N, by = .(metric_id, estimand)])
  one <- function(id, check, expected, observed, ok) data.table::data.table(check_id = id, check = check, expected = as.character(expected),
                                                                            observed = as.character(observed), ok = isTRUE(ok))
  x <- rbind(
    one("P1", "every lmer fit has its declared rank (no rank stop)", sprintf("%d fits", nrow(lm4)), sprintf("%d of %d", sum(rank_ok & rank_err), nrow(lm4)), all(rank_ok & rank_err)),
    one("P2", "status values OK or FAILED; FAILED rows carry no estimate", nrow(est), sum(stat_ok), all(stat_ok)),
    one("P3", "identities GC-C08a hold on the placebo (exact algebra; not evaluated only when every identity needs a FAILED fit)", "pass",
        if (!idg$evaluated) "not evaluated" else if (isTRUE(id_ok)) "pass" else "fail", id_ok),
    one("P4", "bridge recombination GC-C13b holds on the placebo", "pass", if (isTRUE(run$g13[gate_id == "GC-C13b"]$passed)) "pass" else "fail",
        isTRUE(run$g13[gate_id == "GC-C13b"]$passed)),
    data.table::rbindlist(lapply(names(exp_n), function(k) one(paste0("P5_", k), paste("row count of", k), exp_n[[k]], n_obs[[k]], exp_n[[k]] == n_obs[[k]]))),
    one("P6", "no p, statistic or t column in any table", 0L, sum(vapply(list(mods, est, run$bridge$rows, run$influence, run$variance, run$corr), function(z) sum(bad_cols(z)), 0L)),
        all(vapply(list(mods, est, run$bridge$rows, run$influence, run$variance, run$corr), function(z) !any(bad_cols(z)), TRUE))),
    one("P7", "bootstrap streams have B rows (M1 rate, M1 occupancy, empty model; empty only for a FAILED fit)", paste(rep(B, 3), collapse = "/"),
        paste(dims, collapse = "/"), all(boot_ok)),
    one("P8", "every primary estimand has a lead row per metric", 6L, lead_n, lead_n == 6L))
  x[, tier := S33_TIER][]
}

# ---------------------------------------------------------------- Phase 3
#' The 8 SIS_ONLY rows of the frozen Stage 29 v1.0.1 continuous estimates (parent comparator; read-only).
s33c_read_parent_rows <- function(ctx) {
  x <- data.table::fread(ctx$inputs[["s29_continuous_estimates"]], select = c("population", "predictor", "estimand", "estimate", "se", "df",
                                                                              "ci_low", "ci_high", "se_cr2", "df_cr2", "n"), showProgress = FALSE)
  x[population == "SIS_ONLY"]
}

s33c_phase3 <- function(design_out, p2, ctx) s33c_phase3_core(design_out, p2, s33c_read_parent_rows(ctx), ctx)

#' Phase 3 of module C on in-memory inputs (frozen = the 8 SIS_ONLY parent rows).
s33c_phase3_core <- function(design_out, p2, frozen, ctx) {
  st <- p2$state
  permuted <- isTRUE(design_out$permuted_outcomes)
  real <- !identical(ctx$run_mode, "development") && !permuted
  a <- design_out$animals
  cz <- a[pop_sis_87 == TRUE, .(AnimalNum, Batch, Sex, CombZ, n_components_present)]
  data.table::setkeyv(cz, "AnimalNum")
  G <- list()
  G[[1]] <- s33c_outcome_join_gate(cz, st)
  if (real) s33_stop_on_gates(G[[1]])
  # ---- GC-C09: the placebo, before any real fit (no checkpoints; internal streams C_placebo_1..3 with B placebo)
  cpl <- ctx; cpl$no_checkpoints <- TRUE
  Bp <- as.integer(ctx$B[["placebo"]])
  pseeds <- vapply(S33C_PLACEBO_SEEDS, function(s) as.integer(ctx$seeds[[s]]), 0L)
  perm <- s33c_permute(cz, as.integer(ctx$seeds[["C_placebo"]]))
  pl_log <- new.env()
  pl <- s33c_estimate(st, perm, NULL, cpl, pseeds, Bp, pl_log, stop_on_bridge = FALSE, check_frozen = FALSE)
  psc <- s33c_structure_checks(pl, st, Bp)
  psc <- rbind(psc, data.table::data.table(check_id = "P9", check = "the permutation keeps every cohort's CombZ multiset", expected = "TRUE",
    observed = as.character(all(perm[, sort(CombZ), by = Batch]$V1 == cz[, sort(CombZ), by = Batch]$V1)),
    ok = all(perm[, sort(CombZ), by = Batch]$V1 == cz[, sort(CombZ), by = Batch]$V1), tier = S33_TIER),
    data.table::data.table(check_id = "P10", check = "the placebo neither read nor wrote a checkpoint", expected = "0", observed = as.character(length(ls(pl_log))),
                           ok = length(ls(pl_log)) == 0L, tier = S33_TIER))
  rm(pl)
  G[[2]] <- s33_gate_rows("GC-C09", "placebo: the full module C path on a within-cohort CombZ permutation (structure only: ranks, statuses, identities, row counts, columns); before any real fit",
                          psc$ok, detail = sprintf("%d structure checks", nrow(psc)))
  if (real) s33_stop_on_gates(G[[2]])
  # ---- the real path
  log <- new.env()
  seeds <- vapply(S33C_BOOT_SEEDS, function(s) as.integer(ctx$seeds[[s]]), 0L)
  B <- as.integer(ctx$B[["parametric"]])
  run <- s33c_estimate(st, cz, frozen, ctx, seeds, B, log, stop_on_bridge = real, check_frozen = !permuted)
  G[[3]] <- run$g13
  G[[4]] <- s33c_identity_gates(run)
  mods <- run$models
  G[[5]] <- s33_gate_rows("GC-C07b", "fit statuses recorded (FAILED fits never stop the run): lmer fits with the declared rank",
                          mods[engine == "lmer"]$failed | mods[engine == "lmer"]$n_fixed_cols == mods[engine == "lmer"]$expected_rank, hard = FALSE,
                          detail = sprintf("%d lmer fits; %d FAILED; %d singular; %d KR fallback rows", nrow(mods[engine == "lmer"]),
                                           sum(mods$failed, na.rm = TRUE), sum(mods$singular %in% TRUE), sum(run$est$ddf_fallback %in% TRUE)))
  # ---- tables: products extended by the CombZ columns / Phase-3 rows
  pr <- p2$products
  c02 <- data.table::copy(pr$c02_animal_terms)
  c02[, `:=`(CombZ = cz$CombZ[match(AnimalNum, cz$AnimalNum)], combz_n_components = cz$n_components_present[match(AnimalNum, cz$AnimalNum)])]
  c02[in_model == FALSE, `:=`(CombZ = NA_real_, combz_n_components = NA_integer_)]
  c03 <- data.table::copy(pr$c03_cage_means)
  cm <- data.table::rbindlist(lapply(S33C_METRICS, function(m) data.table::rbindlist(lapply(names(st$terms[[m]]), function(v) {
    t <- data.table::copy(st$terms[[m]][[v]])[, CombZ := cz$CombZ[match(AnimalNum, cz$AnimalNum)]]
    t[, .(ybar = mean(CombZ), n_c = .N, Batch = Batch[1]), by = CageEpisodeID][
      , combz_cohort_mean := sum(n_c * ybar) / sum(n_c), by = Batch][
      , .(metric_id = m, variant = v, CageEpisodeID, combz_cage_mean = ybar, combz_cohort_mean, combz_cage_mean_centred = ybar - combz_cohort_mean)] }))))
  k3 <- paste(c03$metric_id, c03$variant, c03$CageEpisodeID); km <- paste(cm$metric_id, cm$variant, cm$CageEpisodeID)
  c03[, `:=`(combz_cage_mean = cm$combz_cage_mean[match(k3, km)], combz_cohort_mean = cm$combz_cohort_mean[match(k3, km)],
             combz_cage_mean_centred = cm$combz_cage_mean_centred[match(k3, km)])]
  est <- run$est
  est[, `:=`(metric_id = factor(metric_id, S33C_METRICS))]
  data.table::setorderv(est, "metric_id")
  est[, metric_id := as.character(metric_id)]
  data.table::setcolorder(est, c("metric_id", "metric_label", "variant", "model", "role", "estimand", "estimand_label", "level", "scale", "L", "estimate", "se", "df",
                                 "ci_low", "ci_high", "interval_method", "interval_basis", "units", "status", "singular", "ddf_fallback", "lead",
                                 "quote_role", "compatible_sentence_interval"))
  c06 <- rbind(pr$c06_models, mods, fill = TRUE)
  c10 <- rbind(pr$c10_variance, run$variance)
  tables <- list(c01_population = pr$c01_population, c02_animal_terms = c02, c03_cage_means = c03, c04_information = pr$c04_information,
                 c05_reallocation = pr$c05_reallocation, c06_models = c06, c07_estimates = est, c08_parent_bridge = run$bridge$rows,
                 c09_influence_summary = run$influence, c10_variance = c10, c11_correlations = run$corr, c12_board_offsets = pr$c12_board_offsets,
                 c13_design_sensitivity = pr$c13_design_sensitivity)
  tables <- tables[S33_TABLES[["C"]]]
  audit <- list(c_expected_value_checks = pr$c_expected_value_checks, c_placebo_structure_checks = psc)
  ck <- rbind(st$checkpoints, s33c_ckpt_rows(log, ctx))
  s3 <- run$est[model == "S3_peer_loo" & scale == "per_frozen_sd" & metric_id == "crossing_rate"]
  # stream records for the run audit (audit/seeds_and_streams.csv): the four reallocation streams (c05) and the three
  # parametric bootstrap streams; the placebo's internal streams are never written
  streams <- data.table::rbindlist(list(
    pr$c05_reallocation[, .(seed_name = unname(S33C_REALLOC_VARIANTS[variant]), seed, B, rng_kinds = rng_kind,
                            scheme = "within-cohort reallocation of the CC1 rate (cage sizes kept)", stream_id = paste0("reallocation_", variant),
                            unit_order = row_order)],
    data.table::data.table(seed_name = unname(S33C_BOOT_SEEDS), seed = unname(seeds), B = B, rng_kinds = paste(RNGkind(), collapse = "/"),
                           scheme = "parametric bootstrap (bootMer, use.u = FALSE)", stream_id = names(S33C_BOOT_SEEDS),
                           unit_order = "parametric refits of the fitted model")), fill = TRUE)
  list(tables = tables, audit = audit[S33_AUDIT_TABLES[["C"]]], gates = data.table::rbindlist(G, fill = TRUE), checkpoints = ck,
       cross = c(st$cross, list(s3_delta_peer_per_sd = s3$estimate, s3_singular = s3$singular)), streams = streams)
}
