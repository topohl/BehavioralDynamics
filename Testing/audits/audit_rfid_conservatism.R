# ================================================================
# CONSERVATISM AUDIT: how much power does each analytic choice cost?
# MMMSociability
# ================================================================
# PURPOSE
#
# Stage 28 reaches a largely null conclusion. A null is only worth anything if we
# know it was not manufactured by the analysis. This audit enumerates every way
# the design could be needlessly conservative and QUANTIFIES each one, so that
# "we found nothing" can be separated from "we could not have found anything".
#
# THE AUDIT'S PRIMARY CURRENCY IS PRECISION, NOT SIGNIFICANCE.
#
# Every section reports standard errors, residual variance, effective degrees of
# freedom and minimum detectable effect FIRST. These are phenotype-blind: they do
# not depend on which animals are CON, RES or SUS, and they answer "how much
# power did this choice cost?" without reference to any result. p-values appear
# only as a secondary column, and every p produced here is EXPLORATORY and is
# reported with the count of tests that generated it. Nothing in this file
# changes any Stage 28 decision, and no analysis choice may be made on the basis
# of a p-value found here.
#
# WHAT IS AUDITED
#
#   A  Adjustment cost          what Batch / cage / animal terms cost, separately
#                               for the BETWEEN-cage and WITHIN-cage contrasts,
#                               including the mediator argument against adjusting
#                               for cage when treatment is cage-assigned
#   B  Random-effect warrant    is each random effect earning its keep, per domain
#   C  Distribution and robustness   normality, outliers, heteroscedasticity, and
#                               whether rank-based or robust estimators would be
#                               MORE powerful than the Gaussian model
#   D  Prognostic covariates    is residual variance being left on the table
#   E  ANCOVA vs change score   the standard power comparison for pre-post designs
#   F  Estimand: dispersion     are groups differing in VARIANCE rather than mean
#   G  Temporal localisation    is a 12 h average diluting a time-localised acute
#                               response - the largest single aggregation loss
#   H  Multivariate omnibus     is a joint test across the 9 raw features more
#                               powerful than four univariate composites
#   I  Multiplicity cost        what each family declaration actually costs
#   J  Information loss         thresholded groups vs the continuous CombZ
#
# READS Stage 28 outputs and the Stage 01 canonical table. WRITES only into its
# own audit directory. Modifies nothing.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(tibble)
})

.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"),
                                file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("project_paths.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("first_night_window_helpers.R")
source_mmm_helper("first_night_domain_helpers.R")
source_mmm_helper("rfid_domain_core.R")
source_mmm_helper("rfid_domain_scaling.R")
source_mmm_helper("rfid_acute_window_helpers.R")
source_mmm_helper("rfid_domain_inference.R")
source_mmm_helper("rfid_longitudinal_inference.R")

ROOT <- mmm_project_root()
BIN <- "10min_based"; BS <- 600
S28 <- function(bin = "10min") file.path(
  behavior_stage_dir(ROOT, "28", "rfid_behavioral_domains", bin), "tables")
OUT <- mmm_behavior_guard_numbered_output_path(
  mmm_behavior_output_active_root("rfid_conservatism_audit", project_root = ROOT), ROOT)
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
written <- character(0)
w <- function(x, nm) {
  if (is.null(x) || nrow(x) == 0L) return(invisible(NULL))
  write_csv(x, file.path(OUT, nm)); written <<- c(written, nm); invisible(nm)
}
rd <- function(p) suppressWarnings(read_csv(p, col_types = cols(.default = col_guess()),
                                            progress = FALSE))
need <- function(p) { if (!file.exists(p)) stop("Missing Stage 28 input: ", p,
  ". Run Analysis/28_rfid_behavioral_domains.R first.", call. = FALSE); p }

has <- function(pkg) requireNamespace(pkg, quietly = TRUE)
CS <- c(-1, 0.5, 0.5); SR <- c(0, -1, 1)
DOM <- MMM_RFID_CORE_DOMAINS

message("CONSERVATISM AUDIT - quantifying the power cost of each analytic choice")

fn <- rd(need(file.path(S28(), "first_night_domain_scores.csv"))) %>%
  filter(is.finite(.data$DomainScore))
lg <- rd(need(file.path(S28(), "longitudinal_domain_scores.csv"))) %>%
  filter(is.finite(.data$DomainScore))
prep <- function(d) d %>%
  mutate(AnimalNum = as.character(.data$AnimalNum),
         Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
         Batch = factor(as.character(.data$Batch)),
         CageEpochID = factor(as.character(.data$CageEpochID)),
         Sex = factor(as.character(.data$Sex)))
fn <- prep(fn); lg <- prep(lg)
if ("CageChangeLabel" %in% names(lg)) lg$CC <- factor(as.character(lg$CageChangeLabel))

# ---------------------------------------------------------------- helpers
#' Fit one model spec and return PRECISION metrics plus the two orthogonal contrasts.
fit_spec <- function(d, spec_label, fixed_rhs, re_terms = character(0)) {
  d <- droplevels(d)
  fml <- stats::as.formula(paste("DomainScore ~", fixed_rhs,
    if (length(re_terms)) paste("+", paste(re_terms, collapse = " + ")) else ""))
  use_re <- length(re_terms) > 0L
  fit <- tryCatch({
    if (use_re) suppressMessages(suppressWarnings(lmerTest::lmer(fml, data = d, REML = TRUE)))
    else stats::lm(fml, data = d)
  }, error = function(e) NULL)
  if (is.null(fit) || !has("emmeans")) return(NULL)
  ct <- tryCatch(as.data.frame(suppressMessages(emmeans::contrast(
    suppressMessages(emmeans::emmeans(fit, ~ Group)),
    list(stress_vs_CON = CS, SUS_vs_RES = SR), adjust = "none", infer = c(TRUE, TRUE)))),
    error = function(e) NULL)
  if (is.null(ct)) return(NULL)
  resid_sd <- if (use_re) {
    vc <- as.data.frame(lme4::VarCorr(fit)); sqrt(vc$vcov[vc$grp == "Residual"][1])
  } else stats::sigma(fit)
  sdv <- stats::sd(d$DomainScore)
  tibble(spec = spec_label, contrast = as.character(ct$contrast),
         estimate = ct$estimate, SE = ct$SE, df = ct$df, raw_p = ct$p.value,
         residual_sd = resid_sd, outcome_sd = sdv,
         mde_hedges_g = mmm_rfid_mde(ct$SE, ct$df, sdv),
         n = nrow(d), model_formula = paste(deparse(fml), collapse = " "))
}

# ================================================================
# A. ADJUSTMENT COST
# ================================================================
# The decisive question is NOT "is there cage clustering" (there is) but "is the
# cage term a CONFOUNDER or a MEDIATOR". Treatment here is assigned at CAGE level
# for CON: an entire cage is control or entirely stressed. When treatment is
# cluster-assigned, between-cage variance CONTAINS the treatment effect, so
# conditioning on cage removes part of what we are trying to measure. For the
# WITHIN-cage phenotype contrast the same term is a pure confounder-control and
# costs nothing. This section separates the two.
message("  A. adjustment cost")
SPECS <- list(
  list("unadjusted",          "Group", character(0)),
  list("batch_only",          "Group + Batch", character(0)),
  list("cage_RE_only",        "Group", "(1 | CageEpochID)"),
  list("batch_plus_cage_RE",  "Group + Batch", "(1 | CageEpochID)")
)
adj <- purrr::map_dfr(DOM, function(dm) {
  d <- fn %>% filter(.data$Domain == dm)
  purrr::map_dfr(SPECS, function(s) {
    r <- fit_spec(d, s[[1]], s[[2]], s[[3]])
    if (is.null(r)) return(tibble()); r %>% mutate(Domain = dm)
  })
})
if (nrow(adj) > 0L) {
  ref <- adj %>% filter(.data$spec == "unadjusted") %>%
    select("Domain", "contrast", se_unadj = "SE", df_unadj = "df", mde_unadj = "mde_hedges_g")
  adj <- adj %>%
    left_join(ref, by = c("Domain", "contrast")) %>%
    mutate(se_ratio_vs_unadjusted = .data$SE / .data$se_unadj,
           df_ratio_vs_unadjusted = .data$df / .data$df_unadj,
           mde_ratio_vs_unadjusted = .data$mde_hedges_g / .data$mde_unadj,
           identification = ifelse(.data$contrast == "SUS_vs_RES",
                                   "within_cage", "between_cage"),
           cost_verdict = dplyr::case_when(
             .data$se_ratio_vs_unadjusted > 1.10 ~ "adjustment COSTS precision",
             .data$se_ratio_vs_unadjusted < 0.95 ~ "adjustment IMPROVES precision",
             TRUE ~ "approximately neutral"),
           mediator_caveat = ifelse(
             .data$contrast == "stress_vs_CON",
             paste("Treatment is CAGE-ASSIGNED for this contrast (CON cages are",
                   "entirely CON), so between-cage variance CONTAINS the treatment",
                   "effect and the cage term is partly a MEDIATOR, not purely a",
                   "confounder. Adjusting is conservative BY CONSTRUCTION here. The",
                   "design cannot separate the two; with 6 control cages the",
                   "cluster-level analysis is the honest one."),
             paste("RES and SUS share cages, so treatment is NOT cage-assigned for",
                   "this contrast. The cage term is a pure confounder-control and",
                   "adjusting is free or beneficial.")))
}
w(adj, "A_adjustment_cost.csv")

# ================================================================
# B. IS EACH RANDOM EFFECT EARNING ITS KEEP?
# ================================================================
message("  B. random-effect warrant")
re_warrant <- purrr::map_dfr(DOM, function(dm) {
  d <- droplevels(fn %>% filter(.data$Domain == dm))
  if (!has("lmerTest") || nlevels(d$CageEpochID) < 3L) return(tibble())
  m1 <- tryCatch(suppressMessages(suppressWarnings(
    lme4::lmer(DomainScore ~ Group + Batch + (1 | CageEpochID), data = d, REML = FALSE))),
    error = function(e) NULL)
  m0 <- tryCatch(stats::lm(DomainScore ~ Group + Batch, data = d), error = function(e) NULL)
  if (is.null(m1) || is.null(m0)) return(tibble())
  ll1 <- as.numeric(stats::logLik(m1)); ll0 <- as.numeric(stats::logLik(m0))
  lrt <- max(0, 2 * (ll1 - ll0))
  # boundary-corrected: 0.5*chi2_0 + 0.5*chi2_1
  p_lrt <- 0.5 * stats::pchisq(lrt, df = 1, lower.tail = FALSE)
  vc <- as.data.frame(lme4::VarCorr(m1))
  tau <- vc$vcov[vc$grp == "CageEpochID"][1]; sig <- vc$vcov[vc$grp == "Residual"][1]
  tibble(Domain = dm, n = nrow(d), n_cages = nlevels(d$CageEpochID),
         cage_var = tau, resid_var = sig, cage_icc = tau / (tau + sig),
         singular = lme4::isSingular(m1, tol = 1e-5),
         lrt_stat = lrt, lrt_p_boundary_corrected = p_lrt,
         warrant = dplyr::case_when(
           tau / (tau + sig) < 0.01 ~ "cage RE carries ~no variance; retaining it is near-free but also near-pointless",
           p_lrt < 0.05 ~ "cage RE WARRANTED by the data",
           TRUE ~ "cage RE not statistically warranted, but see the design argument"),
         design_argument = paste(
           "A random effect can be warranted by DESIGN even when the LRT is not",
           "significant: animals really are housed in cages and proximity is",
           "mechanically coupled within one. Dropping it because the LRT is null",
           "would be a data-dependent model choice. This column exists to show the",
           "cost, not to license dropping the term."))
})
w(re_warrant, "B_random_effect_warrant.csv")

# ================================================================
# C. DISTRIBUTION, OUTLIERS, HETEROSCEDASTICITY, ROBUST ALTERNATIVES
# ================================================================
message("  C. distribution and robustness")
dist_audit <- purrr::map_dfr(DOM, function(dm) {
  d <- droplevels(fn %>% filter(.data$Domain == dm))
  x <- d$DomainScore
  m <- stats::lm(DomainScore ~ Group + Batch, data = d)
  r <- stats::residuals(m)
  sw <- tryCatch(stats::shapiro.test(r), error = function(e) NULL)
  # Brown-Forsythe (median-centred Levene) for heteroscedasticity by Group
  bf <- tryCatch({
    ag <- stats::ave(x, d$Group, FUN = function(v) abs(v - stats::median(v)))
    stats::anova(stats::lm(ag ~ d$Group))
  }, error = function(e) NULL)
  md <- stats::median(r); mad_r <- stats::mad(r)
  rz <- if (mad_r > 0) (r - md) / (1.4826 * mad_r) else rep(NA_real_, length(r))
  # Rank-based (van der Waerden normal scores) alternative on the same design
  nsc <- stats::qnorm((rank(x) - 0.375) / (length(x) + 0.25))
  d$.ns <- nsc
  p_rank <- tryCatch(stats::anova(stats::lm(.ns ~ Batch + Group, data = d))["Group", "Pr(>F)"],
                     error = function(e) NA_real_)
  p_gauss <- tryCatch(stats::anova(stats::lm(DomainScore ~ Batch + Group, data = d))["Group", "Pr(>F)"],
                      error = function(e) NA_real_)
  tibble(Domain = dm, n = nrow(d),
         skewness = mean((x - mean(x))^3) / stats::sd(x)^3,
         excess_kurtosis = mean((x - mean(x))^4) / stats::sd(x)^4 - 3,
         shapiro_W = if (is.null(sw)) NA_real_ else unname(sw$statistic),
         shapiro_p = if (is.null(sw)) NA_real_ else sw$p.value,
         brown_forsythe_p = if (is.null(bf)) NA_real_ else bf[1, "Pr(>F)"],
         n_resid_beyond_3_robust_sd = sum(abs(rz) > 3, na.rm = TRUE),
         max_abs_robust_resid_z = suppressWarnings(max(abs(rz), na.rm = TRUE)),
         p_gaussian = p_gauss, p_normal_scores = p_rank,
         # A trivially smaller p is not a power gain. Require a MATERIAL improvement.
         rank_would_be_more_powerful = is.finite(p_rank) && is.finite(p_gauss) &&
           p_rank < 0.7 * p_gauss,
         note = paste(
           "If residuals are near-Gaussian and no outlier dominates, the Gaussian",
           "model is already close to optimal and a robust/rank alternative buys",
           "little. A large Brown-Forsythe signal means the groups differ in SPREAD,",
           "which section F tests directly as an estimand in its own right."))
})
w(dist_audit, "C_distribution_and_robustness.csv")

# ================================================================
# D. PROGNOSTIC COVARIATES LEFT ON THE TABLE
# ================================================================
# Adjusting for a covariate that predicts the OUTCOME but not the exposure raises
# power by shrinking residual variance. Available here: cage size, and for the
# longitudinal analysis the CC1 baseline value.
message("  D. prognostic covariates")
cage_size <- fn %>% count(.data$CageEpochID, name = "cage_size")
cov_gain <- purrr::map_dfr(DOM, function(dm) {
  d <- droplevels(fn %>% filter(.data$Domain == dm) %>%
                    left_join(cage_size, by = "CageEpochID"))
  base <- stats::lm(DomainScore ~ Group + Batch, data = d)
  with_cs <- stats::lm(DomainScore ~ Group + Batch + cage_size, data = d)
  tibble(Domain = dm, covariate = "cage_size",
         resid_sd_without = stats::sigma(base), resid_sd_with = stats::sigma(with_cs),
         pct_resid_sd_reduction = 100 * (1 - stats::sigma(with_cs) / stats::sigma(base)),
         covariate_p = tryCatch(summary(with_cs)$coefficients["cage_size", 4],
                                error = function(e) NA_real_),
         verdict = ifelse(100 * (1 - stats::sigma(with_cs) / stats::sigma(base)) > 2,
                          "material precision gain available",
                          "negligible - not worth the df"))
})
w(cov_gain, "D_prognostic_covariate_gain.csv")

# ================================================================
# E. ANCOVA vs CHANGE SCORE
# ================================================================
# For a pre-post design, regressing the follow-up on the baseline (ANCOVA) is
# more powerful than analysing the raw change whenever the baseline-follow-up
# correlation is below 1. Stage 28 uses change scores.
message("  E. ANCOVA vs change score")
ancova <- if (all(c("CC", "AnimalNum") %in% names(lg))) {
  purrr::map_dfr(DOM, function(dm) {
    wdat <- lg %>% filter(.data$Domain == dm) %>%
      select("AnimalNum", "Group", "Sex", "Batch", "CC", "DomainScore") %>%
      tidyr::pivot_wider(names_from = "CC", values_from = "DomainScore")
    base_cage <- lg %>% filter(.data$Domain == dm, .data$CC == "CC1") %>%
      distinct(.data$AnimalNum, baseline_cage = .data$CageEpochID)
    wdat <- wdat %>% left_join(base_cage, by = "AnimalNum")
    purrr::map_dfr(c("CC2", "CC3", "CC4"), function(tgt) {
      if (!all(c("CC1", tgt) %in% names(wdat))) return(tibble())
      dd <- wdat %>%
        mutate(y = .data[[tgt]], base = .data$CC1, chg = .data[[tgt]] - .data$CC1) %>%
        filter(is.finite(.data$y), is.finite(.data$base)) %>%
        droplevels()
      if (nrow(dd) < 20L) return(tibble())
      r_bf <- stats::cor(dd$base, dd$y)
      m_chg <- stats::lm(chg ~ Batch + Group, data = dd)
      m_anc <- stats::lm(y ~ Batch + base + Group, data = dd)
      tibble(Domain = dm, timepoint = tgt, n = nrow(dd),
             baseline_followup_r = r_bf,
             resid_sd_change_score = stats::sigma(m_chg),
             resid_sd_ancova = stats::sigma(m_anc),
             pct_resid_sd_reduction = 100 * (1 - stats::sigma(m_anc) / stats::sigma(m_chg)),
             p_change_score = stats::anova(m_chg)["Group", "Pr(>F)"],
             p_ancova = stats::anova(m_anc)["Group", "Pr(>F)"],
             ancova_more_powerful = stats::sigma(m_anc) < stats::sigma(m_chg),
             note = paste(
               "ANCOVA beats the change score whenever baseline-followup r < 1;",
               "the change score implicitly forces the baseline coefficient to 1.",
               "Neither is cage-adjusted here - this compares the ESTIMATOR, not the",
               "final inference."))
    })
  })
} else tibble()
w(ancova, "E_ancova_vs_change_score.csv")

# ================================================================
# F. WRONG ESTIMAND? DISPERSION RATHER THAN MEAN
# ================================================================
# Stress frequently increases BETWEEN-ANIMAL HETEROGENEITY without moving the
# mean. A mean-comparison is blind to that. This tests dispersion directly.
message("  F. dispersion as an estimand")
disp <- purrr::map_dfr(DOM, function(dm) {
  purrr::map_dfr(c("CC1", "all"), function(scope) {
    d <- if (scope == "CC1") fn %>% filter(.data$Domain == dm) else
      lg %>% filter(.data$Domain == dm)
    d <- droplevels(d)
    if (nrow(d) < 20L) return(tibble())
    # Brown-Forsythe on Group, and the same for the two orthogonal splits
    ad <- stats::ave(d$DomainScore, d$Group, FUN = function(v) abs(v - stats::median(v)))
    p_grp <- tryCatch(stats::anova(stats::lm(ad ~ d$Group))[1, "Pr(>F)"],
                      error = function(e) NA_real_)
    stressed <- factor(ifelse(as.character(d$Group) == "CON", "CON", "stressed"))
    ad2 <- stats::ave(d$DomainScore, stressed, FUN = function(v) abs(v - stats::median(v)))
    p_cs <- tryCatch(stats::anova(stats::lm(ad2 ~ stressed))[1, "Pr(>F)"],
                     error = function(e) NA_real_)
    sr <- d %>% filter(.data$Group != "CON") %>% droplevels()
    p_sr <- if (nrow(sr) > 10L) tryCatch({
      ad3 <- stats::ave(sr$DomainScore, sr$Group, FUN = function(v) abs(v - stats::median(v)))
      stats::anova(stats::lm(ad3 ~ sr$Group))[1, "Pr(>F)"]
    }, error = function(e) NA_real_) else NA_real_
    sds <- d %>% group_by(.data$Group) %>%
      summarise(s = stats::sd(.data$DomainScore), .groups = "drop")
    tibble(Domain = dm, scope = scope, n = nrow(d),
           sd_CON = sds$s[sds$Group == "CON"][1],
           sd_RES = sds$s[sds$Group == "RES"][1],
           sd_SUS = sds$s[sds$Group == "SUS"][1],
           brown_forsythe_p_3group = p_grp,
           brown_forsythe_p_stress_vs_CON = p_cs,
           brown_forsythe_p_SUS_vs_RES = p_sr)
  })
}) %>%
  mutate(note = paste(
    "A dispersion difference is a DIFFERENT scientific claim from a mean",
    "difference and is not tested anywhere in Stage 28. Exploratory:",
    "4 domains x 2 scopes x 3 splits = 24 tests; correct accordingly."))
w(disp, "F_dispersion_estimand.csv")

# ================================================================
# G. IS A 12 h AVERAGE DILUTING A TIME-LOCALISED ACUTE RESPONSE?
# ================================================================
# This is the largest single aggregation loss in the design. Animals are placed
# in the new cage during the preceding Inactive phase, so the first Active block
# is not the moment of introduction - but an acute response, if it exists, is
# still far more likely to sit in the first hours than to be spread evenly over
# 12 h. Averaging over 72 bins would dilute it badly. Here the same domains are
# rebuilt on DISJOINT sub-windows and the two orthogonal contrasts re-estimated.
message("  G. temporal localisation (rebuilding features per sub-window)")
sub_windows <- list(
  "h0_2"   = c(1L, 12L),
  "h2_6"   = c(13L, 36L),
  "h6_12"  = c(37L, 72L),
  "h0_6"   = c(1L, 36L),
  "h0_12_full" = c(1L, 72L)
)
stage01 <- file.path(mmm_derived_metrics_output_root(ROOT), BIN,
                     "all_behavior_metrics.csv")
temporal <- tibble()
if (file.exists(stage01)) {
  dat <- read_csv(stage01, col_types = cols(AnimalNum = col_character(),
                  BinStart = col_datetime(), .default = col_guess()), progress = FALSE)
  dat$AnimalNum <- canonical_animal_id(dat$AnimalNum)
  sel_cc1 <- mmm_rfid_select_acute_window(dat, bin_size_sec = BS, cage_change = "CC1")
  temporal <- purrr::map_dfr(names(sub_windows), function(swn) {
    rg <- sub_windows[[swn]]
    sub <- sel_cc1 %>% filter(.data$target_slot >= rg[1], .data$target_slot <= rg[2])
    ft <- mmm_rfid_build_window_features(sub)
    ft$CageEpochID <- mmm_rfid_cage_epoch_id(ft$Batch, ft$System, ft$CageChangeLabel)
    z <- mmm_rfid_standardize_within_sex(ft, reference_label = swn)$scaled
    sco <- mmm_rfid_score_all_domains(
      z, id_cols = c("AnimalNum", "Group", "Sex", "Batch", "CageEpochID")) %>%
      filter(is.finite(.data$DomainScore)) %>%
      mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
             Batch = factor(as.character(.data$Batch)),
             CageEpochID = factor(as.character(.data$CageEpochID)))
    purrr::map_dfr(DOM, function(dm) {
      r <- fit_spec(droplevels(sco %>% filter(.data$Domain == dm)),
                    swn, "Group + Batch", "(1 | CageEpochID)")
      if (is.null(r)) return(tibble())
      r %>% mutate(Domain = dm, sub_window = swn,
                   n_slots = rg[2] - rg[1] + 1L)
    })
  })
  if (nrow(temporal) > 0L) {
    full <- temporal %>% filter(.data$sub_window == "h0_12_full") %>%
      select("Domain", "contrast", est_full = "estimate", p_full = "raw_p",
             mde_full = "mde_hedges_g")
    temporal <- temporal %>%
      left_join(full, by = c("Domain", "contrast")) %>%
      mutate(abs_est_vs_full = abs(.data$estimate) / abs(.data$est_full),
             identification = ifelse(.data$contrast == "SUS_vs_RES",
                                     "within_cage", "between_cage"),
             # A ratio is meaningless when the full-window estimate is near zero, and a
             # larger point estimate in a noisier sub-window is not evidence of
             # dilution unless it is also BETTER RESOLVED. Require all three.
             dilution_flag = .data$sub_window != "h0_12_full" &
               is.finite(.data$abs_est_vs_full) & .data$abs_est_vs_full > 1.5 &
               abs(.data$estimate) > 0.2 &
               is.finite(.data$raw_p) & is.finite(.data$p_full) & .data$raw_p < .data$p_full,
             caveat = paste(
               "RMSSD and ACF1 are lag-in-bin quantities computed over FEWER slots",
               "in a sub-window, so a sub-window score is not the same construct as",
               "the 12 h score and its sampling variance is larger. A larger point",
               "estimate in a sub-window is a DILUTION SIGNAL to follow up, not a",
               "result. Exploratory: 5 windows x 4 domains x 2 contrasts = 40 tests."))
  }
}
w(temporal, "G_temporal_localisation.csv")

# ================================================================
# H. MULTIVARIATE OMNIBUS ACROSS THE NINE RAW FEATURES
# ================================================================
# Four univariate composites may be less powerful than one joint test that uses
# the full covariance structure of the nine raw features.
message("  H. multivariate omnibus")
multi <- tibble()
rawf <- file.path(S28(), "acute_window_raw_features.csv")
if (file.exists(rawf)) {
  rf <- rd(rawf) %>% filter(.data$CageChangeLabel == "CC1")
  zz <- mmm_rfid_standardize_within_sex(rf, reference_label = "multivariate")$scaled
  Y <- as.matrix(zz[, MMM_RFID_CONTRIBUTORS, drop = FALSE])
  keep <- stats::complete.cases(Y)
  Yk <- Y[keep, , drop = FALSE]
  dk <- zz[keep, ] %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           Batch = factor(as.character(.data$Batch)))
  mv <- tryCatch({
    fitm <- stats::lm(Yk ~ Batch + Group, data = dk)
    a <- stats::anova(fitm, test = "Pillai")
    tibble(test = "MANOVA (Pillai) on 9 standardized raw features, Batch-adjusted",
           n = nrow(Yk), n_features = ncol(Yk),
           pillai = a["Group", "Pillai"], approx_F = a["Group", "approx F"],
           df_num = a["Group", "num Df"], df_den = a["Group", "den Df"],
           raw_p = a["Group", "Pr(>F)"])
  }, error = function(e) tibble(test = "MANOVA", n = nrow(Yk), n_features = ncol(Yk),
                                pillai = NA_real_, approx_F = NA_real_, df_num = NA_real_,
                                df_den = NA_real_, raw_p = NA_real_))
  multi <- mv %>% mutate(
    caveat = paste(
      "A MANOVA ignores the cage clustering entirely and so is ANTICONSERVATIVE",
      "here; it is reported only to bound how much a joint test could possibly buy.",
      "If even the anticonservative joint test is null, no multivariate gain is",
      "being left on the table."))
}
w(multi, "H_multivariate_omnibus.csv")

# ================================================================
# I. WHAT EACH MULTIPLICITY DECLARATION COSTS
# ================================================================
message("  I. multiplicity cost")
mult_cost <- tibble()
op <- file.path(S28(), "first_night_orthogonal_contrasts_pooled.csv")
if (file.exists(op)) {
  o <- rd(op)
  mult_cost <- o %>%
    select("Domain", "contrast", "raw_p") %>%
    group_by(.data$contrast) %>%
    mutate(q_within_contrast_n4 = stats::p.adjust(.data$raw_p, "BH")) %>%
    ungroup() %>%
    mutate(q_pooled_both_contrasts_n8 = stats::p.adjust(.data$raw_p, "BH"),
           q_bonferroni_n8 = pmin(1, .data$raw_p * dplyr::n()),
           p_unadjusted = .data$raw_p,
           interpretation = paste(
             "Three defensible declarations, shown side by side. Correcting across",
             "BOTH contrast types (n=8) treats the manipulation question and the",
             "phenotype question as one family, which they are not. Correcting",
             "within each contrast (n=4) is the shipped choice. No correction at",
             "all is defensible ONLY if each domain is declared a separate primary",
             "endpoint in advance, which it was not."))
}
w(mult_cost, "I_multiplicity_cost.csv")

# ================================================================
# J. INFORMATION LOST BY THRESHOLDING THE OUTCOME
# ================================================================
message("  J. thresholded vs continuous outcome")
contin <- tibble()
czp <- tryCatch(mmm_path_get("behavior.later_outcome_combz", "animal_level",
                             required = FALSE), error = function(e) NA_character_)
if (!is.na(czp) && file.exists(czp)) {
  cz <- rd(czp)
  idc <- intersect(c("AnimalNum"), names(cz))[1]
  if (!is.na(idc) && "CombZ" %in% names(cz)) {
    czs <- cz %>% transmute(AnimalNum = as.character(.data[[idc]]),
                            CombZ = as.numeric(.data$CombZ))
    contin <- purrr::map_dfr(DOM, function(dm) {
      d <- droplevels(fn %>% filter(.data$Domain == dm) %>% inner_join(czs, by = "AnimalNum"))
      if (nrow(d) < 20L || !has("lmerTest")) return(tibble())
      m_grp <- suppressMessages(suppressWarnings(
        lmerTest::lmer(DomainScore ~ Group + Batch + (1 | CageEpochID), data = d)))
      m_con <- suppressMessages(suppressWarnings(
        lmerTest::lmer(DomainScore ~ CombZ + Batch + (1 | CageEpochID), data = d)))
      a <- as.data.frame(stats::anova(m_grp, type = 2))
      s <- as.data.frame(summary(m_con)$coefficients)
      tibble(Domain = dm, n = nrow(d),
             p_group_3level = a["Group", "Pr(>F)"],
             p_combz_continuous = s["CombZ", "Pr(>|t|)"],
             spearman_domain_combz = stats::cor(d$DomainScore, d$CombZ, method = "spearman"),
             continuous_more_powerful = is.finite(s["CombZ", "Pr(>|t|)"]) &
               s["CombZ", "Pr(>|t|)"] < a["Group", "Pr(>F)"])
    }) %>%
      mutate(overlap_warning = paste(
        "Stage 09 ALREADY OWNS the continuous CombZ association for Movement_mean,",
        "and Movement output IS Movement_mean_z, so that row DUPLICATES a frozen",
        "Stage 09 result and must not be presented as an independent finding. The",
        "other three domains are not tested by Stage 09."))
  }
}
w(contin, "J_continuous_vs_thresholded_outcome.csv")

# ================================================================
# VERDICT
# ================================================================
flag <- function(cond) isTRUE(any(cond, na.rm = TRUE))
verdict <- tibble(
  section = c("A_adjustment", "B_random_effects", "C_distribution", "D_covariates",
              "E_ancova", "F_dispersion", "G_temporal", "H_multivariate",
              "I_multiplicity", "J_continuous"),
  recoverable_power_found = c(
    if (nrow(adj)) flag(adj$se_ratio_vs_unadjusted > 1.10 &
                          adj$contrast == "SUS_vs_RES") else NA,
    if (nrow(re_warrant)) FALSE else NA,
    if (nrow(dist_audit)) flag(dist_audit$rank_would_be_more_powerful) else NA,
    if (nrow(cov_gain)) flag(cov_gain$pct_resid_sd_reduction > 2) else NA,
    if (nrow(ancova)) flag(ancova$pct_resid_sd_reduction > 2) else NA,
    if (nrow(disp)) flag(disp$brown_forsythe_p_3group < 0.05) else NA,
    if (nrow(temporal)) flag(temporal$dilution_flag) else NA,
    if (nrow(multi)) flag(multi$raw_p < 0.05) else NA,
    if (nrow(mult_cost)) flag(mult_cost$p_unadjusted < 0.05 &
                                mult_cost$q_within_contrast_n4 >= 0.05) else NA,
    if (nrow(contin)) flag(contin$continuous_more_powerful) else NA),
  meaning = c(
    "cage adjustment costing precision on the WITHIN-cage contrast (it should not)",
    "a cage RE with ~zero variance - retaining it is near-free, so no power is recoverable here (see B table for which domains)",
    "a rank-based estimator outperforming the Gaussian one",
    "an available covariate that would shrink residual variance >2%",
    "ANCOVA shrinking residual variance >2% versus the change score",
    "groups differing in DISPERSION, an estimand Stage 28 never tests",
    "an effect >1.5x larger in a sub-window than in the 12 h average",
    "an (anticonservative) joint multivariate test reaching p<0.05",
    "a result nominally significant but lost to the declared family",
    "the continuous outcome outperforming the thresholded groups"))
verdict$tables_written <- length(written)
verdict$audit_role <- paste(
  "DIAGNOSTIC ONLY. This audit quantifies the power cost of analytic choices. It",
  "does not change any Stage 28 decision, and no model, domain, window or family",
  "may be selected on the basis of a p-value computed here. Every p in this file",
  "is EXPLORATORY and carries the count of tests that produced it.")
w(verdict, "VERDICT.csv")

message("  wrote ", length(written), " tables to ", OUT)
for (i in seq_len(nrow(verdict))) {
  message(sprintf("    %-16s recoverable power: %s", verdict$section[i],
                  ifelse(is.na(verdict$recoverable_power_found[i]), "not evaluated",
                         ifelse(verdict$recoverable_power_found[i], "YES", "no"))))
}
