# ================================================================
# ALTERNATIVE INFERENCE AUDIT: different statistics, grouping, multiplicity
# MMMSociability
# ================================================================
# Companion to audit_rfid_conservatism.R, which asked what each MODEL TERM costs.
# This one asks three different questions:
#
#   1. DIFFERENT STATISTICS. Is the Gaussian mixed model with CR2 the right tool
#      for 6 control cages? With that few clusters, CR2's Satterthwaite df are
#      known to be conservative. A RANDOMIZATION test that permutes the actual
#      unit of assignment is assumption-free and is the reference standard here.
#      For the phenotype contrast a WITHIN-CAGE permutation is an exact
#      conditional test. A wild cluster bootstrap is the third alternative.
#
#   2. DIFFERENT GROUPING. Is CON/RES/SUS the most informative split? A
#      within-block (fixed-cage) estimator is the matched analysis for a
#      within-cage contrast. Extreme-group splits on the continuous CombZ raise
#      contrast. Batch as a random effect costs fewer df than six dummies.
#
#   3. IS MULTIPLICITY EVEN THE BINDING CONSTRAINT? Reported by showing every
#      result under NO correction alongside BH, Holm and Bonferroni. If a finding
#      is not compelling unadjusted, the correction was never what killed it.
#
#   4. MESSY DATA HAS A CEILING. Behavioural measures are noisy, and an unreliable
#      score attenuates every effect computed from it. Repeated measures across
#      CC1-CC4 let us ESTIMATE that reliability directly and compute the
#      disattenuated effect a perfectly reliable version of the same score could
#      have shown. This is phenotype-blind and it bounds what the design can do.
#
# DIAGNOSTIC ONLY. Nothing here changes a Stage 28 decision. Every p is
# exploratory. Reads Stage 28 outputs; writes only to its own directory.
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
source_mmm_helper("rfid_domain_core.R")
source_mmm_helper("rfid_domain_inference.R")

ROOT <- mmm_project_root()
S28 <- function(bin = "10min") file.path(
  behavior_stage_dir(ROOT, "28", "rfid_behavioral_domains", bin), "tables")
OUT <- mmm_behavior_guard_numbered_output_path(
  mmm_behavior_output_active_root("rfid_alternative_inference_audit", project_root = ROOT), ROOT)
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
written <- character(0)
w <- function(x, nm) {
  if (is.null(x) || nrow(x) == 0L) return(invisible(NULL))
  write_csv(x, file.path(OUT, nm)); written <<- c(written, nm); invisible(nm)
}
rd <- function(p) suppressWarnings(read_csv(p, col_types = cols(.default = col_guess()),
                                            progress = FALSE))
need <- function(p) { if (!file.exists(p)) stop("Missing Stage 28 input: ", p, call. = FALSE); p }
has <- function(pkg) requireNamespace(pkg, quietly = TRUE)

SEED <- 20260923L
NPERM <- 2000L
NBOOT <- 2000L
DOM <- MMM_RFID_CORE_DOMAINS
CS <- c(-1, 0.5, 0.5); SR <- c(0, -1, 1)

message("ALTERNATIVE INFERENCE AUDIT")

fn <- rd(need(file.path(S28(), "first_night_domain_scores.csv"))) %>%
  filter(is.finite(.data$DomainScore)) %>%
  mutate(AnimalNum = as.character(.data$AnimalNum),
         Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
         Batch = factor(as.character(.data$Batch)),
         CageEpochID = factor(as.character(.data$CageEpochID)))
lg <- rd(need(file.path(S28(), "longitudinal_domain_scores.csv"))) %>%
  filter(is.finite(.data$DomainScore)) %>%
  mutate(AnimalNum = as.character(.data$AnimalNum),
         Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
         Batch = factor(as.character(.data$Batch)),
         CageEpochID = factor(as.character(.data$CageEpochID)),
         CC = factor(as.character(.data$CageChangeLabel)))

# ================================================================
# 1. MEASUREMENT RELIABILITY AND THE ATTENUATION CEILING
# ================================================================
# Reliability is estimated from the four repeated acute windows as the
# animal-level ICC: var(animal) / (var(animal) + var(residual)) after removing
# the shared CageChange mean. This is phenotype-blind.
#
# It is a LOWER BOUND on instrument reliability, because genuine behavioural
# change between cage changes is counted as error here. An observed correlation
# is attenuated by sqrt(reliability), so the disattenuated effect is
# observed / sqrt(reliability): the effect a perfectly reliable version of the
# same score would have shown.
message("  1. reliability / attenuation ceiling")
relia <- purrr::map_dfr(DOM, function(dm) {
  d <- droplevels(lg %>% filter(.data$Domain == dm))
  if (!has("lme4") || dplyr::n_distinct(d$AnimalNum) < 20L) return(tibble())
  m <- tryCatch(suppressMessages(suppressWarnings(
    lme4::lmer(DomainScore ~ CC + (1 | AnimalNum), data = d))), error = function(e) NULL)
  if (is.null(m)) return(tibble())
  vc <- as.data.frame(lme4::VarCorr(m))
  va <- vc$vcov[vc$grp == "AnimalNum"][1]; ve <- vc$vcov[vc$grp == "Residual"][1]
  icc1 <- va / (va + ve)
  k <- 4
  tibble(Domain = dm, n_obs = nrow(d), n_animals = dplyr::n_distinct(d$AnimalNum),
         var_animal = va, var_residual = ve,
         reliability_single_window_ICC1 = icc1,
         reliability_mean_of_4_windows_ICC1k = (k * icc1) / (1 + (k - 1) * icc1),
         attenuation_factor_sqrt_rel = sqrt(icc1),
         max_observable_r_given_noise = sqrt(icc1),
         interpretation = dplyr::case_when(
           icc1 < 0.20 ~ "POOR: the single-window score is mostly occasion-specific noise; group effects are heavily attenuated and no sample size fixes it",
           icc1 < 0.40 ~ "FAIR: substantial attenuation; true effects are ~1.6-2.2x larger than observed",
           icc1 < 0.60 ~ "MODERATE: meaningful attenuation",
           TRUE ~ "GOOD: little attenuation"))
})
w(relia, "1_reliability_attenuation_ceiling.csv")

# Disattenuated first-night effects.
disatt <- tibble()
op <- file.path(S28(), "first_night_orthogonal_contrasts_pooled.csv")
if (file.exists(op) && nrow(relia) > 0L) {
  disatt <- rd(op) %>%
    select("Domain", "contrast", "estimate", "SE", "raw_p", "mde_hedges_g") %>%
    left_join(relia %>% select("Domain", rel = "reliability_single_window_ICC1"),
              by = "Domain") %>%
    mutate(disattenuated_estimate = .data$estimate / sqrt(.data$rel),
           disattenuated_mde = .data$mde_hedges_g / sqrt(.data$rel),
           note = paste(
             "disattenuated_estimate is what this contrast would be if the domain",
             "score were measured without occasion-specific noise. It is NOT a",
             "result: it has no valid standard error and must never be tested. It",
             "bounds how much of the null is instrument noise rather than absence",
             "of an effect."))
}
w(disatt, "1b_disattenuated_effects.csv")

# ================================================================
# 2. RANDOMIZATION TESTS ON THE ACTUAL UNIT OF ASSIGNMENT
# ================================================================
message("  2. randomization tests")
set.seed(SEED)

# 2a. BETWEEN-CAGE: restricted permutation of WHICH CAGE IS THE CONTROL CAGE,
#     within batch. In this design each batch has exactly one CON-only cage, so
#     the randomization set is "which of that batch's cages was the control one".
#     That is the design's own randomization, and the test is exact up to Monte
#     Carlo error.
cage_map <- fn %>%
  distinct(.data$CageEpochID, .data$Batch, .data$Group) %>%
  group_by(.data$CageEpochID, .data$Batch) %>%
  summarise(is_con_cage = all(as.character(.data$Group) == "CON"), .groups = "drop")
perm_between <- purrr::map_dfr(DOM, function(dm) {
  d <- droplevels(fn %>% filter(.data$Domain == dm) %>%
                    left_join(cage_map, by = c("CageEpochID", "Batch")))
  obs_stat <- function(dd) {
    m <- stats::lm(DomainScore ~ Batch + is_con_cage, data = dd)
    unname(stats::coef(m)["is_con_cageTRUE"])
  }
  t_obs <- tryCatch(obs_stat(d), error = function(e) NA_real_)
  if (!is.finite(t_obs)) return(tibble())
  batches <- levels(d$Batch)
  null_stats <- vapply(seq_len(NPERM), function(i) {
    newlab <- rep(FALSE, nrow(d))
    for (b in batches) {
      cg <- unique(as.character(d$CageEpochID[as.character(d$Batch) == b]))
      if (length(cg) < 2L) next
      pick <- sample(cg, 1L)
      newlab[as.character(d$CageEpochID) == pick] <- TRUE
    }
    dd <- d; dd$is_con_cage <- newlab
    tryCatch(obs_stat(dd), error = function(e) NA_real_)
  }, numeric(1))
  null_stats <- null_stats[is.finite(null_stats)]
  tibble(Domain = dm, contrast = "CON_cage_vs_stressed_cages",
         test = "restricted cage-level randomization (which cage was control, within batch)",
         observed = t_obs, n_perm = length(null_stats),
         perm_p_two_sided = (1 + sum(abs(null_stats) >= abs(t_obs))) / (1 + length(null_stats)))
})
w(perm_between, "2a_permutation_between_cage.csv")

# 2b. WITHIN-CAGE: permute SUS/RES labels WITHIN each mixed cage. This conditions
#     on cage composition exactly, so it needs no cage model at all and is exact.
perm_within <- purrr::map_dfr(DOM, function(dm) {
  d <- droplevels(fn %>% filter(.data$Domain == dm, .data$Group != "CON"))
  if (nrow(d) < 20L) return(tibble())
  mixed <- d %>% group_by(.data$CageEpochID) %>%
    summarise(k = dplyr::n_distinct(.data$Group), .groups = "drop") %>%
    filter(.data$k == 2L) %>% pull("CageEpochID")
  dm2 <- droplevels(d %>% filter(.data$CageEpochID %in% mixed))
  if (nrow(dm2) < 12L) return(tibble())
  stat <- function(g) {
    # cage-demeaned difference: the exact within-block contrast
    y <- dm2$DomainScore
    cg <- as.character(dm2$CageEpochID)
    ybar <- stats::ave(y, cg, FUN = mean)
    gi <- ifelse(g == "SUS", 1, -1)
    gbar <- stats::ave(gi, cg, FUN = mean)
    sum((y - ybar) * (gi - gbar))
  }
  g_obs <- as.character(dm2$Group)
  t_obs <- stat(g_obs)
  null_stats <- vapply(seq_len(NPERM), function(i) {
    gp <- unsplit(lapply(split(g_obs, as.character(dm2$CageEpochID)), sample),
                  as.character(dm2$CageEpochID))
    stat(gp)
  }, numeric(1))
  tibble(Domain = dm, contrast = "SUS_vs_RES_within_cage",
         test = "exact conditional permutation of SUS/RES labels WITHIN mixed cages",
         n_mixed_cages = length(mixed), n_animals = nrow(dm2),
         observed = t_obs, n_perm = NPERM,
         perm_p_two_sided = (1 + sum(abs(null_stats) >= abs(t_obs))) / (1 + NPERM))
})
w(perm_within, "2b_permutation_within_cage.csv")

# 2c. WILD CLUSTER BOOTSTRAP-t on cages, for the between-cage contrast. With few
#     clusters this generally outperforms CR2.
message("  2c. wild cluster bootstrap")
set.seed(SEED + 1L)
wild <- purrr::map_dfr(DOM, function(dm) {
  d <- droplevels(fn %>% filter(.data$Domain == dm))
  X <- stats::model.matrix(~ Batch + Group, data = d)
  y <- d$DomainScore
  cl <- as.character(d$CageEpochID)
  fit <- stats::lm.fit(X, y)
  idx <- grep("^Group", colnames(X))
  if (length(idx) == 0L) return(tibble())
  cvec <- rep(0, ncol(X)); nmX <- colnames(X)
  cvec[nmX == "GroupRES"] <- 0.5; cvec[nmX == "GroupSUS"] <- 0.5
  tstat <- function(yy) {
    f <- stats::lm.fit(X, yy)
    b <- f$coefficients; b[is.na(b)] <- 0
    est <- sum(cvec * b)
    r <- yy - X %*% b
    XtXi <- tryCatch(solve(crossprod(X)), error = function(e) NULL)
    if (is.null(XtXi)) return(NA_real_)
    meat <- matrix(0, ncol(X), ncol(X))
    for (g in unique(cl)) {
      ii <- which(cl == g)
      sg <- crossprod(X[ii, , drop = FALSE], r[ii, drop = FALSE])
      meat <- meat + tcrossprod(sg)
    }
    V <- XtXi %*% meat %*% XtXi
    se <- sqrt(max(0, as.numeric(t(cvec) %*% V %*% cvec)))
    if (se <= 0) return(NA_real_)
    est / se
  }
  t_obs <- tstat(y)
  if (!is.finite(t_obs)) return(tibble())
  # impose the null: refit without the Group terms, bootstrap those residuals
  X0 <- X[, setdiff(seq_len(ncol(X)), idx), drop = FALSE]
  f0 <- stats::lm.fit(X0, y)
  r0 <- y - X0 %*% f0$coefficients
  fitted0 <- as.numeric(X0 %*% f0$coefficients)
  ucl <- unique(cl)
  tb <- vapply(seq_len(NBOOT), function(i) {
    v <- stats::setNames(sample(c(-1, 1), length(ucl), replace = TRUE), ucl)
    ystar <- fitted0 + as.numeric(r0) * v[cl]
    tstat(ystar)
  }, numeric(1))
  tb <- tb[is.finite(tb)]
  tibble(Domain = dm, contrast = "stress_vs_CON",
         test = "wild cluster bootstrap-t (Rademacher, null imposed, cages as clusters)",
         t_observed = t_obs, n_boot = length(tb),
         boot_p_two_sided = (1 + sum(abs(tb) >= abs(t_obs))) / (1 + length(tb)))
})
w(wild, "2c_wild_cluster_bootstrap.csv")

# ================================================================
# 3. ALTERNATIVE GROUPINGS AND ESTIMATORS
# ================================================================
message("  3. alternative groupings")

# 3a. WITHIN-BLOCK (fixed-cage) estimator for the phenotype contrast. For a
#     contrast identified within cages this is the matched analysis and uses only
#     within-cage information - no partial pooling, no between-cage leakage.
block <- purrr::map_dfr(DOM, function(dm) {
  d <- droplevels(fn %>% filter(.data$Domain == dm, .data$Group != "CON"))
  if (nrow(d) < 20L) return(tibble())
  keep <- d %>% group_by(.data$CageEpochID) %>%
    filter(dplyr::n_distinct(.data$Group) == 2L) %>% ungroup() %>% droplevels()
  if (nrow(keep) < 12L) return(tibble())
  m_fixed <- stats::lm(DomainScore ~ CageEpochID + Group, data = keep)
  a <- stats::anova(m_fixed)
  co <- summary(m_fixed)$coefficients
  rn <- grep("^Group", rownames(co), value = TRUE)[1]
  m_re <- if (has("lmerTest")) tryCatch(suppressMessages(suppressWarnings(
    lmerTest::lmer(DomainScore ~ Group + (1 | CageEpochID), data = keep))),
    error = function(e) NULL) else NULL
  p_re <- if (!is.null(m_re)) tryCatch(
    as.data.frame(stats::anova(m_re, type = 2))["Group", "Pr(>F)"],
    error = function(e) NA_real_) else NA_real_
  tibble(Domain = dm, n_mixed_cages = dplyr::n_distinct(keep$CageEpochID),
         n_animals = nrow(keep),
         estimate_fixed_block = unname(co[rn, 1]), SE_fixed_block = unname(co[rn, 2]),
         df_fixed_block = m_fixed$df.residual,
         p_fixed_block = unname(co[rn, 4]),
         p_random_intercept = p_re,
         note = paste(
           "The fixed-cage estimator conditions on cage exactly and is the matched",
           "within-block analysis. It discards between-cage information, which for",
           "a within-cage contrast is noise rather than signal. Comparing it with",
           "the random-intercept p shows whether partial pooling is helping or",
           "leaking between-cage variation into the phenotype contrast."))
})
w(block, "3a_within_block_fixed_cage_estimator.csv")

# 3b. BATCH AS RANDOM rather than six fixed dummies.
batch_re <- purrr::map_dfr(DOM, function(dm) {
  d <- droplevels(fn %>% filter(.data$Domain == dm))
  if (!has("lmerTest")) return(tibble())
  mf <- tryCatch(suppressMessages(suppressWarnings(
    lmerTest::lmer(DomainScore ~ Group + Batch + (1 | CageEpochID), data = d))),
    error = function(e) NULL)
  mr <- tryCatch(suppressMessages(suppressWarnings(
    lmerTest::lmer(DomainScore ~ Group + (1 | Batch) + (1 | CageEpochID), data = d))),
    error = function(e) NULL)
  if (is.null(mf) || is.null(mr) || !has("emmeans")) return(tibble())
  gg <- function(m) {
    ct <- as.data.frame(suppressMessages(emmeans::contrast(
      suppressMessages(emmeans::emmeans(m, ~ Group)),
      list(stress_vs_CON = CS, SUS_vs_RES = SR), adjust = "none")))
    setNames(ct$p.value, as.character(ct$contrast))
  }
  pf <- gg(mf); pr <- gg(mr)
  tibble(Domain = dm, contrast = names(pf),
         p_batch_fixed = unname(pf), p_batch_random = unname(pr[names(pf)]),
         note = "Batch as a random effect spends ~1 df instead of 5; with 6 batches the gain is small but free.")
})
w(batch_re, "3b_batch_fixed_vs_random.csv")

# 3c. EXTREME-GROUP split on the continuous CombZ among stressed animals.
#     Contrasting the tails raises the expected effect size at the cost of n.
message("  3c. extreme-group split")
extreme <- tibble()
czp <- tryCatch(mmm_path_get("behavior.later_outcome_combz", "animal_level",
                             required = FALSE), error = function(e) NA_character_)
if (!is.na(czp) && file.exists(czp)) {
  cz <- rd(czp)
  if (all(c("AnimalNum", "CombZ") %in% names(cz))) {
    czs <- cz %>% transmute(AnimalNum = as.character(.data$AnimalNum),
                            CombZ = as.numeric(.data$CombZ))
    extreme <- purrr::map_dfr(DOM, function(dm) {
      d <- fn %>% filter(.data$Domain == dm, .data$Group != "CON") %>%
        inner_join(czs, by = "AnimalNum") %>% filter(is.finite(.data$CombZ))
      if (nrow(d) < 30L) return(tibble())
      purrr::map_dfr(c(0.50, 0.33, 0.25), function(fr) {
        lo <- stats::quantile(d$CombZ, fr); hi <- stats::quantile(d$CombZ, 1 - fr)
        dd <- droplevels(d %>%
          mutate(tail = dplyr::case_when(.data$CombZ <= lo ~ "low",
                                         .data$CombZ >= hi ~ "high",
                                         TRUE ~ NA_character_)) %>%
          filter(!is.na(.data$tail)) %>%
          mutate(tail = factor(.data$tail, levels = c("low", "high"))))
        if (dplyr::n_distinct(dd$tail) < 2L || nrow(dd) < 16L) return(tibble())
        m <- stats::lm(DomainScore ~ Batch + tail, data = dd)
        co <- summary(m)$coefficients
        rn <- grep("^tail", rownames(co), value = TRUE)[1]
        sp <- stats::sd(dd$DomainScore)
        tibble(Domain = dm, split_fraction_per_tail = fr, n = nrow(dd),
               estimate = unname(co[rn, 1]), SE = unname(co[rn, 2]),
               p = unname(co[rn, 4]), hedges_like_g = unname(co[rn, 1]) / sp)
      })
    }) %>%
      mutate(note = paste(
        "Extreme-group splits inflate the observed effect size BY CONSTRUCTION and",
        "the resulting g is not comparable to a full-sample g. Reported to show",
        "whether any phenotype signal exists at the tails at all, not as an effect",
        "estimate. Ignores cage clustering."))
  }
}
w(extreme, "3c_extreme_group_split.csv")

# ================================================================
# 4. IS MULTIPLICITY THE BINDING CONSTRAINT?
# ================================================================
message("  4. multiplicity is / is not the binding constraint")
mult <- tibble()
if (file.exists(op)) {
  o <- rd(op)
  mult <- o %>%
    select("Domain", "contrast", "raw_p") %>%
    group_by(.data$contrast) %>%
    mutate(n_in_family = dplyr::n(),
           q_BH = stats::p.adjust(.data$raw_p, "BH"),
           p_holm = stats::p.adjust(.data$raw_p, "holm"),
           p_bonferroni = pmin(1, .data$raw_p * dplyr::n())) %>%
    ungroup() %>%
    mutate(survives_unadjusted_05 = .data$raw_p < 0.05,
           survives_BH_05 = .data$q_BH < 0.05,
           lost_only_to_correction = .data$survives_unadjusted_05 & !.data$survives_BH_05,
           verdict = dplyr::case_when(
             .data$survives_BH_05 ~ "survives even after correction",
             .data$lost_only_to_correction ~ "LOST TO CORRECTION - multiplicity is the binding constraint here",
             TRUE ~ "not significant even UNADJUSTED - correction is not what killed it"))
}
w(mult, "4_multiplicity_binding_or_not.csv")

# ================================================================
# VERDICT
# ================================================================
safe_min <- function(x) if (length(x) && any(is.finite(x))) min(x, na.rm = TRUE) else NA_real_
verdict <- tibble(
  check = c("reliability_ceiling", "permutation_between_cage", "permutation_within_cage",
            "wild_cluster_bootstrap", "within_block_estimator", "batch_random",
            "extreme_group", "multiplicity_binding"),
  headline = c(
    if (nrow(relia)) paste0("min single-window reliability = ",
                            signif(safe_min(relia$reliability_single_window_ICC1), 3)) else NA,
    if (nrow(perm_between)) paste0("min permutation p = ",
                                   signif(safe_min(perm_between$perm_p_two_sided), 3)) else NA,
    if (nrow(perm_within)) paste0("min exact within-cage permutation p = ",
                                  signif(safe_min(perm_within$perm_p_two_sided), 3)) else NA,
    if (nrow(wild)) paste0("min wild-bootstrap p = ",
                           signif(safe_min(wild$boot_p_two_sided), 3)) else NA,
    if (nrow(block)) paste0("min fixed-block p = ",
                            signif(safe_min(block$p_fixed_block), 3)) else NA,
    if (nrow(batch_re)) paste0("min p with Batch random = ",
                               signif(safe_min(batch_re$p_batch_random), 3)) else NA,
    if (nrow(extreme)) paste0("min extreme-group p = ",
                              signif(safe_min(extreme$p), 3)) else NA,
    if (nrow(mult)) paste0(sum(mult$lost_only_to_correction, na.rm = TRUE),
                           " result(s) lost ONLY to multiplicity correction") else NA))
verdict$audit_role <- paste(
  "DIAGNOSTIC ONLY. Every p here is exploratory and none may be used to change a",
  "Stage 28 decision. The randomization tests are the reference standard for this",
  "design because they permute the actual unit of assignment and assume nothing",
  "about the error distribution.")
w(verdict, "VERDICT.csv")

message("  wrote ", length(written), " tables to ", OUT)
for (i in seq_len(nrow(verdict))) {
  message(sprintf("    %-26s %s", verdict$check[i],
                  ifelse(is.na(verdict$headline[i]), "not evaluated", verdict$headline[i])))
}
