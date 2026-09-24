# ================================================================
# Inference for the four core raw-RFID domains
# MMMSociability
# ================================================================
# WHY THIS FILE EXISTS
#
# The legacy first-night inference is lm(DomainScore ~ Group * Sex) with no batch
# term and no cage term, and a FLAT Benjamini-Hochberg family of 5 domains x 3
# contrasts = 15 tests per Sex. Two things are wrong with that for this design:
#
#   1. DEPENDENCE. Animals are housed in cages and run in batches. Proximity is
#      mechanically coupled within a cage (animal A's co-location is measured
#      against the same cage-mates as animal B's), and movement and entropy share
#      cage-level environmental variance. Measured cage ICCs at CC1 are 0.45
#      (movement), 0.30 (volatility), 0.22 (social-spatial) and 0.06 (entropy
#      dynamics), so treating animals as exchangeable is anticonservative.
#
#   2. MULTIPLICITY SHAPE. A flat 15-test family answers no question anybody asks.
#      The scientific question is first "does this DOMAIN respond at all", and only
#      then "which groups differ". That is a hierarchy, so the correction is a
#      hierarchy: BH across the four domain omnibus tests, then Holm within a
#      domain whose parent survived.
#
# WHAT THIS FILE DELIBERATELY DOES NOT DO
#   * It does not choose a model by which one gives the smaller p-value. All four
#     model variants are reported side by side for every domain, always.
#   * It does not conceal rank deficiency. Sex is perfectly nested in Batch in this
#     experiment (Male = B1/B2/B5, Female = B3/B4/B6), so a model containing both
#     Sex and Batch is rank deficient BY DESIGN. mmm_rfid_aliasing_audit() names the
#     aliased columns instead of letting R drop them silently.
#   * It does not promote an isolated nominal pairwise result whose parent domain
#     omnibus is unsupported.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

MMM_RFID_GROUP_LEVELS <- c("CON", "RES", "SUS")
MMM_RFID_SEX_LEVELS <- c("Female", "Male")
MMM_RFID_CONTRASTS <- list("RES-CON" = c(-1, 1, 0),
                           "SUS-CON" = c(-1, 0, 1),
                           "SUS-RES" = c(0, -1, 1))

MMM_RFID_DESCRIPTIVE_GUARD <- paste(
  "DESCRIPTIVE with respect to CON/RES/SUS. RES and SUS are defined by a LATER",
  "CombZ composite, so a group difference in first-night or acute behaviour is a",
  "characterisation of animals grouped by their later outcome, not prospective",
  "validation and not a causal claim. Stage 09 owns the prospective question.")

# ------------------------------------------------------------ effect sizes

#' Hedges g for comp - ref, with the small-sample correction.
mmm_rfid_hedges_g <- function(ref, comp) {
  x <- ref[is.finite(ref)]; y <- comp[is.finite(comp)]
  nx <- length(x); ny <- length(y)
  if (nx < 2L || ny < 2L) return(NA_real_)
  sp2 <- ((nx - 1) * stats::var(x) + (ny - 1) * stats::var(y)) / (nx + ny - 2)
  if (!is.finite(sp2) || sp2 <= 0) return(NA_real_)
  d <- (mean(y) - mean(x)) / sqrt(sp2)
  J <- 1 - 3 / (4 * (nx + ny) - 9)
  d * J
}

# --------------------------------------------------------- aliasing audit

#' Report rank deficiency of a fixed-effect design EXPLICITLY.
#'
#' Returns which model-matrix columns are aliased rather than allowing lm()/lmer()
#' to drop them silently. Used before every combined Group x Sex model.
mmm_rfid_aliasing_audit <- function(formula, data, label = "model") {
  mm <- tryCatch(stats::model.matrix(formula, data = data), error = function(e) NULL)
  if (is.null(mm)) {
    return(tibble::tibble(model_label = label, n_columns = NA_integer_, rank = NA_integer_,
                          rank_deficient = NA, aliased_columns = "model matrix not constructible"))
  }
  qrm <- qr(mm)
  aliased <- character(0)
  if (qrm$rank < ncol(mm)) aliased <- colnames(mm)[qrm$pivot[(qrm$rank + 1L):ncol(mm)]]
  tibble::tibble(
    model_label = label,
    n_columns = ncol(mm),
    rank = qrm$rank,
    rank_deficient = qrm$rank < ncol(mm),
    n_aliased = length(aliased),
    aliased_columns = if (length(aliased)) paste(aliased, collapse = "; ") else NA_character_
  )
}

#' Find an anova row by the SET of variables in its term, not by label order.
#'
#' R labels interaction terms in the order the variables first appear in the
#' formula, so the same interaction is "Group:Sex:CC" under one parameterization
#' and "Group:CC:Sex" under another. Matching on the literal string silently
#' misses the term and makes a perfectly estimable effect look non-estimable.
mmm_rfid_anova_row <- function(av, vars) {
  if (is.null(av) || is.null(rownames(av))) return(NA_character_)
  want <- sort(vars)
  hit <- vapply(rownames(av), function(rn) {
    identical(sort(strsplit(rn, ":", fixed = TRUE)[[1]]), want)
  }, logical(1))
  if (!any(hit)) return(NA_character_)
  rownames(av)[which(hit)[1]]
}

# ------------------------------------------------------- omnibus Group test

.mmm_rfid_empty_omnibus <- function(domain, sex, model_label, status) {
  tibble::tibble(Domain = domain, Sex = sex, model = model_label,
                 test_statistic = NA_real_, df_num = NA_real_, df_den = NA_real_,
                 raw_p = NA_real_, n_animals = NA_integer_, n_cages = NA_integer_,
                 cage_sd = NA_real_, residual_sd = NA_real_, cage_icc = NA_real_,
                 singular_fit = NA, model_formula = NA_character_, model_status = status)
}

#' One omnibus Group test for one domain within one Sex, under four models.
#'
#' M1 legacy_lm        DomainScore ~ Group                       (no adjustment)
#' M2 batch_lm         DomainScore ~ Group + factor(Batch)
#' M3 cage_lmer        DomainScore ~ Group + factor(Batch) + (1|CageEpochID)
#' M4 cage_cr2         M2 with clubSandwich CR2 cluster-robust vcov, clustered on
#'                     CageEpochID, tested with the HTZ small-sample Wald test.
#'
#' A singular cage variance is RECORDED (singular_fit, cage_sd = 0), never silently
#' dropped, so "the cage effect was zero here" and "we ignored cages" stay
#' distinguishable.
mmm_rfid_omnibus_group <- function(d, domain, sex) {
  d <- d %>%
    filter(is.finite(.data$DomainScore)) %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           Batch = factor(as.character(.data$Batch)),
           CageEpochID = factor(as.character(.data$CageEpochID))) %>%
    filter(!is.na(.data$Group))
  n_a <- nrow(d); n_c <- nlevels(droplevels(d$CageEpochID))
  if (n_a < 6L || nlevels(droplevels(d$Group)) < 2L) {
    return(.mmm_rfid_empty_omnibus(domain, sex, "all", "insufficient_data"))
  }
  d$Group <- droplevels(d$Group); d$Batch <- droplevels(d$Batch)
  d$CageEpochID <- droplevels(d$CageEpochID)
  multi_batch <- nlevels(d$Batch) > 1L

  out <- list()

  # ---- M1 legacy LM, no adjustment
  f1 <- stats::lm(DomainScore ~ Group, data = d)
  a1 <- stats::anova(f1)
  out[["legacy_lm"]] <- tibble::tibble(
    Domain = domain, Sex = sex, model = "legacy_lm",
    test_statistic = a1["Group", "F value"], df_num = a1["Group", "Df"],
    df_den = a1["Residuals", "Df"], raw_p = a1["Group", "Pr(>F)"],
    n_animals = n_a, n_cages = n_c, cage_sd = NA_real_, residual_sd = stats::sigma(f1),
    cage_icc = NA_real_, singular_fit = NA,
    model_formula = "DomainScore ~ Group", model_status = "fitted")

  # ---- M2 batch-adjusted LM
  f2 <- if (multi_batch) stats::lm(DomainScore ~ Group + Batch, data = d) else f1
  out[["batch_lm"]] <- if (multi_batch) {
    a2 <- stats::anova(stats::lm(DomainScore ~ Batch + Group, data = d))
    tibble::tibble(
      Domain = domain, Sex = sex, model = "batch_lm",
      test_statistic = a2["Group", "F value"], df_num = a2["Group", "Df"],
      df_den = a2["Residuals", "Df"], raw_p = a2["Group", "Pr(>F)"],
      n_animals = n_a, n_cages = n_c, cage_sd = NA_real_, residual_sd = stats::sigma(f2),
      cage_icc = NA_real_, singular_fit = NA,
      model_formula = "DomainScore ~ Group + factor(Batch)", model_status = "fitted")
  } else {
    .mmm_rfid_empty_omnibus(domain, sex, "batch_lm", "single_batch_no_adjustment_possible")
  }

  # ---- M3 cage random intercept
  out[["cage_lmer"]] <- local({
    if (!requireNamespace("lmerTest", quietly = TRUE)) {
      return(.mmm_rfid_empty_omnibus(domain, sex, "cage_lmer", "lmerTest_unavailable"))
    }
    if (n_c < 3L) {
      return(.mmm_rfid_empty_omnibus(domain, sex, "cage_lmer", "too_few_cages"))
    }
    fml <- if (multi_batch) DomainScore ~ Group + Batch + (1 | CageEpochID) else
                            DomainScore ~ Group + (1 | CageEpochID)
    fit <- tryCatch(suppressMessages(suppressWarnings(
      lmerTest::lmer(fml, data = d, REML = TRUE))), error = function(e) e)
    if (inherits(fit, "error")) {
      return(.mmm_rfid_empty_omnibus(domain, sex, "cage_lmer", conditionMessage(fit)))
    }
    vc <- as.data.frame(lme4::VarCorr(fit))
    tau <- vc$vcov[vc$grp == "CageEpochID"][1]
    sig <- vc$vcov[vc$grp == "Residual"][1]
    sing <- lme4::isSingular(fit, tol = 1e-5)
    av <- tryCatch(as.data.frame(stats::anova(fit, type = 2)), error = function(e) NULL)
    if (is.null(av) || !"Group" %in% rownames(av)) {
      return(.mmm_rfid_empty_omnibus(domain, sex, "cage_lmer", "group_term_not_testable"))
    }
    tibble::tibble(
      Domain = domain, Sex = sex, model = "cage_lmer",
      test_statistic = av["Group", "F value"], df_num = av["Group", "NumDF"],
      df_den = av["Group", "DenDF"], raw_p = av["Group", "Pr(>F)"],
      n_animals = n_a, n_cages = n_c,
      cage_sd = sqrt(tau), residual_sd = sqrt(sig),
      cage_icc = tau / (tau + sig), singular_fit = sing,
      model_formula = paste0("DomainScore ~ Group",
                             if (multi_batch) " + factor(Batch)" else "",
                             " + (1 | CageEpochID)"),
      model_status = if (isTRUE(sing)) "fitted_singular_cage_variance" else "fitted")
  })

  # ---- M4 cluster-robust CR2 on cages
  out[["cage_cr2"]] <- local({
    if (!requireNamespace("clubSandwich", quietly = TRUE)) {
      return(.mmm_rfid_empty_omnibus(domain, sex, "cage_cr2", "clubSandwich_unavailable"))
    }
    if (n_c < 3L) {
      return(.mmm_rfid_empty_omnibus(domain, sex, "cage_cr2", "too_few_cages"))
    }
    fit <- if (multi_batch) stats::lm(DomainScore ~ Group + Batch, data = d) else
                            stats::lm(DomainScore ~ Group, data = d)
    idx <- grep("^Group", names(stats::coef(fit)))
    if (length(idx) == 0L) {
      return(.mmm_rfid_empty_omnibus(domain, sex, "cage_cr2", "no_group_coefficients"))
    }
    wt <- tryCatch(suppressWarnings(clubSandwich::Wald_test(
      fit, constraints = clubSandwich::constrain_zero(idx),
      vcov = "CR2", cluster = d$CageEpochID, test = "HTZ")), error = function(e) e)
    if (inherits(wt, "error")) {
      return(.mmm_rfid_empty_omnibus(domain, sex, "cage_cr2", conditionMessage(wt)))
    }
    wt <- as.data.frame(wt)
    tibble::tibble(
      Domain = domain, Sex = sex, model = "cage_cr2",
      test_statistic = wt$Fstat[1], df_num = wt$df_num[1], df_den = wt$df_denom[1],
      raw_p = wt$p_val[1], n_animals = n_a, n_cages = n_c,
      cage_sd = NA_real_, residual_sd = stats::sigma(fit), cage_icc = NA_real_,
      singular_fit = NA,
      model_formula = paste0("DomainScore ~ Group",
                             if (multi_batch) " + factor(Batch)" else "",
                             "  [CR2 cluster-robust on CageEpochID, HTZ]"),
      model_status = "fitted")
  })

  dplyr::bind_rows(out)
}

# ------------------------------------------------------ pairwise decomposition

#' Pairwise Group contrasts for one domain within one Sex.
#'
#' Estimates and CIs come from the batch-adjusted cage random-intercept model when
#' it is available (that is the primary model); Hedges g always comes from the raw
#' scores so the descriptive effect size never depends on the model.
mmm_rfid_pairwise <- function(d, domain, sex) {
  d <- d %>%
    filter(is.finite(.data$DomainScore)) %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           Batch = factor(as.character(.data$Batch)),
           CageEpochID = factor(as.character(.data$CageEpochID))) %>%
    filter(!is.na(.data$Group))
  d$Batch <- droplevels(d$Batch); d$CageEpochID <- droplevels(d$CageEpochID)
  multi_batch <- nlevels(d$Batch) > 1L
  present <- levels(droplevels(d$Group))

  desc <- purrr::map_dfr(names(MMM_RFID_CONTRASTS), function(cn) {
    ref <- sub("^.*-", "", cn); comp <- sub("-.*$", "", cn)
    x <- d$DomainScore[as.character(d$Group) == ref]
    y <- d$DomainScore[as.character(d$Group) == comp]
    tibble::tibble(
      Domain = domain, Sex = sex, contrast = cn, group_ref = ref, group_comp = comp,
      n_ref = length(x), n_comp = length(y),
      n_cages_ref = dplyr::n_distinct(d$CageEpochID[as.character(d$Group) == ref]),
      n_cages_comp = dplyr::n_distinct(d$CageEpochID[as.character(d$Group) == comp]),
      mean_ref = if (length(x)) mean(x) else NA_real_,
      mean_comp = if (length(y)) mean(y) else NA_real_,
      hedges_g = mmm_rfid_hedges_g(x, y))
  })

  model_part <- tryCatch({
    if (!requireNamespace("emmeans", quietly = TRUE) || length(present) < 2L) stop("unavailable")
    use_lmer <- requireNamespace("lmerTest", quietly = TRUE) &&
      nlevels(d$CageEpochID) >= 3L
    fit <- if (use_lmer) {
      fml <- if (multi_batch) DomainScore ~ Group + Batch + (1 | CageEpochID) else
                              DomainScore ~ Group + (1 | CageEpochID)
      suppressMessages(suppressWarnings(lmerTest::lmer(fml, data = d, REML = TRUE)))
    } else if (multi_batch) {
      stats::lm(DomainScore ~ Group + Batch, data = d)
    } else {
      stats::lm(DomainScore ~ Group, data = d)
    }
    emm <- suppressMessages(emmeans::emmeans(fit, ~ Group))
    cv <- MMM_RFID_CONTRASTS[vapply(MMM_RFID_CONTRASTS, function(v) {
      all(MMM_RFID_GROUP_LEVELS[v != 0] %in% present)
    }, logical(1))]
    cv <- lapply(cv, function(v) v[MMM_RFID_GROUP_LEVELS %in% present])
    ct <- as.data.frame(suppressMessages(emmeans::contrast(
      emm, method = cv, adjust = "none", infer = c(TRUE, TRUE))))
    tibble::tibble(
      contrast = as.character(ct$contrast), estimate = ct$estimate, SE = ct$SE,
      df = ct$df, ci_low = ct$lower.CL, ci_high = ct$upper.CL,
      t_ratio = ct$t.ratio, raw_p = ct$p.value,
      model_engine = if (use_lmer) "lmerTest::lmer + emmeans" else "stats::lm + emmeans",
      model_formula = paste(deparse(stats::formula(fit)), collapse = " "))
  }, error = function(e) {
    tibble::tibble(contrast = names(MMM_RFID_CONTRASTS), estimate = NA_real_, SE = NA_real_,
                   df = NA_real_, ci_low = NA_real_, ci_high = NA_real_, t_ratio = NA_real_,
                   raw_p = NA_real_, model_engine = "not_estimable",
                   model_formula = conditionMessage(e))
  })

  desc %>%
    left_join(model_part, by = "contrast") %>%
    mutate(contrast_orientation = "comp - ref",
           interpretation_guard = MMM_RFID_DESCRIPTIVE_GUARD)
}

# ------------------------------------------------------------- multiplicity

#' BH against an EXPLICITLY declared family size.
#'
#' p.adjust() shrinks the family when values are NA, which would silently relax the
#' correction; the declared n is used instead and non-estimable cells stay NA.
mmm_rfid_bh <- function(p, declared_n, family_id) {
  q <- rep(NA_real_, length(p))
  ok <- is.finite(p)
  if (any(ok)) q[ok] <- stats::p.adjust(p[ok], method = "BH", n = declared_n)
  if (sum(ok) != declared_n) {
    warning("FDR family ", family_id, " declares n = ", declared_n, " but ", sum(ok),
            " test(s) are estimable; adjusting against the declared n.", call. = FALSE)
  }
  q
}

#' Holm within a domain's three pairwise contrasts (gated follow-up).
mmm_rfid_holm <- function(p, declared_n = 3L) {
  q <- rep(NA_real_, length(p))
  ok <- is.finite(p)
  if (any(ok)) q[ok] <- stats::p.adjust(p[ok], method = "holm", n = declared_n)
  q
}

#' Apply the hierarchy: BH across domain omnibus tests, then gated Holm pairwise.
#'
#' Pairwise results for an UNSUPPORTED parent are still exported, with CIs and
#' Hedges g, but are marked descriptive/non-gated and carry no adjusted p. That is
#' deliberate: transparency without promotion.
mmm_rfid_apply_hierarchy <- function(omnibus, pairwise, alpha = 0.05,
                                     family_prefix = "RFID_FIRSTNIGHT",
                                     primary_model = "cage_lmer") {
  parents <- omnibus %>%
    filter(.data$model == primary_model) %>%
    group_by(.data$Sex) %>%
    mutate(family_id = paste0(family_prefix, "__", .data$Sex, "__DOMAIN_OMNIBUS_n",
                              dplyr::n()),
           n_tests_in_family = dplyr::n(),
           q = mmm_rfid_bh(.data$raw_p, dplyr::n(), dplyr::first(.data$family_id))) %>%
    ungroup() %>%
    mutate(parent_supported = is.finite(.data$q) & .data$q < alpha,
           multiplicity_method = "Benjamini-Hochberg across the domain omnibus tests within Sex",
           family_role = "PRIMARY")

  gated <- pairwise %>%
    left_join(parents %>% select("Domain", "Sex", parent_q = "q", "parent_supported"),
              by = c("Domain", "Sex")) %>%
    group_by(.data$Domain, .data$Sex) %>%
    mutate(
      pairwise_role = if_else(isTRUE(dplyr::first(.data$parent_supported)),
                              "gated_followup_inference", "descriptive_non_gated"),
      p_holm_within_domain = if (isTRUE(dplyr::first(.data$parent_supported))) {
        mmm_rfid_holm(.data$raw_p, 3L)
      } else {
        rep(NA_real_, dplyr::n())
      }) %>%
    ungroup() %>%
    mutate(
      gating_rule = paste(
        "Pairwise contrasts are inferential ONLY when the parent domain omnibus",
        "survives the primary BH family; otherwise they are exported as descriptive",
        "effect sizes with CIs and carry no adjusted p. An isolated nominal p < 0.05",
        "under an unsupported parent is NOT promoted."))

  list(omnibus = parents, pairwise = gated)
}

# ------------------------------------------------- combined Group x Sex models

#' Formal Group x Sex interaction test for one domain, with aliasing made explicit.
#'
#' Sex is perfectly nested in Batch here, so `~ Group * Sex + Batch` is rank
#' deficient by design: the Sex MAIN effect is not estimable alongside Batch. Two
#' parameterizations are therefore fitted and both are reported:
#'
#'   combined_nobatch   DomainScore ~ Group * Sex + (1 | CageEpochID)
#'                      Sex main effect estimable; batch variation unadjusted.
#'   batch_absorbed     DomainScore ~ Batch + Group + Group:Sex + (1 | CageEpochID)
#'                      Batch absorbs Sex; the Group:Sex differential IS estimable.
#'
#' No Sex MAIN effect is claimed from either model, because it cannot be separated
#' from batch in this experiment.
mmm_rfid_group_sex_interaction <- function(d, domain) {
  d <- d %>%
    filter(is.finite(.data$DomainScore)) %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           Sex = factor(as.character(.data$Sex), levels = MMM_RFID_SEX_LEVELS),
           Batch = factor(as.character(.data$Batch)),
           CageEpochID = factor(as.character(.data$CageEpochID))) %>%
    filter(!is.na(.data$Group), !is.na(.data$Sex))
  if (nrow(d) < 12L || nlevels(droplevels(d$Sex)) < 2L) {
    return(list(
      test = tibble::tibble(Domain = domain, term = "Group:Sex", model = "all",
                            test_statistic = NA_real_, df_num = NA_real_, df_den = NA_real_,
                            raw_p = NA_real_, model_formula = NA_character_,
                            model_status = "insufficient_data"),
      aliasing = mmm_rfid_aliasing_audit(~ Group * Sex + Batch, d,
                                         paste0(domain, ": Group*Sex+Batch"))))
  }

  alias <- dplyr::bind_rows(
    mmm_rfid_aliasing_audit(~ Group * Sex, d, paste0(domain, " :: Group*Sex")),
    mmm_rfid_aliasing_audit(~ Group * Sex + Batch, d, paste0(domain, " :: Group*Sex+Batch")),
    mmm_rfid_aliasing_audit(~ Batch + Group + Group:Sex, d,
                            paste0(domain, " :: Batch+Group+Group:Sex"))
  ) %>% mutate(Domain = domain)

  one <- function(fml, label, want_vars, term_label) {
    if (!requireNamespace("lmerTest", quietly = TRUE)) {
      return(tibble::tibble(Domain = domain, term = term_label, model = label,
                            test_statistic = NA_real_, df_num = NA_real_, df_den = NA_real_,
                            raw_p = NA_real_, model_formula = paste(deparse(fml), collapse = " "),
                            model_status = "lmerTest_unavailable"))
    }
    fit <- tryCatch(suppressMessages(suppressWarnings(
      lmerTest::lmer(fml, data = d, REML = TRUE))), error = function(e) e)
    if (inherits(fit, "error")) {
      return(tibble::tibble(Domain = domain, term = term_label, model = label,
                            test_statistic = NA_real_, df_num = NA_real_, df_den = NA_real_,
                            raw_p = NA_real_, model_formula = paste(deparse(fml), collapse = " "),
                            model_status = conditionMessage(fit)))
    }
    av <- tryCatch(as.data.frame(stats::anova(fit, type = 2)), error = function(e) NULL)
    row_key <- mmm_rfid_anova_row(av, want_vars)
    if (is.null(av) || is.na(row_key)) {
      return(tibble::tibble(Domain = domain, term = term_label, model = label,
                            test_statistic = NA_real_, df_num = NA_real_, df_den = NA_real_,
                            raw_p = NA_real_, model_formula = paste(deparse(fml), collapse = " "),
                            model_status = "term_not_estimable"))
    }
    tibble::tibble(Domain = domain, term = term_label, model = label,
                   test_statistic = av[row_key, "F value"], df_num = av[row_key, "NumDF"],
                   df_den = av[row_key, "DenDF"], raw_p = av[row_key, "Pr(>F)"],
                   model_formula = paste(deparse(fml), collapse = " "),
                   model_status = if (lme4::isSingular(fit, tol = 1e-5))
                     "fitted_singular_cage_variance" else "fitted")
  }

  test <- dplyr::bind_rows(
    one(DomainScore ~ Group * Sex + (1 | CageEpochID), "combined_nobatch",
        c("Group", "Sex"), "Group:Sex"),
    one(DomainScore ~ Batch + Group + Group:Sex + (1 | CageEpochID),
        "batch_absorbed", c("Group", "Sex"), "Group:Sex")
  ) %>%
    mutate(sex_effect_claim_guard = paste(
      "Sex is perfectly nested in Batch in this experiment, so no independently",
      "estimated Sex MAIN effect is reported from any model. A difference between",
      "female and male significance is NOT a sex difference; sex-differential",
      "language requires this formal Group:Sex interaction."))

  list(test = test, aliasing = alias)
}

# ---------------------------------------------------- leave-one-batch-out

#' Leave-one-batch-out effect stability for the pairwise contrasts.
#'
#' This is an EFFECT-STABILITY diagnostic, not a second significance screen. Only
#' estimates and their spread are reported; no p-value from it is ever adjusted or
#' promoted.
mmm_rfid_leave_one_batch_out <- function(d, domain, sex) {
  batches <- sort(unique(as.character(d$Batch)))
  if (length(batches) < 2L) return(tibble::tibble())
  per <- purrr::map_dfr(batches, function(b) {
    sub <- d %>% filter(as.character(.data$Batch) != b)
    if (dplyr::n_distinct(sub$Group) < 2L || nrow(sub) < 6L) return(tibble::tibble())
    mmm_rfid_pairwise(sub, domain, sex) %>%
      mutate(batch_left_out = b) %>%
      select("Domain", "Sex", "contrast", "batch_left_out", "estimate", "hedges_g",
             "n_ref", "n_comp")
  })
  if (nrow(per) == 0L) return(tibble::tibble())
  full <- mmm_rfid_pairwise(d, domain, sex) %>%
    select("Domain", "Sex", "contrast", full_estimate = "estimate", full_g = "hedges_g")
  per %>%
    left_join(full, by = c("Domain", "Sex", "contrast")) %>%
    group_by(.data$Domain, .data$Sex, .data$contrast) %>%
    mutate(
      n_batches_left_out = dplyr::n(),
      estimate_min = min(.data$estimate, na.rm = TRUE),
      estimate_max = max(.data$estimate, na.rm = TRUE),
      estimate_range = .data$estimate_max - .data$estimate_min,
      sign_consistent = length(unique(sign(.data$estimate[is.finite(.data$estimate)]))) == 1L,
      sign_matches_full = sign(.data$estimate) == sign(.data$full_estimate),
      max_abs_shift_from_full = max(abs(.data$estimate - .data$full_estimate), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(diagnostic_role = paste(
      "Effect-stability diagnostic only. Not a significance screen; no p-value from",
      "a leave-one-batch-out refit is adjusted, reported as inference, or used to",
      "promote or demote a domain."))
}

# ================================================================
# BETTER-POWERED PREDECLARED ANALYSES
# ================================================================
# Added after a power audit showed the first design was conservative in two
# specific, fixable ways. Neither change relaxes the cage adjustment, which the
# audit confirmed is necessary: cage ICC is substantial in ALL THREE groups
# (0.30 CON, 0.41 RES, 0.50 SUS for movement), so it is not an artefact of the
# CON arm and must not be dropped.
#
# 1. SEX POOLING. Splitting by Sex halves n and doubles the multiplicity family.
#    It is only justified if the sexes actually differ. The Group x Sex
#    interaction is null for all four domains, so the PREDECLARED RULE is: test
#    the interaction first; if no domain shows an interaction at q < 0.05, the
#    pooled-sex model is primary and the within-sex split becomes the
#    sensitivity. Batch absorbs Sex (Sex is nested in Batch), so the pooled model
#    still adjusts for sex.
#
# 2. FOCUSED TESTS INSTEAD OF DIFFUSE OMNIBUS TESTS. CON < RES < SUS is ORDERED
#    by construction (both labels are thresholds on the same later CombZ
#    composite), so a 2-df unordered omnibus spends power on an alternative
#    nobody hypothesises. The 1-df ordered trend is the matched test. It is
#    declared here, not chosen after seeing results.
#
# WHAT THESE DO NOT CHANGE
#   * The cage random intercept and the CR2 cluster-robust variant still run.
#   * The CON contrasts remain BETWEEN-CAGE and still rest on 6 control cages.
#   * Nothing is selected on the basis of a p-value.

#' Label each contrast by whether it is identified within or between cages.
#'
#' This matters more than anything else in this design. CON animals occupy
#' CON-only cages, so RES-CON and SUS-CON are BETWEEN-cage contrasts whose
#' effective n is 6 control cages. SUS-RES is identified WITHIN the mixed cages
#' and therefore GAINS precision from cage adjustment: measured SE ratio 0.96
#' (cage-adjusted / unadjusted) with residual df ~45, against ratio ~1.2 and
#' df ~12 for the two CON contrasts.
MMM_RFID_CONTRAST_IDENTIFICATION <- c(
  "RES-CON" = "between_cage_6_control_cages",
  "SUS-CON" = "between_cage_6_control_cages",
  "SUS-RES" = "within_cage_identified")

#' Minimum detectable effect at a given power, in Hedges g units.
#'
#' Reported for every contrast so that a null is interpretable. A null on a
#' contrast whose MDE is g = 1.2 is NOT evidence of absence; it means the design
#' could not have detected anything smaller.
mmm_rfid_mde <- function(se, df, sd_outcome, power = 0.80, alpha = 0.05) {
  n <- length(se)
  # sd_outcome is frequently a SCALAR while se/df are vectors. Indexing a
  # length-1 vector with a length-n logical silently yields NA past position 1,
  # so recycle explicitly before masking.
  sd_outcome <- rep_len(sd_outcome, n)
  df <- rep_len(df, n)
  ok <- is.finite(se) & is.finite(df) & df > 0 & is.finite(sd_outcome) & sd_outcome > 0
  out <- rep(NA_real_, n)
  out[ok] <- (stats::qt(1 - alpha / 2, df[ok]) + stats::qt(power, df[ok])) *
    se[ok] / sd_outcome[ok]
  out
}

#' Predeclared ordered-trend (CON < RES < SUS) test, 1 df.
mmm_rfid_ordered_trend <- function(d, domain, stratum_label = "pooled") {
  d <- d %>%
    filter(is.finite(.data$DomainScore)) %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           Batch = factor(as.character(.data$Batch)),
           CageEpochID = factor(as.character(.data$CageEpochID))) %>%
    filter(!is.na(.data$Group))
  d$Batch <- droplevels(d$Batch); d$CageEpochID <- droplevels(d$CageEpochID)
  if (nrow(d) < 10L || nlevels(droplevels(d$Group)) < 3L ||
      !requireNamespace("emmeans", quietly = TRUE)) {
    return(tibble::tibble(Domain = domain, stratum = stratum_label,
                          term = "ordered_trend_CON_RES_SUS", estimate = NA_real_,
                          SE = NA_real_, df = NA_real_, ci_low = NA_real_,
                          ci_high = NA_real_, raw_p = NA_real_, n = nrow(d),
                          mde_hedges_g = NA_real_, model_formula = NA_character_,
                          model_status = "not_estimable", rationale = NA_character_))
  }
  multi_batch <- nlevels(d$Batch) > 1L
  use_lmer <- requireNamespace("lmerTest", quietly = TRUE) && nlevels(d$CageEpochID) >= 3L
  fit <- if (use_lmer) {
    fml <- if (multi_batch) DomainScore ~ Group + Batch + (1 | CageEpochID) else
                            DomainScore ~ Group + (1 | CageEpochID)
    suppressMessages(suppressWarnings(lmerTest::lmer(fml, data = d, REML = TRUE)))
  } else if (multi_batch) {
    stats::lm(DomainScore ~ Group + Batch, data = d)
  } else {
    stats::lm(DomainScore ~ Group, data = d)
  }
  ct <- as.data.frame(suppressMessages(emmeans::contrast(
    suppressMessages(emmeans::emmeans(fit, ~ Group)),
    list(ordered_trend = c(-1, 0, 1)), adjust = "none", infer = c(TRUE, TRUE))))
  sdv <- stats::sd(d$DomainScore)
  tibble::tibble(
    Domain = domain, stratum = stratum_label, term = "ordered_trend_CON_RES_SUS",
    estimate = ct$estimate, SE = ct$SE, df = ct$df,
    ci_low = ct$lower.CL, ci_high = ct$upper.CL, raw_p = ct$p.value, n = nrow(d),
    mde_hedges_g = mmm_rfid_mde(ct$SE, ct$df, sdv),
    model_formula = paste(deparse(stats::formula(fit)), collapse = " "),
    model_status = "fitted",
    rationale = paste(
      "CON < RES < SUS is ordered by construction, so the 1-df ordered trend is the",
      "matched test and the 2-df unordered omnibus spends power on an alternative",
      "that is not hypothesised. Declared before inspecting any result. The trend",
      "is still dominated by the two BETWEEN-CAGE control contrasts."))
}

#' Pooled-sex analysis; primary ONLY when no Group x Sex interaction survives.
mmm_rfid_pooled_sex_analysis <- function(scores, interaction_table, alpha = 0.05) {
  any_interaction <- any(is.finite(interaction_table$q) & interaction_table$q < alpha)
  domains <- intersect(MMM_RFID_CORE_DOMAINS, unique(as.character(scores$Domain)))

  omni <- purrr::map_dfr(domains, function(dm) {
    mmm_rfid_omnibus_group(scores %>% filter(.data$Domain == dm), dm, "pooled")
  })
  trend <- purrr::map_dfr(domains, function(dm) {
    mmm_rfid_ordered_trend(scores %>% filter(.data$Domain == dm), dm, "pooled")
  })
  pw <- purrr::map_dfr(domains, function(dm) {
    mmm_rfid_pairwise(scores %>% filter(.data$Domain == dm), dm, "pooled")
  }) %>%
    mutate(contrast_identification =
             unname(MMM_RFID_CONTRAST_IDENTIFICATION[.data$contrast]))

  omni_primary <- omni %>%
    filter(.data$model == "cage_lmer") %>%
    mutate(family_id = paste0("RFID_POOLED_SEX__DOMAIN_OMNIBUS_n", dplyr::n()),
           n_tests_in_family = dplyr::n(),
           q = mmm_rfid_bh(.data$raw_p, dplyr::n(), "RFID_POOLED_SEX__DOMAIN_OMNIBUS"))
  trend_primary <- trend %>%
    mutate(family_id = paste0("RFID_POOLED_SEX__ORDERED_TREND_n", dplyr::n()),
           n_tests_in_family = dplyr::n(),
           q = mmm_rfid_bh(.data$raw_p, dplyr::n(), "RFID_POOLED_SEX__ORDERED_TREND"))

  gate <- tibble::tibble(
    any_group_by_sex_interaction_supported = any_interaction,
    pooling_justified = !any_interaction,
    n_interaction_tests = nrow(interaction_table),
    min_interaction_q = suppressWarnings(min(interaction_table$q, na.rm = TRUE)),
    predeclared_rule = paste(
      "Test the Group x Sex interaction first. If NO domain shows an interaction at",
      "q < 0.05, the pooled-sex model is primary (n doubles, the family halves from",
      "8 tests to 4) and the within-sex split is reported as the sensitivity. If ANY",
      "domain shows an interaction, the within-sex split stays primary. Batch absorbs",
      "Sex, so the pooled model still adjusts for sex."))

  list(gate = gate, omnibus = omni_primary, omnibus_all_models = omni,
       ordered_trend = trend_primary, pairwise = pw)
}

# ================================================================
# ORTHOGONAL DECOMPOSITION: manipulation effect vs phenotype effect
# ================================================================
# WHY THIS REPLACES THE 2-df OMNIBUS AS THE PRIMARY DECOMPOSITION
#
# CON / RES / SUS is not one three-level factor asking one question. It encodes
# TWO questions that differ in what they are identified by and in how much they
# can be trusted:
#
#   C1  (RES+SUS)/2 - CON   THE MANIPULATION. Did social instability change
#                           behaviour at all? CON is cage-segregated, so this is
#                           a BETWEEN-cage contrast resting on 6 control cages.
#                           It is also confounded with regrouping: CON is the
#                           only arm that keeps its cage-mates.
#
#   C2  SUS - RES           THE PHENOTYPE. Do the outcome-derived phenotypes
#                           differ? RES and SUS share cages and are regrouped
#                           identically, so this is a WITHIN-cage contrast, it
#                           GAINS precision from cage adjustment, and it is free
#                           of both the cage confound and the regrouping confound.
#
# C1 and C2 are orthogonal, so they partition the 2 df of Group exactly. Reporting
# them separately is strictly more informative than one omnibus F, and it stops a
# well-identified phenotype contrast from being buried under a poorly-identified
# control contrast (measured: df ~12 and MDE g ~1.2 for C1, df ~100 and MDE
# g ~0.3 for C2).
#
# NOTE ON MULTIPLICITY. C1 and C2 answer different questions and are therefore
# DIFFERENT FAMILIES. Each is BH-corrected across the four domains on its own.
# They are not pooled, and neither is pooled with the omnibus.

MMM_RFID_ORTHOGONAL_CONTRASTS <- list(
  stress_vs_CON = c(-1, 0.5, 0.5),
  SUS_vs_RES    = c(0, -1, 1)
)

MMM_RFID_ORTHOGONAL_META <- c(
  stress_vs_CON = paste(
    "MANIPULATION contrast (RES+SUS)/2 - CON. BETWEEN-cage: CON is cage-segregated",
    "at every cage change, so this rests on 6 control cages and its effective n is",
    "cages, not animals. ALSO CONFOUNDED WITH REGROUPING - CON is the only arm that",
    "keeps its cage-mates - so a difference here cannot be attributed to stress",
    "phenotype alone."),
  SUS_vs_RES = paste(
    "PHENOTYPE contrast SUS - RES. WITHIN-cage: RES and SUS share cages and are",
    "regrouped identically, so this contrast is free of both the cage confound and",
    "the regrouping confound, and it GAINS precision from cage adjustment. This is",
    "the best-identified comparison in the design. It remains DESCRIPTIVE: RES and",
    "SUS are defined by a later CombZ composite.")
)

#' Orthogonal manipulation / phenotype decomposition for one domain.
#'
#' @param d rows for one domain (optionally one Sex, one cage change).
#' @param include_animal_re add (1|AnimalNum); required when d spans cage changes.
mmm_rfid_orthogonal_contrasts <- function(d, domain, stratum_label = "pooled",
                                          include_animal_re = FALSE,
                                          cagechange_term = FALSE) {
  d <- d %>%
    filter(is.finite(.data$DomainScore)) %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           Batch = factor(as.character(.data$Batch)),
           CageEpochID = factor(as.character(.data$CageEpochID))) %>%
    filter(!is.na(.data$Group))
  if ("AnimalNum" %in% names(d)) d$AnimalNum <- factor(as.character(d$AnimalNum))
  if ("CageChangeLabel" %in% names(d)) d$CC <- factor(as.character(d$CageChangeLabel))
  for (v in intersect(c("Batch", "CageEpochID", "AnimalNum", "CC"), names(d))) {
    d[[v]] <- droplevels(d[[v]])
  }
  empty <- tibble::tibble(
    Domain = domain, stratum = stratum_label,
    contrast = names(MMM_RFID_ORTHOGONAL_CONTRASTS),
    estimate = NA_real_, SE = NA_real_, df = NA_real_, ci_low = NA_real_,
    ci_high = NA_real_, raw_p = NA_real_, n = nrow(d), n_cages = NA_integer_,
    mde_hedges_g = NA_real_, identification = NA_character_,
    model_formula = NA_character_, model_status = "not_estimable",
    contrast_meaning = unname(MMM_RFID_ORTHOGONAL_META))
  if (nrow(d) < 12L || nlevels(droplevels(d$Group)) < 3L ||
      !requireNamespace("emmeans", quietly = TRUE)) return(empty)

  rhs <- "Group"
  if (isTRUE(cagechange_term) && "CC" %in% names(d) && nlevels(d$CC) > 1L) {
    rhs <- paste(rhs, "+ CC")
  }
  if (nlevels(d$Batch) > 1L) rhs <- paste(rhs, "+ Batch")
  res <- character(0)
  if (isTRUE(include_animal_re) && "AnimalNum" %in% names(d) &&
      nlevels(d$AnimalNum) < nrow(d)) res <- c(res, "(1 | AnimalNum)")
  if (nlevels(d$CageEpochID) >= 3L) res <- c(res, "(1 | CageEpochID)")

  fml <- stats::as.formula(paste("DomainScore ~", rhs,
                                 if (length(res)) paste("+", paste(res, collapse = " + ")) else ""))
  fit <- if (length(res) && requireNamespace("lmerTest", quietly = TRUE)) {
    tryCatch(suppressMessages(suppressWarnings(lmerTest::lmer(fml, data = d, REML = TRUE))),
             error = function(e) NULL)
  } else {
    tryCatch(stats::lm(fml, data = d), error = function(e) NULL)
  }
  if (is.null(fit)) return(empty)
  ct <- tryCatch(as.data.frame(suppressMessages(emmeans::contrast(
    suppressMessages(emmeans::emmeans(fit, ~ Group)),
    MMM_RFID_ORTHOGONAL_CONTRASTS, adjust = "none", infer = c(TRUE, TRUE)))),
    error = function(e) NULL)
  if (is.null(ct)) return(empty)
  sdv <- stats::sd(d$DomainScore)
  tibble::tibble(
    Domain = domain, stratum = stratum_label, contrast = as.character(ct$contrast),
    estimate = ct$estimate, SE = ct$SE, df = ct$df,
    ci_low = ct$lower.CL, ci_high = ct$upper.CL, raw_p = ct$p.value,
    n = nrow(d), n_cages = nlevels(d$CageEpochID),
    mde_hedges_g = mmm_rfid_mde(ct$SE, ct$df, sdv),
    identification = ifelse(as.character(ct$contrast) == "SUS_vs_RES",
                            "within_cage", "between_cage_6_control_cages"),
    model_formula = paste(deparse(fml), collapse = " "),
    model_status = "fitted",
    contrast_meaning = unname(MMM_RFID_ORTHOGONAL_META[as.character(ct$contrast)]))
}

#' Run the orthogonal decomposition over all domains and BH within each contrast.
mmm_rfid_orthogonal_family <- function(scores, stratum_label = "pooled",
                                       family_prefix = "RFID_ORTHOGONAL",
                                       include_animal_re = FALSE,
                                       cagechange_term = FALSE) {
  domains <- intersect(MMM_RFID_CORE_DOMAINS, unique(as.character(scores$Domain)))
  out <- purrr::map_dfr(domains, function(dm) {
    mmm_rfid_orthogonal_contrasts(scores %>% filter(.data$Domain == dm), dm,
                                  stratum_label, include_animal_re, cagechange_term)
  })
  if (nrow(out) == 0L) return(out)
  out %>%
    group_by(.data$contrast) %>%
    mutate(family_id = paste0(family_prefix, "__", .data$stratum, "__",
                              .data$contrast, "_n", dplyr::n()),
           n_tests_in_family = dplyr::n(),
           q = mmm_rfid_bh(.data$raw_p, dplyr::n(), dplyr::first(.data$family_id))) %>%
    ungroup() %>%
    mutate(family_rationale = paste(
      "The manipulation contrast and the phenotype contrast answer DIFFERENT",
      "questions and are corrected as SEPARATE families of 4 domains each. They are",
      "not pooled with each other and not pooled with the omnibus family."))
}
