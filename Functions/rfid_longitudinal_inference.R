# ================================================================
# Longitudinal adaptation across repeated cage changes (CC1-CC4)
# MMMSociability
# ================================================================
# WHY THIS FILE EXISTS
#
# "Adaptation" is a CHANGE IN THE SAME ACUTE RESPONSE across repeated
# perturbations. It cannot be read off a single 12-h composite, and it is not what
# the existing broad Stage 14 epoch model tests: that model is
#   DomainScore ~ Group * Sex + factor(CageChangeIndex) + (1 | AnimalNum)
# (Functions/hmm_stage14_helpers.R:480), which has NO Group x CageChange term at
# all and therefore cannot express a phenotype-specific trajectory, and no cage
# term despite substantial cage-level variance.
#
# THE PRIMARY TEST HERE IS THE JOINT Group x CageChange INTERACTION.
#   * A Group MAIN effect is not adaptation - it is a constant offset.
#   * A CageChange MAIN effect is not phenotype-specific adaptation - in this
#     dataset it is large and shared (movement rises from CC1 to CC4 in every
#     group), plausibly reflecting habituation to the apparatus and ageing.
#   * CageChange is CATEGORICAL in the primary model, because behavioural
#     adaptation need not be monotonic. A linear CC trend is a sensitivity only,
#     never a substitute chosen after seeing the trajectory.
#
# CROSSED DEPENDENCE
# Repeated observations are nested in animals, and animals share a cage within
# each cage change, but cage membership CHANGES between cage changes. The two
# groupings are therefore CROSSED, not nested: (1 | AnimalNum) + (1 | CageEpochID).
#
# A STRUCTURAL CONFOUND THAT MUST TRAVEL WITH EVERY RESULT
# CON animals keep the SAME cage-mates across CC1-CC4 (one distinct mate-set per
# animal; Jaccard = 1.000 across all 72 CON transitions), while every RES and SUS
# animal is fully reshuffled at each cage change (four distinct mate-sets; mean
# Jaccard 0.002-0.007). A Group x CageChange interaction therefore contrasts
# "stable social group" against "novel social group every four days". That IS the
# intended social-instability manipulation, but it means a trajectory difference
# cannot be attributed to stress-phenotype adaptation alone. Every longitudinal
# output carries MMM_RFID_LONGITUDINAL_CONFOUND_GUARD.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
  source_mmm_helper("rfid_domain_inference.R")
}

MMM_RFID_LONGITUDINAL_CONFOUND_GUARD <- paste(
  "STRUCTURAL CONFOUND: CON animals retain identical cage-mates across CC1-CC4",
  "(1 distinct mate-set per animal, Jaccard = 1.000 over all 72 CON transitions),",
  "whereas RES and SUS animals are fully reshuffled at every cage change (4",
  "distinct mate-sets, mean Jaccard 0.002-0.007). Any Group x CageChange effect",
  "therefore confounds stress phenotype with repeated exposure to a NOVEL social",
  "group. This is the intended manipulation, not a data defect, but a trajectory",
  "difference must not be described as phenotype-specific adaptation alone.")

MMM_RFID_CON_CAGE_GUARD <- paste(
  "CON animals are housed in CON-only cages at every cage change (6 CON-only cages,",
  "one per batch, at each of CC1-CC4; no cage ever mixes CON with RES/SUS).",
  "RES-CON and SUS-CON are therefore BETWEEN-CAGE contrasts resting on 6 control",
  "cages, and their effective sample size is cages, not animals. SUS-RES is the",
  "only contrast identified WITHIN cages.")

.mmm_rfid_empty_traj <- function(domain, sex, model_label, status) {
  tibble::tibble(Domain = domain, Sex = sex, model = model_label, term = "Group:CageChange",
                 test_statistic = NA_real_, df_num = NA_real_, df_den = NA_real_,
                 raw_p = NA_real_, n_obs = NA_integer_, n_animals = NA_integer_,
                 n_cages = NA_integer_, animal_sd = NA_real_, cage_sd = NA_real_,
                 residual_sd = NA_real_, singular_fit = NA,
                 model_formula = NA_character_, model_status = status)
}

#' Primary within-Sex trajectory model for one domain.
#'
#' DomainScore ~ Group * factor(CageChangeIndex) + factor(Batch)
#'               + (1 | AnimalNum) + (1 | CageEpochID)
#'
#' Reported alongside reduced variants so the reader can see exactly what the cage
#' and batch terms change. A singular cage or animal variance is recorded, never
#' silently dropped.
mmm_rfid_trajectory_omnibus <- function(d, domain, sex) {
  d <- d %>%
    filter(is.finite(.data$DomainScore)) %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           CC = factor(as.character(.data$CageChangeLabel)),
           Batch = factor(as.character(.data$Batch)),
           AnimalNum = factor(as.character(.data$AnimalNum)),
           CageEpochID = factor(as.character(.data$CageEpochID))) %>%
    filter(!is.na(.data$Group), !is.na(.data$CC))
  d$Group <- droplevels(d$Group); d$CC <- droplevels(d$CC)
  d$Batch <- droplevels(d$Batch); d$AnimalNum <- droplevels(d$AnimalNum)
  d$CageEpochID <- droplevels(d$CageEpochID)

  if (nrow(d) < 20L || nlevels(d$CC) < 2L || nlevels(d$Group) < 2L) {
    return(.mmm_rfid_empty_traj(domain, sex, "all", "insufficient_data"))
  }
  multi_batch <- nlevels(d$Batch) > 1L
  n_obs <- nrow(d); n_an <- nlevels(d$AnimalNum); n_cg <- nlevels(d$CageEpochID)

  fit_one <- function(fml, label) {
    if (!requireNamespace("lmerTest", quietly = TRUE)) {
      return(.mmm_rfid_empty_traj(domain, sex, label, "lmerTest_unavailable"))
    }
    fit <- tryCatch(suppressMessages(suppressWarnings(
      lmerTest::lmer(fml, data = d, REML = TRUE))), error = function(e) e)
    if (inherits(fit, "error")) {
      return(.mmm_rfid_empty_traj(domain, sex, label, conditionMessage(fit)))
    }
    av <- tryCatch(as.data.frame(stats::anova(fit, type = 2)), error = function(e) NULL)
    # NOTE: this lookup key must NOT be called `term`. tibble() data-masks its
    # arguments sequentially, so a local named `term` would be shadowed by the
    # `term =` column created earlier in the same tibble() call and the lookup
    # would silently return NA for every row.
    row_key <- mmm_rfid_anova_row(av, c("Group", "CC"))
    if (is.null(av) || is.na(row_key)) {
      return(.mmm_rfid_empty_traj(domain, sex, label, "interaction_not_estimable"))
    }
    vc <- as.data.frame(lme4::VarCorr(fit))
    gv <- function(g) { v <- vc$vcov[vc$grp == g]; if (length(v)) sqrt(v[1]) else NA_real_ }
    tibble::tibble(
      Domain = domain, Sex = sex, model = label, term = "Group:CageChange",
      test_statistic = av[row_key, "F value"], df_num = av[row_key, "NumDF"],
      df_den = av[row_key, "DenDF"], raw_p = av[row_key, "Pr(>F)"],
      n_obs = n_obs, n_animals = n_an, n_cages = n_cg,
      animal_sd = gv("AnimalNum"), cage_sd = gv("CageEpochID"),
      residual_sd = gv("Residual"),
      singular_fit = lme4::isSingular(fit, tol = 1e-5),
      model_formula = paste(deparse(fml), collapse = " "),
      model_status = if (lme4::isSingular(fit, tol = 1e-5))
        "fitted_singular_variance_component" else "fitted")
  }

  out <- list()
  out[["animal_only"]] <- fit_one(DomainScore ~ Group * CC + (1 | AnimalNum), "animal_only")
  if (multi_batch) {
    out[["animal_batch"]] <- fit_one(DomainScore ~ Group * CC + Batch + (1 | AnimalNum),
                                     "animal_batch")
    out[["primary"]] <- fit_one(
      DomainScore ~ Group * CC + Batch + (1 | AnimalNum) + (1 | CageEpochID), "primary")
  } else {
    out[["primary"]] <- fit_one(
      DomainScore ~ Group * CC + (1 | AnimalNum) + (1 | CageEpochID), "primary")
  }
  dplyr::bind_rows(out) %>%
    mutate(primary_test_note = paste(
      "The JOINT Group x CageChange interaction is the primary adaptation test.",
      "A Group main effect is a constant offset, not adaptation; a CageChange main",
      "effect is shared change, not phenotype-specific adaptation."),
      structural_confound = MMM_RFID_LONGITUDINAL_CONFOUND_GUARD)
}

#' Estimated marginal means for every Group x CageChange cell.
mmm_rfid_trajectory_emmeans <- function(d, domain, sex) {
  d <- d %>%
    filter(is.finite(.data$DomainScore)) %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           CC = factor(as.character(.data$CageChangeLabel)),
           Batch = factor(as.character(.data$Batch)),
           AnimalNum = factor(as.character(.data$AnimalNum)),
           CageEpochID = factor(as.character(.data$CageEpochID))) %>%
    filter(!is.na(.data$Group), !is.na(.data$CC))
  d$Group <- droplevels(d$Group); d$CC <- droplevels(d$CC); d$Batch <- droplevels(d$Batch)
  if (!requireNamespace("emmeans", quietly = TRUE) || nrow(d) < 20L) return(tibble::tibble())
  multi_batch <- nlevels(d$Batch) > 1L
  fml <- if (multi_batch) {
    DomainScore ~ Group * CC + Batch + (1 | AnimalNum) + (1 | CageEpochID)
  } else {
    DomainScore ~ Group * CC + (1 | AnimalNum) + (1 | CageEpochID)
  }
  fit <- tryCatch(suppressMessages(suppressWarnings(
    lmerTest::lmer(fml, data = d, REML = TRUE))), error = function(e) NULL)
  if (is.null(fit)) return(tibble::tibble())
  emm <- tryCatch(as.data.frame(suppressMessages(
    emmeans::emmeans(fit, ~ Group | CC))), error = function(e) NULL)
  if (is.null(emm)) return(tibble::tibble())
  # Raw cell descriptives travel alongside the model-based means.
  raw <- d %>%
    group_by(Group = as.character(.data$Group), CC = as.character(.data$CC)) %>%
    summarise(n_animals = dplyr::n(), raw_mean = mean(.data$DomainScore),
              raw_sd = stats::sd(.data$DomainScore),
              n_cages = dplyr::n_distinct(.data$CageEpochID), .groups = "drop")
  tibble::as_tibble(emm) %>%
    mutate(Group = as.character(.data$Group), CC = as.character(.data$CC)) %>%
    rename(emmean_score = "emmean", ci_low = "lower.CL", ci_high = "upper.CL") %>%
    left_join(raw, by = c("Group", "CC")) %>%
    mutate(Domain = domain, Sex = sex,
           structural_confound = MMM_RFID_LONGITUDINAL_CONFOUND_GUARD) %>%
    select("Domain", "Sex", "Group", CageChangeLabel = "CC", "n_animals", "n_cages",
           "emmean_score", "SE", "df", "ci_low", "ci_high", "raw_mean", "raw_sd",
           "structural_confound")
}

#' Predeclared, scientifically interpretable trajectory follow-ups.
#'
#' These four contrasts are fixed BEFORE looking at any trajectory and are the ONLY
#' follow-ups tested. Every possible CageChange contrast is deliberately not tested.
#' They are corrected with Holm WITHIN a supported domain.
MMM_RFID_TRAJECTORY_FOLLOWUPS <- c(
  "CC4 - CC1",
  "CC2 - CC1",
  "mean(CC3,CC4) - CC1",
  "mean(CC2,CC3,CC4) - CC1"
)

#' Within-group change-from-CC1 contrasts, and their Group differences.
mmm_rfid_trajectory_followups <- function(d, domain, sex) {
  d <- d %>%
    filter(is.finite(.data$DomainScore)) %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           CC = factor(as.character(.data$CageChangeLabel)),
           Batch = factor(as.character(.data$Batch)),
           AnimalNum = factor(as.character(.data$AnimalNum)),
           CageEpochID = factor(as.character(.data$CageEpochID))) %>%
    filter(!is.na(.data$Group), !is.na(.data$CC))
  d$Group <- droplevels(d$Group); d$CC <- droplevels(d$CC); d$Batch <- droplevels(d$Batch)
  ccl <- levels(d$CC)
  if (!requireNamespace("emmeans", quietly = TRUE) || !"CC1" %in% ccl || length(ccl) < 2L) {
    return(tibble::tibble())
  }
  multi_batch <- nlevels(d$Batch) > 1L
  fml <- if (multi_batch) {
    DomainScore ~ Group * CC + Batch + (1 | AnimalNum) + (1 | CageEpochID)
  } else {
    DomainScore ~ Group * CC + (1 | AnimalNum) + (1 | CageEpochID)
  }
  fit <- tryCatch(suppressMessages(suppressWarnings(
    lmerTest::lmer(fml, data = d, REML = TRUE))), error = function(e) NULL)
  if (is.null(fit)) return(tibble::tibble())

  wt <- function(target) {
    w <- rep(0, length(ccl)); names(w) <- ccl
    w["CC1"] <- -1
    hit <- intersect(target, ccl)
    if (length(hit) == 0L) return(NULL)
    w[hit] <- 1 / length(hit)
    w
  }
  specs <- list(
    "CC4 - CC1" = wt("CC4"),
    "CC2 - CC1" = wt("CC2"),
    "mean(CC3,CC4) - CC1" = wt(c("CC3", "CC4")),
    "mean(CC2,CC3,CC4) - CC1" = wt(c("CC2", "CC3", "CC4"))
  )
  specs <- specs[!vapply(specs, is.null, logical(1))]
  if (length(specs) == 0L) return(tibble::tibble())

  emm <- tryCatch(suppressMessages(emmeans::emmeans(fit, ~ CC | Group)),
                  error = function(e) NULL)
  if (is.null(emm)) return(tibble::tibble())
  ct <- tryCatch(as.data.frame(suppressMessages(emmeans::contrast(
    emm, method = specs, adjust = "none", infer = c(TRUE, TRUE)))),
    error = function(e) NULL)
  if (is.null(ct)) return(tibble::tibble())

  tibble::as_tibble(ct) %>%
    mutate(Domain = domain, Sex = sex,
           followup_set = "predeclared",
           followup_note = paste(
             "Predeclared trajectory follow-ups only:",
             paste(MMM_RFID_TRAJECTORY_FOLLOWUPS, collapse = "; "),
             "- within each Group, relative to CC1. Not an exhaustive scan of every",
             "possible CageChange contrast."),
           structural_confound = MMM_RFID_LONGITUDINAL_CONFOUND_GUARD) %>%
    rename(change_contrast = "contrast", ci_low = "lower.CL", ci_high = "upper.CL")
}

#' Within-animal change-score analysis of the predeclared trajectory contrasts.
#'
#' WHY THIS EXISTS. The full Group x CageChange interaction is a 6-df diffuse test.
#' A predeclared change contrast such as mean(CC3,CC4) - CC1 is 2 df and is far
#' better powered for the same question, because each animal serves as its own
#' control and every time-invariant animal-level confounder cancels.
#'
#' WHY IT STILL NEEDS A CAGE TERM. A change score is NOT free of cage dependence:
#' animals remain clustered in cages at CC1 and again at CC3/CC4. Fitting
#' lm(change ~ Group + Batch) without a cage term gives a markedly smaller p-value
#' than the same contrast with a CC1-cage random intercept (measured: 0.0094 vs
#' 0.096 for cross-channel volatility, pooled sexes), and the difference is driven
#' entirely by the BETWEEN-CAGE control contrasts. Both are therefore reported,
#' and the cage-adjusted one is primary.
mmm_rfid_change_score_analysis <- function(scores, change_spec = c("CC3", "CC4"),
                                           baseline = "CC1", stratum_label = "pooled",
                                           label = "mean(CC3,CC4) - CC1") {
  need <- c("AnimalNum", "Group", "Sex", "Batch", "CageEpochID", "CageChangeLabel",
            "Domain", "DomainScore")
  if (!all(need %in% names(scores))) {
    stop("mmm_rfid_change_score_analysis() requires: ",
         paste(setdiff(need, names(scores)), collapse = ", "), call. = FALSE)
  }
  base_cage <- scores %>%
    filter(.data$CageChangeLabel == baseline) %>%
    distinct(.data$AnimalNum, baseline_cage = .data$CageEpochID)

  wide <- scores %>%
    filter(is.finite(.data$DomainScore)) %>%
    select("AnimalNum", "Group", "Sex", "Batch", "Domain", "CageChangeLabel",
           "DomainScore") %>%
    tidyr::pivot_wider(names_from = "CageChangeLabel", values_from = "DomainScore")
  if (!all(c(baseline, change_spec) %in% names(wide))) return(tibble::tibble())
  wide$change <- rowMeans(as.matrix(wide[, change_spec, drop = FALSE])) -
    wide[[baseline]]
  wide <- wide %>%
    left_join(base_cage, by = "AnimalNum") %>%
    filter(is.finite(.data$change))

  domains <- intersect(MMM_RFID_CORE_DOMAINS, unique(as.character(wide$Domain)))
  purrr::map_dfr(domains, function(dm) {
    d <- wide %>% filter(.data$Domain == dm)
    if (stratum_label != "pooled") d <- d %>% filter(.data$Sex == stratum_label)
    d <- d %>%
      mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
             Batch = factor(as.character(.data$Batch)),
             baseline_cage = factor(as.character(.data$baseline_cage)))
    d$Batch <- droplevels(d$Batch); d$baseline_cage <- droplevels(d$baseline_cage)
    if (nrow(d) < 12L || nlevels(droplevels(d$Group)) < 2L) return(tibble::tibble())
    multi_batch <- nlevels(d$Batch) > 1L

    naive <- if (multi_batch) stats::lm(change ~ Batch + Group, data = d) else
      stats::lm(change ~ Group, data = d)
    an <- as.data.frame(stats::anova(naive))
    p_naive <- an["Group", "Pr(>F)"]

    p_cage <- NA_real_; f_cage <- NA_real_; df_cage <- NA_real_; cage_sd <- NA_real_
    if (requireNamespace("lmerTest", quietly = TRUE) && nlevels(d$baseline_cage) >= 3L) {
      fml <- if (multi_batch) change ~ Group + Batch + (1 | baseline_cage) else
        change ~ Group + (1 | baseline_cage)
      fc <- tryCatch(suppressMessages(suppressWarnings(
        lmerTest::lmer(fml, data = d, REML = TRUE))), error = function(e) NULL)
      if (!is.null(fc)) {
        av <- tryCatch(as.data.frame(stats::anova(fc, type = 2)), error = function(e) NULL)
        rk <- mmm_rfid_anova_row(av, "Group")
        if (!is.na(rk)) {
          f_cage <- av[rk, "F value"]; df_cage <- av[rk, "DenDF"]; p_cage <- av[rk, "Pr(>F)"]
        }
        vc <- as.data.frame(lme4::VarCorr(fc))
        cage_sd <- sqrt(vc$vcov[vc$grp == "baseline_cage"][1])
      }
    }
    p_cr2 <- NA_real_
    if (requireNamespace("clubSandwich", quietly = TRUE) && nlevels(d$baseline_cage) >= 3L) {
      idx <- grep("^Group", names(stats::coef(naive)))
      wt <- tryCatch(suppressWarnings(as.data.frame(clubSandwich::Wald_test(
        naive, constraints = clubSandwich::constrain_zero(idx), vcov = "CR2",
        cluster = d$baseline_cage, test = "HTZ"))), error = function(e) NULL)
      if (!is.null(wt)) p_cr2 <- wt$p_val[1]
    }
    ct <- tibble::tibble()
    if (requireNamespace("emmeans", quietly = TRUE) &&
        nlevels(droplevels(d$Group)) == 3L) {
      cc <- as.data.frame(suppressMessages(emmeans::contrast(
        suppressMessages(emmeans::emmeans(naive, ~ Group)), MMM_RFID_CONTRASTS,
        adjust = "none", infer = c(TRUE, TRUE))))
      ct <- tibble::tibble(
        contrast = paste(cc$contrast, collapse = " | "),
        estimates = paste(sprintf("%s=%+.3f(p=%.3g)", cc$contrast, cc$estimate,
                                  cc$p.value), collapse = "; "))
    }
    tibble::tibble(
      Domain = dm, stratum = stratum_label, change_measure = label, n = nrow(d),
      n_baseline_cages = nlevels(d$baseline_cage),
      p_naive_no_cage = p_naive,
      F_cage_adjusted = f_cage, df_cage_adjusted = df_cage,
      p_cage_adjusted = p_cage, baseline_cage_sd = cage_sd,
      p_cr2_cluster_robust = p_cr2,
      contrast_detail = if (nrow(ct)) ct$estimates else NA_character_,
      primary_model = "change ~ Group + factor(Batch) + (1 | baseline CC1 cage)",
      caveat = paste(
        "A change score removes time-invariant ANIMAL confounders but NOT cage",
        "dependence: animals are still clustered at CC1 and again at CC3/CC4. The",
        "cage-adjusted column is primary; p_naive_no_cage is shown only to make the",
        "size of the cage effect visible. RES-CON and SUS-CON remain BETWEEN-CAGE",
        "contrasts on 6 control cages, and CON is also the only never-regrouped arm."))
  })
}

#' Linear CageChange trend, reported as a SENSITIVITY only.
mmm_rfid_trajectory_linear_trend <- function(d, domain, sex) {
  d <- d %>%
    filter(is.finite(.data$DomainScore)) %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           cc_num = suppressWarnings(as.numeric(sub("^\\D*", "", as.character(.data$CageChangeLabel)))),
           Batch = factor(as.character(.data$Batch)),
           AnimalNum = factor(as.character(.data$AnimalNum)),
           CageEpochID = factor(as.character(.data$CageEpochID))) %>%
    filter(!is.na(.data$Group), is.finite(.data$cc_num))
  d$Group <- droplevels(d$Group); d$Batch <- droplevels(d$Batch)
  if (!requireNamespace("lmerTest", quietly = TRUE) || nrow(d) < 20L) return(tibble::tibble())
  multi_batch <- nlevels(d$Batch) > 1L
  fml <- if (multi_batch) {
    DomainScore ~ Group * cc_num + Batch + (1 | AnimalNum) + (1 | CageEpochID)
  } else {
    DomainScore ~ Group * cc_num + (1 | AnimalNum) + (1 | CageEpochID)
  }
  fit <- tryCatch(suppressMessages(suppressWarnings(
    lmerTest::lmer(fml, data = d, REML = TRUE))), error = function(e) NULL)
  if (is.null(fit)) return(tibble::tibble())
  av <- tryCatch(as.data.frame(stats::anova(fit, type = 2)), error = function(e) NULL)
  lin_key <- mmm_rfid_anova_row(av, c("Group", "cc_num"))
  if (is.null(av) || is.na(lin_key)) return(tibble::tibble())
  tibble::tibble(
    Domain = domain, Sex = sex, term = "Group:CageChange_linear",
    test_statistic = av[lin_key, "F value"], df_num = av[lin_key, "NumDF"],
    df_den = av[lin_key, "DenDF"], raw_p = av[lin_key, "Pr(>F)"],
    model_formula = paste(deparse(fml), collapse = " "),
    role = paste("SENSITIVITY ONLY. The categorical Group x CageChange test remains",
                 "primary because behavioural adaptation need not be monotonic. This",
                 "linear trend is never substituted for the categorical test after",
                 "inspecting the trajectory."))
}

#' Formal Group x Sex x CageChange three-way interaction, aliasing made explicit.
mmm_rfid_three_way_interaction <- function(d, domain) {
  d <- d %>%
    filter(is.finite(.data$DomainScore)) %>%
    mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
           Sex = factor(as.character(.data$Sex), levels = MMM_RFID_SEX_LEVELS),
           CC = factor(as.character(.data$CageChangeLabel)),
           Batch = factor(as.character(.data$Batch)),
           AnimalNum = factor(as.character(.data$AnimalNum)),
           CageEpochID = factor(as.character(.data$CageEpochID))) %>%
    filter(!is.na(.data$Group), !is.na(.data$Sex), !is.na(.data$CC))
  for (v in c("Group", "Sex", "CC", "Batch", "AnimalNum", "CageEpochID")) {
    d[[v]] <- droplevels(d[[v]])
  }
  if (nrow(d) < 40L || nlevels(d$Sex) < 2L || nlevels(d$CC) < 2L) {
    return(list(test = tibble::tibble(
      Domain = domain, term = "Group:Sex:CageChange", model = "all",
      test_statistic = NA_real_, df_num = NA_real_, df_den = NA_real_, raw_p = NA_real_,
      model_formula = NA_character_, model_status = "insufficient_data"),
      aliasing = tibble::tibble()))
  }

  alias <- dplyr::bind_rows(
    mmm_rfid_aliasing_audit(~ Group * Sex * CC, d, paste0(domain, " :: Group*Sex*CC")),
    mmm_rfid_aliasing_audit(~ Group * Sex * CC + Batch, d,
                            paste0(domain, " :: Group*Sex*CC+Batch"))
  ) %>% mutate(Domain = domain)

  one <- function(fml, label) {
    fit <- tryCatch(suppressMessages(suppressWarnings(
      lmerTest::lmer(fml, data = d, REML = TRUE))), error = function(e) e)
    if (inherits(fit, "error")) {
      return(tibble::tibble(Domain = domain, term = "Group:Sex:CageChange", model = label,
                            test_statistic = NA_real_, df_num = NA_real_, df_den = NA_real_,
                            raw_p = NA_real_, model_formula = paste(deparse(fml), collapse = " "),
                            model_status = conditionMessage(fit)))
    }
    av <- tryCatch(as.data.frame(stats::anova(fit, type = 2)), error = function(e) NULL)
    tt <- mmm_rfid_anova_row(av, c("Group", "Sex", "CC"))
    if (is.null(av) || is.na(tt)) {
      return(tibble::tibble(Domain = domain, term = "Group:Sex:CageChange", model = label,
                            test_statistic = NA_real_, df_num = NA_real_, df_den = NA_real_,
                            raw_p = NA_real_, model_formula = paste(deparse(fml), collapse = " "),
                            model_status = "three_way_term_not_estimable"))
    }
    tibble::tibble(Domain = domain, term = "Group:Sex:CageChange", model = label,
                   test_statistic = av[tt, "F value"], df_num = av[tt, "NumDF"],
                   df_den = av[tt, "DenDF"], raw_p = av[tt, "Pr(>F)"],
                   model_formula = paste(deparse(fml), collapse = " "),
                   model_status = if (lme4::isSingular(fit, tol = 1e-5))
                     "fitted_singular_variance_component" else "fitted")
  }

  test <- dplyr::bind_rows(
    one(DomainScore ~ Group * Sex * CC + (1 | AnimalNum) + (1 | CageEpochID),
        "combined_nobatch"),
    one(DomainScore ~ Batch + Group * CC + Sex:CC + Group:Sex + Group:Sex:CC +
          (1 | AnimalNum) + (1 | CageEpochID), "batch_absorbed")
  ) %>%
    mutate(scientific_question = paste(
      "The three-way Group x Sex x CageChange interaction is the sex-differential",
      "ADAPTATION question. The Sex MAIN effect is not reported and not claimed:",
      "Sex is perfectly nested in Batch in this experiment."),
      structural_confound = MMM_RFID_LONGITUDINAL_CONFOUND_GUARD)

  list(test = test, aliasing = alias)
}

# ================================================================
# PER-CAGE-CHANGE and EARLY/LATE decomposition of the two orthogonal contrasts
# ================================================================
# The 6-df Group x CageChange omnibus is diffuse and cannot show WHERE a
# difference sits. These functions estimate the manipulation contrast and the
# phenotype contrast separately at each cage change, and in predeclared early
# (CC1,CC2) and late (CC3,CC4) windows, plus the 1-df "emergence" contrast
# late - early.
#
# STATUS DISCIPLINE. Only the per-cage-change estimates and the emergence test are
# treated as planned decompositions of the primary trajectory question. The
# early/late window contrasts themselves are marked EXPLORATORY, because the
# choice of which window to highlight was made AFTER inspecting per-cage-change
# estimates. mmm_rfid_exploratory_path_accounting() records the full set of tests
# examined so the honest q can be computed and shipped rather than described.

MMM_RFID_WINDOW_SETS <- list(
  early = c("CC1", "CC2"),
  late  = c("CC3", "CC4")
)

#' Manipulation and phenotype contrasts at each cage change, and early/late/emergence.
mmm_rfid_window_contrasts <- function(scores, stratum_label = "pooled") {
  domains <- intersect(MMM_RFID_CORE_DOMAINS, unique(as.character(scores$Domain)))
  purrr::map_dfr(domains, function(dm) {
    d <- scores %>%
      filter(.data$Domain == dm, is.finite(.data$DomainScore)) %>%
      mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
             CC = factor(as.character(.data$CageChangeLabel)),
             Batch = factor(as.character(.data$Batch)),
             AnimalNum = factor(as.character(.data$AnimalNum)),
             CageEpochID = factor(as.character(.data$CageEpochID))) %>%
      filter(!is.na(.data$Group), !is.na(.data$CC))
    for (v in c("Group", "CC", "Batch", "AnimalNum", "CageEpochID")) d[[v]] <- droplevels(d[[v]])
    if (nrow(d) < 40L || nlevels(d$CC) < 2L || nlevels(d$Group) < 3L ||
        !requireNamespace("lmerTest", quietly = TRUE) ||
        !requireNamespace("emmeans", quietly = TRUE)) return(tibble::tibble())

    rhs <- if (nlevels(d$Batch) > 1L) "Group * CC + Batch" else "Group * CC"
    fml <- stats::as.formula(paste("DomainScore ~", rhs,
                                   "+ (1 | AnimalNum) + (1 | CageEpochID)"))
    fit <- tryCatch(suppressMessages(suppressWarnings(
      lmerTest::lmer(fml, data = d, REML = TRUE))), error = function(e) NULL)
    if (is.null(fit)) return(tibble::tibble())
    em <- tryCatch(suppressMessages(emmeans::emmeans(fit, ~ Group * CC)),
                   error = function(e) NULL)
    if (is.null(em)) return(tibble::tibble())
    grid <- as.data.frame(em)[, c("Group", "CC")]
    grid$Group <- as.character(grid$Group); grid$CC <- as.character(grid$CC)
    ccl <- unique(grid$CC)

    wvec <- function(gw, cw) {
      vapply(seq_len(nrow(grid)), function(i) {
        unname(gw[grid$Group[i]]) * unname(cw[grid$CC[i]])
      }, numeric(1))
    }
    ccw <- function(keep) {
      k <- intersect(keep, ccl)
      w <- setNames(rep(0, length(ccl)), ccl)
      if (length(k)) w[k] <- 1 / length(k)
      w
    }
    specs <- list(); meta <- character(0)
    for (cn in names(MMM_RFID_ORTHOGONAL_CONTRASTS)) {
      gw <- setNames(MMM_RFID_ORTHOGONAL_CONTRASTS[[cn]], MMM_RFID_GROUP_LEVELS)
      for (cc in ccl) {
        specs[[paste0(cn, "@", cc)]] <- wvec(gw, ccw(cc))
        meta <- c(meta, "per_cage_change")
      }
      e <- ccw(MMM_RFID_WINDOW_SETS$early); l <- ccw(MMM_RFID_WINDOW_SETS$late)
      if (sum(e) > 0 && sum(l) > 0) {
        specs[[paste0(cn, "@early")]] <- wvec(gw, e); meta <- c(meta, "window_early")
        specs[[paste0(cn, "@late")]]  <- wvec(gw, l); meta <- c(meta, "window_late")
        specs[[paste0(cn, "@emergence")]] <- wvec(gw, l) - wvec(gw, e)
        meta <- c(meta, "emergence_late_minus_early")
      }
    }
    ct <- tryCatch(as.data.frame(suppressMessages(emmeans::contrast(
      em, specs, adjust = "none", infer = c(TRUE, TRUE)))), error = function(e) NULL)
    if (is.null(ct)) return(tibble::tibble())
    lbl <- as.character(ct$contrast)
    tibble::tibble(
      Domain = dm, stratum = stratum_label, contrast_label = lbl,
      contrast = sub("@.*$", "", lbl), window = sub("^.*@", "", lbl),
      contrast_kind = meta[match(lbl, names(specs))],
      estimate = ct$estimate, SE = ct$SE, df = ct$df,
      ci_low = ct$lower.CL, ci_high = ct$upper.CL, raw_p = ct$p.value,
      identification = ifelse(sub("@.*$", "", lbl) == "SUS_vs_RES",
                              "within_cage", "between_cage_6_control_cages"),
      analysis_status = ifelse(meta[match(lbl, names(specs))] == "per_cage_change",
                               "PLANNED_decomposition", "EXPLORATORY_window_chosen_after_inspection"),
      model_formula = paste(deparse(fml), collapse = " "),
      contrast_meaning = unname(MMM_RFID_ORTHOGONAL_META[sub("@.*$", "", lbl)]))
  })
}

#' Honest multiplicity across the full set of window contrasts actually examined.
#'
#' Ships BOTH the narrow per-window family and the wide family over every window
#' contrast computed, so a reader cannot be shown only the smaller q.
mmm_rfid_exploratory_path_accounting <- function(window_contrasts) {
  if (nrow(window_contrasts) == 0L) return(window_contrasts)
  window_contrasts %>%
    group_by(.data$stratum, .data$contrast, .data$window) %>%
    mutate(q_narrow_within_window = mmm_rfid_bh(
      .data$raw_p, dplyr::n(), paste0("narrow__", dplyr::first(.data$window))),
      n_narrow_family = dplyr::n()) %>%
    ungroup() %>%
    group_by(.data$stratum, .data$contrast) %>%
    # The WIDE family counts only the tests that represent a CHOICE: the window
    # contrasts (early / late / emergence). The per-cage-change estimates are a
    # planned descriptive decomposition of the trajectory, not separate hypotheses,
    # so including them would over-correct.
    mutate(.is_choice = .data$contrast_kind != "per_cage_change",
           n_wide_family = sum(.data$.is_choice),
           q_wide_all_windows = {
             qq <- rep(NA_real_, dplyr::n())
             ch <- .data$.is_choice
             if (any(ch)) qq[ch] <- mmm_rfid_bh(.data$raw_p[ch], sum(ch),
                                               paste0("wide__", dplyr::first(.data$contrast)))
             qq
           }) %>%
    ungroup() %>%
    select(-".is_choice") %>%
    mutate(multiplicity_note = paste(
      "q_narrow_within_window corrects across the 4 domains WITHIN one window and",
      "is only honest if that window was declared in advance.",
      "q_wide_all_windows corrects across EVERY window contrast computed for this",
      "contrast type, which is the defensible default for any window that was",
      "chosen after inspecting the per-cage-change estimates. Quote the wide q",
      "unless the window is prespecified."))
}
