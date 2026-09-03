# ================================================================
# First-night (CC1, first Active 12 h) domain analysis
# MMMSociability
# ================================================================
# ONE canonical implementation of the audited first-night panel. Stage 14
# previously contained three separate row-count window constructions and scored
# the CC1 heatmap through a generic equal-weight inventory scorer, which does not
# reproduce the declared domain formulas. This helper replaces all of that.
#
# Contract (audit branch: ec1edda / 2075739 / edc49e0)
# ---------------------------------------------------
# Window   first cage change, first Active block, 18:30 inclusive -> 06:30
#          exclusive, exactly 12 h, per-session clock anchor. Delegated to
#          mmm_select_first_night_window(); never a row count.
# Rows     exactly FIVE displayed domains, fixed across resolutions. No
#          HMM-derived row: first-night mean_dwell_minutes is not reproducible
#          across equally-good 10-min HMM partitions (r 0.08 between two fits at
#          identical logLik), so no HMM quantity is displayed here at all.
# Scaling  each raw contributor z-scored WITHIN SEX, separately per resolution.
#          Never by Group.
# Missing  strict completeness: a domain is finite only if ALL contributors its
#          declared formula requires are finite. No na.rm = TRUE, no
#          coalesce(missing, 0). No animal is ever scored on a different formula
#          from its peers.
# Temporal RMSSD/ACF1 use only truly adjacent expected slots
#          (diff(target_slot) == 1); missing bins are never bridged and never
#          interpolated.
# Model    lm(DomainScore ~ Group * Sex), model-based contrasts within Sex.
# FDR      5 domains x 3 contrasts = 15 tests per Sex; interactions 5 tests.
#
# 10 min is the primary resolution for THIS panel so its temporal resolution and
# clock window match canonical Stage 09, which owns the first-12-h prospective
# question. It is NOT claimed that these Stage 14 domain endpoints were
# historically prespecified at 10 min. Stage 14's own global 5-min backbone is
# unchanged; this panel loads its own Stage 01 input per resolution.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
})

for (.dep in c("phase_classification_helpers.R", "first_night_window_helpers.R")) {
  if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) source_mmm_helper(.dep)
}

MMM_FIRST_NIGHT_PRIMARY_BIN_LEVEL <- "10min_based"
MMM_FIRST_NIGHT_SENSITIVITY_BIN_LEVEL <- "5min_based"

# Displayed rows, fixed. Order is presentation order; Psychomotor activation is
# last so it reads as the locomotion reference for the rows above it.
MMM_FIRST_NIGHT_DISPLAYED_DOMAINS <- c(
  "Behavioral volatility / fragmentation",
  "Behavioral flexibility / predictability",
  "Social spatial organization",
  "Active-phase adaptation / exploration",
  "Psychomotor activation"
)

# Extra raw-RFID candidates considered and pruned by the documented redundancy
# audit. Retained ONLY for the transparency multiplicity sensitivity; never
# displayed. #8 = #2 + 0.5*(Ea - Ma), #9 = #3 + 0.5*(Pm - Pa), #10 = #1 - Pm.
MMM_FIRST_NIGHT_AUDIT_ONLY_DOMAINS <- c(
  "Early active spatial flexibility",
  "Early social engagement",
  "Early social withdrawal"
)

MMM_FIRST_NIGHT_RAW_FEATURES <- c(
  "Movement_mean", "Movement_rmssd", "Movement_acf1",
  "Entropy_mean", "Entropy_rmssd", "Entropy_acf1",
  "Proximity_mean", "Proximity_rmssd", "Proximity_acf1"
)

# Declared contributor sets. Strict completeness is evaluated against these.
MMM_FIRST_NIGHT_DOMAIN_CONTRIBUTORS <- list(
  "Psychomotor activation" = "Movement_mean_z",
  "Behavioral flexibility / predictability" = c("Entropy_mean_z", "Entropy_rmssd_z", "Entropy_acf1_z"),
  "Social spatial organization" = c("Proximity_mean_z", "Proximity_acf1_z", "Proximity_rmssd_z"),
  "Behavioral volatility / fragmentation" = c("Movement_rmssd_z", "Entropy_rmssd_z", "Proximity_rmssd_z"),
  "Active-phase adaptation / exploration" = c("Movement_mean_z", "Entropy_mean_z", "Proximity_mean_z",
                                              "Movement_acf1_z", "Entropy_acf1_z"),
  "Early active spatial flexibility" = c("Entropy_mean_z", "Entropy_rmssd_z",
                                         "Movement_acf1_z", "Entropy_acf1_z"),
  "Early social engagement" = c("Proximity_mean_z", "Proximity_rmssd_z"),
  "Early social withdrawal" = c("Movement_mean_z", "Proximity_mean_z")
)

MMM_FIRST_NIGHT_DOMAIN_FORMULAS <- c(
  "Psychomotor activation" = "Movement_mean_z",
  "Behavioral flexibility / predictability" = "0.5*(Entropy_mean_z + Entropy_rmssd_z) - Entropy_acf1_z",
  "Social spatial organization" = "0.5*(Proximity_mean_z + Proximity_acf1_z) - Proximity_rmssd_z",
  "Behavioral volatility / fragmentation" = "(Movement_rmssd_z + Entropy_rmssd_z + Proximity_rmssd_z)/3",
  "Active-phase adaptation / exploration" = "(Movement_mean_z + Entropy_mean_z + Proximity_mean_z)/3 - (Movement_acf1_z + Entropy_acf1_z)/2",
  "Early active spatial flexibility" = "0.5*(Entropy_mean_z + Entropy_rmssd_z) - 0.5*(Movement_acf1_z + Entropy_acf1_z)",
  "Early social engagement" = "Proximity_mean_z - Proximity_rmssd_z",
  "Early social withdrawal" = "Movement_mean_z - Proximity_mean_z"
)

# ---------------------------------------------------------------- adjacency-aware
# Minimum valid ADJACENT PAIR counts. The historical code required >= 3 finite
# values for RMSSD and >= 4 for ACF1, i.e. >= 2 and >= 3 consecutive differences
# when nothing was missing; those thresholds are preserved here but are now
# counted on genuinely adjacent slot pairs rather than on finite values.
MMM_FIRST_NIGHT_MIN_PAIRS_RMSSD <- 2L
MMM_FIRST_NIGHT_MIN_PAIRS_ACF1 <- 3L

#' Indices of adjacent, both-finite slot pairs.
mmm_adjacent_pairs <- function(value, slot) {
  o <- order(slot)
  value <- value[o]; slot <- slot[o]
  n <- length(value)
  if (n < 2L) return(list(x = numeric(0), y = numeric(0), n_pairs = 0L))
  keep <- (diff(slot) == 1L) & is.finite(value[-n]) & is.finite(value[-1L])
  list(x = value[-n][keep], y = value[-1L][keep], n_pairs = sum(keep))
}

#' RMSSD over truly adjacent expected slots. Missing bins are NOT bridged.
mmm_rmssd_adjacent <- function(value, slot, min_pairs = MMM_FIRST_NIGHT_MIN_PAIRS_RMSSD) {
  p <- mmm_adjacent_pairs(value, slot)
  if (p$n_pairs < min_pairs) return(NA_real_)
  sqrt(mean((p$y - p$x)^2))
}

#' Lag-1 autocorrelation over truly adjacent expected slots.
mmm_acf1_adjacent <- function(value, slot, min_pairs = MMM_FIRST_NIGHT_MIN_PAIRS_ACF1) {
  p <- mmm_adjacent_pairs(value, slot)
  if (p$n_pairs < min_pairs) return(NA_real_)
  if (!is.finite(stats::sd(p$x)) || !is.finite(stats::sd(p$y)) ||
      stats::sd(p$x) == 0 || stats::sd(p$y) == 0) return(NA_real_)
  suppressWarnings(stats::cor(p$x, p$y))
}

#' Count of valid adjacent pairs, exported per animal per metric for QC.
mmm_n_adjacent_pairs <- function(value, slot) mmm_adjacent_pairs(value, slot)$n_pairs

# ------------------------------------------------------------------ z within Sex
#' Standardize within Sex, returning the parameters used so they can be audited.
mmm_first_night_standardize_within_sex <- function(features, feature_cols) {
  params <- features %>%
    group_by(Sex) %>%
    summarise(across(all_of(feature_cols),
                     list(mean = ~mean(.x, na.rm = TRUE), sd = ~stats::sd(.x, na.rm = TRUE),
                          n_finite = ~sum(is.finite(.x))),
                     .names = "{.col}__{.fn}"),
              .groups = "drop")
  scaled <- features %>%
    group_by(Sex) %>%
    mutate(across(all_of(feature_cols),
                  ~{
                    s <- stats::sd(.x, na.rm = TRUE)
                    m <- mean(.x, na.rm = TRUE)
                    # A zero-variance contributor cannot be standardized; return
                    # NA rather than 0 so strict completeness catches it instead
                    # of silently contributing a constant.
                    if (!is.finite(s) || s == 0) rep(NA_real_, length(.x)) else (.x - m) / s
                  },
                  .names = "{.col}_z")) %>%
    ungroup()
  list(scaled = scaled, parameters = params)
}

# --------------------------------------------------------------------- scoring
#' Score one domain with strict contributor completeness.
mmm_first_night_score_domain <- function(z, domain) {
  need <- MMM_FIRST_NIGHT_DOMAIN_CONTRIBUTORS[[domain]]
  if (is.null(need)) stop("Unknown first-night domain: ", domain, call. = FALSE)
  missing_cols <- setdiff(need, names(z))
  if (length(missing_cols) > 0L) {
    stop("First-night domain '", domain, "' is missing contributor column(s): ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  mat <- as.matrix(z[, need, drop = FALSE])
  complete <- rowSums(!is.finite(mat)) == 0L

  g <- function(nm) z[[nm]]
  raw <- switch(
    domain,
    "Psychomotor activation" = g("Movement_mean_z"),
    "Behavioral flexibility / predictability" =
      0.5 * (g("Entropy_mean_z") + g("Entropy_rmssd_z")) - g("Entropy_acf1_z"),
    "Social spatial organization" =
      0.5 * (g("Proximity_mean_z") + g("Proximity_acf1_z")) - g("Proximity_rmssd_z"),
    "Behavioral volatility / fragmentation" =
      (g("Movement_rmssd_z") + g("Entropy_rmssd_z") + g("Proximity_rmssd_z")) / 3,
    "Active-phase adaptation / exploration" =
      (g("Movement_mean_z") + g("Entropy_mean_z") + g("Proximity_mean_z")) / 3 -
        (g("Movement_acf1_z") + g("Entropy_acf1_z")) / 2,
    "Early active spatial flexibility" =
      0.5 * (g("Entropy_mean_z") + g("Entropy_rmssd_z")) -
        0.5 * (g("Movement_acf1_z") + g("Entropy_acf1_z")),
    "Early social engagement" = g("Proximity_mean_z") - g("Proximity_rmssd_z"),
    "Early social withdrawal" = g("Movement_mean_z") - g("Proximity_mean_z"),
    stop("Unhandled first-night domain: ", domain, call. = FALSE)
  )
  tibble(
    AnimalNum = z$AnimalNum, Group = z$Group, Sex = z$Sex,
    Domain = domain,
    DomainScore = ifelse(complete, raw, NA_real_),
    required_contributor_count = length(need),
    available_contributor_count = rowSums(is.finite(mat)),
    complete_contributors = complete,
    missing_contributors = apply(mat, 1, function(r) paste(need[!is.finite(r)], collapse = "|")),
    score_formula = unname(MMM_FIRST_NIGHT_DOMAIN_FORMULAS[[domain]]),
    contributor_policy = "strict_complete_contributors_required"
  )
}

# ------------------------------------------------------------------- inference
#' Model-based Group contrasts within Sex plus the formal Group x Sex test.
mmm_first_night_domain_inference <- function(scores, domain,
                                             group_levels = c("CON", "RES", "SUS"),
                                             sex_levels = c("Female", "Male")) {
  if (!requireNamespace("emmeans", quietly = TRUE)) {
    stop("emmeans is required for first-night inference.", call. = FALSE)
  }
  d <- scores %>%
    filter(.data$Domain == domain, is.finite(.data$DomainScore)) %>%
    transmute(AnimalNum = as.character(.data$AnimalNum),
              Group = factor(as.character(.data$Group), levels = group_levels),
              Sex = factor(as.character(.data$Sex), levels = sex_levels),
              DomainScore = as.numeric(.data$DomainScore)) %>%
    filter(!is.na(.data$Group), !is.na(.data$Sex))
  if (anyDuplicated(d$AnimalNum) > 0L) {
    stop("First-night inference requires exactly one value per animal for domain '", domain,
         "'; duplicates found.", call. = FALSE)
  }
  model_formula <- "DomainScore ~ Group * Sex"
  warns <- character()
  fit <- tryCatch(
    withCallingHandlers(stats::lm(stats::as.formula(model_formula), data = d),
                        warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) e
  )
  contrast_vectors <- list("RES-CON" = c(-1, 1, 0), "SUS-CON" = c(-1, 0, 1), "SUS-RES" = c(0, -1, 1))
  empty <- tidyr::crossing(Sex = sex_levels, contrast = names(contrast_vectors)) %>%
    mutate(Domain = domain, estimate = NA_real_, SE = NA_real_, df = NA_real_,
           ci_low = NA_real_, ci_high = NA_real_, t_ratio = NA_real_, raw_p = NA_real_,
           n_ref = NA_integer_, n_comp = NA_integer_, mean_ref = NA_real_, mean_comp = NA_real_,
           hedges_g = NA_real_, model_status = if (inherits(fit, "error")) conditionMessage(fit) else "not_estimable")
  if (inherits(fit, "error")) {
    return(list(contrasts = empty, interaction = tibble(
      Domain = domain, term = "Group:Sex", df_num = NA_real_, df_den = NA_real_,
      F_value = NA_real_, raw_p = NA_real_, model_status = conditionMessage(fit))))
  }
  emm <- emmeans::emmeans(fit, ~ Group | Sex)
  ct <- as.data.frame(emmeans::contrast(emm, method = contrast_vectors, adjust = "none",
                                        infer = c(TRUE, TRUE))) %>%
    transmute(Sex = as.character(.data$Sex), contrast = as.character(.data$contrast),
              estimate = .data$estimate, SE = .data$SE, df = .data$df,
              ci_low = .data$lower.CL, ci_high = .data$upper.CL,
              t_ratio = .data$t.ratio, raw_p = .data$p.value)
  desc <- map_dfr(sex_levels, function(sx) {
    map_dfr(names(contrast_vectors), function(cn) {
      ref <- sub("^.*-", "", cn); comp <- sub("-.*$", "", cn)
      x <- d$DomainScore[as.character(d$Sex) == sx & as.character(d$Group) == ref]
      y <- d$DomainScore[as.character(d$Sex) == sx & as.character(d$Group) == comp]
      tibble(Sex = sx, contrast = cn, group_ref = ref, group_comp = comp,
             n_ref = sum(is.finite(x)), n_comp = sum(is.finite(y)),
             mean_ref = if (any(is.finite(x))) mean(x) else NA_real_,
             mean_comp = if (any(is.finite(y))) mean(y) else NA_real_,
             # Same orientation as the model contrast: comp - ref.
             hedges_g = hmm_hedges_g(x, y))
    })
  })
  contrasts <- desc %>%
    left_join(ct, by = c("Sex", "contrast")) %>%
    mutate(Domain = domain,
           contrast_orientation = "comp - ref",
           ci_method = "emmeans t-based 95% CI on lm residual df",
           model_formula = model_formula, model_engine = "stats::lm + emmeans",
           model_status = "fitted",
           model_warnings = paste(unique(warns), collapse = " | "))
  av <- as.data.frame(stats::anova(fit))
  interaction <- if (!"Group:Sex" %in% rownames(av)) {
    tibble(Domain = domain, term = "Group:Sex", df_num = NA_real_, df_den = NA_real_,
           F_value = NA_real_, raw_p = NA_real_, model_status = "interaction_not_estimable")
  } else {
    tibble(Domain = domain, term = "Group:Sex",
           df_num = av["Group:Sex", "Df"], df_den = av["Residuals", "Df"],
           F_value = av["Group:Sex", "F value"], raw_p = av["Group:Sex", "Pr(>F)"],
           model_status = "fitted")
  }
  list(contrasts = contrasts, interaction = interaction)
}

#' Assert model estimate and Hedges g agree in sign for the same contrast.
mmm_assert_effect_sign_agreement <- function(contrasts, tol = 1e-8, label = "first-night contrasts") {
  chk <- contrasts %>%
    filter(is.finite(.data$estimate), is.finite(.data$hedges_g),
           abs(.data$estimate) > tol, abs(.data$hedges_g) > tol)
  bad <- chk %>% filter(sign(.data$estimate) != sign(.data$hedges_g))
  if (nrow(bad) > 0L) {
    stop(label, ": model estimate and Hedges g disagree in sign for ", nrow(bad),
         " contrast(s), e.g. ", bad$Domain[1], " / ", bad$Sex[1], " / ", bad$contrast[1],
         " (estimate ", signif(bad$estimate[1], 3), ", g ", signif(bad$hedges_g[1], 3),
         "). Both must be oriented comp - ref.", call. = FALSE)
  }
  invisible(TRUE)
}

#' BH adjustment with an EXPLICIT declared family size.
#'
#' p.adjust() shrinks the family when values are NA, which would silently
#' relax the correction. The declared n is used instead and any non-estimable
#' cell is reported.
mmm_first_night_bh <- function(p, declared_n, family_id) {
  n_estimable <- sum(is.finite(p))
  if (n_estimable != declared_n) {
    warning("FDR family ", family_id, " declares n = ", declared_n,
            " but only ", n_estimable, " test(s) are estimable; adjusting against the declared n.",
            call. = FALSE)
  }
  q <- rep(NA_real_, length(p))
  ok <- is.finite(p)
  if (any(ok)) q[ok] <- stats::p.adjust(p[ok], method = "BH", n = declared_n)
  q
}
