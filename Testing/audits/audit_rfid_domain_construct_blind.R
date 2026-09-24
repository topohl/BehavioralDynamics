# ================================================================
# PHENOTYPE-BLIND construct audit of the four core raw-RFID domains
# MMMSociability
# ================================================================
# This audit runs BEFORE any group inference is interpreted and uses NO Group,
# no CombZ, and no phenotype label of any kind. Group/Sex columns are dropped from
# the working table at STEP 1, except Sex, which is retained because it is the
# declared STANDARDIZATION stratum (not an outcome).
#
# It answers:
#   1. algebraic overlap of the coefficient vectors
#   2. empirical Pearson/Spearman correlations among domain scores
#   3. pooled and sex-stratified redundancy
#   4. correlation with Movement_mean (locomotion dominance)
#   5. 10-min vs 5-min score stability
#   6. missing-contributor frequency
#   7. variance
#   8. extreme-value / influence behaviour
#   9. current vs equal-weight formula stability
#
# Flagging thresholds (declared before looking):
#   |rho| >= 0.70  meaningful overlap
#   |rho| >= 0.85  high redundancy
#   |rho| >= 0.95  near duplication
# A flag is REPORTED, never acted on automatically: no domain is dropped by this
# script, and no formula is chosen by it.
#
# Reads Stage 28 outputs; writes only into its own audit directory.
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

ROOT <- mmm_project_root()
stage_tables <- function(bin) file.path(
  behavior_stage_dir(ROOT, "28", "rfid_behavioral_domains", sub("_based$", "", bin)), "tables")
OUT <- mmm_behavior_output_active_root("rfid_construct_audit", project_root = ROOT)
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

FLAG <- function(r) dplyr::case_when(
  !is.finite(r) ~ "not_estimable",
  abs(r) >= 0.95 ~ "near_duplication",
  abs(r) >= 0.85 ~ "high_redundancy",
  abs(r) >= 0.70 ~ "meaningful_overlap",
  TRUE ~ "acceptable")

read_scores <- function(bin) {
  f <- file.path(stage_tables(bin), "first_night_domain_scores.csv")
  if (!file.exists(f)) stop("Missing Stage 28 first-night scores for ", bin,
                            ". Run Analysis/28_rfid_behavioral_domains.R first.", call. = FALSE)
  read_csv(f, col_types = cols(AnimalNum = col_character(), .default = col_guess()),
           progress = FALSE)
}
read_features <- function(bin) {
  f <- file.path(stage_tables(bin), "acute_window_raw_features.csv")
  read_csv(f, col_types = cols(AnimalNum = col_character(), .default = col_guess()),
           progress = FALSE) %>% filter(.data$CageChangeLabel == "CC1")
}
read_sens <- function(bin) {
  f <- file.path(stage_tables(bin), "first_night_equal_weight_sensitivity_scores.csv")
  read_csv(f, col_types = cols(AnimalNum = col_character(), .default = col_guess()),
           progress = FALSE)
}

wide_scores <- function(scores) {
  scores %>%
    select("AnimalNum", "Sex", "Domain", "DomainScore") %>%
    pivot_wider(names_from = "Domain", values_from = "DomainScore")
}

DOM <- MMM_RFID_CORE_DOMAINS
written <- character(0)
w <- function(x, nm) { write_csv(x, file.path(OUT, nm)); written <<- c(written, nm); invisible(nm) }

message("PHENOTYPE-BLIND construct audit of the four core RFID domains")

# --------------------------------------------------- 1. algebraic overlap
M <- mmm_rfid_coefficient_matrix()
pairs <- t(utils::combn(rownames(M), 2))
alg <- purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
  a <- M[pairs[i, 1], ]; b <- M[pairs[i, 2], ]
  shared <- names(a)[a != 0 & b != 0]
  na <- sqrt(sum(a^2)); nb <- sqrt(sum(b^2))
  tibble(domain_a = pairs[i, 1], domain_b = pairs[i, 2],
         n_shared_contributors = length(shared),
         shared_contributors = if (length(shared)) paste(shared, collapse = "; ") else NA_character_,
         coefficient_cosine = if (na > 0 && nb > 0) sum(a * b) / (na * nb) else NA_real_,
         exact_algebraic_identity = isTRUE(all.equal(as.numeric(a), as.numeric(b))))
}) %>% mutate(coefficient_overlap_flag = FLAG(.data$coefficient_cosine))
w(as_tibble(M, rownames = "Domain"), "domain_coefficient_matrix.csv")
w(alg, "algebraic_overlap.csv")

# --------------------------- 2-3. empirical redundancy, pooled + per Sex
redundancy <- purrr::map_dfr(c("10min_based", "5min_based"), function(bin) {
  sc <- read_scores(bin)
  wd <- wide_scores(sc)
  strata <- list(pooled = wd, Female = filter(wd, .data$Sex == "Female"),
                 Male = filter(wd, .data$Sex == "Male"))
  purrr::map_dfr(names(strata), function(st) {
    X <- as.matrix(strata[[st]][, DOM, drop = FALSE])
    purrr::map_dfr(seq_len(nrow(pairs)), function(i) {
      a <- X[, pairs[i, 1]]; b <- X[, pairs[i, 2]]
      ok <- is.finite(a) & is.finite(b)
      tibble(bin_level = bin, stratum = st, domain_a = pairs[i, 1], domain_b = pairs[i, 2],
             n_complete_pairs = sum(ok),
             pearson_r = if (sum(ok) > 2) stats::cor(a[ok], b[ok]) else NA_real_,
             spearman_rho = if (sum(ok) > 2) stats::cor(a[ok], b[ok], method = "spearman") else NA_real_)
    })
  })
}) %>% mutate(pearson_flag = FLAG(.data$pearson_r), spearman_flag = FLAG(.data$spearman_rho))
w(redundancy, "empirical_redundancy_matrix.csv")

# ------------------------------------- 4. locomotion dominance + 7. variance
loco <- purrr::map_dfr(c("10min_based", "5min_based"), function(bin) {
  sc <- read_scores(bin); ft <- read_features(bin)
  wd <- wide_scores(sc) %>% left_join(ft %>% select("AnimalNum", "Movement_mean"), by = "AnimalNum")
  strata <- list(pooled = wd, Female = filter(wd, .data$Sex == "Female"),
                 Male = filter(wd, .data$Sex == "Male"))
  purrr::map_dfr(names(strata), function(st) {
    z <- strata[[st]]
    purrr::map_dfr(DOM, function(dm) {
      a <- z[[dm]]; m <- z$Movement_mean; ok <- is.finite(a) & is.finite(m)
      tibble(bin_level = bin, stratum = st, Domain = dm, n = sum(ok),
             pearson_vs_movement_mean = if (sum(ok) > 2) stats::cor(a[ok], m[ok]) else NA_real_,
             spearman_vs_movement_mean = if (sum(ok) > 2) stats::cor(a[ok], m[ok], method = "spearman") else NA_real_,
             score_variance = if (sum(is.finite(a)) > 1) stats::var(a[is.finite(a)]) else NA_real_,
             score_sd = if (sum(is.finite(a)) > 1) stats::sd(a[is.finite(a)]) else NA_real_)
    })
  })
}) %>%
  mutate(
    # "Movement output" IS Movement_mean_z, a strictly monotone within-Sex
    # transform of Movement_mean, so its within-Sex Spearman with Movement_mean is
    # 1.000 BY CONSTRUCTION. That is the definition of the domain, not a redundancy
    # finding, and it must not be allowed to trip the redundancy verdict. It is the
    # declared locomotion REFERENCE against which the other three are judged.
    is_locomotion_reference = .data$Domain == "Movement output",
    locomotion_dominance_flag = if_else(
      .data$is_locomotion_reference, "by_construction_locomotion_reference",
      FLAG(.data$spearman_vs_movement_mean)))
w(loco, "locomotion_dominance_and_variance.csv")

# --------------------------------------- 5. 10-min vs 5-min score stability
s10 <- wide_scores(read_scores("10min_based"))
s05 <- wide_scores(read_scores("5min_based"))
stab <- purrr::map_dfr(DOM, function(dm) {
  j <- inner_join(s10 %>% select("AnimalNum", "Sex", a = all_of(dm)),
                  s05 %>% select("AnimalNum", b = all_of(dm)), by = "AnimalNum")
  ok <- is.finite(j$a) & is.finite(j$b)
  tibble(Domain = dm, n = sum(ok),
         pearson_r_10min_vs_5min = if (sum(ok) > 2) stats::cor(j$a[ok], j$b[ok]) else NA_real_,
         spearman_rho_10min_vs_5min = if (sum(ok) > 2) stats::cor(j$a[ok], j$b[ok], method = "spearman") else NA_real_,
         max_abs_rank_shift = if (sum(ok) > 2) max(abs(rank(j$a[ok]) - rank(j$b[ok]))) else NA_real_)
}) %>%
  mutate(resolution_caveat = paste(
    "RMSSD and ACF1 are LAG-IN-BIN quantities, so the 5-min score is a DIFFERENT",
    "temporal construct, not a replication of the 10-min construct. Low agreement",
    "here is partly expected for every domain except Movement output, which is",
    "built from the lag-free Movement_mean only."))
w(stab, "cross_resolution_score_stability.csv")

# --------------------------------- 6. missing-contributor frequency
miss <- purrr::map_dfr(c("10min_based", "5min_based"), function(bin) {
  read_scores(bin) %>%
    group_by(bin_level = bin, .data$Domain, .data$Sex) %>%
    summarise(n_animals = dplyr::n(),
              n_scored = sum(is.finite(.data$DomainScore)),
              n_missing = sum(!is.finite(.data$DomainScore)),
              missing_fraction = mean(!is.finite(.data$DomainScore)),
              missing_contributor_names = paste(sort(unique(
                .data$missing_contributors[nzchar(.data$missing_contributors)])), collapse = " | "),
              .groups = "drop")
})
w(miss, "missing_contributor_frequency.csv")

# Which animals, and is the loss structural?
miss_animals <- read_scores("10min_based") %>%
  filter(!is.finite(.data$DomainScore)) %>%
  select("AnimalNum", "Sex", "Domain", "missing_contributors", "CageEpochID") %>%
  mutate(note = paste(
    "Structural, not random: an animal housed alone has no cage-mate, so",
    "ProximityFraction = ProximitySeconds / dyadic_observation_seconds is undefined.",
    "It is correctly NA and must never be imputed as 0."))
w(miss_animals, "unscored_animals.csv")

# ------------------------------- 8. extreme values / influence behaviour
infl <- purrr::map_dfr(c("10min_based", "5min_based"), function(bin) {
  wd <- wide_scores(read_scores(bin))
  purrr::map_dfr(c("Female", "Male"), function(sx) {
    z <- filter(wd, .data$Sex == sx)
    purrr::map_dfr(DOM, function(dm) {
      a <- z[[dm]]; a <- a[is.finite(a)]
      if (length(a) < 4) return(tibble())
      md <- stats::median(a); mad <- stats::mad(a)
      rz <- if (mad > 0) (a - md) / (1.4826 * mad) else rep(NA_real_, length(a))
      q <- stats::quantile(a, c(0.25, 0.75))
      iqr <- q[2] - q[1]
      tibble(bin_level = bin, Sex = sx, Domain = dm, n = length(a),
             min = min(a), max = max(a), median = md, IQR = unname(iqr),
             skewness = mean((a - mean(a))^3) / (stats::sd(a)^3),
             kurtosis_excess = mean((a - mean(a))^4) / (stats::sd(a)^4) - 3,
             n_beyond_3_robust_sd = sum(abs(rz) > 3, na.rm = TRUE),
             n_beyond_1p5_IQR = sum(a < q[1] - 1.5 * iqr | a > q[2] + 1.5 * iqr),
             max_abs_robust_z = suppressWarnings(max(abs(rz), na.rm = TRUE)))
    })
  })
})
w(infl, "extreme_value_and_influence.csv")

# ------------------------- 9. current vs EQUAL-WEIGHT formula stability
wsens <- purrr::map_dfr(c("10min_based", "5min_based"), function(bin) {
  prim <- wide_scores(read_scores(bin))
  sens <- read_sens(bin) %>% select("AnimalNum", "Sex", "Domain", "DomainScore") %>%
    pivot_wider(names_from = "Domain", values_from = "DomainScore")
  purrr::map_dfr(names(MMM_RFID_SENSITIVITY_COEFFICIENTS), function(dm) {
    j <- inner_join(prim %>% select("AnimalNum", "Sex", a = all_of(dm)),
                    sens %>% select("AnimalNum", b = all_of(dm)), by = "AnimalNum")
    purrr::map_dfr(c("pooled", "Female", "Male"), function(st) {
      k <- if (st == "pooled") j else filter(j, .data$Sex == st)
      ok <- is.finite(k$a) & is.finite(k$b)
      if (sum(ok) < 3) return(tibble())
      tibble(bin_level = bin, stratum = st, Domain = dm, n = sum(ok),
             primary_formula = unname(MMM_RFID_CORE_FORMULAS[[dm]]),
             equal_weight_formula = unname(MMM_RFID_SENSITIVITY_FORMULAS[[dm]]),
             pearson_r = stats::cor(k$a[ok], k$b[ok]),
             spearman_rho = stats::cor(k$a[ok], k$b[ok], method = "spearman"),
             max_abs_rank_shift = max(abs(rank(k$a[ok]) - rank(k$b[ok]))),
             mean_abs_rank_shift = mean(abs(rank(k$a[ok]) - rank(k$b[ok]))))
    })
  })
}) %>%
  mutate(materially_weighting_sensitive = is.finite(.data$spearman_rho) & .data$spearman_rho < 0.90,
         decision_rule = paste(
           "The CURRENT audited formula remains PRIMARY. The equal-weight variant is",
           "reported only as a construct-stability check and is never selected on the",
           "basis of group p-values. spearman_rho < 0.90 is reported as materially",
           "weighting-sensitive."))
w(wsens, "weighting_sensitivity.csv")

# ----------------------------------------------------------- verdict
flags <- bind_rows(
  redundancy %>% filter(.data$spearman_flag != "acceptable") %>%
    transmute(check = "empirical_redundancy", bin_level = .data$bin_level,
              stratum = .data$stratum,
              detail = paste0(.data$domain_a, " ~ ", .data$domain_b),
              statistic = .data$spearman_rho, flag = .data$spearman_flag),
  alg %>% filter(.data$coefficient_overlap_flag != "acceptable") %>%
    transmute(check = "algebraic_overlap", bin_level = NA_character_, stratum = "coefficients",
              detail = paste0(.data$domain_a, " ~ ", .data$domain_b),
              statistic = .data$coefficient_cosine, flag = .data$coefficient_overlap_flag),
  loco %>%
    filter(!.data$is_locomotion_reference,
           .data$locomotion_dominance_flag != "acceptable") %>%
    transmute(check = "locomotion_dominance", bin_level = .data$bin_level,
              stratum = .data$stratum, detail = .data$Domain,
              statistic = .data$spearman_vs_movement_mean, flag = .data$locomotion_dominance_flag),
  wsens %>% filter(.data$materially_weighting_sensitive) %>%
    transmute(check = "weighting_sensitivity", bin_level = .data$bin_level,
              stratum = .data$stratum, detail = .data$Domain,
              statistic = .data$spearman_rho, flag = "materially_weighting_sensitive")
)
w(flags, "flags.csv")

# STOP-FOR-REVIEW is decided on BETWEEN-DOMAIN redundancy only. A domain's
# correlation with its own single contributor is definitional, not redundancy.
between_domain <- flags %>% filter(.data$check %in% c("empirical_redundancy", "algebraic_overlap"))
verdict <- tibble(
  n_core_domains = length(DOM),
  n_flags_total = nrow(flags),
  n_near_duplication = sum(flags$flag == "near_duplication"),
  n_high_redundancy = sum(flags$flag == "high_redundancy"),
  n_meaningful_overlap = sum(flags$flag == "meaningful_overlap"),
  n_between_domain_serious =
    sum(between_domain$flag %in% c("near_duplication", "high_redundancy")),
  stop_for_review =
    sum(between_domain$flag %in% c("near_duplication", "high_redundancy")) > 0,
  verdict_basis = paste(
    "STOP-FOR-REVIEW is triggered ONLY by BETWEEN-DOMAIN redundancy at",
    "|rho| >= 0.85. Movement output is Movement_mean_z, so its within-Sex Spearman",
    "with Movement_mean is 1.000 by construction; that is the domain's definition,",
    "is excluded from the verdict, and is reported separately as the locomotion",
    "reference."),
  phenotype_blind = TRUE,
  blindness_note = paste(
    "No Group, CombZ or phenotype label is read anywhere in this script. Sex is used",
    "ONLY as the declared standardization/reporting stratum."),
  action_note = paste(
    "Flags are REPORTED, never acted on automatically. No domain is removed and no",
    "formula is selected by this audit. A high_redundancy or near_duplication flag",
    "means STOP FOR REVIEW before the four-domain set is used in the manuscript."))
w(verdict, "verdict.csv")

message("  flags: ", nrow(flags), "; stop_for_review = ", verdict$stop_for_review)
message("  wrote ", length(written), " tables to ", OUT)
