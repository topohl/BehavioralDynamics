# ================================================================
# RELIABILITY DECOMPOSITION AND WHAT WOULD ACTUALLY IMPROVE IT
# MMMSociability
# ================================================================
# WHY THIS EXISTS
#
# audit_rfid_alternative_inference.R estimated reliability as the animal-level
# ICC across CC1-CC4 and found 0.12-0.51. That number is NOT interpretable as
# instrument reliability, and quoting it as such would be a mistake:
#
#   With ONE observation per animal per cage change, occasion-specific TRUE
#   CHANGE and MEASUREMENT ERROR are not separable. Animals demonstrably do
#   change across cage changes (mean movement rises from 3.65 at CC1 to 4.78 at
#   CC4), and every bit of that real change is being charged to "error" in a
#   cross-occasion ICC. A low cross-CC ICC is therefore consistent with a
#   perfectly reliable assay measuring a genuinely labile behaviour.
#
# The two can be separated. Each 12 h window contains 72 bins, so the window can
# be split into two interleaved halves and the same domain score computed twice
# from the SAME occasion. The correlation between halves is INTERNAL CONSISTENCY:
# measurement error only, with biological change held constant by construction.
#
#   split-half r (Spearman-Brown corrected)  = instrument reliability
#   cross-occasion ICC                        = stability of the trait
#   the GAP between them                      = genuine behavioural lability
#
# The split is by INTERLEAVED BLOCKS of consecutive bins, not by alternating
# single bins. RMSSD and ACF1 are lag-1 quantities: an odd/even bin split would
# turn them into lag-2 statistics and measure a different construct. Blocks of
# consecutive bins preserve lag-1 adjacency inside each run.
#
# The audit then asks what would actually raise power:
#   A  which of the nine raw contributors are reliable, and which are noise
#   B  would a reliability-weighted composite beat the equal-weight one
#   C  does averaging across cage changes help, EMPIRICALLY, or does it destroy
#      the acute-response construct
#   D  what the design would need for the manipulation contrast
#
# DIAGNOSTIC ONLY. Changes no Stage 28 decision. Writes to its own directory.
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

ROOT <- mmm_project_root()
BIN <- "10min_based"; BS <- 600
S28 <- function(bin = "10min") file.path(
  behavior_stage_dir(ROOT, "28", "rfid_behavioral_domains", bin), "tables")
OUT <- mmm_behavior_guard_numbered_output_path(
  mmm_behavior_output_active_root("rfid_reliability_audit", project_root = ROOT), ROOT)
if (!dir.exists(OUT)) dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
written <- character(0)
w <- function(x, nm) {
  if (is.null(x) || nrow(x) == 0L) return(invisible(NULL))
  write_csv(x, file.path(OUT, nm)); written <<- c(written, nm); invisible(nm)
}
rd <- function(p) suppressWarnings(read_csv(p, col_types = cols(.default = col_guess()),
                                            progress = FALSE))
has <- function(pkg) requireNamespace(pkg, quietly = TRUE)
DOM <- MMM_RFID_CORE_DOMAINS
FEAT <- MMM_RFID_RAW_FEATURES
CS <- c(-1, 0.5, 0.5); SR <- c(0, -1, 1)
BLOCK <- 6L   # bins per interleaved run; 6 x 10 min = 1 h

message("RELIABILITY DECOMPOSITION AND IMPROVEMENT AUDIT")

stage01 <- file.path(mmm_derived_metrics_output_root(ROOT), BIN,
                     "all_behavior_metrics.csv")
if (!file.exists(stage01)) stop("Missing Stage 01 input: ", stage01, call. = FALSE)
dat <- read_csv(stage01, col_types = cols(AnimalNum = col_character(),
                BinStart = col_datetime(), .default = col_guess()), progress = FALSE)
dat$AnimalNum <- canonical_animal_id(dat$AnimalNum)
sel_all <- mmm_rfid_select_all_acute_windows(dat, bin_size_sec = BS)
sel_all$CageEpochID <- mmm_rfid_cage_epoch_id(sel_all$Batch, sel_all$System,
                                              sel_all$CageChangeLabel)

# ================================================================
# 1. SPLIT-HALF INTERNAL CONSISTENCY (measurement error only)
# ================================================================
message("  1. split-half internal consistency (interleaved blocks)")
sel_all$.blk <- ((sel_all$target_slot - 1L) %/% BLOCK)
sel_all$.half <- ifelse(sel_all$.blk %% 2L == 0L, "A", "B")

half_feats <- purrr::map_dfr(c("A", "B"), function(h) {
  mmm_rfid_build_window_features(sel_all %>% filter(.data$.half == h)) %>%
    mutate(half = h)
})

split_half <- purrr::map_dfr(unique(sel_all$CageChangeLabel), function(cc) {
  a <- half_feats %>% filter(.data$half == "A", .data$CageChangeLabel == cc)
  b <- half_feats %>% filter(.data$half == "B", .data$CageChangeLabel == cc)
  j <- inner_join(a %>% select("AnimalNum", "Sex", all_of(FEAT)),
                  b %>% select("AnimalNum", all_of(FEAT)),
                  by = "AnimalNum", suffix = c(".A", ".B"))
  purrr::map_dfr(FEAT, function(f) {
    x <- j[[paste0(f, ".A")]]; y <- j[[paste0(f, ".B")]]
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 10L) return(tibble())
    r <- stats::cor(x[ok], y[ok])
    tibble(CageChangeLabel = cc, level = "raw_feature", measure = f, n = sum(ok),
           split_half_r = r,
           spearman_brown_full_window = (2 * r) / (1 + r))
  })
})

# Domain scores computed independently in each half, then correlated.
half_domain <- purrr::map_dfr(c("A", "B"), function(h) {
  ft <- half_feats %>% filter(.data$half == h)
  z <- mmm_rfid_standardize_within_sex(ft, reference_label = paste0("half_", h))$scaled
  mmm_rfid_score_all_domains(z, id_cols = c("AnimalNum", "Group", "Sex",
                                            "CageChangeLabel")) %>%
    mutate(half = h)
})
split_half_dom <- purrr::map_dfr(unique(sel_all$CageChangeLabel), function(cc) {
  purrr::map_dfr(DOM, function(dm) {
    a <- half_domain %>% filter(.data$half == "A", .data$CageChangeLabel == cc,
                                .data$Domain == dm)
    b <- half_domain %>% filter(.data$half == "B", .data$CageChangeLabel == cc,
                                .data$Domain == dm)
    j <- inner_join(a %>% select("AnimalNum", sa = "DomainScore"),
                    b %>% select("AnimalNum", sb = "DomainScore"), by = "AnimalNum")
    ok <- is.finite(j$sa) & is.finite(j$sb)
    if (sum(ok) < 10L) return(tibble())
    r <- stats::cor(j$sa[ok], j$sb[ok])
    tibble(CageChangeLabel = cc, level = "domain", measure = dm, n = sum(ok),
           split_half_r = r, spearman_brown_full_window = (2 * r) / (1 + r))
  })
})
reliab <- bind_rows(split_half, split_half_dom) %>%
  mutate(caveat = paste(
    "Split halves are INTERLEAVED BLOCKS of", BLOCK, "consecutive bins, so lag-1",
    "adjacency is preserved inside each run and RMSSD/ACF1 remain lag-1. Each half",
    "has half the bins, so the raw split-half r UNDERSTATES full-window",
    "reliability; the Spearman-Brown column is the corrected estimate."))
w(reliab, "1_split_half_internal_consistency.csv")

# ================================================================
# 2. DECOMPOSITION: measurement error vs genuine lability
# ================================================================
message("  2. reliability vs stability")
lg <- rd(file.path(S28(), "longitudinal_domain_scores.csv")) %>%
  filter(is.finite(.data$DomainScore)) %>%
  mutate(AnimalNum = as.character(.data$AnimalNum),
         CC = factor(as.character(.data$CageChangeLabel)))
cross_icc <- purrr::map_dfr(DOM, function(dm) {
  d <- droplevels(lg %>% filter(.data$Domain == dm))
  if (!has("lme4")) return(tibble())
  m <- tryCatch(suppressMessages(suppressWarnings(
    lme4::lmer(DomainScore ~ CC + (1 | AnimalNum), data = d))), error = function(e) NULL)
  if (is.null(m)) return(tibble())
  vc <- as.data.frame(lme4::VarCorr(m))
  va <- vc$vcov[vc$grp == "AnimalNum"][1]; ve <- vc$vcov[vc$grp == "Residual"][1]
  tibble(measure = dm, cross_occasion_ICC = va / (va + ve))
})
decomp <- split_half_dom %>%
  group_by(measure = .data$measure) %>%
  summarise(internal_consistency_SB = mean(.data$spearman_brown_full_window,
                                           na.rm = TRUE), .groups = "drop") %>%
  left_join(cross_icc, by = "measure") %>%
  mutate(
    genuine_lability = pmax(0, .data$internal_consistency_SB - .data$cross_occasion_ICC),
    verdict = dplyr::case_when(
      .data$internal_consistency_SB < 0.5 ~
        "THE ASSAY IS NOISY: the score does not even agree with itself within one night. Improving the SCORE is the lever.",
      .data$cross_occasion_ICC < 0.3 ~
        "ASSAY IS FINE, BEHAVIOUR IS LABILE: the score is internally consistent but genuinely changes between cage changes. Averaging occasions measures a trait; a single window measures that night. Neither is 'unreliable'.",
      TRUE ~ "both internally consistent and stable"),
    interpretation = paste(
      "internal_consistency_SB is measurement reliability with biological change",
      "held constant. cross_occasion_ICC additionally charges real between-occasion",
      "change to error. A LARGE GAP means the behaviour is labile, NOT that the",
      "instrument is bad - and a labile measure is the correct instrument for an",
      "ACUTE RESPONSE question."))
w(decomp, "2_reliability_vs_stability_decomposition.csv")

# ================================================================
# 3. WHICH CONTRIBUTORS ARE RELIABLE?
# ================================================================
message("  3. contributor-level reliability")
contrib <- split_half %>%
  group_by(measure = .data$measure) %>%
  summarise(mean_SB = mean(.data$spearman_brown_full_window, na.rm = TRUE),
            min_SB = min(.data$spearman_brown_full_window, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(channel = sub("_.*$", "", .data$measure),
         statistic = sub("^[^_]*_", "", .data$measure),
         grade = dplyr::case_when(.data$mean_SB >= 0.8 ~ "excellent",
                                  .data$mean_SB >= 0.6 ~ "good",
                                  .data$mean_SB >= 0.4 ~ "marginal",
                                  TRUE ~ "POOR - contributes mostly noise"),
         note = paste(
           "A composite cannot be more reliable than its contributors. If one term",
           "is near-noise it adds variance without signal and DILUTES the score.",
           "This is the most direct lever on power available without new data."))
w(contrib, "3_contributor_reliability.csv")

# ================================================================
# 4. WOULD A RELIABILITY-WEIGHTED COMPOSITE DO BETTER?
# ================================================================
# Weighting each contributor by its reliability is the standard fix for a
# composite diluted by a noisy term. Compared PHENOTYPE-BLIND on internal
# consistency, then - separately and marked exploratory - on the contrasts.
message("  4. reliability-weighted composite")
relw <- setNames(contrib$mean_SB, contrib$measure)
wcoef <- purrr::map(MMM_RFID_CORE_COEFFICIENTS, function(cf) {
  base <- sub("_z$", "", names(cf))
  ww <- relw[base]; ww[!is.finite(ww)] <- 0
  out <- cf * ww
  if (sum(abs(out)) == 0) return(cf)
  out * (sum(abs(cf)) / sum(abs(out)))   # keep the original total weight
})
wf <- setNames(vapply(names(wcoef), function(d)
  paste(sprintf("%s*%+.3f", names(wcoef[[d]]), unname(wcoef[[d]])), collapse = " "),
  character(1)), names(wcoef))

wt_half <- purrr::map_dfr(c("A", "B"), function(h) {
  ft <- half_feats %>% filter(.data$half == h, .data$CageChangeLabel == "CC1")
  z <- mmm_rfid_standardize_within_sex(ft, reference_label = h)$scaled
  mmm_rfid_score_all_domains(z, coefficients = wcoef, formulas = wf,
                             id_cols = c("AnimalNum", "Group", "Sex")) %>%
    mutate(half = h)
})
wt_rel <- purrr::map_dfr(DOM, function(dm) {
  a <- wt_half %>% filter(.data$half == "A", .data$Domain == dm)
  b <- wt_half %>% filter(.data$half == "B", .data$Domain == dm)
  j <- inner_join(a %>% select("AnimalNum", sa = "DomainScore"),
                  b %>% select("AnimalNum", sb = "DomainScore"), by = "AnimalNum")
  ok <- is.finite(j$sa) & is.finite(j$sb)
  if (sum(ok) < 10L) return(tibble())
  r <- stats::cor(j$sa[ok], j$sb[ok])
  tibble(Domain = dm, weighted_split_half_r = r,
         weighted_SB = (2 * r) / (1 + r))
}) %>%
  left_join(split_half_dom %>% filter(.data$CageChangeLabel == "CC1") %>%
              select(Domain = "measure", equal_weight_SB = "spearman_brown_full_window"),
            by = "Domain") %>%
  mutate(reliability_gain = .data$weighted_SB - .data$equal_weight_SB,
         worth_it = .data$reliability_gain > 0.05,
         decision_guard = paste(
           "Compared on RELIABILITY, which is phenotype-blind. A reweighting must",
           "never be adopted because it lowers a group p-value. The equal-weight",
           "audited formula stays primary unless the reliability gain is material",
           "AND the change is predeclared."))
w(wt_rel, "4_reliability_weighted_composite.csv")

# ================================================================
# 5. DOES AVERAGING OCCASIONS ACTUALLY HELP? (empirical)
# ================================================================
message("  5. averaging occasions - empirical power check")
avg_scores <- lg %>%
  group_by(.data$AnimalNum, .data$Group, .data$Sex, .data$Batch, .data$Domain) %>%
  summarise(DomainScore = mean(.data$DomainScore), n_occasions = dplyr::n(),
            .groups = "drop") %>%
  filter(.data$n_occasions >= 3L)
# Cage identity is not constant across occasions, so the CC1 cage is used as the
# blocking unit for an averaged score; that is its social origin.
cc1cage <- lg %>% filter(.data$CC == "CC1") %>%
  distinct(.data$AnimalNum, CageEpochID = .data$CageEpochID)
avg_scores <- avg_scores %>% left_join(cc1cage, by = "AnimalNum") %>%
  mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
         Batch = factor(as.character(.data$Batch)),
         CageEpochID = factor(as.character(.data$CageEpochID)))
fn <- rd(file.path(S28(), "first_night_domain_scores.csv")) %>%
  filter(is.finite(.data$DomainScore)) %>%
  mutate(Group = factor(as.character(.data$Group), levels = MMM_RFID_GROUP_LEVELS),
         Batch = factor(as.character(.data$Batch)),
         CageEpochID = factor(as.character(.data$CageEpochID)))
cmp_avg <- purrr::map_dfr(DOM, function(dm) {
  one <- function(d, lbl) {
    d <- droplevels(d)
    if (nrow(d) < 20L || !has("lmerTest") || !has("emmeans")) return(tibble())
    m <- tryCatch(suppressMessages(suppressWarnings(
      lmerTest::lmer(DomainScore ~ Group + Batch + (1 | CageEpochID), data = d))),
      error = function(e) NULL)
    if (is.null(m)) return(tibble())
    ct <- as.data.frame(suppressMessages(emmeans::contrast(
      suppressMessages(emmeans::emmeans(m, ~ Group)),
      list(stress_vs_CON = CS, SUS_vs_RES = SR), adjust = "none", infer = c(TRUE, TRUE))))
    sdv <- stats::sd(d$DomainScore)
    tibble(score = lbl, contrast = as.character(ct$contrast), n = nrow(d),
           estimate = ct$estimate, SE = ct$SE, df = ct$df, raw_p = ct$p.value,
           mde_hedges_g = mmm_rfid_mde(ct$SE, ct$df, sdv))
  }
  bind_rows(
    one(fn %>% filter(.data$Domain == dm), "CC1_single_window"),
    one(avg_scores %>% filter(.data$Domain == dm), "mean_of_CC1_to_CC4")
  ) %>% mutate(Domain = dm)
})
if (nrow(cmp_avg) > 0L) {
  cmp_avg <- cmp_avg %>%
    select("Domain", "contrast", "score", "n", "estimate", "SE", "df", "raw_p",
           "mde_hedges_g") %>%
    tidyr::pivot_wider(names_from = "score",
                       values_from = c("n", "estimate", "SE", "df", "raw_p",
                                       "mde_hedges_g")) %>%
    mutate(mde_ratio_avg_vs_single =
             .data$mde_hedges_g_mean_of_CC1_to_CC4 / .data$mde_hedges_g_CC1_single_window,
           averaging_helps = is.finite(.data$mde_ratio_avg_vs_single) &
             .data$mde_ratio_avg_vs_single < 0.9,
           construct_warning = paste(
             "Averaging CC1-CC4 measures a TRAIT, not the acute first-night",
             "response. If the scientific question is the acute response, a more",
             "reliable trait score is the WRONG instrument however much power it",
             "gains. Use this only for trait-style characterisation."))
}
w(cmp_avg, "5_averaging_occasions_power.csv")

# ================================================================
# 6. WHAT WOULD THE DESIGN NEED?
# ================================================================
message("  6. design requirements")
design <- tibble()
op <- file.path(S28(), "first_night_orthogonal_contrasts_pooled.csv")
if (file.exists(op)) {
  o <- rd(op)
  design <- o %>%
    filter(.data$contrast == "stress_vs_CON") %>%
    select("Domain", "estimate", "SE", "df", "n_cages") %>%
    left_join(decomp %>% select(Domain = "measure", "internal_consistency_SB"),
              by = "Domain") %>%
    mutate(
      n_control_cages_now = 6L,
      # SE scales as 1/sqrt(k) in the number of control clusters
      se_if_12_control_cages = .data$SE * sqrt(6 / 12),
      se_if_18_control_cages = .data$SE * sqrt(6 / 18),
      implied_p_12 = 2 * stats::pt(abs(.data$estimate / .data$se_if_12_control_cages),
                                   df = .data$df + 6, lower.tail = FALSE),
      implied_p_18 = 2 * stats::pt(abs(.data$estimate / .data$se_if_18_control_cages),
                                   df = .data$df + 12, lower.tail = FALSE),
      lever = paste(
        "For the MANIPULATION contrast the binding constraint is the number of",
        "CONTROL CAGES (6), not the number of animals (24). Adding animals to the",
        "existing 6 control cages buys almost nothing; adding control CAGES buys",
        "precision as 1/sqrt(k). These projections assume the observed effect and",
        "variance components hold, and are a DESIGN aid, not a result."))
}
w(design, "6_design_requirements.csv")

# ================================================================
# VERDICT
# ================================================================
verdict <- decomp %>%
  select("measure", "internal_consistency_SB", "cross_occasion_ICC",
         "genuine_lability", "verdict") %>%
  mutate(headline = paste0(
    "internal consistency ", signif(.data$internal_consistency_SB, 2),
    " vs cross-occasion ", signif(.data$cross_occasion_ICC, 2)))
verdict$audit_role <- paste(
  "The cross-occasion ICC reported by audit_rfid_alternative_inference.R is NOT",
  "instrument reliability and must not be quoted as such. This audit separates",
  "measurement error from genuine behavioural lability using within-occasion",
  "split halves. DIAGNOSTIC ONLY; changes no Stage 28 decision.")
w(verdict, "VERDICT.csv")

message("  wrote ", length(written), " tables to ", OUT)
for (i in seq_len(nrow(decomp))) {
  message(sprintf("    %-34s internal %.2f | cross-occasion %.2f", decomp$measure[i],
                  decomp$internal_consistency_SB[i], decomp$cross_occasion_ICC[i]))
}
