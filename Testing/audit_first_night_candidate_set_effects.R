## audit_first_night_candidate_set_effects.R
## ===========================================================================
## FIRST-NIGHT (CC1, canonical CLOCK window 18:30 -> 06:30) 10-DOMAIN CANDIDATE SET:
##   (1) within-Sex group contrasts for ALL 10 candidate domains (+ the non-displayed duplicate),
##   (2) the FORMAL Group:Sex interaction test for every candidate, with BH across the 10 tests,
##   (3) multiplicity sensitivity of every raw p under multiple row-set sizes x 2 FDR families
##       (within-Sex PRIMARY, pooled-Sex SENSITIVITY),
##   (4) resolution agreement (10min primary vs 5min sensitivity) and locomotion dominance.
##
## INPUT (built and verified upstream; NOT recomputed here):
##   OUT/first_night_10domain_scores.csv  <- Testing/audit_first_night_candidate_set_scores.R
##   That script owns the window derivation, the phase rule, the nine raw z-features, the
##   z-within-SEX-ONLY standardization contract and the score_mean()/coalesce(x,0) semantics.
##   This script performs NO feature engineering: it only models the delivered per-animal scores.
##
## MODEL (exactly as specified, no deviation):
##   DomainScore ~ Group * Sex   (plain stats::lm; ONE value per animal, so NO random effect)
##   emmeans::emmeans(fit, ~ Group | Sex)
##   contrast(list("RES-CON"=c(-1,1,0), "SUS-CON"=c(-1,0,1), "SUS-RES"=c(0,-1,1)), adjust="none")
##   Group levels c("CON","RES","SUS"); Sex levels c("Female","Male")
##   CI = estimate +/- qt(0.975, df) * SE, df = lm residual df (emmeans Wald t interval,
##        adjust = "none", level = 0.95)
##   Effect size = animal-level Hedges g via Functions/hmm_stage14_helpers.R :: hmm_hedges_g()
##
## SIGNIFICANCE ROLE -- READ THIS.
##   This script SUPPLIES numbers; it makes NO inclusion decision. No domain is added because a
##   contrast is strong and none is dropped because a contrast is null. The row-set definitions
##   used for the multiplicity sensitivity are built from STRUCTURAL redundancy facts established
##   upstream (algebraic identities, Spearman redundancy class, HMM temporal-order invariance),
##   never from p-values, and every row-set is reported in full so no set is privileged here.
##
## INTERPRETATION GUARDS enforced throughout
##   - RFID proximity is a social-spatial CO-LOCATION proxy, NEVER "sociability".
##   - RES/SUS are LATER phenotype labels derived from subsequent CombZ. Every contrast is a
##     DESCRIPTIVE association with later phenotype -- never prospective, never causal.
##   - SEX-DIFFERENTIAL LANGUAGE REQUIRES THE FORMAL Group:Sex INTERACTION. A significant
##     within-Female contrast alongside a null within-Male contrast is NOT evidence of a sex
##     difference. Enforced mechanically by sex_differential_language_supported.
##
## READ-ONLY with respect to Analysis/ and Functions/. Writes only into
##   <STAGE14>/audit_hmm_state_architecture/first_night_domain_heatmap/
## ===========================================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(purrr); library(tibble); library(emmeans)
})

setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")

PROJ    <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
STAGE14 <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based")
OUT     <- file.path(STAGE14, "audit_hmm_state_architecture/first_night_domain_heatmap")
DERIV   <- file.path(PROJ, "analysis_ready/03_derived_metrics")
stopifnot(dir.exists(OUT))

THIS_SCRIPT  <- "Testing/audit_first_night_candidate_set_effects.R"
UPSTREAM     <- "Testing/audit_first_night_candidate_set_scores.R"
SCORES_CSV   <- file.path(OUT, "first_night_10domain_scores.csv")
GROUP_LEVELS <- c("CON", "RES", "SUS")
SEX_LEVELS   <- c("Female", "Male")
RESOLUTIONS  <- c("10min_based", "5min_based")
RES_ROLE     <- c("10min_based" = "primary", "5min_based" = "sensitivity")
LOCO_FLAG    <- 0.70

CONTRASTS <- list("RES-CON" = c(-1, 1, 0), "SUS-CON" = c(-1, 0, 1), "SUS-RES" = c(0, -1, 1))
CONTRAST_REF  <- c("RES-CON" = "CON", "SUS-CON" = "CON", "SUS-RES" = "RES")
CONTRAST_COMP <- c("RES-CON" = "RES", "SUS-CON" = "SUS", "SUS-RES" = "SUS")

MODEL_FORMULA <- "DomainScore ~ Group * Sex"
MODEL_ENGINE  <- paste0("stats::lm + emmeans::emmeans(~ Group | Sex) + contrast(adjust='none'); ",
                        "ONE score per animal so no random effect is identifiable")
CI_FORMULA    <- paste0("estimate +/- qt(0.975, df) * SE, df = lm residual df ",
                        "(emmeans Wald t interval, adjust='none', level=0.95)")
GUARD <- paste0("RFID proximity = social-spatial CO-LOCATION proxy, never 'sociability'. ",
                "RES/SUS are LATER phenotype labels from subsequent CombZ: every contrast is a ",
                "DESCRIPTIVE association with later phenotype, never prospective or causal.")
SIG_ROLE <- paste0("Significance comes LAST and determines NO inclusion. This script supplies ",
                   "numbers only; no row is added for a strong contrast or dropped for a null one.")

hr  <- function(x) cat("\n", strrep("=", 92), "\n", x, "\n", strrep("=", 92), "\n", sep = "")
sec <- function(x) cat("\n--- ", x, " ---\n", sep = "")
pf  <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"
r3  <- function(x) round(x, 3)
r4  <- function(x) round(x, 4)
sci <- function(x) format(x, scientific = TRUE, digits = 3)
pfmt <- function(p) ifelse(is.na(p), NA_character_,
                    ifelse(p < 1e-4, format(p, scientific = TRUE, digits = 3), sprintf("%.5f", p)))

ASSERT <- list()
add_assert <- function(assertion, method, ok, evidence) {
  ASSERT[[length(ASSERT) + 1L]] <<- tibble(assertion = assertion, method = method,
                                           result = pf(ok), evidence = evidence)
  cat("  [", pf(ok), "] ", assertion, "\n        ", evidence, "\n", sep = "")
  invisible(ok)
}
safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = method))
}
safe_cor_p <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 4 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor.test(x[ok], y[ok], method = method)$p.value)
}

## ==========================================================================
hr("STEP 0. Canonical 111-animal roster (independent identity check)")
## ==========================================================================
roster <- build_canonical_identity_roster(
  read_csv(file.path(DERIV, "5min_based/all_behavior_metrics.csv"),
           col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                            Group = col_character(), Sex = col_character()), progress = FALSE),
  "Stage 01 canonical roster (5min_based all_behavior_metrics.csv)")
cat("canonical roster animals:", nrow(roster), "\n")
print(as.data.frame(roster %>% count(Group, Sex) %>% arrange(Group, Sex)), row.names = FALSE)
stopifnot(nrow(roster) == 111L)

## ==========================================================================
hr("STEP 1. Read the upstream per-animal 10-domain score matrix")
## ==========================================================================
scores_raw <- read_csv(SCORES_CSV,
                       col_types = cols(.default = col_character(),
                                        DomainScore = col_double(),
                                        row_id = col_integer(),
                                        n_bins = col_double(),
                                        coverage_fraction = col_double(),
                                        Movement_mean_z = col_double(),
                                        Movement_rmssd_z = col_double(),
                                        Movement_acf1_z = col_double(),
                                        Entropy_mean_z = col_double(),
                                        Entropy_rmssd_z = col_double(),
                                        Entropy_acf1_z = col_double(),
                                        Proximity_mean_z = col_double(),
                                        Proximity_rmssd_z = col_double(),
                                        Proximity_acf1_z = col_double()),
                       progress = FALSE)
cat("rows read:", nrow(scores_raw), " cols:", ncol(scores_raw), "\n")

DOM_ORDER <- scores_raw %>% distinct(row_id, Domain, candidate_status, displayed_in_current_7) %>%
  arrange(row_id)
print(as.data.frame(DOM_ORDER %>% mutate(Domain = str_trunc(Domain, 42),
                                         candidate_status = str_trunc(candidate_status, 46))),
      row.names = FALSE)
ALL_DOMS  <- DOM_ORDER$Domain
DUP_DOM   <- DOM_ORDER$Domain[DOM_ORDER$row_id == 11L]
DISPLAY_7 <- DOM_ORDER$Domain[DOM_ORDER$row_id <= 7L]
CAND_STATUS <- setNames(DOM_ORDER$candidate_status, DOM_ORDER$Domain)
ROWID       <- setNames(DOM_ORDER$row_id, DOM_ORDER$Domain)

scores <- scores_raw %>%
  mutate(Group = factor(Group, levels = GROUP_LEVELS),
         Sex   = factor(Sex,   levels = SEX_LEVELS)) %>%
  filter(!is.na(Group), !is.na(Sex))

add_assert("input score matrix has exactly one row per AnimalNum x Domain x resolution",
           "count(AnimalNum, Domain, bin_resolution) filtered to n != 1",
           nrow(scores %>% count(AnimalNum, Domain, bin_resolution) %>% filter(n != 1L)) == 0L,
           sprintf("%s rows, %s distinct keys, %s offending cells", nrow(scores),
                   nrow(distinct(scores, AnimalNum, Domain, bin_resolution)),
                   nrow(scores %>% count(AnimalNum, Domain, bin_resolution) %>% filter(n != 1L))))
add_assert("all animals in the score matrix are in the canonical 111 roster",
           "setdiff(scores$AnimalNum, roster$AnimalNum)",
           length(setdiff(unique(scores$AnimalNum), roster$AnimalNum)) == 0L,
           sprintf("%s distinct animals in scores; %s not in roster",
                   n_distinct(scores$AnimalNum),
                   length(setdiff(unique(scores$AnimalNum), roster$AnimalNum))))

sec("per-domain n_finite by resolution")
nfin <- scores %>% group_by(row_id, Domain, bin_resolution) %>%
  summarise(n_finite = sum(is.finite(DomainScore)), .groups = "drop") %>%
  pivot_wider(names_from = bin_resolution, values_from = n_finite) %>% arrange(row_id)
print(as.data.frame(nfin %>% mutate(Domain = str_trunc(Domain, 42))), row.names = FALSE)

sec("per-domain x Group x Sex cell counts (10min primary) -- the TRUE n behind every contrast")
cellN <- scores %>% filter(bin_resolution == "10min_based", is.finite(DomainScore)) %>%
  count(row_id, Domain, Sex, Group) %>%
  unite("cell", Sex, Group, sep = "_") %>%
  pivot_wider(names_from = cell, values_from = n) %>% arrange(row_id)
print(as.data.frame(cellN %>% mutate(Domain = str_trunc(Domain, 42))), row.names = FALSE)

## ==========================================================================
hr("STEP 2. OUT/first_night_10domain_effects.csv  --  lm(Score ~ Group*Sex) contrasts")
## ==========================================================================
fit_one <- function(dat) {
  d <- dat %>% filter(is.finite(DomainScore)) %>%
    mutate(Group = droplevels(factor(Group, levels = GROUP_LEVELS)),
           Sex   = droplevels(factor(Sex,   levels = SEX_LEVELS)))
  need <- length(levels(d$Group)) == 3L && length(levels(d$Sex)) == 2L &&
    nrow(d %>% count(Group, Sex) %>% filter(n >= 2L)) == 6L
  if (!need) return(list(fit = NULL, dat = d, status = "insufficient_cells"))
  fit <- try(stats::lm(DomainScore ~ Group * Sex, data = d), silent = TRUE)
  if (inherits(fit, "try-error")) return(list(fit = NULL, dat = d, status = "lm_failed"))
  list(fit = fit, dat = d, status = "ok_lm_group_by_sex")
}

effects <- pmap_dfr(expand_grid(Domain = ALL_DOMS, resolution = RESOLUTIONS),
  function(Domain, resolution) {
    dom <- Domain; res <- resolution
    m <- fit_one(scores %>% filter(Domain == dom, bin_resolution == res))
    grid <- expand_grid(Sex = SEX_LEVELS, contrast = names(CONTRASTS))
    if (is.null(m$fit)) {
      return(grid %>% mutate(Domain = dom, resolution = res, model_status = m$status,
                             n_ref = NA_integer_, n_comp = NA_integer_, mean_ref = NA_real_,
                             mean_comp = NA_real_, hedges_g = NA_real_, estimate = NA_real_,
                             SE = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_,
                             raw_p = NA_real_))
    }
    emm <- emmeans::emmeans(m$fit, ~ Group | Sex)
    ct  <- as.data.frame(summary(emmeans::contrast(emm, method = CONTRASTS, adjust = "none"),
                                 infer = c(TRUE, TRUE), level = 0.95, adjust = "none"))
    names(ct)[names(ct) == "lower.CL"] <- "ci_low"
    names(ct)[names(ct) == "upper.CL"] <- "ci_high"
    ct <- ct %>% transmute(Sex = as.character(Sex), contrast = as.character(contrast),
                           estimate, SE, df, ci_low, ci_high, raw_p = p.value)
    desc <- pmap_dfr(grid, function(Sex, contrast) {
      sx <- Sex; cn <- contrast
      rv <- m$dat$DomainScore[m$dat$Sex == sx & m$dat$Group == CONTRAST_REF[[cn]]]
      cv <- m$dat$DomainScore[m$dat$Sex == sx & m$dat$Group == CONTRAST_COMP[[cn]]]
      tibble(Sex = sx, contrast = cn, n_ref = length(rv), n_comp = length(cv),
             mean_ref = mean(rv), mean_comp = mean(cv), hedges_g = hmm_hedges_g(rv, cv))
    })
    desc %>% left_join(ct, by = c("Sex", "contrast")) %>%
      mutate(Domain = dom, resolution = res, model_status = m$status)
  })

effects_out <- effects %>%
  mutate(row_id = ROWID[Domain], candidate_status = CAND_STATUS[Domain],
         bin_resolution = resolution, resolution_role = RES_ROLE[resolution],
         ci_formula = CI_FORMULA, model_formula = MODEL_FORMULA, model_engine = MODEL_ENGINE,
         cage_change = "CC1",
         phase_window = "first Active phase, clock 18:30 (incl) -> 06:30 (excl)",
         window_hours = 12,
         standardization = paste0("raw features z-scored WITHIN SEX ONLY upstream; ",
                                  "domain scores modelled as delivered"),
         interpretation_guard = GUARD, significance_role = SIG_ROLE,
         source_table = "first_night_10domain_scores.csv", source_script = UPSTREAM,
         script = THIS_SCRIPT) %>%
  select(row_id, Domain, candidate_status, Sex, contrast, bin_resolution, resolution_role,
         n_ref, n_comp, mean_ref, mean_comp, hedges_g, estimate, SE, df, ci_low, ci_high,
         ci_formula, raw_p, model_formula, model_engine, model_status,
         cage_change, phase_window, window_hours, standardization,
         interpretation_guard, significance_role, source_table, source_script, script) %>%
  arrange(bin_resolution != "10min_based", row_id, Sex, contrast)

write_csv(effects_out, file.path(OUT, "first_night_10domain_effects.csv"))
cat("WROTE", file.path(OUT, "first_night_10domain_effects.csv"), "rows:", nrow(effects_out), "\n")

show_eff <- function(res, sx) {
  sec(sprintf("EFFECTS  [%s | %s]  (10 candidates + duplicate x 3 contrasts)", res, sx))
  print(as.data.frame(effects_out %>% filter(bin_resolution == res, Sex == sx) %>%
    transmute(id = row_id, Domain = str_trunc(Domain, 38), contrast,
              n = paste0(n_ref, "v", n_comp), g = r3(hedges_g), est = r3(estimate),
              SE = r3(SE), df, CI = sprintf("[%s, %s]", r3(ci_low), r3(ci_high)),
              p = pfmt(raw_p), sig = ifelse(is.na(raw_p), "", ifelse(raw_p < 0.05, "*", "")))),
    row.names = FALSE)
}
for (sx in SEX_LEVELS) show_eff("10min_based", sx)
for (sx in SEX_LEVELS) show_eff("5min_based", sx)

add_assert("every candidate x Sex x contrast x resolution cell has a fitted lm contrast",
           "model_status == 'ok_lm_group_by_sex' over 11 domains x 2 Sex x 3 contrasts x 2 res",
           all(effects_out$model_status == "ok_lm_group_by_sex"),
           sprintf("%s rows; statuses: %s", nrow(effects_out),
                   paste(sprintf("%s=%s", names(table(effects_out$model_status)),
                                 as.integer(table(effects_out$model_status))), collapse = ", ")))
dev_est <- max(abs(effects_out$estimate - (effects_out$mean_comp - effects_out$mean_ref)), na.rm = TRUE)
add_assert("emmeans contrast estimate equals the raw cell-mean difference (saturated-lm identity)",
           "max |estimate - (mean_comp - mean_ref)| over all fitted rows",
           dev_est < 1e-9,
           sprintf("max abs deviation = %s (cell means ARE the emmeans EMMs for a saturated Group*Sex lm)",
                   sci(dev_est)))
dev_ci <- max(abs(effects_out$ci_low - (effects_out$estimate - qt(0.975, effects_out$df) * effects_out$SE)),
              abs(effects_out$ci_high - (effects_out$estimate + qt(0.975, effects_out$df) * effects_out$SE)),
              na.rm = TRUE)
add_assert("CI reproduces estimate +/- qt(0.975, df) * SE",
           "max |ci_low - (estimate - qt(0.975, df)*SE)| and the upper analogue",
           dev_ci < 1e-9,
           sprintf("max abs deviation = %s; formula recorded in the ci_formula column", sci(dev_ci)))
dup_a <- effects_out %>% filter(row_id == 5L) %>% arrange(bin_resolution, Sex, contrast)
dup_b <- effects_out %>% filter(row_id == 11L) %>% arrange(bin_resolution, Sex, contrast)
add_assert("the non-displayed duplicate gives numerically identical contrasts to Active-phase adaptation",
           "max |estimate| and |raw_p| difference between row 11 and row 5, both resolutions",
           max(abs(dup_a$estimate - dup_b$estimate), abs(dup_a$raw_p - dup_b$raw_p)) < 1e-12,
           sprintf("max abs est diff = %s, max abs p diff = %s (Analysis/14:5563 sets them equal verbatim)",
                   sci(max(abs(dup_a$estimate - dup_b$estimate))),
                   sci(max(abs(dup_a$raw_p - dup_b$raw_p)))))

## ==========================================================================
hr("STEP 3. OUT/first_night_group_sex_interactions.csv  --  the FORMAL Group:Sex test")
## ==========================================================================
inter <- pmap_dfr(expand_grid(Domain = ALL_DOMS, resolution = RESOLUTIONS),
  function(Domain, resolution) {
    dom <- Domain; res <- resolution
    m <- fit_one(scores %>% filter(Domain == dom, bin_resolution == res))
    if (is.null(m$fit)) {
      return(tibble(Domain = dom, resolution = res, term = "Group:Sex", NumDF = NA_real_,
                    DenDF = NA_real_, F_value = NA_real_, p_uncorrected = NA_real_,
                    n_animals = nrow(m$dat), model_status = m$status))
    }
    av <- as.data.frame(stats::anova(m$fit))
    rw <- av["Group:Sex", ]
    tibble(Domain = dom, resolution = res, term = "Group:Sex",
           NumDF = rw$Df, DenDF = av["Residuals", "Df"], F_value = rw[["F value"]],
           p_uncorrected = rw[["Pr(>F)"]], n_animals = nrow(m$dat), model_status = m$status)
  })

## BH across the 10 CANDIDATE interaction tests, within resolution. The duplicate (row 11) is a
## literal copy of row 5 and is EXCLUDED from the adjustment family (including it would
## double-count one test); it is still reported with its uncorrected p and an explicit note.
inter <- inter %>%
  mutate(row_id = ROWID[Domain], candidate_status = CAND_STATUS[Domain]) %>%
  group_by(resolution) %>%
  mutate(q_BH_across_10_interaction_tests =
           p.adjust(ifelse(row_id <= 10L, p_uncorrected, NA_real_), method = "BH")) %>%
  ungroup()

nomsig <- effects_out %>% filter(!is.na(raw_p)) %>%
  group_by(Domain, bin_resolution) %>%
  summarise(any_within_sex_contrast_nominally_sig = any(raw_p < 0.05),
            n_nominally_sig_contrasts = sum(raw_p < 0.05),
            nominally_sig_cells = paste(sprintf("%s %s p=%s", Sex, contrast, pfmt(raw_p))[raw_p < 0.05],
                                        collapse = "; "),
            female_only_sig_pattern = any(raw_p < 0.05 & Sex == "Female") &
                                      !any(raw_p < 0.05 & Sex == "Male"),
            male_only_sig_pattern   = any(raw_p < 0.05 & Sex == "Male") &
                                      !any(raw_p < 0.05 & Sex == "Female"),
            .groups = "drop") %>%
  rename(resolution = bin_resolution)

inter_out <- inter %>% left_join(nomsig, by = c("Domain", "resolution")) %>%
  mutate(
    bin_resolution = resolution, resolution_role = RES_ROLE[resolution],
    multiplicity_treatment = ifelse(
      row_id <= 10L,
      paste0("p_uncorrected is PRIMARY for the interaction test. Rationale: the Group:Sex test is ",
             "the SEX-DIFFERENTIAL GATEKEEPER -- one pre-specified structural question per ",
             "candidate, not a screen over an exchangeable family. FDR-controlling the 10 ",
             "gatekeepers would make sex-differential language HARDER to license exactly where it ",
             "is already weak and would inflate type-II error on a guard whose job is to BLOCK ",
             "over-claiming. q_BH_across_10_interaction_tests (BH over the 10 candidate ",
             "interaction tests, within resolution) is reported as a SECONDARY, more conservative ",
             "reference. Neither p nor q is used to include or exclude any candidate row."),
      paste0("EXCLUDED from the BH family: this row is a literal duplicate of ",
             "'Active-phase adaptation/exploration' (Analysis/14:5563), so including it would ",
             "double-count one test. Uncorrected p reported for completeness only.")),
    multiplicity_primary = ifelse(row_id <= 10L, "p_uncorrected", "not_in_family"),
    ## HARD GUARD: sex-differential language is licensed ONLY by the formal interaction.
    sex_differential_language_supported = is.finite(p_uncorrected) & p_uncorrected < 0.05,
    interaction_p_that_justifies_it = p_uncorrected,
    sex_differential_verdict = case_when(
      !is.finite(p_uncorrected) ~ "NOT ASSESSABLE (model not fitted)",
      p_uncorrected < 0.05 & any_within_sex_contrast_nominally_sig ~
        sprintf("SUPPORTED: Group:Sex F(%s,%s) = %s, p = %s < 0.05",
                NumDF, DenDF, r3(F_value), pfmt(p_uncorrected)),
      p_uncorrected < 0.05 ~
        sprintf("Interaction significant (p = %s) but NO within-Sex contrast is nominally significant",
                pfmt(p_uncorrected)),
      any_within_sex_contrast_nominally_sig ~
        sprintf(paste0("NOT SUPPORTED: within-Sex contrast(s) nominally significant [%s] but the ",
                       "FORMAL Group:Sex interaction is NULL, F(%s,%s) = %s, p = %s. ",
                       "Sex-differential / 'female-specific' / 'male-specific' language is NOT ",
                       "licensed for this domain. A significant contrast in one sex plus a null ",
                       "contrast in the other is NOT evidence of a sex difference."),
                nominally_sig_cells, NumDF, DenDF, r3(F_value), pfmt(p_uncorrected)),
      TRUE ~ sprintf(paste0("NOT SUPPORTED and moot: interaction null (p = %s) and no within-Sex ",
                            "contrast is nominally significant"), pfmt(p_uncorrected))),
    interpretation_guard = paste0(GUARD, " SEX-DIFFERENTIAL LANGUAGE REQUIRES THIS INTERACTION: ",
      "a significant female contrast next to a null male contrast is NOT a sex difference."),
    significance_role = SIG_ROLE,
    model_formula = MODEL_FORMULA,
    model_engine = paste0("stats::lm + stats::anova (sequential SS; Group:Sex is the LAST term, ",
                          "so its SS equals the Type-III interaction SS regardless of term order)"),
    source_table = "first_night_10domain_scores.csv", source_script = UPSTREAM,
    script = THIS_SCRIPT) %>%
  select(row_id, Domain, candidate_status, bin_resolution, resolution_role, term,
         NumDF, DenDF, F_value, p_uncorrected, q_BH_across_10_interaction_tests,
         multiplicity_primary, multiplicity_treatment,
         any_within_sex_contrast_nominally_sig, n_nominally_sig_contrasts, nominally_sig_cells,
         female_only_sig_pattern, male_only_sig_pattern,
         sex_differential_language_supported, interaction_p_that_justifies_it,
         sex_differential_verdict, n_animals, model_status, model_formula, model_engine,
         interpretation_guard, significance_role, source_table, source_script, script) %>%
  arrange(bin_resolution != "10min_based", row_id)

write_csv(inter_out, file.path(OUT, "first_night_group_sex_interactions.csv"))
cat("WROTE", file.path(OUT, "first_night_group_sex_interactions.csv"), "rows:", nrow(inter_out), "\n")

for (res in RESOLUTIONS) {
  sec(sprintf("FORMAL Group:Sex INTERACTION  [%s]", res))
  print(as.data.frame(inter_out %>% filter(bin_resolution == res) %>%
    transmute(id = row_id, Domain = str_trunc(Domain, 40), NumDF, DenDF, F = r3(F_value),
              p = pfmt(p_uncorrected), q_BH10 = pfmt(q_BH_across_10_interaction_tests),
              n = n_animals, any_sig_contrast = any_within_sex_contrast_nominally_sig,
              sexdiff_OK = sex_differential_language_supported)), row.names = FALSE)
}
sec("EXPLICIT sex-differential verdicts for every domain with a nominally significant within-Sex contrast (10min PRIMARY)")
vv <- inter_out %>% filter(bin_resolution == "10min_based", any_within_sex_contrast_nominally_sig)
for (i in seq_len(nrow(vv))) {
  cat("\n  * ", vv$Domain[i], "\n", sep = "")
  cat("      nominally significant cells : ", vv$nominally_sig_cells[i], "\n", sep = "")
  cat("      Group:Sex interaction       : F(", vv$NumDF[i], ",", vv$DenDF[i], ") = ",
      r3(vv$F_value[i]), ", p = ", pfmt(vv$p_uncorrected[i]),
      " (BH q over 10 = ", pfmt(vv$q_BH_across_10_interaction_tests[i]), ")\n", sep = "")
  cat("      sex_differential_supported  : ", vv$sex_differential_language_supported[i], "\n", sep = "")
  cat("      verdict                     : ", vv$sex_differential_verdict[i], "\n", sep = "")
}
add_assert("sex-differential language is licensed by the formal interaction and nothing else",
           "check sex_differential_language_supported == (interaction p < 0.05) for every row",
           all(inter_out$sex_differential_language_supported ==
               (is.finite(inter_out$p_uncorrected) & inter_out$p_uncorrected < 0.05)),
           sprintf("10min: %s of %s domains show a ONE-SEX-ONLY nominal pattern; %s of those have a significant Group:Sex interaction",
                   sum(inter_out$bin_resolution == "10min_based" &
                       (inter_out$female_only_sig_pattern | inter_out$male_only_sig_pattern), na.rm = TRUE),
                   sum(inter_out$bin_resolution == "10min_based"),
                   sum(inter_out$bin_resolution == "10min_based" &
                       (inter_out$female_only_sig_pattern | inter_out$male_only_sig_pattern) &
                       inter_out$sex_differential_language_supported, na.rm = TRUE)))

## ==========================================================================
hr("STEP 4. OUT/first_night_multiplicity_sensitivity.csv  --  row sets x 2 FDR families")
## ==========================================================================
## ROW SETS. Membership is defined ONLY from structural facts established upstream -- algebraic
## identity, Spearman redundancy class, and HMM temporal-order invariance. NO p-value was consulted.
## Removal order for shrinking below 10: the candidates whose STRUCTURAL redundancy with an
## already-displayed row is highest go first (#8, since #8 = #2 + 0.5*Ea - 0.5*Ma EXACTLY and
## rho(#8,#2) = 0.927 near_duplicate; then #9, since #9 = #3 + 0.5*Pm - 0.5*Pa EXACTLY and
## rho(#9,#3) = 0.911; then #10, which is #1 - Pm EXACTLY, rho 0.668 with #1). Below 7 the removed
## row is #6, on the upstream structural finding that occupancy composition is EXACTLY invariant to
## shuffling the within-epoch state sequence. ALTERNATIVE compositions at sizes 6, 8 and 9 are ALSO
## reported, to show that BH depends on family MEMBERSHIP and not only on family SIZE.
ROW_SETS <- list(
  list(label = "set10_all_10_candidates", ids = 1:10,
       size_note = "all 10 audited candidates",
       basis = "the full audited candidate set"),
  list(label = "set09_drop_early_active_spatial_flexibility", ids = c(1:7, 9, 10),
       size_note = "10 minus #8",
       basis = "structural: #8 = #2 + 0.5*Ea - 0.5*Ma EXACTLY and rho(#8,#2) = 0.927 (near_duplicate)"),
  list(label = "set09_alt_drop_early_social_withdrawal", ids = 1:9,
       size_note = "10 minus #10 (ALTERNATIVE composition, same size)",
       basis = "structural alternative: #10 = #1 - Pm EXACTLY; included to prove BH depends on family MEMBERSHIP, not only size"),
  list(label = "set08_drop_8_and_9", ids = c(1:7, 10),
       size_note = "10 minus #8 and #9",
       basis = "structural: both #8 and #9 are exact affine offsets of displayed rows (rho 0.927 / 0.911)"),
  list(label = "set08_alt_keep_9_drop_8_and_10", ids = c(1:7, 9),
       size_note = "10 minus #8 and #10 (ALTERNATIVE composition, same size)",
       basis = "structural alternative at size 8"),
  list(label = "set07_current_displayed", ids = 1:7,
       size_note = "the CURRENT 7 displayed rows",
       basis = "the shipped Stage 14 first-night panel (rows 1-7)"),
  list(label = "set06_drop_latent_state_occupancy", ids = c(1:5, 7),
       size_note = "current 7 minus #6",
       basis = "structural: occupancy composition is EXACTLY invariant to shuffling the within-epoch state sequence (max change 0, r = 1.000000), so it carries NO temporal-order information"),
  list(label = "set06_alt_drop_latent_state_persistence", ids = 1:6,
       size_note = "current 7 minus #7 (ALTERNATIVE composition, same size)",
       basis = "structural alternative at size 6")
)

FAM_PRIMARY_DOC <- paste0(
  "q_PRIMARY = Benjamini-Hochberg WITHIN Sex, across (n_domains x 3 contrasts) tests. ",
  "This is the family the shipped Stage 14 heatmap uses and the one this audit regards as PRIMARY, ",
  "because the Female and Male simple-effect families answer separate within-sex questions and the ",
  "sex-differential question is answered by the separate Group:Sex gatekeeper, NOT by merging the ",
  "two within-Sex families into one screen.")
FAM_SENS_DOC <- paste0(
  "q_SENSITIVITY = Benjamini-Hochberg across BOTH sexes POOLED, over (n_domains x 3 contrasts x 2 ",
  "sexes) tests. Strictly more conservative; reported so the reader can see how much of the ",
  "surviving signal is a function of the family DEFINITION rather than of the data.")

eff_p <- effects_out %>% filter(!is.na(raw_p))

multi <- map_dfr(ROW_SETS, function(rs) {
  doms <- names(ROWID)[ROWID %in% rs$ids]
  map_dfr(RESOLUTIONS, function(res) {
    e <- eff_p %>% filter(bin_resolution == res, Domain %in% doms)
    n_dom <- n_distinct(e$Domain)
    e %>% group_by(Sex) %>%
      mutate(q_PRIMARY = p.adjust(raw_p, method = "BH"),
             family_id_PRIMARY = sprintf("BH_withinSex|%s|%s|%s", rs$label, res, first(Sex)),
             n_tests_in_family_PRIMARY = n()) %>% ungroup() %>%
      mutate(q_SENSITIVITY = p.adjust(raw_p, method = "BH"),
             family_id_SENSITIVITY = sprintf("BH_pooledSex|%s|%s", rs$label, res),
             n_tests_in_family_SENSITIVITY = n(),
             row_set_label = rs$label, row_set_size = n_dom,
             row_set_membership_note = rs$size_note, row_set_basis = rs$basis,
             row_set_domains = paste(sort(unique(ROWID[doms])), collapse = "+"))
  })
})

multi_out <- multi %>%
  mutate(family_definition_PRIMARY = FAM_PRIMARY_DOC,
         family_definition_SENSITIVITY = FAM_SENS_DOC,
         which_family_is_primary = "q_PRIMARY (BH within Sex)",
         estimate_ci_invariance_note = paste0(
           "estimate, SE, df, ci_low and ci_high are IDENTICAL across every row_set_label and ",
           "every family definition -- multiplicity changes ONLY the q column. Interpret the ",
           "effect from the estimate and CI, not from whether the star survives."),
         significance_role = SIG_ROLE, interpretation_guard = GUARD,
         source_table = "first_night_10domain_effects.csv", source_script = THIS_SCRIPT,
         script = THIS_SCRIPT) %>%
  select(row_set_label, row_set_size, row_set_membership_note, row_set_basis, row_set_domains,
         bin_resolution, resolution_role, row_id, Domain, candidate_status, Sex, contrast,
         n_ref, n_comp, hedges_g, estimate, SE, df, ci_low, ci_high, ci_formula, raw_p,
         q_PRIMARY, family_id_PRIMARY, n_tests_in_family_PRIMARY,
         q_SENSITIVITY, family_id_SENSITIVITY, n_tests_in_family_SENSITIVITY,
         family_definition_PRIMARY, family_definition_SENSITIVITY, which_family_is_primary,
         estimate_ci_invariance_note, significance_role, interpretation_guard,
         source_table, source_script, script) %>%
  arrange(row_set_size, row_set_label, bin_resolution != "10min_based", Sex, row_id, contrast)

write_csv(multi_out, file.path(OUT, "first_night_multiplicity_sensitivity.csv"))
cat("WROTE", file.path(OUT, "first_night_multiplicity_sensitivity.csv"), "rows:", nrow(multi_out), "\n")

sec("family sizes per row set (10min primary)")
print(as.data.frame(multi_out %>% filter(bin_resolution == "10min_based") %>%
  distinct(row_set_label, row_set_size, row_set_domains, Sex, n_tests_in_family_PRIMARY,
           n_tests_in_family_SENSITIVITY) %>%
  arrange(row_set_size, row_set_label, Sex)), row.names = FALSE)

sec("SURVIVORS at q < 0.05, by row set and family (10min primary) -- REPORTED, NOT used to decide")
surv <- multi_out %>% filter(bin_resolution == "10min_based") %>%
  group_by(row_set_label, row_set_size) %>%
  summarise(n_raw_p_lt_05 = sum(raw_p < 0.05),
            n_qPRIMARY_lt_05 = sum(q_PRIMARY < 0.05),
            n_qSENSITIVITY_lt_05 = sum(q_SENSITIVITY < 0.05),
            survivors_PRIMARY = paste(sprintf("%s %s %s", Sex, str_trunc(Domain, 28),
                                              contrast)[q_PRIMARY < 0.05], collapse = " | "),
            .groups = "drop") %>% arrange(row_set_size, row_set_label)
print(as.data.frame(surv), row.names = FALSE)

sec("full q table for every cell with raw_p < 0.05 at 10min, across all row sets")
print(as.data.frame(multi_out %>% filter(bin_resolution == "10min_based", raw_p < 0.05) %>%
  transmute(row_set = str_trunc(row_set_label, 40), k = row_set_size, Sex,
            Domain = str_trunc(Domain, 32), contrast, g = r3(hedges_g), est = r3(estimate),
            p = pfmt(raw_p), qP = r4(q_PRIMARY), nP = n_tests_in_family_PRIMARY,
            qS = r4(q_SENSITIVITY), nS = n_tests_in_family_SENSITIVITY) %>%
  arrange(Sex, Domain, contrast, k)), row.names = FALSE)

## ==========================================================================
hr("STEP 5. OUT/first_night_volatility_multiplicity_focus.csv")
## ==========================================================================
FOCUS_DOM <- "Behavioral volatility / fragmentation"
focus_base <- multi_out %>% filter(Domain == FOCUS_DOM, Sex == "Female", contrast == "RES-CON")
f10 <- focus_base %>% filter(bin_resolution == "10min_based")
INVAR_STMT <- paste0(
  "The ESTIMATE and the 95% CI are UNCHANGED by family choice and by row-set size: only q moves. ",
  "10min primary: estimate = ", r4(f10$estimate[1]), ", SE = ", r4(f10$SE[1]),
  ", 95% CI [", r4(f10$ci_low[1]), ", ", r4(f10$ci_high[1]), "], Hedges g = ",
  r4(f10$hedges_g[1]), ", raw p = ", pfmt(f10$raw_p[1]),
  ". Interpret this effect from the estimate and CI, INDEPENDENTLY of whether the star survives ",
  "any particular multiplicity family.")

focus <- focus_base %>%
  mutate(target_note = paste0("FOCUS CELL: Female '", FOCUS_DOM, "' RES-CON -- the ONLY FDR ",
                              "survivor in the shipped 7x3 within-Sex family at first night."),
         estimate_unchanged_by_family = TRUE,
         estimate_ci_invariance_statement = INVAR_STMT,
         q_survives_05_PRIMARY = q_PRIMARY < 0.05,
         q_survives_05_SENSITIVITY = q_SENSITIVITY < 0.05) %>%
  select(row_set_label, row_set_size, row_set_membership_note, bin_resolution, resolution_role,
         row_id, Domain, Sex, contrast, n_ref, n_comp, hedges_g, estimate, SE, df,
         ci_low, ci_high, ci_formula, raw_p,
         q_PRIMARY, n_tests_in_family_PRIMARY, family_id_PRIMARY, q_survives_05_PRIMARY,
         q_SENSITIVITY, n_tests_in_family_SENSITIVITY, family_id_SENSITIVITY,
         q_survives_05_SENSITIVITY,
         family_definition_PRIMARY, family_definition_SENSITIVITY,
         target_note, estimate_unchanged_by_family, estimate_ci_invariance_statement,
         significance_role, interpretation_guard, source_script, script) %>%
  arrange(bin_resolution != "10min_based", row_set_size, row_set_label)

write_csv(focus, file.path(OUT, "first_night_volatility_multiplicity_focus.csv"))
cat("WROTE", file.path(OUT, "first_night_volatility_multiplicity_focus.csv"), "rows:", nrow(focus), "\n")

sec("FOCUS: Female 'Behavioral volatility / fragmentation' RES-CON under every row set")
print(as.data.frame(focus %>% transmute(row_set = str_trunc(row_set_label, 42), k = row_set_size,
  res = ifelse(bin_resolution == "10min_based", "10min", "5min"),
  g = r3(hedges_g), est = r3(estimate), CI = sprintf("[%s, %s]", r3(ci_low), r3(ci_high)),
  p = pfmt(raw_p), qP = r4(q_PRIMARY), nP = n_tests_in_family_PRIMARY,
  survP = q_survives_05_PRIMARY, qS = r4(q_SENSITIVITY), nS = n_tests_in_family_SENSITIVITY,
  survS = q_survives_05_SENSITIVITY)), row.names = FALSE)
cat("\n", INVAR_STMT, "\n", sep = "")

add_assert("estimate / SE / CI of the focus cell are invariant across all row sets and both families",
           "n_distinct of estimate, SE, ci_low, ci_high within resolution for the focus cell",
           n_distinct(f10$estimate) == 1L && n_distinct(f10$SE) == 1L &&
             n_distinct(f10$ci_low) == 1L && n_distinct(f10$ci_high) == 1L,
           sprintf("10min: %s row sets, %s distinct estimate(s), %s distinct CI(s), but %s distinct q_PRIMARY value(s) -- multiplicity moves ONLY q",
                   nrow(f10), n_distinct(f10$estimate), n_distinct(f10$ci_low),
                   n_distinct(f10$q_PRIMARY)))

## ==========================================================================
hr("STEP 6. Resolution agreement per candidate x Sex x contrast")
## ==========================================================================
res_agree <- effects_out %>%
  select(row_id, Domain, candidate_status, Sex, contrast, bin_resolution,
         estimate, SE, ci_low, ci_high, raw_p, hedges_g) %>%
  pivot_wider(names_from = bin_resolution,
              values_from = c(estimate, SE, ci_low, ci_high, raw_p, hedges_g)) %>%
  mutate(
    same_sign = sign(estimate_10min_based) == sign(estimate_5min_based),
    ci_overlap = (ci_low_10min_based <= ci_high_5min_based) &
                 (ci_low_5min_based  <= ci_high_10min_based),
    abs_ratio_5min_over_10min = abs(estimate_5min_based) / abs(estimate_10min_based),
    est_diff_5min_minus_10min = estimate_5min_based - estimate_10min_based,
    both_nominally_sig = raw_p_10min_based < 0.05 & raw_p_5min_based < 0.05,
    sig_status_agrees = (raw_p_10min_based < 0.05) == (raw_p_5min_based < 0.05),
    agreement_verdict = case_when(
      same_sign & ci_overlap ~ "AGREES (same sign, CIs overlap)",
      same_sign & !ci_overlap ~ "same sign but CIs do NOT overlap",
      !same_sign & ci_overlap ~ "SIGN FLIP but CIs overlap (both near zero)",
      TRUE ~ "SIGN FLIP and CIs do NOT overlap -- resolution-unstable"),
    ci_overlap_note = paste0("CI overlap is a DESCRIPTIVE consistency check between the primary ",
                             "and sensitivity resolution, not a test of their difference."),
    script = THIS_SCRIPT, source_table = "first_night_10domain_effects.csv",
    significance_role = SIG_ROLE, interpretation_guard = GUARD) %>%
  arrange(row_id, Sex, contrast)

write_csv(res_agree, file.path(OUT, "first_night_10domain_resolution_agreement.csv"))
cat("WROTE", file.path(OUT, "first_night_10domain_resolution_agreement.csv"),
    "rows:", nrow(res_agree), "\n")

for (sx in SEX_LEVELS) {
  sec(sprintf("RESOLUTION AGREEMENT [%s]  10min (primary) vs 5min (sensitivity)", sx))
  print(as.data.frame(res_agree %>% filter(Sex == sx) %>%
    transmute(id = row_id, Domain = str_trunc(Domain, 34), contrast,
              est10 = r3(estimate_10min_based), est5 = r3(estimate_5min_based),
              ratio = r3(abs_ratio_5min_over_10min), same_sign, ci_ovl = ci_overlap,
              p10 = pfmt(raw_p_10min_based), p5 = pfmt(raw_p_5min_based),
              verdict = str_trunc(agreement_verdict, 32))), row.names = FALSE)
}
sec("resolution agreement summary")
print(as.data.frame(res_agree %>% group_by(Sex) %>%
  summarise(n = n(), n_same_sign = sum(same_sign), n_ci_overlap = sum(ci_overlap),
            n_both = sum(same_sign & ci_overlap), n_sig_status_agrees = sum(sig_status_agrees),
            median_abs_ratio = r3(median(abs_ratio_5min_over_10min)),
            min_ratio = r3(min(abs_ratio_5min_over_10min)),
            max_ratio = r3(max(abs_ratio_5min_over_10min)), .groups = "drop")), row.names = FALSE)
add_assert("resolution agreement is reported for every candidate x Sex x contrast",
           "nrow of the agreement table vs 11 domains x 2 sexes x 3 contrasts",
           nrow(res_agree) == 11L * 2L * 3L,
           sprintf("%s rows; %s/%s same sign, %s/%s CIs overlap, %s/%s both",
                   nrow(res_agree), sum(res_agree$same_sign), nrow(res_agree),
                   sum(res_agree$ci_overlap), nrow(res_agree),
                   sum(res_agree$same_sign & res_agree$ci_overlap), nrow(res_agree)))

## ==========================================================================
hr("STEP 7. Locomotion dominance per candidate x Sex (|Spearman rho| >= 0.70 with #1)")
## ==========================================================================
wide <- scores %>% select(AnimalNum, Group, Sex, bin_resolution, Domain, DomainScore) %>%
  pivot_wider(names_from = Domain, values_from = DomainScore)

loco <- pmap_dfr(expand_grid(Domain = ALL_DOMS, resolution = RESOLUTIONS,
                             stratum = c("pooled", SEX_LEVELS)),
  function(Domain, resolution, stratum) {
    dom <- Domain; res <- resolution; st <- stratum
    d <- wide %>% filter(bin_resolution == res)
    if (st != "pooled") d <- d %>% filter(Sex == st)
    x <- d[["Psychomotor activation"]]; y <- d[[dom]]
    ok <- is.finite(x) & is.finite(y)
    rho <- safe_cor(x, y, "spearman")
    tibble(row_id = ROWID[dom], Domain = dom, candidate_status = CAND_STATUS[dom],
           bin_resolution = res, resolution_role = RES_ROLE[res], stratum = st,
           n = sum(ok), spearman_rho_with_psychomotor_activation = rho,
           spearman_p = safe_cor_p(x, y, "spearman"),
           pearson_r = safe_cor(x, y, "pearson"),
           locomotion_dominance_threshold = LOCO_FLAG,
           locomotion_dominated = is.finite(rho) & abs(rho) >= LOCO_FLAG)
  }) %>%
  mutate(flag_rule = paste0("repo flag: |Spearman rho| >= ", LOCO_FLAG,
                            " against 'Psychomotor activation' (= Movement_mean_z). ",
                            "Domain #1 IS Movement_mean_z, so its self-correlation is 1 by construction."),
         no_formal_sex_difference_test_note = paste0(
           "A rho that clears the flag in one Sex stratum and not the other is NOT a ",
           "sex-differential finding: NO formal test of the Female-vs-Male difference between ",
           "these correlations was performed. Sex-differential language requires the Group:Sex ",
           "interaction (see first_night_group_sex_interactions.csv)."),
         significance_role = SIG_ROLE, interpretation_guard = GUARD,
         source_table = "first_night_10domain_scores.csv", script = THIS_SCRIPT) %>%
  arrange(bin_resolution != "10min_based", row_id, stratum)

write_csv(loco, file.path(OUT, "first_night_10domain_locomotion_dominance.csv"))
cat("WROTE", file.path(OUT, "first_night_10domain_locomotion_dominance.csv"), "rows:", nrow(loco), "\n")

for (res in RESOLUTIONS) {
  sec(sprintf("LOCOMOTION DOMINANCE [%s]  Spearman rho vs Psychomotor activation", res))
  print(as.data.frame(loco %>% filter(bin_resolution == res) %>%
    transmute(row_id, Domain = str_trunc(Domain, 40), stratum, n,
              rho = r3(spearman_rho_with_psychomotor_activation),
              flagged = locomotion_dominated) %>%
    pivot_wider(names_from = stratum, values_from = c(rho, n, flagged))), row.names = FALSE)
}
sec("candidates FLAGGED as locomotion-dominated (|rho| >= 0.70), any stratum, excluding #1 itself")
lf <- loco %>% filter(locomotion_dominated, row_id != 1L)
if (nrow(lf) == 0) {
  cat("  none besides domain #1 itself (self-correlation = 1 by construction)\n")
} else {
  print(as.data.frame(lf %>% transmute(row_id, Domain = str_trunc(Domain, 40), bin_resolution,
                                       stratum, n,
                                       rho = r3(spearman_rho_with_psychomotor_activation))),
        row.names = FALSE)
  cat("\n  GUARD: ", unique(lf$no_formal_sex_difference_test_note)[1], "\n", sep = "")
}
add_assert("locomotion dominance is evaluated for all candidates in all 3 strata at both resolutions",
           "count of rows and of flagged non-self rows",
           nrow(loco) == length(ALL_DOMS) * 2L * 3L,
           sprintf("%s rows; flagged (excluding #1 self): %s -> %s", nrow(loco), nrow(lf),
                   if (nrow(lf) == 0) "none" else
                     paste(unique(sprintf("#%s %s [%s|%s]", lf$row_id, str_trunc(lf$Domain, 26),
                                          lf$stratum, lf$bin_resolution)), collapse = "; ")))

## ==========================================================================
hr("STEP 8. ASSERTION REGISTER")
## ==========================================================================
areg <- bind_rows(ASSERT) %>% mutate(script = THIS_SCRIPT)
print(as.data.frame(areg %>% transmute(result, assertion = str_trunc(assertion, 96))), row.names = FALSE)
cat("\nPASS:", sum(areg$result == "PASS"), " FAIL:", sum(areg$result == "FAIL"), "\n")
write_csv(areg, file.path(OUT, "first_night_candidate_set_effects_assertions.csv"))
cat("WROTE", file.path(OUT, "first_night_candidate_set_effects_assertions.csv"), "\n")
if (any(areg$result == "FAIL")) {
  cat("\n*** ONE OR MORE ASSERTIONS FAILED -- do not consume these tables ***\n")
  print(as.data.frame(areg %>% filter(result == "FAIL")), row.names = FALSE)
}
hr("DONE")
