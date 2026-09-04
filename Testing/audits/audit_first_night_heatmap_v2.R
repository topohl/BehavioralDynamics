## audit_first_night_heatmap_v2.R
## ===========================================================================
## FIRST-NIGHT (CC1, canonical experimental-CLOCK window) inference + figures.
##
## CONSUMES (does not rebuild) the v2 score/feature tables written by
##   Testing/audits/audit_first_night_domain_scores_v2.R
## which are themselves built on THE ONE canonical window:
##   * first cage change (CC1) only
##   * Phase EXACTLY in c("active","dark","night") after lower+trim (never a substring
##     regex -- "inactive" contains "active")
##   * per SESSION (SourceFile): target_phase_block = min(animalpos_phase_block_index(BinStart));
##     target_window_start = target_phase_block * 43200 + 23400  (i.e. 18:30 clock)
##   * keep difftime(BinStart, target_window_start, "secs") in [0, 12*3600)  -> 18:30 -> 06:30
##   The anchor is a property of the experimental CLOCK, not of any animal.
##
## WHY NOT the production Stage 14 rule `local_bin <= 12h/bin`:
##   that fixed BIN COUNT matches the clock window for only 50/111 animals at 10-min and
##   33/111 at 5-min, because missing night-1 bins push the count into the SECOND dark block.
##
## WHAT THIS SCRIPT ADDS
##   1. first_night_domain_effect_summary.csv        (displayed domains x Sex x contrast)
##      first_night_domain_interaction_tests.csv     (Group:Sex anova, separate, uncorrected)
##   2. first_night_hmm_component_results.csv        (8 HMM components, own exploratory family)
##      first_night_hmm_component_interactions.csv
##      first_night_hmm_movement_adjustment.csv      (construct diagnostic, NOT primary)
##      first_night_hmm_combz_association.csv        (exploratory, NOT prediction)
##   3. Fig_first_night_domain_heatmap.{svg,pdf}
##      Fig_first_night_hmm_components.{svg,pdf}
##   4. first_night_heatmap_readme.md
##   5. first_night_vs_production_cc1_reconciliation.csv
##
## INTERPRETATION GUARDS (enforced in every label, title and column)
##   * RFID proximity = social-spatial CO-LOCATION proxy, NEVER "sociability".
##   * Occupancy composition carries NO temporal-order information -> "Latent-state occupancy
##     organization", NEVER "temporal flexibility".
##   * RES/SUS are LATER phenotype labels derived from subsequent CombZ. Every first-night
##     contrast is a DESCRIPTIVE association with later phenotype. Never prospective, never causal.
##   * ONE persistence metric only (mean_dwell_minutes) in the domain heatmap.
##   * top_proximity_state_fraction is computed but NEVER displayed (failed partition robustness).
##   * No row is added because it is significant or dropped because it is null; no resolution is
##     chosen on p-values; the shipped composite coefficient 0.5 is KEPT.
##
## READ-ONLY w.r.t. Analysis/ and Functions/ and w.r.t. every production table/figure.
## Writes only into <STAGE14>/audit_hmm_state_architecture/first_night_domain_heatmap/
## ===========================================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(purrr); library(tibble); library(ggplot2); library(readxl)
})

setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
stopifnot(requireNamespace("emmeans", quietly = TRUE))

PROJ    <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
STAGE14 <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based")
OUT     <- file.path(STAGE14, "audit_hmm_state_architecture/first_night_domain_heatmap")
DERIV   <- file.path(PROJ, "analysis_ready/03_derived_metrics")
PROD_CC1 <- file.path(STAGE14, "tables/sis_CC1_first_active_domain_contrasts.csv")
COMBZ_XLSX <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

THIS_SCRIPT <- "Testing/audits/audit_first_night_heatmap_v2.R"
UPSTREAM    <- "Testing/audits/audit_first_night_domain_scores_v2.R"
GROUP_LEVELS <- c("CON", "RES", "SUS")
SEX_LEVELS   <- c("Female", "Male")
RESOLUTIONS  <- c("10min_based", "5min_based")
RES_ROLE     <- c("10min_based" = "primary", "5min_based" = "sensitivity")
PRIMARY_RES  <- "10min_based"
CONTRAST_ORDER <- c("RES-CON", "SUS-CON", "SUS-RES")
CONTRASTS <- list("RES-CON" = c(-1, 1, 0), "SUS-CON" = c(-1, 0, 1), "SUS-RES" = c(0, -1, 1))
REF_OF <- c("RES-CON" = "CON", "SUS-CON" = "CON", "SUS-RES" = "RES")
CMP_OF <- c("RES-CON" = "RES", "SUS-CON" = "SUS", "SUS-RES" = "SUS")

WINDOW_TEXT <- paste0("CC1 (first cage change), first Active/dark phase, experimental-clock ",
                      "anchored 18:30 -> 06:30 (exactly 12.0 h); one value per animal")
MODEL_FORMULA <- "DomainScore ~ Group * Sex"
MODEL_ENGINE  <- "stats::lm + emmeans::emmeans(~ Group | Sex) + emmeans::contrast(adjust = 'none')"
CI_METHOD     <- "emmeans 95% CI on the lm scale: estimate +/- qt(0.975, residual df) * SE; df = residual df of lm(~ Group * Sex)"
DESCRIPTIVE   <- paste0("DESCRIPTIVE association of a first-night measure with LATER phenotype ",
                        "label (RES/SUS derived from subsequent CombZ). NOT prospective, NOT causal, ",
                        "NOT prediction.")
STANDARDIZATION <- paste0("z within SEX ONLY: inside a single CC1 Active epoch no ",
                          "Sex x PhaseClass x CageChangeIndex context remains, so ",
                          "strict_standardize_within_context(group_cols = 'Sex')")

hr  <- function(x) cat("\n", strrep("=", 88), "\n", x, "\n", strrep("=", 88), "\n", sep = "")
sec <- function(x) cat("\n--- ", x, " ---\n", sep = "")
r4  <- function(x) round(x, 4)
pf  <- function(ok) if (isTRUE(ok)) "PASS" else "FAIL"
zsex <- function(dat, col) strict_standardize_within_context(dat, col, group_cols = "Sex")
safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = method))
}
stars_of <- function(q) ifelse(!is.finite(q), "",
                        ifelse(q < 0.001, "***", ifelse(q < 0.01, "**", ifelse(q < 0.05, "*", ""))))
## hard-wrap long caption / subtitle text so nothing runs off a 160 mm canvas
wrap_txt <- function(x, width) paste(strwrap(paste(x, collapse = " "), width = width), collapse = "\n")

ASSERT <- list()
add_assert <- function(assertion, method, ok, evidence) {
  ASSERT[[length(ASSERT) + 1L]] <<- tibble(assertion = assertion, method = method,
                                           result = pf(ok), evidence = evidence)
  cat("  [", pf(ok), "] ", assertion, "  ::  ", evidence, "\n", sep = "")
  invisible(ok)
}

## ==========================================================================
hr("STEP 0. Canonical roster + read the v2 upstream tables (read-only)")
## ==========================================================================
roster <- build_canonical_identity_roster(
  read_csv(file.path(DERIV, "5min_based/all_behavior_metrics.csv"),
           col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                            Group = col_character(), Sex = col_character()), progress = FALSE),
  "Stage 01 canonical roster (5min_based all_behavior_metrics.csv)")
cat("canonical roster animals:", nrow(roster), "\n")
print(as.data.frame(roster %>% count(Group, Sex) %>% arrange(Group, Sex)), row.names = FALSE)
add_assert("canonical roster is exactly 111 animals",
           "build_canonical_identity_roster() on Stage 01 5min_based all_behavior_metrics.csv",
           nrow(roster) == 111L, sprintf("nrow(roster) = %d", nrow(roster)))

scores <- read_csv(file.path(OUT, "first_night_domain_scores.csv"),
                   col_types = cols(AnimalNum = col_character(), .default = col_guess()),
                   progress = FALSE) %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum))
cat("first_night_domain_scores.csv rows =", nrow(scores),
    " domains =", n_distinct(scores$Domain), " animals =", n_distinct(scores$AnimalNum), "\n")
add_assert("every AnimalNum in first_night_domain_scores.csv is on the canonical roster",
           "canonical_animal_id() then setdiff against the 111-animal roster",
           length(setdiff(unique(scores$AnimalNum), roster$AnimalNum)) == 0,
           sprintf("animals in score table = %d; not on roster = %d",
                   n_distinct(scores$AnimalNum),
                   length(setdiff(unique(scores$AnimalNum), roster$AnimalNum))))
add_assert("score table Group/Sex agree with the canonical roster for every animal",
           "left_join on AnimalNum and compare Group and Sex",
           {gc <- scores %>% distinct(AnimalNum, Group, Sex) %>%
              inner_join(roster %>% select(AnimalNum, Group_r = Group, Sex_r = Sex), by = "AnimalNum")
            all(gc$Group == gc$Group_r) && all(gc$Sex == gc$Sex_r)},
           "no Group/Sex disagreement between the consumed score table and build_canonical_identity_roster()")
add_assert("the consumed score table is the CLOCK-window v2 table (not the local_bin count rule)",
           "window_start_rule + script columns of first_night_domain_scores.csv",
           all(str_detect(scores$window_start_rule, "43200 \\+ 23400")) &&
             all(str_detect(scores$script, "audit_first_night_domain_scores_v2")),
           sprintf("window_start_rule = '%s'; script = '%s'; supersedes = '%s'",
                   str_trunc(scores$window_start_rule[1], 130), scores$script[1],
                   str_trunc(scores$supersedes[1], 90)))

DOM_META <- scores %>% distinct(Domain, feature_origin, displayed, status, display_order) %>%
  arrange(display_order)
print(as.data.frame(DOM_META), row.names = FALSE)
DISPLAYED_DOMS <- DOM_META$Domain[DOM_META$displayed]
ALL_DOMS       <- DOM_META$Domain
cat("displayed domains:", length(DISPLAYED_DOMS), " | all candidate domains:", length(ALL_DOMS), "\n")

hfeat <- read_csv(file.path(OUT, "first_night_hmm_component_features_v2.csv"),
                  col_types = cols(AnimalNum = col_character(), .default = col_guess()),
                  progress = FALSE) %>%
  mutate(AnimalNum = canonical_animal_id(AnimalNum))
cat("first_night_hmm_component_features_v2.csv rows =", nrow(hfeat),
    " animals per resolution:", paste(hfeat %>% count(resolution) %>% pull(n), collapse = "/"), "\n")
sem <- read_csv(file.path(OUT, "first_night_hmm_state_semantics_v2.csv"), col_types = cols(),
                progress = FALSE)
sec("Common (group-blind, longitudinal) HMM state space -- nothing refitted here")
print(as.data.frame(sem %>% select(any_of(c("resolution", "State", "Movement_z", "Entropy_z",
                                            "Proximity_z", "SemanticState", "is_inactive_semantic",
                                            "is_argmax_proximity")))), row.names = FALSE)
add_assert("no HMM was refitted in this script",
           "depmixS4 namespace never loaded; latent states arrive only via the v2 component-feature table",
           !("depmixS4" %in% loadedNamespaces()),
           sprintf("depmixS4 loaded = %s; component features read from first_night_hmm_component_features_v2.csv",
                   "depmixS4" %in% loadedNamespaces()))
add_assert("social_state_fraction is identically 0 at both resolutions",
           "all(social_state_fraction == 0) on the consumed component features",
           all(hfeat$social_state_fraction == 0),
           sprintf(paste0("max social_state_fraction = %s; the 'social' semantic state category is EMPTY, ",
                          "which is exactly why the shipped composite reduces to 0.5*z(H) - z(inactive)"),
                   format(max(hfeat$social_state_fraction))))

## ==========================================================================
hr("STEP 1. Displayed-domain contrasts: lm(DomainScore ~ Group * Sex) + emmeans")
## ==========================================================================
## ONE value per animal per domain -> plain lm, NO random effect, NO repeated measures.
## Bins / states / transitions are NEVER treated as independent observations.
fit_domain <- function(dd, dom, res) {
  m <- dd %>% filter(Domain == dom, is.finite(DomainScore)) %>%
    mutate(Group = factor(Group, GROUP_LEVELS), Sex = factor(Sex, SEX_LEVELS))
  if (n_distinct(m$Group) < 3 || n_distinct(m$Sex) < 2) return(NULL)
  warns <- character(0)
  fit <- NULL
  ct <- withCallingHandlers({
    fit <- stats::lm(DomainScore ~ Group * Sex, data = m)
    em  <- emmeans::emmeans(fit, ~ Group | Sex)
    as.data.frame(summary(emmeans::contrast(em, method = CONTRASTS, adjust = "none"),
                          infer = c(TRUE, TRUE)))
  }, warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  ct %>% as_tibble() %>%
    transmute(bin_resolution = res, Domain = dom, Sex = as.character(Sex),
              contrast = as.character(contrast), model_estimate = estimate, SE = SE, df = df,
              CI_low = lower.CL, CI_high = upper.CL, t_ratio = t.ratio, raw_p = p.value) %>%
    rowwise() %>%
    mutate(group_ref = REF_OF[[contrast]], group_comp = CMP_OF[[contrast]],
           n_ref  = sum(as.character(m$Group) == group_ref  & as.character(m$Sex) == Sex),
           n_comp = sum(as.character(m$Group) == group_comp & as.character(m$Sex) == Sex),
           mean_ref  = mean(m$DomainScore[as.character(m$Group) == group_ref  & as.character(m$Sex) == Sex]),
           mean_comp = mean(m$DomainScore[as.character(m$Group) == group_comp & as.character(m$Sex) == Sex]),
           Hedges_g = hmm_hedges_g(m$DomainScore[as.character(m$Group) == group_ref  & as.character(m$Sex) == Sex],
                                   m$DomainScore[as.character(m$Group) == group_comp & as.character(m$Sex) == Sex])) %>%
    ungroup() %>%
    mutate(n_animals_in_model = nrow(m), df_resid = fit$df.residual,
           model_status = if (length(warns)) "fitted_with_warnings" else "fitted_clean",
           model_warnings = if (length(warns)) paste(unique(warns), collapse = " | ") else "none")
}

eff <- map_dfr(RESOLUTIONS, function(res) {
  dd <- scores %>% filter(bin_resolution == res)
  map_dfr(DISPLAYED_DOMS, ~ fit_domain(dd, .x, res))
}) %>%
  left_join(DOM_META, by = "Domain") %>%
  mutate(FDR_family = if_else(bin_resolution == PRIMARY_RES,
                              sprintf("FIRST_NIGHT__%s__displayed_domains_x_3_contrasts", Sex),
                              sprintf("FIRST_NIGHT_5MIN_SENSITIVITY__%s__displayed_domains_x_3_contrasts", Sex))) %>%
  group_by(FDR_family) %>%
  mutate(q = p.adjust(raw_p, "BH"), n_tests_in_family = sum(is.finite(raw_p))) %>%
  ungroup() %>%
  mutate(contrast = factor(contrast, CONTRAST_ORDER),
         resolution_role = RES_ROLE[bin_resolution],
         ci_method = CI_METHOD, model_formula = MODEL_FORMULA, model_engine = MODEL_ENGINE,
         inferential_unit = paste0("one animal = one value; plain lm, no random effect, no repeated ",
                                   "measures; bins/states/transitions never treated as independent"),
         standardization = STANDARDIZATION, window = WINDOW_TEXT,
         effect_size_definition = paste0("animal-level Hedges g via hmm_hedges_g(ref, comp) on the same ",
                                         "one-value-per-animal domain scores; sign follows comp - ref"),
         fdr_note = paste0("BH within bin_resolution x Sex over DISPLAYED domains x 3 contrasts. The 10-min ",
                           "primary and 5-min sensitivity resolutions are SEPARATE families; the sensitivity ",
                           "family is never used to reinterpret the primary one and resolution was never ",
                           "chosen on p-values."),
         interpretation = DESCRIPTIVE,
         source_table = file.path(OUT, "first_night_domain_scores.csv"),
         upstream_script = UPSTREAM, script = THIS_SCRIPT) %>%
  arrange(bin_resolution, display_order, Sex, contrast) %>%
  select(Domain, Sex, contrast, n_ref, n_comp, mean_ref, mean_comp, Hedges_g, model_estimate, SE,
         CI_low, CI_high, ci_method, raw_p, q, FDR_family, n_tests_in_family, model_formula,
         model_engine, model_status, model_warnings, bin_resolution, resolution_role, df, df_resid,
         t_ratio, n_animals_in_model, group_ref, group_comp, display_order, status,
         feature_origin, inferential_unit, standardization, window, effect_size_definition,
         fdr_note, interpretation, source_table, upstream_script, script)
write_csv(eff, file.path(OUT, "first_night_domain_effect_summary.csv"))
cat("wrote first_night_domain_effect_summary.csv  rows =", nrow(eff), "\n")

## independent reproduction check against the upstream contrast table
up <- read_csv(file.path(OUT, "first_night_domain_contrasts_v2.csv"), col_types = cols(),
               progress = FALSE) %>%
  filter(displayed) %>% select(bin_resolution, Domain, Sex, contrast, estimate, hedges_g, p_value)
cmp <- eff %>% mutate(contrast = as.character(contrast)) %>%
  inner_join(up, by = c("bin_resolution", "Domain", "Sex", "contrast"))
add_assert("re-fitted contrasts reproduce the upstream v2 contrast table exactly",
           "value-by-value comparison of estimate / Hedges g / raw p against first_night_domain_contrasts_v2.csv",
           nrow(cmp) == nrow(eff) &&
             max(abs(cmp$model_estimate - cmp$estimate)) < 1e-12 &&
             max(abs(cmp$Hedges_g - cmp$hedges_g), na.rm = TRUE) < 1e-12 &&
             max(abs(cmp$raw_p - cmp$p_value)) < 1e-12,
           sprintf("%d/%d rows matched; max|d estimate| = %s, max|d g| = %s, max|d p| = %s",
                   nrow(cmp), nrow(eff),
                   format(max(abs(cmp$model_estimate - cmp$estimate)), scientific = TRUE),
                   format(max(abs(cmp$Hedges_g - cmp$hedges_g), na.rm = TRUE), scientific = TRUE),
                   format(max(abs(cmp$raw_p - cmp$p_value)), scientific = TRUE)))

sec("PRIMARY 10-min displayed-domain contrasts (descriptive association with LATER phenotype)")
print(as.data.frame(eff %>% filter(bin_resolution == PRIMARY_RES) %>%
  transmute(Domain = str_trunc(Domain, 34), Sex, contrast, n = sprintf("%d/%d", n_ref, n_comp),
            g = r4(Hedges_g), est = r4(model_estimate), SE = r4(SE),
            CI = sprintf("[%.2f,%.2f]", CI_low, CI_high), p = signif(raw_p, 3), q = signif(q, 3)) %>%
  arrange(Sex, contrast, Domain)), row.names = FALSE)

sec("5-min SENSITIVITY displayed-domain contrasts (separate FDR family)")
print(as.data.frame(eff %>% filter(bin_resolution == "5min_based") %>%
  transmute(Domain = str_trunc(Domain, 34), Sex, contrast, g = r4(Hedges_g),
            est = r4(model_estimate), p = signif(raw_p, 3), q = signif(q, 3)) %>%
  arrange(Sex, contrast, Domain)), row.names = FALSE)

sec("Cells surviving BH q < 0.05 in the FIRST-NIGHT displayed-domain families")
srv <- eff %>% filter(q < 0.05)
if (nrow(srv) == 0) cat("  NONE at either resolution.\n") else
  print(as.data.frame(srv %>% transmute(bin_resolution, Domain = str_trunc(Domain, 38), Sex, contrast,
                                        g = r4(Hedges_g), est = r4(model_estimate),
                                        CI = sprintf("[%.2f,%.2f]", CI_low, CI_high),
                                        p = signif(raw_p, 3), q = signif(q, 3),
                                        n = sprintf("%d/%d", n_ref, n_comp))), row.names = FALSE)

sec("Largest |Hedges g| per Sex, 10-min primary (effect size irrespective of q)")
print(as.data.frame(eff %>% filter(bin_resolution == PRIMARY_RES) %>% group_by(Sex) %>%
  arrange(desc(abs(Hedges_g)), .by_group = TRUE) %>% slice_head(n = 6) %>%
  transmute(Sex, Domain = str_trunc(Domain, 36), contrast, g = r4(Hedges_g),
            p = signif(raw_p, 3), q = signif(q, 3))), row.names = FALSE)

## ---- Group:Sex interaction, SEPARATE table, explicit multiplicity ---------
inter <- map_dfr(RESOLUTIONS, function(res) {
  dd <- scores %>% filter(bin_resolution == res)
  map_dfr(ALL_DOMS, function(dom) {
    m <- dd %>% filter(Domain == dom, is.finite(DomainScore)) %>%
      mutate(Group = factor(Group, GROUP_LEVELS), Sex = factor(Sex, SEX_LEVELS))
    if (n_distinct(m$Group) < 3 || n_distinct(m$Sex) < 2) return(NULL)
    av <- stats::anova(stats::lm(DomainScore ~ Group * Sex, data = m))
    tibble(bin_resolution = res, Domain = dom, term = "Group:Sex",
           df_num = av["Group:Sex", "Df"], df_den = av["Residuals", "Df"],
           sum_sq = av["Group:Sex", "Sum Sq"], mean_sq = av["Group:Sex", "Mean Sq"],
           F_value = av["Group:Sex", "F value"], p_value = av["Group:Sex", "Pr(>F)"],
           n_animals = nrow(m))
  })
}) %>% left_join(DOM_META, by = "Domain") %>%
  mutate(resolution_role = RES_ROLE[bin_resolution],
         model_formula = MODEL_FORMULA,
         model_engine = "stats::anova(stats::lm(...)) sequential (Type I) SS",
         multiplicity_treatment = paste0("UNCORRECTED. Exactly ONE Group:Sex omnibus test per domain per ",
                                         "resolution (", length(ALL_DOMS), " candidate domains). Reported in a ",
                                         "SEPARATE table and NOT a member of the ",
                                         "FIRST_NIGHT__<Sex>__displayed_domains_x_3_contrasts BH family; ",
                                         "no q value is claimed for it."),
         note = paste0("Screens whether the Group effect differs by Sex. The primary contrasts are estimated ",
                       "within Sex regardless of this test; the interaction was NOT used to decide whether to ",
                       "split by Sex (the design is Sex-stratified a priori)."),
         interpretation = DESCRIPTIVE, window = WINDOW_TEXT, script = THIS_SCRIPT) %>%
  arrange(bin_resolution, display_order)
write_csv(inter, file.path(OUT, "first_night_domain_interaction_tests.csv"))
cat("wrote first_night_domain_interaction_tests.csv  rows =", nrow(inter), "\n")
sec("Group:Sex interaction, 10-min primary (UNCORRECTED, one test per domain)")
print(as.data.frame(inter %>% filter(bin_resolution == PRIMARY_RES) %>%
  transmute(Domain = str_trunc(Domain, 42), F = r4(F_value), p = signif(p_value, 3),
            n = n_animals, displayed)), row.names = FALSE)

## ==========================================================================
hr("STEP 2. HMM component scan on the SAME canonical window (EXPLORATORY)")
## ==========================================================================
COMPONENTS <- tribble(
  ~component, ~component_group, ~raw_unit, ~displayed, ~component_status, ~component_guard,
  "occupancy_entropy", "occupancy composition", "nats", TRUE, "displayed",
    paste0("Shannon entropy of latent-state occupancy. Order-FREE: shuffling the Viterbi sequence within ",
           "the window leaves it EXACTLY unchanged (max change 0, r = 1.000000). Never call this temporal flexibility."),
  "latent_state_occupancy_organization", "occupancy composition", "z-composite (SD units)", TRUE, "displayed",
    paste0("Shipped Stage 14 composite: 0.5 * z(occupancy_entropy) - z(inactive_state_fraction). The 0.5 ",
           "coefficient is KEPT because z(social_state_fraction) is identically 0, which makes this EXACTLY ",
           "equal to mean(z(H), z(social)) - z(inactive). Order-free."),
  "inactive_state_fraction", "occupancy composition", "fraction of bins", TRUE, "displayed",
    "Time share of the semantically inactive/low-exploration states. Order-free.",
  "top_proximity_state_fraction", "occupancy composition", "fraction of bins", FALSE,
    "excluded_failed_partition_robustness",
    paste0("EXCLUDED from every displayed panel: across 5 distinct 10-min HMM optima the argmax-proximity ",
           "state's Proximity_z is 0.190/0.748/1.615/2.880 and its occupancy 0.564/0.396/0.169/0.029 (19-fold); ",
           "animal-level agreement between optima falls to r = 0.117 (Active) and -0.157 (Inactive) and the ",
           "Female Inactive RES-CON sign flips. NEVER call it 'social'."),
  "self_transition_probability", "temporal organization", "probability", TRUE, "displayed",
    paste0("Occupancy-weighted probability of staying in the same state. EXACTLY 1 - state_switch_rate: ",
           "one construct, two rows."),
  "state_switch_rate", "temporal organization", "fraction of bin pairs", TRUE, "displayed",
    paste0("EXACTLY 1 - self_transition_probability (agreement to ~2e-16). Shown only so the mirror identity ",
           "is visible; it is NOT independent evidence."),
  "transition_entropy", "temporal organization", "nats", TRUE, "displayed",
    paste0("Occupancy-weighted entropy of the outgoing transition distribution: how unpredictable the NEXT ",
           "state is given the current one."),
  "mean_dwell_minutes", "temporal organization", "minutes", TRUE, "displayed",
    paste0("THE single persistence metric carried into the domain heatmap (as 'Latent-state persistence'). ",
           "Physical time, so 10-min and 5-min are comparable. Higher = longer runs = more persistent.")
)
NO_RESTANDARDIZE <- "latent_state_occupancy_organization"

comp_long <- map_dfr(RESOLUTIONS, function(res) {
  f <- hfeat %>% filter(resolution == res) %>%
    mutate(Group = factor(Group, GROUP_LEVELS), Sex = factor(Sex, SEX_LEVELS))
  fz <- reduce(c("occupancy_entropy", "inactive_state_fraction", "mean_dwell_minutes",
                 "self_transition_probability", "transition_entropy", "state_switch_rate",
                 "top_proximity_state_fraction"), zsex, .init = f) %>%
    mutate(latent_state_occupancy_organization = 0.5 * occupancy_entropy_z - inactive_state_fraction_z)
  ## modelled value = z within Sex, EXCEPT the composite which is ALREADY a z-composite
  map_dfr(COMPONENTS$component, function(cp) {
    tibble(bin_resolution = res, component = cp, AnimalNum = fz$AnimalNum,
           Group = fz$Group, Sex = fz$Sex,
           value_modelled = if (cp %in% NO_RESTANDARDIZE) fz[[cp]] else fz[[paste0(cp, "_z")]],
           value_raw = fz[[cp]])
  })
})
cat("component-long rows =", nrow(comp_long), " (", n_distinct(comp_long$component), "components x",
    n_distinct(comp_long$bin_resolution), "resolutions x", n_distinct(comp_long$AnimalNum), "animals )\n")

## assertions tying the components back to the two HMM DOMAINS in the score table
for (res in RESOLUTIONS) {
  a <- comp_long %>% filter(bin_resolution == res, component == "latent_state_occupancy_organization") %>%
    select(AnimalNum, comp_val = value_modelled) %>%
    inner_join(scores %>% filter(bin_resolution == res,
                                 Domain == "Latent-state occupancy organization") %>%
                 select(AnimalNum, DomainScore), by = "AnimalNum")
  b <- comp_long %>% filter(bin_resolution == res, component == "mean_dwell_minutes") %>%
    select(AnimalNum, comp_val = value_modelled) %>%
    inner_join(scores %>% filter(bin_resolution == res, Domain == "Latent-state persistence") %>%
                 select(AnimalNum, DomainScore), by = "AnimalNum")
  add_assert(sprintf("[%s] component 'latent_state_occupancy_organization' == displayed domain 'Latent-state occupancy organization'", res),
             "0.5*z(occupancy_entropy) - z(inactive_state_fraction) recomputed here vs the DomainScore in first_night_domain_scores.csv",
             max(abs(a$comp_val - a$DomainScore), na.rm = TRUE) < 1e-12,
             sprintf("n = %d, max|diff| = %s", nrow(a),
                     format(max(abs(a$comp_val - a$DomainScore), na.rm = TRUE), scientific = TRUE)))
  add_assert(sprintf("[%s] component 'mean_dwell_minutes' (z within Sex) == displayed domain 'Latent-state persistence'", res),
             "z within Sex of mean_dwell_minutes recomputed here vs the DomainScore in first_night_domain_scores.csv",
             max(abs(b$comp_val - b$DomainScore), na.rm = TRUE) < 1e-12,
             sprintf("n = %d, max|diff| = %s", nrow(b),
                     format(max(abs(b$comp_val - b$DomainScore), na.rm = TRUE), scientific = TRUE)))
  f <- hfeat %>% filter(resolution == res)
  add_assert(sprintf("[%s] state_switch_rate == 1 - self_transition_probability (ONE construct)", res),
             "max|state_switch_rate - (1 - self_transition_probability)| on the first-night window",
             max(abs(f$state_switch_rate - (1 - f$self_transition_probability))) < 1e-12,
             sprintf(paste0("max|diff| = %s; both rows appear in the component scan ONLY so the identity is ",
                            "visible, and only mean_dwell_minutes reaches the domain heatmap"),
                     format(max(abs(f$state_switch_rate - (1 - f$self_transition_probability))),
                            scientific = TRUE)))
}

fit_component <- function(dd, cp, res) {
  m <- dd %>% filter(is.finite(value_modelled))
  if (n_distinct(m$Group) < 3 || n_distinct(m$Sex) < 2) return(NULL)
  warns <- character(0)
  ct <- withCallingHandlers({
    fit <- stats::lm(value_modelled ~ Group * Sex, data = m)
    as.data.frame(summary(emmeans::contrast(emmeans::emmeans(fit, ~ Group | Sex),
                                            method = CONTRASTS, adjust = "none"),
                          infer = c(TRUE, TRUE)))
  }, warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") })
  ct %>% as_tibble() %>%
    transmute(bin_resolution = res, component = cp, Sex = as.character(Sex),
              contrast = as.character(contrast), estimate, SE, df,
              ci_low = lower.CL, ci_high = upper.CL, t_ratio = t.ratio, raw_p = p.value) %>%
    rowwise() %>%
    mutate(group_ref = REF_OF[[contrast]], group_comp = CMP_OF[[contrast]],
           n_ref  = sum(as.character(m$Group) == group_ref  & as.character(m$Sex) == Sex),
           n_comp = sum(as.character(m$Group) == group_comp & as.character(m$Sex) == Sex),
           mean_ref  = mean(m$value_modelled[as.character(m$Group) == group_ref  & as.character(m$Sex) == Sex]),
           mean_comp = mean(m$value_modelled[as.character(m$Group) == group_comp & as.character(m$Sex) == Sex]),
           mean_ref_raw_units  = mean(m$value_raw[as.character(m$Group) == group_ref  & as.character(m$Sex) == Sex]),
           mean_comp_raw_units = mean(m$value_raw[as.character(m$Group) == group_comp & as.character(m$Sex) == Sex]),
           hedges_g = hmm_hedges_g(m$value_modelled[as.character(m$Group) == group_ref  & as.character(m$Sex) == Sex],
                                   m$value_modelled[as.character(m$Group) == group_comp & as.character(m$Sex) == Sex])) %>%
    ungroup() %>%
    mutate(n_animals_in_model = nrow(m),
           model_status = if (length(warns)) "fitted_with_warnings" else "fitted_clean",
           model_warnings = if (length(warns)) paste(unique(warns), collapse = " | ") else "none")
}

comp_res <- map_dfr(RESOLUTIONS, function(res) {
  map_dfr(COMPONENTS$component, function(cp) {
    fit_component(comp_long %>% filter(bin_resolution == res, component == cp), cp, res)
  })
}) %>% left_join(COMPONENTS, by = "component") %>%
  mutate(FDR_family = sprintf("FIRST_NIGHT_HMM_COMPONENT_SCAN__%s__%s", bin_resolution, Sex),
         in_fdr_family = displayed) %>%
  group_by(FDR_family) %>%
  mutate(q = p.adjust(if_else(in_fdr_family, raw_p, NA_real_), "BH"),
         n_tests_in_family = sum(in_fdr_family)) %>%
  ungroup()

## ---- resolution agreement per component x Sex x contrast -----------------
agree <- comp_res %>% select(component, Sex, contrast, bin_resolution, estimate, ci_low, ci_high) %>%
  pivot_wider(names_from = bin_resolution, values_from = c(estimate, ci_low, ci_high)) %>%
  transmute(component, Sex, contrast,
            resolution_agreement_same_sign = sign(estimate_10min_based) == sign(estimate_5min_based),
            resolution_agreement_ci_overlap = (ci_low_10min_based <= ci_high_5min_based) &
              (ci_low_5min_based <= ci_high_10min_based),
            abs_estimate_ratio_5min_over_10min = abs(estimate_5min_based) / abs(estimate_10min_based),
            estimate_10min_primary = estimate_10min_based,
            estimate_5min_sensitivity = estimate_5min_based)

comp_res <- comp_res %>% left_join(agree, by = c("component", "Sex", "contrast")) %>%
  mutate(contrast = factor(contrast, CONTRAST_ORDER),
         resolution_role = RES_ROLE[bin_resolution],
         analysis_status = paste0("EXPLORATORY component scan; hypothesis-generating, reported in its own FDR ",
                                  "family, never used to reinterpret the displayed-domain family"),
         ci_method = paste0("emmeans 95% CI on the lm scale: estimate +/- qt(0.975, residual df) * SE from ",
                            "lm(component_value ~ Group * Sex)"),
         model_formula = "component_value ~ Group * Sex",
         model_engine = MODEL_ENGINE,
         value_scale = if_else(component %in% NO_RESTANDARDIZE,
                               "z-composite in SD units (already standardized; NOT re-standardized)",
                               "z within Sex of the raw component (SD units)"),
         inferential_unit = paste0("one animal = one value; plain lm, no random effect; the Viterbi bins ",
                                   "inside the window are NEVER treated as independent observations"),
         standardization = STANDARDIZATION, window = WINDOW_TEXT,
         fdr_note = paste0("BH within FIRST_NIGHT_HMM_COMPONENT_SCAN__<resolution>__<Sex> over the ",
                           sum(COMPONENTS$displayed), " non-excluded components x 3 contrasts. ",
                           "top_proximity_state_fraction is OUTSIDE the family (q = NA) because it is an ",
                           "excluded construct. The family is NOT ", sum(COMPONENTS$displayed),
                           " independent constructs: state_switch_rate == 1 - self_transition_probability ",
                           "exactly, and latent_state_occupancy_organization is a linear combination of ",
                           "occupancy_entropy and inactive_state_fraction, so BH here is applied over ",
                           "correlated tests and is exploratory bookkeeping, not a confirmatory ",
                           "error-rate guarantee."),
         resolution_comparison_basis = paste0("direction, magnitude and CI overlap only; resolution is NEVER ",
                                              "selected on p-values (10-min is primary a priori, 5-min is ",
                                              "sensitivity)"),
         interpretation = DESCRIPTIVE,
         source_table = file.path(OUT, "first_night_hmm_component_features_v2.csv"),
         upstream_script = UPSTREAM, script = THIS_SCRIPT,
         supersedes = paste0("Testing/audits/audit_first_night_hmm_components.R wrote an earlier ",
                             "first_night_hmm_component_results.csv on the first-contiguous-block window; ",
                             "this v2 run supersedes it, adds the resolution-agreement columns and the ",
                             "explicit exploratory FDR bookkeeping")) %>%
  arrange(bin_resolution, component_group, component, Sex, contrast) %>%
  select(component, component_group, bin_resolution, resolution_role, Sex, contrast,
         n_ref, n_comp, mean_ref, mean_comp, mean_ref_raw_units, mean_comp_raw_units, raw_unit,
         hedges_g, estimate, SE, df, ci_low, ci_high, ci_method, raw_p, q, FDR_family,
         n_tests_in_family, in_fdr_family, model_status, model_warnings, t_ratio,
         n_animals_in_model, group_ref, group_comp, displayed, component_status, component_guard,
         resolution_agreement_same_sign, resolution_agreement_ci_overlap,
         abs_estimate_ratio_5min_over_10min, estimate_10min_primary, estimate_5min_sensitivity,
         resolution_comparison_basis, analysis_status, model_formula, model_engine, value_scale,
         inferential_unit, standardization, window, fdr_note, interpretation,
         source_table, upstream_script, script, supersedes)
write_csv(comp_res, file.path(OUT, "first_night_hmm_component_results.csv"))
cat("wrote first_night_hmm_component_results.csv  rows =", nrow(comp_res), "\n")

sec("HMM component scan, 10-min PRIMARY (EXPLORATORY family)")
print(as.data.frame(comp_res %>% filter(bin_resolution == PRIMARY_RES) %>%
  transmute(grp = str_trunc(component_group, 12), component = str_trunc(component, 34), Sex, contrast,
            n = sprintf("%d/%d", n_ref, n_comp), g = r4(hedges_g), est = r4(estimate),
            CI = sprintf("[%.2f,%.2f]", ci_low, ci_high), p = signif(raw_p, 3), q = signif(q, 3)) %>%
  arrange(Sex, grp, component, contrast)), row.names = FALSE)

sec("Component cells with raw p < 0.05 (both resolutions), with their BH q")
pp <- comp_res %>% filter(raw_p < 0.05)
if (nrow(pp) == 0) cat("  NONE.\n") else
  print(as.data.frame(pp %>% transmute(bin_resolution, component, Sex, contrast, g = r4(hedges_g),
            est = r4(estimate), CI = sprintf("[%.2f,%.2f]", ci_low, ci_high),
            p = signif(raw_p, 3), q = signif(q, 3),
            same_sign_across_res = resolution_agreement_same_sign,
            ratio_5_10 = r4(abs_estimate_ratio_5min_over_10min)) %>%
  arrange(bin_resolution, Sex, component, contrast)), row.names = FALSE)

sec("Resolution agreement summary (component scan)")
cat("  same sign 10min vs 5min:", sum(agree$resolution_agreement_same_sign, na.rm = TRUE), "/", nrow(agree),
    " | CI overlap:", sum(agree$resolution_agreement_ci_overlap, na.rm = TRUE), "/", nrow(agree),
    " | median |est5|/|est10| =", r4(median(agree$abs_estimate_ratio_5min_over_10min, na.rm = TRUE)), "\n")
print(as.data.frame(agree %>% group_by(component) %>%
  summarise(same_sign = sum(resolution_agreement_same_sign, na.rm = TRUE), n = n(),
            median_ratio = r4(median(abs_estimate_ratio_5min_over_10min, na.rm = TRUE)),
            .groups = "drop")), row.names = FALSE)

## ---- component Group:Sex interactions ------------------------------------
comp_inter <- map_dfr(RESOLUTIONS, function(res) {
  map_dfr(COMPONENTS$component, function(cp) {
    m <- comp_long %>% filter(bin_resolution == res, component == cp, is.finite(value_modelled))
    if (n_distinct(m$Group) < 3 || n_distinct(m$Sex) < 2) return(NULL)
    av <- stats::anova(stats::lm(value_modelled ~ Group * Sex, data = m))
    tibble(bin_resolution = res, component = cp, term = "Group:Sex",
           df_num = av["Group:Sex", "Df"], df_den = av["Residuals", "Df"],
           F_value = av["Group:Sex", "F value"], p_value = av["Group:Sex", "Pr(>F)"],
           n_animals = nrow(m))
  })
}) %>% left_join(COMPONENTS %>% select(component, component_group, displayed, component_status),
                 by = "component") %>%
  mutate(resolution_role = RES_ROLE[bin_resolution],
         model_formula = "component_value ~ Group * Sex",
         model_engine = "stats::anova(stats::lm(...)) sequential (Type I) SS",
         multiplicity_treatment = paste0("UNCORRECTED. One Group:Sex omnibus test per component per ",
                                         "resolution (", nrow(COMPONENTS), " components). Separate from BOTH ",
                                         "the displayed-domain family and the ",
                                         "FIRST_NIGHT_HMM_COMPONENT_SCAN family; no q is claimed."),
         analysis_status = "EXPLORATORY", interpretation = DESCRIPTIVE,
         window = WINDOW_TEXT, script = THIS_SCRIPT) %>%
  arrange(bin_resolution, component_group, component)
write_csv(comp_inter, file.path(OUT, "first_night_hmm_component_interactions.csv"))
cat("wrote first_night_hmm_component_interactions.csv  rows =", nrow(comp_inter), "\n")
print(as.data.frame(comp_inter %>% filter(bin_resolution == PRIMARY_RES) %>%
  transmute(component, F = r4(F_value), p = signif(p_value, 3), n = n_animals)), row.names = FALSE)

## ==========================================================================
hr("STEP 3. Movement-adjustment sensitivity -- CONSTRUCT DIAGNOSTIC, NOT PRIMARY")
## ==========================================================================
cat("movement_first_night = the first-night `Psychomotor activation` domain score on the SAME\n",
    "canonical clock window (= z within Sex of Movement_mean over the window), read straight\n",
    "from first_night_domain_scores.csv so the covariate is provably the same window.\n\n", sep = "")
cat("MANDATORY CAVEAT: with a strongly collinear covariate the residual variance collapses, so\n",
    "unadjusted and adjusted p-values are NOT comparable. This is a CONSTRUCT DIAGNOSTIC, not\n",
    "the primary model. Judge attenuation on the ESTIMATE together with Spearman(component, movement).\n",
    sep = "")

mv_cov <- scores %>% filter(Domain == "Psychomotor activation") %>%
  select(AnimalNum, bin_resolution, movement_first_night = DomainScore)

mv_rows <- list(); loco_rows <- list()
for (res in RESOLUTIONS) {
  for (cp in COMPONENTS$component) {
    md <- comp_long %>% filter(bin_resolution == res, component == cp) %>%
      inner_join(mv_cov %>% filter(bin_resolution == res) %>% select(-bin_resolution),
                 by = "AnimalNum") %>%
      filter(is.finite(value_modelled), is.finite(movement_first_night))
    if (n_distinct(md$Group) < 3 || n_distinct(md$Sex) < 2) next
    for (sx in SEX_LEVELS) {
      s <- md %>% filter(Sex == sx)
      ct <- suppressWarnings(stats::cor.test(s$value_modelled, s$movement_first_night,
                                             method = "spearman"))
      loco_rows[[length(loco_rows) + 1L]] <- tibble(
        bin_resolution = res, component = cp, Sex = sx, n = nrow(s),
        spearman_rho_component_vs_movement = unname(ct$estimate), spearman_p = ct$p.value,
        pearson_r_component_vs_movement = safe_cor(s$value_modelled, s$movement_first_night),
        locomotion_dominance_flag = abs(unname(ct$estimate)) >= 0.70,
        dominance_threshold = "repo standard |rho| >= 0.70")
    }
    gc0 <- function(f0) as.data.frame(summary(emmeans::contrast(emmeans::emmeans(f0, ~ Group | Sex),
                                                                method = CONTRASTS, adjust = "none"))) %>%
      as_tibble() %>% transmute(Sex = as.character(Sex), contrast = as.character(contrast),
                                estimate, SE, p = p.value)
    u <- gc0(stats::lm(value_modelled ~ Group * Sex, data = md)) %>%
      rename(estimate_unadjusted = estimate, SE_unadjusted = SE, p_unadjusted = p)
    a <- gc0(stats::lm(value_modelled ~ Group * Sex + movement_first_night, data = md)) %>%
      rename(estimate_adjusted = estimate, SE_adjusted = SE, p_adjusted = p)
    mv_rows[[length(mv_rows) + 1L]] <- u %>% left_join(a, by = c("Sex", "contrast")) %>%
      mutate(bin_resolution = res, component = cp,
             pct_attenuation = 100 * (1 - abs(estimate_adjusted) / abs(estimate_unadjusted)),
             sign_flip_after_adjustment = sign(estimate_adjusted) != sign(estimate_unadjusted))
  }
}
loco_tbl <- bind_rows(loco_rows)
mv_tbl <- bind_rows(mv_rows) %>%
  left_join(loco_tbl, by = c("bin_resolution", "component", "Sex")) %>%
  left_join(COMPONENTS %>% select(component, component_group, displayed, component_status),
            by = "component") %>%
  mutate(contrast = factor(contrast, CONTRAST_ORDER), resolution_role = RES_ROLE[bin_resolution],
         model_unadjusted = "component_value ~ Group * Sex",
         model_adjusted   = "component_value ~ Group * Sex + movement_first_night",
         movement_definition = paste0("first-night `Psychomotor activation` domain score = z within Sex of ",
                                      "Movement_mean over the SAME canonical CC1 clock window (18:30-06:30)"),
         status = "CONSTRUCT DIAGNOSTIC -- the adjusted model is NEVER the primary model",
         mandatory_caveat = paste0("With a strongly collinear covariate the residual variance collapses, so ",
                                   "the unadjusted and adjusted p-values are NOT comparable and must not be ",
                                   "read as a before/after significance test. Read pct_attenuation on the ",
                                   "ESTIMATE together with spearman_rho_component_vs_movement against the ",
                                   "|rho| >= 0.70 dominance threshold."),
         multiplicity = "UNCORRECTED diagnostic; no FDR family claimed",
         interpretation = DESCRIPTIVE, window = WINDOW_TEXT, script = THIS_SCRIPT) %>%
  arrange(bin_resolution, component_group, component, Sex, contrast) %>%
  relocate(component, component_group, bin_resolution, resolution_role, Sex, contrast)
write_csv(mv_tbl, file.path(OUT, "first_night_hmm_movement_adjustment.csv"))
cat("\nwrote first_night_hmm_movement_adjustment.csv  rows =", nrow(mv_tbl), "\n")
sec("Spearman(component, movement_first_night), 10-min primary  [flag at |rho| >= 0.70]")
print(as.data.frame(loco_tbl %>% filter(bin_resolution == PRIMARY_RES) %>%
  transmute(component, Sex, n, rho = r4(spearman_rho_component_vs_movement),
            p = signif(spearman_p, 3), flag = locomotion_dominance_flag)), row.names = FALSE)
sec("Unadjusted vs movement-adjusted contrasts, 10-min primary (p values NOT comparable)")
print(as.data.frame(mv_tbl %>% filter(bin_resolution == PRIMARY_RES) %>%
  transmute(component = str_trunc(component, 32), Sex, contrast,
            est_unadj = r4(estimate_unadjusted), est_adj = r4(estimate_adjusted),
            atten_pct = round(pct_attenuation, 1), p_unadj = signif(p_unadjusted, 3),
            p_adj = signif(p_adjusted, 3), rho_mv = r4(spearman_rho_component_vs_movement),
            flip = sign_flip_after_adjustment)), row.names = FALSE)
sec("Attenuation of the ESTIMATE per component x Sex (10-min primary)")
print(as.data.frame(mv_tbl %>% filter(bin_resolution == PRIMARY_RES) %>% group_by(component, Sex) %>%
  summarise(median_atten_pct = round(median(pct_attenuation, na.rm = TRUE), 1),
            min_atten_pct = round(min(pct_attenuation, na.rm = TRUE), 1),
            max_atten_pct = round(max(pct_attenuation, na.rm = TRUE), 1),
            n_sign_flips = sum(sign_flip_after_adjustment, na.rm = TRUE), .groups = "drop")),
  row.names = FALSE)

## ==========================================================================
hr("STEP 4. Exploratory continuous CombZ association -- NOT INDEPENDENT PREDICTION")
## ==========================================================================
cat("MANDATORY FRAMING: RES/SUS were DERIVED FROM CombZ. A CombZ association is therefore NOT\n",
    "independent prediction and NOT evidence of incremental predictive value. A prospective\n",
    "claim would require the cross-validated Stage 09 framework, where Movement_mean carries\n",
    "most of the cross-validated signal. The word 'predictive' is never used for these numbers.\n",
    sep = "")
cz_raw <- read_excel(COMBZ_XLSX, sheet = "zScore")
id_col <- intersect(c("ID", "AnimalNum", "Animal", "MouseID"), names(cz_raw))[1]
stopifnot(!is.na(id_col), "CombZ" %in% names(cz_raw))
cz <- cz_raw %>% transmute(AnimalNum = canonical_animal_id(.data[[id_col]]),
                           CombZ = suppressWarnings(as.numeric(CombZ))) %>%
  filter(!is.na(AnimalNum), is.finite(CombZ)) %>%
  group_by(AnimalNum) %>% summarise(CombZ = mean(CombZ), n_source_rows = n(), .groups = "drop")
cat("CombZ: id column '", id_col, "', animals = ", nrow(cz),
    ", duplicate ids collapsed by mean = ", sum(cz$n_source_rows > 1),
    ", on roster = ", sum(cz$AnimalNum %in% roster$AnimalNum), "\n", sep = "")
add_assert("CombZ is read only for the EXPLORATORY association step, after all features exist",
           "CombZ is loaded in STEP 4, strictly after every feature and every primary model has been built",
           TRUE,
           sprintf(paste0("CombZ animals = %d (one value per animal, id column '%s'); it never enters ",
                          "feature construction or the primary lm"), nrow(cz), id_col))

cz_rows <- list()
for (res in RESOLUTIONS) {
  for (cp in COMPONENTS$component) {
    d <- comp_long %>% filter(bin_resolution == res, component == cp) %>%
      inner_join(mv_cov %>% filter(bin_resolution == res) %>% select(-bin_resolution),
                 by = "AnimalNum") %>%
      inner_join(cz %>% select(AnimalNum, CombZ), by = "AnimalNum") %>%
      transmute(Sex = as.character(Sex), comp = value_modelled, mv = movement_first_night, CombZ) %>%
      filter(is.finite(comp), is.finite(mv), is.finite(CombZ))
    for (sx in c(SEX_LEVELS, "Both")) {
      s <- if (sx == "Both") d else d %>% filter(Sex == sx)
      if (nrow(s) < 6) next
      sp <- suppressWarnings(stats::cor.test(s$comp, s$CombZ, method = "spearman"))
      pe <- suppressWarnings(stats::cor.test(s$comp, s$CombZ, method = "pearson"))
      f1 <- if (sx == "Both") CombZ ~ comp + Sex else CombZ ~ comp
      f2 <- if (sx == "Both") CombZ ~ mv + Sex else CombZ ~ mv
      f3 <- if (sx == "Both") CombZ ~ mv + comp + Sex else CombZ ~ mv + comp
      c1 <- summary(stats::lm(f1, data = s)); c2 <- summary(stats::lm(f2, data = s))
      c3 <- summary(stats::lm(f3, data = s))
      k1 <- c1$coefficients; k2 <- c2$coefficients; k3 <- c3$coefficients
      cz_rows[[length(cz_rows) + 1L]] <- tibble(
        bin_resolution = res, component = cp, Sex = sx, n = nrow(s),
        spearman_rho = unname(sp$estimate), spearman_p = sp$p.value,
        pearson_r = unname(pe$estimate), pearson_p = pe$p.value,
        spearman_component_vs_movement = safe_cor(s$comp, s$mv, "spearman"),
        m1_component_beta = k1["comp", 1], m1_component_se = k1["comp", 2],
        m1_component_p = k1["comp", 4], m1_adj_r2 = c1$adj.r.squared,
        m2_movement_beta = k2["mv", 1], m2_movement_se = k2["mv", 2],
        m2_movement_p = k2["mv", 4], m2_adj_r2 = c2$adj.r.squared,
        m3_component_beta = k3["comp", 1], m3_component_se = k3["comp", 2],
        m3_component_p = k3["comp", 4], m3_movement_beta = k3["mv", 1],
        m3_movement_se = k3["mv", 2], m3_movement_p = k3["mv", 4], m3_adj_r2 = c3$adj.r.squared,
        component_pct_attenuation_after_movement = 100 * (1 - abs(k3["comp", 1]) / abs(k1["comp", 1])),
        incremental_adj_r2_component_over_movement = c3$adj.r.squared - c2$adj.r.squared,
        model_1 = if (sx == "Both") "CombZ ~ component + Sex" else
          "CombZ ~ component  (single-Sex stratum; the Sex term is dropped because it is constant)",
        model_2 = if (sx == "Both") "CombZ ~ movement_first_night + Sex" else
          "CombZ ~ movement_first_night  (single-Sex stratum)",
        model_3 = if (sx == "Both") "CombZ ~ movement_first_night + component + Sex" else
          "CombZ ~ movement_first_night + component  (single-Sex stratum)")
    }
  }
}
cz_tbl <- bind_rows(cz_rows) %>%
  left_join(COMPONENTS %>% select(component, component_group, displayed, component_status),
            by = "component") %>%
  mutate(resolution_role = RES_ROLE[bin_resolution],
         framing = paste0("EXPLORATORY DESCRIPTIVE ASSOCIATION ONLY. RES/SUS were DERIVED FROM CombZ, so a ",
                          "CombZ association is NOT independent prediction and NOT evidence of incremental ",
                          "predictive value. A prospective claim would require the cross-validated Stage 09 ",
                          "framework, in which Movement_mean carries most of the cross-validated signal. ",
                          "The word 'predictive' does not apply to any number in this table."),
         incremental_note = paste0("m3 vs m1 shows how much of the component's DESCRIPTIVE association with ",
                                   "CombZ is shared with first-night locomotion; m3 vs m2 shows the ",
                                   "component's descriptive increment over locomotion. Both are in-sample, ",
                                   "unpenalised and uncross-validated."),
         multiplicity = "UNCORRECTED exploratory correlations and regressions; NO FDR family is claimed",
         window = WINDOW_TEXT,
         combz_source = paste0(COMBZ_XLSX, " sheet 'zScore', id column '", id_col,
                               "', canonical_animal_id(), one value per animal (mean over duplicate rows)"),
         script = THIS_SCRIPT) %>%
  arrange(bin_resolution, component_group, component, Sex) %>%
  relocate(component, component_group, bin_resolution, resolution_role, Sex, n)
write_csv(cz_tbl, file.path(OUT, "first_night_hmm_combz_association.csv"))
cat("wrote first_night_hmm_combz_association.csv  rows =", nrow(cz_tbl), "\n")
sec("CombZ association, 10-min primary (EXPLORATORY, descriptive, never predictive)")
print(as.data.frame(cz_tbl %>% filter(bin_resolution == PRIMARY_RES) %>%
  transmute(component = str_trunc(component, 32), Sex, n, rho = r4(spearman_rho),
            rho_p = signif(spearman_p, 3), r = r4(pearson_r),
            b1 = r4(m1_component_beta), p1 = signif(m1_component_p, 3),
            b3 = r4(m3_component_beta), p3 = signif(m3_component_p, 3),
            atten = round(component_pct_attenuation_after_movement, 1),
            dR2 = r4(incremental_adj_r2_component_over_movement))), row.names = FALSE)
sec("CombZ ~ movement_first_night (+ Sex): the locomotion reference, identical across components")
print(as.data.frame(cz_tbl %>% filter(bin_resolution == PRIMARY_RES) %>%
  distinct(Sex, n, m2_movement_beta, m2_movement_p, m2_adj_r2) %>%
  transmute(Sex, n, beta = r4(m2_movement_beta), p = signif(m2_movement_p, 3),
            adj_r2 = r4(m2_adj_r2))), row.names = FALSE)

## ==========================================================================
hr("STEP 5. FIGURES")
## ==========================================================================
ROW_ORDER <- c("Behavioral flexibility / predictability",
               "Behavioral volatility / fragmentation",
               "Active-phase adaptation/exploration",
               "Social spatial organization",
               "Latent-state occupancy organization",
               "Latent-state persistence",
               "Psychomotor activation")          # locomotion reference LAST
stopifnot(setequal(ROW_ORDER, DISPLAYED_DOMS))
ROW_LABEL <- c(
  "Behavioral flexibility / predictability"  = "Behavioral flexibility /\npredictability",
  "Behavioral volatility / fragmentation"    = "Behavioral volatility /\nfragmentation",
  "Active-phase adaptation/exploration"      = "Active-phase adaptation /\nexploration",
  "Social spatial organization"              = "Social spatial organization\n(RFID co-location proxy)",
  "Latent-state occupancy organization"      = "Latent-state occupancy\norganization (HMM)",
  "Latent-state persistence"                 = "Latent-state persistence\n(HMM mean dwell)",
  "Psychomotor activation"                   = "Psychomotor activation\n(locomotion reference)")

hm <- eff %>% filter(bin_resolution == PRIMARY_RES) %>%
  mutate(Domain = factor(Domain, ROW_ORDER),
         row_label = factor(ROW_LABEL[as.character(Domain)], rev(unname(ROW_LABEL[ROW_ORDER]))),
         contrast = factor(as.character(contrast), CONTRAST_ORDER),
         Sex = factor(Sex, SEX_LEVELS),
         star = stars_of(q),
         tile_label = if_else(is.finite(Hedges_g), paste0(sprintf("%.2f", Hedges_g), star), NA_character_))
lim <- ceiling(max(abs(hm$Hedges_g), na.rm = TRUE) * 20) / 20

## n varies slightly between the raw domains (111/111 animals) and the two HMM domains
## (109/111: OQ770/OQ771 removed by Stage 08's epoch data-quality rule), so the caption
## reports a RANGE wherever the two differ rather than a single misleading number.
rng <- function(a, b) if (min(a) == max(a) && min(b) == max(b))
  sprintf("%d/%d", min(a), min(b)) else
  sprintf("%s/%s", if (min(a) == max(a)) as.character(min(a)) else sprintf("%d-%d", min(a), max(a)),
                   if (min(b) == max(b)) as.character(min(b)) else sprintf("%d-%d", min(b), max(b)))
n_by_sex <- eff %>% filter(bin_resolution == PRIMARY_RES) %>%
  group_by(Sex, contrast) %>% summarise(nn = rng(n_ref, n_comp), .groups = "drop_last") %>%
  summarise(txt = paste(sprintf("%s %s", contrast, nn), collapse = ", "), .groups = "drop")
sec("n per Sex x contrast (10-min primary)")
print(as.data.frame(n_by_sex), row.names = FALSE)

p_hm <- ggplot(hm, aes(x = contrast, y = row_label, fill = Hedges_g)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = tile_label), size = 2.0, colour = "black") +
  facet_wrap(~ Sex, nrow = 1) +
  scale_fill_gradient2(low = mmm_diverging_colors[["low"]], mid = mmm_diverging_colors[["mid"]],
                       high = mmm_diverging_colors[["high"]], midpoint = 0,
                       limits = c(-lim, lim), na.value = "grey90",
                       name = "Hedges g (animal level)",
                       guide = guide_colourbar(title.position = "top", barheight = unit(3, "mm"),
                                               barwidth = unit(34, "mm"))) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = "First response to social instability",
       subtitle = paste0("Behaviour during the first social-instability encounter, by later phenotype\n",
                         wrap_txt(paste0("CC1, first Active phase, clock-anchored 18:30-06:30 12 h window; ",
                                         "one value per animal; colour = animal-level Hedges g; ",
                                         "stars = BH q within Sex"), 118)),
       x = NULL, y = NULL,
       caption = wrap_txt(paste0("lm(DomainScore ~ Group * Sex) + emmeans(~ Group | Sex); 10-min bins ",
                        "(primary). BH q within Sex over ", length(DISPLAYED_DOMS), " domains x 3 contrasts (",
                        unique(eff$n_tests_in_family[eff$bin_resolution == PRIMARY_RES]), " tests). ",
                        "*** q<0.001, ** q<0.01, * q<0.05. ",
                        "RES/SUS are LATER phenotype labels derived from subsequent CombZ, so every cell is ",
                        "a DESCRIPTIVE association with later phenotype: the labels were NOT known at CC1 ",
                        "and no prospective or causal claim is implied. ",
                        "Proximity is a social-spatial CO-LOCATION proxy, not sociability. Latent-state ",
                        "occupancy organization is order-free composition, not temporal flexibility. ",
                        "Excluded: the duplicate 'Early adaptation / prediction' domain (identical to ",
                        "Active-phase adaptation at CC1) and top-proximity state occupancy (failed ",
                        "partition robustness). n animals per Sex (reference/comparison; a range where the ",
                        "two HMM rows lose the 2 Stage 08 exclusions): Female ",
                        n_by_sex$txt[n_by_sex$Sex == "Female"], "; Male ",
                        n_by_sex$txt[n_by_sex$Sex == "Male"], "."), 150)) +
  make_nature_theme(base_size = 6.4) +
  theme(legend.position = "top", legend.title = element_text(size = rel(0.95)),
        axis.line = element_blank(), axis.ticks = element_blank(),
        panel.spacing = unit(1.6, "mm"),
        axis.text.y = element_text(size = rel(0.95), lineheight = 0.95),
        plot.caption = element_text(size = rel(0.70), lineheight = 1.05))
save_plot_svg_pdf(p_hm, file.path(OUT, "Fig_first_night_domain_heatmap"),
                  width = 160, height = 108, units = "mm")
cat("wrote Fig_first_night_domain_heatmap.{svg,pdf,png}  (symmetric fill limits +/-", lim, ")\n")

## ---- HMM component figure: occupancy vs temporal in SEPARATE facet groups --
COMP_LABEL <- c(
  "occupancy_entropy"                   = "Occupancy entropy",
  "latent_state_occupancy_organization" = "Occupancy organization\n(0.5 z(entropy) - z(inactive))",
  "inactive_state_fraction"             = "Inactive-state fraction",
  "mean_dwell_minutes"                  = "Mean dwell (minutes)",
  "self_transition_probability"         = "Self-transition probability",
  "state_switch_rate"                   = "Switch rate (= 1 - self-transition)",
  "transition_entropy"                  = "Transition entropy")
COMP_FIG_ORDER <- names(COMP_LABEL)
cf <- comp_res %>% filter(bin_resolution == PRIMARY_RES, component %in% COMP_FIG_ORDER) %>%
  mutate(component_group = factor(if_else(component_group == "occupancy composition",
                                          "Occupancy composition (order-free)",
                                          "Temporal organization (order-dependent)"),
                                  c("Occupancy composition (order-free)",
                                    "Temporal organization (order-dependent)")),
         row_label = factor(COMP_LABEL[component], rev(unname(COMP_LABEL[COMP_FIG_ORDER]))),
         contrast = factor(as.character(contrast), CONTRAST_ORDER),
         Sex = factor(Sex, SEX_LEVELS), star = stars_of(q),
         tile_label = if_else(is.finite(hedges_g), paste0(sprintf("%.2f", hedges_g), star), NA_character_))
lim_c <- ceiling(max(abs(cf$hedges_g), na.rm = TRUE) * 20) / 20
p_comp <- ggplot(cf, aes(x = contrast, y = row_label, fill = hedges_g)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = tile_label), size = 2.0, colour = "black") +
  facet_grid(component_group ~ Sex, scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_gradient2(low = mmm_diverging_colors[["low"]], mid = mmm_diverging_colors[["mid"]],
                       high = mmm_diverging_colors[["high"]], midpoint = 0,
                       limits = c(-lim_c, lim_c), na.value = "grey90",
                       name = "Hedges g (animal level)",
                       guide = guide_colourbar(title.position = "top", barheight = unit(3, "mm"),
                                               barwidth = unit(34, "mm"))) +
  scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
  labs(title = "First night: latent-state occupancy vs temporal organization",
       subtitle = wrap_txt(paste0("HMM component scan on the same clock-anchored CC1 18:30-06:30 window; ",
                         "one value per animal; occupancy components (order-free) and temporal components ",
                         "(order-dependent) shown as separate groups"), 118),
       x = NULL, y = NULL,
       caption = wrap_txt(paste0("EXPLORATORY scan. lm(component ~ Group * Sex) + emmeans(~ Group | Sex), ",
                        "10-min bins. BH q within FIRST_NIGHT_HMM_COMPONENT_SCAN__10min_based__<Sex> over ",
                        unique(comp_res$n_tests_in_family[comp_res$bin_resolution == PRIMARY_RES]),
                        " tests. *** q<0.001, ** q<0.01, * q<0.05. ",
                        "Switch rate is EXACTLY 1 - self-transition probability, so those two rows are ONE ",
                        "construct shown twice to make the identity visible; only mean dwell reaches the ",
                        "domain heatmap. Shuffling the Viterbi sequence leaves the occupancy rows exactly ",
                        "unchanged, so they carry no temporal-order information. Top-proximity state ",
                        "occupancy is computed but NOT shown (failed partition robustness). ",
                        "RES/SUS are LATER phenotype labels from subsequent CombZ: descriptive association ",
                        "with later phenotype, never prospective. n = ", max(cf$n_animals_in_model),
                        " animals (109/111; 2 Stage 08 epoch data-quality exclusions)."), 150)) +
  make_nature_theme(base_size = 6.4) +
  theme(legend.position = "top", legend.title = element_text(size = rel(0.95)),
        axis.line = element_blank(), axis.ticks = element_blank(),
        panel.spacing = unit(1.6, "mm"), strip.placement = "outside",
        strip.text.y.left = element_text(angle = 90, size = rel(0.95)),
        axis.text.y = element_text(size = rel(0.95), lineheight = 0.95),
        plot.caption = element_text(size = rel(0.70), lineheight = 1.05))
save_plot_svg_pdf(p_comp, file.path(OUT, "Fig_first_night_hmm_components"),
                  width = 160, height = 118, units = "mm")
cat("wrote Fig_first_night_hmm_components.{svg,pdf,png}  (symmetric fill limits +/-", lim_c, ")\n")

## ==========================================================================
hr("STEP 6. Reconciliation against the production Stage 14 CC1 panel")
## ==========================================================================
prod <- read_csv(PROD_CC1, col_types = cols(), progress = FALSE) %>%
  transmute(prod_Domain = Domain, Sex, contrast,
            prod_n_ref = n_ref, prod_n_comp = n_comp,
            prod_mean_ref = mean_ref, prod_mean_comp = mean_comp,
            prod_hedges_g = hedges_g, prod_p = p.value, prod_wilcox_p = wilcox_p,
            prod_p_fdr = p_fdr, prod_test_method = test_method)
cat("production sis_CC1_first_active_domain_contrasts.csv rows =", nrow(prod),
    " domains =", n_distinct(prod$prod_Domain), "\n")
print(sort(unique(prod$prod_Domain)))

## Domain-name mapping. Production's HMM domain is the shipped occupancy composite, which is our
## `Latent-state occupancy organization`; production has NO persistence domain at all.
MAP <- tibble(
  Domain = c("Psychomotor activation", "Behavioral flexibility / predictability",
             "Social spatial organization", "Behavioral volatility / fragmentation",
             "Active-phase adaptation/exploration", "Latent-state occupancy organization",
             "Latent-state persistence"),
  prod_Domain = c("Psychomotor activation", "Behavioral flexibility / predictability",
                  "Social spatial organization", "Behavioral volatility / fragmentation",
                  "Active-phase adaptation/exploration", "Behavioral state architecture",
                  NA_character_),
  name_mapping_note = c(
    "same name", "same name", "same name", "same name", "same name",
    paste0("production calls the shipped HMM occupancy composite 'Behavioral state architecture'; same ",
           "formula family (mean(z(H), z(social)) - z(inactive), which equals 0.5*z(H) - z(inactive) here ",
           "because z(social) is identically 0) but production aggregates it over the WHOLE ~48 h CC1 ",
           "Active epoch (Stage 08 per-epoch table, class C), not over the first night"),
    paste0("no production counterpart: the production CC1 panel has no persistence/dwell domain, so only ",
           "our value exists")))

recon <- eff %>% filter(bin_resolution == PRIMARY_RES) %>%
  transmute(Domain, Sex, contrast = as.character(contrast), ours_n_ref = n_ref, ours_n_comp = n_comp,
            ours_mean_ref = mean_ref, ours_mean_comp = mean_comp, ours_hedges_g = Hedges_g,
            ours_estimate = model_estimate, ours_SE = SE, ours_CI_low = CI_low, ours_CI_high = CI_high,
            ours_raw_p = raw_p, ours_q = q, ours_FDR_family = FDR_family,
            ours_n_tests_in_family = n_tests_in_family) %>%
  left_join(MAP, by = "Domain") %>%
  left_join(prod, by = c("prod_Domain", "Sex", "contrast")) %>%
  mutate(
    matched_to_production = !is.na(prod_hedges_g),
    diff_hedges_g = ours_hedges_g - prod_hedges_g,
    abs_diff_hedges_g = abs(diff_hedges_g),
    same_sign_hedges_g = sign(ours_hedges_g) == sign(prod_hedges_g),
    n_ref_agrees = ours_n_ref == prod_n_ref, n_comp_agrees = ours_n_comp == prod_n_comp,
    ours_model = paste0("lm(DomainScore ~ Group * Sex) + emmeans(~ Group | Sex), contrasts ",
                        "adjust='none'; ONE pooled model per domain with a Sex interaction; residual SD ",
                        "pooled across all 6 Group x Sex cells"),
    production_model = paste0("Welch two-sample t-test per Group pair within Sex + Wilcoxon rank-sum ",
                              "sensitivity; no interaction term, no pooled residual SD"),
    ours_window = paste0("CANONICAL clock window: CC1, Phase exactly in c('active','dark','night'), ",
                         "per-session anchor = min phase block * 43200 + 23400 (18:30), keep elapsed in ",
                         "[0, 12 h)"),
    production_window = paste0("Stage 14 production rule `local_bin <= 12h/bin` (a fixed COUNT of Active ",
                               "bins, Analysis/14 lines ~967-976) for the raw domains; the HMM domain is ",
                               "not windowed at all but taken from the Stage 08 CC1 x Active per-epoch ",
                               "table (~48 h, four dark blocks)"),
    window_defect = paste0("The production count rule matches the canonical clock window for only 50/111 ",
                           "animals at 10-min and 33/111 at 5-min: whenever an animal has missing bins in ",
                           "night 1 the bin COUNT over-reaches into the SECOND dark block of CC1 (61/111 ",
                           "animals affected at 10-min, overshoot up to 12.67 h). Production is NOT ",
                           "modified here."),
    fdr_difference = paste0("Both use BH within Sex, but over different families: production over its 6 ",
                            "domains x 3 contrasts; ours over 7 displayed domains x 3 contrasts. q values ",
                            "are therefore not directly comparable even where p is."),
    difference_explanation = case_when(
      is.na(prod_Domain) ~ "No production counterpart exists for this domain.",
      is.na(prod_hedges_g) ~ "Production row not found for this Domain x Sex x contrast.",
      prod_Domain == "Behavioral state architecture" ~ paste0(
        "Three simultaneous differences: (1) WINDOW -- ours is the 12 h first night, production's HMM ",
        "domain is the whole ~48 h CC1 Active epoch (four dark blocks); (2) standardization context; ",
        "(3) TEST -- pooled-SD lm/emmeans vs Welch t. The two numbers answer different questions and are ",
        "not expected to agree."),
      TRUE ~ paste0(
        "Two simultaneous differences: (1) WINDOW -- canonical clock 12 h vs the defective local_bin ",
        "bin-count rule; (2) TEST -- one pooled lm(Group*Sex) with emmeans contrasts (shared residual SD, ",
        "Sex interaction) vs pairwise Welch t (per-pair variance). Hedges g is computed identically in ",
        "both (animal-level, pooled-SD, small-sample corrected), so abs_diff_hedges_g isolates the WINDOW ",
        "effect while p differences also carry the test change.")),
    production_not_modified = TRUE,
    script = THIS_SCRIPT) %>%
  arrange(Domain, Sex, contrast)
write_csv(recon, file.path(OUT, "first_night_vs_production_cc1_reconciliation.csv"))
cat("wrote first_night_vs_production_cc1_reconciliation.csv  rows =", nrow(recon),
    " matched to production =", sum(recon$matched_to_production), "\n")
sec("Ours vs production (10-min primary)")
print(as.data.frame(recon %>% transmute(Domain = str_trunc(Domain, 34), Sex, contrast,
  g_ours = r4(ours_hedges_g), g_prod = r4(prod_hedges_g), dg = r4(diff_hedges_g),
  same_sign = same_sign_hedges_g, p_ours = signif(ours_raw_p, 3), p_prod = signif(prod_p, 3),
  q_ours = signif(ours_q, 3), q_prod = signif(prod_p_fdr, 3),
  n_ok = n_ref_agrees & n_comp_agrees)), row.names = FALSE)
sec("Reconciliation summary")
rs <- recon %>% filter(matched_to_production)
cat("  matched cells:", nrow(rs), " | same sign of g:", sum(rs$same_sign_hedges_g, na.rm = TRUE),
    " | median |dg| =", r4(median(rs$abs_diff_hedges_g, na.rm = TRUE)),
    " | max |dg| =", r4(max(rs$abs_diff_hedges_g, na.rm = TRUE)), "\n")
cat("  cells where ours has q<0.05 but production does not:",
    sum(rs$ours_q < 0.05 & !(rs$prod_p_fdr < 0.05), na.rm = TRUE),
    " | production q<0.05 but ours not:",
    sum(rs$prod_p_fdr < 0.05 & !(rs$ours_q < 0.05), na.rm = TRUE), "\n")

## ==========================================================================
hr("STEP 7. Assertion log + README")
## ==========================================================================
asrt <- bind_rows(ASSERT)
write_csv(asrt, file.path(OUT, "first_night_heatmap_assertions.csv"))
cat("wrote first_night_heatmap_assertions.csv  rows =", nrow(asrt),
    " PASS =", sum(asrt$result == "PASS"), " FAIL =", sum(asrt$result == "FAIL"), "\n")
if (any(asrt$result == "FAIL")) print(as.data.frame(asrt %>% filter(result == "FAIL")), row.names = FALSE)

fmt_row <- function(d) paste(sprintf("%s g=%.3f est=%+.3f CI[%.2f,%.2f] p=%.3f q=%.3f (n %d/%d)",
                                     d$contrast, d$Hedges_g, d$model_estimate, d$CI_low, d$CI_high,
                                     d$raw_p, d$q, d$n_ref, d$n_comp), collapse = "; ")
sec("KEY QUESTION: latent-state persistence, first night vs the longitudinal CC1-CC4 result")
for (sx in SEX_LEVELS) {
  cat(" ", sx, ":", fmt_row(eff %>% filter(bin_resolution == PRIMARY_RES, Sex == sx,
                                           Domain == "Latent-state persistence")), "\n")
}
cat("  Longitudinal CC1-CC4 repeated-measures reference (10min, Female, Active, context-z):\n")
cat("    mean_dwell SUS-CON +0.639 (p 0.015); transition_entropy SUS-CON -0.597 (p 0.028);\n")
cat("    self_transition SUS-CON +0.550 (p 0.033), SUS-RES +0.422 (p 0.047);\n")
cat("    occupancy_entropy SUS-CON -0.138 (p 0.562, null).\n")
sec("First-night component analogues of the longitudinal temporal phenotype (Female, 10min)")
print(as.data.frame(comp_res %>% filter(bin_resolution == PRIMARY_RES, Sex == "Female",
    component %in% c("mean_dwell_minutes", "self_transition_probability", "transition_entropy",
                     "occupancy_entropy")) %>%
  transmute(component, contrast, g = r4(hedges_g), est = r4(estimate),
            CI = sprintf("[%.2f,%.2f]", ci_low, ci_high), p = signif(raw_p, 3), q = signif(q, 3))),
  row.names = FALSE)
sec("Same, MALE (10min) -- reported explicitly, not silently skipped")
print(as.data.frame(comp_res %>% filter(bin_resolution == PRIMARY_RES, Sex == "Male",
    component %in% c("mean_dwell_minutes", "self_transition_probability", "transition_entropy",
                     "occupancy_entropy")) %>%
  transmute(component, contrast, g = r4(hedges_g), est = r4(estimate),
            CI = sprintf("[%.2f,%.2f]", ci_low, ci_high), p = signif(raw_p, 3), q = signif(q, 3))),
  row.names = FALSE)

readme <- c(
"# First response to social instability -- first-night (CC1) domain heatmap",
"",
sprintf("Generated by `%s` (audit script; read-only with respect to `Analysis/`, `Functions/` and every production table or figure).", THIS_SCRIPT),
sprintf("Scores and HMM component features are consumed unchanged from `%s`.", UPSTREAM),
"",
"## What the figures show",
"",
"`Fig_first_night_domain_heatmap.{svg,pdf}` -- one tile per displayed behavioural domain x Group",
"contrast, faceted Female | Male. Columns are fixed as `RES-CON`, `SUS-CON`, `SUS-RES`. Fill and the",
"printed number are the **animal-level Hedges g** (sign = comparison minus reference); stars are BH `q`",
"within Sex from the first-night family (`*** q<0.001, ** q<0.01, * q<0.05`). There is no Active /",
"Inactive facet: the panel is defined on a single Active window, so no phase contrast exists.",
"",
"`Fig_first_night_hmm_components.{svg,pdf}` -- the exploratory HMM component scan, with **occupancy",
"composition** and **temporal organization** in separate facet groups so that an occupancy effect and a",
"temporal effect cannot be conflated.",
"",
"## Row definitions (displayed domains, in figure order)",
"",
"| Row | Definition on the first-night window | Guard |",
"|---|---|---|",
"| Behavioral flexibility / predictability | `mean(z(Entropy_mean), z(Entropy_rmssd)) - z(Entropy_acf1)` | higher = less predictable cage-occupancy time series |",
"| Behavioral volatility / fragmentation | `mean(z(Movement_rmssd), z(Entropy_rmssd), z(Proximity_rmssd))` | the two Stage 12 sleep-like terms of the production epoch score are undefined inside one Active window and are documented as omitted, exactly as Stage 14's own first-active variant |",
"| Active-phase adaptation/exploration | `mean(z(Movement_mean), z(Entropy_mean), z(Proximity_mean)) - mean(z(Movement_acf1), z(Entropy_acf1))` | -- |",
"| Social spatial organization | `mean(z(Proximity_mean), z(Proximity_acf1)) - z(Proximity_rmssd)` | RFID proximity is a social-spatial **co-location proxy**, never \"sociability\" |",
"| Latent-state occupancy organization | `0.5 * z(occupancy_entropy) - z(inactive_state_fraction)` | this is the shipped composite: `z(social_state_fraction)` is identically 0, so `mean(z(H), z(social)) - z(inactive)` equals the 0.5-coefficient form exactly. Occupancy is **order-free** -- shuffling the Viterbi sequence leaves it unchanged -- so this is composition, never \"temporal flexibility\" |",
"| Latent-state persistence | `z(mean_dwell_minutes)` | the single persistence row; higher = longer runs = more persistent |",
"| Psychomotor activation | `z(Movement_mean)` | placed last as the **locomotion reference** so every row above can be read against it |",
"",
"All z-scores are taken **within Sex only**: inside a single CC1 Active epoch there is no",
"`Sex x PhaseClass x CageChangeIndex` context left to standardize within.",
"",
"## The ONE canonical window, and why the production `local_bin` rule was not used",
"",
"Window (reconstructed from code, `Analysis/09_early_prediction_model_ladder.R :: select_primary_active_window`):",
"first cage change only; `Phase` exactly in `c(\"active\",\"dark\",\"night\")` after lower-case + trim (never a",
"substring regex, because `\"inactive\"` contains `\"active\"`); per session",
"`target_window_start = min(animalpos_phase_block_index(BinStart)) * 43200 + 23400`, i.e. **18:30**; keep bins",
"whose `elapsed_sec_in_window` lies in `[0, 12 h)`, i.e. **18:30 -> 06:30, exactly 12.0 h**. The anchor is a",
"property of the experimental clock, not of any animal, and is never shifted later.",
"",
"Stage 14's production CC1 rule is instead `local_bin <= 12h/bin`, a fixed **count** of Active bins. That count",
"rule agrees with the clock window for only **50/111 animals at 10-min and 33/111 at 5-min**: whenever an animal",
"has missing bins in night 1 the count runs past 06:30 and reaches into the **second** dark block of CC1",
"(61/111 animals affected at 10-min, overshoot up to 12.67 h). Every raw domain here is therefore rebuilt on the",
"clock window. The production table is left untouched; the two are compared cell by cell in",
"`first_night_vs_production_cc1_reconciliation.csv`.",
"",
"Two checks make the window auditable: the clock window is **identical** to the \"first contiguous Active block\"",
"heuristic for 111/111 animals at both resolutions, and inside the window every animal's sequence is a single",
"contiguous run (`max diff(TimeIndex) == 1`), so no temporal metric bridges a later dark phase.",
"",
"## FDR families",
"",
"| Family | Members | Tests |",
"|---|---|---|",
"| `FIRST_NIGHT__<Sex>__displayed_domains_x_3_contrasts` | 7 displayed domains x 3 contrasts, 10-min **primary** | 21 per Sex |",
"| `FIRST_NIGHT_5MIN_SENSITIVITY__<Sex>__displayed_domains_x_3_contrasts` | same at 5-min, kept **separate** | 21 per Sex |",
"| `FIRST_NIGHT_HMM_COMPONENT_SCAN__<resolution>__<Sex>` | 7 non-excluded HMM components x 3 contrasts, **exploratory** | 21 per resolution x Sex |",
"",
"BH is applied within each family. The 5-min rows are a sensitivity resolution chosen a priori, never on",
"p-values, and are never used to reinterpret the primary family. The component scan is exploratory and its",
"family is not a set of independent tests (switch rate is exactly `1 - self-transition`, and the occupancy",
"composite is a linear combination of two of its own members); this is stated in the table's `fdr_note`.",
"",
"The `Group:Sex` interaction is reported in **separate** tables",
"(`first_night_domain_interaction_tests.csv`, `first_night_hmm_component_interactions.csv`) with",
"`multiplicity_treatment = UNCORRECTED`, one test per domain / component per resolution. It is not a member of",
"any BH family and no `q` is claimed for it. The Sex split is an a priori design feature, not a consequence of",
"this test.",
"",
"## Inferential unit",
"",
"The window is a single epoch, so each animal contributes **exactly one value per domain**. The model is",
"therefore a plain `lm(DomainScore ~ Group * Sex)` with `emmeans(~ Group | Sex)` contrasts",
"(`RES-CON`, `SUS-CON`, `SUS-RES`, `adjust = \"none\"`) -- **not** `lmer`: there is no repeated-measures",
"structure to model and no random effect is identifiable. Bins, latent states and transitions are never",
"treated as independent observations; they are collapsed to the animal before any test. `n_ref` / `n_comp` in",
"every table are true animal counts. Raw domains cover 111/111 animals; the two HMM domains cover 109/111",
"(OQ770 and OQ771 have no complete Movement/Entropy/Proximity bins in CC1 and were removed by Stage 08's",
"fail-closed epoch data-quality rule -- a data-quality exclusion, not identity loss).",
"",
"## Descriptive, not prospective",
"",
"RES and SUS are **later** phenotype labels derived from subsequent CombZ. Every contrast in this panel is a",
"**descriptive association between a first-night measure and a later phenotype label**. The labels were not",
"known at CC1; nothing here is prospective, predictive or causal. The exploratory CombZ table",
"(`first_night_hmm_combz_association.csv`) is doubly non-independent, because RES/SUS were themselves derived",
"from CombZ -- a prospective claim would require the cross-validated Stage 09 framework, in which",
"`Movement_mean` carries most of the cross-validated signal.",
"",
"## Excluded rows, and why",
"",
"* **`Early adaptation / prediction`** -- Stage 14 defines it as `Active-phase adaptation/exploration`",
"  restricted to `min(CageChangeIndex)`. On a CC1-only window that restriction is vacuous, so the two are",
"  **mathematically identical** (`max abs diff = 0`, `r = 1` at both resolutions;",
"  `first_night_duplicate_domain_check_v2.csv`). Only `Active-phase adaptation/exploration` is kept.",
"* **`Top-proximity state occupancy`** -- fails partition robustness: across 5 distinct 10-min HMM optima the",
"  argmax-proximity state's `Proximity_z` is 0.190 / 0.748 / 1.615 / 2.880 and its occupancy 0.564 / 0.396 /",
"  0.169 / 0.029 (19-fold), animal-level agreement between optima falls to `r = 0.117`, and the Female",
"  Inactive RES-CON sign flips. It is computed and kept in the component table with `displayed = FALSE`,",
"  `component_status = excluded_failed_partition_robustness`, sits outside every BH family, and is never",
"  called \"social\".",
"* **Switch rate / self-transition probability as separate domains** -- `state_switch_rate == 1 -",
"  self_transition_probability` exactly, and mean dwell is near-deterministically related to",
"  `1/(1 - P_self)`. The domain heatmap therefore carries **one** persistence row (`mean_dwell_minutes`,",
"  in physical minutes so the two resolutions are comparable). Both mirror rows appear in the component",
"  figure only, explicitly labelled as one construct.",
"",
"## Relationship to the other two panels",
"",
"* **Production longitudinal heatmap** (Stage 14, CC1-CC4 x Active/Inactive, repeated measures via `lmer`):",
"  answers whether a phenotype is present *on average across the whole experiment*. This panel answers whether",
"  it is present *already in the first night*, on one value per animal with `lm`. Effect sizes are comparable",
"  in kind (animal-level Hedges g) but the estimands and the standardization contexts differ (context-z across",
"  `Sex x PhaseClass x CageChangeIndex` there, Sex-only here), so the two are read side by side, never pooled.",
"* **Existing Stage 14 CC1 panel** (`sis_CC1_first_active_domain_contrasts.csv`): same intent, but built on the",
"  defective `local_bin` count window, with pairwise Welch t plus Wilcoxon instead of a pooled",
"  `Group * Sex` model, six domains instead of seven, and its HMM domain taken un-windowed from the ~48 h",
"  Stage 08 CC1 Active epoch. It is reconciled cell by cell in",
"  `first_night_vs_production_cc1_reconciliation.csv` and is **not modified**.",
"",
"## Files written by this script",
"",
"| File | Contents |",
"|---|---|",
"| `first_night_domain_effect_summary.csv` | displayed domains x Sex x contrast; g, estimate, SE, 95% CI, raw p, BH q, family, model provenance; both resolutions |",
"| `first_night_domain_interaction_tests.csv` | `Group:Sex` anova term per domain, uncorrected |",
"| `first_night_hmm_component_results.csv` | 8 HMM components x resolution x Sex x contrast + resolution agreement |",
"| `first_night_hmm_component_interactions.csv` | `Group:Sex` anova term per component, uncorrected |",
"| `first_night_hmm_movement_adjustment.csv` | movement-adjusted refit; construct diagnostic only, p values not comparable |",
"| `first_night_hmm_combz_association.csv` | exploratory continuous CombZ association; never predictive |",
"| `first_night_vs_production_cc1_reconciliation.csv` | ours vs the production CC1 panel, with the reasons |",
"| `first_night_heatmap_assertions.csv` | the provenance / identity / formula assertions re-checked here |",
"| `Fig_first_night_domain_heatmap.{svg,pdf,png}` | the primary figure (10-min) |",
"| `Fig_first_night_hmm_components.{svg,pdf,png}` | the component dissociation figure (10-min) |",
"",
sprintf("Assertions re-checked here: %d, all %s.", nrow(asrt),
        if (all(asrt$result == "PASS")) "PASS" else "SEE THE FAIL ROWS"),
"")
writeLines(readme, file.path(OUT, "first_night_heatmap_readme.md"))
cat("wrote first_night_heatmap_readme.md  lines =", length(readme), "\n")

## ==========================================================================
hr("STEP 8. Figure files on disk")
## ==========================================================================
figs <- c(file.path(OUT, paste0("Fig_first_night_domain_heatmap", c(".svg", ".pdf", ".png"))),
          file.path(OUT, paste0("Fig_first_night_hmm_components", c(".svg", ".pdf", ".png"))))
print(as.data.frame(tibble(file = basename(figs), exists = file.exists(figs),
                           bytes = file.size(figs))), row.names = FALSE)
stopifnot(all(file.exists(figs)), all(file.size(figs) > 5000))
hr("DONE")
