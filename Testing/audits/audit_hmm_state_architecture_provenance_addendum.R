# ==============================================================================
# AUDIT ADDENDUM (read-only): marginal Sex / CageChangeIndex terms in the
# primary heatmap model, given that the score is z-scored inside
# Sex x PhaseClass x CageChangeIndex.
#
# Also records which Stage 08 HMM artifacts are consumed by the heatmap path.
# ==============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
  library(stringr); library(tibble)
})

repo <- "C:/Users/topohl/Documents/GitHub/MMMSociability"
setwd(repo)
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")

project <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
audit_out <- file.path(project, "analysis_ready/12_systems_neuroscience_summary/5min_based",
                       "audit_hmm_state_architecture")
dir.create(audit_out, recursive = TRUE, showWarnings = FALSE)

resolutions <- c("5min_based", "10min_based")
hmm_dir <- function(res) {
  file.path(project, "analysis_ready/06_behavioral_dynamics/hmm_states", res, "tables")
}

roster_raw <- read_csv(
  file.path(project, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
  col_types = cols(.default = col_skip(), AnimalNum = col_character(),
                   Group = col_character(), Sex = col_character()),
  progress = FALSE
)
roster <- build_canonical_identity_roster(roster_raw, "Stage 01 5min_based roster")

labels <- map(set_names(resolutions), function(res) {
  annotate_hmm_semantic_states(
    read_csv(file.path(hmm_dir(res), "hmm_state_summary.csv"), show_col_types = FALSE), res)
})
occ <- map(set_names(resolutions), function(res) {
  d <- read_csv(file.path(hmm_dir(res), "hmm_state_occupancy.csv"),
                col_types = cols(AnimalNum = col_character()))
  assert_hmm_identity_audit(audit_hmm_identity(d, roster, paste("occupancy", res)))
  d
})
epoch <- imap(occ, ~ build_hmm_epoch_scores(.x, labels[[.y]], roster, .y))

domain_dat <- imap_dfr(epoch, function(e, res) {
  e$scores %>%
    transmute(resolution = res, AnimalNum, Group, Sex, CageChange, CageChangeIndex,
              PhaseClass, Domain = "Behavioral state architecture",
              DomainScore = .data[["Behavioral state architecture"]]) %>%
    filter(is.finite(DomainScore))
})

# ---- Type III anova for Sex and CageChangeIndex, + marginal Sex EMM ----------
anova_tbl <- map_dfr(resolutions, function(res) {
  map_dfr(c("Active", "Inactive"), function(ph) {
    md <- domain_dat %>%
      filter(resolution == res, PhaseClass == ph) %>%
      transmute(AnimalNum = factor(AnimalNum),
                Group = factor(Group, levels = c("CON", "RES", "SUS")),
                Sex = factor(Sex, levels = c("Female", "Male")),
                CageChangeIndex = factor(CageChangeIndex),
                DomainScore = as.numeric(DomainScore))
    fit <- lmerTest::lmer(DomainScore ~ Group * Sex + factor(CageChangeIndex) + (1 | AnimalNum),
                          data = md)
    a <- as.data.frame(anova(fit)) %>% rownames_to_column("term") %>% as_tibble()
    emm_sex <- emmeans::emmeans(fit, ~ Sex) %>% as.data.frame() %>% as_tibble() %>%
      transmute(term = paste0("EMM_Sex_", Sex), Estimate = emmean, SE, df)
    emm_cc <- emmeans::emmeans(fit, ~ CageChangeIndex) %>% as.data.frame() %>% as_tibble() %>%
      transmute(term = paste0("EMM_CC", CageChangeIndex), Estimate = emmean, SE, df)
    bind_rows(
      a %>% mutate(kind = "anova"),
      emm_sex %>% mutate(kind = "emmean"),
      emm_cc %>% mutate(kind = "emmean")
    ) %>% mutate(resolution = res, PhaseClass = ph, .before = 1)
  })
})
write_csv(anova_tbl, file.path(audit_out, "audit_doc_v6_sex_and_cc_terms.csv"))
print(anova_tbl %>% filter(kind == "anova") %>%
        select(resolution, PhaseClass, term, `F value`, NumDF, DenDF, `Pr(>F)`), n = 40)
print(anova_tbl %>% filter(kind == "emmean") %>%
        select(resolution, PhaseClass, term, Estimate, SE), n = 60)

# ---- Which Stage 08 artifacts reach the heatmap value? -----------------------
consumed <- tribble(
  ~stage08_table, ~written_at, ~heatmap_consumer, ~reaches_heatmap_value,
  "hmm_state_summary.csv", "08:417",
  "annotate_hmm_semantic_states() -> SemanticState labels only (14:3191-3194, helpers:203-230)", TRUE,
  "hmm_state_occupancy.csv", "08:464",
  "build_hmm_epoch_scores(occupancy=...) frac_time (14:5372-5375, helpers:280-306)", TRUE,
  "hmm_state_assignments.csv", "08:405",
  "not read by build_hmm_epoch_scores; not in hmm_required_filenames (14:3121-3126)", FALSE,
  "hmm_transition_probabilities.csv", "08:433",
  "hmm_bundles$transition_prob -> Fig_systems_hmm_transition_difference / alluvial (14:3221-3299); CC1 panel (14:5721-5735); load_hmm_system_features (14:1208-1226)", FALSE,
  "hmm_transition_counts.csv", "08:427",
  "not read anywhere in Stage 14", FALSE,
  "hmm_state_dwell_times.csv", "08:456",
  "hmm_bundles$dwell -> Fig_systems_hmm_dwell_time_ridges (14:3303-3326); CC1 panel (14:5709-5718); load_hmm_system_features (14:1194-1206)", FALSE,
  "epoch_duration_qc.csv", "08:294",
  "not read by the heatmap path", FALSE,
  "hmm_model_qc.csv", "08:401",
  "not read by the heatmap path (used by Testing/tests/test_hmm_stage14_contract.R:196-207)", FALSE
)
write_csv(consumed, file.path(audit_out, "audit_doc_v7_artifact_consumption_map.csv"))
print(consumed)

files_present <- map_dfr(resolutions, function(res) {
  tibble(resolution = res, file = list.files(hmm_dir(res), pattern = "\\.csv$"))
})
write_csv(files_present, file.path(audit_out, "audit_doc_v7b_stage08_files_present.csv"))
cat("stage08 csv files per resolution:",
    paste(capture.output(print(table(files_present$resolution))), collapse = " "), "\n")

cat("\nADDENDUM COMPLETE\n")
