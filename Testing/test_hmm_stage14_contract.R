# Portable regression checks for the canonical HMM -> Stage 14 contract.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})

source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

# A. Numeric aliases use the repository-wide canonical identity contract.
roster <- tibble(
  AnimalNum = c("4", "654"),
  Group = c("SUS", "RES"),
  Sex = c("Male", "Female")
)
alias_input <- tibble(
  AnimalNum = c("0004", "4", "00654", "654"),
  Group = c("SUS", "SUS", "RES", "RES"),
  Sex = c("Male", "Male", "Female", "Female"),
  value = 1:4
)
alias_audit <- audit_hmm_identity(alias_input, roster, "alias fixture")
check(alias_audit$passed, "A: concordant aliases should pass identity reconciliation")
check(setequal(unique(alias_audit$data$AnimalNum), c("4", "654")), "A: 0004/4 and 00654/654 must collapse")
check(sum(alias_audit$alias_audit$alias_merge_required) == 2L, "A: both canonical animals should record merged aliases")

# B. Conflicting phenotype metadata across aliases fails closed before roster inheritance.
conflict_input <- alias_input
conflict_input$Group[conflict_input$AnimalNum == "0004"] <- "RES"
conflict_audit <- audit_hmm_identity(conflict_input, roster, "conflict fixture")
check(!conflict_audit$passed, "B: conflicting alias Group metadata must fail")
conflict_error <- tryCatch({
  assert_hmm_identity_audit(conflict_audit)
  NULL
}, error = function(e) e)
check(inherits(conflict_error, "error"), "B: the identity assertion must stop on conflict")
check(grepl("4", conditionMessage(conflict_error), fixed = TRUE), "B: the error must identify the affected animal")

# D/E/I. Padding cannot cause a Stage 14 loss; Sex is mandatory in the exact
# standardization context; a missing social state is explicit and contributes zero.
state_summary <- tibble(
  State = 1:4,
  Movement_z = c(-2, -1, 5, 0),
  Entropy_z = c(-2, -1, 1, 0),
  Proximity_z = c(-2, 5, -1, -3)
)
state_labels <- annotate_hmm_semantic_states(state_summary, "10min_based")
semantic_audit <- audit_hmm_semantic_categories(state_labels, "10min_based")
check(
  semantic_audit$category_missing[semantic_audit$SemanticState == "social"],
  "I: a missing social semantic state must be detected"
)

occupancy_fixture <- crossing(
  AnimalNum = c("0004", "00654"),
  Phase = c("Active", "Inactive"),
  CageChange = c("CC1", "CC2"),
  State = as.character(1:4)
) %>%
  mutate(
    Group = if_else(AnimalNum == "0004", "SUS", "RES"),
    Sex = if_else(AnimalNum == "0004", "Male", "Female"),
    frac_time = rep(c(0.5, 0.2, 0.2, 0.1), length.out = n()),
    n = 10L
  )
epoch_result <- build_hmm_epoch_scores(occupancy_fixture, state_labels, roster, "10min_based")
check(n_distinct(epoch_result$scores$AnimalNum) == 2L, "D: no animal may be lost solely because of zero padding")
check("Sex" %in% names(epoch_result$scores), "E: Sex must survive HMM epoch score construction")
check(
  all(epoch_result$scores$standardization_context == "Sex x PhaseClass x CageChangeIndex"),
  "E: the HMM scaling context must be Sex x PhaseClass x CageChangeIndex"
)
social_component <- epoch_result$component_audit %>% filter(component == "social_state_fraction")
check(social_component$is_all_zero, "I: missing social states must yield an explicitly audited zero component")
check(grepl("0.5", social_component$mathematical_reduction, fixed = TRUE), "I: reduced composite formula must be reported")
coverage_fixture <- occupancy_fixture %>%
  transmute(
    AnimalNum = canonical_animal_id(AnimalNum), Group, Sex,
    PhaseClass = Phase, CageChangeIndex = as.numeric(sub("CC", "", CageChange))
  ) %>%
  distinct()
coverage_audit <- audit_hmm_coverage(coverage_fixture, epoch_result$scores, "10min_based")
check(all(coverage_audit$animals_missing == 0L), "D: coverage audit must show no formatting-driven animal loss")
check(all(coverage_audit$epochs_expected == coverage_audit$epochs_with_hmm), "D: expected and HMM epoch coverage must agree")

missing_sex_error <- tryCatch({
  strict_standardize_within_context(
    tibble(PhaseClass = "Active", CageChangeIndex = 1, x = 1),
    "x"
  )
  NULL
}, error = function(e) e)
check(inherits(missing_sex_error, "error"), "E: scaling must fail if Sex disappears")
check(grepl("Sex", conditionMessage(missing_sex_error), fixed = TRUE), "E: the missing-context error must name Sex")
sex_specific_fixture <- tibble(
  Sex = rep(c("Female", "Male"), each = 2),
  PhaseClass = "Active",
  CageChangeIndex = 1L,
  value = c(1, 3, 100, 102)
)
sex_specific_scaled <- strict_standardize_within_context(sex_specific_fixture, "value")
check(
  all(abs(sex_specific_scaled$value_z - rep(c(-1, 1) / sqrt(2), 2)) < 1e-10),
  "E: values must actually be scaled separately within Sex"
)

# F. Artifact resolution is an exact configured path, not first-existing-file selection.
resolution_root <- file.path(tempdir(), paste0("hmm_resolution_", as.integer(runif(1, 1, 1e9))))
primary_path <- file.path(
  resolution_root,
  "analysis_ready/06_behavioral_dynamics/hmm_states/10min_based/tables/hmm_state_occupancy.csv"
)
decoy_path <- file.path(
  resolution_root,
  "analysis_ready/06_behavioral_dynamics/hmm_states/5min_based/tables/hmm_state_occupancy.csv"
)
dir.create(dirname(primary_path), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(decoy_path), recursive = TRUE, showWarnings = FALSE)
writeLines("primary", primary_path)
resolved_before <- resolve_configured_hmm_artifact(resolution_root, "10min_based")
writeLines("decoy", decoy_path)
resolved_after <- resolve_configured_hmm_artifact(resolution_root, "10min_based")
check(identical(resolved_before$path, resolved_after$path), "F: adding another resolution must not change the primary source")
check(grepl("10min_based", resolved_after$path, fixed = TRUE), "F: the explicit primary resolution must remain selected")
unlink(resolution_root, recursive = TRUE)

# G/H. Heatmap p-values come from lmerTest/emmeans while Hedges g and n use
# one mean per independent animal, not the repeated cage-change rows.
set.seed(20260902)
model_fixture <- crossing(
  Group = c("CON", "RES", "SUS"),
  Sex = c("Female", "Male"),
  animal_index = 1:8,
  CageChangeIndex = 1:4,
  PhaseClass = c("Active", "Inactive")
) %>%
  mutate(
    AnimalNum = paste(Group, Sex, animal_index, sep = "_"),
    Domain = "Behavioral state architecture",
    animal_intercept = as.numeric(factor(AnimalNum)) / 100,
    DomainScore =
      c(CON = 0, RES = -0.35, SUS = 0.25)[Group] +
      c(Female = 0.10, Male = -0.10)[Sex] +
      0.04 * CageChangeIndex + animal_intercept + rnorm(n(), sd = 0.12)
  )
model_results <- analyze_repeated_measures_heatmap(
  model_fixture,
  "Behavioral state architecture",
  "10min_based"
)$contrasts
check(
  all(model_results$significance_method == "repeated-measures mixed-model emmeans contrast"),
  "G: final heatmap significance must be model/emmeans-based"
)
check(!any(grepl("Welch", model_results$significance_method)), "G: Welch must not be the final heatmap test")
check(
  all(model_results$n_ref_animals == 8L & model_results$n_comp_animals == 8L),
  "H: displayed n must count independent animals"
)
check(
  all(model_results$n_ref_animals < 32L & model_results$n_comp_animals < 32L),
  "H: displayed n must not count the four repeated cage-change rows"
)

# C. The full E9 fixture/data check is enabled automatically when S: is present.
full_data_path <- paste0(
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/",
  "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"
)
if (file.exists(full_data_path)) {
  full_roster_input <- read_csv(
    full_data_path,
    col_types = cols(
      .default = col_skip(),
      AnimalNum = col_character(),
      Group = col_character(),
      Sex = col_character()
    ),
    progress = FALSE
  )
  full_roster <- build_canonical_identity_roster(full_roster_input, "full E9 Stage 01 fixture")
  check(nrow(full_roster) == 111L, "C: full E9 Stage 01 data must resolve to 111 biological animals")

  for (resolution in c("5min_based", "10min_based")) {
    hmm_table_dir <- file.path(
      dirname(dirname(dirname(full_data_path))),
      "06_behavioral_dynamics/hmm_states",
      resolution,
      "tables"
    )
    qc_path <- file.path(hmm_table_dir, "hmm_model_qc.csv")
    occupancy_path <- file.path(hmm_table_dir, "hmm_state_occupancy.csv")
    if (file.exists(qc_path) && file.exists(occupancy_path)) {
      hmm_qc <- read_csv(qc_path, show_col_types = FALSE)
      hmm_occupancy <- read_csv(
        occupancy_path,
        col_types = cols(AnimalNum = col_character()),
        show_col_types = FALSE
      )
      occupancy_roster <- build_canonical_identity_roster(hmm_occupancy, paste(resolution, "occupancy"))
      check(hmm_qc$n_animals[[1]] == 111L, paste("C:", resolution, "HMM must retain 111 animals"))
      check(hmm_qc$n_occupied_states[[1]] == 4L, paste("I:", resolution, "HMM must occupy all four fitted states"))
      check(
        setequal(occupancy_roster$AnimalNum, full_roster$AnimalNum),
        paste("D:", resolution, "HMM and current Stage 01 rosters must match exactly")
      )
      check(
        identical(canonical_animal_id(hmm_occupancy$AnimalNum), as.character(hmm_occupancy$AnimalNum)),
        paste("A:", resolution, "HMM outputs must contain canonical IDs only")
      )
    }
  }

  stage14_dir <- file.path(
    dirname(dirname(dirname(full_data_path))),
    "12_systems_neuroscience_summary/5min_based"
  )
  stage14_identity_path <- file.path(stage14_dir, "tables/systems_hmm_identity_summary.csv")
  stage14_coverage_path <- file.path(stage14_dir, "tables/systems_stage14_hmm_coverage_audit.csv")
  stage14_context_path <- file.path(stage14_dir, "tables/systems_hmm_standardization_context_audit.csv")
  stage14_semantic_path <- file.path(stage14_dir, "tables/systems_hmm_semantic_category_audit.csv")
  stage14_provenance_path <- file.path(stage14_dir, "tables/systems_hmm_resolution_provenance.csv")
  stage14_effect_path <- file.path(stage14_dir, "stats_tables/systems_sis_domain_effect_summary.csv")
  stage14_sensitivity_path <- file.path(stage14_dir, "stats_tables/systems_sis_hmm_resolution_sensitivity.csv")
  if (all(file.exists(c(
    stage14_identity_path, stage14_coverage_path, stage14_context_path,
    stage14_semantic_path, stage14_provenance_path, stage14_effect_path,
    stage14_sensitivity_path
  )))) {
    stage14_identity <- read_csv(stage14_identity_path, show_col_types = FALSE)
    stage14_coverage <- read_csv(stage14_coverage_path, show_col_types = FALSE)
    stage14_context <- read_csv(stage14_context_path, show_col_types = FALSE)
    stage14_semantic <- read_csv(stage14_semantic_path, show_col_types = FALSE)
    stage14_provenance <- read_csv(stage14_provenance_path, show_col_types = FALSE)
    stage14_effect <- read_csv(stage14_effect_path, show_col_types = FALSE)
    stage14_sensitivity <- read_csv(stage14_sensitivity_path, show_col_types = FALSE)

    check(all(stage14_identity$passed), "D: all live Stage 14 HMM identity audits must pass")
    check(all(stage14_identity$canonical_animals == 111L), "C: every live Stage 14 HMM table must contain 111 canonical animals")
    check(all(stage14_coverage$animals_missing == 0L), "D: live Stage 14 must have no animal-level HMM loss")
    check(
      all(stage14_context$standardization_grouping_variables == "Sex x PhaseClass x CageChangeIndex"),
      "E: live Stage 14 HMM scores must record the exact sex-specific context"
    )
    check(
      all(stage14_semantic$category_missing[stage14_semantic$SemanticState == "social"]),
      "I: live semantic audits must flag the absent social category at both resolutions"
    )
    check(
      all(stage14_provenance$configured_primary == (stage14_provenance$resolution == "10min_based")) &&
        all(stage14_provenance$selected_for_primary_heatmap == (stage14_provenance$resolution == "10min_based")),
      "F: live provenance must select only the prespecified 10-min HMM as primary"
    )
    check(
      all(
        stage14_effect$significance_method[is.finite(stage14_effect$mixed_model_p)] ==
          "repeated-measures mixed-model emmeans contrast"
      ),
      "G: every live heatmap p-value must be a repeated-measures model contrast"
    )
    check(
      all(
        stage14_effect$n_ref == stage14_effect$n_ref_animals &
          stage14_effect$n_comp == stage14_effect$n_comp_animals,
        na.rm = TRUE
      ),
      "H: compatibility n columns in the live heatmap table must equal animal counts"
    )
    female_active <- stage14_sensitivity %>%
      filter(Domain == "Behavioral state architecture", Sex == "Female", Phase == "Active")
    check(setequal(female_active$resolution, c("5min_based", "10min_based")), "F: live sensitivity must include both configured HMM resolutions")
    check(
      n_distinct(round(female_active$animal_level_hedges_g, 10)) > 3L,
      "F: live 5-min and 10-min sensitivity effects must not be duplicate routed inputs"
    )
  }
}

# Structural regression: the executable Stage 14 scaling helper is strict and
# the broad heatmap is driven by the repeated-measures helper.
stage14_source <- paste(readLines("Analysis/14_systems_neuroscience_summary_dashboard.R", warn = FALSE), collapse = "\n")
check(
  grepl("strict_standardize_within_context(dat, value_col, group_cols)", stage14_source, fixed = TRUE),
  "E: Stage 14 must delegate context scaling to the strict helper"
)
check(
  grepl("analyze_repeated_measures_heatmap", stage14_source, fixed = TRUE),
  "G: Stage 14 must use the repeated-measures heatmap engine"
)
check(
  grepl(".data$resolution == .env$resolution", stage14_source, fixed = TRUE),
  "F: Stage 14 resolution filtering must compare the column with the configured function argument"
)

cat("HMM / Stage 14 identity and inference contract checks: PASS\n")
