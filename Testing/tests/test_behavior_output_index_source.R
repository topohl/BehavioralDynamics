# Validate the Stage 16 index definition without running Stage 16 or writing to S:.
source_lines <- readLines("Analysis/16_manuscript_behavior_report.R", warn = FALSE)
start <- grep("^output_group_index <- function\\(group\\) \\{$", source_lines)
end <- grep("^if \\(anyDuplicated\\(na.omit\\(output_index\\$canonical_path\\)\\)\\) \\{$", source_lines)
stopifnot(length(start) == 1L, length(end) == 1L, end > start)
index_source <- paste(source_lines[start:(end - 1L)], collapse = "\n")
env <- new.env(parent = baseenv())
env$tribble <- tibble::tribble
source("Functions/project_paths.R", local = env)
fixture_root <- file.path(tempdir(), "behavior_output_path_fixture")
foundation_group <- "behavior_metrics_foundation"
foundation_current <- env$mmm_behavior_output_group_root(foundation_group, "current", fixture_root)
foundation_semantic <- env$mmm_behavior_output_group_root(foundation_group, "semantic", fixture_root)
foundation_receipt <- file.path(fixture_root, "analysis_ready", "_migration_control",
                                paste0(foundation_group, ".json"))
stopifnot(identical(foundation_current,
                    file.path(fixture_root, "analysis_ready", "03_derived_metrics")),
          identical(foundation_semantic,
                    file.path(fixture_root, "analysis_ready", "foundations/behavior_metrics")))
dir.create(foundation_current, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(foundation_receipt), recursive = TRUE, showWarnings = FALSE)
stopifnot(identical(env$mmm_derived_metrics_output_root(fixture_root, NULL), foundation_current))
cookiehab_root <- file.path(fixture_root, "cookiehab", "analysis_ready", "03_derived_metrics")
stopifnot(identical(env$mmm_derived_metrics_output_root(fixture_root, cookiehab_root),
                    cookiehab_root),
          inherits(try(env$mmm_derived_metrics_output_root(fixture_root, ""),
                       silent = TRUE), "try-error"))
foundation_record <- list(group = foundation_group, state = "prepared", files = 1L,
                          group_plan_sha256 = paste(rep("a", 64), collapse = ""),
                          contract_sha256 = paste(rep("b", 64), collapse = ""),
                          target_root_rel = "foundations/behavior_metrics",
                          source_retained = TRUE)
jsonlite::write_json(foundation_record, foundation_receipt, auto_unbox = TRUE)
stopifnot(identical(env$mmm_derived_metrics_output_root(fixture_root, NULL), foundation_current))
dir.create(foundation_semantic, recursive = TRUE, showWarnings = FALSE)
stopifnot(inherits(try(env$mmm_derived_metrics_output_root(fixture_root, NULL),
                     silent = TRUE), "try-error"))
foundation_record$state <- "activated"
jsonlite::write_json(foundation_record, foundation_receipt, auto_unbox = TRUE)
stopifnot(identical(env$mmm_derived_metrics_output_root(fixture_root, NULL), foundation_semantic),
          identical(env$mmm_derived_metrics_output_root(fixture_root, cookiehab_root),
                    cookiehab_root))
env$base_dir <- fixture_root
eval(parse(text = index_source), envir = env)
foundation_index_row <- env$output_index[env$output_index$stage == "01", ]
stopifnot(nrow(foundation_index_row) == 1L,
          identical(foundation_index_row$canonical_path,
                    "analysis_ready/foundations/behavior_metrics/"),
          identical(foundation_index_row$status, "migrated_source_retained"),
          identical(foundation_index_row$legacy_path,
                    "analysis_ready/03_derived_metrics/"))
identity_history <- env$output_index[env$output_index$stage == "01-identity-history", ]
stopifnot(nrow(identity_history) == 1L,
          is.na(identity_history$canonical_path),
          identical(identity_history$status, "historical_source_retained"),
          identical(identity_history$legacy_path,
                    "analysis_ready/03_derived_metrics/qc/"))
hmm_current <- env$output_index[env$output_index$stage == "08-audit-current", ]
hmm_history <- env$output_index[env$output_index$stage == "08-audit-history", ]
stopifnot(nrow(hmm_current) == 1L,
          nrow(hmm_history) == 1L,
          identical(hmm_current$canonical_path,
                    "analysis_ready/analyses/hmm_revalidation_runs/current_stage08_review_20260924/"),
          identical(hmm_current$status, "current_revalidation_unpromoted"),
          is.na(hmm_history$canonical_path),
          identical(hmm_history$status, "historical_source_retained"),
          identical(hmm_current$legacy_path, hmm_history$legacy_path))
unlink(foundation_receipt)
unlink(foundation_semantic, recursive = TRUE)
stage01_source <- paste(readLines("Analysis/01_build_multiscale_behavior_metrics.R",
                                  warn = FALSE), collapse = "\n")
stopifnot(grepl("output_root <- mmm_behavior_guard_numbered_output_path(",
               stage01_source, fixed = TRUE),
          grepl("  mmm_derived_metrics_output_root())",
               stage01_source, fixed = TRUE))
foundation_readers <- c(
  "Analysis/00_qc_tracking_integrity.R",
  "Analysis/03_primary_raw_movement_phase_stats.R",
  "Analysis/04_temporal_instability.R",
  "Analysis/05_behavioral_state_space.R",
  "Analysis/06_dynamic_social_networks.R",
  "Analysis/07_gamm_trajectory_features.R",
  "Analysis/08_hmm_behavioral_states_optional.R",
  "Analysis/09_early_prediction_model_ladder.R",
  "Analysis/11_behavioral_adaptation_kinetics.R",
  "Analysis/12_sleep_like_quiescence_metrics.R",
  "Analysis/13_ethological_phase_organization.R",
  "Analysis/14_systems_neuroscience_summary_dashboard.R",
  "Analysis/15_behavior_proteomics_integration.R",
  "Analysis/20_first_night_gamm.R",
  "Analysis/21_cc1_active_longitudinal_gamm.R",
  "Analysis/22_repeated_cagechange_acute_gamm.R",
  "Analysis/23_first_inactive_gamm.R",
  "Analysis/24_cc1_inactive_longitudinal_gamm.R",
  "Analysis/25_repeated_cagechange_inactive_gamm.R",
  "Analysis/28_rfid_behavioral_domains.R",
  "Analysis/_supporting/13_nonlinear_systems_dynamics.R",
  "Analysis/_supporting/14_nextgen_behavioral_phenotyping.R",
  "Functions/first_night_domain_driver.R"
)
for (reader_file in foundation_readers) {
  reader_source <- paste(readLines(reader_file, warn = FALSE), collapse = "\n")
  stopifnot(grepl("mmm_derived_metrics_output_root", reader_source, fixed = TRUE),
            !grepl('file.path(project_root, "analysis_ready/03_derived_metrics"',
                   reader_source, fixed = TRUE),
            !grepl('file.path(base_dir, "analysis_ready/03_derived_metrics"',
                   reader_source, fixed = TRUE))
}
identity_audit_source <- paste(readLines(
  "Testing/audits/validate_cross_scale_animal_identity.R", warn = FALSE),
  collapse = "\n")
stopifnot(grepl("mmm_derived_metrics_output_root(base_dir)",
               identity_audit_source, fixed = TRUE),
          grepl('"cross_scale_identity_validation"',
                identity_audit_source, fixed = TRUE),
          !grepl('out_dir <- file.path(derived_metrics_root, "qc")',
                 identity_audit_source, fixed = TRUE))
env$base_dir <- fixture_root
eval(parse(text = index_source), envir = env)
idx <- env$output_index

stopifnot(!anyDuplicated(na.omit(idx$canonical_path)))
row <- function(id) idx[idx$stage == id, , drop = FALSE]
stopifnot(nrow(row("10")) == 1L,
          nrow(row("01")) == 1L,
          is.na(row("01")$canonical_path),
          identical(row("01")$status, "legacy_pending_migration"),
          identical(row("01")$legacy_path,
                    "analysis_ready/03_derived_metrics/"),
          identical(row("10")$canonical_path, "analysis_ready/pipeline/10_systems_prediction/10min/"),
          is.na(row("10")$legacy_path),
          nrow(row("14-first-night-10min")) == 1L,
          nrow(row("14-first-night-5min")) == 1L,
          grepl("/first_night/", row("14-first-night-10min")$legacy_path, fixed = TRUE),
          nrow(row("15-primary")) == 1L,
          nrow(row("15-sensitivity")) == 1L,
          nrow(row("11-history")) == 1L,
          nrow(row("12-history")) == 1L,
          nrow(row("13-history")) == 1L,
          nrow(row("support-13")) == 1L,
          nrow(row("support-14")) == 1L,
          nrow(row("14-inactive-qc-audit")) == 1L,
          nrow(row("14-rfid-domain-comparison-audit")) == 1L,
          identical(row("14-rfid-domain-comparison-audit")$status,
                    "legacy_pending_migration"),
          nrow(row("14-rfid-leading-bin-seed-audit")) == 1L,
          identical(row("14-rfid-leading-bin-seed-audit")$status,
                    "legacy_pending_migration"),
          all(vapply(c("14-rfid-construct-audit", "14-rfid-conservatism-audit",
                       "14-rfid-alternative-inference-audit",
                       "14-rfid-reliability-audit"),
                     function(id) nrow(row(id)) == 1L &&
                       identical(row(id)$status, "legacy_pending_migration"),
                     logical(1))),
          nrow(row("02")) == 1L,
          nrow(row("06")) == 1L,
          nrow(row("06-history")) == 1L,
          nrow(row("07")) == 1L,
          nrow(row("07-history")) == 1L,
          nrow(row("05")) == 1L,
          nrow(row("05-history")) == 1L,
          nrow(row("08")) == 1L,
          nrow(row("08-5min")) == 1L,
          nrow(row("04")) == 1L,
          nrow(row("04-history")) == 1L,
          identical(row("04")$legacy_path,
                    "analysis_ready/06_behavioral_dynamics/temporal_instability/10sec_based/"),
          identical(row("08")$legacy_path,
                    "analysis_ready/06_behavioral_dynamics/hmm_states/10min_based/"),
          identical(row("05")$legacy_path,
                    "analysis_ready/06_behavioral_dynamics/state_space/5min_based/"),
          is.na(row("05-history")$canonical_path),
          identical(row("07")$legacy_path,
                    "analysis_ready/06_behavioral_dynamics/gamm_features/10min_based/"),
          is.na(row("07-history")$canonical_path),
          identical(row("06")$legacy_path,
                    "analysis_ready/06_behavioral_dynamics/social_networks/5min_based/"),
          is.na(row("06-history")$canonical_path),
          nrow(row("15-sensitivity")) == 1L,
          nrow(row("19-tables")) == 1L,
          nrow(row("19-audit")) == 1L,
          nrow(row("19-models")) == 1L,
          nrow(row("19-figures")) == 1L,
          nrow(row("28")) == 1L,
          identical(row("28")$status, "local_untracked_candidate"))
stopifnot(!any(grepl("_quarantine|_archive", na.omit(idx$legacy_path))))
history_ids <- c("04-history-1min", "04-history-5min",
                 "05-history-1min", "05-history-10min",
                 "06-history-10sec", "06-history-1min",
                 "06-history-10min", "06-history-30min",
                 "07-history-30min")
stopifnot(all(vapply(history_ids, function(id) nrow(row(id)) == 1L &&
                       is.na(row(id)$canonical_path) &&
                       identical(row(id)$status, "legacy_pending_migration") &&
                       startsWith(row(id)$legacy_path,
                                  "analysis_ready/06_behavioral_dynamics/"),
                     logical(1))))

# One activated history receipt updates only its own detail row. Aggregate
# family rows remain provenance overviews and other resolutions remain old.
history_group <- "history_social_networks_10min"
history_old <- env$mmm_behavior_output_group_root(history_group, "current", fixture_root)
history_new <- env$mmm_behavior_output_group_root(history_group, "semantic", fixture_root)
history_receipt <- file.path(fixture_root, "analysis_ready", "_migration_control",
                             paste0(history_group, ".json"))
dir.create(history_old, recursive = TRUE, showWarnings = FALSE)
dir.create(history_new, recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(list(
  group = history_group, state = "activated", files = 1L,
  target_root_rel = "history/social_networks/10min",
  group_plan_sha256 = paste(rep("c", 64L), collapse = ""),
  contract_sha256 = paste(rep("d", 64L), collapse = ""),
  source_retained = TRUE), history_receipt, auto_unbox = TRUE)
eval(parse(text = index_source), envir = env)
activated_index <- env$output_index
activated_row <- activated_index[activated_index$stage == "06-history-10min", ]
stopifnot(identical(activated_row$canonical_path,
                    "analysis_ready/history/social_networks/10min/"),
          identical(activated_row$status, "migrated_source_retained"),
          is.na(activated_index$canonical_path[
            activated_index$stage == "06-history-1min"]),
          is.na(activated_index$canonical_path[
            activated_index$stage == "06-history"]))
unlink(history_receipt)
unlink(history_new, recursive = TRUE)
unlink(history_old, recursive = TRUE)
eval(parse(text = index_source), envir = env)
cat("PASS: Stage 16 output-index source paths and physical output groups\n")

# Path accessors are explicit and stay on the existing tree by default.
source("Analysis/_pipeline_setup.R")
source_mmm_helper("project_paths.R")
stopifnot(identical(
  mmm_behavior_output_group_root("first_night_10min", project_root = fixture_root),
  file.path(fixture_root, "analysis_ready", "12_systems_neuroscience_summary/5min_based/first_night/10min_based")))
stopifnot(identical(mmm_source_relative_path(
  file.path(fixture_root, "analysis_ready", "analyses/systems_dashboard/5min/stats_tables/example.csv"),
  fixture_root), "analyses/systems_dashboard/5min/stats_tables/example.csv"),
  inherits(try(mmm_source_relative_path(
    file.path(fixture_root, "elsewhere/example.csv"), fixture_root), silent = TRUE),
    "try-error"))
stopifnot(identical(
  mmm_behavior_output_group_root("spatial_models", layout = "semantic", project_root = fixture_root),
  file.path(fixture_root, "analysis_ready", "analyses/spatial_occupancy/models")))
stopifnot(identical(
  mmm_behavior_output_group_root("inactive_phase_qc_audit", project_root = fixture_root),
  file.path(fixture_root, "analysis_ready",
            "12_systems_neuroscience_summary/5min_based/audit_inactive_phase_qc")),
  identical(
    mmm_behavior_output_group_root("inactive_phase_qc_audit", "semantic", fixture_root),
    file.path(fixture_root, "analysis_ready/analyses/inactive_phase_qc_audit")))
stopifnot(identical(
  mmm_behavior_output_group_root("rfid_domain_comparison_audit", project_root = fixture_root),
  file.path(fixture_root, "analysis_ready",
            "12_systems_neuroscience_summary/5min_based/audit_rfid_legacy_vs_new")),
  identical(
    mmm_behavior_output_group_root("rfid_domain_comparison_audit", "semantic", fixture_root),
    file.path(fixture_root, "analysis_ready/analyses/rfid_domain_comparison_audit")))
stopifnot(identical(
  mmm_social_network_resolution_root("5min_based", fixture_root),
  file.path(fixture_root, "analysis_ready", "06_behavioral_dynamics/social_networks/5min_based")),
  identical(mmm_social_network_resolution_root("10min_based", fixture_root),
            file.path(fixture_root, "analysis_ready", "06_behavioral_dynamics/social_networks/10min_based")),
  inherits(try(mmm_social_network_resolution_root("invalid", fixture_root),
               silent = TRUE), "try-error"))
stopifnot(inherits(try(mmm_behavior_output_group_root("unknown", project_root = fixture_root),
                     silent = TRUE), "try-error"))
stopifnot(identical(mmm_behavior_output_layout_state("first_night_10min", fixture_root),
                    "current"))
current <- mmm_behavior_output_group_root("first_night_10min", "current", fixture_root)
semantic <- mmm_behavior_output_group_root("first_night_10min", "semantic", fixture_root)
receipt <- file.path(fixture_root, "analysis_ready", "_migration_control",
                     "first_night_10min.json")
dir.create(current, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(receipt), recursive = TRUE, showWarnings = FALSE)
record <- list(group = "first_night_10min", state = "prepared", files = 10L,
               group_plan_sha256 = paste(rep("a", 64), collapse = ""),
               contract_sha256 = paste(rep("b", 64), collapse = ""),
               target_root_rel = "analyses/first_night_five_domain_characterization/10min",
               source_retained = TRUE)
jsonlite::write_json(record, receipt, auto_unbox = TRUE)
stopifnot(identical(mmm_behavior_output_layout_state("first_night_10min", fixture_root),
                    "current"))
dir.create(semantic, recursive = TRUE, showWarnings = FALSE)
stopifnot(inherits(try(mmm_behavior_output_active_root("first_night_10min", fixture_root),
                     silent = TRUE), "try-error"))
record$state <- "activated"
jsonlite::write_json(record, receipt, auto_unbox = TRUE)
stopifnot(identical(mmm_behavior_output_active_root("first_night_10min", fixture_root),
                    semantic),
          identical(mmm_behavior_output_index_entry("first_night_10min", fixture_root)$status,
                    "migrated_source_retained"))
eval(parse(text = index_source), envir = env)
activated_row <- env$output_index[env$output_index$stage == "14-first-night-10min", ]
stopifnot(nrow(activated_row) == 1L,
          identical(activated_row$canonical_path,
                    "analysis_ready/analyses/first_night_five_domain_characterization/10min/"),
          identical(activated_row$status, "migrated_source_retained"),
          is.na(env$output_index$canonical_path[
            env$output_index$stage == "14-first-night-5min"]))
stopifnot(inherits(try(mmm_behavior_output_assert_uniform_layout(
  c("first_night_10min", "first_night_5min"), fixture_root, "Stage 14"),
  silent = TRUE), "try-error"))
unlink(receipt)
stopifnot(inherits(try(mmm_behavior_output_active_root("first_night_10min", fixture_root),
                     silent = TRUE), "try-error"))
unlink(semantic, recursive = TRUE)
dashboard_current <- mmm_behavior_output_group_root(
  "systems_dashboard_5min", "current", fixture_root)
dashboard_semantic <- mmm_behavior_output_group_root(
  "systems_dashboard_5min", "semantic", fixture_root)
dashboard_receipt <- file.path(fixture_root, "analysis_ready", "_migration_control",
                               "systems_dashboard_5min.json")
dir.create(dashboard_current, recursive = TRUE, showWarnings = FALSE)
stopifnot(identical(mmm_behavior_output_active_root("systems_dashboard_5min", fixture_root),
                    dashboard_current),
          identical(.mmm_path_specs()[["behavior.combz_components"]]$dir(fixture_root),
                    file.path(dashboard_current, "tables")),
          identical(.mmm_path_specs()[["behavior.rfid_domain_summary_broad"]]$dir(fixture_root),
                    file.path(dashboard_current, "stats_tables")))
dashboard_record <- list(group = "systems_dashboard_5min", state = "prepared", files = 1L,
                         group_plan_sha256 = paste(rep("a", 64), collapse = ""),
                         contract_sha256 = paste(rep("b", 64), collapse = ""),
                         target_root_rel = "analyses/systems_dashboard/5min",
                         source_retained = TRUE)
jsonlite::write_json(dashboard_record, dashboard_receipt, auto_unbox = TRUE)
stopifnot(identical(mmm_behavior_output_active_root("systems_dashboard_5min", fixture_root),
                    dashboard_current))
dir.create(dashboard_semantic, recursive = TRUE, showWarnings = FALSE)
stopifnot(inherits(try(mmm_behavior_output_active_root("systems_dashboard_5min", fixture_root),
                     silent = TRUE), "try-error"))
dashboard_record$state <- "activated"
jsonlite::write_json(dashboard_record, dashboard_receipt, auto_unbox = TRUE)
stopifnot(identical(mmm_behavior_output_active_root("systems_dashboard_5min", fixture_root),
                    dashboard_semantic),
          identical(.mmm_path_specs()[["behavior.combz_components"]]$dir(fixture_root),
                    file.path(dashboard_semantic, "tables")),
          identical(.mmm_path_specs()[["behavior.rfid_domain_summary_broad"]]$dir(fixture_root),
                    file.path(dashboard_semantic, "stats_tables")))
eval(parse(text = index_source), envir = env)
dashboard_row <- env$output_index[env$output_index$stage == "14", ]
stopifnot(nrow(dashboard_row) == 1L,
          identical(dashboard_row$canonical_path,
                    "analysis_ready/analyses/systems_dashboard/5min/"),
          identical(dashboard_row$status, "migrated_source_retained"))
unlink(dashboard_receipt)
unlink(dashboard_semantic, recursive = TRUE)
comparison_current <- mmm_behavior_output_group_root(
  "rfid_domain_comparison_audit", "current", fixture_root)
comparison_semantic <- mmm_behavior_output_group_root(
  "rfid_domain_comparison_audit", "semantic", fixture_root)
comparison_receipt <- file.path(fixture_root, "analysis_ready", "_migration_control",
                                "rfid_domain_comparison_audit.json")
dir.create(comparison_current, recursive = TRUE, showWarnings = FALSE)
stopifnot(identical(mmm_behavior_output_active_root(
  "rfid_domain_comparison_audit", fixture_root), comparison_current))
comparison_record <- list(group = "rfid_domain_comparison_audit", state = "prepared",
                          files = 4L,
                          group_plan_sha256 = paste(rep("a", 64), collapse = ""),
                          contract_sha256 = paste(rep("b", 64), collapse = ""),
                          target_root_rel = "analyses/rfid_domain_comparison_audit",
                          source_retained = TRUE)
jsonlite::write_json(comparison_record, comparison_receipt, auto_unbox = TRUE)
stopifnot(identical(mmm_behavior_output_active_root(
  "rfid_domain_comparison_audit", fixture_root), comparison_current))
dir.create(comparison_semantic, recursive = TRUE, showWarnings = FALSE)
stopifnot(inherits(try(mmm_behavior_output_active_root(
  "rfid_domain_comparison_audit", fixture_root), silent = TRUE), "try-error"))
comparison_record$state <- "activated"
jsonlite::write_json(comparison_record, comparison_receipt, auto_unbox = TRUE)
stopifnot(identical(mmm_behavior_output_active_root(
  "rfid_domain_comparison_audit", fixture_root), comparison_semantic))
eval(parse(text = index_source), envir = env)
comparison_row <- env$output_index[
  env$output_index$stage == "14-rfid-domain-comparison-audit", ]
stopifnot(nrow(comparison_row) == 1L,
          identical(comparison_row$canonical_path,
                    "analysis_ready/analyses/rfid_domain_comparison_audit/"),
          identical(comparison_row$status, "migrated_source_retained"))
unlink(comparison_receipt)
unlink(comparison_semantic, recursive = TRUE)
seed_current <- mmm_behavior_output_group_root(
  "rfid_leading_bin_seed_audit", "current", fixture_root)
seed_semantic <- mmm_behavior_output_group_root(
  "rfid_leading_bin_seed_audit", "semantic", fixture_root)
seed_receipt <- file.path(fixture_root, "analysis_ready", "_migration_control",
                          "rfid_leading_bin_seed_audit.json")
dir.create(seed_current, recursive = TRUE, showWarnings = FALSE)
stopifnot(identical(mmm_behavior_output_active_root(
  "rfid_leading_bin_seed_audit", fixture_root), seed_current))
seed_record <- list(group = "rfid_leading_bin_seed_audit", state = "prepared",
                    files = 12L,
                    group_plan_sha256 = paste(rep("a", 64), collapse = ""),
                    contract_sha256 = paste(rep("b", 64), collapse = ""),
                    target_root_rel = "analyses/rfid_leading_bin_seed_audit",
                    source_retained = TRUE)
jsonlite::write_json(seed_record, seed_receipt, auto_unbox = TRUE)
stopifnot(identical(mmm_behavior_output_active_root(
  "rfid_leading_bin_seed_audit", fixture_root), seed_current))
dir.create(seed_semantic, recursive = TRUE, showWarnings = FALSE)
stopifnot(inherits(try(mmm_behavior_output_active_root(
  "rfid_leading_bin_seed_audit", fixture_root), silent = TRUE), "try-error"))
seed_record$state <- "activated"
jsonlite::write_json(seed_record, seed_receipt, auto_unbox = TRUE)
stopifnot(identical(mmm_behavior_output_active_root(
  "rfid_leading_bin_seed_audit", fixture_root), seed_semantic))
eval(parse(text = index_source), envir = env)
seed_row <- env$output_index[
  env$output_index$stage == "14-rfid-leading-bin-seed-audit", ]
stopifnot(nrow(seed_row) == 1L,
          identical(seed_row$canonical_path,
                    "analysis_ready/analyses/rfid_leading_bin_seed_audit/"),
          identical(seed_row$status, "migrated_source_retained"))
unlink(seed_receipt)
unlink(seed_semantic, recursive = TRUE)
rfid_audits <- list(
  rfid_construct_audit = c("audit_rfid_domain_construct_blind",
                           "14-rfid-construct-audit",
                           "Testing/audits/audit_rfid_domain_construct_blind.R"),
  rfid_conservatism_audit = c("audit_rfid_conservatism",
                              "14-rfid-conservatism-audit",
                              "Testing/audits/audit_rfid_conservatism.R"),
  rfid_alternative_inference_audit = c("audit_rfid_alternative_inference",
                                      "14-rfid-alternative-inference-audit",
                                      "Testing/audits/audit_rfid_alternative_inference.R"),
  rfid_reliability_audit = c("audit_rfid_reliability_improvement",
                             "14-rfid-reliability-audit",
                             "Testing/audits/audit_rfid_reliability_and_improvement.R")
)
for (group in names(rfid_audits)) {
  detail <- rfid_audits[[group]]
  current_root <- mmm_behavior_output_group_root(group, "current", fixture_root)
  semantic_root <- mmm_behavior_output_group_root(group, "semantic", fixture_root)
  receipt_path <- file.path(fixture_root, "analysis_ready", "_migration_control",
                            paste0(group, ".json"))
  stopifnot(identical(current_root,
                    file.path(fixture_root, "analysis_ready",
                              "12_systems_neuroscience_summary/5min_based",
                              detail[[1]])),
            identical(semantic_root,
                      file.path(fixture_root, "analysis_ready", "analyses", group)))
  dir.create(current_root, recursive = TRUE, showWarnings = FALSE)
  stopifnot(identical(mmm_behavior_output_active_root(group, fixture_root),
                      current_root))
  record <- list(group = group, state = "prepared", files = 1L,
                 group_plan_sha256 = paste(rep("a", 64), collapse = ""),
                 contract_sha256 = paste(rep("b", 64), collapse = ""),
                 target_root_rel = paste0("analyses/", group),
                 source_retained = TRUE)
  jsonlite::write_json(record, receipt_path, auto_unbox = TRUE)
  stopifnot(identical(mmm_behavior_output_active_root(group, fixture_root),
                      current_root))
  dir.create(semantic_root, recursive = TRUE, showWarnings = FALSE)
  stopifnot(inherits(try(mmm_behavior_output_active_root(group, fixture_root),
                       silent = TRUE), "try-error"))
  record$state <- "activated"
  jsonlite::write_json(record, receipt_path, auto_unbox = TRUE)
  stopifnot(identical(mmm_behavior_output_active_root(group, fixture_root),
                      semantic_root))
  eval(parse(text = index_source), envir = env)
  audit_row <- env$output_index[env$output_index$stage == detail[[2]], ]
  stopifnot(nrow(audit_row) == 1L,
            identical(audit_row$canonical_path,
                      paste0("analysis_ready/analyses/", group, "/")),
            identical(audit_row$status, "migrated_source_retained"))
  writer <- paste(readLines(detail[[3]], warn = FALSE), collapse = "\n")
  stopifnot(grepl(paste0('mmm_behavior_output_active_root("', group,
                         '", project_root = ROOT)'), writer, fixed = TRUE))
  unlink(receipt_path)
  unlink(semantic_root, recursive = TRUE)
}
for (group in c("dyadic_contacts", "social_networks_5min",
                "gamm_features_10min", "state_space_5min",
                "hmm_states_10min", "hmm_states_5min",
                "temporal_instability_10sec", "proteomics_mnn_primary",
                "proteomics_mnn_sensitivity",
                "adaptation_kinetics_10min", "sleep_like_inactivity_10min",
                "phase_organization_10min", "nonlinear_dynamics_5min",
                "systems_phenotyping_5min", "inactive_phase_qc_audit")) {
  current_group <- mmm_behavior_output_group_root(group, "current", fixture_root)
  semantic_group <- mmm_behavior_output_group_root(group, "semantic", fixture_root)
  receipt_group <- file.path(fixture_root, "analysis_ready", "_migration_control",
                             paste0(group, ".json"))
  dir.create(current_group, recursive = TRUE, showWarnings = FALSE)
  dir.create(semantic_group, recursive = TRUE, showWarnings = FALSE)
  record_group <- list(group = group, state = "activated", files = 1L,
                       group_plan_sha256 = paste(rep("a", 64), collapse = ""),
                       contract_sha256 = paste(rep("b", 64), collapse = ""),
                       target_root_rel = switch(group,
                         dyadic_contacts = "analyses/dyadic_contacts",
                         social_networks_5min = "analyses/dynamic_social_networks/5min",
                         gamm_features_10min = "analyses/gamm_trajectory_features/10min",
                         state_space_5min = "analyses/behavioral_state_space/5min",
                         hmm_states_10min = "analyses/hmm_states/10min",
                         hmm_states_5min = "analyses/hmm_states/5min",
                         temporal_instability_10sec = "analyses/temporal_instability/10sec",
                         proteomics_mnn_primary = "analyses/behavior_proteomics/proteomics_mnn_primary",
                         proteomics_mnn_sensitivity = "analyses/behavior_proteomics/proteomics_mnn_sensitivity",
                         adaptation_kinetics_10min = "analyses/adaptation_kinetics/10min",
                         sleep_like_inactivity_10min = "analyses/sleep_like_inactivity/10min",
                         phase_organization_10min = "analyses/phase_organization/10min",
                         nonlinear_dynamics_5min = "analyses/nonlinear_dynamics/5min",
                         systems_phenotyping_5min = "analyses/systems_phenotyping/5min",
                         inactive_phase_qc_audit = "analyses/inactive_phase_qc_audit"),
                       source_retained = TRUE)
  jsonlite::write_json(record_group, receipt_group, auto_unbox = TRUE)
  stopifnot(identical(mmm_behavior_output_active_root(group, fixture_root),
                      semantic_group))
}
stopifnot(identical(mmm_social_network_resolution_root("5min_based", fixture_root),
                    mmm_behavior_output_group_root("social_networks_5min", "semantic", fixture_root)),
          identical(mmm_social_network_resolution_root("10min_based", fixture_root),
                    file.path(fixture_root, "analysis_ready",
                              "06_behavioral_dynamics/social_networks/10min_based")),
          identical(mmm_social_network_resolution_root(
            c("5min_based", "10min_based"), fixture_root),
            c(mmm_behavior_output_group_root("social_networks_5min", "semantic",
                                             fixture_root),
              file.path(fixture_root, "analysis_ready",
                        "06_behavioral_dynamics/social_networks/10min_based"))),
          identical(mmm_gamm_features_resolution_root(
            c("10min_based", "30min_based"), fixture_root),
            c(file.path(fixture_root, "analysis_ready",
                        "analyses/gamm_trajectory_features/10min"),
              file.path(fixture_root, "analysis_ready",
                        "06_behavioral_dynamics/gamm_features/30min_based"))))
stopifnot(identical(mmm_hmm_resolution_root(c("10min_based", "5min_based"),
                                            fixture_root),
                    c(file.path(fixture_root, "analysis_ready",
                                "analyses/hmm_states/10min"),
                      file.path(fixture_root, "analysis_ready",
                                "analyses/hmm_states/5min"))))
stopifnot(identical(mmm_temporal_instability_resolution_root(
  c("10sec_based", "5min_based"), fixture_root),
  c(file.path(fixture_root, "analysis_ready", "analyses/temporal_instability/10sec"),
    file.path(fixture_root, "analysis_ready",
              "06_behavioral_dynamics/temporal_instability/5min_based"))))
stopifnot(identical(mmm_phase_analysis_resolution_root(
  "adaptation_kinetics", c("10min_based", "5min_based"), fixture_root),
  c(file.path(fixture_root, "analysis_ready", "analyses/adaptation_kinetics/10min"),
    file.path(fixture_root, "analysis_ready",
              "15_behavioral_adaptation_kinetics/5min_based"))),
  inherits(try(mmm_phase_analysis_resolution_root("adaptation_kinetics",
                                                  "30min_based", fixture_root),
               silent = TRUE), "try-error"))
stopifnot(identical(mmm_supporting_resolution_root(
  "nonlinear_dynamics", c("5min_based", "10min_based"), fixture_root),
  c(file.path(fixture_root, "analysis_ready", "analyses/nonlinear_dynamics/5min"),
    file.path(fixture_root, "analysis_ready",
              "13_nonlinear_systems_dynamics/10min_based"))),
  inherits(try(mmm_supporting_resolution_root("systems_phenotyping",
                                                 "invalid", fixture_root),
               silent = TRUE), "try-error"))
mmm_behavior_output_assert_uniform_layout(
  c("hmm_states_10min", "hmm_states_5min"), fixture_root, "Stage 08")
mmm_behavior_output_assert_uniform_layout(
  c("proteomics_mnn_primary", "proteomics_mnn_sensitivity"),
  fixture_root, "Stage 15")
stopifnot(identical(mmm_behavior_proteomics_base_dir(fixture_root),
                    file.path(fixture_root, "analysis_ready",
                              "analyses/behavior_proteomics")))
hmm_sensitivity_receipt <- file.path(fixture_root, "analysis_ready",
                                     "_migration_control", "hmm_states_5min.json")
hmm_sensitivity_record <- jsonlite::fromJSON(hmm_sensitivity_receipt)
hmm_sensitivity_record$state <- "prepared"
jsonlite::write_json(hmm_sensitivity_record, hmm_sensitivity_receipt,
                     auto_unbox = TRUE)
stopifnot(inherits(try(mmm_behavior_output_assert_uniform_layout(
  c("hmm_states_10min", "hmm_states_5min"), fixture_root, "Stage 08"),
  silent = TRUE), "try-error"))
hmm_sensitivity_record$state <- "activated"
jsonlite::write_json(hmm_sensitivity_record, hmm_sensitivity_receipt,
                     auto_unbox = TRUE)
eval(parse(text = index_source), envir = env)
stopifnot(identical(env$output_index$canonical_path[
                    env$output_index$stage == "06"],
                    "analysis_ready/analyses/dynamic_social_networks/5min/"),
          identical(env$output_index$canonical_path[
                    env$output_index$stage == "07"],
                    "analysis_ready/analyses/gamm_trajectory_features/10min/"),
          identical(env$output_index$canonical_path[
                    env$output_index$stage == "05"],
                    "analysis_ready/analyses/behavioral_state_space/5min/"),
          identical(mmm_gamm_features_resolution_root("30min_based", fixture_root),
                    file.path(fixture_root, "analysis_ready",
                              "06_behavioral_dynamics/gamm_features/30min_based")),
          identical(mmm_state_space_resolution_root(
            c("5min_based", "10min_based"), fixture_root),
            c(file.path(fixture_root, "analysis_ready",
                        "analyses/behavioral_state_space/5min"),
              file.path(fixture_root, "analysis_ready",
                        "06_behavioral_dynamics/state_space/10min_based"))))
stopifnot(identical(env$output_index$canonical_path[
                    env$output_index$stage == "08"],
                    "analysis_ready/analyses/hmm_states/10min/"),
          identical(env$output_index$canonical_path[
                    env$output_index$stage == "08-5min"],
                    "analysis_ready/analyses/hmm_states/5min/"),
          identical(env$output_index$canonical_path[
                    env$output_index$stage == "04"],
                    "analysis_ready/analyses/temporal_instability/10sec/"),
          identical(env$output_index$canonical_path[
                    env$output_index$stage == "15-primary"],
                    "analysis_ready/analyses/behavior_proteomics/proteomics_mnn_primary/"),
          identical(env$output_index$canonical_path[
                    env$output_index$stage == "15-sensitivity"],
                    "analysis_ready/analyses/behavior_proteomics/proteomics_mnn_sensitivity/"),
          identical(env$output_index$canonical_path[env$output_index$stage == "11"],
                    "analysis_ready/analyses/adaptation_kinetics/10min/"),
          identical(env$output_index$canonical_path[env$output_index$stage == "12"],
                    "analysis_ready/analyses/sleep_like_inactivity/10min/"),
          identical(env$output_index$canonical_path[env$output_index$stage == "13"],
                    "analysis_ready/analyses/phase_organization/10min/"),
          identical(env$output_index$canonical_path[
                    env$output_index$stage == "support-13"],
                    "analysis_ready/analyses/nonlinear_dynamics/5min/"),
          identical(env$output_index$canonical_path[
                    env$output_index$stage == "support-14"],
                    "analysis_ready/analyses/systems_phenotyping/5min/"),
          identical(env$output_index$canonical_path[
                    env$output_index$stage == "14-inactive-qc-audit"],
                    "analysis_ready/analyses/inactive_phase_qc_audit/"))
plan <- read.csv("docs/BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv", stringsAsFactors = FALSE)
stopifnot(nrow(plan) == 938L, !anyDuplicated(plan$source_rel),
          !anyDuplicated(paste(plan$target_root_rel, plan$target_file)))
stopifnot(identical(as.integer(table(factor(plan$group, levels = c(
  "first_night_10min", "first_night_5min", "spatial_tables", "spatial_audit",
  "spatial_models", "spatial_figures", "dyadic_contacts",
  "social_networks_5min", "gamm_features_10min", "state_space_5min",
  "hmm_states_10min", "hmm_states_5min",
  "temporal_instability_10sec", "proteomics_mnn_primary",
  "proteomics_mnn_sensitivity", "adaptation_kinetics_10min",
  "sleep_like_inactivity_10min", "phase_organization_10min",
  "nonlinear_dynamics_5min", "systems_phenotyping_5min",
  "inactive_phase_qc_audit")))),
  c(10L, 10L, 6L, 8L, 6L, 5L, 16L, 94L, 18L, 97L, 36L, 36L, 212L, 43L, 43L,
    19L, 19L, 22L, 97L, 138L, 3L)))
release_lines <- grep('a\\("fn_', readLines("Analysis/build_publication_release.R",
                                        warn = FALSE), value = TRUE)
release_files <- sub('.*file.path\\(FIRSTNIGHT, "([^"]+)"\\).*', '\\1', release_lines)
stopifnot(length(release_files) == 8L,
          all(release_files %in% plan$target_file[plan$group == "first_night_10min"]))
for (group in unique(plan$group)) {
  rows <- plan[plan$group == group, , drop = FALSE]
  stopifnot(length(unique(rows$target_root_rel)) == 1L,
            length(unique(rows$gate)) == 1L,
            unique(rows$gate) %in% c("blocked_code_contract", "ready"),
            identical(gsub("\\\\", "/", mmm_behavior_output_group_root(
              group, layout = "semantic", project_root = fixture_root)),
              gsub("\\\\", "/", file.path(fixture_root, "analysis_ready",
                                              rows$target_root_rel[1]))))
}
cat("PASS: explicit semantic output-group paths\n")

stage14 <- paste(readLines("Analysis/14_systems_neuroscience_summary_dashboard.R", warn = FALSE),
                 collapse = "\n")
fig1_candidates <- paste(readLines(
  "manuscript/Fig1_behavior_candidates/build_fig1_candidates.R", warn = FALSE),
  collapse = "\n")
stage27 <- paste(readLines("Analysis/27_build_behavior_main_figure.R", warn = FALSE),
                 collapse = "\n")
stopifnot(grepl('mmm_behavior_output_active_root("systems_dashboard_5min", project_root)',
               stage14, fixed = TRUE),
          grepl('mmm_behavior_output_active_root("systems_dashboard_5min", project_root)',
                fig1_candidates, fixed = TRUE),
          grepl(paste0('pre_fix_effect_path <- file.path(\n',
                       '  mmm_behavior_retained_source_root("systems_dashboard_5min", project_root),'),
                stage14, fixed = TRUE),
          !grepl('mmm_behavior_output_group_root("systems_dashboard_5min", "current"',
                 stage14, fixed = TRUE),
          !grepl('file.copy(domain_effect_path, pre_fix_effect_path',
                 stage14, fixed = TRUE),
          grepl('broad_source_rel <- mmm_source_relative_path(broad_source_path, project_root)',
                stage27, fixed = TRUE),
          !grepl('"12_systems_neuroscience_summary/5min_based/stats_tables/systems_sis_domain_effect_summary.csv"',
                 stage27, fixed = TRUE))
stage02 <- paste(readLines("Analysis/02_build_dyadic_rfid_contacts.R", warn = FALSE),
                 collapse = "\n")
stage06 <- paste(readLines("Analysis/06_dynamic_social_networks.R", warn = FALSE),
                 collapse = "\n")
stage05 <- paste(readLines("Analysis/05_behavioral_state_space.R", warn = FALSE),
                 collapse = "\n")
stage07 <- paste(readLines("Analysis/07_gamm_trajectory_features.R", warn = FALSE),
                 collapse = "\n")
stage11 <- paste(readLines("Analysis/11_behavioral_adaptation_kinetics.R", warn = FALSE),
                 collapse = "\n")
stage12 <- paste(readLines("Analysis/12_sleep_like_quiescence_metrics.R", warn = FALSE),
                 collapse = "\n")
stage13 <- paste(readLines("Analysis/13_ethological_phase_organization.R", warn = FALSE),
                 collapse = "\n")
support13 <- paste(readLines("Analysis/_supporting/13_nonlinear_systems_dynamics.R",
                             warn = FALSE), collapse = "\n")
support14 <- paste(readLines("Analysis/_supporting/14_nextgen_behavioral_phenotyping.R",
                             warn = FALSE), collapse = "\n")
stage10 <- paste(readLines("Analysis/10_systems_feature_prediction_ladder.R",
                           warn = FALSE), collapse = "\n")
stage15 <- paste(readLines("Analysis/15_behavior_proteomics_integration.R", warn = FALSE),
                 collapse = "\n")
stage19 <- paste(readLines("Analysis/19_spatial_occupancy_maps.R", warn = FALSE),
                 collapse = "\n")
release <- paste(readLines("Analysis/build_publication_release.R", warn = FALSE),
                 collapse = "\n")
inactive_qc_audit <- paste(readLines("Testing/audits/audit_inactive_phase_qc_redesign.R",
                                warn = FALSE), collapse = "\n")
stopifnot(grepl('mmm_behavior_output_active_root("inactive_phase_qc_audit", PROJ)',
               inactive_qc_audit, fixed = TRUE),
          grepl('mmm_behavior_output_active_root("systems_dashboard_5min", PROJ)',
                inactive_qc_audit, fixed = TRUE),
          !grepl('OUT <- file.path(ST14, "audit_inactive_phase_qc")',
                 inactive_qc_audit, fixed = TRUE))
registry <- utils::read.csv("docs/MANUSCRIPT_ANALYSIS_REGISTRY.csv",
                            check.names = FALSE, na.strings = "NA")
first_night <- registry[registry$analysis_id == "FIRSTNIGHT_5DOMAIN_PANEL", , drop = FALSE]
stopifnot(nrow(first_night) == 1L,
          identical(first_night$source_artifact,
                    "analysis_ready/analyses/first_night_five_domain_characterization/10min/first_night_group_contrasts.csv"))
stage15_registry <- registry[registry$analysis_id == "S15_BEHAVIOR_PROTEOMICS", , drop = FALSE]
stopifnot(nrow(stage15_registry) == 1L,
          grepl("HISTORICAL EXPLORATORY", stage15_registry$current_status, fixed = TRUE),
          grepl("reviewed rerun required", stage15_registry$current_status, fixed = TRUE),
          identical(stage15_registry$publication_ready, "no"))
stopifnot(grepl('mmm_behavior_output_active_root(group, project_root = project_root)', stage14,
               fixed = TRUE),
          grepl('mmm_behavior_output_active_root("spatial_models", project_root = RFID_ROOT)',
                stage19, fixed = TRUE),
          grepl('mmm_behavior_output_active_root("first_night_10min"',
                release, fixed = TRUE))
stopifnot(grepl('mmm_behavior_output_active_root("dyadic_contacts")', stage02,
               fixed = TRUE),
          grepl('getOption("mmm.dyadic_contacts_dir", NULL)', stage02, fixed = TRUE),
          grepl('mmm_social_network_resolution_root(bin_level)', stage06,
                fixed = TRUE),
          grepl('c("dyadic_contacts", "social_networks_5min")', stage06,
                fixed = TRUE),
          grepl('mmm_behavior_output_active_root("dyadic_contacts")', stage06,
                fixed = TRUE),
          grepl('mmm_gamm_features_resolution_root(bin_level)', stage07,
                fixed = TRUE),
          grepl('mmm_state_space_resolution_root(bin_level)', stage05,
                fixed = TRUE),
          grepl('mmm_phase_analysis_resolution_root("adaptation_kinetics", bin_level, base_dir)',
                stage11, fixed = TRUE),
          grepl('mmm_phase_analysis_resolution_root("sleep_like_inactivity", bin_level, base_dir)',
                stage12, fixed = TRUE),
          grepl('mmm_phase_analysis_resolution_root("phase_organization", bin_level, base_dir)',
                stage13, fixed = TRUE),
          grepl('mmm_phase_analysis_resolution_root("adaptation_kinetics", domain_bin_preference("adaptive_recovery"), project_root)',
                stage14, fixed = TRUE),
          grepl('mmm_supporting_resolution_root("nonlinear_dynamics", bin_level, project_root)',
                support13, fixed = TRUE),
          grepl('mmm_supporting_resolution_root("systems_phenotyping", bin_level, project_root)',
                support14, fixed = TRUE),
          grepl('mmm_supporting_resolution_root("nonlinear_dynamics", "5min_based", base_dir)',
                stage10, fixed = TRUE),
          grepl('mmm_supporting_resolution_root("systems_phenotyping", "5min_based", base_dir)',
                stage10, fixed = TRUE),
          !grepl('file.path(base_dir, "analysis_ready/13_nonlinear_systems_dynamics")',
                 stage10, fixed = TRUE),
          grepl('mmm_behavior_proteomics_base_dir(project_root)',
                stage15, fixed = TRUE),
          !grepl('analysis_ready/06_behavioral_dynamics/social_networks', stage14,
                 fixed = TRUE),
          grepl('mmm_social_network_resolution_root(behavior_bin_level, project_root)',
                stage15, fixed = TRUE))
comparison_writer <- paste(readLines(
  "Testing/audits/audit_rfid_legacy_vs_new_domains.R", warn = FALSE),
  collapse = "\n")
stopifnot(grepl('mmm_behavior_output_active_root("first_night_10min", project_root = ROOT)',
               comparison_writer, fixed = TRUE),
          grepl('mmm_behavior_audit_replay_output_root("rfid_legacy_vs_new_domains", ROOT)',
                comparison_writer, fixed = TRUE),
          !grepl('mmm_behavior_output_active_root("rfid_domain_comparison_audit", project_root = ROOT)',
                 comparison_writer, fixed = TRUE),
          !grepl('"5min_based", "audit_rfid_legacy_vs_new"',
                 comparison_writer, fixed = TRUE))
seed_writer <- paste(readLines(
  "Testing/audits/audit_rfid_leading_bin_seed_sensitivity.R", warn = FALSE),
  collapse = "\n")
seed_reader <- paste(readLines("Testing/tests/test_rfid_domain_contract.R",
                               warn = FALSE), collapse = "\n")
stopifnot(grepl('mmm_behavior_output_active_root("rfid_leading_bin_seed_audit", project_root = ROOT)',
               seed_writer, fixed = TRUE),
          grepl('mmm_behavior_output_active_root("rfid_leading_bin_seed_audit", project_root = ROOT)',
                seed_reader, fixed = TRUE))
cat("PASS: key producer and release paths follow the explicit migration receipt\n")
