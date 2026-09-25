# Historical replay path contract. No audit calculation or live S: write.
source("Functions/project_paths.R")
root <- file.path(tempdir(), paste0("historical_audit_replay_",
                                    as.integer(runif(1L, 1L, 1e9))))
ready <- file.path(root, "analysis_ready")
old03 <- file.path(ready, "03_derived_metrics")
old06 <- file.path(ready, "06_behavioral_dynamics")
old12 <- file.path(ready, "12_systems_neuroscience_summary")
for (path in c(old03, file.path(old06, "dyadic_contacts"),
               file.path(old12, "5min_based"))) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
}
same_path <- function(a, b) identical(normalizePath(a, winslash = "/", mustWork = FALSE),
                                      normalizePath(b, winslash = "/", mustWork = FALSE))
stopifnot(same_path(mmm_behavior_numbered_source_root("03_derived_metrics", root), old03),
          same_path(mmm_behavior_numbered_source_root("06_behavioral_dynamics", root), old06),
          same_path(mmm_behavior_numbered_source_root("12_systems_neuroscience_summary", root), old12))

out <- mmm_behavior_audit_replay_output_root(
  "first_night_candidate_set_scores", root, "review_20260924")
stopifnot(identical(out, file.path(ready, "analyses", "historical_audit_replays",
                                   "review_20260924", "first_night_candidate_set_scores")),
          inherits(try(mmm_behavior_audit_replay_output_root(
            "first_night_candidate_set_scores", root, "../escape"), silent = TRUE),
            "try-error"),
          inherits(try(mmm_behavior_audit_replay_output_root(
            "first_night_candidate_set_scores", root, ""), silent = TRUE),
            "try-error"),
          # Shared writers mirror /figures/ paths and canonicalize /pipeline/.
          inherits(try(mmm_behavior_audit_replay_output_root(
            "first_night_candidate_set_scores", root, "figures"), silent = TRUE),
            "try-error"),
          inherits(try(mmm_behavior_audit_replay_output_root(
            "first_night_candidate_set_scores", root, "pipeline"), silent = TRUE),
            "try-error"),
          # Long run ids would push the longest replay outputs past 240 characters.
          inherits(try(mmm_behavior_audit_replay_output_root(
            "first_night_candidate_set_scores", root, strrep("r", 21L)), silent = TRUE),
            "try-error"),
          !inherits(try(mmm_behavior_audit_replay_path(
            "first_night_candidate_set_scores", root, strrep("r", 20L)), silent = TRUE),
            "try-error"))
dir.create(out, recursive = TRUE, showWarnings = FALSE)
stopifnot(inherits(try(mmm_behavior_audit_replay_output_root(
  "first_night_candidate_set_scores", root, "review_20260924"), silent = TRUE),
  "try-error"),
  identical(mmm_behavior_audit_replay_input_root(
    "first_night_candidate_set_scores", root, "review_20260924"), out),
  inherits(try(mmm_behavior_audit_replay_input_root(
    "first_night_candidate_set_effects", root, "review_20260924"),
    silent = TRUE), "try-error"))

archived06 <- file.path(ready, "history", "original_layout", "06_behavioral_dynamics")
dir.create(dirname(archived06), recursive = TRUE, showWarnings = FALSE)
stopifnot(file.rename(old06, archived06))
receipt_dir <- file.path(ready, "_migration_control", "numbered_root_archive")
dir.create(receipt_dir, recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(list(
  root = "06_behavioral_dynamics",
  source_root_rel = "06_behavioral_dynamics",
  archive_root_rel = "history/original_layout/06_behavioral_dynamics",
  state = "activated", files = 1L, bytes = 1L,
  manifest_sha256 = paste(rep("c", 64L), collapse = ""),
  reader_gate_kind = "ArchivePath", reader_gate_sha256 = strrep("e", 64L),
  reader_queue_sha256 = strrep("f", 64L)),
  file.path(receipt_dir, "06_behavioral_dynamics.json"), auto_unbox = TRUE)
stopifnot(same_path(mmm_behavior_numbered_source_root("06_behavioral_dynamics", root),
                    archived06))

script <- readLines("Testing/audits/audit_first_night_candidate_set_scores.R",
                    warn = FALSE)
invisible(parse(text = script))
stopifnot(any(grepl('anchor_long_path <- file.path(HIST_AUDIT,', script,
                   fixed = TRUE)),
          any(grepl('mmm_behavior_audit_replay_output_root(', script,
                   fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary', script,
                     fixed = TRUE)))
effects <- readLines("Testing/audits/audit_first_night_candidate_set_effects.R",
                     warn = FALSE)
invisible(parse(text = effects))
stopifnot(any(grepl('mmm_behavior_audit_replay_input_root(', effects,
                   fixed = TRUE)),
          any(grepl('SCORES_CSV   <- file.path(INPUT,', effects,
                   fixed = TRUE)),
          any(grepl('mmm_behavior_audit_replay_output_root(', effects,
                   fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary', effects,
                     fixed = TRUE)))
decision <- readLines("Testing/audits/audit_first_night_candidate_set_decision.R",
                      warn = FALSE)
invisible(parse(text = decision))
stopifnot(sum(grepl('mmm_behavior_audit_replay_input_root(', decision,
                   fixed = TRUE)) == 2L,
          any(grepl('f_scores   <- file.path(SCORES_INPUT,', decision,
                   fixed = TRUE)),
          any(grepl('f_effects  <- file.path(EFFECTS_INPUT,', decision,
                   fixed = TRUE)),
          any(grepl('mmm_behavior_audit_replay_output_root(', decision,
                   fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary', decision,
                     fixed = TRUE)))
algebra <- readLines(
  "Testing/audits/audit_first_night_candidate_set_algebra_crosscheck.R",
  warn = FALSE)
invisible(parse(text = algebra))
stopifnot(any(grepl('mmm_behavior_numbered_source_root("03_derived_metrics", PROJ)',
                   algebra, fixed = TRUE)),
          !any(grepl('analysis_ready/03_derived_metrics', algebra,
                     fixed = TRUE)))
window_provenance <- readLines(
  "Testing/audits/audit_first_night_window_provenance.R", warn = FALSE)
invisible(parse(text = window_provenance))
stopifnot(sum(grepl('mmm_behavior_numbered_source_root(', window_provenance,
                   fixed = TRUE)) == 2L,
          !any(grepl('analysis_ready/06_behavioral_dynamics', window_provenance,
                     fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary',
                     window_provenance, fixed = TRUE)))
production_parity <- readLines(
  "Testing/audits/audit_first_night_production_parity.R", warn = FALSE)
invisible(parse(text = production_parity))
stopifnot(any(grepl('mmm_behavior_audit_replay_output_root(', production_parity,
                   fixed = TRUE)),
          sum(grepl('mmm_behavior_numbered_source_root(', production_parity,
                    fixed = TRUE)) == 2L,
          !any(grepl('scratchpad/orch/first_night_prod', production_parity,
                     fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary',
                     production_parity, fixed = TRUE)))

time_anchor <- readLines("Testing/audits/audit_first_night_time_anchor.R",
                         warn = FALSE)
invisible(parse(text = time_anchor))
stopifnot(any(grepl('mmm_behavior_audit_replay_output_root("first_night_time_anchor", PROJ)',
                   time_anchor, fixed = TRUE)),
          any(grepl('mmm_behavior_numbered_source_root("03_derived_metrics", PROJ)',
                    time_anchor, fixed = TRUE)),
          !any(grepl('analysis_ready/03_derived_metrics', time_anchor,
                     fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary', time_anchor,
                     fixed = TRUE)))

domain_v2 <- readLines("Testing/audits/audit_first_night_domain_scores_v2.R",
                       warn = FALSE)
invisible(parse(text = domain_v2))
stopifnot(any(grepl('mmm_behavior_audit_replay_input_root("first_night_time_anchor", PROJ)',
                   domain_v2, fixed = TRUE)),
          any(grepl('anchor_long_path <- file.path(ANCHOR,', domain_v2,
                    fixed = TRUE)),
          any(grepl('mmm_behavior_audit_replay_output_root("first_night_domain_scores_v2", PROJ)',
                    domain_v2, fixed = TRUE)),
          sum(grepl('mmm_behavior_numbered_source_root(', domain_v2,
                    fixed = TRUE)) == 3L,
          !any(grepl('analysis_ready/06_behavioral_dynamics', domain_v2,
                     fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary', domain_v2,
                     fixed = TRUE)))

dwell <- readLines("Testing/audits/audit_first_night_dwell_partition_stability.R",
                   warn = FALSE)
invisible(parse(text = dwell))
stopifnot(any(grepl('mmm_behavior_audit_replay_output_root("first_night_dwell_partition_stability", PROJ)',
                   dwell, fixed = TRUE)),
          any(grepl('mmm_behavior_numbered_source_root("03_derived_metrics", PROJ)',
                    dwell, fixed = TRUE)),
          !any(grepl('analysis_ready/03_derived_metrics', dwell,
                     fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary', dwell,
                     fixed = TRUE)))

dwell_compare <- readLines("Testing/audits/audit_first_night_dwell_shipped_vs_refit.R",
                           warn = FALSE)
invisible(parse(text = dwell_compare))
stopifnot(any(grepl('mmm_behavior_audit_replay_input_root("first_night_dwell_partition_stability", PROJ)',
                   dwell_compare, fixed = TRUE)),
          any(grepl('P <- read_csv(file.path(INPUT,', dwell_compare,
                    fixed = TRUE)),
          any(grepl('mmm_behavior_audit_replay_output_root("first_night_dwell_shipped_vs_refit", PROJ)',
                    dwell_compare, fixed = TRUE)),
          sum(grepl('mmm_behavior_numbered_source_root(', dwell_compare,
                    fixed = TRUE)) == 2L,
          !any(grepl('analysis_ready/06_behavioral_dynamics', dwell_compare,
                     fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary', dwell_compare,
                     fixed = TRUE)))

for (name in c("audit_first_night_heatmap_v2.R",
               "audit_first_night_window_sensitivity.R")) {
  downstream <- readLines(file.path("Testing/audits", name), warn = FALSE)
  invisible(parse(text = downstream))
  stopifnot(any(grepl('mmm_behavior_audit_replay_input_root("first_night_domain_scores_v2", PROJ)',
                     downstream, fixed = TRUE)),
            any(grepl('mmm_behavior_audit_replay_output_root(', downstream,
                      fixed = TRUE)),
            any(grepl('read_csv(file.path(INPUT, "first_night_domain_scores.csv")',
                      downstream, fixed = TRUE)),
            !any(grepl('analysis_ready/12_systems_neuroscience_summary',
                       downstream, fixed = TRUE)))
}

for (entry in list(
  c("audit_first_night_domain_scores.R", "first_night_domain_scores_v1"),
  c("audit_first_night_hmm_components.R", "first_night_hmm_components"))) {
  standalone <- readLines(file.path("Testing/audits", entry[[1L]]),
                          warn = FALSE)
  invisible(parse(text = standalone))
  stopifnot(any(grepl(paste0('mmm_behavior_audit_replay_output_root("',
                            entry[[2L]], '", PROJ)'), standalone,
                     fixed = TRUE)),
            sum(grepl('mmm_behavior_numbered_source_root(', standalone,
                      fixed = TRUE)) == 3L,
            !any(grepl('analysis_ready/06_behavioral_dynamics', standalone,
                       fixed = TRUE)),
            !any(grepl('analysis_ready/12_systems_neuroscience_summary',
                       standalone, fixed = TRUE)))
}

stage09 <- readLines("Testing/audits/audit_stage09_stale_artifacts.R",
                     warn = FALSE)
invisible(parse(text = stage09))
stopifnot(any(grepl('mmm_behavior_audit_replay_output_root("stage09_stale_artifacts", RFID)',
                   stage09, fixed = TRUE)),
          sum(grepl('mmm_behavior_numbered_source_root(', stage09,
                    fixed = TRUE)) == 2L,
          !any(grepl('file.path(AR,"03_derived_metrics/', stage09,
                     fixed = TRUE)),
          !any(grepl('file.path(AR,"06_behavioral_dynamics/', stage09,
                     fixed = TRUE)))

stage10 <- readLines("Testing/audits/audit_stage10_semantic_discovery_parity.R",
                     warn = FALSE)
invisible(parse(text = stage10))
stopifnot(any(grepl('numbered_root <- mmm_behavior_numbered_source_root(',
                   stage10, fixed = TRUE)),
          any(grepl('recorded_old_map <- normalizePath(file.path(ready, old_map_rel)',
                    stage10, fixed = TRUE)),
          !any(grepl('numbered_root <- file.path(ready, "06_behavioral_dynamics")',
                     stage10, fixed = TRUE)))

phase_bug <- readLines("Testing/audits/audit_phase_bug_impact.R", warn = FALSE)
invisible(parse(text = phase_bug))
stopifnot(any(grepl('mmm_behavior_audit_replay_output_root("phase_bug_impact", PROJ)',
                   phase_bug, fixed = TRUE)),
          any(grepl('mmm_behavior_numbered_source_root("03_derived_metrics", PROJ)',
                    phase_bug, fixed = TRUE)),
          !any(grepl('analysis_ready/03_derived_metrics', phase_bug,
                     fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary', phase_bug,
                     fixed = TRUE)))

temporal <- readLines(
  "Testing/audits/audit_hmm_state_architecture_temporal_components.R",
  warn = FALSE)
invisible(parse(text = temporal))
stopifnot(any(grepl('mmm_behavior_audit_replay_output_root("hmm_architecture_temporal_components", PROJ)',
                   temporal, fixed = TRUE)),
          sum(grepl('mmm_behavior_numbered_source_root(', temporal,
                    fixed = TRUE)) == 2L,
          !any(grepl('analysis_ready/06_behavioral_dynamics', temporal,
                     fixed = TRUE)))

for (entry in list(
  c("audit_hmm_state_architecture_gap_aware.R", "hmm_architecture_gap_aware"),
  c("audit_hmm_state_architecture_qc_sensitivity.R", "hmm_architecture_qc_sensitivity"),
  c("audit_hmm_state_architecture_locomotion_dominance.R", "hmm_architecture_locomotion_dominance"))) {
  downstream <- readLines(file.path("Testing/audits", entry[[1L]]),
                          warn = FALSE)
  invisible(parse(text = downstream))
  stopifnot(any(grepl('mmm_behavior_audit_replay_input_root("hmm_architecture_temporal_components", PROJ)',
                     downstream, fixed = TRUE)),
            any(grepl(paste0('mmm_behavior_audit_replay_output_root("',
                              entry[[2L]], '", PROJ)'), downstream,
                      fixed = TRUE)),
            any(grepl('read_csv(file.path(INPUT, "hmm_architecture_temporal_epoch_metrics.csv")',
                      downstream, fixed = TRUE)),
            !any(grepl('analysis_ready/12_systems_neuroscience_summary',
                       downstream, fixed = TRUE)))
}

for (entry in list(
  c("audit_hmm_state_architecture_identifiability.R", "hmm_architecture_identifiability"),
  c("audit_hmm_state_architecture_partition_robustness.R", "hmm_architecture_partition_robustness"),
  c("audit_hmm_state_architecture_semantic_erasure.R", "hmm_architecture_semantic_erasure"))) {
  probe <- readLines(file.path("Testing/audits", entry[[1L]]), warn = FALSE)
  invisible(parse(text = probe))
  stopifnot(any(grepl(paste0('mmm_behavior_audit_replay_output_root("',
                            entry[[2L]], '", PROJ)'), probe, fixed = TRUE)),
            any(grepl('mmm_behavior_numbered_source_root("03_derived_metrics", PROJ)',
                      probe, fixed = TRUE)),
            !any(grepl('analysis_ready/03_derived_metrics', probe,
                       fixed = TRUE)),
            !any(grepl('analysis_ready/12_systems_neuroscience_summary', probe,
                       fixed = TRUE)))
}
components <- readLines("Testing/audits/audit_hmm_state_architecture_components.R",
                        warn = FALSE)
invisible(parse(text = components))
stopifnot(any(grepl('mmm_behavior_audit_replay_output_root(', components,
                   fixed = TRUE)),
          sum(grepl('mmm_behavior_numbered_source_root(', components,
                    fixed = TRUE)) == 3L,
          any(grepl('path <- file.path(hmm_root, resolution, "tables", filename)',
                    components, fixed = TRUE)),
          !any(grepl('resolve_configured_hmm_artifact(', components,
                     fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary',
                     components, fixed = TRUE)))

for (entry in list(
  c("audit_hmm_state_architecture_provenance.R", "hmm_architecture_provenance"),
  c("audit_hmm_state_architecture_provenance_addendum.R", "hmm_architecture_provenance_addendum"))) {
  provenance <- readLines(file.path("Testing/audits", entry[[1L]]),
                          warn = FALSE)
  invisible(parse(text = provenance))
  stopifnot(any(grepl('mmm_behavior_audit_replay_output_root(', provenance,
                     fixed = TRUE)),
            any(grepl(paste0('"', entry[[2L]], '", project)'), provenance,
                      fixed = TRUE)),
            sum(grepl('mmm_behavior_numbered_source_root(', provenance,
                      fixed = TRUE)) == 2L,
            !any(grepl('analysis_ready/06_behavioral_dynamics', provenance,
                       fixed = TRUE)),
            !any(grepl('analysis_ready/12_systems_neuroscience_summary',
                       provenance, fixed = TRUE)))
}

for (entry in list(
  c("audit_hmm_state_architecture_phaseA_formula_units_qc.R", "hmm_architecture_phasea_formula_units_qc"),
  c("audit_hmm_state_architecture_phaseA_gapaware_contrasts.R", "hmm_architecture_phasea_gapaware_contrasts"),
  c("audit_hmm_state_architecture_phaseA_partition_and_gap.R", "hmm_architecture_phasea_partition_and_gap"))) {
  phase_a <- readLines(file.path("Testing/audits", entry[[1L]]),
                       warn = FALSE)
  invisible(parse(text = phase_a))
  stopifnot(any(grepl('mmm_behavior_audit_replay_input_root("hmm_architecture_components", PROJ)',
                     phase_a, fixed = TRUE)),
            any(grepl(paste0('mmm_behavior_audit_replay_output_root("',
                              entry[[2L]], '", PROJ)'), phase_a,
                      fixed = TRUE)),
            any(grepl('read_csv(file.path(INPUT,', phase_a,
                      fixed = TRUE)),
            !any(grepl('analysis_ready/12_systems_neuroscience_summary',
                       phase_a, fixed = TRUE)))
}

redundancy <- readLines("Testing/audits/audit_hmm_state_architecture_redundancy.R",
                        warn = FALSE)
invisible(parse(text = redundancy))
stopifnot(any(grepl('mmm_behavior_audit_replay_input_root(', redundancy,
                   fixed = TRUE)),
          any(grepl('"hmm_architecture_components", project_root)', redundancy,
                    fixed = TRUE)),
          any(grepl('mmm_behavior_audit_replay_output_root(', redundancy,
                    fixed = TRUE)),
          any(grepl('foundation_file <- file.path(INPUT,', redundancy,
                    fixed = TRUE)),
          any(grepl('l2_file <- file.path(INPUT,', redundancy,
                    fixed = TRUE)))

profile <- readLines("Testing/audits/audit_hmm_state_architecture_profile.R",
                     warn = FALSE)
invisible(parse(text = profile))
stopifnot(any(grepl('mmm_behavior_audit_replay_output_root(', profile,
                   fixed = TRUE)),
          sum(grepl('mmm_behavior_numbered_source_root(', profile,
                    fixed = TRUE)) == 2L,
          !any(grepl('resolve_configured_hmm_artifact(', profile,
                     fixed = TRUE)),
          !any(grepl('analysis_ready/12_systems_neuroscience_summary',
                     profile, fixed = TRUE)))

for (entry in list(
  c("audit_hmm_state_architecture_component_models.R", "hmm_architecture_component_models"),
  c("audit_hmm_state_architecture_longitudinal.R", "hmm_architecture_longitudinal"),
  c("audit_hmm_state_architecture_construct_comparison.R", "hmm_architecture_construct_comparison"))) {
  model <- readLines(file.path("Testing/audits", entry[[1L]]), warn = FALSE)
  invisible(parse(text = model))
  stopifnot(any(grepl('"hmm_architecture_components", project_root)', model,
                     fixed = TRUE)),
            any(grepl('mmm_behavior_audit_replay_output_root(', model,
                      fixed = TRUE)),
            any(grepl(paste0('"', entry[[2L]], '", project_root)'), model,
                      fixed = TRUE)),
            !any(grepl('analysis_ready/12_systems_neuroscience_summary',
                       model, fixed = TRUE)))
}
construct <- readLines(
  "Testing/audits/audit_hmm_state_architecture_construct_comparison.R",
  warn = FALSE)
stopifnot(any(grepl('"hmm_architecture_redundancy", project_root)',
                   construct, fixed = TRUE)),
          any(grepl('proposal_path <- file.path(REDUNDANCY,', construct,
                    fixed = TRUE)),
          any(grepl('foundation_path <- file.path(FOUNDATION,', construct,
                    fixed = TRUE)))

rfid_comparison <- readLines(
  "Testing/audits/audit_rfid_legacy_vs_new_domains.R", warn = FALSE)
invisible(parse(text = rfid_comparison))
stopifnot(any(grepl('mmm_behavior_output_active_root("first_night_10min",',
                   rfid_comparison, fixed = TRUE)),
          any(grepl('mmm_behavior_audit_replay_output_root("rfid_legacy_vs_new_domains", ROOT)',
                    rfid_comparison, fixed = TRUE)),
          !any(grepl('mmm_behavior_output_active_root("rfid_domain_comparison_audit",',
                     rfid_comparison, fixed = TRUE)))
cat("Historical audit replay source and fresh-output paths: PASS\n")
