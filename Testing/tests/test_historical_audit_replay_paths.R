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
  manifest_sha256 = paste(rep("c", 64L), collapse = "")),
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
cat("Historical audit replay source and fresh-output paths: PASS\n")
