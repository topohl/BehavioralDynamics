# Archive receipt routing is tested only in a temporary fixture. No live
# numbered output root or scientific file is moved by this test.
source("Functions/project_paths.R")

root <- file.path(tempdir(), paste0("numbered_archive_resolver_",
                                    as.integer(runif(1L, 1L, 1e9))))
ready <- file.path(root, "analysis_ready")
group <- "dyadic_contacts"
numbered <- "06_behavioral_dynamics"
old_root <- file.path(ready, numbered)
old_group <- mmm_behavior_output_group_root(group, "current", root)
semantic <- mmm_behavior_output_group_root(group, "semantic", root)
archived_root <- file.path(ready, "history", "original_layout", numbered)
archived_group <- file.path(archived_root, "dyadic_contacts")
archive_receipt <- file.path(ready, "_migration_control",
                             "numbered_root_archive", paste0(numbered, ".json"))
activation_receipt <- file.path(ready, "_migration_control", paste0(group, ".json"))
dir.create(old_group, recursive = TRUE, showWarnings = FALSE)
dir.create(semantic, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(archive_receipt), recursive = TRUE, showWarnings = FALSE)
jsonlite::write_json(list(
  group = group, state = "activated", files = 1L,
  target_root_rel = "analyses/dyadic_contacts",
  group_plan_sha256 = paste(rep("a", 64L), collapse = ""),
  contract_sha256 = paste(rep("b", 64L), collapse = ""),
  source_retained = TRUE), activation_receipt, auto_unbox = TRUE)

check_error <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
# The same fields Maintenance/Invoke-BehaviorNumberedRootArchive.ps1 writes.
receipt_fields <- function(state, archive_rel = paste0(
                             "history/original_layout/", root_name),
                           files = 1L, bytes = 10L, root_name = numbered) {
  list(root = root_name, source_root_rel = root_name,
       archive_root_rel = archive_rel, state = state,
       files = files, bytes = bytes,
       manifest_sha256 = strrep("c", 64L), reader_gate_kind = "ArchivePath",
       reader_gate_sha256 = strrep("e", 64L),
       reader_queue_sha256 = strrep("f", 64L))
}
write_archive_receipt <- function(state, ...) {
  jsonlite::write_json(receipt_fields(state, ...), archive_receipt,
                       auto_unbox = TRUE, digits = NA)
}
# Other spellings of the same directory: case-folded, and a UNC name for the
# local drive (//localhost/C$/...).
unc_spelling <- function(path) {
  sub("^([A-Za-z]):", "//localhost/\\1$",
      normalizePath(path, winslash = "/", mustWork = FALSE))
}

stopifnot(identical(mmm_behavior_retained_source_root(group, root), old_group),
          identical(mmm_behavior_output_active_root(group, root), semantic),
          identical(mmm_behavior_numbered_writer_root(numbered, root), old_root),
          identical(mmm_behavior_guard_numbered_output_path(old_group, root),
                    old_group),
          identical(mmm_behavior_guard_numbered_output_path(semantic, root),
                    semantic),
          check_error(mmm_behavior_guard_numbered_output_path(archived_group,
                                                              root)))

# An unreceipted archive, including one made while the original still exists,
# must not silently select either copy.
dir.create(archived_root, recursive = TRUE, showWarnings = FALSE)
stopifnot(check_error(mmm_behavior_output_active_root(group, root)),
          check_error(mmm_behavior_numbered_writer_root(numbered, root)))
stopifnot(file.rename(archived_root, paste0(archived_root, "_unreceipted")))

write_archive_receipt("prepared")
stopifnot(identical(mmm_behavior_output_active_root(group, root), semantic),
          identical(mmm_behavior_retained_source_root(group, root), old_group),
          check_error(mmm_behavior_numbered_writer_root(numbered, root)),
          check_error(mmm_behavior_guard_numbered_output_path(old_group, root)))
# Case-folded and UNC spellings of the numbered root are refused as well;
# a semantic destination spelled through UNC is not over-blocked.
stopifnot(check_error(mmm_behavior_guard_numbered_output_path(tolower(old_group), root)),
          check_error(mmm_behavior_guard_numbered_output_path(
            file.path(tolower(old_root), "hmm_states", "1min_based"), root)),
          check_error(mmm_behavior_guard_numbered_output_path(
            unc_spelling(old_group), root)),
          identical(mmm_behavior_guard_numbered_output_path(
            unc_spelling(semantic), root), unc_spelling(semantic)))
# A prepared receipt with an archive directory already present fails closed.
dir.create(archived_root, recursive = TRUE, showWarnings = FALSE)
stopifnot(check_error(mmm_behavior_retained_source_root(group, root)))
unlink(archived_root, recursive = TRUE)
write_archive_receipt("transferring")
stopifnot(check_error(mmm_behavior_output_active_root(group, root)),
          check_error(mmm_behavior_numbered_writer_root(numbered, root)),
          check_error(mmm_behavior_guard_numbered_output_path(old_group, root)))
write_archive_receipt("activated")
stopifnot(check_error(mmm_behavior_output_active_root(group, root)))

dir.create(dirname(archived_root), recursive = TRUE, showWarnings = FALSE)
stopifnot(file.rename(old_root, archived_root))
stopifnot(identical(mmm_behavior_retained_source_root(group, root), archived_group),
          identical(mmm_behavior_output_active_root(group, root), semantic),
          check_error(mmm_behavior_numbered_writer_root(numbered, root)),
          check_error(mmm_behavior_guard_numbered_output_path(archived_group,
                                                              root)),
          check_error(mmm_behavior_guard_numbered_output_path(old_group, root)))
# Another spelling of the project root still resolves the archive, and other
# spellings of the archive are refused as write targets.
stopifnot(identical(tolower(mmm_behavior_retained_source_root(group, tolower(root))),
                    tolower(archived_group)),
          check_error(mmm_behavior_guard_numbered_output_path(
            file.path(tolower(archived_root), "hmm_states", "new"), root)),
          check_error(mmm_behavior_guard_numbered_output_path(
            unc_spelling(archived_group), root)))

# The live 06 root holds 18,194,653,380 bytes. A receipt above 2^31 must still
# validate; malformed JSON or an incomplete, ambiguous, or mistyped receipt
# must stop readers and writers.
write_archive_receipt("activated", files = 1469, bytes = 18194653380)
stopifnot(grepl('"bytes":18194653380', paste(readLines(archive_receipt,
                                                       warn = FALSE),
                                              collapse = ""), fixed = TRUE),
          identical(mmm_behavior_retained_source_root(group, root), archived_group))
writeLines("{", archive_receipt)
stopifnot(check_error(mmm_behavior_retained_source_root(group, root)),
          check_error(mmm_behavior_guard_numbered_output_path(old_group, root)))
write_receipt_value <- function(fields) {
  jsonlite::write_json(fields, archive_receipt, auto_unbox = TRUE, digits = NA)
}
without_state <- receipt_fields("activated")
without_state$state <- NULL
write_receipt_value(without_state)
stopifnot(check_error(mmm_behavior_retained_source_root(group, root)))
# `$state` would partially match state_note; the resolver must not.
write_receipt_value(c(without_state, list(state_note = "activated")))
stopifnot(check_error(mmm_behavior_retained_source_root(group, root)))
json <- jsonlite::toJSON(receipt_fields("prepared"), auto_unbox = TRUE)
writeLines(sub("}$", ',"state":"activated"}', json), archive_receipt)
stopifnot(check_error(mmm_behavior_retained_source_root(group, root)))
for (field in list(list(files = 1.5), list(files = "1"), list(bytes = -1),
                   list(reader_gate_kind = "Other"),
                   list(reader_gate_sha256 = "abc"),
                   list(reader_gate_kind = NULL))) {
  fields <- receipt_fields("activated")
  fields[names(field)] <- field
  write_receipt_value(fields)
  stopifnot(check_error(mmm_behavior_retained_source_root(group, root)))
}
write_archive_receipt("activated")
stopifnot(identical(mmm_behavior_retained_source_root(group, root), archived_group))

# Stage 15 without its own activation reads and would write the retained 06
# root; after the archive that is the archive, which the guard refuses.
proteomics_base <- mmm_behavior_proteomics_base_dir(root)
stopifnot(identical(normalizePath(proteomics_base, winslash = "/"),
                    normalizePath(archived_root, winslash = "/")),
          check_error(mmm_behavior_guard_numbered_output_path(proteomics_base, root)))

# A group without its own semantic activation still resolves the retained file
# inside the archive when the root-level archive receipt is activated.
other_group <- "history_social_networks_1min"
other_archived <- file.path(archived_root, "social_networks", "1min_based")
dir.create(other_archived, recursive = TRUE, showWarnings = FALSE)
stopifnot(identical(mmm_behavior_output_active_root(other_group, root),
                    other_archived))

dir.create(old_root, recursive = TRUE, showWarnings = FALSE)
stopifnot(check_error(mmm_behavior_output_active_root(group, root)))
stopifnot(file.rename(old_root, paste0(old_root, "_recreated")))
write_archive_receipt("activated", "history/original_layout/another_root")
stopifnot(check_error(mmm_behavior_output_active_root(group, root)))
write_archive_receipt("unknown")
stopifnot(check_error(mmm_behavior_output_active_root(group, root)))

# The older movement scripts read the complete 03 root, including optional
# resolutions, through the same activated root-level receipt.
archived03 <- file.path(ready, "history", "original_layout",
                        "03_derived_metrics")
dir.create(archived03, recursive = TRUE, showWarnings = FALSE)
receipt03 <- file.path(ready, "_migration_control", "numbered_root_archive",
                       "03_derived_metrics.json")
jsonlite::write_json(receipt_fields("activated", root_name = "03_derived_metrics"),
                     receipt03, auto_unbox = TRUE)
stopifnot(identical(mmm_behavior_numbered_source_root("03_derived_metrics",
                                                     root), archived03),
          check_error(mmm_behavior_guard_numbered_output_path(archived03, root)),
          check_error(mmm_behavior_guard_numbered_output_path(
            file.path(ready, "03_derived_metrics"), root)),
          identical(mmm_behavior_guard_numbered_output_path(
            file.path(ready, "foundations", "behavior_metrics"), root),
            file.path(ready, "foundations", "behavior_metrics")),
          identical(mmm_behavior_guard_numbered_output_path(
            file.path(root, "cookiehab", "analysis_ready", "03_derived_metrics"),
            root),
            file.path(root, "cookiehab", "analysis_ready", "03_derived_metrics")))

# Stage 14's frozen pre-fix table exists only in the retained 12 original.
# After the 12 archive the static 'current' path no longer finds it; the
# retained resolver does. An unactivated dashboard group would resolve its
# writer root inside the archive, which the Stage 14 guard refuses.
pre_fix_rel <- "stats_tables/systems_sis_domain_effect_summary_pre_hmm_identity_fix.csv"
dashboard_old <- mmm_behavior_output_group_root("systems_dashboard_5min", "current", root)
dir.create(file.path(dashboard_old, "stats_tables"), recursive = TRUE, showWarnings = FALSE)
writeLines("Domain,contrast", file.path(dashboard_old, pre_fix_rel))
archived12 <- file.path(ready, "history", "original_layout",
                        "12_systems_neuroscience_summary")
stopifnot(file.rename(file.path(ready, "12_systems_neuroscience_summary"), archived12))
jsonlite::write_json(receipt_fields("activated",
                                    root_name = "12_systems_neuroscience_summary"),
                     file.path(ready, "_migration_control", "numbered_root_archive",
                               "12_systems_neuroscience_summary.json"),
                     auto_unbox = TRUE)
dashboard_retained <- mmm_behavior_retained_source_root("systems_dashboard_5min", root)
stopifnot(identical(dashboard_retained, file.path(archived12, "5min_based")),
          !file.exists(file.path(dashboard_old, pre_fix_rel)),
          file.exists(file.path(dashboard_retained, pre_fix_rel)),
          check_error(mmm_behavior_guard_numbered_output_path(
            mmm_behavior_output_active_root("systems_dashboard_5min", root), root)))
for (script in c("Analysis/_archive/18_raw_movement_publication_trajectory.R",
                 "Analysis/_archive/18b_raw_movement_broad_phase_stats.R",
                 "Testing/legacy/check_behavioral_dynamics_structure.R")) {
  code <- readLines(script, warn = FALSE)
  invisible(parse(text = code))
  stopifnot(any(grepl('mmm_behavior_numbered_source_root(', code,
                     fixed = TRUE)))
}

contracts <- c(
  "Analysis/01_build_multiscale_behavior_metrics.R",
  "Analysis/02_build_dyadic_rfid_contacts.R",
  "Analysis/14_systems_neuroscience_summary_dashboard.R",
  "Analysis/15_behavior_proteomics_integration.R",
  "Analysis/19_spatial_occupancy_maps.R",
  "Analysis/04_temporal_instability.R",
  "Analysis/05_behavioral_state_space.R",
  "Analysis/06_dynamic_social_networks.R",
  "Analysis/07_gamm_trajectory_features.R",
  "Analysis/08_hmm_behavioral_states_optional.R")
for (script in contracts) {
  code <- readLines(script, warn = FALSE)
  invisible(parse(text = code))
  stopifnot(any(grepl('mmm_behavior_guard_numbered_output_path(', code,
                     fixed = TRUE)))
}

contracts <- c(
  "Analysis/14_systems_neuroscience_summary_dashboard.R",
  "Analysis/_archive/08_early_prediction_models.R",
  "Testing/legacy/run_behavioral_dynamics_pipeline.R",
  "Testing/legacy/run_full_systems_behavior_pipeline.R")
for (script in contracts) {
  code <- readLines(script, warn = FALSE)
  invisible(parse(text = code))
  stopifnot(any(grepl('mmm_behavior_numbered_writer_root(', code,
                     fixed = TRUE)))
}
cat("Numbered root archive receipt resolver: PASS\n")
