# Run the actual read-only Stage 10 / Stage 19 boundary audit against a
# synthetic recorded feature-source table, before and after a synthetic 06
# root archive. No live S: path is read or written.
root <- normalizePath(file.path(tempdir(), paste0("s10_spatial_",
                                                  as.integer(runif(1L, 1L, 1e9)))),
                      winslash = "/", mustWork = FALSE)
ready <- file.path(root, "analysis_ready")
old06 <- file.path(ready, "06_behavioral_dynamics")
dir.create(file.path(old06, "dyadic_contacts"), recursive = TRUE)
loaded <- file.path(old06, "dyadic_contacts", sprintf("feature_%03d.csv", 1:176))
for (path in loaded) writeLines("AnimalNum", path)
models <- file.path(ready, "04_model_outputs", "spatial_occupancy",
                    sprintf("model_%d.csv", 1:4))
tables <- file.path(ready, "pipeline", "10_systems_prediction", "10min", "tables")
dir.create(tables, recursive = TRUE)
write.csv(data.frame(source_file = c(loaded, models),
                     loaded_as_feature_table = c(rep(TRUE, 176L), rep(FALSE, 4L))),
          file.path(tables, "feature_source_audit.csv"), row.names = FALSE)

# system2(env = ) is not supported on Windows, so set the root for the child.
run_audit <- function() {
  old <- Sys.getenv("MMM_BEHAVIOR_PROJECT_ROOT", unset = NA)
  Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = root)
  on.exit(if (is.na(old)) Sys.unsetenv("MMM_BEHAVIOR_PROJECT_ROOT")
          else Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = old))
  output <- suppressWarnings(system2(
    "Rscript", "Testing/audits/audit_stage10_spatial_feature_boundary.R",
    stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  list(ok = is.null(status) || identical(status, 0L), output = output)
}
before <- run_audit()
stopifnot(before$ok, any(grepl("176 loaded sources remain present", before$output,
                               fixed = TRUE)))

archived06 <- file.path(ready, "history", "original_layout", "06_behavioral_dynamics")
dir.create(dirname(archived06), recursive = TRUE)
stopifnot(file.rename(old06, archived06))
dir.create(file.path(ready, "_migration_control", "numbered_root_archive"),
           recursive = TRUE)
jsonlite::write_json(list(
  root = "06_behavioral_dynamics", source_root_rel = "06_behavioral_dynamics",
  archive_root_rel = "history/original_layout/06_behavioral_dynamics",
  state = "activated", files = 176L, bytes = 1L,
  manifest_sha256 = strrep("c", 64L), reader_gate_kind = "ArchivePath",
  reader_gate_sha256 = strrep("e", 64L), reader_queue_sha256 = strrep("f", 64L)),
  file.path(ready, "_migration_control", "numbered_root_archive",
            "06_behavioral_dynamics.json"), auto_unbox = TRUE)
after <- run_audit()
stopifnot(after$ok)

# A recorded source missing from the retained archive still fails.
unlink(file.path(archived06, "dyadic_contacts", "feature_001.csv"))
missing <- run_audit()
stopifnot(!missing$ok, any(grepl("no longer present", missing$output, fixed = TRUE)))
cat("Stage 10 spatial boundary audit after synthetic 06 archive: PASS\n")
