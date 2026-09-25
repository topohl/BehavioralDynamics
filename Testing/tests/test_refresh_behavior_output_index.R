# Run Maintenance/Refresh-BehaviorOutputIndex.R against a temporary project
# root. No live S: path is read or written.
root <- normalizePath(file.path(tempdir(), paste0("refresh_index_",
                                                  as.integer(runif(1L, 1L, 1e9)))),
                      winslash = "/", mustWork = FALSE)
ready <- file.path(root, "analysis_ready")
dir.create(file.path(ready, "03_derived_metrics", "qc"), recursive = TRUE)
dir.create(file.path(ready, "_migration_control"), recursive = TRUE)
index <- file.path(ready, "output_index.csv")
writeLines("stage", index)

refresh <- function(...) {
  old <- Sys.getenv("MMM_BEHAVIOR_PROJECT_ROOT", unset = NA)
  Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = root)
  on.exit(if (is.na(old)) Sys.unsetenv("MMM_BEHAVIOR_PROJECT_ROOT")
          else Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = old))
  out <- suppressWarnings(system2("Rscript", c("Maintenance/Refresh-BehaviorOutputIndex.R", ...),
                                  stdout = TRUE, stderr = TRUE))
  list(ok = is.null(attr(out, "status")), out = out)
}
digest_file <- function(path) paste(readLines(path, warn = FALSE), collapse = "\n")

# A dry run never writes; --write needs a new, well-formed backup name.
first <- refresh()
stopifnot(first$ok, any(grepl("Dry run: nothing written.", first$out, fixed = TRUE)),
          identical(readLines(index), "stage"),
          !refresh("--write")$ok,
          !refresh("--write", "--backup=../escape.csv")$ok,
          identical(readLines(index), "stage"))
written <- refresh("--write", "--backup=output_index_before_fixture_initial.csv")
stopifnot(written$ok,
          identical(readLines(file.path(ready, "_migration_control",
                                        "output_index_before_fixture_initial.csv")), "stage"),
          any(grepl("0 changed cell", refresh()$out, fixed = TRUE)))
stopifnot(!refresh("--write", "--backup=output_index_before_fixture_initial.csv")$ok)

# After a synthetic 03 archive the dry run lists only the four 03 notes.
dir.create(file.path(ready, "03_derived_metrics", "spatial_occupancy"))
dir.create(file.path(ready, "history", "original_layout"), recursive = TRUE)
stopifnot(file.rename(file.path(ready, "03_derived_metrics"),
                      file.path(ready, "history", "original_layout", "03_derived_metrics")))
receipt <- file.path(ready, "_migration_control", "numbered_root_archive",
                     "03_derived_metrics.json")
dir.create(dirname(receipt), recursive = TRUE)
write_receipt <- function(state) jsonlite::write_json(list(
  root = "03_derived_metrics", source_root_rel = "03_derived_metrics",
  archive_root_rel = "history/original_layout/03_derived_metrics", state = state,
  files = 1L, bytes = 1L, manifest_sha256 = strrep("c", 64L),
  reader_gate_kind = "ArchivePath", reader_gate_sha256 = strrep("e", 64L),
  reader_queue_sha256 = strrep("f", 64L)), receipt, auto_unbox = TRUE)
write_receipt("activated")
before <- digest_file(index)
dry <- refresh()
changed_rows <- sub("^\\[([^]]+)\\] notes$", "\\1", grep("^\\[.*\\] notes$", dry$out, value = TRUE))
stopifnot(dry$ok, identical(digest_file(index), before),
          setequal(changed_rows, c("01", "01-identity-history", "19-tables", "19-audit")),
          any(grepl("4 changed cell", dry$out, fixed = TRUE)))

# While a root is transferring the index definition stops and nothing is written.
write_receipt("transferring")
stopifnot(!refresh("--write", "--backup=output_index_before_fixture_transfer.csv")$ok,
          identical(digest_file(index), before),
          !file.exists(file.path(ready, "_migration_control",
                                 "output_index_before_fixture_transfer.csv")))
write_receipt("activated")
final <- refresh("--write", "--backup=output_index_before_fixture_archive.csv")
refreshed <- read.csv(index, stringsAsFactors = FALSE)
stopifnot(final$ok,
          identical(digest_file(file.path(ready, "_migration_control",
                                          "output_index_before_fixture_archive.csv")), before),
          grepl("Retained original: analysis_ready/history/original_layout/03_derived_metrics/qc/",
                refreshed$notes[refreshed$stage == "01-identity-history"], fixed = TRUE),
          identical(refreshed$legacy_path[refreshed$stage == "01"],
                    "analysis_ready/03_derived_metrics/"))

# A legacy folder absent before its root moved gets no note: of the 06 rows,
# only Stage 02's folder exists in this synthetic archive (live, the Stage 09
# folder was quarantined before 06 moved).
dir.create(file.path(ready, "history", "original_layout", "06_behavioral_dynamics",
                     "dyadic_contacts"), recursive = TRUE)
jsonlite::write_json(list(
  root = "06_behavioral_dynamics", source_root_rel = "06_behavioral_dynamics",
  archive_root_rel = "history/original_layout/06_behavioral_dynamics", state = "activated",
  files = 1L, bytes = 1L, manifest_sha256 = strrep("c", 64L),
  reader_gate_kind = "ArchivePath", reader_gate_sha256 = strrep("e", 64L),
  reader_queue_sha256 = strrep("f", 64L)),
  file.path(dirname(receipt), "06_behavioral_dynamics.json"), auto_unbox = TRUE)
dry06 <- refresh()
changed06 <- sub("^\\[([^]]+)\\] notes$", "\\1", grep("^\\[.*\\] notes$", dry06$out, value = TRUE))
stopifnot(dry06$ok, identical(changed06, "02"),
          any(grepl("history/original_layout/06_behavioral_dynamics/dyadic_contacts/",
                    dry06$out, fixed = TRUE)))
cat("Output index refresh after synthetic 03 and 06 archives: PASS\n")
