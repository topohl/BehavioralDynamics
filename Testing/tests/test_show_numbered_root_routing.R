# Run Maintenance/Show-BehaviorNumberedRootRouting.R on a synthetic archived
# 03 root. No live S: path is read or written.
root <- normalizePath(file.path(tempdir(), paste0("routing_report_",
                                                  as.integer(runif(1L, 1L, 1e9)))),
                      winslash = "/", mustWork = FALSE)
ready <- file.path(root, "analysis_ready")
archived <- file.path(ready, "history", "original_layout", "03_derived_metrics")
dir.create(file.path(archived, "qc"), recursive = TRUE)
writeLines("AnimalNum", file.path(archived, "qc", "x.csv"))
manifest <- file.path(root, "manifest.csv")
write.csv(data.frame(relative_path = c("qc/x.csv", "qc/missing.csv"),
                     size_bytes = c("10", "1"), sha256 = strrep("a", 64L)),
          manifest, row.names = FALSE)
receipt <- file.path(ready, "_migration_control", "numbered_root_archive",
                     "03_derived_metrics.json")
dir.create(dirname(receipt), recursive = TRUE)
jsonlite::write_json(list(
  root = "03_derived_metrics", source_root_rel = "03_derived_metrics",
  archive_root_rel = "history/original_layout/03_derived_metrics", state = "activated",
  files = 2L, bytes = 11L, manifest_sha256 = strrep("c", 64L),
  reader_gate_kind = "ArchivePath", reader_gate_sha256 = strrep("e", 64L),
  reader_queue_sha256 = strrep("f", 64L)), receipt, auto_unbox = TRUE)
old <- Sys.getenv("MMM_BEHAVIOR_PROJECT_ROOT", unset = NA)
Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = root)
out <- suppressWarnings(system2("Rscript", c("Maintenance/Show-BehaviorNumberedRootRouting.R",
                                             "03_derived_metrics", manifest),
                                stdout = TRUE, stderr = TRUE))
if (is.na(old)) Sys.unsetenv("MMM_BEHAVIOR_PROJECT_ROOT") else Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = old)
line <- function(label) grep(paste0("^", label), out, value = TRUE)
stopifnot(is.null(attr(out, "status")),
          grepl("activated", line("receipt state")),
          grepl("history/original_layout/03_derived_metrics", paste(out, collapse = " "), fixed = TRUE),
          grepl("ERROR", line("guard old root")), grepl("ERROR", line("guard archive")),
          grepl(" 1$", line("R can open at retained location")),
          !dir.exists(file.path(ready, "03_derived_metrics")))
cat("Numbered root routing report on a synthetic archive: PASS\n")
