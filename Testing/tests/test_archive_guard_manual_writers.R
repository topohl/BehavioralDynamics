# The dashboard metadata refresher and the identity repair utility refuse the
# retained archive and an archived numbered root of the project that holds
# their target. Every path used here is under a temporary project root; both
# tools stop before writing if the guard does not fire.
suppressPackageStartupMessages(library(dplyr))
root <- normalizePath(file.path(tempdir(), paste0("manual_writer_guard_",
                                                  as.integer(runif(1L, 1L, 1e9)))),
                      winslash = "/", mustWork = FALSE)
ready <- file.path(root, "analysis_ready")
archived12 <- file.path(ready, "history", "original_layout",
                        "12_systems_neuroscience_summary", "5min_based")
archived03 <- file.path(ready, "history", "original_layout", "03_derived_metrics",
                        "5min_based")
dir.create(file.path(archived12, "figures"), recursive = TRUE)
dir.create(archived03, recursive = TRUE)
writeLines("<svg/>", file.path(archived12, "figures", "panel.svg"))
receipt_dir <- file.path(ready, "_migration_control", "numbered_root_archive")
dir.create(receipt_dir, recursive = TRUE)
for (r in c("03_derived_metrics", "12_systems_neuroscience_summary")) {
  jsonlite::write_json(list(
    root = r, source_root_rel = r,
    archive_root_rel = paste0("history/original_layout/", r), state = "activated",
    files = 1L, bytes = 1L, manifest_sha256 = strrep("c", 64L),
    reader_gate_kind = "ArchivePath", reader_gate_sha256 = strrep("e", 64L),
    reader_queue_sha256 = strrep("f", 64L)),
    file.path(receipt_dir, paste0(r, ".json")), auto_unbox = TRUE)
}
listing <- function() sort(list.files(root, recursive = TRUE, all.files = TRUE))
before <- listing()

# Refresher: refused inside the archive, before any file is written.
repo <- normalizePath(".", winslash = "/")
out <- suppressWarnings(system2("Rscript", c("Maintenance/Refresh-SystemsDashboardMetadata.R",
                                             shQuote(archived12), "1", shQuote(repo)),
                                stdout = TRUE, stderr = TRUE))
stopifnot(!is.null(attr(out, "status")),
          any(grepl("Refusing to write into retained numbered archive", out, fixed = TRUE)),
          identical(listing(), before))

# Repair utility: refused for an archived 03 original, which stays unchanged.
metrics <- file.path(archived03, "all_behavior_metrics.csv")
writeLines(c("AnimalNum,Sex", "OR101,Female"), metrics)
before <- listing()
original <- readLines(metrics)
source("Testing/audits/repair_existing_metrics_identity_utility.R")
refused <- tryCatch({
  repair_existing_metrics_identity_for_testing(metrics, NULL, NULL)
  FALSE
}, error = function(e) grepl("Refusing to write into retained numbered archive",
                             conditionMessage(e), fixed = TRUE))
stopifnot(refused, identical(readLines(metrics), original), identical(listing(), before))
cat("Manual writers refuse the retained archive: PASS\n")
