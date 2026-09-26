# The release builder's upstream cross-check hashes a Stage 16 source at its
# retained location once its numbered root is archived. Only that mapping
# function is evaluated, against a temporary fixture; no release is built.
source("Functions/project_paths.R")
code <- as.list(parse("Analysis/build_publication_release.R", keep.source = FALSE))
definition <- Filter(function(e) is.call(e) && identical(e[[1L]], as.name("<-")) &&
                       identical(e[[2L]], as.name("upstream_live_path")), code)
stopifnot(length(definition) == 1L)

PROJECT_ROOT <- normalizePath(file.path(tempdir(), paste0("release_upstream_",
                                                          as.integer(runif(1L, 1L, 1e9)))),
                              winslash = "/", mustWork = FALSE)
eval(definition[[1L]])
ready <- file.path(PROJECT_ROOT, "analysis_ready")
recorded <- "analysis_ready/00_qc_tracking_integrity/tables/tracking_qc_by_animal.csv"
dir.create(file.path(ready, "00_qc_tracking_integrity", "tables"), recursive = TRUE)
stopifnot(identical(upstream_live_path(recorded), file.path(PROJECT_ROOT, recorded)),
          identical(upstream_live_path("analysis_ready/manuscript/behavior/x.csv"),
                    file.path(PROJECT_ROOT, "analysis_ready/manuscript/behavior/x.csv")))

dir.create(file.path(ready, "history", "original_layout"), recursive = TRUE)
stopifnot(file.rename(file.path(ready, "00_qc_tracking_integrity"),
                      file.path(ready, "history", "original_layout", "00_qc_tracking_integrity")))
receipts <- file.path(ready, "_migration_control", "numbered_root_archive")
dir.create(receipts, recursive = TRUE)
jsonlite::write_json(list(
  root = "00_qc_tracking_integrity", source_root_rel = "00_qc_tracking_integrity",
  archive_root_rel = "history/original_layout/00_qc_tracking_integrity", state = "activated",
  files = 1L, bytes = 1L, manifest_sha256 = strrep("c", 64L),
  reader_gate_kind = "ArchivePath", reader_gate_sha256 = strrep("e", 64L),
  reader_queue_sha256 = strrep("f", 64L)),
  file.path(receipts, "00_qc_tracking_integrity.json"), auto_unbox = TRUE)
stopifnot(identical(upstream_live_path(recorded),
                    file.path(ready, "history", "original_layout", "00_qc_tracking_integrity",
                              "tables", "tracking_qc_by_animal.csv")),
          identical(upstream_live_path(gsub("/", "\\\\", recorded)),
                    upstream_live_path(recorded)))
cat("Release builder hashes archived upstream sources at their retained location: PASS\n")
