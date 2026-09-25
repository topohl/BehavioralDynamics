# The release verifier checks recorded source paths at their retained
# location once a numbered root is archived. Only its mapping function is
# evaluated here; no release bundle or live S: path is read.
code <- as.list(parse("Analysis/verify_publication_release.R", keep.source = FALSE))
definition <- Filter(function(e) is.call(e) && identical(e[[1L]], as.name("<-")) &&
                       identical(e[[2L]], as.name("retained_source")), code)
stopifnot(length(definition) == 1L)
eval(definition[[1L]])

root <- normalizePath(file.path(tempdir(), paste0("release_sources_",
                                                  as.integer(runif(1L, 1L, 1e9)))),
                      winslash = "/", mustWork = FALSE)
ready <- file.path(root, "analysis_ready")
recorded <- file.path(ready, "12_systems_neuroscience_summary", "5min_based", "x.csv")
archived <- file.path(ready, "history", "original_layout",
                      "12_systems_neuroscience_summary", "5min_based", "x.csv")
other <- file.path(ready, "pipeline", "09_early_prediction", "x.csv")
receipt <- file.path(ready, "_migration_control", "numbered_root_archive",
                     "12_systems_neuroscience_summary.json")
dir.create(dirname(receipt), recursive = TRUE)
stopifnot(identical(retained_source(recorded), recorded),
          identical(retained_source(other), other))
write_state <- function(state) jsonlite::write_json(list(state = state), receipt,
                                                    auto_unbox = TRUE)
write_state("activated")
stopifnot(identical(retained_source(recorded), archived),
          identical(retained_source(gsub("/", "\\\\", recorded)), archived),
          identical(retained_source(other), other))
for (state in c("prepared", "transferring", "unknown")) {
  write_state(state)
  stopifnot(identical(retained_source(recorded), recorded))
}
cat("Release verifier source mapping after a numbered-root archive: PASS\n")
