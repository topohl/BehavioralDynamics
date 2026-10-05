# No semantic path key may resolve into a superseded, bundled, archived or retired tree.
#
# Lifted from the retired Stage 27 contract test on 2026-10-05, with /history/retired/ added for the outputs of
# the producers retired that day. Every key in Functions/project_paths.R is resolved without requiring the file.
#
# Portable: resolves paths only; nothing is read or written.

source("Analysis/_pipeline_setup.R")
source_mmm_helper("project_paths.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

BAD_SEGMENTS <- c("_quarantine", "quarantine_legacy", "_archive", "snapshot",
                  "/releases/", "_pre_hmm_identity_fix", "_erroneous",
                  "/history/original_layout/", "/history/retired/")
keys <- mmm_path_keys()
check(length(keys) > 0L, "the path registry declares no keys")
all_registry <- unlist(lapply(keys, function(k) mmm_path_get(k, required = FALSE)))
check(length(all_registry) >= length(keys), "a registry key resolved to no path")
for (seg in BAD_SEGMENTS) {
  hits <- all_registry[grepl(seg, all_registry, fixed = TRUE)]
  check(length(hits) == 0L,
        paste0("registry resolves a path containing '", seg, "': ", paste(hits, collapse = ", ")))
}
ok(sprintf("%d keys, %d paths: none in a snapshot, quarantine, archive, release or retired tree",
           length(keys), length(all_registry)))

cat("\nPASS: path registry segments\n")
