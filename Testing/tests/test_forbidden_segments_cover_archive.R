# Every forbidden-segment list that keeps retired or archived trees out of the
# release bundle and the main figure refuses the retained numbered roots under
# history/original_layout/. Static: each file is parsed, never run.
lists <- c(
  "Analysis/build_publication_release.R" = "FORBIDDEN_SEGMENTS",
  "Analysis/verify_publication_release.R" = "bad",
  "Analysis/27_build_behavior_main_figure.R" = "FORBIDDEN_SOURCE_SEGMENTS",
  "Analysis/27_candidate_recompose_behavior_main_figure.R" = "forbidden")
assignments <- function(e, name) {
  if (!is.call(e)) return(list())
  here <- if (identical(e[[1L]], as.name("<-")) && identical(e[[2L]], as.name(name))) list(e) else list()
  c(here, unlist(lapply(as.list(e)[-1L], assignments, name = name), recursive = FALSE))
}
for (file in names(lists)) {
  found <- unlist(lapply(as.list(parse(file, keep.source = FALSE)), assignments,
                         name = lists[[file]]), recursive = FALSE)
  stopifnot(length(found) == 1L)
  segments <- eval(found[[1L]][[3L]], baseenv())
  if (!"/history/original_layout/" %in% segments) {
    stop(file, ": ", lists[[file]], " does not refuse /history/original_layout/", call. = FALSE)
  }
}
cat("Forbidden-segment lists refuse history/original_layout: PASS\n")
