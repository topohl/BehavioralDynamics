# Parse-based input/output contract for the queued historical audits. Nothing
# is sourced or executed. It checks three regressions the text-pattern path
# test cannot see:
#   1. every write, directory creation or deletion is rooted at the script's
#      own replay output root (console output is allowed);
#   2. no read uses a path assembled from a numbered-root literal outside the
#      archive-aware resolvers (for example file.path(PROJ, "analysis_ready",
#      "06_behavioral_dynamics"));
#   3. every file a same-run consumer reads from a prerequisite folder is a
#      file its producer writes there.
queue <- read.csv("docs/behavior_output_archive_audit_script_queue.csv",
                  stringsAsFactors = FALSE)
plan_file <- tempfile(fileext = ".csv")
stopifnot(identical(suppressWarnings(system2(
  "Rscript", "Maintenance/Get-BehaviorAuditReplayPlan.R", stdout = plan_file,
  stderr = FALSE)), 0L))
plan <- read.csv(plan_file, stringsAsFactors = FALSE)
stopifnot(identical(sort(plan$script), sort(queue$script)))

# Every top-level tree of the original layout, parsed from the resolver.
root_list <- Filter(function(e) is.call(e) && identical(e[[1L]], as.name("<-")) &&
                      identical(e[[2L]], as.name("MMM_NUMBERED_BEHAVIOR_ROOTS")),
                    as.list(parse("Functions/project_paths.R", keep.source = FALSE)))
stopifnot(length(root_list) == 1L)
numbered_literal <- paste0("(^|[/\\\\])(",
                           paste(eval(root_list[[1L]][[3L]], baseenv()), collapse = "|"),
                           ")([/\\\\]|$)")
resolvers <- c("mmm_behavior_numbered_source_root", "mmm_behavior_numbered_writer_root",
               "mmm_behavior_retained_source_root")
# Path argument of each writer: a formal name and its position.
writers <- list(
  write_csv = c("file", 2), write_tsv = c("file", 2), write_delim = c("file", 2),
  write.csv = c("file", 2), write.table = c("file", 2), fwrite = c("file", 2),
  write_table = c("path", 2), saveRDS = c("file", 2), write_json = c("path", 2),
  write_xlsx = c("path", 2), write.xlsx = c("file", 2), ggsave = c("filename", 1),
  save_plot_svg_pdf = c("filename_base", 2), dir.create = c("path", 1),
  ensure_dir = c("path", 1), png = c("filename", 1), pdf = c("file", 1),
  svg = c("filename", 1), file.copy = c("to", 2), file.rename = c("to", 2),
  unlink = c("x", 1), file.remove = c("...", 1), sink = c("file", 1),
  writeLines = c("con", 2), cat = c("file", NA), save = c("file", NA))
readers <- c("read_csv", "read_tsv", "read_delim", "read.csv", "read.table", "fread",
             "readRDS", "readLines", "read_excel", "read_xlsx", "list.files", "list.dirs",
             "dir", "fromJSON", "read_json", "file.exists", "dir.exists", "file.info",
             "Sys.glob", "load", "read_lines", "vroom")

call_name <- function(x) {
  f <- x[[1L]]
  if (is.symbol(f)) return(as.character(f))
  if (is.call(f) && as.character(f[[1L]]) %in% c("::", ":::")) return(as.character(f[[3L]]))
  ""
}
# Empty arguments (x[, 1], function(a, b)) are the missing-argument symbol,
# which cannot be bound to a variable; test them in place.
blank <- function(container, i) {
  is.null(container[[i]]) ||
    (is.symbol(container[[i]]) && !nzchar(as.character(container[[i]])))
}
calls_in <- function(x, acc = list()) {
  if (is.call(x)) {
    acc[[length(acc) + 1L]] <- x
    for (i in seq_along(x)) {
      if (i == 1L && is.symbol(x[[1L]])) next
      if (!blank(x, i)) acc <- calls_in(x[[i]], acc)
    }
  } else if (is.pairlist(x) || is.expression(x) || is.list(x)) {
    for (i in seq_along(x)) if (!blank(x, i)) acc <- calls_in(x[[i]], acc)
  }
  acc
}
symbols_in <- function(x) unique(all.names(x, functions = FALSE))
path_arg <- function(call, spec) {
  if (length(call) < 2L) return(NULL)
  nm <- names(call) %||% rep("", length(call))
  idx <- seq_along(call)[-1L]
  hit <- idx[nm[idx] == spec[[1L]]]
  if (length(hit)) return(if (blank(call, hit[[1L]])) NULL else call[[hit[[1L]]]])
  if (is.na(spec[[2L]])) return(NULL)
  unnamed <- idx[!nzchar(nm[idx])]
  pos <- as.integer(spec[[2L]])
  if (length(unnamed) >= pos && !blank(call, unnamed[[pos]])) call[[unnamed[[pos]]]] else NULL
}
`%||%` <- function(x, y) if (is.null(x)) y else x
# Literals naming a numbered root, outside the archive-aware resolver calls.
tainted_literal <- function(x) {
  if (is.character(x)) return(any(grepl(numbered_literal, x)))
  if (is.call(x)) {
    if (call_name(x) %in% resolvers) return(FALSE)
    if (identical(call_name(x), "mmm_behavior_output_group_root") &&
        any(vapply(as.list(x)[-1L], function(a) identical(a, "current"), logical(1)))) return(TRUE)
    for (i in seq_along(x)) if (!blank(x, i) && tainted_literal(x[[i]])) return(TRUE)
    return(FALSE)
  }
  FALSE
}
assignments <- function(exprs) {
  out <- list()
  for (cl in calls_in(exprs)) {
    if (call_name(cl) %in% c("<-", "=", "<<-") && length(cl) == 3L && is.symbol(cl[[2L]])) {
      out[[length(out) + 1L]] <- list(lhs = as.character(cl[[2L]]), rhs = cl[[3L]])
    }
  }
  out
}
closure <- function(assign, seed) {
  set <- seed
  repeat {
    added <- unique(unlist(lapply(assign, function(a)
      if (!a$lhs %in% set && any(symbols_in(a$rhs) %in% set)) a$lhs)))
    if (length(added) == 0L) return(set)
    set <- c(set, added)
  }
}
seeded_by <- function(assign, fn, id = NULL) {
  unique(unlist(lapply(assign, function(a) {
    hits <- Filter(function(cl) identical(call_name(cl), fn) &&
                     (is.null(id) || identical(cl[[2L]], id)), calls_in(a$rhs))
    if (length(hits)) a$lhs
  })))
}
# Literal filenames under file.path(<symbol in roots>, ...).
file_literals <- function(expr, roots) {
  found <- character()
  for (cl in calls_in(expr)) {
    if (identical(call_name(cl), "file.path") && length(cl) >= 3L &&
        any(symbols_in(cl[[2L]]) %in% roots)) {
      parts <- as.list(cl)[-(1:2)]
      if (length(parts) && all(vapply(parts, is.character, logical(1)))) {
        found <- c(found, paste(unlist(parts), collapse = "/"))
      }
    }
  }
  found
}

violations <- character()
written <- list()
consumed <- list()
for (i in seq_len(nrow(plan))) {
  script <- plan$script[[i]]
  exprs <- parse(file = script, keep.source = FALSE)
  assign <- assignments(exprs)
  out_roots <- closure(assign, seeded_by(assign, "mmm_behavior_audit_replay_output_root"))
  tainted <- closure(assign, unique(unlist(lapply(assign, function(a)
    if (tainted_literal(a$rhs)) a$lhs))))
  all_calls <- calls_in(exprs)
  produced <- character()
  for (cl in all_calls) {
    name <- call_name(cl)
    if (name %in% names(writers)) {
      target <- path_arg(cl, writers[[name]])
      console <- is.null(target) || (name %in% c("cat", "writeLines", "sink") &&
                                       (identical(target, "") || is.null(target) ||
                                        identical(target, quote(stdout())) ||
                                        identical(target, quote(stderr()))))
      if (!console && !any(symbols_in(target) %in% out_roots)) {
        violations <- c(violations, sprintf("%s: %s() target not under its replay output root: %s",
                                            script, name, paste(deparse(target), collapse = " ")))
      }
    }
    if (name %in% readers) {
      target <- path_arg(cl, c("file", 1))
      if (!is.null(target) && (tainted_literal(target) || any(symbols_in(target) %in% tainted))) {
        violations <- c(violations, sprintf("%s: %s() reads a path built from a numbered-root literal: %s",
                                            script, name, paste(deparse(target), collapse = " ")))
      }
    }
  }
  # Paths are often assigned before they are written (p <- file.path(OUT,
  # "x.csv"); write_csv(d, p)), so collect every literal path built under the
  # output root. Rule 1 above already requires each write to use that root.
  produced <- c(produced, file_literals(exprs, out_roots))
  # Filenames produced through local helpers such as w(x, "name.csv") that
  # write file.path(OUT, nm): accept literal names passed to such helpers.
  helper_writers <- unique(unlist(lapply(assign, function(a)
    if (is.call(a$rhs) && identical(call_name(a$rhs), "function") &&
        any(vapply(calls_in(a$rhs[[3L]]), function(cl) call_name(cl) %in% names(writers),
                   logical(1))) &&
        any(symbols_in(a$rhs[[3L]]) %in% out_roots)) a$lhs)))
  for (cl in all_calls) {
    if (call_name(cl) %in% helper_writers && length(cl) > 1L) {
      for (j in seq_along(cl)[-1L]) {
        if (!blank(cl, j) && is.character(cl[[j]])) produced <- c(produced, cl[[j]])
      }
    }
  }
  if (nzchar(plan$output_id[[i]])) written[[plan$output_id[[i]]]] <- unique(produced)
  if (nzchar(plan$prerequisite_output_ids[[i]])) {
    for (id in strsplit(plan$prerequisite_output_ids[[i]], ";", fixed = TRUE)[[1L]]) {
      roots <- closure(assign, seeded_by(assign, "mmm_behavior_audit_replay_input_root", id))
      if (length(roots) == 0L) {
        violations <- c(violations, sprintf("%s: no input root for prerequisite %s", script, id))
      }
      # Every literal path built under the prerequisite folder is a read of
      # that folder: the audits only read from their input roots.
      reads <- unique(file_literals(exprs, roots))
      consumed[[length(consumed) + 1L]] <- list(script = script, id = id, files = reads)
    }
  }
}
for (edge in consumed) {
  if (length(edge$files) == 0L) {
    violations <- c(violations, sprintf("%s: reads no literal file from prerequisite %s",
                                        edge$script, edge$id))
  }
  missing_files <- setdiff(edge$files, written[[edge$id]])
  if (length(missing_files)) {
    violations <- c(violations, sprintf("%s: reads %s from %s, which its producer does not write",
                                        edge$script, paste(missing_files, collapse = ", "), edge$id))
  }
}
if (length(violations)) {
  cat(violations, sep = "\n")
  stop(length(violations), " historical audit I/O contract violation(s)")
}
cat(sprintf("Historical audit replay I/O contract: PASS (%d scripts, %d same-run edges)\n",
            nrow(plan), length(consumed)))
