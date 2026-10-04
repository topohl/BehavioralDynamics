# Scripts that portable tests source must not do work when sourced.
#
# On 2026-09-25 a test that sourced an unguarded runner ran the real cookie-habituation pipeline on live data. Since
# then a test may source an Analysis/ or Testing/audits/ script only if its top level holds nothing but definitions:
#   * function definitions and constant assignments (pure constructors only: c, list, tribble, file.path, ...);
#   * package loading and helper sourcing (also a for loop over helpers);
#   * at most one main guard: if (sys.nframe() == 0L) {...}, or a flag computed from commandArgs() in local().
# Scripts sourced by those scripts are checked too (31d sources 31c). No test may execute an Analysis/ script through
# Rscript, system() or system2().
#
# Portable: parses files only; nothing is sourced or run.

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

WORK_DIRS <- "^(Analysis|Testing/audits)/"
SETUP <- "Analysis/_pipeline_setup.R"
PURE <- c("c", "list", "character", "numeric", "integer", "logical", "paste", "paste0", "sprintf", "file.path", "setNames",
          "structure", "seq", "seq_len", "seq_along", "rep", "names", "unlist", "-", "+", "*", "/", "^", ":", "(", "{", "[",
          "[[", "$", "!", "&&", "||", "&", "|", "==", "!=", "<", ">", "<=", ">=", "%in%", "tribble", "tibble", "data.frame",
          "local", "commandArgs", "sub", "gsub", "grepl", "length", "nzchar", "exists", "getOption", "Sys.getenv",
          "normalizePath", "basename", "dirname", "tolower", "toupper", "as.character", "as.numeric", "as.integer",
          "is.null", "isTRUE", "identical", "sys.nframe", "interactive", "environment", "globalenv", "new.env", "function",
          "quote", "match.arg", "regmatches", "regexpr", "max", "min", "sum", "<-", "=", "if")
LOADERS <- c("library", "require", "requireNamespace", "suppressPackageStartupMessages", "suppressMessages", "suppressWarnings")
SOURCERS <- c("source", "sys.source", "source_mmm_helper")
EXECUTORS <- c("system", "system2", "shell", "rscript")   # rscript: callr::rscript

fname <- function(call) { h <- call[[1]]; if (is.name(h)) as.character(h) else if (is.call(h) && as.character(h[[1]]) %in% c("::", ":::")) as.character(h[[3]]) else "" }
# Arguments of a call without the empty ones (as in x[, 1]), which cannot be bound to a variable.
args_of <- function(e) {
  a <- as.list(e)[-1]
  if (!length(a)) return(a)
  a[!vapply(seq_along(a), function(i) is.name(a[[i]]) && identical(as.character(a[[i]]), ""), logical(1))]
}

# Calls evaluated when the expression runs (the bodies of function definitions are not run).
top_calls <- function(e) {
  if (!is.call(e)) return(character(0))
  f <- fname(e)
  if (identical(f, "function")) return("function")
  c(f, unlist(lapply(args_of(e), top_calls)))
}
# Literal path of a source()/sys.source() target: a string, or file.path() of strings after an optional root symbol.
literal_path <- function(arg) {
  if (is.character(arg) && length(arg) == 1L) return(arg)
  if (is.call(arg) && fname(arg) == "file.path") {
    parts <- args_of(arg)
    if (length(parts) && is.name(parts[[1]])) parts <- parts[-1]
    if (length(parts) && all(vapply(parts, function(p) is.character(p) && length(p) == 1L, logical(1))))
      return(do.call(file.path, parts))
  }
  NA_character_
}
walk <- function(e, visit) { if (is.call(e)) { visit(e); for (a in args_of(e)) walk(a, visit) }; invisible(NULL) }
literals <- function(e) if (is.character(e)) e else if (is.call(e)) unlist(lapply(args_of(e), literals)) else character(0)
sourced_targets <- function(file) {
  out <- character(0)
  for (e in as.list(parse(file, keep.source = FALSE))) walk(e, function(cl) {
    if (fname(cl) %in% c("source", "sys.source") && length(cl) >= 2L) {
      p <- literal_path(cl[[2]])
      if (!is.na(p)) out <<- c(out, p)
    }
  })
  unique(out)
}

#' Problems in the top level of one script (character(0) when it only defines things).
script_problems <- function(file) {
  ex <- as.list(parse(file, keep.source = FALSE))
  defined <- character(0); main_flags <- character(0); guards <- 0L; bad <- character(0)
  for (e in ex) if (is.call(e) && fname(e) %in% c("<-", "=") && is.name(e[[2]])) {
    rhs <- e[[3]]
    if (is.call(rhs) && fname(rhs) == "function") defined <- c(defined, as.character(e[[2]]))
    if (is.call(rhs) && fname(rhs) == "local" && "commandArgs" %in% top_calls(rhs)) main_flags <- c(main_flags, as.character(e[[2]]))
  }
  pure_ok <- function(x) { calls <- top_calls(x); !length(setdiff(calls, PURE)) && !any(calls %in% defined) }
  allowed <- function(e) {
    f <- if (is.call(e)) fname(e) else ""
    if (!is.call(e)) return(TRUE)
    if (f %in% c("<-", "=")) return(is.call(e[[3]]) && fname(e[[3]]) == "function" || pure_ok(e[[3]]))
    if (f %in% SOURCERS) return(TRUE)
    if (f %in% LOADERS) return(all(top_calls(e) %in% c(LOADERS, PURE)))
    if (f == "for") return(all(top_calls(e[[4]]) %in% c(SOURCERS, "{")))
    if (f == "if") return(pure_ok(e[[2]]) && all(vapply(as.list(e)[-(1:2)], function(b)
      if (is.call(b) && fname(b) == "{") all(vapply(as.list(b)[-1], allowed, logical(1))) else allowed(b), logical(1))))
    FALSE
  }
  for (e in ex) {
    if (is.call(e) && fname(e) == "if") {
      cond <- paste(deparse(e[[2]]), collapse = " ")
      if (grepl("sys\\.nframe\\(\\)", cond) || (is.name(e[[2]]) && as.character(e[[2]]) %in% main_flags)) {
        guards <- guards + 1L; next
      }
    }
    if (!allowed(e)) bad <- c(bad, paste0("top-level code runs when sourced: ", substr(paste(deparse(e), collapse = " "), 1, 90)))
  }
  if (guards > 1L) bad <- c(bad, sprintf("%d main guards (at most one)", guards))
  bad
}

# ------------------------------------------------------------------ 1. the checker itself
cat("1. the checker flags unguarded work\n")
tmp <- tempfile(fileext = ".R")
writeLines(c('source("Analysis/_pipeline_setup.R")', 'for (h in c("a.R", "b.R")) source_mmm_helper(h)',
             'K <- c(a = 1, b = 2)', 'run <- function() read.csv("x.csv")', 'if (sys.nframe() == 0L) {', '  run()', '}'), tmp)
check(!length(script_problems(tmp)), "1: definitions, helper sourcing and one main guard pass")
writeLines(c('run <- function() 1', 'x <- read.csv("S:/live.csv")'), tmp)
check(length(script_problems(tmp)) == 1L, "1: a top-level data read is flagged")
writeLines(c('run <- function() 1', 'res <- run()'), tmp)
check(length(script_problems(tmp)) == 1L, "1: calling a function defined in the script is flagged")
writeLines(c('run <- function() 1', 'run()'), tmp)
check(length(script_problems(tmp)) == 1L, "1: a bare top-level call is flagged")
writeLines(c('if (sys.nframe() == 0L) 1', 'if (sys.nframe() == 0L) 2'), tmp)
check(length(script_problems(tmp)) == 1L, "1: two main guards are flagged")
writeLines(c('is_main <- local({ a <- commandArgs(trailingOnly = FALSE); length(a) > 0 && grepl("x[.]R$", a[[1]]) })',
             'if (!exists("helper")) helper <- function() 1', 'if (is_main) { helper() }'), tmp)
check(!length(script_problems(tmp)), "1: a commandArgs() main flag and a conditional definition pass")
unlink(tmp)
ok("the checker passes guarded scripts and flags top-level work")

# ------------------------------------------------------------------ 2. scripts sourced by the portable tests
cat("\n2. scripts sourced by the portable tests\n")
tests <- list.files("Testing/tests", pattern = "^test_.*[.]R$", full.names = TRUE)
queue <- unique(unlist(lapply(tests, sourced_targets)))
queue <- setdiff(queue[grepl(WORK_DIRS, queue)], SETUP)
seen <- character(0)
while (length(queue)) {
  f <- queue[1]; queue <- queue[-1]
  if (f %in% seen) next
  check(file.exists(f), paste0("2: a test sources a missing script: ", f))
  seen <- c(seen, f)
  more <- setdiff(sourced_targets(f), c(seen, SETUP))
  queue <- c(queue, more[grepl(WORK_DIRS, more)])
}
check(length(seen) >= 6L, sprintf("2: expected the CC4 and identity scripts among the sourced targets (found %d)", length(seen)))
for (f in seen) {
  p <- script_problems(f)
  check(!length(p), paste0("2: ", f, " is sourced by a test but does work when sourced:\n  ", paste(p, collapse = "\n  ")))
}
ok(sprintf("%d sourced scripts hold only definitions, helper sourcing and one main guard: %s", length(seen),
           paste(basename(seen), collapse = ", ")))

# ------------------------------------------------------------------ 3. no test executes an Analysis/ script
cat("\n3. no test executes an Analysis/ script\n")
executed_analysis <- function(exprs) {
  hits <- character(0)
  for (e in exprs) walk(e, function(cl) {
    if (fname(cl) %in% EXECUTORS) hits <<- c(hits, grep("(^|/)Analysis/[^/]+[.][Rr]$", literals(cl), value = TRUE))
  })
  hits
}
check(identical(executed_analysis(as.list(parse(text = 'x <- system2("Rscript", c("Analysis/14_x.R", "--real"))'))), "Analysis/14_x.R") &&
        !length(executed_analysis(as.list(parse(text = 'system2("Rscript", "Maintenance/Get-Plan.R")')))),
      "3: the check finds an Analysis/ script run through Rscript and ignores others")
for (t in tests) {
  h <- executed_analysis(as.list(parse(t, keep.source = FALSE)))
  check(!length(h), paste0("3: ", t, " executes an Analysis/ script: ", paste(h, collapse = ", ")))
}
ok(sprintf("%d portable tests parsed; none runs an Analysis/ script", length(tests)))

cat("\nPASS: sourced scripts are guarded\n")
