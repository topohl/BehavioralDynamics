# Preprocessing function ownership.
#
# Functions/animalpos_preprocessing_helpers.R declares
#
#     preprocess_animalpos_file <- function(..., remove_phases_fn = remove_phases, ...)
#
# and remove_phases() is defined ONLY in Functions/E9_SIS_AnimalPos-functions.R.
# Because that default argument is evaluated LAZILY, dropping the source() of the
# legacy file would fail only at call time during a real preprocessing run --
# never during an analysis stage, and never in any other test.
#
# This test makes that dependency explicit and load-bearing, so the coupling
# cannot be broken silently. It intentionally does NOT assert which file owns the
# function: it asserts that exactly one definition exists and that the declared
# default resolves. If the two functions are later migrated into the modern
# helper (see docs/ARCHIVE_DEPENDENCY_AUDIT.csv) this test keeps passing.
#
# Runs entirely in memory. No S: drive access required.

source("Analysis/_pipeline_setup.R")

check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)

LEGACY <- "Functions/E9_SIS_AnimalPos-functions.R"
MODERN <- "Functions/animalpos_preprocessing_helpers.R"
CONSUMER <- "Formatting/E9_SIS_AnimalPos-preprocessing_parallell.r"

for (f in c(LEGACY, MODERN, CONSUMER)) {
  check(file.exists(f), paste0("required file is missing: ", f))
}

# ------------------------------------------------------------------
# 1. The lazily-defaulted argument is still declared as documented.
# ------------------------------------------------------------------
modern_src <- paste(readLines(MODERN, warn = FALSE), collapse = "\n")
check(grepl("remove_phases_fn = remove_phases", modern_src, fixed = TRUE),
      "1: preprocess_animalpos_file must still default remove_phases_fn to remove_phases")

# ------------------------------------------------------------------
# 2. The load-bearing functions are defined exactly once across Functions/.
# ------------------------------------------------------------------
fn_files <- list.files("Functions", pattern = "[.][Rr]$", full.names = TRUE)
count_defs <- function(name) {
  pat <- paste0("^\\s*", name, "\\s*<-\\s*function")
  sum(vapply(fn_files, function(f) sum(grepl(pat, readLines(f, warn = FALSE))), integer(1)))
}
for (nm in c("remove_phases", "count_half_hours_elapsed")) {
  n <- count_defs(nm)
  check(n == 1L, paste0("2: ", nm, "() must be defined exactly once under Functions/, found ", n))
}

# ------------------------------------------------------------------
# 3. Sourcing the two helpers together resolves the default at CALL time.
#    This is the check that a lazy default would otherwise hide.
# ------------------------------------------------------------------
env <- new.env(parent = globalenv())
suppressWarnings(suppressMessages({
  sys.source(LEGACY, envir = env)
  sys.source(MODERN, envir = env)
}))
check(is.function(get0("remove_phases", envir = env)),
      "3: remove_phases() must be available after sourcing the preprocessing helpers")
check(is.function(get0("count_half_hours_elapsed", envir = env)),
      "3: count_half_hours_elapsed() must be available after sourcing the preprocessing helpers")

pf <- get0("preprocess_animalpos_file", envir = env)
check(is.function(pf), "3: preprocess_animalpos_file() must be defined")
dflt <- formals(pf)$remove_phases_fn
check(!is.null(dflt), "3: preprocess_animalpos_file must declare remove_phases_fn")
resolved <- tryCatch(eval(dflt, envir = env), error = function(e) e)
check(is.function(resolved),
      "3: the remove_phases_fn default must RESOLVE to a function in the sourced environment")

# ------------------------------------------------------------------
# 4. The active preprocessing entry point still asserts both helper files.
# ------------------------------------------------------------------
cons_src <- paste(readLines(CONSUMER, warn = FALSE), collapse = "\n")
check(grepl("E9_SIS_AnimalPos-functions.R", cons_src, fixed = TRUE),
      "4: the preprocessing entry point must still source the legacy function file")
check(grepl("animalpos_preprocessing_helpers.R", cons_src, fixed = TRUE),
      "4: the preprocessing entry point must still source the modern helper")
check(grepl("stopifnot(all(file.exists(function_files)))", cons_src, fixed = TRUE),
      "4: the preprocessing entry point must fail closed if a helper file is absent")

cat("PASS: preprocessing function ownership (lazy remove_phases default resolves; single definition)\n")
