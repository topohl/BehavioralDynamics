# Portable contract check: Stage 15 must not consume the unverified historical
# five-minute phase-analysis tables in a new proteomics integration run.
source_file <- "Analysis/15_behavior_proteomics_integration.R"
source_text <- readLines(source_file, warn = FALSE)
stopifnot(any(grepl("assert_verified_phase_source(path, source_label, scale_label)",
                  source_text, fixed = TRUE)))

exprs <- parse(file = source_file)
guard <- Filter(function(expr) is.call(expr) && identical(expr[[1]], as.name("<-")) &&
                  identical(expr[[2]], as.name("assert_verified_phase_source")),
                as.list(exprs))
stopifnot(length(guard) == 1L)
env <- new.env(parent = baseenv())
eval(guard[[1]], envir = env)

probe_dir <- file.path(tempdir(), "stage15_phase_source_guard")
dir.create(probe_dir, recursive = TRUE, showWarnings = FALSE)
existing <- file.path(probe_dir, "phase_features.csv")
writeLines("AnimalNum,value\n1,2", existing)

for (source_label in c("adaptation_kinetics", "sleep_like_inactivity",
                       "phase_organization")) {
  error <- tryCatch({
    env$assert_verified_phase_source(existing, source_label, "5min_based")
    NULL
  }, error = conditionMessage)
  stopifnot(is.character(error),
            grepl("UNVERIFIED HISTORICAL PHASE INPUT", error, fixed = TRUE),
            grepl(existing, error, fixed = TRUE))
}

# Declared-resolution files and unrelated five-minute sources are unaffected.
stopifnot(identical(env$assert_verified_phase_source(existing,
                  "phase_organization", "10min_based"), existing))
stopifnot(identical(env$assert_verified_phase_source(existing,
                  "social_networks", "5min_based"), existing))
missing <- file.path(probe_dir, "not_present.csv")
stopifnot(identical(env$assert_verified_phase_source(missing,
                  "phase_organization", "5min_based"), missing))

# Exercise the actual loader entry point and verify rejection happens before
# the table reader touches the historical file.
loader <- Filter(function(expr) is.call(expr) && identical(expr[[1]], as.name("<-")) &&
                   identical(expr[[2]], as.name("load_curated_behavior_table")),
                 as.list(exprs))
stopifnot(length(loader) == 1L)
eval(loader[[1]], envir = env)
env$read_called <- FALSE
env$read_optional_table <- function(path) {
  env$read_called <- TRUE
  stop("table reader reached")
}
blocked <- tryCatch({
  env$load_curated_behavior_table(existing, "phase_organization", "phase_organization",
                                  "5min_based", "phase_contrast_strength")
  NULL
}, error = conditionMessage)
stopifnot(grepl("UNVERIFIED HISTORICAL PHASE INPUT", blocked, fixed = TRUE),
          !env$read_called)
allowed <- tryCatch({
  env$load_curated_behavior_table(existing, "phase_organization", "phase_organization",
                                  "10min_based", "phase_contrast_strength")
  NULL
}, error = conditionMessage)
stopifnot(identical(allowed, "table reader reached"), env$read_called)

cat("Stage 15 historical phase-source guard: PASS\n")
