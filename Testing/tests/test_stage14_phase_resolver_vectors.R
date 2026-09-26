# Stage 14 passes whole domain preference vectors to the Stage 11-13 phase
# resolver. The resolver used to accept only 5min and 10min, so Stage 14 could
# not start. Evaluate the real vectors from Stage 14 source (parsed, never run)
# and resolve them against a temporary fixture; no live path is read or written.
source("Functions/project_paths.R")

STAGE14 <- "Analysis/14_systems_neuroscience_summary_dashboard.R"
exprs <- parse(STAGE14, keep.source = FALSE)

# Only the bin-level assignments and domain_bin_preference() are evaluated.
env <- new.env(parent = globalenv())
env$safe_name <- function(x) tolower(gsub("^_|_$", "", gsub("[^A-Za-z0-9]+", "_", x)))
wanted <- c("primary_bin_level", "sensitivity_bin_levels",
            "optional_import_bin_levels", "domain_bin_preference")
assigned <- vapply(exprs, function(e) {
  if (is.call(e) && identical(e[[1L]], as.name("<-")) && is.name(e[[2L]]))
    as.character(e[[2L]]) else ""
}, character(1))
stopifnot(all(wanted %in% assigned))
for (i in which(assigned %in% wanted)) eval(exprs[[i]], env)

# Every resolution argument Stage 14 passes to the phase resolver.
calls <- list()
walk <- function(e) {
  if (!is.call(e)) return(invisible())
  if (identical(e[[1L]], as.name("mmm_phase_analysis_resolution_root")))
    calls[[length(calls) + 1L]] <<- e
  for (i in seq_along(e)[-1L]) {
    if (!identical(e[[i]], quote(expr = ))) walk(e[[i]])   # skip empty x[, j] slots
  }
}
for (e in exprs) walk(e)
stopifnot(length(calls) >= 10L)

# sleep_candidate_bin_levels is the one argument held in a variable.
sleep_i <- which(assigned == "sleep_candidate_bin_levels")
stopifnot(length(sleep_i) == 1L)
eval(exprs[[sleep_i]], env)

root <- normalizePath(file.path(tempdir(), paste0("stage14_phase_vectors_",
                                                  as.integer(runif(1L, 1L, 1e9)))),
                      winslash = "/", mustWork = FALSE)
ready <- file.path(root, "analysis_ready")
dir.create(file.path(ready, "_migration_control", "numbered_root_archive"),
           recursive = TRUE)
old_roots <- c(adaptation_kinetics = "15_behavioral_adaptation_kinetics",
               sleep_like_inactivity = "16_sleep_like_inactivity_metrics",
               phase_organization = "17_ethological_phase_organization")
for (analysis in names(old_roots)) {
  for (res in c("5min_based", "10min_based")) {
    dir.create(file.path(ready, old_roots[[analysis]], res, "tables"), recursive = TRUE)
  }
}

check_error <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
resolve_all <- function() {
  lapply(calls, function(e) {
    analysis <- eval(e[[2L]], env)
    resolution <- eval(e[[3L]], env)
    list(analysis = analysis, resolution = resolution,
         path = mmm_phase_analysis_resolution_root(analysis, resolution, root))
  })
}

# 1. Before any copy or archive, every vector resolves inside the old roots.
for (hit in resolve_all()) {
  stopifnot(length(hit$path) == length(hit$resolution),
            identical(hit$path, file.path(ready, old_roots[[hit$analysis]],
                                          hit$resolution)))
}

# 2. With the 10min copies activated and the old roots archived, only 10min
#    reads the copy; every other resolution reads the retained original, and
#    the producer guard refuses each of those as an output root.
for (analysis in names(old_roots)) {
  target <- paste0("analyses/", analysis, "/10min")
  dir.create(file.path(ready, target, "tables"), recursive = TRUE)
  jsonlite::write_json(list(
    group = paste0(analysis, "_10min"), state = "activated", files = 1L,
    target_root_rel = target, group_plan_sha256 = strrep("a", 64L),
    contract_sha256 = strrep("b", 64L), source_retained = TRUE),
    file.path(ready, "_migration_control", paste0(analysis, "_10min.json")),
    auto_unbox = TRUE)
  dir.create(file.path(ready, "history", "original_layout"), recursive = TRUE,
             showWarnings = FALSE)
  stopifnot(file.rename(file.path(ready, old_roots[[analysis]]),
                        file.path(ready, "history", "original_layout",
                                  old_roots[[analysis]])))
  jsonlite::write_json(list(
    root = old_roots[[analysis]], source_root_rel = old_roots[[analysis]],
    archive_root_rel = paste0("history/original_layout/", old_roots[[analysis]]),
    state = "activated", files = 1L, bytes = 1L, manifest_sha256 = strrep("c", 64L),
    reader_gate_kind = "ArchivePath", reader_gate_sha256 = strrep("e", 64L),
    reader_queue_sha256 = strrep("f", 64L)),
    file.path(ready, "_migration_control", "numbered_root_archive",
              paste0(old_roots[[analysis]], ".json")), auto_unbox = TRUE)
}
for (hit in resolve_all()) {
  expected <- ifelse(hit$resolution == "10min_based",
                     file.path(ready, "analyses", hit$analysis, "10min"),
                     file.path(ready, "history", "original_layout",
                               old_roots[[hit$analysis]], hit$resolution))
  stopifnot(identical(hit$path, unname(expected)))
  for (i in which(hit$resolution != "10min_based")) {
    stopifnot(check_error(mmm_behavior_guard_numbered_output_path(hit$path[[i]], root)))
  }
  stopifnot(identical(mmm_behavior_guard_numbered_output_path(
    hit$path[[which(hit$resolution == "10min_based")]], root),
    file.path(ready, "analyses", hit$analysis, "10min")))
}

# 3. An unknown resolution is still refused.
stopifnot(check_error(mmm_phase_analysis_resolution_root(
  "adaptation_kinetics", "15min_based", root)),
  check_error(mmm_phase_analysis_resolution_root(
    "adaptation_kinetics", c("10min_based", NA), root)))

# 4. The three Stage 11-13 producers and both supporting producers pass their
#    output root through the numbered-root guard.
producers <- c("Analysis/11_behavioral_adaptation_kinetics.R",
               "Analysis/12_sleep_like_quiescence_metrics.R",
               "Analysis/13_ethological_phase_organization.R",
               "Analysis/_supporting/13_nonlinear_systems_dynamics.R",
               "Analysis/_supporting/14_nextgen_behavioral_phenotyping.R")
for (script in producers) {
  text <- paste(readLines(script, warn = FALSE), collapse = "\n")
  stopifnot(grepl("output_dir <- mmm_behavior_guard_numbered_output_path(\n  mmm_",
                  text, fixed = TRUE))
}
cat("Stage 14 phase resolver vectors resolve on both layouts: PASS\n")
