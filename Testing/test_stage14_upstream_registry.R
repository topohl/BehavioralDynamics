# Portable regression tests for the Stage 14 upstream artifact registry
# (Stage 04 temporal-instability and Stage 09 early-prediction resolution).
# Runs entirely against tempdir() fixtures; no S: drive access required.

suppressPackageStartupMessages({
  library(readr)
})

source("Analysis/_pipeline_setup.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

write_stub <- function(path, content = "stub") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(content, path)
}

# ------------------------------------------------------------------
# Test 1: canonical + legacy both present -> canonical wins
# ------------------------------------------------------------------
base1 <- file.path(tempdir(), paste0("mmm_test1_", as.integer(runif(1, 1, 1e9))))
canonical_path_1 <- file.path(base1, "analysis_ready/pipeline/09_early_prediction/10min/tables/foo.csv")
legacy_path_1 <- file.path(base1, "analysis_ready/06_behavioral_dynamics/early_prediction_model_ladder/10min_based/tables/foo.csv")
write_stub(canonical_path_1, "canonical-content")
write_stub(legacy_path_1, "legacy-content")

res1 <- resolve_stage09_early_prediction_artifact(base1, "foo.csv", c("10min_based"))
check(identical(res1$resolution, "canonical"), "Test 1: expected resolution 'canonical'")
check(normalizePath(res1$path, winslash = "/") == normalizePath(canonical_path_1, winslash = "/"), "Test 1: expected canonical path selected")
check(isTRUE(res1$exists), "Test 1: expected exists=TRUE")
unlink(base1, recursive = TRUE)

# ------------------------------------------------------------------
# Test 2: only legacy present -> legacy selected, explicitly marked fallback,
# and a warning is raised
# ------------------------------------------------------------------
base2 <- file.path(tempdir(), paste0("mmm_test2_", as.integer(runif(1, 1, 1e9))))
canonical_path_2 <- file.path(base2, "analysis_ready/pipeline/09_early_prediction/10min/tables/foo.csv")
legacy_path_2 <- file.path(base2, "analysis_ready/06_behavioral_dynamics/early_prediction_model_ladder/10min_based/tables/foo.csv")
write_stub(legacy_path_2, "legacy-content")

warned <- FALSE
res2 <- withCallingHandlers(
  resolve_stage09_early_prediction_artifact(base2, "foo.csv", c("10min_based")),
  warning = function(w) { warned <<- TRUE; invokeRestart("muffleWarning") }
)
check(identical(res2$resolution, "legacy_fallback"), "Test 2: expected resolution 'legacy_fallback'")
check(normalizePath(res2$path, winslash = "/") == normalizePath(legacy_path_2, winslash = "/"), "Test 2: expected legacy path selected")
check(warned, "Test 2: expected a warning when a legacy fallback is used")
unlink(base2, recursive = TRUE)

# ------------------------------------------------------------------
# Test 3: required source absent everywhere -> hard error (fail closed)
# ------------------------------------------------------------------
base3 <- file.path(tempdir(), paste0("mmm_test3_", as.integer(runif(1, 1, 1e9))))
err3 <- tryCatch({
  resolve_stage09_early_prediction_artifact(base3, "foo.csv", c("10min_based", "5min_based"), required = TRUE)
  NULL
}, error = function(e) e)
check(!is.null(err3), "Test 3: expected an error when a required source is absent")

# Non-required absence must not error, and must report resolution == 'missing'
res3b <- resolve_stage09_early_prediction_artifact(base3, "foo.csv", c("10min_based"), required = FALSE)
check(identical(res3b$resolution, "missing"), "Test 3b: expected resolution 'missing' for non-required absent source")
check(!isTRUE(res3b$exists), "Test 3b: expected exists=FALSE for missing source")
unlink(base3, recursive = TRUE)

# ------------------------------------------------------------------
# Test 4: Stage 04 resolves temporal_instability, never the dead
# 'burstiness' assumption -- even when a decoy burstiness file exists.
# ------------------------------------------------------------------
base4 <- file.path(tempdir(), paste0("mmm_test4_", as.integer(runif(1, 1, 1e9))))
real_path_4 <- file.path(base4, "analysis_ready/06_behavioral_dynamics/temporal_instability/10sec_based/tables/bar.csv")
decoy_burstiness_path <- file.path(base4, "analysis_ready/06_behavioral_dynamics/burstiness/10sec_based/tables/bar.csv")
write_stub(real_path_4, "real-content")
write_stub(decoy_burstiness_path, "decoy-content-should-never-be-selected")

res4 <- resolve_stage04_temporal_instability_artifact(base4, "bar.csv", c("10sec_based"))
check(isTRUE(res4$exists), "Test 4: expected the real temporal_instability file to be found")
check(normalizePath(res4$path, winslash = "/") == normalizePath(real_path_4, winslash = "/"), "Test 4: expected temporal_instability path selected")
check(!grepl("burstiness", res4$path, fixed = TRUE), "Test 4: resolved path must never point at the dead burstiness/ folder")
check(!any(grepl("burstiness", res4$tried, fixed = TRUE)), "Test 4: burstiness/ must not even appear among tried candidates")
unlink(base4, recursive = TRUE)

# ------------------------------------------------------------------
# Test 5: multi-resolution preference order, canonical class only -- first
# resolution missing, second resolution's canonical file present -> found
# there, still labeled 'canonical'.
# ------------------------------------------------------------------
base5 <- file.path(tempdir(), paste0("mmm_test5_", as.integer(runif(1, 1, 1e9))))
present_path_5 <- file.path(base5, "analysis_ready/pipeline/09_early_prediction/10min/tables/foo.csv")
write_stub(present_path_5, "content")
res5 <- resolve_stage09_early_prediction_artifact(base5, "foo.csv", c("5min_based", "10min_based"))
check(identical(res5$resolution, "canonical"), "Test 5: expected resolution 'canonical' at the second preferred resolution")
check(identical(res5$resolution_bin_level, "10min_based"), "Test 5: expected resolution_bin_level '10min_based'")
unlink(base5, recursive = TRUE)

# ------------------------------------------------------------------
# Global canonical-over-legacy precedence (source class beats resolution
# preference; resolution preference only breaks ties WITHIN a class). These
# four cases are the exact scenarios that catch the bug where a
# resolution-by-resolution loop returns on the first match at each
# resolution, letting a legacy hit at a higher-preference resolution beat a
# canonical hit at a lower-preference one.
# ------------------------------------------------------------------

# Case A:
#   canonical 5min     missing
#   canonical 10min    exists
#   legacy 5min        exists
# => canonical 10min (ANY canonical beats ANY legacy, regardless of
#    resolution preference order).
baseA <- file.path(tempdir(), paste0("mmm_testA_", as.integer(runif(1, 1, 1e9))))
write_stub(file.path(baseA, "analysis_ready/pipeline/09_early_prediction/10min/tables/foo.csv"), "canonical-10min")
write_stub(file.path(baseA, "analysis_ready/06_behavioral_dynamics/early_prediction_model_ladder/5min_based/tables/foo.csv"), "legacy-5min")
resA <- resolve_stage09_early_prediction_artifact(baseA, "foo.csv", c("5min_based", "10min_based"))
check(identical(resA$resolution, "canonical"), "Case A: expected resolution 'canonical' (canonical must beat legacy across resolutions)")
check(identical(resA$resolution_bin_level, "10min_based"), "Case A: expected the canonical hit's resolution '10min_based'")
check(grepl("pipeline/09_early_prediction/10min", resA$path, fixed = TRUE), "Case A: expected the canonical 10min path to be selected")
unlink(baseA, recursive = TRUE)

# Case B:
#   canonical (all)    missing
#   legacy 5min        exists
#   legacy 10min       exists
# => legacy 5min, marked legacy_fallback (within the legacy class, resolution
#    preference order still applies).
baseB <- file.path(tempdir(), paste0("mmm_testB_", as.integer(runif(1, 1, 1e9))))
write_stub(file.path(baseB, "analysis_ready/06_behavioral_dynamics/early_prediction_model_ladder/5min_based/tables/foo.csv"), "legacy-5min")
write_stub(file.path(baseB, "analysis_ready/06_behavioral_dynamics/early_prediction_model_ladder/10min_based/tables/foo.csv"), "legacy-10min")
warnedB <- FALSE
resB <- withCallingHandlers(
  resolve_stage09_early_prediction_artifact(baseB, "foo.csv", c("5min_based", "10min_based")),
  warning = function(w) { warnedB <<- TRUE; invokeRestart("muffleWarning") }
)
check(identical(resB$resolution, "legacy_fallback"), "Case B: expected resolution 'legacy_fallback'")
check(identical(resB$resolution_bin_level, "5min_based"), "Case B: expected the higher-preference legacy resolution '5min_based' to win")
check(grepl("early_prediction_model_ladder/5min_based", resB$path, fixed = TRUE), "Case B: expected the legacy 5min path to be selected")
check(warnedB, "Case B: expected a warning when falling back to legacy")
unlink(baseB, recursive = TRUE)

# Case C:
#   canonical 5min     exists
#   canonical 10min    exists
#   legacy (any)       exists
# => canonical 5min (within the canonical class, resolution preference order
#    still applies; the presence of a legacy file is irrelevant).
baseC <- file.path(tempdir(), paste0("mmm_testC_", as.integer(runif(1, 1, 1e9))))
write_stub(file.path(baseC, "analysis_ready/pipeline/09_early_prediction/5min/tables/foo.csv"), "canonical-5min")
write_stub(file.path(baseC, "analysis_ready/pipeline/09_early_prediction/10min/tables/foo.csv"), "canonical-10min")
write_stub(file.path(baseC, "analysis_ready/06_behavioral_dynamics/early_prediction_model_ladder/5min_based/tables/foo.csv"), "legacy-5min")
resC <- resolve_stage09_early_prediction_artifact(baseC, "foo.csv", c("5min_based", "10min_based"))
check(identical(resC$resolution, "canonical"), "Case C: expected resolution 'canonical'")
check(identical(resC$resolution_bin_level, "5min_based"), "Case C: expected the higher-preference canonical resolution '5min_based' to win")
check(grepl("pipeline/09_early_prediction/5min", resC$path, fixed = TRUE), "Case C: expected the canonical 5min path to be selected")
unlink(baseC, recursive = TRUE)

# Case D: nothing exists anywhere and required=TRUE -> error (fail closed).
baseD <- file.path(tempdir(), paste0("mmm_testD_", as.integer(runif(1, 1, 1e9))))
errD <- tryCatch({
  resolve_stage09_early_prediction_artifact(baseD, "foo.csv", c("5min_based", "10min_based"), required = TRUE)
  NULL
}, error = function(e) e)
check(!is.null(errD), "Case D: expected an error when nothing exists anywhere and required=TRUE")

# ------------------------------------------------------------------
# Test 6: Stage 14's audit path equals the path actually imported.
#
# 14_systems_neuroscience_summary_dashboard.R is a top-to-bottom procedural
# dashboard script that reads real S: drive data end to end, so it cannot be
# executed here. Instead this asserts the structural property that makes the
# two paths provably identical: the Stage 04/09 artifacts are resolved
# exactly once into stage04_temporal_instability_primary /
# stage09_early_prediction_primary, and every downstream consumer -- the
# optional-feature loader (import) as well as the integration/dependency
# audits -- references that same precomputed object rather than
# re-deriving candidate paths independently.
# ------------------------------------------------------------------
stage14_path <- "Analysis/14_systems_neuroscience_summary_dashboard.R"
stage14_src <- paste(readLines(stage14_path, warn = FALSE), collapse = "\n")

check(
  lengths(regmatches(stage14_src, gregexpr("stage04_temporal_instability_primary\\s*<-\\s*resolve_stage04_temporal_instability_artifact", stage14_src))) == 1,
  "Test 6: expected exactly one computation site for the Stage 04 artifact"
)
check(
  lengths(regmatches(stage14_src, gregexpr("stage09_early_prediction_primary\\s*<-\\s*resolve_stage09_early_prediction_artifact", stage14_src))) == 1,
  "Test 5: expected exactly one computation site for the Stage 09 artifact"
)
n_stage04_uses <- lengths(regmatches(stage14_src, gregexpr("stage04_temporal_instability_primary\\$", stage14_src)))
n_stage09_uses <- lengths(regmatches(stage14_src, gregexpr("stage09_early_prediction_primary\\$", stage14_src)))
check(n_stage04_uses >= 2, "Test 5: expected the Stage 04 resolved object to be reused by both an importer and an audit")
check(n_stage09_uses >= 2, "Test 5: expected the Stage 09 resolved object to be reused by both an importer and an audit")

check(!grepl("06_behavioral_dynamics/burstiness", stage14_src, fixed = TRUE), "Test 5: no hardcoded reference to the dead burstiness/ path may remain")
check(!grepl('06_behavioral_dynamics/early_prediction"', stage14_src, fixed = TRUE), "Test 5: no hardcoded reference to the never-real early_prediction/ (without _model_ladder) path may remain")
check(!grepl("06_behavioral_dynamics/early_prediction[/,]", stage14_src), "Test 5: no hardcoded reference to the never-real early_prediction/ (without _model_ladder) path may remain")

cat("Stage 14 upstream registry contract checks: PASS\n")
