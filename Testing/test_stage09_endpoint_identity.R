# Portable regression tests for the Stage 09 endpoint-identity contract.
#
# Guards the regression where Stage 09 canonicalized the behavioral metrics
# side of the CombZ join (Stage 01 writes canonical AnimalNum) but NOT the
# endpoint side. Endpoint sources such as E9_Behavior_Data.xlsx store
# zero-padded spellings ("0004", "00303", ...), so those animals silently
# failed to join and were dropped from every primary model (n 111 -> 88).
#
# Runs entirely in memory / tempdir(); no S: drive access required.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

source("Analysis/_pipeline_setup.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

# Mirrors Analysis/09_early_prediction_model_ladder.R exactly. Kept in sync by
# the source-parity assertions at the bottom of this file.
collapse_endpoint_by_canonical_animal <- function(dat, animal_col, value_col, source_label) {
  tidy <- dat %>%
    transmute(
      AnimalNum_raw = as.character(.data[[animal_col]]),
      AnimalNum = canonical_animal_id(.data[[animal_col]]),
      outcome = suppressWarnings(as.numeric(.data[[value_col]]))
    ) %>%
    filter(!is.na(AnimalNum))

  conflicts <- tidy %>%
    filter(is.finite(outcome)) %>%
    group_by(AnimalNum) %>%
    summarise(
      n_distinct_outcome = n_distinct(outcome),
      raw_ids = paste(sort(unique(AnimalNum_raw)), collapse = " | "),
      outcome_values = paste(format(sort(unique(outcome))), collapse = " | "),
      .groups = "drop"
    ) %>%
    filter(n_distinct_outcome > 1L)

  if (nrow(conflicts) > 0L) {
    stop(
      "Endpoint aliases collapsing onto one canonical AnimalNum carry conflicting finite ",
      value_col, " values (", source_label, "). Resolve the source data before rerunning:\n",
      paste(utils::capture.output(print(as.data.frame(conflicts))), collapse = "\n"),
      call. = FALSE
    )
  }

  tidy %>%
    group_by(AnimalNum) %>%
    summarise(outcome = first(na.omit(outcome)), .groups = "drop")
}

# ------------------------------------------------------------------
# Test 1: zero-padded endpoint IDs join to canonical metrics IDs.
# This is the exact regression: metrics say "4", endpoint says "0004".
# ------------------------------------------------------------------
endpoint <- tibble(
  ID = c("0004", "00303", "OQ754", "13856"),
  CombZ = c(-1.5, 0.25, 0.8, -0.4)
)
metrics_animals <- c("4", "303", "OQ754", "13856")

collapsed <- collapse_endpoint_by_canonical_animal(endpoint, "ID", "CombZ", "test")
check(setequal(collapsed$AnimalNum, c("4", "303", "OQ754", "13856")),
      "Test 1: canonicalized endpoint IDs must be 4, 303, OQ754, 13856")
matched <- intersect(metrics_animals, collapsed$AnimalNum[is.finite(collapsed$outcome)])
check(length(matched) == 4L, paste0("Test 1: expected all 4 animals to join, got ", length(matched)))
check(collapsed$outcome[collapsed$AnimalNum == "4"] == -1.5, "Test 1: animal 4's CombZ must survive canonicalization")
check(collapsed$outcome[collapsed$AnimalNum == "303"] == 0.25, "Test 1: animal 303's CombZ must survive canonicalization")

# The un-canonicalized behavior that caused the bug must NOT reproduce.
naive <- endpoint %>% transmute(AnimalNum = ID, outcome = CombZ)
naive_matched <- intersect(metrics_animals, naive$AnimalNum)
check(length(naive_matched) == 2L,
      "Test 1 (control): the old un-canonicalized join should match only the 2 already-canonical IDs")
check(length(matched) > length(naive_matched),
      "Test 1: canonicalizing endpoint IDs must strictly increase coverage over the buggy behavior")

# ------------------------------------------------------------------
# Test 2: alphanumeric IDs are NOT aliased together (OR004 != OR4 != 004).
# ------------------------------------------------------------------
alnum <- tibble(ID = c("OR004", "OR4", "004"), CombZ = c(1, 2, 3))
alnum_collapsed <- collapse_endpoint_by_canonical_animal(alnum, "ID", "CombZ", "test")
check(nrow(alnum_collapsed) == 3L,
      "Test 2: OR004 / OR4 / 004 must remain three distinct canonical animals")

# ------------------------------------------------------------------
# Test 3: FAIL CLOSED when aliases of one canonical animal disagree.
# "0004" and "4" are the same animal, so two different finite CombZ values
# is a genuine data conflict and must not be silently resolved.
# ------------------------------------------------------------------
conflicting <- tibble(ID = c("0004", "4"), CombZ = c(-1.5, 0.9))
err <- tryCatch({
  collapse_endpoint_by_canonical_animal(conflicting, "ID", "CombZ", "test")
  NULL
}, error = function(e) e)
check(!is.null(err), "Test 3: conflicting finite CombZ across aliases must raise an error")
check(grepl("conflicting finite", conditionMessage(err)),
      "Test 3: the error must name the conflict explicitly")
check(grepl("0004", conditionMessage(err)) && grepl("\\b4\\b", conditionMessage(err)),
      "Test 3: the error must report the offending raw alias spellings")

# A silent first(na.omit(...)) would have returned one arbitrary value instead.
check(!identical(
        tryCatch(collapse_endpoint_by_canonical_animal(conflicting, "ID", "CombZ", "t")$outcome,
                 error = function(e) "errored"),
        c(-1.5)),
      "Test 3: must not silently pick the first alias value")

# ------------------------------------------------------------------
# Test 4: agreeing aliases collapse cleanly (duplicate rows are fine).
# ------------------------------------------------------------------
agreeing <- tibble(ID = c("0004", "4", "00303"), CombZ = c(-1.5, -1.5, 0.25))
agreed <- collapse_endpoint_by_canonical_animal(agreeing, "ID", "CombZ", "test")
check(nrow(agreed) == 2L, "Test 4: agreeing aliases must collapse to one row per canonical animal")
check(agreed$outcome[agreed$AnimalNum == "4"] == -1.5, "Test 4: the agreed value must be preserved")

# NA on one alias and a finite value on another is not a conflict.
partial <- tibble(ID = c("0004", "4"), CombZ = c(NA_real_, -1.5))
partial_ok <- collapse_endpoint_by_canonical_animal(partial, "ID", "CombZ", "test")
check(nrow(partial_ok) == 1L && partial_ok$outcome == -1.5,
      "Test 4: a missing value on one alias must not block collapsing to the finite one")

# ------------------------------------------------------------------
# Test 5: missing/blank endpoint IDs are dropped, not turned into an NA animal.
# ------------------------------------------------------------------
with_missing <- tibble(ID = c("0004", NA, "", "   "), CombZ = c(-1.5, 1, 2, 3))
dropped <- collapse_endpoint_by_canonical_animal(with_missing, "ID", "CombZ", "test")
check(nrow(dropped) == 1L && dropped$AnimalNum == "4",
      "Test 5: missing/blank endpoint IDs must be dropped entirely")

# ------------------------------------------------------------------
# Test 6: source parity -- Stage 09 must actually canonicalize the endpoint
# ID in BOTH endpoint-loading branches, and must use the fail-closed helper.
# ------------------------------------------------------------------
stage09_src <- paste(readLines("Analysis/09_early_prediction_model_ladder.R", warn = FALSE), collapse = "\n")

check(grepl("canonical_animal_id", stage09_src, fixed = TRUE),
      "Test 6: Stage 09 must reference canonical_animal_id()")
n_helper_defs <- lengths(regmatches(stage09_src, gregexpr("collapse_endpoint_by_canonical_animal\\s*<-\\s*function", stage09_src)))
check(n_helper_defs == 1,
      paste0("Test 6: expected exactly one definition of the endpoint helper, found ", n_helper_defs))
n_helper_calls <- lengths(regmatches(stage09_src, gregexpr("collapse_endpoint_by_canonical_animal\\(", stage09_src)))
check(n_helper_calls == 2,
      paste0("Test 6: expected the endpoint helper to be called in BOTH endpoint-loading branches (2 call sites), found ", n_helper_calls))
check(grepl("conflicting finite", stage09_src, fixed = TRUE),
      "Test 6: Stage 09 must fail closed on conflicting finite endpoint values")
check(!grepl("transmute(AnimalNum = .data[[endpoint_animal_col]]", stage09_src, fixed = TRUE),
      "Test 6: the un-canonicalized endpoint transmute must no longer be present")
check(grepl("endpoint_coverage_by_animal", stage09_src, fixed = TRUE),
      "Test 6: Stage 09 must emit an endpoint coverage audit")

cat("Stage 09 endpoint identity contract checks: PASS\n")
