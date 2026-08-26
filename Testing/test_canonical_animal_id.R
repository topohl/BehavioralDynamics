# Focused unit tests for the vectorized canonical_animal_id() contract
# (Functions/behavioral_dynamics_helpers.R). Portable: pure in-memory vectors
# and tibbles only, no S: drive access required.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

source("Analysis/_pipeline_setup.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

# ------------------------------------------------------------------
# Test 1: exact contract examples from the task specification.
# ------------------------------------------------------------------
examples <- c("0004", "4", "00303", "0000", "OR004", "OQ754", " OR004 ", "or004", "0")
expected <- c("4",    "4", "303",   "0",    "OR004", "OQ754", "OR004",   "OR004", "0")
result <- canonical_animal_id(examples)
check(identical(result, expected), paste0("Test 1: contract mismatch.\n  got: ", paste(result, collapse=","), "\n  expected: ", paste(expected, collapse=",")))

# ------------------------------------------------------------------
# Test 2: whitespace normalization (leading/trailing/internal/tabs/newlines).
# ------------------------------------------------------------------
check(identical(canonical_animal_id("  4  "), "4"), "Test 2: surrounding whitespace must be stripped")
check(identical(canonical_animal_id("\t0004\n"), "4"), "Test 2: tabs/newlines must be stripped")
check(identical(canonical_animal_id("0004 0004"), "40004"), "Test 2: internal whitespace is removed (concatenated), matching legacy behavior")

# ------------------------------------------------------------------
# Test 3: case normalization to uppercase.
# ------------------------------------------------------------------
check(identical(canonical_animal_id("or004"), "OR004"), "Test 3: lowercase alphanumeric IDs must be uppercased")
check(identical(canonical_animal_id("oQ754"), "OQ754"), "Test 3: mixed-case alphanumeric IDs must be uppercased")

# ------------------------------------------------------------------
# Test 4: missing/empty values remain missing.
# ------------------------------------------------------------------
check(is.na(canonical_animal_id(NA_character_)), "Test 4: NA input must remain NA")
check(is.na(canonical_animal_id("")), "Test 4: empty string must become NA")
check(is.na(canonical_animal_id("   ")), "Test 4: whitespace-only string must become NA")
check(identical(canonical_animal_id(c("4", NA, "")), c("4", NA_character_, NA_character_)), "Test 4: missingness must be preserved elementwise in a vector")

# ------------------------------------------------------------------
# Test 5: no conversion through floating-point numeric representation; large
# numeric-looking RFID strings must not lose precision.
# ------------------------------------------------------------------
big_id <- "99999999999999999999123456789"
check(identical(canonical_animal_id(big_id), big_id), "Test 5: a large numeric-looking RFID string with no leading zeros must be returned unchanged")
check(identical(canonical_animal_id(paste0("00", big_id)), big_id), "Test 5: leading zeros on a large RFID string must be stripped without altering the remaining digits")
# A value that would silently lose trailing-digit precision if ever routed
# through double-precision floating point (2^53 ~= 9.007e15; this ID exceeds it).
precision_sensitive_id <- "0009007199254740993000000001"
check(
  identical(canonical_animal_id(precision_sensitive_id), sub("^0+(?=.)", "", precision_sensitive_id, perl = TRUE)),
  "Test 5: precision-sensitive numeric-looking ID must be preserved exactly, character-for-character"
)

# ------------------------------------------------------------------
# Test 6: vectorized call is equivalent to elementwise application (proves
# the vectorization did not change per-element semantics).
# ------------------------------------------------------------------
mixed <- c("0004", "OR004", NA, "", "00303", "9", "0009")
elementwise <- vapply(mixed, function(v) canonical_animal_id(v), character(1), USE.NAMES = FALSE)
vectorized <- canonical_animal_id(mixed)
check(identical(elementwise, vectorized), "Test 6: vectorized output must match elementwise application exactly")

# ------------------------------------------------------------------
# Test 7: many-to-one aliasing -- multiple raw spellings collapse onto the
# same canonical AnimalNum, as required for identity-correction joins.
# ------------------------------------------------------------------
aliases <- c("4", "04", "0004", " 4 ", "4\t")
canon <- canonical_animal_id(aliases)
check(length(unique(canon)) == 1L, "Test 7: all numeric aliases of animal 4 must canonicalize to the same value")
check(identical(unique(canon), "4"), "Test 7: the shared canonical value for animal 4's aliases must be '4'")

alphanumeric_not_aliased <- canonical_animal_id(c("OR004", "OR4", "004"))
check(length(unique(alphanumeric_not_aliased)) == 3L, "Test 7: alphanumeric IDs must NOT be treated as numeric aliases of each other or of a bare numeric ID")

# ------------------------------------------------------------------
# Test 8: canonicalization must not leave contradictory Group/Sex metadata
# for one canonical AnimalNum (the invariant Stage 01/03 enforce downstream).
# ------------------------------------------------------------------
clean_metrics <- tibble(
  AnimalNum_raw = c("4", "04", "0004", "303", "00303"),
  Group = c("SUS", "SUS", "SUS", "RES", "RES"),
  Sex = c("Male", "Male", "Male", "Female", "Female")
) %>% mutate(AnimalNum = canonical_animal_id(AnimalNum_raw))

clean_conflicts <- clean_metrics %>%
  distinct(AnimalNum, Group, Sex) %>%
  group_by(AnimalNum) %>%
  summarise(n_groups = n_distinct(Group), n_sexes = n_distinct(Sex), .groups = "drop") %>%
  filter(n_groups > 1L | n_sexes > 1L)
check(nrow(clean_conflicts) == 0L, "Test 8: consistently-labeled aliases must produce zero Group/Sex conflicts after canonicalization")

contradictory_metrics <- tibble(
  AnimalNum_raw = c("4", "04", "0004"),
  Group = c("SUS", "SUS", "CON"),   # "0004" disagrees with "4"/"04"
  Sex = c("Male", "Male", "Male")
) %>% mutate(AnimalNum = canonical_animal_id(AnimalNum_raw))

contradictory_conflicts <- contradictory_metrics %>%
  distinct(AnimalNum, Group, Sex) %>%
  group_by(AnimalNum) %>%
  summarise(n_groups = n_distinct(Group), n_sexes = n_distinct(Sex), .groups = "drop") %>%
  filter(n_groups > 1L | n_sexes > 1L)
check(nrow(contradictory_conflicts) == 1L, "Test 8: a genuine Group contradiction across aliases of the same canonical AnimalNum must be detected, not silently merged")

cat("canonical_animal_id() contract checks: PASS\n")
