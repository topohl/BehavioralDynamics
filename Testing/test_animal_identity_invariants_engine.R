# Portable regression tests for the cross-scale animal identity invariant
# engine (Functions/animal_identity_invariants_helpers.R). Pure in-memory
# tibbles only; no S: drive access required.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

source("Analysis/_pipeline_setup.R")
source("Functions/animal_identity_invariants_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

make_scale <- function(animal_nums, groups, sexes) {
  tibble(AnimalNum = animal_nums, Group = groups, Sex = sexes)
}

# ------------------------------------------------------------------
# Test 1: a fully clean, consistent multi-scale roster passes every check.
# ------------------------------------------------------------------
clean_10min <- make_scale(c("3", "4", "303"), c("RES", "SUS", "CON"), c("Male", "Male", "Female"))
clean_5min  <- make_scale(c("3", "4", "303"), c("RES", "SUS", "CON"), c("Male", "Male", "Female"))
result1 <- validate_cross_scale_animal_identity(
  scale_data = list(`10min` = clean_10min, `5min` = clean_5min)
)
check(isTRUE(result1$all_checks_passed), "Test 1: a clean, identical roster across scales must pass all checks")
check(all(result1$format_checks$passed), "Test 1: canonical format checks should pass for already-canonical IDs")
check(nrow(result1$cross_scale_group_sex_conflicts) == 0L, "Test 1: no cross-scale Group/Sex conflicts expected")

# ------------------------------------------------------------------
# Test 2: a non-canonical AnimalNum ("04") must be flagged, whether or not
# its canonical form ("4") also appears in the same scale.
# ------------------------------------------------------------------
bad_format <- make_scale(c("04", "303"), c("SUS", "CON"), c("Male", "Female"))
result2 <- validate_cross_scale_animal_identity(scale_data = list(`10min` = bad_format))
check(!isTRUE(result2$all_checks_passed), "Test 2: a non-canonical AnimalNum must fail overall validation")
check(result2$format_checks$n_violations[result2$format_checks$scale == "10min"] == 1L, "Test 2: expected exactly one canonical-format violation")
check(grepl("04", result2$format_checks$offending_values[result2$format_checks$scale == "10min"]), "Test 2: the offending value '04' should be named")

# ------------------------------------------------------------------
# Test 3: exactly-one-Group/Sex-per-AnimalNum violation within a single scale.
# ------------------------------------------------------------------
conflicted_within_scale <- tibble(
  AnimalNum = c("4", "4", "303"),
  Group = c("SUS", "CON", "RES"),  # animal 4 has two different Groups within the SAME scale
  Sex = c("Male", "Male", "Female")
)
result3 <- validate_cross_scale_animal_identity(scale_data = list(`10min` = conflicted_within_scale))
check(!isTRUE(result3$all_checks_passed), "Test 3: an in-scale Group conflict must fail overall validation")
check(result3$group_sex_checks_within_scale$n_violations[1] == 1L, "Test 3: expected exactly one within-scale Group/Sex conflict")

# ------------------------------------------------------------------
# Test 4: cross-scale roster mismatch (an animal present at one scale but
# missing at another, e.g. an incomplete rebuild) must be detected.
# ------------------------------------------------------------------
scale_a <- make_scale(c("3", "4", "303"), c("RES", "SUS", "CON"), c("Male", "Male", "Female"))
scale_b <- make_scale(c("3", "4"), c("RES", "SUS"), c("Male", "Male"))  # missing animal 303
result4 <- validate_cross_scale_animal_identity(scale_data = list(a = scale_a, b = scale_b), reference_scale = "a")
check(!isTRUE(result4$all_checks_passed), "Test 4: a cross-scale roster mismatch must fail overall validation")
mismatch_row <- result4$roster_comparison_across_scales[result4$roster_comparison_across_scales$scale == "b", ]
check(!isTRUE(mismatch_row$roster_matches_reference), "Test 4: scale 'b' should be flagged as not matching the reference roster")
check(grepl("303", mismatch_row$missing_vs_reference), "Test 4: animal 303 should be listed as missing from scale 'b'")

# ------------------------------------------------------------------
# Test 5: cross-scale Group/Sex disagreement (same AnimalNum, different
# Group at a different scale) must be detected even when both scales'
# internal within-scale checks pass individually.
# ------------------------------------------------------------------
scale_c <- make_scale(c("3", "4"), c("RES", "SUS"), c("Male", "Male"))
scale_d <- make_scale(c("3", "4"), c("RES", "CON"), c("Male", "Male"))  # animal 4: SUS vs CON across scales
result5 <- validate_cross_scale_animal_identity(scale_data = list(c = scale_c, d = scale_d), reference_scale = "c")
check(!isTRUE(result5$all_checks_passed), "Test 5: a cross-scale Group disagreement must fail overall validation")
check(nrow(result5$cross_scale_group_sex_conflicts) == 1L, "Test 5: expected exactly one cross-scale Group/Sex conflict")
check(identical(result5$cross_scale_group_sex_conflicts$AnimalNum, "4"), "Test 5: the conflict must be attributed to animal 4")

# ------------------------------------------------------------------
# Test 6: downstream (Stage 03 / Stage 09) inheritance -- an unknown animal
# or a Group/Sex disagreement relative to Stage 01 must be detected.
# ------------------------------------------------------------------
stage01_scale <- make_scale(c("3", "4", "303"), c("RES", "SUS", "CON"), c("Male", "Male", "Female"))
stage09_ok <- tibble(AnimalNum = c("3", "4"), Group = c("RES", "SUS"), Sex = c("Male", "Male"))
stage09_bad <- tibble(AnimalNum = c("3", "999"), Group = c("SUS", "CON"), Sex = c("Male", "Female"))  # animal 3 Group disagrees; 999 unknown

result6_ok <- validate_cross_scale_animal_identity(scale_data = list(`10min` = stage01_scale), downstream = list(stage09 = stage09_ok))
check(isTRUE(result6_ok$all_checks_passed), "Test 6: a downstream table consistent with Stage 01 identity should pass")

result6_bad <- validate_cross_scale_animal_identity(scale_data = list(`10min` = stage01_scale), downstream = list(stage09 = stage09_bad))
check(!isTRUE(result6_bad$all_checks_passed), "Test 6: an inconsistent downstream table must fail overall validation")
bad_row <- result6_bad$downstream_inheritance_checks[result6_bad$downstream_inheritance_checks$downstream == "stage09", ]
check(bad_row$n_unknown_animals == 1L, "Test 6: animal 999 should be flagged as unknown to Stage 01")
check(bad_row$n_group_sex_disagreements == 1L, "Test 6: animal 3's Group disagreement should be flagged")

# ------------------------------------------------------------------
# Test 7: expected-vs-observed roster counts are reported, never silently
# enforced as a hardcoded universal invariant (e.g. no baked-in 111).
# ------------------------------------------------------------------
result7 <- validate_cross_scale_animal_identity(
  scale_data = list(`10min` = clean_10min),
  expected_roster = c("3", "4", "303", "9999")  # one expected animal not observed
)
check(result7$roster_count_summary$n_observed_animals == 3L, "Test 7: observed animal count should reflect the actual data, not a hardcoded constant")
check(result7$roster_count_summary$n_expected_not_observed == 1L, "Test 7: exactly one expected-but-not-observed animal should be reported")
check(isTRUE(result7$all_checks_passed), "Test 7: an expected/observed roster mismatch alone must not fail validation (it is reported, not enforced)")

result7b <- validate_cross_scale_animal_identity(scale_data = list(`10min` = clean_10min))
check(is.na(result7b$roster_count_summary$n_expected_animals), "Test 7b: with no expected_roster supplied, expected-count fields must be NA, not a default guess")

# ------------------------------------------------------------------
# Test 8: expected phenotype must come from RAW preprocessed input + SUS/CON
# reference files, never from an already-derived scale's Group column. A
# zero-padded SUS animal mislabeled RES in one derived scale must be caught
# even though that scale is internally consistent (one Group per AnimalNum)
# and even if every OTHER scale happens to agree with it (i.e. no cross-scale
# disagreement exists to catch this any other way).
# ------------------------------------------------------------------
source("Analysis/_pipeline_setup.R")

sus_ids_8 <- canonical_animal_id(c("0004", "0303"))   # canonical: "4", "303"
con_ids_8 <- canonical_animal_id(c("0500"))            # canonical: "500"

# Raw preprocessed AnimalID values as they would appear in *_preprocessed.csv
# BEFORE any Group assignment -- this is the only thing the expected
# phenotype may be derived from.
preprocessed_ids_8 <- c("0004", "0004", "0303", "0500", "0700")  # "700" is an unlisted/RES animal

expected_phenotype_8 <- derive_expected_phenotype_from_preprocessed(preprocessed_ids_8, sus_ids_8, con_ids_8)
check(identical(sort(expected_phenotype_8$AnimalNum), sort(c("4", "303", "500", "700"))), "Test 8: expected roster must be the canonicalized preprocessed animal set")
check(expected_phenotype_8$ExpectedGroup[expected_phenotype_8$AnimalNum == "4"] == "SUS", "Test 8: animal 4 must be expected SUS per the reference file")
check(expected_phenotype_8$ExpectedGroup[expected_phenotype_8$AnimalNum == "700"] == "RES", "Test 8: unlisted animal 700 must be expected RES under the documented SIS contract")

# A derived scale where the zero-padded SUS animal ("0004" -> canonical "4")
# is internally-consistently, but WRONGLY, labeled RES throughout.
mislabeled_scale <- tibble(
  AnimalNum = c("4", "303", "500", "700"),
  Group = c("RES", "SUS", "CON", "RES"),  # animal 4 should be SUS, not RES
  Sex = c("Male", "Male", "Female", "Male")
)
result8_bad <- validate_cross_scale_animal_identity(
  scale_data = list(`10min` = mislabeled_scale),
  expected_phenotype = expected_phenotype_8
)
check(!isTRUE(result8_bad$all_checks_passed), "Test 8: a scale mislabeling animal 4 as RES must fail overall validation")
mismatch_row_8 <- result8_bad$expected_phenotype_checks[result8_bad$expected_phenotype_checks$scale == "10min", ]
check(mismatch_row_8$n_violations == 1L, "Test 8: expected exactly one expected-phenotype mismatch")
check(grepl("^4\\(observed=RES,expected=SUS\\)$", mismatch_row_8$offending_values), "Test 8: the mismatch must name animal 4, observed RES, expected SUS")
check(identical(sort(result8_bad$roster_count_summary$expected_roster_source), "preprocessed_input_plus_reference_files"),
      "Test 8: roster_count_summary must record that the expected roster came from preprocessed input, not a derived scale")

# The same scale but correctly labeled must pass the expected-phenotype check.
correct_scale <- mislabeled_scale %>% dplyr::mutate(Group = c("SUS", "SUS", "CON", "RES"))
result8_ok <- validate_cross_scale_animal_identity(
  scale_data = list(`10min` = correct_scale),
  expected_phenotype = expected_phenotype_8
)
check(isTRUE(result8_ok$all_checks_passed), "Test 8: a correctly-labeled scale must pass the expected-phenotype check")

cat("Cross-scale animal identity invariant engine contract checks: PASS\n")
