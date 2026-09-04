# ================================================================
# Cross-scale animal identity invariant checks
# MMMSociability
# ================================================================
# Pure validation engine for the post-Stage-01 identity invariants. No file
# I/O happens here; Testing/audits/validate_cross_scale_animal_identity.R does the
# reading and calls into this file, which keeps this logic testable with
# synthetic tempdir/in-memory fixtures.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tibble)
})

#' Rows whose AnimalNum is not already canonical. Because Stage 01 applies
#' canonical_animal_id() when building AnimalNum, any survivor here is either
#' a bug in that application or a raw value that leaked through downstream.
#' This single check also catches "a zero-padded alias survives as a second
#' AnimalNum": a padded value such as "04" is non-canonical on its own
#' (canonical_animal_id("04") == "4" != "04") regardless of whether "4" is
#' also present in the roster.
check_canonical_animal_num_format <- function(animal_nums, scale_label) {
  animal_nums <- animal_nums[!is.na(animal_nums)]
  canon <- canonical_animal_id(animal_nums)
  offending <- sort(unique(animal_nums[canon != animal_nums]))
  tibble(
    scale = scale_label,
    check = "canonical_animal_num_format",
    n_violations = length(offending),
    passed = length(offending) == 0L,
    offending_values = paste(offending, collapse = "; ")
  )
}

#' Exactly one Group and one Sex per AnimalNum, within a single scale's data.
check_one_group_one_sex_within_scale <- function(dat, scale_label,
                                                  animal_col = "AnimalNum", group_col = "Group", sex_col = "Sex") {
  if (!all(c(animal_col, group_col, sex_col) %in% names(dat))) {
    return(tibble(scale = scale_label, check = "one_group_one_sex_within_scale",
                  n_violations = NA_integer_, passed = NA, offending_values = "missing_required_columns"))
  }
  conflicts <- dat %>%
    transmute(AnimalNum = .data[[animal_col]], Group = .data[[group_col]], Sex = .data[[sex_col]]) %>%
    distinct() %>%
    group_by(AnimalNum) %>%
    summarise(n_groups = n_distinct(Group), n_sexes = n_distinct(Sex), .groups = "drop") %>%
    filter(n_groups > 1L | n_sexes > 1L)
  tibble(
    scale = scale_label,
    check = "one_group_one_sex_within_scale",
    n_violations = nrow(conflicts),
    passed = nrow(conflicts) == 0L,
    offending_values = paste(conflicts$AnimalNum, collapse = "; ")
  )
}

#' Distinct (AnimalNum, Group, Sex) roster for one scale.
build_scale_roster <- function(dat, scale_label, animal_col = "AnimalNum", group_col = "Group", sex_col = "Sex") {
  if (!all(c(animal_col, group_col, sex_col) %in% names(dat))) {
    return(tibble(AnimalNum = character(), Group = character(), Sex = character(), scale = character()))
  }
  dat %>%
    transmute(AnimalNum = as.character(.data[[animal_col]]), Group = as.character(.data[[group_col]]), Sex = as.character(.data[[sex_col]])) %>%
    distinct() %>%
    mutate(scale = scale_label)
}

#' Compare animal rosters across all scales: same canonical roster (unless a
#' documented reason exists) and consistent Group/Sex assignment across scales.
#' `rosters` is a named list of build_scale_roster() outputs, one per scale.
#' `reference_scale` defaults to the first element; pass it explicitly to pin
#' a specific scale as ground truth.
compare_rosters_across_scales <- function(rosters, reference_scale = names(rosters)[[1]]) {
  rosters <- rosters[lengths(lapply(rosters, function(x) x$AnimalNum)) >= 0]
  ids_by_scale <- map(rosters, ~ sort(unique(.x$AnimalNum)))
  reference_ids <- ids_by_scale[[reference_scale]]

  roster_comparison <- imap_dfr(ids_by_scale, function(ids, scale_label) {
    tibble(
      scale = scale_label,
      reference_scale = reference_scale,
      n_animals = length(ids),
      n_reference_animals = length(reference_ids),
      missing_vs_reference = paste(setdiff(reference_ids, ids), collapse = "; "),
      extra_vs_reference = paste(setdiff(ids, reference_ids), collapse = "; "),
      roster_matches_reference = setequal(ids, reference_ids)
    )
  })

  combined <- bind_rows(rosters)
  cross_scale_group_sex_conflicts <- combined %>%
    distinct(AnimalNum, Group, Sex) %>%
    group_by(AnimalNum) %>%
    summarise(
      n_groups = n_distinct(Group), n_sexes = n_distinct(Sex),
      groups_seen = paste(sort(unique(Group)), collapse = "|"),
      sexes_seen = paste(sort(unique(Sex)), collapse = "|"),
      .groups = "drop"
    ) %>%
    filter(n_groups > 1L | n_sexes > 1L)

  list(roster_comparison = roster_comparison, cross_scale_group_sex_conflicts = cross_scale_group_sex_conflicts)
}

#' Derive the expected canonical roster AND expected phenotype assignment
#' from the RAW preprocessed Stage 01 input (e.g. the AnimalID column read
#' directly from *_preprocessed.csv), never from an already-derived scale's
#' Group column. This is what keeps "which animals, and which phenotype, do
#' we expect" independent of anything that may have gone wrong in a
#' derived-metrics rebuild -- including a scale where every animal agrees
#' with every other scale but all of them are wrong relative to the SUS/CON
#' reference contract.
#'
#' Mirrors Stage 01's own assignment rule (Analysis/01_build_multiscale_behavior_metrics.R)
#' exactly, computed independently here for validation.
derive_expected_phenotype_from_preprocessed <- function(preprocessed_animal_ids, sus_ids, con_ids,
                                                         assign_unlisted_as_res = TRUE) {
  overlap <- intersect(sus_ids, con_ids)
  if (length(overlap) > 0L) {
    stop("Canonical AnimalIDs occur in both SUS and CON reference files: ", paste(overlap, collapse = ", "), call. = FALSE)
  }
  canonical_ids <- canonical_animal_id(preprocessed_animal_ids)
  canonical_ids <- sort(unique(canonical_ids[!is.na(canonical_ids)]))
  tibble(
    AnimalNum = canonical_ids,
    ExpectedGroup = case_when(
      canonical_ids %in% sus_ids ~ "SUS",
      canonical_ids %in% con_ids ~ "CON",
      assign_unlisted_as_res ~ "RES",
      TRUE ~ NA_character_
    )
  )
}

#' Compare one scale's OBSERVED (AnimalNum, Group) against the EXPECTED
#' phenotype derived independently from preprocessed input + reference files
#' (derive_expected_phenotype_from_preprocessed()). This catches mislabeling
#' even when every derived scale agrees with every other scale -- i.e. cases
#' check_one_group_one_sex_within_scale() and the cross-scale conflict check
#' cannot catch on their own, because there is no *disagreement* to find, only
#' agreement with a wrong ground truth.
check_scale_matches_expected_phenotype <- function(dat, scale_label, expected_phenotype,
                                                    animal_col = "AnimalNum", group_col = "Group") {
  if (!all(c(animal_col, group_col) %in% names(dat))) {
    return(tibble(scale = scale_label, check = "matches_expected_phenotype",
                  n_violations = NA_integer_, passed = NA, offending_values = "missing_required_columns"))
  }
  observed <- dat %>%
    transmute(AnimalNum = as.character(.data[[animal_col]]), ObservedGroup = as.character(.data[[group_col]])) %>%
    distinct()
  joined <- observed %>% inner_join(expected_phenotype, by = "AnimalNum")
  mismatches <- joined %>% filter(!is.na(ExpectedGroup), ObservedGroup != ExpectedGroup)
  tibble(
    scale = scale_label,
    check = "matches_expected_phenotype",
    n_violations = nrow(mismatches),
    passed = nrow(mismatches) == 0L,
    offending_values = paste(
      sprintf("%s(observed=%s,expected=%s)", mismatches$AnimalNum, mismatches$ObservedGroup, mismatches$ExpectedGroup),
      collapse = "; "
    )
  )
}

#' Confirm a downstream stage's animal roster and Group/Sex map inherit
#' Stage 01's canonical identity: no animal unknown to Stage 01, and no
#' Group/Sex disagreement for animals both stages share.
check_downstream_inherits_identity <- function(stage01_roster, downstream_dat, downstream_label,
                                               animal_col = "AnimalNum", group_col = "Group", sex_col = "Sex") {
  if (is.null(downstream_dat) || nrow(downstream_dat) == 0L) {
    return(tibble(downstream = downstream_label, check = "inherits_stage01_identity",
                  n_unknown_animals = NA_integer_, n_group_sex_disagreements = NA_integer_,
                  passed = NA, unknown_animals = NA_character_, disagreeing_animals = NA_character_))
  }
  has_group_sex <- all(c(group_col, sex_col) %in% names(downstream_dat))
  downstream_roster <- downstream_dat %>%
    transmute(
      AnimalNum = as.character(.data[[animal_col]]),
      Group = if (has_group_sex) as.character(.data[[group_col]]) else NA_character_,
      Sex = if (has_group_sex) as.character(.data[[sex_col]]) else NA_character_
    ) %>%
    distinct()

  unknown <- sort(unique(setdiff(downstream_roster$AnimalNum, stage01_roster$AnimalNum)))

  disagreeing <- character()
  if (has_group_sex) {
    joined <- downstream_roster %>%
      inner_join(stage01_roster %>% select(AnimalNum, Group_ref = Group, Sex_ref = Sex), by = "AnimalNum")
    disagreeing <- joined %>%
      filter(Group != Group_ref | Sex != Sex_ref) %>%
      pull(AnimalNum) %>%
      unique() %>%
      sort()
  }

  tibble(
    downstream = downstream_label,
    check = "inherits_stage01_identity",
    n_unknown_animals = length(unknown),
    n_group_sex_disagreements = length(disagreeing),
    passed = length(unknown) == 0L && length(disagreeing) == 0L,
    unknown_animals = paste(unknown, collapse = "; "),
    disagreeing_animals = paste(disagreeing, collapse = "; ")
  )
}

#' Run every per-scale and cross-scale check and return one structured result.
#' `scale_data`: named list of data frames, one per fixed-width/phase scale.
#' `downstream`: optional named list of data frames (e.g. list(stage03 = ..., stage09 = ...)).
#' `expected_phenotype`: optional tibble(AnimalNum, ExpectedGroup) from
#' derive_expected_phenotype_from_preprocessed() -- i.e. derived from RAW
#' preprocessed input, NOT from any derived scale's own Group column. When
#' supplied, every scale's observed Group is checked against it directly
#' (check_scale_matches_expected_phenotype()), and it also supplies the
#' expected roster for roster_count_summary unless expected_roster is given
#' explicitly.
#' `expected_roster`: optional character vector of AnimalNums expected to be
#' present. If NULL and expected_phenotype is supplied, this is derived from
#' expected_phenotype$AnimalNum. Observed-vs-expected counts are reported but
#' never silently enforced as a hardcoded universal invariant.
validate_cross_scale_animal_identity <- function(scale_data,
                                                 downstream = list(),
                                                 expected_phenotype = NULL,
                                                 expected_roster = NULL,
                                                 reference_scale = names(scale_data)[[1]],
                                                 animal_col = "AnimalNum", group_col = "Group", sex_col = "Sex") {
  stopifnot(length(scale_data) > 0L, all(nzchar(names(scale_data))))
  if (is.null(expected_roster) && !is.null(expected_phenotype)) {
    expected_roster <- unique(expected_phenotype$AnimalNum)
  }

  format_checks <- imap_dfr(scale_data, ~ check_canonical_animal_num_format(.x[[animal_col]], .y))
  group_sex_checks <- imap_dfr(scale_data, ~ check_one_group_one_sex_within_scale(.x, .y, animal_col, group_col, sex_col))
  rosters <- imap(scale_data, ~ build_scale_roster(.x, .y, animal_col, group_col, sex_col))
  cross_scale <- compare_rosters_across_scales(rosters, reference_scale = reference_scale)

  reference_roster <- rosters[[reference_scale]]
  downstream_checks <- imap_dfr(downstream, ~ check_downstream_inherits_identity(reference_roster, .x, .y, animal_col, group_col, sex_col))

  expected_phenotype_checks <- if (is.null(expected_phenotype)) {
    tibble(scale = character(), check = character(), n_violations = integer(), passed = logical(), offending_values = character())
  } else {
    imap_dfr(scale_data, ~ check_scale_matches_expected_phenotype(.x, .y, expected_phenotype, animal_col, group_col))
  }

  roster_count_summary <- tibble(
    reference_scale = reference_scale,
    n_observed_animals = length(unique(reference_roster$AnimalNum)),
    n_expected_animals = if (is.null(expected_roster)) NA_integer_ else length(unique(expected_roster)),
    n_observed_not_expected = if (is.null(expected_roster)) NA_integer_ else length(setdiff(reference_roster$AnimalNum, expected_roster)),
    n_expected_not_observed = if (is.null(expected_roster)) NA_integer_ else length(setdiff(expected_roster, reference_roster$AnimalNum)),
    expected_roster_source = if (is.null(expected_roster)) NA_character_ else if (!is.null(expected_phenotype)) "preprocessed_input_plus_reference_files" else "caller_supplied"
  )

  all_checks_passed <-
    all(format_checks$passed %in% TRUE) &&
    all(group_sex_checks$passed %in% TRUE) &&
    all(cross_scale$roster_comparison$roster_matches_reference %in% TRUE) &&
    nrow(cross_scale$cross_scale_group_sex_conflicts) == 0L &&
    (nrow(downstream_checks) == 0L || all(downstream_checks$passed %in% TRUE)) &&
    (nrow(expected_phenotype_checks) == 0L || all(expected_phenotype_checks$passed %in% TRUE))

  list(
    format_checks = format_checks,
    group_sex_checks_within_scale = group_sex_checks,
    roster_comparison_across_scales = cross_scale$roster_comparison,
    cross_scale_group_sex_conflicts = cross_scale$cross_scale_group_sex_conflicts,
    downstream_inheritance_checks = downstream_checks,
    expected_phenotype_checks = expected_phenotype_checks,
    roster_count_summary = roster_count_summary,
    all_checks_passed = all_checks_passed
  )
}
