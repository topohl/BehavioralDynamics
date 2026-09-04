# ================================================================
# Forensic/testing-only identity repair utility (NOT part of the pipeline)
# ================================================================
# This utility patches AnimalNum/AnimalID/Group in a SINGLE already-generated
# all_behavior_metrics.csv file in place, using the canonical_animal_id()
# contract. It exists only to let a developer inspect what an identity
# correction would change on one existing scale without waiting for a full
# rebuild, or to use in ad hoc forensic/debugging sessions.
#
# THIS IS NOT A SUBSTITUTE FOR A FULL PIPELINE REBUILD. It repairs exactly the
# one file you point it at; it does not regenerate the other scales
# (10sec/1min/5min/10min/30min/phase_based), does not recompute anything
# derived from raw tracking (movement events, occupancy intervals, dyadic
# proximity), and does not touch any downstream stage. The canonical recovery
# procedure after an identity-contract change is rerunning
# Analysis/01_build_multiscale_behavior_metrics.R in full; see that script's
# header comment.
#
# Unlike the removed production code path, this helper never calls quit() and
# is never invoked automatically by any Analysis/ script.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("Analysis/_pipeline_setup.R")

#' Repair canonical identity metadata in one existing derived-metrics file.
#'
#' @param path Path to an existing all_behavior_metrics.csv to rewrite in place.
#' @param sus_animals_file One-ID-per-line file of canonical SUS AnimalNums.
#' @param con_animals_file One-ID-per-line file of canonical CON AnimalNums.
#' @param assign_unlisted_animals_as_res If TRUE, animals not listed in either
#'   reference file are assigned Group "RES". Set FALSE if unlisted animals
#'   should instead be left NA.
#' @param expected_n_animals Optional sanity check on the resulting number of
#'   distinct canonical AnimalNum values. NULL (default) skips the check; this
#'   is dataset-specific and must not be treated as a universal invariant.
#' @return The repaired data, invisibly. Also rewrites `path`.
repair_existing_metrics_identity_for_testing <- function(path,
                                                         sus_animals_file,
                                                         con_animals_file,
                                                         assign_unlisted_animals_as_res = TRUE,
                                                         expected_n_animals = NULL) {
  if (!file.exists(path)) {
    stop("Cannot repair identity metadata; derived metrics file is missing: ", path, call. = FALSE)
  }

  read_animal_id_list <- function(ref_path, label) {
    if (is.null(ref_path) || !file.exists(ref_path)) {
      warning("Animal reference file not found for ", label, ": ", ref_path, call. = FALSE)
      return(character())
    }
    readr::read_lines(ref_path, progress = FALSE) %>%
      canonical_animal_id() %>%
      purrr::discard(~ is.na(.x) || .x == "") %>%
      unique()
  }

  sus_ids <- read_animal_id_list(sus_animals_file, "SUS")
  con_ids <- read_animal_id_list(con_animals_file, "CON")
  overlap <- intersect(sus_ids, con_ids)
  if (length(overlap) > 0L) {
    stop("Canonical AnimalIDs occur in both SUS and CON references: ", paste(overlap, collapse = ", "), call. = FALSE)
  }

  dat <- readr::read_csv(path, show_col_types = FALSE)
  animal_col <- first_existing_col(dat, c("AnimalNum", "AnimalID", "Animal"), TRUE, "derived-metrics animal column")
  sex_col <- first_existing_col(dat, c("Sex", "sex"), FALSE, "derived-metrics sex column")
  if (is.na(sex_col)) stop("Derived metrics file lacks Sex; cannot validate identity metadata.", call. = FALSE)

  dat <- dat %>%
    mutate(
      AnimalID_raw = if ("AnimalID_raw" %in% names(.)) as.character(AnimalID_raw) else as.character(.data[[animal_col]]),
      AnimalNum = canonical_animal_id(.data[[animal_col]]),
      AnimalID = AnimalNum,
      Group = case_when(
        AnimalNum %in% sus_ids ~ "SUS",
        AnimalNum %in% con_ids ~ "CON",
        assign_unlisted_animals_as_res ~ "RES",
        TRUE ~ NA_character_
      ),
      Sex = as.character(.data[[sex_col]])
    )

  conflicts <- dat %>%
    distinct(AnimalNum, Group, Sex) %>%
    group_by(AnimalNum) %>%
    summarise(n_groups = n_distinct(Group), n_sexes = n_distinct(Sex), .groups = "drop") %>%
    filter(n_groups > 1L | n_sexes > 1L)
  if (nrow(conflicts) > 0L) {
    stop("Identity repair would leave conflicting Group or Sex metadata.", call. = FALSE)
  }

  if (!is.null(expected_n_animals) && n_distinct(dat$AnimalNum) != expected_n_animals) {
    stop("Identity repair expected ", expected_n_animals, " canonical animals but found ", n_distinct(dat$AnimalNum), call. = FALSE)
  }

  readr::write_csv(dat, path, na = "NA")
  message(
    "Repaired canonical animal identity/group metadata in: ", path, "\n",
    "Reminder: this only patched one file. Run a full Stage 01 rebuild before ",
    "treating any downstream stage as reflecting the identity correction."
  )
  invisible(dat)
}
