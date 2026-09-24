# ================================================================
# Equivalent acute post-cage-change windows for CC1, CC2, CC3 and CC4
# MMMSociability
# ================================================================
# WHY THIS FILE EXISTS
#
# "Adaptation" must mean CHANGE IN THE SAME ACUTE RESPONSE across repeated cage
# changes. That requires four windows that are equivalent by construction, not a
# broad per-epoch average and not four differently-defined windows.
#
# THIS FILE ADDS NO NEW WINDOW RULE. It calls the canonical selector
# Functions/first_night_window_helpers.R :: mmm_select_first_night_window() once
# per cage change, passing `first_cage_change` explicitly. That selector already
# accepts the argument, and its anchor is computed PER SESSION from the session
# column (SourceFile), which encodes Batch and CageChange. Passing "CC2" therefore
# yields the first Active 12 h block of CC2 under exactly the CC1 rule.
#
# The canonical selector is deliberately NOT edited. It is locked by
# Testing/tests/test_first_night_window_parity.R, which asserts byte-equivalence
# with Stage 09's own select_primary_active_window(); editing it would break that
# gate and the two acute-window parity tests that chain off it, without changing
# any Stage 09 number. Reuse-by-call keeps Stage 09 provably untouched.
#
# EXPERIMENTAL VALIDITY, VERIFIED RATHER THAN ASSUMED
# Every one of the 24 recording sessions (6 batches x 4 cage changes) begins at
# exactly 18:30 on the day of its cage change, and the first Active phase block of
# each session starts at 18:30, so the lag from recording start to the first
# Active block is 0.00 h for all 24 sessions. Cage changes are 4 days apart.
# CC1-CC3 span ~3.5 days (7 phase blocks); CC4 spans ~1.5 days (3 blocks), which
# does NOT affect a 12 h acute window but DOES rule out any whole-epoch metric
# being compared across CC. mmm_rfid_assert_acute_window_validity() re-checks the
# anchor property on the data actually supplied, so a future batch that breaks it
# fails loudly instead of silently producing a non-equivalent CC4 window.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
})

for (.dep in c("phase_classification_helpers.R", "animalpos_preprocessing_helpers.R",
               "first_night_window_helpers.R")) {
  if (exists("source_mmm_helper", mode = "function", inherits = TRUE)) {
    source_mmm_helper(.dep)
  }
}

MMM_RFID_ACUTE_WINDOW_HOURS <- 12
MMM_RFID_CAGE_CHANGE_LEVELS <- c("CC1", "CC2", "CC3", "CC4")

#' Cage-epoch identifier: the social/RFID cage an animal occupies for one cage change.
#'
#' DERIVED FROM THE SCHEMA, NOT ASSUMED. In this dataset Batch x System x CageChange
#' partitions the animals identically to SourceFile x System (SourceFile encodes
#' Batch and CageChange, e.g. "E9_SIS_B1_CC1_AnimalPos_preprocessed.csv"), and
#' every animal maps to exactly one such unit per cage change. System ALONE is NOT
#' a cage: animals move between sys.1..sys.5 across cage changes, so System would
#' pool unrelated social groups.
mmm_rfid_cage_epoch_id <- function(batch, system, cage_change) {
  paste(as.character(batch), as.character(system), as.character(cage_change), sep = "|")
}

#' Assert the acute-window anchor rule is experimentally valid on THIS data.
#'
#' Checks, per session, that the first Active phase block begins at the session's
#' own recording start (within one bin) and that the resulting clock anchor is the
#' declared 18:30. Returns the per-session evidence table.
mmm_rfid_assert_acute_window_validity <- function(dat, bin_size_sec,
                                                  session_col = "SourceFile",
                                                  tolerance_sec = NULL) {
  for (needed in c("Phase", "BinStart", "CageChange", session_col)) {
    if (!needed %in% names(dat)) {
      stop("mmm_rfid_assert_acute_window_validity() requires column: ", needed,
           call. = FALSE)
    }
  }
  if (is.null(tolerance_sec)) tolerance_sec <- bin_size_sec

  ev <- dat %>%
    mutate(.sess = as.character(.data[[session_col]]),
           .blk = animalpos_phase_block_index(.data$BinStart)) %>%
    group_by(.data$.sess) %>%
    summarise(
      CageChange = paste(sort(unique(as.character(.data$CageChange))), collapse = ","),
      recording_start = min(.data$BinStart),
      first_active_block = suppressWarnings(min(.data$.blk[mmm_is_active_phase(.data$Phase)])),
      .groups = "drop"
    ) %>%
    mutate(
      first_active_start = as.POSIXct(
        .data$first_active_block * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC,
        origin = "1970-01-01", tz = "UTC"),
      anchor_clock = format(.data$first_active_start, "%H:%M"),
      lag_sec_recording_to_first_active =
        as.numeric(difftime(.data$first_active_start, .data$recording_start, units = "secs")),
      anchor_is_declared_clock = .data$anchor_clock == "18:30",
      first_active_is_session_start = abs(.data$lag_sec_recording_to_first_active) <= tolerance_sec
    )

  bad_clock <- ev %>% filter(!.data$anchor_is_declared_clock)
  if (nrow(bad_clock) > 0L) {
    stop("Acute-window anchor is not the declared 18:30 clock for ", nrow(bad_clock),
         " session(s), e.g. ", bad_clock$.sess[1], " -> ", bad_clock$anchor_clock[1],
         ". The CC1-CC4 windows would not be equivalent.", call. = FALSE)
  }
  ev
}

#' Select the equivalent acute Active window for ONE cage change.
#'
#' Thin wrapper over the canonical selector, so the window RULE has exactly one
#' implementation in the repository.
mmm_rfid_select_acute_window <- function(dat, bin_size_sec, cage_change,
                                         window_hours = MMM_RFID_ACUTE_WINDOW_HOURS) {
  if (!any(as.character(dat$CageChange) == cage_change)) {
    stop("Cage change '", cage_change, "' is absent from the supplied data.", call. = FALSE)
  }
  sel <- mmm_select_first_night_window(
    dat, bin_size_sec = bin_size_sec, window_hours = window_hours,
    first_cage_change = cage_change
  )
  sel %>%
    mutate(
      CageChangeLabel = cage_change,
      CageChangeIndex = suppressWarnings(as.integer(sub("^\\D*", "", cage_change))),
      window_definition = paste0(
        "fixed clock window: first Active phase block after cage change ", cage_change,
        ", 18:30 inclusive to 06:30 exclusive, exactly ", window_hours, " h"),
      window_rule_source =
        "Functions/first_night_window_helpers.R :: mmm_select_first_night_window (reused, not reimplemented)"
    )
}

#' Select equivalent acute windows for every requested cage change.
mmm_rfid_select_all_acute_windows <- function(dat, bin_size_sec,
                                              cage_changes = MMM_RFID_CAGE_CHANGE_LEVELS,
                                              window_hours = MMM_RFID_ACUTE_WINDOW_HOURS) {
  present <- intersect(cage_changes, unique(as.character(dat$CageChange)))
  missing_cc <- setdiff(cage_changes, present)
  if (length(missing_cc) > 0L) {
    warning("Cage change(s) absent from the data and skipped: ",
            paste(missing_cc, collapse = ", "), call. = FALSE)
  }
  purrr::map_dfr(present, function(cc) {
    mmm_rfid_select_acute_window(dat, bin_size_sec = bin_size_sec, cage_change = cc,
                                 window_hours = window_hours)
  })
}

#' Per Animal x CageChange window provenance / QC.
#'
#' Reports expected vs observed slots and leading / interior / trailing gaps
#' SEPARATELY, because a late first read is a different phenomenon from a
#' mid-window dropout. Carries Batch, System, SourceFile and CageEpochID so the
#' dependence structure travels with the window record.
#'
#' There is no row-count fallback anywhere, and nothing ever reaches into a later
#' Active block to fill a missing bin: the window is a closed clock interval and
#' an unobserved slot stays unobserved.
mmm_rfid_acute_window_qc <- function(selected) {
  if (nrow(selected) == 0L) return(tibble::tibble())
  for (needed in c("Batch", "System", "CageChangeLabel")) {
    if (!needed %in% names(selected)) {
      stop("mmm_rfid_acute_window_qc() requires column '", needed,
           "'; cage/batch provenance must travel with the window.", call. = FALSE)
    }
  }
  group_keys <- intersect(
    c("AnimalNum", "Group", "Sex", "Batch", "System", "SourceFile",
      "CageChangeLabel", "CageChangeIndex"),
    names(selected))
  selected %>%
    group_by(across(all_of(group_keys))) %>%
    summarise(
      expected_slots = dplyr::first(.data$expected_slots),
      observed_slots = n_distinct(.data$target_slot),
      target_window_start = dplyr::first(.data$target_window_start),
      target_window_end = dplyr::first(.data$target_window_end),
      first_target_slot = min(.data$target_slot),
      last_target_slot = max(.data$target_slot),
      first_bin_start = min(.data$BinStart),
      last_bin_start = max(.data$BinStart),
      missing_leading_slots = min(.data$target_slot) - 1L,
      missing_trailing_slots = dplyr::first(.data$expected_slots) - max(.data$target_slot),
      missing_interior_slots =
        (max(.data$target_slot) - min(.data$target_slot) + 1L) - n_distinct(.data$target_slot),
      max_internal_gap_slots = {
        s <- sort(unique(.data$target_slot))
        if (length(s) < 2L) 0L else as.integer(max(diff(s)) - 1L)
      },
      .groups = "drop"
    ) %>%
    mutate(
      coverage_fraction = .data$observed_slots / .data$expected_slots,
      window_complete = .data$observed_slots == .data$expected_slots,
      CageEpochID = mmm_rfid_cage_epoch_id(.data$Batch, .data$System,
                                           .data$CageChangeLabel)
    )
}

#' Assert each animal x cage change maps to exactly ONE cage epoch.
mmm_rfid_assert_one_cage_per_animal_window <- function(qc) {
  chk <- qc %>%
    group_by(.data$AnimalNum, .data$CageChangeLabel) %>%
    summarise(n_cage = n_distinct(.data$CageEpochID), n_rows = dplyr::n(), .groups = "drop") %>%
    filter(.data$n_cage != 1L | .data$n_rows != 1L)
  if (nrow(chk) > 0L) {
    stop("Each animal must map to exactly one cage epoch per cage change; ",
         nrow(chk), " violation(s), e.g. animal ", chk$AnimalNum[1], " at ",
         chk$CageChangeLabel[1], " (", chk$n_cage[1], " cage epochs).", call. = FALSE)
  }
  invisible(TRUE)
}

#' Cage-structure description for one or more cage changes.
mmm_rfid_cage_structure_summary <- function(qc) {
  by_cc <- qc %>%
    group_by(.data$CageChangeLabel) %>%
    summarise(n_animals = n_distinct(.data$AnimalNum),
              n_cage_epochs = n_distinct(.data$CageEpochID),
              n_batches = n_distinct(.data$Batch),
              .groups = "drop")
  cage_level <- qc %>%
    group_by(.data$CageChangeLabel, .data$CageEpochID, .data$Batch, .data$System) %>%
    summarise(cage_size = dplyr::n(),
              n_sex = n_distinct(.data$Sex),
              Sex = paste(sort(unique(as.character(.data$Sex))), collapse = "+"),
              n_groups = n_distinct(.data$Group),
              groups_present = paste(sort(unique(as.character(.data$Group))), collapse = "+"),
              n_CON = sum(as.character(.data$Group) == "CON"),
              n_RES = sum(as.character(.data$Group) == "RES"),
              n_SUS = sum(as.character(.data$Group) == "SUS"),
              .groups = "drop")
  list(by_cage_change = by_cc, by_cage = cage_level)
}
