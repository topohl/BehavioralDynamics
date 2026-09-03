# ================================================================
# Exact circadian phase classification
# MMMSociability
# ================================================================
# One shared, exact phase classifier for the whole pipeline.
#
# Why this exists
# ---------------
# The historical pattern
#
#     str_detect(str_to_lower(Phase), "active|dark|night") ~ "Active"
#
# is unsafe because the string "inactive" CONTAINS the substring "active".
# When that branch is evaluated before the inactive branch, every Inactive
# epoch is silently relabelled Active. The same hazard applies to any
# `if_else(str_detect(..., "active|dark|night"), ...)` used to attribute
# durations to a phase.
#
# The contract here is explicit membership on a normalized label, so
# "inactive" can never satisfy the Active predicate regardless of evaluation
# order. Unrecognised labels return NA rather than defaulting to a phase:
# a silent misclassification is far more damaging than a visible NA.
#
# Anchored word-boundary regexes such as "\\bactive\\b" are also safe, because
# there is no word boundary inside "inactive". Membership is preferred anyway:
# it is order-independent, cheaper, and states the accepted vocabulary.

MMM_ACTIVE_PHASE_VALUES <- c("active", "dark", "night")
MMM_INACTIVE_PHASE_VALUES <- c("inactive", "light", "day")

#' Normalize a phase label for comparison (lower-case, trimmed, no inner space).
mmm_normalize_phase_label <- function(x) {
  gsub("\\s+", "", tolower(trimws(as.character(x))))
}

#' TRUE only for labels that are exactly an accepted Active vocabulary item.
mmm_is_active_phase <- function(x) {
  mmm_normalize_phase_label(x) %in% MMM_ACTIVE_PHASE_VALUES
}

#' TRUE only for labels that are exactly an accepted Inactive vocabulary item.
mmm_is_inactive_phase <- function(x) {
  mmm_normalize_phase_label(x) %in% MMM_INACTIVE_PHASE_VALUES
}

#' Map raw phase labels to the canonical "Active" / "Inactive" classes.
#'
#' @param x Raw phase labels.
#' @param unmatched Value returned for labels in neither vocabulary. Defaults
#'   to NA_character_ so that unknown labels are visible instead of being
#'   absorbed into a phase. Pass `unmatched = "keep"` to fall through to the
#'   original label where a caller genuinely needs the historical behaviour.
mmm_phase_class <- function(x, unmatched = NA_character_) {
  p <- mmm_normalize_phase_label(x)
  out <- if (identical(unmatched, "keep")) as.character(x) else rep(unmatched, length(p))
  # Inactive assigned first, then Active: with exact membership the order is
  # irrelevant, and writing it this way makes that independence explicit.
  out[p %in% MMM_INACTIVE_PHASE_VALUES] <- "Inactive"
  out[p %in% MMM_ACTIVE_PHASE_VALUES] <- "Active"
  out
}

#' Fail closed when phase labels cannot be classified.
#'
#' Use at stage boundaries where an unclassifiable phase would corrupt
#' downstream duration attribution or window selection.
mmm_assert_phase_classifiable <- function(x, source_label = "phase column") {
  cls <- mmm_phase_class(x)
  bad <- is.na(cls) & !is.na(x)
  if (any(bad)) {
    stop(
      source_label, " contains phase labels outside the accepted vocabulary: ",
      paste(sort(unique(as.character(x)[bad])), collapse = ", "),
      ". Accepted Active: ", paste(MMM_ACTIVE_PHASE_VALUES, collapse = "/"),
      "; accepted Inactive: ", paste(MMM_INACTIVE_PHASE_VALUES, collapse = "/"),
      ".", call. = FALSE
    )
  }
  invisible(TRUE)
}
