# Regression tests for the exact shared phase classifier.
# The historical substring pattern misclassified "Inactive" as "Active"; these
# tests lock that failure mode out permanently.

source("Analysis/_pipeline_setup.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("duration_normalization_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

# A. The required mapping.
check(identical(mmm_phase_class("Active"), "Active"), "A: Active -> Active")
check(identical(mmm_phase_class("Inactive"), "Inactive"), "A: Inactive -> Inactive")
check(identical(mmm_phase_class("dark"), "Active"), "A: dark -> Active")
check(identical(mmm_phase_class("night"), "Active"), "A: night -> Active")
check(identical(mmm_phase_class("light"), "Inactive"), "A: light -> Inactive")
check(identical(mmm_phase_class("day"), "Inactive"), "A: day -> Inactive")

# B. "Inactive" can NEVER enter Active, in any casing or with whitespace.
inactive_spellings <- c("Inactive", "inactive", "INACTIVE", " Inactive ", "In active", "inActive")
check(
  !any(mmm_is_active_phase(inactive_spellings)),
  "B: no spelling of 'inactive' may satisfy the Active predicate"
)
check(
  all(mmm_phase_class(inactive_spellings) == "Inactive"),
  "B: every spelling of 'inactive' must classify as Inactive"
)

# C. The historical substring pattern really was broken (documents the bug).
historical_is_active <- grepl("active|dark|night", tolower("Inactive"))
check(historical_is_active, "C: the historical substring pattern must match 'Inactive' (bug documented)")
check(!mmm_is_active_phase("Inactive"), "C: the exact classifier must not match 'Inactive'")

# D. Order independence: classification does not depend on which branch is tested first.
mixed <- c("Active", "Inactive", "dark", "light", "night", "day")
check(
  identical(mmm_phase_class(mixed), c("Active", "Inactive", "Active", "Inactive", "Active", "Inactive")),
  "D: vectorized classification must be exact and order-independent"
)
check(
  !any(mmm_is_active_phase(mixed) & mmm_is_inactive_phase(mixed)),
  "D: no label may satisfy both predicates"
)

# E. Unknown labels are visible, not absorbed into a phase.
check(is.na(mmm_phase_class("Twilight")), "E: unknown label must return NA by default")
check(identical(mmm_phase_class("Twilight", unmatched = "keep"), "Twilight"), "E: unmatched='keep' passes through")
unknown_error <- tryCatch({ mmm_assert_phase_classifiable(c("Active", "Twilight"), "fixture"); NULL },
                          error = function(e) e)
check(inherits(unknown_error, "error"), "E: the assertion must stop on an unclassifiable label")
check(grepl("Twilight", conditionMessage(unknown_error), fixed = TRUE), "E: the error must name the offending label")

# F. NA handling.
check(is.na(mmm_phase_class(NA_character_)), "F: NA in -> NA out")
check(!mmm_is_active_phase(NA_character_), "F: NA is not Active")
mmm_assert_phase_classifiable(c("Active", NA_character_), "fixture with NA")

# G. Duration attribution: an Inactive epoch must contribute ZERO active hours
#    and its full duration to inactive hours. This is the concrete failure the
#    shared duration helper previously produced.
qc <- calculate_observation_duration(
  data.frame(
    AnimalNum = rep("1", 8),
    Group = "CON", Sex = "Female", BinLevel = "10min_based",
    CageChange = "CC1",
    Phase = rep(c("Active", "Inactive"), each = 4),
    TimeIndex = c(0:3, 0:3),
    Movement = 1, Entropy = 1, Proximity = 0,
    observation_seconds = 600,
    stringsAsFactors = FALSE
  ),
  metric_source = "phase_classification_test",
  bin_size_sec = 600
)
act <- qc[mmm_phase_class(qc$Phase) == "Active", , drop = FALSE]
ina <- qc[mmm_phase_class(qc$Phase) == "Inactive", , drop = FALSE]
check(nrow(act) == 1 && nrow(ina) == 1, "G: fixture must yield one Active and one Inactive epoch")
check(act$inactive_duration_hours == 0, "G: an Active epoch must contribute zero inactive hours")
check(ina$active_duration_hours == 0, "G: an Inactive epoch must contribute zero ACTIVE hours")
check(ina$inactive_duration_hours > 0, "G: an Inactive epoch must contribute its duration to inactive hours")
check(
  isTRUE(all.equal(act$active_duration_hours, act$total_observation_duration_hours)),
  "G: an Active epoch's active hours must equal its total observed hours"
)
check(
  isTRUE(all.equal(ina$inactive_duration_hours, ina$total_observation_duration_hours)),
  "G: an Inactive epoch's inactive hours must equal its total observed hours"
)

# H. Production stages must not reintroduce the permissive pattern.
permissive <- "str_to_lower(as.character(Phase)), \"active|dark|night\""
for (f in c("Analysis/11_behavioral_adaptation_kinetics.R",
            "Analysis/12_sleep_like_quiescence_metrics.R",
            "Analysis/13_ethological_phase_organization.R",
            "Functions/duration_normalization_helpers.R")) {
  src <- paste(readLines(f, warn = FALSE), collapse = "\n")
  check(!grepl(permissive, src, fixed = TRUE),
        paste0("H: ", f, " must not use the permissive substring phase pattern"))
  check(!grepl("str_detect(str_to_lower(first(Phase)), \"active|dark|night\")", src, fixed = TRUE),
        paste0("H: ", f, " must not attribute duration via permissive phase matching"))
}

cat("Phase classification regression checks: PASS\n")
