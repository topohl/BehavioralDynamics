# Portable regression tests for the Stage 09 primary prospective window.
#
# Guards the confirmed results-changing defect where
#   early_phase_pattern <- "active|dark|night"
# was applied with str_detect(), which ALSO matches "Inactive" because
# "inactive" contains the substring "active". Combined with ranking inside
# group_by(AnimalNum, Phase), that retained 72 Active AND 72 Inactive bins per
# animal, so the canonical features were computed over ~24 h spanning both
# phases instead of the declared first 12 h of the Active phase.
#
# Runs entirely in memory; no S: drive access required.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(stringr)
})

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

# Load the REAL definitions out of the production script (top-level assignments
# only; the script body is never executed). Tests below therefore exercise
# production code, not a copy that could silently drift from it.
STAGE09 <- "Analysis/09_early_prediction_model_ladder.R"
.prod <- new.env(parent = globalenv())
.wanted <- c("active_phase_values", "inactive_phase_values", "normalize_phase_label",
             "is_active_phase", "early_window_hours", "bin_size_min",
             "expected_early_bins_per_animal", "bin_seconds",
             "select_primary_active_window")
for (e in parse(STAGE09)) {
  if (is.call(e) && length(e) >= 3 && as.character(e[[1]]) %in% c("<-", "=")) {
    nm <- tryCatch(as.character(e[[2]]), error = function(err) "")
    if (length(nm) == 1 && nm %in% .wanted) tryCatch(eval(e, envir = .prod), error = function(err) {})
  }
}
missing_defs <- setdiff(.wanted, ls(.prod))
if (length(missing_defs)) {
  stop("FAIL: could not load production definition(s): ", paste(missing_defs, collapse = ", "), call. = FALSE)
}
active_phase_values         <- .prod$active_phase_values
inactive_phase_values       <- .prod$inactive_phase_values
normalize_phase_label       <- .prod$normalize_phase_label
is_active_phase             <- .prod$is_active_phase          # <- production classifier under test
early_window_hours          <- .prod$early_window_hours
bin_size_min                <- .prod$bin_size_min
bin_seconds                 <- .prod$bin_seconds
expected_early_bins_per_animal <- .prod$expected_early_bins_per_animal

stopifnot(early_window_hours == 12, bin_size_min == 10,
          bin_seconds == 600, expected_early_bins_per_animal == 72)

# The production window selector itself is under test.
select_primary_window <- .prod$select_primary_active_window

# ------------------------------------------------------------------
# A. Phase matching: Active/active included; Inactive/inactive excluded.
# ------------------------------------------------------------------
check(is_active_phase("Active"),   "A: 'Active' must be included")
check(is_active_phase("active"),   "A: 'active' must be included")
check(is_active_phase(" Active "), "A: whitespace-padded 'Active' must be included")
check(!is_active_phase("Inactive"), "A: 'Inactive' must be EXCLUDED")
check(!is_active_phase("inactive"), "A: 'inactive' must be EXCLUDED")
check(!is_active_phase("InActive"), "A: 'InActive' must be EXCLUDED")
check(is_active_phase("dark") && is_active_phase("night"), "A: documented active aliases must be included")
check(!is_active_phase("light") && !is_active_phase("day"), "A: documented inactive aliases must be excluded")
check(identical(is_active_phase(c("Active","Inactive","active","inactive")), c(TRUE,FALSE,TRUE,FALSE)),
      "A: vectorized classification must be elementwise correct")

# ------------------------------------------------------------------
# B. Demonstrate the historical defect: the OLD pattern matched "inactive".
# ------------------------------------------------------------------
old_pattern <- "active|dark|night"
check(str_detect(str_to_lower("Inactive"), old_pattern),
      "B: the historical pattern is expected to (wrongly) match 'Inactive' -- this documents the bug")
check(str_detect(str_to_lower("Active"), old_pattern), "B: historical pattern matched 'Active' too")
check(!is_active_phase("Inactive") && str_detect(str_to_lower("Inactive"), old_pattern),
      "B: the corrected classifier must reject exactly what the old pattern wrongly accepted")

# ------------------------------------------------------------------
# C. 72 Active + 72 Inactive bins -> exactly 72 selected rows, Active only.
# ------------------------------------------------------------------
mk_animal <- function(id, n_active = 72, n_inactive = 72) {
  tibble(
    AnimalNum = id,
    TimeIndex = 0:(n_active + n_inactive - 1L),
    Phase = c(rep("Active", n_active), rep("Inactive", n_inactive)),
    Movement = seq_len(n_active + n_inactive)
  )
}
selC <- select_primary_window(mk_animal("A1"))
check(nrow(selC) == 72, paste0("C: expected exactly 72 selected rows, got ", nrow(selC)))
check(all(selC$Phase == "Active"), "C: only Active rows may be selected")
check(sum(normalize_phase_label(selC$Phase) %in% inactive_phase_values) == 0, "C: zero Inactive rows must be selected")
check(max(selC$elapsed_sec_in_window) == 71 * bin_seconds, "C: last selected bin must start at 11h50m elapsed")

# Under the OLD logic the same animal yielded 144 rows across both phases.
old_logic <- mk_animal("A1") %>%
  filter(str_detect(str_to_lower(as.character(Phase)), old_pattern)) %>%
  group_by(AnimalNum, Phase) %>% arrange(TimeIndex, .by_group = TRUE) %>%
  mutate(early_rank = row_number()) %>% filter(early_rank <= 72) %>% ungroup()
check(nrow(old_logic) == 144, "C: the old logic is expected to have produced 144 rows (documents the bug)")
check(n_distinct(old_logic$Phase) == 2, "C: the old logic mixed both phases")

# ------------------------------------------------------------------
# D. Window boundary: bins within the first 12 h in; first bin at/after 12 h out.
# ------------------------------------------------------------------
long_active <- tibble(AnimalNum = "B1", TimeIndex = 0:99, Phase = "Active", Movement = 1)
selD <- select_primary_window(long_active)
check(nrow(selD) == 72, paste0("D: a 100-bin Active run must be truncated to 72 bins, got ", nrow(selD)))
check(max(selD$TimeIndex) == 71, "D: TimeIndex 71 (11h50m) must be the last included bin")
check(!72 %in% selD$TimeIndex, "D: TimeIndex 72 (exactly 12 h elapsed) must be EXCLUDED")
check(max(selD$elapsed_sec_in_window) < early_window_hours * 3600, "D: all selected bins must be strictly under 12 h elapsed")

# Non-zero starting TimeIndex: elapsed time is measured from the episode start.
offset_active <- tibble(AnimalNum = "B2", TimeIndex = 500:599, Phase = "Active", Movement = 1)
selD2 <- select_primary_window(offset_active)
check(nrow(selD2) == 72, "D: elapsed time must be measured from the episode start, not absolute TimeIndex")
check(min(selD2$TimeIndex) == 500 && max(selD2$TimeIndex) == 571, "D: offset episode must select its own first 72 bins")

# Only the FIRST Active episode is used, never a later one.
two_episodes <- bind_rows(
  tibble(AnimalNum = "B3", TimeIndex = 0:71,    Phase = "Active",   Movement = 1),
  tibble(AnimalNum = "B3", TimeIndex = 72:143,  Phase = "Inactive", Movement = 2),
  tibble(AnimalNum = "B3", TimeIndex = 144:215, Phase = "Active",   Movement = 3)
)
selD3 <- select_primary_window(two_episodes)
check(nrow(selD3) == 72, "D: only the first Active episode may be selected")
check(max(selD3$TimeIndex) == 71, "D: the second Active episode must not be reached")
check(all(selD3$Movement == 1), "D: values must come from the first episode only")

# A short first episode is not padded from the next one.
short_first <- bind_rows(
  tibble(AnimalNum = "B4", TimeIndex = 0:49,    Phase = "Active",   Movement = 1),
  tibble(AnimalNum = "B4", TimeIndex = 50:121,  Phase = "Inactive", Movement = 2),
  tibble(AnimalNum = "B4", TimeIndex = 122:193, Phase = "Active",   Movement = 3)
)
selD4 <- select_primary_window(short_first)
check(nrow(selD4) == 50, "D: a short first Active episode must yield only its own bins")
check(all(selD4$Movement == 1), "D: a short window must never bridge into a later episode")

# Hardest case: the SECOND Active episode begins INSIDE the 12 h elapsed
# window. Only episode-boundary awareness (not the elapsed filter alone) can
# exclude it, so this is what detects a missing episode restriction.
early_second_episode <- bind_rows(
  tibble(AnimalNum = "B5", TimeIndex = 0:9,   Phase = "Active",   Movement = 1),
  tibble(AnimalNum = "B5", TimeIndex = 10:29, Phase = "Inactive", Movement = 2),
  tibble(AnimalNum = "B5", TimeIndex = 30:99, Phase = "Active",   Movement = 3)
)
selD5 <- select_primary_window(early_second_episode)
check(nrow(selD5) == 10,
      paste0("D: only the first Active episode's 10 bins may be selected, got ", nrow(selD5)))
check(all(selD5$Movement == 1),
      "D: a later Active episode inside the 12 h span must still be excluded")
check(max(selD5$TimeIndex) == 9, "D: selection must stop at the end of the first Active episode")
check(n_distinct(selD5$active_episode_id) == 1, "D: exactly one Active episode may be selected")

# ------------------------------------------------------------------
# E. Multi-animal invariant shape (SIS-like): every animal 72 Active, 0 Inactive.
# ------------------------------------------------------------------
many <- bind_rows(lapply(sprintf("A%02d", 1:20), mk_animal))
selE <- select_primary_window(many)
per <- selE %>% count(AnimalNum, name = "n")
check(n_distinct(selE$AnimalNum) == 20, "E: all animals must be retained")
check(all(per$n == 72), "E: every animal must contribute exactly 72 bins")
check(sum(normalize_phase_label(selE$Phase) %in% inactive_phase_values) == 0, "E: zero Inactive rows across all animals")
check(nrow(selE) == 20 * 72, "E: total selected rows must equal n_animals * 72")

# ------------------------------------------------------------------
# F. Source parity with the production script.
# ------------------------------------------------------------------
src_lines <- readLines("Analysis/09_early_prediction_model_ladder.R", warn = FALSE)
src <- paste(src_lines, collapse = "\n")
# Strip whole-line comments so the explanatory note describing the historical
# defect does not count as live code.
code_lines <- src_lines[!grepl("^\\s*#", src_lines)]
code <- paste(code_lines, collapse = "\n")
check(!grepl("early_phase_pattern", code, fixed = TRUE),
      "F: the permissive phase regex must no longer be active code")
check(!grepl("str_detect(str_to_lower(as.character(Phase))", code, fixed = TRUE),
      "F: phase selection must not use permissive substring matching")
check(grepl("active_phase_values", src, fixed = TRUE), "F: production must define an explicit active phase value set")
check(grepl("is_active_phase", src, fixed = TRUE), "F: production must use the explicit classifier")
check(grepl("elapsed_sec_in_window", src, fixed = TRUE), "F: production must select by elapsed time")
check(grepl("active_episode_id", src, fixed = TRUE), "F: production must restrict to the first Active episode")
check(!grepl("max_early_bins_per_animal", code, fixed = TRUE),
      "F: the row-count-based window definition must be gone")
check(grepl("early_window_contract_summary", src, fixed = TRUE), "F: production must emit a window contract summary")
check(grepl("window_contract_status", src, fixed = TRUE), "F: production must emit per-animal contract status")
# The Inactive-selection invariant must be enforced in production, not just here.
check(grepl("declared window is Active-phase only", src, fixed = TRUE),
      "F: production must hard-fail if any Inactive row is selected")

cat("Stage 09 primary window contract checks: PASS\n")
