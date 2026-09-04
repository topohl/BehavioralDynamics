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

# The clock-anchored window selector depends on the shared phase-block helpers.
# Repo root: honour MMM_REPO_DIR when set, otherwise discover it by walking up
# from the working directory, exactly as Analysis/_pipeline_setup.R does. The
# previous fallback was a hard-coded Windows clone path, which made this test
# fail anywhere else, including CI.
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "behavioral_dynamics_helpers.R")) &&
           dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
source(file.path(repo, "Functions", "animalpos_preprocessing_helpers.R"))

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
#
# Fixtures are built on the real experimental clock: an Active phase block runs
# 18:30 inclusive to 06:30 exclusive, followed by an Inactive block. The window
# is anchored on that clock, never on the first bin an animal happens to have.
# ------------------------------------------------------------------
BLOCK_START <- as.POSIXct("2022-10-28 18:30:00", tz = "UTC")

# active_slots: 1-based target slot indices within the first Active block.
mk_clock_animal <- function(id, active_slots = 1:72, n_inactive = 72,
                            movement = 1, session = "S1", block_start = BLOCK_START) {
  act <- tibble(
    AnimalNum = id, SourceFile = session, Batch = "B1", System = "sys.1",
    Phase = "Active",
    BinStart = block_start + (active_slots - 1L) * bin_seconds,
    TimeIndex = active_slots - 1L,
    Movement = movement
  )
  inact <- if (n_inactive > 0) tibble(
    AnimalNum = id, SourceFile = session, Batch = "B1", System = "sys.1",
    Phase = "Inactive",
    BinStart = block_start + 12 * 3600 + (seq_len(n_inactive) - 1L) * bin_seconds,
    TimeIndex = 72L + seq_len(n_inactive) - 1L,
    Movement = movement + 100
  ) else NULL
  bind_rows(act, inact)
}

selC <- select_primary_window(mk_clock_animal("A1"))
check(nrow(selC) == 72, paste0("C: expected exactly 72 selected rows, got ", nrow(selC)))
check(all(selC$Phase == "Active"), "C: only Active rows may be selected")
check(sum(normalize_phase_label(selC$Phase) %in% inactive_phase_values) == 0,
      "C: zero Inactive rows must be selected")
check(max(selC$elapsed_sec_in_window) == 71 * bin_seconds,
      "C: last selected bin must start at 11h50m elapsed")
check(identical(sort(selC$target_slot), 1:72), "C: target slots must be exactly 1..72")

# Under the OLD permissive-regex logic the same animal yielded 144 rows.
old_logic <- mk_clock_animal("A1") %>%
  filter(str_detect(str_to_lower(as.character(Phase)), old_pattern)) %>%
  group_by(AnimalNum, Phase) %>% arrange(TimeIndex, .by_group = TRUE) %>%
  mutate(early_rank = row_number()) %>% filter(early_rank <= 72) %>% ungroup()
check(nrow(old_logic) == 144, "C: the old logic is expected to have produced 144 rows (documents the bug)")
check(n_distinct(old_logic$Phase) == 2, "C: the old logic mixed both phases")

# ------------------------------------------------------------------
# D. Window boundary, clock anchoring, and single-block restriction.
# ------------------------------------------------------------------
long_active <- tibble(
  AnimalNum = "B1", SourceFile = "S1", Batch = "B1", System = "sys.1", Phase = "Active",
  BinStart = BLOCK_START + (seq_len(100) - 1L) * bin_seconds,
  TimeIndex = seq_len(100) - 1L, Movement = 1
)
selD <- select_primary_window(long_active)
check(nrow(selD) == 72, paste0("D: a 100-bin Active run must be truncated to 72 bins, got ", nrow(selD)))
check(max(selD$TimeIndex) == 71, "D: TimeIndex 71 (11h50m) must be the last included bin")
check(!72 %in% selD$TimeIndex, "D: the bin at exactly 12 h elapsed must be EXCLUDED")
check(max(selD$elapsed_sec_in_window) < early_window_hours * 3600,
      "D: all selected bins must be strictly under 12 h elapsed")

# The window is anchored on the CLOCK. An animal first observed 5 slots in still
# has its window start at 18:30, and reports slots 6..72 rather than 1..67.
late_animal <- mk_clock_animal("B2", active_slots = 6:72, n_inactive = 0)
selD2 <- select_primary_window(late_animal)
check(nrow(selD2) == 67, paste0("D: a late-starting animal must contribute 67 bins, got ", nrow(selD2)))
check(min(selD2$target_slot) == 6L,
      "D: the window must NOT shift later for an animal whose first observation is late")
check(dplyr::first(selD2$target_window_start) == BLOCK_START,
      "D: the target window start must remain the clock block start")
check(min(selD2$elapsed_sec_in_window) == 5 * bin_seconds,
      "D: elapsed time must be measured from the clock window start")

# Only the FIRST Active block is used, never a later one.
two_blocks <- bind_rows(
  mk_clock_animal("B3", active_slots = 1:72, n_inactive = 0, movement = 1),
  tibble(AnimalNum = "B3", SourceFile = "S1", Batch = "B1", System = "sys.1", Phase = "Active",
         BinStart = BLOCK_START + 24 * 3600 + (seq_len(72) - 1L) * bin_seconds,
         TimeIndex = 144L + seq_len(72) - 1L, Movement = 3)
)
selD3 <- select_primary_window(two_blocks)
check(nrow(selD3) == 72, "D: only the first Active block may be selected")
check(all(selD3$Movement == 1), "D: values must come from the first Active block only")
check(n_distinct(animalpos_phase_block_index(selD3$BinStart)) == 1L,
      "D: exactly one Active phase block may be selected")

# A short first block is not padded from the next one.
short_first <- bind_rows(
  mk_clock_animal("B4", active_slots = 1:50, n_inactive = 0, movement = 1),
  tibble(AnimalNum = "B4", SourceFile = "S1", Batch = "B1", System = "sys.1", Phase = "Active",
         BinStart = BLOCK_START + 24 * 3600 + (seq_len(72) - 1L) * bin_seconds,
         TimeIndex = 144L + seq_len(72) - 1L, Movement = 3)
)
selD4 <- select_primary_window(short_first)
check(nrow(selD4) == 50, "D: a short first Active block must yield only its own bins")
check(all(selD4$Movement == 1), "D: a short window must never bridge into a later block")
check(max(selD4$target_slot) == 50L, "D: trailing target slots must simply be absent, not filled")
check(!any(duplicated(selD4$target_slot)), "D: target slots must remain unique")
check(nrow(selD4) < expected_early_bins_per_animal,
      "D: an incomplete block must remain incomplete rather than being padded")

# ------------------------------------------------------------------
# E. Multi-animal invariant shape (SIS-like): every animal 72 Active, 0 Inactive.
# ------------------------------------------------------------------
many <- bind_rows(lapply(sprintf("A%02d", 1:20), mk_clock_animal))
selE <- select_primary_window(many)
per <- selE %>% count(AnimalNum, name = "n")
check(n_distinct(selE$AnimalNum) == 20, "E: all animals must be retained")
check(all(per$n == 72), "E: every animal must contribute exactly 72 bins")
check(sum(normalize_phase_label(selE$Phase) %in% inactive_phase_values) == 0,
      "E: zero Inactive rows across all animals")
check(nrow(selE) == 20 * 72, "E: total selected rows must equal n_animals * 72")
check(all(selE$target_window_start == BLOCK_START), "E: all animals share the session clock window")

# ------------------------------------------------------------------
# F. Source parity with the production script.
# ------------------------------------------------------------------
src_lines <- readLines("Analysis/09_early_prediction_model_ladder.R", warn = FALSE)
src <- paste(src_lines, collapse = "\n")
code_lines <- src_lines[!grepl("^\\s*#", src_lines)]
code <- paste(code_lines, collapse = "\n")
check(!grepl("early_phase_pattern", code, fixed = TRUE),
      "F: the permissive phase regex must no longer be active code")
check(!grepl("str_detect(str_to_lower(as.character(Phase))", code, fixed = TRUE),
      "F: phase selection must not use permissive substring matching")
check(grepl("active_phase_values", src, fixed = TRUE),
      "F: production must define an explicit active phase value set")
check(grepl("is_active_phase", src, fixed = TRUE), "F: production must use the explicit classifier")
check(grepl("elapsed_sec_in_window", src, fixed = TRUE), "F: production must select by elapsed time")
check(!grepl("max_early_bins_per_animal", code, fixed = TRUE),
      "F: the row-count-based window definition must be gone")
check(grepl("early_window_contract_summary", src, fixed = TRUE),
      "F: production must emit a window contract summary")
check(grepl("window_contract_status", src, fixed = TRUE),
      "F: production must emit per-animal contract status")
check(grepl("declared window is Active-phase only", src, fixed = TRUE),
      "F: production must hard-fail if any Inactive row is selected")
# The window must be clock-anchored, and completeness must be QC rather than an
# invariant, so absent leading observation is never fabricated.
check(grepl("target_phase_block", code, fixed = TRUE),
      "F: production must anchor the window on a clock phase block")
check(grepl("target_slot", code, fixed = TRUE), "F: production must index fixed target slots")
check(grepl("structural_invariants_pass", code, fixed = TRUE),
      "F: production must report structural invariants separately")
check(grepl("all_target_slots_observed", code, fixed = TRUE),
      "F: production must report target-slot coverage separately from structural validity")
check(!grepl("all_invariants_pass", code, fixed = TRUE),
      "F: the conflated all_invariants_pass flag must be gone")
check(grepl("OK_missing_leading_observation", code, fixed = TRUE),
      "F: leading-only absence must have its own non-failing status")

cat("Stage 09 primary window contract checks: PASS\n")
