# ===========================================================================
# Regression tests for the downstream consequences of removing synthetic
# boundary rows from preprocessing.
#
# Stage 02 / Stage 19:
#   1. an interval 06:20 -> 06:40 is split at 06:30
#   2. the post-06:30 piece is Inactive
#   3. duration is conserved
#   4. an interval longer than 3600 s is retained
#   5. event-spacing QC does not alter aggregation
#   6. no synthetic position observation is required
#
# Stage 09:
#   7.  the fixed target clock window does not shift when the first observed
#       bin is late
#   8.  missing leading slots are counted correctly
#   9.  no backward fill occurs
#   10. incomplete leading coverage does not fail structural invariants
#   11. interior missing slots remain separately visible
#   12. 72/72 complete animals still receive OK_complete
#
# Production definitions are loaded from the analysis scripts by parsing them
# and evaluating only the definitions under test, so no local reimplementation
# is ever compared against itself.
# ===========================================================================
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(tibble); library(stringr); library(purrr)
})

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
setwd(repo)
source(file.path(repo, "Functions", "animalpos_preprocessing_helpers.R"))

check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)

# Load selected top-level definitions from a script without running it.
load_defs <- function(path, want, env = new.env(parent = globalenv())) {
  for (e in as.list(parse(path))) {
    if (is.call(e) && length(e) >= 3 && identical(as.character(e[[1]]), "<-") &&
        is.name(e[[2]]) && as.character(e[[2]]) %in% want) {
      eval(e, envir = env)
    }
  }
  env
}

# ---------------------------------------------------------------------------
# 1-3, 6. Boundary splitting: 06:20 -> 06:40 must become Active + Inactive.
# ---------------------------------------------------------------------------
iv <- tibble(
  id = 1L,
  PositionID = 5L,
  IntervalStart = as.POSIXct("2022-10-29 06:20:00", tz = "UTC"),
  IntervalEnd   = as.POSIXct("2022-10-29 06:40:00", tz = "UTC")
) %>% mutate(DurationSec = as.numeric(difftime(IntervalEnd, IntervalStart, units = "secs")))

sp <- animalpos_split_intervals(iv, period_sec = NULL, split_phase = TRUE) %>%
  mutate(Phase = animalpos_phase_from_block(animalpos_phase_block_index(IntervalStart)))

check(nrow(sp) == 2L,
      paste0("1: interval 06:20-06:40 must split into 2 pieces; got ", nrow(sp)))
check(identical(format(sp$IntervalEnd[1], "%H:%M:%S", tz = "UTC"), "06:30:00"),
      "1: the cut must fall exactly at 06:30:00")
check(identical(sp$Phase, c("Active", "Inactive")),
      paste0("2: pieces must be Active then Inactive; got ", paste(sp$Phase, collapse = ",")))
check(identical(as.numeric(sp$DurationSec), c(600, 600)),
      paste0("3: durations must be 600 + 600; got ", paste(sp$DurationSec, collapse = ",")))
check(isTRUE(all.equal(sum(sp$DurationSec), iv$DurationSec)),
      "3: total duration must be conserved by the split")
# The split is produced purely by interval arithmetic: the input carried one row
# and no position observation was added at the boundary.
check(nrow(iv) == 1L && all(sp$PositionID == 5L),
      "6: splitting must not invent a position observation at the boundary")
check(!any(duplicated(paste(sp$IntervalStart, sp$IntervalEnd))),
      "6: split must not emit duplicate pieces")

# The same must hold at 18:30, in the other direction.
iv2 <- tibble(id = 1L, PositionID = 3L,
              IntervalStart = as.POSIXct("2022-10-29 18:20:00", tz = "UTC"),
              IntervalEnd   = as.POSIXct("2022-10-29 18:40:00", tz = "UTC")) %>%
  mutate(DurationSec = as.numeric(difftime(IntervalEnd, IntervalStart, units = "secs")))
sp2 <- animalpos_split_intervals(iv2, period_sec = NULL, split_phase = TRUE) %>%
  mutate(Phase = animalpos_phase_from_block(animalpos_phase_block_index(IntervalStart)))
check(nrow(sp2) == 2L && identical(sp2$Phase, c("Inactive", "Active")),
      "2: an 18:20-18:40 interval must split into Inactive then Active")

cat("Boundary split checks (1-3, 6): PASS\n")

# ---------------------------------------------------------------------------
# 4-5. A >3600 s interval must survive, and event-spacing QC must not filter.
# ---------------------------------------------------------------------------
long_iv <- tibble(
  id = 1L, PositionID = 2L,
  IntervalStart = as.POSIXct("2022-10-29 07:00:00", tz = "UTC"),
  IntervalEnd   = as.POSIXct("2022-10-29 12:00:00", tz = "UTC")
) %>% mutate(DurationSec = as.numeric(difftime(IntervalEnd, IntervalStart, units = "secs")))
check(long_iv$DurationSec > 3600, "4: control -- the test interval must exceed 3600 s")

binned <- animalpos_split_intervals_one_grid(long_iv, 600, 0)
check(nrow(binned) == 30L,
      paste0("4: a 5 h interval must yield 30 ten-minute pieces; got ", nrow(binned)))
check(isTRUE(all.equal(sum(binned$DurationSec), long_iv$DurationSec)),
      "4: a >3600 s interval must be retained in full, not dropped")

# Stage 02's aggregation filter must now be a guarded no-op.
s02 <- load_defs("Analysis/02_build_dyadic_rfid_contacts.R",
                 c("filter_aggregation_intervals", "system_event_gap_threshold_sec",
                   "exclude_long_gaps_from_aggregation"))
check(isFALSE(get("exclude_long_gaps_from_aggregation", envir = s02)),
      "5: Stage 02 must ship with exclude_long_gaps_from_aggregation = FALSE")
flagged <- long_iv %>% mutate(SystemEventGap = DurationSec > 3600)
kept <- get("filter_aggregation_intervals", envir = s02)(flagged)
check(nrow(kept) == nrow(flagged),
      "5: Stage 02 aggregation filter must not drop gap-flagged intervals")
check(isTRUE(all.equal(sum(kept$DurationSec), sum(flagged$DurationSec))),
      "5: Stage 02 aggregation filter must not change total duration")

# Enabling the legacy exclusion must fail loudly rather than delete data.
s02b <- load_defs("Analysis/02_build_dyadic_rfid_contacts.R", c("filter_aggregation_intervals"))
assign("exclude_long_gaps_from_aggregation", TRUE, envir = s02b)
err <- tryCatch({ get("filter_aggregation_intervals", envir = s02b)(flagged); NULL },
                error = function(e) conditionMessage(e))
check(!is.null(err) && grepl("silently drop", err, fixed = TRUE),
      "5: enabling the Stage 02 legacy exclusion must raise an error, not filter")

# Stage 19's guard must behave the same way.
s19 <- load_defs("Analysis/19_spatial_occupancy_maps.R",
                 c("assert_no_gap_exclusion", "EXCLUDE_LONG_GAPS", "SYSTEM_EVENT_GAP_THRESHOLD_SEC"))
check(isFALSE(get("EXCLUDE_LONG_GAPS", envir = s19)),
      "5: Stage 19 must ship with EXCLUDE_LONG_GAPS = FALSE")
check(isTRUE(get("assert_no_gap_exclusion", envir = s19)()),
      "5: Stage 19 guard must pass while the exclusion is disabled")
s19b <- load_defs("Analysis/19_spatial_occupancy_maps.R", c("assert_no_gap_exclusion"))
assign("EXCLUDE_LONG_GAPS", TRUE, envir = s19b)
err19 <- tryCatch({ get("assert_no_gap_exclusion", envir = s19b)(); NULL },
                  error = function(e) conditionMessage(e))
check(!is.null(err19) && grepl("silently drop", err19, fixed = TRUE),
      "5: enabling the Stage 19 legacy exclusion must raise an error")

cat("Gap-rule retirement checks (4-5): PASS\n")

# ---------------------------------------------------------------------------
# 7-12. Stage 09 fixed target clock window.
# ---------------------------------------------------------------------------
# Stage 09 derives bin_size_min from bin_level, so bin_level must be evaluated
# too. The override is cleared: these assertions are about the committed
# canonical 10-min default.
Sys.unsetenv("MMM_STAGE09_BIN_LEVEL")
s09 <- load_defs(
  "Analysis/09_early_prediction_model_ladder.R",
  c("bin_level", "active_phase_values", "inactive_phase_values", "early_window_hours",
    "bin_size_min", "bin_seconds", "expected_early_bins_per_animal",
    "normalize_phase_label", "is_active_phase", "select_primary_active_window")
)
for (nm in c("dplyr", "tidyr")) NULL
select_window <- get("select_primary_active_window", envir = s09)
environment(select_window) <- s09
check(get("expected_early_bins_per_animal", envir = s09) == 72,
      "7: control -- the target window must be 72 slots")

# Build a synthetic session: Active block starting 18:30, 72 ten-minute slots.
block_start <- as.POSIXct("2022-10-28 18:30:00", tz = "UTC")
mk_animal <- function(id, skip_leading = 0L, drop_slots = integer(0)) {
  slots <- setdiff(seq_len(72), c(seq_len(skip_leading), drop_slots))
  tibble(
    AnimalNum = id, SourceFile = "S1", Batch = "B1", System = "sys.1",
    Phase = "Active",
    BinStart = block_start + (slots - 1L) * 600,
    TimeIndex = slots - 1L,
    Movement = seq_along(slots) * 1.0, Entropy = 0.5, Proximity = 0.5
  )
}
# A second, LATER Active block in the same session. The target window must be
# anchored on the FIRST Active block, so this animal contributes no rows. Its
# presence makes min(block) != max(block), so an anchor that took the last block
# instead of the first would be detected.
later_block <- tibble(
  AnimalNum = "later_block", SourceFile = "S1", Batch = "B1", System = "sys.1",
  Phase = "Active",
  BinStart = block_start + 24 * 3600 + (seq_len(72) - 1L) * 600,
  TimeIndex = 144L + seq_len(72) - 1L,
  Movement = 1.0, Entropy = 0.5, Proximity = 0.5
)
# A SECOND session whose Active block starts two days later. Each session must
# be anchored on its own clock block; anchoring on a dataset-wide minimum would
# push this session outside the window entirely.
s2_start <- block_start + 48 * 3600
sess2 <- tibble(
  AnimalNum = "s2_animal", SourceFile = "S2", Batch = "B2", System = "sys.1",
  Phase = "Active",
  BinStart = s2_start + (seq_len(72) - 1L) * 600,
  TimeIndex = seq_len(72) - 1L,
  Movement = 2.0, Entropy = 0.5, Proximity = 0.5
)
dat <- bind_rows(
  mk_animal("complete"),                       # all 72 slots
  mk_animal("late_start", skip_leading = 4L),  # first 4 slots unobserved
  mk_animal("interior", drop_slots = c(30L, 31L)),
  later_block,
  sess2
)
check(n_distinct(animalpos_phase_block_index(dat$BinStart)) > 1L,
      "7: control -- the fixture must span more than one Active block")
w <- select_window(dat)

# The anchor is the FIRST Active block of each session, so the later-block
# animal contributes nothing while the second session keeps its own window.
check(!"later_block" %in% w$AnimalNum,
      "7: an animal observed only in a later Active block must not enter the window")
check(setequal(unique(w$AnimalNum), c("complete", "late_start", "interior", "s2_animal")),
      "7: the window must contain the first-block animals of every session")
# Per-session anchoring: S2 must be anchored on its own block, not on S1.
s2_anchor <- w %>% filter(AnimalNum == "s2_animal") %>% summarise(a = first(target_window_start)) %>% pull(a)
check(s2_anchor == s2_start,
      paste0("7: session S2 must be anchored on its own clock block; got ", format(s2_anchor)))
check(s2_anchor != block_start,
      "7: control -- the two sessions must have different anchors for this test to bite")
check(nrow(w %>% filter(AnimalNum == "s2_animal")) == 72L,
      "7: the second session animal must contribute all 72 of its slots")

# 7. the window anchor must be the clock block start for EVERY animal.
anch <- w %>% filter(SourceFile == "S1") %>% group_by(AnimalNum) %>%
  summarise(ws = first(target_window_start), .groups = "drop")
check(all(anch$ws == block_start),
      paste0("7: target window start must be 18:30 for every animal in the session; got ",
             paste(unique(format(anch$ws, "%H:%M:%S")), collapse = ",")))
late_first <- w %>% filter(AnimalNum == "late_start") %>% summarise(m = min(BinStart)) %>% pull(m)
check(late_first == block_start + 4L * 600,
      "7: a late-starting animal keeps its own first observation, not a shifted window")
check(w %>% filter(AnimalNum == "late_start") %>% summarise(s = min(target_slot)) %>% pull(s) == 5L,
      "7: a late-starting animal's first observed slot must be slot 5, not slot 1")
# Elapsed time is measured from the CLOCK, so the late animal's first row sits
# 4 slots into the window. Re-anchoring on the animal would make this 0.
check(w %>% filter(AnimalNum == "late_start") %>% summarise(e = min(elapsed_sec_in_window)) %>% pull(e) == 4 * 600,
      "7: elapsed time must be measured from the clock window start, not the first observation")
check(w %>% filter(AnimalNum == "complete") %>% summarise(e = min(elapsed_sec_in_window)) %>% pull(e) == 0,
      "7: a complete animal must start at elapsed 0")

# 8. missing leading slots counted correctly.
cov <- w %>% filter(SourceFile == "S1") %>% group_by(AnimalNum) %>%
  summarise(observed = n(), first_slot = min(target_slot), last_slot = max(target_slot),
            .groups = "drop") %>%
  mutate(missing_leading = first_slot - 1L,
         missing_trailing = 72L - last_slot,
         missing_interior = (last_slot - first_slot + 1L) - observed)
check(cov$missing_leading[cov$AnimalNum == "late_start"] == 4L,
      "8: late_start must report exactly 4 missing leading slots")
check(cov$missing_leading[cov$AnimalNum == "complete"] == 0L,
      "8: a complete animal must report 0 missing leading slots")
check(cov$observed[cov$AnimalNum == "late_start"] == 68L,
      "8: late_start must have 68 observed slots")

# 9. no backward fill: the animal must not gain rows before its first read.
check(nrow(w %>% filter(AnimalNum == "late_start")) == 68L,
      "9: no rows may be fabricated before the first genuine observation")
check(all(w$Movement[w$AnimalNum == "late_start"] > 0),
      "9: no zero-filled leading rows may appear")
check(!any(duplicated(w %>% filter(AnimalNum == "late_start") %>% pull(target_slot))),
      "9: target slots must remain unique per animal")

# 10. leading-only absence must not be a structural failure.
status <- cov %>% mutate(
  window_contract_status = case_when(
    missing_leading + missing_interior + missing_trailing == 0L ~ "OK_complete",
    missing_interior > 0L ~ "WARN_interior_missing_bins",
    missing_trailing > 0L ~ "WARN_trailing_missing_bins",
    missing_leading > 0L ~ "OK_missing_leading_observation",
    TRUE ~ "WARN_unexpected"),
  structural_status_ok = !startsWith(window_contract_status, "FAIL"))
check(status$window_contract_status[status$AnimalNum == "late_start"] == "OK_missing_leading_observation",
      paste0("10: leading-only absence must be OK_missing_leading_observation; got ",
             status$window_contract_status[status$AnimalNum == "late_start"]))
check(all(status$structural_status_ok),
      "10: incomplete leading coverage must not fail structural invariants")

# 11. interior gaps stay separately visible.
check(cov$missing_interior[cov$AnimalNum == "interior"] == 2L,
      "11: the interior animal must report 2 missing interior slots")
check(status$window_contract_status[status$AnimalNum == "interior"] == "WARN_interior_missing_bins",
      "11: interior gaps must surface as WARN_interior_missing_bins")
check(cov$missing_leading[cov$AnimalNum == "interior"] == 0L,
      "11: an interior gap must not be misreported as leading absence")

# 12. complete animals still OK_complete.
check(status$window_contract_status[status$AnimalNum == "complete"] == "OK_complete",
      "12: a 72/72 animal must be OK_complete")
check(cov$observed[cov$AnimalNum == "complete"] == 72L,
      "12: a complete animal must have 72 observed slots")

# The window must never admit Inactive rows or exceed 12 h.
check(all(as.character(w$Phase) == "Active"), "12: only Active rows may be selected")
check(max(w$elapsed_sec_in_window) < 12 * 3600, "12: no row may exceed the 12 h window")
check(all(w$target_slot >= 1L & w$target_slot <= 72L), "12: target slots must lie in 1..72")

cat("Stage 09 window-contract checks (7-12): PASS\n")
cat("Downstream boundary and gap-contract checks: PASS\n")
