# Parity + contract test for the phase-generic acute-window selector.
#
# ACTIVE   : mmm_select_acute_phase_window(phase = "Active") must reproduce the
#            validated canonical first-night selector row-for-row, and must
#            also agree with the Active-only helper it generalizes.
# INACTIVE : the window must start 06:30, end 18:30, be exactly 12 h, follow
#            the first Active block, and never spill into the next Inactive
#            block.
#
# Stage 09 and first_night_window_helpers.R are neither modified nor
# re-implemented; the canonical selector is called directly.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
})

source("Analysis/_pipeline_setup.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("first_night_window_helpers.R")
source_mmm_helper("acute_active_window_helpers.R")
source_mmm_helper("acute_phase_window_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

PROJECT_ROOT <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
BIN_LEVEL <- "10min_based"
BIN_SIZE_SEC <- 600

dat <- read_csv(
  file.path(PROJECT_ROOT, "analysis_ready/03_derived_metrics", BIN_LEVEL, "all_behavior_metrics.csv"),
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE
)
cat("Stage 01 rows:", nrow(dat), "\n\n")

key_cols <- c("AnimalNum", "BinStart")

# ------------------------------------------------------------ 1. Active parity
cat("1. phase-generic Active == canonical first night == Active-only helper\n")
canonical <- mmm_select_first_night_window(dat, bin_size_sec = BIN_SIZE_SEC) %>%
  arrange(across(all_of(key_cols)))
generic_act <- mmm_select_acute_phase_window(dat, BIN_SIZE_SEC, cage_change = "CC1",
                                             phase = "Active") %>%
  arrange(across(all_of(key_cols)))
active_only <- mmm_select_acute_active_window(dat, BIN_SIZE_SEC, cage_change = "CC1") %>%
  arrange(across(all_of(key_cols)))

check(nrow(canonical) == nrow(generic_act),
      sprintf("row counts must match (canonical %d, generic %d)", nrow(canonical), nrow(generic_act)))
check(identical(as.character(canonical$AnimalNum), as.character(generic_act$AnimalNum)),
      "AnimalNum sequence must match canonical")
check(identical(canonical$BinStart, generic_act$BinStart), "BinStart sequence must match canonical")
for (col in c("target_phase_block", "target_window_start", "elapsed_sec_in_window",
              "target_slot", "elapsed_hours_in_window", "Movement")) {
  a <- canonical[[col]]; b <- generic_act[[col]]
  same <- if (is.numeric(a)) isTRUE(all.equal(a, b, tolerance = 0)) else identical(a, b)
  check(same, paste0("value-identical to canonical: ", col))
}
check(identical(active_only$BinStart, generic_act$BinStart),
      "phase-generic Active must equal the Active-only helper")
ok(sprintf("Active parity holds on %d rows, tolerance 0", nrow(generic_act)))

# ---------------------------------------------------------- 2. Inactive clock
cat("\n2. Inactive window clock contract\n")
inact <- mmm_select_acute_phase_window(dat, BIN_SIZE_SEC, cage_change = "CC1", phase = "Inactive")

starts <- unique(inact$target_window_start)
ends <- unique(inact$target_window_end)
check(all(hour(starts) == 6 & minute(starts) == 30),
      paste0("every Inactive window must start 06:30; saw ",
             paste(unique(format(starts, "%H:%M")), collapse = ", ")))
check(all(hour(ends) == 18 & minute(ends) == 30),
      paste0("every Inactive window must end 18:30; saw ",
             paste(unique(format(ends, "%H:%M")), collapse = ", ")))
check(all(as.numeric(difftime(ends, starts, units = "hours")) == 12),
      "every Inactive window must be exactly 12 h")
check(all(inact$TimeHours >= 0 & inact$TimeHours < 12), "TimeHours must lie in [0, 12)")
check(all(mmm_is_inactive_phase(inact$Phase)), "every selected row must be exact-Inactive")
ok(sprintf("Inactive window is 06:30-18:30, exactly 12 h (%d rows, %d animals)",
           nrow(inact), n_distinct(inact$AnimalNum)))

# ------------------------------------------- 3. Inactive follows first Active
cat("\n3. Inactive block follows the first Active block, no spill\n")
act_anchor <- generic_act %>%
  distinct(.data$Batch, .data$target_phase_block) %>%
  rename(active_block = "target_phase_block")
inact_anchor <- inact %>%
  distinct(.data$Batch, .data$target_phase_block) %>%
  rename(inactive_block = "target_phase_block")
joined <- inner_join(act_anchor, inact_anchor, by = "Batch")
check(nrow(joined) > 0L, "anchors must be comparable by Batch")
check(all(joined$inactive_block == joined$active_block + 1L),
      "the Inactive anchor must be exactly one block after the Active anchor")
check(all(joined$active_block %% 2L == 1L), "Active anchor blocks must be odd")
check(all(joined$inactive_block %% 2L == 0L), "Inactive anchor blocks must be even")
check(min(inact$BinStart) > min(generic_act$BinStart),
      "Inactive data must begin after the Active window opens")
ok("Inactive anchor = Active anchor + 1 for every session")

# No spill: every selected row must belong to the anchor block itself.
inact_blocks <- animalpos_phase_block_index(inact$BinStart)
check(identical(as.integer(inact_blocks), as.integer(inact$target_phase_block)),
      "no selected Inactive row may come from a different Inactive block")
dup <- inact %>% count(.data$AnimalNum, .data$target_slot) %>% filter(.data$n > 1L)
check(nrow(dup) == 0L, "(AnimalNum, target_slot) must be unique in the Inactive window")
ok("no spill into any neighbouring Inactive block")

# ------------------------------------------------- 4. multi-block Inactive set
cat("\n4. multi-block Inactive selector\n")
iblocks <- mmm_select_cc_phase_blocks(dat, BIN_SIZE_SEC, cage_change = "CC1", phase = "Inactive")
first_block <- iblocks %>% filter(.data$PhaseBlock == 1L) %>% arrange(across(all_of(key_cols)))
inact_sorted <- inact %>% arrange(across(all_of(key_cols)))
check(nrow(first_block) == nrow(inact_sorted),
      sprintf("PhaseBlock 1 must equal the acute Inactive window (%d vs %d)",
              nrow(first_block), nrow(inact_sorted)))
check(identical(first_block$BinStart, inact_sorted$BinStart),
      "PhaseBlock 1 rows must equal the acute Inactive window rows")
check(all(iblocks$TimeWithinPhaseHours >= 0 & iblocks$TimeWithinPhaseHours < 12),
      "every multi-block row must lie on its own 0-12 h axis")
check(all(iblocks$phase_block_index %% 2L == 0L), "all Inactive block indices must be even")
ok(sprintf("%d Inactive blocks in CC1; block 1 == the acute Inactive window",
           n_distinct(iblocks$PhaseBlock)))

# Active multi-block must still agree with the Active-only helper.
ablocks <- mmm_select_cc_phase_blocks(dat, BIN_SIZE_SEC, cage_change = "CC1", phase = "Active")
prev_nights <- mmm_select_cc_active_nights(dat, BIN_SIZE_SEC, cage_change = "CC1")
check(nrow(ablocks) == nrow(prev_nights),
      "multi-block Active must match mmm_select_cc_active_nights row count")
check(n_distinct(ablocks$PhaseBlock) == n_distinct(prev_nights$ActiveNight),
      "multi-block Active must find the same number of blocks")
ok(sprintf("Active multi-block agrees with the existing night selector (%d blocks)",
           n_distinct(ablocks$PhaseBlock)))

cat("\nPASS: acute phase-window parity and Inactive clock contract\n")
