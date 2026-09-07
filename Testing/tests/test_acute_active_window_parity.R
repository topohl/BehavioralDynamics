# Parity test: the generalized acute Active-window selector must reproduce the
# validated canonical first-night selector exactly when it is pointed at the
# first cage change.
#
# Neither Stage 09 nor Functions/first_night_window_helpers.R is modified or
# re-implemented here. The canonical selector is called directly, and
# first_night_window_helpers.R is itself held to Stage 09 by
# Testing/tests/test_first_night_window_parity.R, so passing both tests chains
# the generalized selector back to production Stage 09 semantics.
#
# It also asserts the estimand-nesting claim that the three GAMM analyses rely
# on: CC1 ActiveNight 1 (multi-night selector) IS the CC1 acute window IS the
# canonical first night. Those are the same rows, not three datasets.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("Analysis/_pipeline_setup.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("first_night_window_helpers.R")
source_mmm_helper("acute_active_window_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

PROJECT_ROOT <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
BIN_LEVEL <- "10min_based"
BIN_SIZE_SEC <- 600

input_file <- file.path(PROJECT_ROOT, "analysis_ready/03_derived_metrics", BIN_LEVEL,
                        "all_behavior_metrics.csv")
check(file.exists(input_file), paste0("Stage 01 input must exist: ", input_file))

dat <- read_csv(
  input_file,
  col_select = c("AnimalNum", "Batch", "CageChange", "Group", "Sex", "Phase",
                 "BinStart", "BinSizeSec", "Movement", "SourceFile"),
  show_col_types = FALSE, progress = FALSE
)
check(nrow(dat) > 0L, "Stage 01 input must be non-empty")
check(all(dat$BinSizeSec == BIN_SIZE_SEC),
      paste0("every row must carry BinSizeSec == ", BIN_SIZE_SEC))
cat("Stage 01 rows:", nrow(dat), "| animals:", n_distinct(dat$AnimalNum), "\n\n")

# ---------------------------------------------------------------- 1. CC1 parity
cat("1. generalized acute selector vs canonical first-night selector (CC1)\n")

canonical <- mmm_select_first_night_window(dat, bin_size_sec = BIN_SIZE_SEC)
generalized_default <- mmm_select_acute_active_window(dat, bin_size_sec = BIN_SIZE_SEC)
generalized_explicit <- mmm_select_acute_active_window(dat, bin_size_sec = BIN_SIZE_SEC,
                                                       cage_change = "CC1")

check(nrow(canonical) == nrow(generalized_default),
      sprintf("row count must match (canonical %d vs generalized %d)",
              nrow(canonical), nrow(generalized_default)))
check(nrow(generalized_explicit) == nrow(generalized_default),
      "explicit cage_change='CC1' must equal the inferred-first-CC result")
ok(sprintf("row counts identical: %d", nrow(canonical)))

# Row identity on the natural key, then value identity on every shared quantity.
key_cols <- c("AnimalNum", "BinStart")
canon_k <- canonical %>% arrange(across(all_of(key_cols)))
gen_k <- generalized_default %>% arrange(across(all_of(key_cols)))

check(identical(as.character(canon_k$AnimalNum), as.character(gen_k$AnimalNum)),
      "AnimalNum sequence must be identical")
check(identical(canon_k$BinStart, gen_k$BinStart), "BinStart sequence must be identical")
ok("selected row sets are identical on (AnimalNum, BinStart)")

for (col in c("target_phase_block", "target_window_start", "target_window_end",
              "elapsed_sec_in_window", "target_slot", "elapsed_hours_in_window",
              "expected_slots", "Movement")) {
  check(col %in% names(canon_k) && col %in% names(gen_k),
        paste0("both outputs must carry column ", col))
  a <- canon_k[[col]]; b <- gen_k[[col]]
  same <- if (is.numeric(a)) isTRUE(all.equal(a, b, tolerance = 0)) else identical(a, b)
  check(same, paste0("column must be value-identical: ", col))
}
ok("all shared window columns are value-identical (tolerance 0)")

check(identical(unique(as.character(canonical$first_cage_change)),
                unique(as.character(generalized_default$target_cage_change))),
      "cage-change label must agree")

# The two QC functions describe the same window from different code paths, so
# their per-animal coverage must agree exactly.
qc_canon <- mmm_first_night_window_qc(canonical) %>% arrange(.data$AnimalNum)
qc_gen <- mmm_active_window_qc(generalized_default, block_cols = "target_cage_change") %>%
  arrange(.data$AnimalNum)
check(sum(qc_canon$observed_slots) == nrow(canonical),
      "canonical QC observed_slots must account for every selected row")
check(nrow(qc_canon) == nrow(qc_gen), "both QC tables must cover the same animals")
check(identical(as.integer(qc_canon$observed_slots), as.integer(qc_gen$observed_bins)),
      "per-animal observed bin counts must agree between the two QC functions")
check(identical(as.integer(qc_canon$missing_leading_slots), as.integer(qc_gen$leading_gaps)),
      "per-animal leading gaps must agree")
check(identical(as.integer(qc_canon$missing_interior_slots), as.integer(qc_gen$interior_gaps)),
      "per-animal interior gaps must agree")
ok(sprintf("cage-change label agrees; QC agrees for all %d animals", nrow(qc_gen)))

# ------------------------------------------------- 2. ActiveNight 1 == acute CC1
cat("\n2. CC1 ActiveNight 1 == CC1 acute window == canonical first night\n")

nights <- mmm_select_cc_active_nights(dat, bin_size_sec = BIN_SIZE_SEC, cage_change = "CC1")
night1 <- nights %>% filter(.data$ActiveNight == 1L) %>% arrange(across(all_of(key_cols)))

check(nrow(night1) == nrow(canon_k),
      sprintf("ActiveNight 1 row count must equal the canonical first night (%d vs %d)",
              nrow(night1), nrow(canon_k)))
check(identical(as.character(night1$AnimalNum), as.character(canon_k$AnimalNum)),
      "ActiveNight 1 AnimalNum sequence must match the canonical first night")
check(identical(night1$BinStart, canon_k$BinStart),
      "ActiveNight 1 BinStart sequence must match the canonical first night")
check(isTRUE(all.equal(night1$TimeWithinActiveHours, canon_k$elapsed_hours_in_window, tolerance = 0)),
      "ActiveNight 1 clock axis must equal the canonical elapsed_hours_in_window")
check(identical(as.integer(night1$target_slot), as.integer(canon_k$target_slot)),
      "ActiveNight 1 target_slot must equal the canonical target_slot")
ok(sprintf("CC1 ActiveNight 1 is the canonical first night, row for row (%d rows)", nrow(night1)))

# --------------------------------------------------- 3. nights are not overlaid
cat("\n3. multi-night selector keeps nights distinct\n")

n_nights <- n_distinct(nights$ActiveNight)
check(n_nights > 1L, "CC1 must contain more than one Active night in these data")
blocks <- nights %>% distinct(.data$ActiveNight, .data$active_phase_block, .data$block_window_start) %>%
  arrange(.data$ActiveNight)
check(n_distinct(nights$active_phase_block) >= n_nights,
      "each ActiveNight must map to at least one distinct phase block")
check(all(nights$TimeWithinActiveHours >= 0 & nights$TimeWithinActiveHours < 12),
      "every row must lie inside its own 0-12 h block axis")
# The archived failure mode: distinct nights collapsing onto one axis with
# multiple observations per (animal, time). Here each night is separate, so
# (animal, night, slot) must be unique.
dup <- nights %>% count(.data$AnimalNum, .data$ActiveNight, .data$target_slot) %>% filter(.data$n > 1L)
check(nrow(dup) == 0L, "(AnimalNum, ActiveNight, target_slot) must be unique")
ok(sprintf("%d distinct Active nights, each on its own axis, no collisions", n_nights))

# ------------------------------------------------------- 4. AR1 sequence safety
cat("\n4. AR1 sequences never bridge a night, a gap, or an animal\n")

ordered <- mmm_order_for_ar1(nights, block_cols = "ActiveNight")
ar_start <- mmm_build_ar_start(ordered, block_cols = "ActiveNight")
check(length(ar_start) == nrow(ordered), "AR.start must be one flag per row")
check(isTRUE(ar_start[1]), "first row must open a sequence")

seq_id <- cumsum(ar_start)
bridge <- tibble(seq_id = seq_id,
                 animal = as.character(ordered$AnimalNum),
                 night = as.integer(ordered$ActiveNight),
                 slot = as.integer(ordered$target_slot)) %>%
  group_by(.data$seq_id) %>%
  summarise(n_animals = n_distinct(.data$animal),
            n_nights = n_distinct(.data$night),
            contiguous = all(diff(.data$slot) == 1L) || n() == 1L,
            .groups = "drop")

check(all(bridge$n_animals == 1L), "no AR1 sequence may span two animals")
check(all(bridge$n_nights == 1L), "no AR1 sequence may span two Active nights")
check(all(bridge$contiguous), "every AR1 sequence must be slot-contiguous")
ok(sprintf("%d AR1 sequences, all single-animal, single-night, contiguous", nrow(bridge)))

proof <- mmm_ar1_sequence_proof(ordered, ar_start, block_cols = "ActiveNight")
check(isFALSE(proof$bridges_active_night), "proof must record that nights are not bridged")
check(proof$n_sequences >= proof$n_blocks, "sequences cannot be fewer than blocks")
ok(sprintf("sequence proof: %d blocks -> %d sequences (%d gap restarts)",
           proof$n_blocks, proof$n_sequences, proof$n_gap_restarts))

cat("\nPASS: acute Active-window parity\n")
