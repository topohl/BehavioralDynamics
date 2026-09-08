# Contract test for the Inactive-phase Markov state construction.
#
# PrevState must be START at the first scheduled slot of every window AND after
# every real missing-slot gap, and must otherwise reflect the immediately
# preceding CONTIGUOUS slot. It must never bridge animals, windows, blocks,
# cage changes or gaps.
#
# Tested first on a synthetic fixture with known answers (including a
# deliberately punched gap), then on the real CC1 Inactive window.

suppressPackageStartupMessages({ library(dplyr); library(readr); library(tibble) })

source("Analysis/_pipeline_setup.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("acute_active_window_helpers.R")
source_mmm_helper("acute_phase_window_helpers.R")
source_mmm_helper("gamm_group_inference_helpers.R")
source_mmm_helper("gamm_auc_helpers.R")
source_mmm_helper("inactive_markov_gamm_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

# ------------------------------------------------------------ 1. synthetic
cat("1. synthetic fixture with a known gap and two blocks\n")
fx <- tribble(
  ~AnimalNum, ~Block, ~target_slot, ~Movement,
  # animal A, block 1: slots 1..5 contiguous. Movement 0,2,0,0,4
  "A", 1L, 1L, 0,   "A", 1L, 2L, 2,   "A", 1L, 3L, 0,   "A", 1L, 4L, 0,  "A", 1L, 5L, 4,
  # animal A, block 2: slot 1, then a GAP (3 missing), then slot 5
  "A", 2L, 1L, 1,   "A", 2L, 5L, 3,
  # animal B, block 1: slots 1..3
  "B", 1L, 1L, 0,   "B", 1L, 2L, 0,   "B", 1L, 3L, 7
)
st <- mmm_build_prev_state(fx, block_cols = "Block")

expected <- c(
  "START", "INACTIVE", "ACTIVE", "INACTIVE", "INACTIVE",   # A block1
  "START", "START",                                        # A block2 (gap -> START)
  "START", "INACTIVE", "INACTIVE"                          # B block1
)
got <- as.character(st$PrevState)
check(identical(got, expected),
      paste0("PrevState must match the hand-computed answer.\n  expected: ",
             paste(expected, collapse = ","), "\n  got:      ", paste(got, collapse = ",")))
ok("PrevState is exactly correct on the fixture, including the gap -> START")

check(identical(as.integer(st$ActiveBin), c(0L,1L,0L,0L,1L, 1L,1L, 0L,0L,1L)),
      "ActiveBin must be 1{Movement > 0}")
ok("ActiveBin = 1{Movement > 0}")

# The row after the gap must be START even though the previous row was ACTIVE.
gaprow <- st %>% filter(.data$AnimalNum == "A", .data$Block == 2L, .data$target_slot == 5L)
check(gaprow$PrevState == "START", "the first row after a missing-slot gap must be START")
# The first row of animal B must be START even though animal A ended ACTIVE.
brow <- st %>% filter(.data$AnimalNum == "B", .data$target_slot == 1L)
check(brow$PrevState == "START", "a new animal must not inherit the previous animal's state")
ok("no bridging across gaps, blocks or animals")

pf <- mmm_prev_state_proof(st, block_cols = "Block")
check(pf$every_block_starts_with_START, "every block must open with START")
check(pf$start_count_matches,
      sprintf("START count must equal blocks + gaps (got %d, expected %d)",
              pf$n_START, pf$n_START_expected))
ok(sprintf("proof: %d blocks, %d START (= blocks + gaps)", pf$n_blocks, pf$n_START))

# ------------------------------------------------------------ 2. real data
cat("\n2. real CC1 Inactive window\n")
PROJECT_ROOT <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
dat <- read_csv(file.path(PROJECT_ROOT, "analysis_ready/03_derived_metrics/10min_based",
                          "all_behavior_metrics.csv"),
                col_select = c("AnimalNum","Batch","CageChange","Group","Sex","Phase",
                               "BinStart","BinSizeSec","Movement","SourceFile"),
                show_col_types = FALSE, progress = FALSE)
w <- mmm_select_acute_phase_window(dat, 600, cage_change = "CC1", phase = "Inactive")
ws <- mmm_build_prev_state(w, block_cols = "target_cage_change")

check(all(levels(ws$PrevState) == MMM_PREV_STATE_LEVELS), "PrevState must have exactly 3 levels")
check(!any(is.na(ws$PrevState)), "PrevState must never be NA")
pf2 <- mmm_prev_state_proof(ws, block_cols = "target_cage_change")
check(pf2$every_block_starts_with_START, "every real window must open with START")
check(pf2$start_count_matches, "real START count must equal windows + gaps")
# Coverage is complete in this window, so there should be exactly one START per animal.
check(pf2$n_START == dplyr::n_distinct(ws$AnimalNum),
      sprintf("with complete coverage START must equal n animals (%d vs %d)",
              pf2$n_START, dplyr::n_distinct(ws$AnimalNum)))
ok(sprintf("%d rows, %d blocks, %d START, %d INACTIVE, %d ACTIVE",
           pf2$n_rows, pf2$n_blocks, pf2$n_START, pf2$n_INACTIVE, pf2$n_ACTIVE))

tr <- mmm_observed_transitions(ws)
p01 <- tr %>% filter(.data$PrevState == "INACTIVE") %>% summarise(p = weighted.mean(.data$p_active, .data$n)) %>% pull(.data$p)
p11 <- tr %>% filter(.data$PrevState == "ACTIVE") %>% summarise(p = weighted.mean(.data$p_active, .data$n)) %>% pull(.data$p)
check(p01 > 0 && p01 < 0.2, sprintf("activation probability should be small (got %.4f)", p01))
check(p11 > 0.3 && p11 < 0.7, sprintf("persistence probability should be moderate (got %.4f)", p11))
check(p11 > p01, "persistence must exceed activation - the process must be state dependent")
ok(sprintf("observed activation p01 = %.4f, persistence p11 = %.4f", p01, p11))

# ------------------------------------------------------------ 3. recursion sanity
cat("\n3. marginal recursion sanity\n")
# With constant transition probabilities the recursion must converge to the
# stationary probability p01 / (1 - p11 + p01).
n <- 72L
p01c <- 0.062; p11c <- 0.5
p <- numeric(n); p[1] <- p01c
for (t in 2:n) p[t] <- p01c * (1 - p[t - 1]) + p11c * p[t - 1]
stat_p <- p01c / (1 - p11c + p01c)
check(abs(p[n] - stat_p) < 1e-8,
      sprintf("recursion must reach the stationary value (%.6f vs %.6f)", p[n], stat_p))
ok(sprintf("recursion converges to the stationary probability %.4f", stat_p))
check(all(p >= 0 & p <= 1), "marginal probability must stay in [0, 1]")
ok("marginal probability stays in [0, 1]")

cat("\nPASS: Inactive Markov state construction and recursion\n")
