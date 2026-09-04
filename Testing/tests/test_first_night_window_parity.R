# Parity test: the shared first-night selector must agree exactly with the
# validated Stage 09 production selector.
#
# Stage 09 is NOT modified and NOT executed. Its real function definitions are
# extracted from the source file by parsing it and evaluating only the top-level
# assignments the selector needs. That way the test compares against production
# code rather than against a hand-copied restatement of it.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("Analysis/_pipeline_setup.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("first_night_window_helpers.R")
source_mmm_helper("behavioral_dynamics_helpers.R")
source_mmm_helper("hmm_stage14_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

# ---------------------------------------------------------------- Stage 09 ref
stage09_file <- "Analysis/09_early_prediction_model_ladder.R"
check(file.exists(stage09_file), "Stage 09 source must be present")
# Stage 09 now derives bin_size_min from bin_level, and bin_level honours
# MMM_STAGE09_BIN_LEVEL. These assertions are about the COMMITTED CANONICAL
# DEFAULT, so the override is cleared before parsing; otherwise the result
# would depend on ambient environment.
Sys.unsetenv("MMM_STAGE09_BIN_LEVEL")
stage09_exprs <- parse(stage09_file)
wanted <- c("bin_level", "active_phase_values", "inactive_phase_values", "early_window_hours",
            "bin_size_min", "bin_seconds", "normalize_phase_label", "is_active_phase",
            "select_primary_active_window", "get_first_cage_change")
ref_env <- new.env(parent = globalenv())
found <- character()
for (e in stage09_exprs) {
  if (!is.call(e)) next
  if (!as.character(e[[1]]) %in% c("<-", "=")) next
  target <- tryCatch(as.character(e[[2]]), error = function(err) NA_character_)
  if (length(target) != 1L || is.na(target) || !target %in% wanted) next
  eval(e, envir = ref_env)
  found <- c(found, target)
}
missing_defs <- setdiff(wanted, found)
check(
  length(missing_defs) == 0L,
  paste0("Stage 09 must still define: ", paste(missing_defs, collapse = ", "),
         ". If Stage 09 was refactored, this parity test must be updated deliberately.")
)
check(
  identical(get("early_window_hours", envir = ref_env), MMM_FIRST_NIGHT_WINDOW_HOURS),
  "Stage 09's early_window_hours must equal MMM_FIRST_NIGHT_WINDOW_HOURS"
)
stage09_select <- get("select_primary_active_window", envir = ref_env)
stage09_bin_seconds <- get("bin_seconds", envir = ref_env)

compare_selectors <- function(dat, bin_size_sec, label) {
  # Stage 09 restricts to the first cage change BEFORE calling its selector
  # (09:587-589), so the reference must be given the same pre-filtered input.
  # The shared helper performs that restriction internally, which is the only
  # intentional API difference between the two.
  stage09_first_cc <- get("get_first_cage_change", envir = ref_env)(dat$CageChange)
  ref <- stage09_select(dat[as.character(dat$CageChange) == stage09_first_cc, , drop = FALSE],
                        window_hours = MMM_FIRST_NIGHT_WINDOW_HOURS,
                        bin_size_seconds = bin_size_sec)
  new <- mmm_select_first_night_window(dat, bin_size_sec = bin_size_sec)
  check(identical(stage09_first_cc, mmm_first_cage_change(dat$CageChange)),
        paste0(label, ": first-cage-change inference must agree with Stage 09"))

  key <- function(d) paste(as.character(d$AnimalNum), format(d$BinStart, "%Y-%m-%d %H:%M:%S"), d$target_slot)
  ref_k <- sort(key(ref)); new_k <- sort(key(new))
  check(nrow(ref) == nrow(new),
        paste0(label, ": selected row count must match (Stage 09 ", nrow(ref), " vs shared ", nrow(new), ")"))
  check(identical(ref_k, new_k), paste0(label, ": selected (AnimalNum, BinStart, target_slot) sets must be identical"))
  check(setequal(unique(as.character(ref$AnimalNum)), unique(as.character(new$AnimalNum))),
        paste0(label, ": selected AnimalNum sets must be identical"))
  check(identical(sort(unique(format(ref$target_window_start))), sort(unique(format(new$target_window_start)))),
        paste0(label, ": window START timestamps must be identical"))
  check(identical(sort(unique(format(ref$target_window_end))), sort(unique(format(new$target_window_end)))),
        paste0(label, ": window END timestamps must be identical"))
  j <- ref %>%
    transmute(AnimalNum = as.character(AnimalNum), BinStart, ref_slot = target_slot,
              ref_elapsed = elapsed_sec_in_window) %>%
    inner_join(new %>% transmute(AnimalNum = as.character(AnimalNum), BinStart,
                                 new_slot = target_slot, new_elapsed = elapsed_sec_in_window),
               by = c("AnimalNum", "BinStart"))
  check(nrow(j) == nrow(ref), paste0(label, ": every Stage 09 row must join to a shared-selector row"))
  check(all(j$ref_slot == j$new_slot), paste0(label, ": target_slot must match row-for-row"))
  check(max(abs(j$ref_elapsed - j$new_elapsed)) < 1e-9,
        paste0(label, ": elapsed_sec_in_window must match row-for-row"))
  # Coverage parity, computed per animal from each implementation independently.
  cov_ref <- ref %>% count(AnimalNum, name = "n_ref")
  cov_new <- new %>% count(AnimalNum, name = "n_new")
  cj <- full_join(cov_ref, cov_new, by = "AnimalNum")
  check(all(!is.na(cj$n_ref) & !is.na(cj$n_new) & cj$n_ref == cj$n_new),
        paste0(label, ": per-animal coverage must match"))
  invisible(list(ref = ref, new = new))
}

# ------------------------------------------------------- A. synthetic fixtures
# Anchor: block boundaries are floor((t - 06:30)/12h), so an Active block starts
# at 18:30. Build a session whose first Active bin is at 18:30 on a known date.
mk <- function(start, n, bin_sec, animal, phase = "Active", cc = "CC1", session = "S1") {
  data.frame(
    AnimalNum = animal, Group = "CON", Sex = "Female",
    SourceFile = session, CageChange = cc, Phase = phase,
    BinStart = start + seq_len(n) * bin_sec - bin_sec,
    TimeIndex = seq_len(n) - 1L,
    Movement = 1, Entropy = 0.5, ProximityFraction = 0.1,
    stringsAsFactors = FALSE
  )
}
w_start <- as.POSIXct("2023-01-29 18:30:00", tz = "UTC")
BS <- 600L

# Animal A: complete night 1 (72 slots) plus a full night 2 that must be ignored.
a1 <- mk(w_start, 72, BS, "A")
a2 <- mk(w_start + 24 * 3600, 72, BS, "A")
# Animal B: night-1 slots 1-5 MISSING; night 2 fully observed.
b1 <- mk(w_start + 5 * BS, 67, BS, "B")
b2 <- mk(w_start + 24 * 3600, 72, BS, "B")
# Animal C: Inactive rows only inside the window -- must select nothing for C.
c1 <- mk(w_start, 72, BS, "C", phase = "Inactive")
fixture <- bind_rows(a1, a2, b1, b2, c1, mk(w_start, 12, BS, "C", phase = "Active"))

res <- compare_selectors(fixture, BS, "A: synthetic fixture")

sel <- res$new
check(all(sel$BinStart < w_start + 12 * 3600), "A: no selected row may fall outside the first 12 h")
check(!any(sel$BinStart >= w_start + 24 * 3600), "A: NO night-2 row may be selected")
qc <- mmm_first_night_window_qc(sel)
qcA <- qc[qc$AnimalNum == "A", ]; qcB <- qc[qc$AnimalNum == "B", ]; qcC <- qc[qc$AnimalNum == "C", ]
check(qcA$observed_slots == 72 && isTRUE(all.equal(qcA$coverage_fraction, 1)), "A: animal A must be complete")
check(qcB$observed_slots == 67, "A: animal B must have 67 observed slots (5 missing)")
check(qcB$coverage_fraction < 1, "A: animal B coverage must be < 1")
check(qcB$missing_leading_slots == 5L, "A: animal B must show 5 MISSING LEADING slots")
check(qcB$missing_interior_slots == 0L, "A: animal B has no interior gap")
check(qcB$first_target_slot == 6L,
      "A: animal B's x-axis must begin at the TRUE elapsed slot 6, not be zero-shifted")
check(min(sel$elapsed_hours_in_window[sel$AnimalNum == "B"]) > 0.8,
      "A: animal B's first elapsed hour must reflect the real 50-min offset")
check(nrow(qcC) == 1 && qcC$observed_slots == 12L,
      "A: animal C must contribute only its 12 exact-Active rows, never its Inactive rows")

# Inactive rows must never be admitted, even though "inactive" contains "active".
check(!any(mmm_is_active_phase(fixture$Phase[fixture$Phase == "Inactive"])),
      "A: Inactive fixture rows must fail the exact Active predicate")

# ------------------------------------------- B. current 111-animal production data
project_root <- getOption("mmm.project_root",
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID")
for (res_label in c("10min_based", "5min_based")) {
  bs <- if (res_label == "10min_based") 600 else 300
  f <- file.path(project_root, "analysis_ready/03_derived_metrics", res_label, "all_behavior_metrics.csv")
  if (!file.exists(f)) {
    message("SKIP live parity for ", res_label, ": input not available at ", f)
    next
  }
  live <- read_csv(f, col_types = cols(AnimalNum = col_character(), BinStart = col_datetime(),
                                       .default = col_guess()), progress = FALSE)
  roster <- build_canonical_identity_roster(
    live %>% select(AnimalNum, Group, Sex), paste0("Stage 01 ", res_label, " roster"))
  live <- live %>% mutate(AnimalNum = canonical_animal_id(AnimalNum)) %>%
    semi_join(roster, by = "AnimalNum")
  out <- compare_selectors(live, bs, paste0("B: live ", res_label))
  q <- mmm_first_night_window_qc(out$new)
  check(nrow(q) == nrow(roster),
        paste0("B: ", res_label, " must yield a window for every canonical animal (",
               nrow(q), " of ", nrow(roster), ")"))
  check(all(q$missing_interior_slots == 0L),
        paste0("B: ", res_label, " must show no interior missing slots"))
  check(all(q$coverage_fraction > 0.9),
        paste0("B: ", res_label, " coverage must exceed 0.9 for every animal"))
  cat(sprintf("  live parity %s: %d rows, %d animals, coverage %.4f-%.4f\n",
              res_label, nrow(out$new), nrow(q), min(q$coverage_fraction), max(q$coverage_fraction)))
}

cat("First-night window parity checks: PASS\n")
