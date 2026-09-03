## CANONICAL FIRST-NIGHT TIME ANCHOR
## Implements the Stage 09 window contract FROM CODE (Analysis/09_early_prediction_model_ladder.R,
## select_primary_active_window): the first Active phase block after the first cage change,
## 18:30 inclusive -> 06:30 exclusive, 12 h, anchored to the experimental clock (per session),
## never shifted to an animal's first read.
## The Stage 09 artifacts on disk are STALE (written by the archived 08_early_prediction_models.R):
## 113 animals, zero-padded IDs, both phases, all 4 CCs, n_early_bins = 4. Not usable as a reference.
suppressMessages({library(dplyr); library(tidyr); library(readr); library(stringr); library(purrr)})
setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("hmm_stage14_helpers.R")

## Verbatim copy of Stage 09 get_first_cage_change (Analysis/09_early_prediction_model_ladder.R:288)
get_first_cage_change <- function(x) {
  ux <- unique(as.character(x))
  cc_num <- suppressWarnings(as.numeric(str_extract(ux, "[0-9]+")))
  if (any(is.finite(cc_num))) ux[which.min(ifelse(is.finite(cc_num), cc_num, Inf))] else sort(ux)[1]
}
OUT <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID/analysis_ready/12_systems_neuroscience_summary/5min_based/audit_hmm_state_architecture/first_night_domain_heatmap"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
PROJ <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"

cat("Constants: ANIMALPOS_INACTIVE_START_SEC =", ANIMALPOS_INACTIVE_START_SEC, "(=",
    ANIMALPOS_INACTIVE_START_SEC/3600, "h = 06:30);  PHASE_LENGTH_SEC =", ANIMALPOS_PHASE_LENGTH_SEC,
    "(=", ANIMALPOS_PHASE_LENGTH_SEC/3600, "h)\n")

## Stage 09's EXACT phase rule: explicit membership, never a substring regex.
active_phase_values <- c("active", "dark", "night")
is_active_phase <- function(x) str_to_lower(str_trim(as.character(x))) %in% active_phase_values
WINDOW_HOURS <- 12

roster <- build_canonical_identity_roster(
  read_csv(file.path(PROJ,"analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv"),
    col_types = cols(.default=col_skip(), AnimalNum=col_character(), Group=col_character(), Sex=col_character()),
    progress = FALSE), "roster")

anchor_tbl <- list(); design <- list()
for (res in c("10min_based","5min_based")) {
  bs <- if (res=="10min_based") 600 else 300
  d <- read_csv(file.path(PROJ,"analysis_ready/03_derived_metrics",res,"all_behavior_metrics.csv"),
                col_types = cols(AnimalNum=col_character(), BinStart=col_datetime(), .default=col_guess()),
                progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum)) %>%
    semi_join(roster, by = "AnimalNum")
  first_cc <- get_first_cage_change(d$CageChange)
  cat("\n######## ", res, "  first cage change label = ", first_cc, "\n", sep="")

  cc1 <- d %>% filter(as.character(CageChange) == first_cc)
  cat("  CC1 rows:", nrow(cc1), " animals:", n_distinct(cc1$AnimalNum), "\n")
  cat("  CC1 phases present:", paste(sort(unique(as.character(cc1$Phase))), collapse=", "), "\n")
  cat("  CC1 BinStart span:", format(min(cc1$BinStart)), "->", format(max(cc1$BinStart)), "\n")

  ## Stage 09 anchor: per session, first Active phase block within CC1
  act <- cc1 %>% filter(is_active_phase(Phase))
  session_col <- if ("SourceFile" %in% names(act)) "SourceFile" else "Batch"
  anchors <- act %>%
    mutate(.blk = animalpos_phase_block_index(BinStart)) %>%
    group_by(.session = as.character(.data[[session_col]])) %>%
    summarise(target_phase_block = min(.blk, na.rm = TRUE), .groups = "drop") %>%
    mutate(target_window_start = as.POSIXct(
             target_phase_block * ANIMALPOS_PHASE_LENGTH_SEC + ANIMALPOS_INACTIVE_START_SEC,
             origin = "1970-01-01", tz = "UTC"),
           target_window_end = target_window_start + WINDOW_HOURS*3600)
  cat("  sessions:", nrow(anchors), " (column: ", session_col, ")\n", sep="")
  cat("  anchor clock times (UTC):", paste(unique(format(anchors$target_window_start, "%H:%M")), collapse=", "),
      " -> end ", paste(unique(format(anchors$target_window_end, "%H:%M")), collapse=", "), "\n")
  print(as.data.frame(anchors %>% transmute(session=.session, target_phase_block,
        start=format(target_window_start), end=format(target_window_end))), row.names=FALSE)

  win <- act %>%
    mutate(.session = as.character(.data[[session_col]])) %>%
    left_join(anchors, by = ".session") %>%
    mutate(elapsed_sec_in_window = as.numeric(difftime(BinStart, target_window_start, units="secs")),
           target_slot = as.integer(elapsed_sec_in_window %/% bs) + 1L) %>%
    filter(elapsed_sec_in_window >= 0, elapsed_sec_in_window < WINDOW_HOURS*3600)
  expected_slots <- WINDOW_HOURS*3600/bs
  cat("  expected slots in window:", expected_slots, "; rows retained:", nrow(win),
      "; animals:", n_distinct(win$AnimalNum), "\n")

  ## ordering-based "first contiguous Active block" (the heuristic we are REPLACING)
  step <- median(diff(sort(unique(act$TimeIndex))), na.rm = TRUE)
  blocks <- act %>% group_by(AnimalNum) %>% arrange(TimeIndex, .by_group=TRUE) %>%
    mutate(block = cumsum(c(0, diff(TimeIndex)) > 1.5*step) + 1L) %>% ungroup()
  blk1 <- blocks %>% filter(block == 1L)
  ## Stage-14-style local_bin <= 12h/bin heuristic
  lb <- act %>% group_by(AnimalNum) %>% arrange(TimeIndex, .by_group=TRUE) %>%
    mutate(local_bin = row_number()) %>% filter(local_bin <= expected_slots) %>% ungroup()

  per_animal <- roster %>%
    left_join(cc1 %>% group_by(AnimalNum) %>% summarise(CC1_timestamp = min(BinStart), .groups="drop"), by="AnimalNum") %>%
    left_join(win %>% group_by(AnimalNum) %>%
      summarise(first_active_start = min(BinStart), first_active_end = max(BinStart) + bs,
                target_window_start = first(target_window_start), target_window_end = first(target_window_end),
                n_bins_window = n(), .groups="drop"), by="AnimalNum") %>%
    left_join(blk1 %>% group_by(AnimalNum) %>%
      summarise(n_bins_block1 = n(), block1_start = min(BinStart), block1_end = max(BinStart)+bs, .groups="drop"), by="AnimalNum") %>%
    left_join(lb %>% group_by(AnimalNum) %>% summarise(n_bins_localbin = n(), .groups="drop"), by="AnimalNum") %>%
    mutate(resolution = res, bin_size_sec = bs, expected_bins = expected_slots,
           duration_hours = as.numeric(difftime(target_window_end, target_window_start, units="hours")),
           coverage_fraction = n_bins_window / expected_slots,
           is_exactly_12h = abs(duration_hours - 12) < 1e-9,
           block1_equals_window = coalesce(n_bins_block1 == n_bins_window, FALSE),
           localbin_equals_window = coalesce(n_bins_localbin == n_bins_window, FALSE))
  anchor_tbl[[res]] <- per_animal

  cat("\n  --- window vs heuristics (n animals) ---\n")
  cat("   animals with any window rows      :", sum(!is.na(per_animal$n_bins_window)), "/", nrow(per_animal), "\n")
  cat("   window duration exactly 12 h      :", sum(per_animal$is_exactly_12h, na.rm=TRUE), "\n")
  cat("   n_bins_window: median", median(per_animal$n_bins_window, na.rm=TRUE),
      " range [", min(per_animal$n_bins_window, na.rm=TRUE), ",", max(per_animal$n_bins_window, na.rm=TRUE), "]",
      " of", expected_slots, "expected\n")
  cat("   coverage_fraction: median", round(median(per_animal$coverage_fraction, na.rm=TRUE),4),
      " min", round(min(per_animal$coverage_fraction, na.rm=TRUE),4), "\n")
  cat("   animals with coverage < 1         :", sum(per_animal$coverage_fraction < 1, na.rm=TRUE), "\n")
  cat("   FIRST-CONTIGUOUS-BLOCK == window  :", sum(per_animal$block1_equals_window), "/", nrow(per_animal), "\n")
  cat("   local_bin<=N heuristic == window  :", sum(per_animal$localbin_equals_window), "/", nrow(per_animal), "\n")
  cat("   n_bins_block1: median", median(per_animal$n_bins_block1, na.rm=TRUE),
      " | n_bins_localbin: median", median(per_animal$n_bins_localbin, na.rm=TRUE), "\n")
  design[[res]] <- tibble(resolution=res, bin_size_sec=bs, window_hours=WINDOW_HOURS,
    expected_bins=expected_slots, anchor_rule="Stage 09 select_primary_active_window: per-session first Active phase block after first cage change; start = block*12h + 06:30; keep elapsed in [0,12h)",
    anchor_clock_start=paste(unique(format(anchors$target_window_start,"%H:%M")),collapse="|"),
    anchor_clock_end=paste(unique(format(anchors$target_window_end,"%H:%M")),collapse="|"),
    phase_rule="exact membership in c('active','dark','night'); never substring regex",
    n_sessions=nrow(anchors), n_animals_with_window=sum(!is.na(per_animal$n_bins_window)),
    median_bins=median(per_animal$n_bins_window, na.rm=TRUE),
    n_block1_equals_window=sum(per_animal$block1_equals_window),
    n_localbin_equals_window=sum(per_animal$localbin_equals_window))
}
at <- bind_rows(anchor_tbl)
out <- at %>% transmute(AnimalNum, Group, Sex, resolution, CC1_timestamp,
  first_active_start, first_active_end,
  target_window_start, target_window_end, duration_hours, is_exactly_12h,
  n_bins_window, expected_bins, coverage_fraction,
  n_bins_block1, block1_equals_window, n_bins_localbin, localbin_equals_window,
  matches_stage09_window = TRUE,
  stage09_reference = "reconstructed from Analysis/09_early_prediction_model_ladder.R select_primary_active_window(); on-disk Stage 09 artifacts are STALE (archived 08_early_prediction_models.R: 113 animals, both phases, all CCs, n_early_bins=4)")
wide <- out %>% select(-c(n_bins_window, expected_bins, coverage_fraction, n_bins_block1, block1_equals_window,
                          n_bins_localbin, localbin_equals_window, duration_hours, is_exactly_12h)) %>%
  distinct(AnimalNum, .keep_all = TRUE) %>% select(-resolution) %>%
  left_join(out %>% filter(resolution=="5min_based") %>% transmute(AnimalNum, n_5min_bins=n_bins_window,
              coverage_fraction_5min=coverage_fraction), by="AnimalNum") %>%
  left_join(out %>% filter(resolution=="10min_based") %>% transmute(AnimalNum, n_10min_bins=n_bins_window,
              coverage_fraction_10min=coverage_fraction, duration_hours, is_exactly_12h), by="AnimalNum")
write_csv(wide, file.path(OUT,"first_night_time_anchor_audit.csv"))
write_csv(out,  file.path(OUT,"first_night_time_anchor_audit_long.csv"))
write_csv(bind_rows(design), file.path(OUT,"first_night_window_contract.csv"))
cat("\n===== per-animal anchor audit written; n rows =", nrow(wide), "=====\n")
print(as.data.frame(head(wide %>% transmute(AnimalNum, Group, Sex,
  CC1=format(CC1_timestamp), win_start=format(target_window_start), win_end=format(target_window_end),
  dur=duration_hours, n5=n_5min_bins, n10=n_10min_bins,
  cov10=round(coverage_fraction_10min,3)), 8)), row.names=FALSE)
cat("\nanimals with incomplete 10-min coverage:\n")
print(as.data.frame(wide %>% filter(coverage_fraction_10min < 1 | is.na(coverage_fraction_10min)) %>%
  transmute(AnimalNum, Group, Sex, n_10min_bins, cov=round(coverage_fraction_10min,4))), row.names=FALSE)
