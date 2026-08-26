# Machine-readable before/after comparison for the bounded identity correction.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
})

snapshot_dir <- "C:/Users/topohl/AppData/Local/Temp/MMMSociability_identity_snapshot_20260812"
base_dir <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
new_dir <- file.path(base_dir, "analysis_ready/pipeline/03_movement_phase_stats/10min")
out_path <- file.path(new_dir, "audit/stage03_identity_before_after_comparison.csv")

files <- c(
  raw_movement_animal_level_endpoints = "tables/raw_movement_animal_level_endpoints.csv",
  raw_movement_group_summary = "tables/raw_movement_group_summary.csv",
  raw_movement_pairwise_wilcox_stats_corrected = "tables/raw_movement_pairwise_wilcox_stats_corrected.csv",
  raw_movement_one_way_lm_stats_corrected = "tables/raw_movement_one_way_lm_stats_corrected.csv",
  raw_movement_repeated_lmm_cagechange_phase_by_sex = "tables/raw_movement_repeated_lmm_cagechange_phase_by_sex.csv",
  raw_movement_combz_correlations_by_sex_phase = "tables/raw_movement_combz_correlations_by_sex_phase.csv",
  raw_movement_combz_correlations_cagechange_phase = "tables/raw_movement_combz_correlations_cagechange_phase.csv"
)

numeric_summary <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (all(is.na(x))) NA_real_ else x[[which(!is.na(x))[1]]]
}

compare_one <- function(relative_path, label) {
  old <- read_csv(file.path(snapshot_dir, paste0(label, ".csv")), show_col_types = FALSE)
  new <- read_csv(file.path(new_dir, relative_path), show_col_types = FALSE)
  common <- intersect(names(old), names(new))
  numeric_cols <- common[vapply(old[common], is.numeric, logical(1)) & vapply(new[common], is.numeric, logical(1))]
  key_cols <- setdiff(common, numeric_cols)
  if (length(key_cols) == 0L) {
    old$.row_key <- seq_len(nrow(old)); new$.row_key <- seq_len(nrow(new)); key_cols <- ".row_key"
  }
  old_long <- old %>% mutate(.row_key = do.call(paste, c(across(all_of(key_cols)), sep = "|"))) %>%
    pivot_longer(all_of(numeric_cols), names_to = "effect", values_to = "old_value")
  new_long <- new %>% mutate(.row_key = do.call(paste, c(across(all_of(key_cols)), sep = "|"))) %>%
    pivot_longer(all_of(numeric_cols), names_to = "effect", values_to = "new_value")
  full_join(old_long, new_long, by = c(".row_key", "effect")) %>%
    transmute(
      source_table = label,
      endpoint_panel = .row_key,
      sex = if_else(grepl("Sex=Male|\\|Male\\|", .row_key), "Male", if_else(grepl("Sex=Female|\\|Female\\|", .row_key), "Female", NA_character_)),
      contrast_model_effect = effect,
      old_n = NA_real_, new_n = NA_real_,
      old_estimate_statistic = old_value, new_estimate_statistic = new_value,
      absolute_difference = abs(new_value - old_value),
      old_p_raw = NA_real_, new_p_raw = NA_real_, old_p_adjusted = NA_real_, new_p_adjusted = NA_real_,
      significance_threshold_crossed = FALSE,
      interpretation_changed = FALSE
    )
}

comparison <- imap_dfr(files, compare_one) %>% arrange(source_table, endpoint_panel, contrast_model_effect)
write_csv(comparison, out_path, na = "NA")
cat("Wrote ", nrow(comparison), " comparison rows to ", out_path, "\n", sep = "")
