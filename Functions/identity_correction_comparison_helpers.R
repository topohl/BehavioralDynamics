# ================================================================
# Identity-correction before/after comparison engine
# MMMSociability
# ================================================================
# Pure comparison engine plus the Stage 03 / Stage 09 table registry used by
# Testing/compare_identity_correction_before_after.R. No file I/O or other
# side effects happen at source() time, so this can be sourced directly in
# portable unit tests against synthetic tempdir fixtures.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(stringr)
})

# ------------------------------------------------------------------
# Table registry: stable semantic keys and column roles
# ------------------------------------------------------------------
# key_cols_str: '|'-delimited stable semantic key columns, taken from each
# producer's actual column contract (Analysis/03_primary_raw_movement_phase_stats.R,
# Analysis/09_early_prediction_model_ladder.R) -- NOT "every non-numeric column".
# *_col fields name the single column (if any) that plays that manuscript-facing
# role for the headline view; every numeric column is still compared regardless.
# legacy_subfolder / legacy_filename describe the documented pre-migration
# location (mirrors Analysis/16_manuscript_behavior_report.R's already-proven
# source_registry) used only when resolving an old baseline snapshot.
identity_comparison_table_registry <- tribble(
  ~stage, ~table_name,                                         ~key_cols_str,                                                 ~n_col,          ~estimate_col,           ~effect_size_col, ~p_raw_col,      ~p_adjusted_col,      ~legacy_subfolder, ~legacy_filename,
  "03",   "raw_movement_animal_level_endpoints",               "AnimalNum|ScopeType|Endpoint|CageChange|PhaseClass",         NA_character_,   "mean_movement",         NA_character_,    NA_character_,   NA_character_,        "tables",          NA_character_,
  "03",   "raw_movement_group_summary",                        "ScopeType|Endpoint|Sex|Group|CageChange|PhaseClass",         "n_animals",     "mean_movement",         NA_character_,    NA_character_,   NA_character_,        "tables",          NA_character_,
  "03",   "raw_movement_pairwise_wilcox_stats_corrected",      "ScopeType|Endpoint|Sex|CageChange|PhaseClass|contrast",      "n2",            "estimate_diff",         NA_character_,    "p_raw",         "p_holm_panel",       "stats_tables",    NA_character_,
  "03",   "raw_movement_one_way_lm_stats_corrected",           "ScopeType|Endpoint|Sex|CageChange|PhaseClass|test",          NA_character_,   NA_character_,           NA_character_,    "p_raw",         "p_holm_panel",       "stats_tables",    NA_character_,
  "03",   "raw_movement_repeated_lmm_cagechange_phase_by_sex", "Sex|term",                                                    NA_character_,   "F value",               NA_character_,    "Pr(>F)",        NA_character_,        "stats_tables",    NA_character_,
  "03",   "raw_movement_combz_correlations_by_sex_phase",      "Sex|Endpoint|PhaseClass",                                     "n_animals",     "spearman_rho",          "pearson_r",      "spearman_p",    "spearman_p_bh",      "stats_tables",    NA_character_,
  "03",   "raw_movement_combz_correlations_cagechange_phase",  "Sex|CageChange|PhaseClass",                                   "n_animals",     "spearman_rho",          "pearson_r",      "spearman_p",    "spearman_p_bh",      "stats_tables",    NA_character_,
  "09",   "model_ladder_input",                                "AnimalNum",                                                   NA_character_,   NA_character_,           NA_character_,    NA_character_,   NA_character_,        "models",          NA_character_,
  "09",   "primary_movement_entropyacf1_associations",         "feature",                                                     "n",             "spearman_rho",          "pearson_r",      "spearman_p",    "spearman_p_bh",      "statistics",      NA_character_,
  "09",   "primary_movement_entropyacf1_correlations_by_sex",  "Sex|feature",                                                 "n",             "spearman_rho",          "pearson_r",      "spearman_p",    "spearman_p_bh_within_sex", "statistics", NA_character_,
  "09",   "primary_feature_sex_interactions",                  "feature",                                                     "n",             "interaction_estimate",  NA_character_,    "interaction_p", "interaction_p_bh",   "statistics",      NA_character_,
  "09",   "primary_prediction_performance",                    "model_id",                                                    "n_animals",     "cv_r2",                 "pearson_r",      "permutation_p", NA_character_,       "models",          NA_character_,
  "09",   "primary_prediction_predictions",                    "Model|AnimalNum",                                            NA_character_,   "predicted",             NA_character_,    NA_character_,   NA_character_,        "models",          "matched_ladder_loo_predictions",
  "09",   "primary_prediction_permutation_test",               "model",                                                       "n_permutations", "observed_statistic",   NA_character_,    "empirical_p",   NA_character_,        "models",          NA_character_
)

split_key_cols <- function(key_cols_str) strsplit(key_cols_str, "\\|", perl = TRUE)[[1]]

# ------------------------------------------------------------------
# Core comparison engine
# ------------------------------------------------------------------

#' Build a stable semantic row key and stop if it does not uniquely identify rows.
make_row_key <- function(dat, key_cols, table_name, side) {
  present <- key_cols[key_cols %in% names(dat)]
  if (length(present) == 0L) {
    stop("None of the declared semantic key columns (", paste(key_cols, collapse = ", "),
         ") are present in the ", side, " '", table_name, "' table.", call. = FALSE)
  }
  key <- do.call(paste, c(lapply(present, function(cc) as.character(dat[[cc]])), sep = "||"))
  if (anyDuplicated(key) > 0L) {
    stop("Declared key columns (", paste(present, collapse = ", "), ") do not uniquely identify rows in the ",
         side, " '", table_name, "' table; this key is not stable for this table.", call. = FALSE)
  }
  key
}

#' Compare one old/new table pair. Either may be NULL (table absent on that side).
#' Returns a long-format tibble: one row per (row_key, column) comparison.
compare_identity_table <- function(old, new, table_name, stage,
                                   key_cols,
                                   n_col = NA_character_, estimate_col = NA_character_,
                                   effect_size_col = NA_character_,
                                   p_raw_col = NA_character_, p_adjusted_col = NA_character_,
                                   significance_threshold = 0.05) {
  if (is.null(old) && is.null(new)) {
    return(tibble(
      stage = character(), table = character(), row_key = character(), column = character(),
      column_role = character(), old_value = numeric(), new_value = numeric(),
      row_status = character(), absolute_difference = numeric(), signed_difference = numeric(),
      sign_changed = logical(), significance_threshold_crossed = logical()
    ))
  }
  if (is.null(old)) old <- new[0, , drop = FALSE]
  if (is.null(new)) new <- old[0, , drop = FALSE]

  old_key <- make_row_key(old, key_cols, table_name, "OLD")
  new_key <- make_row_key(new, key_cols, table_name, "NEW")
  old <- old %>% mutate(.row_key = old_key)
  new <- new %>% mutate(.row_key = new_key)

  common_cols <- intersect(names(old), names(new))
  common_cols <- setdiff(common_cols, ".row_key")
  numeric_cols <- common_cols[
    vapply(old[common_cols], is.numeric, logical(1)) & vapply(new[common_cols], is.numeric, logical(1))
  ]

  if (length(numeric_cols) == 0L) {
    all_keys <- union(old$.row_key, new$.row_key)
    return(tibble(
      stage = stage, table = table_name, row_key = all_keys, column = NA_character_,
      column_role = NA_character_, old_value = NA_real_, new_value = NA_real_,
      row_status = case_when(
        !all_keys %in% old$.row_key ~ "added",
        !all_keys %in% new$.row_key ~ "removed",
        TRUE ~ "matched"
      ),
      absolute_difference = NA_real_, signed_difference = NA_real_,
      sign_changed = NA, significance_threshold_crossed = NA
    ))
  }

  all_keys <- union(old$.row_key, new$.row_key)
  row_status_of <- case_when(
    !all_keys %in% old$.row_key ~ "added",
    !all_keys %in% new$.row_key ~ "removed",
    TRUE ~ "matched"
  )

  map_dfr(numeric_cols, function(col) {
    old_val <- setNames(as.numeric(old[[col]]), old$.row_key)
    new_val <- setNames(as.numeric(new[[col]]), new$.row_key)
    is_p_col <- (!is.na(p_raw_col) && identical(col, p_raw_col)) ||
      (!is.na(p_adjusted_col) && identical(col, p_adjusted_col))
    ov <- unname(old_val[all_keys])
    nv <- unname(new_val[all_keys])
    tibble(
      stage = stage,
      table = table_name,
      row_key = all_keys,
      column = col,
      column_role = case_when(
        !is.na(n_col) & col == n_col ~ "n",
        !is.na(estimate_col) & col == estimate_col ~ "estimate",
        !is.na(effect_size_col) & col == effect_size_col ~ "effect_size",
        !is.na(p_raw_col) & col == p_raw_col ~ "p_raw",
        !is.na(p_adjusted_col) & col == p_adjusted_col ~ "p_adjusted",
        TRUE ~ "other_numeric"
      ),
      old_value = ov,
      new_value = nv,
      row_status = row_status_of,
      absolute_difference = abs(nv - ov),
      signed_difference = nv - ov,
      sign_changed = row_status_of == "matched" & is.finite(ov) & is.finite(nv) & ov != 0 & nv != 0 & sign(ov) != sign(nv),
      significance_threshold_crossed = is_p_col & row_status_of == "matched" & is.finite(ov) & is.finite(nv) &
        ((ov < significance_threshold) != (nv < significance_threshold))
    )
  })
}

#' Old/new animal-composition diff for animal-keyed tables.
compare_animal_composition <- function(old, new, table_name, animal_col = "AnimalNum") {
  if (is.null(old) || is.null(new) || !animal_col %in% names(old) || !animal_col %in% names(new)) {
    return(tibble(
      table = table_name, old_n_animals = NA_integer_, new_n_animals = NA_integer_,
      n_added = NA_integer_, n_removed = NA_integer_, n_retained = NA_integer_,
      added_animals = NA_character_, removed_animals = NA_character_
    ))
  }
  old_ids <- unique(as.character(old[[animal_col]]))
  new_ids <- unique(as.character(new[[animal_col]]))
  added <- sort(setdiff(new_ids, old_ids))
  removed <- sort(setdiff(old_ids, new_ids))
  tibble(
    table = table_name,
    old_n_animals = length(old_ids),
    new_n_animals = length(new_ids),
    n_added = length(added),
    n_removed = length(removed),
    n_retained = length(intersect(old_ids, new_ids)),
    added_animals = paste(added, collapse = "; "),
    removed_animals = paste(removed, collapse = "; ")
  )
}

#' Concise summary: counts of changed rows/results by stage/table.
summarise_identity_comparison <- function(detail) {
  if (nrow(detail) == 0L) {
    return(tibble(
      stage = character(), table = character(), n_rows_compared = integer(),
      n_rows_added = integer(), n_rows_removed = integer(), n_rows_matched = integer(),
      n_values_changed = integer(), n_sign_changes = integer(),
      n_significance_threshold_crossings = integer(), max_absolute_difference = numeric()
    ))
  }
  detail %>%
    group_by(stage, table) %>%
    summarise(
      n_rows_compared = n_distinct(row_key),
      n_rows_added = n_distinct(row_key[row_status == "added"]),
      n_rows_removed = n_distinct(row_key[row_status == "removed"]),
      n_rows_matched = n_distinct(row_key[row_status == "matched"]),
      n_values_changed = sum(row_status == "matched" & is.finite(absolute_difference) & absolute_difference > 0, na.rm = TRUE),
      n_sign_changes = sum(sign_changed, na.rm = TRUE),
      n_significance_threshold_crossings = sum(significance_threshold_crossed, na.rm = TRUE),
      max_absolute_difference = suppressWarnings(max(absolute_difference[row_status == "matched"], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(max_absolute_difference = if_else(is.infinite(max_absolute_difference), NA_real_, max_absolute_difference))
}

#' Focused Stage 09 headline view: just the manuscript-facing columns (n,
#' estimate, effect size, raw/adjusted p), excluding incidental numeric columns.
stage09_headline_summary <- function(detail) {
  detail %>%
    filter(stage == "09", column_role %in% c("n", "estimate", "effect_size", "p_raw", "p_adjusted")) %>%
    select(table, row_key, column, column_role, old_value, new_value,
           absolute_difference, signed_difference, sign_changed,
           significance_threshold_crossed, row_status) %>%
    arrange(table, row_key, column_role)
}
