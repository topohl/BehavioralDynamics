# Portable regression tests for the identity-correction comparison engine
# (Functions/identity_correction_comparison_helpers.R). Pure in-memory tibbles
# only; no S: drive access required.

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
})

source("Functions/identity_correction_comparison_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

value_at <- function(detail, key, column) {
  row <- detail[detail$row_key == key & detail$column == column, ]
  if (nrow(row) != 1L) stop("expected exactly one matching row for key=", key, " column=", column)
  row
}

# ------------------------------------------------------------------
# Test 1: matched row with a changed value -> correct abs/signed diff,
# sign change and significance-threshold-crossing detection.
# ------------------------------------------------------------------
old1 <- tibble(feature = c("Movement_mean", "Movement_rmssd"), n = c(58L, 58L),
               spearman_rho = c(0.10, -0.30), spearman_p = c(0.02, 0.20), spearman_p_bh = c(0.03, 0.20))
new1 <- tibble(feature = c("Movement_mean", "Movement_rmssd"), n = c(58L, 58L),
               spearman_rho = c(-0.05, -0.30), spearman_p = c(0.09, 0.20), spearman_p_bh = c(0.11, 0.20))

d1 <- compare_identity_table(
  old1, new1, table_name = "primary_movement_entropyacf1_associations", stage = "09",
  key_cols = c("feature"), n_col = "n", estimate_col = "spearman_rho",
  p_raw_col = "spearman_p", p_adjusted_col = "spearman_p_bh"
)

r <- value_at(d1, "Movement_mean", "spearman_rho")
check(isTRUE(all.equal(r$old_value, 0.10)), "Test 1: old_value for Movement_mean spearman_rho")
check(isTRUE(all.equal(r$new_value, -0.05)), "Test 1: new_value for Movement_mean spearman_rho")
check(isTRUE(all.equal(r$absolute_difference, 0.15)), "Test 1: absolute_difference should be 0.15")
check(isTRUE(all.equal(r$signed_difference, -0.15)), "Test 1: signed_difference should be -0.15")
check(isTRUE(r$sign_changed), "Test 1: Movement_mean spearman_rho crossed zero and should be flagged sign_changed")
check(identical(r$column_role, "estimate"), "Test 1: spearman_rho should carry the 'estimate' role")

p_raw_row <- value_at(d1, "Movement_mean", "spearman_p")
check(isTRUE(p_raw_row$significance_threshold_crossed), "Test 1: p_raw crossing 0.05 (0.02 -> 0.09) should be flagged")
p_bh_row <- value_at(d1, "Movement_mean", "spearman_p_bh")
check(isTRUE(p_bh_row$significance_threshold_crossed), "Test 1: p_adjusted crossing 0.05 (0.03 -> 0.11) should be flagged")

r2 <- value_at(d1, "Movement_rmssd", "spearman_p")
check(!isTRUE(r2$significance_threshold_crossed), "Test 1: unchanged p (0.20 -> 0.20) should not cross the threshold")

# ------------------------------------------------------------------
# Test 2: added and removed rows are detected (e.g. an animal merged away by
# the identity correction, or a newly-eligible animal appearing).
# ------------------------------------------------------------------
old2 <- tibble(AnimalNum = c("3", "4", "303"), outcome = c(1.1, 2.2, 3.3))
new2 <- tibble(AnimalNum = c("3", "303", "500"), outcome = c(1.1, 3.3, 9.9))
d2 <- compare_identity_table(old2, new2, table_name = "model_ladder_input", stage = "09", key_cols = c("AnimalNum"))
check(identical(sort(unique(d2$row_key[d2$row_status == "removed"])), "4"), "Test 2: AnimalNum '4' should be detected as removed")
check(identical(sort(unique(d2$row_key[d2$row_status == "added"])), "500"), "Test 2: AnimalNum '500' should be detected as added")
check(all(c("3", "303") %in% d2$row_key[d2$row_status == "matched"]), "Test 2: AnimalNum '3' and '303' should be matched")

comp <- compare_animal_composition(old2, new2, "model_ladder_input")
check(comp$n_added == 1L && comp$n_removed == 1L && comp$n_retained == 2L, "Test 2: animal composition diff counts are wrong")
check(identical(comp$added_animals, "500"), "Test 2: added_animals should list '500'")
check(identical(comp$removed_animals, "4"), "Test 2: removed_animals should list '4'")

# ------------------------------------------------------------------
# Test 3: an unstable key (declared key columns do not uniquely identify
# rows) must raise a clear error rather than silently comparing garbage.
# ------------------------------------------------------------------
dupe <- tibble(feature = c("Movement_mean", "Movement_mean"), spearman_rho = c(0.1, 0.2))
err3 <- tryCatch({
  compare_identity_table(dupe, dupe, table_name = "x", stage = "09", key_cols = c("feature"))
  NULL
}, error = function(e) e)
check(!is.null(err3), "Test 3: a non-unique key must raise an error, not silently proceed")

# ------------------------------------------------------------------
# Test 4: summary aggregation counts match the detail table.
# ------------------------------------------------------------------
summary4 <- summarise_identity_comparison(d1)
row4 <- summary4[summary4$table == "primary_movement_entropyacf1_associations", ]
check(nrow(row4) == 1L, "Test 4: expected exactly one summary row for the compared table")
check(row4$n_rows_compared == 2L, "Test 4: expected 2 compared rows (Movement_mean, Movement_rmssd)")
check(row4$n_values_changed >= 1L, "Test 4: expected at least one changed value")
check(row4$n_sign_changes == 1L, "Test 4: expected exactly one sign change")

# ------------------------------------------------------------------
# Test 5: the headline view only surfaces roled columns (n/estimate/
# effect_size/p_raw/p_adjusted), not incidental numeric columns.
# ------------------------------------------------------------------
old5 <- tibble(model_id = "movement_mean", n_animals = 58L, cv_r2 = 0.10, pearson_r = 0.3, permutation_p = 0.04, cv_r2_q025 = 0.01)
new5 <- tibble(model_id = "movement_mean", n_animals = 58L, cv_r2 = 0.20, pearson_r = 0.3, permutation_p = 0.04, cv_r2_q025 = 0.02)
d5 <- compare_identity_table(
  old5, new5, table_name = "primary_prediction_performance", stage = "09", key_cols = c("model_id"),
  n_col = "n_animals", estimate_col = "cv_r2", effect_size_col = "pearson_r", p_raw_col = "permutation_p"
)
headline5 <- stage09_headline_summary(d5)
check(!("cv_r2_q025" %in% headline5$column), "Test 5: unroled numeric columns must be excluded from the headline view")
check("cv_r2" %in% headline5$column, "Test 5: the estimate column must appear in the headline view")

# ------------------------------------------------------------------
# Test 6: never silently compare a file/table with itself -- comparing
# identical old/new data must report zero changes, not spurious diffs.
# ------------------------------------------------------------------
d6 <- compare_identity_table(old1, old1, table_name = "primary_movement_entropyacf1_associations", stage = "09",
                              key_cols = c("feature"), estimate_col = "spearman_rho")
check(all(d6$absolute_difference == 0 | is.na(d6$absolute_difference)), "Test 6: comparing identical data must show zero differences")
check(!any(d6$sign_changed, na.rm = TRUE), "Test 6: comparing identical data must show no sign changes")

cat("Identity comparison engine contract checks: PASS\n")
