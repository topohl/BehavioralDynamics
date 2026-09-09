# Contract tests for the Stage 09 permutation-draw persistence.
#
# The change to Stage 09 in this pass is PERSISTENCE ONLY: it writes out the
# per-permutation statistics it already computed. These tests exist to make
# that claim checkable, i.e. to prove that the persisted draws are the real
# refit statistics and that no pre-existing scientific value moved.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat.

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(readr) })

source("Analysis/_pipeline_setup.R")
source_mmm_helper("project_paths.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")

STAGE09 <- "Analysis/09_early_prediction_model_ladder.R"
skipped <- character()

raw <- readLines(STAGE09, warn = FALSE)
code <- raw[!grepl("^\\s*#", raw)]

# =====================================================================
cat("\n[A] the Stage 09 change is persistence only\n")
# =====================================================================

# The frozen scientific contract: these constants must not move.
FROZEN <- c(
  "n_primary_outcome_permutations <- 1000",
  "primary_outcome_permutation_seed <- 20260811",
  'outcome_col <- "CombZ"',
  'endpoint_excel_sheet <- "zScore"')
for (f in FROZEN) {
  check(any(grepl(f, code, fixed = TRUE)),
        paste0("Stage 09 no longer declares the frozen contract line: ", f))
}
ok("permutation count, seed, endpoint and source sheet all unchanged")

# The permutation engine must still permute the OUTCOME and refit the whole loop.
check(any(grepl("perm_dat$outcome <- sample(dat$outcome, replace = FALSE)",
                code, fixed = TRUE)),
      "the outcome-permutation step changed")
check(any(grepl("empirical_p = (sum(finite_null >= observed_statistic) + 1) / (length(finite_null) + 1)",
                code, fixed = TRUE)),
      "the empirical p-value formula changed")
check(sum(grepl("loo_lm_predict(", code, fixed = TRUE)) >= 2L,
      "the leave-one-animal-out predictor is no longer used by the permutation")
ok("outcome permutation, full refit and empirical-p formula unchanged")

# The persisted list-column must be stripped before the pre-existing summary
# table is written, so that file keeps its original schema.
strip <- grep("select(-null_draws)", code, fixed = TRUE)
write_summary <- grep('primary_prediction_permutation_test.csv', code, fixed = TRUE)
check(length(strip) >= 1L,
      "Stage 09 must strip the null_draws list-column before writing the summary")
check(length(write_summary) >= 1L, "the permutation summary write disappeared")
check(min(strip) < max(write_summary),
      "null_draws must be stripped BEFORE primary_prediction_permutation_test.csv is written")
ok("the list-column is stripped before the pre-existing summary table is written")

# The draw writer must assert its own count contract in-script.
check(any(grepl("Permutation draw persistence contract violated", code, fixed = TRUE)),
      "Stage 09 must assert the draw-count contract where it writes the draws")
ok("Stage 09 asserts the draw-count contract itself")

# =====================================================================
# DATA-DEPENDENT SECTION
# =====================================================================

project_root <- mmm_project_root()
draws_path <- tryCatch(
  mmm_path_get("behavior.early_prediction", "permutation_draws",
               required = FALSE, root = project_root),
  error = function(e) NA_character_)
perm_path <- tryCatch(
  mmm_path_get("behavior.early_prediction", "permutation",
               required = FALSE, root = project_root),
  error = function(e) NA_character_)
perf_path <- tryCatch(
  mmm_path_get("behavior.early_prediction", "performance",
               required = FALSE, root = project_root),
  error = function(e) NA_character_)

check(!is.na(draws_path),
      "the permutation draws are not registered in the path layer")
ok("permutation draws resolvable by semantic key")

have <- !is.na(draws_path) && file.exists(draws_path) &&
        file.exists(perm_path) && file.exists(perf_path)

if (!have) {
  skipped <- c(skipped, "[B]-[D] value checks (Stage 09 outputs not reachable)")
} else {
  d <- read_csv(draws_path, show_col_types = FALSE, progress = FALSE)
  pt <- read_csv(perm_path, show_col_types = FALSE, progress = FALSE)
  pf <- read_csv(perf_path, show_col_types = FALSE, progress = FALSE)

  cat("\n[B] exactly n_permutations null draws are persisted per model\n")
  REQ <- c("permutation_id", "seed", "model_id", "model_label", "feature_set",
           "endpoint", "cv_scheme", "performance_metric", "performance_value",
           "is_observed", "n_permutations")
  check(all(REQ %in% names(d)),
        paste0("draws table missing column(s): ",
               paste(setdiff(REQ, names(d)), collapse = ", ")))
  s <- d %>% group_by(.data$model_id) %>%
    summarise(n_null = sum(!.data$is_observed), n_obs = sum(.data$is_observed),
              declared = dplyr::first(.data$n_permutations), .groups = "drop")
  check(nrow(s) == nrow(pt),
        paste0("draws cover ", nrow(s), " models but the summary has ", nrow(pt)))
  for (i in seq_len(nrow(s))) {
    check(s$n_null[i] == 1000L,
          paste0(s$model_id[i], " has ", s$n_null[i],
                 " persisted null draws; the frozen contract is 1000"))
    check(s$n_null[i] == s$declared[i],
          paste0(s$model_id[i], " null-draw count disagrees with its own ",
                 "declared n_permutations"))
    check(s$n_obs[i] == 1L,
          paste0(s$model_id[i], " must carry exactly one observed row"))
  }
  check(all(d$seed == 20260811),
        "every persisted draw must carry the frozen seed 20260811")
  check(all(d$endpoint == "CombZ"), "every draw must record CombZ as the endpoint")
  check(all(grepl("one-animal-out", d$cv_scheme, ignore.case = TRUE)),
        "every draw must record the leave-one-animal-out scheme")
  check(all(d$performance_metric == "cv_r2_vs_mean"),
        "the persisted performance metric must be cv_r2_vs_mean")
  check(!any(is.na(d$performance_value)),
        "no persisted draw may be missing its performance value")
  ok(paste0(nrow(s), " models x 1000 null draws + 1 observed row, frozen seed and metric"))

  cat("\n[C] the persisted draws reproduce the published summaries exactly\n")
  TOL <- 1e-12
  for (m in unique(d$model_id)) {
    dm <- d[d$model_id == m, ]
    nulls <- dm$performance_value[!dm$is_observed]
    obs <- dm$performance_value[dm$is_observed]
    r <- pt[pt$model == m, ]
    check(nrow(r) == 1L, paste0("no summary row for model ", m))
    cmp <- c(
      observed = abs(obs - r$observed_statistic[1]),
      null_median = abs(stats::median(nulls) - r$null_median[1]),
      null_q025 = abs(stats::quantile(nulls, 0.025, names = FALSE) - r$null_q025[1]),
      null_q975 = abs(stats::quantile(nulls, 0.975, names = FALSE) - r$null_q975[1]),
      empirical_p = abs((sum(nulls >= obs) + 1) / (length(nulls) + 1) -
                          r$empirical_p[1]))
    bad <- names(cmp)[cmp > TOL]
    check(length(bad) == 0L,
          paste0(m, ": persisted draws do not reproduce ",
                 paste(bad, collapse = ", "), " (max diff ",
                 format(max(cmp)), ")"))
    # the observed statistic must also match the performance table
    pr <- pf[pf$model_id == m, ]
    check(nrow(pr) == 1L, paste0("no performance row for model ", m))
    check(abs(obs - pr$cv_r2[1]) <= TOL,
          paste0(m, ": the observed draw does not match cv_r2 in the ",
                 "performance table"))
    check(abs(r$empirical_p[1] - pr$permutation_p[1]) <= TOL,
          paste0(m, ": permutation_p disagrees between the summary and the ",
                 "performance table"))
  }
  ok("observed statistic, null median, both quantiles and empirical p all reproduced")

  cat("\n[D] the frozen headline values are unchanged\n")
  head_perf <- pf[pf$model_id == "movement_mean", ]
  head_perm <- pt[pt$model == "movement_mean", ]
  EXPECT <- list(
    cv_r2 = 0.1593945586, permutation_p = 0.000999000999,
    repeated_cv_mean_r2 = 0.1558227254, cv_r2_q025 = 0.1159402748,
    cv_r2_q975 = 0.1790433407)
  for (nm in names(EXPECT)) {
    got <- head_perf[[nm]][1]
    check(abs(got - EXPECT[[nm]]) < 1e-8,
          paste0("movement_mean ", nm, " is ", format(got, digits = 12),
                 ", expected ", format(EXPECT[[nm]], digits = 12)))
  }
  EXPECT_NULL <- list(null_median = -0.03131646461, null_q025 = -0.04420903341,
                      null_q975 = 0.005863676405)
  for (nm in names(EXPECT_NULL)) {
    got <- head_perm[[nm]][1]
    check(abs(got - EXPECT_NULL[[nm]]) < 1e-8,
          paste0("movement_mean ", nm, " is ", format(got, digits = 12),
                 ", expected ", format(EXPECT_NULL[[nm]], digits = 12)))
  }
  base <- pf$cv_r2[pf$model_id == "mean_only"]
  check(abs(base - (-0.01826446281)) < 1e-8,
        paste0("intercept-only baseline is ", format(base, digits = 12),
               ", expected -0.01826446281"))
  check(head_perf$n_animals[1] == 111L, "the analysed n must remain 111")
  check(!("null_draws" %in% names(pt)),
        "the null_draws list-column leaked into the summary table")
  ok("CV R2 0.15939, permutation p 1/1001, null summaries and baseline all frozen")
}

if (length(skipped) > 0L) {
  cat("\nSKIPPED (environment-dependent):\n")
  for (s in skipped) cat("  -", s, "\n")
}

cat("\nStage 09 permutation-draw persistence checks: PASS\n")
