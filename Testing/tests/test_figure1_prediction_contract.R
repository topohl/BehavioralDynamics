# Contract tests for the Figure 1 prediction provenance repair.
#
# Two defects motivated this file.
#
# (1) The repeated grouped-CV seed was a bare literal at its call site - the one
#     prediction parameter with no name to reference - so the Figure 1 bundle
#     builders transcribed the make_grouped_folds() DEFAULT (123) instead of the
#     value actually passed (521). The frozen contract shipped a seed that
#     reproduces nothing. Two seeds legitimately coexist here and are easy to
#     confuse: 521 assigns CV folds, 123 drives the association bootstrap.
#
# (2) Every model in the legacy adjusted ladder carries Sex + Group, yet several
#     were displayed with labels like "Movement" and machine keys like
#     "Movement only". A reviewer matching the manuscript's headline model
#     against that registry would land on a group-adjusted model, and Group is
#     derived from the outcome.
#
# These tests make both claims checkable, and pin the quantitative outputs so a
# provenance repair cannot quietly move a scientific value.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat.

suppressPackageStartupMessages({ library(dplyr) })

source("Analysis/_pipeline_setup.R")
source_mmm_helper("project_paths.R")
source_mmm_helper("figure1_prediction_contract.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")

STAGE09  <- "Analysis/09_early_prediction_model_ladder.R"
BUILDERS <- c("Analysis/_supporting/build_figure1_export_bundle.R",
              "Analysis/_supporting/build_figure1_manuscript_bridge.R")
skipped <- character()

raw  <- readLines(STAGE09, warn = FALSE)
code <- raw[!grepl("^\\s*#", raw)]

# =====================================================================
cat("\n[A] the repeated-CV seed has a single source of truth\n")
# =====================================================================

check(FIGURE1_PREDICTION_CONTRACT$repeated_cv_seed == 521L,
      "the canonical repeated grouped-CV seed must be 521")
check(FIGURE1_PREDICTION_CONTRACT$association_bootstrap_seed == 123L,
      "the association bootstrap seed must remain 123")
check(FIGURE1_PREDICTION_CONTRACT$repeated_cv_seed !=
        FIGURE1_PREDICTION_CONTRACT$association_bootstrap_seed,
      "the CV seed and the bootstrap seed must stay distinct")
ok("contract declares CV seed 521 and bootstrap seed 123, distinctly")

# The canonical call site must READ the contract, not restate a literal.
call_site <- grep("seed = FIGURE1_PREDICTION_CONTRACT$repeated_cv_seed", code,
                  fixed = TRUE)
check(length(call_site) == 1L,
      "exactly one prediction call site must take its seed from the contract")
check(!any(grepl("seed = 521", code, fixed = TRUE)),
      "seed 521 must not be re-hard-coded at the call site")
ok("the primary fold map reads its seed from the contract")

# The fold map that feeds the repeated CV must be the one built with it.
check(any(grepl("primary_fold_map <- make_grouped_folds(", code, fixed = TRUE)),
      "primary_fold_map must still be built by make_grouped_folds()")
check(any(grepl("kfold_lm_predict(model_dat, .x, .y, primary_fold_map)", code,
                fixed = TRUE)),
      "the repeated CV must consume primary_fold_map")
ok("the contract seed reaches the repeated CV through primary_fold_map")

# =====================================================================
cat("\n[B] no Figure 1 builder hard-codes the CV seed\n")
# =====================================================================

for (b in BUILDERS) {
  check(file.exists(b), paste0("missing builder: ", b))
  bl <- readLines(b, warn = FALSE)
  bc <- bl[!grepl("^\\s*#", bl)]
  # the exact defect: a literal "seed 123" in a prediction-design string
  check(!any(grepl("seed 123", bc, fixed = TRUE)),
        paste0(b, " still hard-codes 'seed 123' in a contract string"))
  check(!any(grepl("seed = 123", bc, fixed = TRUE)),
        paste0(b, " still hard-codes 'seed = 123'"))
  # and it must source the single source of truth
  check(any(grepl("figure1_prediction_contract.R", bc, fixed = TRUE)),
        paste0(b, " does not source Functions/figure1_prediction_contract.R"))
  check(any(grepl("figure1_repeated_cv_description()", bc, fixed = TRUE)),
        paste0(b, " does not derive the repeated-CV description from the contract"))
}
ok("both builders derive the prediction design from the contract")

# The rendered sentence must carry 521 and must not carry 123.
desc <- figure1_repeated_cv_description()
check(grepl("seed 521", desc, fixed = TRUE),
      "the rendered repeated-CV description must state seed 521")
check(!grepl("123", desc, fixed = TRUE),
      "the rendered repeated-CV description must not mention 123")
check(grepl("k=5", desc, fixed = TRUE) && grepl("100 repeats", desc, fixed = TRUE) &&
        grepl("group=AnimalNum", desc, fixed = TRUE),
      "the rendered description must keep k, repeats and grouping unit")
ok(paste0("rendered: ", desc))

# =====================================================================
cat("\n[C] the association bootstrap keeps its own, different seed\n")
# =====================================================================

check(any(grepl("bootstrap_correlation_ci <- function", code, fixed = TRUE)),
      "bootstrap_correlation_ci must still exist")
boot_line <- grep("bootstrap_correlation_ci <- function", code, fixed = TRUE)
check(grepl("seed = 123", code[boot_line], fixed = TRUE),
      "the association bootstrap must still default to seed 123")
adesc <- figure1_association_description()
check(grepl("seed 123", adesc, fixed = TRUE),
      "the association description must state seed 123")
check(grepl("5000", adesc, fixed = TRUE),
      "the association description must state 5000 resamples")
ok("123 is preserved exactly where it belongs, and nowhere else")

# =====================================================================
cat("\n[D] model labels cannot misrepresent an adjusted model\n")
# =====================================================================

mk <- function(lab, g, s, n) data.frame(
  Model = "m", DisplayModel = lab, UsesGroup = g, UsesSex = s,
  n_predictors = n, stringsAsFactors = FALSE)
rejects <- function(d) inherits(try(figure1_assert_label_integrity(d),
                                    silent = TRUE), "try-error")

# the historical labels, all of which must now be refused
check(rejects(mk("Movement", TRUE, TRUE, 3L)),
      "a bare 'Movement' label on a Sex+Group model must be refused")
check(rejects(mk("Entropy persistence", TRUE, TRUE, 3L)),
      "a bare 'Entropy persistence' label on a Sex+Group model must be refused")
check(rejects(mk("Compact behavior", TRUE, TRUE, 10L)),
      "a bare 'Compact behavior' label on a Sex+Group model must be refused")
check(rejects(mk("Movement only", TRUE, TRUE, 3L)),
      "an 'only' label on an adjusted model must be refused")
# the corrected labels, and the legitimate ones
check(!rejects(mk("Movement + sex + group", TRUE, TRUE, 3L)),
      "a fully disclosed adjusted label must be accepted")
check(!rejects(mk("Mean only", FALSE, FALSE, 0L)),
      "'Mean only' with zero predictors is honest and must be accepted")
check(!rejects(mk("Movement mean", FALSE, FALSE, 1L)),
      "the true behaviour-only headline label must be accepted")
ok("labels must disclose Group and Sex, not merely avoid the word 'only'")

# the live label map in Stage 09 must satisfy its own rule
lab_start <- grep("model_display_labels <- c(", code, fixed = TRUE)
check(length(lab_start) == 1L, "model_display_labels must be declared once")
lab_block <- code[lab_start:(lab_start + 8L)]
for (key in c("Movement only", "Entropy ACF1 only", "Movement + Entropy ACF1",
              "Movement x Entropy ACF1", "Full behavior compact")) {
  ln <- grep(paste0('"', key, '" = '), lab_block, fixed = TRUE)
  check(length(ln) == 1L, paste0("missing display label for key: ", key))
  shown <- sub('.*= "([^"]*)".*', "\\1", lab_block[ln])
  check(grepl("group", shown, ignore.case = TRUE),
        paste0("display label for '", key, "' does not disclose group: '",
               shown, "'"))
  check(grepl("sex", shown, ignore.case = TRUE),
        paste0("display label for '", key, "' does not disclose sex: '",
               shown, "'"))
}
ok("every adjusted ladder label in Stage 09 discloses sex and group")

# machine keys are stable identifiers and must NOT have been renamed
for (key in c('"Mean only" = character(0)', '"Movement only" = c(candidate_covars, "Movement_mean")')) {
  check(any(grepl(key, code, fixed = TRUE)),
        paste0("a stable machine key changed; downstream joins would break: ", key))
}
ok("machine model keys are unchanged")

# =====================================================================
cat("\n[E] a Group-using model can never be headline evidence\n")
# =====================================================================

check(any(grepl("figure1_assert_headline_is_behaviour_only(", code, fixed = TRUE)),
      "Stage 09 must assert that headline models exclude Group")
check(any(grepl("figure1_assert_label_integrity(", code, fixed = TRUE)),
      "Stage 09 must assert label integrity before writing the audit")

bad <- data.frame(model_id = "movement_mean", uses_group = TRUE,
                  reporting_role = "primary_behavior_only",
                  stringsAsFactors = FALSE)
check(inherits(try(figure1_assert_headline_is_behaviour_only(bad), silent = TRUE),
               "try-error"),
      "a Group-using model marked primary_behavior_only must be refused")
good <- data.frame(model_id = c("movement_mean", "adj"),
                   uses_group = c(FALSE, TRUE),
                   reporting_role = c("primary_behavior_only", "supplementary"),
                   stringsAsFactors = FALSE)
check(!inherits(try(figure1_assert_headline_is_behaviour_only(good), silent = TRUE),
                "try-error"),
      "a clean registry must be accepted")
check(FIGURE1_PREDICTION_CONTRACT$headline_model_id == "movement_mean",
      "the declared headline model id must be movement_mean")
check(FIGURE1_PREDICTION_CONTRACT$headline_model_predictors == "Movement_mean",
      "the headline model must predict from Movement_mean alone")

# the pre-existing hard stop on the primary specs must survive
check(any(grepl("Primary behavior-only predictors must be exactly the a priori behavior features",
                code, fixed = TRUE)),
      "the Stage 09 hard stop on primary predictors was removed")
ok("headline evidence is structurally barred from carrying Group")

# =====================================================================
cat("\n[F] no scientific value moved\n")
# =====================================================================

T10 <- NULL
cand <- file.path(
  "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID",
  "analysis_ready/pipeline/09_early_prediction/10min/tables")
if (dir.exists(cand)) T10 <- cand

if (is.null(T10)) {
  skipped <- c(skipped, "[F] analysis_ready tables not reachable from this host")
} else {
  perf <- utils::read.csv(file.path(T10, "primary_prediction_performance.csv"),
                          stringsAsFactors = FALSE)
  g <- function(id, col) perf[[col]][perf$model_id == id]
  near <- function(a, b, tol = 1e-9) isTRUE(abs(a - b) < tol)

  # The constants in block [F] were re-frozen on 2026-09-21 against the
  # CORRECTED CombZ (producer commit 497deb7), which changed the endpoint for 19
  # of 117 animals and moved four females from SUS to RES. The pre-correction set
  # is reproducible by substituting combz_as_recorded from
  # later_outcome_combz_animal_level.csv, which the producer carries for exactly
  # this purpose. Each value below was re-derived independently of the pipeline,
  # by leave-one-animal-out OLS computed two ways (an explicit n=111 refit loop
  # and the hat-matrix PRESS identity), and the derivation was reproduced a
  # second time from the raw workbook before being written here.
  check(near(g("movement_mean", "cv_r2"), 0.17255580295966455),
        "LOAO R2 for movement_mean moved")
  # mean_only is deliberately NOT re-frozen: it is the closed form
  # 1 - (111/110)^2 and is invariant to the endpoint.
  check(near(g("mean_only", "cv_r2"), -0.01826446280991756),
        "intercept-only baseline R2 moved")
  check(near(g("movement_mean", "repeated_cv_mean_r2"), 0.16830676077585088),
        "repeated grouped CV mean R2 moved")
  check(near(g("movement_mean", "cv_r2_q025"), 0.1250461423044919),
        "repeated CV lower quantile moved")
  check(near(g("movement_mean", "cv_r2_q975"), 0.19279224602828185),
        "repeated CV upper quantile moved")
  check(all(perf$n_animals == 111L), "the analysed n must remain 111")
  ok("prediction performance, baseline and CV interval all frozen")

  pt <- utils::read.csv(file.path(T10, "primary_prediction_permutation_test.csv"),
                        stringsAsFactors = FALSE)
  check(near(pt$empirical_p[pt$model == "movement_mean"], 9.99000999000999e-4),
        "permutation p moved")
  check(all(pt$n_permutations == 1000L), "permutation draw count moved")
  check(all(pt$seed == 20260811L), "the outcome-permutation seed moved")
  ok("permutation p, draw count and seed all frozen")

  as_ <- utils::read.csv(file.path(T10,
    "primary_movement_entropyacf1_associations.csv"), stringsAsFactors = FALSE)
  a <- function(f, col) as_[[col]][as_$feature == f]
  check(near(a("Movement_mean", "spearman_rho"), -0.40379966225192016),
        "Movement_mean rho moved")
  check(near(a("Movement_mean", "spearman_boot_ci_low"), -0.55648160455900575),
        "Movement_mean CI lower moved")
  check(near(a("Movement_mean", "spearman_boot_ci_high"), -0.2234232099085772),
        "Movement_mean CI upper moved")
  check(near(a("Movement_mean", "spearman_p_bh"), 3.3265647943764399e-05),
        "Movement_mean q moved")
  check(near(a("Movement_rmssd", "spearman_rho"), -0.24648122148122148),
        "Movement_rmssd rho moved")
  check(near(a("Entropy_acf1", "spearman_rho"), -0.18006318006318006),
        "Entropy_acf1 rho moved")
  check(all(as_$n == 111L), "association n must remain 111")
  ok("all three association rho, CI and q values frozen")

  sx <- utils::read.csv(file.path(T10, "primary_feature_sex_interactions.csv"),
                        stringsAsFactors = FALSE)
  qcol <- grep("bh", names(sx), value = TRUE, ignore.case = TRUE)[1]
  check(!is.na(qcol), "the sex-interaction BH column is missing")
  # Before the 2026-09-21 endpoint correction all three BH q values tied at
  # 0.8951970487582287, so a single all()-against-one-scalar test expressed the
  # contract. They are now two distinct values, so each is pinned separately.
  # All three remain far from significance, which is the property that matters.
  sxq <- function(f) sx[[qcol]][sx$feature == f]
  check(near(sxq("Movement_mean"),  0.97536221617972285),
        "the Movement_mean-by-sex interaction q moved")
  check(near(sxq("Movement_rmssd"), 0.76637416508998391),
        "the Movement_rmssd-by-sex interaction q moved")
  check(near(sxq("Entropy_acf1"),   0.76637416508998391),
        "the Entropy_acf1-by-sex interaction q moved")
  ok("sex-interaction q values frozen; all three remain non-significant")
}

if (length(skipped) > 0L) {
  cat("\nSKIPPED (environment-dependent):\n")
  for (s in skipped) cat("  -", s, "\n")
}

cat("\nFigure 1 prediction contract checks: PASS\n")
