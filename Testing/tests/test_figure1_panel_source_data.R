# Contract tests for the frozen Figure 1 panel source data.
#
# This directory is the interface the proteomics manuscript renderer consumes.
# The renderer fits nothing, so every number the published figure shows has to be
# recoverable from these seven tables - and has to still equal the value the
# canonical analysis produced. These tests check both directions: that the export
# reproduces the canonical statistics, and that it has not quietly grown a column
# that would let a panel imply something the analysis does not support.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat.

suppressPackageStartupMessages({ library(dplyr) })

source("Analysis/_pipeline_setup.R")
source_mmm_helper("figure1_prediction_contract.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
near  <- function(a, b, tol = 1e-9) isTRUE(abs(a - b) < tol)

SRC <- file.path(MMM_REPO_ROOT, "results/manuscript_bridge/figure1/figure_source_data")
EXPORTER <- "Analysis/_supporting/build_figure1_panel_source_data.R"
skipped <- character()

TABLES <- c("figure1a_timeline_source.csv", "figure1b_combz_classification_source.csv",
            "figure1c_movement_combz_source.csv", "figure1d_loao_predictions_source.csv",
            "figure1e_permutation_source.csv", "figure1f_repeated_cv_source.csv",
            "figure1_panel_statistics.csv")

if (!dir.exists(SRC)) {
  skipped <- c(skipped, "figure source data not built in this checkout")
} else {

rd <- function(f) utils::read.csv(file.path(SRC, f), stringsAsFactors = FALSE)

# =====================================================================
cat("\n[A] the export is complete and self-describing\n")
# =====================================================================

present <- list.files(SRC, pattern = "[.]csv$")
check(setequal(present, c(TABLES, "figure1_panel_source_manifest.csv")),
      paste0("unexpected file set in the figure source data: ",
             paste(setdiff(present, c(TABLES, "figure1_panel_source_manifest.csv")),
                   collapse = ", ")))
man <- rd("figure1_panel_source_manifest.csv")
check(nrow(man) == length(TABLES), "the manifest must describe every exported table")
for (f in TABLES) {
  check(f %in% man$file, paste0("manifest omits ", f))
  d <- rd(f)
  check(nrow(d) > 0L, paste0(f, " is empty"))
  # every table names the analysis that owns its numbers
  check(all(c("figure_panel", "scientific_owner_stage", "owner_table",
              "exported_by") %in% names(d)) || f == "figure1_panel_statistics.csv",
        paste0(f, " lacks provenance columns"))
}
ok("seven panel tables plus a manifest, each carrying its own provenance")

# The freeze must not describe itself as authoritative if it was built from an
# uncommitted tree. This is the FC-10 lesson, enforced rather than remembered.
check(all(man$source_worktree_state == "clean"),
      "the frozen source data was exported from a dirty worktree")
check(all(man$creation_state == "authoritative"),
      "the manifest marks itself non-authoritative")
check(all(nchar(man$source_commit) == 40L), "manifest lacks a full source commit")
ok("manifest records a clean tree and a full source commit")

# hashes must match the bytes on disk
if (requireNamespace("digest", quietly = TRUE)) {
  for (i in seq_len(nrow(man))) {
    p <- file.path(SRC, man$file[i])
    check(file.size(p) == man$bytes[i], paste0("byte size drifted: ", man$file[i]))
    check(identical(digest::digest(p, algo = "sha256", file = TRUE), man$sha256[i]),
          paste0("sha256 drifted: ", man$file[i]))
  }
  ok("every recorded hash and byte size matches the file on disk")
} else {
  skipped <- c(skipped, "digest unavailable; hash check not run")
}

# =====================================================================
cat("\n[B] animal-level panels are animal-level\n")
# =====================================================================

c1 <- rd("figure1c_movement_combz_source.csv")
d1 <- rd("figure1d_loao_predictions_source.csv")
b1 <- rd("figure1b_combz_classification_source.csv")

check(nrow(c1) == 111L && length(unique(c1$AnimalID)) == 111L,
      "1c must be exactly 111 unique animals")
check(nrow(d1) == 111L && !anyDuplicated(d1$AnimalID),
      "1d must be exactly 111 unique animals")
check(!anyDuplicated(b1$AnimalID), "1b has duplicate animals")
check(setequal(c1$AnimalID, d1$AnimalID),
      "1c and 1d must describe the same 111 animals")
check(all(c1$AnimalID %in% b1$AnimalID),
      "every analysed animal must appear in the classification table")
ok("1b, 1c and 1d are one row per biological animal, on a consistent cohort")

# =====================================================================
cat("\n[C] canonical statistics reproduce from the exported tables\n")
# =====================================================================

st <- rd("figure1_panel_statistics.csv")
sv <- function(panel, statistic) {
  v <- st$value[st$figure_panel == panel & st$statistic == statistic]
  check(length(v) == 1L, paste0("panel_statistics lacks ", panel, " / ", statistic))
  suppressWarnings(as.numeric(v))
}
check(near(sv("1c", "Spearman rho"), -0.3902639142724438), "Movement rho moved")
check(near(sv("1c", "95% CI lower"), -0.547434593760078), "CI lower moved")
check(near(sv("1c", "95% CI upper"), -0.20917806610402842), "CI upper moved")
check(near(sv("1c", "BH q"), 6.8776554749896e-05), "BH q moved")
check(sv("1c", "n animals") == 111, "1c n moved")
check(near(sv("1d", "LOAO R2"), 0.15939455855319962), "LOAO R2 moved")
check(near(sv("1d", "intercept-only baseline R2"), -0.01826446280991756),
      "baseline R2 moved")
check(near(sv("1f", "repeated grouped CV mean R2"), 0.15582272536971034),
      "repeated CV mean R2 moved")
check(near(sv("1f", "2.5th percentile across repeats"), 0.11594027484397953),
      "repeated CV lower quantile moved")
check(near(sv("1f", "97.5th percentile across repeats"), 0.17904334073063455),
      "repeated CV upper quantile moved")
check(sv("1f", "repeated CV seed") == 521, "repeated CV seed must be 521")
check(near(sv("1b", "male susceptibility threshold"), -0.4366416981697792),
      "male threshold moved")
check(near(sv("1b", "female susceptibility threshold"), -0.22239084402294293),
      "female threshold moved")
ok("association, prediction, repeated CV, seed and thresholds all reproduce")

# the permutation must be recomputable from the stored draws, not merely quoted
e1 <- rd("figure1e_permutation_source.csv")
obs <- e1$performance_value[e1$row_role == "observed_statistic"]
nulls <- e1$performance_value[e1$row_role == "permutation_draw"]
check(length(nulls) == 1000L, "1e must carry 1000 null draws")
check(length(obs) == 1L, "1e must carry one observed statistic")
check(near(obs, 0.15939455855319962), "1e observed statistic is not the LOAO R2")
check(sum(nulls >= obs) == 0L, "a null draw reached the observed value")
check(near((sum(nulls >= obs) + 1) / (length(nulls) + 1), 1 / 1001),
      "p recomputed from the stored draws is not 1/1001")
check(all(e1$seed == FIGURE1_PREDICTION_CONTRACT$outcome_permutation_seed),
      "1e seed is not the canonical outcome-permutation seed")
ok("p = 1/1001 recomputes from the 1000 persisted draws")

# =====================================================================
cat("\n[D] the panels cannot imply what the analysis does not support\n")
# =====================================================================

# Group is derived from CombZ. It may be used for display, but it must never be
# a predictor column in the prediction panel.
check(all(d1$model_id == FIGURE1_PREDICTION_CONTRACT$headline_model_id),
      "1d contains a model other than the headline movement-mean model")
check(!any(grepl("predictor|feature|Movement_mean", names(d1))),
      "1d must not carry predictor columns; it is observed versus predicted only")
check(!any(grepl("gamm|hmm", unlist(lapply(TABLES, function(f) names(rd(f)))),
                 ignore.case = TRUE)),
      "a GAMM- or HMM-derived column reached the figure source data")

# 1b defines the phenotype; it must not ship the six components as plottable
# series, because their group differences are true by construction.
COMPONENTS <- c("NOR", "sucrose_pref", "weight_dev", "delta_cort",
                "adrenal_weight", "spleen_weight")
check(!any(COMPONENTS %in% names(b1)),
      "1b must not carry the six CombZ components as plottable columns")
check(all(c("susceptibility_threshold", "control_reference_mean",
            "control_reference_population_sd") %in% names(b1)),
      "1b must carry the classification rule it draws")

# nothing in the main-figure set promotes the unsupported secondary features
for (f in c("figure1c_movement_combz_source.csv", "figure1d_loao_predictions_source.csv")) {
  nm <- names(rd(f))
  check(!any(grepl("rmssd|entropy", nm, ignore.case = TRUE)),
        paste0(f, " carries a secondary feature that is not an independent predictor"))
}
ok("no outcome-derived predictor, no GAMM or HMM, no by-construction series")

# =====================================================================
cat("\n[E] the exporter computes nothing\n")
# =====================================================================

src <- readLines(EXPORTER, warn = FALSE)
code <- src[!grepl("^\\s*#", src)]
# Match CALLS, not prose. The exporter legitimately names the bootstrap and the
# permutation in descriptive notes attached to the statistics it copies; what it
# must never do is perform one.
FORBIDDEN <- c("\\blm\\(", "\\bglm\\(", "cor\\.test\\(", "\\bcor\\(",
               "p\\.adjust\\(", "\\bsample\\(", "set\\.seed\\(", "\\bgam\\(",
               "\\bboot\\(", "replicate\\(", "\\bt\\.test\\(", "wilcox\\.test\\(")
for (f in FORBIDDEN) {
  check(!any(grepl(f, code)),
        paste0("the exporter appears to compute something: ", f))
}
check(any(grepl("must(", code, fixed = TRUE)),
      "the exporter must assert its reproduction checks")
ok("export and validation only; no model, no resampling, no adjustment")
}

if (length(skipped) > 0L) {
  cat("\nSKIPPED (environment-dependent):\n")
  for (s in skipped) cat("  -", s, "\n")
}

cat("\nFigure 1 panel source data checks: PASS\n")
