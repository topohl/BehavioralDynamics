# Stage 31b-31d: historical (frozen) results against the QC-revised reruns of rule cc4e_v1.0 (UNREGISTERED post hoc
# QC-sensitivity analysis; the frozen registered outputs stay the record and are never replaced).
#   historical            the frozen runs phase_groups_20260929_v2, phase_statistics_20260929_v1,
#                         phase_phenotypes_20260929_v1 (every cage episode kept: the all-episode sensitivity)
#   qc_primary            the reruns with MMM_CC4_OUTCOME_ELIGIBILITY = the primary eligibility file of cc4e_v1.0_4aebc90
#                         (B2|sys.5|CC4 active family A1-A5 excluded; its inactive family kept, flagged)
#   qc_inactive_sens      the reruns with the inactive-sensitivity eligibility file (cc4e_v1.0_4aebc90_isens: the B2|sys.5
#                         inactive family left out as well)
# For every stage: included animals and independent cages; estimates and 95% intervals; unadjusted and Holm-adjusted P
# where the stage computes them; fit and bootstrap diagnostics; the stage's own sensitivity estimates; and flags of a
# change in interpretation (interval excluding the null, Holm P < 0.05, direction). No model is fitted here; every value
# is read from the run folders after their manifests are verified.
#
# Run from the repository root (light):
#   Rscript Testing/audits/compare_cc4_qc_reruns.R [--run-id=<id>] [--output-root=<absolute>]
# Writes <Behavior>/RANM/cc4_qc_comparison/runs/<run_id>/ only (built in runs/.tmp_<run_id>; protection guard); the
# default id is cqc_v1.0_<commit>; a release needs committed code.

CQC_VERSION <- "cqc_v1.0"
CQC_CODE <- c("Testing/audits/compare_cc4_qc_reruns.R", "Functions/cc4_phase_group_helpers.R", "Functions/cc4_grid_exposure_helpers.R", "Functions/project_paths.R",
              "Functions/protected_outputs.R", "Functions/qc_run_protection.R", "Functions/frozen_input_guard.R", "Analysis/_pipeline_setup.R")
# The three versions of each stage: folder names under the frozen root (historical) and the QC rerun root.
CQC_RUNS <- data.frame(
  stage = rep(c("31b", "31c", "31d"), each = 3),
  version = rep(c("historical", "qc_primary", "qc_inactive_sens"), 3),
  root = rep(c("frozen", "qc", "qc"), 3),
  folder = c("phase_groups_20260929_v2", "p31b_cc4e10_20261015", "p31b_cc4e10_isens_20261015",
             "phase_statistics_20260929_v1", "p31c_cc4e10_20261015", "p31c_cc4e10_isens_20261015",
             "phase_phenotypes_20260929_v1", "p31d_cc4e10_20261015", "p31d_cc4e10_isens_20261015"),
  stringsAsFactors = FALSE)
CQC_VERSIONS <- c("historical", "qc_primary", "qc_inactive_sens")
CQC_ALPHA <- 0.05   # only to flag a change in the reading of a Holm-adjusted P; no new test is made

cqc_args <- function(args) {
  known <- "^--(run-id|output-root)="
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  list(run_id = val("run-id"), output_root = val("output-root"))
}

#' One table of one run, read after the run's manifest has been verified (grid31_verify_manifest()); NULL if the run has
#' no such table.
cqc_table <- function(run_dir, name) {
  f <- file.path(run_dir, paste0(name, ".csv"))
  if (!file.exists(f)) return(NULL)
  data.table::fread(f, colClasses = list(character = c("Sex", "Phase")), na.strings = c("", "NA"), showProgress = FALSE)
}

#' Stacks one table over the three versions of a stage: data.table with a version column (the tables of a QC rerun carry
#' extra eligibility columns; only the common columns are kept).
cqc_stack <- function(runs, name) {
  x <- lapply(seq_len(nrow(runs)), function(i) { t <- cqc_table(runs$dir[i], name); if (!is.null(t)) t[, version := runs$version[i]]; t })
  x <- Filter(Negate(is.null), x)
  if (!length(x)) return(NULL)
  common <- Reduce(intersect, lapply(x, names))
  data.table::rbindlist(lapply(x, function(t) t[, common, with = FALSE]))
}

#' Wide comparison: one row per key, the value columns of each version side by side (<col>_<version>), with the
#' differences qc_primary - historical and qc_inactive_sens - historical for numeric columns.
cqc_wide <- function(stacked, keys, values) {
  if (is.null(stacked) || !nrow(stacked)) return(data.table::data.table())
  w <- data.table::dcast(stacked, stats::as.formula(paste(paste(keys, collapse = " + "), "~ version")), value.var = values,
                         fun.aggregate = function(v) if (length(v)) v[1] else NA)
  for (v in values) {
    h <- paste0(v, "_historical")
    for (q in c("qc_primary", "qc_inactive_sens")) {
      cc <- paste0(v, "_", q)
      if (all(c(h, cc) %in% names(w)) && is.numeric(w[[h]]) && is.numeric(w[[cc]])) w[, (paste0(v, "_diff_", q)) := get(cc) - get(h)]
    }
  }
  w[]
}

#' Interpretation flags per version for estimates with an interval: the interval excludes the null value (1 for ratios),
#' Holm P < CQC_ALPHA, direction (above or below the null); changed = any flag differs from the historical version.
cqc_flags <- function(stacked, keys, null = 1, est = "Estimate", lo = "Lower95", hi = "Upper95", holm = "HolmP") {
  if (is.null(stacked) || !nrow(stacked)) return(data.table::data.table())
  s <- data.table::copy(stacked)
  s[, `:=`(ci_excludes_null = !is.na(get(lo)) & !is.na(get(hi)) & (get(lo) > null | get(hi) < null),
           holm_below_alpha = if (holm %in% names(s)) !is.na(get(holm)) & get(holm) < CQC_ALPHA else NA,
           direction = ifelse(is.na(get(est)), NA_character_, ifelse(get(est) > null, "above_null", ifelse(get(est) < null, "below_null", "at_null"))))]
  w <- data.table::dcast(s, stats::as.formula(paste(paste(keys, collapse = " + "), "~ version")),
                         value.var = c("ci_excludes_null", "holm_below_alpha", "direction"), fun.aggregate = function(v) if (length(v)) v[1] else NA)
  for (q in c("qc_primary", "qc_inactive_sens")) {
    ch <- function(f) { a <- w[[paste0(f, "_historical")]]; b <- w[[paste0(f, "_", q)]]; !is.na(a) & !is.na(b) & a != b }
    w[, (paste0("interpretation_changed_", q)) := ch("ci_excludes_null") | ch("holm_below_alpha") | ch("direction")]
  }
  w[]
}

#' Every comparison table from the three versions of each stage (runs: CQC_RUNS with a dir column).
cqc_compare <- function(runs) {
  r <- function(st) runs[runs$stage == st, ]
  out <- list()
  # included animals and cages
  p31b <- cqc_stack(r("31b"), "population")
  if (!is.null(p31b)) {
    s <- p31b[, .(Animals = sum(PrimaryAnimals), Cages = sum(PrimaryCages)), by = .(version, Sex, Group, Phase, PhaseLabel)]
    out$sizes_31b <- cqc_wide(s, c("Sex", "Group", "Phase", "PhaseLabel"), c("Animals", "Cages"))
  }
  e31c <- cqc_stack(r("31c"), "primary_effects")
  if (!is.null(e31c)) out$sizes_31c <- cqc_wide(unique(e31c[Measure == "LogRRR", .(version, Sex, Phase, Cages, CONCages, Animals)]), c("Sex", "Phase"), c("Cages", "CONCages", "Animals"))
  p31d <- cqc_stack(r("31d"), "population")
  if (!is.null(p31d)) out$sizes_31d <- cqc_wide(p31d, c("Sex", "Phase", "Group"), c("Animals", "Cages", "Batches"))
  # estimates, intervals, P
  rat <- cqc_stack(r("31c"), "primary_ratios")
  if (!is.null(rat)) {
    out$effects_31c_ratios <- cqc_wide(rat, c("Sex", "Phase", "Measure"), c("Estimate", "Lower95", "Upper95", "P", "MCSE", "HolmP", "Status"))
    out$flags_31c <- cqc_flags(rat, c("Sex", "Phase", "Measure"))
  }
  if (!is.null(e31c)) out$effects_31c_model_rates <- cqc_wide(e31c[Measure != "LogRRR"], c("Sex", "Phase", "Measure"), c("Estimate", "Lower95", "Upper95", "Status"))
  om <- cqc_stack(r("31d"), "omnibus")
  if (!is.null(om)) out$effects_31d_omnibus <- cqc_wide(om, c("Sex", "Phase"), c("LR", "DF", "P", "MCSE", "HolmP", "Status"))
  ct <- cqc_stack(r("31d"), "contrasts")
  if (!is.null(ct)) {
    out$effects_31d_contrasts <- cqc_wide(ct, c("Sex", "Phase", "Contrast"), c("Estimate", "Lower95", "Upper95", "P", "MCSE", "HolmP", "TestStatus", "IntervalStatus"))
    out$flags_31d <- cqc_flags(ct, c("Sex", "Phase", "Contrast"))
  }
  gc <- cqc_stack(r("31b"), "group_phase_contrasts")
  if (!is.null(gc)) {
    out$effects_31b_contrasts <- cqc_wide(gc, c("Sex", "Contrast", "Phase", "PhaseLabel", "Measure"), c("Estimate", "Lower95", "Upper95", "Batches"))
    out$flags_31b <- cqc_flags(gc, c("Sex", "Contrast", "Phase", "PhaseLabel", "Measure"), null = 0, holm = "none")
  }
  # diagnostics
  d31c <- cqc_stack(r("31c"), "fit_diagnostics")
  if (!is.null(d31c)) out$diagnostics_31c_fits <- cqc_wide(d31c, c("Sex", "Phase", "Model"), c("Valid", "Boundary", "Convergence", "PositiveHessian", "MaxGradient", "MinRandomSD"))
  b31c <- cqc_stack(r("31c"), "bootstrap_status")
  if (!is.null(b31c)) out$diagnostics_31c_bootstrap <- cqc_wide(b31c, c("Sex", "Phase", "Type"), c("Status", "Attempted", "Successful", "Used", "Failed", "Boundary"))
  d31d <- cqc_stack(r("31d"), "fit_diagnostics")
  if (!is.null(d31d)) out$diagnostics_31d_fits <- cqc_wide(d31d, c("Sex", "Phase", "Hypothesis"), c("Valid", "Boundary", "Convergence", "PositiveHessian", "MaxGradient", "MinRandomSD"))
  b31d <- cqc_stack(r("31d"), "bootstrap_status")
  if (!is.null(b31d)) out$diagnostics_31d_bootstrap <- cqc_wide(b31d, c("Sex", "Phase", "Hypothesis"), c("Status", "Attempted", "Successful", "Used", "Failed", "Boundary", "Retry"))
  # the stages' own sensitivity estimates
  s31c <- cqc_stack(r("31c"), "sensitivity_estimates")
  if (!is.null(s31c)) out$sensitivity_31c <- cqc_wide(s31c, c("Sex", "Phase", "Analysis"), c("RRR", "Valid", "Boundary"))
  s31d <- cqc_stack(r("31d"), "sensitivity_estimates")
  if (!is.null(s31d)) out$sensitivity_31d <- cqc_wide(s31d, c("Sex", "Phase", "Analysis", "Contrast"), c("Estimate", "Valid", "Boundary"))
  out
}

#' Short summary rows: per stage and comparison, how many rows changed their interpretation flags.
cqc_summary <- function(cmp) {
  rows <- list()
  for (nm in c("flags_31b", "flags_31c", "flags_31d")) {
    f <- cmp[[nm]]
    if (is.null(f) || !nrow(f)) next
    for (q in c("qc_primary", "qc_inactive_sens")) {
      col <- paste0("interpretation_changed_", q)
      rows[[length(rows) + 1L]] <- data.table::data.table(table = nm, comparison = paste(q, "vs historical"), rows = nrow(f),
                                                          changed = sum(f[[col]], na.rm = TRUE))
    }
  }
  data.table::rbindlist(rows)
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages(library(data.table))
  data.table::setDTthreads(1L)
  source("Analysis/_pipeline_setup.R")
  for (h in c("project_paths.R", "cc4_grid_exposure_helpers.R", "cc4_phase_group_helpers.R", "qc_run_protection.R", "frozen_input_guard.R", "protected_outputs.R")) source_mmm_helper(h)
  a <- cqc_args(commandArgs(trailingOnly = TRUE))
  project_root <- mmm_project_root()
  git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, "--no-optional-locks", args), stdout = TRUE, stderr = TRUE))
  head_commit <- git(c("rev-parse", "HEAD"))[1]
  run_id <- if (is.na(a$run_id)) paste0(CQC_VERSION, "_", substr(head_commit, 1, 7)) else a$run_id
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,39}$", run_id)) stop("Bad run id: ", run_id, call. = FALSE)
  dev <- startsWith(run_id, "dev")
  dirty <- git(c("status", "--porcelain", "--untracked-files=all", "--", CQC_CODE))
  if (!dev && length(dirty)) stop("A release run needs committed code:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  ranm <- file.path(dirname(project_root), "RANM")
  base <- mmm_isolated_output_root_check(if (is.na(a$output_root)) file.path(ranm, "cc4_qc_comparison") else a$output_root, project_root)
  out_dir <- file.path(base, "runs", run_id); tmp <- file.path(base, "runs", paste0(".tmp_", run_id))
  if (file.exists(out_dir) || file.exists(tmp)) stop("Run folder exists; choose a new --run-id: ", out_dir, call. = FALSE)
  guard <- mmm_protection_guard_begin(base, run_id, "cc4_qc_comparison", project_root)
  runs <- CQC_RUNS
  runs$dir <- ifelse(runs$root == "frozen", file.path(project_root, "analysis_ready", "analyses", "cc4_grid_exposure", runs$folder),
                     file.path(ranm, "cc4_qc_reruns", "runs", runs$folder))
  for (d in runs$dir) invisible(grid31_verify_manifest(d, "manifest.csv"))   # every run verified before it is read
  cmp <- cqc_compare(runs)
  summ <- cqc_summary(cmp)
  dir.create(file.path(tmp, "tables"), recursive = TRUE); dir.create(file.path(tmp, "manifest"))
  for (nm in names(cmp)) data.table::fwrite(cmp[[nm]], file.path(tmp, "tables", paste0(nm, ".csv")))
  data.table::fwrite(summ, file.path(tmp, "tables", "interpretation_summary.csv"))
  inputs <- data.table::data.table(run = runs$folder, stage = runs$stage, version = runs$version, manifest = file.path(runs$dir, "manifest.csv"))
  inputs[, sha256 := mmm_file_sha256(manifest)]
  data.table::fwrite(inputs, file.path(tmp, "manifest", "input_runs.csv"))
  code <- data.table::data.table(file = CQC_CODE)[, sha256 := mmm_file_sha256(file.path(MMM_REPO_ROOT, file))]
  data.table::fwrite(code, file.path(tmp, "manifest", "code_hashes.csv"))
  data.table::fwrite(data.table::data.table(item = c("run_id", "version", "git_commit", "code_clean", "dev_run", "alpha_flag_only"),
                                            value = c(run_id, CQC_VERSION, head_commit, as.character(!length(dirty)), as.character(dev), CQC_ALPHA)),
                     file.path(tmp, "manifest", "run.csv"))
  writeLines(capture.output(sessionInfo()), file.path(tmp, "manifest", "session_info.txt"))
  writeLines(c(paste("STAGE 31b-31d: HISTORICAL vs QC-REVISED (", CQC_VERSION, "; UNREGISTERED post hoc QC-sensitivity comparison)"), "",
               paste("run:", run_id, "| commit:", head_commit), "",
               "historical       = frozen registered runs (every cage episode; the all-episode sensitivity); they stay the record",
               "qc_primary       = reruns with cc4e_v1.0 (B2|sys.5|CC4 active A1-A5 excluded; inactive kept, flagged)",
               "qc_inactive_sens = reruns with the inactive sensitivity (B2|sys.5|CC4 inactive left out as well)", "",
               "Interpretation flags: interval excluding the null (1 for ratios, 0 for 31b differences), Holm P < 0.05, direction.",
               capture.output(print(summ)), "",
               "No model is fitted here; values are read from the run folders after their manifests are verified."), file.path(tmp, "README.txt"))
  mmm_protection_guard_end(guard, file.path(tmp, "manifest"))
  outs <- list.files(tmp, recursive = TRUE)
  data.table::fwrite(data.table::data.table(file = outs, sha256 = mmm_file_sha256(file.path(tmp, outs)), bytes = file.size(file.path(tmp, outs))),
                     file.path(tmp, "manifest", "output_files_sha256.csv"))
  if (file.exists(out_dir)) stop("Run folder appeared during the run: ", out_dir, call. = FALSE)
  if (!file.rename(tmp, out_dir)) stop("Could not rename ", tmp, call. = FALSE)
  print(summ)
  message("cc4 QC comparison done: ", out_dir)
}
