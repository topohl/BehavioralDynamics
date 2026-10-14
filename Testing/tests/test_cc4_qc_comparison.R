# ===========================================================================
# Stage 31b-31d historical vs QC-revised comparison (Testing/audits/compare_cc4_qc_reruns.R, sourced for its definitions
# only). Portable: fixture run folders under tempdir(); no model is fitted and nothing on the lab drive is read.
#
#  1. a table is stacked over the three versions on the common columns (QC reruns carry extra eligibility columns)
#  2. the wide comparison puts each version side by side with the differences against the historical run
#  3. interpretation flags: interval excluding the null, Holm P < 0.05 and direction, and a change against the historical run
#  4. the whole comparison on fixture folders of the three stages: sizes, estimates, diagnostics, sensitivity, summary
#  5. sourcing the script runs nothing
# ===========================================================================
suppressPackageStartupMessages(library(data.table))
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Testing", "audits", "compare_cc4_qc_reruns.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
before <- ls()
source("Testing/audits/compare_cc4_qc_reruns.R")
check(all(grepl("^(cqc_|CQC_)", setdiff(ls(), c(before, "before")))), "5: sourcing the script defines only cqc_ objects")
tmp <- tempfile("cqc_"); dir.create(tmp)

# fixture runs: three versions of each stage; the QC runs carry an extra eligibility column
runs <- data.frame(CQC_RUNS, dir = file.path(tmp, CQC_RUNS$folder), stringsAsFactors = FALSE)
put <- function(stage, version, name, d) {
  dd <- runs$dir[runs$stage == stage & runs$version == version]; dir.create(dd, showWarnings = FALSE)
  if (version != "historical") d$EligibilityStatus <- "included"
  fwrite(d, file.path(dd, paste0(name, ".csv")))
}
ratio <- function(est, lo, hi, holm, cages, animals) data.table(Sex = "Male", Phase = c("Active", "Inactive"), Measure = "SIS_CON_ratio_of_rate_ratios",
  Estimate = est, Lower95 = lo, Upper95 = hi, P = holm / 2, MCSE = 0.001, LR = 1, Status = "PASS", Cages = cages, CONCages = 3L, Animals = animals, HolmP = holm)
put("31c", "historical", "primary_ratios", ratio(c(1.20, 0.90), c(1.05, 0.70), c(1.40, 1.10), c(0.02, 0.60), c(14L, 14L), c(49L, 49L)))
put("31c", "qc_primary", "primary_ratios", ratio(c(1.10, 0.90), c(0.95, 0.70), c(1.30, 1.10), c(0.20, 0.60), c(13L, 14L), c(45L, 49L)))
put("31c", "qc_inactive_sens", "primary_ratios", ratio(c(1.10, 0.85), c(0.95, 0.66), c(1.30, 1.08), c(0.20, 0.50), c(13L, 13L), c(45L, 45L)))
eff <- function(cages, animals) data.table(Sex = "Male", Phase = c("Active", "Inactive"), Measure = "LogRRR", Estimate = 0.1, Lower95 = 0, Upper95 = 0.2,
  P = 0.1, MCSE = 0.001, LR = 1, Status = "PASS", Cages = cages, CONCages = 3L, Animals = animals, HolmP = 0.2)
put("31c", "historical", "primary_effects", eff(c(14L, 14L), c(49L, 49L))); put("31c", "qc_primary", "primary_effects", eff(c(13L, 14L), c(45L, 49L)))
put("31c", "qc_inactive_sens", "primary_effects", eff(c(13L, 13L), c(45L, 45L)))
for (v in CQC_VERSIONS) {
  put("31c", v, "bootstrap_status", data.table(Sex = "Male", Phase = "Active", Type = "null", Status = "PASS", Requested = 4999L, Attempted = 5200L,
                                                Successful = if (v == "historical") 5094L else 5100L, Used = 4999L, Failed = 10L, Boundary = 5L, Seed = 1L))
  put("31d", v, "contrasts", data.table(Sex = "Male", Phase = "Active", Contrast = "SUS-RES", LogRRR = 0.1, Estimate = if (v == "historical") 1.109 else 0.98,
                                         Lower95 = if (v == "historical") 1.005 else 0.88, Upper95 = if (v == "historical") 1.231 else 1.10, P = 0.06, MCSE = 0.003,
                                         LR = 3, DF = 1L, TestStatus = "PASS", IntervalStatus = "PASS", HolmP = 0.746))
  put("31d", v, "population", data.table(Sex = "Male", Phase = "Active", Group = c("RES", "SUS"), Animals = if (v == "historical") c(22L, 15L) else c(19L, 14L),
                                          Cages = if (v == "historical") c(9L, 8L) else c(8L, 7L), Batches = 3L))
  put("31b", v, "population", data.table(Sex = "Male", Batch = c("B1", "B2"), Group = "RES", Phase = "Active", PhaseLabel = "A3", RecordedAnimals = 4L,
                                          EligibleAnimals = 4L, PrimaryAnimals = if (v == "historical") c(4L, 4L) else c(4L, 1L), PrimaryCages = 1L))
}

# 1-2. stacking on common columns and the wide comparison
st <- cqc_stack(runs[runs$stage == "31c", ], "primary_ratios")
check(nrow(st) == 6L && !"EligibilityStatus" %in% names(st) && setequal(st$version, CQC_VERSIONS), "1: stacked over three versions on the common columns")
w <- cqc_wide(st, c("Sex", "Phase", "Measure"), c("Estimate", "Cages", "Status"))
check(abs(w[Phase == "Active"]$Estimate_diff_qc_primary - (1.10 - 1.20)) < 1e-12 && w[Phase == "Active"]$Cages_diff_qc_primary == -1L &&
        w[Phase == "Inactive"]$Cages_diff_qc_inactive_sens == -1L && identical(w[Phase == "Active"]$Status_qc_primary, "PASS") &&
        !"Status_diff_qc_primary" %in% names(w), "2: versions side by side with numeric differences against the historical run")

# 3. interpretation flags
f <- cqc_flags(st, c("Sex", "Phase", "Measure"))
check(isTRUE(f[Phase == "Active"]$ci_excludes_null_historical) && isFALSE(f[Phase == "Active"]$ci_excludes_null_qc_primary) &&
        isTRUE(f[Phase == "Active"]$holm_below_alpha_historical) && isFALSE(f[Phase == "Active"]$holm_below_alpha_qc_primary) &&
        isTRUE(f[Phase == "Active"]$interpretation_changed_qc_primary) && isFALSE(f[Phase == "Inactive"]$interpretation_changed_qc_primary) &&
        isFALSE(f[Phase == "Inactive"]$interpretation_changed_qc_inactive_sens), "3: a change of interval or Holm reading is flagged; an unchanged one is not")
f31b <- cqc_flags(data.table(version = CQC_VERSIONS, k = "x", Estimate = c(2, -1, 2), Lower95 = c(0.5, -3, 0.5), Upper95 = c(3, 1, 3)), "k", null = 0, holm = "none")
check(isTRUE(f31b$interpretation_changed_qc_primary) && isFALSE(f31b$interpretation_changed_qc_inactive_sens) && all(is.na(f31b$holm_below_alpha_historical)),
      "3: descriptive differences against 0 without a Holm P")

# 4. the whole comparison
cmp <- cqc_compare(runs)
check(all(c("sizes_31b", "sizes_31c", "sizes_31d", "effects_31c_ratios", "flags_31c", "effects_31d_contrasts", "flags_31d", "diagnostics_31c_bootstrap") %in% names(cmp)),
      "4: the comparison tables of the stages with fixture data")
check(cmp$sizes_31b$Animals_historical == 8L && cmp$sizes_31b$Animals_qc_primary == 5L && cmp$sizes_31c[Phase == "Active"]$Animals_diff_qc_primary == -4L &&
        cmp$sizes_31d[Group == "RES"]$Animals_diff_qc_primary == -3L, "4: included animals and cages per stage")
check(isTRUE(cmp$flags_31d$interpretation_changed_qc_primary) && cmp$diagnostics_31c_bootstrap$Successful_diff_qc_primary == 6L,
      "4: a 31d interval that now includes 1 is flagged; bootstrap counts are compared")
sm <- cqc_summary(cmp)
check(sm[table == "flags_31c" & comparison == "qc_primary vs historical"]$changed == 1L && sm[table == "flags_31d" & comparison == "qc_primary vs historical"]$changed == 1L,
      "4: the summary counts the changed interpretations")
unlink(tmp, recursive = TRUE)
cat("test_cc4_qc_comparison: all checks passed\n")
