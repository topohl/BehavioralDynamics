# Read-only check of SLEAPanalyzer's 03_derived_metrics receipt lookup. It
# evaluates only the retained_derived_metrics() definition from
# validation/exp9_boris/scripts/01_build_metadata.R; the script itself is not
# run, because it creates folders and writes metadata.
#
#   Rscript Maintenance/Check-SleapAnalyzerArchiveLookup.R <SLEAPanalyzer repo> <Exp9 root>
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) stop("Usage: <SLEAPanalyzer repo> <Exp9 root>", call. = FALSE)
script <- file.path(args[[1L]], "validation/exp9_boris/scripts/01_build_metadata.R")
exp9 <- args[[2L]]
definitions <- Filter(function(e) is.call(e) && identical(e[[1L]], as.name("<-")) &&
                        identical(e[[2L]], as.name("retained_derived_metrics")),
                      as.list(parse(script, keep.source = FALSE)))
if (length(definitions) != 1L) stop("Lookup function not found in ", script, call. = FALSE)
eval(definitions[[1L]])
root <- retained_derived_metrics(exp9)
ready <- file.path(exp9, "Analysis/Behavior/RFID/analysis_ready")
cat("retained 03 root:          ", root, "\n")
cat("phenotype table present:   ", file.exists(file.path(
  root, "qc/cross_scale_identity_expected_phenotype_from_preprocessed.csv")), "\n")
cat("foundation assignment copy:", file.exists(file.path(
  ready, "foundations/behavior_metrics/qc/animal_group_sex_assignment_qc.csv")), "\n")
