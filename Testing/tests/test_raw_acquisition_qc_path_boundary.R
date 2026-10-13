# ===========================================================================
# Stage 00b raw-acquisition QC: run configuration and output boundaries (Functions/tracking_qc_run_config.R,
# Functions/project_paths.R). Portable: a fixture root under tempdir(); nothing is run and nothing is written by the
# configuration.
#
#  1. default root: analysis_ready/quality_control/raw_acquisition, distinct from the Stage 00 roots
#  2. arguments: --run-id required (1-40 characters, no path), no unknown or repeated argument, --recordings only with
#     --output-root and as a valid regular expression
#  3. isolated roots: absolute only; refused inside the live root (drive and UNC spelling), the raw data, a data-version
#     root or the measurement-validation outputs; a temporary folder is accepted
#  4. existing run folders are refused; a partial .tmp_ folder only with --resume; the configuration creates nothing
#  5. raw-log root precedence: option, then environment variable, then the folder beside the analysis tree
#  6. path budget: the longest planned output path under the live root stays within 240 characters, also in UNC spelling
#  7. Stage 00b is not registered in the runner
# ===========================================================================
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "tracking_qc_run_config.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
refused <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
source("Functions/project_paths.R")
source("Functions/tracking_qc_run_config.R")

fx <- gsub("\\\\", "/", tempfile("raqc_"))
root <- file.path(fx, "Exp", "Analysis", "Behavior", "RFID")
dir.create(root, recursive = TRUE)
iso <- file.path(fx, "isolated")

# 1
check(identical(mmm_raw_acquisition_qc_root(root), file.path(root, "analysis_ready", "quality_control", "raw_acquisition")), "1: default root")
check(!identical(mmm_raw_acquisition_qc_root(root), mmm_tracking_qc_new_run_root(root)), "1: distinct from Stage 00")
cfg <- mmm_raw_acquisition_qc_run_config(root, "--run-id=raqc_20261013")
check(identical(cfg$output_dir, file.path(root, "analysis_ready", "quality_control", "raw_acquisition", "runs", "raqc_20261013")) &&
        isTRUE(cfg$require_clean_code) && !cfg$isolated, "1: live-root run needs clean code")

# 2
check(refused(mmm_raw_acquisition_qc_run_config(root, character())), "2: no arguments")
check(refused(mmm_raw_acquisition_qc_run_config(root, c("--run-id=a", "--force"))), "2: unknown argument")
check(refused(mmm_raw_acquisition_qc_run_config(root, c("--run-id=a", "--run-id=b"))), "2: repeated argument")
check(refused(mmm_raw_acquisition_qc_run_config(root, "--run-id=../escape")), "2: path in run id")
check(refused(mmm_raw_acquisition_qc_run_config(root, paste0("--run-id=", strrep("a", 41)))), "2: run id longer than 40")
check(!refused(mmm_raw_acquisition_qc_run_config(root, paste0("--run-id=", strrep("a", 40)))), "2: 40 characters accepted")
check(refused(mmm_raw_acquisition_qc_run_config(root, c("--run-id=a", "--recordings=B1_CC1"))), "2: subset runs never go to the live root")
check(refused(mmm_raw_acquisition_qc_run_config(root, c("--run-id=a", paste0("--output-root=", iso), "--recordings=B1_CC[1"))), "2: invalid regex")

# 3
ok <- mmm_raw_acquisition_qc_run_config(root, c("--run-id=dev01", paste0("--output-root=", iso), "--recordings=B2_CC[34]"))
check(isTRUE(ok$isolated) && !ok$require_clean_code && identical(ok$recordings, "B2_CC[34]") && startsWith(ok$output_dir, iso), "3: isolated root accepted")
for (bad in c(file.path(root, "qc"), "//mdc-berlin.net/fs/AG/Exp/Analysis/Behavior/RFID/qc", file.path(fx, "Exp", "Analysis", "Behavior", "RANT", "qc"),
              file.path(fx, "Exp", "Analysis", "Behavior", "RANA"), file.path(fx, "Exp", "Raw Data", "qc"), file.path(fx, "MMMdata", "x"),
              file.path(fx, "Exp", "Analysis", "Behavior", "RANM", "measurement_validation", "qc"), "relative/folder")) {
  check(refused(mmm_raw_acquisition_qc_run_config(root, c("--run-id=a", paste0("--output-root=", bad)))), paste("3: refused", bad))
}
check(!refused(mmm_raw_acquisition_qc_run_config(root, c("--run-id=a", paste0("--output-root=", file.path(fx, "Exp", "Analysis", "Behavior", "RANM", "raw_acquisition_qc"))))),
      "3: a sibling of the measurement-validation outputs is accepted")

# 4
check(!dir.exists(iso), "4: the configuration creates nothing")
dir.create(file.path(iso, "runs", "dev02"), recursive = TRUE)
check(refused(mmm_raw_acquisition_qc_run_config(root, c("--run-id=dev02", paste0("--output-root=", iso)))), "4: existing run folder")
dir.create(file.path(iso, "runs", ".tmp_dev03"), recursive = TRUE)
check(refused(mmm_raw_acquisition_qc_run_config(root, c("--run-id=dev03", paste0("--output-root=", iso)))), "4: partial run without --resume")
check(isTRUE(mmm_raw_acquisition_qc_run_config(root, c("--run-id=dev03", paste0("--output-root=", iso), "--resume"))$resume), "4: --resume")

# 5
old <- Sys.getenv("MMM_RAW_ANTENNA_DIR"); oo <- options(mmm.raw_antenna_dir = NULL)
Sys.unsetenv("MMM_RAW_ANTENNA_DIR")
check(identical(mmm_raw_antenna_root(root), file.path(fx, "Exp", "Raw Data", "Behavior", "RFID", "MMMdata")), "5: default raw root")
Sys.setenv(MMM_RAW_ANTENNA_DIR = "E:/env_raw")
check(identical(mmm_raw_antenna_root(root), "E:/env_raw"), "5: environment variable")
options(mmm.raw_antenna_dir = "E:/opt_raw")
check(identical(mmm_raw_antenna_root(root), "E:/opt_raw"), "5: option first")
options(oo); if (nzchar(old)) Sys.setenv(MMM_RAW_ANTENNA_DIR = old) else Sys.unsetenv("MMM_RAW_ANTENNA_DIR")

# 6. Longest planned path: <root>/analysis_ready/quality_control/raw_acquisition/runs/.tmp_<40>/rec/B6_CC4/animal_reader_block.csv
for (live in c(MMM_PROJECT_ROOT_DEFAULT, "//mdc-berlin.net/fs/AG_Hoernberg/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID")) {
  p <- file.path(live, "analysis_ready", "quality_control", "raw_acquisition", "runs", paste0(".tmp_", strrep("a", 40)), "rec", "B6_CC4",
                 "animal_reader_block.csv")
  check(nchar(p) <= 240, paste("6: path budget", nchar(p)))
}

# 7
runner <- readLines("Analysis/run_all_analysis.R", warn = FALSE)
check(!any(grepl("00b_qc_raw_acquisition", runner, fixed = TRUE)), "7: Stage 00b is a manual diagnostic, not in the runner")
unlink(fx, recursive = TRUE)
cat("test_raw_acquisition_qc_path_boundary: all checks passed\n")
