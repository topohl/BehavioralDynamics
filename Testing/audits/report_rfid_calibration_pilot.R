# One-day RFID calibration pilot: post-pilot REPORT on a finished pre-registered analysis run (acp_criteria_v1.1 /
# schedule_one_day_v2). Group-blind; isolated output; the acp run folder is read only, after its output manifest has been
# verified. Every decision row is classified with the hash-pinned status mapping docs/rfid_calibration/acp_report_status_v1.0.csv
# (resolved / uncertain / not obtained / supporting, the specification v1.1 item and the consequence); the projection test
# of the day-2 rule is traced per threshold; the day-2 verdict is printed as recorded by the pre-registered analysis, never
# recomputed (a disagreement of the trace is flagged). Definitions: Functions/antenna_calibration_report.R; the
# pre-registered analysis (unchanged): Functions/antenna_calibration_pilot.R and Testing/audits/analyse_rfid_calibration_pilot.R;
# tests (synthetic acp run folders only): Testing/tests/test_antenna_calibration_report.R.
#
# Run from the repository root (one light R process; data.table threads 1), after analyse_rfid_calibration_pilot.R:
#   Rscript Testing/audits/report_rfid_calibration_pilot.R --acp-run=<absolute path of a finished acp run folder>
#           [--report-id=<new id>] [--output-root=<absolute>]
# Output: <Behavior>/RANM/calibration_reports/runs/<report_id>/ only (base checked with mmm_isolated_output_root_check(); the
# runs/ level is the layout the protection guard leaves out of its snapshots); never inside the acp run folder, and a base
# that contains the acp run is refused; built in runs/.tmp_<report_id> and renamed at the end; an existing report or build
# folder is refused. Protected locations (the whole RANM tree with every acp run, the live and registered outputs, the
# manuscript imports, docs/rfid_calibration and the other repository pin files) are snapshotted before the first write and
# compared before the rename (Functions/protected_outputs.R). A release report (id not starting with "dev") needs committed
# code and a release acp run (not a dev run, committed code, the pinned board history); the default id is
# acr_v1.0_<acp run id>_<commit>, with the prefix "dev_" when the acp run is not a release.

ACR_REPORT_CODE <- c("Testing/audits/report_rfid_calibration_pilot.R", "Functions/antenna_calibration_report.R",
                     "Functions/antenna_calibration_pilot.R", "Functions/project_paths.R", "Functions/behavioral_dynamics_helpers.R",
                     "Functions/frozen_input_guard.R", "Functions/qc_run_protection.R", "Functions/protected_outputs.R",
                     "Analysis/_pipeline_setup.R", "docs/rfid_calibration/acp_report_status_v1.0.csv",
                     "docs/rfid_calibration/acp_criteria_v1.1.csv")
ACR_REPORT_FILES <- c("README.txt", "status_by_decision.csv", "spec_consequences.csv", "day2_trace.csv", "day2_trace_check.csv",
                      "uncertain_without_day2.csv")
ACR_LONGEST_RELATIVE <- c("uncertain_without_day2.csv", file.path("manifest", "protection_snapshot_before.csv"),
                          file.path("manifest", "output_files_sha256.csv"))

acr_report_args <- function(args) {
  known <- "^--(acp-run|report-id|output-root)="
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  if (anyDuplicated(sub("=.*$", "", args))) stop("Each argument may be given once.", call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  out <- list(acp_run = val("acp-run"), report_id = val("report-id"), output_root = val("output-root"))
  if (is.na(out$acp_run) || !nzchar(out$acp_run)) stop("--acp-run=<absolute path of a finished acp run folder> is required.", call. = FALSE)
  out
}

acr_git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, args), stdout = TRUE, stderr = TRUE))

#' Writes the report files (ACR_REPORT_FILES) of acr_summary() `rep` into `dir` (no manifest).
acr_write_report <- function(rep, dir, info) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(rep$classified, file.path(dir, "status_by_decision.csv"))
  data.table::fwrite(rep$spec, file.path(dir, "spec_consequences.csv"))
  data.table::fwrite(rep$trace, file.path(dir, "day2_trace.csv"))
  data.table::fwrite(rep$check, file.path(dir, "day2_trace_check.csv"))
  data.table::fwrite(rep$uncertain_without_day2, file.path(dir, "uncertain_without_day2.csv"))
  writeLines(acr_readme_lines(rep, info), file.path(dir, "README.txt"))
  invisible(file.path(dir, ACR_REPORT_FILES))
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages(library(data.table))
  data.table::setDTthreads(1L)
  source("Analysis/_pipeline_setup.R")
  for (h in c("project_paths.R", "antenna_calibration_pilot.R", "antenna_calibration_report.R", "frozen_input_guard.R",
              "qc_run_protection.R", "protected_outputs.R")) source_mmm_helper(h)
  a <- acr_report_args(commandArgs(trailingOnly = TRUE))
  started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  # the pinned criteria and status mapping, or nothing runs
  crit <- acp_criteria(file.path(MMM_REPO_ROOT, ACP_CRITERIA_FILE))
  map <- acr_status_map(file.path(MMM_REPO_ROOT, ACR_STATUS_FILE))
  project_root <- mmm_project_root()
  acp <- acr_verify_acp_run(a$acp_run)
  head_commit <- acr_git(c("rev-parse", "HEAD"))[1]
  report_id <- if (is.na(a$report_id)) paste0(if (acp$release) "" else "dev_", "acr_v1.0_", basename(acp$dir), "_", substr(head_commit, 1, 7)) else a$report_id
  dev <- startsWith(report_id, "dev")
  dirty <- acr_git(c("status", "--porcelain", "--untracked-files=all", "--", ACR_REPORT_CODE))
  if (!dev && length(dirty)) stop("A release report needs committed code and mapping:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  if (!dev && !acp$release) stop("A release report needs a release acp run (dev_run FALSE, code clean, board history pinned); use a dev report id.",
                                 call. = FALSE)
  base <- if (is.na(a$output_root)) file.path(dirname(project_root), "RANM", "calibration_reports") else a$output_root
  dirs <- acr_report_dirs(acp$dir, base, report_id, project_root)
  mmm_assert_output_path_budget(c(file.path(dirs$tmp, ACR_LONGEST_RELATIVE), file.path(dirs$out, ACR_LONGEST_RELATIVE)),
                                source_label = "calibration report")
  guard <- mmm_protection_guard_begin(dirs$base, report_id, "calibration report", project_root)

  # the report (pure), from the verified acp run
  x <- acr_read_acp(acp$dir)
  det_path <- file.path(acp$dir, "tables", "detection.csv")
  footprint <- if (file.exists(det_path)) acr_footprint(data.table::fread(det_path, select = c("board", "PositionID", "class"))) else NULL
  rep <- acr_summary(x$decisions, map, crit, x$day2, footprint)
  info <- list(report_id = report_id, commit = head_commit, acp_dir = acp$dir, acp_run = acp$run, release = acp$release,
               map_sha256 = attr(map, "sha256"))
  acr_write_report(rep, dirs$tmp, info)

  # manifest, protection check, output hashes, rename
  dir.create(file.path(dirs$tmp, "manifest"), showWarnings = FALSE)
  code <- data.table::data.table(file = ACR_REPORT_CODE)
  code[, sha256 := mmm_file_sha256(file.path(MMM_REPO_ROOT, file))]
  data.table::fwrite(code, file.path(dirs$tmp, "manifest", "code_hashes.csv"))
  data.table::fwrite(cbind(acp_run = acp$dir, acp$files), file.path(dirs$tmp, "manifest", "acp_run_files_verified.csv"))
  run <- data.table::data.table(item = c("report_id", "report_version", "status_mapping_version", "status_mapping_content_sha256",
                                         "criteria_version", "criteria_content_sha256", "acp_run", "acp_run_id", "acp_git_commit", "acp_release",
                                         "git_commit", "code_clean", "dev_report", "trace_flags", "day2_recommendation_recorded", "started",
                                         "finished", "R"),
                                value = c(report_id, ACR_VERSION, ACR_STATUS_VERSION, attr(map, "sha256"), ACP_VERSION, attr(crit, "sha256"),
                                          acp$dir, acp$run[["run_id"]], acp$run[["git_commit"]], as.character(acp$release), head_commit,
                                          as.character(!length(dirty)), as.character(dev), as.character(length(rep$flags)), rep$verdict$reason,
                                          started, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), R.version.string))
  data.table::fwrite(run, file.path(dirs$tmp, "manifest", "run.csv"))
  writeLines(capture.output(sessionInfo()), file.path(dirs$tmp, "manifest", "session_info.txt"))
  acr_verify_acp_run(acp$dir)   # the acp run is unchanged after the report was built
  mmm_protection_guard_end(guard, file.path(dirs$tmp, "manifest"))
  outs <- list.files(dirs$tmp, recursive = TRUE)
  data.table::fwrite(data.table::data.table(file = outs, sha256 = mmm_file_sha256(file.path(dirs$tmp, outs)), bytes = file.size(file.path(dirs$tmp, outs))),
                     file.path(dirs$tmp, "manifest", "output_files_sha256.csv"))
  if (file.exists(dirs$out)) stop("Report folder appeared during the run: ", dirs$out, call. = FALSE)
  if (!file.rename(dirs$tmp, dirs$out)) stop("Could not rename ", dirs$tmp, call. = FALSE)
  message("calibration report done: ", dirs$out, "\n", rep$verdict$reason, if (length(rep$flags)) paste0("\nFLAGS:\n", paste(rep$flags, collapse = "\n")))
}
