# One-day RFID calibration pilot: the PRE-REGISTERED analysis (decision criteria acp_criteria_v1.1, seeded schedule
# schedule_one_day_v2; amended 2026-10-15 before any acquisition). Group-blind; isolated output. Definitions: Functions/antenna_calibration_pilot.R; protocol,
# criteria, day-2 rule and analysis plan: docs/RFID_CALIBRATION_PILOT.md sections 1-4; tests (synthetic logs only):
# Testing/tests/test_antenna_calibration_pilot.R.
#
# Run from the repository root (one light R process; data.table threads 1):
#   Rscript Testing/audits/analyse_rfid_calibration_pilot.R --date=<YYYYMMDD> [--run-id=<new id>] [--output-root=<absolute>]
#           [--raw-dir=<absolute>] [--board-history=<absolute>]
# Inputs, read-only: Raw Data/Behavior/RFID/calibration/<YYYYMMDD>/ (beside MMMdata): the MultiMouseMonitor log (exactly one
# candidate with detections), placement_sheet.csv, annotation_movement.csv, annotation_survey.csv, tags.csv and, if
# present, sha256_manifest.csv (columns file, sha256), which every listed file must match. The only Exp9 input is the
# group-blind Stage 00b tables/board_history.csv of run raqc_20261014_6165adf (content hash pinned).
# Output: <Behavior>/RANM/calibration/<run_id>/ only, checked with mmm_isolated_output_root_check(); built in
# .tmp_<run_id> and renamed at the end; an existing run or build folder is refused. The run refuses to start unless the
# criteria file and the schedule have their pinned content hashes (acp_criteria(), acp_read_schedule()). A release run
# (run id not starting with "dev") needs committed code, the default raw folder and the pinned board history; --raw-dir,
# --board-history and uncommitted code are for dev runs. The default run id is acp_v1.1_<date>_<commit>.
# Board substitution: a tested board is replaced only through an explicit authorization, board_substitution.csv in the raw
# day folder (acp_read_substitution(): only board 3 for role 4 or board 1 for role 2, with reason, authorized_by and
# authorized_on), listed in sha256_manifest.csv when that exists. Without it, any placement on another physical board than
# the schedule's stops the run: never substituted silently; abort and reschedule.

ACP_RUN_CODE <- c("Testing/audits/analyse_rfid_calibration_pilot.R", "Functions/antenna_calibration_pilot.R",
                  "Functions/antenna_log_helpers.R", "Functions/animalpos_reconstruction.R", "Functions/antenna_measurement.R",
                  "Functions/antenna_spatial_reconstruction.R", "Functions/antenna_acquisition_qc.R",
                  "Functions/animalpos_preprocessing_helpers.R", "Functions/project_paths.R", "Analysis/_pipeline_setup.R",
                  "docs/rfid_calibration/make_schedule_one_day_v2.R")
ACP_RAW_FILES <- c(sheet = "placement_sheet.csv", movement = "annotation_movement.csv", survey = "annotation_survey.csv", tags = "tags.csv")

acp_runner_args <- function(args) {
  known <- "^--(date|run-id|output-root|raw-dir|board-history)="
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  if (anyDuplicated(sub("=.*$", "", args))) stop("Each argument may be given once.", call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  out <- list(date = val("date"), run_id = val("run-id"), output_root = val("output-root"), raw_dir = val("raw-dir"),
              board_history = val("board-history"))
  if (is.na(out$date) || !grepl("^20[0-9]{6}$", out$date)) stop("--date=<YYYYMMDD> is required.", call. = FALSE)
  out
}

acp_git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, args), stdout = TRUE, stderr = TRUE))

#' The one raw log of the day with detections among MultiMouseMonitor-*.csv (initScan files are not recordings).
acp_day_log <- function(raw_dir) {
  cand <- list.files(raw_dir, pattern = "^MultiMouseMonitor.*[.]csv$", full.names = TRUE)
  cand <- cand[!grepl("initScan", basename(cand), fixed = TRUE)]
  has <- vapply(cand, function(p) tryCatch(nrow(rfid_read_raw_log(p)$events) > 0, error = function(e) FALSE), logical(1))
  if (sum(has) != 1L) stop("Expected exactly one MultiMouseMonitor log with detections in ", raw_dir, "; found ", sum(has), call. = FALSE)
  cand[has]
}

#' Every data.frame of the result as tables/<name>.csv (list elements as <name>_<part>.csv).
acp_write_tables <- function(res, dir) {
  w <- function(x, nm) if (is.data.frame(x)) data.table::fwrite(x, file.path(dir, paste0(nm, ".csv")))
  for (nm in names(res)) {
    x <- res[[nm]]
    if (is.data.frame(x)) w(x, nm) else if (is.list(x)) for (part in names(x)) w(x[[part]], paste0(nm, "_", part))
  }
  invisible(TRUE)
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages({ library(data.table); library(dplyr); library(tibble) })
  data.table::setDTthreads(1L)
  source("Analysis/_pipeline_setup.R")
  for (h in c("project_paths.R", "animalpos_preprocessing_helpers.R", "antenna_log_helpers.R", "animalpos_reconstruction.R",
              "antenna_measurement.R", "antenna_spatial_reconstruction.R", "antenna_acquisition_qc.R", "antenna_calibration_pilot.R")) source_mmm_helper(h)
  a <- acp_runner_args(commandArgs(trailingOnly = TRUE))
  started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  # pre-registration: the pinned criteria and schedule, or nothing runs
  crit <- acp_criteria(file.path(MMM_REPO_ROOT, ACP_CRITERIA_FILE))
  sched <- acp_read_schedule(file.path(MMM_REPO_ROOT, ACP_SCHEDULE_FILE))
  project_root <- mmm_project_root()
  head_commit <- acp_git(c("rev-parse", "HEAD"))[1]
  run_id <- if (is.na(a$run_id)) paste0("acp_v1.1_", a$date, "_", substr(head_commit, 1, 7)) else a$run_id
  dev <- startsWith(run_id, "dev")
  dirty <- acp_git(c("status", "--porcelain", "--untracked-files=all", "--", ACP_RUN_CODE, ACP_CRITERIA_FILE, ACP_SCHEDULE_FILE))
  if (!dev && length(dirty)) stop("A release run needs committed code, criteria and schedule:\n", paste(dirty, collapse = "\n"), call. = FALSE)
  if (!dev && (!is.na(a$raw_dir) || !is.na(a$board_history))) stop("--raw-dir and --board-history are for dev runs only.", call. = FALSE)
  raw_dir <- if (is.na(a$raw_dir)) file.path(dirname(mmm_raw_antenna_root(project_root)), "calibration", a$date) else a$raw_dir
  bh_path <- if (is.na(a$board_history)) {
    file.path(mmm_raw_acquisition_qc_root(project_root), "runs", ACP_BOARD_HISTORY_RUN, "tables", "board_history.csv")
  } else a$board_history
  for (f in c(raw_dir, file.path(raw_dir, ACP_RAW_FILES), bh_path)) if (!file.exists(f)) stop("Missing input: ", f, call. = FALSE)
  bh_sha <- acp_content_sha256(bh_path)
  if (is.na(a$board_history) && !identical(bh_sha, ACP_BOARD_HISTORY_SHA256)) {
    stop("The Stage 00b board history differs from the pinned ", ACP_BOARD_HISTORY_RUN, " content: ", bh_path, call. = FALSE)
  }
  dirs <- acp_run_dirs(if (is.na(a$output_root)) file.path(dirname(project_root), "RANM", "calibration") else a$output_root, run_id, project_root)
  log_path <- acp_day_log(raw_dir)
  subst_path <- file.path(raw_dir, ACP_SUBSTITUTION_FILE)
  boards <- if (file.exists(subst_path)) acp_read_substitution(subst_path) else ACP_BOARDS
  inputs <- data.table::data.table(role = c("raw log", names(ACP_RAW_FILES), "Stage 00b board history", if (file.exists(subst_path)) "board substitution"),
                                   file = c(log_path, file.path(raw_dir, ACP_RAW_FILES), bh_path, if (file.exists(subst_path)) subst_path))
  inputs[, `:=`(sha256 = mmm_file_sha256(file), bytes = file.size(file))]
  man_path <- file.path(raw_dir, "sha256_manifest.csv")
  if (file.exists(man_path)) {
    rm_ <- data.table::fread(man_path, colClasses = "character")
    chk <- merge(inputs[, .(file = basename(file), sha256)], rm_[, .(file = basename(file), listed = tolower(sha256))], by = "file")
    if (any(chk$sha256 != chk$listed)) stop("Raw input differs from its sha256 manifest: ", paste(chk[sha256 != listed]$file, collapse = ", "), call. = FALSE)
    if (file.exists(subst_path) && !ACP_SUBSTITUTION_FILE %in% basename(rm_$file)) {
      stop("The board substitution file is not listed in sha256_manifest.csv; an authorization must be archived with the day's data.", call. = FALSE)
    }
  }

  # the analysis (pure)
  tags <- acp_read_tags(file.path(raw_dir, ACP_RAW_FILES[["tags"]]))
  session <- acp_read_session(log_path, tags)
  sheet <- acp_read_sheet(file.path(raw_dir, ACP_RAW_FILES[["sheet"]]), sched, boards)
  if (any(sheet$problems$check == "board_mismatch")) {
    stop("A placement was made on another physical board than the schedule", if (file.exists(subst_path)) " and the authorized substitution" else "",
         " name (", paste(head(sheet$problems[check == "board_mismatch"]$placement_id, 10), collapse = ", "), "): never substituted silently. ",
         "Abort and reschedule, or archive an authorized ", ACP_SUBSTITUTION_FILE, " (acp_read_substitution()).", call. = FALSE)
  }
  ann <- acp_read_annotation(file.path(raw_dir, ACP_RAW_FILES[["movement"]]), sheet$sheet)
  survey <- acp_read_survey(file.path(raw_dir, ACP_RAW_FILES[["survey"]]))
  bh <- data.table::fread(bh_path, select = c("board", "mean_gap_ms", "lattice_k1"))   # acquisition columns only
  res <- acp_analyse(sched, sheet, session, ann, survey, bh, crit, boards = boards)
  res$sheet_problems <- sheet$problems
  res$annotation_problems <- ann$problems

  # outputs, built in .tmp_<run_id>
  for (d in c("tables", "manifest")) dir.create(file.path(dirs$tmp, d), recursive = TRUE, showWarnings = FALSE)
  data.table::fwrite(res$decisions, file.path(dirs$tmp, "decisions.csv"))
  data.table::fwrite(res$day2, file.path(dirs$tmp, "day2_recommendation.csv"))
  acp_write_tables(res[setdiff(names(res), c("decisions", "day2"))], file.path(dirs$tmp, "tables"))
  data.table::fwrite(crit, file.path(dirs$tmp, "manifest", "criteria_used.csv"))
  code <- data.table::data.table(file = c(ACP_RUN_CODE, ACP_CRITERIA_FILE, ACP_SCHEDULE_FILE))
  code[, sha256 := mmm_file_sha256(file.path(MMM_REPO_ROOT, file))]
  data.table::fwrite(code, file.path(dirs$tmp, "manifest", "code_hashes.csv"))
  data.table::fwrite(inputs, file.path(dirs$tmp, "manifest", "input_hashes.csv"))
  run <- data.table::data.table(item = c("run_id", "criteria_version", "criteria_content_sha256", "criteria_pinned_sha256", "schedule_version",
                                         "schedule_content_sha256", "schedule_pinned_sha256", "board_history_run", "board_history_content_sha256",
                                         "board_history_pinned", "boards", "board_substitution", "bootstrap_B", "bootstrap_seed", "git_commit", "code_clean", "dev_run", "raw_dir",
                                         "raw_sha256_manifest", "started", "finished", "R"),
                                value = c(run_id, ACP_VERSION, attr(crit, "sha256"), ACP_CRITERIA_SHA256, ACP_SCHEDULE_VERSION, attr(sched, "sha256"),
                                          ACP_SCHEDULE_SHA256, ACP_BOARD_HISTORY_RUN, bh_sha, as.character(identical(bh_sha, ACP_BOARD_HISTORY_SHA256)),
                                          paste(sprintf("role %s -> board %d", names(boards), boards), collapse = "; "),
                                          if (file.exists(subst_path)) paste(apply(attr(boards, "substitution"), 1, paste, collapse = " | "), collapse = "; ") else "none",
                                          format(acp_th(crit, "G02"), scientific = FALSE), format(acp_th(crit, "G03"), scientific = FALSE), head_commit,
                                          as.character(!length(dirty)), as.character(dev), raw_dir, as.character(file.exists(man_path)), started,
                                          format(Sys.time(), "%Y-%m-%d %H:%M:%S"), R.version.string))
  data.table::fwrite(run, file.path(dirs$tmp, "manifest", "run.csv"))
  writeLines(capture.output(sessionInfo()), file.path(dirs$tmp, "manifest", "session_info.txt"))
  rec <- res$day2[trigger == "recommendation"]$reason
  writeLines(c(paste("RFID CALIBRATION PILOT: pre-registered analysis,", ACP_VERSION, "/", ACP_SCHEDULE_VERSION, "(group-blind; isolated)"), "",
               paste("run:", run_id, "| commit:", head_commit, "| day:", a$date), "",
               "decisions.csv               one row per decision or supporting statistic: criterion, rule_id, estimate, 95% CI, decision",
               "day2_recommendation.csv     the day-2 rule (D1-D7): triggers and content; a recommendation only, Tobias decides",
               "tables/                     endpoints (detection, confusion, co-reads, competition, shielding, board ratio, flicker,",
               "                            visits, boundary timing, known-empty, cadence, vendor emulation), sheet and annotation checks",
               "manifest/                   criteria used, code, input and output hashes, run parameters, session info", "",
               paste("Day-2 rule:", rec),
               paste("Boards:", paste(sprintf("role %s -> board %d", names(boards), boards), collapse = "; "),
                     if (file.exists(subst_path)) "(authorized substitution; manifest/run.csv)" else "(no substitution)"),
               "Criteria may not be changed after the data; a change is a new criteria version reported next to the earlier one.",
               "No group label or outcome was read; the only Exp9 input is the Stage 00b board history."),
             file.path(dirs$tmp, "README.txt"))
  outs <- list.files(dirs$tmp, recursive = TRUE)
  data.table::fwrite(data.table::data.table(file = outs, sha256 = mmm_file_sha256(file.path(dirs$tmp, outs)), bytes = file.size(file.path(dirs$tmp, outs))),
                     file.path(dirs$tmp, "manifest", "output_files_sha256.csv"))
  if (file.exists(dirs$out)) stop("Run folder appeared during the run: ", dirs$out, call. = FALSE)
  if (!file.rename(dirs$tmp, dirs$out)) stop("Could not rename ", dirs$tmp, call. = FALSE)
  message("calibration pilot analysis done: ", dirs$out, "\n", rec)
}
