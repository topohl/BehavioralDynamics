# ================================================================
# Parallel run of the descriptive pipeline on a raw-antenna data version
# MMMSociability -- Analysis/run_raw_antenna_parallel.R
# ================================================================
# The raw-antenna data versions (Formatting/build_raw_antenna_data_versions.R) hold the raw MultiMouseMonitor logs in
# the schema of the vendor preprocessed AnimalPos files. This runner feeds one of them to the existing stages, unchanged,
# with every output in a separate project root beside the live one:
#   options(mmm.rant_version = "flip2s")  -> root <parent of live root>/RANT  (2-s flip filter; primary raw sensitivity)
#   options(mmm.rant_version = "noflip")  -> root <parent of live root>/RANU  (no flip filter; filter-dependence audit)
# Nothing runs unless mmm.rant_version is set. Optional: mmm.rant_stages (default: the descriptive tier below),
# mmm.rant_dv_date (data-version date, default "20261012"), mmm.rant_continue_on_error (default FALSE).
#
# Isolation (decided 2026-10-12):
#   - the target root must not be, or lie inside, the live data root; it holds its own data version (the builder writes
#     both into RANT; the noflip version is copied into RANU, hash-checked) and the files must match its manifest;
#   - MMM_BEHAVIOR_PROJECT_ROOT (read by Stage 09) and options(mmm.project_root) both point at the target root, the
#     input is options(mmm.preprocessed_dir), vendor seeding is off (mmm.seed_from_vendor_raw = FALSE) and the vendor
#     raw_data folder is unreachable; every setting is restored on exit;
#   - the CombZ endpoint tables (position-independent) are copied into the target root and hash-checked.
# Scope: the descriptive tier. Stage 14, 16b/c, 19 and the registered Stages 29-33 are not run here (the Stage 29
# raw sensitivity variant has its own script). A fresh root without receipts uses the numbered output layout.
#
# Run from the repository root:  options(mmm.rant_version = "flip2s"); source("Analysis/run_raw_antenna_parallel.R")
# ================================================================

RANT_VERSION <- getOption("mmm.rant_version", NULL)
RANT_DESCRIPTIVE_STAGES <- c("01_build_multiscale_behavior_metrics.R", "02_build_dyadic_rfid_contacts.R",
                             "03_primary_raw_movement_phase_stats.R", "04_temporal_instability.R",
                             "05_behavioral_state_space.R", "06_dynamic_social_networks.R",
                             "07_gamm_trajectory_features.R", "08_hmm_behavioral_states_optional.R",
                             "09_early_prediction_model_ladder.R", "10_systems_feature_prediction_ladder.R",
                             "11_behavioral_adaptation_kinetics.R", "12_sleep_like_quiescence_metrics.R",
                             "13_ethological_phase_organization.R", "20_first_night_gamm.R",
                             "22_repeated_cagechange_acute_gamm.R", "28_rfid_behavioral_domains.R")
RANT_ROOT_NAMES <- c(flip2s = "RANT", noflip = "RANU")

if (is.null(RANT_VERSION)) {
  message("run_raw_antenna_parallel.R: nothing to do; set options(mmm.rant_version = \"flip2s\" or \"noflip\").")
} else local({
  if (!RANT_VERSION %in% names(RANT_ROOT_NAMES)) stop("mmm.rant_version must be one of ", paste(names(RANT_ROOT_NAMES), collapse = ", "), call. = FALSE)
  repo <- normalizePath(getwd(), winslash = "/")
  if (!file.exists(file.path(repo, "Analysis", "_pipeline_setup.R"))) stop("Run from the repository root.", call. = FALSE)
  source(file.path(repo, "Functions", "project_paths.R"), local = TRUE)
  norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
  live_root <- norm(MMM_PROJECT_ROOT_DEFAULT)
  root <- norm(file.path(dirname(live_root), RANT_ROOT_NAMES[[RANT_VERSION]]))
  if (startsWith(tolower(paste0(root, "/")), tolower(paste0(live_root, "/")))) stop("The target root lies inside the live root.", call. = FALSE)
  dv_date <- getOption("mmm.rant_dv_date", "20261012")
  dv_dir <- file.path(root, "MMMSociability", "data_versions", paste0("raw_antenna_", RANT_VERSION, "_", dv_date))
  pre_dir <- file.path(dv_dir, "preprocessed_data")
  manifest <- utils::read.csv(file.path(dv_dir, "MANIFEST_SHA256.csv"), stringsAsFactors = FALSE)
  files <- sort(list.files(pre_dir, pattern = "_CC[0-9]_AnimalPos_preprocessed[.]csv$"))
  if (!identical(files, sort(manifest$file)) || length(files) != 24) stop("Data version files do not match the manifest.", call. = FALSE)
  got <- vapply(file.path(pre_dir, manifest$file), function(f) digest::digest(file = f, algo = "sha256"), "")
  if (!identical(unname(got), manifest$sha256)) stop("Data version files fail their SHA-256 manifest.", call. = FALSE)

  # CombZ endpoint (position-independent) into the target root, hash-checked.
  rel <- file.path("analysis_ready", "canonical", "later_outcome_combz", "tables")
  src <- file.path(live_root, rel); dst <- file.path(root, rel)
  dir.create(dst, recursive = TRUE, showWarnings = FALSE)
  for (f in list.files(src)) {
    if (!file.exists(file.path(dst, f))) file.copy(file.path(src, f), file.path(dst, f))
    if (!identical(digest::digest(file = file.path(src, f), algo = "sha256"), digest::digest(file = file.path(dst, f), algo = "sha256"))) {
      stop("Endpoint copy differs from the live table: ", f, call. = FALSE)
    }
  }

  old_env <- Sys.getenv(c("MMM_BEHAVIOR_PROJECT_ROOT", "MMM_PROJECT_ROOT"), unset = NA)
  path_opts <- grep("^mmm[.]", names(options()), value = TRUE)
  old_opts <- options()[path_opts]
  Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = root)
  Sys.unsetenv("MMM_PROJECT_ROOT")
  options(stats::setNames(rep(list(NULL), length(path_opts)), path_opts))   # no stray mmm.* path from the session
  options(mmm.project_root = root, mmm.preprocessed_dir = pre_dir, mmm.seed_from_vendor_raw = FALSE,
          mmm.raw_position_root = file.path(root, "no_vendor_raw_data"))
  on.exit({
    for (v in names(old_env)) if (is.na(old_env[[v]])) Sys.unsetenv(v) else do.call(Sys.setenv, stats::setNames(list(old_env[[v]]), v))
    options(stats::setNames(rep(list(NULL), length(grep("^mmm[.]", names(options()), value = TRUE))), grep("^mmm[.]", names(options()), value = TRUE)))
    options(old_opts)
  }, add = TRUE)
  if (!identical(norm(mmm_project_root()), root)) stop("The project root did not switch to ", root, call. = FALSE)

  stages <- getOption("mmm.rant_stages", RANT_DESCRIPTIVE_STAGES)
  continue_on_error <- isTRUE(getOption("mmm.rant_continue_on_error", FALSE))
  log_file <- file.path(root, "raw_antenna_run_log.csv")
  head <- system2("git", c("-C", repo, "rev-parse", "HEAD"), stdout = TRUE)
  for (s in stages) {
    t0 <- Sys.time(); message("\n==== ", RANT_VERSION, " | ", s, " -> ", root)
    err <- tryCatch({ source(file.path(repo, "Analysis", s), local = new.env(parent = globalenv())); NULL },
                    error = function(e) conditionMessage(e))
    row <- data.frame(started = format(t0, "%Y-%m-%d %H:%M:%S"), minutes = round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
                      version = RANT_VERSION, data_version = basename(dv_dir), stage = s, status = if (is.null(err)) "ok" else "failed",
                      error = if (is.null(err)) "" else substr(err, 1, 300), git_head = head)
    utils::write.table(row, log_file, sep = ",", row.names = FALSE, col.names = !file.exists(log_file), append = file.exists(log_file))
    if (!is.null(err)) {
      message("FAILED: ", s, ": ", err)
      if (!continue_on_error) stop("Stage failed: ", s, ": ", err, call. = FALSE)
    }
  }
  message("\nRaw-antenna run (", RANT_VERSION, ") finished; log: ", log_file)
})
