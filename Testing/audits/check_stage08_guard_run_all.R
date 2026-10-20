# ================================================================
# Check: the Stage 08 overwrite protection through the real entry point Analysis/run_all_analysis.R
# MMMSociability -- Testing/audits/check_stage08_guard_run_all.R
# ================================================================
# Opt-in (not part of the portable suite, which must not run Analysis scripts). Everything happens under tempdir():
#   1. a stand-in for the live root (activated layout receipts, the 72 pinned HMM files as placeholders, the live root
#      declared an alias): run_all_analysis.R with options(mmm.pipeline_stages = "08") must stop with the Stage 08 guard
#      refusal, and no file of the stand-in may change;
#   2. an isolated root (no alias, like RANT/RANU/RANV/RANA): the guard lets Stage 08 start; it then stops on its
#      missing Stage 01 input, which shows the protection does not block isolated runs.
# Before sourcing the runner the script asserts that mmm_project_root() is the temporary root.
# Run from the repository root:  Rscript Testing/audits/check_stage08_guard_run_all.R
# ================================================================

check_root <- function(name, live_alias) {
  source("Functions/project_paths.R", local = TRUE)
  pins <- utils::read.csv("docs/frozen_output_pins/stage08_hmm_states_20261019.csv", stringsAsFactors = FALSE)
  rel <- sub(paste0("^", MMM_PROJECT_ROOT_DEFAULT, "/"), "", pins$input)
  root <- normalizePath(file.path(tempdir(), name), winslash = "/", mustWork = FALSE)
  for (g in c("hmm_states_10min", "hmm_states_5min")) {
    dir.create(mmm_behavior_output_group_root(g, "current", root), recursive = TRUE, showWarnings = FALSE)
    dir.create(mmm_behavior_output_group_root(g, "semantic", root), recursive = TRUE, showWarnings = FALSE)
    rec <- file.path(root, "analysis_ready", "_migration_control", paste0(g, ".json"))
    dir.create(dirname(rec), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(list(group = g, state = "activated", files = 36L, group_plan_sha256 = strrep("a", 64), contract_sha256 = strrep("b", 64),
                              target_root_rel = if (g == "hmm_states_10min") "analyses/hmm_states/10min" else "analyses/hmm_states/5min",
                              source_retained = TRUE), rec, auto_unbox = TRUE)
  }
  for (r in rel) { p <- file.path(root, r); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); writeLines(paste("pinned", r), p) }
  snap <- function() { f <- sort(list.files(root, recursive = TRUE, all.files = TRUE))
    data.frame(f = f, sha = vapply(file.path(root, f), function(p) digest::digest(file = p, algo = "sha256"), ""), stringsAsFactors = FALSE) }
  before <- snap()
  options(mmm.project_root = root, mmm.pipeline_stages = "08", mmm.allow_pinned_overwrite = FALSE,
          mmm.live_root_aliases = if (live_alias) c(MMM_PROJECT_ROOT_DEFAULT, root) else NULL)
  Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = root, MMM_ALLOW_PINNED_OVERWRITE = "")
  if (!identical(normalizePath(mmm_project_root(), winslash = "/", mustWork = FALSE), root)) stop("The project root did not resolve to the temporary root.")
  msg <- tryCatch({ source("Analysis/run_all_analysis.R"); "finished without error" }, error = function(e) conditionMessage(e))
  after <- snap()
  list(root = root, message = msg, unchanged_pinned = identical(before[before$f %in% rel, ], after[after$f %in% rel, ]),
       unchanged_all = identical(before, after))
}

if (sys.nframe() == 0L) {
  a <- check_root(paste0("s08_runall_live_", as.integer(Sys.time())), live_alias = TRUE)
  ok1 <- grepl("Stage 08 rewrites 72 file", a$message) && a$unchanged_all
  cat(sprintf("1. stand-in live root via run_all_analysis.R: %s\n   message: %s\n   every file unchanged: %s\n",
              if (ok1) "REFUSED BEFORE ANY WRITE (PASS)" else "FAIL", substr(a$message, 1, 160), a$unchanged_all))
  b <- check_root(paste0("s08_runall_isolated_", as.integer(Sys.time())), live_alias = FALSE)
  ok2 <- !grepl("Stage 08 rewrites", b$message) && b$unchanged_pinned
  cat(sprintf("2. isolated root via run_all_analysis.R: %s\n   message: %s\n   placeholder outputs unchanged before the stage failed on its input: %s\n",
              if (ok2) "NOT BLOCKED BY THE GUARD (PASS)" else "FAIL", substr(b$message, 1, 160), b$unchanged_pinned))
  unlink(c(a$root, b$root), recursive = TRUE)
  if (!(ok1 && ok2)) quit(status = 1)
  cat("PASS: Stage 08 protection through run_all_analysis.R\n")
}
