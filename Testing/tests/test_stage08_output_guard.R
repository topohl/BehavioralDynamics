# Overwrite protection of the live Stage 08 HMM outputs (Functions/frozen_input_guard.R; pin list
# docs/frozen_output_pins/stage08_hmm_states_20261019.csv; Analysis/08_hmm_behavioral_states_optional.R).
#
# A. the pin list: 72 files (36 per resolution) on the live root, historical hashes equal to the migration plan, current hashes equal to the rerun record; the 10-min dwell
#    table equal to the figure-support pin; registered once, with Stage 08 as a self-guarded writer
# B. a fixture standing in for the live root (activated layout receipts, the 72 files, the live root as an alias):
#    Stage 08's own statements, evaluated in a child environment as run_all_analysis.R sources a stage, stop at the
#    guard before any write; also a primary-only run; nothing in the fixture changes and nothing is backed up
# C. the override is deliberate and stage-scoped: "01" leaves Stage 08 refused; "08", "08,28" or TRUE let it start and
#    back up all 72 files first; a changed file then warns, without permission it stops
# D. indirect writes: another stage's snapshot covers the HMM pins, so a change by it stops the run
# E. parallel roots (RANT, RANU, RANV, RANA, sandboxes) are not guarded
# F. Stage 08 calls the guard before any write and after its last one; the runner leaves it to the stage
#
# Portable: temporary roots only; Stage 08 and the runner are parsed, never sourced as scripts or run.

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")
errors_with <- function(expr, pattern) { m <- tryCatch({ force(expr); "" }, error = function(e) conditionMessage(e)); nzchar(m) && grepl(pattern, m) }
source("Functions/project_paths.R")
source("Functions/frozen_input_guard.R")
sha <- function(f) digest::digest(file = f, algo = "sha256")
PIN <- MMM_REPO_PIN_LISTS[["stage08_hmm_20261019"]]
LIVE <- MMM_PROJECT_ROOT_DEFAULT

# ---------------------------------------------------------------- A. pin list
cat("A. pin list\n")
pl <- utils::read.csv(PIN, stringsAsFactors = FALSE, colClasses = "character")
rel <- sub(paste0("^", LIVE, "/"), "", pl$input)
check(nrow(pl) == 72 && !anyDuplicated(pl$input) && all(startsWith(pl$input, paste0(LIVE, "/analysis_ready/analyses/hmm_states/"))) &&
        sum(grepl("/hmm_states/10min/", pl$input)) == 36 && sum(grepl("/hmm_states/5min/", pl$input)) == 36,
      "A: 72 distinct live files, 36 per resolution")
# Historical migration pins remain immutable; current pins follow the reviewed producer rerun.
plan <- utils::read.csv("docs/BEHAVIOR_OUTPUT_MIGRATION_PLAN.csv", stringsAsFactors = FALSE)
plan <- plan[plan$group %in% c("hmm_states_10min", "hmm_states_5min"), ]
historical <- utils::read.csv("docs/frozen_output_pins/stage08_hmm_states_20260922.csv", stringsAsFactors = FALSE)
historical_rel <- sub(paste0("^", LIVE, "/"), "", historical$input)
hm <- match(historical_rel, paste0("analysis_ready/", plan$target_root_rel, "/", plan$target_file))
check(!anyNA(hm) && all(historical$sha256 == plan$source_sha256[hm]), "A: historical pins equal the preserved migration plan")
record <- utils::read.csv("docs/behavior_output_producer_reruns/stage08_20261019.csv", stringsAsFactors = FALSE)
m <- match(rel, paste0("analysis_ready/", record$target_root_rel, "/", record$target_file))
check(nrow(record) == 72L && !anyNA(m) && all(pl$sha256 == record$sha256[m]), "A: current pins equal the reviewed producer rerun record")
check(all(record$prior_sha256[m] == historical$sha256[match(rel, historical_rel)]), "A: rerun prior hashes preserve the historical pins")

fsb <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("FSB_HMM_DWELL")),
              as.list(parse("Functions/figure_support_bundle.R")))
dwell <- eval(fsb[[1]][[3]], baseenv())
check(pl$sha256[endsWith(pl$input, "/10min/tables/hmm_state_dwell_times.csv")] == dwell$sha256, "A: the dwell table equals the figure-support pin")
check(sum(grepl("stage08", c(names(MMM_FROZEN_PIN_LISTS), names(MMM_REPO_PIN_LISTS)))) == 1 && "08" %in% MMM_SELF_GUARDED_STAGES &&
        identical(MMM_PINNED_WRITER_ROOTS[["08"]], "analysis_ready/analyses/hmm_states"), "A: registered once; Stage 08 guards its own folders")
ok("72 pinned files, consistent with historical migration, rerun record and figure-support pin")

# ---------------------------------------------------------------- fixtures
make_root <- function(name, with_files = TRUE) {
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
  if (with_files) for (r in rel) { p <- file.path(root, r); dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); writeLines(paste("pinned", r), p) }
  root
}
snapshot_files <- function(root) { f <- sort(list.files(root, recursive = TRUE, all.files = TRUE)); data.frame(f = f, sha = vapply(file.path(root, f), sha, ""), mtime = as.numeric(file.mtime(file.path(root, f)))) }
s08 <- parse("Analysis/08_hmm_behavioral_states_optional.R", keep.source = TRUE)
txt08 <- vapply(attr(s08, "srcref"), function(s) paste(as.character(s), collapse = "\n"), "")
guard_at <- which(startsWith(txt08, "frozen_guard_snapshot <- mmm_frozen_guard_before(\"08\""))
check(length(guard_at) == 1, "the Stage 08 guard statement exists once")
# Stage 08's statements up to and including the guard, in a child environment (as run_all_analysis.R sources a stage).
stage08_prologue <- function(root, extra_options = list()) {
  old <- options(c(list(mmm.project_root = root), extra_options)); old_env <- Sys.getenv("MMM_BEHAVIOR_PROJECT_ROOT", unset = NA)
  Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = root)
  on.exit({ options(old); if (is.na(old_env)) Sys.unsetenv("MMM_BEHAVIOR_PROJECT_ROOT") else Sys.setenv(MMM_BEHAVIOR_PROJECT_ROOT = old_env) })
  E <- new.env(parent = globalenv())
  for (i in seq_len(guard_at)) {
    if (i == guard_at && !identical(normalizePath(E$project_root, winslash = "/", mustWork = FALSE), root)) fail("the prologue resolved another root")
    eval(s08[[i]], E)
  }
  E
}

# ---------------------------------------------------------------- B. refusal on the live root
cat("\nB. refusal before any write\n")
live <- make_root(paste0("s08live_", as.integer(Sys.time())))
options(mmm.live_root_aliases = c(LIVE, live), mmm.allow_pinned_overwrite = FALSE); Sys.setenv(MMM_ALLOW_PINNED_OVERWRITE = "")
check(nrow(mmm_frozen_guard_pins(live)) == 72, "B: on the stand-in live root the 72 pins map onto its files")
before <- snapshot_files(live)
check(errors_with(suppressMessages(stage08_prologue(live)), "Stage 08 rewrites 72 file.*hmm_states/10min.*hmm_states/5min.*stage08_hmm_20261019"),
      "B: Stage 08 stops at its guard, naming both resolutions and the pin list")
check(errors_with(suppressMessages(stage08_prologue(live, list(mmm.hmm.sensitivity_bin_levels = character(0)))), "Stage 08 rewrites 36 file"),
      "B: a primary-only run is refused for its 36 files")
check(identical(snapshot_files(live), before), "B: no file of the stand-in live root changed and nothing was written or backed up")
ok("Stage 08 refuses to start on the live root, before any write")

# ---------------------------------------------------------------- C. deliberate, stage-scoped override
cat("\nC. override\n")
check(errors_with(suppressMessages(stage08_prologue(live, list(mmm.allow_pinned_overwrite = "01"))), "Stage 08 rewrites 72"),
      "C: permission for Stage 01 does not open Stage 08")
check(mmm_pinned_overwrite_allowed("08") == FALSE && { options(mmm.allow_pinned_overwrite = "08"); mmm_pinned_overwrite_allowed("08") } &&
        !mmm_pinned_overwrite_allowed("01") && !mmm_pinned_overwrite_allowed(), "C: \"08\" allows Stage 08 only (not the global form)")
options(mmm.allow_pinned_overwrite = FALSE)
Sys.setenv(MMM_ALLOW_PINNED_OVERWRITE = "08,28"); check(mmm_pinned_overwrite_allowed("08") && mmm_pinned_overwrite_allowed("28") && !mmm_pinned_overwrite_allowed("01"), "C: the variable lists stages")
Sys.setenv(MMM_ALLOW_PINNED_OVERWRITE = "1"); check(mmm_pinned_overwrite_allowed("01") && mmm_pinned_overwrite_allowed(), "C: 1 keeps meaning every stage")
Sys.setenv(MMM_ALLOW_PINNED_OVERWRITE = "")
E <- suppressMessages(stage08_prologue(live, list(mmm.allow_pinned_overwrite = "08")))
snap <- E$frozen_guard_snapshot
check(isTRUE(attr(snap, "allowed")) && nrow(snap) == 72, "C: with \"08\" the stage may start and keeps a snapshot of the pins")
bk <- list.files(file.path(live, "analysis_ready/_migration_control"), pattern = "^pinned_inputs_before_stage08_", full.names = TRUE)
man <- utils::read.csv(file.path(bk, "backup_manifest.csv"), stringsAsFactors = FALSE)
check(length(bk) == 1 && nrow(man) == 72 && all(man$sha256_backup == vapply(file.path(live, man$rel), sha, "")), "C: all 72 files were backed up first, byte for byte")
Sys.sleep(1.1); writeLines("changed", file.path(live, rel[1]))
w <- tryCatch({ mmm_frozen_guard_after(snap); "" }, warning = function(w) conditionMessage(w))
check(grepl("Stage 08 changed 1 pinned file.*ACTIVATION_RECORD", w), "C: a permitted change warns and asks for an activation record")
ok("only a stage-scoped or global permission opens Stage 08, with a full backup first")

# ---------------------------------------------------------------- D. indirect writes
cat("\nD. indirect writes\n")
s14 <- mmm_frozen_guard_before("14", live)
check(is.data.frame(s14) && sum(grepl("hmm_states", s14$rel)) == 72, "D: another stage's snapshot covers the HMM pins")
Sys.sleep(1.1); writeLines("changed again", file.path(live, rel[2]))
check(errors_with(mmm_frozen_guard_after(s14), "Stage 14 changed .*hmm_states"), "D: a change to an HMM output by another stage stops the run")
ok("indirect writes to the HMM outputs are caught after any guarded stage")

# ---------------------------------------------------------------- E. parallel roots
cat("\nE. parallel roots\n")
for (nm in c("RANT", "RANA")) {
  par <- make_root(paste0(nm, "_", as.integer(Sys.time())))
  check(is.null(mmm_frozen_guard_pins(par)) && is.null(mmm_frozen_guard_before("08", par)), paste("E:", nm, "has no pins"))
  E <- suppressMessages(stage08_prologue(par))
  check(is.null(E$frozen_guard_snapshot), paste("E: Stage 08 passes its guard in", nm))
}
ok("parallel and sandbox roots are not guarded")

# ---------------------------------------------------------------- F. Stage 08 and the runner (parsed)
cat("\nF. order of the calls\n")
writes <- c("ensure_dir", "analysis_output_dirs", "write_output_manifest", "write_table", "write_csv", "writeLines", "dir.create", "file.copy",
            "save_plot_svg_pdf", "harmonize_analysis_outputs", "run_hmm_resolution")
calls_in <- function(e) unique(all.names(e))
first_write <- which(vapply(s08, function(e) any(writes %in% calls_in(e)) && !startsWith(paste(deparse(e)[1], collapse = ""), "run_hmm_resolution <- function"), logical(1)))[1]
run_at <- which(startsWith(txt08, "hmm_run_qc <- withCallingHandlers(map_dfr(hmm_run_bin_levels, run_hmm_resolution), error = function(e) try(mmm_frozen_guard_after(frozen_guard_snapshot)))"))
after_at <- which(txt08 == "mmm_frozen_guard_after(frozen_guard_snapshot)")
check(guard_at < first_write && length(run_at) == 1 && length(after_at) == 1 && after_at == run_at + 1,
      "F: the guard precedes every write; the after-check follows the last one")
check(grepl("output_dir <- hmm_output_roots[[bin_level]]", paste(txt08, collapse = "\n"), fixed = TRUE), "F: the stage writes into the folders the guard checked")
runner <- paste(readLines("Analysis/run_all_analysis.R", warn = FALSE), collapse = "\n")
check(grepl("if (row$stage %in% MMM_SELF_GUARDED_STAGES) NULL else mmm_frozen_guard_before(row$stage)", runner, fixed = TRUE) &&
        grepl("mmm_frozen_guard_after(snapshot)", runner, fixed = TRUE), "F: the runner leaves Stage 08 to its own guard and checks every other stage")
ok("guard before the first write, after-check after the last; the runner relies on it")

options(mmm.live_root_aliases = NULL, mmm.allow_pinned_overwrite = NULL)
unlink(list.files(tempdir(), pattern = "^(s08live|RANT|RANA)_", full.names = TRUE), recursive = TRUE)
cat("\nPASS: Stage 08 output guard\n")
