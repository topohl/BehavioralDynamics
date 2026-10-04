# The frozen-input guard (Functions/frozen_input_guard.R) and its use by the pipeline runner and Stages 01 and 28.
#
# A. pins: only derived files under the root the pin lists record; a sandbox or alias root; conflicting pins.
# B. before(): a stage that writes pinned files is refused on that root; its real output folder decides; other stages
#    pass; with permission the pinned files are backed up first.
# C. after(): a byte-identical rewrite passes, changed bytes stop the run, or warn with permission.
# D. the runner (parsed, never run): no default run; 01 and 09 only in their own profiles; every stage guarded.
#
# Portable: synthetic temporary roots only, no S: access; no Analysis/ script is sourced or run.

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

source("Functions/frozen_input_guard.R")
sha <- function(f) digest::digest(file = f, algo = "sha256")
put <- function(path, text) { dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); writeLines(text, path); path }
pin_list <- function(path, files, extra = NULL) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  x <- data.frame(input = c(files, names(extra)), bytes = 1, sha256 = c(vapply(files, sha, ""), unname(extra)), role = "r")
  utils::write.csv(x, path, row.names = FALSE)
}
errors_with <- function(expr, pattern) {
  m <- tryCatch({ force(expr); "" }, error = function(e) conditionMessage(e))
  nzchar(m) && grepl(pattern, m)
}

root <- normalizePath(file.path(tempdir(), paste0("fig_", as.integer(Sys.time()))), winslash = "/", mustWork = FALSE)
f01 <- put(file.path(root, "analysis_ready/foundations/behavior_metrics/10min_based/all_behavior_metrics.csv"), "a,b")
f09 <- put(file.path(root, "analysis_ready/pipeline/09_early_prediction/10min/tables/model_ladder_input.csv"), "x,y")
fcz <- put(file.path(root, "analysis_ready/canonical/later_outcome_combz/tables/combz.csv"), "z")
fraw <- put(file.path(root, "MMMSociability/raw_data/B1/raw.csv"), "r")
pin_list(file.path(root, MMM_FROZEN_PIN_LISTS[["stage29_v101"]]), c(f01, f09, fcz, fraw),
         extra = c("S:/elsewhere/sus_animals.csv" = strrep("0", 64)))
pin_list(file.path(root, MMM_FROZEN_PIN_LISTS[["stage32_v11"]]), c(f09, fcz))

# ------------------------------------------------------------------ A. pins
cat("A. pinned files\n")
p <- mmm_frozen_guard_pins(root)
check(is.data.frame(p) && nrow(p) == 3L, "A: three derived pinned files (raw data and files outside the root ignored)")
check(identical(p$pin_lists[p$rel == "analysis_ready/pipeline/09_early_prediction/10min/tables/model_ladder_input.csv"],
                "stage29_v101,stage32_v11"), "A: a file pinned by two runs lists both")
sandbox <- file.path(tempdir(), paste0("fig_sandbox_", as.integer(Sys.time())))
dir.create(sandbox, showWarnings = FALSE)
invisible(file.copy(file.path(root, "analysis_ready"), sandbox, recursive = TRUE))
check(is.null(mmm_frozen_guard_pins(sandbox)), "A: a sandbox holding copies is not the pinned root (guard silent)")
check(is.null(mmm_frozen_guard_before("01", sandbox)), "A: before() does nothing on a sandbox root")
check(is.null(mmm_frozen_guard_pins(file.path(tempdir(), "no_such_root"))), "A: no pin list, no pins")
# The pin lists record the drive form (X:/...); the caller names the same folder differently (the real temp path).
alias_root <- normalizePath(file.path(tempdir(), paste0("fig_alias_", as.integer(Sys.time()))), winslash = "/", mustWork = FALSE)
fa <- put(file.path(alias_root, "analysis_ready/pipeline/09_early_prediction/t.csv"), "t")
dir.create(dirname(file.path(alias_root, MMM_FROZEN_PIN_LISTS[["stage29_v101"]])), recursive = TRUE, showWarnings = FALSE)
utils::write.csv(data.frame(input = "X:/live/RFID/analysis_ready/pipeline/09_early_prediction/t.csv", sha256 = sha(fa)),
                 file.path(alias_root, MMM_FROZEN_PIN_LISTS[["stage29_v101"]]), row.names = FALSE)
check(is.null(mmm_frozen_guard_pins(alias_root)), "A: a root that is not a known alias of the recorded root is not pinned")
pa <- mmm_frozen_guard_pins(alias_root, aliases = c("X:/live/RFID", alias_root))
check(is.data.frame(pa) && nrow(pa) == 1L && identical(pa$path, fa), "A: a known alias maps the pinned files onto the caller's root")
check(.mmm_fg_same_place("s:/lab/x/rfid", "//server/share/lab/x/rfid") && .mmm_fg_same_place("//server/share/lab/x/rfid", "s:/lab/x/rfid") &&
        !.mmm_fg_same_place("s:/lab/x/rfid", "c:/sandbox/rfid") && !.mmm_fg_same_place("s:/lab/x/rfid", "//server/share/lab/y/rfid"),
      "A: a drive path and its UNC name are the same root; other roots are not")
ok("pins are the derived files under the recorded root; sandbox and alias roots are handled")

# ------------------------------------------------------------------ B. before()
cat("\nB. refusal before a stage\n")
options(mmm.allow_pinned_overwrite = FALSE); Sys.setenv(MMM_ALLOW_PINNED_OVERWRITE = "")
check(errors_with(mmm_frozen_guard_before("01", root), "Stage 01 rewrites 1 file.*all_behavior_metrics"),
      "B: Stage 01 is refused on the pinned root")
check(errors_with(mmm_frozen_guard_before("09", root), "Stage 09 rewrites 1 file.*model_ladder_input.*stage29_v101,stage32_v11"),
      "B: Stage 09 is refused and the message names both runs")
check(is.data.frame(mmm_frozen_guard_before("28", root)), "B: Stage 28 passes when none of its outputs is pinned")
cookie <- file.path(root, "cookiehab", "analysis_ready", "03_derived_metrics")
s01c <- mmm_frozen_guard_before("01", root, output_roots = cookie)
check(is.data.frame(s01c) && !isTRUE(attr(s01c, "allowed")), "B: Stage 01 writing to another folder (cookie data) is not refused")
s04 <- mmm_frozen_guard_before("04", root)
check(is.data.frame(s04) && nrow(s04) == 3L, "B: a stage without pinned outputs gets a snapshot of every pinned file")
check(!any(.mmm_fg_under(c("//server/share/x", "S:/a/b"), NULL)), "B: no output folder, no match")
ok("01 and 09 refused, their real output folders decide, other stages snapshot")

# ------------------------------------------------------------------ C. after()
cat("\nC. check after a stage\n")
Sys.sleep(1.1); writeLines("x,y", f09)                       # same bytes, new time stamp
check(isTRUE(suppressMessages(mmm_frozen_guard_after(s04))), "C: a byte-identical rewrite passes")
s04 <- mmm_frozen_guard_before("04", root)
Sys.sleep(1.1); writeLines("x,y,changed", f09)
check(errors_with(mmm_frozen_guard_after(s04), "Stage 04 changed 1 pinned file.*model_ladder_input"),
      "C: changed bytes stop the run")
writeLines("x,y", f09)
check(isTRUE(mmm_frozen_guard_after(NULL)), "C: no snapshot, nothing to check")
ok("byte-identical rewrites pass, changed bytes stop")

cat("\nD. permission and backup\n")
options(mmm.allow_pinned_overwrite = TRUE)
s09 <- suppressMessages(mmm_frozen_guard_before("09", root))
check(isTRUE(attr(s09, "allowed")), "D: with permission Stage 09 may start")
bk <- list.files(file.path(root, "analysis_ready/_migration_control"), pattern = "^pinned_inputs_before_stage09_", full.names = TRUE)
check(length(bk) == 1L && file.exists(file.path(bk, "analysis_ready/pipeline/09_early_prediction/10min/tables/model_ladder_input.csv")),
      "D: the pinned file was backed up first")
man <- utils::read.csv(file.path(bk, "backup_manifest.csv"), stringsAsFactors = FALSE)
check(nrow(man) == 1L && identical(man$sha256, man$sha256_backup), "D: the backup manifest shows identical bytes")
Sys.sleep(1.1); writeLines("x,y,new", f09)
w <- tryCatch({ mmm_frozen_guard_after(s09); "" }, warning = function(w) conditionMessage(w))
check(grepl("changed 1 pinned file.*ACTIVATION_RECORD", w), "D: a permitted change warns and asks for an activation record")
options(mmm.allow_pinned_overwrite = FALSE)
pin_list(file.path(root, MMM_FROZEN_PIN_LISTS[["stage30_v10"]]), character(0), extra = setNames(strrep("1", 64), f01))
check(errors_with(mmm_frozen_guard_pins(root), "disagree"), "D: pin lists that disagree on a file stop the guard")
ok("permission backs up first and warns; conflicting pins stop")

# ------------------------------------------------------------------ E. runner and stage scripts (parsed only)
cat("\nE. runner and stage scripts\n")
runner <- "Analysis/run_all_analysis.R"
ex <- as.list(parse(runner, keep.source = FALSE))
prof_expr <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("PIPELINE_PROFILES")), ex)
check(length(prof_expr) == 1L, "E: the runner defines PIPELINE_PROFILES once")
prof <- eval(prof_expr[[1]][[3]], new.env(parent = baseenv()))
check(all(unlist(prof) %in% sprintf("%02d", 1:15)), "E: profiles name stages 01-15 only")
check(identical(names(Filter(function(s) "09" %in% s, prof)), "early_prediction") &&
        identical(names(Filter(function(s) "01" %in% s, prof)), "heatmap_inputs"),
      "E: Stage 09 only in early_prediction, Stage 01 only in heatmap_inputs")
src <- paste(readLines(runner, warn = FALSE), collapse = "\n")
check(grepl("runs nothing by default", src, fixed = TRUE), "E: the runner refuses to run without a profile or stage list")
check(grepl("mmm_frozen_guard_before(row$stage)", src, fixed = TRUE) && grepl("mmm_frozen_guard_after(snapshot)", src, fixed = TRUE),
      "E: the runner guards every stage it runs itself")
for (st in MMM_SELF_GUARDED_STAGES) {
  f <- list.files("Analysis", pattern = paste0("^", st, "_.*[.]R$"), full.names = TRUE)
  s <- paste(readLines(f, warn = FALSE), collapse = "\n")
  check(length(f) == 1L && grepl(sprintf('mmm_frozen_guard_before("%s"', st), s, fixed = TRUE) &&
          grepl("mmm_frozen_guard_after(frozen_guard_snapshot)", s, fixed = TRUE),
        paste0("E: Stage ", st, " calls the guard before its first write and at its end"))
}
s09src <- readLines(list.files("Analysis", pattern = "^09_.*[.]R$", full.names = TRUE), warn = FALSE)
check(!any(grepl("frozen_guard", s09src)), "E: Stage 09 itself is unchanged (the runner guards it)")
ok("no default run; 01 and 09 isolated in profiles; runner, Stage 01 and Stage 28 call the guard")

unlink(c(root, sandbox, alias_root), recursive = TRUE)
cat("\nPASS: frozen-input guard\n")
