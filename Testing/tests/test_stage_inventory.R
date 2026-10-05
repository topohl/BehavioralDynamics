# Analysis/STAGE_INVENTORY.csv lists every Analysis script once and agrees with the code.
#
#   1. one row per Analysis/*.R and Analysis/_supporting/*.R, no row for a missing file, no empty layer or status;
#   2. runner profiles equal PIPELINE_PROFILES in Analysis/run_all_analysis.R (parsed, never run);
#   3. the legacy products retired on 2026-10-05 stay retired: no legacy-product row, no script behind their opt-in;
#   4. the stages that rewrite pinned inputs (Functions/frozen_input_guard.R) say so, and frozen-lineage rows are frozen.
#
# Portable: repository files only.

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

inv <- utils::read.csv("Analysis/STAGE_INVENTORY.csv", stringsAsFactors = FALSE, na.strings = character(0))
need <- c("script", "stage", "layer", "status", "status_confirmed", "runner_profile", "output_root", "write_policy", "note")
check(identical(names(inv), need), paste("columns must be:", paste(need, collapse = ", ")))

cat("1. coverage\n")
scripts <- c(list.files("Analysis", pattern = "[.][Rr]$"), file.path("_supporting", list.files("Analysis/_supporting", pattern = "[.][Rr]$")))
check(!anyDuplicated(inv$script), paste("duplicate rows:", paste(inv$script[duplicated(inv$script)], collapse = ", ")))
check(!length(setdiff(scripts, inv$script)), paste("scripts without a row:", paste(setdiff(scripts, inv$script), collapse = ", ")))
check(!length(setdiff(inv$script, scripts)), paste("rows for missing scripts:", paste(setdiff(inv$script, scripts), collapse = ", ")))
check(all(nzchar(inv$layer) & nzchar(inv$status) & inv$status_confirmed %in% c("yes", "no")),
      "every row needs a layer, a status and status_confirmed yes/no")
check(all(grepl("^proposed:", inv$status) == (inv$status_confirmed == "no")),
      "exactly the unconfirmed rows carry a 'proposed:' status")
ok(sprintf("%d scripts, one row each (%d statuses still to confirm)", length(scripts), sum(inv$status_confirmed == "no")))

cat("\n2. runner profiles\n")
ex <- Filter(function(e) is.call(e) && identical(e[[1]], as.name("<-")) && identical(e[[2]], as.name("PIPELINE_PROFILES")),
             as.list(parse("Analysis/run_all_analysis.R", keep.source = FALSE)))
prof <- eval(ex[[1]][[3]], new.env(parent = baseenv()))
from_runner <- stack(prof)
from_runner <- setNames(as.character(from_runner$ind), from_runner$values)
in_inv <- inv[nzchar(inv$runner_profile), ]
check(setequal(names(from_runner), in_inv$stage), "the stages with a runner profile are the stages the runner's profiles name")
check(all(from_runner[in_inv$stage] == in_inv$runner_profile), "each stage's runner profile matches PIPELINE_PROFILES")
ok(sprintf("%d stages in %d profiles match the runner", nrow(in_inv), length(prof)))

cat("\n3. retired legacy products\n")
gated <- scripts[vapply(file.path("Analysis", scripts), function(f) any(grepl("MMM_ALLOW_LEGACY_BEHAVIOR_PRODUCTS", readLines(f, warn = FALSE), fixed = TRUE)), logical(1))]
check(!length(gated), paste("the legacy products were retired on 2026-10-05; scripts behind their opt-in:", paste(gated, collapse = ", ")))
check(!any(inv$layer == "legacy product"), paste("legacy-product rows:", paste(inv$script[inv$layer == "legacy product"], collapse = ", ")))
ok("no legacy product and no script behind the retired opt-in")

cat("\n4. pinned producers and frozen lineage\n")
guard <- new.env(); sys.source("Functions/frozen_input_guard.R", envir = guard)
writers <- names(guard$MMM_PINNED_WRITER_ROOTS)
check(all(grepl("pinned by frozen runs", inv$write_policy[inv$stage %in% writers])),
      paste("the stages that rewrite pinned inputs must say so:", paste(writers, collapse = ", ")))
fl <- inv[inv$layer == "frozen lineage", ]
check(all(grepl("^frozen", fl$status)), "every frozen-lineage row has a frozen status")
ok(sprintf("stages %s marked as rewriting pinned inputs; %d frozen-lineage rows frozen", paste(writers, collapse = ", "), nrow(fl)))

cat("\nPASS: stage inventory\n")
