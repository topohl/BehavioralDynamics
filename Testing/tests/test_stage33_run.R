# Stage 33 run machinery (Functions/stage33_run.R) and runner structure (Analysis/33_posthoc_cohort_followups.R, read as
# text only - never sourced or executed):
#
#   1. runner: one sys.nframe() guard; arguments and opt-in before any read; gates before the design; writes only through
#      s33_write_run(); no estimate printed; no data-root or colour literal;
#   2. arguments, run id, refusals (.tmp_, existing folder, rerun rule) and the path budget;
#   3. seeds unique and in the declared ranges; replicate counts;
#   4. lint: README blocks and declared columns pass; a barred RFID label, a p column, a journal name and a
#      SIS-minus-CON column in module D fail; arena rows and a10/a11 are exempt;
#   5. the writer on a temporary project root, including the pre-copy refusal (GC-10); the run file set (= the planned
#      outputs), the run manifest, the README interval counts and the recorded deviations;
#   6. checkpoints: keys, reuse, integrity (a corrupted file is recomputed), no checkpoint without a folder;
#   7. the Stage 33 rows of Analysis/STAGE_INVENTORY.csv and of the output index.
# Portable: temporary folders only; no S: drive.

suppressPackageStartupMessages({ library(data.table); library(digest) })
fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")
must_error <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
for (f in c("Functions/stage30_figure_bundle.R", "Functions/stage32_run.R", "Functions/stage33_common.R", "Functions/stage33_run.R")) source(f)

cat("1. runner structure\n")
rt <- readLines("Analysis/33_posthoc_cohort_followups.R", warn = FALSE); code <- rt[!grepl("^\\s*#", rt)]; txt <- paste(code, collapse = "\n")
check(sum(grepl("sys.nframe() == 0L", code, fixed = TRUE)) == 1L, "exactly one sys.nframe() guard")
pos <- function(p) regexpr(p, txt, fixed = TRUE)[1]
check(pos("s33_parse_args(args)") > 0 && pos("s33_parse_args(args)") < pos("s33_design(paths"), "arguments parsed before the design is read")
check(pos("S33_OPTIN_ENV") < pos("s33_input_spec("), "opt-in checked before inputs are declared")
check(pos("s33_code_gates(") < pos("s33_design(paths") && pos("s33_input_gates(spec)") < pos("s33_design(paths"), "code and input gates before any data row")
check(pos("s33_attach_outcomes(") > pos("_phase2\"))"), "outcomes read after the outcome-free phase")
check(!any(grepl("fwrite\\(|write\\.csv\\(|saveRDS\\(|writeLines\\(|file\\.copy\\(", code)), "the runner writes nothing itself")
check(!any(grepl("print\\(|cat\\(", code)), "the runner prints no estimate (messages only)")
check(pos("s33_assemble_files(results") > pos("s33_lint(") && pos("s33_write_run(") > pos("s33_assemble_files(results"),
      "the file set is assembled after the lint and written only by the writer")
check(!any(grepl("S:/", code, fixed = TRUE)) && !any(grepl("#[0-9A-Fa-f]{6}\\b", code)), "no data-root or colour literal")
check(grepl("pdf(NULL)", txt, fixed = TRUE) && grepl("setwd(local_root)", txt, fixed = TRUE), "null graphics device and local working directory")
ok("one guard; order of reads; no writes, prints or literals")

cat("\n2. arguments, run id, refusals, budget\n")
check(identical(s33_parse_args(c("--real", "--modules=ABCDE")), LETTERS[1:5]), "--real --modules=ABCDE")
for (bad in list(character(), "--real", c("--real", "--modules=BA"), c("--real", "--modules=AA"), c("--real", "--modules=F"),
                 c("--dry-run", "--modules=A"), c("--real", "--modules=A", "--x")))
  check(must_error(s33_parse_args(bad)), paste("refuses", paste(bad, collapse = " ")))
check(identical(s33_run_id(c("A", "D"), "1234567abcdef"), "v1.0_AD_1234567"), "run id v1.0_<modules>_<commit7>")
fx <- file.path(tempdir(), paste0("s33run_", Sys.getpid())); out <- file.path(fx, "out"); dir.create(out, recursive = TRUE)
check(length(s33_output_refusals(out, "v1.0_A_1234567", "A")) == 0L, "an empty stage folder passes")
dir.create(file.path(out, "v1.0_D_aaaaaaa"))
check(must_error(s33_output_refusals(out, "v1.0_D_bbbbbbb", "D")), "an earlier run holding the module needs a rerun reason")
check(identical(s33_output_refusals(out, "v1.0_D_bbbbbbb", "D", "reason"), "v1.0_D_aaaaaaa"), "with a reason the earlier run is recorded")
check(identical(s33_output_refusals(out, "v1.0_A_bbbbbbb", "A"), "v1.0_D_aaaaaaa"), "other modules are not blocked")
dir.create(file.path(out, "v1.0_A_bbbbbbb")); check(must_error(s33_output_refusals(out, "v1.0_A_bbbbbbb", "A", "x")), "an existing run folder is refused")
dir.create(file.path(out, ".tmp_v1.0_C_ccccccc")); check(must_error(s33_output_refusals(out, "v1.0_E_ccccccc", "E")), "any .tmp_ entry is refused")
b <- s33_path_budget(paste0("S:/", strrep("x", 60)), "v1.0_ABCDE_1234567", s33_planned_outputs(LETTERS[1:5]))
check(isTRUE(b$ok) && b$longest <= 240L, "planned outputs fit the 240-character budget under a 62-character root")
check(!s33_path_budget(paste0("S:/", strrep("x", 170)), "v1.0_ABCDE_1234567", s33_planned_outputs(LETTERS[1:5]))$ok, "a long root fails the budget")
ok("arguments, run id, refusals, rerun rule and budget")

cat("\n3. seeds and replicate counts\n")
check(!anyDuplicated(S33_SEEDS) && !anyDuplicated(names(S33_SEEDS)), "seeds unique, one per stream")
pre <- substr(names(S33_SEEDS), 1, 1); ranges <- c(A = 3301, B = 3302, C = 3303, D = 3304, E = 3305)
check(all(S33_SEEDS %/% 10000L == ranges[pre]), "every seed lies in its module's 33<m>xxxx range")
check(identical(unname(S33_B[c("nonparametric", "parametric", "parametric_sensitivity", "wild")]), c(4000L, 1999L, 999L, 9999L)), "B 4000 / 1999 / 999 / 9999")
ok("seeds and B")

cat("\n4. lint\n")
pats <- s33_lint_patterns()
good <- list(a05_component_slopes = data.table(metric_id = "crossing_rate", metric_label = "RFID position-change rate", estimand = "within_cohort_slope",
                                               level = "animal_within_cohort", tier = S33_TIER),
             b10_arena_cohort_means = data.table(metric_id = "arena_distance", metric_label = "SLEAP arena distance (cm)", tier = S33_TIER),
             a10_s32_multiplicity_rows = data.table(note = "registered p-values copied verbatim", p_raw = 0.5))
readme <- s33_readme("v1.0_AB_1234567", c("A", "B"), "1234567", list(A = "a05_component_slopes", B = "b10_arena_cohort_means"), "3 recorded")
check(nrow(s33_lint(good, readme, pats)) == 0L, "declared columns, labels, README blocks, arena rows and a10/a11 pass")
bad1 <- list(a05_component_slopes = data.table(metric_id = "crossing_rate", metric_label = "RFID distance travelled", tier = S33_TIER))
bad2 <- list(a05_component_slopes = data.table(metric_id = "crossing_rate", p_value = 0.1, tier = S33_TIER))
bad3 <- list(d03_separation_by_window = data.table(sis_minus_con = 1, tier = S33_TIER))
bad4 <- list(b05_associations = data.table(note = "as in Nature methods", tier = S33_TIER))
check(nrow(s33_lint(bad1, character(), pats)) > 0L, "an RFID row with a barred label fails")
check(nrow(s33_lint(bad2, character(), pats)) > 0L, "a p column fails")
check(nrow(s33_lint(bad3, character(), pats)) > 0L, "a SIS-minus-CON column in module D fails")
check(nrow(s33_lint(bad4, character(), pats)) > 0L, "a journal name fails")
check(nrow(s33_lint(list(), "Batch expl" %s33or% "", pats)) == 0L && nrow(s33_lint(list(), paste0("Batch expl", "ains it"), pats)) > 0L, "batch attribution in the README fails")
plan_txt <- paste(readLines(S33_PLAN$path, warn = FALSE), collapse = "\n")
fixed_in_plan <- regmatches(plan_txt, regexpr("(?<=Fixed sentence: ')[^']+(?=')", plan_txt, perl = TRUE))
check(identical(unname(S33_FIXED_SENTENCES[["b_selection"]]), fixed_in_plan), "the fixed B2/B6 sentence is the plan's text verbatim")
fx_tab <- list(b12_arena_cohort_contrasts = data.table(metric_id = "crossing_rate", selection_statement = S33_FIXED_SENTENCES[["b_selection"]], tier = S33_TIER))
check(nrow(s33_lint(fx_tab, character(), pats)) == 0L, "a cell equal to the fixed sentence passes")
fx_tab$b12_arena_cohort_contrasts[, selection_statement := paste(selection_statement, "Cause unknown.")]
check(nrow(s33_lint(fx_tab, character(), pats)) > 0L, "an altered fixed sentence is linted")
S33Q_LINT_EXEMPT_COLUMNS <- "engine_notes"
check(all(c(S33_LINT_EXEMPT_CORE, "engine_notes") %in% s33_lint_exempt_columns("Q")) && identical(s33_lint_exempt_columns("Z"), S33_LINT_EXEMPT_CORE),
      "module-declared exempt columns join the core set")
eng <- list(c06_models = data.table(engine_notes = paste("the optimizer", "is due to converge"), tier = S33_TIER))
check(nrow(s33_lint(eng, character(), pats)) > 0L && nrow(s33_lint(eng, character(), pats, exempt_columns = s33_lint_exempt_columns("Q"))) == 0L,
      "a declared engine-message column is not linted")
rm(S33Q_LINT_EXEMPT_COLUMNS)
ok("scoped lint")

cat("\n5. writer (temporary project root)\n")
spec <- data.table(role = "in", path = file.path(fx, "in.csv"), sha256 = NA_character_, module = "core")
writeLines("a,b\n1,2", spec$path); spec[, sha256 := digest::digest(file = path, algo = "sha256")]
files <- list("A_components/tables/a01_cohort_component_means.csv" = data.table(Batch = "B1", estimate = 1.5, tier = S33_TIER),
              "README.txt" = c("line one", "line two"))
w <- s33_write_run(file.path(fx, "local"), file.path(fx, "proj"), "v1.0_A_1234567", files, spec)
check(all(w$checks$passed) && dir.exists(file.path(fx, "proj", "v1.0_A_1234567")) && !dir.exists(file.path(fx, "proj", ".tmp_v1.0_A_1234567")),
      "staged, verified, copied, renamed")
fl <- list.files(file.path(fx, "proj", "v1.0_A_1234567"), recursive = TRUE, full.names = TRUE)
check(length(fl) == 3L && all(file.access(fl, 2L) != 0L), "two files plus the manifest, read-only")
check(must_error(s33_write_run(file.path(fx, "local"), file.path(fx, "proj"), "v1.0_A_1234567", files, spec)), "an existing run folder is refused")
check(must_error(s33_write_run(file.path(fx, "local"), file.path(fx, "proj"), "v1.0_B_1234567", files, spec, pre_copy_check = function() stop("refused"))),
      "a failing pre-copy check (GC-10) leaves nothing on the root")
check(!dir.exists(file.path(fx, "proj", "v1.0_B_1234567")) && !dir.exists(file.path(fx, "proj", ".tmp_v1.0_B_1234567")), "nothing copied after the refusal")
writeLines("a,b\n1,3", spec$path)
check(must_error(s33_write_run(file.path(fx, "local"), file.path(fx, "proj"), "v1.0_C_1234567", files, spec)), "a changed input stops before the copy")
ok("writer with GC-10")

cat("\n5b. run file set, interval counts, deviations\n")
tabD <- data.table(level = "cohort", estimate = 1, ci_low = c(0, NA), ci_high = c(2, 3), resampling_low = 0.5, resampling_high = 1.5,
                   metric_label = "x", units = "u", lead = FALSE, interval_basis = "none", tier = S33_TIER)
resD <- list(D = list(tables = setNames(lapply(S33_TABLES$D, function(n) tabD), S33_TABLES$D),
                      audit = setNames(lapply(S33_AUDIT_TABLES$D, function(n) data.table(a = 1, tier = S33_TIER)), S33_AUDIT_TABLES$D)))
run <- list(repo = normalizePath(getwd(), winslash = "/"), run_id = "v1.0_D_1234567", commit = "1234567", root = "C:/x", modules = "D",
            stage33_files = "Functions/stage33_run.R", earlier = character(), rerun_reason = "", timings = list(phase1 = 1, phase2_D = 2),
            input_table = data.table(role = "in", observed_sha256 = "x"), prod_hashes = list(D = data.table(module = "D", product = "d01", sha256 = "x")),
            gates_all = s33_gate_rows("GC-1", "x", TRUE), lint = data.table(where = character(), pattern = character(), text = character()),
            checkpoints = data.table(), readme = c("a", "b"))
af <- s33_assemble_files(resD, run)
check(setequal(c(names(af), "audit/output_manifest.csv"), s33_planned_outputs("D")) && !anyDuplicated(names(af)), "file set = planned outputs")
check(length(s33_planned_outputs(LETTERS[1:5])) == 77L, "77 files in a full run (plan section 14)")
rm <- af[["audit/run_manifest.csv"]]
check(identical(names(rm), c("key", "value")) && rm[key == "run_id", value] == "v1.0_D_1234567" && rm[key == "seconds_phase2_D", value] == "2.0",
      "run_manifest has key/value rows (no data.table key argument)")
resD_bad <- resD; resD_bad$D$tables[[1]] <- NULL
check(must_error(s33_assemble_files(resD_bad, run)), "a missing declared table stops the assembly")
check(identical(s33_interval_counts(resD, "D"), c(D = as.integer(length(S33_TABLES$D) * 3L))), "interval counts = pairs of finite limits")
rd <- s33_readme("v1.0_D_1234567", "D", "1234567", S33_TABLES["D"], "1 recorded", deviations = s33_deviations("D"), partial = TRUE,
                 interval_counts = s33_interval_counts(resD, "D"))
check(any(grepl("^Intervals reported .*D 24; 24 in total[.]$", rd)) && any(grepl("^  - core: ", rd)), "README carries interval counts and deviations")
check(all(startsWith(s33_deviations("D"), "core: ")) && all(S33_DEVIATIONS$module %in% c("core", LETTERS[1:5])), "deviations by module")
check(nrow(s33_lint(list(), s33_readme("v1.0_ABCDE_1234567", LETTERS[1:5], "1234567", S33_TABLES, "1 recorded",
                                       deviations = s33_deviations(LETTERS[1:5])), pats)) == 0L, "the full README and the deviations pass the lint")
ok("file set, run manifest, interval counts, deviations")

cat("\n6. checkpoints\n")
ctx <- list(checkpoint_dir = file.path(fx, "ck"), run_mode = "test", head = "abc", checkpoint_log = new.env())
n <- 0; f1 <- function() { n <<- n + 1; data.table(x = 1:3) }
v1 <- s33_checkpoint(ctx, "E", "scenario_1", f1, list(seed = 1)); v2 <- s33_checkpoint(ctx, "E", "scenario_1", f1, list(seed = 1))
check(n == 1L && isTRUE(all.equal(v1, v2)), "a stored value is reused")
invisible(s33_checkpoint(ctx, "E", "scenario_1", f1, list(seed = 2))); check(n == 2L, "another key recomputes")
ckf <- list.files(ctx$checkpoint_dir, pattern = "[.]rds$", full.names = TRUE)[1]; writeBin(as.raw(1:10), ckf)
invisible(s33_checkpoint(ctx, "E", "scenario_1", f1, list(seed = 1))); invisible(s33_checkpoint(ctx, "E", "scenario_1", f1, list(seed = 2)))
check(n == 3L, "a corrupted checkpoint fails its sidecar check and is recomputed")
check(identical(s33_checkpoint(list(), "E", "x", function() 7), 7), "without a checkpoint folder the value is computed")
log <- rbindlist(mget(sort(ls(ctx$checkpoint_log)), envir = ctx$checkpoint_log)); check(nrow(log) == 5L && sum(log$reused) == 2L, "uses recorded")
ok("checkpoint keys, reuse, integrity")

cat("\n7. inventory and index rows\n")
inv <- read.csv("Analysis/STAGE_INVENTORY.csv", stringsAsFactors = FALSE)
r33 <- inv[inv$script == "33_posthoc_cohort_followups.R", ]
check(nrow(r33) == 1L && r33$runner_profile == "" && grepl("posthoc_cohort_followups/<run_id>/", r33$output_root) &&
        grepl("STAGE33_POSTHOC_COHORT_FOLLOWUPS_PLAN_v1.0.md", r33$note), "STAGE_INVENTORY row for Stage 33 (no runner profile)")
idx <- readLines("Functions/behavior_output_index.R", warn = FALSE)
check(sum(grepl('^    "33", "Post hoc cohort follow-ups', idx)) == 1L && any(grepl("analysis_ready/analyses/posthoc_cohort_followups/", idx, fixed = TRUE)),
      "output index row for Stage 33")
ok("Stage 33 rows")

Sys.chmod(list.files(fx, recursive = TRUE, full.names = TRUE), mode = "0666"); unlink(fx, recursive = TRUE)
cat("\nPASS: stage 33 run machinery\n")
