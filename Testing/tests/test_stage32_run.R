# Contract test for the Stage 32 runner contract, gates and writer (Functions/stage32_run.R; Analysis/32 read as text only).
#
# Synthetic data in memory / tempdir() only: nothing here reads project data or any S: path, and the runner is never sourced
# or executed (it is parsed and read as text). The repository copy of the frozen hypothesis table is read (docs/, not data).
# Checks:
#   1. static: the runner parses its single --real mode before reading anything, needs the commit7 opt-in, has no dry-run mode,
#      sources no Analysis/ file other than _pipeline_setup.R, writes no file itself, runs every gate before the first fit,
#      prints no estimate; the helpers hold no S: path and source nothing; this test sources only _pipeline_setup.R;
#   2. run name, LF-form hashing, sha list lookup; registry gates on a synthetic frozen copy (pass, and fail when a byte changes);
#   3. the implemented hypothesis map = the frozen docs/STAGE32_HYPOTHESES_v1.0.csv (13 rows, 6 families of m = 2 + H13);
#   4. forbidden-word scan; CSV preparation of POSIXct / factors / quotes and (addendum A1) whitespace trimming: the exact
#      v1.0 failure string '0 removed; ' is refused by the frozen writer as is and written after s32r_prepare;
#   5. writer (addendum A1): local staging of the tables, determinism check against pinned table hashes, completion and
#      verification of the local folder, then the S: copy; manifest, read-only files, README, QC PNGs; a gate detail with a
#      trailing space; a failure before the copy leaves nothing in the stage folder; refusal of a second run (any v1.1_* or
#      .tmp_v1.1_* entry); a .tmp_v1.0_* folder does not block v1.1; aborted-folder and addendum gates on synthetic copies;
#   6. QC plots render from synthetic descriptives and Module F tables; a single-phase panel draws no line.
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root:
#   Rscript Testing/tests/test_stage32_run.R

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
for (h in c("stage30_figure_bundle.R", "stage32_run.R", "stage32_reporting.R")) source_mmm_helper(h)
fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
errmsg <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
TMP <- file.path(normalizePath(tempdir(), winslash = "/"), "s32r"); unlink(TMP, recursive = TRUE, force = TRUE); dir.create(TMP)

# ---------------------------------------------------------------- 1. static checks
RUNNER <- file.path(MMM_REPO_ROOT, "Analysis", "32_behavior_exposure_adaptation.R")
HELPERS <- file.path(MMM_REPO_ROOT, "Functions", c("stage32_windows.R", "stage32_inference.R", "stage32_prediction.R", "stage32_run.R", "stage32_reporting.R"))
SELF_DIR <- file.path(MMM_REPO_ROOT, "Testing", "tests")
code_only <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
invisible(parse(file = RUNNER, keep.source = FALSE)); for (h in HELPERS) invisible(parse(file = h, keep.source = FALSE))
rl <- code_only(RUNNER)
first_read <- min(grep("\\b(fread|readLines|readRDS|s30fb_sha|s32r_registry_gates|mmm_project_root|s30mv_read_preprocessed)\\(", rl))
check(min(grep("s32r_parse_mode\\(", rl)) < first_read, "the runner parses its mode before reading anything")
check(!any(grepl("dry.run|DRY", rl)), "the runner has no dry-run mode (registry section 8: one REAL run)")
check(any(grepl("S32_OPTIN_ENV", rl)) && any(grepl("substr(commit, 1, 7)", rl, fixed = TRUE)), "the runner requires the commit7 opt-in")
src <- grep("source\\(", rl, value = TRUE)
check(!any(grepl("Analysis/", src) & !grepl("_pipeline_setup", src)), "the runner sources no Analysis/ file other than _pipeline_setup.R")
check(!any(grepl("\\b(fwrite|writeLines|write\\.csv|saveRDS|file\\.copy|ggsave|Sys\\.chmod)\\(", rl)), "the runner writes no file itself (s32r_write_run / s32r_render_plots do)")
first_fit <- min(grep("s32i_run_all\\(|s32p_module\\(|s32p_h13\\(", rl))
last_pre_gate <- max(grep('^gate\\("[0-4]-|gate_rows\\(', rl))
check(first_fit > last_pre_gate, "every identity / input / measure / label / design gate precedes the first fit")
check(!any(grepl("\\bmessage\\(.*(\\$estimate|MULT|ESTIMATES|EST\\[|FT\\$|H13\\$)", rl)), "the runner prints no estimate")
check(which(grepl("s32r_write_run\\(", rl))[1] > max(grep('gate\\("6-', rl)), "the write follows every post-fit gate")
for (h in HELPERS) { hl <- paste(readLines(h, warn = FALSE), collapse = "\n")
  check(!grepl("S:/", hl, fixed = TRUE), paste("helper holds no S: path:", basename(h)))
  check(!any(grepl("^\\s*source\\(", readLines(h, warn = FALSE))), paste("helper sources nothing:", basename(h))) }
for (tf in list.files(SELF_DIR, "^test_stage32_.*[.]R$", full.names = TRUE)) {
  sl <- grep("^\\s*source\\(", readLines(tf, warn = FALSE), value = TRUE)
  check(length(sl) == 1L && grepl("_pipeline_setup.R", sl), paste("test sources only Analysis/_pipeline_setup.R:", basename(tf)))
  check(!any(grepl("32_behavior_exposure_adaptation.R\"\\)", sl)), paste("test never sources the runner:", basename(tf))) }
ok("static: --real parsed first, opt-in, gates before fits, no estimate printed, helpers path-free, tests never source the runner")

# ---------------------------------------------------------------- 2. identity utilities and registry gates
cm <- "0123456789abcdef0123456789abcdef01234567"
check(identical(s32r_run_name(cm), "v1.1_0123456") && grepl("full git commit", errmsg(s32r_run_name("abc1234"))), "run name v1.1_<commit7> (addendum A1)")
check(identical(s32r_parse_mode("--real"), "REAL") && grepl("only mode is --real", errmsg(s32r_parse_mode(character()))) &&
      grepl("only mode", errmsg(s32r_parse_mode(c("--real", "--dry-run")))), "only --real is accepted")
lf <- file.path(TMP, "lf.txt"); crlf <- file.path(TMP, "crlf.txt")
writeBin(charToRaw("a\nb\n"), lf); writeBin(charToRaw("a\r\nb\r\n"), crlf)
check(identical(s32r_sha_lf(lf), s32r_sha_lf(crlf)) && identical(s32r_sha_lf(lf), s30fb_sha(lf)), "LF-form hash ignores CR bytes")
check(identical(s32r_sha_list_lookup(c("# c", paste(strrep("a", 64), " X.md"), paste(strrep("b", 64), "*Y.csv")), "Y.csv"), strrep("b", 64)) &&
      is.na(s32r_sha_list_lookup(paste(strrep("a", 64), " X.md"), "Z.md")), "sha list lookup")
canon <- file.path(TMP, "canon"); dir.create(canon); repo <- file.path(TMP, "repo"); dir.create(repo)
md <- c("# Stage 32 registry v1.0", "", "**Status.** FROZEN 2026-09-30T22:41:10+0200, before any model was fitted.", "body")
csv <- c("HypothesisID,FamilyID", "H01,F32-A1-EXP")
wlf <- function(lines, path) writeBin(charToRaw(paste0(paste(lines, collapse = "
"), "
")), path)   # LF bytes (the committed form)
for (d0 in c(canon, repo)) { wlf(md, file.path(d0, "STAGE32_REGISTRY_v1.0.md")); wlf(csv, file.path(d0, "STAGE32_HYPOTHESES_v1.0.csv")) }
ex <- modifyList(S32_REGISTRY, list(sha256 = s30fb_sha(file.path(canon, "STAGE32_REGISTRY_v1.0.md")), csv_sha256 = s30fb_sha(file.path(canon, "STAGE32_HYPOTHESES_v1.0.csv"))))
wlf(c("# frozen", paste0(ex$sha256, "  STAGE32_REGISTRY_v1.0.md"), paste0(ex$csv_sha256, "  STAGE32_HYPOTHESES_v1.0.csv")), file.path(canon, "REGISTRY_SHA256.txt"))
Sys.chmod(list.files(canon, full.names = TRUE), "0444")
g <- s32r_registry_gates(canon, file.path(repo, "STAGE32_REGISTRY_v1.0.md"), file.path(repo, "STAGE32_HYPOTHESES_v1.0.csv"), ex)
check(nrow(g) == 7L && all(g$passed), "registry gates pass on a synthetic frozen copy")
wlf(c(md, "changed"), file.path(repo, "STAGE32_REGISTRY_v1.0.md"))
g2 <- s32r_registry_gates(canon, file.path(repo, "STAGE32_REGISTRY_v1.0.md"), file.path(repo, "STAGE32_HYPOTHESES_v1.0.csv"), ex)
check(!g2[gate == "registry repository copy (LF form) SHA-256 = frozen", passed], "a changed repository copy fails its gate")
pk <- s32r_package_gates(c(data.table = as.character(packageVersion("data.table")), digest = "0.0.0"), R.version.string)
check(nrow(pk) == 3L && pk$passed[1] && !pk$passed[2] && pk$passed[3], "package / R version gates")
ok("run name, mode, hashing, registry and package gates")

# ---------------------------------------------------------------- 3. hypothesis map = the frozen table (repository copy)
H <- s32r_hypotheses(file.path(MMM_REPO_ROOT, S32_REGISTRY$repo_csv))
check(identical(H$FamilyID, S32_HYP_MAP$FamilyID) && identical(H$Model, S32_HYP_MAP$Model) && identical(H$HypothesisID, S32_HYP_MAP$HypothesisID),
      "implemented hypothesis map = frozen hypothesis table (IDs, families, models)")
check(all(table(H$FamilyID[H$FamilyID != "none"]) == 2L) && length(unique(H$FamilyID[H$FamilyID != "none"])) == 6L && H[HypothesisID == "H13", Status] == "secondary",
      "6 Holm families of m = 2; H13 single secondary test")
check(identical(s32r_sha_lf(file.path(MMM_REPO_ROOT, S32_REGISTRY$repo_csv)), S32_REGISTRY$csv_sha256) &&
      identical(s32r_sha_lf(file.path(MMM_REPO_ROOT, S32_REGISTRY$repo_path)), S32_REGISTRY$sha256), "repository copies hash to the frozen sha256 (LF form)")
ok("hypothesis map and frozen registry identity")

# ---------------------------------------------------------------- 4. forbidden words and CSV preparation
tb <- list(a = data.table(x = c("fine", "restricted permutation"), y = 1:2), b = data.table(`significant effect` = 1))
bad <- s32r_forbidden(tb, c("post hoc; registered before fitting", "no sleep here"))
check(any(grepl("significant", bad)) && any(grepl("sleep", bad)) && !any(grepl("restricted", bad)), "forbidden-word scan (cells, column names, README; word-bounded 'rest')")
check(length(s32r_forbidden(list(a = data.table(x = "within-Batch permutation")), "Stage 32")) == 0L, "clean text passes")
pr <- s32r_prepare(data.table(t = as.POSIXct("2022-10-28 18:30:00.25", tz = "UTC"), f = factor("A"), s = 'say "x"'))
check(identical(pr$t, "2022-10-28T18:30:00.250Z") && identical(pr$f, "A") && identical(pr$s, "say 'x'"), "POSIXct -> ISO UTC text; factor -> text; double quotes -> single")
v10 <- data.table(stage = "3-coverage", detail = c("0 removed; ", "", NA, " lead", "a, b ", "CC1 L4 kept=0 ends_ok=0; CC4 A3 kept=111 ends_ok=107"))
check(grepl("Round-trip text differs", errmsg(s30fb_write_csv(v10, file.path(TMP, "v10_as_is.csv")))), "the exact v1.0 failure: '0 removed; ' is refused by the frozen writer as is")
s30fb_write_csv(s32r_prepare(v10), file.path(TMP, "v10_prepared.csv"))
back <- fread(file.path(TMP, "v10_prepared.csv"), colClasses = "character", na.strings = "")
check(identical(back$detail, c("0 removed;", "", NA, "lead", "a, b", "CC1 L4 kept=0 ends_ok=0; CC4 A3 kept=111 ends_ok=107")), "after s32r_prepare the same cells round-trip (edges trimmed, inner text kept)")
clean <- data.table(x = c("A1 of CC1", "a;b"), y = c(1.25, NA))
check(identical(s32r_prepare(clean)$x, clean$x) && identical(s32r_prepare(clean)$y, clean$y), "trimming is a no-op on cells without edge whitespace")
ok("forbidden words and CSV preparation (whitespace trimming, the v1.0 failure string)")

# ---------------------------------------------------------------- 5 / 6. plots and writer
set.seed(3)
desc <- rbindlist(lapply(c("crossing_rate", "shared_zone_use", "light_crossing_rate"), function(m) {
  ph <- if (m == "light_crossing_rate") c("L1", "L2", "L3") else c("A1", "A2", "A3", "A4")
  g <- CJ(Group = c("CON", "RES", "SUS"), Sex = c("Female", "Male"), CC = paste0("CC", 1:4), phase = ph)[, `:=`(level = "group", n = 10L, mean = runif(.N, 10, 30))]
  cg <- CJ(CageEpisodeID = paste0("B", 1:6, "|sys.1"), CC = paste0("CC", 1:4), phase = ph)[, `:=`(level = "CON_cage_mean", Group = "CON", n = 4L, mean = runif(.N, 10, 30))]
  rbind(g, cg, fill = TRUE)[, `:=`(metric = m, phase_type = if (m == "light_crossing_rate") "light" else "active")] }), fill = TRUE)
held <- CJ(population = "SIS", model = c("A", "C"), scheme = "LOCO", AnimalNum = paste0("a", 1:30))[, `:=`(behaviour_set = ifelse(model == "A", "none (Batch only)", "movement_mean"),
          CombZ = rnorm(.N), prediction = rnorm(.N))]
nulls <- CJ(null = c("within_batch", "unrestricted"), behaviour_set = c("movement_mean", "primary_behavior_family"), draw = 1:200)[, delta_r2 := rnorm(.N, 0, 0.02)]
tests <- data.table(behaviour_set = c("movement_mean", "primary_behavior_family"), delta_r2_loco = c(0.01, 0.03))
png_dir <- file.path(TMP, "png"); files <- s32r_render_plots(desc, held, data.table(), nulls, tests, png_dir)
check(length(files) == 5L && all(file.exists(files)) && all(file.size(files) > 5000), "five QC PNGs render")
# single-phase panel (the CC4 light panel holds L1 only): points, no line, no single-observation message
desc1 <- desc[!(metric == "light_crossing_rate" & CC == "CC4" & phase != "L1")]
msgs <- character()
invisible(withCallingHandlers(s32r_render_plots(desc1, held, data.table(), nulls, tests, file.path(TMP, "png1")),
                    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") },
                    warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") }))
check(!any(grepl("only one observation", msgs)), "a single-phase panel renders without a single-observation line")
ok("QC plots")

# writer (addendum A1): local staging -> determinism -> local completion and verification -> S: copy
out_root <- file.path(TMP, "pipeline", S32_STAGE_DIR); local_root <- file.path(TMP, "local"); dir.create(local_root)
tabs <- setNames(lapply(S32_TABLES, function(k) data.table(k = k, v = 1.5, t = as.POSIXct("2022-10-28 18:30:00", tz = "UTC"))), S32_TABLES)
aud <- setNames(lapply(S32_AUDIT, function(k) data.table(k = k, passed = TRUE)), S32_AUDIT)
aud$gate_results <- data.table(stage = c("3-coverage", "3-coverage"), gate = c("strict variant", "empty detail"), passed = TRUE, hard = TRUE, detail = c("0 removed; ", ""))
run1 <- s32r_run_name(cm)
dir.create(file.path(out_root, ".tmp_v1.0_4f9c016"), recursive = TRUE)                # an aborted v1.0 staging folder does not block v1.1
check(length(s32r_blocking_entries(out_root)) == 0L, "a .tmp_v1.0_* folder is not a v1.1 blocking entry")
st1 <- s32r_stage_tables(local_root, run1, tabs)
check(identical(list.files(out_root, all.files = TRUE, no.. = TRUE), ".tmp_v1.0_4f9c016"), "table staging writes nothing to the stage folder")
exp_sha <- setNames(vapply(S32_TABLES, function(k) s30fb_sha(file.path(st1, "tables", paste0(k, ".csv"))), ""), S32_TABLES)
det <- s32r_determinism(st1, exp_sha)
check(nrow(det) == 12L && all(det$identical), "determinism: every staged table = its pinned sha256")
det2 <- s32r_determinism(st1, replace(exp_sha, "estimates", strrep("0", 64)))
check(!all(det2$identical) && identical(det2[identical == FALSE, file], "tables/estimates.csv"), "determinism: a differing table is named")
check(grepl("already exists", errmsg(s32r_stage_tables(local_root, run1, tabs))), "an existing local staging folder is refused")
W <- s32r_write_run(out_root, run1, st1, aud, c("Stage 32 synthetic", "line 2"), png_dir)
fl <- list.files(W$dir, recursive = TRUE, full.names = TRUE)
check(all(W$post_gates$passed) && length(fl) == length(S32_TABLES) + length(S32_AUDIT) + 5L + 2L && all(file.access(fl, 2L) != 0L),
      "writer: tables, audit, 5 PNGs, README, output manifest; every file read-only; post-write gates pass")
check(identical(sort(list.files(out_root, all.files = TRUE, no.. = TRUE)), sort(c(".tmp_v1.0_4f9c016", run1))), "the run folder is renamed; no .tmp_v1.1_* left")
man <- fread(file.path(W$dir, S32_MANIFEST_FILE))
check(nrow(man) == length(fl) - 1L && !("audit/output_manifest.csv" %in% man$file) && "README.txt" %in% man$file, "the manifest lists every other file")
lm_ <- s30fb_manifest_check(W$local_dir, man)
check(all(lm_$ok), "the S: copy is byte-identical to the local staging folder")
check(identical(fread(file.path(W$dir, "tables", "estimates.csv"), colClasses = "character")$t, "2022-10-28T18:30:00.000Z"), "timestamps written as ISO UTC text")
check(identical(fread(file.path(W$dir, "audit", "gate_results.csv"), colClasses = "character", na.strings = "")$detail, c("0 removed;", "")),
      "a gate detail with a trailing space ('0 removed; ', the v1.0 failure) is written")
run2 <- s32r_run_name(sub("^0", "1", cm)); st2 <- s32r_stage_tables(local_root, run2, tabs)
check(grepl("Refusing to write", errmsg(s32r_write_run(out_root, run2, st2, aud, "x", png_dir))), "a second run at any commit is refused")
check(identical(s32r_blocking_entries(out_root), run1), "the blocking entry is the earlier v1.1_* folder")
out2 <- file.path(TMP, "pipeline2"); dir.create(file.path(out2, ".tmp_v1.1_abcdef1"), recursive = TRUE)
st3 <- s32r_stage_tables(file.path(TMP, "local3"), run1, tabs)
check(grepl("Refusing to write", errmsg(s32r_write_run(out2, run1, st3, aud, "x", png_dir))), "a leftover .tmp_v1.1_* staging folder blocks the run")
out4 <- file.path(TMP, "pipeline4"); st4 <- s32r_stage_tables(file.path(TMP, "local4"), run1, tabs)
bad_aud <- aud; bad_aud$coverage_counts <- data.table(x = list(1, 2))
check(grepl("List column", errmsg(s32r_write_run(out4, run1, st4, bad_aud, "x", png_dir))) && !dir.exists(out4),
      "a failure while completing the local folder leaves nothing in the stage folder (it is never created)")
check(grepl("declared set", errmsg(s32r_stage_tables(file.path(TMP, "local5"), run1, tabs[-1]))), "the declared table set is enforced")
check(grepl("declared set", errmsg(s32r_write_run(file.path(TMP, "p6"), run1, st3, aud[-1], "x", png_dir))), "the declared audit set is enforced")
ok("the run-once read-only writer (local staging first)")

# aborted-folder evidence (synthetic pinned copy) and addendum gates
ab_root <- file.path(TMP, "ab_stage"); ab <- file.path(ab_root, ".tmp_v1.0_abc1234"); dir.create(file.path(ab, "tables"), recursive = TRUE); dir.create(file.path(ab, "audit"))
for (k in S32_TABLES) writeLines(paste("table", k), file.path(ab, "tables", paste0(k, ".csv")))
for (k in c("input_hashes", "gate_results")) writeLines(paste("audit", k), file.path(ab, "audit", paste0(k, ".csv")))
ab_exp <- list(name = ".tmp_v1.0_abc1234", commit = cm,
               table_sha256 = setNames(vapply(S32_TABLES, function(k) s30fb_sha(file.path(ab, "tables", paste0(k, ".csv"))), ""), S32_TABLES),
               audit_sha256 = setNames(vapply(c("input_hashes", "gate_results"), function(k) s30fb_sha(file.path(ab, "audit", paste0(k, ".csv"))), ""), c("input_hashes", "gate_results")))
a0 <- s32r_aborted_check(ab_root, ab_exp)
check(nrow(a0$gates) == 3L && a0$gates$passed[1] && a0$gates$passed[2] && !a0$gates$passed[3], "aborted folder: pinned files pass; writable files fail the read-only gate")
Sys.chmod(list.files(ab, recursive = TRUE, full.names = TRUE), "0444")
a1 <- s32r_aborted_check(ab_root, ab_exp)
check(all(a1$gates$passed) && nrow(a1$evidence) == 14L && all(a1$evidence$read_only), "aborted folder: 14 pinned read-only files pass")
writeLines("extra", file.path(ab, "audit", "run_manifest.csv"))
a2 <- s32r_aborted_check(ab_root, ab_exp)
check(!a2$gates$passed[2] && any(a2$evidence$status == "UNEXPECTED FILE"), "aborted folder: an extra file fails")
invisible(file.remove(file.path(ab, "audit", "run_manifest.csv"))); dir.create(file.path(ab_root, "v1.0_abc1234"))
a3 <- s32r_aborted_check(ab_root, ab_exp)
check(!a3$gates$passed[1] && a3$gates$passed[2], "aborted folder: any other entry in the stage folder fails")
unlink(file.path(ab_root, "v1.0_abc1234"), recursive = TRUE)
Sys.chmod(file.path(ab, "tables", "models.csv"), "0644"); writeLines("changed", file.path(ab, "tables", "models.csv"))
a4 <- s32r_aborted_check(ab_root, ab_exp)
check(!a4$gates$passed[2] && a4$evidence[file == "tables/models.csv", status] == "CHANGED", "aborted folder: a changed file fails")
add_md <- c("# Stage 32 registry v1.0, addendum A1", "", "**Status.** FROZEN 2026-10-01T00:00:00+0200, before the v1.1 execution.", "body")
wlf(add_md, file.path(canon, "STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md")); wlf(add_md, file.path(repo, "STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md"))
exa <- modifyList(S32_ADDENDUM, list(sha256 = s30fb_sha(file.path(canon, "STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md"))))
wlf(c("# frozen", paste0(exa$sha256, "  STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md")), file.path(canon, "ADDENDUM_A1_SHA256.txt"))
Sys.chmod(file.path(canon, c("STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md", "ADDENDUM_A1_SHA256.txt")), "0444")
ga <- s32r_addendum_gates(canon, file.path(repo, "STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md"), exa)
check(nrow(ga) == 5L && all(ga$passed), "addendum gates pass on a synthetic frozen copy")
wlf(c(add_md, "changed"), file.path(repo, "STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md"))
ga2 <- s32r_addendum_gates(canon, file.path(repo, "STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md"), exa)
check(!ga2$passed[2] && ga2$passed[1], "a changed repository copy of the addendum fails its gate")
ok("aborted-folder evidence and addendum gates")
for (d0 in c(W$dir, ab, canon)) Sys.chmod(list.files(d0, recursive = TRUE, full.names = TRUE), "0644")
cat("test_stage32_run: all checks passed\n")
