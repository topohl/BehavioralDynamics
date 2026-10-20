# Contract test for the post hoc movement-CombZ display-line helpers (Functions/posthoc_movement_combz_fit.R; registry v1.1).
#
# Synthetic data in memory / tempdir() only: nothing here reads project data, the S: registry, a bundle or any S: path,
# and no Analysis/ runner is sourced or executed (the runner is only read as text / parsed).
# Checks:
#   1. static: the runner has one mode (--real) parsed before any read, an explicit commit opt-in, sources no Analysis/
#      file other than _pipeline_setup.R and writes nothing itself; the helper holds no S: path; this test sources only
#      Analysis/_pipeline_setup.R;
#   2. mode and run name v1.1_<commit7>; registry gates on a synthetic frozen copy;
#   3. population gates (counts per sex, one row per animal, finite values) and their failures;
#   4. fits = a direct stats::lm + predict(interval = "confidence", level = 0.95) on the 101-point grid over the observed
#      range; coefficients = coef / SE / sigma / R squared; no t or P column; output gates;
#   5. failure rule: fewer than 10 animals or a rank-deficient fit -> FAILED, no line; the output gate then fails;
#   6. writer: manifest, read-only files, README, refusal of a second run (any v1.1_* or .tmp_v1.1_* entry), post-write check.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root:
#   Rscript Testing/tests/test_posthoc_movement_combz_fit.R

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
source_mmm_helper("stage30_figure_bundle.R"); source_mmm_helper("posthoc_con_contrasts.R"); source_mmm_helper("posthoc_movement_combz_fit.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
errmsg <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
TMP <- file.path(normalizePath(tempdir(), winslash = "/"), "pmf")
wipe <- function(p) { if (dir.exists(p)) Sys.chmod(list.files(p, recursive = TRUE, full.names = TRUE, all.files = TRUE), mode = "0666"); unlink(p, recursive = TRUE, force = TRUE) }
wipe(TMP); dir.create(TMP)

# ---------------------------------------------------------------- 1. static checks
HELPER <- file.path(MMM_REPO_ROOT, "Functions", "posthoc_movement_combz_fit.R")
RUNNER <- file.path(MMM_REPO_ROOT, "Analysis", "29c_posthoc_movement_combz_fit.R")
SELF <- file.path(MMM_REPO_ROOT, "Testing", "tests", "test_posthoc_movement_combz_fit.R")
code_only <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
invisible(parse(file = RUNNER, keep.source = FALSE)); invisible(parse(file = HELPER, keep.source = FALSE))
rl <- code_only(RUNNER); hl <- code_only(HELPER)
first_read <- min(grep("\\b(fread|readRDS|readLines|s30fb_sha|s30fb_verify_[a-z0-9_]+|phc_registry_gates|mmm_project_root)\\(", rl))
check(min(grep("pmf_parse_mode\\(", rl)) < first_read, "the runner parses its mode before reading anything")
check(any(grepl("PMF_OPTIN_ENV", rl)) && any(grepl("identical\\(OPTIN, substr\\(commit, 1, 7\\)\\)", rl)), "the runner requires the commit7 opt-in")
src <- unlist(regmatches(rl, gregexpr("source\\([^)]*\\)", rl)))
check(length(src) == 1L && grepl(".pipeline_setup", src, fixed = TRUE), "the runner sources only _pipeline_setup.R directly (helpers via source_mmm_helper)")
check(!any(grepl("\\b(fwrite|write\\.csv|writeLines|saveRDS|file\\.copy|dir\\.create)\\(", rl)), "the runner writes nothing itself (only through pmf_write_run)")
check(!any(grepl("(^|[^A-Za-z])[Ss]:[/\\\\]|mdc-berlin", hl)), "the helper holds no S: or UNC path")
sl <- code_only(SELF)
check(!any(grepl("source\\([^)]*Analysis/(?!_pipeline_setup)", sl, perl = TRUE)), "this test sources no Analysis/ runner")
check(!any(grepl("\\b(lm|predict)\\(", gsub("\"[^\"]*\"", "\"\"", rl))), "the runner fits nothing itself (all model code is in the helper)")
ok("static: mode first, opt-in, sources, no writes, no S: path, model code in the helper")

# ---------------------------------------------------------------- 2. mode, run name, registry gates
check(identical(pmf_parse_mode("--real"), "REAL"), "--real accepted")
for (a in list(character(), "--dry-run", c("--real", "x"), "real")) check(!is.na(errmsg(pmf_parse_mode(a))), paste("mode refused:", paste(a, collapse = " ")))
check(identical(pmf_run_name(strrep("a", 40)), "v1.1_aaaaaaa"), "run name v1.1_<commit7>")
check(!is.na(errmsg(pmf_run_name("abc1234"))), "run name needs a full hash")
rdir <- file.path(TMP, "reg"); dir.create(rdir)
body <- c("# synthetic registry", "", "**Status.** FROZEN 2026-01-01T00:00:00+0100, test copy.", "")
con <- file(file.path(rdir, "R.md"), "wb"); writeLines(body, con, sep = "\n"); close(con)
repo_copy <- file.path(TMP, "R_repo.md"); con <- file(repo_copy, "wb"); writeLines(body, con, sep = "\r\n"); close(con)
sha <- s30fb_sha(file.path(rdir, "R.md"))
writeLines(c("# list", paste0(sha, "  R.md")), file.path(rdir, "SHA.txt"))
Sys.chmod(list.files(rdir, full.names = TRUE), mode = "0444")
exp <- list(file = "R.md", sha_file = "SHA.txt", sha256 = sha)
rg <- phc_registry_gates(rdir, repo_copy, expect = exp)
check(nrow(rg) == 5L && all(rg$passed), "registry gates pass on a frozen read-only copy (CRLF repository copy hashed in LF form)")
rg2 <- phc_registry_gates(rdir, repo_copy, expect = modifyList(exp, list(sha256 = strrep("0", 64))))
check(!any(rg2$passed[1:3]), "registry gates fail on a different frozen sha256")
# v1.0: sha256 a617bb5be709b558ed628a61fab0095b598345912c86e9c25070d07b02cb8098, freeze commit 2be501d8b849346bdde47e246fb738a9df05ad98
check(identical(PMF_REGISTRY$version, "1.1") && grepl("^[0-9a-f]{64}$", PMF_REGISTRY$sha256),
      "registry v1.1 identity: the frozen sha256")
ok("mode, run name, registry gates")

# ---------------------------------------------------------------- 3. population gates
set.seed(29)
syn <- function(nf = 58L, nm = 53L) {
  n <- nf + nm; sex <- c(rep("Female", nf), rep("Male", nm))
  x <- c(runif(nf, 1, 7.2), runif(nm, 1.5, 6.1)); y <- 0.9 - 0.25 * x + rnorm(n, 0, 0.6)
  data.table(AnimalNum = sprintf("A%03d", seq_len(n)), Group = sample(c("CON", "RES", "SUS"), n, TRUE), Sex = sex,
             observed_CombZ = y, heldout_prediction = y + rnorm(n, 0, 0.1), Movement_mean = x, crossing_rate_equiv_per_h = x * 3)
}
A <- syn()
pg <- pmf_population_gates(A); check(nrow(pg) == 4L && all(pg$passed), "population gates pass on 111 animals (58 / 53), finite")
check(!all(pmf_population_gates(syn(57L, 54L))$passed), "wrong sex counts fail")
B <- copy(A); B[2, AnimalNum := A$AnimalNum[1]]; check(!all(pmf_population_gates(B)$passed), "a duplicated animal fails")
B <- copy(A); B[5, Movement_mean := NA_real_]; check(!all(pmf_population_gates(B)$passed), "a non-finite predictor fails")
check(!pmf_population_gates(A[, !"Movement_mean"])$passed[1], "a missing column fails")
ok("population gates")

# ---------------------------------------------------------------- 4. fits = direct lm + predict(interval = 'confidence')
R <- pmf_run_all(A)
for (s in PMF_SEXES) {
  d <- A[Sex == s]; f <- lm(observed_CombZ ~ Movement_mean, data = d)
  g <- data.frame(Movement_mean = seq(min(d$Movement_mean), max(d$Movement_mean), length.out = 101L))
  p <- predict(f, newdata = g, interval = "confidence", level = 0.95)
  fl <- R$fit_lines[Sex == s][order(grid_index)]
  check(nrow(fl) == 101L && max(abs(fl$x - g$Movement_mean)) < 1e-12, paste(s, "grid: 101 points from min to max"))
  check(max(abs(fl$fit - p[, "fit"])) < 1e-10 && max(abs(fl$ci_low - p[, "lwr"])) < 1e-10 && max(abs(fl$ci_high - p[, "upr"])) < 1e-10,
        paste(s, "line and band = predict(interval = 'confidence', level = 0.95)"))
  cf <- R$coefficients[Sex == s]; sm <- summary(f)
  check(max(abs(cf$estimate - unname(coef(f)))) < 1e-12 && max(abs(cf$se - unname(sm$coefficients[, 2]))) < 1e-12 &&
          all(abs(cf$residual_sd - sm$sigma) < 1e-12) && all(abs(cf$r_squared - sm$r.squared) < 1e-12) && all(cf$df_residual == nrow(d) - 2L),
        paste(s, "coefficients, SE, residual SD, R squared, df"))
  check(all(fl$t_crit == qt(0.975, nrow(d) - 2L)) && all(fl$level == 0.95) && all(fl$n == nrow(d)), paste(s, "t quantile, level, n recorded"))
}
check(all(R$diagnostics$status == "OK") && all(R$diagnostics$n_excluded_nonfinite == 0L), "both fits OK, none excluded")
check(all(R$fit_lines$registry_sha256 == PMF_REGISTRY$sha256) && all(R$coefficients$tier == PMF_TIER), "registry identity on every row")
og <- pmf_output_gates(R, A); check(nrow(og) == 6L && all(og$passed), "output gates pass")
check(!any(grepl("(^p_|_p$|^p$|statistic|t_value|p_value)", c(names(R$fit_lines), names(R$coefficients), names(R$diagnostics)))), "no t or P column")
ok("fits match lm / predict; coefficients; output gates")

# ---------------------------------------------------------------- 5. failure rule
small <- pmf_fit_sex(syn(9L, 53L), "Female")
check(is.null(small$lines) && small$diagnostics$status == "FAILED" && grepl("fewer than 10", small$diagnostics$failure_reason) &&
        all(is.na(small$coefficients$estimate)), "fewer than 10 animals -> FAILED, no line")
flat <- copy(A); flat[Sex == "Male", Movement_mean := 3]
rk <- pmf_fit_sex(flat, "Male")
check(is.null(rk$lines) && rk$diagnostics$status == "FAILED" && grepl("rank-deficient", rk$diagnostics$failure_reason), "rank-deficient fit -> FAILED, no line")
RF <- pmf_run_all(flat); ogf <- pmf_output_gates(RF, flat)
check(!ogf$passed[1] && nrow(RF$fit_lines) == 101L, "a FAILED fit fails the output gate; only the OK sex has a line")
ok("failure rule")

# ---------------------------------------------------------------- 6. writer
out <- file.path(TMP, "stage"); run <- pmf_run_name(strrep("b", 40))
aud <- list(gates = data.table(stage = "x", gate = "y", passed = TRUE, hard = TRUE, detail = ""),
            inputs = data.table(input = "a", bytes = 1, sha256 = strrep("c", 64), role = "r"), run = data.table(mode = "REAL"))
W <- pmf_write_run(out, run, list(fit_lines = R$fit_lines, coefficients = R$coefficients, diagnostics = R$diagnostics), aud, c("readme", "line 2"))
check(all(W$post_gates$passed) && dir.exists(file.path(out, run)), "write: post-write gates pass")
files <- list.files(W$dir, recursive = TRUE, full.names = TRUE)
check(length(files) == 8L && all(file.access(files, 2L) != 0L), "write: 8 files (3 tables, 3 audit, manifest, README), all read-only")
back <- fread(file.path(W$dir, PMF_TABLE_FILES[["fit_lines"]]))
check(nrow(back) == 202L && max(abs(back$fit - R$fit_lines$fit)) == 0, "write: fit lines re-read exactly")
check(grepl("Refusing to write", errmsg(pmf_write_run(out, pmf_run_name(strrep("d", 40)), list(fit_lines = R$fit_lines, coefficients = R$coefficients,
      diagnostics = R$diagnostics), aud, "x"))), "a second run is refused (fitted once)")
out2 <- file.path(TMP, "stage2")
check(grepl("Post-write verification failed", errmsg(pmf_write_run(out2, run, list(fit_lines = R$fit_lines, coefficients = R$coefficients,
      diagnostics = R$diagnostics), aud, "x", .before_verify = function(bd) { f <- file.path(bd, PMF_README); Sys.chmod(f, "0666"); cat("tamper", file = f, append = TRUE) }))),
      "a file changed after the manifest fails the post-write check")
check(grepl("declared set", errmsg(pmf_write_run(file.path(TMP, "stage3"), run, list(fit_lines = R$fit_lines), aud, "x"))), "an undeclared table set is refused")
ok("writer: once, read-only, verified")

wipe(TMP)
cat("All post hoc movement-CombZ display-line checks passed.\n")
