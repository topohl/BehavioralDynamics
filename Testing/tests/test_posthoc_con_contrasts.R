# Contract test for the post hoc CON / RES / SUS contrast helpers (Functions/posthoc_con_contrasts.R; registry v1.0).
#
# Synthetic data in memory / tempdir() only: nothing here reads project data, the S: registry, a bundle or any S: path,
# and no Analysis/ runner is sourced or executed (the runner is only read as text / parsed).
# Checks:
#   1. static: the runner has one mode (--real) parsed before any read, an explicit commit opt-in, sources no Analysis/
#      file other than _pipeline_setup.R and writes nothing itself; the helper holds no S: path; this test sources only
#      Analysis/_pipeline_setup.R;
#   2. mode, run name v1.0_<commit7>, LF-form hashing and the sha list lookup; registry gates on a synthetic frozen copy;
#   3. population gates (counts, Sex nested in Batch, 3 CON-only cages per sex, non-finite sets) and their failures;
#   4. L-vectors: batch-balanced means and the three contrasts;
#   5. primary fits = a direct lmerTest::lmer + contest(ddf = "Kenward-Roger") with hand-built L (estimate, SE, df, CI, p),
#      Holm = p.adjust(holm, m = 3), mean differences = contrasts; sensitivity = direct (1 | CageEpisodeID) fits;
#      diagnostics (counts, cages, variance components, KR df); sensitivity agreement columns;
#   6. failure rule: an erroring design -> FAILED rows without estimates; a non-converged fit -> FAILED; the KR ->
#      Satterthwaite fallback -> FAILED row; Holm keeps m = 3 when a member FAILED;
#   7. writer: manifest, read-only files, README, refusal of a second run (any v1.0_* or .tmp_v1.0_* entry), post-write check.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root:
#   Rscript Testing/tests/test_posthoc_con_contrasts.R

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
source_mmm_helper("stage30_figure_bundle.R"); source_mmm_helper("rfid_canonical_inference.R"); source_mmm_helper("posthoc_con_contrasts.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
errmsg <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
TMP <- file.path(normalizePath(tempdir(), winslash = "/"), "phc"); unlink(TMP, recursive = TRUE, force = TRUE); dir.create(TMP)

# ---------------------------------------------------------------- 1. static checks
HELPER <- file.path(MMM_REPO_ROOT, "Functions", "posthoc_con_contrasts.R")
RUNNER <- file.path(MMM_REPO_ROOT, "Analysis", "29b_posthoc_con_contrasts.R")
SELF <- file.path(MMM_REPO_ROOT, "Testing", "tests", "test_posthoc_con_contrasts.R")
code_only <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
invisible(parse(file = RUNNER, keep.source = FALSE))
rl <- code_only(RUNNER); hl <- code_only(HELPER)
first_read <- min(grep("\\b(fread|readRDS|readLines|fromJSON|s30fb_sha|s30fb_verify_[a-z0-9_]+|phc_registry_gates|mmm_project_root)\\(", rl))
check(min(grep("phc_parse_mode\\(", rl)) < first_read, "the runner parses its mode before reading anything")
check(!any(grepl("dry.run|DRY", rl)), "the runner has no dry-run mode (registry section 7)")
check(any(grepl("PHC_OPTIN_ENV", rl)) && any(grepl("substr(commit, 1, 7)", rl, fixed = TRUE)), "the runner requires the commit7 opt-in")
src <- grep("source\\(", rl, value = TRUE)
check(!any(grepl("Analysis/", src) & !grepl("_pipeline_setup", src)), "the runner sources no Analysis/ file other than _pipeline_setup.R")
check(!any(grepl("\\b(fwrite|writeLines|write\\.csv|saveRDS|file\\.copy)\\(", rl)), "the runner writes no file itself (phc_write_run does)")
check(which(grepl("phc_run_all\\(", rl))[1] > max(grep("gate_rows\\(phc_population_gates|phc_blocking_entries\\(", rl)), "every gate precedes the fit")
check(!any(grepl("\\bmessage\\(.*R\\$(estimates|sensitivity)", rl)), "the runner prints no estimate")
check(!grepl("S:/", paste(readLines(HELPER, warn = FALSE), collapse = "\n"), fixed = TRUE), "the helper holds no S: path")
check(!any(grepl("\\bsource\\(", hl)), "the helper sources nothing")
sl <- grep("^\\s*source\\(", readLines(SELF, warn = FALSE), value = TRUE)
check(length(sl) == 1L && grepl("_pipeline_setup.R", sl), "this test sources only Analysis/_pipeline_setup.R")
ok("static: one --real mode parsed first, opt-in, gates before the fit, no Analysis runner sourced, helper has no S: path")

# ---------------------------------------------------------------- 2. mode, run name, hashes, registry gates
check(grepl("only mode is --real", errmsg(phc_parse_mode(character()))) && grepl("only mode", errmsg(phc_parse_mode("--dry-run"))) &&
      grepl("only mode", errmsg(phc_parse_mode(c("--real", "--real")))) && identical(phc_parse_mode("--real"), "REAL"), "only --real is accepted")
cm <- "0123456789abcdef0123456789abcdef01234567"
check(identical(phc_run_name(cm), "v1.0_0123456") && grepl("full git commit", errmsg(phc_run_name("abc1234"))), "run name v1.0_<commit7>")
txt <- c("# Registry", "", "**Status.** FROZEN 2026-09-30T15:40:15+0200, before any model was fitted.", "body")
lf <- file.path(TMP, "lf.md"); crlf <- file.path(TMP, "crlf.md")
con <- file(lf, "wb"); writeLines(txt, con, sep = "\n"); close(con); con <- file(crlf, "wb"); writeLines(txt, con, sep = "\r\n"); close(con)
check(s30fb_sha(lf) != s30fb_sha(crlf) && identical(phc_sha_lf(crlf), s30fb_sha(lf)) && identical(phc_sha_lf(lf), s30fb_sha(lf)), "LF-form hash of a CRLF checkout = the committed LF bytes")
sl2 <- c("# comment", paste0(strrep("a", 64), "  X.md"), paste0(strrep("b", 64), " *dir/Y.md"))
check(identical(phc_sha_list_lookup(sl2, "X.md"), strrep("a", 64)) && identical(phc_sha_list_lookup(sl2, "Y.md"), strrep("b", 64)) &&
      is.na(phc_sha_list_lookup(sl2, "Z.md")), "sha list lookup")
CAN <- file.path(TMP, "canon"); dir.create(CAN)
REGX <- list(file = "REG.md", sha_file = "REGISTRY_SHA256.txt", sha256 = s30fb_sha(lf))
invisible(file.copy(lf, file.path(CAN, "REG.md"))); writeLines(c("# frozen", paste0(REGX$sha256, "  REG.md")), file.path(CAN, "REGISTRY_SHA256.txt"))
check(!all(phc_registry_gates(CAN, crlf, REGX)$passed), "writable S: registry files fail the read-only gate")
Sys.chmod(list.files(CAN, full.names = TRUE), "0444")
check(all(phc_registry_gates(CAN, crlf, REGX)$passed), "registry gates pass for a frozen read-only copy and a CRLF repository copy")
REGY <- REGX; REGY$sha256 <- strrep("0", 64)
check(sum(!phc_registry_gates(CAN, crlf, REGY)$passed) == 3L, "a different frozen sha fails the S:, list and repository gates")
dr <- file.path(TMP, "draft.md"); writeLines(sub("FROZEN", "DRAFT", txt), dr)
check(!all(phc_registry_gates(CAN, dr, REGX)$passed), "an edited repository copy fails")
check(all(phc_package_gates(c(data.table = as.character(utils::packageVersion("data.table"))))$passed) && !all(phc_package_gates(c(data.table = "0.0.1"))$passed),
      "package gate")
ok("mode, run name, LF hashing, sha list, registry and package gates")

# ---------------------------------------------------------------- synthetic A1 (3 batches per sex; 1 CON cage per batch)
set.seed(20260930)
mk_sex <- function(sex, batches, n_sis_cages = 4L) {
  rbindlist(lapply(batches, function(b) {
    con <- data.table(AnimalNum = paste0(b, "c", 1:4), Sex = sex, Batch = b, Group = "CON", CC = "CC1", CageEpisodeID = paste(b, "sys.1", "CC1", sep = "|"))
    sis <- rbindlist(lapply(seq_len(n_sis_cages), function(k) {
      g <- if (k %% 2L == 1L) c("RES", "RES", "SUS", "SUS") else c("RES", "RES", "RES", "SUS")
      data.table(AnimalNum = paste0(b, "s", k, 1:4), Sex = sex, Batch = b, Group = g, CC = "CC1", CageEpisodeID = paste(b, paste0("sys.", k + 1L), "CC1", sep = "|")) }))
    rbind(con, sis) }))
}
A1 <- rbind(mk_sex("Female", c("B3", "B4", "B6")), mk_sex("Male", c("B1", "B2", "B5")))
ce <- unique(A1$CageEpisodeID); cage_eff <- setNames(rnorm(length(ce), sd = 1.5), ce)
bat_eff <- c(B1 = 0, B2 = 1, B3 = -1, B4 = 0.5, B5 = 2, B6 = -0.5)
grp_eff <- c(CON = 0, RES = 1.2, SUS = -0.8)
A1[, crossing_rate := 25 + bat_eff[Batch] + grp_eff[Group] + ifelse(Group == "CON", 0.4, 1) * cage_eff[CageEpisodeID] + rnorm(.N, sd = 2)]
A1[, shared_zone_use := 0.25 + 0.02 * bat_eff[Batch] + 0.03 * grp_eff[Group] + 0.02 * cage_eff[CageEpisodeID] + rnorm(.N, sd = 0.04)]
A1[AnimalNum %in% c("B1s11", "B2s21"), shared_zone_use := NA_real_]
NX <- list(animals = nrow(A1), con_cages_per_sex = 3L, con_per_cage = 4L,
           by_sex_group = lapply(split(A1, A1$Sex), function(x) { t <- table(x$Group); setNames(as.integer(t), names(t))[c("CON", "RES", "SUS")] }))
NFX <- list(crossing_rate = character(), shared_zone_use = c("B1s11", "B2s21"))

# ---------------------------------------------------------------- 3. population gates
DES <- phc_prepare(A1)
check(is.factor(DES$group) && identical(levels(DES$group), c("CON", "RES", "SUS")) && all(DES[Group == "CON", conCage] == 1) && all(DES[Group != "CON", sisCage] == 1),
      "design: group factor with CON reference; conCage / sisCage from the cage epoch")
check(all(phc_population_gates(DES, NX, nonfinite = NFX)$passed), "population gates pass on the synthetic A1")
bad <- copy(A1); bad[AnimalNum == "B3s11", CageEpisodeID := "B3|sys.1|CC1"]
check(!all(phc_population_gates(phc_prepare(bad), NX, nonfinite = NFX)$passed), "a SIS animal in the CON cage fails (cage no longer CON-only)")
bad <- copy(A1); bad[AnimalNum == "B4c1", CageEpisodeID := "B4|sys.9|CC1"]
check(!all(phc_population_gates(phc_prepare(bad), NX, nonfinite = NFX)$passed), "a split CON cage fails (4 CON cages / 3 animals)")
bad <- copy(A1); bad[AnimalNum == "B1s12", shared_zone_use := NA_real_]
check(!all(phc_population_gates(phc_prepare(bad), NX, nonfinite = NFX)$passed), "an unexpected non-finite value fails")
bad <- copy(A1); bad[AnimalNum == "B1c1", Sex := "Female"]
check(!all(phc_population_gates(phc_prepare(bad), NX, nonfinite = NFX)$passed), "Sex not nested in Batch fails")
check(grepl("outside CON / RES / SUS", errmsg(phc_prepare(copy(A1)[1, Group := "SIS"]))), "an unknown group label stops")
ok("population gates and their failures")

# ---------------------------------------------------------------- 4. L-vectors
W <- phc_estimand_weights(c("B4", "B3", "B6", "B3"))
check(identical(names(W), PHC_ESTIMANDS), "six estimands in registry order")
check(isTRUE(all.equal(W$CON_mean, c(`(Intercept)` = 1, BatchB4 = 1 / 3, BatchB6 = 1 / 3), tolerance = 0)) &&
      isTRUE(all.equal(W$SUS_mean, c(`(Intercept)` = 1, BatchB4 = 1 / 3, BatchB6 = 1 / 3, groupSUS = 1), tolerance = 0)),
      "batch-balanced means: intercept + 1/3 x each within-sex Batch dummy + group coefficient")
check(identical(W$RES_minus_CON, c(groupRES = 1)) && identical(W$SUS_minus_CON, c(groupSUS = 1)) && identical(W$SUS_minus_RES, c(groupSUS = 1, groupRES = -1)),
      "contrasts: RES - CON = groupRES; SUS - CON = groupSUS; SUS - RES = groupSUS - groupRES")
check(grepl("two batches", errmsg(phc_estimand_weights("B1"))), "one batch refuses")
ok("L-vectors")

# ---------------------------------------------------------------- 5. fits against direct lmerTest KR
RR <- phc_run_all(DES, data.table(measure = PHC_MEASURES, measure_label = c("RFID position-change rate", "shared RFID-position occupancy"),
                                  unit = c("position changes/hour", "fraction")))
E <- RR$estimates; S <- RR$sensitivity; D <- RR$diagnostics
check(nrow(E) == 24L && nrow(S) == 24L && nrow(D) == 8L && all(E$status == "OK") && all(D$status == "OK"), "4 families x 6 estimands; 8 fits, all OK")
direct <- function(measure, sex, f) {
  x <- DES[Sex == sex & is.finite(get(measure))]; x <- copy(x); x[, y := get(measure)]; x[, Batch := droplevels(factor(Batch))]
  fit <- lmerTest::lmer(stats::as.formula(f), data = x, REML = TRUE, control = lme4::lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
  nm <- names(lme4::fixef(fit)); Lm <- function(w) { L <- setNames(rep(0, length(nm)), nm); L[names(w)] <- w; L }
  bl <- levels(x$Batch); base <- c(`(Intercept)` = 1, setNames(rep(1 / 3, 2), paste0("Batch", bl[2:3])))
  Ls <- list(CON_mean = Lm(base), RES_mean = Lm(c(base, groupRES = 1)), SUS_mean = Lm(c(base, groupSUS = 1)),
             RES_minus_CON = Lm(c(groupRES = 1)), SUS_minus_CON = Lm(c(groupSUS = 1)), SUS_minus_RES = Lm(c(groupSUS = 1, groupRES = -1)))
  out <- rbindlist(lapply(names(Ls), function(e) { ct <- lmerTest::contest(fit, Ls[[e]], joint = FALSE, ddf = "Kenward-Roger", confint = TRUE)
    data.table(estimand = e, estimate = ct$Estimate, se = ct$`Std. Error`, df = ct$df, ci_low = ct$lower, ci_high = ct$upper, p = ct$`Pr(>|t|)`) }))
  out[, p_holm := NA_real_][4:6, p_holm := p.adjust(p, "holm")]
  list(out = out, fit = fit, x = x)
}
near <- function(a, b, tol = 1e-8) isTRUE(all(abs(a - b) <= tol * pmax(1, abs(b)), na.rm = FALSE))
for (k in PHC_MEASURES) for (sx in PHC_SEXES) {
  dp <- direct(k, sx, PHC_MODELS$primary$formula); e <- E[measure == k & Sex == sx][match(dp$out$estimand, estimand)]
  check(near(e$estimate, dp$out$estimate) && near(e$se, dp$out$se) && near(e$df, dp$out$df) && near(e$ci_low, dp$out$ci_low) && near(e$ci_high, dp$out$ci_high),
        paste("primary", k, sx, ": estimate / SE / KR df / CI = direct lmerTest KR"))
  check(near(e$p_raw[4:6], dp$out$p[4:6]) && near(e$p_holm[4:6], dp$out$p_holm[4:6]) && all(is.na(e$p_raw[1:3])) && all(is.na(e$p_holm[1:3])),
        paste("primary", k, sx, ": contrast p and Holm (m = 3) = direct; means carry no p"))
  check(abs((e$estimate[2] - e$estimate[1]) - e$estimate[4]) < 1e-10 && abs((e$estimate[3] - e$estimate[1]) - e$estimate[5]) < 1e-10 &&
        abs((e$estimate[3] - e$estimate[2]) - e$estimate[6]) < 1e-10, paste("primary", k, sx, ": mean differences = contrasts"))
  ds <- direct(k, sx, PHC_MODELS$sensitivity$formula); s <- S[measure == k & Sex == sx][match(ds$out$estimand, estimand)]
  check(near(s$estimate, ds$out$estimate) && near(s$df, ds$out$df) && near(s$p_holm[4:6], ds$out$p_holm[4:6]) && all(s$model == PHC_MODELS$sensitivity$model),
        paste("sensitivity", k, sx, "= direct (1 | CageEpisodeID) KR"))
  check(near(s$primary_estimate, e$estimate) && near(s$estimate_diff, s$estimate - e$estimate, 1e-12) &&
        identical(s$same_sign, sign(s$estimate) == sign(e$estimate)), paste("sensitivity", k, sx, ": agreement columns"))
  dg <- D[measure == k & Sex == sx & model == PHC_MODELS$primary$model]
  vc <- as.data.frame(lme4::VarCorr(dp$fit))
  check(dg$n_obs == nrow(dp$x) && dg$n_CON == 12L && dg$n_con_cages == 3L && dg$n_sis_cages == 12L && dg$n_batches == 3L && dg$rank == 5L &&
        near(dg$vc_con_cage, vc$vcov[vc$var1 %in% "conCage"]) && near(dg$vc_sis_cage, vc$vcov[vc$var1 %in% "sisCage"]) &&
        near(dg$vc_residual, vc$vcov[vc$grp == "Residual"]) && is.na(dg$vc_cage) && near(dg$kr_df_SUS_minus_RES, dp$out$df[6]) &&
        identical(dg$singular, lme4::isSingular(dp$fit)), paste("diagnostics", k, sx))
}
check(identical(D[measure == "shared_zone_use" & Sex == "Male", unique(excluded_nonfinite)], "B1s11;B2s21") &&
      all(E[measure == "shared_zone_use" & Sex == "Male", n_obs] == 58L), "non-finite rows excluded and recorded")
check(identical(E[estimand == "SUS_minus_RES", unique(estimand_label)], paste("SUS", intToUtf8(0x2212L), "RES")) && all(E$tier == PHC_TIER) &&
      all(E$registry_sha256 == PHC_REGISTRY$sha256) && all(E[tested == TRUE, holm_m] == 3L) && identical(unique(E$holm_family[E$measure == "crossing_rate" & E$Sex == "Male"]), "crossing_rate|Male"),
      "labels, tier, registry identity and Holm family")
check(!any(grepl("confirmatory|preregistered|significan|acute|antenna|crossing", paste(c(E$estimand_label, E$measure_label, E$display_note, E$caveats, E$registered_rs_note)), ignore.case = TRUE)),
      "display text carries no banned wording")
ok("primary and sensitivity fits = direct lmerTest KR; Holm m = 3; mean differences = contrasts; diagnostics; agreement")

# ---------------------------------------------------------------- 6. failure rule
x1 <- phc_subset(DES, "crossing_rate", "Female")
xb <- copy(x1)[Batch == "B3"]; xb[, Batch := droplevels(Batch)]
f_err <- phc_fit(xb, PHC_MODELS$primary$formula, "err")
check(is.null(f_err$fit) && isTRUE(f_err$info$failed) && grepl("fit stopped", f_err$failure_reason), "a design the engine refuses -> FAILED stub (no silent fallback)")
xr <- copy(x1); xr[Batch == "B4", `:=`(Batch = "B3")]; xr[, Batch := droplevels(factor(Batch))]
fr <- phc_fit_family(xr, "crossing_rate", "Female", "primary")
check(all(fr$rows$status == "FAILED") && all(is.na(fr$rows$estimate)) && all(is.na(fr$rows$p_holm)) && fr$diag$status == "FAILED" && all(grepl("fit stopped", fr$rows$failure_reason)),
      "a FAILED fit yields FAILED rows without estimate, p or Holm")
# simulate a non-converged fit: the engine's result with converged = FALSE (the real fit object is kept)
with_nc <- function() { f0 <- mmm_ci_fit; assign("mmm_ci_fit", function(...) { m <- f0(...); m$info[, converged := FALSE]; m }, envir = globalenv())
  on.exit(assign("mmm_ci_fit", f0, envir = globalenv())); phc_fit_family(x1, "crossing_rate", "Female", "primary") }
fn <- with_nc()
check(all(fn$rows$status == "FAILED") && all(is.na(fn$rows$estimate)) && all(fn$rows$failure_reason == PHC_FAILURE_NONCONVERGED) && fn$diag$status == "FAILED",
      "a non-converged fit is FAILED (registry section 5), even with a fit object")
check(identical(mmm_ci_fit, get("mmm_ci_fit", envir = globalenv())) && all(phc_fit_family(x1, "crossing_rate", "Female", "primary")$rows$status == "OK"), "engine restored")
kg <- phc_kr_guard({ warning("Unable to compute Kenward-Roger t-test: using Satterthwaite instead"); data.table(estimate = 1, se = 1, df = 3, statistic = 1, ci_low = 0, ci_high = 2, p_raw = 0.4, status = "OK") })
check(kg$status == "FAILED" && is.na(kg$estimate) && is.na(kg$p_raw) && grepl("Satterthwaite", kg$failure_reason) && grepl("Unable to compute", kg$kr_messages),
      "KR -> Satterthwaite fallback FAILS the row")
kw <- phc_kr_guard({ warning("some other note"); data.table(estimate = 1, p_raw = 0.4, status = "OK") })
check(kw$status == "OK" && kw$estimate == 1 && grepl("some other note", kw$kr_messages), "another warning is recorded, not fatal")
check(identical(mmm_ci_holm(c(0.01, NA, 0.02)), p.adjust(c(0.01, NA, 0.02), "holm", n = 3)) && isTRUE(all.equal(mmm_ci_holm(c(0.01, NA, 0.02))[c(1, 3)], c(0.03, 0.04))),
      "Holm keeps m = 3 when a member FAILED")
ok("failure rule: FAILED stub, non-converged FAILED, KR fallback FAILED, Holm m kept")

# ---------------------------------------------------------------- 7. writer
OUT <- file.path(TMP, "pipeline", PHC_STAGE_DIR); RUN <- phc_run_name(cm)
AUD <- list(gates = data.table(stage = "s", gate = "g", passed = TRUE, hard = TRUE, detail = ""), inputs = data.table(input = "x", bytes = 1, sha256 = "s", role = "r"),
            run = data.table(mode = "REAL", git_commit = cm))
TB <- list(estimates = E, sensitivity = S, diagnostics = D)
w <- phc_write_run(OUT, RUN, TB, AUD, c("Stage 29b synthetic", paste("commit", cm), paste("registry sha256", PHC_REGISTRY$sha256)))
man <- fread(file.path(w$dir, PHC_MANIFEST_FILE))
check(identical(basename(w$dir), "v1.0_0123456") && setequal(man$file, c(PHC_TABLE_FILES, PHC_AUDIT_FILES, PHC_README)) && all(s30fb_manifest_check(w$dir, man)$ok) &&
      setequal(s30fb_files_on_disk(w$dir), c(man$file, PHC_MANIFEST_FILE)), "output_manifest lists every other file; bytes / sha256 match; nothing else")
check(all(file.access(list.files(w$dir, recursive = TRUE, full.names = TRUE), 2L) != 0L) && all(w$post_gates$passed), "files read-only; post-write gates pass")
eb <- fread(file.path(w$dir, PHC_TABLE_FILES[["estimates"]]), encoding = "UTF-8")
check(identical(eb$estimate, E$estimate) && identical(eb$p_holm, E$p_holm) && identical(eb$estimand_label, E$estimand_label), "estimates written at full precision (UTF-8 labels)")
check(any(readLines(file.path(w$dir, PHC_README)) == paste("commit", cm)), "README carries the commit line")
check(grepl("earlier REAL run", errmsg(phc_write_run(OUT, "v1.0_fedcba9", TB, AUD, "x"))), "a second run (any commit) is refused")
OUT2 <- file.path(TMP, "o2"); dir.create(file.path(OUT2, ".tmp_v1.0_1111111"), recursive = TRUE)
check(grepl("earlier REAL run or staging", errmsg(phc_write_run(OUT2, RUN, TB, AUD, "x"))), "an aborted staging folder blocks a run")
check(grepl("declared set", errmsg(phc_write_run(file.path(TMP, "o3"), RUN, TB[-1], AUD, "x"))), "a missing table is refused")
OUT4 <- file.path(TMP, "o4"); tamper <- function(bd) { f <- file.path(bd, PHC_TABLE_FILES[["diagnostics"]]); Sys.chmod(f, "0666"); cat("x\n", file = f, append = TRUE) }
check(grepl("Post-write verification failed", errmsg(phc_write_run(OUT4, RUN, TB, AUD, "x", .before_verify = tamper))), "a post-write mismatch stops")
check(identical(phc_no_dquote(data.table(a = "x\"y", b = 1))$a, "x'y"), "embedded double quotes in text are replaced before writing")
check(length(phc_blocking_entries(OUT)) == 1L && !length(phc_blocking_entries(file.path(TMP, "none"))), "blocking entries")
ok("writer: manifest, read-only, README, refusal of a second run / staging folder, post-write check")

Sys.chmod(list.files(TMP, recursive = TRUE, full.names = TRUE, all.files = TRUE), "0666"); unlink(TMP, recursive = TRUE, force = TRUE)
cat("test_posthoc_con_contrasts.R: all checks passed\n")
