# Contract test for the Stage 30 figure-bundle helpers (Functions/stage30_figure_bundle.R).
#
# Synthetic data in memory / tempdir() only: nothing here reads project data, a label list, an outcome table or any S:
# path, and no Analysis/ runner is sourced or executed (the runner file is only read as text for static checks). Checks:
#   1. static: no model / test / adjustment call in the helper or the runner; the runner parses its mode before any read
#      and sources no other Analysis/ file; this test sources only Analysis/_pipeline_setup.R;
#   2. run mode (--dry-run / --real, refusal without a mode), clean-code check, bundle id format;
#   3. hash gates: manifest check with CR stripping, the frozen Stage 30 run gates (manifest hash, file tamper, extra file,
#      README commit), the Stage 29 bundle gates (manifest hash, FROZEN registry row), input_hashes and sha-list lookups;
#   4. Group rule (CON > SUS > RES) and the label / CombZ attachment guards;
#   5. standardized CI rescaling (CONT x SD_x, CAT / SD_y, L none) with the 1e-12 assert;
#   6. COOKIE-CONT centroid line identity (hand-computed within-batch OLS slope; n-weighted batch intercepts), the n assert,
#      the 'sex-resolved centroid' derivation wording and the banned-display-word guard on it;
#   7. descriptive summaries (quantile type 7, counts, registry QC row copied);
#   8. estimate rows (keys, sensitivity copy, Movement-adjusted cross-check), rate-inactivity relationship rounding gate,
#      display labels (typesetting, banned wording), screen-matrix layout keys;
#   9. full-precision CSV writer (exact round trip);
#  10. bundle writer: manifest completeness (incl. H3_gate_results = the pre-write gates), provenance keys
#      (pre_write_gates_passed counts the gate table), read-only files, post-write gates BEFORE the registry row (a
#      post-write failure leaves no registry row), the complete gate log beside the registry, refusal to overwrite.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root:
#   Rscript Testing/tests/test_stage30_figure_bundle.R

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
source_mmm_helper("stage30_figure_bundle.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
errmsg <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
near <- function(a, b, tol = 1e-12) isTRUE(all(abs(a - b) <= tol))
TMP <- file.path(tempdir(), "s30fb"); unlink(TMP, recursive = TRUE, force = TRUE); dir.create(TMP)
wcsv <- function(x, p) { dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE); fwrite(x, p); invisible(p) }

# ---------------------------------------------------------------- 1. static checks
HELPER <- file.path(MMM_REPO_ROOT, "Functions", "stage30_figure_bundle.R")
RUNNER <- file.path(MMM_REPO_ROOT, "Analysis", "30b_stage30_figure_bundle.R")
SELF <- file.path(MMM_REPO_ROOT, "Testing", "tests", "test_stage30_figure_bundle.R")
INFER <- paste0("\\b(lm|glm|lmer|glmer|lme|gam|bam|gamm|nls|loess|lowess|smooth\\.spline|splinefun|cor|cor\\.test|t\\.test|wilcox\\.test|",
                "kruskal\\.test|aov|anova|predict|coef|coefficients|fitted|resid|residuals|confint|vcov|p\\.adjust|geom_smooth|stat_smooth|",
                "mmm_ci_[A-Za-z_]+|s30sc_fit|s30sc_run_screen|s30sc_run_entry|s30sc_standardizer|s30sc_sensitivities|s30sc_lobo|s30sc_influence|",
                "s30sc_design_gate|s30sc_nested_lobo_prediction)\\s*\\(")
for (f in c(HELPER, RUNNER)) {
  ln <- readLines(f, warn = FALSE); hit <- grep(INFER, ln, perl = TRUE)
  check(!length(hit), paste0(basename(f), " contains a model / test / adjustment call: ", paste(ln[hit], collapse = " | ")))
}
rl <- readLines(RUNNER, warn = FALSE)
src <- grep("source\\(", rl, value = TRUE)
check(!any(grepl("Analysis/", src) & !grepl("_pipeline_setup", src)), "the runner sources no Analysis/ file other than _pipeline_setup.R")
first_read <- min(grep("\\b(fread|readRDS|readLines|fromJSON|s30fb_sha|s30fb_verify_[a-z0-9_]+)\\(", rl))
check(min(grep("s30fb_parse_mode\\(", rl)) < first_read, "the runner parses its mode before reading anything")
sl <- grep("^\\s*source\\(", readLines(SELF, warn = FALSE), value = TRUE)
check(length(sl) == 1L && grepl("_pipeline_setup.R", sl), "this test sources only Analysis/_pipeline_setup.R")
check(!grepl("S:/", paste(readLines(HELPER, warn = FALSE), collapse = "\n"), fixed = TRUE), "the helper holds no S: path")
check(!any(grepl("\\bfwrite\\(", rl)) && !any(grepl("gates_passed\\s*=", rl) & !grepl("pre_write_gates_passed\\s*=", rl)),
      "the runner writes no file itself (the helper writes bundle, registry and gate log) and has no bare gates_passed key")
ok("static: no model call in helper / runner; mode parsed before any read; no Analysis runner sourced")

# ---------------------------------------------------------------- 2. run mode, clean code, bundle id
check(grepl("Refusing to run", errmsg(s30fb_parse_mode(character()))), "no argument refuses")
check(grepl("Both", errmsg(s30fb_parse_mode(c("--dry-run", "--real")))), "both modes refuse")
check(grepl("Unknown", errmsg(s30fb_parse_mode("--force"))), "unknown argument refuses")
check(identical(s30fb_parse_mode("--dry-run"), "DRY") && identical(s30fb_parse_mode("--real"), "REAL"), "modes parse")
files <- c("Analysis/a.R", "Functions/b.R")
check(isTRUE(s30fb_code_clean(character(), files, files)$ok), "clean and tracked -> ok")
check(!s30fb_code_clean(" M Functions/b.R", files, files)$ok, "a modified file -> not clean")
check(!s30fb_code_clean(character(), files[1], files)$ok, "an untracked file -> not clean")
cm <- "0123456789abcdef0123456789abcdef01234567"
bid <- s30fb_bundle_id(as.Date("2026-09-29"), cm)
check(identical(bid, "s30b_v10_20260929_0123456") && grepl("^s30b_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$", bid), "bundle id s30b_v10_<YYYYMMDD>_<commit7>")
check(grepl("full git commit", errmsg(s30fb_bundle_id(Sys.Date(), "abc1234"))), "a short commit is refused")
ok("run mode, clean-code check, bundle id")

# ---------------------------------------------------------------- 3. hash gates
d <- file.path(TMP, "m"); dir.create(d)
writeLines("alpha", file.path(d, "a.txt")); writeLines("beta", file.path(d, "b.txt"))
man <- data.table(file = c("a.txt", "b.txt"), bytes = paste0(file.size(file.path(d, c("a.txt", "b.txt"))), "\r"),
                  sha256 = paste0(vapply(file.path(d, c("a.txt", "b.txt")), s30fb_sha, "", USE.NAMES = FALSE), "\r"))
check(all(s30fb_manifest_check(d, man)$ok), "manifest check passes with CR-terminated fields (CR stripped)")
writeLines("ALPHA", file.path(d, "a.txt"))
mc <- s30fb_manifest_check(d, man); check(!mc$ok[1] && mc$ok[2], "a modified file fails its manifest row")
unlink(file.path(d, "b.txt")); check(!s30fb_manifest_check(d, man)$ok[2], "a missing file fails its manifest row")

# synthetic frozen Stage 30 run
RUN <- file.path(TMP, "r", "v1.0_be71e2f")
EXP <- list(run_name = "v1.0_be71e2f", run_commit = strrep("a", 40), run_mode = "REAL_EXPLORATORY_POST_HOC_CONTEXT", registry_sha256 = strrep("b", 64),
            output_manifest_n = 4L, status_counts = "OK=48")
dir.create(file.path(RUN, "audit"), recursive = TRUE); dir.create(file.path(RUN, "tables"))
writeLines(c("Stage 30 exploratory outcome screen", "", paste("commit", EXP$run_commit), paste("registry sha256", EXP$registry_sha256),
             "Stage 29 folder S:/x/29_canonical_behavior_releases/v101_dv2_b2ce507"), file.path(RUN, "README.txt"))
wcsv(data.table(run_mode = EXP$run_mode, mode = "REAL", git_commit = EXP$run_commit, registry_sha256 = EXP$registry_sha256, status_counts = "OK=48",
                n_block_failures = 0L, classification_counts = "C=41"), file.path(RUN, "audit", "run_manifest.csv"))
writeLines("run_mode,block,error", file.path(RUN, "audit", "run_failures.csv"))
wcsv(data.table(run_mode = EXP$run_mode, id = "X-1", estimate = 0.25), file.path(RUN, "tables", "master_hypothesis_table.csv"))
write_run_manifest <- function() {
  fl <- c("README.txt", "audit/run_manifest.csv", "audit/run_failures.csv", "tables/master_hypothesis_table.csv")
  lines <- c("run_mode,file,bytes,sha256", sprintf("%s,%s,%d,%s", EXP$run_mode, fl, as.integer(file.size(file.path(RUN, fl))), vapply(file.path(RUN, fl), s30fb_sha, "", USE.NAMES = FALSE)))
  con <- file(file.path(RUN, "audit", "output_manifest.csv"), "wb"); writeLines(lines, con, sep = "\r\n"); close(con)   # CRLF, as the frozen run
  s30fb_sha(file.path(RUN, "audit", "output_manifest.csv"))
}
EXP$output_manifest_sha256 <- write_run_manifest()
v <- s30fb_verify_stage30_run(RUN, EXP)
check(all(v$gates$passed) && nrow(v$gates) == 12L, paste("synthetic frozen run passes every gate:", paste(v$gates[passed == FALSE, gate], collapse = "; ")))
check(identical(v$stage29_folder, "S:/x/29_canonical_behavior_releases/v101_dv2_b2ce507"), "README Stage 29 folder extracted")
g <- function(v, pat) v$gates[grepl(pat, gate, fixed = TRUE), passed]
check(!g(s30fb_verify_stage30_run(RUN, modifyList(EXP, list(output_manifest_sha256 = strrep("0", 64)))), "SHA-256 = pinned"), "a manifest hash mismatch fails")
Sys.chmod(file.path(RUN, "tables", "master_hypothesis_table.csv"), "0666"); cat("tampered\n", file = file.path(RUN, "tables", "master_hypothesis_table.csv"), append = TRUE)
check(!g(s30fb_verify_stage30_run(RUN, EXP), "bytes and SHA-256 match"), "a tampered run file fails")
wcsv(data.table(run_mode = EXP$run_mode, id = "X-1", estimate = 0.25), file.path(RUN, "tables", "master_hypothesis_table.csv")); EXP$output_manifest_sha256 <- write_run_manifest()
writeLines("x", file.path(RUN, "tables", "extra.csv"))
check(!g(s30fb_verify_stage30_run(RUN, EXP), "outside the manifest"), "an extra file in the run folder fails"); unlink(file.path(RUN, "tables", "extra.csv"))
check(!g(s30fb_verify_stage30_run(RUN, modifyList(EXP, list(run_commit = strrep("c", 40)))), "README commit"), "a README commit mismatch fails")
check(all(s30fb_verify_stage30_run(RUN, EXP)$gates$passed), "the restored run passes again")

# synthetic Stage 29 bundle
BR <- file.path(TMP, "bb"); BD <- file.path(BR, "ebb_v101_20260929_b2ce507"); dir.create(BD, recursive = TRUE)
wcsv(data.table(AnimalNum = "1", Group = "RES"), file.path(BD, "A1_animal_cc1.csv"))
bm <- data.table(file = "A1_animal_cc1.csv", bytes = file.size(file.path(BD, "A1_animal_cc1.csv")), sha256 = s30fb_sha(file.path(BD, "A1_animal_cc1.csv")), schema_version = 1L)
wcsv(bm, file.path(BD, "00_manifest.csv")); bsha <- s30fb_sha(file.path(BD, "00_manifest.csv"))
wcsv(data.table(bundle_id = basename(BD), status = "FROZEN", manifest_sha256 = bsha), file.path(BR, "BUNDLE_REGISTRY.csv"))
E29 <- list(bundle_id = basename(BD), manifest_sha256 = bsha)
check(all(s30fb_verify_stage29_bundle(BD, file.path(BR, "BUNDLE_REGISTRY.csv"), E29)$gates$passed), "synthetic Stage 29 bundle passes")
check(!all(s30fb_verify_stage29_bundle(BD, file.path(BR, "BUNDLE_REGISTRY.csv"), modifyList(E29, list(manifest_sha256 = strrep("0", 64))))$gates$passed), "bundle manifest hash mismatch fails")
wcsv(data.table(bundle_id = basename(BD), status = "DRAFT", manifest_sha256 = bsha), file.path(BR, "BUNDLE_REGISTRY.csv"))
check(!all(s30fb_verify_stage29_bundle(BD, file.path(BR, "BUNDLE_REGISTRY.csv"), E29)$gates$passed), "a non-FROZEN registry row fails")

ih <- data.table(run_mode = "R", input = c("S:/a/sus.csv", "S:/a/con.csv", "S:/a/con.csv"), bytes = c("10", "20", "20"), sha256 = c("s1", "c1", "c2"),
                 role = c("freeze_record:sus_list", "freeze_record:con_list", "freeze_record:con_list"))
check(identical(s30fb_recorded_input(ih, "freeze_record:sus_list")$sha256, "s1"), "input_hashes lookup by role")
check(grepl("exactly one", errmsg(s30fb_recorded_input(ih, "freeze_record:con_list"))), "an ambiguous role stops")
check(grepl("exactly one", errmsg(s30fb_recorded_input(ih, "inactivity_reference"))), "a missing role stops")
sl <- c(paste0(strrep("1", 64), " *./w1_03_x.csv\r"), paste0(strrep("2", 64), " *./w1_04_gate_values.csv\r"))
check(identical(s30fb_sha_list_lookup(sl, "w1_04_gate_values.csv"), strrep("2", 64)) && is.na(s30fb_sha_list_lookup(sl, "nope.csv")), "sha-list lookup")
ok("hash gates: manifest (CR), frozen run, Stage 29 bundle, input_hashes, sha lists")

# ---------------------------------------------------------------- 4. Group rule and label guards
gr <- s30fb_group_from_lists(c("A", "B", "C", "D"), sus = c("B", "C"), con = c("C", "X"))
check(identical(gr, c("RES", "SUS", "CON", "RES")), "Group: CON if on con, else SUS if on sus, else RES (con wins over sus)")
fw <- data.table(run_mode = "R", AnimalNum = c("1", "2", "1", "2"), Sex = "Female", Batch = "B3", CC = c("CC1", "CC1", "CC2", "CC2"), CageEpisodeID = "B3|sys.1|CC1",
                 light_phase_crossing_rate = c(1.5, 2.5, 9, 9), posinact40_light = c(0.995, 0.985, 0, 0), posinact60_light = 0.9, posinact40_active = 0.94)
grp <- data.table(AnimalNum = c("1", "2"), Group = c("RES", "SUS")); cz <- data.table(AnimalNum = c("1", "2"), CombZ = c(0.3, -0.7))
s1 <- s30fb_light_animals(fw, grp, cz, n_expected = 2L)
check(nrow(s1) == 2L && identical(s1$Group, c("RES", "SUS")) && identical(s1$posinact40_light, c(0.995, 0.985)) && !"run_mode" %in% names(s1), "S1: CC1 rows, Group, CombZ, no run_mode")
check(grepl("Group RES or SUS", errmsg(s30fb_light_animals(fw, data.table(AnimalNum = c("1", "2"), Group = c("RES", "CON")), cz, 2L))), "a CON animal in S1 stops")
check(grepl("CombZ missing", errmsg(s30fb_light_animals(fw, grp, cz[1], 2L))), "a missing CombZ stops")
check(grepl("expected 3", errmsg(s30fb_light_animals(fw, grp, cz, 3L))), "the S1 animal count is asserted")
fk <- data.table(run_mode = "R", AnimalNum = c("1", "2"), Batch = c("B3", "B4"), Sex = "Female", CageEpisodeID = "B3|sys.2", PRE60 = 1L, POST60 = 3L, dcookie60 = 2L,
                 PRE45 = 1, POST45 = 2, dcookie45 = 1, cc1_crossing_rate = 20, or646_protocol_deviation = FALSE)
s3 <- s30fb_cookie_animals(fk, grp, cz, list(B3 = "2023-05-19", B4 = "2023-08-19"), n_expected = 2L)
check(identical(s3$epm1_date, c("2023-05-19", "2023-08-19")), "S3: registry EPM+1 date by batch")
check(grepl("EPM\\+1 date", errmsg(s30fb_cookie_animals(fk, grp, cz, list(B3 = "2023-05-19"), 2L))), "a batch without a registry EPM+1 date stops")
ok("Group rule and label / CombZ guards")

# ---------------------------------------------------------------- 5. standardized CI rescaling
x <- data.table(id = c("cont", "cat", "l"), standardization_rule = c("estimate x SD_x (sd of resid ...)", "estimate / SD_y (sd of resid ...)", "none (L: joint F)"),
                estimate = c(0.5, 2, NA), standardizer_sd = c(4, 8, NA), ci_low = c(0.1, 1, NA), ci_high = c(0.9, 3, NA), standardized_estimate = c(2, 0.25, NA))
r <- s30fb_add_standardized_ci(x)
check(near(r$standardized_ci_low[1:2], c(0.4, 0.125)) && near(r$standardized_ci_high[1:2], c(3.6, 0.375)) && is.na(r$standardized_ci_low[3]), "CONT x SD_x, CAT / SD_y, L none")
check(attr(r, "std_max_abs_diff") == 0, "exact reproduction reports 0")
y <- copy(x); y$standardized_estimate[1] <- 2 + 5e-13; check(is.na(errmsg(s30fb_add_standardized_ci(y))), "a difference below 1e-12 passes")
y$standardized_estimate[1] <- 2 + 1e-9; check(grepl("does not reproduce", errmsg(s30fb_add_standardized_ci(y))), "a difference above 1e-12 stops")
y <- copy(x); y$standardization_rule[1] <- "estimate * 2"; check(grepl("Unknown standardization_rule", errmsg(s30fb_add_standardized_ci(y))), "an unknown rule stops")
y <- copy(x); y$standardized_estimate[3] <- 1; check(grepl("rule-'none'", errmsg(s30fb_add_standardized_ci(y))), "a standardized value on an L row stops")
y <- copy(x); y$standardizer_sd[2] <- 0; check(grepl("non-positive", errmsg(s30fb_add_standardized_ci(y))), "a zero standardizer stops")
ok("standardized CI rescaling with the 1e-12 assert")

# ---------------------------------------------------------------- 6. COOKIE-CONT centroid line identity
set.seed(20260929)
ck <- rbindlist(list(data.table(Sex = "Female", Batch = rep(c("B3", "B4", "B6"), c(5, 7, 4))), data.table(Sex = "Male", Batch = rep(c("B2", "B5"), c(6, 5)))))
ck[, `:=`(dcookie60 = round(rnorm(.N, 20, 15)), CombZ = rnorm(.N) + ifelse(Batch %in% c("B3", "B2"), 0.8, -0.3))]
within_slope <- function(d) { d <- copy(d)[, `:=`(xc = dcookie60 - mean(dcookie60), yc = CombZ - mean(CombZ)), by = Batch]; sum(d$xc * d$yc) / sum(d$xc^2) }  # hand-computed OLS slope of y ~ Batch + x
bF <- within_slope(ck[Sex == "Female"]); bM <- within_slope(ck[Sex == "Male"])
mst <- data.table(id = c("COOKIE-CONT-DC60-F", "COOKIE-CONT-DC60-M"), sex_analysis = c("F", "M"), estimate = c(bF, bM), ci_low = c(bF, bM) - 0.1, ci_high = c(bF, bM) + 0.1,
                  n_animals = c(16L, 11L), n_batches = c(3L, 2L), formula = "CombZ ~ Batch + x")
L <- s30fb_cookie_lines(ck, mst)
for (sx in c("Female", "Male")) {
  d <- ck[Sex == sx]; l <- L[Sex == sx]; b <- l$slope
  bi <- d[, .(n = .N, a = mean(CombZ) - b * mean(dcookie60)), by = Batch]                     # per-batch OLS intercepts given the slope
  check(near(sum(bi$n * bi$a) / sum(bi$n), l$y_mean - b * l$x_mean, 1e-12), paste(sx, ": n-weighted batch intercept = line intercept (centroid identity)"))
  fit_mean <- mean(bi$a[match(d$Batch, bi$Batch)] + b * d$dcookie60)
  check(near(fit_mean, l$y_mean, 1e-12) && near(l$x_mean, mean(d$dcookie60)) && near(l$y_mean, mean(d$CombZ)), paste(sx, ": centroid = means over the model's animals"))
  check(near(l$y_at_x_min, l$y_mean + b * (min(d$dcookie60) - l$x_mean)) && near(l$y_at_x_max, l$y_mean + b * (max(d$dcookie60) - l$x_mean)) &&
        l$x_min == min(d$dcookie60) && l$x_max == max(d$dcookie60), paste(sx, ": end points on the frozen-slope line"))
}
check(identical(L$derivation[1], S30FB_LINE_DERIVATION) && identical(L$n, c(16L, 11L)), "derivation text and n")
check(grepl("sex-resolved centroid", S30FB_LINE_DERIVATION, fixed = TRUE) && !grepl(S30FB_FORBIDDEN_DISPLAY, S30FB_LINE_DERIVATION, ignore.case = TRUE),
      "derivation says 'sex-resolved centroid' and passes the banned-display-word guard")
check(!any(grepl("sex-specific", c(S30FB_LINE_DERIVATION, S30FB_DESCRIPTIVE_COMPUTATIONS), fixed = TRUE)), "no 'sex-specific' in the derivation or descriptive computations")
check(grepl("banned display wording in derivation", errmsg(s30fb_cookie_lines(ck, mst, derivation = "line through the sex-specific centroid"))),
      "a banned word in the S3d derivation stops")
bad <- copy(mst); bad$n_animals[1] <- 17L; check(grepl("frozen model has n_animals = 17", errmsg(s30fb_cookie_lines(ck, bad))), "an n mismatch stops")
bad <- copy(mst); bad$n_batches[2] <- 3L; check(grepl("batches", errmsg(s30fb_cookie_lines(ck, bad))), "a batch-count mismatch stops")
bad <- copy(mst); bad$formula <- "CombZ ~ x"; check(grepl("registered", errmsg(s30fb_cookie_lines(ck, bad))), "a non-registered formula stops")
ok("COOKIE-CONT line through the centroid with the frozen slope; n assert")

# ---------------------------------------------------------------- 7. descriptive summaries
d5 <- s30fb_describe(c(10, 1, 4, 2, 3)); d4 <- s30fb_describe(c(1, 2, 3, 4))
check(d5$n == 5L && d5$median == 3 && d5$q25 == 2 && d5$q75 == 4 && d5$min == 1 && d5$max == 10, "odd-length summary (type 7)")
check(d4$median == 2.5 && near(d4$q25, 1.75) && near(d4$q75, 3.25), "even-length summary (type 7)")
check(grepl("NA", errmsg(s30fb_describe(c(1, NA)))), "NA in a summary stops")
s1s <- data.table(Sex = rep(c("Female", "Male"), each = 4), Group = rep(c("RES", "RES", "SUS", "SUS"), 2), light_phase_crossing_rate = 1:8,
                  posinact40_light = c(0.995, 0.999, 0.98, 0.991, 1, 0.97, 0.99, 0.95))
lc <- s30fb_light_descriptives(s1s)
check(nrow(lc) == 10L && "n_ge_0.99" %in% names(lc), "S1c: 4 cells x 2 measures + 2 overall rows")
check(lc[scope == "sex_group" & Sex == "Male" & Group == "SUS" & measure == "posinact40_light", n_ge_0.99] == 1L &&
      lc[scope == "overall_8" & measure == "posinact40_light", n_ge_0.99] == 5L && lc[scope == "overall_8" & measure == "posinact40_light", min] == 0.95 &&
      is.na(lc[scope == "overall_8" & measure == "light_phase_crossing_rate", n_ge_0.99]), "S1c: counts >= 0.99 (inclusive) and the minimum")
s3s <- data.table(Sex = rep(c("Female", "Male"), c(4, 3)), Group = c("RES", "RES", "SUS", "SUS", "RES", "SUS", "SUS"), PRE60 = c(1, 2, 3, 4, 5, 6, 7),
                  POST60 = c(11, 2, 13, 3, 25, 16, 9), dcookie60 = c(10, 0, 10, -1, 20, 10, 2))
cd <- s30fb_cookie_descriptives(s3s, list(median = 18, iqr = list(6, 37), increased = "94/97"))
check(cd[scope == "sex" & Sex == "Female" & measure == "dcookie60", n_increased] == 2L && cd[scope == "pooled_7" & measure == "dcookie60", n_increased] == 5L &&
      is.na(cd[scope == "pooled_7" & measure == "PRE60", n_increased]), "S3b: n_increased counts dcookie60 > 0 only")
q <- cd[scope == "all_97_registry_qc"]
check(nrow(q) == 1L && q$n == 97L && q$median == 18 && q$q25 == 6 && q$q75 == 37 && q$n_increased == 94L && grepl("copied", q$source), "S3b: registry all-97 QC row copied")
check(nrow(cd[scope == "sex_group"]) == 4L, "S3b: dcookie60 by Sex x Group")
ok("descriptive summaries")

# ---------------------------------------------------------------- 8. estimate rows, relationship, labels, screen matrix
ids <- paste0("SLEEP-CAT-IA40L-", c("F", "M", "INT", "POOL"))
base <- function(id, role, sa) data.table(run_mode = "R", id = id, role = role, family = "SLEEP-CAT", question = "CAT", sex_analysis = sa, measure_col = "posinact40_light",
                                          estimate = 0.003, ci_low = 0.001, ci_high = 0.005, se = 0.001, df = 36.5, df_method = "Kenward-Roger", standardized_estimate = 0.7,
                                          n_animals = 46L, n_cages = 12L, n_batches = 3L, classification = "A", movadj_estimate = 9e-4, movadj_ci_low = 2e-4, movadj_ci_high = 1.6e-3)
mm <- rbind(base(ids[1], "DISCOVERY", "F"), base(ids[2], "DISCOVERY", "M"), base(ids[3], "DISCOVERY", "INT"))[, `:=`(p_raw = c(0.016, 0.10, 0.004), q_local_bh = c(0.05, 0.2, 0.03), family_m = 6L)]
pp <- base(ids[4], "ESTIMATION_ONLY", "POOL")[, classification := "none (ESTIMATION_ONLY)"]
ss <- rbindlist(lapply(ids, function(i) data.table(run_mode = "R", id = i, sensitivity_id = c("POSINACT60", "MOVADJ"), estimate = c(0.0045, 9e-4), ci_low = c(0.001, 2e-4),
                                                   ci_high = c(0.008, 1.6e-3), status = "OK")))
er <- s30fb_estimate_rows(mm, pp, ss, ids, "POSINACT60", "posinact60")
check(identical(er$estimand_key, c("RS_F", "RS_M", "INT_FM", "POOL")) && identical(er$id, ids) && is.na(er$p_raw[4]) && er$n[1] == 46L, "S1b keys, order, POOL without p, n")
check(all(er$posinact60_estimate == 0.0045) && !"run_mode" %in% names(er), "sensitivity estimate copied; run_mode dropped")
bad <- copy(ss); bad[id == ids[2] & sensitivity_id == "MOVADJ", estimate := 1e-3]
check(grepl("Movement-adjusted", errmsg(s30fb_estimate_rows(mm, pp, bad, ids, "POSINACT60", "posinact60"))), "a Movement-adjusted mismatch stops")
check(grepl("exactly one row", errmsg(s30fb_estimate_rows(mm, pp, ss[sensitivity_id != "POSINACT60" | id != ids[1]], ids, "POSINACT60", "posinact60"))), "a missing sensitivity row stops")

gv <- data.table(window = c("active", "light", "active"), X_label = c("X40", "X40", "X60"), rho_rate = c(-0.9595, -0.9326, -0.95), ICC_resid = c(0.1103, 0.0837, 0.18),
                 median_frac = c(0.94156, 0.99587, 0.91), share_ge_099 = c(0.002, 0.87387, 0))
x4 <- data.table(window = rep(c("active", "light"), each = 6), X = 40L, AnimalNum = rep(c("1", "1", "2", "2", "3", "3"), 2), CC = "CC1")
qc <- list(rho_with_rate_BxCC = list(-0.96, -0.93), icc_after_rate = list(0.11, 0.08), active_median = 0.942, light_median = 0.996, "light_windows_ge_0.99" = 0.874, basis = "basis text")
rr <- s30fb_rate_relationship(qc, gv, x4, "w1_04.csv", "sha")
check(identical(rr$phase, c("active", "light")) && identical(rr$rho_full, c(-0.9595, -0.9326)) && all(rr$n_windows == 6L) && all(rr$n_animals == 3L), "S2b rows, full values, counts")
qsw <- qc; qsw$rho_with_rate_BxCC <- list(-0.93, -0.96); qic <- qc; qic$icc_after_rate <- list(0.11, 0.09)
check(grepl("registry", errmsg(s30fb_rate_relationship(qsw, gv, x4, "f", "s"))) && grepl("icc_full", errmsg(s30fb_rate_relationship(qic, gv, x4, "f", "s"))),
      "swapped rho / wrong rounded ICC stops")
check(all(s30fb_qc_rounding_checks(qc, gv)$ok) && !all(s30fb_qc_rounding_checks(modifyList(qc, list(light_median = 0.99)), gv)$ok), "registry QC rounding checks")

meas <- list(position_change_rate = list(identifier = "crossing_rate", display = "RFID position-change rate", unit = "position changes/h"),
             light_phase_position_change_rate = list(identifier = "light_phase_crossing_rate", display = "light-phase RFID position-change rate", unit = "position changes/h"),
             shared_position = list(identifier = "shared_zone_use", display = "shared RFID-position occupancy", unit = "fraction of co-assigned dyadic time (0-1)"),
             occupancy_dispersion = list(identifier = "occupancy_dispersion"), fragmentation = list(identifier = "fragmentation"),
             positional_inactivity_40 = list(identifier = "posinact40_frac", display = "RFID-defined sustained positional inactivity (>= 40 s)"),
             positional_inactivity_60 = list(identifier = "posinact60_frac"),
             cookie_response_60 = list(identifier = "dcookie60", display = "home-cage cookie response (second presentation, EPM+1)", unit = "position changes/h"),
             cookie_response_45 = list(identifier = "dcookie45"))
cfgm <- list(occupancy_dispersion = list(label = "occupancy dispersion", unit = "bits"), fragmentation = list(label = "fragmentation", unit = "proportion of bouts"))
s5 <- s30fb_display_labels(meas, cfgm)
check(nrow(s5) == nrow(S30FB_LABEL_SPEC) && !anyNA(s5$display_label) && !anyNA(s5$unit), "S5: every declared column labelled")
check(identical(s5[measure_col == "posinact40_light", display_label], "RFID-defined sustained positional inactivity (\u226540 s)") &&
      identical(s5[measure_col == "posinact60_light", display_label], "RFID-defined sustained positional inactivity (\u226560 s)") &&
      identical(s5[measure_col == "light_phase_crossing_rate", unit], "position changes/h") && grepl("config", s5[measure_col == "fragmentation", label_source]),
      "S5: registry labels typeset (>= -> U+2265), config fallback recorded")
bm2 <- modifyList(meas, list(position_change_rate = list(identifier = "crossing_rate", display = "antenna crossing rate", unit = "crossings/h")))
check(grepl("banned display wording", errmsg(s30fb_display_labels(bm2, cfgm))), "a banned display word stops")

mk <- function(q, b, mc, meas_txt, fam) data.table(id = paste(fam, c("F", "M", "INT"), sep = "-"), block = b, question = q, sex_analysis = c("F", "M", "INT"), family = fam,
                                                   family_m = 3L, measure = meas_txt, measure_col = mc, display_metric = "synthetic metric label", statistic = c(2, 1, 3),
                                                   standardized_estimate = if (q == "L") NA_real_ else c(0.1, -0.2, 0.3), standardized_ci_low = NA_real_, standardized_ci_high = NA_real_,
                                                   df1 = if (q == "L") 3L else NA_integer_, df2 = 80, p_raw = 0.5, q_local_bh = 0.6, classification = "C")
s0 <- rbind(mk("L", "broad", "crossing_rate", "crossing_rate (active, CC1-CC4)", "SCREEN-L-PCR"), mk("CAT", "inactivity", "posinact40_light", "posinact40_frac (CC1 light)", "SLEEP-CAT-IA40L"),
            mk("CONT", "broad", "shared_zone_use", "shared_zone_use (data version v2; active, CC1-CC4)", "SCREEN-CONT-SPO"))[, row_order := seq_len(.N)]
sm <- s30fb_screen_matrix(s0)
check(identical(sm[col_order == 1L, question], c("CONT", "CAT", "L")) && identical(sm[col_order == 1L, row_order], 1:3), "S4: rows ordered CONT, CAT, L")
check(identical(unique(sm$sex_col), c("Female", "Male", "Female \u2212 male")) && all(is.na(sm[question != "L", F])) && identical(sm[question == "L", F], c(2, 1, 3)),
      "S4: sex columns, F only for L rows")
check(identical(sm[question == "CONT", window_label][1], "active, CC1-CC4") && identical(sm[question == "L", question_label][1], "CC1\u2013CC4 trajectory"), "S4: window and question labels")
check(grepl("exactly F, M and INT", errmsg(s30fb_screen_matrix(s0[-1]))), "S4: an incomplete metric row stops")
ok("estimate rows, rate-inactivity relationship, display labels, screen matrix")

# ---------------------------------------------------------------- 9. full-precision CSV writer
v <- c(pi, 1 / 3, 0.1 + 0.2, 1e-300, -2.5e-17, 123456789.123456789, NA, 0.5, -0)
check(identical(s30fb_num_chr(0.1 + 0.2), "0.30000000000000004") && identical(s30fb_num_chr(0.5), "0.5") && is.na(s30fb_num_chr(NA_real_)), "shortest round-trip text")
rt <- data.table(d = v, i = c(1:8, NA), l = c(TRUE, FALSE, NA, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE),
                 s = c("a,b", "it's", "\u2265 40 s", "Female \u2212 male", NA, "", "B3|sys.1|CC1", "0004", "x"))
p <- file.path(TMP, "rt.csv"); s30fb_write_csv(rt, p)
check(grepl("Embedded double quote", errmsg(s30fb_write_csv(data.table(s = "q\"uote"), file.path(TMP, "dq.csv")))), "an embedded double quote is refused")
check(identical(utils::read.csv(p, colClasses = "character", na.strings = "", encoding = "UTF-8")$s[1:2], c("a,b", "it's")), "read.csv reads the same text")
back <- fread(p, colClasses = list(numeric = "d", character = c("i", "l", "s")), encoding = "UTF-8", na.strings = "")
check(identical(is.na(back$d), is.na(v)) && all(back$d[!is.na(v)] == v[!is.na(v)]), "every double reads back identically (fread)")
check(identical(back$s[3], "\u2265 40 s") && identical(back$s[4], "Female \u2212 male") && identical(back$s[8], "0004"), "UTF-8 text and leading zeros preserved")
ok("full-precision CSV writer")

# ---------------------------------------------------------------- 10. bundle writer: manifest completeness, provenance, immutability
tabs <- setNames(lapply(S30FB_TABLES, function(n) data.table(k = n, value = c(1 / 7, 2))), S30FB_TABLES)
GTS <- rbind(s30fb_gate_row("0-code", "synthetic pre-write gate 1", TRUE, "c,1"), s30fb_gate_row("6-tables", "synthetic pre-write gate 2", TRUE, ""),
             s30fb_gate_row("0-code", "synthetic soft gate", FALSE, "not met", hard = FALSE))
prov_fields <- setNames(as.list(paste0("v_", S30FB_PROVENANCE_KEYS)), S30FB_PROVENANCE_KEYS)
prov_fields$descriptive_computations <- S30FB_DESCRIPTIVE_COMPUTATIONS
prov_fields$pre_write_gates_passed <- s30fb_gate_count(GTS)
H <- s30fb_provenance(prov_fields)
check(grepl(" || ", H[key == "descriptive_computations", value], fixed = TRUE), "descriptive computations listed")
check(grepl("lacks key", errmsg(s30fb_provenance(prov_fields[-1]))), "a missing provenance key stops")
check(grepl("lacks key", errmsg(s30fb_provenance(prov_fields[names(prov_fields) != "pre_write_gates_passed"]))), "pre_write_gates_passed is a required provenance key")
check(!"gates_passed" %in% S30FB_PROVENANCE_KEYS && identical(s30fb_gate_count(GTS), "2 of 3"), "the gate-count key is pre_write_gates_passed and counts passed rows")
IN <- data.table(input = "x", bytes = 1, sha256 = "s", role = "r")
OUT <- file.path(TMP, "out"); BID <- "s30b_v10_20260929_0123456"
wb <- function(id, out = OUT, gates = GTS, prov = H, tb = tabs, ...) s30fb_write_bundle(tb, out, id, prov, IN, gates, S30FB_STATUS_DRY, strrep("a", 40), cm, "t", ...)
w <- s30fb_write_bundle(tabs, OUT, BID, H, IN, GTS, S30FB_STATUS_DRY, strrep("a", 40), cm, "2026-09-29T12:00:00+0200")
man <- fread(file.path(w$dir, "00_manifest.csv"))
check(setequal(man$file, c(paste0(S30FB_TABLES, ".csv"), "H_provenance.csv", "H2_inputs.csv", "H3_gate_results.csv")) && setequal(man$file, s30fb_bundle_files()) &&
      !"00_manifest.csv" %in% man$file && setequal(list.files(w$dir), c(man$file, "00_manifest.csv")), "00_manifest lists every other file (incl. H3_gate_results) and nothing else exists")
check(all(s30fb_manifest_check(w$dir, man)$ok) && all(man$schema_version == 1L), "manifest bytes / sha256 match; schema_version 1")
hp <- fread(file.path(w$dir, "H_provenance.csv")); check(all(S30FB_PROVENANCE_KEYS %in% hp$key), "H_provenance carries every DESIGN key")
check(identical(hp[key == "pre_write_gates_passed", value], "2 of 3") && !"gates_passed" %in% hp$key, "H_provenance pre_write_gates_passed")
h3 <- fread(file.path(w$dir, "H3_gate_results.csv"), colClasses = "character")
check(identical(names(h3), S30FB_GATE_COLS) && nrow(h3) == 3L && identical(h3$gate, GTS$gate) && identical(h3$passed, c("TRUE", "TRUE", "FALSE")) &&
      !any(h3$stage %in% c("7-write", "8-register")), "H3_gate_results = the pre-write gate table only")
check(all(file.access(list.files(w$dir, full.names = TRUE), 2L) != 0L), "bundle files are read-only")
reg <- fread(file.path(OUT, "BUNDLE_REGISTRY.csv"), colClasses = "character")
check(nrow(reg) == 1L && identical(names(reg), c("bundle_id", "status", "manifest_sha256", "stage30_run_commit", "mmm_git_commit", "created_at")) &&
      identical(reg$manifest_sha256, s30fb_sha(file.path(w$dir, "00_manifest.csv"))) && identical(reg$status, S30FB_STATUS_DRY), "BUNDLE_REGISTRY row beside the bundle")
lg <- fread(w$log, colClasses = "character")
check(identical(w$log, file.path(OUT, "logs", paste0(BID, "_gate_results.csv"))) && file.access(w$log, 2L) != 0L && identical(w$log_sha256, s30fb_sha(w$log)),
      "gate log logs/<bundle_id>_gate_results.csv beside the registry, read-only")
check(identical(names(lg), S30FB_GATE_COLS) && nrow(lg) == 3L + 5L && identical(lg$gate[1:3], GTS$gate) && identical(lg$stage[4:8], c(rep("7-write", 4), "8-register")) &&
      all(lg$passed[4:8] == "TRUE") && nrow(w$post_gates) == 5L, "gate log = pre-write + 4 post-write + registration gates")
check(!file.exists(file.path(w$dir, basename(w$log))) && !identical(normalizePath(dirname(w$log)), normalizePath(w$dir)) && !"logs" %in% man$file,
      "the log sits outside the bundle folder and is not in its manifest")
check(grepl("immutable", errmsg(wb(BID))), "a second write of the same bundle is refused")
check(nrow(fread(file.path(OUT, "BUNDLE_REGISTRY.csv"))) == 1L, "a refused write leaves the registry unchanged")
dir.create(file.path(OUT, ".tmp_s30b_v10_20260930_0123456"))
check(grepl("staging folder", errmsg(wb("s30b_v10_20260930_0123456"))), "a leftover staging folder is refused")
check(grepl("already registered", errmsg({ unlink(file.path(OUT, ".tmp_s30b_v10_20260930_0123456"), recursive = TRUE)
  fwrite(data.table(bundle_id = "s30b_v10_20261001_0123456", status = "FROZEN", manifest_sha256 = "x", stage30_run_commit = "a", mmm_git_commit = "b", created_at = "c"),
         file.path(OUT, "BUNDLE_REGISTRY.csv"), append = TRUE)
  wb("s30b_v10_20261001_0123456") })), "a registered bundle id is refused")
writeLines("x", s30fb_log_path(OUT, "s30b_v10_20261002_0123456"))
check(grepl("gate log for this bundle exists", errmsg(wb("s30b_v10_20261002_0123456"))) && !dir.exists(file.path(OUT, "s30b_v10_20261002_0123456")), "an existing gate log is refused before any write")
check(grepl("declared set", errmsg(wb(BID, out = file.path(TMP, "out2"), tb = tabs[-1]))), "a missing table is refused")
check(grepl("hard pre-write gate", errmsg(wb(BID, out = file.path(TMP, "out2"), gates = rbind(GTS, s30fb_gate_row("x", "failed hard gate", FALSE))))) &&
      !dir.exists(file.path(TMP, "out2")), "a failed hard gate in the gate table is refused before any write")
check(grepl("does not count", errmsg(wb(BID, out = file.path(TMP, "out2"), gates = GTS[1:2]))), "pre_write_gates_passed must count the gate table")
# post-write failure: a file changed after the chmod -> post-write gates fail, NO registry row, the log records the failure
OUT3 <- file.path(TMP, "out3"); BID3 <- "s30b_v10_20260929_0fedcba"
tamper <- function(bd) { f <- file.path(bd, "S0_master_hypotheses.csv"); Sys.chmod(f, "0666"); cat("tampered\n", file = f, append = TRUE) }
e <- errmsg(wb(BID3, out = OUT3, .before_verify = tamper))
check(grepl("NOT registered", e) && dir.exists(file.path(OUT3, BID3)) && !file.exists(file.path(OUT3, "BUNDLE_REGISTRY.csv")), "a post-write failure stops before the registry row is appended")
lg3 <- fread(s30fb_log_path(OUT3, BID3), colClasses = "character")
check(nrow(lg3) == 3L + 4L && !"8-register" %in% lg3$stage && identical(lg3[stage == "7-write", passed], c("TRUE", "FALSE", "TRUE", "FALSE")),
      "the gate log records the failed post-write gates (manifest mismatch, not read-only)")
check(grepl("immutable", errmsg(wb(BID3, out = OUT3))), "a failed-then-left bundle folder is not overwritten")
pw <- s30fb_post_write_gates(w$dir, man); check(nrow(pw) == 4L && all(pw$passed), "post-write gates pass on the intact bundle")
check(!all(s30fb_post_write_gates(w$dir, man[-1])$passed), "post-write gates fail for an incomplete manifest")
ok("bundle writer: H3 pre-write gates, post-write gates before registration, gate log, read-only, refusal to overwrite")

Sys.chmod(list.files(TMP, recursive = TRUE, full.names = TRUE), "0666"); unlink(TMP, recursive = TRUE, force = TRUE)
cat("test_stage30_figure_bundle.R: all checks passed\n")
