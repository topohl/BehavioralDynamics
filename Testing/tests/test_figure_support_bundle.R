# Contract test for the figure-support bundle helpers (Functions/figure_support_bundle.R).
#
# Synthetic data in memory / tempdir() only: nothing here reads project data, a label list, an outcome table or any S:
# path, and no Analysis/ runner is sourced or executed (runner and producer files are only read as text / parsed).
# Checks:
#   1. static: no model / test / adjustment call in the helper or the runner; the runner parses its mode before any read,
#      sources no Analysis/ file other than _pipeline_setup.R and never sources or evaluates the CombZ producer file; the
#      helper parses (never sources) it; this test sources only Analysis/_pipeline_setup.R;
#   2. run mode and bundle id fsb_v5_<YYYYMMDD>_<commit7>;
#   3. frozen CombZ definition parser: constants and composite rule read without executing the file; refusal of a missing,
#      duplicated or non-literal constant and of a missing / non-consecutive rule; the real producer source yields the
#      six components and three inversions;
#   4. frozen composite rule: mean of the present components, NA when none;
#   5. F2: signed_z = stored value, z = direction x signed_z, present flags, heatmap order, threshold flag, the 1e-12
#      recomposition gate and the n_components_present gate;
#   6. F2b: directions, reasons, relationship text, banned-wording guard;
#   7. F1 / F1b: hand-computed cage means, the mean-of-cage-means = B2 gate, cage count / size / batch gates, mixed-cage and
#      composition checks, NA refusal;
#   8. (v2) post hoc run 29b: run verification (manifest hash, files, extra file, read-only, README, run_manifest), the
#      verbatim text copy (P1 / P1b / P1c) and its gates, refusal of a table that differs from its manifest hash;
#   8b. (v3) Stage 14 display tables: input hash and promotion-record gates, state display labels, transition differences
#      (reconstruction gate, refusal of a tampered difference or an unknown label), flows (rank, shown), dwell-time counts
#      (bins, fractions, refusal outside the bins or at another resolution), the Wald interval of the resolution rows;
#   8c. (v4) Stage 09 correlations (E1 verbatim copy, n and per-animal input identity gates), post hoc run 29c (verification
#      without an n_failed column, verbatim copies, registry / OK / n / x-range gates), the workbook date parsers (both
#      date forms, locale independent) and the design ages on a hand-computed two-animal workbook with its failure gates;
#   9. writer: manifest, provenance keys, read-only files, post-write gates (incl. the line-level verbatim gates of the
#      copied post hoc tables, D1, E1 and E2-E2c) before the registry row, gate log, refusal to overwrite.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root:
#   Rscript Testing/tests/test_figure_support_bundle.R

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
source_mmm_helper("stage30_figure_bundle.R"); source_mmm_helper("figure_support_bundle.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
errmsg <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
TMP <- file.path(normalizePath(tempdir(), winslash = "/"), "fsb"); unlink(TMP, recursive = TRUE, force = TRUE); dir.create(TMP)

# ---------------------------------------------------------------- 1. static checks
HELPER <- file.path(MMM_REPO_ROOT, "Functions", "figure_support_bundle.R")
RUNNER <- file.path(MMM_REPO_ROOT, "Analysis", "16c_figure_support_bundle.R")
PRODUCER <- file.path(MMM_REPO_ROOT, "Analysis", "build_later_outcome_combz.R")
SELF <- file.path(MMM_REPO_ROOT, "Testing", "tests", "test_figure_support_bundle.R")
code_only <- function(f) { x <- readLines(f, warn = FALSE); x[!grepl("^\\s*#", x)] }
INFER <- paste0("\\b(lm|glm|lmer|glmer|lme|gam|bam|gamm|nls|loess|lowess|smooth\\.spline|splinefun|cor|cor\\.test|t\\.test|wilcox\\.test|",
                "kruskal\\.test|aov|anova|predict|coef|coefficients|fitted|resid|residuals|confint|vcov|p\\.adjust|quantile|sd|var|",
                "geom_smooth|stat_smooth|mmm_ci_[A-Za-z_]+)\\s*\\(")
for (f in c(HELPER, RUNNER)) {
  ln <- code_only(f); hit <- grep(INFER, ln, perl = TRUE)
  check(!length(hit), paste0(basename(f), " contains a model / test / adjustment call: ", paste(ln[hit], collapse = " | ")))
}
rl <- code_only(RUNNER); hl <- code_only(HELPER)
src <- grep("source\\(", rl, value = TRUE)
check(!any(grepl("Analysis/", src) & !grepl("_pipeline_setup", src)), "the runner sources no Analysis/ file other than _pipeline_setup.R")
check(!any(grepl("build_later_outcome_combz", c(src, grep("sys\\.source|eval\\(parse|source\\(", hl, value = TRUE)))),
      "neither the runner nor the helper sources the CombZ producer")
check(any(grepl("parse(file = path, keep.source = FALSE)", hl, fixed = TRUE)) && !any(grepl("^\\s*source\\(", hl)),
      "the helper parses the producer file and sources nothing")
first_read <- min(grep("\\b(fread|readRDS|readLines|fromJSON|s30fb_sha|s30fb_verify_[a-z0-9_]+|fsb_read_combz_definition)\\(", rl))
check(min(grep("fsb_parse_mode\\(", rl)) < first_read, "the runner parses its mode before reading anything")
sl <- grep("^\\s*source\\(", readLines(SELF, warn = FALSE), value = TRUE)
check(length(sl) == 1L && grepl("_pipeline_setup.R", sl), "this test sources only Analysis/_pipeline_setup.R")
check(!grepl("S:/", paste(readLines(HELPER, warn = FALSE), collapse = "\n"), fixed = TRUE), "the helper holds no S: path")
check(!any(grepl("\\b(fwrite|writeLines|write\\.csv|saveRDS|file\\.copy)\\(", rl)), "the runner writes no file itself (the helper writes bundle, registry, gate log)")
ok("static: no model call in helper / runner; mode parsed first; no Analysis runner or producer sourced")

# ---------------------------------------------------------------- 2. run mode, bundle id
check(grepl("Refusing to run", errmsg(fsb_parse_mode(character()))), "no argument refuses")
check(grepl("Both", errmsg(fsb_parse_mode(c("--dry-run", "--real")))), "both modes refuse")
check(identical(fsb_parse_mode("--dry-run"), "DRY") && identical(fsb_parse_mode("--real"), "REAL"), "modes parse")
cm <- "0123456789abcdef0123456789abcdef01234567"
bid <- fsb_bundle_id(as.Date("2026-09-30"), cm)
check(identical(bid, "fsb_v5_20260930_0123456") && grepl("^fsb_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$", bid), "bundle id fsb_v5_<YYYYMMDD>_<commit7>")
check(grepl("full git commit", errmsg(fsb_bundle_id(Sys.Date(), "abc1234"))), "a short commit is refused")
ok("run mode and bundle id")

# ---------------------------------------------------------------- 3. frozen CombZ definition parser
SENTINEL <- file.path(TMP, "EXECUTED.txt")
producer_lines <- function(extra = character(), drop = character(), rule_gap = FALSE) {
  x <- c('stop("the producer must never be executed")',
         sprintf('writeLines("x", "%s")', SENTINEL),
         'COMBZ_DEFINITION_ID <- "def_id"', 'COMBZ_REFERENCE_POP_ID <- "ref_id"', 'COMBZ_CLASSIFICATION_ID <- "cls_id"',
         'COMBZ_COMPONENTS <- c("A", "B", "C")',
         'COMBZ_COMPONENT_META <- tibble::tribble(~component, ~xlsx_column, ~domain, ~raw_measure, ~raw_derivation, ~sign_inverted, ~higher_means,',
         '  "A", "E", "dom a", "measure a", "deriv a", FALSE, "better a",',
         '  "B", "F", "dom b", "measure b", "deriv b", TRUE, "less b",',
         '  "C", "G", "dom c", "measure c", "deriv c", FALSE, "better c")',
         'PARITY_TOL_COMBZ <- 1e-12', 'COMBZ_CORRECTION_ID <- "corr_id"',
         'dat <- read.csv("never.csv")',
         FSB_COMBZ_RULE[1:2], if (rule_gap) 'x <- 1', FSB_COMBZ_RULE[3:4], extra)
  x[!x %in% drop]
}
wprod <- function(lines, name = "producer.R") { p <- file.path(TMP, name); writeLines(lines, p); p }
DEFS <- fsb_read_combz_definition(wprod(producer_lines()))
check(!file.exists(SENTINEL), "parsing the producer executes none of its other lines")
check(identical(DEFS$constants$COMBZ_COMPONENTS, c("A", "B", "C")) && identical(DEFS$constants$COMBZ_COMPONENT_META$sign_inverted, c(FALSE, TRUE, FALSE)) &&
      identical(DEFS$constants$COMBZ_DEFINITION_ID, "def_id") && length(DEFS$rule) == 4L, "constants and the 4-line rule are read")
check(grepl("exactly one top-level assignment to COMBZ_CORRECTION_ID", errmsg(fsb_read_combz_definition(wprod(producer_lines(drop = 'COMBZ_CORRECTION_ID <- "corr_id"'))))),
      "a missing constant is refused")
check(grepl("exactly one", errmsg(fsb_read_combz_definition(wprod(producer_lines(extra = 'COMBZ_COMPONENTS <- c("A")'))))), "a duplicated constant is refused")
bad_rhs <- sub('COMBZ_DEFINITION_ID <- "def_id"', 'COMBZ_DEFINITION_ID <- readLines("x")', producer_lines(), fixed = TRUE)
check(grepl("refusing to evaluate", errmsg(fsb_read_combz_definition(wprod(bad_rhs)))) && !file.exists(SENTINEL), "a non-literal constant is not evaluated")
check(grepl("not found exactly once", errmsg(fsb_read_combz_definition(wprod(producer_lines(drop = FSB_COMBZ_RULE[3]))))), "a missing rule line is refused")
check(grepl("consecutive", errmsg(fsb_read_combz_definition(wprod(producer_lines(rule_gap = TRUE))))), "a non-consecutive rule is refused")
# the real producer SOURCE (code text, no data): the frozen components and inversions
DEFR <- fsb_read_combz_definition(PRODUCER)
check(identical(DEFR$constants$COMBZ_COMPONENTS, c("NOR", "sucrose_pref", "weight_dev", "delta_cort", "adrenal_weight", "spleen_weight")) &&
      identical(DEFR$constants$COMBZ_COMPONENT_META$sign_inverted, c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE)) &&
      identical(DEFR$constants$COMBZ_DEFINITION_ID, "combz_v1_six_component_unweighted_mean"), "the real producer source parses to the frozen definition")
ok("frozen definition parser: nothing executed; refusals; the real producer source parses")

# ---------------------------------------------------------------- 4. frozen composite rule
S <- data.frame(A = c(1, 2, NA, NA), B = c(3, NA, 1, NA), C = c(5, 4, NA, NA))
R <- fsb_combz_recompose(DEFS, S)
check(isTRUE(all.equal(R$CombZ[1:3], c(3, 3, 1), tolerance = 0)) && is.na(R$CombZ[4]) && identical(R$n_components_present, c(3L, 2L, 1L, 0L)),
      "CombZ = mean of the present components (missing ignored, not zero); NA with none present")
ok("frozen composite rule")

# ---------------------------------------------------------------- 5. F2 components
CZ <- data.table(AnimalNum = c("a1", "a2", "a3", "a4", "a5"), Sex = c("Female", "Female", "Female", "Male", "Male"), Batch = c("B3", "B4", "B6", "B1", "B2"),
                 outcome_group = c("CON", "RES", "SUS", "CON", "SUS"), A = c(0.5, 1, -1, 0.2, -2), B = c(0.1, -0.5, -1.2, NA, -0.4), C = c(-0.3, 0.8, -0.6, 0.4, -1))
CZ[, `:=`(n_components_present = c(3L, 3L, 3L, 2L, 3L), combz_definition_id = "def_id")]
CZ[, CombZ := rowMeans(as.matrix(CZ[, .(A, B, C)]), na.rm = TRUE)]
THR <- data.table(Sex = c("Female", "Male"), susceptibility_threshold = c(-0.5, -0.5))
F2 <- fsb_combz_components(CZ, DEFS, THR, n_expected = 5L)
check(nrow(F2) == 15L && attr(F2, "recompose_max_abs_diff") <= 1e-12 && attr(F2, "n_present_mismatch") == 0L, "15 rows; recomposition gate passes")
r <- F2[AnimalNum == "a3" & component == "B"]
check(r$signed_z == -1.2 && r$direction == -1L && r$z == 1.2 && r$present && is.na(r$raw_value) && is.na(r$reference_mean) && is.na(r$reference_sd),
      "inverted component: signed_z stored, direction -1, z = direction x signed_z; raw / reference NA")
r <- F2[AnimalNum == "a4" & component == "B"]; check(!r$present && is.na(r$signed_z) && r$n_components_present == 2L, "a missing component: present FALSE")
check(identical(F2[component == "A" & Sex == "Female", AnimalNum], c("a3", "a1", "a2")) && identical(F2[component == "A" & Sex == "Female", heatmap_order_within_sex], 1:3),
      "heatmap order = CombZ rank within sex")
check(identical(F2[component == "A", setNames(below_threshold, AnimalNum)][c("a1", "a2", "a3", "a4", "a5")], c(a1 = FALSE, a2 = FALSE, a3 = TRUE, a4 = FALSE, a5 = TRUE)),
      "below_threshold = CombZ < stored within-sex threshold")
check(identical(names(F2)[1:13], c("AnimalNum", "Sex", "Batch", "Group", "component", "component_order", "raw_value", "reference_mean", "reference_sd", "z",
                                   "direction", "signed_z", "present")), "F2 column order follows OPTION3_SPEC section 1")
bad <- copy(CZ); bad[2, CombZ := CombZ + 1e-9]
check(grepl("differs from the stored CombZ", errmsg(fsb_combz_components(bad, DEFS, THR, n_expected = 5L))), "a stored CombZ off by 1e-9 stops the export")
bad <- copy(CZ); bad[4, n_components_present := 3L]
check(grepl("n_components_present differs", errmsg(fsb_combz_components(bad, DEFS, THR, n_expected = 5L))), "an n_components_present mismatch stops the export")
bad <- copy(CZ); bad[, combz_definition_id := "other"]
check(grepl("definition", errmsg(fsb_combz_components(bad, DEFS, THR, n_expected = 5L))), "another CombZ definition id stops the export")
check(grepl("expected 6 unique", errmsg(fsb_combz_components(CZ, DEFS, THR, n_expected = 6L))), "a wrong animal count stops the export")
ok("F2: stored signed_z, direction, present, order, threshold flag, recomposition and n-present gates")

# ---------------------------------------------------------------- 6. F2b definition
CD <- data.table(component = c("A", "B", "C"), xlsx_column = c("E", "F", "G"), raw_measure = c("measure a", "measure b", "measure c"), sign_inverted = c(FALSE, TRUE, FALSE),
                 higher_means = c("better a", "less b", "better c"), weight_in_composite = 1 / 3, combz_definition_id = "def_id", reference_population_id = "ref_id",
                 sd_convention = "population SD", reproducible_in_repository = FALSE, reproducibility_note = "carried verbatim")
check(all(fsb_component_definition_gates(DEFS, CD)$passed), "component-definition gates pass for an agreeing table")
cd2 <- copy(CD); cd2[2, sign_inverted := FALSE]; check(!all(fsb_component_definition_gates(DEFS, cd2)$passed), "a disagreeing sign_inverted fails a gate")
F2b <- fsb_combz_definition_table(DEFS, CD, F2, "x/later_outcome_combz_animal_level.csv", strrep("f", 64))
check(nrow(F2b) == 3L && identical(F2b$direction, c(1L, -1L, 1L)) && grepl("multiplied by -1", F2b$direction_reason[2]) && grepl("not inverted", F2b$direction_reason[1]),
      "F2b directions and reasons")
check(identical(F2b$n_present, c(5L, 4L, 5L)) && identical(F2b$missing_animals[2], "a4") && grepl("1 animals with 2 of 3 present", F2b$relationship_to_combz[1], fixed = TRUE) &&
      grepl("unweighted mean of the present", F2b$relationship_to_combz[1]) && all(!F2b$standardisation_reproducible_in_repository), "F2b counts and relationship text")
D2 <- DEFS; D2$constants$COMBZ_COMPONENT_META$higher_means[1] <- "acute response"
check(grepl("banned display wording", errmsg(fsb_combz_definition_table(D2, CD, F2, "x", "y"))), "banned wording in F2b text stops the export")
ok("F2b: directions, reasons, counts, relationship, banned-wording guard")

# ---------------------------------------------------------------- 7. F1 / F1b CON cage means
set.seed(1)
mk <- function(sex, batch, cc, grp, ids, sys) data.table(AnimalNum = ids, Sex = sex, Batch = batch, Group = grp, CC = cc, CageEpisodeID = paste(batch, sys, cc, sep = "|"),
                                                          n_in_cage = 4L, crossing_rate = round(runif(4, 10, 30), 3), shared_zone_use = round(runif(4, 0.1, 0.4), 4),
                                                          occupancy_dispersion = round(runif(4, 1.8, 2.3), 4), fragmentation = round(runif(4, 0.2, 0.5), 4))
B1 <- rbindlist(lapply(c("CC1", "CC2"), function(cc) rbindlist(c(
  lapply(c("B3", "B4", "B6"), function(b) mk("Female", b, cc, "CON", paste0(b, "c", 1:4), if (cc == "CC1") "sys.3" else "sys.1")),
  lapply(c("B1", "B2", "B5"), function(b) mk("Male", b, cc, "CON", paste0(b, "c", 1:4), if (cc == "CC1") "sys.3" else "sys.2")),
  lapply(c("B3", "B1"), function(b) mk(if (b == "B3") "Female" else "Male", b, cc, "RES", paste0(b, "s", 1:4), "sys.4"))))))
B2 <- B1[Group == "CON", .(n = .N, mean = mean(crossing_rate)), by = .(CC, Sex, Group)][, construct := "crossing_rate"]
for (m in setdiff(FSB_MEASURES, "crossing_rate")) B2 <- rbind(B2, B1[Group == "CON", .(n = .N, mean = mean(get(m))), by = .(CC, Sex, Group)][, construct := m])
B2 <- rbind(B2, B1[Group == "RES", .(n = .N, mean = mean(crossing_rate)), by = .(CC, Sex, Group)][, construct := "crossing_rate"])
LAB <- fsb_metric_labels(list(crossing_rate = list(label = "RFID position-change rate", unit = "position changes/hour", tier = "primary"),
                              shared_zone_use = list(label = "shared RFID-position occupancy", unit = "fraction", tier = "primary"),
                              occupancy_dispersion = list(label = "occupancy dispersion", unit = "bits", tier = "secondary"),
                              fragmentation = list(label = "fragmentation", unit = "proportion of bouts", tier = "secondary")))
check(grepl("banned", errmsg(fsb_metric_labels(list(crossing_rate = list(label = "antenna crossings", unit = "u", tier = "t")), "crossing_rate"))), "a banned metric label stops")
F1 <- fsb_con_cage_means(B1, LAB); F1b <- fsb_con_reference_means(B2, LAB)
hand <- mean(B1[CageEpisodeID == "B4|sys.1|CC2", crossing_rate])
check(nrow(F1) == 4L * 2L * 2L * 3L && nrow(F1b) == 4L * 2L * 2L && !any(F1$Group != "CON") && !"RES" %in% F1b$Group, "F1 rows = measures x sexes x CCs x 3 cages; CON only")
check(F1[measure == "crossing_rate" & CageEpisodeID == "B4|sys.1|CC2", cage_mean] == hand &&
      F1[measure == "crossing_rate" & CageEpisodeID == "B4|sys.1|CC2", n_animals] == 4L &&
      identical(F1[measure == "crossing_rate" & CageEpisodeID == "B4|sys.1|CC2", animals], "B4c1;B4c2;B4c3;B4c4"), "a cage mean equals the hand-computed mean of its 4 animals")
CK <- fsb_con_cage_checks(F1, F1b, cages_expected = 3L, per_cage_expected = 4L)
check(nrow(CK) == 16L && all(CK$ok_cages) && all(CK$ok_sizes) && all(CK$ok_batches) && all(CK$ok_n) && all(CK$ok_mean) && max(CK$abs_diff) <= 1e-12,
      "every check passes; mean of the 3 cage means = the reference mean within 1e-12")
check(F1b[measure == "fragmentation" & Sex == "Male" & CC == "CC1", mean] == B2[construct == "fragmentation" & Sex == "Male" & CC == "CC1" & Group == "CON", mean] &&
      all(grepl("^copied", F1b$source)), "F1b copies the B2 CON mean")
b1x <- B1[!(AnimalNum == "B6c4" & CC == "CC1")]
ckx <- fsb_con_cage_checks(fsb_con_cage_means(b1x, LAB), F1b)
check(!all(ckx$ok_sizes) && !all(ckx$ok_n) && !all(ckx$ok_mean), "a 3-animal cage fails the size, n and mean-of-means checks")
b2x <- copy(B2); b2x[construct == "shared_zone_use" & Sex == "Female" & CC == "CC2" & Group == "CON", mean := mean + 1e-9]
check(sum(!fsb_con_cage_checks(F1, fsb_con_reference_means(b2x, LAB))$ok_mean) == 1L, "a reference mean off by 1e-9 fails the gate")
b1y <- copy(B1); b1y[AnimalNum == "B1c1" & CC == "CC2", `:=`(Batch = "B2", CageEpisodeID = "B2|sys.2|CC2")]
cky <- fsb_con_cage_checks(fsb_con_cage_means(b1y, LAB), F1b)
check(!all(cky$ok_sizes) && all(cky$ok_n), "an animal moved to another cage fails the size check")
check(isTRUE(fsb_con_composition(B1)$ok_pure) && isTRUE(fsb_con_composition(B1)$ok_stable), "synthetic CON cages are pure and stable")
b1z <- copy(B1); b1z[AnimalNum == "B3s1" & CC == "CC1", CageEpisodeID := "B3|sys.3|CC1"]
check(!fsb_con_composition(b1z)$ok_pure, "a SIS animal in a CON cage is detected")
b1w <- copy(B1); b1w[AnimalNum == "B5c2" & CC == "CC2", AnimalNum := "B5c9"]
check(!fsb_con_composition(b1w)$ok_stable, "a changed CON group composition is detected")
b1n <- copy(B1); b1n[1, crossing_rate := NA_real_]
check(grepl("non-finite", errmsg(fsb_con_cage_means(b1n, LAB))), "an NA CON value stops the export")
ok("F1 / F1b: hand-computed cage mean, mean-of-cage-means gate, cage / size / batch / composition checks")

# ---------------------------------------------------------------- 8. post hoc run 29b (synthetic run folder)
PHR <- file.path(TMP, "pipeline", FSB_POSTHOC$stage_dir, FSB_POSTHOC$run_name); dir.create(file.path(PHR, "tables"), recursive = TRUE); dir.create(file.path(PHR, "audit"))
mn <- intToUtf8(0x2212L); est_ids <- c("CON_mean", "RES_mean", "SUS_mean", "RES_minus_CON", "SUS_minus_CON", "SUS_minus_RES")
ph_rows <- function(model) { x <- CJ(measure = c("crossing_rate", "shared_zone_use"), Sex = c("Female", "Male"), estimand = est_ids, sorted = FALSE)
  x[, `:=`(measure_label = ifelse(measure == "crossing_rate", "RFID position-change rate", "shared RFID-position occupancy"), unit = "u", model = model,
           estimand_label = c(CON_mean = "CON", RES_mean = "RES", SUS_mean = "SUS", RES_minus_CON = paste("RES", mn, "CON"), SUS_minus_CON = paste("SUS", mn, "CON"),
                              SUS_minus_RES = paste("SUS", mn, "RES"))[estimand], tested = grepl("minus", estimand), estimate = rnorm(.N) / 3, kr_messages = "",
           failure_reason = NA_character_, status = "OK", registry_version = FSB_POSTHOC$registry_version, registry_sha256 = FSB_POSTHOC$registry_sha256, tier = FSB_POSTHOC$tier,
           display_note = "Post hoc, cage-aware; CON = 3 cages/sex", caveats = "CON = 3 cages per sex, one per batch", registered_rs_note = "registered FU-CC1 remains authoritative")]
  x[, p_holm := ifelse(tested, runif(.N), NA_real_)][, holm_m := ifelse(tested, 3L, NA_integer_)][] }
s30fb_write_csv(ph_rows("primary_separate_cage_variances"), file.path(PHR, "tables", "estimates.csv"))
s30fb_write_csv(ph_rows("sensitivity_common_cage_variance")[, same_sign := TRUE], file.path(PHR, "tables", "sensitivity_common_cage_variance.csv"))
s30fb_write_csv(data.table(model_id = paste0("m", 1:8), singular = c(TRUE, rep(FALSE, 7)), vc_con_cage = c(0, runif(7)), error = NA_character_), file.path(PHR, "tables", "diagnostics.csv"))
s30fb_write_csv(data.table(stage = "s", gate = "g", passed = TRUE), file.path(PHR, "audit", "gate_results.csv"))
s30fb_write_csv(data.table(input = "x", sha256 = "y"), file.path(PHR, "audit", "input_hashes.csv"))
s30fb_write_csv(data.table(mode = "REAL", git_commit = FSB_POSTHOC$run_commit, registry_sha256 = FSB_POSTHOC$registry_sha256, registry_version = FSB_POSTHOC$registry_version,
                           status_counts = FSB_POSTHOC$status_counts, n_failed = 0L), file.path(PHR, "audit", "run_manifest.csv"))
writeLines(c("Stage 29b", paste("commit", FSB_POSTHOC$run_commit), paste("registry sha256", FSB_POSTHOC$registry_sha256)), file.path(PHR, "README.txt"))
phf <- sort(setdiff(s30fb_files_on_disk(PHR), "audit/output_manifest.csv"))
s30fb_write_csv(data.table(file = phf, bytes = as.numeric(file.size(file.path(PHR, phf))), sha256 = vapply(file.path(PHR, phf), s30fb_sha, "", USE.NAMES = FALSE)),
                file.path(PHR, "audit", "output_manifest.csv"))
EXPH <- FSB_POSTHOC; EXPH$output_manifest_sha256 <- s30fb_sha(file.path(PHR, "audit", "output_manifest.csv"))
v0 <- fsb_verify_posthoc_run(PHR, EXPH)
check(!all(v0$gates$passed) && identical(v0$gates[passed == FALSE, gate], "every run file is read-only"), "writable run files fail only the read-only gate")
Sys.chmod(list.files(PHR, recursive = TRUE, full.names = TRUE), "0444")
VP <- fsb_verify_posthoc_run(PHR, EXPH)
check(all(VP$gates$passed) && nrow(VP$manifest) == 7L, "post hoc run verification passes for a frozen synthetic run")
check(sum(!fsb_verify_posthoc_run(PHR)$gates$passed) == 1L, "the pinned manifest hash fails on another run folder")
e2 <- EXPH; e2$run_commit <- strrep("1", 40); check(sum(!fsb_verify_posthoc_run(PHR, e2)$gates$passed) == 2L, "another run commit fails the README and run_manifest gates")
e3 <- EXPH; e3$status_counts <- "primary OK=3; FAILED=1"; check(!all(fsb_verify_posthoc_run(PHR, e3)$gates$passed), "other status counts fail")
PTS <- lapply(FSB_POSTHOC_TABLES, function(rel) fsb_posthoc_copy(PHR, rel, VP$check[file == rel, observed_sha256], "29b/v1.0_x"))
check(all(vapply(PTS, function(x) all(vapply(x, is.character, TRUE)), TRUE)) && identical(names(PTS$P1_posthoc_con_contrasts)[1:3], c("measure", "Sex", "estimand")) &&
      identical(tail(names(PTS$P1b_posthoc_sensitivity), 3), FSB_POSTHOC_ADDED) && all(PTS$P1c_posthoc_diagnostics$source_file == "tables/diagnostics.csv"),
      "copies: every cell as text, source column order kept, provenance columns appended")
src_e <- fread(file.path(PHR, "tables", "estimates.csv"))
check(identical(as.numeric(PTS$P1_posthoc_con_contrasts$estimate), src_e$estimate) && identical(PTS$P1_posthoc_con_contrasts$kr_messages[1], "") &&
      is.na(PTS$P1_posthoc_con_contrasts$failure_reason[1]) && is.na(PTS$P1_posthoc_con_contrasts$p_holm[1]), "the text copy reads back to the identical doubles; blank = NA, empty string kept")
check(all(fsb_posthoc_gates(PTS, PHR, EXPH)$passed), "post hoc copy gates pass")
bp <- PTS; bp$P1_posthoc_con_contrasts <- copy(bp$P1_posthoc_con_contrasts)[1, estimate := "0.1"]
check(sum(!fsb_posthoc_gates(bp, PHR, EXPH)$passed) == 1L, "a changed cell fails the verbatim gate")
bp <- PTS; bp$P1_posthoc_con_contrasts <- copy(bp$P1_posthoc_con_contrasts)[1, estimand_label := "acute CON"]
check(grepl("banned display wording", errmsg(fsb_posthoc_gates(bp, PHR, EXPH))), "banned wording in the copied labels stops")
check(grepl("differs from its output-manifest", errmsg(fsb_posthoc_copy(PHR, "tables/estimates.csv", strrep("0", 64), "x"))), "a table off its manifest hash is refused")
ok("post hoc run 29b: verification, verbatim text copy, copy gates, refusals")

# ---------------------------------------------------------------- 8b. (v3) Stage 14 display tables (synthetic)
S14 <- file.path(TMP, "s14"); dir.create(file.path(S14, "stats_tables"), recursive = TRUE); dir.create(file.path(S14, "tables"))
lab_src <- data.table(resolution = rep(c("10min_based", "5min_based"), each = 4L), State = rep(1:4, 2),
                      SemanticState = rep(c("burst", "inactive", "mixed", "inactive"), 2),
                      StateLabel = paste0("S", rep(1:4, 2), "\n", rep(c("burst", "inactive", "mixed", "inactive"), 2)),
                      DisplayState = c("Many", "None, together", "Few", "None, apart", "a", "b", "c", "d"))
LBS <- fsb_hmm_state_labels(lab_src)
check(identical(LBS$State, FSB_HMM_STATES) && identical(LBS$DisplayState, c("Many", "None, together", "Few", "None, apart")), "state labels of the 10-minute HMM, in state order")
lx <- copy(lab_src); lx[2, DisplayState := "Many"]
check(grepl("unique labels", errmsg(fsb_hmm_state_labels(lx))), "a duplicated display label is refused")
lx <- copy(lab_src); lx[3, DisplayState := "sleep-like"]
check(grepl("banned display wording", errmsg(fsb_hmm_state_labels(lx))), "a banned display label is refused")
tm <- CJ(Sex = c("Female", "Male"), Group = FSB_HMM_GROUPS, From = 1:4, To = 1:4)
tm[, `:=`(FromLabel = LBS$StateLabel[From], ToLabel = LBS$StateLabel[To], mean_probability = round(runif(.N, 0, 1), 6))]
tmw <- dcast(tm, Sex + FromLabel + ToLabel ~ Group, value.var = "mean_probability")
td <- rbindlist(lapply(FSB_HMM_CONTRASTS, function(cn) { g <- strsplit(cn, " - ")[[1]]
  copy(tmw)[, `:=`(Contrast = cn, DeltaProbability = get(g[1]) - get(g[2]))] }))
M1s <- fsb_hmm_transition_difference(td, LBS)
check(nrow(M1s) == 96L && attr(M1s, "max_abs_recon_diff") == 0 && all(M1s$colour_limit == max(abs(td$DeltaProbability))) &&
      identical(M1s[1, c(from_label, to_label)], c("Many", "Many")), "M1: 96 rows, exact reconstruction, the figure's own limit, display labels")
tdx <- copy(td); tdx[5, DeltaProbability := DeltaProbability + 1e-9]
check(grepl("differs from the stored means", errmsg(fsb_hmm_transition_difference(tdx, LBS))), "a difference off its means by 1e-9 is refused")
tdx <- copy(td); tdx[1, FromLabel := "S9\nnew"]
check(grepl("without a state", errmsg(fsb_hmm_transition_difference(tdx, LBS))), "an unknown state label is refused")
M2s <- fsb_hmm_transition_flows(tm[, .(Sex, Group, FromLabel, ToLabel, mean_probability)], LBS)
top <- tm[Sex == "Male" & Group == "SUS"][order(-mean_probability)][1:FSB_HMM_FLOW_TOP]
check(nrow(M2s) == 96L && all(M2s[, sum(shown), by = .(Sex, Group)]$V1 == FSB_HMM_FLOW_TOP) &&
      setequal(M2s[Sex == "Male" & Group == "SUS" & shown == TRUE, paste(from_state, to_state)], top[, paste(From, To)]) &&
      identical(M2s[Sex == "Male" & Group == "SUS", rank_within_sex_group], 1:16), "M2: the 8 largest per sex x group shown, ranks 1-16")
dw <- CJ(Sex = c("Female", "Male"), Phase = c("Active", "Inactive"), Group = FSB_HMM_GROUPS, State = 1:4, AnimalNum = paste0("a", 1:3), CageChange = c("CC1", "CC2"))
dw[, `:=`(BinLevel = "10min_based", bin_size_sec = 600, mean_dwell_bins = c(1, 1.5, 2, 3.99, 64, 128)[(seq_len(.N) - 1L) %% 6L + 1L])]
M3s <- fsb_hmm_dwell_distribution(dw, LBS)
g1 <- M3s[Sex == "Female" & Phase == "Active" & Group == "CON" & State == "1"]
check(nrow(M3s) == 2L * 2L * 3L * 4L * 7L && sum(M3s$n_records) == nrow(dw) && identical(attr(M3s, "n_records_total"), nrow(dw)) &&
      max(abs(M3s[, sum(fraction), by = .(Sex, Phase, Group, State)]$V1 - 1)) <= 1e-12 &&
      identical(g1$n_records, c(2L, 2L, 0L, 0L, 0L, 0L, 2L)) && identical(g1$bin_label[c(1, 7)], c("10-20 min", "640-1280 min")) && all(g1$n_animals == 3L),
      "M3: complete grid, counts per doubling bin (lower bound inclusive, last bin closed), fractions sum to 1")
dwx <- copy(dw); dwx[1, mean_dwell_bins := 0.5]
check(grepl("outside", errmsg(fsb_hmm_dwell_distribution(dwx, LBS))), "a dwell below one bin is refused")
dwx <- copy(dw); dwx[1, BinLevel := "5min_based"]
check(grepl("not the 10min_based HMM", errmsg(fsb_hmm_dwell_distribution(dwx, LBS))), "another HMM resolution is refused")
rs <- CJ(resolution = c("5min_based", "10min_based"), Sex = c("Female", "Male"), Phase = c("Active", "Inactive"), contrast = c("RES-CON", "SUS-CON", "SUS-RES"))
rs[, `:=`(Domain = "Behavioral state architecture", animal_level_hedges_g = 0.1, mixed_model_estimate = 0.25, mixed_model_SE = 0.5)]
M4s <- fsb_hmm_resolution_sensitivity(rs)
check(nrow(M4s) == 24L && all(M4s$wald_ci_low == 0.25 - 0.98) && all(M4s$wald_ci_high == 0.25 + 0.98) && !"wald_ci_low" %in% names(rs) &&
      identical(unique(M4s$phase_label), c("Dark phase", "Light phase")), "M4: the Wald interval estimate -/+ 1.96 SE; the input is not modified")
d1_src <- data.table(heatmap_id = "all_blocks", window_label = "all phase blocks, CC1-CC4 pooled", variant = "picked", row_label = "Position-change rate",
                     row_description = "Psychomotor activation", display_tier = "Primary", PhaseClass = "Active", Sex = "Female", contrast = "RES-CON",
                     cell_status = "estimated", hedges_g = c(0.12, -0.5), construct_source = "canonical crossing_rate (not antenna crossings)")
s30fb_write_csv(d1_src, file.path(S14, "stats_tables", "domain.csv"))
for (k in c("means", "diffs", "res", "labels")) s30fb_write_csv(data.table(x = k), file.path(S14, "tables", paste0(k, ".csv")))
pin14 <- list(root_rel = "analyses/s14", files = c(domain_heatmap = "stats_tables/domain.csv", transition_means = "tables/means.csv",
              transition_differences = "tables/diffs.csv", resolution_sensitivity = "tables/res.csv", state_display_labels = "tables/labels.csv"))
pin14$sha256 <- vapply(pin14$files, function(f) s30fb_sha(file.path(S14, f)), "")
rec14 <- data.table(group = "g", target_root_rel = pin14$root_rel, target_file = pin14$files, prior_sha256 = "p", sha256 = pin14$sha256, change = "unchanged")
check(all(fsb_verify_stage14_inputs(S14, rec14, pin14)$passed), "Stage 14 input gates pass for pinned files the record lists")
rx <- copy(rec14); rx[2, sha256 := strrep("0", 64)]
check(sum(!fsb_verify_stage14_inputs(S14, rx, pin14)$passed) == 1L, "a file whose record hash differs fails one gate")
D1s <- fsb_posthoc_copy(S14, "stats_tables/domain.csv", pin14$sha256[["domain_heatmap"]], "analyses/s14")
check(all(vapply(D1s, is.character, TRUE)) && identical(tail(names(D1s), 3), FSB_POSTHOC_ADDED) && isTRUE(fsb_check_display(D1s, FSB_D1_DISPLAY_COLS, "D1")),
      "D1: a text copy with provenance cells; only display columns are held to the wording guard")
ok("Stage 14 display tables: input gates, labels, differences, flows, dwell counts, Wald interval, D1 copy")

# ---------------------------------------------------------------- 8c. (v4) Stage 09 correlations, post hoc run 29c, design ages (synthetic)
set.seed(4)
A2s <- data.table(AnimalNum = c(as.character(1:6), paste0("OR", 7:12)), Group = rep(c("CON", "RES", "SUS"), 4), Sex = rep(c("Female", "Male"), each = 6),
                  observed_CombZ = round(rnorm(12), 6), Movement_mean = round(runif(12, 1, 7), 6))
S09D <- file.path(TMP, "s09"); dir.create(file.path(S09D, "tables"), recursive = TRUE); S09F <- file.path(S09D, "tables", "corr.csv")
e1_src <- CJ(Sex = c("Female", "Male"), feature = FSB_STAGE09$features, sorted = FALSE)
e1_src[, `:=`(feature_label = c(Movement_mean = "Movement", Movement_rmssd = "Movement RMSSD", Entropy_acf1 = "Entropy persistence")[feature], n = 6L,
              spearman_rho = -0.4, spearman_p = 0.01, spearman_p_bh_within_sex = 0.02, CorrelationUse = "Descriptive sex-stratified estimate; not an interaction test")]
s30fb_write_csv(e1_src, S09F)
mli_s <- A2s[, .(AnimalNum, Group, Sex, Movement_mean, outcome = observed_CombZ)]
E1s <- fsb_posthoc_copy(S09D, "tables/corr.csv", s30fb_sha(S09F), "pipeline/09_early_prediction/10min")
check(all(fsb_stage09_gates(E1s, S09F, A2s, mli_s)$passed), "E1 gates pass: verbatim, 6 rows, n per sex, Stage 09 input = A2")
mx <- copy(mli_s); mx[3, outcome := outcome + 1e-9]
check(sum(!fsb_stage09_gates(E1s, S09F, A2s, mx)$passed) == 1L, "a Stage 09 input off A2 by 1e-9 fails the identity gate only")
ex <- copy(E1s)[1, n := "7"]
check(sum(!fsb_stage09_gates(ex, S09F, A2s, mli_s)$passed) == 2L, "a changed n fails the verbatim and the n gate")
P29 <- file.path(TMP, "pipeline", FSB_POSTHOC29C$stage_dir, FSB_POSTHOC29C$run_name); dir.create(file.path(P29, "tables"), recursive = TRUE); dir.create(file.path(P29, "audit"))
tag29 <- function(x) x[, `:=`(registry_version = FSB_POSTHOC29C$registry_version, registry_sha256 = FSB_POSTHOC29C$registry_sha256, tier = FSB_POSTHOC29C$tier)][]
fl_s <- rbindlist(lapply(c("Female", "Male"), function(s) { x <- A2s[Sex == s]; g <- seq(min(x$Movement_mean), max(x$Movement_mean), length.out = 101L)
  data.table(Sex = s, grid_index = 1:101, x = g, fit = 0.5 - 0.2 * g, ci_low = 0.4 - 0.2 * g, ci_high = 0.6 - 0.2 * g, n = nrow(x)) }))
s30fb_write_csv(tag29(fl_s), file.path(P29, "tables", "fit_lines.csv"))
s30fb_write_csv(tag29(data.table(Sex = rep(c("Female", "Male"), each = 2), term = rep(c("intercept", "slope"), 2), estimate = c(0.5, -0.2, 0.5, -0.2),
                                 se = 0.1, n = 6L)), file.path(P29, "tables", "coefficients.csv"))
s30fb_write_csv(tag29(data.table(Sex = c("Female", "Male"), n = 6L, status = "OK")), file.path(P29, "tables", "diagnostics.csv"))
s30fb_write_csv(data.table(stage = "s", gate = "g", passed = TRUE), file.path(P29, "audit", "gate_results.csv"))
s30fb_write_csv(data.table(input = "x", sha256 = "y"), file.path(P29, "audit", "input_hashes.csv"))
s30fb_write_csv(data.table(mode = "REAL", git_commit = FSB_POSTHOC29C$run_commit, registry_sha256 = FSB_POSTHOC29C$registry_sha256, registry_version = FSB_POSTHOC29C$registry_version,
                           status_counts = FSB_POSTHOC29C$status_counts), file.path(P29, "audit", "run_manifest.csv"))
writeLines(c("Stage 29c", paste("commit", FSB_POSTHOC29C$run_commit), paste("registry sha256", FSB_POSTHOC29C$registry_sha256)), file.path(P29, "README.txt"))
p29f <- sort(setdiff(s30fb_files_on_disk(P29), "audit/output_manifest.csv"))
s30fb_write_csv(data.table(file = p29f, bytes = as.numeric(file.size(file.path(P29, p29f))), sha256 = vapply(file.path(P29, p29f), s30fb_sha, "", USE.NAMES = FALSE)),
                file.path(P29, "audit", "output_manifest.csv"))
Sys.chmod(list.files(P29, recursive = TRUE, full.names = TRUE), "0444")
EX29 <- FSB_POSTHOC29C; EX29$output_manifest_sha256 <- s30fb_sha(file.path(P29, "audit", "output_manifest.csv"))
V29s <- fsb_verify_posthoc_run(P29, EX29, FSB_POSTHOC29C_TABLES, "1e-posthoc29c")
check(all(V29s$gates$passed) && all(V29s$gates$stage == "1e-posthoc29c"), "29c run verification passes (run_manifest without an n_failed column)")
P29s <- lapply(FSB_POSTHOC29C_TABLES, function(rel) fsb_posthoc_copy(P29, rel, V29s$check[file == rel, observed_sha256], "pipeline/29c/v1.0_x"))
check(all(fsb_posthoc29c_gates(P29s, P29, A2s, EX29)$passed), "29c copy gates pass: rows, verbatim, registry, OK fits, n and x range = A2, no P column")
bx <- P29s; bx$E2_movement_combz_fit_lines <- copy(bx$E2_movement_combz_fit_lines)[1, x := "0.5"]
check(sum(!fsb_posthoc29c_gates(bx, P29, A2s, EX29)$passed) == 2L, "a changed grid start fails the verbatim and the x-range gate")
pd <- fsb_parse_day(c("11/16/2022", "21-Apr-24", "26-APR-24", "2024-04-21", "31-Foo-24", NA))
check(identical(format(pd), c("2022-11-16", "2024-04-21", "2024-04-26", NA, NA, NA)), "day parser: m/d/Y and d-Mon-yy (any case); anything else NA")
pw <- fsb_parse_window(c("11/11/2022 - 11/16/2022", "21-Apr-24 - 26-Apr-24", "11/11/2022"))
check(identical(format(pw$start), c("2022-11-11", "2024-04-21", NA)) && identical(format(pw$end), c("2022-11-16", "2024-04-26", NA)) &&
      identical(fsb_parse_window(c("38 - 43", NA), as_int = TRUE)$end, c(43L, NA)), "window parser: both date forms, integer windows, NA when unparsable")
tl_s <- data.table(ID = c("HKH0-OQ752", "HKH1-00003"), Batch = c("1", "2"), DoB = c("44838", "44930"), `SIS start` = c("44862", "44955"),
                   SucPref = c("11/11/2022 - 11/16/2022", "12-Feb-23 - 17-Feb-23"), `...17` = c("38 - 43", NA),
                   SocPref = c("44882", "44975"), `...19` = c("44", "45"), OFT = c("44884", "44977"), `...21` = c("46", "47"),
                   NOR = c("44885", "44978"), `...23` = c("47", "48"), EPM = c("44886", "44979"), `...25` = c("48", "49"),
                   `End date` = c("44890", "44985"), Age = c("52", "55"))
a4_s <- data.table(AnimalNum = c("OQ752", "3"), Sex = c("Male", "Female"), Batch = c("B1", "B2"), Group = c("SUS", "CON"))
v_s <- data.table(Batch = c("", "B1", "B1", "B2", "B2"), CC = c("", "CC1", "CC4", "CC1", "CC4"),
                  t0 = c("", "2022-10-28 18:31:05", "2022-11-09 18:30:53", "2023-01-29 18:30:43", "2023-02-10 18:31:14"))
Ds <- fsb_design_ages(tl_s, a4_s, v_s)
check(identical(Ds$T1$event_id, FSB_DESIGN_EVENTS$event_id) && identical(Ds$T1$age_min, c(24L, 37L, 38L, 44L, 46L, 47L, 48L, 52L)) &&
      identical(Ds$T1$age_max, c(25L, 38L, 44L, 45L, 47L, 48L, 49L, 55L)) && all(Ds$T1$n_animals == 2L) && nrow(Ds$T1b) == 16L &&
      identical(Ds$T1b[event_id == "shuffle_stop" & AnimalNum == "3", date_start], "2023-02-11"),
      "design ages = hand-computed: CC1, CC4 + 1 day, the sucrose window, the test days and the end date minus the date of birth")
check(all(fsb_design_age_gates(Ds, tl_s, n_expected = 2L)$passed), "design-age gates pass, including every age the workbook records itself")
tx <- copy(tl_s)[1, `...21` := "45"]
gx <- fsb_design_age_gates(fsb_design_ages(tx, a4_s, v_s), tx, n_expected = 2L)
check(identical(gx[passed == FALSE, gate], "workbook-recorded OFT age = computed age (2 animals with a recorded age)"), "a recorded age that disagrees fails its gate only")
tx <- copy(tl_s)[2, `SIS start` := "44590"]
check(sum(!fsb_design_age_gates(fsb_design_ages(tx, a4_s, v_s), tx, n_expected = 2L)$passed) == 1L, "a SIS start off the CC1 date fails one gate")
tx <- copy(tl_s)[1, DoB := "44841"]
gx <- fsb_design_age_gates(fsb_design_ages(tx, a4_s, v_s), tx, n_expected = 2L)
check(any(!gx$passed & grepl("^CC1 ages", gx$gate)), "a CC1 age outside 22-29 fails the CC1 gate")
check(grepl("lacks column", errmsg(fsb_design_ages(tl_s[, !"Age"], a4_s, v_s))), "a workbook without a declared column is refused")
ok("v4: E1 copy and identity gates, 29c verification and copies, date parsers, design ages and their gates")

# ---------------------------------------------------------------- 9. writer
tabs <- c(list(F1_con_cage_means = F1, F1b_con_reference_means = F1b, F2_combz_components = F2, F2b_combz_definition = F2b), PTS,
          list(D1_domain_heatmap_cells = D1s, M1_hmm_transition_difference = M1s, M2_hmm_transition_flows = M2s,
               M3_hmm_dwell_distribution = M3s, M4_hmm_resolution_sensitivity = M4s, E1_movement_combz_spearman_by_sex = E1s),
          P29s, list(T1_design_ages = Ds$T1, T1b_design_ages_by_animal = Ds$T1b))
VB <- c(setNames(file.path(PHR, FSB_POSTHOC_TABLES), names(FSB_POSTHOC_TABLES)), D1_domain_heatmap_cells = file.path(S14, "stats_tables", "domain.csv"),
        E1_movement_combz_spearman_by_sex = S09F, setNames(file.path(P29, FSB_POSTHOC29C_TABLES), names(FSB_POSTHOC29C_TABLES)))

GTS <- rbind(s30fb_gate_row("0-code", "synthetic pre-write gate", TRUE, "c"), s30fb_gate_row("0-code", "soft gate", FALSE, "not met", hard = FALSE))
pf <- setNames(as.list(paste0("v_", FSB_PROVENANCE_KEYS)), FSB_PROVENANCE_KEYS)
pf$descriptive_computations <- FSB_DESCRIPTIVE_COMPUTATIONS; pf$scientific_recomputation <- FSB_SCIENTIFIC_RECOMPUTATION; pf$pre_write_gates_passed <- fsb_gate_count(GTS)
H <- fsb_provenance(pf)
check(grepl("lacks key", errmsg(fsb_provenance(pf[-1]))), "a missing provenance key stops")
check(startsWith(H[key == "scientific_recomputation", value], "none; descriptive cage means and the frozen CombZ standardisation reproduced exactly; no model"),
      "scientific_recomputation carries the OPTION3_SPEC wording")
IN <- data.table(input = "x", bytes = 1, sha256 = "s", role = "r")
OUT <- file.path(TMP, "out"); BID <- "fsb_v3_20260930_0123456"
wb <- function(id, out = OUT, gates = GTS, tb = tabs, ...) fsb_write_bundle(tb, out, id, H, IN, gates, FSB_STATUS_DRY, "ebb_x", "czsha", cm, "t", verbatim = VB, ...)
w <- wb(BID)
man <- fread(file.path(w$dir, "00_manifest.csv"))
check(setequal(man$file, fsb_bundle_files()) && setequal(list.files(w$dir), c(man$file, "00_manifest.csv")) && all(s30fb_manifest_check(w$dir, man)$ok),
      "00_manifest lists every other file; bytes / sha256 match")
check(all(file.access(list.files(w$dir, full.names = TRUE), 2L) != 0L), "bundle files are read-only")
reg <- fread(file.path(OUT, "BUNDLE_REGISTRY.csv"), colClasses = "character")
check(nrow(reg) == 1L && identical(names(reg), FSB_REGISTRY_COLS) && identical(reg$manifest_sha256, s30fb_sha(file.path(w$dir, "00_manifest.csv"))), "registry row beside the bundle")
f2back <- fread(file.path(w$dir, "F2_combz_components.csv"))
check(nrow(f2back) == nrow(F2) && isTRUE(all(f2back$signed_z == F2$signed_z, na.rm = TRUE)) && identical(is.na(f2back$signed_z), is.na(F2$signed_z)), "F2 written at full precision")
lg <- fread(w$log, colClasses = "character")
check(nrow(lg) == 2L + 13L && identical(lg$stage[3:15], c(rep("7-write", 12), "8-register")) && all(lg$passed[3:15] == "TRUE") && sum(grepl("the source line", lg$gate)) == 8L,
      "gate log = pre-write + post-write (incl. 8 line-level verbatim gates: P1-P1c, D1, E1, E2-E2c) + registration")
p1w <- readLines(file.path(w$dir, "P1_posthoc_con_contrasts.csv"), encoding = "UTF-8"); p1s <- readLines(VB[["P1_posthoc_con_contrasts"]], encoding = "UTF-8")
check(length(p1w) == 25L && all(startsWith(p1w, paste0(p1s, ","))) && grepl(paste("SUS", mn, "RES"), p1w[7], fixed = TRUE), "written P1 lines = the source lines + provenance cells (UTF-8 labels kept)")
OUT5 <- file.path(TMP, "o5"); tb5 <- tabs; tb5$P1b_posthoc_sensitivity <- copy(tb5$P1b_posthoc_sensitivity)[2, estimate := "9"]
check(grepl("NOT registered", errmsg(wb("fsb_v3_20260930_5555555", out = OUT5, tb = tb5))) && !file.exists(file.path(OUT5, "BUNDLE_REGISTRY.csv")),
      "a P table that is not a verbatim copy fails the post-write gate before the registry row")

check(grepl("immutable", errmsg(wb(BID))), "a second write of the same bundle is refused")
check(grepl("declared set", errmsg(wb("fsb_v3_20260930_1111111", out = file.path(TMP, "o2"), tb = tabs[-1]))), "a missing table is refused")
check(grepl("hard pre-write gate", errmsg(wb("fsb_v3_20260930_1111111", out = file.path(TMP, "o2"), gates = rbind(GTS, s30fb_gate_row("x", "failed", FALSE))))) &&
      !dir.exists(file.path(TMP, "o2")), "a failed hard gate is refused before any write")
OUT3 <- file.path(TMP, "o3"); BID3 <- "fsb_v3_20260930_0fedcba"
tamper <- function(bd) { f <- file.path(bd, "F2_combz_components.csv"); Sys.chmod(f, "0666"); cat("x\n", file = f, append = TRUE) }
check(grepl("NOT registered", errmsg(wb(BID3, out = OUT3, .before_verify = tamper))) && !file.exists(file.path(OUT3, "BUNDLE_REGISTRY.csv")),
      "a post-write failure stops before the registry row")
ok("writer: manifest, read-only, registry, gate log, refusals")

Sys.chmod(list.files(TMP, recursive = TRUE, full.names = TRUE), "0666"); unlink(TMP, recursive = TRUE, force = TRUE)
cat("test_figure_support_bundle.R: all checks passed\n")
