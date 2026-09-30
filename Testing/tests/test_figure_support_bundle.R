# Contract test for the figure-support bundle helpers (Functions/figure_support_bundle.R).
#
# Synthetic data in memory / tempdir() only: nothing here reads project data, a label list, an outcome table or any S:
# path, and no Analysis/ runner is sourced or executed (runner and producer files are only read as text / parsed).
# Checks:
#   1. static: no model / test / adjustment call in the helper or the runner; the runner parses its mode before any read,
#      sources no Analysis/ file other than _pipeline_setup.R and never sources or evaluates the CombZ producer file; the
#      helper parses (never sources) it; this test sources only Analysis/_pipeline_setup.R;
#   2. run mode and bundle id fsb_v1_<YYYYMMDD>_<commit7>;
#   3. frozen CombZ definition parser: constants and composite rule read without executing the file; refusal of a missing,
#      duplicated or non-literal constant and of a missing / non-consecutive rule; the real producer source yields the
#      six components and three inversions;
#   4. frozen composite rule: mean of the present components, NA when none;
#   5. F2: signed_z = stored value, z = direction x signed_z, present flags, heatmap order, threshold flag, the 1e-12
#      recomposition gate and the n_components_present gate;
#   6. F2b: directions, reasons, relationship text, banned-wording guard;
#   7. F1 / F1b: hand-computed cage means, the mean-of-cage-means = B2 gate, cage count / size / batch gates, mixed-cage and
#      composition checks, NA refusal;
#   8. writer: manifest, provenance keys, read-only files, post-write gates before the registry row, gate log, refusal to
#      overwrite.
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
check(identical(bid, "fsb_v1_20260930_0123456") && grepl("^fsb_v[0-9]+_[0-9]{8}_[0-9a-f]{7}$", bid), "bundle id fsb_v1_<YYYYMMDD>_<commit7>")
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

# ---------------------------------------------------------------- 8. writer
tabs <- list(F1_con_cage_means = F1, F1b_con_reference_means = F1b, F2_combz_components = F2, F2b_combz_definition = F2b)
GTS <- rbind(s30fb_gate_row("0-code", "synthetic pre-write gate", TRUE, "c"), s30fb_gate_row("0-code", "soft gate", FALSE, "not met", hard = FALSE))
pf <- setNames(as.list(paste0("v_", FSB_PROVENANCE_KEYS)), FSB_PROVENANCE_KEYS)
pf$descriptive_computations <- FSB_DESCRIPTIVE_COMPUTATIONS; pf$scientific_recomputation <- FSB_SCIENTIFIC_RECOMPUTATION; pf$pre_write_gates_passed <- fsb_gate_count(GTS)
H <- fsb_provenance(pf)
check(grepl("lacks key", errmsg(fsb_provenance(pf[-1]))), "a missing provenance key stops")
check(startsWith(H[key == "scientific_recomputation", value], "none; descriptive cage means and the frozen CombZ standardisation reproduced exactly; no model"),
      "scientific_recomputation carries the OPTION3_SPEC wording")
IN <- data.table(input = "x", bytes = 1, sha256 = "s", role = "r")
OUT <- file.path(TMP, "out"); BID <- "fsb_v1_20260930_0123456"
wb <- function(id, out = OUT, gates = GTS, tb = tabs, ...) fsb_write_bundle(tb, out, id, H, IN, gates, FSB_STATUS_DRY, "ebb_x", "czsha", cm, "t", ...)
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
check(nrow(lg) == 2L + 5L && identical(lg$stage[3:7], c(rep("7-write", 4), "8-register")) && all(lg$passed[3:7] == "TRUE"), "gate log = pre-write + post-write + registration")
check(grepl("immutable", errmsg(wb(BID))), "a second write of the same bundle is refused")
check(grepl("declared set", errmsg(wb("fsb_v1_20260930_1111111", out = file.path(TMP, "o2"), tb = tabs[-1]))), "a missing table is refused")
check(grepl("hard pre-write gate", errmsg(wb("fsb_v1_20260930_1111111", out = file.path(TMP, "o2"), gates = rbind(GTS, s30fb_gate_row("x", "failed", FALSE))))) &&
      !dir.exists(file.path(TMP, "o2")), "a failed hard gate is refused before any write")
OUT3 <- file.path(TMP, "o3"); BID3 <- "fsb_v1_20260930_0fedcba"
tamper <- function(bd) { f <- file.path(bd, "F2_combz_components.csv"); Sys.chmod(f, "0666"); cat("x\n", file = f, append = TRUE) }
check(grepl("NOT registered", errmsg(wb(BID3, out = OUT3, .before_verify = tamper))) && !file.exists(file.path(OUT3, "BUNDLE_REGISTRY.csv")),
      "a post-write failure stops before the registry row")
ok("writer: manifest, read-only, registry, gate log, refusals")

Sys.chmod(list.files(TMP, recursive = TRUE, full.names = TRUE), "0666"); unlink(TMP, recursive = TRUE, force = TRUE)
cat("test_figure_support_bundle.R: all checks passed\n")
