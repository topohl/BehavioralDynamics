# Contract test for the Stage 32 addendum A1 reporting flags (Functions/stage32_reporting.R).
#
# Synthetic tables in memory / tempdir() only: nothing here reads project data or any S: path; the runner is never sourced.
# Checks:
#   1. rule assignment: RF1 (CON-referenced rows of multi-phase models only; not A-exp, not SIS means), RF2 (singular, only as
#      an add-on), RF3 (sex terms; not 'sexavg'), RF4 (C8 constraint), RF5 (H13), RF6 (labels), RF7 (strict), RF8 (C7);
#   2. comparators: frozen Stage 29 row, same-run estimates / sensitivities rows, H13 x6 with CR2 SE / df; a missing comparator
#      is recorded (comparator_found FALSE), never a stop;
#   3. output shape (S32F_COLS), CSV round trip through s32r_prepare + the frozen writer, and no forbidden word in the rule texts,
#      the README block or the run-manifest disclosures.
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat. Run from the repo root:
#   Rscript Testing/tests/test_stage32_reporting.R

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
for (h in c("stage30_figure_bundle.R", "stage32_run.R", "stage32_reporting.R")) source_mmm_helper(h)
fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
errmsg <- function(expr) tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
TMP <- file.path(normalizePath(tempdir(), winslash = "/"), "s32f"); unlink(TMP, recursive = TRUE, force = TRUE); dir.create(TMP)

CON <- "SIS - CON"; CZ <- "direction: behaviour ~ CombZ_wb (within SIS; CombZ centred within batch)"
row <- function(model_id, estimand, direction, estimate = 0.5, df = 100, hyp = NA_character_)
  data.table(hypothesis_id = hyp, model_id = model_id, estimand = estimand, direction = direction, estimate = estimate, se = 0.2, df = df,
             ci_low = estimate - 0.4, ci_high = estimate + 0.4)
est <- rbindlist(list(
  row("A_EXP|crossing_rate", "SIS_minus_CON", CON, -0.64, 4, "H01"),
  row("A_EXP|crossing_rate", "SIS_minus_CON_x_sex", CON, -1.27, 4),
  row("A_CZ|crossing_rate", "CombZ_wb_slope_x_sex", CZ, -4.2, 72),
  row("B_EXP|crossing_rate", "SC_sexavg_TR_CC1", CON, -0.75, 5),
  row("B_EXP|crossing_rate", "DiD_SC_TR_CC1", CON, -0.93, 5),
  row("B_EXP|crossing_rate", "Exposure_x_CC2_x_Sex", CON, -4.16, 22),
  row("C_EXP|crossing_rate", "SIS_minus_CON_phase_slope", CON, 0.14, 108, "H07"),
  row("C_EXP|crossing_rate", "shared_phase_slope", CON, 0.44, 108),
  row("C_EXP|crossing_rate", "SIS_minus_CON_A1_CC1", CON, -0.44, 50),
  row("C_CZ|crossing_rate", "SIS_phase_slope_at_CombZ_wb_0", CZ, 0.51, 90),
  row("C_EXP_PHASExSEX|crossing_rate", "SIS_minus_CON_phase_slope_x_sex", CON, 0.2, 107),
  row("C_CZ_PHASExSEX|crossing_rate", "CombZ_wb_phase_slope_x_sex", CZ, 0.14, 77),
  row("D_EXP|crossing_rate", "CON_mean_A1", CON, 23.2, 63),
  row("D_EXP|crossing_rate", "SIS_mean_A1", CON, 22.7, 154),
  row("D_CZ|crossing_rate", "later_phase_level_CombZ_wb_slope", CZ, -1.1, 90),
  row("E_EXP|light_crossing_rate", "SIS_minus_CON_L1_CC1", CON, 0.92, 146),
  row("H13|Movement_mean", "Movement_mean:sex_c", "CombZ ~ Batch + Movement_mean + Movement_mean:sex_c", -0.307, 9.4, "H13")))
sens <- rbindlist(list(
  row("SENS_C_SEPSLOPE_EXP|crossing_rate", "SIS_minus_CON_phase_slope", CON, 0.084, 27.8),
  row("SENS_C_STRICT_EXP|crossing_rate", "SIS_minus_CON_phase_slope", CON, 0.14, 108),
  row("SENS_C_STRICT_CZ|crossing_rate", "CombZ_wb_phase_slope", CZ, -0.14, 97),
  row("SENS_C_PHASEF_EXP|shared_zone_use", "A1_to_A2_change", CON, -0.019, 1021),
  row("SENS_A_COMMON|crossing_rate", "SIS_minus_CON", CON, -0.68, 20),
  row("SENS_B_COMMON|crossing_rate", "Exposure_x_CC_joint", CON, NA_real_, NA_real_)))
sens[, hypothesis_id := NA]                                       # as read back: an all-empty column is logical
models <- data.table(model_id = unique(c(est$model_id, sens$model_id)))[, singular := model_id %in% c("E_EXP|light_crossing_rate", "SENS_C_PHASEF_EXP|shared_zone_use")]
models[model_id == "H13|Movement_mean", singular := NA]
frozen <- data.table(model_id = "EXPOSURE_CC1|light_phase_crossing_rate", estimand = "SC_sexavg_CC1", estimate = 0.878, se = 0.447, df = 4.0, ci_low = -0.363, ci_high = 2.119)
cont <- data.table(population = c("SIS_ONLY", "ALL_RFID"), predictor = "crossing_rate", estimand = "slope_DiD_F_minus_M", estimate = c(-0.0511, -0.048),
                   se = 0.027, se_cr2 = c(0.0190, 0.0215), df_cr2 = c(9.39, 12.5))
W <- list(estimates = est, sensitivities = sens, models = models)
FL <- s32f_reporting_flags(W, frozen, cont)
has <- function(m, e, code) isTRUE(grepl(code, FL[model_id == m & estimand == e, flags], fixed = TRUE))
absent <- function(m, e) FL[model_id == m & estimand == e, .N] == 0L

# ---------------------------------------------------------------- 1. rules
check(absent("A_EXP|crossing_rate", "SIS_minus_CON") && absent("SENS_A_COMMON|crossing_rate", "SIS_minus_CON"), "RF1 not on single-window CON contrasts (A-exp, A common)")
check(has("C_EXP|crossing_rate", "SIS_minus_CON_phase_slope", "RF1_CON_DF") && has("C_EXP|crossing_rate", "SIS_minus_CON_A1_CC1", "RF1_CON_DF") &&
      has("E_EXP|light_crossing_rate", "SIS_minus_CON_L1_CC1", "RF1_CON_DF") && has("D_EXP|crossing_rate", "CON_mean_A1", "RF1_CON_DF") &&
      has("SENS_C_SEPSLOPE_EXP|crossing_rate", "SIS_minus_CON_phase_slope", "RF1_CON_DF") && has("SENS_C_PHASEF_EXP|shared_zone_use", "A1_to_A2_change", "RF1_CON_DF"),
      "RF1 on CON-referenced rows of C-exp, E-exp, D-exp and the C-exp sensitivities")
check(!has("D_EXP|crossing_rate", "SIS_mean_A1", "RF1_CON_DF") && has("D_EXP|crossing_rate", "SIS_mean_A1", "RF8_C7"), "RF1 not on SIS means; RF8 (C7) on D rows")
check(absent("SENS_B_COMMON|crossing_rate", "Exposure_x_CC_joint"), "joint rows without an estimate are not flagged")
check(has("E_EXP|light_crossing_rate", "SIS_minus_CON_L1_CC1", "RF2_SINGULAR") && !has("C_EXP|crossing_rate", "SIS_minus_CON_A1_CC1", "RF2_SINGULAR"),
      "RF2 marks singular fits among flagged rows only")
check(has("A_EXP|crossing_rate", "SIS_minus_CON_x_sex", "RF3_SEX_BATCH") && has("A_CZ|crossing_rate", "CombZ_wb_slope_x_sex", "RF3_SEX_BATCH") &&
      has("B_EXP|crossing_rate", "DiD_SC_TR_CC1", "RF3_SEX_BATCH") && has("B_EXP|crossing_rate", "Exposure_x_CC2_x_Sex", "RF3_SEX_BATCH") &&
      has("H13|Movement_mean", "Movement_mean:sex_c", "RF3_SEX_BATCH") && absent("B_EXP|crossing_rate", "SC_sexavg_TR_CC1"),
      "RF3 on every sex term (x_sex, x_Sex, :sex_c, DiD), not on sex-averaged rows")
check(has("C_EXP_PHASExSEX|crossing_rate", "SIS_minus_CON_phase_slope_x_sex", "RF4_C8_EXP") && has("C_CZ_PHASExSEX|crossing_rate", "CombZ_wb_phase_slope_x_sex", "RF4_C8_CZ"),
      "RF4 (C8 constraint) on the C secondary three-way sex terms")
check(has("H13|Movement_mean", "Movement_mean:sex_c", "RF5_H13"), "RF5 on H13")
check(identical(FL[estimand == "shared_phase_slope", display_label], "CON/SIS-average phase slope") &&
      identical(FL[estimand == "SIS_phase_slope_at_CombZ_wb_0", display_label], "SIS phase slope at CombZ_wb = 0") &&
      has("C_EXP|crossing_rate", "shared_phase_slope", "RF6_LABEL_AVG"), "RF6 labels")
check(has("SENS_C_STRICT_EXP|crossing_rate", "SIS_minus_CON_phase_slope", "RF7_STRICT") && has("SENS_C_STRICT_CZ|crossing_rate", "CombZ_wb_phase_slope", "RF7_STRICT"),
      "RF7 on the strict sensitivity rows")
check(all(nzchar(FL$flags)) && all(vapply(strsplit(FL$flags, ";"), function(k) all(k %in% names(S32F_RULES)), TRUE)) &&
      all(lengths(strsplit(FL$reporting_rule, " | ", fixed = TRUE)) == lengths(strsplit(FL$flags, ";"))), "every flag code has its rule text")
ok("rule assignment")

# ---------------------------------------------------------------- 2. comparators
e <- FL[model_id == "E_EXP|light_crossing_rate"]
check(isTRUE(e$comparator_found) && identical(e$comparator_estimate, 0.878) && identical(e$comparator_df, 4.0) && grepl("frozen", e$comparator_source), "E: frozen Stage 29 comparator")
a <- FL[model_id == "C_EXP|crossing_rate" & estimand == "SIS_minus_CON_A1_CC1"]
check(isTRUE(a$comparator_found) && identical(a$comparator_estimate, -0.64) && identical(a$comparator_model_id, "A_EXP|crossing_rate"), "C-exp A1: A-exp comparator from estimates")
s <- FL[model_id == "C_EXP|crossing_rate" & estimand == "SIS_minus_CON_phase_slope"]
check(isTRUE(s$comparator_found) && identical(s$comparator_estimate, 0.084) && identical(s$comparator_df, 27.8), "H07: separate-variance comparator from sensitivities")
h <- FL[model_id == "H13|Movement_mean"]
check(isTRUE(h$comparator_found) && isTRUE(all.equal(h$comparator_estimate, 6 * -0.0511)) && isTRUE(all.equal(h$comparator_se, 6 * 0.0190)) &&
      identical(h$comparator_df, 9.39) && isTRUE(all.equal(h$comparator_ci_low, 6 * (-0.0511 - qt(0.975, 9.39) * 0.0190))), "H13: Stage 29 SIS_ONLY x6 with CR2 SE and df")
FL2 <- s32f_reporting_flags(list(estimates = est[model_id != "A_EXP|crossing_rate"], sensitivities = sens, models = models), frozen[0], cont[population != "SIS_ONLY"])
check(identical(FL2[model_id == "C_EXP|crossing_rate" & estimand == "SIS_minus_CON_A1_CC1", comparator_found], FALSE) &&
      identical(FL2[model_id == "E_EXP|light_crossing_rate", comparator_found], FALSE) && identical(FL2[model_id == "H13|Movement_mean", comparator_found], FALSE) &&
      all(is.na(FL2[comparator_found == FALSE, comparator_estimate])), "a missing comparator is recorded, not a stop")
check(grepl("lacks", errmsg(s32f_reporting_flags(list(estimates = est[, !"direction"], sensitivities = sens, models = models), frozen, cont))), "missing input columns stop")
ok("comparators")

# ---------------------------------------------------------------- 3. shape, round trip, wording
check(identical(names(FL), S32F_COLS), "columns = S32F_COLS")
check(identical(FL$source_table, c(rep("estimates", sum(FL$source_table == "estimates")), rep("sensitivities", sum(FL$source_table == "sensitivities")))),
      "row order follows the tables")
p <- file.path(TMP, "reporting_flags.csv"); s30fb_write_csv(s32r_prepare(FL), p)
back <- fread(p, na.strings = "")
check(nrow(back) == nrow(FL) && identical(back$flags, FL$flags), "flags round-trip through s32r_prepare and the frozen writer")
check(!length(s32r_forbidden(list(flags = FL, rules = data.table(r = S32F_RULES)), c(S32F_README_A1, S32F_EXECUTION_HISTORY, S32F_REGISTRY_NOTE))),
      "no forbidden word in the rule texts, the README block or the run-manifest disclosures")
check(!any(grepl("\"", c(S32F_RULES, S32F_README_A1, S32F_EXECUTION_HISTORY, S32F_REGISTRY_NOTE), fixed = TRUE)), "no double quote in the disclosure texts")
ok("shape, round trip and wording")
cat("test_stage32_reporting: all checks passed\n")
