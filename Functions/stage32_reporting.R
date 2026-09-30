# ================================================================
# Stage 32: registry addendum A1 reporting flags (Exp9 SIS RFID)
# MMMSociability -- Functions/stage32_reporting.R
# ================================================================
# Pure constants and functions for Analysis/32_behavior_exposure_adaptation.R implementing section C of the FROZEN
# docs/STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md: the machine-readable reporting flags (audit/reporting_flags.csv), derived from
# the written tables/estimates.csv, tables/sensitivities.csv and tables/models.csv (read back from the local staging folder)
# and two frozen Stage 29 v1.0.1 release tables (comparators). Nothing here fits, tests or changes a number: the flags only
# restrict how a row may be described. Sourcing this file reads and writes nothing.
# Requires: data.table.
# ================================================================

# rule texts (addendum A1 section C); codes are joined with ";" in the flags column
S32F_RULES <- c(
  RF1_CON_DF = paste("CON-referenced row from a multi-phase model (C-exp, a C-exp secondary, D-exp, E-exp or a C-exp sensitivity): the KR df is",
                     "not bounded by the 6 intact CON groups (the 24 CON cage episodes, 6 groups x 4 CCs, enter as separate clusters), so the",
                     "interval is optimistic; show it next to its comparator where one is given; not evidence of an exposure effect (H05-H08 are null)"),
  RF2_SINGULAR = "singular fit (kept and flagged, registry section 3)",
  RF3_SEX_BATCH = paste("sex term: sex is nested in batch (females B3/B4/B6, males B1/B2/B5), so any sex contrast compares 3 with 3 batches and is",
                        "batch-confounded; estimation only; no sex difference is inferred"),
  RF4_C8_EXP = paste("g_SIS:phase:sex_c fitted without phase:sex_c (registry section 4 wording, convention C8): the CON and SIS sex differences",
                     "in phase slope are forced to be equal and opposite, so this is not a clean Exposure x phase x Sex contrast; do not interpret"),
  RF4_C8_CZ = paste("CombZ_wb:phase:sex_c fitted without phase:sex_c (convention C8): the sex difference in the phase slope at CombZ_wb = 0 is",
                    "forced to 0; less affected because CombZ_wb has mean 0 within each batch and hence within each sex; estimation only"),
  RF5_H13 = paste("post hoc restatement of an estimate stored and seen before registration (Stage 29 v1.0.1 CONTINUOUS SIS_ONLY crossing_rate",
                  "slope_DiD_F_minus_M, x6 to the Movement_mean scale; Movement_mean is about crossing_rate / 6): not new information and not a",
                  "discovery claim; batch-confounded (sex nested in batch; 24 CC1 cages, CR2 df about 9.4); unadjusted"),
  RF6_LABEL_AVG = paste("label: CON/SIS-average phase slope (the phase coefficient under +-1/2 g_SIS coding is the unweighted average of the CON and",
                        "SIS phase slopes, not a slope shared by CON and SIS); a phase pattern without a supporting Exposure interaction is not",
                        "adaptation to social instability (registry section 9; H07 / H08 null)"),
  RF6_LABEL_SIS = paste("label: SIS phase slope at CombZ_wb = 0 (within SIS, no CON reference); not attributable to social instability",
                        "(registry section 9)"),
  RF7_STRICT = paste("strict completeness (tolerance 0) removed 0 animal-phases under the registered raw-record board interval, so this row",
                     "equals the primary: vacuous, not a robustness check (addendum A1 C1; registry section 2 inconsistency disclosed)"),
  RF8_C7 = paste("categorical-phase model whose random part keeps only the numeric-phase terms (convention C7): non-linear cage-level",
                 "deviations enter the residual, so the df of phase-specific contrasts is inflated"))
S32F_DISPLAY <- c(shared_phase_slope = "CON/SIS-average phase slope", SIS_phase_slope_at_CombZ_wb_0 = "SIS phase slope at CombZ_wb = 0")
# explicit comparators (addendum A1 C3): row -> comparator row
S32F_COMPARATORS <- data.table::data.table(
  model_id = c("E_EXP|light_crossing_rate", "C_EXP|crossing_rate", "C_EXP|shared_zone_use", "C_EXP|crossing_rate", "C_EXP|shared_zone_use"),
  estimand = c("SIS_minus_CON_L1_CC1", "SIS_minus_CON_A1_CC1", "SIS_minus_CON_A1_CC1", "SIS_minus_CON_phase_slope", "SIS_minus_CON_phase_slope"),
  comparator_source = c("stage29_exposure_estimates (frozen Stage 29 v1.0.1)", "estimates", "estimates", "sensitivities", "sensitivities"),
  comparator_model_id = c("EXPOSURE_CC1|light_phase_crossing_rate", "A_EXP|crossing_rate", "A_EXP|shared_zone_use", "SENS_C_SEPSLOPE_EXP|crossing_rate",
                          "SENS_C_SEPSLOPE_EXP|shared_zone_use"),
  comparator_estimand = c("SC_sexavg_CC1", "SIS_minus_CON", "SIS_minus_CON", "SIS_minus_CON_phase_slope", "SIS_minus_CON_phase_slope"),
  comparator_note = c("same window (light phase after the first active phase after CC1) in the frozen single-window EXPOSURE_CC1 model: separate CON / SIS cage variances, one row per animal",
                      "A-exp (H01): the single-window model of the same A1-of-CC1 contrast",
                      "A-exp (H02): the single-window model of the same A1-of-CC1 contrast",
                      "registry section 7 sensitivity: separate CON / SIS cage phase-slope variances",
                      "registry section 7 sensitivity: separate CON / SIS cage phase-slope variances"))
S32F_H13_REF <- list(population = "SIS_ONLY", predictor = "crossing_rate", estimand = "slope_DiD_F_minus_M", scale = 6)
S32F_COLS <- c("source_table", "model_id", "estimand", "hypothesis_id", "display_label", "estimate", "se", "df", "ci_low", "ci_high", "singular",
               "flags", "reporting_rule", "comparator_source", "comparator_model_id", "comparator_estimand", "comparator_found", "comparator_estimate",
               "comparator_se", "comparator_df", "comparator_ci_low", "comparator_ci_high", "comparator_note")

# disclosures (addendum A1 sections A, B and C) for audit/run_manifest.csv and README.txt
S32F_EXECUTION_HISTORY <- paste(
  "first REAL execution: commit 4f9c016, 2026-09-30 23:21-23:23; every gate before and after fitting passed and all 65 models fitted, then the",
  "writer stopped (Round-trip text differs in gate_results.csv column detail: the strict-variant gate detail ended with a trailing space, which",
  "data.table::fread strips); it left .tmp_v1.0_4f9c016 with the 12 tables, audit/input_hashes.csv and audit/gate_results.csv, which stays on S:",
  "unmodified and read-only as evidence (audit/aborted_run_evidence.csv) and was never a registered output; its tables were read (implementer",
  "report, independent review) before this execution. This v1.1 folder is the second execution of the registered fits (registry addendum A1),",
  "with the write step corrected; its 12 tables are byte-identical to the aborted staging tables (audit/determinism_gate.csv), so no number",
  "differs from what had been seen.")
S32F_REGISTRY_NOTE <- paste(
  "registry v1.0 section 2 says the strict sensitivity removes every A4 whose recording ends before 06:30; that describes the preprocessed-file",
  "ends (all 333 used A4 rows at CC1-CC3 end 0.2-66.7 s before 06:30), not the registered raw-record board interval, under which boards run",
  "3.49-4.78 h past every A4 end (CC4 A2: 7.51 h), so the 10-min tolerance and the strict rule never bind in the clean set and the strict",
  "sensitivity is vacuous (0 rows removed). Not changed (a change would be a deviation); disclosed (addendum A1 C1). Cosmetic: the hypothesis",
  "table header reads Estimand_test where the registry table reads 'Estimand / test' (all 13 x 10 cells identical).")
S32F_README_A1 <- c(
  "Execution history (registry addendum A1, docs/STAGE32_REGISTRY_v1.0_ADDENDUM_A1.md, FROZEN before this execution):",
  "  * The first REAL execution (commit 4f9c016, 2026-09-30 23:21-23:23) passed every gate before and after fitting and fitted all 65 models,",
  "    then stopped inside the writer: one gate detail ended with a trailing space ('0 removed; '), data.table::fread strips it, and the frozen",
  "    CSV round-trip check refused audit/gate_results.csv. Its staging folder ../.tmp_v1.0_4f9c016 (12 tables, input_hashes, gate_results; no",
  "    run manifest, README, plots or output manifest) stays unmodified and read-only as evidence and keeps every v1.0 run name blocked; it is",
  "    not a registered output. Its tables were read before this execution.",
  "  * This folder (v1.1) is the second execution of the registered fits, with the write step corrected (local staging and round-trip checks",
  "    before any S: write; no trailing separator; whitespace trimmed). The numeric Stage 32 code is unchanged since 4f9c016 (git blobs), and",
  "    the 12 tables are byte-identical (sha256) to the aborted staging tables (audit/determinism_gate.csv): no number differs from what had",
  "    been seen, and no analytic choice was exercised by the second execution.",
  "",
  "Reporting rules (addendum A1 section C; binding for any text, figure or bundle; row-level flags in audit/reporting_flags.csv):",
  "  * Strict completeness (H07-H10 sensitivity) is vacuous: under the registered raw-record board interval it removed 0 animal-phases, so",
  "    SENS_C_STRICT_* equal the primary. It is not a robustness check that passed. The registry's expectation (every A4 ending before 06:30",
  "    removed) describes preprocessed-file ends, not the registered rule: a disclosed registry inconsistency, not changed.",
  "  * H13 is a post hoc restatement, not new information: the Stage 29 v1.0.1 CONTINUOUS SIS_ONLY crossing_rate F - M slope difference,",
  "    stored and seen before registration, gives -0.3069 (CR2 SE 0.1143, df 9.39) on the Movement_mean scale (x6), numerically near-identical",
  "    to H13; sex is nested in batch (3 v 3 batches), so H13 is batch-confounded and unadjusted; never presented as a finding.",
  "  * CON-referenced intervals from the multi-phase models (C-exp and its secondaries, D-exp, E-exp, C-exp sensitivities) have KR df that",
  "    are not bounded by the 6 intact CON groups (for example E-exp SIS - CON at L1 of CC1, KR df 146, singular; the frozen single-window",
  "    EXPOSURE_CC1 value for the same window is 0.878 [-0.363, 2.119], KR df 4.0; H07 KR df 108 against 27.8 with separate CON / SIS",
  "    phase-slope variances, 0.084 [-1.04, 1.21]). Such rows are shown next to their comparator and flagged; no phase-specific or light-phase",
  "    SIS - CON interval is evidence of an exposure effect (H05-H08 are null).",
  "  * C secondary g_SIS:phase:sex_c is fitted without phase:sex_c (convention C8): not a clean Exposure x phase x Sex contrast; not interpreted.",
  "  * Every sex term compares 3 female with 3 male batches (sex nested in batch): batch-confounded, estimation only.",
  "  * shared_phase_slope is the CON/SIS-average phase slope (convention C17); a phase pattern without a supporting Exposure interaction is not",
  "    adaptation to social instability (registry section 9).",
  "  * Convention C13 wording corrected (a centring shift would change CombZ_wb:phase and CombZ_wb:CC; no effect here, since every C-cz, D-cz",
  "    and E-cz model uses all 87 SIS animals, the centring set).")

#' The three written tables the flags are derived from, read back from a (local) run folder exactly as written.
s32f_read_written <- function(run_dir) {
  rd <- function(k) data.table::fread(file.path(run_dir, "tables", paste0(k, ".csv")), na.strings = "", encoding = "UTF-8")
  list(estimates = rd("estimates"), sensitivities = rd("sensitivities"), models = rd("models"))
}

#' Reporting flags (addendum A1 section C). `written` = s32f_read_written(); `frozen29` = the Stage 29 v1.0.1
#' exposure_estimates.csv; `cont29` = its continuous_estimates.csv. Never stops on a missing comparator (comparator_found FALSE).
s32f_reporting_flags <- function(written, frozen29, cont29) {
  cols <- c("model_id", "estimand", "hypothesis_id", "direction", "estimate", "se", "df", "ci_low", "ci_high")
  pick <- function(x, src) { x <- data.table::as.data.table(x)
    data.table::data.table(source_table = src, model_id = as.character(x$model_id), estimand = as.character(x$estimand),
                           hypothesis_id = as.character(x$hypothesis_id), direction = as.character(x$direction), estimate = as.numeric(x$estimate),
                           se = as.numeric(x$se), df = as.numeric(x$df), ci_low = as.numeric(x$ci_low), ci_high = as.numeric(x$ci_high)) }
  for (k in c("estimates", "sensitivities")) { miss <- setdiff(cols, names(written[[k]])); if (length(miss)) stop(k, " lacks ", paste(miss, collapse = ","), call. = FALSE) }
  R <- data.table::rbindlist(list(pick(written$estimates, "estimates"), pick(written$sensitivities, "sensitivities")))
  R[, row_order := .I]
  M <- data.table::as.data.table(written$models)
  R[, singular := as.logical(M$singular)[match(model_id, as.character(M$model_id))]]
  fam <- sub("[|].*$", "", R$model_id)
  con_ref <- R$direction %in% "SIS - CON" & !grepl("^SIS_mean_", R$estimand) & !is.na(R$estimate)
  multi <- grepl("^(C_EXP|D_EXP|E_EXP)", fam) | grepl("^SENS_C_.*_EXP$", fam)
  FL <- list(RF1_CON_DF = con_ref & multi,
            RF3_SEX_BATCH = grepl("(x_sex|x_Sex|:sex_c)$|^DiD", R$estimand),
            RF4_C8_EXP = fam == "C_EXP_PHASExSEX", RF4_C8_CZ = fam == "C_CZ_PHASExSEX",
            RF5_H13 = R$model_id %in% "H13|Movement_mean",
            RF6_LABEL_AVG = R$estimand %in% "shared_phase_slope", RF6_LABEL_SIS = R$estimand %in% "SIS_phase_slope_at_CombZ_wb_0",
            RF7_STRICT = grepl("^SENS_C_STRICT_", fam),
            RF8_C7 = grepl("^(D_EXP|D_CZ|SENS_C_PHASEF_EXP|SENS_C_PHASEF_CZ)$", fam) & !is.na(R$estimate))
  any_flag <- Reduce(`|`, FL)
  FL$RF2_SINGULAR <- any_flag & R$singular %in% TRUE
  order_codes <- names(S32F_RULES)
  codes <- vapply(seq_len(nrow(R)), function(i) paste(order_codes[vapply(order_codes, function(k) isTRUE(FL[[k]][i]), TRUE)], collapse = ";"), "")
  R[, flags := codes]
  R <- R[any_flag]
  R[, reporting_rule := vapply(strsplit(flags, ";", fixed = TRUE), function(k) paste(S32F_RULES[k], collapse = " | "), "")]
  R[, display_label := unname(S32F_DISPLAY[estimand])]
  # comparators
  R[, `:=`(comparator_source = NA_character_, comparator_model_id = NA_character_, comparator_estimand = NA_character_, comparator_found = NA,
           comparator_estimate = NA_real_, comparator_se = NA_real_, comparator_df = NA_real_, comparator_ci_low = NA_real_, comparator_ci_high = NA_real_,
           comparator_note = NA_character_)]
  all_rows <- data.table::rbindlist(list(pick(written$estimates, "estimates"), pick(written$sensitivities, "sensitivities")))
  fz <- data.table::as.data.table(frozen29)
  for (i in seq_len(nrow(S32F_COMPARATORS))) {
    cp <- S32F_COMPARATORS[i]; hit <- which(R$model_id == cp$model_id & R$estimand == cp$estimand)
    if (!length(hit)) next
    ref <- if (grepl("^stage29", cp$comparator_source)) fz[as.character(model_id) == cp$comparator_model_id & as.character(estimand) == cp$comparator_estimand]
           else all_rows[source_table == cp$comparator_source & model_id == cp$comparator_model_id & estimand == cp$comparator_estimand]
    found <- nrow(ref) == 1L
    data.table::set(R, i = hit, j = c("comparator_source", "comparator_model_id", "comparator_estimand", "comparator_found", "comparator_note"),
                    value = list(cp$comparator_source, cp$comparator_model_id, cp$comparator_estimand, found, cp$comparator_note))
    if (found) data.table::set(R, i = hit, j = c("comparator_estimate", "comparator_se", "comparator_df", "comparator_ci_low", "comparator_ci_high"),
                               value = list(as.numeric(ref$estimate), as.numeric(ref$se), as.numeric(ref$df), as.numeric(ref$ci_low), as.numeric(ref$ci_high)))
  }
  h <- which(R$model_id == "H13|Movement_mean")
  if (length(h)) {
    c29 <- data.table::as.data.table(cont29)
    ref <- c29[population == S32F_H13_REF$population & predictor == S32F_H13_REF$predictor & estimand == S32F_H13_REF$estimand]
    found <- nrow(ref) == 1L && all(is.finite(c(ref$estimate, ref$se_cr2, ref$df_cr2)))
    data.table::set(R, i = h, j = c("comparator_source", "comparator_model_id", "comparator_estimand", "comparator_found", "comparator_note"),
                    value = list("stage29_continuous_estimates (frozen Stage 29 v1.0.1)", paste("CONTINUOUS", S32F_H13_REF$population, S32F_H13_REF$predictor),
                                 S32F_H13_REF$estimand, found,
                                 "x6 to the Movement_mean scale; CR2 SE and CR2 Satterthwaite df as stored (se_cr2, df_cr2); CI = estimate +- t(0.975, df_cr2) x SE"))
    if (found) { s <- S32F_H13_REF$scale; q <- stats::qt(0.975, ref$df_cr2)
      data.table::set(R, i = h, j = c("comparator_estimate", "comparator_se", "comparator_df", "comparator_ci_low", "comparator_ci_high"),
                      value = list(s * ref$estimate, s * ref$se_cr2, ref$df_cr2, s * (ref$estimate - q * ref$se_cr2), s * (ref$estimate + q * ref$se_cr2))) }
  }
  data.table::setorder(R, row_order)
  R[, c("row_order", "direction") := NULL]
  data.table::setcolorder(R, S32F_COLS)
  R[]
}
