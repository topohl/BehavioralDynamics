# Model-to-figure consistency tests for the GAMM manuscript outputs.
#
# These verify that the Stage 26 publication tree is a faithful, traceable view
# of the frozen Stage 20-25 analysis tables: same numbers, same q values, same
# palette, no invalidated inputs, and nothing recomputed during assembly.

suppressPackageStartupMessages({ library(dplyr); library(readr); library(stringr); library(lubridate) })

source("Analysis/_pipeline_setup.R")
source_mmm_helper("mmm_publication_theme.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

ROOT <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
P <- file.path(ROOT, "analysis_ready", "pipeline")
M <- file.path(P, "26_gamm_manuscript_outputs", "10min")
sd_dir <- file.path(M, "source_data")
rdc <- function(...) read_csv(file.path(...), show_col_types = FALSE, progress = FALSE)

check(dir.exists(M), "Stage 26 output tree must exist")

# ---------------------------------------------------- 1. panels trace to source
cat("1. every panel traces to a canonical analysis table\n")
fman <- rdc(M, "manifests", "figure_manifest.csv")
sman <- rdc(sd_dir, "source_data_manifest.csv")
check(nrow(fman) > 0, "figure manifest must be non-empty")
check(all(fman$stem %in% str_remove(sman$figure_file, "[.]pdf$")),
      "every figure must appear in the source-data manifest")
for (f in sman$source_data_file) {
  check(file.exists(file.path(sd_dir, f)), paste0("source data file must exist: ", f))
}
for (t in unique(sman$source_analysis_table)) {
  parts <- str_split_fixed(t, "/", 3)
  p <- file.path(P, list.files(P, pattern = paste0("^", parts[1], "_")), "10min",
                 parts[2], parts[3])
  check(any(file.exists(p)), paste0("canonical analysis table must exist: ", t))
}
ok(sprintf("%d figures, %d source tables, all analysis tables resolve", nrow(fman), nrow(sman)))

# --------------------------------------- 2 & 12. no invalidated Gaussian inputs
cat("\n2/12. invalidated Gaussian Inactive outputs are excluded\n")
inv <- list.files(P, pattern = "gaussian_log1p_invalid", recursive = TRUE,
                  include.dirs = TRUE, full.names = TRUE)
check(length(inv) > 0, "the quarantine directories must still exist as provenance")
for (f in list.files(sd_dir, pattern = "[.]csv$", full.names = TRUE)) {
  x <- rdc(f)
  if ("inference_status" %in% names(x))
    check(!any(x$inference_status == "MODEL_INADEQUATE__DO_NOT_INTERPRET", na.rm = TRUE),
          paste0("source data must not carry an invalid status: ", basename(f)))
}
for (f in c(list.files(file.path(M, "tables", "manuscript"), full.names = TRUE),
            list.files(file.path(M, "tables", "extended_data"), full.names = TRUE))) {
  txt <- paste(readLines(f, warn = FALSE), collapse = " ")
  check(!grepl("MODEL_INADEQUATE", txt, fixed = TRUE),
        paste0("manuscript table must not contain an invalid marker: ", basename(f)))
}
check(!any(grepl("gaussian_log1p_invalid", sman$source_analysis_table, fixed = TRUE)),
      "no manuscript panel may source a quarantined table")
ok("quarantine preserved; no invalid value reached any manuscript artefact")

# ------------------------------------------------ 3 & 4. fixed palette, both phases
cat("\n3/4. group colours are exactly the fixed manuscript palette\n")
expected <- c(CON = "#3E3C6F", RES = "#C6C3BB", SUS = "#E63A48")
check(identical(MMM_GROUP_COLOURS[c("CON","RES","SUS")], expected),
      paste0("palette constant must be exact; got ",
             paste(MMM_GROUP_COLOURS, collapse = ", ")))
svgs <- c(list.files(file.path(M, "figures", "main_candidates"), "[.]svg$", full.names = TRUE),
          list.files(file.path(M, "figures", "extended_data"), "[.]svg$", full.names = TRUE))
check(length(svgs) > 0, "there must be exported SVGs to inspect")
grp_svgs <- svgs[grepl("trajectory|probability|auc_by_cagechange", basename(svgs))]
for (f in grp_svgs) {
  txt <- tolower(paste(readLines(f, warn = FALSE), collapse = ""))
  for (col in tolower(expected)) {
    check(grepl(col, txt, fixed = TRUE),
          paste0("group figure must use ", col, ": ", basename(f)))
  }
}
ok(sprintf("all %d group-coloured figures use the exact fixed palette (Active and Inactive)",
           length(grp_svgs)))

# ------------------------------------------------ 5. nothing recomputed in Stage 26
cat("\n5. Stage 26 recomputes no statistics\n")
src <- readLines("Analysis/26_build_gamm_manuscript_outputs.R", warn = FALSE)
code <- src[!grepl("^\\s*#", src)]
banned <- c("\\bbam\\(", "\\bgam\\(", "\\bglm\\(", "\\blm\\(", "p\\.adjust\\(",
            "vcov\\.gam\\(", "mvrnorm\\(", "mmm_fit_gamm_ar1\\(", "mmm_numeric_delta\\(",
            "mmm_markov_sim_group\\(", "pnorm\\(")
for (b in banned) {
  hits <- grep(b, code, value = TRUE)
  check(length(hits) == 0,
        paste0("Stage 26 must not call ", b, " (found: ", paste(head(hits, 1), collapse = ""), ")"))
}
ok("no model fitting, no p.adjust, no simulation, no distribution calls in Stage 26")

# --------------------------------- 6. source data equals the analysis table exactly
cat("\n6. source data equals its analysis table exactly\n")
s20 <- rdc(P, "20_first_night_gamm", "10min", "tables", "first_active_primary_contrasts.csv")
sB <- rdc(sd_dir, "source_first_active_auc_contrasts.csv")
j <- inner_join(s20 %>% select(Sex, contrast, ref = AUC_diff_log1p, refq = p_bh),
                sB %>% select(Sex, contrast, src = AUC_diff_log1p, srcq = p_bh),
                by = c("Sex", "contrast"))
check(nrow(j) == 6L, "all six first-Active contrasts must join")
check(isTRUE(all.equal(j$ref, j$src, tolerance = 0)), "estimates must match at tolerance 0")
check(isTRUE(all.equal(j$refq, j$srcq, tolerance = 0)), "q values must match at tolerance 0")
s22 <- rdc(P, "22_repeated_cagechange_acute_gamm", "10min", "tables",
           "allcc_active_gamm_group_auc.csv")
sC <- rdc(sd_dir, "source_active_auc_by_cagechange.csv")
j2 <- inner_join(s22 %>% select(Sex, Group, stratum, ref = AUC_log1p),
                 sC %>% select(Sex, Group, stratum, src = AUC_log1p),
                 by = c("Sex", "Group", "stratum"))
check(nrow(j2) == nrow(s22), "all group-AUC rows must join")
check(isTRUE(all.equal(j2$ref, j2$src, tolerance = 0)), "group AUC must match at tolerance 0")
ok(sprintf("source data reproduces %d analysis values at tolerance 0", nrow(j) + nrow(j2)))

# ------------------------------- 7. manuscript q values match the family registry
cat("\n7. manuscript q values match their multiplicity registry\n")
mk <- rdc(M, "tables", "manuscript", "manuscript_active_key_results.csv")
reg <- rdc(P, "22_repeated_cagechange_acute_gamm", "10min", "tables",
           "allcc_active_adaptation_multiplicity_registry.csv")
fmtp <- function(p) ifelse(is.na(p), NA_character_,
  ifelse(p < 1e-3, formatC(p, format = "e", digits = 2), formatC(p, format = "g", digits = 3)))
ov <- mk %>% filter(grepl("^Overall", .data$`Biological comparison`))
rv <- reg %>% filter(.data$family_key == "A_OVERALL")
check(nrow(ov) == nrow(rv), "overall-adaptation rows must correspond")
check(setequal(ov$`BH q`, fmtp(rv$q_BH_family)),
      "manuscript overall-adaptation q values must equal the A_OVERALL family q values")
n20 <- rdc(P, "20_first_night_gamm", "10min", "tables", "first_active_primary_contrasts.csv")
fn <- mk %>% filter(grepl("First Active", .data$Analysis))
check(setequal(fn$`BH q`, fmtp(n20$p_bh)),
      "manuscript first-Active q values must equal the six-test family q values")
check(all(!is.na(mk$`Multiplicity family`)), "every manuscript row must name its family")
ok("all manuscript q values trace to their registry")

# -------------------- 8/9/10. upstream parity contracts still hold
cat("\n8/9/10. upstream parity contracts\n")
for (t in c("Testing/tests/test_stage09_primary_window.R",
            "Testing/tests/test_first_night_window_parity.R",
            "Testing/tests/test_acute_active_window_parity.R",
            "Testing/tests/test_acute_phase_window_parity.R")) {
  r <- system2("Rscript", t, stdout = TRUE, stderr = TRUE)
  check(!any(grepl("FAIL|^Error|^Fehler", r)), paste0("must still pass: ", basename(t)))
  ok(paste0("passes: ", basename(t)))
}
i23 <- rdc(P, "23_first_inactive_gamm", "10min", "tables", "first_inactive_window_qc.csv")
st <- unique(format(as.POSIXct(i23$scheduled_block_start, tz = "UTC"), "%H:%M"))
en <- unique(format(as.POSIXct(i23$scheduled_block_end, tz = "UTC"), "%H:%M"))
check(identical(st, "06:30"), paste0("Inactive window must start 06:30; got ", paste(st, collapse = ",")))
check(identical(en, "18:30"), paste0("Inactive window must end 18:30; got ", paste(en, collapse = ",")))
ok("first Inactive selector is exactly 06:30 to 18:30")

# ------------------------------------- 11. no no-AR1 estimate is used as primary
cat("\n11. no manuscript figure uses a no-AR1 Active estimate\n")
for (f in c("source_first_active_trajectory.csv", "source_first_active_auc_contrasts.csv",
            "source_active_auc_by_cagechange.csv", "source_active_cc4_minus_cc1.csv")) {
  x <- rdc(sd_dir, f)
  if ("variant" %in% names(x))
    check(all(x$variant == "primary"),
          paste0("only the primary variant may be plotted: ", f))
}
spec <- rdc(P, "20_first_night_gamm", "10min", "tables", "first_active_model_specification.csv")
pr <- spec %>% filter(.data$variant == "primary")
check(all(pr$ar1_applied), "the primary Active model must have AR1 applied")
ciw <- rdc(P, "20_first_night_gamm", "10min", "tables", "first_active_ci_width_diagnostics.csv")
check(all(ciw$variant[grepl("no_ar1", ciw$variant)] |> length() > 0),
      "the no-AR1 variant must exist as a labelled diagnostic")
check(all(grepl("DIAGNOSTIC ONLY", ciw$note[grepl("no_ar1", ciw$variant)])),
      "the no-AR1 variant must be labelled DIAGNOSTIC ONLY")
ok("primary Active estimates are AR1; no-AR1 exists only as a labelled diagnostic")

cat("\nPASS: GAMM manuscript consistency\n")
