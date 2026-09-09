# Contract tests for the canonical later composite stress-burden score (CombZ).
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), sourced from the
# repository root, no testthat, no package beyond the CI set.
#
# PORTABLE checks run everywhere and guard the SOURCE CODE: that the canonical
# sheet is the only one any consumer can reach, that the noncanonical composites
# are named and refused, and that no manuscript stage recomputes the outcome.
# DATA checks are guarded on the endpoint source being reachable and verify the
# actual numbers.

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(readr) })

source("Analysis/_pipeline_setup.R")
source_mmm_helper("project_paths.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")

PRODUCER  <- "Analysis/build_later_outcome_combz.R"
STAGE09   <- "Analysis/09_early_prediction_model_ladder.R"
STAGE27   <- "Analysis/27_build_behavior_main_figure.R"
CANON_SHEET <- "zScore"
NONCANON  <- c("combZScore", "CombZScore_noBatch", "sus_animals_batchCorrected")
skipped <- character()

code_lines <- function(p) {
  x <- readLines(p, warn = FALSE)
  x[!grepl("^\\s*#", x)]
}

# =====================================================================
cat("\n[A] the canonical producer exists and is wired into the path layer\n")
# =====================================================================

check(file.exists(PRODUCER), paste0("missing canonical producer: ", PRODUCER))
check("behavior.later_outcome_combz" %in% mmm_path_keys(),
      "the canonical CombZ output is not registered in the path layer")
check("behavior.combz_upstream_workbook" %in% mmm_path_keys(),
      "the upstream endpoint workbook is not registered in the path layer")
roles <- names(.mmm_path_specs()[["behavior.later_outcome_combz"]]$files)
for (r in c("animal_level", "component_definition", "classification_thresholds",
            "parity_audit", "alternative_composite_audit")) {
  check(r %in% roles, paste0("path key behavior.later_outcome_combz lacks role '", r, "'"))
}
ok("producer present; both upstream and canonical keys registered with all roles")

prod <- code_lines(PRODUCER)
check(any(grepl('CANONICAL_SHEET\\s*<-\\s*"zScore"', prod)),
      "the producer must pin the canonical sheet to 'zScore'")
check(any(grepl("PARITY FAILURE", prod, fixed = TRUE)),
      "the producer must hard-stop on parity failure")
check(any(grepl("population_sd", prod, fixed = TRUE)),
      "the producer must use a population-SD helper for the classification")
check(!any(grepl("stats::sd\\(|[^_a-zA-Z.]sd\\(", prod)),
      "the producer must not use sample SD anywhere in the classification path")
ok("producer pins the canonical sheet, uses population SD, and gates on parity")

# =====================================================================
cat("\n[B] noncanonical composites can never become the endpoint\n")
# =====================================================================

# The producer must NAME them (so the guard is explicit) and must not READ them.
for (nc in NONCANON) {
  check(any(grepl(nc, prod, fixed = TRUE)),
        paste0("the producer must explicitly name the noncanonical artifact '", nc,
               "' so it is auditable"))
}
read_calls <- grep("read_excel|read\\.csv|read_csv|readLines", prod, value = TRUE)
for (nc in NONCANON) {
  bad <- grep(nc, read_calls, fixed = TRUE, value = TRUE)
  check(length(bad) == 0L,
        paste0("the producer appears to READ the noncanonical artifact '", nc,
               "': ", paste(bad, collapse = " | ")))
}
ok("producer names all noncanonical alternatives and reads none of them")

# Stage 09 must read only the canonical sheet.
s09 <- code_lines(STAGE09)
sheet_refs <- grep("endpoint_excel_sheet|excel_sheets|sheet\\s*=", s09, value = TRUE)
check(any(grepl(paste0('endpoint_excel_sheet\\s*<-\\s*"', CANON_SHEET, '"'), s09)),
      "Stage 09 must pin endpoint_excel_sheet to the canonical 'zScore' sheet")
for (nc in NONCANON) {
  hit <- grep(nc, s09, fixed = TRUE)
  check(length(hit) == 0L,
        paste0("Stage 09 references the noncanonical artifact '", nc,
               "' at line(s) ", paste(hit, collapse = ", ")))
}
check(any(grepl('outcome_col\\s*<-\\s*"CombZ"', s09)),
      "Stage 09 must declare CombZ as its outcome column")
ok("Stage 09 pins the canonical sheet and the CombZ endpoint, references no alternative")

# Stage 27 must not touch the endpoint definition at all.
s27 <- code_lines(STAGE27)
for (nc in c(NONCANON, "read_excel", "excel_sheets")) {
  hit <- grep(nc, s27, fixed = TRUE)
  check(length(hit) == 0L,
        paste0("Stage 27 must not reach the endpoint workbook; found '", nc,
               "' at line(s) ", paste(hit, collapse = ", ")))
}
# These patterns are deliberately narrow. `population_sd` and
# `susceptibility_threshold` also occur as CANONICAL COLUMN NAMES that Stage 27
# legitimately carries through into Source Data, so only defining or CALLING a
# standard-deviation helper, or assigning a threshold, counts as recomputation.
s27_raw <- readLines(STAGE27, warn = FALSE)
s27_is_comment <- grepl("^\\s*#", s27_raw)
recompute_grep <- function(pat) {
  hit <- grep(pat, s27_raw)
  hit[!s27_is_comment[hit]]
}
for (pat in c("(^|[^_.$[:alnum:]])CombZ\\s*<-",
              "\\b(mutate|transmute)\\s*\\(\\s*CombZ\\s*=",
              "\\browMeans\\s*\\(",
              "population_sd\\s*(<-\\s*function|\\()",
              "(^|[^_.$[:alnum:]\"])susceptibility_threshold\\s*<-",
              "\\bstats::sd\\s*\\(", "[^_.$[:alnum:]]sd\\s*\\(")) {
  hit <- recompute_grep(pat)
  check(length(hit) == 0L,
        paste0("Stage 27 appears to recompute the outcome, pattern '", pat,
               "' at line(s) ", paste(hit, collapse = ", ")))
}
# Positive control: the canonical column names ARE expected to appear, because
# Stage 27 exports them verbatim. If they vanish, the provenance has been lost.
check(any(grepl("control_population_sd_combz", s27_raw, fixed = TRUE)),
      paste("Stage 27 should carry the canonical control_population_sd_combz",
            "column into Source Data; its absence means the threshold",
            "provenance is no longer exported"))
ok("Stage 27 neither reads the workbook nor recomputes CombZ or the thresholds")

# =====================================================================
# DATA-DEPENDENT SECTION
# =====================================================================

project_root <- mmm_project_root()
wb <- tryCatch(mmm_path_get("behavior.combz_upstream_workbook", "workbook",
                            required = FALSE, root = project_root),
               error = function(e) NA_character_)
have_wb  <- !is.na(wb) && file.exists(wb) && requireNamespace("readxl", quietly = TRUE)
canon <- tryCatch(mmm_path_get("behavior.later_outcome_combz", "animal_level",
                               required = FALSE, root = project_root),
                  error = function(e) NA_character_)
have_out <- !is.na(canon) && file.exists(canon)

if (!have_out) {
  skipped <- c(skipped, "[C]-[E] value checks (canonical CombZ output not present)")
} else {
  cat("\n[C] the canonical output satisfies its own contract\n")
  a <- read_csv(canon, show_col_types = FALSE, progress = FALSE)
  REQ <- c("AnimalNum", "Sex", "experimental_condition", "NOR", "sucrose_pref",
           "weight_dev", "delta_cort", "adrenal_weight", "spleen_weight",
           "CombZ", "outcome_group", "combz_definition_id",
           "reference_population_id", "classification_rule_id")
  check(all(REQ %in% names(a)),
        paste0("canonical animal table missing column(s): ",
               paste(setdiff(REQ, names(a)), collapse = ", ")))
  check(nrow(a) == 117L, paste0("expected 117 animals, got ", nrow(a)))
  check(!anyDuplicated(a$AnimalNum), "AnimalNum must be unique")
  check(all(a$Sex %in% c("Female", "Male")),
        paste0("Sex must be Female/Male; found ",
               paste(setdiff(unique(a$Sex), c("Female", "Male")), collapse = ", ")))
  check(all(a$outcome_group %in% c("CON", "RES", "SUS")),
        "outcome_group must be CON/RES/SUS")
  check(all(a$experimental_condition %in% c("CON", "SIS")),
        "experimental_condition must be CON/SIS")
  check(length(unique(a$combz_definition_id)) == 1L,
        "combz_definition_id must be constant")
  # CON animals may never be relabelled as a phenotype group
  check(all(a$outcome_group[a$experimental_condition == "CON"] == "CON"),
        "control animals must never be labelled RES or SUS")
  check(!any(a$outcome_group[a$experimental_condition == "SIS"] == "CON"),
        "SIS-exposed animals must never be labelled CON")
  ok(paste0("117 animals, unique ids, vocabularies and condition/group separation intact"))

  # the composite must be the unweighted mean of the six components
  comps <- c("NOR", "sucrose_pref", "weight_dev", "delta_cort",
             "adrenal_weight", "spleen_weight")
  M <- as.matrix(a[, comps])
  recomputed <- rowMeans(M, na.rm = TRUE)
  d <- max(abs(a$CombZ - recomputed), na.rm = TRUE)
  check(d <= 1e-12,
        paste0("CombZ is not the unweighted mean of the six components (max diff ",
               format(d), ")"))
  check(all(a$n_components_present %in% c(5L, 6L)),
        "every animal must have five or six components present")
  check(sum(a$n_components_present == 5L) == 2L,
        paste0("expected exactly 2 animals with 5 of 6 components, got ",
               sum(a$n_components_present == 5L)))
  ok(paste0("CombZ = unweighted mean of six components (max diff ", format(d), ")"))

  cat("\n[D] the classification thresholds reproduce exactly\n")
  thr <- read_csv(mmm_path_get("behavior.later_outcome_combz",
                               "classification_thresholds", root = project_root),
                  show_col_types = FALSE, progress = FALSE)
  EXPECT <- c(Male = -0.436641698, Female = -0.222390844)
  for (sx in names(EXPECT)) {
    got <- thr$susceptibility_threshold[thr$Sex == sx]
    check(length(got) == 1L, paste0("no threshold row for Sex=", sx))
    check(abs(got - EXPECT[[sx]]) < 1e-8,
          paste0(sx, " threshold is ", format(got, digits = 12),
                 ", expected ", format(EXPECT[[sx]], digits = 12)))
  }
  check(all(thr$n_control == 12L), "each sex must have 12 control reference animals")
  check(all(grepl("population", thr$sd_convention, ignore.case = TRUE)),
        "the recorded SD convention must be population SD")
  ok("Male -0.436641698 and Female -0.222390844, 12 controls per sex, population SD")

  # the rule must reproduce the labels from CombZ alone
  pop_sd <- function(v) { v <- v[is.finite(v)]; sqrt(mean((v - mean(v))^2)) }
  derived <- rep(NA_character_, nrow(a))
  for (sx in unique(a$Sex)) {
    cz <- a$CombZ[a$Sex == sx & a$outcome_group == "CON"]
    t <- mean(cz) - pop_sd(cz)
    i <- which(a$Sex == sx)
    derived[i] <- ifelse(a$experimental_condition[i] == "CON", "CON",
                  ifelse(a$CombZ[i] < t, "SUS", "RES"))
  }
  mism <- sum(derived != a$outcome_group)
  check(mism == 0L,
        paste0(mism, " outcome_group label(s) are not reproduced by the ",
               "within-sex control mean - population SD rule"))
  ok("all 117 labels reproduced from CombZ by the declared rule, 0 mismatches")

  cat("\n[E] the parity audit records a pass against the workbook\n")
  pa <- read_csv(mmm_path_get("behavior.later_outcome_combz", "parity_audit",
                              root = project_root),
                 show_col_types = FALSE, progress = FALSE)
  check(all(c("quantity", "n_compared", "max_abs_difference", "tolerance",
              "passed") %in% names(pa)), "parity audit schema incomplete")
  check(all(pa$passed), paste0("parity audit contains a failure: ",
        paste(pa$quantity[!pa$passed], collapse = ", ")))
  cz_row <- pa[pa$quantity == "CombZ", ]
  check(nrow(cz_row) == 1L && cz_row$max_abs_difference[1] <= 1e-12,
        "the CombZ parity row must record machine-precision agreement")
  lb_row <- pa[grepl("outcome_group", pa$quantity), ]
  check(nrow(lb_row) == 1L && lb_row$max_abs_difference[1] == 0,
        "the label-parity row must record zero mismatches")
  ok(paste0("parity audit: all ", nrow(pa), " rows passed"))

  alt <- read_csv(mmm_path_get("behavior.later_outcome_combz",
                               "alternative_composite_audit", root = project_root),
                  show_col_types = FALSE, progress = FALSE)
  check(all(alt$status == "NONCANONICAL_ALTERNATIVE"),
        "every alternative composite must be classified NONCANONICAL_ALTERNATIVE")
  check(!any(alt$read_by_this_producer),
        "no alternative composite may be read by the producer")
  for (nc in c("combZScore", "CombZScore_noBatch")) {
    check(nc %in% alt$artifact,
          paste0("alternative-composite audit does not cover '", nc, "'"))
  }
  ok(paste0(nrow(alt), " alternatives audited, all NONCANONICAL_ALTERNATIVE, none read"))
}

if (have_wb && have_out) {
  cat("\n[F] direct parity against the upstream workbook\n")
  z <- suppressWarnings(as.data.frame(
        readxl::read_excel(wb, sheet = CANON_SHEET)))
  a <- read_csv(canon, show_col_types = FALSE, progress = FALSE)
  z$k <- canonical_animal_id(z$ID)
  m <- merge(data.frame(k = z$k, wb_combz = suppressWarnings(as.numeric(z$CombZ))),
             data.frame(k = canonical_animal_id(a$AnimalNum), repo_combz = a$CombZ),
             by = "k")
  check(nrow(m) == 117L, paste0("workbook/repo join is ", nrow(m), ", expected 117"))
  d <- max(abs(m$wb_combz - m$repo_combz), na.rm = TRUE)
  check(d <= 1e-12,
        paste0("repo CombZ differs from the workbook by ", format(d)))
  ok(paste0("117/117 animals, max |repo - workbook| = ", format(d)))

  # the canonical sheet must remain the only one carrying a column named CombZ
  # that any consumer pins
  sheets <- readxl::excel_sheets(wb)
  for (nc in c("combZScore", "CombZScore_noBatch")) {
    check(nc %in% sheets,
          paste0("expected the noncanonical sheet '", nc,
                 "' to still exist upstream (nothing may be deleted)"))
  }
  ok("noncanonical sheets still present upstream and untouched")
} else if (!have_wb) {
  skipped <- c(skipped, "[F] direct workbook parity (workbook or readxl unavailable)")
}

if (length(skipped) > 0L) {
  cat("\nSKIPPED (environment-dependent):\n")
  for (s in skipped) cat("  -", s, "\n")
}

cat("\nCombZ canonical-definition checks: PASS\n")
