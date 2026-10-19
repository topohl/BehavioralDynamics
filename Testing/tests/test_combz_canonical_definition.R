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
CANON_SHEET <- "combz_canonical"
NONCANON  <- c("combZScore", "CombZScore_noBatch", "combz_alternative_long", "sus_animals_batchCorrected")
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
check(any(grepl('CANONICAL_SHEET\\s*<-\\s*"combz_canonical"', prod)),
      "the producer must pin the canonical sheet to 'combz_canonical'")
check(any(grepl("PARITY FAILURE", prod, fixed = TRUE)),
      "the producer must hard-stop on parity failure")
check(any(grepl("population_sd", prod, fixed = TRUE)),
      "the producer must use a population-SD helper for the classification")
check(!any(grepl("stats::sd\\(|[^_a-zA-Z.]sd\\(", prod)),
      "the producer must not use sample SD anywhere in the classification path")
ok("producer pins the canonical sheet, uses population SD, and gates on parity")

# Version 2 (2026-10-17) reads sheet combz_canonical of the corrected workbook
# through one hash-pinned helper and applies no correction itself. Version 1 read
# sheet zScore of the pre-restructure original (bf257c2c, now archived and read
# only by historical audit replays) and corrected three cells in the producer.
check("source_workbook" %in% names(.mmm_path_specs()[["behavior.combz_upstream_workbook"]]$files),
      "the upstream workbook key lacks the pinned source_workbook role")
check(identical(MMM_COMBZ_SOURCE_WORKBOOK_SHA256,
                "bf257c2c8b77fa35e2a8068ff19e3053b4fe83c61f7c20d5a9434a20f829e33d"),
      "the pinned source-workbook hash changed")
check(identical(MMM_COMBZ_WORKBOOK_SHA256,
                "30ab408cb0539b6c85ab7242d9bf5d09312210ccb149d58c3e9775c252c12efa"),  # RETRACK-PIN
      "the pinned corrected-workbook hash changed")
check(any(grepl("wb\\s*<-\\s*mmm_combz_workbook\\(", prod)),
      "the producer must read the corrected workbook through mmm_combz_workbook()")
check(!any(grepl("sucrosePreference|invertedD|or620", prod)),
      "the producer must not correct components itself; corrections belong in the workbook")
check(any(grepl("combz_abs_diff <- abs(dat$CombZ - dat$combz_upstream)", prod, fixed = TRUE)),
      "the recorded CombZ parity must compare the recomputed composite with the workbook's comb_z")
ok("producer reads the hash-pinned corrected workbook, corrects nothing and records the workbook parity")

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
# Until 2026-09-21 this asserted Stage 09 pinned endpoint_excel_sheet to "zScore",
# i.e. that it read the upstream workbook directly. That is now forbidden rather
# than required: the producer applies documented corrections to the upstream
# components, so a stage reading the workbook directly would silently use a
# different outcome definition. Stage 09 must take the endpoint from the canonical
# producer output instead. The original intent of this check - that Stage 09 can
# never reach a noncanonical composite - is preserved and strengthened, because
# the producer is the only artifact it may now read.
check(any(grepl("later_outcome_combz_animal_level\\.csv", s09)),
      "Stage 09 must take its endpoint from the canonical producer output, not the workbook")
check(!any(grepl('endpoint_excel_sheet\\s*<-\\s*"zScore"', s09)),
      "Stage 09 must not read the upstream workbook sheet directly; it would bypass the documented endpoint corrections")
for (nc in NONCANON) {
  hit <- grep(nc, s09, fixed = TRUE)
  check(length(hit) == 0L,
        paste0("Stage 09 references the noncanonical artifact '", nc,
               "' at line(s) ", paste(hit, collapse = ", ")))
}
check(any(grepl('outcome_col\\s*<-\\s*"CombZ"', s09)),
      "Stage 09 must declare CombZ as its outcome column")
ok("Stage 09 pins the canonical sheet and the CombZ endpoint, references no alternative")

# Stage 14 read the workbook's zScore sheet directly until 2026-09-26, so it used
# the uncorrected CombZ; the restructured workbook (2026-09-23) no longer has
# that sheet. Like Stage 09 it must take the canonical producer output, and it
# must stop rather than run without its primary outcome.
s14 <- code_lines("Analysis/14_systems_neuroscience_summary_dashboard.R")
check(any(grepl("later_outcome_combz_animal_level\\.csv", s14)),
      "Stage 14 must take its endpoint from the canonical producer output, not the workbook")
check(!any(grepl("E9_Behavior_Data|\"zScore\"", s14)),
      "Stage 14 must not read the upstream workbook directly")
check(any(grepl("endpoint_sheet\\s*<-\\s*NULL", s14)),
      "Stage 14 must not name a workbook sheet for its endpoint")
check(any(grepl("!primary_outcome %in% names(endpoint_dat)", s14, fixed = TRUE)),
      "Stage 14 must stop when the endpoint table lacks its primary outcome")
for (nc in NONCANON) {
  check(!any(grepl(nc, s14, fixed = TRUE)),
        paste0("Stage 14 references the noncanonical artifact '", nc, "'"))
}
ok("Stage 14 takes CombZ from the canonical producer output and stops without it")

# Stage 03 is the last live stage that read the workbook; it now takes the
# canonical producer output too.
s03 <- code_lines("Analysis/03_primary_raw_movement_phase_stats.R")
check(any(grepl("later_outcome_combz_animal_level\\.csv", s03)),
      "Stage 03 must take CombZ from the canonical producer output, not the workbook")
check(!any(grepl("E9_Behavior_Data|\"zScore\"|read_excel", s03)),
      "Stage 03 must not read the upstream workbook")
ok("Stage 03 takes CombZ from the canonical producer output")

# The first-night audit replays keep the as-recorded CombZ of their saved
# originals, read from the pinned pre-restructure workbook, never by a
# hard-coded path.
for (aud in c("Testing/audits/audit_first_night_domain_scores.R",
              "Testing/audits/audit_first_night_heatmap_v2.R",
              "Testing/audits/audit_first_night_hmm_components.R")) {
  a_code <- code_lines(aud)
  check(any(grepl("mmm_combz_source_workbook(PROJ)", a_code, fixed = TRUE)),
        paste0(aud, " must read CombZ from the pinned source workbook"))
  check(!any(grepl("SIS_Analysis/E9_Behavior_Data", a_code, fixed = TRUE)),
        paste0(aud, " must not hard-code the workbook path"))
}
ok("the three first-night audits read the pinned pre-restructure workbook")

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
  # Since 2026-10-16 damaged, missing and blank organ weights carry no score:
  # 104 animals have six components, 11 five and 2 four (OR414, OR419). Before,
  # 2 animals had five and the rest six.
  check(all(a$n_components_present %in% 4:6),
        "every animal must have four to six components present")
  n_by <- table(factor(a$n_components_present, levels = 4:6))
  check(identical(as.integer(n_by), c(2L, 10L, 105L)),  # RETRACK-PIN
        paste0("expected 2 / 11 / 104 animals with 4 / 5 / 6 components, got ",
               paste(as.integer(n_by), collapse = " / ")))
  ok(paste0("CombZ = unweighted mean of six components (max diff ", format(d), ")"))

  cat("\n[D] the classification thresholds reproduce exactly\n")
  thr <- read_csv(mmm_path_get("behavior.later_outcome_combz",
                               "classification_thresholds", root = project_root),
                  show_col_types = FALSE, progress = FALSE)
  # History. 2026-09-20: the Female threshold moved from -0.222390844 to
  # -0.316628592 (sucrose alignment, zero-bottle rule, OR620; OR424, OR430,
  # OR434 and OR554 SUS -> RES); Male -0.436641698. 2026-10-16 (workbook
  # corrections 09-14: batch 1 novel-object D2, damaged and blank organ weights
  # unscored, female corticosterone reference rebuilt; OQ765 kept): Female
  # -0.333634857, Male -0.433242738; OQ750, OQ752, OR419 and OR434 RES -> SUS.
  EXPECT <- c(Male = -0.433242738233, Female = -0.333634856715)
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
  ok("Male -0.433242738 and Female -0.333634857, 12 controls per sex, population SD")

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

src_wb <- tryCatch(mmm_combz_source_workbook(project_root), error = function(e) e)
if (inherits(src_wb, "error") && grepl("not the pinned", conditionMessage(src_wb))) {
  fail(conditionMessage(src_wb))
} else if (!inherits(src_wb, "error")) {
  ok("the pinned pre-restructure source workbook is present with the expected SHA-256")
} else {
  skipped <- c(skipped, "[F0] pinned source workbook (not reachable)")
}

if (have_wb && have_out) {
  cat("\n[F] direct parity against the corrected workbook\n")
  sheets <- readxl::excel_sheets(wb)
  a <- read_csv(canon, show_col_types = FALSE, progress = FALSE)
  check(CANON_SHEET %in% sheets, paste0("the workbook has no sheet ", CANON_SHEET))
  z <- suppressWarnings(as.data.frame(readxl::read_excel(wb, sheet = CANON_SHEET)))
  m <- merge(data.frame(k = canonical_animal_id(z$animal_id), wb_combz = z$comb_z,
                        wb_asrec = z$comb_z_as_recorded, wb_group = z$outcome_group,
                        wb_corrected = z$corrected),
             data.frame(k = canonical_animal_id(a$AnimalNum), repo_combz = a$CombZ,
                        repo_asrec = a$combz_as_recorded, repo_group = a$outcome_group),
             by = "k")
  check(nrow(m) == 117L, paste0("workbook/repo join is ", nrow(m), ", expected 117"))
  d_asrec <- max(abs(m$wb_asrec - m$repo_asrec), na.rm = TRUE)
  d_corr <- max(abs(m$wb_combz - m$repo_combz), na.rm = TRUE)
  check(d_asrec <= 1e-12, paste0("as-recorded CombZ differs from the workbook by ", format(d_asrec)))
  check(d_corr <= 1e-12, paste0("CombZ differs from the workbook by ", format(d_corr)))
  check(all(m$wb_group == m$repo_group), "outcome_group differs between the workbook and the producer")
  # every animal's CombZ changed with the corrections of 2026-10-16 (19 with those of 2026-09-20)
  check(sum(m$wb_corrected %in% TRUE) == 117L,
        paste0("expected 117 corrected animals in the workbook, found ", sum(m$wb_corrected %in% TRUE)))
  check("combz_alternative_long" %in% sheets,
        "the workbook must keep the noncanonical composites (combz_alternative_long)")
  ok(paste0("corrected workbook: as-recorded and corrected CombZ both match (max ",
            format(max(d_asrec, d_corr)), "), labels identical, 117 corrected animals"))
} else if (!have_wb) {
  skipped <- c(skipped, "[F] direct workbook parity (workbook or readxl unavailable)")
}

if (length(skipped) > 0L) {
  cat("\nSKIPPED (environment-dependent):\n")
  for (s in skipped) cat("  -", s, "\n")
}

cat("\nCombZ canonical-definition checks: PASS\n")
