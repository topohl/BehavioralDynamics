# ================================================================
# CANONICAL PRODUCER - later composite stress-burden score (CombZ)
# MMMSociability
# ================================================================
# This is the single canonical in-repository definition of the LATER OUTCOME
# that every prospective analysis predicts. It is an upstream producer, not a
# numbered pipeline stage, so it follows the non-numbered convention already
# used by build_publication_release.R. It is deliberately NOT registered in
# Analysis/run_all_analysis.R.
#
# WHY IT EXISTS
#
# CombZ was previously supplied only by a hand-maintained Excel workbook, which
# meant the manuscript's outcome definition was not auditable from the
# repository. This producer makes it auditable: it reads the upstream source,
# reconstructs the composite and the phenotype classification with explicit
# rules, and HARD-STOPS unless it reproduces the workbook exactly.
#
# WHAT IS AND IS NOT REPRODUCIBLE, ESTABLISHED BY DIRECT FORMULA AUDIT
#
#   REPRODUCED HERE, and gated:
#     * the composite:      CombZ = mean of the six component z-scores,
#                           unweighted, ignoring missing components
#                           (Excel AVERAGE semantics) -> parity ~2e-16
#     * the classification: within Sex, among SIS-exposed animals,
#                           SUS if CombZ < mean(CombZ | CON, same Sex)
#                                        - populationSD(CombZ | CON, same Sex)
#                           -> parity: zero label mismatches
#
#   NOT REPRODUCED, and documented as an external dependency:
#     * the six component z-scores themselves. In the workbook these are static
#       pasted values whose upstream formulas standardize each raw measure
#       against a WITHIN-SEX CONTROL reference, but the reference is written as
#       hard-coded absolute ROW POSITIONS in per-sex formula blocks rather than
#       as a semantic criterion. The block boundaries are row ranges, not sex
#       predicates, so no single uniform rule reproduces all animals. Attempting
#       to "tidy" that would CHANGE the primary endpoint, which this pass
#       explicitly must not do. The components are therefore carried through
#       verbatim and their provenance is recorded.
#
# See docs/COMBZ_CANONICAL_DEFINITION.md for the full audit, including the
# noncanonical alternative composites that must never be substituted.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tibble); library(stringr)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("project_paths.R")

if (!requireNamespace("readxl", quietly = TRUE)) {
  stop("readxl is required to read the upstream endpoint workbook.", call. = FALSE)
}

project_root <- mmm_project_root()

# ------------------------------------------------------------------ contract
COMBZ_DEFINITION_ID       <- "combz_v1_six_component_unweighted_mean"
COMBZ_REFERENCE_POP_ID    <- "within_sex_control_reference__upstream_workbook_positional_blocks"
COMBZ_CLASSIFICATION_ID   <- "sus_if_combz_below_within_sex_control_mean_minus_1_population_sd"
CANONICAL_SHEET           <- "zScore"
NONCANONICAL_SHEETS       <- c("combZScore", "CombZScore_noBatch")
NONCANONICAL_ID_LISTS     <- c("sus_animals_batchCorrected.csv")

# zScore!K = AVERAGE(E:J); columns E..J in this exact order.
COMBZ_COMPONENTS <- c("NOR", "sucrose_pref", "weight_dev", "delta_cort",
                      "adrenal_weight", "spleen_weight")

# Direction, as established from the upstream formulas. "inverted" means the
# raw measure was multiplied by -1 after standardization, so that for EVERY
# component a HIGHER value means a MORE RESILIENT-LIKE animal.
COMBZ_COMPONENT_META <- tibble::tribble(
  ~component,        ~xlsx_column, ~domain,                  ~raw_measure,
  ~raw_derivation,                                    ~sign_inverted, ~higher_means,
  "NOR",             "E", "cognition / recognition memory", "NOR discrimination index",
  "(contactNov - contactFam) / (contactNov + contactFam)", FALSE, "better recognition memory",
  "sucrose_pref",    "F", "hedonic / anhedonia",            "combined sucrose preference (%)",
  "100 * sucrose intake / total fluid intake, pooled over three tests", FALSE, "less anhedonia",
  "weight_dev",      "G", "somatic growth",                 "body-weight change",
  "difference between two scheduled weighing days",   FALSE, "better somatic growth",
  "delta_cort",      "H", "HPA-axis reactivity",            "corticosterone rise",
  "response concentration minus baseline concentration", TRUE,  "smaller stress-induced rise",
  "adrenal_weight",  "I", "endocrine organ load",           "adrenal weight ratio",
  "100 * adrenal weight / body weight",               TRUE,  "less adrenal hypertrophy",
  "spleen_weight",   "J", "immune organ load",              "spleen weight ratio",
  "100 * spleen weight / body weight",                TRUE,  "less splenic involution/load"
)
stopifnot(identical(COMBZ_COMPONENT_META$component, COMBZ_COMPONENTS))

PARITY_TOL_COMBZ <- 1e-12   # CSV/float round-trip only; observed agreement ~2e-16
N_EXPECTED_ANIMALS <- 117L

out_dir <- dirname(mmm_path_get("behavior.later_outcome_combz", "animal_level",
                                required = FALSE, root = project_root))
audit_dir <- file.path(dirname(out_dir), "audit")
ensure_dir(out_dir); ensure_dir(audit_dir)

cat("Canonical later-outcome producer | CombZ\n")
cat("  endpoint source root:", mmm_endpoint_source_root(project_root), "\n")
cat("  output               :", out_dir, "\n")

# --------------------------------------------------------------- read upstream
wb  <- mmm_path_get("behavior.combz_upstream_workbook", "workbook", root = project_root)
sus_file <- mmm_path_get("behavior.combz_upstream_workbook", "susceptible_ids",
                         root = project_root)
con_file <- mmm_path_get("behavior.combz_upstream_workbook", "control_ids",
                         root = project_root)

# Guard: refuse to run against a noncanonical classification list.
for (bad in NONCANONICAL_ID_LISTS) {
  if (grepl(bad, sus_file, fixed = TRUE) || grepl(bad, con_file, fixed = TRUE)) {
    stop("Refusing to build the canonical outcome from a noncanonical ",
         "classification list (", bad, ").", call. = FALSE)
  }
}

available <- readxl::excel_sheets(wb)
if (!CANONICAL_SHEET %in% available) {
  stop("The canonical sheet '", CANONICAL_SHEET, "' is absent from ", wb,
       call. = FALSE)
}

raw <- suppressWarnings(as.data.frame(
  readxl::read_excel(wb, sheet = CANONICAL_SHEET)))

required_cols <- c("ID", "Group", "Sex", "Batch", COMBZ_COMPONENTS, "CombZ")
missing_cols <- setdiff(required_cols, names(raw))
if (length(missing_cols) > 0L) {
  stop("Upstream contract violation: sheet '", CANONICAL_SHEET,
       "' is missing column(s): ", paste(missing_cols, collapse = ", "),
       "\nPresent: ", paste(names(raw), collapse = ", "), call. = FALSE)
}
if (nrow(raw) != N_EXPECTED_ANIMALS) {
  stop("Upstream contract violation: expected ", N_EXPECTED_ANIMALS,
       " animals in sheet '", CANONICAL_SHEET, "', found ", nrow(raw), ".",
       call. = FALSE)
}
cat("  read", nrow(raw), "animals from sheet", CANONICAL_SHEET, "\n")

# -------------------------------------------------- normalise identity and sex
SEX_MAP <- c(m = "Male", f = "Female", M = "Male", F = "Female",
             male = "Male", female = "Female")
dat <- raw %>%
  transmute(
    AnimalNum = canonical_animal_id(.data$ID),
    upstream_id = as.character(.data$ID),
    Sex = unname(SEX_MAP[as.character(.data$Sex)]),
    Batch = as.character(.data$Batch),
    experimental_condition = as.character(.data$Group),
    across(all_of(COMBZ_COMPONENTS), ~ suppressWarnings(as.numeric(.x))),
    combz_upstream = suppressWarnings(as.numeric(.data$CombZ)))

if (anyNA(dat$Sex)) {
  stop("Unmapped Sex value(s) in the upstream sheet: ",
       paste(unique(raw$Sex[is.na(dat$Sex)]), collapse = ", "), call. = FALSE)
}
if (anyDuplicated(dat$AnimalNum)) {
  stop("Duplicated canonical AnimalNum in the upstream sheet: ",
       paste(dat$AnimalNum[duplicated(dat$AnimalNum)], collapse = ", "),
       call. = FALSE)
}
if (!all(dat$experimental_condition %in% c("CON", "SIS"))) {
  stop("Unexpected experimental_condition value(s): ",
       paste(setdiff(unique(dat$experimental_condition), c("CON", "SIS")),
             collapse = ", "), call. = FALSE)
}

# ------------------------------------------------------ recompute the composite
# Excel AVERAGE ignores blank cells, so an animal with five of six components is
# averaged over five. That is the canonical semantic; it is NOT missing-as-zero
# and it is NOT drop-the-animal.
comp_mat <- as.matrix(dat[, COMBZ_COMPONENTS])
dat$n_components_present <- rowSums(is.finite(comp_mat))
dat$CombZ <- rowMeans(comp_mat, na.rm = TRUE)
dat$CombZ[dat$n_components_present == 0L] <- NA_real_

combz_abs_diff <- abs(dat$CombZ - dat$combz_upstream)
max_combz_diff <- max(combz_abs_diff, na.rm = TRUE)

component_parity <- tibble(
  quantity = COMBZ_COMPONENTS,
  n_compared = vapply(COMBZ_COMPONENTS,
                      function(c) sum(is.finite(dat[[c]])), integer(1)),
  # components are carried through verbatim, so their parity is exact by
  # construction; recorded explicitly so the audit is complete rather than
  # silently assuming it
  max_abs_difference = 0)

# ------------------------------------------- derive the later outcome group
read_ids <- function(path, label) {
  x <- readLines(path, warn = FALSE)
  x <- trimws(x)
  x <- x[nzchar(x)]
  # tolerate a header line
  x <- x[!grepl("^(id|animal|animalnum|animal_id)$", tolower(x))]
  ids <- canonical_animal_id(x)
  if (length(ids) == 0L) stop("No identifiers read from ", path, call. = FALSE)
  cat("  ", label, "list:", length(ids), "identifiers\n")
  ids
}
sus_ids <- read_ids(sus_file, "susceptible")
con_ids <- read_ids(con_file, "control")
overlap <- intersect(sus_ids, con_ids)
if (length(overlap) > 0L) {
  stop("Identifier(s) appear in BOTH classification lists: ",
       paste(overlap, collapse = ", "), call. = FALSE)
}

# Shipped labelling, exactly as Analysis/01_build_multiscale_behavior_metrics.R
# does it: listed control -> CON, listed susceptible -> SUS, any other
# SIS-exposed animal -> RES.
dat$outcome_group_upstream <- dplyr::case_when(
  dat$AnimalNum %in% con_ids ~ "CON",
  dat$AnimalNum %in% sus_ids ~ "SUS",
  dat$experimental_condition == "SIS" ~ "RES",
  TRUE ~ NA_character_)
if (anyNA(dat$outcome_group_upstream)) {
  stop("Could not assign an upstream outcome group to animal(s): ",
       paste(dat$AnimalNum[is.na(dat$outcome_group_upstream)], collapse = ", "),
       call. = FALSE)
}

# Independent derivation of the classification from CombZ.
population_sd <- function(v) {
  v <- v[is.finite(v)]
  sqrt(mean((v - mean(v))^2))
}
thresholds <- dat %>%
  filter(.data$outcome_group_upstream == "CON") %>%
  group_by(.data$Sex) %>%
  summarise(
    n_control = dplyr::n(),
    control_mean_combz = mean(.data$CombZ),
    control_population_sd_combz = population_sd(.data$CombZ),
    .groups = "drop") %>%
  mutate(
    susceptibility_threshold = .data$control_mean_combz -
      .data$control_population_sd_combz,
    sd_convention = "population SD (n divisor), matching Excel STDEVPA",
    classification_rule_id = COMBZ_CLASSIFICATION_ID)

dat <- dat %>%
  left_join(thresholds %>% select("Sex", "susceptibility_threshold"), by = "Sex") %>%
  mutate(outcome_group_derived = dplyr::case_when(
    .data$outcome_group_upstream == "CON" ~ "CON",
    .data$CombZ < .data$susceptibility_threshold ~ "SUS",
    TRUE ~ "RES"))

label_mismatch <- dat %>%
  filter(.data$outcome_group_derived != .data$outcome_group_upstream)

# ------------------------------------------------------------- PARITY GATE
cat("\n--- workbook parity gate ---\n")
cat("  animals compared              :", nrow(dat), "\n")
cat("  max |CombZ - workbook CombZ|  :", format(max_combz_diff), "\n")
cat("  outcome-group mismatches      :", nrow(label_mismatch), "\n")
cat("  thresholds:\n")
for (i in seq_len(nrow(thresholds))) {
  cat(sprintf("     %-7s n_control=%2d  mean=%.9f  popSD=%.9f  threshold=%.9f\n",
              thresholds$Sex[i], thresholds$n_control[i],
              thresholds$control_mean_combz[i],
              thresholds$control_population_sd_combz[i],
              thresholds$susceptibility_threshold[i]))
}

if (!is.finite(max_combz_diff) || max_combz_diff > PARITY_TOL_COMBZ) {
  stop("PARITY FAILURE: recomputed CombZ differs from the workbook by ",
       format(max_combz_diff), " (tolerance ", format(PARITY_TOL_COMBZ), ").\n",
       "The workbook endpoint is authoritative and must NOT be redefined to ",
       "make this producer pass. Investigate the component columns instead.",
       call. = FALSE)
}
if (nrow(label_mismatch) > 0L) {
  stop("PARITY FAILURE: ", nrow(label_mismatch),
       " outcome-group label(s) do not match the upstream classification:\n",
       paste(sprintf("  %s (%s) CombZ=%.6f upstream=%s derived=%s",
                     label_mismatch$AnimalNum, label_mismatch$Sex,
                     label_mismatch$CombZ, label_mismatch$outcome_group_upstream,
                     label_mismatch$outcome_group_derived), collapse = "\n"),
       call. = FALSE)
}
cat("  PARITY GATE: PASS\n")

# --------------------------------------- noncanonical alternative composites
alt_audit <- tibble(
  artifact = c(NONCANONICAL_SHEETS, NONCANONICAL_ID_LISTS),
  artifact_kind = c(rep("workbook sheet", length(NONCANONICAL_SHEETS)),
                    rep("classification identifier list",
                        length(NONCANONICAL_ID_LISTS))),
  present_upstream = c(
    NONCANONICAL_SHEETS %in% available,
    file.exists(file.path(mmm_endpoint_source_root(project_root),
                          NONCANONICAL_ID_LISTS))),
  status = "NONCANONICAL_ALTERNATIVE",
  read_by_this_producer = FALSE,
  note = c(
    "Five-domain composite that also includes social preference, EPM and OFT; materially different values. Never read by any repository script.",
    "Within-batch-referenced variant of the component z-scores. Never read by any repository script.",
    "Batch-corrected susceptible list. Not the list used by Analysis/01_build_multiscale_behavior_metrics.R."))
stopifnot(nrow(alt_audit) == length(NONCANONICAL_SHEETS) + length(NONCANONICAL_ID_LISTS))

# ------------------------------------------------------------------- exports
animal_level <- dat %>%
  transmute(
    AnimalNum = .data$AnimalNum,
    upstream_id = .data$upstream_id,
    Sex = .data$Sex,
    Batch = .data$Batch,
    experimental_condition = .data$experimental_condition,
    across(all_of(COMBZ_COMPONENTS)),
    n_components_present = .data$n_components_present,
    CombZ = .data$CombZ,
    outcome_group = .data$outcome_group_derived,
    combz_definition_id = COMBZ_DEFINITION_ID,
    reference_population_id = COMBZ_REFERENCE_POP_ID,
    classification_rule_id = COMBZ_CLASSIFICATION_ID) %>%
  arrange(.data$Sex, .data$outcome_group, .data$AnimalNum)

write_csv(animal_level,
          file.path(out_dir, "later_outcome_combz_animal_level.csv"))

component_definition <- COMBZ_COMPONENT_META %>%
  mutate(
    combz_definition_id = COMBZ_DEFINITION_ID,
    weight_in_composite = 1 / length(COMBZ_COMPONENTS),
    standardization = "z-score against the within-sex control reference in the upstream workbook",
    reference_population_id = COMBZ_REFERENCE_POP_ID,
    sd_convention = "population SD (n divisor), Excel STDEVPA",
    reproducible_in_repository = FALSE,
    reproducibility_note = paste(
      "Carried through verbatim from the upstream workbook. The upstream",
      "standardization is written as hard-coded absolute row positions in",
      "per-sex formula blocks, not as a semantic criterion, so no single",
      "uniform rule reproduces every animal. Re-deriving it would change the",
      "primary endpoint."))
write_csv(component_definition,
          file.path(out_dir, "combz_component_definition.csv"))

write_csv(thresholds, file.path(out_dir, "combz_classification_thresholds.csv"))

parity_audit <- bind_rows(
  tibble(quantity = "CombZ", n_compared = sum(is.finite(combz_abs_diff)),
         max_abs_difference = max_combz_diff),
  component_parity,
  tibble(quantity = "outcome_group (label mismatches)",
         n_compared = nrow(dat),
         max_abs_difference = as.numeric(nrow(label_mismatch)))) %>%
  mutate(
    tolerance = ifelse(.data$quantity == "outcome_group (label mismatches)",
                       0, PARITY_TOL_COMBZ),
    passed = .data$max_abs_difference <= .data$tolerance,
    upstream_source = paste0(basename(wb), " :: sheet ", CANONICAL_SHEET),
    combz_definition_id = COMBZ_DEFINITION_ID)
write_csv(parity_audit, file.path(out_dir, "combz_workbook_parity_audit.csv"))
write_csv(alt_audit, file.path(out_dir, "combz_alternative_composite_audit.csv"))

group_table <- animal_level %>% count(.data$Sex, .data$outcome_group)
write_csv(group_table, file.path(audit_dir, "combz_group_counts.csv"))

saveRDS(list(definition_id = COMBZ_DEFINITION_ID,
             reference_population_id = COMBZ_REFERENCE_POP_ID,
             classification_rule_id = COMBZ_CLASSIFICATION_ID,
             n_animals = nrow(animal_level),
             max_combz_diff = max_combz_diff,
             label_mismatches = nrow(label_mismatch),
             thresholds = thresholds,
             upstream_workbook = wb,
             upstream_sheet = CANONICAL_SHEET),
        file.path(audit_dir, "combz_build_record.rds"))

cat("\nWrote canonical later-outcome tables ->", out_dir, "\n")
cat("  animals:", nrow(animal_level), "| group counts:\n")
print(as.data.frame(group_table), row.names = FALSE)
cat("\nAll parity checks passed. CombZ is now canonical in-repository.\n")
