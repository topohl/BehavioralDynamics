# ================================================================
# Behavior main figure assembly - ASSEMBLY ONLY, NO ANALYSIS
# MMMSociability
# ================================================================
# Reads the frozen canonical outputs that own the five behaviour questions and
# produces the publication tree: the assembled main figure, individual panels,
# alternative and extended-data candidates, per-panel Source Data, a manuscript
# key-results table, a legend draft, provenance manifests and a claim trace.
#
# HARD CONTRACT, asserted here and enforced from outside by
# Testing/tests/test_behavior_main_figure_contracts.R:
#   * no statistical model is fitted
#   * no p or q value is computed, adjusted or re-derived
#   * no confidence interval is estimated from data
#   * no multiplicity family is redefined
#   * CombZ is never recomputed and RES/SUS is never redefined
#   * the first-active-phase window is never reselected
#   * no scientific number is changed
# The only arithmetic performed is descriptive summarisation of the values being
# plotted, and unit relabelling. Both are declared in the manifests.
#
# NO PATH LITERAL LIVES IN THIS FILE. Every input is requested by semantic key
# from Functions/project_paths.R and every output directory comes from the
# configured publication root, so relocating either tree does not touch this
# source. Filenames are semantic; the manuscript figure number is assigned
# during whole-manuscript assembly.
# ================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(readr); library(tibble)
  library(ggplot2); library(stringr)
})

.pipeline_setup_candidates <- c(
  file.path(getwd(), "Analysis", "_pipeline_setup.R"),
  file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Could not locate Analysis/_pipeline_setup.R", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("mmm_publication_theme.R")
source_mmm_helper("gamm_publication_style_helpers.R")
source_mmm_helper("project_paths.R")
source_mmm_helper("behavior_main_figure_helpers.R")
source_mmm_helper("behavior_main_figure_docs.R")

source_mmm_helper("first_night_domain_helpers.R")

STAGE <- "27"
project_root <- mmm_project_root()
pub_root <- mmm_publication_root()

# --------------------------------------------------- MAIN FIGURE ARCHITECTURE
#
# FOUR panels, fixed. This is an editorial decision, recorded here and asserted
# by Testing/tests/test_behavior_main_figure_contracts.R:
#
#   a  experimental design, RFID framework and later outcome definition
#   b  first-night Movement by later outcome group        (descriptive)
#   c  early Movement vs later CombZ                      (INFERENTIAL, main)
#   d  out-of-sample prediction of later CombZ            (INFERENTIAL, main)
#
# Panels c and d carry the central claims and receive the most space.
#
# THERE IS NO MAIN-FIGURE SLOT FOR A DOMAIN HEATMAP. The source audit concluded
# B3_NOT_JUSTIFIED__USE_FRAMEWORK_SCHEMATIC: no scientifically clean
# longitudinal multi-domain overview can be built from currently validated
# material. Both heatmap candidates are retained, but only as Extended Data /
# audit candidates, and NO configuration can promote either into the main
# figure. Panel a's framework inventory fulfils the breadth role instead.
MAIN_FIGURE_PANELS <- c("A", "B", "C", "D")
MAIN_FIGURE_STEM <- "behavior_early_signal_and_prediction_main"

# Which domain overviews to render as EXTENDED DATA candidates. This governs
# Extended Data only; it can never affect the main figure.
ED_DOMAIN_OVERVIEWS <- getOption(
  "mmm.ed_domain_overviews", c("first_night_domains", "broad_domain_map"))
ED_DOMAIN_ALLOWED <- c("first_night_domains", "broad_domain_map", "none")
if (!all(ED_DOMAIN_OVERVIEWS %in% ED_DOMAIN_ALLOWED)) {
  stop("mmm.ed_domain_overviews must be a subset of: ",
       paste(ED_DOMAIN_ALLOWED, collapse = ", "), call. = FALSE)
}

# The characterised-domain inventory is an EXPLICIT, AUDITABLE list. Each row
# records what the domain is and what claim status it currently holds, so the
# panel can state breadth of measurement without any of it being mistaken for a
# supported group effect. `claim_status` is exported in the Source Data.
#
# `Behavioral state architecture` and the inactivity-related row are included as
# MEASUREMENT domains only. Their latent-state analyses carry identifiability
# caveats and are explicitly not used for any main claim; the inactive-phase
# reading is barred from biological interpretation altogether by
# docs/KNOWN_LIMITATIONS.md item 3. That is recorded per row here and stated in
# the panel note, so the inventory cannot be read as six findings.
FRAMEWORK_DOMAINS <- tibble::tribble(
  ~domain,                                        ~claim_status,
  "Movement / psychomotor activation",            "characterised; carries the main-figure early signal (panels b-d)",
  "Behavioural flexibility / predictability",     "characterised; no main-figure claim",
  "Social-spatial (co-location) organization",    "characterised; no main-figure claim; NOT direct sociability",
  "Behavioural volatility / fragmentation",       "characterised; no main-figure claim",
  "Behavioural state architecture (latent)",      "characterised only; latent-state identifiability caveats; no main-figure claim",
  "Inactivity-related locomotor organization",    "characterised only; inactive-phase biological interpretation barred (KNOWN_LIMITATIONS item 3)")

dirs <- setNames(
  lapply(names(MMM_PUBLICATION_SUBDIRS), function(k)
    mmm_publication_dir(k, create = TRUE, publication_root = pub_root)),
  names(MMM_PUBLICATION_SUBDIRS))
dirs$root <- pub_root
ensure_dir(pub_root)

cat("Stage 27 | behavior main figure assembly (no analysis)\n")
cat("  analysis-data root :", project_root, "\n")
cat("  publication root   :", pub_root, "\n")

# ------------------------------------------------------- guarded input reader
#
# Refuses to read from a superseded, quarantined or bundled copy. This matters
# concretely: at least three same-schema files named
# primary_prediction_performance.csv exist elsewhere on the share, including one
# in an erroneous 24 h snapshot directory and one holding the 5-min numbers.

FORBIDDEN_SOURCE_SEGMENTS <- c(
  "_quarantine", "quarantine_legacy", "_archive", "snapshot", "/releases/",
  ".old", "_pre_hmm_identity_fix", "_erroneous")

inputs_seen <- list()

rd <- function(key, role, expect_rows = NULL) {
  path <- mmm_path_get(key, file = role, root = project_root)
  hit <- FORBIDDEN_SOURCE_SEGMENTS[
    vapply(FORBIDDEN_SOURCE_SEGMENTS, function(s)
      grepl(s, path, fixed = TRUE), logical(1))]
  if (length(hit) > 0L) {
    stop("Refusing to read a superseded or bundled copy for '", key, "/", role,
         "'. Offending path segment(s): ", paste(hit, collapse = ", "),
         "\n  ", path, call. = FALSE)
  }
  x <- read_csv(path, show_col_types = FALSE, progress = FALSE)
  if (!is.null(expect_rows) && nrow(x) != expect_rows) {
    stop("Input contract violation for '", key, "/", role, "': expected ",
         expect_rows, " rows, got ", nrow(x), ".\n  ", path, call. = FALSE)
  }
  inputs_seen[[paste(key, role, sep = "/")]] <<- path
  x
}

# ------------------------------------------------------------- read canonical
cat("  reading canonical inputs ...\n")

animal      <- rd("behavior.combz_definition", "animal_level", 111)
frozen_res  <- rd("behavior.combz_definition", "primary_results")
s16_valid   <- rd("behavior.combz_definition", "validation")
s09_input   <- rd("behavior.first_night_movement", "model_input", 111)
window_ctr  <- rd("behavior.first_night_movement", "window_contract", 1)
grp_summary <- rd("behavior.first_night_movement", "group_summary")
grp_desc    <- rd("behavior.first_night_movement", "group_contrasts_descriptive")
assoc       <- rd("behavior.first_night_combz_association", "associations", 3)
sex_int     <- rd("behavior.first_night_combz_association", "sex_interactions", 3)
perf        <- rd("behavior.early_prediction", "performance", 5)
perm        <- rd("behavior.early_prediction", "permutation", 2)
heldout     <- rd("behavior.early_prediction_heldout", "predictions", 222)
domain      <- rd("behavior.rfid_domain_summary", "group_contrasts", 30)

# --- canonical later outcome, from the in-repository producer ---------------
# Panel A's outcome definition now comes from a real canonical producer
# (Analysis/build_later_outcome_combz.R), which reproduces the upstream
# workbook exactly and refuses to run otherwise. Stage 27 reads its frozen
# output; it never touches the workbook and never recomputes the composite.
combz         <- rd("behavior.later_outcome_combz", "animal_level", 117)
combz_compdef <- rd("behavior.later_outcome_combz", "component_definition", 6)
combz_thresh  <- rd("behavior.later_outcome_combz", "classification_thresholds", 2)
combz_parity  <- rd("behavior.later_outcome_combz", "parity_audit")

# --- panel B longitudinal context ------------------------------------------
longitudinal  <- rd("behavior.longitudinal_movement", "animal_endpoints")

# --- panel E real permutation null -----------------------------------------
perm_draws    <- rd("behavior.early_prediction", "permutation_draws")
gamm_claims <- rd("gamm.manuscript_outputs", "claim_trace")

# ---------------------------------------------------------- schema contracts
cat("  validating schemas ...\n")

bmf_assert_schema(
  animal, "Stage 16 animal-level source data",
  required = c("AnimalID", "Sex", "Group", "CombZ", "Movement_mean",
               "Movement_rmssd", "Entropy_acf1"),
  keys = "AnimalID",
  allowed = list(Group = BMF_GROUP_LEVELS, Sex = BMF_SEX_LEVELS),
  finite = c("CombZ", "Movement_mean"), n_rows = 111)

bmf_assert_schema(
  domain, "Stage 14 first-active domain contrasts",
  required = c("Sex", "contrast", "Domain", "hedges_g", "q", "family_id",
               "n_tests_in_family", "bin_level", "interpretation_guard"),
  keys = c("Sex", "contrast", "Domain"),
  allowed = list(Sex = BMF_SEX_LEVELS, contrast = BMF_CONTRAST_LEVELS),
  finite = "hedges_g", n_rows = 30)

bmf_assert_schema(
  heldout, "Stage 16 held-out predictions",
  required = c("AnimalID", "observed_CombZ", "predicted_CombZ", "model_id",
               "validation_scheme", "Sex", "Group"),
  keys = c("AnimalID", "model_id"),
  allowed = list(Group = BMF_GROUP_LEVELS, Sex = BMF_SEX_LEVELS),
  finite = c("observed_CombZ", "predicted_CombZ"), n_rows = 222)

bmf_assert_schema(
  perf, "Stage 09 prediction performance",
  required = c("model_id", "model_label", "reporting_role", "predictors",
               "n_animals", "validation_scheme", "cv_r2", "permutation_p",
               "repeated_cv_mean_r2", "cv_r2_q025", "cv_r2_q975",
               "interval_type"), keys = "model_id")

bmf_assert_schema(
  perm, "Stage 09 permutation reference",
  required = c("model", "model_label", "observed_statistic_name",
               "observed_statistic", "null_median", "null_q025", "null_q975",
               "empirical_p", "n_permutations", "cv_scheme", "seed"),
  keys = "model")

bmf_assert_schema(
  assoc, "Stage 09 primary feature associations",
  required = c("feature", "spearman_rho", "spearman_p", "spearman_p_bh",
               "spearman_boot_ci_low", "spearman_boot_ci_high"),
  keys = "feature", n_rows = 3)

# --- window identity must be exactly the frozen Stage 09 window -------------
WINDOW_EXPECTED <- paste(
  "fixed clock window: first Active phase block after first cage change,",
  "18:30 inclusive to 06:30 exclusive")
if (!identical(as.character(window_ctr$primary_window_definition[1]),
               WINDOW_EXPECTED)) {
  stop("Stage 09 first-active window identity changed. Expected:\n  ",
       WINDOW_EXPECTED, "\nFound:\n  ",
       window_ctr$primary_window_definition[1],
       "\nRefusing to assemble a figure against a redefined window.",
       call. = FALSE)
}
stopifnot(window_ctr$bin_size_min[1] == 10,
          window_ctr$target_window_hours[1] == 12,
          window_ctr$expected_target_slots_per_animal[1] == 72,
          window_ctr$first_cage_change[1] == "CC1",
          window_ctr$n_inactive_rows_selected[1] == 0,
          window_ctr$n_animals[1] == 111)

# --- Stage 16 plotted values must equal the Stage 09 analysis frame ---------
# CSV text round-trip loses the last bit or two of a double, so the tolerance is
# 1e-12 rather than 0. Source Data versus plotted values IS checked at 0, later,
# because those come from one in-memory object.
TOL_ROUNDTRIP <- 1e-12
xcheck <- animal %>%
  mutate(.key = as.character(.data$AnimalID)) %>%
  inner_join(s09_input %>% mutate(.key = as.character(.data$AnimalNum)) %>%
               select(".key", s09_movement = "Movement_mean",
                      s09_rmssd = "Movement_rmssd", s09_acf1 = "Entropy_acf1",
                      s09_outcome = "outcome"), by = ".key")
if (nrow(xcheck) != 111) {
  stop("Stage 16 and Stage 09 animal sets do not match: joined ", nrow(xcheck),
       " of 111.", call. = FALSE)
}
drift <- c(
  Movement_mean = max(abs(xcheck$Movement_mean - xcheck$s09_movement)),
  Movement_rmssd = max(abs(xcheck$Movement_rmssd - xcheck$s09_rmssd)),
  Entropy_acf1 = max(abs(xcheck$Entropy_acf1 - xcheck$s09_acf1)),
  CombZ = max(abs(xcheck$CombZ - xcheck$s09_outcome)))
if (any(drift > TOL_ROUNDTRIP)) {
  stop("Stage 16 manuscript values disagree with the Stage 09 analysis frame ",
       "beyond CSV round-trip tolerance:\n",
       paste0("  ", names(drift), ": ", format(drift), collapse = "\n"),
       call. = FALSE)
}
cat("    Stage 16 vs Stage 09 agreement: max drift ",
    format(max(drift)), " (tolerance ", format(TOL_ROUNDTRIP), ")\n", sep = "")

# --- the held-out predictions must be the same animals and observed values --
ho_primary <- heldout %>% filter(.data$model_id == "movement_mean")
stopifnot(nrow(ho_primary) == 111)
ho_check <- ho_primary %>%
  mutate(.key = as.character(.data$AnimalID)) %>%
  inner_join(animal %>% mutate(.key = as.character(.data$AnimalID)) %>%
               select(".key", ref_CombZ = "CombZ"), by = ".key")
stopifnot(nrow(ho_check) == 111,
          max(abs(ho_check$observed_CombZ - ho_check$ref_CombZ)) == 0)

# ------------------------------------------------------------- frozen facts
#
# Every scientific number the figure or legend states is pulled from a canonical
# table here, once, and then only formatted. Nothing below re-derives a value.

pick <- function(df, col, where_col, where_val) {
  hit <- df[[col]][as.character(df[[where_col]]) == where_val]
  if (length(hit) != 1L) {
    stop("Expected exactly one '", col, "' for ", where_col, " == ", where_val,
         "; got ", length(hit), ".", call. = FALSE)
  }
  hit
}

HEADLINE_MODEL <- "movement_mean"
HEADLINE_FEATURE <- "Movement_mean"

# The headline model is fixed by a Stage 16 contract gate, not by us.
gate <- s16_valid %>% filter(str_detect(.data$check_id, "movement_mean_headline"))
if (nrow(gate) == 1L && !identical(toupper(as.character(gate$status[1])), "PASS")) {
  stop("Stage 16 headline-model contract gate is not PASS; refusing to label ",
       HEADLINE_MODEL, " as the headline prediction.", call. = FALSE)
}

assoc_row <- assoc %>% filter(.data$feature == HEADLINE_FEATURE)
stopifnot(nrow(assoc_row) == 1L)

perf_head <- perf %>% filter(.data$model_id == HEADLINE_MODEL)
perm_head <- perm %>% filter(.data$model == HEADLINE_MODEL)
stopifnot(nrow(perf_head) == 1L, nrow(perm_head) == 1L)
baseline_r2 <- pick(perf, "cv_r2", "model_id", "mean_only")

group_counts <- table(factor(as.character(animal$Group), levels = BMF_GROUP_LEVELS))
sex_counts <- table(factor(as.character(animal$Sex), levels = BMF_SEX_LEVELS))

# The component list, its order and its display labels all come from the
# canonical producer's own component-definition table, so the schematic can
# never drift from the endpoint definition and no component name is written as
# a literal here.
bmf_assert_schema(combz_compdef, "canonical CombZ component definition",
                  required = c("component", "xlsx_column", "domain",
                               "raw_measure", "sign_inverted", "higher_means",
                               "weight_in_composite", "combz_definition_id"),
                  keys = "component", n_rows = 6)
combz_compdef <- combz_compdef %>% arrange(.data$xlsx_column)
combz_components <- as.character(combz_compdef$component)
if (length(unique(combz_compdef$weight_in_composite)) != 1L) {
  stop("The canonical CombZ components are not equally weighted; the panel A ",
       "schematic states an unweighted mean and would be wrong.", call. = FALSE)
}
COMBZ_DEFINITION_ID <- unique(as.character(combz_compdef$combz_definition_id))
stopifnot(length(COMBZ_DEFINITION_ID) == 1L)

# Display labels are derived from the canonical `domain` field, wrapped for the
# schematic boxes. Nothing scientific is introduced.
wrap2 <- function(x, width = 13L) {
  vapply(x, function(s) paste(strwrap(s, width = width), collapse = "\n"),
         character(1), USE.NAMES = FALSE)
}
COMBZ_COMPONENT_LABELS <- setNames(
  wrap2(as.character(combz_compdef$domain), width = 14L), combz_components)

# The canonical producer must have passed its own workbook parity gate.
if (!all(combz_parity$passed)) {
  stop("The canonical CombZ parity audit records a failure (",
       paste(combz_parity$quantity[!combz_parity$passed], collapse = ", "),
       "). Refusing to build a figure on an unverified outcome definition.",
       call. = FALSE)
}
# And the animal-level outcome must agree with the manuscript package that
# panels C-E are drawn from, or the figure would mix two outcome definitions.
combz_join <- combz %>%
  mutate(.k = canonical_animal_id(.data$AnimalNum)) %>%
  inner_join(animal %>% mutate(.k = canonical_animal_id(.data$AnimalID)) %>%
               select(".k", s16_combz = "CombZ", s16_group = "Group"), by = ".k")
if (nrow(combz_join) != nrow(animal)) {
  stop("The canonical CombZ table does not cover every analysed animal: ",
       nrow(combz_join), " of ", nrow(animal), ".", call. = FALSE)
}
combz_drift <- max(abs(combz_join$CombZ - combz_join$s16_combz))
if (combz_drift > TOL_ROUNDTRIP) {
  stop("The canonical CombZ producer and the manuscript package disagree by ",
       format(combz_drift), "; the figure would mix two outcome definitions.",
       call. = FALSE)
}
group_relabels <- sum(combz_join$outcome_group != combz_join$s16_group)
if (group_relabels > 0L) {
  stop(group_relabels, " outcome-group label(s) differ between the canonical ",
       "producer and the manuscript package.", call. = FALSE)
}
cat("    canonical CombZ vs manuscript package: drift ", format(combz_drift),
    ", ", group_relabels, " label differences\n", sep = "")

fmt_num <- function(x, d = 3) formatC(as.numeric(x), format = "f", digits = d)
fmt_ci <- function(lo, hi, d = 3) paste0("[", fmt_num(lo, d), ", ", fmt_num(hi, d), "]")

facts <- list(
  n_total = nrow(animal),
  n_female = as.integer(sex_counts[["Female"]]),
  n_male = as.integer(sex_counts[["Male"]]),
  group_counts = group_counts,
  combz_components = unname(COMBZ_COMPONENT_LABELS[combz_components]),
  # Two label vectors on purpose: the schematic boxes need the line breaks, the
  # legend prose must not inherit them.
  combz_components_plain = unname(gsub("\n", " ",
                                       COMBZ_COMPONENT_LABELS[combz_components],
                                       fixed = TRUE)),
  combz_component_ids = combz_components,
  window_definition = paste0(
    as.character(window_ctr$primary_window_definition[1]), "; ",
    window_ctr$target_window_hours[1], " h at ",
    window_ctr$bin_size_min[1], "-min resolution (",
    window_ctr$expected_target_slots_per_animal[1], " expected slots)"),
  n_complete_window = as.integer(window_ctr$n_animals_complete_72_bins[1]),
  movement_unit = "the count of RFID position transitions per 10-min bin, averaged over the observed window bins",
  assoc_rho = fmt_num(assoc_row$spearman_rho, 3),
  assoc_ci = fmt_ci(assoc_row$spearman_boot_ci_low, assoc_row$spearman_boot_ci_high),
  assoc_q = formatC(assoc_row$spearman_p_bh, format = "e", digits = 2),
  pred_r2 = fmt_num(perf_head$cv_r2, 4),
  pred_perm_p = paste0("1/", perm_head$n_permutations[1] + 1L),
  pred_n_perm = paste0(perm_head$n_permutations[1], " full-refit outcome permutations (seed ",
                       perm_head$seed[1], ")"),
  pred_null_median = fmt_num(perm_head$null_median, 4),
  pred_null_interval = fmt_ci(perm_head$null_q025, perm_head$null_q975, 4),
  pred_repeated_r2 = fmt_num(perf_head$repeated_cv_mean_r2, 4),
  pred_repeated_interval = fmt_ci(perf_head$cv_r2_q025, perf_head$cv_r2_q975, 4),
  pred_interval_type = as.character(perf_head$interval_type[1]),
  pred_baseline_r2 = fmt_num(baseline_r2, 4),
  cv_scheme = as.character(perm_head$cv_scheme[1]),
  validation_scheme = unique(as.character(ho_primary$validation_scheme)),
  domain_family = paste(unique(as.character(domain$family_id)), collapse = " and "),
  domain_supported = sum(domain$q < 0.05, na.rm = TRUE),
  domain_n_cells = nrow(domain),
  # canonical outcome definition
  combz_definition_id = COMBZ_DEFINITION_ID,
  combz_n_components = length(combz_components),
  combz_parity_max_diff = format(max(combz_parity$max_abs_difference[
    combz_parity$quantity == "CombZ"])),
  combz_thresholds = paste(
    sprintf("%s %.4f", combz_thresh$Sex, combz_thresh$susceptibility_threshold),
    collapse = "; "),
  combz_sd_convention = unique(as.character(combz_thresh$sd_convention)),
  # panel A framework
  main_figure_panels = MAIN_FIGURE_PANELS,
  main_figure_stem = MAIN_FIGURE_STEM,
  panel_a_longitudinal_axis = "four repeated cage changes (CC1-CC4) x light phase",
  panel_a_domains = FRAMEWORK_DOMAINS$domain,
  # panel D real null
  pred_n_null_draws = sum(!perm_draws$is_observed &
                            perm_draws$model_id == HEADLINE_MODEL))

cat("    frozen facts: rho =", facts$assoc_rho, "| LOAO R2 =", facts$pred_r2,
    "| permutation p =", facts$pred_perm_p, "| null draws =",
    facts$pred_n_null_draws, "| domain cells supported:",
    facts$domain_supported, "of", facts$domain_n_cells, "\n")

# ------------------------------------------------------------- registration
figman <- list(); srcman <- list()

wsrc <- function(x, f) {
  write_csv(x, file.path(dirs$source_data, f)); f
}
# `stage` is the SCIENTIFIC OWNER, never the transport layer. Where the figure
# reads a validated export instead of the owner's own table, `source_stage`
# records that separately; see bmf_source_data() for why one column cannot
# carry both facts.
reg <- function(fm, panel, src_file, src_key, src_table, stage,
                note = NA_character_, source_stage = stage) {
  figman[[length(figman) + 1L]] <<- fm %>%
    mutate(panel_id = panel, scientific_owner_stage = stage,
           immediate_source_stage = source_stage, path_key = src_key,
           script = "27_build_behavior_main_figure.R", note = note)
  if (!is.na(src_file)) {
    srcman[[length(srcman) + 1L]] <<- tibble(
      figure_file = paste0(fm$stem, ".pdf"), panel = panel,
      source_data_file = src_file, scientific_owner_stage = stage,
      immediate_source_stage = source_stage,
      path_key = src_key, source_table = src_table)
  }
  invisible(fm)
}

W <- MMM_WIDTH_DOUBLE_MM

# ================================================================== PANEL A
# Integrated experimental design, RFID framework and later outcome definition.
# This panel merges what were previously two main-figure slots. It is a
# schematic: it states chronology and definitions and carries NO effect size,
# contrast, p or q value.
cat("  panel A: design, RFID framework and later outcome definition ...\n")


# Compact component labels for the schematic, derived from the producer's own
# component-definition table so they cannot drift from the endpoint definition.
COMPONENT_DISPLAY <- c(
  NOR = "Novel object recogn.",
  sucrose_pref = "Sucrose preference",
  weight_dev = "Body-weight dev.",
  delta_cort = "delta corticosterone",
  adrenal_weight = "Adrenal weight",
  spleen_weight = "Spleen weight")
missing_disp <- setdiff(combz_components, names(COMPONENT_DISPLAY))
if (length(missing_disp) > 0L) {
  stop("No display label for CombZ component(s): ",
       paste(missing_disp, collapse = ", "),
       ". The schematic must not invent one.", call. = FALSE)
}

# PROVENANCE WORDING. This distinguishes what the repository reproduces exactly
# (canonical component z-scores -> CombZ -> outcome group) from what it does NOT
# reconstruct (raw measurement -> component z-score). See section 23 of the
# editorial brief and docs/COMBZ_CANONICAL_DEFINITION.md.
COMBZ_PROVENANCE_SHORT <- paste(
  "Canonical CombZ and later outcome assignments are reproduced exactly from",
  "the historically defined component z-scores; the historical upstream",
  "standardization is preserved as source provenance, not redefined.",
  sep = "\n")

pA <- bmf_panel_a_framework(
  domains = FRAMEWORK_DOMAINS,
  components = unname(COMPONENT_DISPLAY[combz_components]),
  window_label = "first 12 h Active block after CC1\n(18:30-06:30, 10-min bins)",
  combz_label = "Later composite\nstress-burden score (CombZ)",
  direction_note = "lower CombZ = greater later stress burden",
  provenance_note = COMBZ_PROVENANCE_SHORT,
  n_animals = facts$n_total, group_counts = facts$group_counts)

# The CombZ distribution is no longer a main-figure element; it is retained as a
# panel candidate so it stays available without crowding panel a.
a_summary <- bmf_descriptive_summary(animal, "CombZ", c("Sex", "Group"))
pA_dist <- bmf_panel_a_distribution(
  animal, a_summary, y_lab = "CombZ (lower = greater later stress burden)")

# Panel A Source Data is definitional metadata rather than plotted points.
srcA <- bind_rows(
  bmf_source_data(FRAMEWORK_DOMAINS, "A", "framework",
                  "Analysis/27_build_behavior_main_figure.R :: FRAMEWORK_DOMAINS",
                  c("domain", "claim_status")) %>%
    mutate(row_role = "characterised_domain"),
  bmf_source_data(combz_compdef, "A", "canonical-endpoint",
                  "canonical/later_outcome_combz/tables/combz_component_definition.csv",
                  c("component", "xlsx_column", "domain", "raw_measure",
                    "raw_derivation", "sign_inverted", "higher_means",
                    "weight_in_composite", "reference_population_id",
                    "sd_convention", "reproducible_in_repository")) %>%
    mutate(row_role = "combz_component_definition"),
  bmf_source_data(combz_thresh, "A", "canonical-endpoint",
                  "canonical/later_outcome_combz/tables/combz_classification_thresholds.csv",
                  c("Sex", "n_control", "control_mean_combz",
                    "control_population_sd_combz", "susceptibility_threshold",
                    "sd_convention", "classification_rule_id")) %>%
    mutate(row_role = "classification_threshold"),
  bmf_source_data(combz_parity, "A", "canonical-endpoint",
                  "canonical/later_outcome_combz/tables/combz_workbook_parity_audit.csv",
                  c("quantity", "n_compared", "max_abs_difference", "tolerance",
                    "passed")) %>%
    mutate(row_role = "workbook_parity"),
  bmf_source_data(window_ctr, "A", "09",
                  "pipeline/09_early_prediction/10min/tables/early_window_contract_summary.csv",
                  c("primary_window_definition", "bin_level", "bin_size_min",
                    "target_window_hours", "expected_target_slots_per_animal",
                    "first_cage_change", "n_animals",
                    "n_animals_complete_72_bins")) %>%
    mutate(row_role = "analysed_window_identity"))
fA <- wsrc(srcA, "source_panel_a_framework_combz.csv")

# ============================ PANEL B (was C): first-night Movement
# Descriptive distribution of the early variable that panels c and d analyse.
# No categorical contrast is drawn and no significance is annotated: the
# repository's declared canonical categorical test on this window is null in
# both sexes, and the one nominally supported pooled-sex contrast is recorded
# as descriptive and non-claimable.

# The longitudinal movement panel that previously occupied a main slot is now
# an Extended Data candidate; it is built in the candidates section below.
long_cells <- longitudinal %>%
  filter(.data$CageChangeIndex %in% 1:4,
         .data$PhaseClass %in% c("Active", "Inactive")) %>%
  mutate(CageChangeIndex = as.integer(.data$CageChangeIndex))
bmf_assert_schema(long_cells, "Stage 03 longitudinal movement endpoints",
                  required = c("AnimalNum", "Group", "Sex", "mean_movement",
                               "Endpoint", "CageChangeIndex", "PhaseClass",
                               "n_bins"),
                  allowed = list(Group = BMF_GROUP_LEVELS, Sex = BMF_SEX_LEVELS,
                                 PhaseClass = c("Active", "Inactive")))


cat("  panel B: first-night Movement (descriptive) ...\n")

c_summary <- bmf_descriptive_summary(animal, "Movement_mean", c("Sex", "Group"))
MOVEMENT_LAB <- "Movement (transitions / 10 min)"
pC <- bmf_panel_c_first_night(animal, c_summary, y_lab = MOVEMENT_LAB)

srcC <- bind_rows(
  # Same owner/source split as panel d1: Stage 09's model frame DEFINES
  # Movement_mean; Stage 16's manuscript export is what this figure reads.
  bmf_source_data(animal, "B", owner_stage = "09",
                  owner_table = paste0("pipeline/09_early_prediction/10min/",
                                       "tables/model_ladder_input.csv"),
                  cols = c("AnimalID", "Sex", "Group", "Movement_mean"),
                  source_stage = "16",
                  source_table = "manuscript/behavior/animal_level_source_data.csv") %>%
    mutate(row_role = "animal"),
  bmf_source_data(c_summary, "B", "27 (descriptive summary of the plotted points)",
                  "derived from the animal rows in this same file",
                  c("Sex", "Group", "n_animals", "median", "q25", "q75", "mean",
                    "summary_type")) %>%
    mutate(row_role = "group_summary"))
fC <- wsrc(srcC, "source_panel_b_first_active_movement.csv")

# ============ PANEL C (was D): early Movement vs later CombZ - INFERENTIAL
cat("  panel C: early Movement vs later CombZ ...\n")

d_trend <- bmf_quantile_trend(animal, "Movement_mean", "CombZ", n_bins = 5L)
d_annot <- paste0(
  "Spearman rho = ", facts$assoc_rho, "\n95% CI ", facts$assoc_ci,
  "\nBH q = ", facts$assoc_q, ", n = ", facts$n_total)
pD <- bmf_panel_d_scatter(  # show_trend = FALSE: the quintile-median guide is non-monotone here,
  # so it reads as noise rather than as a guide. It is retained as a candidate.
  animal, d_trend, annotation = d_annot, show_trend = FALSE, x_lab = MOVEMENT_LAB,
  y_lab = "Later CombZ")

srcD <- bind_rows(
  bmf_source_data(animal, "C", owner_stage = "09",
                  owner_table = paste0("pipeline/09_early_prediction/10min/",
                                       "tables/model_ladder_input.csv"),
                  cols = c("AnimalID", "Sex", "Group", "Movement_mean", "CombZ"),
                  source_stage = "16",
                  source_table = "manuscript/behavior/animal_level_source_data.csv") %>%
    mutate(row_role = "animal"),
  # the quantile guide is NOT drawn in the main panel; its values are retained
  # here so the candidate version stays reproducible from Source Data
  bmf_source_data(d_trend, "C", "27 (descriptive summary; candidate panel only)",
                  "derived from the animal rows in this same file",
                  c("bin", "x_median", "y_median", "n_animals", "trend_type")) %>%
    mutate(row_role = "quantile_trend_candidate_only"))
fD <- wsrc(srcD, "source_panel_c_movement_vs_combz.csv")

# ============ PANEL D (was E): out-of-sample prediction - INFERENTIAL
cat("  panel D: out-of-sample prediction ...\n")

e_annot <- paste0(
  facts$validation_scheme, "\nheld-out R2 = ", facts$pred_r2)
pE1 <- bmf_panel_e_heldout(
  ho_primary, x_lab = "Observed CombZ",
  y_lab = "Held-out predicted CombZ", annotation = e_annot)

e_perf <- perm %>%
  transmute(model = .data$model, model_label = .data$model_label,
            observed_statistic = .data$observed_statistic,
            observed_statistic_name = .data$observed_statistic_name,
            null_median = .data$null_median, null_q025 = .data$null_q025,
            null_q975 = .data$null_q975, empirical_p = .data$empirical_p,
            n_permutations = .data$n_permutations, cv_scheme = .data$cv_scheme,
            seed = .data$seed) %>%
  left_join(perf %>% select("model" = "model_id", "reporting_role",
                            "repeated_cv_mean_r2", "cv_r2_q025", "cv_r2_q975",
                            "interval_type", "predictors", "n_animals"),
            by = "model") %>%
  # Row labels are shortened for the axis only; the full upstream model_label
  # and the predictor list both travel verbatim in the Source Data.
  mutate(row_label = if_else(
    .data$model == HEADLINE_MODEL, "Movement mean\n(headline)",
    "Movement mean +\nRMSSD + entropy ACF1")) %>%
  arrange(desc(.data$observed_statistic))

# Panel E right is now the REAL permutation-null distribution, drawn from the
# per-permutation statistics persisted by Stage 09. Nothing is reconstructed
# from published quantiles.
bmf_assert_schema(perm_draws, "Stage 09 persisted permutation draws",
                  required = c("permutation_id", "seed", "model_id",
                               "model_label", "feature_set", "endpoint",
                               "cv_scheme", "performance_metric",
                               "performance_value", "is_observed",
                               "n_permutations"),
                  finite = "performance_value")
draws_head <- perm_draws %>% filter(.data$model_id == HEADLINE_MODEL)
n_null_head <- sum(!draws_head$is_observed)
if (n_null_head != perm_head$n_permutations[1]) {
  stop("Panel E needs ", perm_head$n_permutations[1],
       " persisted null draws for the headline model but found ", n_null_head,
       ". Re-run Stage 09; the null must never be reconstructed from ",
       "published quantiles.", call. = FALSE)
}
# The persisted draws must reproduce the published summaries exactly, otherwise
# the figure and the table would disagree.
obs_head <- draws_head$performance_value[draws_head$is_observed]
null_head <- draws_head$performance_value[!draws_head$is_observed]
draw_checks <- tibble(
  quantity = c("observed_statistic", "null_median", "null_q025", "null_q975",
               "empirical_p"),
  from_draws = c(obs_head, stats::median(null_head),
                 stats::quantile(null_head, 0.025, names = FALSE),
                 stats::quantile(null_head, 0.975, names = FALSE),
                 (sum(null_head >= obs_head) + 1) / (length(null_head) + 1)),
  published = c(perm_head$observed_statistic[1], perm_head$null_median[1],
                perm_head$null_q025[1], perm_head$null_q975[1],
                perm_head$empirical_p[1])) %>%
  mutate(max_abs_difference = abs(.data$from_draws - .data$published))
if (any(draw_checks$max_abs_difference > TOL_ROUNDTRIP)) {
  stop("The persisted permutation draws do not reproduce the published ",
       "summaries:\n",
       paste(capture.output(print(as.data.frame(draw_checks))), collapse = "\n"),
       call. = FALSE)
}
cat("    persisted null draws reproduce all 5 published summaries (max diff ",
    format(max(draw_checks$max_abs_difference)), ")\n", sep = "")

e_annot2 <- paste0(
  "Grey bars: ", n_null_head, " full-refit outcome permutations (seed ",
  perm_head$seed[1], ").\n",
  # Describe the marks by their form, not their hue: the observed line is drawn
  # in the palette's neutral contrast ink precisely so it cannot be confused
  # with the SUS group colour used for animals in panel d1.
  "Dashed: null median. Solid: observed. Permutation p = ",
  facts$pred_perm_p, ".\n",
  "Leave-one-animal-out; internal validation, not external.")
pE2 <- bmf_panel_e_null_distribution(
  perm_draws, model_id = HEADLINE_MODEL,
  x_lab = "Out-of-sample R2 vs the mean", annotation = e_annot2)

# The summary-interval view is retained as a panel candidate, so the
# repeated-CV interval and the baseline stay available for review.
pE2_summary <- bmf_panel_e_performance(
  e_perf, baseline = baseline_r2, label_col = "row_label",
  x_lab = "Out-of-sample R2 vs the mean",
  annotation = paste0(
    "Grey band + tick: permutation null 2.5-97.5% and median.\n",
    "Point: observed. Thin bar: repeated-CV split-to-split\n",
    "quantiles, NOT a confidence interval. Dashed: intercept-only\n",
    "baseline. Permutation p = ", facts$pred_perm_p, " for both models."))

# Panel D Source Data is split by subpanel, so each file reproduces exactly one
# plot: d1 the held-out per-animal predictions, d2 the actual permutation draws.
srcD1 <- bind_rows(
  # THE ONE PLACE WHERE OWNER AND SOURCE DIFFER. Stage 09 fits the model and
  # runs the leave-one-animal-out loop, so it owns these predictions and owns
  # CLAIM_BEHAV_05. Stage 16 is a validated manuscript EXPORT layer: it filters
  # to the two canonical behavior-only models and renames observed/predicted to
  # observed_CombZ/predicted_CombZ, and contains no model fit, no predict() and
  # no arithmetic on the prediction columns. The values are bit-identical to
  # Stage 09's primary_prediction_predictions.csv, which the contract test
  # asserts with identical(). Recording both stages keeps the export layer from
  # reading as the scientific producer.
  bmf_source_data(ho_primary, "D", owner_stage = "09",
                  owner_table = paste0("pipeline/09_early_prediction/10min/",
                                       "tables/primary_prediction_predictions.csv"),
                  cols = c("AnimalID", "Sex", "Group", "observed_CombZ",
                           "predicted_CombZ", "model_id", "validation_scheme"),
                  source_stage = "16",
                  source_table = "manuscript/behavior/prediction_source_data.csv") %>%
    mutate(row_role = "heldout_prediction"),
  bmf_source_data(e_perf, "D", "09",
                  "pipeline/09_early_prediction/10min/tables/primary_prediction_{performance,permutation_test}.csv",
                  c("model", "model_label", "row_label", "reporting_role", "predictors",
                    "n_animals", "observed_statistic_name",
                    "observed_statistic", "null_median", "null_q025",
                    "null_q975", "empirical_p", "n_permutations", "cv_scheme",
                    "seed", "repeated_cv_mean_r2", "cv_r2_q025", "cv_r2_q975",
                    "interval_type")) %>%
    mutate(row_role = "cv_performance"))
fD1 <- wsrc(srcD1, "source_panel_d1_loao_predictions.csv")

# The ACTUAL persisted permutation draws that panel d2 plots, one row each.
srcD2 <- bmf_source_data(
  perm_draws, "D", "09",
  "pipeline/09_early_prediction/10min/tables/early_prediction_permutation_draws.csv",
  c("permutation_id", "seed", "model_id", "model_label", "feature_set",
    "endpoint", "cv_scheme", "performance_metric", "performance_value",
    "is_observed", "n_permutations")) %>%
  mutate(row_role = if_else(perm_draws$is_observed, "observed_statistic",
                            "permutation_draw"))
fD2 <- wsrc(srcD2, "source_panel_d2_permutation_null.csv")
srcE <- bind_rows(srcD1, srcD2)   # combined view for the zero-tolerance check

# --------------------------- Source Data must equal the plotted values at 0
# Panel a is definitional metadata, so it has no plotted numeric series to
# compare; panels b, c and d each do.
src_zero_checks <- tibble(
  panel = c("B", "C", "C", "D1", "D2"),
  check = c("Movement_mean", "Movement_mean", "CombZ", "predicted_CombZ",
            "permutation performance_value"),
  max_abs_difference = c(
    max(abs(srcC$Movement_mean[srcC$row_role == "animal"] - animal$Movement_mean)),
    max(abs(srcD$Movement_mean[srcD$row_role == "animal"] - animal$Movement_mean)),
    max(abs(srcD$CombZ[srcD$row_role == "animal"] - animal$CombZ)),
    max(abs(srcD1$predicted_CombZ[srcD1$row_role == "heldout_prediction"] -
              ho_primary$predicted_CombZ)),
    max(abs(srcD2$performance_value - perm_draws$performance_value))))
if (any(src_zero_checks$max_abs_difference != 0)) {
  stop("Source Data does not reproduce the plotted values exactly:\n",
       paste(capture.output(print(src_zero_checks)), collapse = "\n"),
       call. = FALSE)
}
cat("    Source Data equals plotted values at tolerance 0\n")

# =========================================================== INDIVIDUAL PANELS
cat("  exporting panels ...\n")

panel_specs <- list(
  list(p = pA, stem = "panel_a_framework_and_outcome_definition", w = W, h = 56,
       panel = "A", src = fA, key = "behavior.later_outcome_combz",
       tbl = "combz_component_definition.csv + combz_classification_thresholds.csv",
       stage = "canonical-endpoint + framework"),
  # `stage` is always the SCIENTIFIC OWNER. `src_stage` names the layer the
  # figure actually read, which for the animal-level and held-out tables is
  # Stage 16's validated manuscript export of Stage 09 values.
  list(p = pC, stem = "panel_b_first_active_movement", w = W * 0.42, h = 56,
       panel = "B", src = fC, key = "behavior.first_night_movement",
       tbl = "model_ladder_input.csv", stage = "09", src_stage = "16"),
  list(p = pD, stem = "panel_c_movement_vs_combz", w = W * 0.52, h = 58,
       panel = "C", src = fD, key = "behavior.first_night_combz_association",
       tbl = "primary_movement_entropyacf1_associations.csv", stage = "09",
       src_stage = "16"),
  # Panel d1 draws Stage 09's held-out predictions, read through the Stage 16
  # export, alongside Stage 09's own CV performance table. Stage 09 owns both.
  # path_key stays single-valued because it is what the input hash is computed
  # from.
  list(p = pE1, stem = "panel_d1_loao_predictions", w = W * 0.42, h = 58,
       panel = "D", src = fD1, key = "behavior.early_prediction_heldout",
       tbl = paste("09: primary_prediction_predictions.csv +",
                   "primary_prediction_{performance,permutation_test}.csv"),
       stage = "09",
       src_stage = "16 (held-out predictions) + 09 (CV performance)"),
  list(p = pE2, stem = "panel_d2_permutation_null", w = W * 0.52, h = 58,
       panel = "D", src = fD2, key = "behavior.early_prediction",
       tbl = "early_prediction_permutation_draws.csv", stage = "09"),
  # --- candidates, not main-figure elements ------------------------------
  list(p = pA_dist, stem = "candidate_combz_distribution_by_group", w = W * 0.42,
       h = 52, panel = "A-cand", src = NA_character_,
       key = "behavior.combz_definition",
       tbl = "animal_level_source_data.csv", stage = "canonical-endpoint",
       src_stage = "16"),
  list(p = pE2_summary,
       stem = "candidate_d2_null_summary_intervals", w = W * 0.40, h = 58,
       panel = "D-cand", src = NA_character_, key = "behavior.early_prediction",
       tbl = "primary_prediction_permutation_test.csv", stage = "09"))

for (s in panel_specs) {
  fm <- mmm_export_figure(s$p, dirs$figures_panels, s$stem, s$w, s$h,
                          png_preview = FALSE)
  reg(fm, s$panel, s$src, s$key, s$tbl, s$stage,
      source_stage = if (is.null(s$src_stage)) s$stage else s$src_stage)
}

# ============================================================== MAIN FIGURE
cat("  assembling main figure ...\n")

if (!requireNamespace("patchwork", quietly = TRUE)) {
  stop("patchwork is required to assemble the composed main figure. The ",
       "individual panels above were exported successfully; install patchwork ",
       "and re-run to produce the composite.", call. = FALSE)
}
library(patchwork)

tag <- function(p, letter) p + mmm_panel_label(letter) + mmm_tag_theme()

# FOUR panels. Row 1 is the full-width framework; row 2 pairs the descriptive
# distribution with the inferential association; row 3 is the prediction panel,
# internally split into d1 (held-out predictions) and d2 (permutation null).
# Panels c and d get the most area, which is where the central claims live.
#
# The column ratios are named rather than inlined because the panel-letter inset
# depends on them: patchwork gives every sub-plot its own 0-1 tag space, so the
# shared relative inset lands at a LARGER absolute x on the full-width row 1
# than on the narrower first columns of rows 2 and 3. Left uncorrected that put
# letter "a" about 1.5 pt right of "b" and "d". Scaling row 1's inset by the
# same first-column fraction puts all four letters flush at the left margin.
ROW_2_WIDTHS <- c(1, 1.42)
ROW_3_WIDTHS <- c(1, 1.30)
ROW_2_FIRST_COL_FRACTION <- ROW_2_WIDTHS[1] / sum(ROW_2_WIDTHS)

# tag() first, so the a-d tagging idiom stays greppable by the contract test,
# then re-state the tag theme to override just the inset for this wider row.
row_1 <- tag(pA, "a") +
  mmm_tag_theme(inset_x = MMM_TAG_INSET_X * ROW_2_FIRST_COL_FRACTION)

row_2 <- tag(pC, "b") + tag(pD, "c") +
  patchwork::plot_layout(widths = ROW_2_WIDTHS)

row_3 <- tag(pE1, "d") +
  (pE2 + theme(plot.margin = margin(3.5, 3.5, 3.5, 3.5))) +
  patchwork::plot_layout(widths = ROW_3_WIDTHS)

main_fig <- row_1 / row_2 / row_3 +
  patchwork::plot_layout(heights = c(1.16, 1, 1.05))

# Target <=170 mm, preferring 150-165 mm so the text stays at a comfortable
# size rather than being shrunk to fit.
MAIN_H <- 162
stopifnot(MAIN_H <= MMM_MAX_HEIGHT_MM)
stopifnot(MAIN_H <= MMM_MAX_HEIGHT_MM, identical(W, MMM_WIDTH_DOUBLE_MM))

# Retire the previous five-panel composition WITH provenance rather than
# silently overwriting it: it moves to a superseded_candidates directory beside
# a note recording why, so a reviewer can still find what it looked like.
superseded_dir <- file.path(pub_root, "figures", "superseded_candidates")
ensure_dir(superseded_dir)
PREVIOUS_STEM <- "behavior_outcome_and_early_prediction_main"
moved <- character()
for (ext in c("pdf", "svg", "png")) {
  for (src_dir in c(dirs$figures_main, dirs$figures_previews)) {
    old <- file.path(src_dir, paste0(PREVIOUS_STEM, ".", ext))
    if (file.exists(old)) {
      dest <- file.path(superseded_dir,
                        paste0(PREVIOUS_STEM, "__five_panel_superseded.", ext))
      if (file.rename(old, dest)) moved <- c(moved, basename(dest))
    }
  }
}
# The five-panel era also wrote panel and Source Data files under names this
# architecture no longer produces. Left in place they would be unreferenced by
# every manifest yet indistinguishable from live output - the same "quoting a
# dead run" hazard the source audit flags upstream. They are RETIRED (moved,
# never deleted) by an explicit enumeration, so a re-run is deterministic.
SUPERSEDED_PANEL_STEMS <- c(
  "panel_a_combz_definition_schematic", "panel_a_combz_distribution",
  "panel_b_rfid_overview", "panel_b_domain_inventory",
  "panel_b_rfid_domain_overview", "panel_c_first_active_movement",
  "panel_d_movement_vs_combz", "panel_d_movement_vs_combz_rank_space",
  "panel_d_movement_vs_combz_by_sex_descriptive",
  "panel_e_out_of_sample_prediction_heldout",
  "panel_e_out_of_sample_prediction_null",
  "panel_e_out_of_sample_prediction_null_summary_intervals")
SUPERSEDED_SOURCE_FILES <- c(
  "source_panel_a_combz_definition.csv", "source_panel_b_rfid_overview.csv",
  "source_panel_b_rfid_domain_overview.csv",
  "source_panel_c_first_active_movement.csv",
  "source_panel_d_movement_vs_combz.csv",
  "source_panel_e_out_of_sample_prediction.csv")

live_stems <- vapply(panel_specs, `[[`, character(1), "stem")
for (stem in setdiff(SUPERSEDED_PANEL_STEMS, live_stems)) {
  for (ext in c("pdf", "svg", "png")) {
    old <- file.path(dirs$figures_panels, paste0(stem, ".", ext))
    if (file.exists(old)) {
      dest <- file.path(superseded_dir, paste0(stem, "__superseded.", ext))
      if (file.rename(old, dest)) moved <- c(moved, basename(dest))
    }
  }
}
live_sources <- vapply(srcman, function(x) x$source_data_file, character(1))
sup_src_dir <- file.path(superseded_dir, "source_data")
for (f in setdiff(SUPERSEDED_SOURCE_FILES, live_sources)) {
  old <- file.path(dirs$source_data, f)
  if (file.exists(old)) {
    ensure_dir(sup_src_dir)
    if (file.rename(old, file.path(sup_src_dir, f))) moved <- c(moved, f)
  }
}

if (length(moved) > 0L) {
  writeLines(c(
    "# Superseded main-figure candidate",
    "",
    paste("The five-panel composition previously written as",
          paste0("`", PREVIOUS_STEM, ".*`"), "is retained here for provenance."),
    "",
    "It was superseded in the final editorial pass, which merged the separate",
    "design/CombZ schematic and longitudinal-context panels into a single",
    "integrated framework panel and reduced the main figure to four panels:",
    "",
    "  a  experimental design, RFID framework and later outcome definition",
    "  b  first-night Movement by later outcome group (descriptive)",
    "  c  early Movement vs later CombZ (inferential)",
    "  d  out-of-sample prediction of later CombZ (inferential)",
    "",
    paste("Reason for the merge: the source audit concluded",
          "B3_NOT_JUSTIFIED__USE_FRAMEWORK_SCHEMATIC, so a separate main-figure",
          "slot for a domain overview was not scientifically justified."),
    "",
    paste("The current main figure is",
          paste0("`figures/main/", MAIN_FIGURE_STEM, ".*`."))),
    file.path(superseded_dir, "README.md"))
  cat("    retired the previous five-panel composition (",
      length(moved), " files) with provenance\n", sep = "")
}

fm_main <- mmm_export_figure(main_fig, dirs$figures_main, MAIN_FIGURE_STEM,
                             W, MAIN_H, png_preview = FALSE)
reg(fm_main, paste(MAIN_FIGURE_PANELS, collapse = "-"), NA_character_, "multiple",
    "see source_data/behavior_main_figure_source_manifest.csv",
    "canonical-endpoint + 09",
    note = "composed four-panel main figure",
    source_stage = "canonical-endpoint + 09 + 16")

fm_prev <- mmm_save_pub(main_fig,
                        file.path(dirs$figures_previews,
                                  paste0(MAIN_FIGURE_STEM, ".png")),
                        W, MAIN_H, dpi = 200)
cat("    main figure:", W, "x", MAIN_H, "mm |", length(MAIN_FIGURE_PANELS),
    "panels\n")

# ================================================== ALTERNATIVE CANDIDATES
cat("  panel candidates and extended-data candidates ...\n")

# ED: longitudinal movement across the repeated cage changes. This previously
# occupied a main-figure slot; panel a's framework now carries the breadth role,
# so it is retained here as registry-cleared secondary characterisation.
b_summary <- bmf_descriptive_summary(
  long_cells, "mean_movement", c("Group", "CageChangeIndex", "PhaseClass"))
pED_long <- bmf_panel_b_longitudinal_movement(
  long_cells, b_summary, y_lab = "Movement (transitions / 10 min)") +
  labs(subtitle = paste(
    "EXTENDED DATA. Descriptive secondary characterisation; no contrast is",
    "drawn and no test is annotated.\nThe two phases are on independent y axes",
    "and must not be compared by eye."))
fm <- mmm_export_figure(pED_long, dirs$figures_ed,
                        "ed_candidate_longitudinal_movement_by_cage_change",
                        W * 0.62, 66, png_preview = FALSE)
srcED_long <- bind_rows(
  bmf_source_data(long_cells, "ED-long", "03",
                  "pipeline/03_movement_phase_stats/10min/tables/raw_movement_animal_level_endpoints.csv",
                  c("AnimalNum", "Sex", "Group", "CageChangeIndex", "PhaseClass",
                    "Endpoint", "n_bins", "mean_movement")) %>%
    mutate(row_role = "animal_cell"),
  bmf_source_data(b_summary, "ED-long",
                  "27 (descriptive summary of the plotted points)",
                  "derived from the animal rows in this same file",
                  c("Group", "CageChangeIndex", "PhaseClass", "n_animals",
                    "median", "q25", "q75", "mean", "summary_type")) %>%
    mutate(row_role = "group_summary"))
reg(fm, "ED-long", wsrc(srcED_long, "source_ed_candidate_longitudinal_movement.csv"),
    "behavior.longitudinal_movement", "raw_movement_animal_level_endpoints.csv",
    "03", note = "EXTENDED DATA: descriptive longitudinal characterisation")

# ED: the registry-cleared first-night domain heatmap (former candidate B1).
if ("first_night_domains" %in% ED_DOMAIN_OVERVIEWS) {
  extra_dom <- setdiff(unique(as.character(domain$Domain)),
                       MMM_FIRST_NIGHT_DISPLAYED_DOMAINS)
  if (length(extra_dom) > 0L) {
    stop("The first-night domain table would display domain(s) outside the ",
         "validated allowlist: ", paste(extra_dom, collapse = ", "),
         call. = FALSE)
  }
  pED_fn <- bmf_panel_b_domain_heatmap(
    domain, fdr_threshold = 0.05,
    domain_order = intersect(
      rev(c("Psychomotor activation", "Behavioral volatility / fragmentation",
            "Behavioral flexibility / predictability",
            "Active-phase adaptation / exploration",
            "Social spatial organization")),
      unique(as.character(domain$Domain)))) +
    labs(subtitle = paste0(
      "EXTENDED DATA. First active phase only. ", facts$domain_supported,
      " of ", facts$domain_n_cells, " cells FDR-supported;\nnot a multi-domain",
      " signature. Only 50 of 111 animals have a complete window."))
  fm <- mmm_export_figure(pED_fn, dirs$figures_ed,
                          "ed_candidate_first_night_domain_map", W * 0.58, 62,
                          png_preview = FALSE)
  srcED_fn <- bmf_source_data(
    domain, "ED-firstnight", "14",
    "12_systems_neuroscience_summary/.../first_night_group_contrasts.csv",
    c("Sex", "contrast", "Domain", "hedges_g", "estimate", "SE", "ci_low",
      "ci_high", "raw_p", "q", "family_id", "n_tests_in_family", "n_ref",
      "n_comp", "bin_level", "interpretation_guard")) %>%
    mutate(row_role = "domain_contrast")
  reg(fm, "ED-firstnight",
      wsrc(srcED_fn, "source_ed_candidate_first_night_domain_map.csv"),
      "behavior.rfid_domain_summary", "first_night_group_contrasts.csv", "14",
      note = "EXTENDED DATA: registry-cleared first-night domain map")
}

# C, rank space: the only line whose slope the canonical rank model licenses.
pD_rank <- bmf_panel_d_rank_space(
  animal, rho = as.numeric(assoc_row$spearman_rho),
  annotation = paste0("reference line slope = canonical rho = ", facts$assoc_rho,
                      "\nrank space; no regression fitted"),
  x_lab = "Rank of first-night Movement", y_lab = "Rank of later CombZ")
fm <- mmm_export_figure(pD_rank, dirs$figures_panels,
                        "candidate_c_movement_vs_combz_rank_space", W * 0.50, 58,
                        png_preview = FALSE)
reg(fm, "C-cand", NA_character_, "behavior.first_night_combz_association",
    "primary_movement_entropyacf1_associations.csv", "09",
    note = "candidate: rank-space companion, slope is the canonical rho")

# D, sex-facetted: DESCRIPTIVE ONLY. The canonical association is pooled and the
# formal feature-by-sex interaction is null, so this must never be presented as
# a sex-difference result.
sexint_row <- sex_int %>% filter(.data$feature == HEADLINE_FEATURE)
pD_sex <- bmf_panel_d_scatter(
  animal, d_trend, show_trend = FALSE,
  annotation = paste0(
    "DESCRIPTIVE sex split only.\nCanonical association is pooled; formal\n",
    "feature-by-sex interaction q = ",
    fmt_num(sexint_row$interaction_p_bh, 3)),
  x_lab = MOVEMENT_LAB, y_lab = "Later CombZ") +
  facet_wrap(~Sex)
fm <- mmm_export_figure(pD_sex, dirs$figures_panels,
                        "candidate_c_movement_vs_combz_by_sex_descriptive",
                        W * 0.60, 58, png_preview = FALSE)
reg(fm, "C-cand", NA_character_, "behavior.first_night_combz_association",
    "primary_feature_sex_interactions.csv", "09",
    note = "candidate: DESCRIPTIVE sex split; interaction is null, not a claim")

# ED: the broad, phase-resolved domain map (former candidate B2). EXTENDED DATA
# ONLY and structurally incapable of reaching the main figure, which has no
# domain-heatmap slot at all. See docs/BEHAVIOR_MAIN_FIGURE_SOURCE_AUDIT.md for
# the six reasons; the binding ones are that it is in no manuscript registry row
# and that all of its FDR-supported cells are Inactive.
broad_available <- FALSE
broad <- if ("broad_domain_map" %in% ED_DOMAIN_OVERVIEWS) {
  tryCatch(rd("behavior.rfid_domain_summary_broad", "domain_effects"),
           error = function(e) NULL)
} else NULL
if (!is.null(broad) && all(c("Domain", "PhaseClass", "Sex", "contrast",
                             "hedges_g", "FDR_q") %in% names(broad))) {
  broad_available <- TRUE
  broad_plot <- broad %>%
    filter(!is.na(.data$hedges_g)) %>%
    mutate(q = .data$FDR_q,
           # The two domain tables disagree on spacing around the slash, which
           # would silently split a row if they were ever aligned by name.
           Domain = str_squish(str_replace_all(.data$Domain, "/", " / ")),
           family_id = .data$FDR_family_id)
  pB_alt <- bmf_panel_b_domain_heatmap(
    broad_plot, fdr_threshold = 0.05,
    key_cols = c("Sex", "contrast", "Domain", "PhaseClass")) +
    facet_grid(rows = vars(.data$Sex), cols = vars(.data$PhaseClass)) +
    labs(subtitle = paste(
      "EXTENDED DATA CANDIDATE. Not registry-declared. Every FDR-supported",
      "cell here is Inactive, which\ndocs/KNOWN_LIMITATIONS.md item 3 forbids",
      "interpreting as biology. No confidence intervals are shipped."))
  fm <- mmm_export_figure(pB_alt, dirs$figures_ed,
                          "ed_candidate_broad_domain_map_phase_resolved",
                          W * 0.72, 78, png_preview = FALSE)
  srcB_alt <- bmf_source_data(
    broad_plot, "B-alt", "14",
    "12_systems_neuroscience_summary/5min_based/stats_tables/systems_sis_domain_effect_summary.csv",
    intersect(c("Domain", "PhaseClass", "Sex", "contrast", "n_ref_animals",
                "n_comp_animals", "hedges_g", "mixed_model_estimate",
                "mixed_model_SE", "mixed_model_p", "FDR_q",
                "n_tests_in_family", "FDR_family_id", "resolution"),
              names(broad_plot)))
  reg(fm, "B-alt", wsrc(srcB_alt, "source_ed_candidate_broad_domain_map.csv"),
      "behavior.rfid_domain_summary_broad", "systems_sis_domain_effect_summary.csv",
      "14", note = "EXTENDED DATA CANDIDATE ONLY - unregistered, Inactive cells forbidden as biology")
  cat("    broad domain alternative built as an extended-data candidate\n")
} else {
  cat("    broad domain alternative unavailable; main panel B unaffected\n")
}

cat("  writing tables, manifests, legends ...\n")

# ============================================================ KEY RESULTS
#
# A CLAIM ID AND A TEST ID ARE DIFFERENT THINGS and must not share a column.
# The upstream frozen-results table keys its rows by an ANALYSIS id
# (S09_ASSOC_Movement_mean, S09_PRED_movement_mean); those are test ids. The
# manuscript claims are CLAIM_BEHAV_01..05 and live in the claim trace.
# Writing the upstream ids into a column headed "Claim id" previously meant
# CLAIM_BEHAV_04 - the single headline number of this figure - appeared nowhere
# in the key-results table and could not be joined to the trace.
#
# Every inferential row now carries the claim it serves, plus a `Row role` that
# says HOW it serves it: the headline estimate, the rest of its BH family, a
# supporting model, or the permutation reference. Family and supporting rows
# share their claim id because they are what makes that claim's q and null
# interpretable; the role column keeps them from reading as separate claims.
KEY_RESULT_CLAIM <- c(
  S09_ASSOC_Movement_mean          = "CLAIM_BEHAV_04",
  S09_ASSOC_Movement_rmssd         = "CLAIM_BEHAV_04",
  S09_ASSOC_Entropy_acf1           = "CLAIM_BEHAV_04",
  S09_PRED_movement_mean           = "CLAIM_BEHAV_05",
  S09_PRED_primary_behavior_family = "CLAIM_BEHAV_05")
KEY_RESULT_ROLE <- c(
  S09_ASSOC_Movement_mean          = "headline_estimate",
  S09_ASSOC_Movement_rmssd         = "multiplicity_family_member",
  S09_ASSOC_Entropy_acf1           = "multiplicity_family_member",
  S09_PRED_movement_mean           = "headline_estimate",
  S09_PRED_primary_behavior_family = "supporting_model")

key_results <- bind_rows(
  frozen_res %>%
    filter(str_detect(.data$claim_id, "^S09_ASSOC_|^S09_PRED_")) %>%
    filter(!str_detect(.data$claim_id, "_sex$")) %>%
    transmute(
      Analysis = .data$analysis_domain, Endpoint = .data$endpoint,
      `Time window` = .data$time_window, Sex = .data$sex,
      `Model or contrast` = .data$contrast_or_model,
      `Effect size type` = .data$effect_size_type,
      Estimate = signif(.data$effect_size, 4),
      `95% CI` = if_else(is.na(.data$ci_low), NA_character_,
                         mmm_fmt_ci(.data$ci_low, .data$ci_high, 3)),
      `raw p` = mmm_fmt_p(.data$p_raw),
      `adjusted p` = mmm_fmt_p(.data$p_adjusted),
      `Adjustment` = .data$adjustment_method,
      `Multiplicity family` = .data$multiplicity_family,
      `n animals` = .data$n_animals,
      `Robustness status` = .data$robustness_status,
      `Claim id` = unname(KEY_RESULT_CLAIM[.data$claim_id]),
      `Test id` = .data$claim_id,
      `Model or feature id` = .data$source_row_key,
      `Row role` = unname(KEY_RESULT_ROLE[.data$claim_id]),
      `Figure panel` = if_else(str_detect(.data$claim_id, "^S09_ASSOC_"),
                               "c", "d")),
  # Panel b's descriptive group summary. Included because the figure shows the
  # distribution, but with no test, no q value and an explicit non-claim status,
  # so it can never be read as a promoted categorical result.
  c_summary %>%
    transmute(
      Analysis = "First-night Movement distribution (descriptive)",
      Endpoint = "Movement (transitions / 10 min)",
      `Time window` = "First active 12 h after first cage change",
      Sex = as.character(.data$Sex),
      `Model or contrast` = paste0("later outcome group ", .data$Group),
      `Effect size type` = "median [interquartile range] of animal values",
      Estimate = signif(.data$median, 4),
      `95% CI` = mmm_fmt_ci(.data$q25, .data$q75, 3),
      `raw p` = NA_character_, `adjusted p` = NA_character_,
      `Adjustment` = "not applicable (no test performed)",
      `Multiplicity family` = "none (descriptive)",
      `n animals` = .data$n_animals,
      `Robustness status` = paste("Descriptive distribution only; no categorical",
                                  "contrast is claimed or annotated"),
      `Claim id` = "CLAIM_BEHAV_03",
      `Test id` = NA_character_,
      `Model or feature id` = "not applicable (distribution only)",
      `Row role` = "descriptive_context",
      `Figure panel` = "b"),
  # The permutation reference for the headline prediction, carried through from
  # the persisted draws. The former row for the single FDR-supported domain cell
  # is deliberately NOT included: no main panel displays that contrast, so
  # listing it among the main-figure key results would imply a claim the figure
  # does not make. It remains in the Extended Data candidate and its Source Data.
  perm %>% filter(.data$model == HEADLINE_MODEL) %>%
    transmute(
      Analysis = "Prospective prediction: permutation reference",
      Endpoint = "CombZ",
      `Time window` = "First active 12 h after first cage change",
      Sex = "Pooled",
      `Model or contrast` = paste0(.data$model_label,
                                   " vs its outcome-permutation null"),
      `Effect size type` = paste0("null distribution of ",
                                  .data$observed_statistic_name),
      Estimate = signif(.data$null_median, 4),
      `95% CI` = mmm_fmt_ci(.data$null_q025, .data$null_q975, 4),
      `raw p` = mmm_fmt_p(.data$empirical_p),
      `adjusted p` = NA_character_,
      `Adjustment` = "None; empirical full-refit outcome permutation",
      `Multiplicity family` = "Model-specific full-refit outcome permutation",
      `n animals` = facts$n_total,
      `Robustness status` = paste0(
        .data$n_permutations, " permutations, seed ", .data$seed,
        "; null median and 2.5-97.5% quantiles recomputed from the persisted ",
        "draws and identical to the published summary"),
      `Claim id` = "CLAIM_BEHAV_05",
      `Test id` = "S09_PERM_movement_mean",
      `Model or feature id` = .data$model,
      `Row role` = "permutation_reference",
      `Figure panel` = "d")) %>%
  arrange(match(.data$`Figure panel`, c("b", "c", "d")),
          match(.data$`Row role`,
                c("headline_estimate", "multiplicity_family_member",
                  "supporting_model", "permutation_reference",
                  "descriptive_context")))

# Contract: no test id may leak into the claim column, every inferential row
# carries a claim, and both main-figure claims are present.
.kr_claims <- unique(na.omit(key_results$`Claim id`))
if (any(grepl("^S09_", .kr_claims)) ||
    !all(c("CLAIM_BEHAV_04", "CLAIM_BEHAV_05") %in% .kr_claims) ||
    anyNA(key_results$`Claim id`)) {
  stop("Key-results claim ids are malformed: every row needs a CLAIM_BEHAV_* ",
       "id, no S09_* test id may appear in the claim column, and both ",
       "CLAIM_BEHAV_04 and CLAIM_BEHAV_05 must be present.", call. = FALSE)
}
write_csv(key_results, file.path(dirs$tables_manuscript,
                                 "behavior_main_key_results.csv"))

# ================================================================ MANIFESTS
figure_manifest <- bind_rows(figman)
write_csv(figure_manifest, file.path(dirs$manifests, "figure_manifest.csv"))

input_manifest <- mmm_path_describe(root = project_root, hash = TRUE) %>%
  mutate(read_by_stage27 = paste(.data$path_key, .data$file_role, sep = "/") %in%
           names(inputs_seen))
write_csv(input_manifest,
          file.path(dirs$manifests, "behavior_main_figure_input_manifest.csv"))

src_manifest <- bind_rows(srcman) %>%
  mutate(
    resolution = case_when(
      scientific_owner_stage == "09" ~ "10min",
      scientific_owner_stage == "14" ~ "10min (5min backbone for non-HMM domains)",
      grepl("^03", scientific_owner_stage) ~ "10min",
      grepl("canonical-endpoint", scientific_owner_stage) ~
        "per animal; the composite has no time resolution",
      grepl("framework", scientific_owner_stage) ~
        "not applicable (measurement framework)",
      grepl("16", immediate_source_stage) ~ "10min (inherited)",
      TRUE ~ NA_character_),
    # Panel letters follow the FINAL four-panel architecture:
    #   A definitional framework | B descriptive | C and D inferential
    model_id = case_when(
      panel == "A" ~ "canonical_later_outcome_combz_v1",
      panel == "B" ~ "not applicable (distribution only)",
      panel == "C" ~ "stage09_spearman_movement_mean_vs_combz",
      panel == "D" ~ "stage09_primary_prediction_movement_mean",
      panel == "ED-long" ~ "stage03_longitudinal_movement_descriptive",
      panel == "ED-firstnight" ~ "stage14_first_night_domain_within_sex_marginal_contrasts",
      TRUE ~ "see source_table"),
    test_id = case_when(
      panel == "C" ~ "spearman_rank_correlation_with_bootstrap_ci",
      panel == "D" ~ "loao_out_of_sample_r2_with_full_refit_outcome_permutation",
      panel == "ED-firstnight" ~ "within_sex_domain_contrasts",
      TRUE ~ "none"),
    multiplicity_family = case_when(
      panel == "C" ~ "Three canonical primary feature associations (BH)",
      panel == "D" ~ "Model-specific full-refit outcome permutation",
      panel == "ED-firstnight" ~ facts$domain_family,
      TRUE ~ "none"),
    claim_class = case_when(
      panel == "A" ~ "OUTCOME_DEFINITION + DESCRIPTIVE_FRAMEWORK",
      panel == "B" ~ "DESCRIPTIVE_DISTRIBUTION",
      panel %in% c("C", "D") ~ "INFERENTIAL_MAIN",
      TRUE ~ "EXTENDED_DATA_CANDIDATE"),
    input_hash = mmm_file_sha256(
      vapply(path_key, function(k)
        tryCatch(mmm_path_get(k, root = project_root, required = FALSE)[[1]],
                 error = function(e) NA_character_), character(1))),
    output_hash = mmm_file_sha256(file.path(dirs$source_data, source_data_file)))
write_csv(src_manifest, file.path(dirs$source_data,
                                  "behavior_main_figure_source_manifest.csv"))

# ============================================================== CLAIM TRACE
claims <- tribble(
  # scientific_owner_stage is the stage whose code DEFINES the claimed
  # quantity. It is never the transport layer: panels b, c and d1 are read
  # through Stage 16's validated manuscript export, but Stage 16 fits no
  # model and owns no claim, so it appears in the Source Data provenance
  # columns and never here.
  ~claim_id, ~manuscript_claim, ~claim_class, ~scientific_owner_stage, ~owner_table,
  ~statistical_test, ~multiplicity_family, ~figure_panel, ~source_data_file,
  ~status,

  "CLAIM_BEHAV_01",
  "Later stress burden is summarized by the canonical composite score (CombZ).",
  "OUTCOME_DEFINITION",
  "canonical-endpoint (Analysis/build_later_outcome_combz.R)",
  "canonical/later_outcome_combz/tables/later_outcome_combz_animal_level.csv",
  paste0("Definitional. Unweighted mean of ", facts$combz_n_components,
         " direction-aligned component z-scores, plus a within-sex ",
         "control-referenced classification (", facts$combz_sd_convention, ")"),
  "none", "A", fA,
  paste0("SUPPORTED AS A DEFINITION. Canonical CombZ and later outcome ",
         "assignments are reproduced exactly from the historically defined ",
         "component z-scores: max |dCombZ| = ", facts$combz_parity_max_diff,
         ", 0 of 93 label mismatches, thresholds ", facts$combz_thresholds,
         ". Lower CombZ = greater later stress burden. The historical upstream ",
         "standardization (raw measurement -> component z-score) is preserved ",
         "as source provenance rather than redefined; see ",
         "docs/COMBZ_CANONICAL_DEFINITION.md."),

  "CLAIM_BEHAV_02",
  "RFID provides broad, continuous, longitudinal behavioural characterization.",
  "DESCRIPTIVE_FRAMEWORK",
  "framework (measurement inventory; no producer owns an inferential result)",
  "Analysis/27_build_behavior_main_figure.R :: FRAMEWORK_DOMAINS",
  "None. Measurement framework only: no contrast, no test, no effect size.",
  "none (descriptive)", "A", fA,
  paste0("DESCRIPTIVE FRAMEWORK ONLY - this claim must never inherit an ",
         "inferential status. Decision of the source audit: ",
         "B3_NOT_JUSTIFIED__USE_FRAMEWORK_SCHEMATIC. No multi-domain ",
         "longitudinal overview is constructible from currently validated ",
         "material, so panel a states WHICH domains are characterised from the ",
         "continuous record and each row carries its own claim_status. Latent ",
         "state and inactivity domains are listed as measured only; the ",
         "inactive-phase biological reading is barred by KNOWN_LIMITATIONS ",
         "item 3. Domain-contrast heatmaps are Extended Data candidates."),

  "CLAIM_BEHAV_03",
  "First-night Movement is the early candidate signal.",
  "DESCRIPTIVE_DISTRIBUTION",
  "09", "pipeline/09_early_prediction/10min/tables/model_ladder_input.csv",
  paste("Distribution display only. No categorical contrast is drawn. The",
        "declared canonical categorical test on this window is Stage 20's",
        "within-sex family, which is null in both sexes"),
  "none applied in this panel (Stage 20 family is null: PRIMARY_FIRST_ACTIVE__2Sex_x_3GroupContrasts)",
  "B", fC,
  paste("DESCRIPTIVE ONLY, and not annotated as a categorical finding. Three",
        "upstream implementations disagree, and the only nominally",
        "FDR-supported contrast is the pooled-sex Wilcoxon that the repository",
        "itself labels descriptive and non-claimable. The panel exists to show",
        "the distribution of the variable that panels c and d analyse; the",
        "early signal is carried by CLAIM_BEHAV_04 and CLAIM_BEHAV_05, which",
        "are continuous."),

  "CLAIM_BEHAV_04",
  "Early Movement relates continuously to later CombZ.",
  "INFERENTIAL_MAIN",
  "09", "pipeline/09_early_prediction/10min/tables/primary_movement_entropyacf1_associations.csv",
  "Spearman rank correlation with a 5000-iteration bootstrap CI, pooled across sex",
  "Three canonical primary feature associations (BH)", "C", fD,
  paste0("SUPPORTED. rho = ", facts$assoc_rho, ", bootstrap 95% CI ",
         facts$assoc_ci, ", BH q = ", facts$assoc_q, ", n = ", facts$n_total,
         ". The negative sign means greater early Movement goes with LOWER ",
         "CombZ, i.e. greater later stress burden. Pooled because the formal ",
         "feature-by-sex interaction is null; sex-stratified views are ",
         "descriptive candidates only. Association, not causation."),

  "CLAIM_BEHAV_05",
  "Early Movement contains internal out-of-sample predictive information about later CombZ.",
  "INFERENTIAL_MAIN",
  "09", "pipeline/09_early_prediction/10min/tables/primary_prediction_{performance,permutation_test,early_prediction_permutation_draws}.csv",
  paste0("Exhaustive leave-one-animal-out cross-validation; metric ",
         perm_head$observed_statistic_name[1], "; reference ", facts$pred_n_perm),
  "Model-specific full-refit outcome permutation", "D",
  paste(fD1, fD2, sep = "; "),
  paste0("SUPPORTED, INTERNAL OUT-OF-SAMPLE VALIDATION ONLY. Held-out R2 = ",
         facts$pred_r2, " against a null of ", facts$pred_n_null_draws,
         " persisted full-refit permutation draws (median ",
         facts$pred_null_median, ", 2.5-97.5% ", facts$pred_null_interval,
         "), permutation p = ", facts$pred_perm_p,
         ". The grouping unit is the animal only - not cage, social group or ",
         "batch. The repeated-CV interval is a split-to-split resampling ",
         "interval, NOT a bootstrap CI. This is internal validation, not ",
         "external validation and not an independent cohort, and R2 of this ",
         "magnitude is informative rather than accurate prediction. No causal ",
         "claim."))

stopifnot(nrow(claims) == 5L,
          setequal(claims$figure_panel, MAIN_FIGURE_PANELS),
          !any(claims$claim_class[claims$claim_id %in%
                 c("CLAIM_BEHAV_02", "CLAIM_BEHAV_03")] == "INFERENTIAL_MAIN"))
write_csv(claims, file.path(dirs$audit, "behavior_main_claim_trace.csv"))

# Stage 26 must remain the owner of the temporal characterisation, and must
# keep disclaiming the prospective claim, which is CLAIM_BEHAV_04/05 here.
s26_boundary <- gamm_claims %>%
  filter(str_detect(.data$status, "OWNED BY STAGE 09"))
if (nrow(s26_boundary) < 1L) {
  stop("Stage 26 no longer disclaims the prospective claim. The claim ",
       "boundary between the GAMM temporal characterisation and the Stage 09 ",
       "prospective analysis must be restored before this figure is used.",
       call. = FALSE)
}
write_csv(s26_boundary, file.path(dirs$audit, "stage26_claim_boundary_carried.csv"))

# ============================================================ CONTRACT CHECK
every_panel <- MAIN_FIGURE_PANELS
traced <- unique(claims$figure_panel)
if (!all(every_panel %in% traced)) {
  stop("Every figure panel must have a claim-trace row. Missing: ",
       paste(setdiff(every_panel, traced), collapse = ", "), call. = FALSE)
}
palette_used <- MMM_GROUP_COLOURS[BMF_GROUP_LEVELS]
stopifnot(identical(unname(palette_used),
                    c("#3E3C6F", "#C6C3BB", "#E63A48")))

all_outputs <- list.files(pub_root, recursive = TRUE, full.names = TRUE)
mmm_assert_publication_path_budget(all_outputs, "Stage 27 publication tree")

flat <- unlist(lapply(list(key_results, claims, src_manifest),
                      function(x) as.character(unlist(x))))
stopifnot(!any(grepl("MODEL_INADEQUATE", flat, fixed = TRUE)))
stopifnot(!any(grepl("DO_NOT_INTERPRET", flat, fixed = TRUE)))

# ============================================================= DOCS + RECORD
bmf_write_legend_draft(
  file.path(dirs$legends, "behavior_main_figure_legend_draft.md"), facts)
bmf_write_readme(file.path(pub_root, "README.md"), facts,
                 input_manifest, MMM_GROUP_COLOURS)

saveRDS(list(stage = STAGE, facts = facts, palette = MMM_GROUP_COLOURS,
             n_figures = nrow(figure_manifest),
             inputs = inputs_seen,
             source_zero_checks = src_zero_checks,
             stage16_vs_stage09_drift = drift),
        file.path(dirs$audit, "stage27_build_record.rds"))

cat("\nStage 27 complete ->", pub_root, "\n")
cat("  figures:", nrow(figure_manifest),
    "| source data files:", length(unique(bind_rows(srcman)$source_data_file)),
    "| inputs read:", length(inputs_seen),
    "| claims traced:", nrow(claims), "\n")
cat("  longest output path:", max(nchar(all_outputs)), "chars\n")
if (broad_available) cat("  broad domain alternative: extended-data candidate written
")
