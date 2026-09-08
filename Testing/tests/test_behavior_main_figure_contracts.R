# Contract tests for the behavior main figure (Stage 27) and its path layer.
#
# Follows the portable-suite idiom: a plain Rscript with fail()/check()/ok(),
# sourced from the repository root, no testthat, and no package beyond what
# .github/workflows/portable-tests.yml installs.
#
# The checks split into two classes:
#   * PORTABLE  - source-code and path-layer contracts. These run everywhere,
#                 including CI, and never touch the private project root.
#   * DATA      - value-identity contracts against the real canonical outputs.
#                 These are guarded on the project root being reachable and are
#                 reported as skipped otherwise, exactly as
#                 Testing/tests/test_output_path_length.R guards its Stage 16
#                 section. They are the ones that would catch a figure drifting
#                 away from the statistics it claims to display.

suppressPackageStartupMessages({ library(dplyr); library(stringr); library(readr) })

source("Analysis/_pipeline_setup.R")
source_mmm_helper("mmm_publication_theme.R")
source_mmm_helper("project_paths.R")
source_mmm_helper("behavior_main_figure_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok <- function(msg) cat("  ok  ", msg, "\n")

STAGE27 <- "Analysis/27_build_behavior_main_figure.R"
NEW_SOURCES <- c(STAGE27,
                 "Functions/project_paths.R",
                 "Functions/behavior_main_figure_helpers.R",
                 "Functions/behavior_main_figure_docs.R")
for (f in NEW_SOURCES) check(file.exists(f), paste0("missing new source file: ", f))

read_code <- function(path, drop_comments = TRUE) {
  x <- readLines(path, warn = FALSE)
  if (isTRUE(drop_comments)) x <- x[!grepl("^\\s*#", x)]
  x
}

#' Grep executable lines of a file, reporting ORIGINAL line numbers.
#'
#' Stripping comments before grepping (the Stage 26 precedent) shifts every line
#' number, which makes a failure message point at the wrong place. This keeps
#' the raw numbering and skips full-line comments instead.
code_grep <- function(path, pattern, fixed = TRUE) {
  raw <- readLines(path, warn = FALSE)
  is_comment <- grepl("^\\s*#", raw)
  hit <- grep(pattern, raw, fixed = fixed)
  hit[!is_comment[hit]]
}

PANEL_KEYS <- c(
  A = "behavior.combz_definition",
  B = "behavior.rfid_domain_summary",
  C = "behavior.first_night_movement",
  D = "behavior.first_night_combz_association",
  E = "behavior.early_prediction")

skipped <- character()

# =====================================================================
cat("\n[1] Stage 27 resolves all five canonical sources via the path layer\n")
# =====================================================================

for (p in names(PANEL_KEYS)) {
  key <- PANEL_KEYS[[p]]
  check(key %in% mmm_path_keys(),
        paste0("panel ", p, " key '", key, "' is not in the path registry"))
  paths <- mmm_path_get(key, required = FALSE)
  check(length(paths) >= 1L && all(nzchar(paths)),
        paste0("key '", key, "' resolved to nothing"))
  ok(paste0("panel ", p, " -> ", key, " (", length(paths), " file role(s))"))
}

# Panel E additionally needs the held-out per-animal predictions.
check("behavior.early_prediction_heldout" %in% mmm_path_keys(),
      "panel E held-out prediction key is not in the path registry")

# Stage 27 must obtain every input through the registry, never by pasting a
# stage directory together itself.
s27 <- read_code(STAGE27)
check(any(grepl("mmm_path_get(", s27, fixed = TRUE)),
      "Stage 27 does not call mmm_path_get(); it is not using the path layer")
check(!any(grepl("behavior_stage_tables(", s27, fixed = TRUE)) &&
      !any(grepl("behavior_stage_dir(", s27, fixed = TRUE)),
      paste("Stage 27 constructs stage directories directly; input paths must",
            "come from the semantic registry so a restructure edits one file"))
ok("Stage 27 requests inputs only by semantic key")

# The registry must never hand back a superseded or bundled copy.
BAD_SEGMENTS <- c("_quarantine", "quarantine_legacy", "_archive", "snapshot",
                  "/releases/", "_pre_hmm_identity_fix", "_erroneous")
all_registry <- unlist(lapply(mmm_path_keys(),
                              function(k) mmm_path_get(k, required = FALSE)))
for (seg in BAD_SEGMENTS) {
  hits <- all_registry[grepl(seg, all_registry, fixed = TRUE)]
  check(length(hits) == 0L,
        paste0("registry resolves a path containing '", seg, "': ",
               paste(hits, collapse = ", ")))
}
ok("no registry key resolves into a snapshot, quarantine, archive or release tree")

# =====================================================================
cat("\n[2] no maintainer-specific absolute path in the new sources\n")
# =====================================================================

MAINTAINER_PATTERNS <- c(
  "C:/Users", "C:\\\\Users", "c:/Users",
  "/c/Users", "//mdc-berlin", "\\\\\\\\mdc-berlin",
  "Lab_Member")
DATA_ROOT_LITERAL <- "S:/Lab_Member"

for (f in NEW_SOURCES) {
  code <- readLines(f, warn = FALSE)
  for (pat in MAINTAINER_PATTERNS) {
    hit <- grep(pat, code, fixed = TRUE)
    if (identical(f, "Functions/project_paths.R") &&
        identical(pat, "Lab_Member")) next
    check(length(hit) == 0L,
          paste0(f, " line(s) ", paste(hit, collapse = ", "),
                 " contain a maintainer-specific path pattern '", pat, "'"))
  }
}
ok("no user-home or UNC path in any new source file")

# The documented data-root default may appear in EXACTLY ONE place, and only as
# the getOption()/Sys.getenv() fallback constant.
for (f in setdiff(NEW_SOURCES, "Functions/project_paths.R")) {
  code <- readLines(f, warn = FALSE)
  hit <- grep(DATA_ROOT_LITERAL, code, fixed = TRUE)
  check(length(hit) == 0L,
        paste0(f, " hard-codes the data root at line(s) ",
               paste(hit, collapse = ", "),
               "; it must call mmm_project_root() instead"))
}
pp <- readLines("Functions/project_paths.R", warn = FALSE)
root_lines <- grep(DATA_ROOT_LITERAL, pp, fixed = TRUE)
root_code_lines <- root_lines[!grepl("^\\s*#", pp[root_lines])]
check(length(root_code_lines) == 1L,
      paste0("the data-root default must appear exactly once in executable code ",
             "in Functions/project_paths.R; found ", length(root_code_lines),
             " at line(s) ", paste(root_code_lines, collapse = ", ")))
check(grepl("MMM_PROJECT_ROOT_DEFAULT", pp[root_code_lines - 1L]) ||
      grepl("MMM_PROJECT_ROOT_DEFAULT", pp[root_code_lines]),
      "the single data-root literal must be the MMM_PROJECT_ROOT_DEFAULT constant")
ok("data-root default is centralised in one constant in project_paths.R")

# It must be overridable by BOTH conventions that already exist in the repo.
# Whatever the caller configured is saved and restored, so probing the override
# mechanism cannot change which root the data section below actually reads --
# an earlier version of this test unset the variable and silently re-pointed
# itself at the default root.
ROOT_ENV_VARS <- c("MMM_BEHAVIOR_PROJECT_ROOT", "MMM_PROJECT_ROOT")
old_opt <- getOption("mmm.project_root")
old_env <- vapply(ROOT_ENV_VARS, Sys.getenv, character(1), unset = NA_character_)
restore_roots <- function() {
  options(mmm.project_root = old_opt)
  for (v in ROOT_ENV_VARS) {
    if (is.na(old_env[[v]])) Sys.unsetenv(v)
    else do.call(Sys.setenv, setNames(list(old_env[[v]]), v))
  }
}
on.exit(restore_roots(), add = TRUE)

options(mmm.project_root = NULL)
for (env_var in ROOT_ENV_VARS) {
  for (v in ROOT_ENV_VARS) Sys.unsetenv(v)
  do.call(Sys.setenv, setNames(list("X:/probe_root"), env_var))
  check(identical(mmm_project_root(), "X:/probe_root"),
        paste0("mmm_project_root() ignores ", env_var))
  ok(paste0("root override honoured: ", env_var))
}
for (v in ROOT_ENV_VARS) Sys.unsetenv(v)
options(mmm.project_root = "X:/probe_option")
check(identical(mmm_project_root(), "X:/probe_option"),
      "mmm_project_root() ignores getOption('mmm.project_root')")
check(identical(mmm_project_root(), "X:/probe_option"),
      "getOption must take precedence over the environment variables")
ok("root override honoured: getOption('mmm.project_root'), with precedence")
restore_roots()

# =====================================================================
cat("\n[3] Stage 27 does not recompute inferential statistics\n")
# =====================================================================
# Mirrors the Stage 26 precedent in test_gamm_manuscript_consistency.R: scan the
# comment-stripped source for calls that would make the assembler a second
# statistical pipeline.

BANNED_CALLS <- c(
  "bam(", "gam(", "glm(", "lm(", "lmer(", "gamm(",
  "p.adjust(", "cor.test(", "t.test(", "wilcox.test(", "chisq.test(",
  "aov(", "anova(", "emmeans", "boot(", "mvrnorm(", "predict(",
  "sample(", "vcov(", "confint(", "quantile(")
# geom_smooth would silently fit a model inside the plotting layer.
BANNED_LAYERS <- c("geom_smooth", "stat_smooth", "method = \"lm\"",
                   "method = \"loess\"", "stat_summary")

for (b in c(BANNED_CALLS, BANNED_LAYERS)) {
  hit <- code_grep(STAGE27, b)
  check(length(hit) == 0L,
        paste0("Stage 27 contains a banned inferential/model call '", b,
               "' at line(s) ", paste(hit, collapse = ", "),
               ". The assembler must carry statistics through, never derive them."))
}
ok(paste0("none of ", length(c(BANNED_CALLS, BANNED_LAYERS)),
          " banned calls present in Stage 27"))

# Redefinition of the science is banned. These are REGEX and deliberately
# narrow: `CombZ = ` also legitimately names an element of a diagnostic vector
# and a select() rename target, neither of which redefines anything, so only
# real assignment and real column creation are forbidden.
REDEFINITION_PATTERNS <- c(
  "(^|[^_.$[:alnum:]])CombZ\\s*<-",
  "\\b(mutate|transmute)\\s*\\(\\s*CombZ\\s*=",
  "\\$CombZ\\s*<-",
  "\\browMeans\\s*\\(",
  "\\b(Group|Sex)\\s*<-\\s*(case_when|if_else|ifelse)",
  "select_primary_active_window",
  "mmm_select_first_night_window")
for (b in REDEFINITION_PATTERNS) {
  hit <- code_grep(STAGE27, b, fixed = FALSE)
  check(length(hit) == 0L,
        paste0("Stage 27 appears to redefine an upstream quantity, pattern '", b,
               "' at line(s) ", paste(hit, collapse = ", ")))
}
ok("Stage 27 does not recompute CombZ, redefine RES/SUS or reselect the window")

# The plotting helpers are allowed exactly the documented descriptive ops.
helper_code <- read_code("Functions/behavior_main_figure_helpers.R")
for (b in setdiff(c(BANNED_CALLS, BANNED_LAYERS),
                  c("quantile(", "predict("))) {
  hit <- code_grep("Functions/behavior_main_figure_helpers.R", b)
  check(length(hit) == 0L,
        paste0("panel helper contains banned call '", b, "' at line(s) ",
               paste(hit, collapse = ", ")))
}
check(all(grepl("stats::quantile\\(", grep("quantile(", helper_code,
                                           fixed = TRUE, value = TRUE))),
      paste("every quantile() in the panel helpers must be the explicit",
            "stats::quantile() descriptive summary of the plotted points"))
for (b in c("read_csv", "read.csv", "write_csv", "write.csv", "readRDS",
            "saveRDS", "ggsave")) {
  hit <- code_grep("Functions/behavior_main_figure_helpers.R", b)
  check(length(hit) == 0L,
        paste0("panel helpers must not do file I/O; found '", b,
               "' at line(s) ", paste(hit, collapse = ", ")))
}
ok("panel helpers are I/O-free and use only documented descriptive summaries")

# =====================================================================
cat("\n[9] fixed identity colours are exactly the manuscript palette\n")
# =====================================================================

EXPECTED_COLOURS <- c(CON = "#3E3C6F", RES = "#C6C3BB", SUS = "#E63A48")
check(identical(MMM_GROUP_COLOURS[c("CON", "RES", "SUS")], EXPECTED_COLOURS),
      paste0("MMM_GROUP_COLOURS changed: ",
             paste(names(MMM_GROUP_COLOURS), MMM_GROUP_COLOURS,
                   sep = "=", collapse = ", ")))
check(identical(BMF_GROUP_LEVELS, c("CON", "RES", "SUS")),
      "group display order must be CON, RES, SUS")
ok("CON #3E3C6F, RES #C6C3BB, SUS #E63A48 in canonical order")

# RES is light, so colour must never be the only encoding.
check(length(unique(MMM_GROUP_SHAPES[c("CON", "RES", "SUS")])) == 3L,
      "the three groups must have three distinct point shapes")
check(length(unique(MMM_GROUP_LINETYPES[c("CON", "RES", "SUS")])) == 3L,
      "the three groups must have three distinct line types")
check(!any(grepl("scale_colour_manual\\(|scale_fill_manual\\(", s27)),
      paste("Stage 27 must not define its own group colour scale; use",
            "mmm_scale_fill_group()/mmm_scale_colour_group()"))
ok("shape and line type carry group identity redundantly")

# =====================================================================
cat("\n[12] the publication output root is configurable without code change\n")
# =====================================================================

old_pub <- getOption("mmm.publication_root")
on.exit(options(mmm.publication_root = old_pub), add = TRUE)
probe <- file.path(tempdir(), "mmm_pub_probe")
options(mmm.publication_root = probe)
check(identical(mmm_publication_root(), normalizePath(probe, winslash = "/",
                                                      mustWork = FALSE)),
      "mmm_publication_root() ignores getOption('mmm.publication_root')")
for (k in names(MMM_PUBLICATION_SUBDIRS)) {
  d <- mmm_publication_dir(k)
  check(startsWith(d, normalizePath(probe, winslash = "/", mustWork = FALSE)),
        paste0("publication subdirectory '", k, "' does not follow the root"))
}
options(mmm.publication_root = NULL)
Sys.setenv(MMM_PUBLICATION_ROOT = probe)
check(identical(mmm_publication_root(), normalizePath(probe, winslash = "/",
                                                      mustWork = FALSE)),
      "mmm_publication_root() ignores MMM_PUBLICATION_ROOT")
Sys.unsetenv("MMM_PUBLICATION_ROOT")
ok("publication root follows both getOption() and the environment variable")

check(any(grepl("mmm_publication_dir(", s27, fixed = TRUE)),
      "Stage 27 must obtain output directories from mmm_publication_dir()")
check(!any(grepl("analysis_ready", s27, fixed = TRUE)),
      paste("Stage 27 must not mention the analysis_ready tree; output location",
            "is the path layer's business"))
ok("Stage 27 writes only through the configured publication root")

# Every intended output path must fit the repository path budget.
budget_paths <- unlist(lapply(names(MMM_PUBLICATION_SUBDIRS), function(k)
  file.path(mmm_publication_dir(k, publication_root = mmm_publication_root(
    project_root = MMM_PROJECT_ROOT_DEFAULT)),
    "source_panel_e_out_of_sample_prediction.csv")))
mmm_assert_publication_path_budget(budget_paths, "Stage 27 intended tree")
check(max(nchar(budget_paths)) <= MMM_MAX_OUTPUT_PATH_CHARS,
      paste0("longest intended Stage 27 path (", max(nchar(budget_paths)),
             ") exceeds the ", MMM_MAX_OUTPUT_PATH_CHARS, "-char budget"))
ok(paste0("longest intended output path ", max(nchar(budget_paths)),
          " chars, budget ", MMM_MAX_OUTPUT_PATH_CHARS))

# Stage 27 must stay unregistered in the pipeline runner for this pass.
runner <- readLines("Analysis/run_all_analysis.R", warn = FALSE)
check(!any(grepl("27_build_behavior_main_figure", runner, fixed = TRUE)),
      "Stage 27 must NOT be registered in run_all_analysis.R in this pass")
ok("Stage 27 is not registered in run_all_analysis.R")

# =====================================================================
# DATA-DEPENDENT SECTION
# =====================================================================

project_root <- mmm_project_root()
pub_root <- mmm_publication_root(project_root = project_root)
have_inputs <- dir.exists(file.path(project_root, "analysis_ready"))
have_outputs <- dir.exists(pub_root)

if (!have_inputs) {
  skipped <- c(skipped, "[4]-[8],[10],[11] value-identity checks (project root not reachable)")
} else {

  cat("\n[4] the Stage 09 first-active window identity is unchanged\n")
  win <- read_csv(mmm_path_get("behavior.first_night_movement", "window_contract"),
                  show_col_types = FALSE, progress = FALSE)
  EXPECTED_WINDOW <- paste(
    "fixed clock window: first Active phase block after first cage change,",
    "18:30 inclusive to 06:30 exclusive")
  check(identical(as.character(win$primary_window_definition[1]), EXPECTED_WINDOW),
        paste0("Stage 09 window definition changed to: ",
               win$primary_window_definition[1]))
  check(win$bin_size_min[1] == 10, "primary window bin size must be 10 min")
  check(win$target_window_hours[1] == 12, "primary window must be 12 h")
  check(win$expected_target_slots_per_animal[1] == 72,
        "primary window must expect 72 slots per animal")
  check(as.character(win$first_cage_change[1]) == "CC1",
        "primary window must be anchored on CC1")
  check(win$n_inactive_rows_selected[1] == 0,
        "no Inactive row may enter the first-active window")
  check(win$n_animals[1] == 111, "primary window must cover 111 animals")
  # The figure must not overstate coverage.
  check(isFALSE(as.logical(win$all_target_slots_observed[1])),
        paste("this dataset does NOT have every target slot observed; if that",
              "ever becomes TRUE the legend wording must be revisited"))
  ok("window identity, resolution, anchor and coverage flags all unchanged")
  # Stage 27 builds the expected string with paste() across two source lines, so
  # assert on the distinctive fragments plus the guard that consumes them rather
  # than on one contiguous literal.
  s27_raw <- readLines(STAGE27, warn = FALSE)
  for (frag in c("fixed clock window: first Active phase block after first cage change,",
                 "18:30 inclusive to 06:30 exclusive",
                 "WINDOW_EXPECTED",
                 "primary_window_definition")) {
    check(any(grepl(frag, s27_raw, fixed = TRUE)),
          paste0("Stage 27 must pin the window identity; missing '", frag, "'"))
  }
  check(any(grepl("Refusing to assemble a figure against a redefined window",
                  s27_raw, fixed = TRUE)),
        "Stage 27 must hard-stop if the window identity ever changes")
  ok("Stage 27 pins and hard-stops on the window identity itself")

  if (!have_outputs) {
    skipped <- c(skipped, "[5]-[8],[10],[11] output checks (Stage 27 not yet run)")
  } else {
    src <- function(f) {
      p <- file.path(mmm_publication_dir("source_data",
                                         publication_root = pub_root), f)
      check(file.exists(p), paste0("missing Source Data file: ", p))
      read_csv(p, show_col_types = FALSE, progress = FALSE)
    }
    s09 <- read_csv(mmm_path_get("behavior.first_night_movement", "model_input"),
                    show_col_types = FALSE, progress = FALSE)
    s09_key <- canonical_animal_id(s09$AnimalNum)

    cat("\n[5] Panel C animal IDs match the canonical first-night animals\n")
    pc <- src("source_panel_c_first_active_movement.csv")
    pc_animals <- pc %>% filter(.data$row_role == "animal")
    check(nrow(pc_animals) == nrow(s09),
          paste0("Panel C has ", nrow(pc_animals), " animals, canonical has ",
                 nrow(s09)))
    check(setequal(canonical_animal_id(pc_animals$AnimalID), s09_key),
          "Panel C animal set differs from the canonical Stage 09 animal set")
    ok(paste0("Panel C animal set identical to canonical (n = ", nrow(s09), ")"))

    cat("\n[6] Panel D values match the canonical Stage 09 association input\n")
    pd <- src("source_panel_d_movement_vs_combz.csv") %>%
      filter(.data$row_role == "animal") %>%
      mutate(.k = canonical_animal_id(.data$AnimalID))
    ref <- tibble::tibble(.k = s09_key, ref_move = s09$Movement_mean,
                          ref_combz = s09$outcome)
    j <- dplyr::inner_join(pd, ref, by = ".k")
    check(nrow(j) == nrow(s09), "Panel D does not join 1:1 to the canonical input")
    # CSV text round-trip can lose the last bit of a double; the tolerance is
    # documented and deliberately tiny, not a licence for drift.
    TOL <- 1e-12
    dm <- max(abs(j$Movement_mean - j$ref_move))
    dz <- max(abs(j$CombZ - j$ref_combz))
    check(dm <= TOL, paste0("Panel D Movement_mean drift ", format(dm)))
    check(dz <= TOL, paste0("Panel D CombZ drift ", format(dz)))
    ok(paste0("Panel D matches canonical values (max drift ",
              format(max(dm, dz)), ", tolerance ", format(TOL), ")"))

    cat("\n[7] Panel E contains only held-out predictions\n")
    pe <- src("source_panel_e_out_of_sample_prediction.csv")
    pe_pred <- pe %>% filter(.data$row_role == "heldout_prediction")
    schemes <- unique(as.character(pe_pred$validation_scheme))
    check(length(schemes) >= 1L, "Panel E carries no validation scheme label")
    check(all(grepl("one-animal-out", schemes, ignore.case = TRUE)),
          paste0("Panel E contains a non-held-out validation scheme: ",
                 paste(schemes, collapse = "; ")))
    check(!any(grepl("in.?sample|training|fitted|apparent", schemes,
                     ignore.case = TRUE)),
          "Panel E validation scheme mentions in-sample or training fit")
    check(nrow(pe_pred) == length(unique(pe_pred$AnimalID)),
          "Panel E must hold exactly one held-out prediction per animal")
    check(setequal(canonical_animal_id(pe_pred$AnimalID), s09_key),
          "Panel E animal set differs from the canonical animal set")
    ok(paste0("all ", nrow(pe_pred), " Panel E predictions are held out (",
              paste(schemes, collapse = "; "), ")"))

    cat("\n[8] Panel E performance equals the canonical Stage 09 numbers\n")
    perf <- read_csv(mmm_path_get("behavior.early_prediction", "performance"),
                     show_col_types = FALSE, progress = FALSE)
    perm <- read_csv(mmm_path_get("behavior.early_prediction", "permutation"),
                     show_col_types = FALSE, progress = FALSE)
    pe_perf <- pe %>% filter(.data$row_role == "cv_performance")
    check(nrow(pe_perf) == nrow(perm),
          paste0("Panel E reports ", nrow(pe_perf), " models, canonical has ",
                 nrow(perm)))
    cmp <- pe_perf %>%
      inner_join(perm %>% select("model", ref_obs = "observed_statistic",
                                 ref_nullmed = "null_median",
                                 ref_lo = "null_q025", ref_hi = "null_q975",
                                 ref_p = "empirical_p", ref_nperm = "n_permutations"),
                 by = "model") %>%
      inner_join(perf %>% select("model" = "model_id",
                                 ref_rep = "repeated_cv_mean_r2",
                                 ref_rlo = "cv_r2_q025", ref_rhi = "cv_r2_q975"),
                 by = "model")
    check(nrow(cmp) == nrow(perm), "Panel E performance rows do not join to canonical")
    for (pair in list(c("observed_statistic", "ref_obs"),
                      c("null_median", "ref_nullmed"),
                      c("null_q025", "ref_lo"), c("null_q975", "ref_hi"),
                      c("empirical_p", "ref_p"),
                      c("repeated_cv_mean_r2", "ref_rep"),
                      c("cv_r2_q025", "ref_rlo"), c("cv_r2_q975", "ref_rhi"))) {
      d <- max(abs(cmp[[pair[1]]] - cmp[[pair[2]]]))
      check(d == 0,
            paste0("Panel E '", pair[1], "' differs from canonical by ", format(d),
                   "; it must be carried through unchanged"))
    }
    check(all(cmp$n_permutations == cmp$ref_nperm),
          "Panel E permutation count differs from canonical")
    ok(paste0("all 8 performance quantities equal canonical at tolerance 0 (",
              nrow(cmp), " models)"))

    cat("\n[10] Source Data equals the plotted values at tolerance 0\n")
    rec_path <- file.path(mmm_publication_dir("audit", publication_root = pub_root),
                          "stage27_build_record.rds")
    check(file.exists(rec_path), "Stage 27 build record is missing")
    rec <- readRDS(rec_path)
    check(!is.null(rec$source_zero_checks),
          "build record has no Source-Data-versus-plotted comparison")
    check(all(rec$source_zero_checks$max_abs_difference == 0),
          paste0("Stage 27 recorded a non-zero Source Data difference: ",
                 paste(rec$source_zero_checks$panel,
                       rec$source_zero_checks$max_abs_difference,
                       sep = "=", collapse = ", ")))
    check(identical(rec$palette[c("CON", "RES", "SUS")], EXPECTED_COLOURS),
          "the palette recorded at build time is not the manuscript palette")
    # Independent re-check against Stage 16, not via the build record.
    a16 <- read_csv(mmm_path_get("behavior.combz_definition", "animal_level"),
                    show_col_types = FALSE, progress = FALSE)
    pa <- src("source_panel_a_combz_definition.csv") %>%
      filter(.data$row_role == "animal") %>%
      mutate(.k = canonical_animal_id(.data$AnimalID))
    ja <- inner_join(pa, tibble::tibble(.k = canonical_animal_id(a16$AnimalID),
                                        ref = a16$CombZ), by = ".k")
    check(nrow(ja) == nrow(a16), "Panel A does not join 1:1 to Stage 16")
    check(max(abs(ja$CombZ - ja$ref)) == 0,
          "Panel A CombZ differs from the Stage 16 manuscript value")
    ok("Source Data reproduces plotted values exactly, palette recorded correctly")

    cat("\n[11] every figure claim has a claim-trace row\n")
    ct <- read_csv(file.path(mmm_publication_dir("audit", publication_root = pub_root),
                             "behavior_main_claim_trace.csv"),
                   show_col_types = FALSE, progress = FALSE)
    check(nrow(ct) == 5L, paste0("claim trace must have 5 rows, has ", nrow(ct)))
    check(all(c("claim_id", "manuscript_claim", "canonical_stage",
                "canonical_table", "statistical_test", "multiplicity_family",
                "figure_panel", "source_data_file", "status") %in% names(ct)),
          "claim trace is missing a required column")
    check(setequal(unique(ct$figure_panel), c("A", "B", "C", "D", "E")),
          paste0("claim trace panels are ",
                 paste(unique(ct$figure_panel), collapse = ", "),
                 "; every panel A-E needs a row"))
    check(all(grepl("^CLAIM_BEHAV_0[1-5]$", ct$claim_id)),
          "claim ids must be CLAIM_BEHAV_01..05")
    sd_dir <- mmm_publication_dir("source_data", publication_root = pub_root)
    for (f in unique(ct$source_data_file)) {
      check(file.exists(file.path(sd_dir, f)),
            paste0("claim trace names a missing Source Data file: ", f))
    }
    # The prospective claim must point at Stage 09, never at Stage 20.
    prosp <- ct %>% filter(.data$claim_id %in% c("CLAIM_BEHAV_04", "CLAIM_BEHAV_05"))
    check(nrow(prosp) == 2L, "the two prospective claims are missing")
    check(all(prosp$canonical_stage == "09"),
          paste0("the prospective claims must be owned by Stage 09; found: ",
                 paste(prosp$canonical_stage, collapse = ", ")))
    check(!any(grepl("Stage 20|stage20", prosp$canonical_table)),
          "Stage 20 must not own a prospective claim")
    # Panel C must not be sold as a categorical finding.
    c3 <- ct %>% filter(.data$claim_id == "CLAIM_BEHAV_03")
    check(grepl("NOT SUPPORTED|not annotated", c3$status[1]),
          paste("CLAIM_BEHAV_03 must record that the categorical contrast is",
                "not supported and not annotated"))
    ok("5 claims, all panels traced, prospective ownership with Stage 09")

    # The source manifest must be complete and self-describing.
    sm <- read_csv(file.path(sd_dir, "behavior_main_figure_source_manifest.csv"),
                   show_col_types = FALSE, progress = FALSE)
    REQ_MANIFEST <- c("panel", "source_data_file", "canonical_producer_stage",
                      "canonical_source_table", "resolution", "model_id",
                      "test_id", "multiplicity_family", "input_hash",
                      "output_hash")
    check(all(REQ_MANIFEST %in% names(sm)),
          paste0("source manifest missing column(s): ",
                 paste(setdiff(REQ_MANIFEST, names(sm)), collapse = ", ")))
    check(all(!is.na(sm$output_hash) & nzchar(sm$output_hash)),
          "every Source Data file must carry an output hash")
    ok(paste0("source manifest complete with ", nrow(sm), " rows and hashes"))
  }
}

if (length(skipped) > 0L) {
  cat("\nSKIPPED (environment-dependent):\n")
  for (s in skipped) cat("  -", s, "\n")
}

cat("\nBehavior main figure contract checks: PASS\n")
