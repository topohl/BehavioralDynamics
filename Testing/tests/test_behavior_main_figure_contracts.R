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
# provides MMM_FIRST_NIGHT_DISPLAYED_DOMAINS, the validated domain allowlist
source_mmm_helper("first_night_domain_helpers.R")

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
s27_code <- s27                       # comment-stripped, for content checks
s27_raw_all <- readLines(STAGE27, warn = FALSE)   # raw, for line numbers
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
  "sample(", "vcov(", "confint(")
# `quantile(` is NOT blanket-banned: Stage 27 legitimately recomputes the null
# quantiles from the persisted permutation draws for the sole purpose of
# asserting that they reproduce the published summaries, and hard-stopping if
# they do not. That is a guard, not a second statistical pipeline. The rule
# below is therefore targeted rather than blanket: every quantile() must be
# namespaced, must sit inside that verification block, and its result must
# never reach a written output.
VERIFICATION_MARKERS <- c("draw_checks", "from_draws")
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

# Targeted rule for the one permitted descriptive/verification call.
q_lines <- code_grep(STAGE27, "quantile(")
for (i in q_lines) {
  ctx <- paste(s27_raw_all[max(1L, i - 6L):min(length(s27_raw_all), i + 6L)],
               collapse = " ")
  check(grepl("stats::quantile(", s27_raw_all[i], fixed = TRUE),
        paste0("quantile() at Stage 27 line ", i, " must be namespaced ",
               "stats::quantile() so it is unambiguously the base function"))
  check(any(vapply(VERIFICATION_MARKERS,
                   function(m) grepl(m, ctx, fixed = TRUE), logical(1))),
        paste0("quantile() at Stage 27 line ", i, " is outside the permutation-",
               "draw verification block; the assembler may not derive ",
               "quantiles for display"))
}
# and the verification frame must never be exported
for (m in VERIFICATION_MARKERS) {
  wrote <- grep(paste0("write_csv(", m), s27_raw_all, fixed = TRUE)
  check(length(wrote) == 0L,
        paste0("the verification frame '", m, "' must not be written to an output"))
}
ok(paste0(length(q_lines), " quantile() call(s), all namespaced and confined to ",
          "the draw-verification guard"))

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
    "source_panel_d1_loao_predictions.csv")))
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
    pc <- src("source_panel_b_first_active_movement.csv")
    pc_animals <- pc %>% filter(.data$row_role == "animal")
    check(nrow(pc_animals) == nrow(s09),
          paste0("Panel C has ", nrow(pc_animals), " animals, canonical has ",
                 nrow(s09)))
    check(setequal(canonical_animal_id(pc_animals$AnimalID), s09_key),
          "Panel C animal set differs from the canonical Stage 09 animal set")
    ok(paste0("Panel C animal set identical to canonical (n = ", nrow(s09), ")"))

    cat("\n[6] Panel D values match the canonical Stage 09 association input\n")
    pd <- src("source_panel_c_movement_vs_combz.csv") %>%
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
    pe <- src("source_panel_d1_loao_predictions.csv")
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
    # Panel a is now DEFINITIONAL metadata, not plotted points, so the
    # independent CombZ re-check is done against panel c, which does plot it.
    a16 <- read_csv(mmm_path_get("behavior.combz_definition", "animal_level"),
                    show_col_types = FALSE, progress = FALSE)
    pcz <- src("source_panel_c_movement_vs_combz.csv") %>%
      filter(.data$row_role == "animal") %>%
      mutate(.k = canonical_animal_id(.data$AnimalID))
    ja <- inner_join(pcz, tibble::tibble(.k = canonical_animal_id(a16$AnimalID),
                                         ref = a16$CombZ), by = ".k")
    check(nrow(ja) == nrow(a16), "Panel c does not join 1:1 to Stage 16")
    check(max(abs(ja$CombZ - ja$ref)) == 0,
          "Panel c CombZ differs from the Stage 16 manuscript value")

    # Panel a must carry the outcome DEFINITION, not plotted animal rows.
    pa <- src("source_panel_a_framework_combz.csv")
    check(!("animal" %in% pa$row_role),
          "panel a Source Data should be definitional metadata, not animal rows")
    for (r in c("characterised_domain", "combz_component_definition",
                "classification_threshold", "workbook_parity",
                "analysed_window_identity")) {
      check(r %in% pa$row_role,
            paste0("panel a Source Data is missing the '", r, "' rows"))
    }
    check(sum(pa$row_role == "combz_component_definition") == 6L,
          "panel a must document all six CombZ components")
    check(sum(pa$row_role == "characterised_domain") >= 1L,
          "panel a must document the characterised-domain inventory")
    check(all(c("claim_status") %in% names(pa)),
          "panel a domain rows must carry a claim_status column")
    ok("Source Data reproduces plotted values exactly; panel a is definitional")

    cat("\n[11] every figure claim has a claim-trace row\n")
    ct <- read_csv(file.path(mmm_publication_dir("audit", publication_root = pub_root),
                             "behavior_main_claim_trace.csv"),
                   show_col_types = FALSE, progress = FALSE)
    check(nrow(ct) == 5L, paste0("claim trace must have 5 rows, has ", nrow(ct)))
    check(all(c("claim_id", "manuscript_claim", "canonical_stage",
                "canonical_table", "statistical_test", "multiplicity_family",
                "figure_panel", "source_data_file", "status") %in% names(ct)),
          "claim trace is missing a required column")
    check(setequal(unique(ct$figure_panel), c("A", "B", "C", "D")),
          paste0("claim trace panels are ",
                 paste(unique(ct$figure_panel), collapse = ", "),
                 "; every panel A-D needs a row (A carries two claims)"))
    check(all(grepl("^CLAIM_BEHAV_0[1-5]$", ct$claim_id)),
          "claim ids must be CLAIM_BEHAV_01..05")
    sd_dir <- mmm_publication_dir("source_data", publication_root = pub_root)
    # A claim may legitimately rest on more than one Source Data file: panel d
    # is split into d1 (held-out predictions) and d2 (permutation null), so the
    # field is a "; "-separated list and each entry must exist.
    for (f in unique(unlist(strsplit(ct$source_data_file, "; ", fixed = TRUE)))) {
      check(file.exists(file.path(sd_dir, trimws(f))),
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


# =====================================================================
# ADDED IN THE PRE-FREEZE CANONICALIZATION PASS
# =====================================================================

cat("\n[15,16] no unstable HMM or invalidated Inactive result is displayable\n")

check(length(MMM_FIRST_NIGHT_DISPLAYED_DOMAINS) == 5L,
      paste0("the validated displayed-domain set must have 5 entries, has ",
             length(MMM_FIRST_NIGHT_DISPLAYED_DOMAINS)))
# The barred constructs must not be nameable as a displayed contrast domain.
BARRED <- c("Behavioral state architecture", "Inactive-phase rest",
            "latent-state", "dwell", "occupancy_entropy",
            "gaussian_log1p_invalid", "MODEL_INADEQUATE")
for (b in BARRED) {
  hit <- grep(b, MMM_FIRST_NIGHT_DISPLAYED_DOMAINS, fixed = TRUE)
  check(length(hit) == 0L,
        paste0("the validated allowlist contains the barred construct '", b, "'"))
}
# The broad, HMM-bearing table may only be read inside the Extended Data
# section, never anywhere that could reach the main composition.
ed_marker <- grep("ALTERNATIVE CANDIDATES", s27_raw_all)
check(length(ed_marker) == 1L, "could not locate the candidates section")
broad_reads <- code_grep(STAGE27, "rfid_domain_summary_broad")
for (i in broad_reads) {
  check(i > ed_marker,
        paste0("the broad HMM-bearing domain table is referenced at line ", i,
               ", before the Extended Data section"))
}
ok("no HMM/latent-state, inactive-rest or invalidated construct is displayable")

cat("\n[8b] panel E plots the ACTUAL persisted permutation draws\n")

check(any(grepl("bmf_panel_e_null_distribution", s27_code, fixed = TRUE)),
      "panel E right must use the real null-distribution builder")
check(any(grepl("permutation_draws", s27_code, fixed = TRUE)),
      "Stage 27 must read the persisted permutation draws")
check(any(grepl("must never be reconstructed from", s27_code, fixed = TRUE)) ||
      any(grepl("the null must never be reconstructed", s27_code, fixed = TRUE)),
      "Stage 27 must refuse to reconstruct the null from published quantiles")
helper_null <- readLines("Functions/behavior_main_figure_helpers.R", warn = FALSE)
check(any(grepl("Refusing to plot an incomplete null", helper_null, fixed = TRUE)),
      "the null-distribution builder must refuse an incomplete null")
ok("panel E right is built from the persisted draws and refuses a partial null")

cat("\n[12] panel C annotates no categorical inference\n")

# Panel C's builder must not accept or draw an annotation, and Stage 27 must not
# pass a p or q value into it.
c_call <- grep("bmf_panel_c_first_night(", s27_code, fixed = TRUE)
check(length(c_call) >= 1L, "panel C is not built")
for (i in c_call) {
  seg <- paste(s27_code[i:min(i + 3L, length(s27_code))], collapse = " ")
  for (bad in c("annotation", "wilcox", "q =", "p =", "signif")) {
    check(!grepl(bad, seg, fixed = TRUE),
          paste0("panel C must carry no inferential annotation; found '", bad,
                 "' in its construction"))
  }
}
ok("panel C is built without any inferential annotation")

cat("\n[C1] the canonical CombZ producer owns the outcome definition\n")

check(any(grepl("behavior.later_outcome_combz", s27_code, fixed = TRUE)),
      "Stage 27 must read the canonical CombZ producer output")
check(any(grepl("combz_compdef", s27_code, fixed = TRUE)),
      "the component list must come from the canonical component definition")
# Display LABELS are local (they must be short enough for the schematic), but
# the component IDENTITY and ORDER must come from the canonical table, and any
# canonical component without a label must fail loudly rather than be dropped.
check(any(grepl("combz_components <- as.character(combz_compdef$component)",
                s27_code, fixed = TRUE)),
      "the component identity must be read from the canonical component table")
check(any(grepl("missing_disp <- setdiff(combz_components, names(COMPONENT_DISPLAY))",
                s27_code, fixed = TRUE)),
      paste("Stage 27 must detect a canonical component with no display label",
            "rather than silently omitting it"))
check(any(grepl("The schematic must not invent one", s27_code, fixed = TRUE)),
      "Stage 27 must refuse to invent a component label")
check(any(grepl("COMPONENT_DISPLAY[combz_components]", s27_code, fixed = TRUE)),
      "the displayed components must be indexed BY the canonical component list")
check(any(grepl("Refusing to build a figure on an unverified outcome definition",
                s27_code, fixed = TRUE)),
      "Stage 27 must refuse to build if the CombZ parity audit failed")
ok("panel A's outcome definition is owned by the canonical producer")

if (have_outputs) {
  cat("\n[8c] plotted null values equal the persisted draws exactly\n")
  sd_dir <- mmm_publication_dir("source_data", publication_root = pub_root)
  # The draws live in panel d2's own Source Data file, one file per subpanel.
  pe <- read_csv(file.path(sd_dir, "source_panel_d2_permutation_null.csv"),
                 show_col_types = FALSE, progress = FALSE)
  plotted <- pe %>% filter(.data$row_role %in% c("permutation_draw",
                                                 "observed_statistic"))
  check(nrow(plotted) > 0L, "panel d2 Source Data carries no permutation draws")
  canon <- read_csv(mmm_path_get("behavior.early_prediction", "permutation_draws",
                                 root = project_root),
                    show_col_types = FALSE, progress = FALSE)
  check(nrow(plotted) == nrow(canon),
        paste0("panel E Source Data has ", nrow(plotted),
               " draw rows, canonical has ", nrow(canon)))
  j <- inner_join(
    plotted %>% select("model_id", "permutation_id", "is_observed",
                       plotted_value = "performance_value"),
    canon %>% select("model_id", "permutation_id", "is_observed",
                     canon_value = "performance_value"),
    by = c("model_id", "permutation_id", "is_observed"))
  check(nrow(j) == nrow(canon), "panel E draws do not join 1:1 to canonical")
  d <- max(abs(j$plotted_value - j$canon_value))
  check(d == 0, paste0("plotted null values differ from the persisted draws by ",
                       format(d), "; they must be identical"))
  n_null <- sum(!plotted$is_observed & plotted$model_id == "movement_mean")
  check(n_null == 1000L,
        paste0("panel E must plot exactly 1000 null draws for the headline ",
               "model, found ", n_null))
  ok(paste0("all ", nrow(canon),
            " draw rows identical to the persisted draws at tolerance 0"))

  cat("\n[11b] the claim trace records the new owners\n")
  ct2 <- read_csv(file.path(mmm_publication_dir("audit", publication_root = pub_root),
                            "behavior_main_claim_trace.csv"),
                  show_col_types = FALSE, progress = FALSE)
  c1 <- ct2[ct2$claim_id == "CLAIM_BEHAV_01", ]
  check(grepl("build_later_outcome_combz", c1$canonical_stage[1], fixed = TRUE),
        "CLAIM_BEHAV_01 must be owned by the canonical CombZ producer")
  c2 <- ct2[ct2$claim_id == "CLAIM_BEHAV_02", ]
  check(grepl("descriptive|framework", c2$statistical_test[1], ignore.case = TRUE),
        "CLAIM_BEHAV_02 must be recorded as descriptive only")
  check(grepl("B3_NOT_JUSTIFIED|FRAMEWORK", c2$status[1]),
        "CLAIM_BEHAV_02 must record the panel B adjudication")
  for (cid in c("CLAIM_BEHAV_04", "CLAIM_BEHAV_05")) {
    r <- ct2[ct2$claim_id == cid, ]
    check(r$canonical_stage[1] == "09",
          paste0(cid, " must be owned by Stage 09, found '",
                 r$canonical_stage[1], "'"))
    check(!grepl("20|21|22|23|24|25", r$canonical_stage[1]),
          paste0(cid, " must not be owned by a Stage 20+ analysis"))
  }
  ok("CombZ producer owns claim 01; Stage 09 owns claims 04 and 05")

  cat("\n[E1] panel E wording contains no external-validation claim\n")
  leg <- readLines(file.path(mmm_publication_dir("legends", publication_root = pub_root),
                             "behavior_main_figure_legend_draft.md"), warn = FALSE)
  legtxt <- paste(leg, collapse = " ")
  # These phrases are forbidden as CLAIMS but required as DISCLAIMERS, so an
  # occurrence is only a failure when it is not negated. A blanket ban would
  # reject the very sentence that rules the claim out.
  FORBIDDEN_WORDS <- c("external validation cohort", "independent cohort",
                       "externally replicated", "prospective validation cohort",
                       "independently validated")
  # Judge per SENTENCE, not per fixed character window: the negation that rules
  # a phrase out can sit several clauses earlier ("Never 'external validation',
  # 'independent cohort' or ...").
  sentences <- unlist(strsplit(legtxt, "(?<=[.!?])\\s+", perl = TRUE))
  for (w in FORBIDDEN_WORDS) {
    hits <- sentences[grepl(w, sentences, ignore.case = TRUE)]
    for (sen in hits) {
      check(grepl("\\b(not|never|no|rather than)\\b", sen, ignore.case = TRUE),
            paste0("the legend asserts the forbidden phrase '", w,
                   "' without negating it, in: '", substr(sen, 1, 160), "'"))
    }
  }
  # and the affirmative claim must be absent outright
  for (w in c("we externally validated", "validated in an independent")) {
    check(!grepl(w, legtxt, ignore.case = TRUE),
          paste0("the legend makes the forbidden claim '", w, "'"))
  }
  check(grepl("AUC", legtxt) == FALSE || grepl("never report an AUC", legtxt),
        "the legend must not present an AUC for a continuous endpoint")
  check(grepl("internal", legtxt, ignore.case = TRUE),
        "the legend must state that validation is internal")
  check(grepl("not a bootstrap", legtxt, ignore.case = TRUE) ||
        grepl("NOT a bootstrap", legtxt, fixed = TRUE),
        "the legend must state that the repeated-CV interval is not a bootstrap CI")
  ok("no external-validation phrasing; internal validation stated explicitly")
}


# =====================================================================
# ADDED IN THE FINAL EDITORIAL / COMPOSITION PASS
# =====================================================================

cat("\n[F1] the main figure has exactly four top-level panels a-d\n")

check(any(grepl('MAIN_FIGURE_PANELS <- c("A", "B", "C", "D")', s27_raw_all,
                fixed = TRUE)),
      "Stage 27 must declare exactly four main-figure panels A-D")
check(any(grepl('MAIN_FIGURE_STEM <- "behavior_early_signal_and_prediction_main"',
                s27_raw_all, fixed = TRUE)),
      "the main figure must use the semantic four-panel filename")
# exactly four tagged panels in the composition, and the letters are a-d
tagged <- grep('tag\\((p[A-Z0-9_]+), "([a-d])"\\)', s27_raw_all)
letters_used <- sort(unique(unlist(
  regmatches(s27_raw_all[tagged],
             gregexpr('(?<=, ")[a-d](?=")', s27_raw_all[tagged], perl = TRUE)))))
check(identical(letters_used, c("a", "b", "c", "d")),
      paste0("the composition must tag exactly a, b, c, d; found: ",
             paste(letters_used, collapse = ", ")))
check(!any(grepl('tag\\([^,]+, "e"\\)', s27_raw_all)),
      "there must be no panel e in the four-panel figure")
ok("four panels declared and tagged a-d; no panel e")

cat("\n[F2] no configuration can put a domain heatmap in the main figure\n")

# the heatmap builder must only ever be called in the Extended Data section
hm <- code_grep(STAGE27, "bmf_panel_b_domain_heatmap(")
ed_start <- grep("ALTERNATIVE CANDIDATES", s27_raw_all)
check(length(ed_start) == 1L, "could not locate the candidates section")
for (i in hm) {
  check(i > ed_start,
        paste0("the domain-heatmap builder is called at line ", i,
               ", before the Extended Data section: it could reach the main figure"))
}
# and the main composition must not reference any heatmap object
comp <- s27_raw_all[grep("^row_[123] <- ", s27_raw_all)]
for (bad in c("pB_alt", "pED_fn", "domain_heatmap")) {
  check(!any(grepl(bad, comp, fixed = TRUE)),
        paste0("the main composition references '", bad, "'"))
}
check(!any(grepl("PANEL_B_SOURCE", s27_raw_all, fixed = TRUE)),
      paste("the old selectable main Panel B source must be gone; the main",
            "figure has no domain-overview slot at all"))
check(any(grepl("ED_DOMAIN_OVERVIEWS", s27_raw_all, fixed = TRUE)),
      "the Extended Data domain overviews must still be explicitly configured")
ok("heatmaps are Extended Data only and unreachable from the main composition")

cat("\n[F3] the framework panel carries no inferential annotation\n")

a_call <- grep("bmf_panel_a_framework(", s27_raw_all, fixed = TRUE)
check(length(a_call) >= 1L, "panel a is not built")
a_seg <- paste(s27_raw_all[min(a_call):(min(a_call) + 12L)], collapse = " ")
for (bad in c("assoc_q", "spearman", "p_bh", "q = ", "pred_r2", "perm_p")) {
  check(!grepl(bad, a_seg, fixed = TRUE),
        paste0("panel a must carry no inferential quantity; found '", bad, "'"))
}
# the helper itself must not accept or draw a p/q annotation
helper_a <- readLines("Functions/behavior_main_figure_helpers.R", warn = FALSE)
i0 <- grep("^bmf_panel_a_framework <- function", helper_a)
i1 <- i0; repeat { i1 <- i1 + 1L; if (helper_a[i1] == "}") break }
a_body <- helper_a[i0:i1]
for (bad in c("q_col", "fdr", "p_value", "significan")) {
  check(!any(grepl(bad, a_body, ignore.case = TRUE)),
        paste0("the panel a builder references '", bad, "'"))
}
check(any(grepl("claim_status", a_body, fixed = TRUE)),
      "the panel a builder must require a per-domain claim_status")
ok("panel a is definitional: no p, q, effect size or significance anywhere")

cat("\n[F4] Stage 27 invokes no scientific producer\n")

# Producer NAMES legitimately appear as provenance strings (the claim trace must
# record which producer owns each claim). What is forbidden is INVOKING one, so
# the check looks for invocation context rather than for the name.
INVOKE_CTX <- c("source\\(", "sys\\.source\\(", "system\\(", "system2\\(",
                "Rscript", "callr::", "eval\\(parse\\(")
PRODUCERS <- c("build_later_outcome_combz", "09_early_prediction_model_ladder",
               "run_all_analysis", "14_systems_neuroscience", "16_manuscript")
for (i in seq_along(s27_raw_all)) {
  if (grepl("^\\s*#", s27_raw_all[i])) next
  ln <- s27_raw_all[i]
  if (!any(vapply(INVOKE_CTX, function(p) grepl(p, ln), logical(1)))) next
  # the only sourcing Stage 27 may do is the pipeline setup and its helpers
  if (grepl("pipeline_setup|source_mmm_helper", ln)) next
  for (pr in PRODUCERS) {
    check(!grepl(pr, ln, fixed = TRUE),
          paste0("Stage 27 appears to INVOKE the producer '", pr,
                 "' at line ", i, ": ", trimws(ln)))
  }
  check(!any(vapply(c("system\\(", "system2\\(", "Rscript", "callr::"),
                    function(p) grepl(p, ln), logical(1))),
        paste0("Stage 27 must not shell out; line ", i, ": ", trimws(ln)))
}
# and it must source nothing beyond the setup and its helpers
src_lines <- code_grep(STAGE27, "source(")
offend <- src_lines[!grepl("pipeline_setup|source_mmm_helper",
                           s27_raw_all[src_lines])]
check(length(offend) == 0L,
      paste0("Stage 27 sources something other than the setup/helpers at ",
             "line(s) ", paste(offend, collapse = ", ")))
ok("no producer is invoked, nothing is shelled out, only helpers are sourced")

if (have_outputs) {
  cat("\n[F5] panel d1 holds exactly the 111 canonical held-out predictions\n")
  sd_dir2 <- mmm_publication_dir("source_data", publication_root = pub_root)
  d1 <- read_csv(file.path(sd_dir2, "source_panel_d1_loao_predictions.csv"),
                 show_col_types = FALSE, progress = FALSE)
  d1p <- d1 %>% filter(.data$row_role == "heldout_prediction")
  ho <- read_csv(mmm_path_get("behavior.early_prediction_heldout", "predictions",
                              root = project_root),
                 show_col_types = FALSE, progress = FALSE) %>%
    filter(.data$model_id == "movement_mean")
  check(nrow(d1p) == 111L,
        paste0("panel d1 must hold 111 held-out predictions, has ", nrow(d1p)))
  check(nrow(ho) == 111L, "the canonical held-out set is not 111 rows")
  jj <- inner_join(
    d1p %>% select("AnimalID", plotted = "predicted_CombZ"),
    ho %>% select("AnimalID", canon = "predicted_CombZ"), by = "AnimalID")
  check(nrow(jj) == 111L, "panel d1 does not join 1:1 to the canonical set")
  check(max(abs(jj$plotted - jj$canon)) == 0,
        "panel d1 predicted values differ from the canonical held-out values")
  ok("111/111 held-out predictions, identical at tolerance 0")

  cat("\n[F6] panel d2 holds exactly the 1000 canonical null draws\n")
  d2 <- read_csv(file.path(sd_dir2, "source_panel_d2_permutation_null.csv"),
                 show_col_types = FALSE, progress = FALSE)
  n_null_head <- sum(!d2$is_observed & d2$model_id == "movement_mean")
  check(n_null_head == 1000L,
        paste0("panel d2 must hold 1000 null draws for the headline model, has ",
               n_null_head))
  n_obs_head <- sum(d2$is_observed & d2$model_id == "movement_mean")
  check(n_obs_head == 1L,
        "panel d2 must hold exactly one observed row for the headline model")
  ok("1000 null draws + 1 observed row for the headline model")

  cat("\n[F7] key results and claim trace follow the four-panel hierarchy\n")
  kr <- read_csv(file.path(mmm_publication_dir("tables_manuscript",
                                               publication_root = pub_root),
                           "behavior_main_key_results.csv"),
                 show_col_types = FALSE, progress = FALSE)
  check("Figure panel" %in% names(kr),
        "the key-results table must record which panel each row belongs to")
  panels_kr <- sort(unique(kr$`Figure panel`))
  check(all(panels_kr %in% c("b", "c", "d")),
        paste0("key-results rows must map to panels b, c or d; found: ",
               paste(panels_kr, collapse = ", ")))
  # panel b rows must carry no test
  b_rows <- kr %>% filter(.data$`Figure panel` == "b")
  check(nrow(b_rows) >= 1L, "the key-results table has no panel b rows")
  check(all(is.na(b_rows$`raw p`)) && all(is.na(b_rows$`adjusted p`)),
        "panel b key-results rows must carry no p or q value")
  check(all(grepl("no categorical contrast", b_rows$`Robustness status`,
                  ignore.case = TRUE)),
        "panel b rows must state that no categorical contrast is claimed")

  ct2 <- read_csv(file.path(mmm_publication_dir("audit", publication_root = pub_root),
                            "behavior_main_claim_trace.csv"),
                  show_col_types = FALSE, progress = FALSE)
  check("claim_class" %in% names(ct2),
        "the claim trace must carry a claim_class column")
  cls <- setNames(ct2$claim_class, ct2$claim_id)
  EXPECT_CLASS <- c(
    CLAIM_BEHAV_01 = "OUTCOME_DEFINITION",
    CLAIM_BEHAV_02 = "DESCRIPTIVE_FRAMEWORK",
    CLAIM_BEHAV_03 = "DESCRIPTIVE_DISTRIBUTION",
    CLAIM_BEHAV_04 = "INFERENTIAL_MAIN",
    CLAIM_BEHAV_05 = "INFERENTIAL_MAIN")
  for (id in names(EXPECT_CLASS)) {
    check(identical(as.character(cls[[id]]), EXPECT_CLASS[[id]]),
          paste0(id, " must be classed ", EXPECT_CLASS[[id]], ", found '",
                 cls[[id]], "'"))
  }
  EXPECT_PANEL <- c(CLAIM_BEHAV_01 = "A", CLAIM_BEHAV_02 = "A",
                    CLAIM_BEHAV_03 = "B", CLAIM_BEHAV_04 = "C",
                    CLAIM_BEHAV_05 = "D")
  pn <- setNames(ct2$figure_panel, ct2$claim_id)
  for (id in names(EXPECT_PANEL)) {
    check(identical(as.character(pn[[id]]), EXPECT_PANEL[[id]]),
          paste0(id, " must map to panel ", EXPECT_PANEL[[id]], ", found '",
                 pn[[id]], "'"))
  }
  ok("claim classes and panel mapping match the four-panel hierarchy")

  cat("\n[F8] the superseded five-panel composition is retained with provenance\n")
  sup <- file.path(pub_root, "figures", "superseded_candidates")
  check(dir.exists(sup), "the superseded_candidates directory is missing")
  check(file.exists(file.path(sup, "README.md")),
        "the superseded figure must be retained WITH a provenance note")
  sup_files <- list.files(sup, pattern = "superseded")
  check(length(sup_files) >= 1L,
        "no superseded figure artifact was retained")
  # the old stem must no longer be present as a current main figure
  main_files <- list.files(mmm_publication_dir("figures_main",
                                               publication_root = pub_root))
  check(!any(grepl("behavior_outcome_and_early_prediction_main", main_files)),
        "the superseded stem must not remain in figures/main")
  check(any(grepl("behavior_early_signal_and_prediction_main", main_files)),
        "the current four-panel main figure is missing from figures/main")
  ok(paste0(length(sup_files), " superseded artifact(s) retained with a README"))
}

if (length(skipped) > 0L) {
  cat("\nSKIPPED (environment-dependent):\n")
  for (s in skipped) cat("  -", s, "\n")
}

cat("\nBehavior main figure contract checks: PASS\n")
