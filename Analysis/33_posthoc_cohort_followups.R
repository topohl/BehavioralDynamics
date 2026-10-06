# ================================================================
# Stage 33 - post hoc cohort follow-ups (modules A-E)
# MMMSociability -- Analysis/33_posthoc_cohort_followups.R
# ================================================================
# Implements the frozen plan docs/STAGE33_POSTHOC_COHORT_FOLLOWUPS_PLAN_v1.0.md (frozen at d914f10). POST HOC,
# descriptive estimation only: no tests, no reported p-values, not a re-test of any registered Stage 29/29b/30/32
# hypothesis. Manually invoked; in no runner profile.
#
#   Phase 0  arguments, opt-in, run id, working directory = local root, null graphics device
#   Phase 1  GC-1 code, GC-2 plan, GC-3 packages, GC-4 output refusals and path budget, GC-5 inputs (no data row before)
#   Phase 2  outcome-free: the shared design (GC-6), then modules D, E, C, B, A (Phase-2 parts); products hashed
#   Phase 3  outcomes read once (GC-7); modules A, B, C (placebo first), D, E
#   Phase 4  cross-module gates X1-X8, GC-8 tables, GC-9 lint, README
#   Phase 5  local staging, verification, input re-hash (GC-10), .tmp_ copy, rename, read-only, re-verification; GC-11
# Nothing is written to the project root before every gate has passed; no estimate is printed.
#
# Run from the repository root:
#   MMM_S33_REAL_RUN=<HEAD commit7> MMM_S33_A4_DIR=<a4 folder> [MMM_S33_LOCAL_STAGING=<folder>] [MMM_S33_RERUN_REASON=<text>]
#   Rscript Analysis/33_posthoc_cohort_followups.R --real --modules=ABCDE
# ================================================================

s33_main <- function(args) {
  repo <- normalizePath(getwd(), winslash = "/")
  if (!file.exists(file.path(repo, "Analysis", "33_posthoc_cohort_followups.R"))) stop("Run from the MMMSociability repository root.", call. = FALSE)
  suppressPackageStartupMessages({ library(data.table); library(digest) })
  source(file.path(repo, "Analysis", "_pipeline_setup.R"))
  source_mmm_helper("project_paths.R")
  for (h in c("stage33_common.R", "stage33_run.R")) source(file.path(repo, "Functions", h))
  T0 <- Sys.time(); timings <- list(); tick <- function(what) timings[[what]] <<- as.numeric(difftime(Sys.time(), T0, units = "secs"))
  # ---- Phase 0
  modules <- s33_parse_args(args)
  commit <- s33_git(repo, "rev-parse", "HEAD")[1]
  if (!grepl("^[0-9a-f]{40}$", commit) || !identical(Sys.getenv(S33_OPTIN_ENV), substr(commit, 1L, 7L)))
    stop("Set ", S33_OPTIN_ENV, " to the HEAD commit7 (", substr(commit, 1L, 7L), ") to run.", call. = FALSE)
  run_id <- s33_run_id(modules, commit)
  root <- mmm_project_root(); ep <- mmm_endpoint_source_root(); pl <- file.path(dirname(ep), "Planning")
  sl <- file.path(dirname(root), "SLEAPanalyzer_v2/releases/2026-09-22_exp9_all-batches_canonical-complete-figures-v3")
  a4 <- Sys.getenv("MMM_S33_A4_DIR", "")
  if ("A" %in% modules && !nzchar(a4)) stop("Module A needs MMM_S33_A4_DIR.", call. = FALSE)
  local_root <- normalizePath(Sys.getenv("MMM_S33_LOCAL_STAGING", file.path(tools::R_user_dir("MMMSociability", "cache"), "stage33")),
                              winslash = "/", mustWork = FALSE)
  dir.create(local_root, recursive = TRUE, showWarnings = FALSE)
  old_wd <- setwd(local_root); on.exit(setwd(old_wd), add = TRUE)
  grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
  message("Stage 33 run ", run_id, " (modules ", paste(modules, collapse = ""), ") at ", commit)
  status0 <- s33_git(repo, "status", "--porcelain", "--ignored", "--untracked-files=all")
  # ---- Phase 1
  G <- list(); addg <- function(g) { G[[length(G) + 1L]] <<- g; s33_stop_on_gates(g); invisible(g) }
  stage33_files <- c("Analysis/33_posthoc_cohort_followups.R", "Functions/stage33_common.R", "Functions/stage33_run.R",
                     paste0("Functions/", c("stage33_a_components.R", "stage33_b_presis.R", "stage33_c_cage.R", "stage33_d_hourly.R", "stage33_e_mates.R")),
                     S33_PLAN$path, "Analysis/_pipeline_setup.R", "Functions/project_paths.R", "Functions/behavioral_dynamics_helpers.R")
  canon_info <- s33_load_canonical_id(file.path(repo, "Functions", "behavioral_dynamics_helpers.R"))
  cfg_env <- new.env(); sys.source(file.path(repo, "Functions", "behavior_analysis_config.R"), envir = cfg_env)
  addg(s33_code_gates(repo, stage33_files, canon_info, cfg_env$MMM_BEHAVIOR_CONFIG))
  # frozen helpers are evaluated only after GC-1, from the bytes just checked
  assign("canonical_animal_id", canon_info$fun, envir = globalenv())
  for (h in c("behavior_analysis_config.R", "rfid_canonical_inference.R", "rfid_event_stream.R", "rfid_binfree_metrics.R", "stage30_movement.R",
              "stage30_figure_bundle.R", "stage32_windows.R", "stage32_inference.R", "stage32_run.R",
              "stage33_a_components.R", "stage33_b_presis.R", "stage33_c_cage.R", "stage33_d_hourly.R", "stage33_e_mates.R"))
    sys.source(file.path(repo, "Functions", h), envir = globalenv())
  addg(s33_plan_gates(repo))
  addg(s33_package_gates())
  out_root <- s33_out_root(root)
  earlier <- s33_output_refusals(out_root, run_id, modules, Sys.getenv("MMM_S33_RERUN_REASON", ""))
  bud <- s33_path_budget(normalizePath(out_root, winslash = "/", mustWork = FALSE), run_id, s33_planned_outputs(modules))
  lr <- tolower(local_root)
  addg(s33_gate_rows("GC-4", "run id valid; no run folder or .tmp_ entry; rerun rule; output paths <= 240; local root outside the project and repository",
                     c(grepl(S33_RUN_ID_PATTERN, run_id), bud$ok, !startsWith(lr, tolower(normalizePath(root, winslash = "/", mustWork = FALSE))),
                       !startsWith(lr, tolower(repo))), detail = paste("longest", bud$longest)))
  spec <- s33_input_spec(root, ep, pl, sl, if ("A" %in% modules) a4 else NA_character_, repo)
  spec <- spec[module %in% c("core", modules)]
  ig <- s33_input_gates(spec); addg(ig$gate)
  v2 <- s33_v2_files(root); ig2 <- s33_input_gates(v2); addg(ig2$gate)
  spec <- rbind(spec, v2); input_table <- rbind(ig$table, ig2$table)
  paths <- s33_paths(spec, root)
  tick("phase1")
  # ---- Phase 2 (outcome-free)
  design <- s33_design(paths, canon_info$fun); addg(design$gates)
  ctx <- list(root = root, ar = file.path(root, "analysis_ready"), ep = ep, pl = pl, sl = sl, a4 = a4, repo = repo, local_root = local_root,
              run_id = run_id, modules = modules, partners = modules, seeds = S33_SEEDS, B = S33_B,
              checkpoint_dir = file.path(local_root, "checkpoints"), checkpoint_log = new.env(), inputs = paths, run_mode = "real",
              head = commit, input_sha256 = input_table$observed_sha256, canon = canon_info$fun)
  p2 <- list(); prod_hashes <- list()
  for (m in intersect(c("D", "E", "C", "B", "A"), modules)) {
    fn <- get(paste0("s33", tolower(m), "_phase2"))
    p2[[m]] <- fn(design, ctx)
    for (nm in names(p2[[m]]$products)) s33_assert_outcome_free(p2[[m]]$products[[nm]], nm)
    addg(p2[[m]]$gates)
    prod_hashes[[m]] <- s33_product_hashes(p2[[m]]$products, m)
    tick(paste0("phase2_", m))
  }
  # ---- Phase 3 (outcomes read once)
  ao <- s33_attach_outcomes(design, paths, canon_info$fun); addg(ao$gates)
  results <- list()
  for (m in intersect(c("A", "B", "C", "D", "E"), modules)) {
    fn <- get(paste0("s33", tolower(m), "_phase3"))
    results[[m]] <- fn(ao$design_out, p2[[m]], ctx)
    if (nrow(results[[m]]$gates)) addg(results[[m]]$gates)
    tick(paste0("phase3_", m))
  }
  # ---- Phase 4
  results <- s33_cross_fill(results, modules)
  addg(s33_cross_gates(results, p2, modules))
  addg(s33_table_gates(results, modules))
  tables <- unlist(lapply(modules, function(m) c(results[[m]]$tables, results[[m]]$audit)), recursive = FALSE)
  gates_all <- data.table::rbindlist(G, fill = TRUE)
  gate_counts <- sprintf("%d recorded before the lint (GC-9) and the git-status record (GC-11), %d hard passed, %d not evaluated; every row in audit/gate_results.csv",
                         nrow(gates_all), sum(gates_all$hard %in% TRUE & gates_all$passed %in% TRUE), sum(!(gates_all$evaluated %in% TRUE)))
  readme <- s33_readme(run_id, modules, commit, S33_TABLES[modules], gate_counts, deviations = s33_deviations(modules), earlier = earlier,
                       rerun_reason = Sys.getenv("MMM_S33_RERUN_REASON", ""), partial = !setequal(modules, LETTERS[1:5]),
                       interval_counts = s33_interval_counts(results, modules))
  lint <- s33_lint(tables, readme, s33_lint_patterns(cfg_env$MMM_BEHAVIOR_CONFIG), exempt_columns = s33_lint_exempt_columns(modules))
  addg(s33_gate_rows("GC-9", "lint: no barred word, p column or SIS-minus-CON column", nrow(lint) == 0L,
                     n_expected = 1L, detail = if (nrow(lint)) paste(lint$where[1], lint$pattern[1]) else ""))
  addg(s33_registry_gate(data.table::rbindlist(G, fill = TRUE), modules))
  tick("phase4")
  # ---- Phase 5
  ck <- data.table::rbindlist(mget(sort(ls(ctx$checkpoint_log)), envir = ctx$checkpoint_log), fill = TRUE)
  status1 <- s33_git(repo, "status", "--porcelain", "--ignored", "--untracked-files=all")
  G[[length(G) + 1L]] <- s33_gate_rows("GC-11", "git status unchanged since Phase 1 (recorded; the worktree is shared)", identical(status0, status1), hard = FALSE)
  files <- s33_assemble_files(results, list(repo = repo, run_id = run_id, commit = commit, root = root, modules = modules,
    stage33_files = stage33_files, earlier = earlier, rerun_reason = Sys.getenv("MMM_S33_RERUN_REASON", ""), timings = timings,
    input_table = input_table, prod_hashes = prod_hashes, gates_all = data.table::rbindlist(G, fill = TRUE), lint = lint,
    checkpoints = ck, readme = readme))
  pre_copy <- function() { s33_output_refusals(out_root, run_id, modules, Sys.getenv("MMM_S33_RERUN_REASON", "")); TRUE }
  w <- s33_write_run(local_root, out_root, run_id, files, spec, pre_copy)
  message("Stage 33 written: ", w$dir, " (", nrow(w$manifest), " files; manifest sha256 ", w$manifest_sha256, ")")
  invisible(w)
}

if (sys.nframe() == 0L) s33_main(commandArgs(trailingOnly = TRUE))
