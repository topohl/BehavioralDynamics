# ================================================================
# Stage 30 figure bundle: helpers (canonical source-data export for manuscript figures)
# MMMSociability -- Functions/stage30_figure_bundle.R
# ================================================================
# Pure constants and functions for Analysis/30b_stage30_figure_bundle.R (record: docs/STAGE30_FIGURE_BUNDLE_v1.md).
# Sourcing this file defines constants and functions only; it reads and writes nothing.
#
# NO INFERENCE. Nothing here fits, tests, refits, adjusts, smooths or predicts, and no p or q value is produced. The
# functions (a) hash-gate the frozen inputs, (b) copy frozen Stage 30 values, (c) rescale stored CIs with the stored
# standardization rule (deterministic unit rescaling, checked against the stored standardized estimate to 1e-12), and
# (d) compute descriptive summaries of declared plotting quantities only: counts, medians, quartiles (R quantile
# type 7), minima / maxima and centroid (arithmetic) means. The one derived line (S3d) is the COOKIE-CONT display line
# through the sex-resolved centroid with the frozen slope; no model is refitted.
# Gate order of a write: every pre-write gate (runner) -> staging folder (tables, H_provenance, H2_inputs, H3_gate_results
# = the pre-write gate table, 00_manifest) -> rename -> 0444 -> post-write gates (manifest re-verification, read-only) ->
# only then the BUNDLE_REGISTRY.csv row -> registration gate -> the complete gate table in logs/<bundle_id>_gate_results.csv.
# Requires: data.table, digest.
# ================================================================

# ---------------------------------------------------------------- frozen identities (DESIGN 2026-09-29 section 2)
S30FB_SCHEMA_VERSION <- "1"
S30FB_BUNDLE_PREFIX <- "s30b_v10"
S30FB_STATUS_REAL <- "FROZEN"
S30FB_STATUS_DRY <- "DRY_RUN_NOT_FOR_USE"
S30FB_STAGE30 <- list(
  run_name = "v1.0_be71e2f",
  run_commit = "be71e2f07bfefeeb13c1e1f82cbc62cff2e052dc",
  run_mode = "REAL_EXPLORATORY_POST_HOC_CONTEXT",
  registry_sha256 = "5c252bf699bc0f517c5508854d710bbaa014d4b6fe98d128eb0071bde89dd05e",
  registry_version = "1.0",
  output_manifest_sha256 = "77f271ad022f8fec9680f09e0f29540ce0c02189cd68143749100c9eac9d68d2",
  output_manifest_n = 27L,
  status_counts = "OK=48")
S30FB_STAGE29 <- list(
  bundle_id = "ebb_v101_20260929_b2ce507",
  manifest_sha256 = "ec59aa337fc63511de4aa2bf3e1ee1f4b34b84d6d671565f74e0bb3939f7226c")
# role in Stage 30 audit/input_hashes.csv -> pinned SHA-256 (the recorded and the on-disk hash must both equal it)
S30FB_INPUT_SHA256 <- c(
  "freeze_record:sus_list" = "dea3804b71b479f84900f946e5494b204ed0e8801498da459a6fc781beb9f391",
  "freeze_record:con_list" = "eddd2ee9c3a182f98211b4cbba689e2bb5f72985aeb178f72b25b7d25db1b75d",
  "freeze_record:combz_table" = "1f6a2a69c6b0781b8de8da50ecdadf309b62bbc9c82106c0fa91d18119a7de54",
  "inactivity_reference" = "a533b239852fbe46c8a4ff776754984f4fe1f29c0c4277082487c1546f7acf8a",
  "registry" = "5c252bf699bc0f517c5508854d710bbaa014d4b6fe98d128eb0071bde89dd05e")
S30FB_W104 <- list(file = "w1_04_gate_values.csv", sha_list = "w1_99_output_sha256.txt",
                   sha256 = "ae80b7e12beb2c4e6beb1570f8f45bd047937f88fcb1b8a207fe023b4efa46ef")

S30FB_STD_TOL <- 1e-12       # rescaled estimate vs stored standardized_estimate (absolute)
S30FB_ID_TOL <- 1e-9         # identity of copied values with Stage 29 / x4_01 (the Stage 30 recomputation tolerance)
S30FB_QUANTILE_TYPE <- 7L
S30FB_GE_THRESHOLD <- 0.99
S30FB_N <- list(sis_cc1 = 87L, cookie_sis = 77L, discovery = 48L, pool = 16L, windows_per_phase = 444L, rfid_animals = 111L, cookie_all = 97L)

S30FB_BLOCK_LABEL <- c(broad = "Broad screen", inactivity = "Inactivity", cookie = "Cookie")
S30FB_QUESTION_LABEL <- c(CONT = "Continuous CombZ", CAT = "RES vs SUS", L = paste0("CC1", intToUtf8(0x2013L), "CC4 trajectory"))
S30FB_QUESTION_ORDER <- c("CONT", "CAT", "L")
S30FB_SEX_COL <- c(F = "Female", M = "Male", INT = paste0("Female ", intToUtf8(0x2212L), " male"))
S30FB_SEX_ORDER <- c("F", "M", "INT")
S30FB_ESTIMAND_KEY <- list(CAT = c(F = "RS_F", M = "RS_M", INT = "INT_FM", POOL = "POOL"),
                           CONT = c(F = "SLOPE_F", M = "SLOPE_M", INT = "INT_FM", POOL = "POOL"))
S30FB_LINE_DERIVATION <- paste("batch-averaged (n-weighted) fitted line of the registered CombZ ~ Batch + x (OLS normal equations:",
                               "passes through the sex-resolved centroid with the frozen slope); no model refitted")
S30FB_SCIENTIFIC_RECOMPUTATION <- paste("none; copies of frozen values; descriptive summaries (counts, medians, quartiles, centroid means)",
                                        "and unit rescaling of stored estimates only")
S30FB_GROUP_RULE <- "CON if on con_animals.csv, else SUS if on sus_animals.csv, else RES (the Stage 30 driver rule, s30sc_labels_from_lists)"
S30FB_DESCRIPTIVE_COMPUTATIONS <- c(
  "S0/S0b/S4 standardized_ci_low/high: stored ci_low/ci_high rescaled with the stored standardization_rule (CONT: x standardizer_sd; CAT: / standardizer_sd); the rescaled stored estimate reproduces the stored standardized_estimate within 1e-12",
  "S1c: n, median, q25/q75 (R quantile type 7), min, max of light_phase_crossing_rate and posinact40_light by Sex x Group and over all 87 SIS animals at CC1; n_ge_0.99 = count of posinact40_light >= 0.99",
  "S2b: n_windows and n_animals counted in x4_01 (X = 40, per phase); rho and ICC are copies (registry rounded; w1_04 full precision)",
  "S3b: n, median, q25/q75 (type 7) of PRE60, POST60, dcookie60 by Sex and pooled (77) and of dcookie60 by Sex x Group; n_increased = count of dcookie60 > 0; the all_97_registry_qc row is copied from the registry",
  "S3d: x_mean, y_mean = arithmetic means of dcookie60 and CombZ over the model's animals (the sex-resolved centroid); x_min, x_max; y_at_x = y_mean + slope * (x - x_mean) with the frozen slope",
  "S4: ordinal layout keys (row_order, col_order) only")
S30FB_PROVENANCE_KEYS <- c("bundle_id", "status", "generated_at", "generator", "mmm_git_commit", "mmm_branch", "stage30_run_dir",
                           "stage30_run_commit", "stage30_registry_sha256", "stage30_output_manifest_sha256", "stage29_bundle_id",
                           "stage29_bundle_manifest_sha256", "scientific_recomputation", "descriptive_computations",
                           "pre_write_gates_passed", "gate_results")
# gate tables: the pre-write gates inside the bundle (listed in 00_manifest); the complete table (pre-write, post-write and
# registration gates) in a run log beside BUNDLE_REGISTRY.csv, outside the immutable bundle folder
S30FB_GATE_FILE <- "H3_gate_results.csv"
S30FB_LOG_DIR <- "logs"
S30FB_GATE_COLS <- c("stage", "gate", "passed", "hard", "detail")
s30fb_log_path <- function(out_root, bundle_id) file.path(out_root, S30FB_LOG_DIR, paste0(bundle_id, "_gate_results.csv"))
s30fb_gate_count <- function(gates) sprintf("%d of %d", sum(gates$passed), nrow(gates))
S30FB_TABLES <- c("S0_master_hypotheses", "S0b_pool_estimates", "S0c_l_components", "S0d_sensitivities", "S0e_lobo",
                  "S1_light_animals_cc1", "S1b_light_estimates", "S1c_light_descriptives", "S2_rate_inactivity_windows",
                  "S2b_rate_inactivity_relationship", "S3_cookie_animals", "S3b_cookie_descriptives", "S3c_cookie_estimates",
                  "S3d_cookie_cont_lines", "S4_screen_matrix", "S5_display_labels")
# words never allowed in a display label (DESIGN section 0 terminology); identifiers such as crossing_rate are not labels
S30FB_FORBIDDEN_DISPLAY <- "antenna|crossing|sleep|confirmatory|preregistered|approach|investigation|consumption|habituation|time near|female-specific|sex-specific"

S30FB_S0_COLS <- c("id", "family", "block", "question", "sex_analysis", "measure", "measure_col", "phase", "family_m",
                   "estimate", "se", "df", "df_method", "ci_low", "ci_high",
                   "standardized_estimate", "standardizer_sd", "standardization_rule", "standardized_ci_low", "standardized_ci_high",
                   "statistic", "statistic_type", "df1", "df2", "p_raw", "q_local_bh", "q_global_bh_descriptive", "classification", "reason_code",
                   "n_animals", "n_cages", "n_batches", "lobo_signs", "lobo_sign_stable", "robustness_labels",
                   "movadj_estimate", "movadj_ci_low", "movadj_ci_high", "movadj_ci_includes_0", "calibration_verdict", "wording_flags",
                   "display_metric", "display_block", "display_question", "row_order")
S30FB_S0B_COLS <- c("id", "family", "block", "question", "sex_analysis", "measure", "measure_col", "phase",
                    "estimate", "se", "df", "ci_low", "ci_high", "standardized_estimate", "standardizer_sd", "standardization_rule",
                    "standardized_ci_low", "standardized_ci_high", "n_animals", "n_cages", "n_batches", "classification", "flag")

# measure_col -> registry measures entry; the label is the registry `display` unless a fallback is declared here
S30FB_LABEL_SPEC <- data.table::data.table(
  measure_col = c("crossing_rate", "light_phase_crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation",
                  "posinact40_active", "posinact40_light", "posinact60_active", "posinact60_light",
                  "dcookie60", "dcookie45", "PRE60", "POST60", "PRE45", "POST45"),
  registry_measure = c("position_change_rate", "light_phase_position_change_rate", "shared_position", "occupancy_dispersion", "fragmentation",
                       "positional_inactivity_40", "positional_inactivity_40", "positional_inactivity_60", "positional_inactivity_60",
                       "cookie_response_60", "cookie_response_45", "cookie_response_60", "cookie_response_60", "cookie_response_45", "cookie_response_45"),
  phase = c("active", "light", "active", "active", "active", "active", "light", "active", "light", rep("cookie (EPM+1)", 6)),
  role = c("discovery measure", "discovery measure", "discovery measure", "discovery measure", "discovery measure",
           "discovery measure", "discovery measure", "measurement sensitivity only", "measurement sensitivity only",
           "discovery measure", "sensitivity only", "window component", "window component", "window component (sensitivity)", "window component (sensitivity)"),
  fallback = c(NA, NA, NA, "config", "config", NA, NA, "posinact60", "posinact60", NA, "dcookie45", "PRE60", "POST60", "PRE45", "POST45"))
S30FB_WINDOW_LABEL <- c(PRE60 = "PRE60, EPM+1 [16:00, 17:00)", POST60 = "POST60, EPM+1 [17:00, 18:00)",
                        PRE45 = "PRE45, EPM+1 [16:15, 17:00)", POST45 = "POST45, EPM+1 [17:00, 17:45)")
S30FB_INACTIVITY_UNIT <- "fraction of observed time (0-1)"

# ---------------------------------------------------------------- small utilities
s30fb_sha <- function(f) digest::digest(file = f, algo = "sha256")
s30fb_strip_cr <- function(x) gsub("\r", "", as.character(x), fixed = TRUE)
s30fb_norm_path <- function(p) tolower(normalizePath(p, winslash = "/", mustWork = FALSE))
s30fb_inside <- function(path, root) startsWith(paste0(s30fb_norm_path(path), "/"), paste0(s30fb_norm_path(root), "/"))
s30fb_gate_row <- function(stage, gate, passed, detail = "", hard = TRUE)
  data.table::data.table(stage = stage, gate = gate, passed = isTRUE(passed), hard = isTRUE(hard), detail = paste(as.character(detail), collapse = "; "))
s30fb_drop_run_mode <- function(x) { x <- data.table::copy(data.table::as.data.table(x)); if ("run_mode" %in% names(x)) x[, run_mode := NULL]; x[] }
s30fb_near <- function(a, b, tol) length(a) == length(b) && identical(is.na(a), is.na(b)) && isTRUE(all(abs(a - b) <= tol, na.rm = TRUE))
s30fb_near_rel <- function(a, b, tol) length(a) == length(b) && identical(is.na(a), is.na(b)) && isTRUE(all(abs(a - b) <= tol * pmax(1, abs(a)), na.rm = TRUE))
s30fb_one <- function(x, what) { if (nrow(x) != 1L) stop("Expected exactly one row for ", what, " (found ", nrow(x), ").", call. = FALSE); x }

#' Run mode from the command-line arguments: exactly one of --dry-run / --real; nothing else is accepted.
s30fb_parse_mode <- function(args) {
  args <- as.character(args)
  unknown <- setdiff(args, c("--dry-run", "--real"))
  if (length(unknown)) stop("Unknown argument(s): ", paste(unknown, collapse = " "), ". Use exactly one of --dry-run or --real.", call. = FALSE)
  dry <- "--dry-run" %in% args; real <- "--real" %in% args
  if (dry && real) stop("Both --dry-run and --real given: give exactly one.", call. = FALSE)
  if (!dry && !real) stop("Refusing to run: no mode selected. Use --dry-run (scratch output, never S:) or --real (writes the S: bundle once).", call. = FALSE)
  if (dry) "DRY" else "REAL"
}
#' Clean-code check from `git status --porcelain --untracked-files=all -- <files>` output and `git ls-files -- <files>`.
s30fb_code_clean <- function(status_lines, tracked, files) {
  dirty <- status_lines[nzchar(trimws(status_lines))]; untracked <- setdiff(files, tracked)
  list(ok = !length(dirty) && !length(untracked), detail = paste(c(dirty, if (length(untracked)) paste("not tracked:", untracked)), collapse = "; "))
}
s30fb_bundle_id <- function(date, commit) {
  if (!grepl("^[0-9a-f]{40}$", commit)) stop("Not a full git commit hash: ", commit, call. = FALSE)
  sprintf("%s_%s_%s", S30FB_BUNDLE_PREFIX, format(as.Date(date), "%Y%m%d"), substr(commit, 1, 7))
}

# ---------------------------------------------------------------- hash gates
#' Compare the files under `dir` with a manifest (columns file, bytes, sha256; CR stripped). One row per manifest entry.
s30fb_manifest_check <- function(dir, manifest) {
  m <- data.table::as.data.table(manifest)
  file <- s30fb_strip_cr(m$file); exp_sha <- tolower(trimws(s30fb_strip_cr(m$sha256))); exp_bytes <- as.numeric(s30fb_strip_cr(m$bytes))
  p <- file.path(dir, file); ex <- file.exists(p)
  obs_sha <- rep(NA_character_, length(p)); obs_sha[ex] <- vapply(p[ex], s30fb_sha, "", USE.NAMES = FALSE)
  obs_bytes <- rep(NA_real_, length(p)); obs_bytes[ex] <- file.size(p[ex])
  data.table::data.table(file = file, exists = ex, expected_bytes = exp_bytes, observed_bytes = obs_bytes, expected_sha256 = exp_sha,
                         observed_sha256 = obs_sha, ok = ex & !is.na(obs_sha) & obs_sha == exp_sha & !is.na(obs_bytes) & obs_bytes == exp_bytes)
}
s30fb_files_on_disk <- function(dir) sort(list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE))

#' Frozen Stage 30 run (DESIGN 2): the audit/output_manifest.csv hash, its 27 files (CR stripped) and no extra file, the
#' README commit / registry lines, run_manifest and run_failures. Returns list(gates, check, manifest, stage29_folder, run).
s30fb_verify_stage30_run <- function(run_dir, expect = S30FB_STAGE30) {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row("stage30_run", name, passed, detail)
  mf <- file.path(run_dir, "audit", "output_manifest.csv")
  add("frozen Stage 30 run folder name", identical(basename(run_dir), expect$run_name), run_dir)
  if (!file.exists(mf)) { add("audit/output_manifest.csv exists", FALSE, mf); return(list(gates = data.table::rbindlist(g))) }
  msha <- s30fb_sha(mf)
  add("audit/output_manifest.csv SHA-256 = pinned", identical(msha, expect$output_manifest_sha256), msha)
  man <- data.table::fread(mf, colClasses = "character")
  for (k in names(man)) data.table::set(man, j = k, value = s30fb_strip_cr(man[[k]]))
  chk <- s30fb_manifest_check(run_dir, man)
  add(sprintf("output manifest lists %d files", expect$output_manifest_n), nrow(man) == expect$output_manifest_n, nrow(man))
  add("every manifest file: bytes and SHA-256 match (CR stripped)", nrow(chk) > 0L && all(chk$ok), paste(chk[ok == FALSE, file], collapse = ","))
  extra <- setdiff(s30fb_files_on_disk(run_dir), c(chk$file, "audit/output_manifest.csv"))
  add("no file in the run folder outside the manifest", !length(extra), paste(extra, collapse = ","))
  add("manifest run_mode = REAL", all(man$run_mode == expect$run_mode), paste(unique(man$run_mode), collapse = ","))
  rd <- s30fb_strip_cr(readLines(file.path(run_dir, "README.txt"), warn = FALSE))
  add("README commit = Stage 30 run commit", any(rd == paste("commit", expect$run_commit)), grep("^commit ", rd, value = TRUE))
  add("README registry sha256 = frozen registry", any(rd == paste("registry sha256", expect$registry_sha256)), grep("^registry sha256 ", rd, value = TRUE))
  s29 <- sub("^Stage 29 folder ", "", grep("^Stage 29 folder ", rd, value = TRUE))
  add("README names one Stage 29 folder", length(s29) == 1L, s29)
  run <- data.table::fread(file.path(run_dir, "audit", "run_manifest.csv"), colClasses = "character")
  add("run_manifest: REAL mode at the run commit with the frozen registry",
      nrow(run) == 1L && identical(run$mode, "REAL") && identical(run$git_commit, expect$run_commit) && identical(run$registry_sha256, expect$registry_sha256),
      paste(run$mode, run$git_commit, run$registry_sha256))
  add("run_manifest: status OK=48, no block failure", identical(run$status_counts, expect$status_counts) && identical(run$n_block_failures, "0"),
      paste(run$status_counts, run$n_block_failures))
  rf <- data.table::fread(file.path(run_dir, "audit", "run_failures.csv"))
  add("audit/run_failures.csv has no row", nrow(rf) == 0L, nrow(rf))
  list(gates = data.table::rbindlist(g), check = chk, manifest = man, manifest_sha256 = msha, stage29_folder = if (length(s29) == 1L) s29 else NA_character_, run = run)
}

#' Stage 29 bundle (DESIGN 2): 00_manifest.csv hash, every file, no extra file, and a FROZEN registry row with that hash.
s30fb_verify_stage29_bundle <- function(bundle_dir, registry_csv, expect = S30FB_STAGE29) {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row("stage29_bundle", name, passed, detail)
  mf <- file.path(bundle_dir, "00_manifest.csv")
  add("Stage 29 bundle folder = pinned bundle id", identical(basename(bundle_dir), expect$bundle_id), bundle_dir)
  if (!file.exists(mf)) { add("00_manifest.csv exists", FALSE, mf); return(list(gates = data.table::rbindlist(g))) }
  msha <- s30fb_sha(mf)
  add("00_manifest.csv SHA-256 = pinned", identical(msha, expect$manifest_sha256), msha)
  man <- data.table::fread(mf, colClasses = "character")
  chk <- s30fb_manifest_check(bundle_dir, man)
  add("every bundle file: bytes and SHA-256 match", nrow(chk) > 0L && all(chk$ok), paste(chk[ok == FALSE, file], collapse = ","))
  extra <- setdiff(s30fb_files_on_disk(bundle_dir), c(chk$file, "00_manifest.csv"))
  add("no file in the bundle outside its manifest", !length(extra), paste(extra, collapse = ","))
  reg <- if (file.exists(registry_csv)) data.table::fread(registry_csv, colClasses = "character") else data.table::data.table(bundle_id = character())
  r <- reg[bundle_id == expect$bundle_id]
  add("BUNDLE_REGISTRY row: FROZEN with the pinned manifest hash", nrow(r) == 1L && identical(r$status, "FROZEN") && identical(r$manifest_sha256, expect$manifest_sha256),
      paste(nrow(r), "row(s)"))
  list(gates = data.table::rbindlist(g), check = chk, manifest = man, manifest_sha256 = msha)
}

#' One recorded input (input, bytes, sha256) by role in the Stage 30 audit/input_hashes.csv.
s30fb_recorded_input <- function(input_hashes, role) {
  x <- data.table::as.data.table(input_hashes); want <- role; hit <- s30fb_strip_cr(x$role) == want   # `role` is also a column name
  x <- unique(x[hit, .(input = s30fb_strip_cr(input), bytes = as.numeric(s30fb_strip_cr(bytes)), sha256 = s30fb_strip_cr(sha256))])
  s30fb_one(x, paste0("input_hashes role ", want))
}
#' SHA-256 recorded for `name` (basename match) in a `sha256sum`-style list ("<sha> *<path>" per line); NA if not exactly one.
s30fb_sha_list_lookup <- function(lines, name) {
  lines <- s30fb_strip_cr(lines); lines <- lines[grepl("^[0-9a-f]{64} ", lines)]
  sha <- sub("^([0-9a-f]{64}) .*$", "\\1", lines); pth <- sub("^[0-9a-f]{64} [*]?", "", lines)
  hit <- basename(pth) == name
  if (sum(hit) != 1L) NA_character_ else sha[hit]
}

# ---------------------------------------------------------------- labels (Group) and display labels
#' Group from the canonical lists (the Stage 30 driver rule): CON if on the con list, else SUS if on the sus list, else RES.
#' `animal`, `sus`, `con` must already be canonical animal ids.
s30fb_group_from_lists <- function(animal, sus, con) ifelse(animal %in% con, "CON", ifelse(animal %in% sus, "SUS", "RES"))

#' Stop if any display column contains a banned word (DESIGN section 0).
s30fb_check_display <- function(x, cols, what) {
  for (k in cols) { v <- as.character(x[[k]]); bad <- !is.na(v) & grepl(S30FB_FORBIDDEN_DISPLAY, v, ignore.case = TRUE)
    if (any(bad)) stop(what, ": banned display wording in ", k, ": ", paste(unique(v[bad]), collapse = " | "), call. = FALSE) }
  invisible(TRUE)
}
#' Typeset a registry label (the only transform: ">= " -> U+2265).
s30fb_typeset <- function(x) gsub(">= ", intToUtf8(0x2265L), x, fixed = TRUE)

#' S5: measure_col -> display label and unit. `measures` = registry JSON `measures` (list); `cfg_metrics` = the Stage 29
#' configuration `metrics` (ebb_v101 I_analysis_config.json), used only where the registry has no display field.
s30fb_display_labels <- function(measures, cfg_metrics) {
  sp <- data.table::copy(S30FB_LABEL_SPEC)
  out <- data.table::rbindlist(lapply(seq_len(nrow(sp)), function(i) {
    r <- sp[i]; m <- measures[[r$registry_measure]]
    if (is.null(m)) stop("Registry has no measures entry ", r$registry_measure, call. = FALSE)
    lab <- m$display; unit <- m$unit; lsrc <- paste0("registry measures.", r$registry_measure, ".display"); usrc <- paste0("registry measures.", r$registry_measure, ".unit")
    fb <- r$fallback
    if (!is.na(fb) && fb == "config") {
      cm <- cfg_metrics[[r$measure_col]]
      lab <- cm$label; unit <- cm$unit
      lsrc <- paste0("ebb_v101 I_analysis_config.json metrics.", r$measure_col, ".label (registry measures.", r$registry_measure, " has no display field)")
      usrc <- paste0("ebb_v101 I_analysis_config.json metrics.", r$measure_col, ".unit")
    } else if (!is.na(fb) && fb == "posinact60") {
      lab <- sub(">= 40 s", ">= 60 s", measures$positional_inactivity_40$display, fixed = TRUE)
      lsrc <- "registry measures.positional_inactivity_40.display with the >= 60 s threshold of measures.positional_inactivity_60.definition"
    } else if (!is.na(fb) && fb == "dcookie45") {
      lab <- paste0(measures$cookie_response_60$display, ", 45-min windows"); unit <- measures$cookie_response_60$unit
      lsrc <- "registry measures.cookie_response_60.display + measures.cookie_response_45.definition (POST45 minus PRE45)"
      usrc <- "registry measures.cookie_response_60.unit"
    } else if (!is.na(fb) && fb %in% names(S30FB_WINDOW_LABEL)) {
      lab <- paste0(measures$position_change_rate$display, ", ", S30FB_WINDOW_LABEL[[fb]]); unit <- measures$position_change_rate$unit
      lsrc <- paste0("registry measures.position_change_rate.display + the window of measures.", r$registry_measure, ".definition")
      usrc <- "registry measures.position_change_rate.unit"
    }
    if (grepl("^positional_inactivity", r$registry_measure)) { unit <- S30FB_INACTIVITY_UNIT
      usrc <- "registry measures.positional_inactivity_40.definition (qualifying run time divided by observed seconds obs_s)" }
    if (is.null(lab) || is.null(unit)) stop("No display label or unit for ", r$measure_col, call. = FALSE)
    data.table::data.table(measure_col = r$measure_col, registry_measure = r$registry_measure, registry_identifier = m$identifier %||% NA_character_,
                           phase = r$phase, role = r$role, display_label_registry = lab, display_label = s30fb_typeset(lab), unit = unit,
                           label_source = lsrc, unit_source = usrc)
  }))
  s30fb_check_display(out, c("display_label", "unit"), "S5_display_labels")
  out[]
}

# ---------------------------------------------------------------- standardized CI (deterministic unit rescaling)
s30fb_rule_kind <- function(rule) {
  r <- trimws(as.character(rule))
  k <- ifelse(startsWith(r, "estimate x SD_x"), "multiply_sd", ifelse(startsWith(r, "estimate / SD_y"), "divide_sd", ifelse(startsWith(r, "none"), "none", NA_character_)))
  if (anyNA(k)) stop("Unknown standardization_rule: ", paste(unique(r[is.na(k)]), collapse = " | "), call. = FALSE)
  k
}
s30fb_rescale <- function(v, sd, kind) ifelse(kind == "multiply_sd", v * sd, ifelse(kind == "divide_sd", v / sd, NA_real_))
#' Adds standardized_ci_low / standardized_ci_high with the stored rule (CONT x SD_x, CAT / SD_y, L none) and asserts that the
#' stored estimate rescaled the same way equals the stored standardized_estimate within `tol` (absolute).
s30fb_add_standardized_ci <- function(x, tol = S30FB_STD_TOL) {
  x <- data.table::copy(data.table::as.data.table(x))
  kind <- s30fb_rule_kind(x$standardization_rule); has <- kind != "none"
  if (any(!has & !is.na(x$standardized_estimate))) stop("A rule-'none' row carries a standardized estimate: ", paste(x$id[!has & !is.na(x$standardized_estimate)], collapse = ","), call. = FALSE)
  if (any(has & !is.na(x$estimate) & !(is.finite(x$standardizer_sd) & x$standardizer_sd > 0)))
    stop("Missing or non-positive standardizer_sd: ", paste(x$id[has & !(is.finite(x$standardizer_sd) & x$standardizer_sd > 0)], collapse = ","), call. = FALSE)
  est <- s30fb_rescale(x$estimate, x$standardizer_sd, kind)
  if (!identical(is.na(est[has]), is.na(x$standardized_estimate[has]))) stop("Standardized estimate NA pattern differs from the estimate's.", call. = FALSE)
  d <- abs(est - x$standardized_estimate); dmax <- if (any(has & !is.na(d))) max(d[has & !is.na(d)]) else 0
  if (dmax > tol) stop(sprintf("Rescaled estimate does not reproduce the stored standardized_estimate (max |diff| %.3g > %g): %s", dmax, tol,
                               paste(x$id[has & !is.na(d) & d > tol], collapse = ",")), call. = FALSE)
  x[, `:=`(standardized_ci_low = s30fb_rescale(ci_low, standardizer_sd, kind), standardized_ci_high = s30fb_rescale(ci_high, standardizer_sd, kind))]
  data.table::setattr(x, "std_max_abs_diff", dmax)
  x[]
}

# ---------------------------------------------------------------- table builders (copies)
#' S0: the 48 discovery rows with the standardized CI and registry display labels.
s30fb_master_table <- function(master, labels, n_expected = S30FB_N$discovery) {
  M <- s30fb_drop_run_mode(master)
  if (nrow(M) != n_expected || anyDuplicated(M$id)) stop("Master table: expected ", n_expected, " unique discovery rows (found ", nrow(M), ").", call. = FALSE)
  if (!all(M$role == "DISCOVERY")) stop("Master table holds a non-DISCOVERY row.", call. = FALSE)
  M <- s30fb_add_standardized_ci(M); dmax <- attr(M, "std_max_abs_diff")
  lab <- labels[match(M$measure_col, labels$measure_col)]
  if (anyNA(lab$display_label)) stop("No display label for measure_col ", paste(unique(M$measure_col[is.na(lab$display_label)]), collapse = ","), call. = FALSE)
  if (!all(M$block %in% names(S30FB_BLOCK_LABEL)) || !all(M$question %in% names(S30FB_QUESTION_LABEL))) stop("Unknown block or question.", call. = FALSE)
  M[, `:=`(display_metric = lab$display_label, display_block = unname(S30FB_BLOCK_LABEL[block]), display_question = unname(S30FB_QUESTION_LABEL[question]),
           row_order = seq_len(.N))]
  out <- M[, ..S30FB_S0_COLS]; data.table::setattr(out, "std_max_abs_diff", dmax)
  out
}
#' S0b: the 16 POOL rows (estimation only; no p).
s30fb_pool_table <- function(pool, n_expected = S30FB_N$pool) {
  P <- s30fb_drop_run_mode(pool)
  if (nrow(P) != n_expected || anyDuplicated(P$id)) stop("Pool table: expected ", n_expected, " unique rows (found ", nrow(P), ").", call. = FALSE)
  if (!all(P$role == "ESTIMATION_ONLY") || !all(P$flag == "ESTIMATION_ONLY")) stop("Pool table: every row must be ESTIMATION_ONLY.", call. = FALSE)
  P <- s30fb_add_standardized_ci(P); dmax <- attr(P, "std_max_abs_diff")
  out <- P[, ..S30FB_S0B_COLS]; data.table::setattr(out, "std_max_abs_diff", dmax)
  out
}
#' Estimate rows for a set of ids (discovery from the master table, POOL from the pool table) with one declared sensitivity
#' (estimate / CI copied from sensitivities.csv) and the Movement-adjusted estimate / CI (master / pool columns, checked
#' against the MOVADJ sensitivity rows).
s30fb_estimate_rows <- function(master, pool, sens, ids, sens_id, sens_prefix, tol = S30FB_STD_TOL) {
  M <- s30fb_drop_run_mode(master); P <- s30fb_drop_run_mode(pool); S <- s30fb_drop_run_mode(sens)
  keep <- c("id", "role", "family", "question", "sex_analysis", "measure_col", "estimate", "ci_low", "ci_high", "se", "df", "df_method",
            "standardized_estimate", "n_animals", "n_cages", "n_batches", "classification", "movadj_estimate", "movadj_ci_low", "movadj_ci_high")
  d <- M[id %in% ids, c(keep, "p_raw", "q_local_bh", "family_m"), with = FALSE]
  p <- P[id %in% ids, keep, with = FALSE][, `:=`(p_raw = NA_real_, q_local_bh = NA_real_, family_m = NA_integer_)]
  out <- rbind(d, p, use.names = TRUE)
  if (!setequal(out$id, ids) || nrow(out) != length(ids)) stop("Estimate rows: ids not found exactly once: ", paste(setdiff(ids, out$id), collapse = ","), call. = FALSE)
  out <- out[match(ids, id)]
  out[, estimand_key := mapply(function(q, s) S30FB_ESTIMAND_KEY[[q]][[s]], question, sex_analysis)]
  pick <- function(i, s) s30fb_one(S[id == out$id[i] & sensitivity_id == s], paste(out$id[i], s))
  sv <- data.table::rbindlist(lapply(seq_len(nrow(out)), function(i) { r <- pick(i, sens_id); data.table::data.table(e = r$estimate, l = r$ci_low, h = r$ci_high, st = r$status) }))
  mv <- data.table::rbindlist(lapply(seq_len(nrow(out)), function(i) { r <- pick(i, "MOVADJ"); data.table::data.table(e = r$estimate, l = r$ci_low, h = r$ci_high) }))
  if (!s30fb_near_rel(mv$e, out$movadj_estimate, tol) || !s30fb_near_rel(mv$l, out$movadj_ci_low, tol) || !s30fb_near_rel(mv$h, out$movadj_ci_high, tol))
    stop("Movement-adjusted columns differ from the MOVADJ sensitivity rows.", call. = FALSE)
  data.table::set(out, j = paste0(sens_prefix, "_estimate"), value = sv$e)
  data.table::set(out, j = paste0(sens_prefix, "_ci_low"), value = sv$l)
  data.table::set(out, j = paste0(sens_prefix, "_ci_high"), value = sv$h)
  data.table::set(out, j = paste0(sens_prefix, "_status"), value = sv$st)
  data.table::setnames(out, "n_animals", "n")
  data.table::setcolorder(out, c("estimand_key", "id", "role", "family", "question", "sex_analysis", "measure_col", "estimate", "ci_low", "ci_high", "se", "df",
                                 "df_method", "p_raw", "q_local_bh", "family_m", "classification", "standardized_estimate"))
  out[]
}

#' Attach Group and CombZ to Stage 30 per-animal rows (asserted complete; Group RES / SUS only).
s30fb_attach_labels <- function(x, group, combz, what) {
  x <- merge(x, data.table::as.data.table(group)[, .(AnimalNum, Group)], by = "AnimalNum", all.x = TRUE, sort = FALSE)
  x <- merge(x, data.table::as.data.table(combz)[, .(AnimalNum, CombZ)], by = "AnimalNum", all.x = TRUE, sort = FALSE)
  if (anyNA(x$Group) || !all(x$Group %in% c("RES", "SUS"))) stop(what, ": every animal needs Group RES or SUS.", call. = FALSE)
  if (!all(is.finite(x$CombZ))) stop(what, ": CombZ missing for ", paste(x$AnimalNum[!is.finite(x$CombZ)], collapse = ","), call. = FALSE)
  x
}
#' S1: SIS animals at CC1 from the full-precision Stage 30 window features.
s30fb_light_animals <- function(fw, group, combz, n_expected = S30FB_N$sis_cc1) {
  x <- s30fb_drop_run_mode(fw)[CC == "CC1"]
  if (nrow(x) != n_expected || anyDuplicated(x$AnimalNum)) stop("S1: expected ", n_expected, " unique CC1 animals (found ", nrow(x), ").", call. = FALSE)
  x <- s30fb_attach_labels(x, group, combz, "S1")
  x[order(Batch, AnimalNum), .(AnimalNum, Sex, Batch, Group, CageEpisodeID, light_phase_crossing_rate, posinact40_light, posinact60_light, posinact40_active, CombZ)]
}
#' S3: cookie SIS animals from the full-precision Stage 30 cookie features, with the registry EPM+1 date of the batch.
s30fb_cookie_animals <- function(fk, group, combz, epm1_dates, n_expected = S30FB_N$cookie_sis) {
  x <- s30fb_drop_run_mode(fk)
  if (nrow(x) != n_expected || anyDuplicated(x$AnimalNum)) stop("S3: expected ", n_expected, " unique cookie animals (found ", nrow(x), ").", call. = FALSE)
  x <- s30fb_attach_labels(x, group, combz, "S3")
  dates <- unlist(epm1_dates)
  x[, epm1_date := unname(dates[Batch])]
  if (anyNA(x$epm1_date)) stop("S3: no registry EPM+1 date for batch ", paste(unique(x$Batch[is.na(x$epm1_date)]), collapse = ","), call. = FALSE)
  x[order(Batch, AnimalNum), .(AnimalNum, Sex, Batch, Group, CombZ, PRE60, POST60, dcookie60, PRE45, POST45, dcookie45, or646_protocol_deviation, epm1_date)]
}
#' S2: group-blind light-phase animal-windows from x4_01 at one threshold.
s30fb_rate_windows <- function(x4, phase = "light", threshold = 40, n_expected = S30FB_N$windows_per_phase) {
  x <- data.table::as.data.table(x4); x <- x[x$window == phase & x$X == threshold]
  if (nrow(x) != n_expected || anyDuplicated(x[, .(AnimalNum, CC)])) stop("S2: expected ", n_expected, " unique animal-windows (found ", nrow(x), ").", call. = FALSE)
  x[order(Batch, AnimalNum, CC), .(AnimalNum, CC, Batch, crossing_rate, frac)]
}

# ---------------------------------------------------------------- descriptive summaries (counts, medians, quartiles, centroid means)
s30fb_describe <- function(v) {
  if (anyNA(v)) stop("Descriptive summary over a vector with NA.", call. = FALSE)
  if (!length(v)) return(data.table::data.table(n = 0L, median = NA_real_, q25 = NA_real_, q75 = NA_real_, min = NA_real_, max = NA_real_))
  q <- stats::quantile(v, c(0.25, 0.75), type = S30FB_QUANTILE_TYPE, names = FALSE)
  data.table::data.table(n = length(v), median = stats::median(v), q25 = q[1], q75 = q[2], min = min(v), max = max(v))
}
#' S1c: light-phase descriptives by Sex x Group and overall.
s30fb_light_descriptives <- function(s1, ge = S30FB_GE_THRESHOLD) {
  s1 <- data.table::as.data.table(s1); ms <- c("light_phase_crossing_rate", "posinact40_light")
  one <- function(scope, sx, gr, m, v) { r <- s30fb_describe(v)
    cbind(data.table::data.table(scope = scope, Sex = sx, Group = gr, measure = m), r, n_ge = if (m == "posinact40_light") sum(v >= ge) else NA_integer_) }
  rows <- list()
  for (sx in c("Female", "Male")) for (gr in c("RES", "SUS")) for (m in ms) rows[[length(rows) + 1L]] <- one("sex_group", sx, gr, m, s1[Sex == sx & Group == gr][[m]])
  for (m in ms) rows[[length(rows) + 1L]] <- one(paste0("overall_", nrow(s1)), "all", "all", m, s1[[m]])
  out <- data.table::rbindlist(rows); data.table::setnames(out, "n_ge", paste0("n_ge_", ge))
  out[]
}
#' S3b: cookie descriptives by Sex, pooled and by Sex x Group, plus the registry all-97 group-blind QC row (copied).
s30fb_cookie_descriptives <- function(s3, qc) {
  s3 <- data.table::as.data.table(s3); win <- c("PRE60", "POST60", "dcookie60"); src <- "computed here (descriptive; quantile type 7) over S3_cookie_animals"
  one <- function(scope, sx, gr, m, v) cbind(data.table::data.table(scope = scope, Sex = sx, Group = gr, measure = m), s30fb_describe(v)[, .(n, median, q25, q75)],
                                            n_increased = if (m == "dcookie60") sum(v > 0) else NA_integer_, source = src)
  rows <- list()
  for (sx in c("Female", "Male")) for (m in win) rows[[length(rows) + 1L]] <- one("sex", sx, "all", m, s3[Sex == sx][[m]])
  for (m in win) rows[[length(rows) + 1L]] <- one(paste0("pooled_", nrow(s3)), "all", "all", m, s3[[m]])
  for (sx in c("Female", "Male")) for (gr in c("RES", "SUS")) rows[[length(rows) + 1L]] <- one("sex_group", sx, gr, "dcookie60", s3[Sex == sx & Group == gr][["dcookie60"]])
  inc <- strsplit(as.character(qc$increased), "/", fixed = TRUE)[[1]]
  if (length(inc) != 2L || length(unlist(qc$iqr)) != 2L) stop("Registry cookie group_blind_qc is not 'k/n' with a two-value IQR.", call. = FALSE)
  rows[[length(rows) + 1L]] <- data.table::data.table(scope = "all_97_registry_qc", Sex = "all", Group = "all (incl. CON)", measure = "dcookie60",
                                                      n = as.integer(inc[2]), median = as.numeric(qc$median), q25 = as.numeric(unlist(qc$iqr)[1]),
                                                      q75 = as.numeric(unlist(qc$iqr)[2]), n_increased = as.integer(inc[1]),
                                                      source = "copied: registry measures.cookie_response_60.group_blind_qc (not computed here)")
  data.table::rbindlist(rows)
}
#' S3d: one display line per sex through the centroid of (x, y) over exactly the model's animals with the frozen slope.
#' For CombZ ~ Batch + x the OLS normal equations give intercept_b = ybar_b - slope * xbar_b in every batch; their
#' n-weighted average is ybar - slope * xbar, so the batch-averaged fitted line passes through the centroid.
#' The derivation text is checked with the banned-display-word guard (it may be shown as a figure note).
s30fb_cookie_lines <- function(s3, master, ids = c(Female = "COOKIE-CONT-DC60-F", Male = "COOKIE-CONT-DC60-M"), x_col = "dcookie60", y_col = "CombZ",
                               derivation = S30FB_LINE_DERIVATION) {
  s3 <- data.table::as.data.table(s3); M <- s30fb_drop_run_mode(master)
  out <- data.table::rbindlist(lapply(names(ids), function(sx) {
    fr <- s30fb_one(M[id == ids[[sx]]], ids[[sx]]); d <- s3[Sex == sx]
    x <- d[[x_col]]; y <- d[[y_col]]
    if (anyNA(x) || anyNA(y)) stop("S3d: NA in the line data for ", sx, call. = FALSE)
    if (nrow(d) != fr$n_animals) stop(sprintf("S3d: %s line uses %d animals; the frozen model has n_animals = %d.", sx, nrow(d), fr$n_animals), call. = FALSE)
    if (data.table::uniqueN(d$Batch) != fr$n_batches) stop(sprintf("S3d: %s line spans %d batches; the frozen model has %d.", sx, data.table::uniqueN(d$Batch), fr$n_batches), call. = FALSE)
    if (!startsWith(fr$formula %||% "CombZ ~ Batch + x", "CombZ ~ Batch + x")) stop("S3d: ", fr$id, " is not the registered CombZ ~ Batch + x model.", call. = FALSE)
    x_mean <- mean(x); y_mean <- mean(y); b <- fr$estimate; x_min <- min(x); x_max <- max(x)
    data.table::data.table(Sex = sx, sex_analysis = fr$sex_analysis, id = fr$id, n = nrow(d), n_batches = data.table::uniqueN(d$Batch), x_col = x_col, y_col = y_col,
                           x_mean = x_mean, y_mean = y_mean, slope = b, slope_ci_low = fr$ci_low, slope_ci_high = fr$ci_high,
                           x_min = x_min, x_max = x_max, y_at_x_min = y_mean + b * (x_min - x_mean), y_at_x_max = y_mean + b * (x_max - x_mean),
                           derivation = derivation)
  }))
  s30fb_check_display(out, "derivation", "S3d_cookie_cont_lines")
  out[]
}

#' S2b: the stored rate-inactivity relationship: registry rounded values and the w1_04 full-precision values, with the
#' window / animal counts of the x4_01 rows behind them. Stops unless round(full, 2) equals the registry value.
s30fb_rate_relationship <- function(qc, gate_values, x4, source_file, source_sha256, threshold = 40, tol = 1e-9) {
  gv <- data.table::as.data.table(gate_values); x4 <- data.table::as.data.table(x4); ph <- c("active", "light")
  rho_r <- as.numeric(unlist(qc$rho_with_rate_BxCC)); icc_r <- as.numeric(unlist(qc$icc_after_rate))
  if (length(rho_r) != 2L || length(icc_r) != 2L) stop("Registry group_blind_qc rho / icc must hold [active, light].", call. = FALSE)
  data.table::rbindlist(lapply(seq_along(ph), function(i) {
    g <- s30fb_one(gv[window == ph[i] & X_label == paste0("X", threshold)], paste("w1_04", ph[i]))
    if (abs(round(g$rho_rate, 2) - rho_r[i]) > tol) stop(sprintf("S2b: round(rho_full, 2) = %.2f != registry %.2f (%s)", round(g$rho_rate, 2), rho_r[i], ph[i]), call. = FALSE)
    if (abs(round(g$ICC_resid, 2) - icc_r[i]) > tol) stop(sprintf("S2b: round(icc_full, 2) = %.2f != registry %.2f (%s)", round(g$ICC_resid, 2), icc_r[i], ph[i]), call. = FALSE)
    w <- x4[x4$window == ph[i] & x4$X == threshold]
    data.table::data.table(phase = ph[i], X = threshold, rho_rounded = rho_r[i], rho_full = g$rho_rate, icc_rounded = icc_r[i], icc_full = g$ICC_resid,
                           n_windows = nrow(w), n_animals = data.table::uniqueN(w$AnimalNum), basis = qc$basis,
                           rounded_source = "registry measures.positional_inactivity_40.group_blind_qc (rho_with_rate_BxCC, icc_after_rate; [active, light])",
                           source_file = source_file, source_sha256 = source_sha256)
  }))
}
#' The other registry group-blind QC roundings reproduced from w1_04 (gate rows; nothing is written from them).
s30fb_qc_rounding_checks <- function(qc, gate_values, tol = 1e-9) {
  gv <- data.table::as.data.table(gate_values); a <- s30fb_one(gv[window == "active" & X_label == "X40"], "w1_04 active"); l <- s30fb_one(gv[window == "light" & X_label == "X40"], "w1_04 light")
  ch <- data.table::data.table(item = c("active_median", "light_median", "light_windows_ge_0.99"),
                               registry = c(qc$active_median, qc$light_median, qc[["light_windows_ge_0.99"]]),
                               full = c(a$median_frac, l$median_frac, l$share_ge_099))
  ch[, rounded := round(full, 3)][, ok := abs(rounded - registry) <= tol]
  ch[]
}

#' S4: layout keys for the screen summary (one row per discovery test; plotting fields copied from S0).
s30fb_screen_matrix <- function(s0) {
  x <- data.table::copy(data.table::as.data.table(s0))
  x[, metric_key := paste(question, measure_col, sep = "|")]
  keys <- unique(x[order(match(question, S30FB_QUESTION_ORDER), row_order), .(question, metric_key)])
  keys[, row_order_new := seq_len(.N)]
  x <- merge(x, keys[, .(metric_key, row_order_new)], by = "metric_key", sort = FALSE)
  if (x[, .N, by = metric_key][, any(N != 3L)] || !all(x$sex_analysis %in% S30FB_SEX_ORDER)) stop("S4: every metric row needs exactly F, M and INT.", call. = FALSE)
  x[, `:=`(block_label = unname(S30FB_BLOCK_LABEL[block]), question_label = unname(S30FB_QUESTION_LABEL[question]), metric_label = display_metric,
           window_label = ifelse(grepl("\\(", measure), sub("^data version v2; ", "", sub("^[^(]*\\((.*)\\)$", "\\1", measure)), NA_character_),
           sex_col = unname(S30FB_SEX_COL[sex_analysis]), col_order = match(sex_analysis, S30FB_SEX_ORDER), F = ifelse(question == "L", statistic, NA_real_))]
  x[, row_order := row_order_new]
  out <- x[order(row_order, col_order), .(id, block, question, sex_analysis, family, family_m, block_label, question_label, metric_label, window_label, measure_col,
                                          sex_col, row_order, col_order, standardized_estimate, standardized_ci_low, standardized_ci_high, F, df1, df2,
                                          p_raw, q_local_bh, classification)]
  s30fb_check_display(out, c("block_label", "question_label", "metric_label", "window_label", "sex_col"), "S4_screen_matrix")
  out[]
}

# ---------------------------------------------------------------- writers (full precision, manifest, provenance, registry)
#' Shortest decimal text (15-17 significant digits) that parses back to exactly the same double.
s30fb_num_chr <- function(x) {
  out <- rep(NA_character_, length(x)); fin <- is.finite(x)
  for (d in 15:17) { need <- fin & is.na(out); if (!any(need)) break
    s <- trimws(formatC(x[need], digits = d, format = "g")); hit <- as.numeric(s) == x[need]
    out[which(need)[hit]] <- s[hit] }
  if (any(fin & is.na(out))) stop("No round-trip decimal representation for some values.", call. = FALSE)
  out[is.infinite(x)] <- ifelse(x[is.infinite(x)] > 0, "Inf", "-Inf")
  out
}
#' Write one CSV with every double at full precision, then re-read it and require every value back exactly.
s30fb_write_csv <- function(x, path) {
  x <- data.table::as.data.table(x)
  if (any(vapply(x, is.list, TRUE))) stop("List column in ", basename(path), call. = FALSE)
  # an embedded double quote is written RFC-4180-doubled, which data.table::fread and utils::read.csv read back differently
  dq <- names(x)[vapply(x, function(v) is.character(v) && any(grepl("\"", v, fixed = TRUE)), TRUE)]
  if (length(dq)) stop("Embedded double quote in ", basename(path), " column(s) ", paste(dq, collapse = ","), call. = FALSE)
  dbl <- names(x)[vapply(x, is.double, TRUE)]
  y <- data.table::copy(x); for (k in dbl) data.table::set(y, j = k, value = s30fb_num_chr(x[[k]]))
  data.table::fwrite(y, path, encoding = "UTF-8")
  oth <- setdiff(names(x), dbl)
  cc <- c(if (length(dbl)) list(numeric = dbl), if (length(oth)) list(character = oth))
  back <- data.table::fread(path, colClasses = cc, encoding = "UTF-8", na.strings = "")
  if (!identical(names(back), names(x)) || nrow(back) != nrow(x)) stop("Round-trip shape differs for ", basename(path), call. = FALSE)
  for (k in names(x)) {
    if (k %in% dbl) { a <- x[[k]]; b <- back[[k]]
      if (!identical(is.na(a), is.na(b)) || any(a[!is.na(a)] != b[!is.na(b)])) stop("Round-trip value differs in ", basename(path), " column ", k, call. = FALSE)
    } else { a <- as.character(x[[k]]); a[is.na(a)] <- ""; b <- as.character(back[[k]]); b[is.na(b)] <- ""
      if (!identical(enc2utf8(a), enc2utf8(b))) stop("Round-trip text differs in ", basename(path), " column ", k, call. = FALSE) }
  }
  invisible(path)
}
#' H_provenance (key, value); stops if a DESIGN key is missing or empty.
s30fb_provenance <- function(fields) {
  v <- vapply(fields, function(z) paste(as.character(z), collapse = " || "), "")
  h <- data.table::data.table(field = names(fields), value = unname(v))
  data.table::setnames(h, "field", "key")   # "key" cannot be a data.table() argument name (as Stage 16b)
  miss <- setdiff(S30FB_PROVENANCE_KEYS, h$key[!is.na(h$value) & nzchar(h$value)])
  if (length(miss)) stop("H_provenance lacks key(s): ", paste(miss, collapse = ", "), call. = FALSE)
  if (anyDuplicated(h$key)) stop("H_provenance has duplicated keys.", call. = FALSE)
  h
}
#' The files a bundle's 00_manifest.csv must list (00_manifest.csv itself is not listed).
s30fb_bundle_files <- function() c(paste0(S30FB_TABLES, ".csv"), "H_provenance.csv", "H2_inputs.csv", S30FB_GATE_FILE)

#' Post-write gates on the renamed bundle folder, run BEFORE the BUNDLE_REGISTRY.csv row is appended: 00_manifest.csv read
#' back equals the manifest written; every listed file matches it (bytes, SHA-256) and nothing else is in the folder; the
#' manifest lists exactly the declared files; every file is read-only. Returns gate rows (stage 7-write); never stops.
s30fb_post_write_gates <- function(bd, manifest, expected_files = s30fb_bundle_files()) {
  g <- list(); add <- function(name, passed, detail = "") g[[length(g) + 1L]] <<- s30fb_gate_row("7-write", name, passed, detail)
  m <- data.table::as.data.table(manifest); mf <- file.path(bd, "00_manifest.csv")
  back <- if (file.exists(mf)) data.table::fread(mf, colClasses = "character") else data.table::data.table()
  add("00_manifest.csv read back = the manifest written (file, bytes, sha256, schema_version)",
      nrow(back) == nrow(m) && identical(names(back), names(m)) && identical(back$file, m$file) && identical(back$sha256, m$sha256) &&
        identical(as.numeric(back$bytes), as.numeric(m$bytes)) && identical(back$schema_version, as.character(m$schema_version)), mf)
  chk <- s30fb_manifest_check(bd, m); on_disk <- s30fb_files_on_disk(bd); extra <- setdiff(on_disk, c(m$file, "00_manifest.csv"))
  add("written bundle: every file = 00_manifest (bytes and SHA-256); nothing else in the folder",
      nrow(chk) > 0L && all(chk$ok) && !length(extra) && "00_manifest.csv" %in% on_disk,
      paste(c(bd, if (!all(chk$ok)) paste("mismatch:", paste(chk$file[!chk$ok], collapse = ",")), if (length(extra)) paste("extra:", paste(extra, collapse = ","))), collapse = "; "))
  add("00_manifest lists exactly the declared files (16 tables, H_provenance, H2_inputs, H3_gate_results)",
      setequal(m$file, expected_files) && !anyDuplicated(m$file), paste(c(setdiff(expected_files, m$file), setdiff(m$file, expected_files)), collapse = ","))
  fl <- list.files(bd, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  add("written files are read-only", length(fl) > 0L && all(file.access(fl, 2L) != 0L), bd)
  data.table::rbindlist(g)
}
#' Registration gate: BUNDLE_REGISTRY.csv holds exactly one row for the bundle, with its status and 00_manifest hash.
s30fb_registration_gate <- function(reg, bundle_id, status, manifest_sha256) {
  r <- if (file.exists(reg)) data.table::fread(reg, colClasses = "character") else data.table::data.table(bundle_id = character(), status = character(), manifest_sha256 = character())
  hit <- r$bundle_id == bundle_id; r <- r[hit]                     # computed outside [ ]: bundle_id is also a column name
  s30fb_gate_row("8-register", "BUNDLE_REGISTRY.csv: exactly one row for the bundle, with its status and 00_manifest SHA-256",
                 nrow(r) == 1L && identical(r$status, status) && identical(r$manifest_sha256, manifest_sha256),
                 paste(nrow(r), "row(s)", paste(r$status, collapse = ","), paste(r$manifest_sha256, collapse = ",")))
}
#' Write a gate table once (refused if it exists), exact round trip, then 0444.
s30fb_write_gate_log <- function(gates, path) {
  if (file.exists(path)) stop("A gate log for this bundle exists (immutable): ", path, call. = FALSE)
  if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
  s30fb_write_csv(gates, path); Sys.chmod(path, mode = "0444")
  invisible(path)
}

#' Write the bundle once. Refuses if the version folder, its staging folder, its registry row or its gate log exists, if a
#' hard gate in `gates` (the runner's pre-write gate table) did not pass, or if H_provenance pre_write_gates_passed does not
#' count that table. Writes every table, H_provenance, H2_inputs, H3_gate_results (= `gates`) and 00_manifest (file, bytes,
#' sha256, schema_version; every other file) into a staging folder, renames it and makes the files read-only (0444). Then
#' runs the post-write gates; only if all pass is the BUNDLE_REGISTRY.csv row appended and checked. The complete gate table
#' (pre-write, post-write, registration) is written to logs/<bundle_id>_gate_results.csv beside the registry (0444), also
#' when a post-write gate fails (the bundle is then NOT registered). `.before_verify` is a test hook (never set by the runner).
s30fb_write_bundle <- function(tables, out_root, bundle_id, provenance, inputs, gates, status, stage30_run_commit, mmm_git_commit, created_at,
                               .before_verify = NULL) {
  bd <- file.path(out_root, bundle_id); stg <- file.path(out_root, paste0(".tmp_", bundle_id)); reg <- file.path(out_root, "BUNDLE_REGISTRY.csv")
  log <- s30fb_log_path(out_root, bundle_id)
  registered <- function() file.exists(reg) && bundle_id %in% data.table::fread(reg, colClasses = "character")$bundle_id
  if (dir.exists(bd)) stop("Bundle version exists (immutable): ", bd, call. = FALSE)
  if (dir.exists(stg)) stop("A staging folder for this bundle exists (earlier aborted write): ", stg, call. = FALSE)
  if (registered()) stop("Bundle id already registered: ", bundle_id, call. = FALSE)
  if (file.exists(log)) stop("A gate log for this bundle exists (immutable): ", log, call. = FALSE)
  bad <- setdiff(names(tables), S30FB_TABLES); miss <- setdiff(S30FB_TABLES, names(tables))
  if (length(bad) || length(miss)) stop("Table set differs from the declared set: extra ", paste(bad, collapse = ","), "; missing ", paste(miss, collapse = ","), call. = FALSE)
  gates <- data.table::as.data.table(gates)
  if (!identical(names(gates), S30FB_GATE_COLS) || !is.logical(gates$passed) || !is.logical(gates$hard))
    stop("The gate table must have columns ", paste(S30FB_GATE_COLS, collapse = ", "), " (passed / hard logical).", call. = FALSE)
  if (!nrow(gates) || anyNA(gates$passed) || any(gates$hard & !gates$passed)) stop("Refusing to write: a hard pre-write gate did not pass.", call. = FALSE)
  pv <- provenance$value[provenance$key == "pre_write_gates_passed"]
  if (!identical(pv, s30fb_gate_count(gates)))
    stop("H_provenance pre_write_gates_passed (", paste(pv, collapse = ","), ") does not count the gate table (", s30fb_gate_count(gates), ").", call. = FALSE)
  if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)
  dir.create(stg)
  for (n in S30FB_TABLES) s30fb_write_csv(tables[[n]], file.path(stg, paste0(n, ".csv")))
  s30fb_write_csv(provenance, file.path(stg, "H_provenance.csv"))
  s30fb_write_csv(inputs, file.path(stg, "H2_inputs.csv"))
  s30fb_write_csv(gates, file.path(stg, S30FB_GATE_FILE))
  fl <- sort(list.files(stg, full.names = TRUE))
  man <- data.table::data.table(file = basename(fl), bytes = as.numeric(file.size(fl)), sha256 = vapply(fl, s30fb_sha, "", USE.NAMES = FALSE), schema_version = S30FB_SCHEMA_VERSION)
  s30fb_write_csv(man, file.path(stg, "00_manifest.csv"))
  if (dir.exists(bd)) stop("Bundle version appeared during the write (immutable): ", bd, call. = FALSE)
  if (!file.rename(stg, bd)) stop("Could not rename ", stg, " to ", bd, call. = FALSE)
  Sys.chmod(list.files(bd, full.names = TRUE), mode = "0444")
  if (is.function(.before_verify)) .before_verify(bd)
  post <- s30fb_post_write_gates(bd, man)
  if (!all(post$passed)) {
    s30fb_write_gate_log(rbind(gates, post), log)
    stop("Post-write gate failed; the bundle is NOT registered (folder ", bd, " left for inspection; gate log ", log, "): ",
         paste(post$gate[!post$passed], collapse = "; "), call. = FALSE)
  }
  msha <- s30fb_sha(file.path(bd, "00_manifest.csv"))
  if (registered()) stop("A registry row for this bundle appeared during the write: ", bundle_id, call. = FALSE)
  if (file.exists(log)) stop("A gate log for this bundle appeared during the write: ", log, call. = FALSE)
  data.table::fwrite(data.table::data.table(bundle_id = bundle_id, status = status, manifest_sha256 = msha, stage30_run_commit = stage30_run_commit,
                                            mmm_git_commit = mmm_git_commit, created_at = created_at), reg, append = file.exists(reg))
  rg <- s30fb_registration_gate(reg, bundle_id, status, msha)
  all_gates <- rbind(gates, post, rg)
  s30fb_write_gate_log(all_gates, log)
  if (!isTRUE(rg$passed)) stop("Registration gate failed: ", rg$detail, call. = FALSE)
  list(dir = bd, manifest_sha256 = msha, manifest = man, registry = reg, log = log, log_sha256 = s30fb_sha(log),
       post_gates = rbind(post, rg), gates = all_gates)
}
