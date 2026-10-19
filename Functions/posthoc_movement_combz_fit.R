# ================================================================
# Post hoc per-sex least-squares display line, CombZ on early movement (registry v1.1): helpers
# MMMSociability -- Functions/posthoc_movement_combz_fit.R
# ================================================================
# Pure constants and functions for Analysis/29c_posthoc_movement_combz_fit.R. Sourcing this file reads and writes nothing.
# Implements docs/POSTHOC_MOVEMENT_COMBZ_FIT_REGISTRY_v1.1.md: registry v1.0 (FROZEN 2026-10-12T11:30:00+0200, commit 2be501d,
# sha256 a617bb5b...; run v1.0_fd61cd9) on the corrected outcome (config v1.0.2, outcome version ov2)
# and nothing else:
#   * data: A2_prediction_animals of the Stage 29 bundle in PMF_STAGE29, y = observed_CombZ, x = Movement_mean; per sex every animal with finite x and y;
#   * one stats::lm(y ~ x) per sex; the fitted line on 101 equally spaced x from that sex's minimum to its maximum x;
#     the pointwise 95% confidence band fit +- qt(0.975, n - 2) x se.fit (stats::predict(se.fit = TRUE)), i.e. the band
#     ggplot2::geom_smooth(method = "lm", level = 0.95) draws;
#   * coefficients without t or P (registry section 3: nothing is tested);
#   * failure rule (section 5): an error, rank < 2 or fewer than 10 animals -> FAILED, no line, no band, no fallback.
# Tier: POST HOC descriptive display (USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION, 2026-10-12); it adds no test and changes no
# registered family or result.
# Requires: data.table, digest; Functions/stage30_figure_bundle.R (s30fb_*) and Functions/posthoc_con_contrasts.R
# (phc_registry_gates, phc_gate) sourced first.
# ================================================================

# v1.0 (run v1.0_fd61cd9): registry sha256 a617bb5be709b558ed628a61fab0095b598345912c86e9c25070d07b02cb8098, freeze commit
# 2be501d8b849346bdde47e246fb738a9df05ad98, ebb_v101_20260929_b2ce507 (manifest ec59aa33..., A2 5b8ee3fb...).
# The PHASE-B values are set when v1.1 is frozen and ebb_v102 exists; until then every REAL-run gate fails closed.
PMF_VERSION <- "1.1"
PMF_REGISTRY <- list(
  version = "1.1", file = "POSTHOC_MOVEMENT_COMBZ_FIT_REGISTRY_v1.1.md", repo_path = "docs/POSTHOC_MOVEMENT_COMBZ_FIT_REGISTRY_v1.1.md",
  sha256 = "PHASE-B",  # RETRACK-PIN
  canonical_rel = file.path("canonical", "posthoc_movement_combz_fit_registry", "v1.1"), sha_file = "REGISTRY_SHA256.txt",
  freeze_commit = "PHASE-B", frozen_at = "PHASE-B")  # RETRACK-PIN
PMF_STAGE29 <- list(bundle_id = "ebb_v102_PHASE-B",  # RETRACK-PIN
                    manifest_sha256 = "PHASE-B",  # RETRACK-PIN
                    a2_file = "A2_prediction_animals.csv", a2_sha256 = "PHASE-B")  # RETRACK-PIN
PMF_STAGE_DIR <- "29c_posthoc_movement_combz_fit"
# any earlier REAL output or staging folder (at any commit) blocks a REAL run: the registry allows exactly one
PMF_REAL_OUT_PATTERN <- "^(\\.tmp_)?v1\\.1_"
PMF_OPTIN_ENV <- "MMM_POSTHOC29C_REAL_RUN"

PMF_X <- "Movement_mean"
PMF_Y <- "observed_CombZ"
PMF_SEXES <- c("Female", "Male")
PMF_N <- list(animals = 111L, by_sex = c(Female = 58L, Male = 53L))
PMF_GRID_N <- 101L
PMF_LEVEL <- 0.95
PMF_MIN_N <- 10L
PMF_TIER <- "POST HOC descriptive display"
PMF_DECISION_BASIS <- "USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION (2026-10-12)"
PMF_DISPLAY_LABEL <- "least-squares line with 95% confidence band (post hoc, display only)"
PMF_NOTE <- paste("adds no test and changes no registered result; the statistic shown with Figure 1d is Stage 09's",
                  "sex-stratified Spearman rho with its BH q within sex")
PMF_TABLE_FILES <- c(fit_lines = "tables/fit_lines.csv", coefficients = "tables/coefficients.csv", diagnostics = "tables/diagnostics.csv")
PMF_AUDIT_FILES <- c(gates = "audit/gate_results.csv", inputs = "audit/input_hashes.csv", run = "audit/run_manifest.csv")
PMF_MANIFEST_FILE <- "audit/output_manifest.csv"
PMF_README <- "README.txt"

# ---------------------------------------------------------------- mode and identity
#' The only mode is --real (registry section 7: one REAL run; the synthetic tests are the only rehearsal).
pmf_parse_mode <- function(args) {
  args <- as.character(args)
  if (!identical(args, "--real"))
    stop("Refusing to run: the only mode is --real (registry section 7: fitted once; no dry run on project data). ",
         "Synthetic checks: Rscript Testing/tests/test_posthoc_movement_combz_fit.R", call. = FALSE)
  "REAL"
}
pmf_run_name <- function(commit) {
  if (!grepl("^[0-9a-f]{40}$", commit)) stop("Not a full git commit hash: ", commit, call. = FALSE)
  paste0("v", PMF_VERSION, "_", substr(commit, 1, 7))
}

# ---------------------------------------------------------------- data and population
#' Population gates on A2 (registry section 1 / section 7). Returns gate rows.
pmf_population_gates <- function(a2, n = PMF_N, x = PMF_X, y = PMF_Y) {
  d <- data.table::as.data.table(a2); g <- list(); add <- function(name, ok, detail = "") g[[length(g) + 1L]] <<- phc_gate("2-population", name, ok, detail)
  miss <- setdiff(c("AnimalNum", "Sex", "Group", x, y), names(d))
  add("A2 has AnimalNum, Sex, Group, the predictor and the outcome", !length(miss), paste(miss, collapse = ","))
  if (length(miss)) return(data.table::rbindlist(g))
  add(sprintf("A2: %d animals, one row each", n$animals), nrow(d) == n$animals && !anyDuplicated(d$AnimalNum),
      paste(nrow(d), data.table::uniqueN(d$AnimalNum)))
  cnt <- d[, .N, by = Sex]
  add(sprintf("animals per sex = %s", paste(names(n$by_sex), n$by_sex, collapse = ", ")),
      nrow(cnt) == length(n$by_sex) && all(vapply(names(n$by_sex), function(s) isTRUE(cnt[Sex == s, N] == n$by_sex[[s]]), TRUE)),
      paste(cnt[order(Sex), paste(Sex, N)], collapse = ", "))
  add("every predictor and outcome value is finite (none excluded)", all(is.finite(d[[x]])) && all(is.finite(d[[y]])),
      paste(d[!is.finite(get(x)) | !is.finite(get(y)), AnimalNum], collapse = ","))
  data.table::rbindlist(g)
}

# ---------------------------------------------------------------- fit (registry sections 2, 3, 5)
#' One sex: lm(y ~ x), the 101-point line with its 95% band, the coefficients (no t, no P) and a diagnostics row.
#' Failure rule: error, rank < 2 or n < PMF_MIN_N -> status FAILED, no line rows, NA coefficients.
pmf_fit_sex <- function(a2, sex, x = PMF_X, y = PMF_Y, grid_n = PMF_GRID_N, level = PMF_LEVEL, min_n = PMF_MIN_N) {
  d <- data.table::as.data.table(a2)[Sex == sex]
  keep <- is.finite(d[[x]]) & is.finite(d[[y]])
  dd <- data.frame(x = d[[x]][keep], y = d[[y]][keep])
  n <- nrow(dd); reason <- NA_character_; fit <- NULL
  if (n < min_n) reason <- sprintf("registry section 5: fewer than %d animals (%d)", min_n, n)
  else {
    fit <- tryCatch(stats::lm(y ~ x, data = dd), error = function(e) e)
    if (inherits(fit, "error")) { reason <- paste("registry section 5: fit error:", conditionMessage(fit)); fit <- NULL }
    else if (fit$rank < 2L) { reason <- sprintf("registry section 5: rank-deficient fit (rank %d)", fit$rank); fit <- NULL }
  }
  status <- if (is.null(fit)) "FAILED" else "OK"
  diag <- data.table::data.table(Sex = sex, n = n, n_excluded_nonfinite = sum(!keep),
    x_min = if (n) min(dd$x) else NA_real_, x_max = if (n) max(dd$x) else NA_real_,
    rank = if (is.null(fit)) NA_integer_ else as.integer(fit$rank), status = status, failure_reason = reason)
  if (is.null(fit)) {
    coef <- data.table::data.table(Sex = sex, term = c("intercept", "slope"), estimate = NA_real_, se = NA_real_,
                                   df_residual = NA_integer_, n = n, residual_sd = NA_real_, r_squared = NA_real_)
    return(list(lines = NULL, coefficients = coef, diagnostics = diag))
  }
  grid <- data.frame(x = seq(min(dd$x), max(dd$x), length.out = grid_n))
  pr <- stats::predict(fit, newdata = grid, se.fit = TRUE)
  dfr <- as.integer(fit$df.residual); tcrit <- stats::qt(1 - (1 - level) / 2, dfr)
  lines <- data.table::data.table(Sex = sex, grid_index = seq_len(grid_n), x = grid$x, fit = unname(pr$fit), se_fit = unname(pr$se.fit),
    df_residual = dfr, t_crit = tcrit, ci_low = unname(pr$fit - tcrit * pr$se.fit), ci_high = unname(pr$fit + tcrit * pr$se.fit),
    level = level, n = n)
  cs <- summary(fit)
  coef <- data.table::data.table(Sex = sex, term = c("intercept", "slope"), estimate = unname(cs$coefficients[, "Estimate"]),
    se = unname(cs$coefficients[, "Std. Error"]), df_residual = dfr, n = n, residual_sd = cs$sigma, r_squared = cs$r.squared)
  list(lines = lines, coefficients = coef, diagnostics = diag)
}

#' Both sexes, with the registry identity on every row. Returns list(fit_lines, coefficients, diagnostics).
pmf_run_all <- function(a2, sexes = PMF_SEXES, registry_sha256 = PMF_REGISTRY$sha256) {
  r <- lapply(sexes, function(s) pmf_fit_sex(a2, s))
  tag <- function(x) { x <- data.table::copy(x)
    x[, `:=`(x_variable = PMF_X, y_variable = PMF_Y, tier = PMF_TIER, decision_basis = PMF_DECISION_BASIS,
             registry_version = PMF_REGISTRY$version, registry_sha256 = registry_sha256)][] }
  list(fit_lines = tag(data.table::rbindlist(lapply(r, `[[`, "lines"))),
       coefficients = tag(data.table::rbindlist(lapply(r, `[[`, "coefficients"))),
       diagnostics = tag(data.table::rbindlist(lapply(r, `[[`, "diagnostics"))))
}

#' Gates on the fitted tables: both fits OK, 101 grid rows per sex spanning the observed range, the band contains the
#' line, the line equals intercept + slope x x, and no column carries a test statistic or P.
pmf_output_gates <- function(R, a2, grid_n = PMF_GRID_N, sexes = PMF_SEXES, tol = 1e-9) {
  g <- list(); add <- function(name, ok, detail = "") g[[length(g) + 1L]] <<- phc_gate("3-fit", name, ok, detail)
  dg <- R$diagnostics; fl <- R$fit_lines; cf <- R$coefficients; d <- data.table::as.data.table(a2)
  add("both sex fits OK (no FAILED fit)", nrow(dg) == length(sexes) && all(dg$status == "OK"), paste(dg$Sex, dg$status, collapse = "; "))
  add(sprintf("fit_lines: %d grid rows per sex", grid_n), nrow(fl) == grid_n * length(sexes) && all(fl[, .N, by = Sex]$N == grid_n), nrow(fl))
  rng <- d[, .(lo = min(get(PMF_X)), hi = max(get(PMF_X))), by = Sex]
  span <- merge(fl[, .(glo = min(x), ghi = max(x)), by = Sex], rng, by = "Sex")
  add("each grid spans exactly that sex's observed predictor range (no extrapolation)",
      nrow(span) == length(sexes) && all(abs(span$glo - span$lo) <= tol) && all(abs(span$ghi - span$hi) <= tol))
  add("the band contains the line at every grid point", all(fl$ci_low <= fl$fit & fl$fit <= fl$ci_high))
  ab <- data.table::dcast(cf, Sex ~ term, value.var = "estimate")
  chk <- merge(fl, ab, by = "Sex")
  add("line = intercept + slope x x at every grid point", max(abs(chk$fit - (chk$intercept + chk$slope * chk$x))) <= tol,
      max(abs(chk$fit - (chk$intercept + chk$slope * chk$x))))
  bad <- grep("(^p_|_p$|^p$|statistic|^t$|t_value|p_value)", unique(c(names(fl), names(cf), names(dg))), value = TRUE)
  add("no test statistic or P column in any table (registry section 3)", !length(bad), paste(bad, collapse = ","))
  data.table::rbindlist(g)
}

# ---------------------------------------------------------------- writer (once, read-only)
pmf_blocking_entries <- function(out_root) {
  if (!dir.exists(out_root)) return(character())
  e <- list.files(out_root, all.files = TRUE, no.. = TRUE)
  e[grepl(PMF_REAL_OUT_PATTERN, e)]
}

#' Write the run once: refuses if the stage folder holds any earlier REAL output or staging folder (any commit). Tables and
#' audit CSVs (full precision, re-read exactly) and README go to .tmp_<run_name>; audit/output_manifest.csv lists every
#' other file (file, bytes, sha256); the folder is renamed and its files set 0444; then the manifest is re-verified.
#' `.before_verify` is a test hook only.
pmf_write_run <- function(out_root, run_name, tables, audit, readme, .before_verify = NULL) {
  blk <- pmf_blocking_entries(out_root)
  if (length(blk)) stop("Refusing to write: an earlier REAL run or staging folder exists (registry: fitted once): ",
                        paste(file.path(out_root, blk), collapse = ", "), call. = FALSE)
  if (!setequal(names(tables), names(PMF_TABLE_FILES)) || !setequal(names(audit), names(PMF_AUDIT_FILES)))
    stop("Table / audit set differs from the declared set.", call. = FALSE)
  bd <- file.path(out_root, run_name); stg <- file.path(out_root, paste0(".tmp_", run_name))
  if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)
  dir.create(file.path(stg, "tables"), recursive = TRUE); dir.create(file.path(stg, "audit"))
  for (k in names(PMF_TABLE_FILES)) s30fb_write_csv(phc_no_dquote(tables[[k]]), file.path(stg, PMF_TABLE_FILES[[k]]))
  for (k in names(PMF_AUDIT_FILES)) s30fb_write_csv(phc_no_dquote(audit[[k]]), file.path(stg, PMF_AUDIT_FILES[[k]]))
  con <- file(file.path(stg, PMF_README), open = "wb"); writeLines(enc2utf8(readme), con, useBytes = TRUE); close(con)
  rel <- sort(c(unname(PMF_TABLE_FILES), unname(PMF_AUDIT_FILES), PMF_README))
  man <- data.table::data.table(file = rel, bytes = as.numeric(file.size(file.path(stg, rel))),
                                sha256 = vapply(file.path(stg, rel), s30fb_sha, "", USE.NAMES = FALSE))
  s30fb_write_csv(man, file.path(stg, PMF_MANIFEST_FILE))
  if (dir.exists(bd)) stop("Run folder appeared during the write: ", bd, call. = FALSE)
  if (!file.rename(stg, bd)) stop("Could not rename ", stg, " to ", bd, call. = FALSE)
  Sys.chmod(list.files(bd, recursive = TRUE, full.names = TRUE), mode = "0444")
  if (is.function(.before_verify)) .before_verify(bd)
  post <- pmf_verify_written(bd, man)
  if (!all(post$passed)) stop("Post-write verification failed (folder left for inspection): ", paste(post$gate[!post$passed], collapse = "; "), call. = FALSE)
  list(dir = bd, manifest = man, manifest_sha256 = s30fb_sha(file.path(bd, PMF_MANIFEST_FILE)), post_gates = post)
}

#' Post-write verification: manifest read back, every file = manifest, nothing else, every file read-only.
pmf_verify_written <- function(bd, manifest) {
  m <- data.table::as.data.table(manifest); mf <- file.path(bd, PMF_MANIFEST_FILE)
  back <- if (file.exists(mf)) data.table::fread(mf, colClasses = "character") else data.table::data.table(file = character(), sha256 = character())
  chk <- s30fb_manifest_check(bd, m); extra <- setdiff(s30fb_files_on_disk(bd), c(m$file, PMF_MANIFEST_FILE))
  fl <- list.files(bd, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  rbind(phc_gate("5-write", "output_manifest read back = the manifest written", identical(back$file, m$file) && identical(back$sha256, m$sha256), mf),
        phc_gate("5-write", "every file = output_manifest (bytes, SHA-256); nothing else in the folder", nrow(chk) > 0L && all(chk$ok) && !length(extra),
                 paste(c(chk$file[!chk$ok], extra), collapse = ",")),
        phc_gate("5-write", "every written file is read-only", length(fl) > 0L && all(file.access(fl, 2L) != 0L), bd))
}
