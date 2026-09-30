# ================================================================
# Post hoc CON / RES / SUS contrasts, first active phase after CC1 (registry v1.0): helpers
# MMMSociability -- Functions/posthoc_con_contrasts.R
# ================================================================
# Pure constants and functions for Analysis/29b_posthoc_con_contrasts.R. Sourcing this file reads and writes nothing.
# Implements docs/POSTHOC_CON_CONTRASTS_REGISTRY_v1.0.md (FROZEN 2026-09-30T15:40:15+0200, commit 322be31, sha256 a6435d8d...)
# and nothing else:
#   * data: ebb_v101 A1_animal_cc1 (first full active phase after CC1), crossing_rate and shared_zone_use; rows with a
#     non-finite value are excluded (as Stage 29: OQ770 / OQ771 for shared_zone_use);
#   * one fit per measure x sex (4 fits): y ~ Batch + group + (0 + conCage | CageEpisodeID) + (0 + sisCage | CageEpisodeID),
#     group = factor(CON [reference], RES, SUS), conCage = 1 if every animal of the cage epoch is CON (mmm_ci_code_design);
#     REML, bobyqa, through the frozen Stage 29 engine mmm_ci_fit() (Functions/rfid_canonical_inference.R);
#   * Kenward-Roger t / CI through lmerTest::contest (mmm_ci_contrast) on named L-vectors (registry section 3):
#       batch-balanced group means (estimation only): (Intercept) + 1/3 x each within-sex Batch dummy [+ group coefficient];
#       RES - CON = groupRES; SUS - CON = groupSUS; SUS - RES = groupSUS - groupRES (tested);
#   * Holm within each measure x sex over its 3 contrasts (m = 3; mmm_ci_holm keeps m when a member FAILED); no global correction;
#   * sensitivity (reported only): the same estimands under y ~ Batch + group + (1 | CageEpisodeID);
#   * failure rule (registry section 5): a fit that errors, is FAILED by the engine or does not converge is FAILED and carries
#     no estimate; lmerTest's Kenward-Roger -> Satterthwaite fallback (a warning) FAILS the row; singular fits are kept and flagged.
# Tier: POST HOC exploratory (USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION, 2026-09-30); it changes no frozen Stage 29 family or result.
# Requires: data.table, digest, lme4, lmerTest, pbkrtest, reformulas; Functions/rfid_canonical_inference.R (mmm_ci_*) and
# Functions/stage30_figure_bundle.R (s30fb_* hash and writer utilities) sourced first.
# ================================================================

PHC_VERSION <- "1.0"
PHC_REGISTRY <- list(
  version = "1.0", file = "POSTHOC_CON_CONTRASTS_REGISTRY_v1.0.md", repo_path = "docs/POSTHOC_CON_CONTRASTS_REGISTRY_v1.0.md",
  sha256 = "a6435d8d84b164671b59486f7a3eb9af2bff29bd251c58866cd4b777f9fa0604",
  canonical_rel = file.path("canonical", "posthoc_con_contrasts_registry", "v1.0"), sha_file = "REGISTRY_SHA256.txt",
  freeze_commit = "322be31af04003b1a573f5ec3ee09b013046175c", frozen_at = "2026-09-30T15:40:15+0200")
PHC_STAGE29 <- list(bundle_id = "ebb_v101_20260929_b2ce507",
                    manifest_sha256 = "ec59aa337fc63511de4aa2bf3e1ee1f4b34b84d6d671565f74e0bb3939f7226c",
                    a1_file = "A1_animal_cc1.csv", a1_sha256 = "c782b38796ad35cc45d246520869ab6f7fd327afd2e197fb4d00407e326b50a4",
                    config_file = "I_analysis_config.json")
# the frozen engine versions (Stage 29 v1.0.1 / Stage 30 package gate)
PHC_PACKAGES <- c(lme4 = "2.0.1", lmerTest = "3.2.1", pbkrtest = "0.5.5")
PHC_STAGE_DIR <- "29b_posthoc_con_contrasts"
# any earlier REAL output or staging folder (at any commit) blocks a REAL run: the registry allows exactly one
PHC_REAL_OUT_PATTERN <- "^(\\.tmp_)?v1\\.0_"
PHC_KR_FALLBACK_PATTERN <- "Unable to compute Kenward-Roger"
PHC_OPTIN_ENV <- "MMM_POSTHOC29B_REAL_RUN"

PHC_MEASURES <- c("crossing_rate", "shared_zone_use")
PHC_SEXES <- c("Female", "Male")
PHC_GROUPS <- c("CON", "RES", "SUS")
PHC_SEX_BATCHES <- list(Female = c("B3", "B4", "B6"), Male = c("B1", "B2", "B5"))
PHC_N <- list(animals = 111L, con_cages_per_sex = 3L, con_per_cage = 4L,
              by_sex_group = list(Female = c(CON = 12L, RES = 28L, SUS = 18L), Male = c(CON = 12L, RES = 25L, SUS = 16L)))
PHC_NONFINITE_EXPECTED <- list(crossing_rate = character(), shared_zone_use = c("OQ770", "OQ771"))
PHC_MODELS <- list(
  primary = list(model = "primary_separate_cage_variances",
                 formula = "y ~ Batch + group + (0 + conCage | CageEpisodeID) + (0 + sisCage | CageEpisodeID)"),
  sensitivity = list(model = "sensitivity_common_cage_variance", formula = "y ~ Batch + group + (1 | CageEpisodeID)"))
PHC_EXPECTED_RANK <- 5L
PHC_MEANS <- c(CON_mean = "CON", RES_mean = "RES", SUS_mean = "SUS")
PHC_CONTRASTS <- list(RES_minus_CON = c(groupRES = 1), SUS_minus_CON = c(groupSUS = 1), SUS_minus_RES = c(groupSUS = 1, groupRES = -1))
PHC_ESTIMANDS <- c(names(PHC_MEANS), names(PHC_CONTRASTS))
.phc_minus <- intToUtf8(0x2212L)
PHC_ESTIMAND_LABEL <- c(CON_mean = "CON", RES_mean = "RES", SUS_mean = "SUS",
                        RES_minus_CON = paste("RES", .phc_minus, "CON"), SUS_minus_CON = paste("SUS", .phc_minus, "CON"),
                        SUS_minus_RES = paste("SUS", .phc_minus, "RES"))
PHC_HOLM_M <- 3L
PHC_TIER <- "POST HOC exploratory"
PHC_DECISION_BASIS <- "USER_REQUEST_AFTER_DESCRIPTIVE_INSPECTION (2026-09-30)"
PHC_DISPLAY_NOTE <- "Post hoc, cage-aware; CON = 3 cages/sex"
PHC_CAVEATS <- paste("CON = 3 cages per sex, one per batch; the CON vs SIS comparisons confound social instability with regrouping itself",
                     "and with the platform assignment at CC1; sex is nested in batch")
PHC_REGISTERED_NOTE <- paste("RES vs SUS: the registered Stage 29 inference (FU-CC1, RES - SUS within sex, Holm over sexes; P-CC1, the sex",
                             "difference in RES - SUS) remains authoritative; SUS - RES is shown here only so that all three pairwise",
                             "contrasts come from one model")
PHC_FAILURE_NONCONVERGED <- "registry section 5: the fit did not converge"
PHC_TABLE_FILES <- c(estimates = "tables/estimates.csv", sensitivity = "tables/sensitivity_common_cage_variance.csv",
                     diagnostics = "tables/diagnostics.csv")
PHC_AUDIT_FILES <- c(gates = "audit/gate_results.csv", inputs = "audit/input_hashes.csv", run = "audit/run_manifest.csv")
PHC_MANIFEST_FILE <- "audit/output_manifest.csv"
PHC_README <- "README.txt"

# ---------------------------------------------------------------- mode, identity, hashes
#' The only mode is --real (registry section 7: one REAL run; the synthetic tests are the only rehearsal).
phc_parse_mode <- function(args) {
  args <- as.character(args)
  if (!identical(args, "--real"))
    stop("Refusing to run: the only mode is --real (registry v1.0 section 7: fitted once; no dry run on project data). ",
         "Synthetic checks: Rscript Testing/tests/test_posthoc_con_contrasts.R", call. = FALSE)
  "REAL"
}
phc_run_name <- function(commit) {
  if (!grepl("^[0-9a-f]{40}$", commit)) stop("Not a full git commit hash: ", commit, call. = FALSE)
  paste0("v", PHC_VERSION, "_", substr(commit, 1, 7))
}
#' SHA-256 of a text file with every CR byte removed (the committed LF form of a file checked out with CRLF).
phc_sha_lf <- function(path) {
  b <- readBin(path, what = "raw", n = file.size(path))
  digest::digest(b[b != as.raw(13L)], algo = "sha256", serialize = FALSE)
}
#' SHA-256 listed for `name` in a "<sha>  <name>" list (comment lines ignored); NA unless exactly one entry.
phc_sha_list_lookup <- function(lines, name) {
  x <- trimws(gsub("\r", "", lines, fixed = TRUE)); x <- x[grepl("^[0-9a-f]{64}\\s", x)]
  sha <- sub("^([0-9a-f]{64})\\s.*$", "\\1", x); nm <- trimws(sub("^[0-9a-f]{64}\\s+[*]?", "", x))
  hit <- basename(nm) == name
  if (sum(hit) != 1L) NA_character_ else sha[hit]
}
phc_gate <- function(stage, gate, passed, detail = "") s30fb_gate_row(stage, gate, passed, detail, hard = TRUE)

#' Registry gates: the read-only S: copy (bytes) and the repository copy (LF form) both hash to the frozen sha256, the
#' S: REGISTRY_SHA256.txt lists it, the S: files are read-only and the status line says FROZEN.
phc_registry_gates <- function(canon_dir, repo_file, expect = PHC_REGISTRY) {
  s_md <- file.path(canon_dir, expect$file); s_sha <- file.path(canon_dir, expect$sha_file)
  sha_s <- if (file.exists(s_md)) s30fb_sha(s_md) else NA_character_
  sha_r <- if (file.exists(repo_file)) phc_sha_lf(repo_file) else NA_character_
  listed <- if (file.exists(s_sha)) phc_sha_list_lookup(readLines(s_sha, warn = FALSE), expect$file) else NA_character_
  st <- if (file.exists(s_md)) grep("^\\*\\*Status\\.\\*\\*", readLines(s_md, warn = FALSE, encoding = "UTF-8"), value = TRUE) else character()
  ro <- c(s_md, s_sha); ro <- ro[file.exists(ro)]
  rbind(phc_gate("0-registry", "registry S: copy SHA-256 = frozen", identical(sha_s, expect$sha256), paste(s_md, sha_s)),
        phc_gate("0-registry", "registry S: REGISTRY_SHA256.txt lists the frozen SHA-256", identical(listed, expect$sha256), paste(s_sha, listed)),
        phc_gate("0-registry", "registry repository copy (LF form) SHA-256 = frozen", identical(sha_r, expect$sha256), paste(repo_file, sha_r)),
        phc_gate("0-registry", "registry status line: FROZEN", length(st) == 1L && grepl("FROZEN", st, fixed = TRUE), st),
        phc_gate("0-registry", "registry S: files read-only", length(ro) == 2L && all(file.access(ro, 2L) != 0L), paste(ro, collapse = "; ")))
}

#' Package gate: the frozen engine versions.
phc_package_gates <- function(expected = PHC_PACKAGES) {
  obs <- vapply(names(expected), function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_), "")
  data.table::rbindlist(lapply(names(expected), function(p)
    phc_gate("0-packages", paste0(p, " version = frozen engine version ", expected[[p]]), identical(obs[[p]], expected[[p]]), obs[[p]])))
}

# ---------------------------------------------------------------- data and population
#' Coded design table from A1 (all animals first, so conCage / sisCage use every animal of the cage epoch).
phc_prepare <- function(a1) {
  a1 <- data.table::as.data.table(a1)
  need <- c("AnimalNum", "Sex", "Batch", "Group", "CC", "CageEpisodeID", PHC_MEASURES)
  miss <- setdiff(need, names(a1)); if (length(miss)) stop("A1 lacks column(s): ", paste(miss, collapse = ","), call. = FALSE)
  if (!all(a1$Group %in% PHC_GROUPS)) stop("A1 Group outside CON / RES / SUS.", call. = FALSE)
  d <- mmm_ci_code_design(a1)
  d[, group := factor(Group, levels = PHC_GROUPS)]
  d[]
}

#' Population gates on the coded design table (registry section 1). Returns gate rows.
phc_population_gates <- function(d, n = PHC_N, sex_batches = PHC_SEX_BATCHES, nonfinite = PHC_NONFINITE_EXPECTED) {
  d <- data.table::as.data.table(d); g <- list(); add <- function(name, ok, detail = "") g[[length(g) + 1L]] <<- phc_gate("2-population", name, ok, detail)
  add(sprintf("A1: %d animals, one row each, all CC1", n$animals), nrow(d) == n$animals && !anyDuplicated(d$AnimalNum) && all(d$CC == "CC1"),
      paste(nrow(d), data.table::uniqueN(d$AnimalNum), paste(unique(d$CC), collapse = ",")))
  cnt <- d[, .N, by = .(Sex, Group)]
  ok_cnt <- all(vapply(names(n$by_sex_group), function(s) all(vapply(names(n$by_sex_group[[s]]), function(gr)
    isTRUE(cnt[Sex == s & Group == gr, N] == n$by_sex_group[[s]][[gr]]), TRUE)), TRUE)) && nrow(cnt) == sum(lengths(n$by_sex_group))
  add("animals per Sex x Group = expected", ok_cnt, paste(cnt[order(Sex, Group), paste0(Sex, " ", Group, " ", N)], collapse = ", "))
  bs <- d[, .(b = paste(sort(unique(Batch)), collapse = ",")), by = Sex]
  add("Sex nested in Batch: Female B3/B4/B6, Male B1/B2/B5",
      nrow(bs) == length(sex_batches) && all(vapply(names(sex_batches), function(s) identical(bs[Sex == s, b], paste(sort(sex_batches[[s]]), collapse = ",")), TRUE)),
      paste(bs$Sex, bs$b, collapse = "; "))
  cc <- d[conCage == 1, .(n_animals = .N, n_con = sum(Group == "CON"), Batch = Batch[1], nb = data.table::uniqueN(Batch)), by = .(Sex, CageEpisodeID)]
  add(sprintf("exactly %d CON-only cage epochs per sex, one per batch, %d CON animals each", n$con_cages_per_sex, n$con_per_cage),
      all(vapply(names(sex_batches), function(s) { x <- cc[Sex == s]
        nrow(x) == n$con_cages_per_sex && setequal(x$Batch, sex_batches[[s]]) && !anyDuplicated(x$Batch) && all(x$nb == 1L) &&
          all(x$n_animals == n$con_per_cage) && all(x$n_con == n$con_per_cage) }, TRUE)),
      paste(cc[, paste0(Sex, " ", CageEpisodeID, " n", n_animals)], collapse = ", "))
  add("every CON animal is in a CON-only cage epoch; no SIS animal is", all(d[Group == "CON", conCage] == 1) && all(d[Group != "CON", conCage] == 0))
  add("sisCage = 1 - conCage", all(d$sisCage == 1 - d$conCage))
  for (k in names(nonfinite)) { nf <- sort(d[!is.finite(get(k)), AnimalNum])
    add(sprintf("%s: non-finite rows = expected (%s)", k, if (length(nonfinite[[k]])) paste(nonfinite[[k]], collapse = ", ") else "none"),
        identical(nf, sort(as.character(nonfinite[[k]]))), paste(nf, collapse = ",")) }
  data.table::rbindlist(g)
}

#' Analysis rows for one measure x sex: finite values only; y = the measure.
phc_subset <- function(d, measure, sex) {
  x <- data.table::copy(d[Sex == sex & is.finite(get(measure))]); x[, y := get(measure)]
  x[, Batch := droplevels(factor(Batch))][]
}

# ---------------------------------------------------------------- estimands
#' Named weight vectors (L on the fixed effects) for the six estimands, from the batch levels of the analysis rows:
#' means = (Intercept) + 1/n_batch x each Batch dummy [+ group coefficient]; contrasts on the group coefficients.
phc_estimand_weights <- function(batch_levels) {
  bl <- sort(unique(as.character(batch_levels)))
  if (length(bl) < 2L) stop("Need at least two batches for the batch-balanced means.", call. = FALSE)
  bd <- paste0("Batch", bl[-1])
  base <- c(`(Intercept)` = 1, stats::setNames(rep(1 / length(bl), length(bd)), bd))
  means <- lapply(PHC_MEANS, function(gr) if (gr == "CON") base else c(base, stats::setNames(1, paste0("group", gr))))
  c(means, PHC_CONTRASTS)
}

#' Evaluate a KR contrast under a warning handler; lmerTest's KR -> Satterthwaite fallback FAILS the row (no estimate).
phc_kr_guard <- function(expr) {
  msgs <- character()
  x <- withCallingHandlers(expr, warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") })
  x <- data.table::as.data.table(x)
  x[, kr_messages := paste(unique(msgs), collapse = " | ")]
  x[, failure_reason := NA_character_]
  if (any(grepl(PHC_KR_FALLBACK_PATTERN, msgs, fixed = TRUE))) {
    for (cl in intersect(c("estimate", "se", "df", "statistic", "ci_low", "ci_high", "p_raw"), names(x))) data.table::set(x, j = cl, value = NA_real_)
    x[, `:=`(status = "FAILED", failure_reason = "KR unavailable: lmerTest fell back to Satterthwaite (never reported as KR)")]
  }
  x[]
}

#' Fit one model with the frozen engine; any stop (rank, expected rank, dropped columns) becomes an explicit FAILED stub.
#' Registry section 5: non-convergence FAILS the fit (the engine's optimizer-check verdict is recorded, not used to rescue it).
phc_fit <- function(d, formula, model_id, expected_rank = PHC_EXPECTED_RANK) {
  m <- tryCatch(mmm_ci_fit(formula, d, model_id, expected_rank = expected_rank), error = function(e) e)
  if (inherits(m, "error")) {
    info <- data.table::data.table(model_id = model_id, engine = "lmer", formula = formula, n_obs = nrow(d),
      n_animals = data.table::uniqueN(d$AnimalNum), n_cage_episodes = data.table::uniqueN(d$CageEpisodeID), n_batches = data.table::uniqueN(d$Batch),
      n_fixed_cols = NA_integer_, rank = NA_integer_, expected_rank = expected_rank, singular = NA, converged = FALSE, messages = "",
      optimizer_check_agree = NA, failed = TRUE, optimizer_check_messages = NA_character_, error = conditionMessage(m))
    return(list(fit = NULL, info = info, data = d, failure_reason = paste("fit stopped:", conditionMessage(m))))
  }
  reason <- NA_character_
  if (isTRUE(m$info$failed)) reason <- if (!is.na(m$info$error)) paste("fit error:", m$info$error) else "engine failure rule: non-converged and optimizers disagree"
  else if (!isTRUE(m$info$converged)) { reason <- PHC_FAILURE_NONCONVERGED; m$info[, failed := TRUE] }
  m$failure_reason <- reason
  m
}

#' Variance components of a fit (NA when FAILED / absent).
phc_variance_components <- function(m) {
  out <- c(vc_con_cage = NA_real_, vc_sis_cage = NA_real_, vc_cage = NA_real_, vc_residual = NA_real_)
  if (is.null(m$fit)) return(out)
  vc <- as.data.frame(lme4::VarCorr(m$fit)); vc <- vc[is.na(vc$var2), ]
  pick <- function(sel) if (any(sel)) sum(vc$vcov[sel]) else NA_real_
  out[["vc_con_cage"]] <- pick(vc$var1 %in% "conCage"); out[["vc_sis_cage"]] <- pick(vc$var1 %in% "sisCage")
  out[["vc_cage"]] <- pick(vc$grp == "CageEpisodeID" & vc$var1 %in% "(Intercept)"); out[["vc_residual"]] <- pick(vc$grp == "Residual")
  out
}

#' One family (measure x sex x model): the fit, its six estimand rows (Holm over the 3 contrasts) and its diagnostics row.
phc_fit_family <- function(d, measure, sex, which = c("primary", "sensitivity"), excluded = character()) {
  which <- match.arg(which); spec <- PHC_MODELS[[which]]
  model_id <- paste("PHC", which, measure, sex, sep = "|")
  m <- phc_fit(d, spec$formula, model_id)
  W <- phc_estimand_weights(d$Batch)
  rows <- data.table::rbindlist(lapply(PHC_ESTIMANDS, function(e) phc_kr_guard(mmm_ci_contrast(m, as.list(W[[e]]), e))), fill = TRUE)
  if (!is.na(m$failure_reason)) rows[status == "FAILED" & is.na(failure_reason), failure_reason := m$failure_reason]
  rows[, estimand_type := ifelse(estimand %in% names(PHC_MEANS), "batch_balanced_mean", "contrast")]
  rows[, tested := estimand_type == "contrast"]
  rows[estimand_type == "batch_balanced_mean", `:=`(statistic = NA_real_, p_raw = NA_real_, test = "KR CI (estimation only; no test)")]
  rows[, p_holm := NA_real_]
  ic <- which(rows$tested)
  rows[ic, p_holm := mmm_ci_holm(p_raw)]
  if (length(ic) != PHC_HOLM_M) stop("Holm family ", model_id, " does not have ", PHC_HOLM_M, " members.", call. = FALSE)
  n_g <- vapply(PHC_GROUPS, function(gr) sum(d$Group == gr), 1L)
  cages <- unique(d[, .(CageEpisodeID, conCage)])
  rows[, `:=`(measure = measure, Sex = sex, model = spec$model, formula = spec$formula, estimand_label = unname(PHC_ESTIMAND_LABEL[estimand]),
              holm_family = if (which == "primary") paste(measure, sex, sep = "|") else paste(measure, sex, "sensitivity", sep = "|"),
              holm_m = ifelse(tested, PHC_HOLM_M, NA_integer_), singular = m$info$singular, converged = m$info$converged,
              n_obs = nrow(d), n_CON = n_g[["CON"]], n_RES = n_g[["RES"]], n_SUS = n_g[["SUS"]],
              n_con_cages = sum(cages$conCage == 1), n_sis_cages = sum(cages$conCage == 0))]
  vc <- phc_variance_components(m); ct <- rows[tested == TRUE]
  kr_df <- stats::setNames(ct$df, paste0("kr_df_", ct$estimand))
  diag <- data.table::data.table(model_id = model_id, measure = measure, Sex = sex, model = spec$model, formula = spec$formula,
    status = if (isTRUE(m$info$failed) || is.null(m$fit)) "FAILED" else "OK", failure_reason = m$failure_reason,
    n_obs = nrow(d), n_animals = data.table::uniqueN(d$AnimalNum), n_CON = n_g[["CON"]], n_RES = n_g[["RES"]], n_SUS = n_g[["SUS"]],
    n_cage_episodes = nrow(cages), n_con_cages = sum(cages$conCage == 1), n_sis_cages = sum(cages$conCage == 0),
    n_batches = data.table::uniqueN(d$Batch), batches = paste(sort(unique(as.character(d$Batch))), collapse = ";"),
    excluded_nonfinite = paste(sort(excluded), collapse = ";"),
    n_fixed_cols = m$info$n_fixed_cols, rank = m$info$rank, expected_rank = m$info$expected_rank,
    singular = m$info$singular, converged = m$info$converged, optimizer_check_agree = m$info$optimizer_check_agree,
    reml_criterion = if (is.null(m$fit)) NA_real_ else as.numeric(lme4::REMLcrit(m$fit)),
    vc_con_cage = vc[["vc_con_cage"]], vc_sis_cage = vc[["vc_sis_cage"]], vc_cage = vc[["vc_cage"]], vc_residual = vc[["vc_residual"]],
    kr_df_RES_minus_CON = unname(kr_df["kr_df_RES_minus_CON"]), kr_df_SUS_minus_CON = unname(kr_df["kr_df_SUS_minus_CON"]),
    kr_df_SUS_minus_RES = unname(kr_df["kr_df_SUS_minus_RES"]),
    kr_messages = paste(unique(rows$kr_messages[nzchar(rows$kr_messages)]), collapse = " | "),
    fit_messages = m$info$messages, error = m$info$error)
  list(m = m, rows = rows, diag = diag)
}

#' Annotate estimand rows with labels and registry identity; fixed column order.
phc_label_rows <- function(rows, labels = NULL, registry_sha256 = PHC_REGISTRY$sha256) {
  x <- data.table::copy(data.table::as.data.table(rows))
  if (is.null(labels)) labels <- data.table::data.table(measure = PHC_MEASURES, measure_label = NA_character_, unit = NA_character_)
  x <- merge(x, data.table::as.data.table(labels)[, .(measure, measure_label, unit)], by = "measure", all.x = TRUE, sort = FALSE)
  x[, `:=`(tier = PHC_TIER, decision_basis = PHC_DECISION_BASIS, registry_version = PHC_REGISTRY$version, registry_sha256 = registry_sha256,
           display_note = PHC_DISPLAY_NOTE, caveats = PHC_CAVEATS, registered_rs_note = PHC_REGISTERED_NOTE)]
  x[, `:=`(measure_o = match(measure, PHC_MEASURES), sex_o = match(Sex, PHC_SEXES), est_o = match(estimand, PHC_ESTIMANDS))]
  data.table::setorderv(x, c("measure_o", "sex_o", "est_o"))
  cols <- c("measure", "measure_label", "unit", "Sex", "model", "estimand", "estimand_label", "estimand_type", "tested",
            "estimate", "se", "df", "ci_low", "ci_high", "statistic", "p_raw", "p_holm", "holm_family", "holm_m", "df_method", "test", "L",
            "status", "failure_reason", "singular", "converged", "n_obs", "n_CON", "n_RES", "n_SUS", "n_con_cages", "n_sis_cages",
            "model_id", "formula", "kr_messages", "tier", "decision_basis", "registry_version", "registry_sha256", "display_note", "caveats",
            "registered_rs_note")
  x[, cols, with = FALSE]
}

#' Sensitivity table: the sensitivity rows plus agreement with the primary estimate (reported, not used for display).
phc_sensitivity_table <- function(primary, sens) {
  p <- data.table::as.data.table(primary)[, .(measure, Sex, estimand, primary_estimate = estimate, primary_ci_low = ci_low,
                                              primary_ci_high = ci_high, primary_p_holm = p_holm, primary_status = status)]
  x <- merge(data.table::as.data.table(sens), p, by = c("measure", "Sex", "estimand"), all.x = TRUE, sort = FALSE)
  x[, estimate_diff := estimate - primary_estimate]
  x[, same_sign := ifelse(is.na(estimate) | is.na(primary_estimate), NA, sign(estimate) == sign(primary_estimate))]
  x[, ci_excludes_zero := ifelse(tested & !is.na(ci_low), ci_low > 0 | ci_high < 0, NA)]
  x[, primary_ci_excludes_zero := ifelse(tested & !is.na(primary_ci_low), primary_ci_low > 0 | primary_ci_high < 0, NA)]
  x[, ci_zero_agreement := ifelse(tested, ci_excludes_zero == primary_ci_excludes_zero, NA)]
  x[, `:=`(measure_o = match(measure, PHC_MEASURES), sex_o = match(Sex, PHC_SEXES), est_o = match(estimand, PHC_ESTIMANDS))]
  data.table::setorderv(x, c("measure_o", "sex_o", "est_o")); x[, c("measure_o", "sex_o", "est_o") := NULL]
  x[]
}

#' All fits: 2 measures x 2 sexes, primary and sensitivity. Returns list(estimates, sensitivity, diagnostics).
phc_run_all <- function(d, labels = NULL, measures = PHC_MEASURES, sexes = PHC_SEXES, registry_sha256 = PHC_REGISTRY$sha256) {
  prim <- list(); sens <- list(); dg <- list()
  for (k in measures) for (sx in sexes) {
    x <- phc_subset(d, k, sx); excl <- d[Sex == sx & !is.finite(get(k)), AnimalNum]
    a <- phc_fit_family(x, k, sx, "primary", excl); b <- phc_fit_family(x, k, sx, "sensitivity", excl)
    prim[[length(prim) + 1L]] <- a$rows; sens[[length(sens) + 1L]] <- b$rows; dg[[length(dg) + 1L]] <- a$diag; dg[[length(dg) + 1L]] <- b$diag
  }
  est <- phc_label_rows(data.table::rbindlist(prim, fill = TRUE), labels, registry_sha256)
  sen <- phc_sensitivity_table(est, phc_label_rows(data.table::rbindlist(sens, fill = TRUE), labels, registry_sha256))
  list(estimates = est, sensitivity = sen, diagnostics = data.table::rbindlist(dg, fill = TRUE))
}

#' Display labels (label, unit) of the measures, copied from the ebb_v101 configuration `metrics`.
phc_metric_labels <- function(cfg_metrics, measures = PHC_MEASURES) {
  data.table::rbindlist(lapply(measures, function(k) { x <- cfg_metrics[[k]]
    if (is.null(x$label) || is.null(x$unit)) stop("ebb_v101 config has no label / unit for ", k, call. = FALSE)
    data.table::data.table(measure = k, measure_label = x$label, unit = x$unit) }))
}

# ---------------------------------------------------------------- writer (once, read-only)
#' Replace embedded double quotes in text columns by single quotes (s30fb_write_csv refuses them; engine / lme4 messages
#' are free text), so that a message can never abort the one REAL write.
phc_no_dquote <- function(x) {
  x <- data.table::copy(data.table::as.data.table(x))
  for (k in names(x)) if (is.character(x[[k]])) data.table::set(x, j = k, value = gsub("\"", "'", x[[k]], fixed = TRUE))
  x[]
}
phc_blocking_entries <- function(out_root) {
  if (!dir.exists(out_root)) return(character())
  e <- list.files(out_root, all.files = TRUE, no.. = TRUE)
  e[grepl(PHC_REAL_OUT_PATTERN, e)]
}

#' Write the run once: refuses if the stage folder holds any earlier REAL output or staging folder (any commit). Tables and
#' audit CSVs (full precision, re-read exactly) and README go to .tmp_<run_name>; audit/output_manifest.csv lists every
#' other file (file, bytes, sha256); the folder is renamed and its files set 0444; then the manifest is re-verified.
#' `tables` / `audit` are named lists keyed like PHC_TABLE_FILES / PHC_AUDIT_FILES. `.before_verify` is a test hook only.
phc_write_run <- function(out_root, run_name, tables, audit, readme, .before_verify = NULL) {
  blk <- phc_blocking_entries(out_root)
  if (length(blk)) stop("Refusing to write: an earlier REAL run or staging folder exists (registry: fitted once): ",
                        paste(file.path(out_root, blk), collapse = ", "), call. = FALSE)
  if (!setequal(names(tables), names(PHC_TABLE_FILES)) || !setequal(names(audit), names(PHC_AUDIT_FILES)))
    stop("Table / audit set differs from the declared set.", call. = FALSE)
  bd <- file.path(out_root, run_name); stg <- file.path(out_root, paste0(".tmp_", run_name))
  if (!dir.exists(out_root)) dir.create(out_root, recursive = TRUE)
  dir.create(file.path(stg, "tables"), recursive = TRUE); dir.create(file.path(stg, "audit"))
  for (k in names(PHC_TABLE_FILES)) s30fb_write_csv(phc_no_dquote(tables[[k]]), file.path(stg, PHC_TABLE_FILES[[k]]))
  for (k in names(PHC_AUDIT_FILES)) s30fb_write_csv(phc_no_dquote(audit[[k]]), file.path(stg, PHC_AUDIT_FILES[[k]]))
  con <- file(file.path(stg, PHC_README), open = "wb"); writeLines(enc2utf8(readme), con, useBytes = TRUE); close(con)
  rel <- sort(c(unname(PHC_TABLE_FILES), unname(PHC_AUDIT_FILES), PHC_README))
  man <- data.table::data.table(file = rel, bytes = as.numeric(file.size(file.path(stg, rel))),
                                sha256 = vapply(file.path(stg, rel), s30fb_sha, "", USE.NAMES = FALSE))
  s30fb_write_csv(man, file.path(stg, PHC_MANIFEST_FILE))
  if (dir.exists(bd)) stop("Run folder appeared during the write: ", bd, call. = FALSE)
  if (!file.rename(stg, bd)) stop("Could not rename ", stg, " to ", bd, call. = FALSE)
  Sys.chmod(list.files(bd, recursive = TRUE, full.names = TRUE), mode = "0444")
  if (is.function(.before_verify)) .before_verify(bd)
  post <- phc_verify_written(bd, man)
  if (!all(post$passed)) stop("Post-write verification failed (folder left for inspection): ", paste(post$gate[!post$passed], collapse = "; "), call. = FALSE)
  list(dir = bd, manifest = man, manifest_sha256 = s30fb_sha(file.path(bd, PHC_MANIFEST_FILE)), post_gates = post)
}

#' Post-write verification: manifest read back, every file = manifest, nothing else, every file read-only.
phc_verify_written <- function(bd, manifest) {
  m <- data.table::as.data.table(manifest); mf <- file.path(bd, PHC_MANIFEST_FILE)
  back <- if (file.exists(mf)) data.table::fread(mf, colClasses = "character") else data.table::data.table(file = character(), sha256 = character())
  chk <- s30fb_manifest_check(bd, m); extra <- setdiff(s30fb_files_on_disk(bd), c(m$file, PHC_MANIFEST_FILE))
  fl <- list.files(bd, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  rbind(phc_gate("5-write", "output_manifest read back = the manifest written", identical(back$file, m$file) && identical(back$sha256, m$sha256), mf),
        phc_gate("5-write", "every file = output_manifest (bytes, SHA-256); nothing else in the folder", nrow(chk) > 0L && all(chk$ok) && !length(extra),
                 paste(c(chk$file[!chk$ok], extra), collapse = ",")),
        phc_gate("5-write", "every written file is read-only", length(fl) > 0L && all(file.access(fl, 2L) != 0L), bd))
}
