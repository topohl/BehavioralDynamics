# ================================================================
# Stage 33 - shared constants, design and estimator helpers
# MMMSociability
# ================================================================
# Post hoc, descriptive (estimation only) follow-ups of the frozen plan
# docs/STAGE33_POSTHOC_COHORT_FOLLOWUPS_PLAN_v1.0.md (sections 0, 3 and 7 are implemented here; the run machinery is in
# Functions/stage33_run.R and the modules in Functions/stage33_<a-e>_*.R). Definitions only: sourcing this file reads
# nothing and writes nothing. No p-value or test statistic leaves these helpers: the frozen engine's p and statistic
# columns are dropped at source, and clubSandwich is called with p_values = FALSE.
#
# Requires data.table; the KR wrappers need Functions/stage32_inference.R (s32i_fit, s32i_contrast) and its sources,
# the CR2 wrappers need clubSandwich.
#
# ---------------------------------------------------------------- MODULE CONTRACT
# Each module m in a-e provides two entry points, called by Analysis/33_posthoc_cohort_followups.R:
#
#   p2  <- s33<m>_phase2(design, ctx)
#          Outcome-free part (plan section 6, Phase 2). Must not read or use any outcome. Returns
#          list(products = named list of data.tables   # declared outcome-free products, hashed by the runner
#               gates    = data.table                  # s33_gate_rows() rows
#               state    = list(...))                  # anything Phase 3 of this module needs
#   out <- s33<m>_phase3(design_out, p2, ctx)
#          Returns list(tables = named list of data.tables (names = S33_TABLES[[<M>]], without '.csv'),
#                       audit  = named list of data.tables (module audit tables, names as in the plan),
#                       gates  = data.table (s33_gate_rows() rows),
#                       checkpoints = data.table(key, file, step, reused)).
#
# `design` (Phase 2, outcome-free) is a list built by s33_design():
#   $animals   117 rows, one per canonical animal (key AnimalNum), see S33_DESIGN_ANIMAL_COLUMNS
#   $episodes  444 rows, tracked animal x CC1-CC4 (from the ebb_v101 bundle B1 table): AnimalNum, Batch, Sex, condition,
#              CC, CageEpisodeID, board, crossing_rate, shared_zone_use, n_in_cage, n_tracked_mates, hardware_flag,
#              in_S13
#   $cages     one row per cage episode with tracked members (CEID): CageEpisodeID, Batch, Sex, CC, board, condition,
#              n_tracked, members (';'-joined AnimalNum)
#   $lags      24 rows, Batch x CC: rec_start (POSIXct UTC), anchor_1830, lag_h, board_first_lag_h
#   $sets      named list: HW_A1 ('AnimalNum|CCk'), S13 ('AnimalNum|CCk'), XMATE, J7, J9, UNTRACKED_B1, SINGLE_CC1,
#              EXCLUDED (the excluded-animals list)
#   $source_cages  Batch, src_cage, n_sis, n_con, line, dob (constant within SIS source cages)
#   $planning  the Social Golfer plan rows used for UNTRACKED_B1 (B1)
# `design_out` (Phase 3) = design with outcome columns added to $animals by s33_attach_outcomes() (S33_OUTCOME_COLUMNS).
# `ctx` (built by the runner): root, ar, ep, pl, sl, a4, repo, local_root, run_id, modules, partners (modules present),
#   seeds = S33_SEEDS, B = S33_B, checkpoint_dir, inputs (named character vector of paths), run_mode.
# ================================================================

# ---------------------------------------------------------------- constants (plan sections 0 and 7)
S33_TIER <- "post hoc, descriptive (estimation only); not registered; not a re-test of Stage 29/29b/30/32 hypotheses"
S33_COHORTS <- paste0("B", 1:6)
S33_SEX_OF_COHORT <- c(B1 = "Male", B2 = "Male", B3 = "Female", B4 = "Female", B5 = "Male", B6 = "Female")
S33_CC <- paste0("CC", 1:4)
S33_RU <- 6                       # one rate unit = 6 position changes/hour (legacy Movement_mean = rate/6)
S33_SD_RATE <- 5.1698             # frozen SDs (constants; recomputed values are gates)
S33_SD_OCC <- 0.0607
S33_METRIC_LABEL <- c(
  crossing_rate = "RFID position-change rate",
  shared_zone_use = "shared RFID-position occupancy",
  Movement_mean = "legacy Stage 09 identifier Movement_mean (= rate/6 within 0.21%)",
  arena_distance = "SLEAP arena distance (cm)")
S33_UNITS <- list(rate = "position changes/hour", ru = "per 6 position changes/hour",
                  per_sd_rate = "per frozen SD (5.1698 position changes/hour)",
                  per_sd_occ = "per frozen SD (0.0607 shared occupancy)")
S33_LEVELS <- c("cohort", "cage_within_cohort", "source_cage_within_cohort", "animal_within_cohort", "within_animal",
                "descriptive_record")
S33_OUTCOME_COLUMNS <- c("CombZ", "outcome_group", "NOR", "sucrose_pref", "weight_dev", "delta_cort", "adrenal_weight",
                         "spleen_weight", "n_components_present", "combz_as_recorded", "tp3", "tp4", "tp5", "tp6",
                         "tp3_date", "tp4_date", "tp5_date", "tp6_date", "cort_baseline", "cort_response",
                         "cycle_phase", "Group", "CombZ_wb")
S33_DESIGN_ANIMAL_COLUMNS <- c(
  "AnimalNum", "Batch", "Sex", "condition", "tracked", "untracked_b1", "xmate_excluded",
  "pop_tracked_111", "pop_sis_87", "pop_focal_85", "pop_sis_93", "pop_canonical_117", "pop_con_24",
  "cc1_cage", "cc2_cage", "cc3_cage", "cc4_cage", "cc1_board", "src_cage", "line", "line_J", "J7", "J9",
  "dob", "cc1_date", "age_cc1", "tp1", "tp2", "tp1_date", "tp2_date",
  "crossing_rate", "x_RU", "shared_zone_use", "hw_a1", "n_in_cage", "n_tracked_mates")
# A row of an estimate table carries these columns (plan section 7); modules add their own descriptive columns.
S33_ESTIMATE_COLUMNS <- c("metric_id", "metric_label", "estimand", "level", "estimate", "se", "df", "ci_low", "ci_high",
                          "interval_method", "interval_basis", "units", "status", "singular", "ddf_fallback", "lead",
                          "n_animals", "n_cages", "n_cohorts", "n_params", "tier")

`%s33or%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# ---------------------------------------------------------------- estimate rows
#' One or more estimate rows with the declared columns (plan section 7). Missing fields are NA; `tier` is always set.
s33_row <- function(estimand, level, estimate = NA_real_, se = NA_real_, df = NA_real_, ci_low = NA_real_,
                    ci_high = NA_real_, interval_method = "none", interval_basis = NA_character_, units = NA_character_,
                    metric_id = NA_character_, status = "OK", singular = NA, ddf_fallback = NA, lead = FALSE,
                    n_animals = NA_integer_, n_cages = NA_integer_, n_cohorts = NA_integer_, n_params = NA_integer_, ...) {
  if (!all(level %in% S33_LEVELS)) stop("Unknown level: ", paste(setdiff(level, S33_LEVELS), collapse = ", "), call. = FALSE)
  lab <- unname(S33_METRIC_LABEL[metric_id]); lab[is.na(metric_id)] <- NA_character_
  x <- data.table::data.table(metric_id = metric_id, metric_label = lab, estimand = estimand, level = level,
    estimate = as.numeric(estimate), se = as.numeric(se), df = as.numeric(df), ci_low = as.numeric(ci_low),
    ci_high = as.numeric(ci_high), interval_method = interval_method, interval_basis = interval_basis, units = units,
    status = status, singular = as.logical(singular), ddf_fallback = as.logical(ddf_fallback), lead = as.logical(lead),
    n_animals = as.integer(n_animals), n_cages = as.integer(n_cages), n_cohorts = as.integer(n_cohorts),
    n_params = as.integer(n_params), ...)
  x[, tier := S33_TIER][]
}

#' Intervals of an estimate from its SE and df (t) or none.
s33_t_interval <- function(est, se, df, level = 0.95) {
  q <- ifelse(is.finite(df) & df > 0, stats::qt(1 - (1 - level) / 2, df), NA_real_)
  list(ci_low = est - q * se, ci_high = est + q * se)
}

# ---------------------------------------------------------------- outcome-free assertion
#' Stop if an object built in Phase 2 carries an outcome column (plan section 6, Phase 2).
s33_assert_outcome_free <- function(x, what = "object") {
  cols <- if (is.data.frame(x)) names(x) else if (is.list(x)) unlist(lapply(x, function(z) if (is.data.frame(z)) names(z))) else character()
  bad <- intersect(cols, S33_OUTCOME_COLUMNS)
  if (length(bad)) stop("Outcome column(s) in the outcome-free ", what, ": ", paste(bad, collapse = ", "), call. = FALSE)
  if (is.data.frame(x) && "condition" %in% names(x) && any(x$condition %in% c("RES", "SUS"), na.rm = TRUE))
    stop("Outcome labels (RES/SUS) in the outcome-free ", what, call. = FALSE)
  invisible(TRUE)
}

# ---------------------------------------------------------------- resampling (plan section 7)
#' Within-cell cage resampling indices for one stream: one set.seed, then for every cell in the declared order a
#' B x G matrix matrix(sample.int(G, G * B, replace = TRUE), nrow = B, byrow = TRUE). `cells` is a named integer vector
#' of cage counts G in the declared order. Returns a named list of integer matrices.
s33_resample_index <- function(cells, B, seed) {
  if (!length(cells) || anyNA(cells) || any(cells < 1L)) stop("Every resampling cell needs at least one cage.", call. = FALSE)
  if (is.null(names(cells)) || anyDuplicated(names(cells))) stop("Resampling cells need unique names.", call. = FALSE)
  old_kind <- RNGkind(); on.exit(do.call(RNGkind, as.list(old_kind)), add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(seed)
  out <- lapply(names(cells), function(k) { G <- as.integer(cells[[k]])
    matrix(sample.int(G, G * B, replace = TRUE), nrow = B, byrow = TRUE) })
  stats::setNames(out, names(cells))
}

#' Multiplicities of the resampled cages: B x G counts (whole cages, a duplicate counts as a separate cluster).
s33_multiplicity <- function(M, G) {
  U <- t(apply(M, 1L, tabulate, nbins = G))
  if (G == 1L) U <- matrix(U, ncol = 1L)
  U
}

#' Resampled means from cage sums S and cage sizes n (both length G): m* = (U %*% S) / (U %*% n).
s33_resampled_means <- function(U, S, n) as.vector((U %*% S) / (U %*% n))

#' Percentile summary of a vector of replicate statistics (type 7, NA-free share recorded).
s33_percentile <- function(v, level = 0.95) {
  ok <- is.finite(v); a <- (1 - level) / 2
  if (!any(ok)) return(list(lo = NA_real_, hi = NA_real_, finite_share = 0))
  q <- stats::quantile(v[ok], c(a, 1 - a), type = 7, names = FALSE)
  list(lo = q[1], hi = q[2], finite_share = mean(ok))
}

# ---------------------------------------------------------------- CR2 (no p-values)
#' CR2 linear contrasts of an lm/lmer fit, clustered by `cluster` (a vector aligned with the fit's data rows).
#' L: numeric matrix (one row per contrast, columns = coefficients) or a named list of coefficient weights.
s33_cr2_contrast <- function(fit, cluster, L, level = 0.95) {
  obj <- if (inherits(fit, "lmerModLmerTest") || inherits(fit, "merMod")) methods::as(fit, "lmerMod") else fit
  b <- if (inherits(obj, "merMod")) lme4::fixef(obj) else stats::coef(obj)
  if (is.list(L)) L <- do.call(rbind, lapply(L, function(w) { v <- stats::setNames(numeric(length(b)), names(b))
    if (!all(names(w) %in% names(b))) stop("Contrast names a coefficient that is not in the model: ",
                                          paste(setdiff(names(w), names(b)), collapse = ", "), call. = FALSE)
    v[names(w)] <- unlist(w); v }))
  if (is.null(dim(L))) L <- matrix(L, nrow = 1L)
  if (ncol(L) != length(b)) stop("Contrast has ", ncol(L), " columns; the model has ", length(b), call. = FALSE)
  V <- clubSandwich::vcovCR(obj, cluster = cluster, type = "CR2")
  lc <- clubSandwich::linear_contrast(obj, vcov = V, contrasts = L, test = "Satterthwaite", level = level, p_values = FALSE)
  data.table::data.table(estimate = lc$Est, se = lc$SE, df = lc$df, ci_low = lc$CI_L, ci_high = lc$CI_U,
                         n_clusters = data.table::uniqueN(cluster))
}

#' CR2 mean of y clustered by `cluster` (intercept-only OLS): estimate, se, Satterthwaite df, 95% interval.
#' Fewer than 3 informative clusters: no interval (plan section 11; same rule used throughout).
s33_cr2_mean <- function(y, cluster, level = 0.95, min_clusters = 3L) {
  ok <- is.finite(y); y <- y[ok]; cluster <- cluster[ok]
  G <- data.table::uniqueN(cluster); m <- if (length(y)) mean(y) else NA_real_
  if (G < min_clusters) return(data.table::data.table(estimate = m, se = NA_real_, df = NA_real_, ci_low = NA_real_,
                                                     ci_high = NA_real_, n_clusters = G, n = length(y)))
  d <- data.frame(y = y, g = cluster)
  out <- s33_cr2_contrast(stats::lm(y ~ 1, data = d), d$g, matrix(1, 1, 1), level = level)
  out[, n := length(y)][]
}

#' Closed-form CR2 variance of a clustered mean (the plan's GD-17 / G17 check):
#' v = sum_g (S_g - n_g m)^2 / (1 - n_g / N) / N^2.
s33_cr2_mean_closed <- function(y, cluster) {
  ok <- is.finite(y); y <- y[ok]; cluster <- cluster[ok]; N <- length(y); m <- mean(y)
  S <- tapply(y, cluster, sum); n <- tapply(y, cluster, length)
  sum((S - n * m)^2 / (1 - n / N)) / N^2
}

#' CR1 variance of a clustered mean with t(G - 1) (plan S2 sensitivities): v1 = G/(G-1) * sum_g (S_g - n_g m)^2 / N^2.
s33_cr1_mean <- function(y, cluster, level = 0.95) {
  ok <- is.finite(y); y <- y[ok]; cluster <- cluster[ok]; N <- length(y); m <- mean(y)
  S <- tapply(y, cluster, sum); n <- tapply(y, cluster, length); G <- length(S)
  if (G < 2L) return(data.table::data.table(estimate = m, se = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_, n_clusters = G))
  se <- sqrt(G / (G - 1) * sum((S - n * m)^2) / N^2); ci <- s33_t_interval(m, se, G - 1, level)
  data.table::data.table(estimate = m, se = se, df = G - 1, ci_low = ci$ci_low, ci_high = ci$ci_high, n_clusters = G)
}

#' Difference of two clustered means (a - b) with CR2 variances per side and a Welch-Satterthwaite df
#' (plan: contrasts of cohort means, 'CR2_Welch_<cluster>').
s33_cr2_welch <- function(ya, ca, yb, cb, level = 0.95, min_clusters = 3L) {
  a <- s33_cr2_mean(ya, ca, level, min_clusters); b <- s33_cr2_mean(yb, cb, level, min_clusters)
  est <- a$estimate - b$estimate
  if (!is.finite(a$se) || !is.finite(b$se)) return(data.table::data.table(estimate = est, se = NA_real_, df = NA_real_,
    ci_low = NA_real_, ci_high = NA_real_, n_clusters_a = a$n_clusters, n_clusters_b = b$n_clusters))
  se <- sqrt(a$se^2 + b$se^2); df <- (a$se^2 + b$se^2)^2 / (a$se^4 / a$df + b$se^4 / b$df)
  ci <- s33_t_interval(est, se, df, level)
  data.table::data.table(estimate = est, se = se, df = df, ci_low = ci$ci_low, ci_high = ci$ci_high,
                         n_clusters_a = a$n_clusters, n_clusters_b = b$n_clusters)
}

# ---------------------------------------------------------------- frozen engine (KR), p and statistic dropped
#' lmer fit through the frozen engine (s32i_fit) with a declared rank; an error, rank stop or non-convergence is a
#' FAILED model (rows kept). The data need AnimalNum and Batch columns (the engine counts them).
s33_kr_fit <- function(formula, data, model_id, expected_rank) {
  s32i_fit(formula, data.table::as.data.table(data), model_id, expected_rank)
}

#' KR contrast (Satterthwaite fallback flagged) of a frozen-engine fit, without p-value or statistic.
s33_kr_contrast <- function(m, weights, estimand) {
  x <- s32i_contrast(m, weights, estimand)
  x[, intersect(c("p_raw", "statistic", "test"), names(x)) := NULL]
  if (!"status" %in% names(x)) x[, status := "OK"]
  x[, interval_method := data.table::fifelse(ddf_fallback %in% TRUE, "Satterthwaite_fallback", "KR")]
  x[, singular := if (is.null(m$info)) NA else m$info$singular[1]][]
}

#' CR2 contrasts of a frozen-engine lmer fit clustered by any vector aligned with m$data (no p-values).
s33_kr_fit_cr2 <- function(m, weights, cluster_col) {
  if (s32i_failed(m)) return(data.table::data.table(estimate = NA_real_, se = NA_real_, df = NA_real_, ci_low = NA_real_,
                                                    ci_high = NA_real_, n_clusters = NA_integer_, status = "FAILED"))
  out <- s33_cr2_contrast(m$fit, m$data[[cluster_col]], list(weights))
  out[, status := "OK"][]
}

# ---------------------------------------------------------------- scales and variance summaries
#' A slope per unit expressed per frozen SD of the predictor.
s33_per_sd <- function(est, sd) est * sd

#' Signed one-way ANOVA moment ICC of y by cluster (unbalanced n0); may be negative.
s33_moment_icc <- function(y, cluster) {
  ok <- is.finite(y); y <- y[ok]; g <- factor(cluster[ok]); k <- nlevels(g); N <- length(y)
  if (k < 2L || N <= k) return(NA_real_)
  n_i <- tabulate(g); m <- mean(y); mi <- tapply(y, g, mean)
  msb <- sum(n_i * (mi - m)^2) / (k - 1); msw <- sum((y - mi[g])^2) / (N - k)
  n0 <- (N - sum(n_i^2) / N) / (k - 1)
  (msb - msw) / (msb + (n0 - 1) * msw)
}

#' The residual SD convention for per-SD units: n - 1 residual SD of x on cohort (x CC) in the given rows.
s33_resid_sd <- function(x, group) {
  ok <- is.finite(x); r <- x[ok] - stats::ave(x[ok], group[ok])
  sqrt(sum(r^2) / (sum(ok) - 1))
}

# ---------------------------------------------------------------- gates
#' Gate rows (plan section 5): non-vacuous by construction, n_expected > 0, n_checked == n_expected and all(ok), NA fails.
s33_gate_rows <- function(gate_id, gate, ok, hard = TRUE, detail = "", n_expected = length(ok), evaluated = TRUE) {
  okv <- as.logical(ok)
  passed <- isTRUE(evaluated) && n_expected > 0L && length(okv) == n_expected && !anyNA(okv) && all(okv)
  data.table::data.table(gate_id = gate_id, gate = gate, passed = if (isTRUE(evaluated)) passed else NA,
    hard = hard, evaluated = isTRUE(evaluated), n_expected = as.integer(n_expected), n_checked = length(okv),
    n_ok = sum(okv %in% TRUE), detail = paste(as.character(detail), collapse = "; "))
}

#' A recorded (never stopping) 'not evaluated' gate row, e.g. a cross-module gate without its partner.
s33_gate_not_evaluated <- function(gate_id, gate, why) {
  data.table::data.table(gate_id = gate_id, gate = gate, passed = NA, hard = FALSE, evaluated = FALSE,
                         n_expected = 0L, n_checked = 0L, n_ok = 0L, detail = paste("not evaluated:", why))
}

#' Stop at the first failed hard gate.
s33_stop_on_gates <- function(g) {
  bad <- g[hard == TRUE & evaluated == TRUE & !(passed %in% TRUE)]
  if (nrow(bad)) stop("Stage 33 gate failed: ", bad$gate_id[1], " ", bad$gate[1], " (", bad$detail[1], ")", call. = FALSE)
  invisible(TRUE)
}

# ---------------------------------------------------------------- identity (plan section 3, GC-1)
#' canonical_animal_id() taken from Functions/behavioral_dynamics_helpers.R by parse() (that file is not sourced: it
#' loads the tidyverse and the palette). Returns the function, the sha256 of its source lines joined by LF (pinned in
#' S33_CODE_PINS) and whether the probe gives the pinned answer.
s33_load_canonical_id <- function(helper_path) {
  ex <- parse(helper_path, keep.source = TRUE)
  k <- which(vapply(ex, function(e) is.call(e) && identical(e[[1]], as.name("<-")) &&
                      identical(e[[2]], as.name("canonical_animal_id")), TRUE))
  if (length(k) != 1L) stop("canonical_animal_id must be assigned exactly once in ", helper_path, call. = FALSE)
  txt <- paste(as.character(attr(ex, "srcref")[[k]]), collapse = "\n")
  fun <- eval(ex[[k]][[3]], baseenv())
  probe <- fun(c("OR004", " or123 ", "00655", "0001", ""))
  list(fun = fun, srcref_sha256 = digest::digest(txt, algo = "sha256", serialize = FALSE),
       probe_ok = identical(probe, c("OR004", "OR123", "655", "1", NA_character_)))
}

#' Planning IDs: strip one 3-letter-plus-digit prefix (HKH0-, HKH1-, MCM0-, LUM0-) before canonical_animal_id().
s33_planning_id <- function(x, canon) canon(sub("^[A-Z]{3}[0-9]-", "", trimws(as.character(x))))

#' Headerless ID list (sus, con, excluded; CRLF): readLines, trimws, drop blanks, strip prefix, canonical. Never fread.
s33_read_id_list <- function(path, canon) {
  x <- trimws(readLines(path, warn = FALSE)); x <- x[nzchar(x)]
  unique(s33_planning_id(x, canon))
}

# ---------------------------------------------------------------- Excel parsers (pure) and readers (thin)
#' Read a sheet as a character matrix whose row and column numbers are Excel's (range from A1).
s33_read_sheet_matrix <- function(path, sheet, range) {
  as.matrix(suppressMessages(readxl::read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text",
                                                range = range, .name_repair = "minimal")))
}

#' Social Golfer plan of B1 (Timeline sheet Group_Composition): rounds at Excel rows 13-16, 19-22, 25-28, 31-34; SIS
#' group columns 2, 11, 20, 29 with the source cage 4 columns to the right; CON in column 40 (rows 13-16).
s33_parse_golfer_plan <- function(m, canon) {
  rounds <- list(CC1 = 13:16, CC2 = 19:22, CC3 = 25:28, CC4 = 31:34); gcols <- c(2L, 11L, 20L, 29L)
  sis <- data.table::rbindlist(lapply(names(rounds), function(cc) data.table::rbindlist(lapply(seq_along(gcols), function(gi)
    data.table::data.table(CC = cc, plan_group = gi, ID = m[rounds[[cc]], gcols[gi]], src = m[rounds[[cc]], gcols[gi] + 4L])))))
  sis[, `:=`(AnimalNum = s33_planning_id(ID, canon), condition = "SIS")]
  con <- data.table::data.table(CC = "CC1", plan_group = 0L, ID = m[13:16, 40L], src = NA_character_)
  con[, `:=`(AnimalNum = s33_planning_id(ID, canon), condition = "CON")]
  rbind(sis, con)[!is.na(ID)]
}

#' GroupCompBatch2 (B2 source cages; a planning record): header row 10, columns 13-16 = ID, Cage, Cage#, Stress.
s33_parse_groupcomp_b2 <- function(m, canon) {
  if (!identical(unname(m[10, 13:16]), c("ID", "Cage", "Cage#", "Stress")))
    stop("GroupCompBatch2: header row 10, columns 13-16 is not ID/Cage/Cage#/Stress", call. = FALSE)
  x <- data.table::data.table(ID = m[11:nrow(m), 13], src = m[11:nrow(m), 14], cage_no = m[11:nrow(m), 15], stress = m[11:nrow(m), 16])
  x <- x[!is.na(ID) & grepl("^[A-Z]{3}[0-9]-", ID)]
  x[, AnimalNum := s33_planning_id(ID, canon)][]
}

#' GroupCompBatch3 / GroupComposition sheets: ID column 7, cage column 8 (^[A-Z]{4,5}[0-9]-[0-9]+$); prefixes stripped,
#' bare 3-digit IDs become 'OR' + ID, other bare IDs kept; duplicates collapsed.
s33_parse_groupcomp_cols78 <- function(m, canon) {
  x <- data.table::data.table(ID = trimws(m[, 7]), src = trimws(m[, 8]))
  x <- x[!is.na(ID) & !is.na(src) & grepl("^[A-Z]{4,5}[0-9]-[0-9]+$", src)]
  x[, raw := sub("^[A-Z]{3}[0-9]-", "", ID)]
  x[, raw := data.table::fifelse(grepl("^[0-9]{3}$", raw), paste0("OR", raw), raw)]
  unique(x[, .(AnimalNum = canon(raw), src)])
}

#' One sheet read with only the declared columns (all others skipped at read; plan section 4, Phase 2 restriction).
s33_read_sheet_cols <- function(path, sheet, cols) {
  hdr <- names(suppressMessages(readxl::read_excel(path, sheet = sheet, n_max = 0)))
  miss <- setdiff(cols, hdr); if (length(miss)) stop("Sheet ", sheet, " lacks column(s) ", paste(miss, collapse = ", "), call. = FALSE)
  ct <- ifelse(hdr %in% cols, "guess", "skip")
  data.table::as.data.table(suppressMessages(readxl::read_excel(path, sheet = sheet, col_types = ct)))
}

#' Recording start of one raw AnimalPos file: the first parsable record (DateTime '%d.%m.%Y %H:%M:%OS', UTC), the lag of
#' the 18:30 anchor on that day after it, and the first record of every board (the board is the 'sys.N' part of the
#' Animal label, as the frozen seed parser reads it). board_first_delay_h = the latest board start after the file start.
#' Board first records leave out the records of `exclude_ids` (animals whose raw cage label data version v2 corrected),
#' so they match Stage 32's board_first; the recording start always uses every record.
s33_recording_start <- function(raw_file, exclude_ids = character(), canon = identity) {
  d <- data.table::fread(raw_file, sep = ";", select = c("DateTime", "Animal"), colClasses = "character", showProgress = FALSE)
  d[, t := as.POSIXct(DateTime, format = "%d.%m.%Y %H:%M:%OS", tz = "UTC")]
  d <- d[!is.na(t)]
  if (!nrow(d)) stop("No parsable record in ", raw_file, call. = FALSE)
  d[, board := regmatches(Animal, regexpr("sys[.][0-9]+", Animal))[seq_len(.N)]]
  d[!grepl("sys[.][0-9]+", Animal), board := NA_character_]
  start <- d$t[1]
  anchor <- as.POSIXct(paste(format(start, "%Y-%m-%d"), "18:30:00"), tz = "UTC")
  d[, id := canon(sub("[-_]sys[.].*$", "", Animal))]
  boards <- d[!is.na(board) & !id %in% exclude_ids, .(first = min(t)), by = board][order(board)]
  list(rec_start = start, anchor_1830 = anchor,
       lag_h = as.numeric(difftime(anchor, start, units = "hours")),
       board_first_delay_h = as.numeric(difftime(max(boards$first), start, units = "hours")),
       boards = boards)
}

# ---------------------------------------------------------------- the shared design (plan section 3; Phase 2)
S33_CC1_DATES <- c(B1 = "2022-10-28", B2 = "2023-01-29", B3 = "2023-04-24", B4 = "2023-07-25", B5 = "2023-11-20", B6 = "2024-04-07")
S33_TP1_DATES <- c(B1 = "2022-10-24", B2 = "2023-01-27", B3 = "2023-04-19", B4 = "2023-07-21", B5 = "2023-11-17", B6 = "2024-04-02")
S33_PLANNED_LAGS <- data.table::data.table(
  Batch = rep(S33_COHORTS, each = 4L), CC = rep(S33_CC, times = 6L),
  lag_h = c(2.754, 3.327, 4.696, 5.621, 3.075, 5.709, 3.352, 2.302, 6.086, 6.199, 6.202, 3.278,
            5.207, 2.857, 5.873, 3.347, 4.719, 5.325, 7.849, 5.349, 4.219, 3.555, 7.687, 6.299))
S33_HW_A1 <- c("692|CC1", "OR112|CC3", "OR141|CC3", "314|CC4", "318|CC4", "OR620|CC4", "OR630|CC4")
S33_XMATE <- c("OR550", "OR553", "OR559", "662", "692", "695")
S33_UNTRACKED_B1 <- c("1", "2", "OQ750", "OQ751", "OQ752", "OQ753")
S33_SINGLE_CC1 <- c("OQ770", "OQ771")
S33_J7 <- c("3", "4", "1545", "1546", "1547", "13856", "13857")
S33_J9 <- c(S33_J7, "1", "2")
S33_SOURCE_ALIAS <- c("BHKH0-00290" = "SHKH0-01641")   # B5: one source cage under two names (plan section 3)
# Animals whose raw cage label data version v2 corrected (board right, label wrong), by Batch|CC.
S33_V2_LABEL_CORRECTED <- list("B1|CC2" = c("OQ764", "OQ770", "OQ772"), "B6|CC4" = "OR646")
S33_BUNDLE_COLS <- c("AnimalNum", "Sex", "Batch", "CC", "CageEpisodeID", "n_in_cage", "n_tracked_mates", "hardware_flag",
                     "obs_s", "n_events", "crossing_rate", "shared_zone_use")

#' The outcome-free design (Phase 2). `paths`: named list with bundle_dir, combz_table, sus_list, con_list,
#' excluded_list, workbook, timeline, groupcomp_b2, groupcomp_b3, groupcomp_b456, raw_dir, s32_window_metrics.
#' `canon` = canonical_animal_id. Returns the design list (module contract) with $gates (GC-6 rows, not yet stopped).
s33_design <- function(paths, canon) {
  G <- list(); gate <- function(id, name, ok, detail = "") G[[length(G) + 1L]] <<- s33_gate_rows(id, name, ok, detail = detail)
  bt <- function(x) paste0("B", x)
  # canonical table: four outcome-free columns only
  cz <- data.table::fread(paths$combz_table, select = c("AnimalNum", "Batch", "Sex", "experimental_condition"),
                          colClasses = list(character = "AnimalNum"), showProgress = FALSE)
  cz[, Batch := bt(Batch)]
  con <- s33_read_id_list(paths$con_list, canon); sus <- s33_read_id_list(paths$sus_list, canon)
  excl <- s33_read_id_list(paths$excluded_list, canon)
  gate("GC-6a", "canonical 117 animals, IDs canonical, lists 35 / 24 / 8",
       c(nrow(cz) == 117L, all(cz$AnimalNum == canon(cz$AnimalNum)), length(sus) == 35L, length(con) == 24L, length(excl) == 8L),
       sprintf("n %d sus %d con %d excluded %d", nrow(cz), length(sus), length(con), length(excl)))
  cz[, condition := data.table::fifelse(AnimalNum %in% con, "CON", "SIS")]
  gate("GC-6b", "condition = con list = canonical experimental_condition", cz$condition == cz$experimental_condition)
  a1 <- data.table::fread(file.path(paths$bundle_dir, "A1_animal_cc1.csv"), select = S33_BUNDLE_COLS,
                          colClasses = list(character = "AnimalNum"), showProgress = FALSE)
  b1 <- data.table::fread(file.path(paths$bundle_dir, "B1_animal_longitudinal.csv"), select = S33_BUNDLE_COLS,
                          colClasses = list(character = "AnimalNum"), showProgress = FALSE)
  s33_assert_outcome_free(a1, "bundle A1 read"); s33_assert_outcome_free(b1, "bundle B1 read")
  a1[, Batch := as.character(Batch)]; b1[, Batch := as.character(Batch)]
  if (!all(grepl("^B[1-6]$", a1$Batch))) { a1[, Batch := bt(Batch)]; b1[, Batch := bt(Batch)] }
  bcc1 <- b1[CC == "CC1"]; data.table::setkey(bcc1, AnimalNum); data.table::setkey(a1, AnimalNum)
  gate("GC-6c", "A1 = B1 CC1 rows (111) and B1 = 444 rows",
       c(nrow(a1) == 111L, nrow(b1) == 444L, identical(a1$AnimalNum, bcc1$AnimalNum),
         isTRUE(all.equal(a1$crossing_rate, bcc1$crossing_rate, tolerance = 0)), identical(a1$CageEpisodeID, bcc1$CageEpisodeID)))
  j <- merge(a1[, .(AnimalNum, Batch, Sex)], cz[, .(AnimalNum, Batch_c = Batch, Sex_c = Sex)], by = "AnimalNum")
  gate("GC-6d", "bundle Batch and Sex = canonical (111/111)", c(nrow(j) == 111L, j$Batch == j$Batch_c, j$Sex == j$Sex_c))
  canon_only <- setdiff(cz$AnimalNum, a1$AnimalNum)
  gate("GC-6e", "canonical-only 6 = UNTRACKED_B1 = the B1 entries of the excluded list",
       c(setequal(canon_only, S33_UNTRACKED_B1), setequal(canon_only, intersect(excl, cz$AnimalNum)),
         all(cz[AnimalNum %in% canon_only, Batch] == "B1")), paste(sort(canon_only), collapse = ","))
  # A1 rates of CC1-CC4 = Stage 32 window metrics (1e-9, 444)
  wm <- data.table::fread(paths$s32_window_metrics, select = c("AnimalNum", "CC", "phase", "crossing_rate", "Batch", "System", "board_first"),
                          colClasses = list(character = c("AnimalNum", "board_first")), showProgress = FALSE)[phase == "A1"]
  wb1 <- merge(b1[, .(AnimalNum, CC, r = crossing_rate)], wm[, .(AnimalNum, CC, v = crossing_rate)], by = c("AnimalNum", "CC"))
  gate("GC-6f", "A1 rates CC1-CC4 = Stage 32 (1e-9, 444)", c(nrow(wb1) == 444L, abs(wb1$r - wb1$v) < 1e-9))
  # Timeline Animals and the source cages
  tl <- data.table::as.data.table(suppressMessages(readxl::read_excel(paths$timeline, sheet = "Animals", col_types = "text")))
  tl <- tl[!is.na(ID)]
  tl[, AnimalNum := s33_planning_id(ID, canon)]
  tl <- tl[AnimalNum %in% cz$AnimalNum]
  gate("GC-6g", "Timeline Animals matches 117/117 once each", c(data.table::uniqueN(tl$AnimalNum) == 117L, !anyDuplicated(tl$AnimalNum)))
  xl_date <- function(x) as.Date(suppressWarnings(as.numeric(x)), origin = "1899-12-30")
  tl <- tl[, .(AnimalNum, src_tl = Cage, line = Line, dob = xl_date(DoB), mark_date = xl_date(`Mark date`), rack = Rack, method = Method)]
  b2 <- s33_parse_groupcomp_b2(s33_read_sheet_matrix(paths$groupcomp_b2, "Sheet1", "A1:Z80"), canon)
  gate("GC-6h", "GroupCompBatch2: 20/20 B2 animals, Stress = condition",
       c(nrow(b2) == 20L, setequal(b2$AnimalNum, cz[Batch == "B2", AnimalNum]),
         b2$stress == cz$condition[match(b2$AnimalNum, cz$AnimalNum)]))
  gc3 <- s33_parse_groupcomp_cols78(s33_read_sheet_matrix(paths$groupcomp_b3, "Sheet1", "A1:L60"), canon)
  gc456 <- data.table::rbindlist(lapply(c("Batch 4", "Batch5", "Batch 6"), function(sh)
    s33_parse_groupcomp_cols78(s33_read_sheet_matrix(paths$groupcomp_b456, sh, "A1:L60"), canon)))
  gcp <- rbind(gc3, gc456)
  gcp[, Batch := cz$Batch[match(AnimalNum, cz$AnimalNum)]]
  per_b <- gcp[!is.na(Batch), .N, by = Batch]
  gate("GC-6i", "GroupComposition matches B3 20/20, B4 19/19, B5 19/19, B6 19/19; unmatched exactly OR567, 655",
       c(identical(per_b[order(Batch), N], c(20L, 19L, 19L, 19L)), setequal(gcp[is.na(Batch), AnimalNum], c("OR567", "655"))),
       paste(per_b[order(Batch), paste(Batch, N)], collapse = "; "))
  d <- merge(cz[, .(AnimalNum, Batch, Sex, condition)], tl, by = "AnimalNum")
  d[, src_cage := src_tl][Batch == "B2", src_cage := b2$src[match(AnimalNum, b2$AnimalNum)]]
  d[src_cage %in% names(S33_SOURCE_ALIAS), src_cage := unname(S33_SOURCE_ALIAS[src_cage])]
  gpl <- gcp[!is.na(Batch)][, src2 := data.table::fifelse(src %in% names(S33_SOURCE_ALIAS), unname(S33_SOURCE_ALIAS[src]), src)]
  chk <- merge(d[, .(AnimalNum, src_cage)], gpl[, .(AnimalNum, src2)], by = "AnimalNum")
  gate("GC-6j", "GroupComposition source cages = Timeline source cages (B3-B6)", c(nrow(chk) == 77L, chk$src_cage == chk$src2))
  # body weights tp1 and tp2 only (Phase 2)
  bw <- s33_read_sheet_cols(paths$workbook, "bodyWeight_long", c("animal_id", "timepoint", "weigh_date", "body_weight_g"))
  bw <- bw[timepoint %in% c(1, 2)][, AnimalNum := canon(animal_id)][AnimalNum %in% cz$AnimalNum]
  gate("GC-6k", "tp1 and tp2 complete and unique for 117", c(nrow(bw) == 234L, !anyDuplicated(bw[, .(AnimalNum, timepoint)]), !anyNA(bw$body_weight_g)))
  w <- data.table::dcast(bw, AnimalNum ~ timepoint, value.var = c("body_weight_g", "weigh_date"))
  data.table::setnames(w, c("body_weight_g_1", "body_weight_g_2", "weigh_date_1", "weigh_date_2"), c("tp1", "tp2", "tp1_date", "tp2_date"))
  d <- merge(d, w[, .(AnimalNum, tp1, tp2, tp1_date = as.Date(tp1_date), tp2_date = as.Date(tp2_date))], by = "AnimalNum")
  d[, cc1_date := as.Date(S33_CC1_DATES[Batch])][, age_cc1 := as.numeric(cc1_date - dob)]
  gate("GC-6l", "dates: tp2 = CC1 date; tp1 = the declared date per cohort; ages 22-29 d",
       c(d$tp2_date == d$cc1_date, d$tp1_date == as.Date(S33_TP1_DATES[d$Batch]), d$age_cc1 >= 22 & d$age_cc1 <= 29))
  # master_wide: CC4 board for all 117
  mw <- s33_read_sheet_cols(paths$workbook, "master_wide", c("animal_id", "cage"))[, AnimalNum := canon(animal_id)][AnimalNum %in% cz$AnimalNum]
  gate("GC-6m", "master_wide cage present once for 117", c(nrow(mw) == 117L, !anyDuplicated(mw$AnimalNum), !anyNA(mw$cage)))
  d[, cc4_board_mw := paste0("sys.", as.integer(mw$cage[match(AnimalNum, mw$AnimalNum)]))]
  # bundle columns and cage episodes
  d <- merge(d, a1[, .(AnimalNum, crossing_rate, shared_zone_use, n_in_cage, n_tracked_mates, cc1_cage = CageEpisodeID)],
             by = "AnimalNum", all.x = TRUE)
  for (k in 2:4) { x <- b1[CC == paste0("CC", k), .(AnimalNum, v = CageEpisodeID)]; data.table::setnames(x, "v", paste0("cc", k, "_cage"))
    d <- merge(d, x, by = "AnimalNum", all.x = TRUE) }
  d[, tracked := !is.na(crossing_rate) | AnimalNum %in% a1$AnimalNum]
  # Social Golfer plan (B1): planned CC1 and CC4 cages of the six untracked animals
  gp <- s33_parse_golfer_plan(s33_read_sheet_matrix(paths$timeline, "Group_Composition", "A1:AZ60"), canon)
  gps <- gp[condition == "SIS"]
  obs <- b1[Batch == "B1", .(AnimalNum, CC, CageEpisodeID)]
  m <- merge(gps, obs, by = c("AnimalNum", "CC"), all.x = TRUE)
  map <- m[!is.na(CageEpisodeID), .(n_cages = data.table::uniqueN(CageEpisodeID), cage = CageEpisodeID[1]), by = .(CC, plan_group)]
  rev_map <- m[!is.na(CageEpisodeID), .(n_groups = data.table::uniqueN(plan_group)), by = .(CC, CageEpisodeID)]
  pp <- gps[, { a <- sort(AnimalNum); if (length(a) > 1L) data.table::as.data.table(t(utils::combn(a, 2L))) }, by = .(CC, plan_group)]
  src_of <- function(id) d$src_cage[match(id, d$AnimalNum)]
  gate("GC-6n", "Social Golfer plan: 16 distinct B1 SIS per round; groups one-to-one with observed cages; no pair meets twice or shares a source cage",
       c(gps[, data.table::uniqueN(AnimalNum), by = CC]$V1 == 16L, all(gps$AnimalNum %in% d[Batch == "B1" & condition == "SIS", AnimalNum]),
         map$n_cages == 1L, rev_map$n_groups == 1L, pp[, .N, by = .(V1, V2)]$N == 1L,
         pp[CC == "CC1", src_of(V1) != src_of(V2)]))
  planned <- merge(gps[AnimalNum %in% S33_UNTRACKED_B1 & CC %in% c("CC1", "CC4")], map[, .(CC, plan_group, cage)], by = c("CC", "plan_group"))
  for (k in c("CC1", "CC4")) { col <- paste0(tolower(k), "_cage"); p <- planned[CC == k]
    d[match(p$AnimalNum, AnimalNum), (col) := p$cage] }
  d[, cc4_cage := paste0(Batch, "|", cc4_board_mw, "|CC4")]
  chk4 <- d[tracked == TRUE, cc4_cage == b1[CC == "CC4"]$CageEpisodeID[match(AnimalNum, b1[CC == "CC4"]$AnimalNum)]]
  pl4 <- d[AnimalNum %in% S33_UNTRACKED_B1, cc4_cage == planned[CC == "CC4"]$cage[match(AnimalNum, planned[CC == "CC4"]$AnimalNum)]]
  gate("GC-6o", "cc4_cage: master_wide = CC4 board (111) and = the planned CC4 cage (6)", c(chk4, pl4, length(chk4) == 111L, length(pl4) == 6L))
  d[, cc1_board := sub("^B[0-9]+[|](sys[.][0-9]+)[|]CC1$", "\\1", cc1_cage)]
  # source-cage facts
  sis_t <- d[condition == "SIS" & tracked == TRUE]
  cc1_pairs <- sis_t[, { a <- sort(AnimalNum); if (length(a) > 1L) data.table::as.data.table(t(utils::combn(a, 2L))) }, by = cc1_cage]
  con_src <- d[condition == "CON", .(n_src = data.table::uniqueN(src_cage)), by = Batch]
  sis_src <- d[condition == "SIS", .(n_line = data.table::uniqueN(line), n_dob = data.table::uniqueN(dob)), by = .(Batch, src_cage)]
  gate("GC-6p", "0 of 123 CC1 SIS pairs share a source cage; each CON cage is one source cage; line and DoB constant within SIS source cages",
       c(nrow(cc1_pairs) == 123L, src_of(cc1_pairs$V1) != src_of(cc1_pairs$V2), con_src$n_src == 1L, sis_src$n_line == 1L, sis_src$n_dob == 1L))
  # lags of the 24 raw files
  boards <- list()
  lags <- data.table::rbindlist(lapply(S33_COHORTS, function(b) data.table::rbindlist(lapply(S33_CC, function(cc) {
    r <- s33_recording_start(file.path(paths$raw_dir, b, paste0("E9_SIS_", b, "_", cc, "_AnimalPos.csv")),
                             exclude_ids = S33_V2_LABEL_CORRECTED[[paste(b, cc, sep = "|")]] %s33or% character(), canon = canon)
    boards[[length(boards) + 1L]] <<- r$boards[, `:=`(Batch = b, CC = cc)]
    data.table::data.table(Batch = b, CC = cc, rec_start = r$rec_start, anchor_1830 = r$anchor_1830, lag_h = r$lag_h,
                           board_first_delay_h = r$board_first_delay_h) }))))
  lc <- merge(lags, S33_PLANNED_LAGS, by = c("Batch", "CC"), suffixes = c("", "_plan"))
  bf <- unique(wm[, .(Batch, CC, board = System, s32_first = as.POSIXct(board_first, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"))])
  bfc <- merge(data.table::rbindlist(boards), bf, by = c("Batch", "CC", "board"))
  gate("GC-6q", "lags = the plan (0.001 h); board first records (v2-corrected labels left out) = Stage 32 board_first (0.02 h)",
       c(nrow(lc) == 24L, abs(lc$lag_h - lc$lag_h_plan) <= 0.001, nrow(bfc) == nrow(bf), nrow(bf) >= 24L,
         abs(as.numeric(difftime(bfc$first, bfc$s32_first, units = "hours"))) <= 0.02))
  # sets, populations and remaining columns
  hw <- b1[hardware_flag %in% TRUE, paste(AnimalNum, CC, sep = "|")]
  gate("GC-6r", "HW_A1 = the bundle hardware flags", setequal(hw, S33_HW_A1), paste(sort(hw), collapse = ","))
  d[, `:=`(untracked_b1 = AnimalNum %in% S33_UNTRACKED_B1, xmate_excluded = AnimalNum %in% S33_XMATE,
           pop_tracked_111 = tracked, pop_sis_87 = tracked & condition == "SIS",
           pop_focal_85 = tracked & condition == "SIS" & !AnimalNum %in% S33_SINGLE_CC1,
           pop_sis_93 = condition == "SIS", pop_canonical_117 = TRUE, pop_con_24 = condition == "CON",
           line_J = as.integer(line == "C57BL/6J"), J7 = AnimalNum %in% S33_J7, J9 = AnimalNum %in% S33_J9,
           x_RU = crossing_rate / S33_RU, hw_a1 = paste(AnimalNum, "CC1", sep = "|") %in% S33_HW_A1)]
  gate("GC-6s", "populations 111 / 87 / 85 / 93 / 117 / 24; sis_87 per cohort 10/16/16/15/15/15",
       c(sum(d$pop_tracked_111) == 111L, sum(d$pop_sis_87) == 87L, sum(d$pop_focal_85) == 85L, sum(d$pop_sis_93) == 93L,
         sum(d$pop_con_24) == 24L, identical(d[pop_sis_87 == TRUE, .N, keyby = Batch]$N, c(10L, 16L, 16L, 15L, 15L, 15L))))
  gate("GC-6t", "J7 = the tracked C57BL/6J SIS animals", setequal(d[line_J == 1L & pop_sis_87 == TRUE, AnimalNum], S33_J7))
  rs <- function(v) { s <- d[pop_sis_87 == TRUE & is.finite(get(v))]; sd(stats::resid(stats::lm(s[[v]] ~ factor(s$Batch)))) }
  gate("GC-6u", "frozen SDs reproduced: rate 5.1698, occupancy 0.0607 (1e-4)",
       c(abs(rs("crossing_rate") - S33_SD_RATE) < 1e-4, abs(rs("shared_zone_use") - S33_SD_OCC) < 1e-4))
  # cage structure
  ep <- merge(b1, d[, .(AnimalNum, condition)], by = "AnimalNum")
  ep[, `:=`(board = sub("^B[0-9]+[|](sys[.][0-9]+)[|].*$", "\\1", CageEpisodeID), in_S13 = paste(AnimalNum, CC, sep = "|") %in% S33_HW_A1 & CC %in% c("CC3", "CC4"))]
  cages <- ep[, .(Sex = Sex[1], board = board[1], condition = paste(sort(unique(condition)), collapse = "/"), n_tracked = .N,
                  members = paste(sort(AnimalNum), collapse = ";")), by = .(CageEpisodeID, Batch, CC)]
  c1 <- cages[CC == "CC1"]
  sizes <- c1[condition == "SIS", .(s = paste(sort(n_tracked), collapse = ",")), keyby = Batch]$s
  con_boards <- c1[condition == "CON", board[order(Batch)]]
  con_const <- ep[condition == "CON", .(m = paste(sort(AnimalNum), collapse = ";")), by = .(Batch, CC)][, data.table::uniqueN(m), by = Batch]$V1
  gate("GC-6v", "CC1: 30 episodes (24 SIS, 6 CON); SIS sizes as declared; CON boards sys.3 except sys.4 (B2), sys.2 (B6); CON constant",
       c(nrow(c1) == 30L, sum(c1$condition == "SIS") == 24L, sum(c1$condition == "CON") == 6L,
         identical(sizes, c("1,1,4,4", "4,4,4,4", "4,4,4,4", "3,4,4,4", "3,4,4,4", "3,4,4,4")),
         identical(con_boards, c("sys.3", "sys.4", "sys.3", "sys.3", "sys.3", "sys.2")), con_const == 1L, !any(grepl("/", cages$condition))))
  later <- cages[CC != "CC1" & condition == "SIS", .N, by = .(Batch, CC)]
  pairs_all <- ep[condition == "SIS", { a <- sort(AnimalNum); if (length(a) > 1L) data.table::as.data.table(t(utils::combn(a, 2L))) }, by = .(CageEpisodeID, CC)]
  meet <- pairs_all[, .(n = data.table::uniqueN(CC), cc1 = any(CC == "CC1")), by = .(V1, V2)][n > 1L & cc1 == TRUE]
  gate("GC-6w", "CC2-CC4: 4 SIS cages per cohort; of the 123 CC1 pairs only 690-OR646 meet again",
       c(later$N == 4L, nrow(later) == 18L, nrow(meet) == 1L, identical(sort(c(meet$V1, meet$V2)), sort(c("690", "OR646")))))
  srcs <- d[, .(n_sis = sum(condition == "SIS"), n_con = sum(condition == "CON"), line = line[1], dob = dob[1]), by = .(Batch, src_cage)]
  data.table::setkey(d, AnimalNum)
  out <- list(animals = d[, c(S33_DESIGN_ANIMAL_COLUMNS, "mark_date", "rack", "method"), with = FALSE],
              episodes = ep[, .(AnimalNum, Batch, Sex, condition, CC, CageEpisodeID, board, crossing_rate, shared_zone_use,
                                n_in_cage, n_tracked_mates, hardware_flag, in_S13, obs_s, n_events)],
              cages = cages, lags = lags, source_cages = srcs, planning = gp,
              sets = list(HW_A1 = S33_HW_A1, S13 = S33_HW_A1[grepl("CC3|CC4", S33_HW_A1)], XMATE = S33_XMATE, J7 = S33_J7,
                          J9 = S33_J9, UNTRACKED_B1 = S33_UNTRACKED_B1, SINGLE_CC1 = S33_SINGLE_CC1, EXCLUDED = excl),
              gates = data.table::rbindlist(G))
  s33_assert_outcome_free(out$animals, "design"); s33_assert_outcome_free(out$episodes, "design episodes")
  out
}

# ---------------------------------------------------------------- outcomes (plan section 3, Phase 3, GC-7)
S33_COMPONENTS <- c("NOR", "sucrose_pref", "weight_dev", "delta_cort", "adrenal_weight", "spleen_weight")

#' Read the outcomes once (Phase 3) and add them to design$animals; returns list(design_out, gates). The bundle's CombZ
#' and Group are read here (and only here) for the GC-7 identity checks.
s33_attach_outcomes <- function(design, paths, canon) {
  G <- list(); gate <- function(id, name, ok, detail = "") G[[length(G) + 1L]] <<- s33_gate_rows(id, name, ok, detail = detail)
  cz <- data.table::fread(paths$combz_table, colClasses = list(character = "AnimalNum"), showProgress = FALSE)
  keep <- c("AnimalNum", "CombZ", "outcome_group", S33_COMPONENTS, "n_components_present")
  cz <- cz[, keep, with = FALSE]
  sus <- s33_read_id_list(paths$sus_list, canon); con <- s33_read_id_list(paths$con_list, canon)
  lst <- data.table::fifelse(cz$AnimalNum %in% con, "CON", data.table::fifelse(cz$AnimalNum %in% sus, "SUS", "RES"))
  a1 <- data.table::fread(file.path(paths$bundle_dir, "A1_animal_cc1.csv"), select = c("AnimalNum", "Group", "CombZ"),
                          colClasses = list(character = "AnimalNum"), showProgress = FALSE)
  j <- merge(a1, cz[, .(AnimalNum, cz = CombZ, og = outcome_group)], by = "AnimalNum")
  comp_mean <- rowMeans(as.matrix(cz[, S33_COMPONENTS, with = FALSE]), na.rm = TRUE)
  thr <- data.table::fread(paths$combz_thresholds, showProgress = FALSE)
  gate("GC-7a", "outcome groups 24/58/35 and list Group = outcome_group = bundle Group",
       c(identical(as.integer(table(factor(cz$outcome_group, c("CON", "RES", "SUS")))), c(24L, 58L, 35L)), lst == cz$outcome_group,
         j$Group == j$og))
  gate("GC-7b", "CombZ(A1) = canonical (1e-12); CombZ = mean of the present components (1e-12)",
       c(nrow(j) == 111L, abs(j$CombZ - j$cz) < 1e-12, abs(comp_mean - cz$CombZ) < 1e-12))
  gate("GC-7c", "missingness: delta_cort only OQ762; adrenal_weight only OR126; others complete",
       c(identical(cz[is.na(delta_cort), AnimalNum], "OQ762"), identical(cz[is.na(adrenal_weight), AnimalNum], "OR126"),
         !anyNA(cz[, c("NOR", "sucrose_pref", "weight_dev", "spleen_weight"), with = FALSE])))
  bw <- s33_read_sheet_cols(paths$workbook, "bodyWeight_long", c("animal_id", "timepoint", "weigh_date", "body_weight_g"))
  bw <- bw[timepoint %in% 3:6][, AnimalNum := canon(animal_id)][AnimalNum %in% design$animals$AnimalNum]
  w <- data.table::dcast(bw, AnimalNum ~ timepoint, value.var = c("body_weight_g", "weigh_date"))
  data.table::setnames(w, paste0("body_weight_g_", 3:6), paste0("tp", 3:6))
  for (k in 3:6) w[, (paste0("tp", k, "_date")) := as.Date(get(paste0("weigh_date_", k)))]
  mw <- s33_read_sheet_cols(paths$workbook, "master_wide", c("animal_id", "cort_baseline_ng_ml", "cort_response_ng_ml"))
  mw[, AnimalNum := canon(animal_id)]
  d <- merge(design$animals, cz, by = "AnimalNum")
  d <- merge(d, w[, c("AnimalNum", paste0("tp", 3:6), paste0("tp", 3:6, "_date")), with = FALSE], by = "AnimalNum", all.x = TRUE)
  d <- merge(d, mw[, .(AnimalNum, cort_baseline = cort_baseline_ng_ml, cort_response = cort_response_ng_ml)], by = "AnimalNum", all.x = TRUE)
  gate("GC-7d", "tp3-tp6 complete for 117; corticosterone baseline 111 tracked, response missing only for OQ762 among the tracked",
       c(nrow(d) == 117L, !anyNA(d[, paste0("tp", 3:6), with = FALSE]), d[tracked == TRUE, sum(is.finite(cort_baseline))] == 111L,
         identical(d[tracked == TRUE & !is.finite(cort_response), AnimalNum], "OQ762")))
  data.table::setkey(d, AnimalNum)
  out <- design; out$animals <- d; out$thresholds <- thr
  list(design_out = out, gates = data.table::rbindlist(G))
}
