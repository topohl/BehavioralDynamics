# ================================================================
# Audit: identifiability of group and antenna-board effects in the Exp9 RFID cage-episode design
# MMMSociability -- Testing/audits/audit_board_group_identifiability.R
# ================================================================
# UNREGISTERED design audit (2026-10-14). DESIGN ONLY: no behavioural metric, CombZ or other outcome is read and no
# group effect is estimated. Every number is a function of the allocation (which cage sat on which antenna board in
# which recording), of the group labels and of group-blind acquisition covariates. alias() and car::vif() need an lm()
# fit; they get a placeholder response, and what they return depends on the design matrix only.
#
# Unit: the cage-episode = Batch x CC x antenna board (120 = 24 recordings x 5 boards). Every recording holds one CON
# cage (the same four animals over the four CCs of a batch) and four SIS cages (regrouped at every CC). Questions: are
# CON vs SIS (and RES vs SUS) contrasts and board effects identifiable from the actual allocation; what does adjusting
# for board, board x CC or a hardware covariate cost; how many CON episodes are effectively independent.
#
# Inputs (read-only):
#   <Behavior>/RANM/raw_acquisition_qc/runs/raqc_20261013_b1783bf/tables/   (Stage 00b QC run)
#     animal_qc.csv      animal x recording -> board (the allocation)
#     reader_qc.csv      per reader: incoming-crosstalk ratio, reads relative to the homologous readers, best session
#                        ratio, zero-read sessions (the three dead channels)
#     board_qc.csv       tracked tags present, silent share
#     board_history.csv  read-gap and lattice signature per board-recording
#   <project root>/analysis_ready/canonical/later_outcome_combz/tables/later_outcome_combz_animal_level.csv
#     only AnimalNum, outcome_group and Sex are read (never CombZ or a component)
# Outputs: <OUT>/runs/<run_id>/, built in runs/.tmp_<run_id>/ and renamed when complete (an existing folder is refused):
#   tables/*.csv, manifest/run.csv (run id, commit, inputs with SHA-256, R version), manifest/output_files_sha256.csv and
#   README.txt (lists every file). OUT = --output-root=<absolute>, default <Behavior>/RANM/board_identifiability, checked
#   by mmm_isolated_output_root_check(); run id = --run-id=<id>, default idf_v1_<short commit>.
#
# Definitions (cage-episode least-squares geometry, sigma = 1):
#   info         squared norm of the target column residualised on the other retained columns; SE/sigma = 1/sqrt(info)
#   VIF          centred sum of squares of the target column / info
#   eff_n_CON    nominal CON n (24; 6 per CC) x info / info of the same contrast without board terms (M0 or M3n)
#   Kish ESS     (sum |w|)^2 / sum w^2 over the CON (or SIS) episodes, w the weights with beta_hat = sum w * y
#   kappa        exact 2-norm condition number of the retained columns scaled to unit length; kappa_scaled_centred is
#                the same after z-scoring the continuous covariates (log_xt, mean_gap_ms, lattice_k1, n_animals)
#   GLS eff n    CON information under V = (1 - rho) I + rho Z Z' (Z: cage; a CON cage spans the four CCs of its
#                batch, an SIS cage-episode is its own cage) relative to the no-board model at rho = 0, x nominal n
#   two-stage    recording and board effects estimated from the SIS cage-episodes only, CON offset = CON - prediction;
#                CON-equivalent n relative to the no-board recording contrast (offset variance 1 + 1/4)
#   log_xt       log of the median incoming-crosstalk ratio over the board's live readers in that recording
#
# Run from the repository root (one R process, well under a minute):
#   Rscript Testing/audits/audit_board_group_identifiability.R [--run-id=<new id>] [--output-root=<absolute>]
# ================================================================

IDF_VERSION <- "idf_v1"
IDF_SCRIPT <- "Testing/audits/audit_board_group_identifiability.R"
IDF_QC_RUN <- "raqc_20261013_b1783bf"
IDF_LABEL_COLUMNS <- c("AnimalNum", "outcome_group", "Sex")
IDF_MALE_BATCHES <- c("B1", "B2", "B5")
IDF_BOARDS <- 1:5
IDF_CONTINUOUS <- c("log_xt", "mean_gap_ms", "lattice_k1", "n_animals")
IDF_CC_TARGETS <- paste0("CON_CC", 1:4)
IDF_RHO <- c(0, 0.1, 0.25, 0.5, 0.75, 0.9)
IDF_RHO_CC <- c(0, 0.5, 0.9)
IDF_HW_COVARIATES <- c("log_xt", "xt_med", "xt_med_all", "rr_med", "best_med", "mean_gap_ms", "lattice_k1", "z_gap", "z_k1",
                       "silent_share")

# Cage-episode parameterisations. tg: the target columns; keep_order: model terms in the order written (else R puts
# main effects before interactions).
IDF_MODELS <- list(
  M0_group_CC_batch = list(rhs = "CON + CC + Batch", tg = "CON"),
  M0s_plus_sex = list(rhs = "CON + sex + CC + Batch", tg = c("CON", "sexM"),
                      note = "sex is a contrast of batches (sex nested in batch)"),
  M0r_recordingFE = list(rhs = "CON + rec", tg = "CON"),
  M1_board = list(rhs = "CON + CC + Batch + boardf", tg = "CON"),
  M1r_recFE_board = list(rhs = "CON + rec + boardf", tg = "CON"),
  M1b_b34only = list(rhs = "CON + CC + Batch + b34", tg = "CON"),
  M2_boardxCC = list(rhs = "CON + CC + Batch + boardf + boardf:CC", tg = "CON"),
  M2r_recFE_boardxCC = list(rhs = "CON + rec + boardf + boardf:CC", tg = "CON"),
  M2s_board_x_sex = list(rhs = "CON + CC + Batch + boardf + boardf:sex", tg = "CON"),
  M2a_board_x_batch = list(rhs = "CON + CC + Batch + boardf + boardf:Batch", tg = "CON"),
  M3_groupxCC_board = list(rhs = "CON_CC1 + CON_CC2 + CON_CC3 + CON_CC4 + CC + Batch + boardf", tg = IDF_CC_TARGETS),
  M3n_groupxCC_noboard = list(rhs = "CON_CC1 + CON_CC2 + CON_CC3 + CON_CC4 + CC + Batch", tg = IDF_CC_TARGETS),
  M4_groupxCC_boardxCC = list(rhs = "CON_CC1 + CON_CC2 + CON_CC3 + CON_CC4 + CC + Batch + boardf + boardf:CC", tg = IDF_CC_TARGETS),
  M4b_groupxCC_b34xCC = list(rhs = "CON_CC1 + CON_CC2 + CON_CC3 + CON_CC4 + CC + Batch + b34 + b34:CC", tg = IDF_CC_TARGETS),
  M5_group_x_board = list(rhs = "CON + CC + Batch + boardf + CON:boardf", tg = "CON"),
  M5b_group_x_board_x_CC = list(rhs = "CON + CC + Batch + boardf + CON:boardf:CC", tg = "CON",
                                note = "group x board x CC: four board x CC cells hold no CON episode"),
  M6_xt = list(rhs = "CON + CC + Batch + log_xt", tg = c("CON", "log_xt")),
  M6b_xt_plus_board = list(rhs = "CON + CC + Batch + boardf + log_xt", tg = c("CON", "log_xt")),
  M6c_groupxCC_xt = list(rhs = "CON_CC1 + CON_CC2 + CON_CC3 + CON_CC4 + CC + Batch + log_xt", tg = c(IDF_CC_TARGETS, "log_xt")),
  M6d_groupxCC_xt_x_CC = list(rhs = "CON_CC1 + CON_CC2 + CON_CC3 + CON_CC4 + CC + Batch + log_xt + log_xt:CC",
                              tg = c(IDF_CC_TARGETS, "log_xt")),
  M7_signature = list(rhs = "CON + CC + Batch + mean_gap_ms + lattice_k1", tg = c("CON", "mean_gap_ms", "lattice_k1")),
  M7b_xt_signature = list(rhs = "CON + CC + Batch + log_xt + mean_gap_ms + lattice_k1",
                          tg = c("CON", "log_xt", "mean_gap_ms", "lattice_k1")),
  M8_ntracked = list(rhs = "CON + CC + Batch + boardf + n_animals", tg = c("CON", "n_animals")),
  M8b_ntracked_x_batch = list(rhs = "CON + CC + Batch + boardf + n_animals + n_animals:Batch", tg = "CON",
                              note = "B2 and B3 track four animals in every cage"),
  M9_full_saturated_board = list(rhs = "CON + rec + boardf:CC", tg = "CON"),
  M9b_board_x_rec = list(rhs = "CON + rec + boardf + boardf:rec", tg = "CON",
                         note = "saturated (120 columns for 120 cage-episodes); the column dropped is arbitrary, so CON is not identified (see M9c)"),
  M9c_board_x_rec_CON_last = list(rhs = "rec + boardf + boardf:rec + CON", tg = "CON", keep_order = TRUE,
                                  note = "M9b with CON entered last: CON is a combination of the board x recording cells")
)

IDF_FILE_DOCS <- c(
  "tables/cage_episodes.csv" = "the 120 cage-episodes (Batch x CC x board): animals, CON/SIS/RES/SUS counts, cage type and id, dead channel, hardware covariates",
  "tables/allocation_batch_cc.csv" = "per recording (Batch x CC): the CON cage's board and the SIS boards",
  "tables/allocation_null.csv" = "CON cage-episodes on boards 3/4: observed against uniform placement within each recording (exact distribution)",
  "tables/board_counts.csv" = "cage-episodes (CON, SIS) and animal-episodes (CON, RES, SUS) per board: overall and by sex, CC, batch, SUS count, RES/SUS mix",
  "tables/cor_CON_board.csv" = "r(CON, boards 3/4), r(CON, board 5) and Cramer's V (CON x board) overall and by CC, sex and sex x CC",
  "tables/design_models.csv" = "one row per model parameterisation: p, rank, aliased columns, residual df, kappa, CON SE factor, VIF, efficiency, effective CON n, Kish ESS",
  "tables/design_targets.csv" = "every model x target column (CON, CON_CCk, sexM, covariates): status, information, SE factor, VIF, efficiency, effective n, Kish ESS",
  "tables/design_cc_specific.csv" = "CC-specific CON contrasts (group x CC models): per model and CC the VIF, effective CON n of 6 and Kish ESS",
  "tables/design_gvif.csv" = "generalised VIF per term (car::vif, type = 'terms') of the full-rank models",
  "tables/design_alias.csv" = "alias() of each rank-deficient model: every aliased column as a combination of the retained ones",
  "tables/con_weights.csv" = "weight of each cage-episode in the CON estimate (sum over CON = 1) without board (M0), with board (M1), with board x CC (M2)",
  "tables/con_weight_summary.csv" = "CON weight per CC and the CON and SIS weight mass on boards 3/4 (M0, M1, M2)",
  "tables/sis_only_board_models.csv" = "board estimated from SIS cage-episodes within recordings: rank, residual df, kappa, SE factors of the board contrasts",
  "tables/two_stage_offsets.csv" = "CON offsets from the SIS-only board fits: offset variance and CON-equivalent n, overall and per CC",
  "tables/con_offset_variance.csv" = "per CON cage-episode offset variance under the SIS-only board and board x CC fits",
  "tables/hardware_covariate_collinearity.csv" = "group-blind hardware covariates: variance share explained by board (and recording), correlation with CON, boards 3/4 and tracked-animal count",
  "tables/hardware_covariate_by_stratum.csv" = "r(CON, log_xt), within board, and r(CON, board-constant crosstalk score) overall, per CC and per sex",
  "tables/hardware_crosstalk_by_board.csv" = "crosstalk ratio per board over the 24 recordings (median, range, SIS-only median) and how often the board is among the two lowest",
  "tables/hardware_con_crosstalk_rank.csv" = "rank of the CON cage's crosstalk within its recording (1 = lowest), per CC",
  "tables/hardware_partial_correlations.csv" = "marginal and partial correlations of CON, tracked-animal count, boards 3/4 and board 5 with the hardware covariates",
  "tables/tracked_animals.csv" = "cage-episodes by batch, cage type and number of tracked animals",
  "tables/gls_effective_n.csv" = "GLS effective number of CON episodes (nominal 24, 6 CON cages) against the cage intra-class correlation rho",
  "tables/gls_effective_n_cc.csv" = "the same for the CC-specific CON contrasts (nominal 6 each), without board and with board x CC",
  "tables/res_sus_correlations.csv" = "SIS animal-episodes: r(SUS, boards 3/4), r(SUS, board 5), r(SUS, board 1) overall and by CC, sex and sex x CC",
  "tables/res_sus_design.csv" = "SUS vs RES contrast at the animal-episode level: rank, VIF, SE factor, Kish ESS, cages informing, efficiency vs no board",
  "tables/res_sus_gls.csv" = "SUS information under animal and cage-episode intra-class correlations, relative to the independent no-board design",
  "tables/three_group_aliasing.csv" = "three-group (RES, CON, SUS) animal-episode models: rank and aliased columns (with and without cage-episode fixed effects)",
  "tables/animal_board_history.csv" = "per animal: group, sex, batch, episodes, CCs on boards 3/4 and on board 5, board sequence over CC1-CC4",
  "tables/animal_board_exposure.csv" = "per sex and group: animal-episode shares on boards 3/4 and 5, mean CCs on boards 3/4 and 5, animals by CCs on boards 3/4",
  "tables/sis_cage_res_sus_mix.csv" = "SIS cage-episodes that are all RES, all SUS or mixed",
  "tables/con_cage_board_sequences.csv" = "per batch: the CON animals' board sequence over CC1-CC4 (one shared sequence = one persistent cage)",
  "tables/input_checks.csv" = "input coverage and structure checks (labels, allocation, dead channels)",
  "tables/key_numbers.csv" = "headline quantities of this audit",
  "manifest/run.csv" = "run id, version, git commit, script SHA-256 and git status, inputs with SHA-256, R and package versions, times",
  "manifest/output_files_sha256.csv" = "SHA-256 and size of every other file of the run",
  "README.txt" = "this file"
)

# ------------------------------------------------------------------ small helpers

idf_args <- function(args) {
  known <- "^--(run-id|output-root)="
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  if (anyDuplicated(sub("=.*$", "", args))) stop("Each argument may be given once.", call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  list(run_id = val("run-id"), output_root = val("output-root"))
}

idf_git <- function(args) suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, args), stdout = TRUE, stderr = TRUE))

idf_phi <- function(a, b) if (stats::sd(a) == 0 || stats::sd(b) == 0) NA_real_ else stats::cor(a, b)

idf_cramers_v <- function(tab) {
  if (min(dim(tab)) < 2L) return(NA_real_)
  x2 <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
  unname(sqrt(x2 / (sum(tab) * (min(dim(tab)) - 1))))
}

idf_kish <- function(w) if (!length(w) || all(w == 0)) 0 else sum(abs(w))^2 / sum(w^2)

# A stand-in response for lm() where only the design matters (alias(), car::vif()); it is never an outcome.
idf_placeholder <- function(n) sin(seq_len(n))

idf_terms <- function(rhs, keep_order = FALSE, lhs = "") stats::terms(stats::as.formula(paste(lhs, "~", rhs)), keep.order = keep_order)

idf_mm <- function(rhs, data, keep_order = FALSE) stats::model.matrix(idf_terms(rhs, keep_order), data)

# Columns kept and dropped by the pivoted QR (tolerance as lm()).
idf_full_rank <- function(X, tol = 1e-7) {
  q <- qr(X, tol = tol); rk <- q$rank
  list(X = X[, q$pivot[seq_len(rk)], drop = FALSE], rank = rk, p = ncol(X),
       aliased = if (rk < ncol(X)) colnames(X)[q$pivot[(rk + 1):ncol(X)]] else character(0))
}

idf_kappa <- function(X) kappa(sweep(X, 2, sqrt(colSums(X^2)), "/"), exact = TRUE)

# Residualised target column of a full-rank matrix: information and estimator weights.
idf_target <- function(Xr, tg) {
  x <- Xr[, tg]
  res <- qr.resid(qr(Xr[, colnames(Xr) != tg, drop = FALSE], tol = 1e-7), x)
  info <- sum(res^2)
  list(x = x, info = info, w = res / info)
}

idf_weights <- function(rhs, tg, d) idf_target(idf_full_rank(idf_mm(rhs, d))$X, tg)$w

# ------------------------------------------------------------------ inputs and the cage-episode table

idf_read_inputs <- function(paths) {
  rd <- function(role, cols) {
    x <- data.table::fread(paths[[role]], select = cols, colClasses = if ("AnimalNum" %in% cols) list(character = "AnimalNum"))
    miss <- setdiff(cols, names(x))
    if (length(miss)) stop(role, ": missing column(s) ", paste(miss, collapse = ", "), call. = FALSE)
    x
  }
  list(animal_qc = rd("animal_qc", c("Batch", "CC", "AnimalNum", "board", "System")),
       reader_qc = rd("reader_qc", c("Batch", "CC", "board", "PositionID", "zero_read_session", "crosstalk_in_ratio",
                                     "reads_rel_homologous", "session_best_ratio")),
       board_qc = rd("board_qc", c("Batch", "CC", "board", "tags_present", "silent_share")),
       board_history = rd("board_history", c("Batch", "CC", "board", "mean_gap_ms", "lattice_k1", "z_gap", "z_k1")),
       labels = rd("labels", IDF_LABEL_COLUMNS))
}

idf_build <- function(inp) {
  aq <- data.table::copy(inp$animal_qc)[, AnimalNum := canonical_animal_id(AnimalNum)]
  gl <- data.table::copy(inp$labels)[, AnimalNum := canonical_animal_id(AnimalNum)]
  if (anyDuplicated(gl$AnimalNum)) stop("The label table repeats an animal.", call. = FALSE)
  if (anyDuplicated(aq[, .(Batch, CC, AnimalNum)])) stop("animal_qc repeats an animal within a recording.", call. = FALSE)
  if (!all(aq$System == paste0("sys.", aq$board))) stop("animal_qc: System is not sys.<board>.", call. = FALSE)
  ae <- merge(aq[, .(Batch, CC, AnimalNum, board)], gl, by = "AnimalNum", all.x = TRUE)
  if (anyNA(ae$outcome_group)) stop("Animal-episodes without a group label: ", paste(unique(ae[is.na(outcome_group)]$AnimalNum), collapse = " "), call. = FALSE)
  if (!all(ae$outcome_group %in% c("CON", "RES", "SUS"))) stop("Unexpected outcome_group value.", call. = FALSE)
  ae[, sex := data.table::fifelse(Batch %in% IDF_MALE_BATCHES, "M", "F")]
  sex_bad <- ae[substr(Sex, 1, 1) != sex, unique(AnimalNum)]
  if (length(sex_bad)) stop("Label Sex disagrees with the batch sex for: ", paste(sex_bad, collapse = " "), call. = FALSE)

  ce <- ae[, .(n_animals = .N, n_CON = sum(outcome_group == "CON"), n_SIS = sum(outcome_group != "CON"),
               n_RES = sum(outcome_group == "RES"), n_SUS = sum(outcome_group == "SUS"),
               animals = paste(sort(AnimalNum), collapse = ";")), by = .(Batch, CC, board)]
  ce[, cage_type := data.table::fifelse(n_SIS == 0L, "CON", data.table::fifelse(n_CON == 0L, "SIS", "MIXED"))]
  if (any(ce$cage_type == "MIXED")) stop("A cage-episode mixes CON and SIS animals.", call. = FALSE)

  # group-blind hardware covariates per board-recording (dead channels left out of the crosstalk median)
  rq <- inp$reader_qc
  hw <- rq[zero_read_session == FALSE, .(xt_med = stats::median(crosstalk_in_ratio, na.rm = TRUE),
                                          rr_med = stats::median(reads_rel_homologous, na.rm = TRUE),
                                          best_med = stats::median(session_best_ratio, na.rm = TRUE)), by = .(Batch, CC, board)]
  hw_all <- rq[, .(xt_med_all = stats::median(crosstalk_in_ratio, na.rm = TRUE), readers = .N,
                   dead_channels = sum(zero_read_session %in% TRUE)), by = .(Batch, CC, board)]
  hw <- merge(merge(hw, hw_all, by = c("Batch", "CC", "board")), inp$board_history, by = c("Batch", "CC", "board"))
  hw <- merge(hw, inp$board_qc, by = c("Batch", "CC", "board"))
  d <- merge(ce, hw, by = c("Batch", "CC", "board"))
  if (nrow(d) != 120L || nrow(ce) != 120L) stop("Expected 120 cage-episodes with hardware covariates, found ", nrow(d), call. = FALSE)

  d[, `:=`(CON = as.integer(cage_type == "CON"), b34 = as.integer(board %in% 3:4), b5 = as.integer(board == 5L),
           sex = factor(data.table::fifelse(Batch %in% IDF_MALE_BATCHES, "M", "F")), boardf = factor(board),
           log_xt = log(xt_med), dropout = dead_channels > 0L)]
  d[, `:=`(Batch = factor(Batch), CC = factor(CC))]
  d[, rec := interaction(Batch, CC, drop = TRUE)]
  d[, cage_id := data.table::fifelse(CON == 1L, paste0(Batch, "_CON"), paste(Batch, CC, board, sep = "_"))]
  for (k in levels(d$CC)) d[, (paste0("CON_", k)) := as.integer(CON == 1L & CC == k)]
  d[, xt_rank_in_recording := data.table::frank(xt_med), by = rec]
  per_rec <- d[, .(boards = data.table::uniqueN(board), n_con = sum(CON)), by = rec]
  if (nrow(per_rec) != 24L || any(per_rec$boards != 5L) || any(per_rec$n_con != 1L))
    stop("Expected 24 recordings with five boards and one CON cage each.", call. = FALSE)

  lab_n <- gl[, .N, by = .(Sex, outcome_group)][order(Sex, outcome_group)]
  dead <- d[dropout == TRUE]
  con_tags <- d[CON == 1L & tags_present != 4L]
  checks <- data.table::data.table(
    check = c("animal_qc rows (animal-episodes)", "animal_qc animals", "label rows", "label animals", "labels by Sex x group",
              "animal-episodes without a label", "labelled animals not in animal_qc", "System equals sys.<board>",
              "label Sex disagreeing with the batch sex", "cage-episodes", "CON / SIS / MIXED cage-episodes",
              "recordings with five boards and one CON cage", "CON cage-episodes with tags_present != 4",
              "dead-channel board-recordings (zero-read sessions)"),
    value = c(nrow(aq), data.table::uniqueN(aq$AnimalNum), nrow(gl), data.table::uniqueN(gl$AnimalNum),
              paste(sprintf("%s %s %d", lab_n$Sex, lab_n$outcome_group, lab_n$N), collapse = "; "),
              0L, paste(sort(setdiff(gl$AnimalNum, aq$AnimalNum)), collapse = " "), "TRUE", 0L, nrow(d),
              paste(sum(d$cage_type == "CON"), sum(d$cage_type == "SIS"), sum(d$cage_type == "MIXED"), sep = " / "), nrow(per_rec),
              if (nrow(con_tags)) paste(sprintf("%s %s board %d (%d tags)", as.character(con_tags$Batch), as.character(con_tags$CC), con_tags$board,
                                                con_tags$tags_present), collapse = "; ") else "none",
              paste(sprintf("%s %s board %d (%s; %d dead of %d readers)", as.character(dead$Batch), as.character(dead$CC), dead$board,
                            dead$cage_type, dead$dead_channels, dead$readers), collapse = "; ")))
  list(ae = ae, d = d, checks = checks)
}

# ------------------------------------------------------------------ allocation

idf_board_counts <- function(board, unit, group, by_name, stratum) {
  y <- data.table::data.table(group = group, stratum = stratum, board = as.integer(board))
  w <- y[, as.list(stats::setNames(tabulate(board, nbins = length(IDF_BOARDS)), paste0("board_", IDF_BOARDS))), by = .(group, stratum)]
  w[, total := rowSums(.SD), .SDcols = paste0("board_", IDF_BOARDS)]
  data.table::set(w, j = "unit", value = unit); data.table::set(w, j = "stratum_by", value = by_name)
  data.table::setcolorder(w, c("unit", "stratum_by"))
  w[order(group, stratum)]
}

idf_allocation <- function(d, ae) {
  alloc <- d[, .(sex = as.character(sex[1]), CON_board = paste(board[CON == 1L], collapse = ","),
                 SIS_boards = paste(board[CON == 0L], collapse = ","), missing_board = paste(setdiff(IDF_BOARDS, board), collapse = ",")),
             by = .(Batch, CC)]
  # CON on boards 3/4 against uniform placement among each recording's occupied boards (exact convolution)
  p <- d[, .(p = sum(b34) / .N), by = rec]$p
  dist <- 1
  for (q in p) dist <- c(dist * (1 - q), 0) + c(0, dist * q)
  obs <- d[CON == 1L, sum(b34)]
  alloc_null <- data.table::data.table(CON_episodes = sum(d$CON), CON_share_b34 = d[CON == 1L, mean(b34)], SIS_share_b34 = d[CON == 0L, mean(b34)],
                                 share_b34_if_random = 2 / 5, observed_CON_on_b34 = obs, expected_CON_on_b34 = sum(p),
                                 P_count_ge_observed = sum(dist[(obs + 1):length(dist)]), P_count_le_observed = sum(dist[1:(obs + 1)]))
  s <- d[CON == 0L]
  mix <- data.table::fifelse(s$n_SUS == 0L, "all RES", data.table::fifelse(s$n_RES == 0L, "all SUS", "mixed"))
  ce_by <- list(all = rep("all", nrow(d)), sex = as.character(d$sex), CC = as.character(d$CC), Batch = as.character(d$Batch))
  ae_by <- list(all = rep("all", nrow(ae)), sex = ae$sex, CC = ae$CC)
  counts <- data.table::rbindlist(c(
    lapply(names(ce_by), function(b) idf_board_counts(d$board, "cage_episode", d$cage_type, b, ce_by[[b]])),
    list(idf_board_counts(s$board, "cage_episode", "SIS", "n_SUS", paste0("n_SUS=", s$n_SUS)),
         idf_board_counts(s$board, "cage_episode", "SIS", "RES/SUS mix", mix)),
    lapply(names(ae_by), function(b) idf_board_counts(ae$board, "animal_episode", ae$outcome_group, b, ae_by[[b]]))))
  mixt <- data.table::data.table(mix = mix, n_animals = s$n_animals)[, .(cage_episodes = .N, animal_episodes = sum(n_animals)), by = mix][order(mix)]
  list(allocation_batch_cc = alloc, allocation_null = alloc_null, board_counts = counts, sis_cage_res_sus_mix = mixt)
}

idf_correlations <- function(d) {
  strata <- list(all = rep("all", nrow(d)), CC = paste0("CC=", d$CC), sex = paste0("sex=", d$sex),
                 sex_CC = paste0("sex=", d$sex, " CC=", d$CC))
  tab <- data.table::rbindlist(lapply(strata, function(g) {
    x <- data.table::copy(d)[, stratum := g]
    x[, .(n = .N, n_CON = sum(CON), CON_on_b34 = sum(CON * b34), SIS_on_b34 = sum((1L - CON) * b34),
          r_CON_b34 = idf_phi(CON, b34), r_CON_b5 = idf_phi(CON, b5), cramers_v_CON_board = idf_cramers_v(table(CON, board))),
      by = stratum][order(stratum)]
  }))
  within <- stats::cor(d$CON - stats::ave(d$CON, d$rec), d$b34 - stats::ave(d$b34, d$rec))
  list(cor_CON_board = tab, r_within_recording = within)
}

# ------------------------------------------------------------------ cage-episode design diagnostics

idf_design <- function(label, spec, d, dz) {
  ko <- isTRUE(spec$keep_order)
  fr <- idf_full_rank(idf_mm(spec$rhs, d, ko))
  frz <- idf_full_rank(idf_mm(spec$rhs, dz, ko))
  isC <- d$CON == 1L
  tg <- data.table::rbindlist(lapply(spec$tg, function(t) {
    if (t %in% fr$aliased) return(data.table::data.table(model = label, target = t, status = "ALIASED"))
    if (!t %in% colnames(fr$X)) return(data.table::data.table(model = label, target = t, status = "absent"))
    z <- idf_target(fr$X, t)
    data.table::data.table(model = label, target = t, status = "identified", info = z$info, se_factor = sqrt(1 / z$info),
                           vif = sum((z$x - mean(z$x))^2) / z$info, kish_CON = idf_kish(z$w[isC]), kish_SIS = idf_kish(z$w[!isC]),
                           wsum_CON = sum(z$w[isC]), n_CON_w_gt1pct = sum(abs(z$w[isC]) > 0.01 * max(abs(z$w[isC]))))
  }), fill = TRUE)
  list(summary = data.table::data.table(model = label, rhs = spec$rhs, term_order = if (ko) "as written" else "R default",
                                        targets = paste(spec$tg, collapse = " "), n = nrow(d), p = fr$p, rank = fr$rank,
                                        n_aliased = length(fr$aliased), aliased = paste(fr$aliased, collapse = " "),
                                        resid_df = nrow(d) - fr$rank, kappa_scaled = idf_kappa(fr$X), kappa_scaled_centred = idf_kappa(frz$X),
                                        note = if (is.null(spec$note)) "" else spec$note),
       targets = tg)
}

idf_term_gvif <- function(label, spec, d) {
  dd <- data.table::copy(d)[, yy := idf_placeholder(.N)]
  fit <- stats::lm(idf_terms(spec$rhs, isTRUE(spec$keep_order), "yy"), data = dd)
  if (anyNA(stats::coef(fit))) return(NULL)
  g <- tryCatch(suppressWarnings(suppressMessages(car::vif(fit, type = "terms"))), error = function(e) NULL)
  if (is.null(g)) return(NULL)
  if (is.null(dim(g))) data.table::data.table(model = label, term = names(g), GVIF = unname(g), Df = 1, GVIF_adj = sqrt(unname(g)))
  else data.table::data.table(model = label, term = rownames(g), GVIF = g[, 1], Df = g[, 2], GVIF_adj = g[, 3])
}

idf_alias <- function(label, spec, d) {
  dd <- data.table::copy(d)[, yy := idf_placeholder(.N)]
  cm <- stats::alias(stats::lm(idf_terms(spec$rhs, isTRUE(spec$keep_order), "yy"), data = dd))$Complete
  if (is.null(cm)) return(NULL)
  data.table::rbindlist(lapply(seq_len(nrow(cm)), function(i) {
    v <- cm[i, ]; nz <- v[abs(v) > 1e-8]
    data.table::data.table(model = label, aliased_column = rownames(cm)[i],
                           combination = if (length(nz)) paste(sprintf("%+.2f*%s", nz, names(nz)), collapse = " ")
                                         else "(zero column: no CON cage-episode in this cell)")
  }))
}

idf_identifiability <- function(d) {
  dz <- data.table::copy(d)
  for (v in IDF_CONTINUOUS) data.table::set(dz, j = v, value = as.numeric(scale(dz[[v]])))
  r <- lapply(names(IDF_MODELS), function(nm) idf_design(nm, IDF_MODELS[[nm]], d, dz))
  summ <- data.table::rbindlist(lapply(r, `[[`, "summary"))
  tgt <- data.table::rbindlist(lapply(r, `[[`, "targets"), fill = TRUE)
  info_M0 <- tgt[model == "M0_group_CC_batch" & target == "CON"]$info
  info_M3n <- tgt[model == "M3n_groupxCC_noboard"]
  tgt[, rel_eff := NA_real_]
  tgt[target == "CON", rel_eff := info / info_M0]
  for (k in IDF_CC_TARGETS) { ref <- info_M3n[target == k]$info; tgt[target == k, rel_eff := info / ref] }
  nominal <- c(CON = sum(d$CON), vapply(IDF_CC_TARGETS, function(k) as.numeric(sum(d[[k]])), numeric(1)))
  tgt[, eff_n := data.table::fifelse(target %in% names(nominal), nominal[target] * rel_eff, NA_real_)]
  con <- tgt[target == "CON", .(model, CON_status = status, CON_se_factor = se_factor, CON_vif = vif, CON_rel_eff = rel_eff,
                                CON_eff_n = eff_n, CON_kish_CON = kish_CON, CON_kish_SIS = kish_SIS, CON_n_w_gt1pct = n_CON_w_gt1pct)]
  models <- merge(summ, con, by = "model", all.x = TRUE, sort = FALSE)
  cc <- tgt[target %in% IDF_CC_TARGETS, .(model, CC = sub("^CON_", "", target), status, se_factor, vif, rel_eff, eff_n_CON_of_6 = eff_n,
                                          kish_CON, kish_SIS, n_CON_w_gt1pct)]
  gv <- data.table::rbindlist(lapply(summ[n_aliased == 0L]$model, function(nm) idf_term_gvif(nm, IDF_MODELS[[nm]], d)))
  al <- data.table::rbindlist(lapply(summ[n_aliased > 0L]$model, function(nm) idf_alias(nm, IDF_MODELS[[nm]], d)))

  w <- d[, .(Batch, CC, board, cage_type, b34)]
  w[, `:=`(w_M0 = idf_weights("CON + CC + Batch", "CON", d), w_M1 = idf_weights("CON + CC + Batch + boardf", "CON", d),
           w_M2 = idf_weights("CON + CC + Batch + boardf + boardf:CC", "CON", d))]
  ws <- rbind(w[cage_type == "CON", .(M0 = sum(w_M0), M1 = sum(w_M1), M2 = sum(w_M2)), by = .(stratum = as.character(CC))][, quantity := "CON weight sum"],
              w[cage_type == "CON" & b34 == 1L, .(stratum = "all", M0 = sum(w_M0), M1 = sum(w_M1), M2 = sum(w_M2))][, quantity := "CON weight mass on boards 3/4"],
              w[cage_type == "SIS" & b34 == 1L, .(stratum = "all", M0 = sum(w_M0), M1 = sum(w_M1), M2 = sum(w_M2))][, quantity := "SIS weight mass on boards 3/4 (signed)"])
  data.table::setcolorder(ws, c("quantity", "stratum"))
  list(design_models = models, design_targets = tgt, design_cc_specific = cc, design_gvif = gv, design_alias = al,
       con_weights = w[, !"b34"], con_weight_summary = ws)
}

# ------------------------------------------------------------------ SIS-only board estimation and two-stage CON offsets

idf_sis_fit <- function(rhs, data, label) {
  fr <- idf_full_rank(idf_mm(rhs, data))
  XtXi <- solve(crossprod(fr$X))
  b <- grep("^boardf[0-9]$", colnames(fr$X))
  list(label = label, rhs = rhs, X = fr$X, XtXi = XtXi, n = nrow(fr$X), p = fr$p, rank = fr$rank, kappa = idf_kappa(fr$X),
       se_board = sqrt(diag(XtXi)[b]))
}

idf_pred_var <- function(f, newd) { Xn <- idf_mm(f$rhs, newd)[, colnames(f$X), drop = FALSE]; Xn %*% f$XtXi %*% t(Xn) }

idf_two_stage <- function(f, key, con, subset) {
  V <- idf_pred_var(f, con); n <- nrow(con); one <- rep(1 / n, n)
  var_mean <- sum(one^2) + as.numeric(t(one) %*% V %*% one)
  data.table::data.table(sis_model = key, sis_model_label = f$label, CON_subset = subset, n_CON = n, mean_var_offset = mean(1 + diag(V)),
                         var_mean_offset = var_mean, eff_n = 1 / var_mean)
}

idf_sis_only <- function(d) {
  s <- d[CON == 0L]
  fits <- list(board = idf_sis_fit("rec + boardf", s, "SIS: recording FE + board"),
               board_x_CC = idf_sis_fit("rec + boardf + boardf:CC", s, "SIS: recording FE + board x CC"),
               board_without_dead = idf_sis_fit("rec + boardf", s[dropout == FALSE], "SIS without the 3 dead-channel board-recordings: recording FE + board"))
  models <- data.table::rbindlist(lapply(names(fits), function(k) {
    f <- fits[[k]]
    cbind(data.table::data.table(model = k, label = f$label, rhs = f$rhs, n = f$n, p = f$p, rank = f$rank, resid_df = f$n - f$rank,
                                 kappa_scaled = f$kappa),
          data.table::as.data.table(as.list(stats::setNames(f$se_board, paste0("se_factor_", names(f$se_board), "_vs_board1")))))
  }))
  con <- d[CON == 1L]
  ts <- rbind(idf_two_stage(fits$board, "board", con, "all"), idf_two_stage(fits$board_x_CC, "board_x_CC", con, "all"))
  for (k in levels(d$CC)) ts <- rbind(ts, idf_two_stage(fits$board, "board", con[CC == k], k),
                                      idf_two_stage(fits$board_x_CC, "board_x_CC", con[CC == k], k))
  # no-board reference: CON minus the mean of the four SIS cages of its recording, offset variance 1 + 1/4
  ts[, `:=`(eff_n_no_board_reference = n_CON / 1.25, CON_equiv_n = n_CON * eff_n / (n_CON / 1.25))]
  ov <- con[, .(Batch, CC, board)]
  ov[, `:=`(var_off_board = 1 + diag(idf_pred_var(fits$board, con)), var_off_board_x_CC = 1 + diag(idf_pred_var(fits$board_x_CC, con)))]
  list(sis_only_board_models = models, two_stage_offsets = ts, con_offset_variance = ov[order(CC, board, Batch)])
}

# ------------------------------------------------------------------ group-blind hardware covariates

idf_hardware <- function(d) {
  fm <- function(lhs, rhs) stats::as.formula(paste(lhs, "~", rhs))
  r2 <- function(f, data = d) summary(stats::lm(f, data = data))$r.squared
  rs <- function(f, data = d) stats::resid(stats::lm(f, data = data))
  coll <- data.table::rbindlist(lapply(IDF_HW_COVARIATES, function(v) {
    y <- d[[v]]
    data.table::data.table(
      covariate = v, R2_board = r2(fm(v, "boardf")), R2_b34 = r2(fm(v, "b34")), R2_board_rec = r2(fm(v, "boardf + rec")),
      R2_CON_given_board_rec_increment = r2(fm(v, "boardf + rec + CON")) - r2(fm(v, "boardf + rec")),
      r_CON = stats::cor(d$CON, y), r_CON_within_board = stats::cor(d$CON - stats::ave(d$CON, d$board), rs(fm(v, "boardf"))),
      r_CON_within_board_rec = stats::cor(rs(fm("CON", "boardf + rec")), rs(fm(v, "boardf + rec"))),
      r_CON_within_boardxCC = stats::cor(rs(fm("CON", "boardf * CC + Batch")), rs(fm(v, "boardf * CC + Batch"))),
      r_b34 = stats::cor(d$b34, y), r_ntracked = stats::cor(d$n_animals, y),
      r_ntracked_within_board_rec = stats::cor(rs(fm("n_animals", "boardf + rec")), rs(fm(v, "boardf + rec"))))
  }))
  # board-constant crosstalk score: the board's median over the SIS cage-episodes (allocation only)
  by_board <- merge(d[, .(median_xt = stats::median(xt_med), min_xt = min(xt_med), max_xt = max(xt_med),
                          recordings_among_two_lowest_xt = sum(xt_rank_in_recording <= 2)), by = board],
                    d[CON == 0L, .(median_xt_SIS = stats::median(xt_med)), by = board], by = "board")
  d2 <- merge(d, by_board[, .(board, median_xt_SIS)], by = "board")
  d2[, log_board_score := log(median_xt_SIS)]
  strata <- list(all = rep("all", nrow(d2)), CC = paste0("CC=", d2$CC), sex = paste0("sex=", d2$sex))
  by_stratum <- data.table::rbindlist(lapply(strata, function(g) {
    x <- data.table::copy(d2)[, stratum := g]
    x[, .(n = .N, r_CON_logxt = stats::cor(CON, log_xt),
          r_CON_logxt_within_board = stats::cor(CON - stats::ave(CON, board), log_xt - stats::ave(log_xt, board)),
          r_CON_log_board_score = stats::cor(CON, log_board_score)), by = stratum][order(stratum)]
  }))
  con_rank <- data.table::dcast(d[CON == 1L, .N, by = .(CC, xt_rank = paste0("rank_", xt_rank_in_recording))], CC ~ xt_rank,
                                value.var = "N", fill = 0L)
  pr <- function(a, b, ctrl = NA_character_, data = d) {
    if (is.na(ctrl)) return(stats::cor(data[[a]], data[[b]]))
    stats::cor(rs(fm(a, ctrl), data), rs(fm(b, ctrl), data))
  }
  b23 <- d[Batch %in% c("B2", "B3")][, rec := droplevels(rec)]
  spec <- list(c("CON", "log_xt", "boardf + rec", "all"), c("CON", "log_xt", "boardf + rec + n_animals", "all"),
               c("CON", "log_xt", "boardf + rec + tags_present", "all"), c("CON", "log_xt", "boardf + rec", "B2+B3"),
               c("n_animals", "log_xt", "boardf + rec", "all"), c("CON", "mean_gap_ms", "boardf + rec", "all"),
               c("CON", "lattice_k1", "boardf + rec", "all"), c("b34", "mean_gap_ms", NA, "all"), c("b34", "lattice_k1", NA, "all"),
               c("b5", "mean_gap_ms", NA, "all"), c("b5", "lattice_k1", NA, "all"), c("CON", "n_animals", NA, "all"),
               c("CON", "tags_present", NA, "all"))
  part <- data.table::rbindlist(lapply(spec, function(z) {
    data <- if (z[4] == "B2+B3") b23 else d
    data.table::data.table(x = z[1], y = z[2], controls = if (is.na(z[3])) "none" else z[3], subset = z[4], n = nrow(data),
                           r = pr(z[1], z[2], z[3], data))
  }))
  part <- rbind(part,
                d2[, .(x = "CON", y = "log board score (SIS median)", controls = "none", subset = "all", n = .N, r = stats::cor(CON, log_board_score))],
                d[, .(x = "CON", y = "n_animals", controls = "none", subset = as.character(Batch[1]), n = .N,
                      r = if (stats::sd(n_animals) > 0) stats::cor(CON, n_animals) else NA_real_), by = Batch][, !"Batch"])
  tracked <- data.table::dcast(d[, .N, by = .(Batch, cage_type, n_animals = paste0("tracked_", n_animals))], Batch + cage_type ~ n_animals,
                               value.var = "N", fill = 0L)
  score <- c(CON_sum_log_board_score = sum(d2[CON == 1L]$log_board_score),
             expected_sum_uniform_within_recording = sum(d2[, mean(log_board_score), by = rec]$V1),
             recordings_boards34_two_lowest_xt = sum(d[, identical(sort(as.integer(board[xt_rank_in_recording <= 2])), 3:4), by = rec]$V1))
  list(hardware_covariate_collinearity = coll, hardware_covariate_by_stratum = by_stratum, hardware_crosstalk_by_board = by_board,
       hardware_con_crosstalk_rank = con_rank, hardware_partial_correlations = part, tracked_animals = tracked, score = score)
}

# ------------------------------------------------------------------ GLS effective n under a persistent CON cage

idf_gls_info <- function(rhs, rho, d, target = "CON") {
  X <- idf_full_rank(idf_mm(rhs, d))$X
  Z <- stats::model.matrix(~ 0 + factor(cage_id), d)
  V <- (1 - rho) * diag(nrow(d)) + rho * tcrossprod(Z)
  1 / solve(crossprod(X, solve(V, X)))[target, target]
}

idf_gls <- function(d) {
  i0 <- idf_gls_info("CON + CC + Batch", 0, d)
  gl <- data.table::rbindlist(lapply(c("CON + CC + Batch", "CON + CC + Batch + boardf", "CON + CC + Batch + boardf + boardf:CC",
                                       "CON + CC + Batch + log_xt"), function(rhs)
    data.table::rbindlist(lapply(IDF_RHO, function(r) {
      inf <- idf_gls_info(rhs, r, d)
      data.table::data.table(rhs = rhs, rho_cage = r, info = inf, eff_n_CON = sum(d$CON) * inf / i0)
    }))))
  cc0 <- "CON_CC1 + CON_CC2 + CON_CC3 + CON_CC4 + CC + Batch"
  i0k <- vapply(IDF_CC_TARGETS, function(k) idf_gls_info(cc0, 0, d, k), numeric(1))
  glc <- data.table::rbindlist(lapply(c(cc0, paste(cc0, "+ boardf + boardf:CC")), function(rhs)
    data.table::rbindlist(lapply(IDF_RHO_CC, function(r) data.table::rbindlist(lapply(IDF_CC_TARGETS, function(k) {
      inf <- idf_gls_info(rhs, r, d, k)
      data.table::data.table(rhs = rhs, rho_cage = r, target = k, info = inf, eff_n = sum(d[[k]]) * inf / i0k[[k]])
    }))))))
  list(gls_effective_n = gl, gls_effective_n_cc = glc)
}

# ------------------------------------------------------------------ RES vs SUS (animal-episodes)

idf_sus_design <- function(label, rhs, s) {
  fr <- idf_full_rank(idf_mm(rhs, s))
  base <- data.table::data.table(model = label, rhs = rhs, n = nrow(s), p = fr$p, rank = fr$rank, n_aliased = length(fr$aliased))
  if ("SUS" %in% fr$aliased) return(base[, status := "ALIASED"])
  z <- idf_target(fr$X, "SUS")
  cbind(base, data.table::data.table(status = "identified", vif = sum((z$x - mean(z$x))^2) / z$info, info = z$info,
                                     se_factor = sqrt(1 / z$info), kish_SUS = idf_kish(z$w[z$x == 1]), kish_RES = idf_kish(z$w[z$x == 0]),
                                     n_cages_informing = data.table::uniqueN(s$ce_id[abs(z$w) > 1e-8])))
}

idf_gls_sus <- function(rhs, s2a, s2c, s) {
  X <- idf_full_rank(idf_mm(rhs, s))$X
  Za <- stats::model.matrix(~ 0 + factor(AnimalNum), s); Zc <- stats::model.matrix(~ 0 + factor(ce_id), s)
  V <- (1 - s2a - s2c) * diag(nrow(s)) + s2a * tcrossprod(Za) + s2c * tcrossprod(Zc)
  1 / solve(crossprod(X, solve(V, X)))["SUS", "SUS"]
}

idf_res_sus <- function(ae, d) {
  a <- merge(ae, d[, .(Batch = as.character(Batch), CC = as.character(CC), board, cage_type, log_xt)], by = c("Batch", "CC", "board"))
  a[, `:=`(boardf = factor(board), CCf = factor(CC), Batchf = factor(Batch), b34 = as.integer(board %in% 3:4),
           SUS = as.integer(outcome_group == "SUS"), ce_id = paste(Batch, CC, board, sep = "_"))]
  s <- a[cage_type == "SIS"]
  strata <- list(all = rep("all", nrow(s)), CC = paste0("CC=", s$CC), sex = paste0("sex=", s$sex), sex_CC = paste0("sex=", s$sex, " CC=", s$CC))
  st <- data.table::rbindlist(lapply(strata, function(g) {
    x <- data.table::copy(s)[, stratum := g]
    x[, .(n = .N, n_SUS = sum(SUS), SUS_on_b34 = sum(SUS * b34), RES_on_b34 = sum((1L - SUS) * b34), r_SUS_b34 = idf_phi(SUS, b34),
          r_SUS_b5 = idf_phi(SUS, as.integer(board == 5L)), r_SUS_b1 = idf_phi(SUS, as.integer(board == 1L))), by = stratum][order(stratum)]
  }))
  within <- stats::cor(s$SUS - stats::ave(s$SUS, s$Batch, s$CC), s$b34 - stats::ave(s$b34, s$Batch, s$CC))
  st <- rbind(st, data.table::data.table(stratum = "all, centred within recording", n = nrow(s), n_SUS = sum(s$SUS), r_SUS_b34 = within), fill = TRUE)

  s[, ce_f := factor(ce_id)]
  rs <- rbind(idf_sus_design("R0 SUS + CC + Batch", "SUS + CCf + Batchf", s),
              idf_sus_design("R1 SUS + CC + Batch + board", "SUS + CCf + Batchf + boardf", s),
              idf_sus_design("R2 SUS + CC + Batch + board + board:CC", "SUS + CCf + Batchf + boardf + boardf:CCf", s),
              idf_sus_design("R3 SUS + cage-episode FE (within cage)", "SUS + ce_f", s),
              idf_sus_design("R3b SUS + cage-episode FE + board (board aliased with cage)", "SUS + ce_f + boardf", s),
              idf_sus_design("R4 SUS + log_xt + CC + Batch", "SUS + CCf + Batchf + log_xt", s), fill = TRUE)
  rs[, rel_eff_vs_R0 := info / info[1]]

  a[, `:=`(grp = factor(outcome_group, levels = c("RES", "CON", "SUS")), ce_f = factor(ce_id))]
  three <- data.table::rbindlist(lapply(c("grp + CCf + Batchf", "grp + CCf + Batchf + boardf", "grp + CCf + Batchf + boardf + boardf:CCf",
                                          "grp + ce_f", "ce_f + grp"), function(rhs) {
    fr <- idf_full_rank(idf_mm(rhs, a))
    data.table::data.table(rhs = rhs, n = nrow(a), p = fr$p, rank = fr$rank, n_aliased = length(fr$aliased),
                           aliased = paste(fr$aliased, collapse = " "), group_term_aliased = any(grepl("^grp", fr$aliased)))
  }))

  grid <- data.table::CJ(s2a = c(0, 0.3, 0.6), s2c = c(0, 0.2, 0.4))[s2a + s2c < 1]
  i0 <- idf_gls_sus("SUS + CCf + Batchf", 0, 0, s)
  ge <- grid[, .(noboard = idf_gls_sus("SUS + CCf + Batchf", s2a, s2c, s) / i0,
                 board = idf_gls_sus("SUS + CCf + Batchf + boardf", s2a, s2c, s) / i0,
                 boardxCC = idf_gls_sus("SUS + CCf + Batchf + boardf + boardf:CCf", s2a, s2c, s) / i0), by = .(s2a, s2c)]

  pa <- a[order(CC), .(n_ep = .N, n_b34 = sum(b34), n_b5 = sum(board == 5L), board_sequence = paste(board, collapse = "")),
          by = .(AnimalNum, outcome_group, sex, Batch)][order(Batch, outcome_group, AnimalNum)]
  expo <- data.table::rbindlist(lapply(c("all", "F", "M"), function(sx) {
    pp <- if (sx == "all") pa else pa[sex == sx]; ee <- if (sx == "all") a else a[sex == sx]
    x <- merge(ee[, .(animal_episodes = .N, share_ep_b34 = mean(b34), share_ep_b5 = mean(board == 5L)), by = outcome_group],
               pp[, c(list(animals = .N, mean_ep = mean(n_ep), mean_CC_on_b34 = mean(n_b34), mean_CC_on_b5 = mean(n_b5)),
                      as.list(stats::setNames(tabulate(n_b34 + 1L, 5L), paste0("animals_", 0:4, "_CC_on_b34")))), by = outcome_group],
               by = "outcome_group")
    x[, sex := sx]
    data.table::setcolorder(x, c("sex", "outcome_group"))
  }))
  seqs <- a[outcome_group == "CON"][order(CC), .(seq = paste(board, collapse = "")), by = .(Batch, AnimalNum)]
  seqs <- seqs[, .(n_animals = .N, n_distinct_sequences = data.table::uniqueN(seq), board_sequence_CC1_to_CC4 = seq[1]), by = Batch]
  list(res_sus_correlations = st, res_sus_design = rs, res_sus_gls = ge, three_group_aliasing = three,
       animal_board_history = pa, animal_board_exposure = expo, con_cage_board_sequences = seqs, r_within = within)
}

# ------------------------------------------------------------------ headline numbers

idf_key_numbers <- function(tb, extra) {
  dm <- tb$design_models; cc <- tb$design_cc_specific; so <- tb$sis_only_board_models; ts <- tb$two_stage_offsets
  hw <- tb$hardware_covariate_collinearity; gl <- tb$gls_effective_n; cr <- tb$cor_CON_board; rs <- tb$res_sus_design
  pc <- tb$hardware_partial_correlations
  k <- function(q, v, note = "") data.table::data.table(quantity = q, value = as.numeric(v), note = note)
  rbind(
    k("CON cage-episodes on boards 3/4, observed", tb$allocation_null$observed_CON_on_b34, "of 24"),
    k("CON cage-episodes on boards 3/4, expected under uniform within-recording placement", tb$allocation_null$expected_CON_on_b34),
    k("P(count >= observed) under uniform within-recording placement", tb$allocation_null$P_count_ge_observed, "allocation description, not a test"),
    k("r(CON, boards 3/4), all cage-episodes", cr[stratum == "all"]$r_CON_b34),
    k("r(CON, boards 3/4), centred within recording", extra$r_within_recording, "equals the marginal r: every recording uses all five boards once"),
    k("r(CON, boards 3/4), CC1", cr[stratum == "CC=CC1"]$r_CON_b34),
    k("Cramer's V (CON x board), all", cr[stratum == "all"]$cramers_v_CON_board),
    k("Cramer's V (CON x board), CC1", cr[stratum == "CC=CC1"]$cramers_v_CON_board),
    k("M1_board: CON VIF", dm[model == "M1_board"]$CON_vif),
    k("M1_board: effective CON n", dm[model == "M1_board"]$CON_eff_n, "of 24"),
    k("M2_boardxCC: effective CON n", dm[model == "M2_boardxCC"]$CON_eff_n, "of 24"),
    k("M4_groupxCC_boardxCC: effective CON n at CC1", cc[model == "M4_groupxCC_boardxCC" & CC == "CC1"]$eff_n_CON_of_6, "of 6"),
    k("M5_group_x_board: CON VIF", dm[model == "M5_group_x_board"]$CON_vif),
    k("M5b_group_x_board_x_CC: aliased columns", dm[model == "M5b_group_x_board_x_CC"]$n_aliased, "group x board x CC not identifiable"),
    k("M9c_board_x_rec_CON_last: CON aliased", as.integer(dm[model == "M9c_board_x_rec_CON_last"]$CON_status == "ALIASED"), "1 = aliased"),
    k("SIS-only recording FE + board: rank", so[model == "board"]$rank),
    k("SIS-only recording FE + board: residual df", so[model == "board"]$resid_df),
    k("SIS-only recording FE + board: kappa (scaled)", so[model == "board"]$kappa_scaled),
    k("two-stage (SIS-only board): CON-equivalent n, all CON", ts[sis_model == "board" & CON_subset == "all"]$CON_equiv_n, "of 24"),
    k("two-stage (SIS-only board): CON-equivalent n, CC1", ts[sis_model == "board" & CON_subset == "CC1"]$CON_equiv_n, "of 6"),
    k("two-stage (SIS-only board x CC): CON-equivalent n, all CON", ts[sis_model == "board_x_CC" & CON_subset == "all"]$CON_equiv_n, "of 24"),
    k("r(log_xt, boards 3/4)", hw[covariate == "log_xt"]$r_b34, "log crosstalk covariate"),
    k("R2 of log_xt on board", hw[covariate == "log_xt"]$R2_board),
    k("recordings in which boards 3 and 4 have the two lowest crosstalk", extra$score[["recordings_boards34_two_lowest_xt"]], "of 24"),
    k("sum over CON cage-episodes of the log board-constant crosstalk score (SIS median)", extra$score[["CON_sum_log_board_score"]]),
    k("the same sum expected under uniform within-recording placement", extra$score[["expected_sum_uniform_within_recording"]]),
    k("partial r(CON, log_xt | board + recording)", pc[x == "CON" & y == "log_xt" & controls == "boardf + rec" & subset == "all"]$r),
    k("partial r(CON, log_xt | board + recording + tracked animals)", pc[x == "CON" & y == "log_xt" & controls == "boardf + rec + n_animals"]$r),
    k("GLS effective CON n at rho 0.5, CON + CC + Batch", gl[rhs == "CON + CC + Batch" & rho_cage == 0.5]$eff_n_CON, "of 24"),
    k("GLS effective CON n at rho 0.5, + board", gl[rhs == "CON + CC + Batch + boardf" & rho_cage == 0.5]$eff_n_CON, "of 24"),
    k("r(SUS, boards 3/4), SIS animal-episodes", tb$res_sus_correlations[stratum == "all"]$r_SUS_b34),
    k("r(SUS, boards 3/4), centred within recording", extra$r_sus_within),
    k("R1 SUS + board: efficiency vs no board", rs[startsWith(model, "R1 ")]$rel_eff_vs_R0),
    k("three-group model with cage-episode FE (group last): group term aliased",
      as.integer(tb$three_group_aliasing[rhs == "ce_f + grp"]$group_term_aliased), "1 = aliased"))
}

# ------------------------------------------------------------------ main

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages(library(data.table))
  data.table::setDTthreads(1L)
  source("Analysis/_pipeline_setup.R")
  source_mmm_helper("project_paths.R")
  if (!requireNamespace("car", quietly = TRUE)) stop("Package car is needed for the GVIF table.", call. = FALSE)
  a <- idf_args(commandArgs(trailingOnly = TRUE))
  started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  project_root <- mmm_project_root()
  behavior <- dirname(project_root)
  head_commit <- idf_git(c("rev-parse", "HEAD"))[1]
  if (!grepl("^[0-9a-f]{40}$", head_commit)) stop("Could not read the git commit: ", head_commit, call. = FALSE)
  run_id <- if (is.na(a$run_id)) paste0(IDF_VERSION, "_", substr(head_commit, 1, 7)) else a$run_id
  if (!grepl("^[A-Za-z0-9][A-Za-z0-9._-]{0,39}$", run_id)) stop("Bad run id: ", run_id, call. = FALSE)
  base <- mmm_isolated_output_root_check(if (is.na(a$output_root)) file.path(behavior, "RANM", "board_identifiability") else a$output_root,
                                         project_root)
  out_dir <- file.path(base, "runs", run_id); tmp <- file.path(base, "runs", paste0(".tmp_", run_id))
  if (file.exists(out_dir)) stop("Run folder exists; choose a new --run-id: ", out_dir, call. = FALSE)
  if (file.exists(tmp)) stop("A partial run folder exists; inspect and remove it first: ", tmp, call. = FALSE)
  mmm_assert_output_path_budget(c(file.path(tmp, names(IDF_FILE_DOCS)), file.path(out_dir, names(IDF_FILE_DOCS))), source_label = "idf audit")
  script_status <- idf_git(c("status", "--porcelain", "--untracked-files=all", "--", IDF_SCRIPT))
  script_status <- if (!length(script_status)) "committed, clean" else if (startsWith(script_status[1], "??")) "untracked" else "modified"

  qc_dir <- file.path(behavior, "RANM", "raw_acquisition_qc", "runs", IDF_QC_RUN, "tables")
  inputs <- data.table(role = c("animal_qc", "reader_qc", "board_qc", "board_history", "labels"),
                       path = c(file.path(qc_dir, c("animal_qc.csv", "reader_qc.csv", "board_qc.csv", "board_history.csv")),
                                file.path(project_root, "analysis_ready", "canonical", "later_outcome_combz", "tables",
                                          "later_outcome_combz_animal_level.csv")))
  miss <- inputs$path[!file.exists(inputs$path)]
  if (length(miss)) stop("Missing input(s):\n  ", paste(miss, collapse = "\n  "), call. = FALSE)
  inputs[, sha256 := mmm_file_sha256(path)]
  inp <- idf_read_inputs(stats::setNames(as.list(inputs$path), inputs$role))

  bld <- idf_build(inp)
  d <- bld$d
  al <- idf_allocation(d, bld$ae)
  co <- idf_correlations(d)
  idn <- idf_identifiability(d)
  so <- idf_sis_only(d)
  hw <- idf_hardware(d)
  gls <- idf_gls(d)
  rsx <- idf_res_sus(bld$ae, d)

  cage_cols <- c("Batch", "CC", "rec", "board", "sex", "cage_type", "cage_id", "n_animals", "n_CON", "n_SIS", "n_RES", "n_SUS", "animals",
                 "tags_present", "readers", "dead_channels", "dropout", "xt_med", "xt_med_all", "log_xt", "xt_rank_in_recording",
                 "rr_med", "best_med", "mean_gap_ms", "lattice_k1", "z_gap", "z_k1", "silent_share")
  tabs <- list(cage_episodes = d[, ..cage_cols], allocation_batch_cc = al$allocation_batch_cc, allocation_null = al$allocation_null,
               board_counts = al$board_counts, cor_CON_board = co$cor_CON_board)
  tabs <- c(tabs, idn, so, hw[setdiff(names(hw), "score")], gls, rsx[setdiff(names(rsx), "r_within")],
            list(sis_cage_res_sus_mix = al$sis_cage_res_sus_mix, input_checks = bld$checks))
  tabs$key_numbers <- idf_key_numbers(tabs, list(r_within_recording = co$r_within_recording, r_sus_within = rsx$r_within, score = hw$score))
  documented <- sub("^tables/(.*)[.]csv$", "\\1", grep("^tables/", names(IDF_FILE_DOCS), value = TRUE))
  if (!setequal(names(tabs), documented))
    stop("Tables and README entries differ: ", paste(c(setdiff(names(tabs), documented), setdiff(documented, names(tabs))), collapse = " "), call. = FALSE)

  dir.create(file.path(tmp, "tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(tmp, "manifest"), showWarnings = FALSE)
  for (nm in documented) fwrite(tabs[[nm]], file.path(tmp, "tables", paste0(nm, ".csv")))
  run <- data.table(
    item = c("run_id", "version", "status", "git_commit", "script", "script_sha256", "script_git_status", "qc_run",
             paste0("input_", inputs$role), paste0("input_", inputs$role, "_sha256"), "label_columns_read", "project_root",
             "output_root", "started", "finished", "R", "data.table", "car"),
    value = c(run_id, IDF_VERSION, "UNREGISTERED design audit; design only: no outcome read, no group effect estimated", head_commit,
              IDF_SCRIPT, mmm_file_sha256(file.path(MMM_REPO_ROOT, IDF_SCRIPT)), script_status, IDF_QC_RUN,
              inputs$path, inputs$sha256, paste(IDF_LABEL_COLUMNS, collapse = " "), project_root, base, started,
              format(Sys.time(), "%Y-%m-%d %H:%M:%S"), R.version.string, as.character(utils::packageVersion("data.table")),
              as.character(utils::packageVersion("car"))))
  fwrite(run, file.path(tmp, "manifest", "run.csv"))
  w <- max(nchar(names(IDF_FILE_DOCS)))
  writeLines(c("BOARD x GROUP IDENTIFIABILITY OF THE EXP9 RFID CAGE-EPISODE DESIGN (UNREGISTERED design audit)", "",
               paste("run:", run_id, "| commit:", head_commit, "| script:", IDF_SCRIPT, paste0("(", script_status, ")")),
               paste("allocation and hardware covariates: Stage 00b QC run", IDF_QC_RUN, "| group labels: later_outcome_combz_animal_level.csv"), "",
               "DESIGN ONLY: no behavioural metric, CombZ or other outcome was read and no group effect was estimated. Of the label",
               "table only AnimalNum, outcome_group and Sex were read. Unit: the cage-episode (Batch x CC x board, 120). Definitions:",
               "the header of the script. Inputs with SHA-256: manifest/run.csv. Headline numbers: tables/key_numbers.csv.", "",
               "Files:", paste(formatC(names(IDF_FILE_DOCS), width = -w), IDF_FILE_DOCS, sep = "  ")),
             file.path(tmp, "README.txt"))
  outs <- setdiff(list.files(tmp, recursive = TRUE), "manifest/output_files_sha256.csv")
  fwrite(data.table(file = outs, sha256 = mmm_file_sha256(file.path(tmp, outs)), bytes = file.size(file.path(tmp, outs))),
         file.path(tmp, "manifest", "output_files_sha256.csv"))
  if (!setequal(list.files(tmp, recursive = TRUE), names(IDF_FILE_DOCS))) stop("Unexpected file set in ", tmp, call. = FALSE)
  if (file.exists(out_dir)) stop("Run folder appeared during the run: ", out_dir, call. = FALSE)
  if (!file.rename(tmp, out_dir)) stop("Could not rename ", tmp, call. = FALSE)
  print(tabs$key_numbers, digits = 4)
  message("board identifiability audit done: ", out_dir)
}
