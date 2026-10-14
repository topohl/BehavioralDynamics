# Technical board adjustment of read-dependent RFID measures: identifiability, board offsets from SIS cages within
# Batch x CC, and adjusted against unadjusted contrasts (UNREGISTERED; isolated root; descriptive; no p-values).
#
# Tobias's decision of 2026-10-14: for read-dependent outcomes, evaluate a technical board adjustment estimated from SIS
# cages within Batch x CC where the design identifies it; board main effects only; no group x board (x CC) terms; cage
# change kept as an experimental factor; adjusted and unadjusted contrasts with their uncertainty and cage-level
# sensitivity; the six CON cages are the limiting independent replication for CON comparisons. Occupancy fractions are
# NOT adjusted (a board offset cannot recover unobserved positions). The registered Stage 29 models are not touched.
#
# Read-dependent outcomes, per animal x CC x phase over the whole span (single-read reconstruction srs_v1.1):
#   reads_per_h       observations (single reads after tie collapse) per observed hour
#   supported60_share share of observed time with a read <= 60 s old
#   colchg_per_h      changes of column (read basis, flip filter 2 s) per observed hour (descriptive; not locomotion)
# Cage-episode value = mean of its animals. The three dead-channel cage episodes (manifest v1.1 SX01-SX03) are left out;
# the censoring (SX04) is already in the observed time.
#
# Contrasts (estimates with 95% intervals; no tests):
#   CON - SIS   per recording: the CON cage minus the mean of its SIS cages; per CC the mean over the batches (t interval);
#               pooled: per batch the mean over CC (the CON cage is persistent), then the mean over the 6 batches (t, df 5).
#               Adjusted: board offsets subtracted; their uncertainty added exactly (a' V a). One-stage check:
#               lm(y ~ recording + board + CON) (assumes the same board effect in CON and SIS cages) with batch-clustered CR1
#               standard errors (the CON cage persists within a batch). Leave-one-batch-out: for the adjusted version the
#               offsets are re-estimated without the batch.
#   SUS - RES   SIS animals: unadjusted, board-adjusted (offsets' uncertainty added), and within-cage (cage-episode fixed
#               effects; board-free by construction, mixed cages only); per CC with cage-clustered (CR1) standard errors.
#   Sensitivity leave-one-batch-out ranges; cage episodes in which 4 animals were housed, read and tracked (presence
#               table: the tracked-tag count is not presence; Testing/audits/audit_cage_episode_presence.R); recordings
#               without a CON or SIS cage in that population are left out and listed in tables/dropped_recordings.csv.
#
# Run from the repository root (light):
#   Rscript Testing/audits/audit_board_adjustment_read_dependent.R --srs-run=<srs run folder> --presence=<csv> [--run-id=<id>]
#           [--output-root=<absolute>]
# presence csv (audit_cage_episode_presence.R, tables/cage_episode_presence.csv): CageEpisodeID, n_animals_present (housed,
# from detection-independent records), n_tags_present (tags with >= 100 in-span reads: the read-competition count),
# n_tracked. Default output root <Behavior>/RANM/board_adjustment (isolated; mmm_isolated_output_root_check()).
# Reads the group labels (AnimalNum, outcome_group, Sex) for the contrasts; no CombZ or other outcome.

BADJ_OUTCOMES <- c("reads_per_h", "supported60_share", "colchg_per_h")

badj_args <- function(args) {
  known <- "^--(srs-run|presence|run-id|output-root)="
  if (any(!grepl(known, args))) stop("Unknown argument(s): ", paste(args[!grepl(known, args)], collapse = " "), call. = FALSE)
  val <- function(k) { a <- args[startsWith(args, paste0("--", k, "="))]; if (length(a)) sub("^[^=]*=", "", a) else NA_character_ }
  list(srs_run = val("srs-run"), presence = val("presence"), run_id = val("run-id"), output_root = val("output-root"))
}

#' Animal x CC x phase outcomes from a srs run's tables (animal_block, occupancy), with the cage-episode keys.
badj_animal_outcomes <- function(animal_block, occupancy) {
  ab <- data.table::as.data.table(animal_block)
  oc <- data.table::as.data.table(occupancy)[basis == "read", .(sup60 = sum(sup_60)), by = .(SourceFile, AnimalNum, block)]
  ab <- merge(ab, oc, by = c("SourceFile", "AnimalNum", "block"), all.x = TRUE)
  ab[is.na(sup60), sup60 := 0]
  ab[observed_s > 0, .(observed_h = sum(observed_s) / 3600, observations = sum(observations), sup60 = sum(sup60),
                       colchg = sum(changes_read_column)), by = .(Batch, CC, board, System, CageEpisodeID, AnimalNum, Phase)][
    , `:=`(reads_per_h = observations / observed_h, supported60_share = sup60 / (observed_h * 3600), colchg_per_h = colchg / observed_h)][]
}

#' Board offsets from SIS cage episodes: lm(y ~ recording + board), board 1 the reference. Stops if the design does not
#' identify the board terms. Returns list(coef (board 1-5, board 1 = 0), vcov (boards 2-5), rank, p, df).
badj_board_offsets <- function(ce_sis) {
  d <- data.table::copy(data.table::as.data.table(ce_sis))
  d[, `:=`(rec = factor(paste(Batch, CC)), boardf = factor(board, levels = 1:5))]
  X <- stats::model.matrix(~ rec + boardf, d)
  qx <- qr(X)
  if (qx$rank < ncol(X)) stop("Board offsets are not identified by the SIS design (rank ", qx$rank, " of ", ncol(X), ").", call. = FALSE)
  fit <- stats::lm(y ~ rec + boardf, d)
  bn <- paste0("boardf", 2:5)
  list(coef = c(`1` = 0, stats::setNames(stats::coef(fit)[bn], 2:5)), vcov = stats::vcov(fit)[bn, bn], rank = qx$rank, p = ncol(X),
       df = fit$df.residual, sigma = summary(fit)$sigma)
}

#' t interval of a mean.
badj_t <- function(x, extra_var = 0) {
  x <- x[is.finite(x)]; n <- length(x)
  if (n < 2) return(data.table::data.table(n = n, estimate = if (n) mean(x) else NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_))
  se <- sqrt(stats::var(x) / n + extra_var); q <- stats::qt(0.975, n - 1)
  data.table::data.table(n = n, estimate = mean(x), se = se, lower = mean(x) - q * se, upper = mean(x) + q * se)
}

#' Cluster-robust (CR1) covariance of an lm fit. k_small: the number of coefficients in the small-sample factor (all of
#' them by default; for fixed effects nested in the clusters only the others, as in the usual within-estimator convention).
badj_cr1 <- function(fit, cluster, k_small = length(stats::coef(fit))) {
  X <- stats::model.matrix(fit); e <- stats::residuals(fit)
  XtXi <- solve(crossprod(X))
  meat <- Reduce(`+`, lapply(split(seq_len(nrow(X)), cluster), function(i) { s <- crossprod(X[i, , drop = FALSE], e[i]); s %*% t(s) }))
  g <- length(unique(cluster)); n <- nrow(X)
  XtXi %*% meat %*% XtXi * (g / (g - 1)) * ((n - 1) / (n - k_small))
}

#' CON - SIS contrasts from cage-episode values (columns Batch, CC, board, cage_type, y): per CC and pooled (per batch the
#' mean over its available CCs, then the mean over batches). A recording without a CON or a SIS cage in the population has
#' no contrast; it is left out and listed (`dropped`). With board offsets `off` (coef, vcov) the values are adjusted, and the
#' offsets' uncertainty is added exactly: a contrast is linear in the offsets, so its extra variance is a' V a for the board
#' weights a of the recordings that enter it.
badj_con_contrast <- function(ce, off = NULL) {
  d <- data.table::copy(data.table::as.data.table(ce))
  if (!is.null(off)) d[, y := y - off$coef[as.character(board)]]
  rec <- d[, .(has = any(cage_type == "CON") && any(cage_type == "SIS")), by = .(Batch, CC)]
  dropped <- rec[has == FALSE, .(Batch, CC)]
  d <- d[rec[has == TRUE, .(Batch, CC)], on = .(Batch, CC)]
  pr <- d[, .(d = mean(y[cage_type == "CON"]) - mean(y[cage_type == "SIS"])), by = .(Batch, CC)]
  # weight of each board offset (boards 2-5; board 1 is the reference) in each recording's contrast
  w <- d[, { v <- numeric(4); con <- board[cage_type == "CON"]; sis <- board[cage_type == "SIS"]
             for (b in con[con > 1]) v[b - 1] <- v[b - 1] + 1 / length(con)
             for (b in sis[sis > 1]) v[b - 1] <- v[b - 1] - 1 / length(sis)
             list(w2 = v[1], w3 = v[2], w4 = v[3], w5 = v[4]) }, by = .(Batch, CC)]
  wc <- c("w2", "w3", "w4", "w5")
  extra <- function(a) if (is.null(off)) 0 else as.numeric(t(a) %*% off$vcov %*% a)
  by_cc <- data.table::rbindlist(lapply(sort(unique(pr$CC)), function(cc)
    badj_t(pr[CC == cc]$d, extra(colMeans(as.matrix(w[CC == cc, ..wc]))))[, CC := cc]))
  wb <- w[, lapply(.SD, mean), by = Batch, .SDcols = wc]
  pooled <- badj_t(pr[, .(m = mean(d)), by = Batch]$m, extra(colMeans(as.matrix(wb[, ..wc]))))[, CC := "pooled (batch means over CC)"]
  list(contrasts = rbind(by_cc, pooled), per_recording = pr, board_weights = w, dropped = dropped)
}

#' Leave-one-batch-out estimates of the pooled CON - SIS contrast; with adjust = TRUE the board offsets are re-estimated
#' without the left-out batch (its SIS cages no longer shape the adjustment).
badj_con_lobo <- function(ce, adjust = FALSE) {
  data.table::rbindlist(lapply(sort(unique(ce$Batch)), function(b) {
    x <- ce[Batch != b]
    off <- if (adjust) badj_board_offsets(x[cage_type == "SIS"]) else NULL
    data.table::data.table(left_out = b, estimate = badj_con_contrast(x, off)$contrasts[CC == "pooled (batch means over CC)"]$estimate)
  }))
}

#' SUS - RES among SIS animals of one CC (columns y, SUS (0/1), CageEpisodeID, board): unadjusted, board-adjusted (pass
#' `off`: y minus the board offsets, whose uncertainty a' V a is added, a = board shares of SUS animals minus those of RES
#' animals) or within-cage (cage-episode fixed effects, mixed cages only; board-free by construction). Cage-clustered CR1
#' standard errors; for the within-cage model the small-sample factor counts the non-fixed-effect coefficients only.
badj_sus_contrast <- function(an, within_cage = FALSE, off = NULL) {
  d <- data.table::copy(data.table::as.data.table(an))[is.finite(y)]
  if (!is.null(off)) d[, y := y - off$coef[as.character(board)]]
  if (within_cage) {
    mixed <- d[, .(m = data.table::uniqueN(SUS)), by = CageEpisodeID][m == 2]$CageEpisodeID
    d <- d[CageEpisodeID %in% mixed]
  }
  if (data.table::uniqueN(d$SUS) < 2 || nrow(d) < 4) return(data.table::data.table(n_animals = nrow(d), n_cages = data.table::uniqueN(d$CageEpisodeID),
                                                                                    estimate = NA_real_, se = NA_real_, lower = NA_real_, upper = NA_real_))
  f <- if (within_cage) y ~ SUS + factor(CageEpisodeID) else y ~ SUS
  fit <- stats::lm(f, d)
  V <- badj_cr1(fit, d$CageEpisodeID, k_small = if (within_cage) 2L else length(stats::coef(fit)))
  extra <- 0
  if (!is.null(off)) {
    a <- vapply(2:5, function(b) mean(d$board[d$SUS == 1] == b) - mean(d$board[d$SUS == 0] == b), numeric(1))
    extra <- as.numeric(t(a) %*% off$vcov %*% a)
  }
  g <- data.table::uniqueN(d$CageEpisodeID)
  est <- stats::coef(fit)[["SUS"]]; se <- sqrt(V["SUS", "SUS"] + extra); q <- stats::qt(0.975, g - 1)
  data.table::data.table(n_animals = nrow(d), n_cages = g, estimate = est, se = se, lower = est - q * se, upper = est + q * se)
}

if (sys.nframe() == 0L) {
  suppressPackageStartupMessages(library(data.table))
  data.table::setDTthreads(1L)
  source("Analysis/_pipeline_setup.R")
  for (h in c("project_paths.R", "spatial_exclusion_manifest.R")) source_mmm_helper(h)
  a <- badj_args(commandArgs(trailingOnly = TRUE))
  if (is.na(a$srs_run) || is.na(a$presence)) stop("--srs-run and --presence are required.", call. = FALSE)
  project_root <- mmm_project_root()
  head_commit <- suppressWarnings(system2("git", c("-C", MMM_REPO_ROOT, "rev-parse", "HEAD"), stdout = TRUE))[1]
  run_id <- if (is.na(a$run_id)) paste0("badj_v1_", substr(head_commit, 1, 7)) else a$run_id
  base <- mmm_isolated_output_root_check(if (is.na(a$output_root)) file.path(dirname(project_root), "RANM", "board_adjustment") else a$output_root, project_root)
  out <- file.path(base, "runs", run_id); tmp <- file.path(base, "runs", paste0(".tmp_", run_id))
  if (file.exists(out) || file.exists(tmp)) stop("Run folder exists; choose a new --run-id: ", out, call. = FALSE)
  man <- sxm_read("v1.1")
  T <- file.path(a$srs_run, "tables")
  ab <- fread(file.path(T, "animal_block.csv"), colClasses = c(AnimalNum = "character"))
  oc <- fread(file.path(T, "occupancy.csv"), colClasses = c(AnimalNum = "character"))
  pres <- fread(a$presence)
  fl <- file.path(project_root, "analysis_ready", "canonical", "later_outcome_combz", "tables", "later_outcome_combz_animal_level.csv")
  lab <- fread(fl, select = c("AnimalNum", "outcome_group", "Sex"), colClasses = c(AnimalNum = "character"))
  lab[, AnimalNum := canonical_animal_id(AnimalNum)]
  inputs <- data.table(input = c(file.path(T, c("animal_block.csv", "occupancy.csv")), a$presence, fl, sxm_path("v1.1")))
  inputs[, sha256 := mmm_file_sha256(input)]

  an <- badj_animal_outcomes(ab, oc)
  an <- an[!sxm_spatial_excluded(man, Batch, CC, board)]
  an <- merge(an, lab, by = "AnimalNum", all.x = TRUE)
  if (anyNA(an$outcome_group)) stop("Animals without a group label: ", paste(unique(an[is.na(outcome_group)]$AnimalNum), collapse = ", "), call. = FALSE)
  an[, cage_type := ifelse(outcome_group == "CON", "CON", "SIS")]
  bad <- an[, .(types = uniqueN(cage_type)), by = CageEpisodeID][types > 1]
  if (nrow(bad)) stop("Cage episodes with CON and SIS animals: ", paste(bad$CageEpisodeID, collapse = ", "), call. = FALSE)
  an <- merge(an, pres[, .(CageEpisodeID, n_animals_present, n_tags_present, n_tracked)], by = "CageEpisodeID", all.x = TRUE)
  if (anyNA(an$n_animals_present)) stop("Cage episodes missing from the presence table.", call. = FALSE)

  res <- list(); offs <- list(); ident <- list(); dropped <- list()
  for (o in BADJ_OUTCOMES) for (ph in c("Active", "Inactive")) {
    a1 <- an[Phase == ph][, y := get(o)]
    ce <- a1[, .(y = mean(y), animals = .N, n_animals_present = n_animals_present[1], n_tags_present = n_tags_present[1], n_tracked = n_tracked[1]),
             by = .(Batch, CC, board, CageEpisodeID, cage_type)]
    off <- badj_board_offsets(ce[cage_type == "SIS"])
    ident[[length(ident) + 1L]] <- data.table(outcome = o, Phase = ph, sis_cage_episodes = nrow(ce[cage_type == "SIS"]), design_rank = off$rank,
                                              design_columns = off$p, residual_df = off$df, residual_sd = off$sigma)
    offs[[length(offs) + 1L]] <- data.table(outcome = o, Phase = ph, board = 1:5, offset = off$coef[as.character(1:5)],
                                            se = c(0, sqrt(diag(off$vcov))))
    for (adj in c("unadjusted", "board-adjusted")) {
      cc <- badj_con_contrast(ce, if (adj == "board-adjusted") off else NULL)
      res[[length(res) + 1L]] <- cc$contrasts[, `:=`(contrast = "CON - SIS", outcome = o, Phase = ph, version = adj, population = "all cage episodes")]
      lo <- badj_con_lobo(ce, adjust = adj == "board-adjusted")
      res[[length(res) + 1L]] <- data.table(n = nrow(lo), estimate = NA_real_, se = NA_real_, lower = min(lo$estimate), upper = max(lo$estimate),
                                            CC = "pooled, leave-one-batch-out range", contrast = "CON - SIS", outcome = o, Phase = ph, version = adj,
                                            population = "all cage episodes")
      ce4 <- ce[n_animals_present == 4 & n_tags_present == 4 & n_tracked == 4]
      if (nrow(ce4[cage_type == "CON"]) && nrow(ce4[cage_type == "SIS"])) {
        cc4 <- badj_con_contrast(ce4, if (adj == "board-adjusted") off else NULL)
        res[[length(res) + 1L]] <- cc4$contrasts[, `:=`(contrast = "CON - SIS", outcome = o, Phase = ph, version = adj,
                                                        population = "cage episodes with 4 animals housed, read and tracked")]
        if (adj == "unadjusted" && nrow(cc4$dropped))
          dropped[[length(dropped) + 1L]] <- cc4$dropped[, `:=`(outcome = o, Phase = ph, population = "cage episodes with 4 animals housed, read and tracked",
                                                                reason = "no CON or no SIS cage episode in the population")]
      }
    }
    one <- stats::lm(y ~ factor(paste(Batch, CC)) + factor(board) + I(cage_type == "CON"), ce)
    nm <- "I(cage_type == \"CON\")TRUE"
    Vb <- badj_cr1(one, ce$Batch)   # the CON cage persists within a batch: batches are the clusters
    est1 <- stats::coef(one)[[nm]]; se1 <- sqrt(Vb[nm, nm]); q1 <- stats::qt(0.975, data.table::uniqueN(ce$Batch) - 1)
    res[[length(res) + 1L]] <- data.table(n = nrow(ce), estimate = est1, se = se1, lower = est1 - q1 * se1, upper = est1 + q1 * se1,
                                          CC = "pooled, one-stage lm(y ~ recording + board + CON), batch-clustered CR1", contrast = "CON - SIS", outcome = o,
                                          Phase = ph, version = "board-adjusted (one-stage; same board effect in CON and SIS)", population = "all cage episodes")
    sis <- a1[cage_type == "SIS"][, SUS := as.integer(outcome_group == "SUS")]
    for (cc in c(sort(unique(sis$CC)))) {
      x <- sis[CC == cc]
      for (v in c("unadjusted", "board-adjusted", "within-cage")) {
        r <- badj_sus_contrast(x[, .(y, SUS, CageEpisodeID, board)], within_cage = v == "within-cage", off = if (v == "board-adjusted") off else NULL)
        res[[length(res) + 1L]] <- r[, `:=`(n = n_animals, CC = cc, contrast = "SUS - RES", outcome = o, Phase = ph, version = v,
                                            population = "SIS animals")][, c("n_animals") := NULL]
      }
    }
  }
  contrasts <- rbindlist(res, fill = TRUE)
  dir.create(file.path(tmp, "tables"), recursive = TRUE); dir.create(file.path(tmp, "manifest"))
  fwrite(contrasts[, .(contrast, outcome, Phase, CC, version, population, n, n_cages, estimate, se, lower, upper)], file.path(tmp, "tables", "contrasts.csv"))
  fwrite(rbindlist(offs), file.path(tmp, "tables", "board_offsets.csv"))
  fwrite(if (length(dropped)) rbindlist(dropped) else data.table(Batch = character(), CC = character()), file.path(tmp, "tables", "dropped_recordings.csv"))
  fwrite(rbindlist(ident), file.path(tmp, "tables", "identifiability.csv"))
  fwrite(an[, .(cage_episodes = uniqueN(CageEpisodeID), animals = uniqueN(AnimalNum)), by = .(Phase, cage_type)], file.path(tmp, "tables", "population.csv"))
  fwrite(unique(an[, .(CageEpisodeID, Batch, CC, board, cage_type, n_animals_present, n_tags_present, n_tracked)]), file.path(tmp, "tables", "cage_episodes.csv"))
  fwrite(inputs, file.path(tmp, "manifest", "input_hashes.csv"))
  fwrite(data.table(item = c("run_id", "git_commit", "srs_run", "R"), value = c(run_id, head_commit, a$srs_run, R.version.string)),
         file.path(tmp, "manifest", "run.csv"))
  writeLines(c("TECHNICAL BOARD ADJUSTMENT OF READ-DEPENDENT RFID MEASURES (unregistered; descriptive; no p-values)", "",
               "Board offsets are estimated from SIS cage episodes within Batch x CC (recording fixed effects + board main effect).",
               "Adjusted contrasts subtract them and add their uncertainty exactly (a' V a). Occupancy fractions are not adjusted.",
               "The CON - SIS contrast rests on the six persistent CON cages (pooled: batch means over CC, t interval with 5 df).",
               "Not a registered or confirmatory analysis; the registered Stage 29 models are unchanged.",
               "Files: tables/contrasts.csv, board_offsets.csv, identifiability.csv, population.csv, cage_episodes.csv; manifest/."),
             file.path(tmp, "README.txt"))
  outs <- list.files(tmp, recursive = TRUE)
  fwrite(data.table(file = outs, sha256 = mmm_file_sha256(file.path(tmp, outs))), file.path(tmp, "manifest", "output_files_sha256.csv"))
  if (!file.rename(tmp, out)) stop("Could not rename ", tmp, call. = FALSE)
  message("board adjustment audit done: ", out)
}
