# ================================================================
# Vectorised carry-forward interval reconstruction (Stages 01 and 02)
# MMMSociability -- Functions/interval_reconstruction.R
# ================================================================
# Pure definitions (dplyr/tibble attached by the caller).
#
# Stage 01 (occupancy and dyadic intervals) and Stage 02 (dyadic intervals) used per-event loops: for every distinct
# event time of a system they filtered the system table for the rows at that time, updated each animal's current
# position, and re-filtered the table for the latest row (the interval metadata). That is quadratic in the number of
# events and took hours on vendor data; the raw-antenna data versions are 5-8x denser. These functions return the same
# rows in the same order with the same values, built with findInterval() instead:
#   - intervals run between consecutive distinct event times of the system (the last time opens no interval);
#   - in an interval, an animal holds the PositionID of its latest row at or before the interval start (rows of one
#     animal at the same time: the last in arrange(DateTime, AnimalID) order wins, as in the loop);
#   - labels (Group, Sex) hold the animal's latest non-missing value so far;
#   - the interval metadata are those of the system's latest row at or before the interval start;
#   - occupancy rows: animals whose held position is finite and > 0, in sort(unique(AnimalID)) order;
#   - dyadic rows: pairs in combn(sort(unique(AnimalID)), 2) order whose held positions are both finite.
# Equivalence with the loops is tested in Testing/tests/test_interval_reconstruction.R (reference copies of the loops,
# boundary and zero-event cases) and was checked on the full vendor data by a sandbox rerun of Stages 01/02.
# ================================================================

#' The latest non-missing value so far, as the loops kept labels (assignments skipped NA).
mmm_ir_carry_label <- function(x) {
  last <- cummax(seq_along(x) * !is.na(x))
  out <- x[0][rep(NA_integer_, length(x))]          # NA of the same type
  out[last > 0] <- x[last[last > 0]]
  out
}

#' Shared skeleton: arranged system table, animals, interval bounds and each animal's latest row per interval.
mmm_ir_state <- function(dat_sys, min_animals) {
  dat_sys <- dat_sys %>% dplyr::arrange(DateTime, AnimalID)
  animals <- sort(unique(dat_sys$AnimalID))
  event_times <- sort(unique(dat_sys$DateTime))
  if (length(animals) < min_animals || length(event_times) < 2) return(NULL)
  t0 <- event_times[-length(event_times)]
  t1 <- event_times[-1]
  t_all <- as.numeric(dat_sys$DateTime)
  t0n <- as.numeric(t0)
  rows_of <- lapply(animals, function(a) which(dat_sys$AnimalID == a))
  latest <- vapply(rows_of, function(r) {
    k <- findInterval(t0n, t_all[r])
    ifelse(k > 0, r[pmax(k, 1L)], NA_integer_)          # row index in dat_sys, NA before the animal's first row
  }, integer(length(t0n)))
  if (is.null(dim(latest))) latest <- matrix(latest, nrow = length(t0n))
  list(dat = dat_sys, animals = animals, rows_of = rows_of, t0 = t0, t1 = t1,
       latest = latest, meta_row = findInterval(t0n, t_all))
}

#' Label held by each animal in each interval (latest non-missing value so far), as a matrix like st$latest.
mmm_ir_labels <- function(st, col) {
  carried <- rep(st$dat[[col]][0][NA_integer_], nrow(st$dat))
  for (r in st$rows_of) carried[r] <- mmm_ir_carry_label(st$dat[[col]][r])
  m <- matrix(carried[st$latest], nrow = nrow(st$latest))
  m
}

#' Occupancy intervals of one system: one row per interval x animal with a valid held position.
#' Columns: AnimalNum, AnimalID, PositionID, <label_cols>, IntervalStart, IntervalEnd, DurationSec, SystemEventGap,
#' then meta_cols.
mmm_occupancy_intervals_vec <- function(dat_sys, meta_cols, gap_threshold_sec, label_cols = c("Group", "Sex")) {
  st <- mmm_ir_state(dat_sys, min_animals = 1)
  if (is.null(st)) return(tibble::tibble())
  pos <- matrix(st$dat$PositionID[st$latest], nrow = nrow(st$latest))
  valid <- is.finite(pos) & pos > 0
  valid[is.na(valid)] <- FALSE
  if (!any(valid)) return(tibble::tibble())
  ij <- which(t(valid), arr.ind = TRUE)                  # interval-major order: by interval, then animal rank
  k <- ij[, 1]; i <- ij[, 2]
  duration <- as.numeric(difftime(st$t1[i], st$t0[i], units = "secs"))
  out <- tibble::tibble(AnimalNum = st$animals[k], AnimalID = st$animals[k],
                        PositionID = as.integer(pos[cbind(i, k)]))
  for (lc in label_cols) out[[lc]] <- mmm_ir_labels(st, lc)[cbind(i, k)]
  out$IntervalStart <- st$t0[i]
  out$IntervalEnd <- st$t1[i]
  out$DurationSec <- duration
  out$SystemEventGap <- duration > gap_threshold_sec
  dplyr::bind_cols(out, st$dat[st$meta_row[i], meta_cols])
}

#' Dyadic intervals of one system: one row per interval x animal pair whose held positions are both finite.
#' Columns: Focal, Partner, PositionID_A, PositionID_B, <label>_Focal, <label>_Partner for each label column,
#' IntervalStart, IntervalEnd, DurationSec, SystemEventGap, meta_cols, then the columns of pair_distance_lookup
#' (left join on PositionID_A, PositionID_B).
mmm_dyad_intervals_vec <- function(dat_sys, meta_cols, gap_threshold_sec, pair_distance_lookup, label_cols = character()) {
  st <- mmm_ir_state(dat_sys, min_animals = 2)
  if (is.null(st)) return(tibble::tibble())
  pos <- matrix(st$dat$PositionID[st$latest], nrow = nrow(st$latest))
  ok <- is.finite(pos)
  ok[is.na(ok)] <- FALSE
  pairs <- utils::combn(seq_along(st$animals), 2)
  parts <- lapply(seq_len(ncol(pairs)), function(p) {
    i <- which(ok[, pairs[1, p]] & ok[, pairs[2, p]])
    if (!length(i)) return(NULL)
    data.frame(i = i, pair = p)
  })
  idx <- do.call(rbind, parts)
  if (is.null(idx) || !nrow(idx)) return(tibble::tibble())
  idx <- idx[order(idx$i, idx$pair), , drop = FALSE]
  i <- idx$i; a <- pairs[1, idx$pair]; b <- pairs[2, idx$pair]
  out <- tibble::tibble(Focal = st$animals[a], Partner = st$animals[b],
                        PositionID_A = as.integer(pos[cbind(i, a)]), PositionID_B = as.integer(pos[cbind(i, b)]))
  for (lc in label_cols) {
    m <- mmm_ir_labels(st, lc)
    out[[paste0(lc, "_Focal")]] <- m[cbind(i, a)]
    out[[paste0(lc, "_Partner")]] <- m[cbind(i, b)]
  }
  duration <- as.numeric(difftime(st$t1[i], st$t0[i], units = "secs"))
  out$IntervalStart <- st$t0[i]
  out$IntervalEnd <- st$t1[i]
  out$DurationSec <- duration
  out$SystemEventGap <- duration > gap_threshold_sec
  out <- dplyr::bind_cols(out, st$dat[st$meta_row[i], meta_cols])
  dplyr::left_join(out, pair_distance_lookup, by = c("PositionID_A", "PositionID_B"))
}
