# Portable tests for the preprocessing helpers that separate measurement events
# from aggregation boundaries. Pure in-memory; no S: drive access required.

suppressPackageStartupMessages({library(dplyr); library(tibble)})
source("Functions/animalpos_preprocessing_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
op <- options(digits.secs = 6); on.exit(options(op), add = TRUE)

# ------------------------------------------------------------------
# 1. SUB-SECOND TIMESTAMP PRESERVATION
# ------------------------------------------------------------------
raw <- c("28.10.2022 15:44:45.508", "28.10.2022 15:44:45.912", "28.10.2022 15:44:46.001")
p <- parse_animalpos_datetime(raw)
check(all(!is.na(p)), "1: all timestamps must parse")
frac <- as.numeric(p) - floor(as.numeric(p))
check(all(abs(frac - c(0.508, 0.912, 0.001)) < 1e-3), "1: fractional seconds must be retained")

# Two genuine events in the same clock second but different milliseconds.
d <- as.numeric(diff(p))
check(all(d > 0), "1: strictly increasing timestamps must remain strictly increasing")
check(abs(d[1] - 0.404) < 1e-3, "1: sub-second spacing must be preserved (0.404 s)")
check(sum(d == 0) == 0, "1: no zero-dt collision may remain after parsing")

# The historical parser collapsed them.
old <- as.POSIXct(substr(raw, 1, 19), format = "%d.%m.%Y %H:%M:%S", tz = "UTC")
check(as.numeric(old[2] - old[1]) == 0, "1: the historical parser is expected to collapse these (documents the bug)")
check(length(unique(old[1:2])) == 1 && length(unique(p[1:2])) == 2,
      "1: fix must separate exactly what the old parser merged")

# Deterministic, order-preserving sort.
shuf <- p[c(3, 1, 2)]
check(identical(sort(shuf), p), "1: sorting must be deterministic and recover chronological order")

# ------------------------------------------------------------------
# 2. PHASE LABELLING (Active 18:30-06:30, Inactive 06:30-18:30)
# ------------------------------------------------------------------
tt <- function(s) as.POSIXct(s, tz = "UTC")
cases <- tribble(
  ~ts,                     ~expect,
  "2022-10-28 18:30:00",   "Active",
  "2022-10-28 23:59:59",   "Active",
  "2022-10-29 00:00:00",   "Active",
  "2022-10-29 06:29:59",   "Active",
  "2022-10-29 06:30:00",   "Inactive",
  "2022-10-29 12:00:00",   "Inactive",
  "2022-10-29 18:29:59",   "Inactive",
  "2022-10-29 18:30:00",   "Active"
)
got <- animalpos_phase_label(tt(cases$ts))
check(identical(got, cases$expect),
      paste0("2: phase labels wrong. got: ", paste(got, collapse = ","), " expected: ", paste(cases$expect, collapse = ",")))

# Boundaries are inclusive-left, matching the historical ">= 18:30 | < 06:30" rule.
b <- animalpos_phase_block_bounds(tt("2022-10-29 07:00:00"))
check(b$Phase == "Inactive", "2: 07:00 belongs to an Inactive block")
check(format(b$block_start, "%H:%M:%S") == "06:30:00", "2: Inactive block starts at 06:30")
check(format(b$block_end, "%H:%M:%S") == "18:30:00", "2: Inactive block ends at 18:30")

# ------------------------------------------------------------------
# 3. PHASE COUNTERS WITHOUT BOUNDARY ROWS
# ------------------------------------------------------------------
# Session starting mid-Inactive, then Active, Inactive, Active ...
seqs <- tt(c("2022-10-28 15:44:45",   # Inactive block 1
             "2022-10-28 18:31:00",   # Active   block 1
             "2022-10-29 03:00:00",   # Active   block 1
             "2022-10-29 07:00:00",   # Inactive block 2
             "2022-10-29 19:00:00",   # Active   block 2
             "2022-10-30 08:00:00"))  # Inactive block 3
pc <- animalpos_phase_counters(seqs)
check(identical(pc$Phase, c("Inactive","Active","Active","Inactive","Active","Inactive")), "3: phase sequence wrong")
check(identical(pc$ConsecInactive, c(1L,0L,0L,2L,0L,3L)), paste0("3: ConsecInactive wrong: ", paste(pc$ConsecInactive, collapse=",")))
check(identical(pc$ConsecActive,   c(0L,1L,1L,0L,2L,0L)), paste0("3: ConsecActive wrong: ", paste(pc$ConsecActive, collapse=",")))

# Counters must NOT depend on a row existing at the boundary: dropping the
# rows adjacent to a transition leaves the surviving rows' indices unchanged.
pc_sparse <- animalpos_phase_counters(seqs[c(1, 4, 6)])
check(identical(pc_sparse$ConsecInactive, c(1L, 2L, 3L)),
      "3: block numbering must come from the contiguous block sequence, not from observed transitions")

# Discriminating case: keep ONLY blocks k and k+4 (both Inactive), skipping the
# unobserved Inactive block between them. Contiguous numbering must still count
# the skipped block, giving 1 and 3 -- deriving the LUT from observed blocks
# alone would wrongly give 1 and 2.
pc_gap <- animalpos_phase_counters(seqs[c(1, 6)])
check(identical(pc_gap$ConsecInactive, c(1L, 3L)),
      paste0("3: unobserved phase blocks must still occupy their place in the numbering; got ",
             paste(pc_gap$ConsecInactive, collapse = ",")))

# ------------------------------------------------------------------
# 4. BOUNDARY INSTANT GENERATION (strictly interior)
# ------------------------------------------------------------------
bi <- animalpos_boundary_instants(tt("2022-10-29 06:22:00"), tt("2022-10-29 06:41:00"), 600)
check(length(bi) == 2, "4: expected 2 ten-minute boundaries strictly inside 06:22-06:41")
check(identical(format(bi, "%H:%M:%S"), c("06:30:00","06:40:00")), "4: wrong ten-minute boundaries")
check(length(animalpos_boundary_instants(tt("2022-10-29 06:30:00"), tt("2022-10-29 06:40:00"), 600)) == 0,
      "4: boundaries exactly at the interval endpoints must not be emitted")
pbi <- animalpos_phase_boundary_instants(tt("2022-10-29 06:22:00"), tt("2022-10-29 06:41:00"))
check(length(pbi) == 1 && format(pbi, "%H:%M:%S") == "06:30:00", "4: expected one phase boundary at 06:30")

# ------------------------------------------------------------------
# 5. INTERVAL SPLITTING -- the stated invariant
#    state position 5 from 06:22 to 06:41 must contribute
#    06:22-06:30 Active(prev block) and 06:30-06:41 Inactive
# ------------------------------------------------------------------
iv <- tibble(AnimalNum = "A", PositionID = 5L,
             IntervalStart = tt("2022-10-29 06:22:00"),
             IntervalEnd   = tt("2022-10-29 06:41:00"),
             DurationSec   = 19 * 60)
sp <- animalpos_split_intervals(iv, split_phase = TRUE)
check(nrow(sp) == 2, paste0("5: expected 2 phase pieces, got ", nrow(sp)))
check(identical(format(sp$IntervalStart, "%H:%M:%S"), c("06:22:00","06:30:00")), "5: wrong piece starts")
check(identical(format(sp$IntervalEnd,   "%H:%M:%S"), c("06:30:00","06:41:00")), "5: wrong piece ends")
check(identical(as.numeric(sp$DurationSec), c(8*60, 11*60)), "5: wrong piece durations")
check(sum(as.numeric(sp$DurationSec)) == 19*60, "5: durations must sum to the original interval")
check(all(sp$PositionID == 5L), "5: position must be carried unchanged into every piece")
check(identical(animalpos_phase_label(sp$IntervalStart), c("Active","Inactive")),
      "5: the two pieces must fall in Active then Inactive")

# Duration conservation over a long multi-boundary interval, every resolution.
iv2 <- tibble(AnimalNum = "A", PositionID = 3L,
              IntervalStart = tt("2022-10-29 05:00:00"),
              IntervalEnd   = tt("2022-10-30 09:00:00"),
              DurationSec   = 28 * 3600)
for (p_sec in c(10, 60, 300, 600, 1800)) {
  s <- animalpos_split_intervals(iv2, period_sec = p_sec, split_phase = TRUE, split_day = TRUE)
  check(abs(sum(as.numeric(s$DurationSec)) - 28*3600) < 1e-6,
        paste0("5: duration must be conserved at period ", p_sec))
  check(all(as.numeric(s$DurationSec) > 0), paste0("5: no zero-length pieces at period ", p_sec))
  check(all(diff(as.numeric(s$IntervalStart)) > 0), paste0("5: pieces must be ordered at period ", p_sec))
  # every piece lies wholly within one bin and one phase
  bin_of <- floor(as.numeric(s$IntervalStart) / p_sec)
  bin_end <- floor((as.numeric(s$IntervalEnd) - 1e-3) / p_sec)
  check(all(bin_of == bin_end), paste0("5: no piece may span a ", p_sec, "s boundary"))
  check(all(animalpos_phase_label(s$IntervalStart) ==
            animalpos_phase_label(as.POSIXct(as.numeric(s$IntervalEnd) - 1e-3, origin="1970-01-01", tz="UTC"))),
        paste0("5: no piece may span a phase boundary at period ", p_sec))
}

# No synthetic position event is created: piece count is boundaries+1 and every
# piece keeps the original PositionID.
check(all(animalpos_split_intervals(iv2, period_sec = 600)$PositionID == 3L),
      "5: splitting must never invent or alter a PositionID")

# ------------------------------------------------------------------
# 6. A BOUNDARY IS NOT AN OBSERVATION
# ------------------------------------------------------------------
src <- paste(readLines("Functions/animalpos_preprocessing_helpers.R", warn = FALSE), collapse = "\n")
code <- paste(grep("^\\s*#", readLines("Functions/animalpos_preprocessing_helpers.R", warn = FALSE),
                   invert = TRUE, value = TRUE), collapse = "\n")
for (bad in c("add_half_hour_transitions", "compute_phase_transitions", "compute_day_transitions")) {
  check(!grepl(bad, code, fixed = TRUE),
        paste0("6: helpers must not call the row-inserting function ", bad))
}
check(grepl("%OS", code, fixed = TRUE), "6: parser must use %OS to retain fractional seconds")

cat("AnimalPos preprocessing helper checks: PASS\n")

# ------------------------------------------------------------------
# 7. SESSION-CONSISTENT PHASE LUT
#    Block numbering must not depend on which subset it is applied to --
#    this is what keeps phase metadata stable after interval splitting.
# ------------------------------------------------------------------
span <- tt(c("2022-10-28 15:44:45", "2022-10-31 09:00:00"))
lut <- animalpos_phase_block_lut(span)
check(nrow(lut) == max(lut$PhaseBlockIndex) - min(lut$PhaseBlockIndex) + 1,
      "7: LUT must be contiguous over the session span")
check(all(diff(lut$PhaseBlockIndex) == 1), "7: LUT blocks must be consecutive")
check(all(lut$Phase[lut$ConsecActive > 0] == "Active"), "7: ConsecActive must only be set on Active blocks")
check(all(lut$Phase[lut$ConsecInactive > 0] == "Inactive"), "7: ConsecInactive only on Inactive blocks")

probe <- tt(c("2022-10-28 16:00:00", "2022-10-29 02:00:00", "2022-10-29 09:00:00",
              "2022-10-30 20:00:00", "2022-10-31 08:00:00"))
full <- animalpos_apply_phase_lut(probe, lut)
# applying to any subset must give identical values for those rows
sub  <- animalpos_apply_phase_lut(probe[c(2, 4)], lut)
check(identical(sub$Phase, full$Phase[c(2, 4)]), "7: subset application must match full application (Phase)")
check(identical(sub$ConsecActive, full$ConsecActive[c(2, 4)]), "7: subset must match (ConsecActive)")
check(identical(sub$ConsecInactive, full$ConsecInactive[c(2, 4)]), "7: subset must match (ConsecInactive)")
check(identical(full$Phase, animalpos_phase_label(probe)), "7: LUT Phase must agree with direct labelling")

# Splitting an interval then applying the LUT must give one phase per piece.
iv3 <- tibble(PositionID = 2L,
              IntervalStart = tt("2022-10-29 05:30:00"),
              IntervalEnd   = tt("2022-10-29 19:30:00"),
              DurationSec   = 14 * 3600)
s3 <- animalpos_split_intervals(iv3, split_phase = TRUE)
meta <- animalpos_apply_phase_lut(s3$IntervalStart, lut)
check(nrow(s3) == 3, paste0("7: 05:30-19:30 must split into 3 phase pieces, got ", nrow(s3)))
check(identical(meta$Phase, c("Active", "Inactive", "Active")), "7: piece phases must be Active/Inactive/Active")
check(sum(as.numeric(s3$DurationSec)) == 14 * 3600, "7: split must conserve duration")
check(length(unique(meta$ConsecActive[meta$Phase == "Active"])) == 2,
      "7: the two Active pieces belong to different Active blocks")

cat("Session phase-LUT checks: PASS\n")

# ---------------------------------------------------------------------------
# 9. HalfHoursElapsed is a property of the session clock, not of any animal.
# ---------------------------------------------------------------------------
hcheck <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)

# The anchor snaps to the containing phase-block start (18:30 for an evening
# start), reproducing the legacy anchor that a synthetic 18:30 row supplied.
t_eve <- as.POSIXct("2022-10-28 18:31:05", tz = "UTC")
hcheck(identical(animalpos_phase_block_bounds(t_eve)$block_start,
                 as.POSIXct("2022-10-28 18:30:00", tz = "UTC")),
       "9: anchor must be the 18:30 phase-block start")
hcheck(identical(animalpos_half_hours_elapsed(t_eve), 0),
       "9: the first half-hour after the block start must be index 0")

# Indices must step on the 18:30 grid, NOT on the first observation. 18:59:05 is
# still index 0; 19:00:05 crosses into index 1. Anchoring on the first row
# instead would put both in index 0 and shift everything downstream.
probe <- t_eve + c(0, 28 * 60, 29 * 60, 60 * 60)
hcheck(identical(animalpos_half_hours_elapsed(probe), c(0, 0, 1, 2)),
       paste0("9: half-hour indices must advance on the 30-minute clock grid; got ",
              paste(animalpos_half_hours_elapsed(probe), collapse = ",")))

# Two animals with different first-detection times must agree on a shared
# instant. The legacy per-animal anchor did not, once synthetic rows were gone.
shared <- t_eve + 60 * 60                      # 19:31:05, an instant both observe
early  <- c(t_eve, shared)                     # animal detected at 18:31
late   <- c(t_eve + 40 * 60, shared)           # animal first detected at 19:11
hcheck(identical(tail(animalpos_half_hours_elapsed(early), 1),
                 tail(animalpos_half_hours_elapsed(late), 1)),
       "9: the same instant must get the same index regardless of first detection")
legacy_per_animal <- function(x) floor(as.numeric(difftime(x, min(x), units = "mins")) / 30)
hcheck(!identical(tail(legacy_per_animal(early), 1), tail(legacy_per_animal(late), 1)),
       "9: control -- the legacy per-animal anchor must disagree here, else this cannot bite")

hcheck(identical(animalpos_half_hours_elapsed(t_eve[0]), numeric(0)),
       "9: zero-length input must return zero-length output")

cat("Half-hours-elapsed checks: PASS\n")

# ---------------------------------------------------------------------------
# 10. Vectorised splitter == loop reference, and == the legacy Stage 01 binning.
# ---------------------------------------------------------------------------
vcheck <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)

set.seed(20260827)
base_t <- as.numeric(as.POSIXct("2022-10-28 18:00:00", tz = "UTC"))
n_rand <- 4000
rs <- base_t + sample(0:(4 * 86400), n_rand, replace = TRUE)
rand_iv <- tibble(
  id = seq_len(n_rand),
  IntervalStart = as.POSIXct(rs, origin = "1970-01-01", tz = "UTC"),
  IntervalEnd   = as.POSIXct(rs + sample(c(1, 7, 60, 599, 600, 1800, 3600, 43200, 90000),
                                         n_rand, replace = TRUE),
                             origin = "1970-01-01", tz = "UTC")
) %>% mutate(DurationSec = as.numeric(difftime(IntervalEnd, IntervalStart, units = "secs")))

norm <- function(d) {
  d <- d[order(d$id, as.numeric(d$IntervalStart)), c("id", "IntervalStart", "IntervalEnd", "DurationSec")]
  rownames(d) <- NULL
  as.data.frame(d)
}
for (spec in list(list(p = 600, ph = FALSE, dy = FALSE), list(p = NULL, ph = TRUE, dy = FALSE),
                  list(p = 1800, ph = TRUE, dy = TRUE),  list(p = 10, ph = TRUE, dy = FALSE))) {
  fast <- animalpos_split_intervals(rand_iv, spec$p, spec$ph, spec$dy)
  ref  <- animalpos_split_intervals_reference(rand_iv, spec$p, spec$ph, spec$dy)
  lbl  <- paste0("period=", if (is.null(spec$p)) "NULL" else spec$p,
                 " phase=", spec$ph, " day=", spec$dy)
  vcheck(isTRUE(all.equal(norm(fast), norm(ref), tolerance = 0)),
         paste0("10: vectorised splitter must equal the loop reference for ", lbl))
  # Duration is conserved per original interval.
  tot <- tapply(fast$DurationSec, fast$id, sum)
  worst <- max(abs(unname(tot[as.character(rand_iv$id)]) - rand_iv$DurationSec))
  vcheck(worst < 1e-6,
         paste0("10: duration must be conserved for ", lbl, "; worst error ", worst, " s"))
}

# Legacy Stage 01 bin-splitting math, reproduced verbatim from
# 01_build_multiscale_behavior_metrics.R before the refactor.
legacy_bin_split <- function(dat, bin_size_sec) {
  s <- as.numeric(dat$IntervalStart); e <- as.numeric(dat$IntervalEnd)
  bs <- floor(s / bin_size_sec) * bin_size_sec
  be <- floor((e - 1e-7) / bin_size_sec) * bin_size_sec
  nb <- pmax(0L, as.integer((be - bs) / bin_size_sec) + 1L)
  keep <- nb > 0
  dat <- dat[keep, , drop = FALSE]; s <- s[keep]; e <- e[keep]; bs <- bs[keep]; nb <- nb[keep]
  ri <- rep(seq_len(nrow(dat)), nb)
  off <- sequence(nb) - 1L
  sbs <- rep(bs, nb) + off * bin_size_sec
  ss <- pmax(rep(s, nb), sbs); ee <- pmin(rep(e, nb), sbs + bin_size_sec)
  out <- dat[ri, , drop = FALSE]
  out$IntervalStart <- as.POSIXct(ss, origin = "1970-01-01", tz = "UTC")
  out$IntervalEnd   <- as.POSIXct(ee, origin = "1970-01-01", tz = "UTC")
  out$DurationSec   <- ee - ss
  out[out$DurationSec > 0, , drop = FALSE]
}
for (bs in c(10, 60, 300, 600, 1800)) {
  vcheck(isTRUE(all.equal(norm(animalpos_split_intervals_one_grid(rand_iv, bs, 0)),
                          norm(legacy_bin_split(rand_iv, bs)), tolerance = 0)),
         paste0("10: splitter must reproduce the legacy Stage 01 binning at ", bs, "s"))
}

# Phase splitting must land exactly on 06:30 / 18:30 and never elsewhere.
ph_pieces <- animalpos_split_intervals(rand_iv, NULL, split_phase = TRUE)
interior <- ph_pieces$IntervalStart[!as.numeric(ph_pieces$IntervalStart) %in% as.numeric(rand_iv$IntervalStart)]
vcheck(length(interior) > 0, "10: control -- phase splitting must actually produce interior cuts")
vcheck(all(format(interior, "%H:%M:%S", tz = "UTC") %in% c("06:30:00", "18:30:00")),
       "10: every interior phase cut must fall on 06:30:00 or 18:30:00")
# No surviving piece may span a phase boundary.
vcheck(all(animalpos_phase_block_index(ph_pieces$IntervalStart) ==
             animalpos_phase_block_index(as.POSIXct(as.numeric(ph_pieces$IntervalEnd) - 1e-3,
                                                    origin = "1970-01-01", tz = "UTC"))),
       "10: no piece may span a phase boundary after phase splitting")

# Degenerate inputs: a zero-duration interval must not survive as a piece, and
# an interval ending exactly on a boundary must stay whole (no zero-length tail).
degen <- tibble(
  id = 1:3,
  IntervalStart = as.POSIXct(c("2022-10-28 18:30:00", "2022-10-28 18:20:00",
                               "2022-10-28 06:30:00"), tz = "UTC"),
  IntervalEnd   = as.POSIXct(c("2022-10-28 18:30:00", "2022-10-28 18:30:00",
                               "2022-10-28 18:30:00"), tz = "UTC")
) %>% mutate(DurationSec = as.numeric(difftime(IntervalEnd, IntervalStart, units = "secs")))
dg <- animalpos_split_intervals(degen, 600, split_phase = TRUE)
vcheck(!1 %in% dg$id, "10: a zero-duration interval must not produce a surviving piece")
vcheck(sum(dg$id == 2) == 1, "10: an interval ending exactly on a boundary must stay whole")
vcheck(sum(dg$id == 3) == 72,
       paste0("10: a full 12 h inactive block must split into 72 ten-minute bins; got ",
              sum(dg$id == 3)))
vcheck(all(dg$DurationSec > 0), "10: no surviving piece may have non-positive duration")

cat("Interval splitter equivalence checks: PASS\n")
