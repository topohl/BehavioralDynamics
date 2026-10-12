# ===========================================================================
# Vectorised interval reconstruction (Functions/interval_reconstruction.R) against the per-event loops it replaced
# in Stage 01 (occupancy, dyads) and Stage 02 (dyads). Portable: synthetic systems only, nothing on the lab drive.
#
#  1. boundary and zero-event cases: empty system, one row, one distinct time, one animal, simultaneous rows,
#     two rows of one animal at the same time, NA and zero PositionID, NA labels, late animals, fractional seconds
#  2. randomized systems (several seeds): identical rows, order, values and types
#  3. the stages call the vectorised helper and no longer define the loops
# ===========================================================================
suppressPackageStartupMessages({ library(dplyr); library(tibble); library(purrr) })
repo <- Sys.getenv("MMM_REPO_DIR", unset = NA_character_)
if (is.na(repo) || !nzchar(repo)) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  while (!(file.exists(file.path(repo, "Functions", "interval_reconstruction.R")) && dir.exists(file.path(repo, "Analysis")))) {
    parent <- dirname(repo)
    if (identical(parent, repo)) stop("Could not locate the repository root.", call. = FALSE)
    repo <- parent
  }
}
setwd(repo)
check <- function(cond, msg) if (!isTRUE(cond)) stop("FAIL: ", msg, call. = FALSE)
source("Functions/interval_reconstruction.R")
system_event_gap_threshold_sec <- 3600
pair_distance_lookup <- tidyr::expand_grid(PositionID_A = 1:8, PositionID_B = 1:8) %>%
  mutate(grid_distance = abs(((PositionID_A - 1) %% 4) - ((PositionID_B - 1) %% 4)) + abs(((PositionID_A - 1) %/% 4) - ((PositionID_B - 1) %/% 4)))
META01 <- c("SourceFile", "Dataset", "RawParadigm", "Batch", "CageChange", "Epoch", "System", "Phase", "ConsecActive",
            "ConsecInactive", "MinutesOfDay", "CookieWindowPrimary", "CookieSubWindow", "CookieHabEpoch")
META02 <- c("SourceFile", "Dataset", "RawParadigm", "Batch", "CageChange", "Epoch", "System", "Phase", "HalfHoursElapsed",
            "MinutesOfDay", "CookieWindowPrimary", "CookieSubWindow", "CookieHabEpoch")

# ---- reference copies of the per-event loops, verbatim from commit e6e60ee (only renamed) ----
ref01_last_meta_before <- function(dat_sys, t0) {
  dat_sys %>%
    filter(DateTime <= t0) %>%
    slice_tail(n = 1) %>%
    transmute(
      SourceFile = first(SourceFile),
      Dataset = first(Dataset),
      RawParadigm = first(RawParadigm),
      Batch = first(Batch),
      CageChange = first(CageChange),
      Epoch = first(Epoch),
      System = first(System),
      Phase = first(Phase),
      ConsecActive = first(ConsecActive),
      ConsecInactive = first(ConsecInactive),
      MinutesOfDay = first(MinutesOfDay),
      CookieWindowPrimary = first(CookieWindowPrimary),
      CookieSubWindow = first(CookieSubWindow),
      CookieHabEpoch = first(CookieHabEpoch)
    )
}
ref01_occupancy <- function(dat_sys) {
  dat_sys <- dat_sys %>% arrange(DateTime, AnimalID)
  animals <- sort(unique(dat_sys$AnimalID))
  event_times <- sort(unique(dat_sys$DateTime))
  if (length(animals) == 0 || length(event_times) < 2) return(tibble())

  out <- vector("list", length(event_times) - 1)
  current_pos <- rep(NA_integer_, length(animals)); names(current_pos) <- animals
  current_group <- rep(NA_character_, length(animals)); names(current_group) <- animals
  current_sex <- rep(NA_character_, length(animals)); names(current_sex) <- animals

  for (i in seq_len(length(event_times) - 1)) {
    t0 <- event_times[i]
    t1 <- event_times[i + 1]
    duration_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    if (!is.finite(duration_sec) || duration_sec <= 0) next

    updates <- dat_sys %>% filter(DateTime == t0)
    if (nrow(updates) > 0) {
      for (j in seq_len(nrow(updates))) {
        a <- updates$AnimalID[j]
        current_pos[a] <- updates$PositionID[j]
        if (!is.na(updates$Group[j])) current_group[a] <- updates$Group[j]
        if (!is.na(updates$Sex[j])) current_sex[a] <- updates$Sex[j]
      }
    }

    valid <- is.finite(current_pos) & current_pos > 0
    if (!any(valid)) next

    interval_meta <- ref01_last_meta_before(dat_sys, t0)
    out[[i]] <- tibble(
      AnimalNum = names(current_pos)[valid],
      AnimalID = names(current_pos)[valid],
      PositionID = as.integer(current_pos[valid]),
      Group = current_group[names(current_pos)[valid]],
      Sex = current_sex[names(current_pos)[valid]],
      IntervalStart = t0,
      IntervalEnd = t1,
      DurationSec = duration_sec,
      SystemEventGap = duration_sec > system_event_gap_threshold_sec
    ) %>%
      bind_cols(interval_meta[rep(1, sum(valid)), ])
  }

  bind_rows(out)
}
ref01_dyads <- function(dat_sys) {
  dat_sys <- dat_sys %>% arrange(DateTime, AnimalID)
  animals <- sort(unique(dat_sys$AnimalID))
  event_times <- sort(unique(dat_sys$DateTime))
  if (length(animals) < 2 || length(event_times) < 2) return(tibble())

  animal_pairs <- combn(animals, 2, simplify = FALSE)
  out <- vector("list", length(event_times) - 1)
  current_pos <- rep(NA_integer_, length(animals)); names(current_pos) <- animals

  for (i in seq_len(length(event_times) - 1)) {
    t0 <- event_times[i]
    t1 <- event_times[i + 1]
    duration_sec <- as.numeric(difftime(t1, t0, units = "secs"))
    if (!is.finite(duration_sec) || duration_sec <= 0) next

    updates <- dat_sys %>% filter(DateTime == t0)
    if (nrow(updates) > 0) {
      for (j in seq_len(nrow(updates))) {
        current_pos[updates$AnimalID[j]] <- updates$PositionID[j]
      }
    }

    pair_tbl <- map_dfr(animal_pairs, function(p) {
      pos_a <- current_pos[p[1]]
      pos_b <- current_pos[p[2]]
      if (!is.finite(pos_a) || !is.finite(pos_b)) return(tibble())
      tibble(Focal = p[1], Partner = p[2], PositionID_A = as.integer(pos_a), PositionID_B = as.integer(pos_b))
    })
    if (nrow(pair_tbl) == 0) next

    interval_meta <- ref01_last_meta_before(dat_sys, t0)
    out[[i]] <- pair_tbl %>%
      mutate(
        IntervalStart = t0,
        IntervalEnd = t1,
        DurationSec = duration_sec,
        SystemEventGap = duration_sec > system_event_gap_threshold_sec
      ) %>%
      bind_cols(interval_meta[rep(1, nrow(pair_tbl)), ]) %>%
      left_join(pair_distance_lookup, by = c("PositionID_A", "PositionID_B"))
  }

  bind_rows(out)
}
ref02_last_meta_before <- function(dat_sys, t0) {
  dat_sys %>%
    filter(DateTime <= t0) %>%
    slice_tail(n = 1) %>%
    transmute(
      SourceFile = first(SourceFile),
      Dataset = first(Dataset),
      RawParadigm = first(RawParadigm),
      Batch = first(Batch),
      CageChange = first(CageChange),
      Epoch = first(Epoch),
      System = first(System),
      Phase = first(Phase),
      HalfHoursElapsed = first(HalfHoursElapsed),
      MinutesOfDay = first(MinutesOfDay),
      CookieWindowPrimary = first(CookieWindowPrimary),
      CookieSubWindow = first(CookieSubWindow),
      CookieHabEpoch = first(CookieHabEpoch)
    )
}
ref02_dyads <- function(dat_sys) {
  dat_sys <- dat_sys %>% arrange(DateTime, AnimalID)
  animals <- sort(unique(dat_sys$AnimalID))

  if (length(animals) < 2) {
    return(tibble())
  }

  event_times <- sort(unique(dat_sys$DateTime))
  if (length(event_times) < 2) {
    return(tibble())
  }

  animal_pairs <- combn(animals, 2, simplify = FALSE)
  out <- vector("list", length(event_times) - 1)

  current_pos <- rep(NA_integer_, length(animals))
  names(current_pos) <- animals
  current_group <- rep(NA_character_, length(animals))
  names(current_group) <- animals
  current_sex <- rep(NA_character_, length(animals))
  names(current_sex) <- animals

  for (i in seq_len(length(event_times) - 1)) {
    t0 <- event_times[i]
    t1 <- event_times[i + 1]
    duration_sec <- as.numeric(difftime(t1, t0, units = "secs"))

    updates <- dat_sys %>% filter(DateTime == t0)
    if (nrow(updates) > 0) {
      for (j in seq_len(nrow(updates))) {
        a <- updates$AnimalID[j]
        current_pos[a] <- updates$PositionID[j]
        if (!is.na(updates$Group[j])) current_group[a] <- updates$Group[j]
        if (!is.na(updates$Sex[j])) current_sex[a] <- updates$Sex[j]
      }
    }

    if (!is.finite(duration_sec) || duration_sec <= 0) next

    # Descriptive provenance only: how far apart the bracketing genuine system
    # events were. Never used to admit or reject an interval.
    system_event_gap <- duration_sec > system_event_gap_threshold_sec

    interval_meta <- ref02_last_meta_before(dat_sys, t0)

    pair_tbl <- map_dfr(animal_pairs, function(p) {
      pos_a <- current_pos[p[1]]
      pos_b <- current_pos[p[2]]
      if (!is.finite(pos_a) || !is.finite(pos_b)) return(tibble())
      tibble(
        Focal = p[1],
        Partner = p[2],
        PositionID_A = as.integer(pos_a),
        PositionID_B = as.integer(pos_b),
        Group_Focal = current_group[p[1]],
        Group_Partner = current_group[p[2]],
        Sex_Focal = current_sex[p[1]],
        Sex_Partner = current_sex[p[2]]
      )
    })

    if (nrow(pair_tbl) == 0) next

    out[[i]] <- pair_tbl %>%
      mutate(
        IntervalStart = t0,
        IntervalEnd = t1,
        DurationSec = duration_sec,
        SystemEventGap = system_event_gap
      ) %>%
      bind_cols(interval_meta[rep(1, nrow(pair_tbl)), ]) %>%
      left_join(pair_distance_lookup, by = c("PositionID_A", "PositionID_B"))
  }

  bind_rows(out)
}

# ---- comparison ----
same <- function(x, y) {
  if (nrow(x) == 0 || nrow(y) == 0) return(nrow(x) == 0 && nrow(y) == 0)
  if (!identical(names(x), names(y)) || nrow(x) != nrow(y)) return(FALSE)
  all(vapply(names(x), function(n) identical(unname(x[[n]]), unname(y[[n]])), logical(1)))
}
compare_all <- function(d, label) {
  check(same(ref01_occupancy(d), mmm_occupancy_intervals_vec(d, META01, system_event_gap_threshold_sec)),
        paste0(label, ": Stage 01 occupancy differs from the loop"))
  check(same(ref01_dyads(d), mmm_dyad_intervals_vec(d, META01, system_event_gap_threshold_sec, pair_distance_lookup)),
        paste0(label, ": Stage 01 dyads differ from the loop"))
  check(same(ref02_dyads(d), mmm_dyad_intervals_vec(d, META02, system_event_gap_threshold_sec, pair_distance_lookup,
                                                    label_cols = c("Group", "Sex"))),
        paste0(label, ": Stage 02 dyads differ from the loop"))
}
T0 <- as.POSIXct("2023-04-24 18:30:00", tz = "UTC")
mk <- function(sec, id, pos, group = "RES", sex = "Female") {
  n <- length(sec)
  tibble(DateTime = T0 + sec, AnimalID = id, PositionID = as.integer(pos), Group = rep_len(group, n), Sex = rep_len(sex, n),
         SourceFile = "f.csv", Dataset = "SIS", RawParadigm = NA_character_, Batch = "B3", CageChange = "CC1", Epoch = "CC1",
         System = "sys.1", Phase = ifelse(sec < 43200, "Active", "Inactive"), ConsecActive = 1L, ConsecInactive = 0L,
         HalfHoursElapsed = as.integer(sec %/% 1800), MinutesOfDay = (1110 + sec / 60) %% 1440,
         CookieWindowPrimary = NA_character_, CookieSubWindow = NA_character_, CookieHabEpoch = NA_character_)
}

# 1 -------------------------------------------------------------------------
cases <- list(
  empty = mk(numeric(), character(), integer()),
  one_row = mk(0, "a", 1),
  one_time = mk(c(5, 5), c("a", "b"), c(1, 2)),
  one_animal = mk(c(0, 10, 20, 30), "a", c(1, 2, 2, 5)),
  simultaneous = mk(c(0, 0, 10, 10, 20), c("a", "b", "a", "b", "c"), c(1, 2, 3, 4, 5)),
  same_animal_same_time = mk(c(0, 10, 10, 20), c("a", "a", "a", "b"), c(1, 2, 6, 3)),
  na_position = mk(c(0, 10, 20, 30), c("a", "b", "a", "b"), c(1, NA, 2, 3)),
  zero_position = mk(c(0, 10, 20), c("a", "b", "a"), c(0, 2, 3)),
  na_labels = mk(c(0, 10, 20, 30), c("a", "a", "b", "a"), c(1, 2, 3, 4), group = c(NA, "SUS", NA, NA), sex = c("Male", NA, NA, "Male")),
  late_animal = mk(c(0, 5, 3600, 3700), c("a", "a", "b", "a"), c(1, 2, 3, 4)),
  fractional = mk(c(0.001, 0.0015, 0.002, 1e-3 + 7200), c("a", "b", "a", "c"), c(1, 2, 3, 4)),
  numeric_ids = mk(c(0, 1, 2, 3), c("10", "9", "0004", "4"), c(1, 2, 3, 4))
)
for (nm in names(cases)) compare_all(cases[[nm]], nm)
cat("1 boundary and zero-event cases (", length(cases), "): PASS\n", sep = "")

# 2 -------------------------------------------------------------------------
for (seed in 1:6) {
  set.seed(seed)
  n <- 400
  ids <- sample(c("OR1", "OR2", "13", "0007", "OQ9"), n, replace = TRUE)
  sec <- sort(round(runif(n, 0, 86400), 3))
  sec[sample(n, 20)] <- sec[sample(n, 20)]                     # force some shared times
  d <- mk(sort(sec), ids, sample(c(1:8, NA, 0), n, replace = TRUE, prob = c(rep(0.12, 8), 0.02, 0.02)),
          group = sample(c("CON", "RES", "SUS", NA), n, replace = TRUE), sex = sample(c("Female", NA), n, replace = TRUE))
  compare_all(d, paste("random seed", seed))
}
cat("2 randomized systems: PASS\n")

# 3 -------------------------------------------------------------------------
s01 <- paste(readLines("Analysis/01_build_multiscale_behavior_metrics.R", warn = FALSE), collapse = "\n")
s02 <- paste(readLines("Analysis/02_build_dyadic_rfid_contacts.R", warn = FALSE), collapse = "\n")
check(grepl("mmm_occupancy_intervals_vec(dat_sys, STAGE01_INTERVAL_META", s01, fixed = TRUE) &&
        grepl("mmm_dyad_intervals_vec(dat_sys, STAGE01_INTERVAL_META", s01, fixed = TRUE) &&
        grepl("mmm_dyad_intervals_vec(dat_sys, STAGE02_INTERVAL_META", s02, fixed = TRUE),
      "3: Stages 01/02 must use the vectorised reconstruction")
check(!grepl("last_meta_before <- function", s01, fixed = TRUE) && !grepl("last_interval_meta_before <- function", s02, fixed = TRUE),
      "3: the per-event loops must be gone")
check(grepl("getOption(\"mmm.seed_from_vendor_raw\", TRUE)", s01, fixed = TRUE),
      "3: Stage 01 must let a pre-seeded input switch the vendor seeding off")
cat("3 stage wiring: PASS\n")
cat("Interval reconstruction checks: PASS\n")
