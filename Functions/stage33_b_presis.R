# ================================================================
# Stage 33 module B - pre-SIS covariates, CC1 board tables and the B2/B6 arena ranks
# MMMSociability
# ================================================================
# Plan section 9 of the frozen docs/STAGE33_POSTHOC_COHORT_FOLLOWUPS_PLAN_v1.0.md, with its shared sections 0-7 and
# 13-17. Post hoc, descriptive (estimation only): no test, no p-value, no SIS-vs-CON quantity of the rate and no RES-vs-SUS
# comparison (this module never reads the outcome groups). Definitions only: sourcing this file reads and writes nothing.
#
#   p2  <- s33b_phase2(design, ctx)          outcome-free (plan section 6, Phase 2): b01, b03 (E7), the rate rows of b04
#                                            and b05 (E1, E3-E5, S1, S2, S4-S9, S11-S14), b08 and b09 (E8), b10-b14 (E9-E12
#                                            with S16-S19), b15 (X1, S20), b_join_audit and the Phase-2 rows of
#                                            b_bootstrap_summaries; gates GB-13, GB-14a, GB-14b (recorded), GB-15, GB-16
#   out <- s33b_phase3(design_out, p2, ctx)  the CombZ rows of b04 and b05 (E2, E3-E6 and their sensitivities), b02, b06,
#                                            b07; gate GB-12; returns the 15 tables and the 2 audit tables
# Re-extraction: b04, b05 and b_bootstrap_summaries keep their Phase-2 rows first and unchanged (computed_in = 'phase2');
# s33b_outcome_free_extract(out) returns every product named in S33B_OUTCOME_FREE_PRODUCTS from the final tables, equal
# to p2$products.
# Conventions of this module: rank_of_3_within_sex = 1 for the highest of the three same-sex cohorts (as module D);
# SD_w (per-SD units of the covariates) = the n - 1 residual SD on cohort in sis_87 (plan section 7), used for the rate
# and the CombZ rows alike; the E10 selection sentence is S33_FIXED_SENTENCES[['b_selection']] verbatim.
#
# Engine: the frozen engine through s33_kr_fit() / s33_kr_contrast() (KR; p and statistic dropped at source), OLS and
# glmmTMB through the frozen mmm_ci_fit(); clubSandwich CR2 without p-values; lme4::bootMer (parametric, use.u = FALSE) and
# profile likelihood for the variance shares. Seeds only from ctx$seeds, replicate counts only from ctx$B, heavy loops
# through s33_checkpoint().
# Requires data.table, digest, lme4, lmerTest, pbkrtest, clubSandwich, glmmTMB (S11) and, for the thin workbook readers
# only, readxl; Functions/rfid_canonical_inference.R, stage32_inference.R, stage33_common.R and stage33_run.R sourced first;
# canonical_animal_id() from ctx$canon or the calling environment.
# ================================================================

# ---------------------------------------------------------------- products, sets and constants (plan section 9)
S33B_OUTCOME_FREE_PRODUCTS <- c("b01_covariates_animal", "b03_balance_cohort", "b04_variance_shares", "b05_associations",
                                "b08_cc1_board_table", "b09_con_board_rotation", "b10_arena_cohort_means",
                                "b11_arena_con_animal_values", "b12_arena_cohort_contrasts", "b13_arena_profile_agreement",
                                "b14_arena_within_cohort_association", "b15_appendix_board_offsets", "b_join_audit",
                                "b_bootstrap_summaries")
# Products holding only their Phase-2 rows (computed_in = 'phase2'); Phase 3 appends its rows after them.
S33B_PHASE2_ROW_PRODUCTS <- c("b04_variance_shares", "b05_associations", "b_bootstrap_summaries")

S33B_TARGET <- c(Male = "B2", Female = "B6")
S33B_REFERENCE <- list(Male = c("B1", "B5"), Female = c("B3", "B4"))
S33B_SAME_BOARDS <- list(Male = c("sys.1", "sys.2", "sys.5"), Female = c("sys.1", "sys.4", "sys.5"))
S33B_SOURCE_LABEL_ALT <- c("SHKH0-01641" = "BHKH0-00290")

# Arena measures (workbook sheets; GB-13 counts over all 117 and over the 111 tracked animals; nominal durations).
S33B_ARENA <- data.table::data.table(
  measure = c("oft_distance", "epm_distance", "nor_distance", "hab_distance", "s1_distance", "s2_distance"),
  speed = c("oft_speed", "epm_speed", "nor_speed", "hab_speed", "s1_speed", "s2_speed"),
  arena = c("OFT", "EPM", "NOR", "three-chamber HAB", "three-chamber S1", "three-chamber S2"),
  nominal_s = c(600, 300, 300, 600, 600, 600),
  n_all = c(113L, 117L, 117L, 116L, 116L, 116L), n_tracked = c(107L, 111L, 111L, 110L, 110L, 110L),
  tier = c("primary", "primary", "primary", "secondary", "secondary", "secondary"))
S33B_INDEX <- list(arena_index = c("oft_distance", "epm_distance", "nor_distance"),
                   arena_index4 = c("oft_distance", "epm_distance", "nor_distance", "hab_distance"),
                   arena_speed_index = c("oft_speed", "epm_speed", "nor_speed"),
                   arena_speed_index4 = c("oft_speed", "epm_speed", "nor_speed", "hab_speed"))
S33B_INDEX_MIN <- c(arena_index = 2L, arena_index4 = 3L, arena_speed_index = 2L, arena_speed_index4 = 3L)
S33B_DIST_MEASURES <- c("oft_distance", "epm_distance", "nor_distance", "arena_index", "hab_distance", "s1_distance",
                        "s2_distance", "arena_index4")
S33B_SPEED_MEASURES <- c("oft_speed", "epm_speed", "nor_speed", "arena_speed_index", "hab_speed", "s1_speed", "s2_speed",
                         "arena_speed_index4")
# GB-14b: flagged 'calibration not verified across recordings'; the primary index excludes HAB (S18 = index4).
S33B_FLAGGED_MEASURES <- c("hab_distance", "s1_distance", "s2_distance", "arena_index4", "hab_speed", "s1_speed", "s2_speed",
                           "arena_speed_index4")
S33B_OFT_DROPPED_CODES <- c("Y8G0", "I1E6")
# Declared SLEAP identifiers and columns (plan section 7 lint: exempt from the RFID-metric labels only).
S33B_SLEAP_METRIC_IDS <- c("arena_distance", "arena_speed")
S33B_METRIC_LABEL <- c(arena_speed = "SLEAP arena speed (cm/s)")
S33B_SLEAP_COLUMNS <- c(S33B_ARENA$measure, S33B_ARENA$speed, names(S33B_INDEX),
                        paste0("u_", c(S33B_ARENA$measure, S33B_ARENA$speed, names(S33B_INDEX))),
                        paste0("in_arena_", S33B_ARENA$measure), "v2_oft_distance", "v2_epm_distance", "v2_nor_distance")
# Columns holding verbatim engine messages (plan section 7: exempt from the lint; read by s33_lint_exempt_columns()).
# The module writes no path column.
S33B_LINT_EXEMPT_COLUMNS <- c("engine_messages", "failure_reason", "profile_messages")
# Arena-matched SIS per measure (plan section 9, Populations; checked in GB-13).
S33B_ARENA_SIS_N <- c(oft_distance = 85L, epm_distance = 87L, nor_distance = 87L, hab_distance = 87L, s1_distance = 87L,
                      s2_distance = 87L)
S33B_RNG <- "Mersenne-Twister/Inversion/Rejection"
# Readings of the plan recorded for the README (module B); 'PLAN ISSUE' marks the code site where the plan cannot be
# followed literally.
S33B_DEVIATIONS <- c(
  paste("E5: clubSandwich does not compute CR2 for lmer fits with crossed random effects (source cage x CC1 cage), so the",
        "CR2-by-source-cage interval of beta_J is computed on the OLS fixed part of the same model (Satterthwaite df); the KR row",
        "comes from the mixed model (PLAN ISSUE)."),
  "rank_of_3_within_sex is 1 for the highest of the three same-sex cohorts, as in module D (the B source plan used 1 = lowest).",
  paste("E10 direction = the sign of the target cohort's contrast with the mean of the other two of its sex; direction_vs_rate",
        "compares it with the sign of the same cohort's A1 rate contrast (same / opposite)."),
  paste("SD_w (per-SD units of age and weight) is the n - 1 residual SD on cohort in sis_87, over the cohorts in which the",
        "covariate varies (source-cage level for w_srcmean), for the rate and the CombZ rows alike; S6 uses the tp2 values."),
  "S11 (glmmTMB) slopes carry the engine's Wald interval, named t_Inf (t_<df> with infinite df) in the section 7 vocabulary.",
  paste("The workbook readers also read the oft blind-code column (to identify the two rows without animal_id that GB-13 names)",
        "and the socialPreference_long phase column (HAB, S1, S2)."),
  "GB-13 also checks the arena-matched SIS counts of the Populations line (OFT 85, EPM 87, NOR 87, HAB/S1/S2 87).",
  paste("Re-expressions of an interval (per SD_w, u units, percent scale of the log offsets) are named lower / upper, so that",
        "the README interval count does not count them twice."))

# Sensitivity parametric bootstraps: seed 33020200 + k (ctx$seeds 'B_sens_<k>'), B = ctx$B['parametric_sensitivity'].
S33B_SENS_BOOT <- c(S1_rate = 1L, S1_CombZ = 2L, S3_CombZ = 3L, S5_log_rate = 4L, S9_rate = 5L, S10_CombZ_noWD = 6L,
                    S10_CombZ_noBW = 7L, S13_rate = 8L, S14_rate = 9L, S14_CombZ = 10L, S15_sis_87 = 11L, S15_sis_93 = 12L)

# Random parts and the E3-E5 model forms (fixed part, declared rank with six cohorts, reported terms).
S33B_RE <- c(primary = "(1 | src_cage) + (1 | CageEpisodeID)", S4a = "(1 | src_cage)", S4b = "(1 | CageEpisodeID)")
S33B_FORMS <- data.table::data.table(
  form = c("E3_joint", "E3_split", "E4_age", "E4_weight", "E4_split", "E5_line", "E5_line_adjusted"),
  estimand_group = c("E3", "E3", "E4", "E4", "E4", "E5", "E5"),
  rhs = c("Batch + age_c + w_c", "Batch + age_c + w_within + w_srcmean", "Batch + age_c", "Batch + w_c",
          "Batch + w_within + w_srcmean", "Batch + line_J", "Batch + line_J + age_c + w_c"),
  rank = c(8L, 9L, 7L, 7L, 8L, 7L, 9L),
  terms = c("age_c;w_c", "age_c;w_within;w_srcmean", "age_c", "w_c", "w_within;w_srcmean", "line_J", "line_J"))
S33B_TERM_LEVEL <- c(age_c = "source_cage_within_cohort", w_c = "animal_within_cohort", w_within = "animal_within_cohort",
                     w_srcmean = "source_cage_within_cohort", line_J = "source_cage_within_cohort")
S33B_TERM_SD_UNIT <- c(age_c = "animal", w_c = "animal", w_within = "animal", w_srcmean = "source_cage")
S33B_COMPONENT <- c(src_cage = "source_cage", CageEpisodeID = "cc1_cage", cc4_cage = "cc4_cage", Residual = "remaining")
S33B_COMPONENT_LEVEL <- c(source_cage = "source_cage_within_cohort", cc1_cage = "cage_within_cohort",
                          cc4_cage = "cage_within_cohort", remaining = "animal_within_cohort")
S33B_MECH_COUPLING <- c(NOR = "none", sucrose_pref = "none", delta_cort = "none", weight_dev = "shared_term",
                        adrenal_weight = "ratio_denominator", spleen_weight = "ratio_denominator", CombZ = "mixed",
                        CombZ_noWD = "ratio_denominator", CombZ_noBW = "none")
S33B_CZ_OUTCOMES <- c("CombZ_noBW", "CombZ_noWD", "CombZ")   # E3 order: CombZ_noBW leads

S33B_LABEL <- list(
  s_cohort = paste("share of total variance lying between the six cohort means (includes the sex difference, since sex is",
                   "nested in cohort); cohort-level structure may be biology of each social network"),
  source_cage = "associated with source cage (shared pre-SIS origin of unrecorded duration)",
  cc1_cage = "associated with CC1 cage (cage and board aliased)",
  cc4_cage = "associated with CC4 cage",
  remaining_rate = "remaining (animal, measurement)",
  remaining_cz = "remaining (animal, later cages, post-CC4 housing, measurement)",
  spread = "spread under the fitted zero component, not a confidence limit",
  icc = paste("signed moment ICC by CC1 cage within cohort; a negative CC1-cage component is absorbed in 'remaining (animal,",
              "later cages, post-CC4 housing, measurement)'"),
  con = paste("the single CON cage of each cohort (4 animals from one pre-SIS cage, housed together throughout; in B2, B5 and",
              "B6 that pre-SIS cage also supplied 2 SIS); interval absent"),
  con_ci = paste("the single CON cage of each cohort (4 animals from one pre-SIS cage, housed together throughout; in B2, B5 and",
                 "B6 that pre-SIS cage also supplied 2 SIS); interval anti-conservative"),
  range = "cage resampling within cohort (about 4 cages per cohort; about 80% coverage)",
  selection_rate = "selection criterion (B2 and B6 had the lowest SIS A1 RFID position-change rates of their sex); no interval",
  calibration = "calibration not verified across recordings",
  substrain = paste("C57BL/6J (n = %d in %d source cages from other colonies) vs C57BL/6JRj; not separable from source cage",
                    "or colony; interval model-based and resting on %d source cages"),
  e5 = "model-based; interval rests on 3 source cages",
  geometry = "a tp1 weight - rate association may partly reflect RFID detection geometry (tag position changes with body size)",
  coupling = c(CombZ = "CombZ contains weight_dev (tp2 baseline; tp6 - tp3 in B6) and two organ ratios to body weight",
               CombZ_noWD = "CombZ_noWD leaves out weight_dev but keeps the two organ ratios to body weight",
               CombZ_noBW = "CombZ_noBW holds no body-weight term (NOR, sucrose preference, delta_cort)",
               weight_dev = "weight_dev is the weight change from tp2 (tp3 in B6) to tp6: the weight term is shared by construction"),
  con_flag = "single CON cage per cohort; anti-conservative",
  e4_age = "one-covariate age row: quoted only with the matching E3 joint row",
  x1 = "technical board-offset check; offsets assumed constant across cohorts and cage changes; not a re-test of H05/H06",
  e8 = paste("a constant per-board read offset is the same on both sides of a same-board difference; board or session",
             "sensitivity that changed between cohorts is not addressed"),
  board = "board assignment is not a cohort property (CON cage board at CC1-CC4 in b09)",
  housing = paste("CON stayed with their source-cage mates; SIS were placed with two or three cage-mates from other recorded",
                  "pre-SIS cages (prior contact and kinship not recorded)"),
  offset_reference = "own-sex 3-cohort mean, own included",
  rank = "rank among the three same-sex cohorts (1 = highest)",
  direction = "sign of the target cohort's contrast with the mean of the other two cohorts of its sex",
  e11 = "six sex-centred cohort values (4 df); no interval",
  rate_label = "RFID position-change rate (A1)",
  arena_label = "SLEAP arena distance (cm)")

# ---------------------------------------------------------------- small helpers
s33b_sha <- function(x) digest::digest(x, algo = "sha256")

#' canonical_animal_id(): ctx$canon, else the function in the calling environment (the runner provides it).
s33b_canon <- function(ctx) {
  if (is.function(ctx$canon)) return(ctx$canon)
  if (exists("canonical_animal_id", mode = "function")) return(get("canonical_animal_id", mode = "function"))
  stop("canonical_animal_id() is needed (ctx$canon or the calling environment).", call. = FALSE)
}

#' s33_checkpoint() for module B. The runner's ctx$checkpoint_log (an environment) receives its rows as usual; `log` (an
#' environment) gets the same rows in the MODULE CONTRACT form (module, step, checkpoint_key, file, reused), returned by
#' Phase 3 as `checkpoints`. Without a checkpoint folder the value is computed and the row has no key and no file.
s33b_ckpt <- function(ctx, step, fun, key_extra, log) {
  env <- if (is.environment(ctx$checkpoint_log)) ctx$checkpoint_log else new.env(parent = emptyenv())
  before <- ls(env); cx <- ctx; cx$checkpoint_log <- env
  v <- s33_checkpoint(cx, "B", step, fun, key_extra)
  new <- setdiff(ls(env), before)
  row <- if (length(new)) data.table::rbindlist(mget(new, envir = env))[, list(module = "B", step, checkpoint_key, file, reused)] else
    data.table::data.table(module = "B", step = step, checkpoint_key = NA_character_, file = NA_character_, reused = FALSE)
  assign(sprintf("%06d", length(ls(log)) + 1L), row, envir = log)
  v
}
s33b_log_rows <- function(log) {
  if (!length(ls(log))) return(data.table::data.table(module = character(), step = character(), checkpoint_key = character(),
                                                      file = character(), reused = logical()))
  data.table::rbindlist(mget(sort(ls(log)), envir = log))
}

#' Linear combination of independent clustered means (CR2 per part): estimate, se and Welch-Satterthwaite df
#' (plan section 7, 'CR2_Welch_<cluster>'); no interval when a weighted part lacks a CR2 se.
s33b_lincomb <- function(est, se, df, w, level = 0.95) {
  k <- w != 0; e <- sum(w[k] * est[k])          # parts with weight 0 never enter (they may be missing)
  if (!all(is.finite(est[k]))) return(list(estimate = NA_real_, se = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  if (!all(is.finite(se[k]) & is.finite(df[k]))) return(list(estimate = e, se = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_))
  vi <- w[k]^2 * se[k]^2; v <- sum(vi); d <- v^2 / sum(vi^2 / df[k])
  ci <- s33_t_interval(e, sqrt(v), d, level)
  list(estimate = e, se = sqrt(v), df = d, ci_low = ci$ci_low, ci_high = ci$ci_high)
}

#' Signed moment ICC of y by cluster within cohort (nested one-way ANOVA: clusters within cohorts; unbalanced n0).
#' With one cohort it equals s33_moment_icc(). May be negative.
s33b_moment_icc_within <- function(y, cluster, cohort) {
  ok <- is.finite(y); y <- y[ok]; g <- as.character(cluster[ok]); h <- as.character(cohort[ok])
  hg <- tapply(h, g, function(v) v[1]); if (any(tapply(h, g, data.table::uniqueN) > 1L)) stop("A cluster spans cohorts.", call. = FALSE)
  N <- length(y); k <- length(hg); C <- data.table::uniqueN(h)
  if (k - C < 1L || N - k < 1L) return(NA_real_)
  mg <- tapply(y, g, mean); ng <- tapply(y, g, length); mh <- tapply(y, h, mean); Nh <- tapply(y, h, length)
  msb <- sum(ng * (mg - mh[hg[names(mg)]])^2) / (k - C)
  msw <- sum((y - mg[g])^2) / (N - k)
  sq <- tapply(ng^2, hg[names(ng)], sum)
  n0 <- (N - sum(sq / Nh[names(sq)])) / (k - C)
  (msb - msw) / (msb + (n0 - 1) * msw)
}

# ---------------------------------------------------------------- thin readers (runner only; readxl) and pure parsers
#' Declared workbook columns only (plan section 4; Phase 2): arena sheets, master_wide oft_distance, the SLEAP v2 distances
#' for GB-14a, and the master_wide_sleap header (GB-14b). Every read is asserted outcome-free.
s33b_read_arena_raw <- function(paths) {
  wb <- paths$workbook
  rd <- function(sheet, cols) s33_read_sheet_cols(wb, sheet, cols)
  # 'code' (blind code) is read on the oft sheet only, to identify the two dropped rows (GB-13)
  x <- list(master_wide = rd("master_wide", c("animal_id", "oft_distance")),
            oft = rd("oft", c("animal_id", "code", "raw_distance", "raw_speed")),
            epm = rd("epm", c("animal_id", "bodycentre_raw_distance", "bodycentre_raw_speed")),
            nor = rd("nor", c("animal_id", "distance", "speed_raw")),
            three_chamber = rd("socialPreference_long", c("animal_id", "phase", "distance", "speed_raw")),
            sleap_v2 = rd("master_wide_sleap", c("animal_id", "sleap_OFT_distance_cm", "sleap_EPM_distance_cm", "sleap_NOR_distance_cm")))
  for (nm in names(x)) s33_assert_outcome_free(x[[nm]], paste("workbook read", nm))
  x$sleap_v2_header <- names(suppressMessages(readxl::read_excel(wb, sheet = "master_wide_sleap", n_max = 0)))
  x
}
s33b_read_calibration_raw <- function(paths) list(geometry = data.table::fread(paths$sleap_geometry, showProgress = FALSE),
                                                  socp_config = readLines(paths$sleap_socp, warn = FALSE))
s33b_read_timeline_ids <- function(paths) as.character(s33_read_sheet_cols(paths$timeline, "Animals", "ID")$ID)

#' One row per canonical animal with the six arena distances and speeds (pure parser of the declared columns):
#' OFT distance = master_wide oft_distance, OFT speed = oft raw_speed; EPM body-centre raw distance and speed; NOR distance
#' and speed_raw; three-chamber HAB, S1, S2 distance and speed_raw. Rows without animal_id are dropped (their codes kept).
s33b_parse_arena <- function(raw, canon, ids) {
  ids <- sort(unique(ids), method = "radix")
  prep <- function(x) {
    x <- data.table::copy(data.table::as.data.table(x))
    x[, id_raw := trimws(as.character(animal_id))]
    na_id <- x[is.na(id_raw) | !nzchar(id_raw)]
    x <- x[!(is.na(id_raw) | !nzchar(id_raw))]
    x[, AnimalNum := canon(id_raw)]
    list(x = x[AnimalNum %in% ids], n_rows = nrow(x) + nrow(na_id), na_id = na_id,
         unmatched = sort(unique(x[!AnimalNum %in% ids, id_raw]), method = "radix"))
  }
  s <- lapply(raw[c("master_wide", "oft", "epm", "nor", "three_chamber", "sleap_v2")], prep)
  pick <- function(tab, col) as.numeric(tab[[col]][match(ids, tab$AnimalNum)])
  a <- data.table::data.table(AnimalNum = ids,
    oft_distance = pick(s$master_wide$x, "oft_distance"), oft_speed = pick(s$oft$x, "raw_speed"),
    epm_distance = pick(s$epm$x, "bodycentre_raw_distance"), epm_speed = pick(s$epm$x, "bodycentre_raw_speed"),
    nor_distance = pick(s$nor$x, "distance"), nor_speed = pick(s$nor$x, "speed_raw"))
  for (ph in c("HAB", "S1", "S2")) {
    z <- s$three_chamber$x[as.character(phase) == ph]
    data.table::set(a, j = paste0(tolower(ph), "_distance"), value = pick(z, "distance"))
    data.table::set(a, j = paste0(tolower(ph), "_speed"), value = pick(z, "speed_raw"))
  }
  v2 <- data.table::data.table(AnimalNum = ids, v2_oft_distance = pick(s$sleap_v2$x, "sleap_OFT_distance_cm"),
                               v2_epm_distance = pick(s$sleap_v2$x, "sleap_EPM_distance_cm"),
                               v2_nor_distance = pick(s$sleap_v2$x, "sleap_NOR_distance_cm"))
  dup <- vapply(names(s), function(nm) if (nm == "three_chamber") anyDuplicated(s[[nm]]$x[, list(AnimalNum, phase)]) > 0L else
    anyDuplicated(s[[nm]]$x$AnimalNum) > 0L, TRUE)
  oft_raw <- pick(s$oft$x, "raw_distance")
  both <- is.finite(oft_raw) & is.finite(a$oft_distance)
  sheet <- c(master_wide = "master_wide", oft = "oft", epm = "epm", nor = "nor", three_chamber = "socialPreference_long",
             sleap_v2 = "master_wide_sleap")
  audit <- data.table::rbindlist(lapply(names(s), function(nm) data.table::data.table(
    block = "workbook sheet", item = sheet[[nm]], n_rows = s[[nm]]$n_rows, n_rows_without_id = nrow(s[[nm]]$na_id),
    codes_without_id = paste(sort(as.character(s[[nm]]$na_id$code %s33or% character()), method = "radix"), collapse = ";"),
    n_matched = data.table::uniqueN(s[[nm]]$x$AnimalNum), unmatched_ids = paste(s[[nm]]$unmatched, collapse = ";"),
    duplicated_ids = dup[[nm]])))
  list(arena = a, sleap_v2 = v2, dropped_codes = sort(as.character(s$oft$na_id$code %s33or% character()), method = "radix"),
       duplicates = dup, oft_check = c(n_both = sum(both), n_equal = sum(abs(oft_raw[both] - a$oft_distance[both]) < 1e-9)),
       audit = audit)
}

#' GB-14b inputs (pure): whether per-recording calibration of the legacy three-chamber values can be verified. Recorded as
#' not documented when the SLEAP v2 workbook sheet carries no three-chamber distance, the v2 three-chamber config does not
#' process HAB, and the three-chamber box width in pixels varies by more than 5% across recordings.
s33b_three_chamber_calibration <- function(socp_config, geometry, sleap_v2_header) {
  ln <- trimws(socp_config); ln <- ln[!grepl("^#", ln)]
  ph <- grep("^phases[[:space:]]*:", ln, value = TRUE)
  phases <- if (length(ph)) trimws(strsplit(gsub("^phases[[:space:]]*:|[][]", "", ph[1]), ",", fixed = TRUE)[[1]]) else character()
  g <- data.table::as.data.table(geometry)[assay == "SocP" & is.finite(width_px)]
  wr <- if (nrow(g)) max(g$width_px) / min(g$width_px) else NA_real_
  checks <- c(no_v2_three_chamber_distance = !any(grepl("^sleap_SocP_.*distance", sleap_v2_header, ignore.case = TRUE)),
              hab_not_in_v2_phases = length(phases) > 0L && !"HAB" %in% phases,
              chamber_width_varies = isTRUE(wr > 1.05))
  list(documented = !all(checks), checks = checks, phases = phases, width_ratio = wr,
       width_range = if (nrow(g)) range(g$width_px) else c(NA_real_, NA_real_))
}

#' Per-cohort median ratio of the SLEAP v2 distance to the legacy distance (OFT, EPM, NOR) over every animal with both.
s33b_sleap_ratios <- function(arena, sleap_v2, batch_of) {
  x <- merge(arena[, list(AnimalNum, oft_distance, epm_distance, nor_distance)], sleap_v2, by = "AnimalNum")
  x[, Batch := unname(batch_of[AnimalNum])]
  data.table::rbindlist(lapply(c("oft", "epm", "nor"), function(k) {
    z <- data.table::data.table(Batch = x$Batch, ratio = x[[paste0("v2_", k, "_distance")]] / x[[paste0(k, "_distance")]])
    z[is.finite(ratio), list(n = .N, median_ratio = stats::median(ratio)), keyby = Batch][, measure := paste0(k, "_distance")][]
  }))[, list(measure, Batch, n, median_ratio)]
}

# ---------------------------------------------------------------- gates (plan section 9)
s33b_gate_gb12 <- function(a) {
  z <- data.table::data.table(Sex = a$Sex, wd = a$weight_dev,
                              dw = ifelse(a$Batch == "B6", a$tp6 - a$tp3, a$tp6 - a$tp2))[is.finite(wd) & is.finite(dw)]
  r <- z[, list(r = stats::cor(wd, dw), n = .N), keyby = Sex]
  s33_gate_rows("GB-12", "weight_dev z correlates above 0.9999 within sex with tp6 - tp2 (B1-B5) and tp6 - tp3 (B6)",
                c(nrow(r) == 2L, r$r > 0.9999), detail = paste(sprintf("%s r %.6f (n %d)", r$Sex, r$r, r$n), collapse = "; "))
}
s33b_gate_gb13 <- function(parsed, tracked_ids, sis_tracked_ids) {
  a <- parsed$arena; m <- S33B_ARENA
  n_all <- vapply(m$measure, function(v) sum(is.finite(a[[v]])), 1L)
  n_trk <- vapply(m$measure, function(v) sum(is.finite(a[[v]][a$AnimalNum %in% tracked_ids])), 1L)
  n_sis <- vapply(m$measure, function(v) sum(is.finite(a[[v]][a$AnimalNum %in% sis_tracked_ids])), 1L)
  dur <- vapply(seq_len(nrow(m)), function(i) { d <- a[[m$measure[i]]] / a[[m$speed[i]]]; d <- d[is.finite(d)]
    length(d) > 0L && mean(abs(d - m$nominal_s[i]) <= 45) >= 0.98 }, TRUE)
  s33_gate_rows("GB-13", paste("arena data: non-missing 113/117/117/116 over all and 107/111/111/110 over tracked animals",
                               "(OFT/EPM/NOR/three-chamber; arena-matched SIS 85/87/87/87); implied durations 600/300/300/600 s",
                               "within 45 s for >= 98%; OFT codes Y8G0, I1E6 dropped; one row per animal and sheet"),
                c(n_all == m$n_all, n_trk == m$n_tracked, n_sis == unname(S33B_ARENA_SIS_N[m$measure]), dur,
                  identical(parsed$dropped_codes, sort(S33B_OFT_DROPPED_CODES, method = "radix")), !parsed$duplicates),
                detail = sprintf("all %s; tracked %s; tracked SIS %s; dropped codes %s", paste(n_all, collapse = "/"),
                                 paste(n_trk, collapse = "/"), paste(n_sis, collapse = "/"), paste(parsed$dropped_codes, collapse = ",")))
}
s33b_gate_gb14a <- function(ratios) {
  s33_gate_rows("GB-14a", "SLEAP v2 / legacy per-cohort median ratio in [0.98, 1.02] for OFT, EPM and NOR",
                c(nrow(ratios) == 18L, ratios$median_ratio >= 0.98 & ratios$median_ratio <= 1.02),
                detail = sprintf("18 cohort x arena medians, range %.4f-%.4f", min(ratios$median_ratio), max(ratios$median_ratio)))
}
s33b_gate_gb14b <- function(cal) {
  s33_gate_rows("GB-14b", paste("recorded: per-recording calibration of the legacy three-chamber values not documented, so the",
                                "primary index excludes HAB and HAB, S1, S2 and index4 are flagged"),
                cal$checks, hard = FALSE,
                detail = sprintf("v2 three-chamber phases %s; chamber width %.0f-%.0f px (ratio %.3f)", paste(cal$phases, collapse = ","),
                                 cal$width_range[1], cal$width_range[2], cal$width_ratio))
}
s33b_cc1_pairs <- function(sis) sis[, if (.N > 1L) { a <- sort(AnimalNum, method = "radix"); data.table::as.data.table(t(utils::combn(a, 2L))) },
                                    by = cc1_cage]
s33b_gate_gb15 <- function(sis) {
  p <- s33b_cc1_pairs(sis); c4 <- function(id) sis$cc4_cage[match(id, sis$AnimalNum)]
  n_share <- if (nrow(p)) sum(c4(p$V1) == c4(p$V2)) else NA_integer_
  s33_gate_rows("GB-15", "1 of 123 CC1 SIS cage-mate pairs share a CC4 cage", c(nrow(p) == 123L, n_share == 1L),
                detail = sprintf("%d of %d pairs", n_share, nrow(p)))
}
s33b_gate_gb16 <- function(seeds) {
  k <- unname(seeds[paste0("B_sens_", unname(S33B_SENS_BOOT))]) - 33020200L
  s33_gate_rows("GB-16", "sensitivity bootstrap seeds 33020200 + k: indices k unique, 1 <= k < 100, as declared",
                c(length(k) == 12L, !anyNA(k), !anyDuplicated(k), all(k >= 1L & k < 100L), identical(as.integer(k), unname(S33B_SENS_BOOT))),
                detail = paste(k, collapse = ","))
}

# ---------------------------------------------------------------- analysis frame, populations and covariates
#' Outcome-free animal frame (117 rows): design columns, arena values, the CC1 recording start and lag of the cohort,
#' numeric sum-to-zero CC1 board columns sysE1-sysE4 (sys.5 = -1) and the log rate.
s33b_frame <- function(design, arena) {
  a <- merge(data.table::copy(design$animals), arena, by = "AnimalNum", all.x = TRUE)
  l1 <- design$lags[CC == "CC1", list(Batch, rec_start_cc1 = rec_start, lag_h_cc1 = lag_h)]
  a <- merge(a, l1, by = "Batch", all.x = TRUE)
  for (k in 1:4) data.table::set(a, j = paste0("sysE", k), value = as.numeric(a$cc1_board == paste0("sys.", k)) - as.numeric(a$cc1_board == "sys.5"))
  a[, log_rate := log(crossing_rate)]
  data.table::setorderv(a, "AnimalNum")
  a[]
}

#' Within-population covariates (plan 9, Variables): age_c and w_c centred within cohort; w_src = source-cage mean of the
#' weight column; w_within = weight - w_src (0 in a singleton source cage, e.g. OR568's); w_srcmean = w_src - cohort mean.
s33b_covariates <- function(x, wcol = "tp1") {
  x <- data.table::copy(data.table::as.data.table(x))
  data.table::set(x, j = "w_raw", value = as.numeric(x[[wcol]]))
  x[, `:=`(age_c = age_cc1 - mean(age_cc1), w_c = w_raw - mean(w_raw), w_coh = mean(w_raw)), by = Batch]
  x[, w_src := mean(w_raw), by = list(Batch, src_cage)]
  x[, `:=`(w_within = w_raw - w_src, w_srcmean = w_src - w_coh)]
  x[, c("w_raw", "w_coh") := NULL]
  x[]
}

#' SD_w: n - 1 SD of (x - cohort mean) over the cohorts in which x varies; over source cages for source-cage-level terms.
s33b_sd_w <- function(x, var, unit = c("animal", "source_cage")) {
  unit <- match.arg(unit)
  y <- if (unit == "animal") data.table::data.table(Batch = x$Batch, v = as.numeric(x[[var]])) else
    unique(data.table::data.table(Batch = x$Batch, src = x$src_cage, v = as.numeric(x[[var]])))[, list(Batch, v)]
  y <- y[is.finite(v)]
  vary <- y[, list(s = if (.N > 1L) stats::var(v) else 0), by = Batch][s > 1e-12, Batch]
  y <- y[Batch %in% vary]
  if (nrow(y) < 2L) return(NA_real_)
  s33_resid_sd(y$v, y$Batch)
}
s33b_sd_w_set <- function(x) c(age_c = s33b_sd_w(x, "age_c"), w_c = s33b_sd_w(x, "w_c"), w_within = s33b_sd_w(x, "w_within"),
                                w_srcmean = s33b_sd_w(x, "w_srcmean", "source_cage"))

#' Rows of one variant (cohort or animals dropped, one sex, B2 source cages coded unknown), covariates recomputed.
s33b_variant_rows <- function(x, v) {
  y <- data.table::copy(x)
  if (length(v$drop_batch)) y <- y[!Batch %in% v$drop_batch]
  if (length(v$sex)) y <- y[Sex == v$sex]
  if (length(v$drop_animals)) y <- y[!AnimalNum %in% v$drop_animals]
  if (isTRUE(v$b2_src_unknown)) y[Batch == "B2", src_cage := paste0("B2_unknown_", AnimalNum)]
  s33b_covariates(y, v$wcol %s33or% "tp1")
}

#' Model data: y = the outcome column (finite rows), Batch a factor in cohort order, CageEpisodeID = the cage column.
s33b_model_data <- function(x, yvar, cage = "cc1_cage") {
  md <- data.table::copy(x); data.table::set(md, j = "y", value = as.numeric(md[[yvar]]))
  md <- md[is.finite(y)]
  md[, Batch := factor(Batch, levels = intersect(S33_COHORTS, unique(Batch)))]
  data.table::set(md, j = "CageEpisodeID", value = md[[cage]])
  md[]
}

# ---------------------------------------------------------------- estimators (no p-value anywhere)
s33b_failed_est <- function(reason = "model FAILED") list(estimate = NA_real_, se = NA_real_, df = NA_real_, ci_low = NA_real_,
  ci_high = NA_real_, interval_method = "none", status = "FAILED", singular = NA, ddf_fallback = NA, failure_reason = reason)

#' KR row of one coefficient (frozen engine; Satterthwaite fallback flagged; FAILED rows kept).
s33b_est_lmer <- function(m, term, estimand) {
  kc <- s33_kr_contrast(m, stats::setNames(list(1), term), estimand)
  list(estimate = kc$estimate, se = kc$se, df = kc$df, ci_low = kc$ci_low, ci_high = kc$ci_high,
       interval_method = if (kc$status == "OK") kc$interval_method else "none", status = kc$status, singular = kc$singular,
       ddf_fallback = kc$ddf_fallback, failure_reason = kc$failure_reason %s33or% NA_character_)
}
#' OLS through the frozen mmm_ci_fit(engine = 'lm') with the declared rank; t interval with the residual df.
s33b_fit_lm <- function(formula, md, model_id, rank) {
  m <- tryCatch(mmm_ci_fit(formula, md, model_id, engine = "lm", expected_rank = rank), error = function(e) e)
  if (inherits(m, "error")) return(list(fit = NULL, info = data.table::data.table(failed = TRUE, converged = FALSE, singular = NA,
                                                                                  messages = ""), failure_reason = conditionMessage(m)))
  m$failure_reason <- if (isTRUE(m$info$failed)) paste("fit error:", m$info$error) else NA_character_; m
}
s33b_est_lm <- function(m, term) {
  if (is.null(m$fit) || isTRUE(m$info$failed)) return(s33b_failed_est(m$failure_reason %s33or% "model FAILED"))
  b <- stats::coef(m$fit); V <- stats::vcov(m$fit); d <- stats::df.residual(m$fit)
  e <- unname(b[[term]]); se <- sqrt(V[term, term]); ci <- s33_t_interval(e, se, d)
  list(estimate = e, se = se, df = d, ci_low = ci$ci_low, ci_high = ci$ci_high, interval_method = paste0("t_", d),
       status = "OK", singular = NA, ddf_fallback = NA, failure_reason = NA_character_)
}
#' glmmTMB with dispformula ~ Batch (S11) through the frozen mmm_ci_fit() (engine failure rule; FAILED rows kept).
s33b_fit_tmb <- function(formula, md, model_id, rank) {
  m <- tryCatch(mmm_ci_fit(formula, md, model_id, engine = "glmmTMB", dispformula = "~ Batch", expected_rank = rank),
                error = function(e) e)
  if (inherits(m, "error")) return(list(fit = NULL, info = data.table::data.table(failed = TRUE, converged = FALSE, singular = NA,
                                                                                  messages = ""), failure_reason = conditionMessage(m)))
  m$failure_reason <- if (isTRUE(m$info$failed)) "glmmTMB fit FAILED (engine failure rule)" else NA_character_
  m
}
#' S11 slope: the frozen engine's glmmTMB Wald row (mmm_ci_glmmtmb_tests); its p-value, statistic and test label are
#' dropped at source. The section 7 vocabulary has no normal-theory name, so the Wald interval is 't_Inf' (t_<df>, df Inf).
s33b_est_tmb <- function(m, term) {
  if (mmm_ci_fit_failed(m)) return(s33b_failed_est(m$failure_reason %s33or% "model FAILED"))
  w <- mmm_ci_glmmtmb_tests(m, one = stats::setNames(list(1), term))$one
  w[, intersect(c("p_raw", "statistic", "test"), names(w)) := NULL]
  list(estimate = w$estimate, se = w$se, df = Inf, ci_low = w$ci_low, ci_high = w$ci_high, interval_method = "t_Inf", status = "OK",
       singular = NA, ddf_fallback = NA, failure_reason = NA_character_)
}
#' CR2 by source cage for beta_J (E5). PLAN ISSUE: clubSandwich refuses lmer fits with crossed random effects
#' ('Non-nested random effects detected'), so the CR2 interval clustered by source cage is computed on the OLS fixed part
#' of the same model (working independence), with Satterthwaite df and no p-value.
s33b_est_cr2_src <- function(rhs, md, term) {
  fit <- stats::lm(stats::as.formula(paste("y ~", rhs)), data = md)
  if (anyNA(stats::coef(fit))) return(s33b_failed_est("OLS working model rank-deficient"))
  lc <- tryCatch(s33_cr2_contrast(fit, md$src_cage, list(stats::setNames(1, term))), error = function(e) e)
  if (inherits(lc, "error")) return(s33b_failed_est(paste("CR2 failed:", conditionMessage(lc))))
  list(estimate = lc$estimate, se = lc$se, df = lc$df, ci_low = lc$ci_low, ci_high = lc$ci_high,
       interval_method = "CR2_Satterthwaite_src_cage", status = "OK", singular = NA, ddf_fallback = NA, failure_reason = NA_character_)
}

# ---------------------------------------------------------------- E3-E6 associations (b05)
s33b_estimand <- function(form, term, wlab = "tp1") {
  joint <- form %in% c("E3_joint", "E3_split")
  switch(term,
         age_c = if (joint) paste0("age_at_cc1_adjusted_for_", wlab) else "age_at_cc1",
         w_c = if (joint) paste0(wlab, "_weight_adjusted_for_age") else paste0(wlab, "_weight"),
         w_within = "within_source_cage",
         w_srcmean = "between_source_cages_within_cohort",
         line_J = if (form == "E5_line") "substrain_c57bl6j" else "substrain_c57bl6j_adjusted_for_age_and_weight")
}
s33b_term_label <- function(form, term, wlab = "tp1", n_j = NA_integer_, n_j_src = NA_integer_) {
  joint <- form %in% c("E3_joint", "E3_split")
  wl <- if (wlab == "tp1") "tp1 weight" else "CC1-day weight (tp2)"
  ww <- if (wlab == "tp1") "weight" else "CC1-day weight (tp2)"      # tp1 rows keep the plan's wording verbatim
  switch(term,
         age_c = if (joint) paste0("age at CC1, adjusted for ", wl) else "age at CC1 (one covariate)",
         w_c = if (joint) paste0(wl, ", adjusted for age") else paste0(wl, " (one covariate)"),
         w_within = paste(ww, "relative to source-cage mates (age-free)"),
         w_srcmean = if (form == "E3_split") paste0("source-cage mean ", ww, ", adjusted for age") else paste0("source-cage mean ", ww),
         line_J = sprintf(S33B_LABEL$substrain, n_j, n_j_src, n_j_src))
}
s33b_units_y <- function(outcome) switch(outcome, crossing_rate = "position changes/hour", log_rate = "log(position changes/hour)",
                                         "CON-SD units")
s33b_units_term <- function(outcome, term) paste0(s33b_units_y(outcome),
  switch(term, age_c = " per day of age", w_c = , w_within = , w_srcmean = " per g", line_J = " (C57BL/6J minus C57BL/6JRj)"))

#' The association jobs of one phase: outcome, population, form and variant (plan 9 E3-E6 and S1-S11).
s33b_assoc_jobs <- function(kind = c("rate", "cz")) {
  kind <- match.arg(kind)
  lobo <- lapply(S33_COHORTS, function(b) list(id = paste0("S7_without_", b), label = paste("S7 leave-one-cohort-out, without", b),
                                               drop_batch = b))
  common <- c(list(list(id = "primary", label = "primary"),
                   list(id = "S1", label = "S1 B2 excluded", drop_batch = "B2"),
                   list(id = "S2", label = "S2 B2 source cages coded unknown (singleton levels)", b2_src_unknown = TRUE),
                   list(id = "S4a", label = "S4a source-cage random effect only", re = S33B_RE[["S4a"]]),
                   list(id = "S4b", label = "S4b CC1-cage random effect only", re = S33B_RE[["S4b"]]),
                   list(id = "S4c", label = "S4c OLS without random effects", engine = "lm")),
              lobo,
              list(list(id = "S8_male", label = "S8 males (B1, B2, B5)", sex = "Male"),
                   list(id = "S8_female", label = "S8 females (B3, B4, B6)", sex = "Female")))
  jobs <- list(); forms <- S33B_FORMS$form
  add <- function(outcome, pop, v, fs) for (f in fs) jobs[[length(jobs) + 1L]] <<- c(list(outcome = outcome, population = pop, form = f), v)
  if (kind == "rate") {
    for (v in common) add("crossing_rate", "sis_87", v, forms)
    add("log_rate", "sis_87", list(id = "S5", label = "S5 log rate"), forms)
    add("crossing_rate", "sis_87", list(id = "S6", label = "S6 CC1-day weight (tp2) in place of tp1", wcol = "tp2"),
        c("E3_joint", "E3_split", "E4_weight", "E4_split"))
    add("crossing_rate", "sis_87", list(id = "S9", label = "S9 without animal 692", drop_animals = "692"), forms)
    add("crossing_rate", "sis_87", list(id = "S11", label = "S11 residual variance by cohort (glmmTMB, dispformula ~ Batch)",
                                        engine = "glmmTMB"), forms)
  } else {
    for (y in S33B_CZ_OUTCOMES) {
      for (v in common) add(y, "sis_93", v, forms)
      add(y, "sis_87", list(id = "S3", label = "S3 the 87 tracked SIS"), forms)
    }
    for (v in common[1:3]) add("weight_dev", "sis_93", v, c("E3_joint", "E3_split"))
    add("weight_dev", "sis_87", list(id = "S3", label = "S3 the 87 tracked SIS"), c("E3_joint", "E3_split"))
  }
  jobs
}

#' Run one association job: fit, one row per reported term (KR, t or glmmTMB), plus the E5 CR2-by-source-cage row (primary).
#' `sdw` = the SD_w set of the weight column used (plan section 7: n - 1 residual SD on cohort in sis_87); weight_dev rows
#' (E6) carry the estimand group E6.
s33b_run_assoc <- function(frame, job, sdw, computed_in) {
  base <- frame[frame[[paste0("pop_", job$population)]] %in% TRUE]
  x <- s33b_variant_rows(base, job)
  fm <- S33B_FORMS[form == job$form]
  grp <- if (identical(job$outcome, "weight_dev")) "E6" else fm$estimand_group
  md <- s33b_model_data(x, job$outcome)
  rank <- fm$rank - (6L - nlevels(md$Batch))
  engine <- job$engine %s33or% "lmer"; re <- job$re %s33or% S33B_RE[["primary"]]
  wlab <- job$wcol %s33or% "tp1"
  model_id <- paste("S33B", grp, job$form, job$outcome, job$population, job$id, sep = "|")
  formula <- if (engine == "lm") paste("y ~", fm$rhs) else paste("y ~", fm$rhs, "+", re)
  m <- switch(engine, lmer = s33_kr_fit(formula, md, model_id, rank), lm = s33b_fit_lm(formula, md, model_id, rank),
              glmmTMB = s33b_fit_tmb(formula, md, model_id, rank))
  terms <- strsplit(fm$terms, ";", fixed = TRUE)[[1]]
  is_rate <- job$outcome %in% c("crossing_rate", "log_rate")
  lead <- job$id == "primary" && fm$estimand_group %in% c("E3", "E5") && job$outcome %in% c("crossing_rate", S33B_CZ_OUTCOMES)
  n_j <- sum(md$line_J == 1L); n_j_src <- data.table::uniqueN(md$src_cage[md$line_J == 1L])
  r_aw <- if (stats::sd(md$age_c) > 0) stats::cor(md$age_c, md$w_c) else NA_real_
  r_as <- if (stats::sd(md$age_c) > 0) stats::cor(md$age_c, md$w_srcmean) else NA_real_
  one <- function(term, e, extra_note = NA_character_) {
    sd_w <- if (term %in% names(sdw)) unname(sdw[[term]]) else NA_real_
    note <- c(if (term == "age_c" && fm$estimand_group == "E4") S33B_LABEL$e4_age,
              if (term %in% c("w_c", "w_within", "w_srcmean") && is_rate) S33B_LABEL$geometry,
              if (term %in% c("w_c", "w_within", "w_srcmean") && !is_rate) unname(S33B_LABEL$coupling[job$outcome]),
              if (term == "line_J") S33B_LABEL$e5, if (!is.na(extra_note)) extra_note)
    s33_row(estimand = s33b_estimand(job$form, term, wlab), level = S33B_TERM_LEVEL[[term]], estimate = e$estimate, se = e$se,
            df = e$df, ci_low = e$ci_low, ci_high = e$ci_high, interval_method = e$interval_method,
            interval_basis = if (e$interval_method == "none") NA_character_ else "conditional_on_cohorts",
            units = s33b_units_term(job$outcome, term), metric_id = if (is_rate) "crossing_rate" else NA_character_,
            status = e$status, singular = e$singular, ddf_fallback = e$ddf_fallback, lead = lead, n_animals = nrow(md),
            n_cages = data.table::uniqueN(md$cc1_cage), n_cohorts = nlevels(md$Batch), n_params = rank,
            estimand_group = grp, form = job$form, outcome = job$outcome, population = job$population,
            variant = job$id, variant_label = job$label, term = term,
            term_label = s33b_term_label(job$form, term, wlab, n_j, n_j_src), model_id = model_id, formula = formula,
            engine = if (e$interval_method == "CR2_Satterthwaite_src_cage") "lm (OLS working model, CR2)" else engine,
            n_source_cages = data.table::uniqueN(md$src_cage), n_cc1_cages = data.table::uniqueN(md$cc1_cage),
            sd_w = sd_w, sd_w_unit = if (term %in% names(S33B_TERM_SD_UNIT)) unname(S33B_TERM_SD_UNIT[[term]]) else NA_character_,
            # the same interval re-expressed per SD_w (named lower / upper so that it is not counted as a second interval)
            estimate_per_sd_w = e$estimate * sd_w, lower_per_sd_w = e$ci_low * sd_w, upper_per_sd_w = e$ci_high * sd_w,
            units_per_sd_w = if (is.finite(sd_w)) sprintf("%s per SD_w (%.4g %s; n - 1 residual SD on cohort in sis_87)",
                                                           s33b_units_y(job$outcome), sd_w, if (term == "age_c") "d" else "g") else NA_character_,
            r_age_c_w_c = r_aw, r_age_c_w_srcmean = r_as,
            mech_coupling = if (is_rate) NA_character_ else unname(S33B_MECH_COUPLING[[job$outcome]]),
            note = if (length(note)) paste(note, collapse = "; ") else NA_character_,
            converged = if (is.null(m$info$converged)) NA else as.logical(m$info$converged[1]),
            failure_reason = e$failure_reason %s33or% NA_character_,
            engine_messages = if (is.null(m$info$messages)) NA_character_ else as.character(m$info$messages[1]),
            computed_in = computed_in)
  }
  rows <- lapply(terms, function(tm) {
    est_name <- s33b_estimand(job$form, tm, wlab)
    e <- switch(engine, lmer = s33b_est_lmer(m, tm, est_name), lm = s33b_est_lm(m, tm), glmmTMB = s33b_est_tmb(m, tm))
    one(tm, e)
  })
  if (fm$estimand_group == "E5" && job$id == "primary")
    rows <- c(rows, list(one("line_J", s33b_est_cr2_src(fm$rhs, md, "line_J"), "CR2 by source cage on the OLS fixed part")))
  data.table::rbindlist(rows)
}

#' S12: CON-only lm(rate ~ Batch + w_within_cage) in the 24 CON (6 cages, df 17); w_within_cage = tp1 - CON cage mean.
s33b_s12 <- function(frame, computed_in) {
  con <- data.table::copy(frame[pop_con_24 %in% TRUE & is.finite(crossing_rate)])
  con[, w_within_cage := tp1 - mean(tp1), by = cc1_cage]
  md <- s33b_model_data(con, "crossing_rate")
  m <- s33b_fit_lm("y ~ Batch + w_within_cage", md, "S33B|S12|crossing_rate|con_24", 7L)
  e <- s33b_est_lm(m, "w_within_cage")
  s33_row(estimand = "tp1_weight_within_con_cage", level = "animal_within_cohort", estimate = e$estimate, se = e$se, df = e$df,
          ci_low = e$ci_low, ci_high = e$ci_high, interval_method = e$interval_method,
          interval_basis = if (e$interval_method == "none") NA_character_ else "conditional_on_cohorts",
          units = "position changes/hour per g", metric_id = "crossing_rate", status = e$status, lead = FALSE,
          n_animals = nrow(md), n_cages = data.table::uniqueN(md$cc1_cage), n_cohorts = nlevels(md$Batch), n_params = 7L,
          estimand_group = "S12", form = "S12_con_ols", outcome = "crossing_rate", population = "con_24", variant = "S12",
          variant_label = "S12 CON only, OLS (no cage effect: aliased with cohort)", term = "w_within_cage",
          term_label = "tp1 weight relative to CON cage mates", model_id = "S33B|S12|crossing_rate|con_24",
          formula = "y ~ Batch + w_within_cage", engine = "lm", n_source_cages = data.table::uniqueN(md$src_cage),
          n_cc1_cages = data.table::uniqueN(md$cc1_cage), sd_w = NA_real_, sd_w_unit = NA_character_, estimate_per_sd_w = NA_real_,
          lower_per_sd_w = NA_real_, upper_per_sd_w = NA_real_, units_per_sd_w = NA_character_, r_age_c_w_c = NA_real_,
          r_age_c_w_srcmean = NA_real_, mech_coupling = NA_character_,
          note = paste(c(if (e$interval_method != "none") S33B_LABEL$con_flag, if (e$interval_method == "none") S33B_LABEL$con else S33B_LABEL$con_ci,
                         S33B_LABEL$geometry), collapse = "; "),
          converged = if (is.null(m$fit)) FALSE else TRUE, failure_reason = e$failure_reason,
          engine_messages = if (is.null(m$info$messages)) NA_character_ else as.character(m$info$messages[1]),
          computed_in = computed_in)
}

# ---------------------------------------------------------------- E1, E2 variance shares (b04)
#' Variance components by group (lme4 names) and v_fix = n-divisor variance of the fitted fixed part X b.
s33b_varcomp <- function(fit) {
  vc <- as.data.frame(lme4::VarCorr(fit))
  xb <- drop(lme4::getME(fit, "X") %*% lme4::fixef(fit))
  c(stats::setNames(vc$vcov, vc$grp), v_fix = mean((xb - mean(xb))^2))
}
#' Parametric bootstrap (bootMer, type 'parametric', use.u = FALSE) of the variance components: one set.seed per stream
#' (Mersenne-Twister / Inversion / Rejection). Returns the B x components replicate matrix (failed refits NA).
s33b_pboot <- function(fit, comps, B, seed) {
  old <- RNGkind(); on.exit(do.call(RNGkind, as.list(old)), add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(seed)
  b <- suppressMessages(suppressWarnings(lme4::bootMer(fit, function(f) s33b_varcomp(f)[comps], nsim = B,
                                                       type = "parametric", use.u = FALSE)))
  matrix(as.numeric(b$t), ncol = length(comps), dimnames = list(NULL, comps))
}
#' Profile-likelihood 95% limits of every variance component, squared to the variance scale (lme4 names).
s33b_profile <- function(fit) {
  w <- character()
  pr <- tryCatch(withCallingHandlers(suppressMessages(stats::confint(fit, parm = "theta_", method = "profile", signames = FALSE,
                                                                     level = 0.95)),
                                     warning = function(x) { w <<- c(w, conditionMessage(x)); invokeRestart("muffleWarning") }),
                 error = function(e) e)
  if (inherits(pr, "error")) return(list(tab = data.table::data.table(group = character(), var_low = numeric(), var_high = numeric()),
                                         status = paste("profile failed:", conditionMessage(pr)), messages = paste(unique(w), collapse = " | ")))
  rn <- rownames(pr)
  grp <- ifelse(rn == "sigma", "Residual", sub("^sd_[(]Intercept[)][|]", "", rn))
  list(tab = data.table::data.table(group = grp, var_low = pr[, 1]^2, var_high = pr[, 2]^2), status = "OK",
       messages = paste(unique(w), collapse = " | "))
}
#' Shares from variance components (vector or replicate matrix): within-cohort shares of every component and s_cohort.
s33b_shares <- function(V, comps) {
  V <- if (is.null(dim(V))) matrix(V, nrow = 1L, dimnames = list(NULL, names(V))) else V
  tot <- rowSums(V[, comps, drop = FALSE])
  out <- V[, comps, drop = FALSE] / tot
  cbind(out, s_cohort = if ("v_fix" %in% colnames(V)) V[, "v_fix"] / (V[, "v_fix"] + tot) else NA_real_)
}

#' The share models of one phase (plan 9 E1, E2 and their sensitivities; bootstrap stream ids of S33B_SENS_BOOT).
s33b_share_jobs <- function(kind = c("rate", "cz")) {
  kind <- match.arg(kind)
  j <- function(...) list(...)
  if (kind == "rate") list(
    j(id = "primary", label = "primary", outcome = "crossing_rate", population = "sis_87", group = "E1", boot = "E1_rate",
      seed_name = "B_e1_rate", B_name = "parametric", cohort_share = TRUE, lead = TRUE),
    j(id = "S1", label = "S1 B2 excluded", outcome = "crossing_rate", population = "sis_87", group = "E1", drop_batch = "B2",
      boot = "S1_rate", cohort_share = TRUE),
    j(id = "S2", label = "S2 B2 source cages coded unknown (singleton levels)", outcome = "crossing_rate", population = "sis_87",
      group = "E1", b2_src_unknown = TRUE, cohort_share = TRUE),
    j(id = "S4a", label = "S4a source-cage random effect only", outcome = "crossing_rate", population = "sis_87", group = "E1",
      re = S33B_RE[["S4a"]], cohort_share = TRUE),
    j(id = "S4b", label = "S4b CC1-cage random effect only", outcome = "crossing_rate", population = "sis_87", group = "E1",
      re = S33B_RE[["S4b"]], cohort_share = TRUE),
    j(id = "S5", label = "S5 log rate", outcome = "log_rate", population = "sis_87", group = "E1", boot = "S5_log_rate", cohort_share = TRUE),
    j(id = "S9", label = "S9 without animal 692", outcome = "crossing_rate", population = "sis_87", group = "E1", drop_animals = "692",
      boot = "S9_rate", cohort_share = TRUE),
    j(id = "S11", label = "S11 residual variance by cohort (glmmTMB, dispformula ~ Batch)", outcome = "crossing_rate",
      population = "sis_87", group = "E1", engine = "glmmTMB"),
    j(id = "S13", label = "S13 numeric sum-to-zero CC1 board columns", outcome = "crossing_rate", population = "sis_87", group = "E1",
      rhs = "Batch + sysE1 + sysE2 + sysE3 + sysE4", rank = 10L, boot = "S13_rate"),
    j(id = "S14", label = "S14 age_c + w_c fixed", outcome = "crossing_rate", population = "sis_87", group = "E1",
      rhs = "Batch + age_c + w_c", rank = 8L, boot = "S14_rate"))
  else list(
    j(id = "primary", label = "primary", outcome = "CombZ", population = "sis_93", group = "E2", boot = "E2_CombZ",
      seed_name = "B_e2_cz", B_name = "parametric", cohort_share = TRUE, lead = TRUE, icc = TRUE),
    j(id = "S1", label = "S1 B2 excluded", outcome = "CombZ", population = "sis_93", group = "E2", drop_batch = "B2",
      boot = "S1_CombZ", cohort_share = TRUE, icc = TRUE),
    j(id = "S2", label = "S2 B2 source cages coded unknown (singleton levels)", outcome = "CombZ", population = "sis_93",
      group = "E2", b2_src_unknown = TRUE, cohort_share = TRUE),
    j(id = "S3", label = "S3 the 87 tracked SIS", outcome = "CombZ", population = "sis_87", group = "E2", boot = "S3_CombZ",
      cohort_share = TRUE, icc = TRUE),
    j(id = "S4a", label = "S4a source-cage random effect only", outcome = "CombZ", population = "sis_93", group = "E2",
      re = S33B_RE[["S4a"]], cohort_share = TRUE),
    j(id = "S4b", label = "S4b CC1-cage random effect only", outcome = "CombZ", population = "sis_93", group = "E2",
      re = S33B_RE[["S4b"]], cohort_share = TRUE),
    j(id = "S10_noWD", label = "S10 CombZ_noWD", outcome = "CombZ_noWD", population = "sis_93", group = "E2",
      boot = "S10_CombZ_noWD", cohort_share = TRUE, icc = TRUE),
    j(id = "S10_noBW", label = "S10 CombZ_noBW", outcome = "CombZ_noBW", population = "sis_93", group = "E2",
      boot = "S10_CombZ_noBW", cohort_share = TRUE, icc = TRUE),
    j(id = "S14", label = "S14 age_c + w_c fixed", outcome = "CombZ", population = "sis_93", group = "E2",
      rhs = "Batch + age_c + w_c", rank = 8L, boot = "S14_CombZ"),
    j(id = "S15_sis_87", label = "S15 + CC4 cage (87 tracked SIS)", outcome = "CombZ", population = "sis_87", group = "E2",
      re = paste(S33B_RE[["primary"]], "+ (1 | cc4_cage)"), boot = "S15_sis_87", cohort_share = TRUE),
    j(id = "S15_sis_93", label = "S15 + CC4 cage (93 SIS; planned CC4 cages for the 6 untracked B1 SIS)", outcome = "CombZ",
      population = "sis_93", group = "E2", re = paste(S33B_RE[["primary"]], "+ (1 | cc4_cage)"), boot = "S15_sis_93",
      cohort_share = TRUE))
}

#' One b04 row (declared columns plus the module's share columns).
s33b_b04_row <- function(sj, estimand, level, estimate, ci_low, ci_high, interval_method, units, comp, comp_label, md, rank,
                         model_id, formula, m, status = "OK", variance = NA_real_, boundary = NA_character_, note = NA_character_,
                         boot = list(), prof = list(), engine = "lmer", resid_by_cohort = NA_character_, computed_in) {
  rate <- sj$outcome %in% c("crossing_rate", "log_rate")
  s33_row(estimand = estimand, level = level, estimate = estimate, ci_low = ci_low, ci_high = ci_high,
          interval_method = interval_method, interval_basis = if (interval_method == "none") NA_character_ else "conditional_on_cohorts",
          units = units, metric_id = if (rate) "crossing_rate" else NA_character_, status = status,
          singular = if (is.null(m$info$singular)) NA else m$info$singular[1], lead = isTRUE(sj$lead) && grepl("^variance_share", estimand),
          n_animals = nrow(md), n_cages = data.table::uniqueN(md$cc1_cage), n_cohorts = nlevels(md$Batch), n_params = rank,
          estimand_group = sj$group, outcome = sj$outcome, population = sj$population, variant = sj$id, variant_label = sj$label,
          model_id = model_id, formula = formula, engine = engine, component = comp, component_label = comp_label,
          variance = variance, boundary_statement = boundary, interval_note = note,
          boot_stream = boot$stream %s33or% NA_character_, boot_seed = boot$seed %s33or% NA_integer_, boot_B = boot$B %s33or% NA_integer_,
          boot_n_success = boot$n_success %s33or% NA_integer_, boot_prop_boundary = boot$prop_boundary %s33or% NA_real_,
          profile_status = prof$status %s33or% NA_character_, profile_messages = prof$messages %s33or% NA_character_,
          n_source_cages = data.table::uniqueN(md$src_cage), n_cc1_cages = data.table::uniqueN(md$cc1_cage),
          n_cc4_cages = data.table::uniqueN(md$cc4_cage),
          converged = if (is.null(m$info$converged)) NA else as.logical(m$info$converged[1]),
          failure_reason = m$failure_reason %s33or% NA_character_,
          engine_messages = if (is.null(m$info$messages)) NA_character_ else as.character(m$info$messages[1]),
          residual_variance_by_cohort = resid_by_cohort, computed_in = computed_in)
}

#' Run one share model: lmer (frozen engine) with parametric-bootstrap and profile limits, or glmmTMB (S11, no interval).
#' Returns list(rows, boot = bootstrap summary row or NULL).
s33b_run_share <- function(frame, sj, ctx, log, computed_in) {
  base <- frame[frame[[paste0("pop_", sj$population)]] %in% TRUE]
  x <- s33b_variant_rows(base, sj)
  md <- s33b_model_data(x, sj$outcome)
  rhs <- sj$rhs %s33or% "Batch"; re <- sj$re %s33or% S33B_RE[["primary"]]
  rank <- (sj$rank %s33or% 6L) - (6L - nlevels(md$Batch))
  model_id <- paste("S33B", sj$group, "shares", sj$outcome, sj$population, sj$id, sep = "|")
  formula <- paste("y ~", rhs, "+", re)
  groups <- regmatches(re, gregexpr("[|] *[A-Za-z0-9_]+", re))[[1]]; groups <- trimws(sub("^[|]", "", groups))
  comps <- c(groups, "Residual"); cid <- unname(S33B_COMPONENT[comps])
  rate <- sj$outcome %in% c("crossing_rate", "log_rate")
  vunits <- switch(sj$outcome, crossing_rate = "(position changes/hour)^2", log_rate = "(log position changes/hour)^2", "(CON-SD units)^2")
  rem_label <- if (sj$group == "E1") S33B_LABEL$remaining_rate else S33B_LABEL$remaining_cz
  clab <- function(k) switch(k, source_cage = S33B_LABEL$source_cage, cc1_cage = S33B_LABEL$cc1_cage, cc4_cage = S33B_LABEL$cc4_cage,
                             remaining = rem_label)
  if (identical(sj$engine, "glmmTMB")) {
    m <- s33b_fit_tmb(formula, md, model_id, rank)
    if (mmm_ci_fit_failed(m)) {
      rows <- lapply(cid, function(k) s33b_b04_row(sj, paste0("variance_share_", k), S33B_COMPONENT_LEVEL[[k]], NA_real_, NA_real_, NA_real_,
                                                   "none", "share of within-cohort variance", k, clab(k), md, rank, model_id, formula, m,
                                                   status = "FAILED", engine = "glmmTMB", computed_in = computed_in))
      return(list(rows = data.table::rbindlist(rows), boot = NULL))
    }
    vc <- glmmTMB::VarCorr(m$fit)$cond
    disp2 <- stats::predict(m$fit, type = "disp")^2
    v <- c(vapply(groups, function(g) unname(attr(vc[[g]], "stddev"))^2, 0), Residual = mean(disp2))
    rbc <- tapply(disp2, as.character(md$Batch), mean)
    rbc_txt <- paste(sprintf("%s %.4g", names(rbc), rbc), collapse = "; ")
    sh <- v / sum(v)
    rows <- lapply(seq_along(comps), function(i) s33b_b04_row(sj, paste0("variance_share_", cid[i]), S33B_COMPONENT_LEVEL[[cid[i]]],
      unname(sh[i]), NA_real_, NA_real_, "none", "share of within-cohort variance", cid[i], clab(cid[i]), md, rank, model_id,
      formula, m, variance = unname(v[i]), engine = "glmmTMB", resid_by_cohort = if (cid[i] == "remaining") rbc_txt else NA_character_,
      note = if (cid[i] == "remaining") "n-weighted mean of the per-cohort residual variances (squared glmmTMB dispersion SD)" else NA_character_,
      computed_in = computed_in))
    return(list(rows = data.table::rbindlist(rows), boot = NULL))
  }
  m <- s33_kr_fit(formula, md, model_id, rank)
  if (s32i_failed(m)) {
    rows <- lapply(cid, function(k) s33b_b04_row(sj, paste0("variance_share_", k), S33B_COMPONENT_LEVEL[[k]], NA_real_, NA_real_, NA_real_,
                                                 "none", "share of within-cohort variance", k, clab(k), md, rank, model_id, formula, m,
                                                 status = "FAILED", computed_in = computed_in))
    return(list(rows = data.table::rbindlist(rows), boot = NULL))
  }
  fit <- methods::as(m$fit, "lmerMod")
  v <- s33b_varcomp(fit)[c(comps, "v_fix")]
  sh <- s33b_shares(v, comps)[1, ]
  reps <- NULL; bmeta <- list(); bsum <- NULL
  if (!is.null(sj$boot)) {
    seed_name <- sj$seed_name %s33or% paste0("B_sens_", S33B_SENS_BOOT[[sj$boot]])
    seed <- unname(ctx$seeds[[seed_name]]); B <- unname(ctx$B[[sj$B_name %s33or% "parametric_sensitivity"]])
    key <- list(frame = s33b_sha(md[, unique(c("AnimalNum", "y", "Batch", groups, all.vars(stats::as.formula(paste("~", rhs))))),
                                    with = FALSE]), formula = formula, seed = seed, B = B)
    reps <- s33b_ckpt(ctx, paste0("pboot_", sj$boot), function() s33b_pboot(fit, c(comps, "v_fix"), B, seed), key, log)
    ok <- stats::complete.cases(reps)
    bmeta <- list(stream = sj$boot, seed = seed, B = B, n_success = sum(ok))
    bsum <- data.table::data.table(stream_id = sj$boot, kind = "parametric_bootstrap", seed_name = seed_name, seed = seed, B = B,
      rng = S33B_RNG, unit_order = "parametric refits of the fitted model (use.u = FALSE)",
      cells = NA_character_, model_id = model_id, n_success = sum(ok), finite_share = mean(ok),
      replicate_sha256 = s33b_sha(reps), index_sha256 = NA_character_, statistics = paste(colnames(reps), collapse = ";"),
      computed_in = computed_in)
  }
  prof <- s33b_profile(fit)
  tot <- sum(v[comps]); bound <- v[comps] < 1e-8 * tot
  rs <- if (!is.null(reps)) { ok <- stats::complete.cases(reps); s33b_shares(reps[ok, , drop = FALSE], comps) } else NULL
  rows <- list()
  for (i in seq_along(comps)) {
    k <- cid[i]; at0 <- isTRUE(bound[i]) && comps[i] != "Residual"
    pl <- prof$tab[group == comps[i]]
    bnd <- if (at0) sprintf("estimated at 0; upper 95%% limit %s %s (profile likelihood)",
                            if (nrow(pl)) format(signif(pl$var_high, 4)) else "not available", vunits) else NA_character_
    pc <- if (!is.null(rs)) s33_percentile(rs[, comps[i]]) else list(lo = NA_real_, hi = NA_real_)
    pb <- if (!is.null(rs)) mean(reps[stats::complete.cases(reps), comps[i]] < 1e-8 * rowSums(reps[stats::complete.cases(reps), comps, drop = FALSE])) else NA_real_
    rows[[length(rows) + 1L]] <- s33b_b04_row(sj, paste0("variance_share_", k), S33B_COMPONENT_LEVEL[[k]], unname(sh[comps[i]]), pc$lo, pc$hi,
      if (is.null(rs)) "none" else if (at0) "parametric_bootstrap_spread" else "parametric_bootstrap_percentile",
      "share of within-cohort variance", k, clab(k), md, rank, model_id, formula, m, variance = unname(v[comps[i]]), boundary = bnd,
      note = if (at0 && !is.null(rs)) S33B_LABEL$spread else NA_character_, boot = c(bmeta, list(prop_boundary = pb)), prof = prof,
      computed_in = computed_in)
    rows[[length(rows) + 1L]] <- s33b_b04_row(sj, paste0("variance_component_", k), S33B_COMPONENT_LEVEL[[k]], unname(v[comps[i]]),
      if (nrow(pl)) pl$var_low else NA_real_, if (nrow(pl)) pl$var_high else NA_real_, if (nrow(pl)) "profile_likelihood" else "none",
      vunits, k, clab(k), md, rank, model_id, formula, m, variance = unname(v[comps[i]]), boundary = bnd, prof = prof,
      computed_in = computed_in)
  }
  if (isTRUE(sj$cohort_share)) {
    pc <- if (!is.null(rs)) s33_percentile(rs[, "s_cohort"]) else list(lo = NA_real_, hi = NA_real_)
    rows[[length(rows) + 1L]] <- s33b_b04_row(sj, "variance_share_between_cohort_means", "cohort", unname(sh[["s_cohort"]]), pc$lo, pc$hi,
      if (is.null(rs)) "none" else "parametric_bootstrap_percentile", "share of total variance", "cohort", S33B_LABEL$s_cohort, md,
      rank, model_id, formula, m, variance = unname(v[["v_fix"]]), boot = bmeta, prof = prof, computed_in = computed_in)
  }
  if (isTRUE(sj$icc)) {
    rows[[length(rows) + 1L]] <- s33b_b04_row(sj, "moment_icc_cc1_cage_within_cohort", "cage_within_cohort",
      s33b_moment_icc_within(md$y, md$cc1_cage, md$Batch), NA_real_, NA_real_, "none", "intraclass correlation (signed)", "cc1_cage",
      S33B_LABEL$icc, md, rank, model_id, formula, m, computed_in = computed_in)
  }
  list(rows = data.table::rbindlist(rows), boot = bsum)
}

# ---------------------------------------------------------------- E9-E12: cohort means, B2/B6 contrasts, profiles (b10-b14)
#' Pooled within-(cohort x condition) SD of v in each sex (df = n - number of strata) and the sex mean, over the rows given.
s33b_unit_scale <- function(x, v) {
  z <- data.table::data.table(Sex = x$Sex, st = paste(x$Batch, x$condition), y = as.numeric(x[[v]]))[is.finite(y)]
  z[, list(measure = v, sex_mean = mean(y), s_w = sqrt(sum((y - stats::ave(y, st))^2) / (.N - data.table::uniqueN(st))), n = .N),
    keyby = Sex]
}
#' u units (plan 9 E9): u = (x - sex mean) / s_w for every base measure; an index = mean of its parts' u with the minimum
#' number present, then re-expressed in its own s_w. Returns list(x with u_<measure> and index columns, scales).
s33b_add_units <- function(x, base, indices = list()) {
  x <- data.table::copy(x); sc <- list()
  put_u <- function(v) { s <- s33b_unit_scale(x, v); sc[[v]] <<- s; i <- match(x$Sex, s$Sex)
    data.table::set(x, j = paste0("u_", v), value = (as.numeric(x[[v]]) - s$sex_mean[i]) / s$s_w[i]) }
  for (v in base) put_u(v)
  for (ix in names(indices)) {
    U <- as.matrix(x[, paste0("u_", indices[[ix]]), with = FALSE]); np <- rowSums(is.finite(U))
    data.table::set(x, j = ix, value = ifelse(np >= S33B_INDEX_MIN[[ix]], rowMeans(U, na.rm = TRUE), NA_real_))
    put_u(ix)
  }
  list(x = x, scales = data.table::rbindlist(sc))
}
#' Cohort means per condition with u-mean, deviation from the own-sex 3-cohort mean and the within-sex rank (1 = highest,
#' as module D's rank_of_3_within_sex).
s33b_cohort_means <- function(x, measures, scales) {
  data.table::rbindlist(lapply(measures, function(v) {
    z <- data.table::data.table(condition = x$condition, Batch = x$Batch, Sex = x$Sex, y = as.numeric(x[[v]]))[is.finite(y)]
    cm <- z[, list(mean_raw = mean(y), sd_raw = if (.N > 1L) stats::sd(y) else NA_real_, n = .N), keyby = list(condition, Batch, Sex)]
    s <- scales[measure == v]; i <- match(cm$Sex, s$Sex)
    cm[, `:=`(measure = v, sex_mean = s$sex_mean[i], s_w = s$s_w[i])]
    cm[, u_mean := (mean_raw - sex_mean) / s_w]
    cm[, dev_u_from_own_sex_3cohort_mean := u_mean - mean(u_mean), by = list(condition, Sex)]
    cm[, rank_of_3_within_sex := as.integer(rank(-mean_raw, ties.method = "min")), by = list(condition, Sex)]
    cm[]
  }))
}
#' E10 direction of a target cohort: the sign of its contrast with the mean of the other two cohorts of its sex, and
#' whether that sign agrees with the sign of the same cohort's A1 rate contrast ('same' / 'opposite'; NA at 0 or NA).
s33b_direction <- function(d_measure, d_rate) {
  dir <- ifelse(!is.finite(d_measure) | d_measure == 0, NA_character_,
                ifelse(d_measure > 0, "above the other two of its sex", "below the other two of its sex"))
  vs <- ifelse(!is.finite(d_measure) | !is.finite(d_rate) | d_measure == 0 | d_rate == 0, NA_character_,
               ifelse(sign(d_measure) == sign(d_rate), "same", "opposite"))
  list(direction = dir, direction_vs_rate = vs)
}
#' Cage sums and counts of finite values within one cell (cages sorted by CEID, C-locale radix): G x V matrices.
s33b_cell_sums <- function(x, cage_col, vars) {
  cg <- sort(unique(x[[cage_col]]), method = "radix"); f <- factor(x[[cage_col]], levels = cg)
  S <- n <- matrix(0, length(cg), length(vars), dimnames = list(cg, vars))
  for (v in vars) { y <- as.numeric(x[[v]]); ok <- is.finite(y)
    s1 <- tapply(y[ok], f[ok], sum); s1[is.na(s1)] <- 0; S[, v] <- as.numeric(s1)
    n1 <- tapply(y[ok], f[ok], length); n1[is.na(n1)] <- 0; n[, v] <- as.numeric(n1) }
  list(cages = cg, S = S, n = n)
}
#' Cage sums for a pooled within-cohort correlation of xv and yv (animals with both): G x 6 (n, sx, sy, sxx, syy, sxy).
s33b_cell_pair_sums <- function(x, cage_col, xv, yv) {
  cg <- sort(unique(x[[cage_col]]), method = "radix"); f <- factor(x[[cage_col]], levels = cg)
  a <- as.numeric(x[[xv]]); b <- as.numeric(x[[yv]]); ok <- is.finite(a) & is.finite(b)
  sm <- function(z) { s <- tapply(z[ok], f[ok], sum); s[is.na(s)] <- 0; as.numeric(s) }
  matrix(c(sm(rep(1, length(a))), sm(a), sm(b), sm(a * a), sm(b * b), sm(a * b)), nrow = length(cg),
         dimnames = list(cg, c("n", "sx", "sy", "sxx", "syy", "sxy")))
}
#' Centred cross-products of one cohort from its (resampled) totals T (rows = replicates).
s33b_centred <- function(T) {
  T <- if (is.null(dim(T))) matrix(T, nrow = 1L, dimnames = list(NULL, names(T))) else T
  n <- T[, "n"]; f <- ifelse(n > 0, 1 / n, 0)
  cbind(sxx = T[, "sxx"] - T[, "sx"]^2 * f, syy = T[, "syy"] - T[, "sy"]^2 * f, sxy = T[, "sxy"] - T[, "sx"] * T[, "sy"] * f)
}
#' Pooled within-cohort Pearson r from per-cohort pair sums (each cage once).
s33b_pooled_r <- function(pairs) {
  acc <- Reduce(`+`, lapply(pairs, function(P) s33b_centred(colSums(P))))
  unname(acc[, "sxy"] / sqrt(acc[, "sxx"] * acc[, "syy"]))
}
#' One cage-resampling stream (plan section 7): one s33_resample_index() call over the declared cells, then the replicate
#' means of every cell's variables and the replicate pooled within-cohort r of every pair variable. Draws are not kept.
s33b_stream_reps <- function(cells, B, seed, sums, pairs = list()) {
  idx <- s33_resample_index(cells, B, seed)
  means <- list(); acc <- list()
  for (cl in names(cells)) {
    U <- s33_multiplicity(idx[[cl]], cells[[cl]])
    if (!is.null(sums[[cl]])) means[[cl]] <- (U %*% sums[[cl]]$S) / (U %*% sums[[cl]]$n)
    for (v in names(pairs[[cl]])) { cp <- s33b_centred(U %*% pairs[[cl]][[v]]); acc[[v]] <- if (is.null(acc[[v]])) cp else acc[[v]] + cp }
  }
  list(means = means, r = lapply(acc, function(a) unname(a[, "sxy"] / sqrt(a[, "sxx"] * a[, "syy"]))), index_sha256 = s33b_sha(idx))
}

#' E10 weights over the six cohort means (target minus the mean of the other two of its sex; average = half of each).
s33b_contrast_weights <- function(target, s_w = NULL) {
  w <- stats::setNames(numeric(6), S33_COHORTS)
  for (sx in c("Male", "Female")) if (target %in% c(sx, "average")) {
    k <- if (target == "average") 0.5 else 1
    w[S33B_TARGET[[sx]]] <- k; w[S33B_REFERENCE[[sx]]] <- -k / 2
  }
  if (!is.null(s_w)) w <- w / s_w[S33_COHORTS]
  w
}

#' The arena analyses of one variant (E9-E12). v: id, label, population ('tracked_111' / 'canonical_117'), measures,
#' base measures, indices, conditions, sis_cluster (arena), stream (seed name) and cells kind, e10/e11/e12 switches.
s33b_arena_variant <- function(frame, v, ctx, log, rate_cm, computed_in) {
  pop <- frame[frame[[paste0("pop_", v$population)]] %in% TRUE]
  un <- s33b_add_units(pop, v$base, v$indices); x <- un$x; sc <- un$scales
  x[, u_crossing_rate := frame$u_crossing_rate[match(AnimalNum, frame$AnimalNum)]]
  ms <- v$measures; arena_ms <- setdiff(ms, "crossing_rate")
  cm <- s33b_cohort_means(x, ms, sc)[condition %in% v$conditions]
  sis <- x[condition == "SIS"]
  clus <- function(meas) if (meas == "crossing_rate") "cc1_cage" else v$sis_cluster
  short <- function(cl) sub("_cage$", "", cl)
  cr2 <- data.table::rbindlist(lapply(ms, function(meas) data.table::rbindlist(lapply(S33_COHORTS, function(b) {
    z <- sis[Batch == b]; r <- s33_cr2_mean(as.numeric(z[[meas]]), z[[clus(meas)]])
    data.table::data.table(measure = meas, Batch = b, se = r$se, df = r$df, ci_low = r$ci_low, ci_high = r$ci_high, n_clusters = r$n_clusters)
  }))))
  # the stream: CC1 cells (rate) then the arena cells, one set.seed; E12 pairs in the arena cells
  arena_cl <- v$sis_cluster
  cells <- integer(); sums <- list(); pairs <- list()
  if ("crossing_rate" %in% ms || v$cells == "cc1") for (b in S33_COHORTS) {
    nm <- paste0(b, "_cc1"); z <- sis[Batch == b]
    vars <- c(if ("crossing_rate" %in% ms) "crossing_rate", if (arena_cl == "cc1_cage") arena_ms)
    s <- s33b_cell_sums(z, "cc1_cage", vars); cells[[nm]] <- length(s$cages); sums[[nm]] <- s
    if (arena_cl == "cc1_cage" && isTRUE(v$e12)) pairs[[nm]] <- stats::setNames(lapply(arena_ms, function(meas)
      s33b_cell_pair_sums(z, "cc1_cage", "u_crossing_rate", paste0("u_", meas))), arena_ms)
  }
  if (arena_cl == "cc4_cage") for (b in S33_COHORTS) {
    nm <- paste0(b, "_cc4"); z <- sis[Batch == b]
    s <- s33b_cell_sums(z, "cc4_cage", arena_ms); cells[[nm]] <- length(s$cages); sums[[nm]] <- s
    if (isTRUE(v$e12)) pairs[[nm]] <- stats::setNames(lapply(arena_ms, function(meas)
      s33b_cell_pair_sums(z, "cc4_cage", "u_crossing_rate", paste0("u_", meas))), arena_ms)
  }
  seed <- unname(ctx$seeds[[v$stream]]); B <- unname(ctx$B[["nonparametric"]])
  reps <- s33b_ckpt(ctx, paste0("cage_", v$id), function() s33b_stream_reps(cells, B, seed, sums, pairs),
                    list(sums = s33b_sha(sums), pairs = s33b_sha(pairs), cells = cells, seed = seed, B = B), log)
  cell_of <- function(meas, b) paste0(b, "_", short(clus(meas)))
  rep_mean <- function(meas, b) { cl <- cell_of(meas, b); if (is.null(reps$means[[cl]])) NULL else reps$means[[cl]][, meas] }
  bsum <- data.table::data.table(stream_id = paste0("arena_", v$id), kind = "cage_resampling", seed_name = v$stream, seed = seed, B = B,
    rng = S33B_RNG, unit_order = "cells in declared order (CC1 cells B1-B6 before CC4 cells B1-B6; see cells); cages by CEID (C-locale radix)",
    cells = paste(sprintf("%s:%d", names(cells), cells), collapse = ";"), model_id = NA_character_, n_success = B,
    finite_share = NA_real_, replicate_sha256 = s33b_sha(reps[c("means", "r")]), index_sha256 = reps$index_sha256,
    statistics = paste(c(sprintf("cohort means (%d cells)", length(reps$means)), if (length(reps$r)) "pooled within-cohort r"), collapse = "; "),
    computed_in = computed_in)
  mlabel <- function(meas) if (meas == "crossing_rate") S33B_LABEL$rate_label else
    if (meas %in% S33B_SPEED_MEASURES) S33B_METRIC_LABEL[["arena_speed"]] else S33B_LABEL$arena_label
  mid <- function(meas) if (meas == "crossing_rate") "crossing_rate" else if (meas %in% S33B_SPEED_MEASURES) "arena_speed" else "arena_distance"
  munits <- function(meas) if (meas == "crossing_rate") "position changes/hour" else
    if (meas %in% names(S33B_INDEX)) "u units (mean of the parts' u values)" else if (meas %in% S33B_SPEED_MEASURES) "cm/s" else "cm"
  mtier <- function(meas) if (meas %in% c("crossing_rate", "oft_distance", "epm_distance", "nor_distance", "arena_index", "oft_speed",
                                          "epm_speed", "nor_speed", "arena_speed_index")) "primary" else "secondary"
  mflag <- function(meas) if (meas %in% S33B_FLAGGED_MEASURES) S33B_LABEL$calibration else NA_character_
  # sensitivity ids of a row: the variant (S16, S17, S19) and S18 for the index with HAB
  sid <- function(meas) { k <- c(if (v$id != "primary") v$id, if (meas %in% c("arena_index4", "arena_speed_index4")) "S18")
    if (length(k)) paste(k, collapse = ";") else NA_character_ }
  # ---- E9 (b10)
  e9 <- data.table::rbindlist(lapply(seq_len(nrow(cm)), function(i) {
    r <- cm[i]; is_sis <- r$condition == "SIS"; meas <- r$measure
    c2 <- if (is_sis) cr2[measure == meas & Batch == r$Batch] else NULL
    rr <- if (is_sis) rep_mean(meas, r$Batch) else NULL
    pc <- if (!is.null(rr)) s33_percentile(rr) else list(lo = NA_real_, hi = NA_real_, finite_share = NA_real_)
    has_ci <- is_sis && isTRUE(is.finite(c2$se))
    out <- s33_row(estimand = "cohort_mean", level = "cohort", estimate = r$mean_raw, se = if (has_ci) c2$se else NA_real_,
      df = if (has_ci) c2$df else NA_real_, ci_low = if (has_ci) c2$ci_low else NA_real_, ci_high = if (has_ci) c2$ci_high else NA_real_,
      interval_method = if (has_ci) paste0("CR2_Satterthwaite_", clus(meas)) else "none",
      interval_basis = if (has_ci) "conditional_on_cohorts" else NA_character_, units = munits(meas), metric_id = mid(meas),
      lead = FALSE, n_animals = r$n, n_cages = if (is_sis) c2$n_clusters else 1L, n_cohorts = 1L, n_params = NA_integer_,
      variant = v$id, variant_label = v$label, sensitivity_id = sid(meas), condition = r$condition, Batch = r$Batch, Sex = r$Sex, measure = meas,
      measure_label = mlabel(meas), measure_tier = mtier(meas), sd_raw = r$sd_raw, sex_mean = r$sex_mean, s_w = r$s_w,
      # the CR2 interval re-expressed in u units (named lower / upper: not a second interval)
      u_mean = r$u_mean, u_lower = if (has_ci) (c2$ci_low - r$sex_mean) / r$s_w else NA_real_,
      u_upper = if (has_ci) (c2$ci_high - r$sex_mean) / r$s_w else NA_real_,
      dev_u_from_own_sex_3cohort_mean = r$dev_u_from_own_sex_3cohort_mean, offset_reference = S33B_LABEL$offset_reference,
      rank_of_3_within_sex = r$rank_of_3_within_sex, rank_definition = S33B_LABEL$rank,
      cluster = if (is_sis) clus(meas) else NA_character_,
      range_method = if (!is.null(rr)) paste0("cage_resampling_range_", short(clus(meas))) else NA_character_,
      range_low = pc$lo, range_high = pc$hi, range_finite_share = pc$finite_share,
      range_B = if (!is.null(rr)) B else NA_integer_, range_seed = if (!is.null(rr)) seed else NA_integer_,
      flag = mflag(meas), note = if (is_sis) S33B_LABEL$range else S33B_LABEL$con, computed_in = computed_in)
    out[]
  }))
  # ---- E10 (b12)
  e10 <- if (isTRUE(v$e10)) data.table::rbindlist(lapply(v$conditions, function(cond) data.table::rbindlist(lapply(ms, function(meas) {
    z <- cm[condition == cond & measure == meas]; mm <- stats::setNames(z$mean_raw, z$Batch)[S33_COHORTS]
    sw <- stats::setNames(z$s_w, z$Batch)[S33_COHORTS]; rk <- stats::setNames(z$rank_of_3_within_sex, z$Batch)
    rc <- rate_cm[condition == cond]; rate_rk <- stats::setNames(rc$rank_of_3_within_sex, rc$Batch)
    rate_mm <- stats::setNames(rc$mean_raw, rc$Batch)[S33_COHORTS]
    is_sis <- cond == "SIS"; is_rate <- meas == "crossing_rate"
    c2 <- if (is_sis) cr2[measure == meas][match(S33_COHORTS, Batch)] else NULL
    R <- if (is_sis && !is_rate) do.call(cbind, lapply(S33_COHORTS, function(b) rep_mean(meas, b))) else NULL
    data.table::rbindlist(lapply(c("raw", "u"), function(scale) data.table::rbindlist(lapply(c("Male", "Female", "average"), function(tg) {
      w <- s33b_contrast_weights(tg, if (scale == "u") sw else NULL)
      lc <- if (is_sis && !is_rate) s33b_lincomb(mm, c2$se, c2$df, w) else list(estimate = sum((w * mm)[w != 0]), se = NA_real_, df = NA_real_, ci_low = NA_real_, ci_high = NA_real_)
      has_ci <- is.finite(lc$se)
      pc <- if (!is.null(R)) s33_percentile(as.vector(R %*% w)) else list(lo = NA_real_, hi = NA_real_, finite_share = NA_real_)
      tb <- if (tg == "average") NA_character_ else S33B_TARGET[[tg]]
      rk_t <- if (is.na(tb)) NA_integer_ else unname(rk[[tb]]); rr_t <- if (is.na(tb)) NA_integer_ else unname(rate_rk[[tb]])
      # direction (per target cohort): sign of its contrast; and agreement with the sign of its A1 rate contrast
      wr <- s33b_contrast_weights(tg)
      dr <- s33b_direction(if (is.na(tb)) NA_real_ else lc$estimate, if (is.na(tb) || is_rate) NA_real_ else sum((wr * rate_mm)[wr != 0]))
      s33_row(estimand = "b2_b6_contrast", level = "cohort", estimate = lc$estimate, se = lc$se, df = lc$df, ci_low = lc$ci_low,
        ci_high = lc$ci_high, interval_method = if (has_ci) paste0("CR2_Welch_", clus(meas)) else "none",
        interval_basis = if (has_ci) "conditional_on_cohorts" else NA_character_,
        units = if (scale == "u") "u units (pooled within-cohort SD of the sex)" else munits(meas), metric_id = mid(meas), lead = FALSE,
        n_animals = sum(z$n[z$Batch %in% names(w)[w != 0]]), n_cages = if (is_sis) sum(c2$n_clusters[w != 0]) else sum(w != 0),
        n_cohorts = sum(w != 0), n_params = NA_integer_, variant = v$id, variant_label = v$label, sensitivity_id = sid(meas), condition = cond, measure = meas,
        measure_label = mlabel(meas), measure_tier = mtier(meas), scale = scale,
        target = if (tg == "average") "average of B2 and B6" else tb,
        contrast_definition = if (tg == "average") "mean of the B2 and B6 contrasts" else sprintf("%s minus the mean of %s", tb, paste(S33B_REFERENCE[[tg]], collapse = " and ")),
        rank_of_3_within_sex = rk_t, rate_rank_of_3_within_sex = rr_t, rank_definition = S33B_LABEL$rank,
        direction = dr$direction, direction_vs_rate = dr$direction_vs_rate, direction_definition = S33B_LABEL$direction,
        cluster = if (is_sis && !is_rate) clus(meas) else NA_character_,
        range_method = if (!is.null(R)) paste0("cage_resampling_range_", short(clus(meas))) else NA_character_,
        range_low = pc$lo, range_high = pc$hi, range_finite_share = pc$finite_share,
        range_B = if (!is.null(R)) B else NA_integer_, range_seed = if (!is.null(R)) seed else NA_integer_, flag = mflag(meas),
        selection_statement = S33_FIXED_SENTENCES[["b_selection"]],
        note = if (!is_sis) S33B_LABEL$con else if (is_rate) S33B_LABEL$selection_rate else S33B_LABEL$range,
        computed_in = computed_in)
    }))))
  })))) else NULL
  # ---- E11 (b13)
  e11 <- if (isTRUE(v$e11)) data.table::rbindlist(lapply(v$conditions, function(cond) data.table::rbindlist(lapply(arena_ms, function(meas) {
    ur <- stats::setNames(rate_cm[condition == cond]$u_mean, rate_cm[condition == cond]$Batch)[S33_COHORTS]
    z <- cm[condition == cond & measure == meas]; um <- stats::setNames(z$u_mean, z$Batch)[S33_COHORTS]
    pa <- s33b_profile_agreement(ur, um)
    s33_row(estimand = "profile_agreement_pearson_r", level = "cohort", estimate = pa$pearson, interval_method = "none",
      units = "Pearson r over six sex-centred cohort values", metric_id = mid(meas), lead = FALSE,
      n_animals = sum(z$n), n_cages = NA_integer_, n_cohorts = 6L, n_params = NA_integer_, variant = v$id, variant_label = v$label, sensitivity_id = sid(meas),
      condition = cond, measure = meas, measure_label = mlabel(meas), measure_tier = mtier(meas), spearman_rho = pa$spearman,
      lobo_pearson_min = pa$lobo_pearson[1], lobo_pearson_max = pa$lobo_pearson[2], lobo_spearman_min = pa$lobo_spearman[1],
      lobo_spearman_max = pa$lobo_spearman[2], lobo_definition = "leave-one-cohort-out: that sex re-centred on its other two cohorts (5 points)",
      rank_order_rate = s33b_rank_order(ur), rank_order_measure = s33b_rank_order(um), flag = mflag(meas), note = S33B_LABEL$e11,
      computed_in = computed_in)
  })))) else NULL
  # ---- E12 (b14)
  e12 <- if (isTRUE(v$e12)) data.table::rbindlist(lapply(v$conditions, function(cond) data.table::rbindlist(lapply(arena_ms, function(meas) {
    xc <- x[condition == cond]; um <- paste0("u_", meas)
    pr <- lapply(S33_COHORTS, function(b) s33b_cell_pair_sums(xc[Batch == b], "cc1_cage", "u_crossing_rate", um))
    r0 <- s33b_pooled_r(pr); n_pair <- sum(vapply(pr, function(P) sum(P[, "n"]), 0))
    is_sis <- cond == "SIS"
    pc <- if (is_sis) s33_percentile(reps$r[[meas]]) else list(lo = NA_real_, hi = NA_real_, finite_share = NA_real_)
    row_r <- s33_row(estimand = "pooled_within_cohort_r", level = "animal_within_cohort", estimate = r0, ci_low = pc$lo, ci_high = pc$hi,
      interval_method = if (is_sis) paste0("cage_resampling_range_", short(v$sis_cluster)) else "none",
      interval_basis = if (is_sis) "conditional_on_cohorts" else NA_character_, units = "Pearson r of u(rate) and u(measure), centred within cohort",
      metric_id = mid(meas), lead = FALSE, n_animals = as.integer(n_pair), n_cages = if (is_sis) data.table::uniqueN(xc[[v$sis_cluster]]) else 6L,
      n_cohorts = 6L, n_params = NA_integer_, variant = v$id, variant_label = v$label, sensitivity_id = sid(meas), condition = cond, measure = meas,
      measure_label = mlabel(meas), measure_tier = mtier(meas), model_id = NA_character_, formula = NA_character_,
      range_B = if (is_sis) B else NA_integer_, range_seed = if (is_sis) seed else NA_integer_,
      range_finite_share = pc$finite_share, flag = mflag(meas),
      note = if (is_sis) paste(S33B_LABEL$range, "(cells of the cohort-mean stream)", sep = "; ") else S33B_LABEL$con,
      computed_in = computed_in)
    if (!is_sis) return(row_r)
    md <- data.table::copy(xc[is.finite(get(um)) & is.finite(u_crossing_rate)])
    md[, `:=`(y = get(um), Batch = factor(Batch, levels = S33_COHORTS), CageEpisodeID = cc4_cage)]
    f <- paste("y ~ Batch + u_crossing_rate + (1 | src_cage) + (1 | CageEpisodeID)", if (isTRUE(v$e12_cc1)) "+ (1 | cc1_cage)" else "")
    mid12 <- paste("S33B|E12", meas, v$id, sep = "|")
    m <- s33_kr_fit(trimws(f), md, mid12, 7L)
    e <- s33b_est_lmer(m, "u_crossing_rate", "within_cohort_slope_u")
    row_s <- s33_row(estimand = "within_cohort_slope_u", level = "animal_within_cohort", estimate = e$estimate, se = e$se, df = e$df,
      ci_low = e$ci_low, ci_high = e$ci_high, interval_method = e$interval_method,
      interval_basis = if (e$interval_method == "none") NA_character_ else "conditional_on_cohorts",
      units = "u(measure) per u(rate)", metric_id = mid(meas), status = e$status, singular = e$singular, ddf_fallback = e$ddf_fallback,
      lead = FALSE, n_animals = nrow(md), n_cages = data.table::uniqueN(md$cc4_cage), n_cohorts = 6L, n_params = 7L,
      variant = v$id, variant_label = v$label, sensitivity_id = sid(meas), condition = cond, measure = meas, measure_label = mlabel(meas),
      measure_tier = mtier(meas), model_id = mid12, formula = trimws(f), range_B = NA_integer_, range_seed = NA_integer_,
      range_finite_share = NA_real_, flag = mflag(meas), note = "source-cage and CC4-cage effects (the last recorded shared cage before the arena sessions)",
      computed_in = computed_in)
    data.table::rbindlist(list(row_r, row_s))
  })))) else NULL
  fill_label <- function(t) { if (!is.null(t) && nrow(t)) t[is.na(metric_label) & metric_id %in% names(S33B_METRIC_LABEL),
                                                               metric_label := unname(S33B_METRIC_LABEL[metric_id])]; t }
  list(e9 = fill_label(e9), e10 = fill_label(e10), e11 = fill_label(e11), e12 = fill_label(e12), boot = bsum, cm = cm, x = x, scales = sc)
}
s33b_rank_order <- function(u) paste(vapply(c("Male", "Female"), function(sx) { k <- names(u)[S33_SEX_OF_COHORT[names(u)] == sx]
  paste(k[order(u[k])], collapse = " < ") }, ""), collapse = "; ")
#' E11: Pearson r and Spearman rho over the six sex-centred cohort values; leave-one-cohort-out re-centres that sex on its
#' other two cohorts (5 points). No interval.
s33b_profile_agreement <- function(u_rate, u_m) {
  ids <- S33_COHORTS[is.finite(u_rate[S33_COHORTS]) & is.finite(u_m[S33_COHORTS])]
  na <- list(pearson = NA_real_, spearman = NA_real_, lobo_pearson = c(NA_real_, NA_real_), lobo_spearman = c(NA_real_, NA_real_))
  if (length(ids) < 6L) return(na)
  ctr <- function(u) u - stats::ave(u, S33_SEX_OF_COHORT[names(u)])
  lo <- vapply(ids, function(b) { k <- setdiff(ids, b); a <- ctr(u_rate[k]); c2 <- ctr(u_m[k])
    c(stats::cor(a, c2), stats::cor(a, c2, method = "spearman")) }, c(0, 0))
  list(pearson = stats::cor(ctr(u_rate[ids]), ctr(u_m[ids])), spearman = stats::cor(ctr(u_rate[ids]), ctr(u_m[ids]), method = "spearman"),
       lobo_pearson = range(lo[1, ]), lobo_spearman = range(lo[2, ]))
}

#' The four arena variants (plan 9 E9-E12, S16, S17, S19; S18 = the index4 rows).
s33b_arena_variants <- function() list(
  list(id = "primary", label = "primary (tracked animals with the measure; CC4-cage clustering for the arena)",
       population = "tracked_111", base = c("crossing_rate", S33B_ARENA$measure), indices = S33B_INDEX[c("arena_index", "arena_index4")],
       measures = c("crossing_rate", S33B_DIST_MEASURES), conditions = c("CON", "SIS"), sis_cluster = "cc4_cage", cells = "cc1+cc4",
       stream = "B_e9_arena", e10 = TRUE, e11 = TRUE, e12 = TRUE),
  list(id = "S16", label = "S16 CC1-cage clustering for the arena values", population = "tracked_111", base = S33B_ARENA$measure,
       indices = S33B_INDEX[c("arena_index", "arena_index4")], measures = S33B_DIST_MEASURES, conditions = "SIS",
       sis_cluster = "cc1_cage", cells = "cc1", stream = "B_s16", e10 = TRUE, e11 = FALSE, e12 = TRUE, e12_cc1 = TRUE),
  list(id = "S17", label = "S17 all 117 animals (planned CC4 cages for the 6 untracked B1 SIS)", population = "canonical_117",
       base = S33B_ARENA$measure, indices = S33B_INDEX[c("arena_index", "arena_index4")], measures = S33B_DIST_MEASURES,
       conditions = "SIS", sis_cluster = "cc4_cage", cells = "cc4", stream = "B_s17", e10 = TRUE, e11 = TRUE, e12 = FALSE),
  list(id = "S19", label = "S19 arena speed in place of distance", population = "tracked_111", base = S33B_ARENA$speed,
       indices = S33B_INDEX[c("arena_speed_index", "arena_speed_index4")], measures = S33B_SPEED_MEASURES, conditions = c("CON", "SIS"),
       sis_cluster = "cc4_cage", cells = "cc4", stream = "B_s19", e10 = TRUE, e11 = TRUE, e12 = TRUE))

# ---------------------------------------------------------------- E7, E8, b01, b09, b11 (outcome-free descriptions)
s33b_b01 <- function(f) {
  src_n <- f[, list(src_cage_n_canonical = .N, src_cage_n_sis = sum(condition == "SIS"), src_cage_is_con_cage = any(condition == "CON")),
             by = src_cage]
  x <- merge(f, src_n, by = "src_cage", all.x = TRUE)
  out <- x[, list(AnimalNum, Batch, Sex, condition, tracked, untracked_b1, src_cage,
    src_cage_record = data.table::fifelse(Batch == "B2", "GroupCompBatch2 planning record", "Timeline Animals"),
    src_cage_label_alt = unname(S33B_SOURCE_LABEL_ALT[src_cage]), src_cage_n_canonical, src_cage_n_sis, src_cage_is_con_cage,
    cc1_cage, cc1_cage_record = data.table::fifelse(untracked_b1, "B1 Social Golfer plan (CC1 round)", "bundle A1 CageEpisodeID"),
    cc1_board, cc2_cage, cc3_cage, cc4_cage,
    cc4_cage_record = data.table::fifelse(untracked_b1, "master_wide cage = B1 Social Golfer plan (CC4 round)", "master_wide cage = bundle CC4 CageEpisodeID"),
    line, line_J, dob, cc1_date, age_cc1, tp1_date, tp1, tp2_date, tp2, tp1_to_cc1_d = as.integer(cc1_date - tp1_date),
    mark_date, rack, rec_start_cc1, lag_h_cc1, pop_sis_87, pop_sis_93, pop_tracked_111)]
  data.table::setorderv(out, "AnimalNum")
  # dates as ISO text (a Date is a double: the frozen writer would otherwise write day counts)
  dc <- names(out)[vapply(out, function(v) inherits(v, "Date"), TRUE)]
  if (length(dc)) out[, (dc) := lapply(.SD, function(d) format(d, "%Y-%m-%d")), .SDcols = dc]
  out[, tier := S33_TIER][]
}
s33b_b03 <- function(f) {
  con_board <- f[condition == "CON", list(con_cage_board = paste(sort(unique(cc1_board), method = "radix"), collapse = ";")), by = Batch]
  x <- f[, list(n = .N, n_tracked = sum(tracked), n_source_cages = data.table::uniqueN(src_cage),
    age_mean = mean(age_cc1), age_min = min(age_cc1), age_max = max(age_cc1),
    tp1_mean = mean(tp1), tp1_sd = stats::sd(tp1), tp1_min = min(tp1), tp1_max = max(tp1),
    tp2_mean = mean(tp2), tp2_sd = stats::sd(tp2), tp2_min = min(tp2), tp2_max = max(tp2), n_line_J = sum(line_J),
    dob_dates = paste(sort(unique(format(dob)), method = "radix"), collapse = ";"),
    tp1_date = paste(sort(unique(format(tp1_date)), method = "radix"), collapse = ";"),
    tp1_to_cc1_d = paste(sort(unique(as.integer(cc1_date - tp1_date))), collapse = ";"),
    cc1_date = paste(sort(unique(format(cc1_date)), method = "radix"), collapse = ";"),
    rec_start_cc1 = rec_start_cc1[1], lag_h_cc1 = lag_h_cc1[1],
    cc1_boards = paste(sort(unique(cc1_board), method = "radix"), collapse = ";"),
    rack = paste(sort(unique(rack), method = "radix"), collapse = ";")), keyby = list(Batch, Sex, condition)]
  x <- merge(x, con_board, by = "Batch", all.x = TRUE)
  data.table::setorderv(x, c("Batch", "condition"))
  x[, note := S33B_LABEL$housing][, tier := S33_TIER][]
}
#' E8: SIS CC1 cage means per cohort x board, CON cages as context rows, same-board differences, sys.3 SIS cages.
s33b_b08 <- function(f, rate_scales, computed_in = "phase2") {
  tr <- f[tracked %in% TRUE & is.finite(crossing_rate)]
  cg <- tr[, list(mean_rate = mean(crossing_rate), n_tracked = .N, board = cc1_board[1], Sex = Sex[1]), keyby = list(Batch, condition, cc1_cage)]
  sw <- function(sx) rate_scales$s_w[rate_scales$Sex == sx]; smean <- function(sx) rate_scales$sex_mean[rate_scales$Sex == sx]
  row <- function(row_type, estimand, level, est, sx, batch, board, cage, n, refs = NA_character_, ref_vals = NA_character_,
                  u = NA_real_, note = NA_character_) s33_row(estimand = estimand, level = level, estimate = est, interval_method = "none",
    units = "position changes/hour", metric_id = "crossing_rate", lead = FALSE, n_animals = n, n_cages = NA_integer_,
    n_cohorts = NA_integer_, n_params = NA_integer_, row_type = row_type, Batch = batch, Sex = sx, board = board, cage = cage,
    reference_cohorts = refs, reference_values = ref_vals, estimate_u = u, u_scale_s_w = sw(sx), note = note, computed_in = computed_in)
  out <- list()
  for (i in seq_len(nrow(cg))) { r <- cg[i]
    out[[length(out) + 1L]] <- if (r$condition == "SIS") row("sis_cage", "cc1_cage_mean", "cage_within_cohort", r$mean_rate, r$Sex, r$Batch,
      r$board, r$cc1_cage, r$n_tracked, u = (r$mean_rate - smean(r$Sex)) / sw(r$Sex)) else
      row("con_context", "con_cage_mean_context", "cage_within_cohort", r$mean_rate, r$Sex, r$Batch, r$board, r$cc1_cage, r$n_tracked,
          u = (r$mean_rate - smean(r$Sex)) / sw(r$Sex), note = paste("context only, never differenced;", S33B_LABEL$con)) }
  sis <- cg[condition == "SIS"]
  for (sx in c("Male", "Female")) {
    tb <- S33B_TARGET[[sx]]; rf <- S33B_REFERENCE[[sx]]; d <- numeric()
    for (bd in S33B_SAME_BOARDS[[sx]]) {
      t1 <- sis[Batch == tb & board == bd]; r1 <- sis[Batch %in% rf & board == bd][match(rf, Batch)]
      dd <- t1$mean_rate - mean(r1$mean_rate); d <- c(d, dd)
      out[[length(out) + 1L]] <- row("same_board_diff", "same_board_difference", "cohort", dd, sx, tb, bd, t1$cc1_cage,
        t1$n_tracked + sum(r1$n_tracked), paste(rf, collapse = ";"), paste(sprintf("%s %.4f (n %d)", r1$Batch, r1$mean_rate, r1$n_tracked), collapse = "; "),
        u = dd / sw(sx), note = S33B_LABEL$e8)
    }
    out[[length(out) + 1L]] <- row("same_board_mean_diff", "same_board_mean_difference", "cohort", mean(d), sx, tb,
      paste(S33B_SAME_BOARDS[[sx]], collapse = ";"), NA_character_, NA_integer_, paste(rf, collapse = ";"), NA_character_,
      u = mean(d) / sw(sx), note = paste("unweighted mean over the three shared boards;", S33B_LABEL$e8))
    s3 <- sis[Batch == tb & board == "sys.3"]; oth <- sis[Batch == tb & board != "sys.3"]
    dd <- s3$mean_rate - mean(oth$mean_rate)
    out[[length(out) + 1L]] <- row("sys3_vs_others", "sys3_sis_cage_vs_other_sis_cages", "cage_within_cohort", dd, sx, tb, "sys.3",
      s3$cc1_cage, s3$n_tracked, tb, paste(sprintf("%s %.4f", oth$board, oth$mean_rate), collapse = "; "), u = dd / sw(sx),
      note = "sys.3 SIS cage mean minus the mean of the other three SIS cage means of the cohort")
  }
  x <- data.table::rbindlist(out)
  x[, note := ifelse(is.na(note), S33B_LABEL$board, paste(note, S33B_LABEL$board, sep = "; "))][]
}
s33b_b09 <- function(design) {
  x <- data.table::dcast(design$cages[condition == "CON", list(Batch, Sex, CC, board)], Batch + Sex ~ CC, value.var = "board")
  data.table::setnames(x, S33_CC, paste0(S33_CC, "_board"))
  x[, changes_at_every_cage_change := CC1_board != CC2_board & CC2_board != CC3_board & CC3_board != CC4_board]
  data.table::setorderv(x, "Batch")
  x[, note := S33B_LABEL$board][, tier := S33_TIER][]
}
s33b_b11 <- function(x) {
  cols <- c("AnimalNum", "Batch", "Sex", "cc1_cage", "cc1_board", "crossing_rate", "u_crossing_rate", S33B_ARENA$measure,
            paste0("u_", S33B_ARENA$measure), "arena_index", "u_arena_index", "arena_index4", "u_arena_index4")
  out <- x[condition == "CON", cols, with = FALSE]
  data.table::setorderv(out, "AnimalNum")
  out[, note := S33B_LABEL$con][, tier := S33_TIER][]
}

# ---------------------------------------------------------------- X1 appendix: RFID board offsets (b15)
#' Board offsets o_1..o_4 (o_5 = -sum) from SIS A1 windows CC1-CC4 with numeric sum-to-zero board columns; implied layout
#' terms; KR. Not a re-test of H05/H06.
s33b_x1 <- function(design, frame, computed_in = "phase2") {
  sis_ids <- frame[pop_sis_87 %in% TRUE, AnimalNum]
  ep <- data.table::copy(design$episodes[condition == "SIS" & AnimalNum %in% sis_ids])
  for (k in 1:4) data.table::set(ep, j = paste0("sysE", k), value = as.numeric(ep$board == paste0("sys.", k)) - as.numeric(ep$board == "sys.5"))
  ep[, BatchCC := factor(paste(Batch, CC, sep = "_"))]
  cnt <- frame[pop_sis_87 %in% TRUE, .N, by = list(Batch, cc1_board)]
  wb <- function(b) { n <- function(bd) sum(cnt[Batch == b & cc1_board == bd, N]); nb <- sum(cnt[Batch == b, N])
    stats::setNames(vapply(1:4, function(k) (n(paste0("sys.", k)) - n("sys.5")) / nb, 0), paste0("sysE", 1:4)) }
  W <- lapply(stats::setNames(S33_COHORTS, S33_COHORTS), wb)
  w_sis <- 0.5 * ((W$B2 - (W$B1 + W$B5) / 2) + (W$B6 - (W$B3 + W$B4) / 2))
  L <- c(stats::setNames(lapply(1:4, function(k) stats::setNames(list(1), paste0("sysE", k))), paste0("o_sys.", 1:4)),
         list(o_sys.5 = list(sysE1 = -1, sysE2 = -1, sysE3 = -1, sysE4 = -1), D_layout_SIS = as.list(w_sis),
              D_layout_CON = list(sysE2 = 0.5, sysE3 = -1, sysE4 = 0.5)))
  rows <- list()
  for (scale in c("raw", "log")) for (pop in c("primary", "S20")) {
    d <- if (pop == "S20") ep[!in_S13 %in% TRUE] else ep
    md <- data.table::copy(d); md[, y := if (scale == "raw") crossing_rate else log(crossing_rate)]
    mid <- paste("S33B|X1", scale, pop, sep = "|")
    m <- s33_kr_fit("y ~ BatchCC + sysE1 + sysE2 + sysE3 + sysE4 + (1 | AnimalNum) + (1 | CageEpisodeID)", md, mid, 28L)
    for (nm in names(L)) {
      kc <- s33_kr_contrast(m, L[[nm]], nm)
      ok <- kc$status == "OK"
      rows[[length(rows) + 1L]] <- s33_row(estimand = if (grepl("^o_", nm)) "board_offset" else "layout_term",
        level = if (grepl("^o_", nm)) "cage_within_cohort" else "cohort", estimate = kc$estimate, se = kc$se, df = kc$df,
        ci_low = kc$ci_low, ci_high = kc$ci_high, interval_method = if (ok) kc$interval_method else "none",
        interval_basis = if (ok) "conditional_on_cohorts" else NA_character_,
        units = if (scale == "raw") "position changes/hour" else "log(position changes/hour)", metric_id = "crossing_rate",
        status = kc$status, singular = kc$singular, ddf_fallback = kc$ddf_fallback, lead = FALSE, n_animals = data.table::uniqueN(md$AnimalNum),
        n_cages = data.table::uniqueN(md$CageEpisodeID), n_cohorts = 6L, n_params = 28L, scale = scale, variant = pop,
        variant_label = if (pop == "S20") "S20 without the six S13 hardware windows (692 retained)" else "primary",
        term = nm, weights = paste(sprintf("%s=%.6g", names(unlist(L[[nm]])), unlist(L[[nm]])), collapse = "; "),
        n_windows = nrow(md), model_id = mid, formula = "y ~ BatchCC + sysE1 + sysE2 + sysE3 + sysE4 + (1 | AnimalNum) + (1 | CageEpisodeID)",
        pct_estimate = if (scale == "log") 100 * (exp(kc$estimate) - 1) else NA_real_,
        pct_lower = if (scale == "log") 100 * (exp(kc$ci_low) - 1) else NA_real_,
        pct_upper = if (scale == "log") 100 * (exp(kc$ci_high) - 1) else NA_real_, note = S33B_LABEL$x1, computed_in = computed_in)
    }
  }
  # observed CC1 A1 rate contrasts (cohort means of the tracked animals), printed beside without interval
  tr <- frame[tracked %in% TRUE & is.finite(crossing_rate)]
  for (scale in c("raw", "log")) for (cond in c("SIS", "CON")) {
    mm <- tr[condition == cond, list(m = mean(if (scale == "raw") crossing_rate else log(crossing_rate))), keyby = Batch]
    mv <- stats::setNames(mm$m, mm$Batch)[S33_COHORTS]
    for (tg in c("Male", "Female", "average")) {
      w <- s33b_contrast_weights(tg); est <- sum((w * mv)[w != 0])
      rows[[length(rows) + 1L]] <- s33_row(estimand = "b2_b6_contrast_observed", level = "cohort", estimate = est, interval_method = "none",
        units = if (scale == "raw") "position changes/hour" else "log(position changes/hour)", metric_id = "crossing_rate", lead = FALSE,
        n_animals = tr[condition == cond & Batch %in% names(w)[w != 0], .N], n_cages = NA_integer_, n_cohorts = sum(w != 0),
        n_params = NA_integer_, scale = scale, variant = "observed", variant_label = paste("observed CC1 A1 contrast,", cond),
        term = paste(cond, if (tg == "average") "average of B2 and B6" else S33B_TARGET[[tg]], sep = " "),
        weights = paste(sprintf("%s=%.6g", names(w)[w != 0], w[w != 0]), collapse = "; "), n_windows = NA_integer_,
        model_id = NA_character_, formula = NA_character_, pct_estimate = if (scale == "log") 100 * (exp(est) - 1) else NA_real_,
        pct_lower = NA_real_, pct_upper = NA_real_,
        note = paste("printed beside the layout terms without interval;", if (cond == "CON") S33B_LABEL$con else S33B_LABEL$selection_rate),
        computed_in = computed_in)
    }
  }
  data.table::rbindlist(rows)
}

# ---------------------------------------------------------------- units applied with fixed scales (b02, b11)
#' u values of every measure of `scales` with those scales (indices from their parts' u with the minimum present).
s33b_apply_units <- function(x, scales, indices = list()) {
  x <- data.table::copy(x)
  put <- function(v, val) { s <- scales[measure == v]; i <- match(x$Sex, s$Sex); data.table::set(x, j = paste0("u_", v), value = (val - s$sex_mean[i]) / s$s_w[i]) }
  for (v in setdiff(unique(scales$measure), names(indices))) put(v, as.numeric(x[[v]]))
  for (ix in intersect(names(indices), unique(scales$measure))) {
    U <- as.matrix(x[, paste0("u_", indices[[ix]]), with = FALSE]); np <- rowSums(is.finite(U))
    data.table::set(x, j = ix, value = ifelse(np >= S33B_INDEX_MIN[[ix]], rowMeans(U, na.rm = TRUE), NA_real_))
    put(ix, x[[ix]])
  }
  x[]
}

# ---------------------------------------------------------------- audit tables
s33b_timeline_prefix_audit <- function(ids, canon, canonical_ids) {
  ids <- trimws(as.character(ids)); ids <- ids[!is.na(ids) & nzchar(ids)]
  four <- unique(canon(sub("^[A-Z]{3}[0-9]-", "", ids))); two <- unique(canon(sub("^(HKH0|HKH1)-", "", ids)))
  data.table::data.table(block = "Timeline Animals ID join",
    item = c("prefix rule ^[A-Z]{3}[0-9]- (HKH0, HKH1, MCM0, LUM0) stripped", "prefix rule HKH0-/HKH1- only stripped"),
    n_expected = length(canonical_ids), n_observed = c(sum(canonical_ids %in% four), sum(canonical_ids %in% two)),
    ok = c(all(canonical_ids %in% four), NA), detail = c("rule used (plan section 3)", "recorded for comparison only"))
}
s33b_join_audit <- function(design, parsed, ratios, cal, tl) {
  g <- design$gates[gate_id %in% paste0("GC-6", c("g", "h", "i", "j", "k", "m", "n", "o", "p"))]
  r2 <- g[, list(block = "design join gate (GC-6)", item = paste(gate_id, gate), n_expected = as.integer(n_expected),
                 n_observed = as.integer(n_ok), ok = as.logical(passed), detail = as.character(detail))]
  r3 <- parsed$audit[, list(block, item, n_expected = 117L, n_observed = as.integer(n_matched), ok = !duplicated_ids,
    detail = sprintf("rows %d; rows without animal_id %d%s; unmatched ids %s", n_rows, n_rows_without_id,
                     ifelse(nzchar(codes_without_id), paste0(" (codes ", codes_without_id, ")"), ""),
                     ifelse(nzchar(unmatched_ids), unmatched_ids, "none")))]
  r4 <- data.table::data.table(block = "workbook check", item = "master_wide oft_distance = oft raw_distance (1e-9)",
    n_expected = as.integer(parsed$oft_check[["n_both"]]), n_observed = as.integer(parsed$oft_check[["n_equal"]]),
    ok = parsed$oft_check[["n_both"]] == parsed$oft_check[["n_equal"]], detail = "animals with both values")
  r5 <- ratios[, list(block = "SLEAP v2 / legacy median ratio (GB-14a)", item = paste(measure, Batch), n_expected = NA_integer_,
                      n_observed = as.integer(n), ok = median_ratio >= 0.98 & median_ratio <= 1.02, detail = sprintf("median ratio %.4f", median_ratio))]
  r6 <- data.table::data.table(block = "three-chamber calibration (GB-14b)", item = names(cal$checks), n_expected = NA_integer_,
    n_observed = NA_integer_, ok = unname(cal$checks),
    detail = c("no SLEAP v2 three-chamber distance column in master_wide_sleap",
               sprintf("SLEAP v2 three-chamber config phases: %s", paste(cal$phases, collapse = ", ")),
               sprintf("three-chamber box width %.0f-%.0f px across recordings", cal$width_range[1], cal$width_range[2])))
  r7 <- design$animals[untracked_b1 %in% TRUE, list(block = "planned cages of the untracked B1 SIS", item = AnimalNum,
    n_expected = NA_integer_, n_observed = NA_integer_, ok = NA, detail = paste0("CC1 ", cc1_cage, "; CC4 ", cc4_cage, " (B1 Social Golfer plan)"))]
  x <- data.table::rbindlist(list(tl, r2, r3, r4, r5, r6, r7), use.names = TRUE)
  x[, tier := S33_TIER][]
}
s33b_boot_table <- function(rows) {
  x <- data.table::rbindlist(rows, use.names = TRUE)
  if (!nrow(x)) return(x)
  x[, tier := S33_TIER][]
}

# ---------------------------------------------------------------- Phase 3 builders (outcomes)
#' Outcome columns (Phase 3): CombZ and its six components, CombZ_noWD = mean(NOR, sucrose_pref, delta_cort, adrenal_weight,
#' spleen_weight; keeps the two organ ratios to body weight), CombZ_noBW = mean(NOR, sucrose_pref, delta_cort; no
#' body-weight term), tp3-tp6. Outcome groups are not read.
s33b_outcomes <- function(design_out, frame) {
  o <- data.table::copy(design_out$animals[, c("AnimalNum", "CombZ", S33_COMPONENTS, "n_components_present", "tp3", "tp4", "tp5", "tp6"), with = FALSE])
  o[, CombZ_noWD := rowMeans(as.matrix(.SD), na.rm = TRUE), .SDcols = c("NOR", "sucrose_pref", "delta_cort", "adrenal_weight", "spleen_weight")]
  o[, CombZ_noBW := rowMeans(as.matrix(.SD), na.rm = TRUE), .SDcols = c("NOR", "sucrose_pref", "delta_cort")]
  for (v in c("CombZ_noWD", "CombZ_noBW")) data.table::set(o, i = which(!is.finite(o[[v]])), j = v, value = NA_real_)
  fz <- merge(frame, o, by = "AnimalNum", all.x = TRUE)
  data.table::setorderv(fz, "AnimalNum")
  fz[]
}
s33b_b02 <- function(fz, units_frame) {
  pr <- s33b_covariates(fz[pop_sis_87 %in% TRUE]); pz <- s33b_covariates(fz[pop_sis_93 %in% TRUE])
  keep <- c("AnimalNum", "Batch", "Sex", "condition", "tracked", "untracked_b1", "src_cage", "cc1_cage", "cc1_board", "cc4_cage",
            "line_J", "age_cc1", "tp1", "tp2", "tp3", "tp6", "crossing_rate", "u_crossing_rate", "log_rate", "hw_a1", "CombZ",
            S33_COMPONENTS, "CombZ_noWD", "CombZ_noBW", "n_components_present", "pop_sis_87", "pop_sis_93", "pop_tracked_111",
            paste0("sysE", 1:4))
  a <- data.table::copy(fz[, keep, with = FALSE])
  for (v in c("age_c", "w_c", "w_within", "w_srcmean")) {
    data.table::set(a, j = paste0(v, "_rate"), value = pr[[v]][match(a$AnimalNum, pr$AnimalNum)])
    data.table::set(a, j = paste0(v, "_cz"), value = pz[[v]][match(a$AnimalNum, pz$AnimalNum)])
  }
  ac <- c(S33B_ARENA$measure, S33B_ARENA$speed, paste0("u_", S33B_ARENA$measure), "arena_index", "u_arena_index", "arena_index4", "u_arena_index4")
  for (v in ac) data.table::set(a, j = v, value = units_frame[[v]][match(a$AnimalNum, units_frame$AnimalNum)])
  for (v in S33B_ARENA$measure) data.table::set(a, j = paste0("in_arena_", v), value = a$tracked & is.finite(a[[v]]))
  data.table::setorderv(a, "AnimalNum")
  a[, tier := S33_TIER][]
}
s33b_b06 <- function(fz) {
  pz <- s33b_covariates(fz[pop_sis_93 %in% TRUE])
  rm <- fz[pop_sis_87 %in% TRUE, list(m = mean(crossing_rate)), by = Batch]
  cz <- pz[, list(m_cz = mean(CombZ, na.rm = TRUE), m_wd = mean(CombZ_noWD, na.rm = TRUE), m_bw = mean(CombZ_noBW, na.rm = TRUE)), by = Batch]
  j <- pz[line_J == 1L]
  out <- j[, list(AnimalNum, Batch, Sex, src_cage, colony_prefix = sub("-.*$", "", src_cage), tracked, age_c, w_c,
    crossing_rate_dev_from_cohort_sis_mean = crossing_rate - rm$m[match(Batch, rm$Batch)],
    CombZ_dev_from_cohort_sis_mean = CombZ - cz$m_cz[match(Batch, cz$Batch)],
    CombZ_noWD_dev_from_cohort_sis_mean = CombZ_noWD - cz$m_wd[match(Batch, cz$Batch)],
    CombZ_noBW_dev_from_cohort_sis_mean = CombZ_noBW - cz$m_bw[match(Batch, cz$Batch)])]
  data.table::setorderv(out, c("Batch", "AnimalNum"))
  out[, note := sprintf(S33B_LABEL$substrain, nrow(out), data.table::uniqueN(out$src_cage), data.table::uniqueN(out$src_cage))]
  out[, tier := S33_TIER][]
}
#' E6 coupling table: within-cohort Pearson r (both variables centred within cohort) of tp1 and tp2 with each component,
#' CombZ, CombZ_noWD and CombZ_noBW among the SIS of sis_93; mech_coupling names the construction link.
s33b_b07 <- function(fz, computed_in = "phase3") {
  pz <- fz[pop_sis_93 %in% TRUE]
  rows <- list()
  for (w in c("tp1", "tp2")) for (k in c(S33_COMPONENTS, "CombZ", "CombZ_noWD", "CombZ_noBW")) {
    z <- data.table::data.table(Batch = pz$Batch, a = as.numeric(pz[[w]]), b = as.numeric(pz[[k]]))[is.finite(a) & is.finite(b)]
    z[, `:=`(ac = a - mean(a), bc = b - mean(b)), by = Batch]
    r <- sum(z$ac * z$bc) / sqrt(sum(z$ac^2) * sum(z$bc^2))
    rows[[length(rows) + 1L]] <- s33_row(estimand = "within_cohort_r", level = "animal_within_cohort", estimate = r, interval_method = "none",
      units = "Pearson r (both variables centred within cohort)", lead = FALSE, n_animals = nrow(z), n_cages = NA_integer_,
      n_cohorts = data.table::uniqueN(z$Batch), n_params = NA_integer_, weight = w, component = k,
      mech_coupling = unname(S33B_MECH_COUPLING[[k]]),
      note = if (w == "tp2" && k == "weight_dev") "weight_dev contains -tp2 (B1-B5; -tp3 in B6): shared term" else
        if (w == "tp1") "tp1 correlates with tp2, the weight_dev baseline" else NA_character_,
      computed_in = computed_in)
  }
  data.table::rbindlist(rows, use.names = TRUE)
}

# ---------------------------------------------------------------- entry points (MODULE CONTRACT)
#' Phase 2 (outcome-free): reads the declared workbook columns, the SLEAP calibration inputs and the Timeline ID column,
#' then builds every outcome-free product (s33b_phase2_core).
s33b_phase2 <- function(design, ctx) {
  canon <- s33b_canon(ctx)
  raw <- s33b_read_arena_raw(ctx$inputs)
  parsed <- s33b_parse_arena(raw, canon, design$animals$AnimalNum)
  craw <- s33b_read_calibration_raw(ctx$inputs)
  cal <- s33b_three_chamber_calibration(craw$socp_config, craw$geometry, raw$sleap_v2_header)
  tl <- s33b_timeline_prefix_audit(s33b_read_timeline_ids(ctx$inputs), canon, design$animals$AnimalNum)
  s33b_phase2_core(design, ctx, parsed, cal, tl)
}

#' Phase 2 from parsed inputs (no file access; used by the synthetic tests).
s33b_phase2_core <- function(design, ctx, parsed, cal, tl) {
  log <- new.env(parent = emptyenv())
  s33_assert_outcome_free(parsed$arena, "arena values"); s33_assert_outcome_free(parsed$sleap_v2, "SLEAP v2 values")
  frame <- s33b_frame(design, parsed$arena)
  batch_of <- stats::setNames(design$animals$Batch, design$animals$AnimalNum)
  ratios <- s33b_sleap_ratios(parsed$arena, parsed$sleap_v2, batch_of)
  gates <- data.table::rbindlist(list(s33b_gate_gb13(parsed, frame[tracked %in% TRUE, AnimalNum], frame[pop_sis_87 %in% TRUE, AnimalNum]),
                                      s33b_gate_gb14a(ratios), s33b_gate_gb14b(cal), s33b_gate_gb15(frame[pop_sis_87 %in% TRUE]),
                                      s33b_gate_gb16(ctx$seeds)))
  rate_sc <- s33b_unit_scale(frame[tracked %in% TRUE], "crossing_rate")
  frame[, u_crossing_rate := (crossing_rate - rate_sc$sex_mean[match(Sex, rate_sc$Sex)]) / rate_sc$s_w[match(Sex, rate_sc$Sex)]]
  # E1 and its rate sensitivities (b04), E3-E5 rate rows, S5, S6, S9, S11 and S12 (b05)
  sh <- lapply(s33b_share_jobs("rate"), function(sj) s33b_run_share(frame, sj, ctx, log, "phase2"))
  # SD_w: one per-SD convention (plan section 7), the n - 1 residual SD on cohort in sis_87 (tp1; tp2 for S6); Phase 3
  # uses the same tp1 set for the CombZ rows
  sdw_rate <- s33b_sd_w_set(s33b_covariates(frame[pop_sis_87 %in% TRUE]))
  sdw_tp2 <- s33b_sd_w_set(s33b_covariates(frame[pop_sis_87 %in% TRUE], "tp2"))
  b05 <- data.table::rbindlist(c(lapply(s33b_assoc_jobs("rate"), function(jb)
    s33b_run_assoc(frame, jb, if (identical(jb$wcol, "tp2")) sdw_tp2 else sdw_rate, "phase2")), list(s33b_s12(frame, "phase2"))), use.names = TRUE)
  # E9-E12 with S16, S17, S19 (b10, b12-b14) and the CON animal values (b11)
  rate_cm <- s33b_cohort_means(frame[tracked %in% TRUE], "crossing_rate", rate_sc)
  av <- lapply(s33b_arena_variants(), function(v) s33b_arena_variant(frame, v, ctx, log, rate_cm, "phase2"))
  units_frame <- s33b_apply_units(frame, av[[1]]$scales, S33B_INDEX[c("arena_index", "arena_index4")])
  products <- list(
    b01_covariates_animal = s33b_b01(frame), b03_balance_cohort = s33b_b03(frame),
    b04_variance_shares = data.table::rbindlist(lapply(sh, `[[`, "rows"), use.names = TRUE), b05_associations = b05,
    b08_cc1_board_table = s33b_b08(frame, rate_sc), b09_con_board_rotation = s33b_b09(design),
    b10_arena_cohort_means = data.table::rbindlist(lapply(av, `[[`, "e9"), use.names = TRUE),
    b11_arena_con_animal_values = s33b_b11(units_frame[tracked %in% TRUE]),
    b12_arena_cohort_contrasts = data.table::rbindlist(lapply(av, `[[`, "e10"), use.names = TRUE),
    b13_arena_profile_agreement = data.table::rbindlist(lapply(av, `[[`, "e11"), use.names = TRUE),
    b14_arena_within_cohort_association = data.table::rbindlist(lapply(av, `[[`, "e12"), use.names = TRUE),
    b15_appendix_board_offsets = s33b_x1(design, frame, "phase2"),
    b_join_audit = s33b_join_audit(design, parsed, ratios, cal, tl),
    b_bootstrap_summaries = s33b_boot_table(c(lapply(sh, `[[`, "boot"), lapply(av, `[[`, "boot"))))
  stopifnot(identical(names(products), S33B_OUTCOME_FREE_PRODUCTS))
  for (nm in names(products)) s33_assert_outcome_free(products[[nm]], nm)
  s33_assert_outcome_free(frame, "module B frame"); s33_assert_outcome_free(units_frame, "module B units frame")
  list(products = products, gates = gates,
       state = list(frame = frame, units_frame = units_frame, rate_scale = rate_sc, sd_w_rate = sdw_rate,
                    arena_scales = av[[1]]$scales, checkpoints = s33b_log_rows(log)))
}

#' Phase 3: CombZ rows of b04/b05 appended after the unchanged Phase-2 rows; b02, b06, b07; gate GB-12.
s33b_phase3 <- function(design_out, p2, ctx) {
  log <- new.env(parent = emptyenv()); st <- p2$state; pr <- p2$products
  fz <- s33b_outcomes(design_out, st$frame)
  gates <- s33b_gate_gb12(fz)
  sh <- lapply(s33b_share_jobs("cz"), function(sj) s33b_run_share(fz, sj, ctx, log, "phase3"))
  b05_cz <- data.table::rbindlist(lapply(s33b_assoc_jobs("cz"), function(jb) s33b_run_assoc(fz, jb, st$sd_w_rate, "phase3")),
                                  use.names = TRUE)
  tables <- list(
    b01_covariates_animal = pr$b01_covariates_animal, b02_analysis_dataset = s33b_b02(fz, st$units_frame),
    b03_balance_cohort = pr$b03_balance_cohort,
    b04_variance_shares = data.table::rbindlist(list(pr$b04_variance_shares, data.table::rbindlist(lapply(sh, `[[`, "rows"), use.names = TRUE)),
                                                use.names = TRUE),
    b05_associations = data.table::rbindlist(list(pr$b05_associations, b05_cz), use.names = TRUE),
    b06_line_J_animals = s33b_b06(fz), b07_weight_component_coupling = s33b_b07(fz),
    b08_cc1_board_table = pr$b08_cc1_board_table, b09_con_board_rotation = pr$b09_con_board_rotation,
    b10_arena_cohort_means = pr$b10_arena_cohort_means, b11_arena_con_animal_values = pr$b11_arena_con_animal_values,
    b12_arena_cohort_contrasts = pr$b12_arena_cohort_contrasts, b13_arena_profile_agreement = pr$b13_arena_profile_agreement,
    b14_arena_within_cohort_association = pr$b14_arena_within_cohort_association,
    b15_appendix_board_offsets = pr$b15_appendix_board_offsets)
  stopifnot(identical(names(tables), S33_TABLES[["B"]]))
  audit <- list(b_join_audit = pr$b_join_audit,
                b_bootstrap_summaries = data.table::rbindlist(list(pr$b_bootstrap_summaries, s33b_boot_table(lapply(sh, `[[`, "boot"))),
                                                              use.names = TRUE))
  stopifnot(identical(names(audit), S33_AUDIT_TABLES[["B"]]))
  # cross contract (plan section 13): B's partners read b01 (AnimalNum, Batch, tp2, src_cage, cc4_cage, lag_h_cc1) and b03
  # (Batch, lag_h_cc1) directly; there is no separate export
  # stream records for the run audit (audit/seeds_and_streams.csv), one per bootstrap or resampling stream
  streams <- audit$b_bootstrap_summaries[, .(seed_name, seed, B, rng_kinds = rng, scheme = kind, stream_id, unit_order, index_sha256,
                                             replicate_sha256)]
  list(tables = tables, audit = audit, gates = gates,
       checkpoints = data.table::rbindlist(list(st$checkpoints, s33b_log_rows(log)), use.names = TRUE), cross = list(), streams = streams)
}

#' The Phase-2 products re-extracted from the final tables (identical to p2$products).
s33b_outcome_free_extract <- function(out) {
  tb <- c(out$tables, out$audit)
  stats::setNames(lapply(S33B_OUTCOME_FREE_PRODUCTS, function(nm) {
    x <- tb[[nm]]
    if (nm %in% S33B_PHASE2_ROW_PRODUCTS) x[computed_in == "phase2"] else x
  }), S33B_OUTCOME_FREE_PRODUCTS)
}
