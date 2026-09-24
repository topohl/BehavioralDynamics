# ================================================================
# Contract tests for the four core raw-RFID behavioural domains (Stage 28)
# MMMSociability
# ================================================================
# Portable: every structural check builds its fixture in memory. Checks that need
# the E9 dataset under S: are guarded by file.exists()/dir.exists() and SKIP
# cleanly when it is absent, so this runs in CI.
#
# Covers the 22 declared QA requirements; each check is labelled with its number.
# ================================================================

suppressPackageStartupMessages({ library(dplyr); library(tibble); library(purrr); library(tidyr) })

source("Analysis/_pipeline_setup.R")
source_mmm_helper("project_paths.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("first_night_window_helpers.R")
source_mmm_helper("first_night_domain_helpers.R")
source_mmm_helper("rfid_domain_core.R")
source_mmm_helper("rfid_domain_scaling.R")
source_mmm_helper("rfid_acute_window_helpers.R")
source_mmm_helper("rfid_domain_inference.R")
source_mmm_helper("rfid_longitudinal_inference.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
skipped <- character(0)
skip <- function(msg) { skipped <<- c(skipped, msg); invisible(TRUE) }

ROOT <- mmm_project_root()
stage28 <- function(bin) file.path(
  behavior_stage_dir(ROOT, "28", "rfid_behavioral_domains", bin), "tables")
have28 <- dir.exists(stage28("10min"))
rd <- function(p) suppressWarnings(readr::read_csv(
  p, col_types = readr::cols(.default = readr::col_guess()), progress = FALSE))

# ---------------------------------------------- (1) exactly four core domains
check(length(MMM_RFID_CORE_DOMAINS) == 4L, "1: exactly four CORE raw-RFID domains")
check(length(MMM_RFID_CORE_COEFFICIENTS) == 4L, "1: exactly four coefficient vectors")
check(setequal(names(MMM_RFID_CORE_COEFFICIENTS), MMM_RFID_CORE_DOMAINS),
      "1: coefficient names must match the declared core domain set exactly")
check(setequal(names(MMM_RFID_CORE_FORMULAS), MMM_RFID_CORE_DOMAINS),
      "1: formula names must match the declared core domain set exactly")

# ------------------------------------------------- (2) exact primary formulas
zf <- tibble(
  AnimalNum = c("a1", "a2", "a3"), Group = c("CON", "RES", "SUS"),
  Sex = rep("Female", 3),
  Movement_mean_z = c(1, 2, -1), Movement_rmssd_z = c(0.5, -0.5, 0.25),
  Movement_acf1_z = c(0.2, -0.2, 0.7),
  Entropy_mean_z = c(-1, 1, 0.3), Entropy_rmssd_z = c(0.3, 0.7, -0.9),
  Entropy_acf1_z = c(-0.4, 0.4, 0.1),
  Proximity_mean_z = c(0.6, -0.6, 0.2), Proximity_rmssd_z = c(0.1, 0.9, -0.4),
  Proximity_acf1_z = c(-0.2, 0.2, 0.8))
sc <- function(dom, coefs = MMM_RFID_CORE_COEFFICIENTS, fs = MMM_RFID_CORE_FORMULAS) {
  mmm_rfid_score_domain(zf, dom, coefficients = coefs, formulas = fs)$DomainScore
}
check(isTRUE(all.equal(sc("Movement output"), zf$Movement_mean_z)),
      "2: Movement output must be exactly Movement_mean_z")
check(isTRUE(all.equal(sc("Spatial entropy dynamics"),
                       0.5 * (zf$Entropy_mean_z + zf$Entropy_rmssd_z) - zf$Entropy_acf1_z)),
      "2: Spatial entropy dynamics must be 0.5*(Em+Er) - Ea")
check(isTRUE(all.equal(sc("Social-spatial organization"),
                       0.5 * (zf$Proximity_mean_z + zf$Proximity_acf1_z) - zf$Proximity_rmssd_z)),
      "2: Social-spatial organization must be 0.5*(Pm+Pa) - Pr")
check(isTRUE(all.equal(sc("Cross-channel behavioral volatility"),
                       (zf$Movement_rmssd_z + zf$Entropy_rmssd_z + zf$Proximity_rmssd_z) / 3)),
      "2: Cross-channel volatility must be the equal-weight mean of the three RMSSD terms")

# The three retained formulas must be IDENTICAL to the audited legacy ones.
check(isTRUE(all.equal(sc("Spatial entropy dynamics"),
                       mmm_first_night_score_domain(zf, "Behavioral flexibility / predictability")$DomainScore)),
      "2: Spatial entropy dynamics must equal the legacy flexibility formula exactly")
check(isTRUE(all.equal(sc("Social-spatial organization"),
                       mmm_first_night_score_domain(zf, "Social spatial organization")$DomainScore)),
      "2: Social-spatial organization must equal the legacy formula exactly")
check(isTRUE(all.equal(sc("Cross-channel behavioral volatility"),
                       mmm_first_night_score_domain(zf, "Behavioral volatility / fragmentation")$DomainScore)),
      "2: Cross-channel volatility must equal the legacy volatility formula exactly")
check(isTRUE(all.equal(sc("Movement output"),
                       mmm_first_night_score_domain(zf, "Psychomotor activation")$DomainScore)),
      "2: Movement output must equal the legacy psychomotor formula exactly")

# The printable formula STRING must evaluate to the coefficient score. This closes
# the drift channel between the published formula and the evaluated arithmetic.
for (dm in MMM_RFID_CORE_DOMAINS) {
  fromstring <- eval(parse(text = MMM_RFID_CORE_FORMULAS[[dm]]), envir = zf)
  check(isTRUE(all.equal(as.numeric(fromstring), as.numeric(sc(dm)))),
        paste0("2: printable formula string must evaluate to the coefficient score for '", dm, "'"))
}
# Equal-weight sensitivity strings likewise.
for (dm in names(MMM_RFID_SENSITIVITY_COEFFICIENTS)) {
  fromstring <- eval(parse(text = MMM_RFID_SENSITIVITY_FORMULAS[[dm]]), envir = zf)
  got <- sc(dm, MMM_RFID_SENSITIVITY_COEFFICIENTS, MMM_RFID_SENSITIVITY_FORMULAS)
  check(isTRUE(all.equal(as.numeric(fromstring), as.numeric(got))),
        paste0("2: equal-weight formula string must evaluate to its coefficient score for '", dm, "'"))
}
# The primary entropy formula must NOT be the generic equal-weight rowMean.
check(!isTRUE(all.equal(sc("Spatial entropy dynamics"),
                        (zf$Entropy_mean_z + zf$Entropy_rmssd_z - zf$Entropy_acf1_z) / 3)),
      "2: the primary entropy formula must differ from the equal-term variant")

# -------------------------------------------------- (3) no HMM domain in core
check(!any(grepl("HMM|latent|latent-state|dwell|state architecture|occupancy organization|persistence",
                 MMM_RFID_CORE_DOMAINS, ignore.case = TRUE)),
      "3: no HMM-derived domain may appear in the CORE set")
check(all(unlist(lapply(MMM_RFID_CORE_COEFFICIENTS, names)) %in% MMM_RFID_CONTRIBUTORS),
      "3: every core contributor must be a standardized RAW RFID feature")
check(all(!mmm_rfid_domain_definition_table()$hmm_derived),
      "3: the shipped definition table must declare hmm_derived = FALSE for all core rows")

# ------------------------------------- (4) no inactive/rest domain in the core
check(!any(grepl("inactive|rest|quiescen|sleep", MMM_RFID_CORE_DOMAINS, ignore.case = TRUE)),
      "4: no inactive/rest domain may appear in the CORE set")
check(all(!mmm_rfid_domain_definition_table()$inactive_phase_derived),
      "4: the definition table must declare inactive_phase_derived = FALSE for all core rows")

# ------------------------------------------ (5,6) strict contributor completeness
zmiss <- zf; zmiss$Proximity_rmssd_z[1] <- NA_real_
vol <- mmm_rfid_score_domain(zmiss, "Cross-channel behavioral volatility")
check(is.na(vol$DomainScore[1]),
      "5: a domain with a missing REQUIRED contributor must be NA")
check(all(is.finite(vol$DomainScore[2:3])), "5: complete animals must still be scored")
check(vol$available_contributor_count[1] == 2L && vol$required_contributor_count[1] == 3L,
      "5: contributor counts must be reported")
check(vol$missing_contributors[1] == "Proximity_rmssd_z", "5: the missing contributor must be named")
# 6: the result must be neither a re-weighted mean NOR a zero-filled score.
naive_mean <- mean(c(zmiss$Movement_rmssd_z[1], zmiss$Entropy_rmssd_z[1]), na.rm = TRUE)
zerofill <- (zmiss$Movement_rmssd_z[1] + zmiss$Entropy_rmssd_z[1] + 0) / 3
check(is.finite(naive_mean) && is.finite(zerofill),
      "6: both na.rm and zero-fill WOULD have produced a value (documents what is forbidden)")
check(!isTRUE(all.equal(vol$DomainScore[1], naive_mean)), "6: must not be an na.rm re-weighted mean")
check(!isTRUE(all.equal(vol$DomainScore[1], zerofill)), "6: must not be a coalesce-to-zero score")
check(!is.na(mmm_rfid_score_domain(zmiss, "Movement output")$DomainScore[1]),
      "5: an unrelated domain must not be penalised by another domain's missing contributor")
# Zero-variance standardization must yield NA, never 0.
fz <- tibble(AnimalNum = as.character(1:4), Sex = "Female", Movement_mean = rep(5, 4))
pz <- mmm_rfid_scaling_parameters(fz, "Movement_mean", "unit-test")
check(is.na(pz$scale[1]), "6: a zero-variance contributor must give scale = NA, never 0")
check(all(is.na(mmm_rfid_apply_scaling(fz, pz, "Movement_mean")$Movement_mean_z)),
      "6: a zero-variance contributor must standardize to NA, never 0")

# -------------------------------------- (7) no temporal bridging across gaps
val <- c(1, 2, 3, 100, 101); slot <- c(1L, 2L, 3L, 6L, 7L)
check(mmm_n_adjacent_pairs(val, slot) == 3L, "7: only genuinely adjacent slot pairs count")
check(isTRUE(all.equal(mmm_rmssd_adjacent(val, slot), 1)),
      "7: RMSSD must not be inflated by bridging the 3->100 jump across the gap")
check(sqrt(mean(diff(val)^2)) > 40, "7: the bridged RMSSD would be far larger (documents the defect)")
check(mmm_n_adjacent_pairs(c(1, 2, 3), c(1L, 5L, 9L)) == 0L,
      "7: isolated slots yield no adjacent pairs")
check(is.na(mmm_rmssd_adjacent(c(1, 2, 3), c(1L, 5L, 9L))),
      "7: RMSSD must be NA when no adjacent pair exists")
check(mmm_n_adjacent_pairs(c(1, NA, 3, 4), 1:4) == 1L,
      "7: an internal NA must invalidate both pairs touching it")
check(MMM_FIRST_NIGHT_MIN_PAIRS_RMSSD == 2L && MMM_FIRST_NIGHT_MIN_PAIRS_ACF1 == 3L,
      "7: minimum valid adjacent-pair requirements must be preserved")

# --------------------------- (8,9,10,11) window selector on a synthetic fixture
animal_group <- c(m1 = "CON", m2 = "RES", m3 = "SUS", m4 = "RES")
mk <- function(cc, day, animals, systems, batch = "B1") {
  t0 <- as.POSIXct(paste0(day, " 18:30:00"), tz = "UTC")
  tidyr::crossing(AnimalNum = animals, slot = 0:71) %>%
    mutate(BinStart = t0 + .data$slot * 600, CageChange = cc, Phase = "Active",
           Batch = batch, System = systems[match(.data$AnimalNum, animals)],
           SourceFile = paste0("E9_SIS_", batch, "_", cc, "_x.csv"),
           Group = unname(animal_group[.data$AnimalNum]),
           Sex = "Male", Movement = as.numeric(.data$slot %% 5),
           Entropy = as.numeric((.data$slot %% 7) / 7),
           ProximityFraction = as.numeric((.data$slot %% 3) / 3))
}
animals <- c("m1", "m2", "m3", "m4"); systems <- c("sys.1", "sys.1", "sys.2", "sys.2")
fix <- bind_rows(
  mk("CC1", "2022-10-28", animals, systems), mk("CC2", "2022-11-01", animals, rev(systems)),
  mk("CC3", "2022-11-05", animals, systems), mk("CC4", "2022-11-09", animals, rev(systems)))

sel1 <- mmm_rfid_select_acute_window(fix, 600, "CC1")
check(nrow(sel1) == 4 * 72, "8: the CC1 selector must return exactly 72 slots per animal")
check(all(sel1$expected_slots == 72L), "8: expected_slots must be 72 at 10 min for a 12 h window")
check(all(format(sel1$target_window_start, "%H:%M") == "18:30"),
      "8: the CC1 window must start at 18:30")
check(all(format(sel1$target_window_end, "%H:%M") == "06:30"),
      "8: the CC1 window must end at 06:30")
check(max(sel1$elapsed_sec_in_window) < 12 * 3600 && min(sel1$elapsed_sec_in_window) >= 0,
      "8: every selected row must lie in [0, 12 h)")
# 8: the generalized selector must reproduce the canonical first-night selector.
canon <- mmm_select_first_night_window(fix, bin_size_sec = 600)
check(nrow(canon) == nrow(sel1) &&
        isTRUE(all.equal(sort(canon$elapsed_sec_in_window), sort(sel1$elapsed_sec_in_window))),
      "8: mmm_rfid_select_acute_window(CC1) must reproduce mmm_select_first_night_window() exactly")

allw <- mmm_rfid_select_all_acute_windows(fix, 600)
check(setequal(unique(allw$CageChangeLabel), c("CC1", "CC2", "CC3", "CC4")),
      "9: the generalized selector must cover CC1-CC4")
check(all(table(allw$CageChangeLabel) == 4 * 72),
      "9: every cage change must yield the same number of selected rows")
check(all(format(allw$target_window_start, "%H:%M") == "18:30"),
      "9: every cage change window must use the identical 18:30 clock anchor")
val_ev <- mmm_rfid_assert_acute_window_validity(fix, 600)
check(nrow(val_ev) == 4 && all(val_ev$anchor_is_declared_clock),
      "9: the anchor-validity assertion must pass for all four sessions")
check(all(val_ev$first_active_is_session_start),
      "9: the first Active block must coincide with each session start")

qc <- mmm_rfid_acute_window_qc(allw)
check(nrow(qc) == 16L, "10: QC must have exactly one row per animal per cage change")
check(all(qc$observed_slots == 72L) && all(qc$window_complete),
      "10: the synthetic fixture must be complete in every window")
check(all(qc$missing_leading_slots == 0L) && all(qc$missing_interior_slots == 0L) &&
        all(qc$missing_trailing_slots == 0L),
      "10: leading/interior/trailing gaps must be reported separately and be zero here")
mmm_rfid_assert_one_cage_per_animal_window(qc)
check(dplyr::n_distinct(qc$CageEpochID) == 8L,
      "10: 2 systems x 4 cage changes must give 8 distinct cage epochs")
check(all(c("Batch", "System", "SourceFile") %in% names(qc)),
      "11: Batch/System/SourceFile provenance must be retained in the window QC")
check(identical(mmm_rfid_cage_epoch_id("B1", "sys.1", "CC1"), "B1|sys.1|CC1"),
      "11: the cage-epoch key must be Batch x System x CageChange")
# A cage id must NOT be System alone: the fixture moves animals between systems.
check(dplyr::n_distinct(paste(qc$Batch, qc$System)) < dplyr::n_distinct(qc$CageEpochID),
      "11: System alone must not be used as a cage id; it pools across cage changes")
# Provenance must survive into the feature table too.
feat_fix <- mmm_rfid_build_window_features(allw)
check(all(c("Batch", "System", "SourceFile", "CageChangeLabel") %in% names(feat_fix)),
      "11: Batch/System/SourceFile must be retained on the feature table")

# ------------------------------- (12,13) first-night scaling within Sex only
fs <- tibble(AnimalNum = as.character(1:8), Group = rep(c("CON", "SUS"), 4),
             Sex = rep(c("Female", "Male"), each = 4),
             Movement_mean = c(1, 2, 3, 4, 101, 102, 103, 104))
st <- mmm_rfid_standardize_within_sex(fs, "Movement_mean")
zz <- st$scaled
check(abs(mean(zz$Movement_mean_z[zz$Sex == "Female"])) < 1e-12 &&
        abs(mean(zz$Movement_mean_z[zz$Sex == "Male"])) < 1e-12,
      "12: z must be mean-zero WITHIN each Sex")
check(isTRUE(all.equal(zz$Movement_mean_z[zz$Sex == "Female"],
                       zz$Movement_mean_z[zz$Sex == "Male"])),
      "12: an additive Sex shift must vanish after within-Sex standardization")
check(nrow(st$parameters) == 2L, "12: scaling parameters must be exported per Sex")
check(all(c("center", "scale", "scaling_reference") %in% names(st$parameters)),
      "12: exported parameters must carry center, scale and their declared reference")
# 13: standardizing within Group would make each Group mean-zero. It must not.
gm <- tapply(zz$Movement_mean_z, paste(zz$Sex, zz$Group), mean)
check(any(abs(gm) > 1e-8), "13: scaling must NOT be within Group (Group means must not all be 0)")
check(!any(grepl("Group", st$parameters$scaling_unit)) ||
        grepl("never within Group", st$parameters$scaling_unit[1]),
      "13: the declared scaling unit must state that Group is never used")

# ----------------- (14,15) longitudinal scaling fixed from CC1, within Sex
longfix <- feat_fix
cc1ref <- longfix %>% filter(.data$CageChangeLabel == "CC1")
p_cc1 <- mmm_rfid_scaling_parameters(cc1ref, reference_label = "CC1_fixed")
zl <- mmm_rfid_apply_scaling(longfix, p_cc1)
check(nrow(p_cc1) == length(MMM_RFID_RAW_FEATURES) * dplyr::n_distinct(cc1ref$Sex),
      "14: CC1 reference parameters must be one row per Sex x feature")
# The parameters applied to CC1 rows must reproduce a CC1-only within-Sex z.
z_direct <- mmm_rfid_standardize_within_sex(cc1ref)$scaled
z_applied <- zl %>% filter(.data$CageChangeLabel == "CC1")
check(isTRUE(all.equal(sort(z_direct$Movement_mean_z), sort(z_applied$Movement_mean_z))),
      "14: applying CC1-reference parameters to CC1 must equal a direct CC1 within-Sex z")
# 15: the scaling must NOT be re-estimated per cage change. If it were, every cage
# change would have mean 0 for every feature.
percc <- zl %>% group_by(.data$CageChangeLabel) %>%
  summarise(m = mean(.data$Movement_mean_z, na.rm = TRUE), .groups = "drop")
check(any(abs(percc$m) > 1e-8) || dplyr::n_distinct(longfix$Movement_mean) == 1L,
      "15: longitudinal z must NOT be re-centred within CageChange")
check(all(grepl("never within CageChange", p_cc1$scaling_unit)),
      "15: the declared scaling unit must state that CageChange is never used")

# ---------------------- (16) longitudinal CC1 scores reproduce first-night scores
fn_scores <- mmm_rfid_score_all_domains(
  mmm_rfid_standardize_within_sex(cc1ref)$scaled,
  id_cols = c("AnimalNum", "Group", "Sex")) %>%
  arrange(.data$Domain, .data$AnimalNum)
lg_scores <- mmm_rfid_score_all_domains(zl, id_cols = c("AnimalNum", "Group", "Sex",
                                                        "CageChangeLabel")) %>%
  filter(.data$CageChangeLabel == "CC1") %>%
  arrange(.data$Domain, .data$AnimalNum)
check(nrow(fn_scores) == nrow(lg_scores),
      "16: CC1 rows of the longitudinal scores must match the first-night score count")
check(isTRUE(all.equal(fn_scores$DomainScore, lg_scores$DomainScore)),
      "16: longitudinal CC1 scores must reproduce the first-night scores exactly")

# ------------------------------------- (17-20) multiplicity family sizes
if (have28) {
  omni <- rd(file.path(stage28("10min"), "first_night_domain_omnibus_primary.csv"))
  check(all(omni$n_tests_in_family == 4L),
        "17: the first-night primary BH family must declare exactly 4 tests")
  per_sex <- omni %>% count(.data$Sex)
  check(all(per_sex$n == 4L) && nrow(per_sex) == 2L,
        "17: exactly four domain omnibus Group tests per Sex")
  check(dplyr::n_distinct(omni$family_id) == 2L,
        "17: the first-night primary family must be declared separately within each Sex")

  inter <- rd(file.path(stage28("10min"), "first_night_group_sex_interaction.csv"))
  check(nrow(inter) == 4L && all(inter$n_tests_in_family == 4L),
        "18: the first-night Group:Sex BH family must be exactly four domain tests")

  traj <- rd(file.path(stage28("10min"), "longitudinal_trajectory_omnibus_primary.csv"))
  check(all(traj$n_tests_in_family == 4L),
        "19: the longitudinal trajectory BH family must declare exactly 4 tests")
  tps <- traj %>% count(.data$Sex)
  check(all(tps$n == 4L) && nrow(tps) == 2L,
        "19: exactly four Group x CageChange tests per Sex")
  check(all(traj$term == "Group:CageChange"),
        "19: the primary longitudinal test must be the Group x CageChange interaction")

  three <- rd(file.path(stage28("10min"), "longitudinal_three_way_interaction.csv"))
  check(nrow(three) == 4L && all(three$n_tests_in_family == 4L),
        "20: the three-way interaction family must be exactly four tests")
  check(all(three$term == "Group:Sex:CageChange"),
        "20: the three-way family must test Group x Sex x CageChange")

  # Gating: no pairwise contrast may carry an adjusted p under an unsupported parent.
  pw <- rd(file.path(stage28("10min"), "first_night_pairwise_contrasts.csv"))
  ungated <- pw %>% filter(.data$pairwise_role == "descriptive_non_gated")
  check(all(is.na(ungated$p_holm_within_domain)),
        "17: pairwise contrasts under an unsupported parent must carry NO adjusted p")
  check(all(is.finite(ungated$hedges_g) | is.na(ungated$hedges_g)),
        "17: non-gated contrasts must still export an effect size for transparency")
} else {
  skip("17-20: Stage 28 outputs absent (S: project root not mounted)")
}

# ------------------------------------- (21) legacy five-domain output preserved
check(length(MMM_FIRST_NIGHT_DISPLAYED_DOMAINS) == 5L,
      "21: the legacy five-domain contract must remain intact")
check("Active-phase adaptation / exploration" %in% MMM_FIRST_NIGHT_DISPLAYED_DOMAINS,
      "21: the legacy fifth domain must still be present in the legacy contract")
check(identical(MMM_RFID_EXCLUDED_LEGACY_DOMAIN$name, "Active-phase adaptation / exploration"),
      "21: the excluded legacy domain must be documented by name")
check(nzchar(MMM_RFID_EXCLUDED_LEGACY_DOMAIN$exclusion_reason) &&
        grepl("does not operationally measure adaptation",
              MMM_RFID_EXCLUDED_LEGACY_DOMAIN$exclusion_reason),
      "21: the exclusion must be justified as a construct decision")
check(!grepl("p-value|significan", MMM_RFID_EXCLUDED_LEGACY_DOMAIN$exclusion_reason,
             ignore.case = TRUE) ||
        grepl("NOT removed on the basis of any p-value",
              MMM_RFID_EXCLUDED_LEGACY_DOMAIN$exclusion_reason),
      "21: the exclusion reason must state it was not p-value driven")
legacy_dir <- mmm_behavior_output_active_root("first_night_10min", project_root = ROOT)
if (dir.exists(legacy_dir)) {
  lg <- rd(file.path(legacy_dir, "first_night_group_contrasts.csv"))
  check(dplyr::n_distinct(lg$Domain) == 5L,
        "21: the shipped legacy artifact must still carry five displayed domains")
  check(all(lg$n_tests_in_family == 15L),
        "21: the legacy flat 15-test family must be preserved unchanged")
} else {
  skip("21: legacy first-night artifacts absent (S: project root not mounted)")
}

# ------------------------------ (22) Stage 09 canonical results unchanged
protected <- c("Analysis/09_early_prediction_model_ladder.R",
               "Analysis/01_build_multiscale_behavior_metrics.R",
               "Analysis/build_later_outcome_combz.R",
               "Functions/animalpos_preprocessing_helpers.R",
               "Functions/behavioral_dynamics_stats_helpers.R",
               "Functions/duration_normalization_helpers.R",
               "Functions/figure1_prediction_contract.R",
               "Functions/first_night_window_helpers.R",
               "Functions/first_night_domain_helpers.R",
               "Functions/first_night_domain_driver.R",
               "Analysis/_pipeline_setup.R")
# The invariant this refactor controls is that STAGE 28 did not touch Stage 09 --
# not that Stage 09 is byte-identical to main, which is a property of the whole
# working tree and can legitimately change through unrelated parallel work. The
# authoritative numeric gates for Stage 09 are its own tests
# (test_stage09_primary_window.R, test_stage09_endpoint_identity.R,
# test_stage09_resolution_contract.R, test_stage09_permutation_draws.R), which
# parse Stage 09's real source and assert its frozen constants.
#
# So: FAIL if any protected file references this refactor's modules, and REPORT
# (loudly, without failing) any other drift so it cannot pass unnoticed.
rfid_modules <- c("rfid_domain_core", "rfid_domain_scaling", "rfid_acute_window_helpers",
                  "rfid_domain_inference", "rfid_longitudinal_inference",
                  "28_rfid_behavioral_domains", "MMM_RFID_")
for (pf in protected) {
  if (!file.exists(pf)) next
  src <- readLines(pf, warn = FALSE)
  hit <- rfid_modules[vapply(rfid_modules, function(m) any(grepl(m, src, fixed = TRUE)),
                             logical(1))]
  check(length(hit) == 0L,
        paste0("22: protected file '", pf, "' must not reference this refactor's ",
               "modules; found: ", paste(hit, collapse = ", ")))
}

gitref <- suppressWarnings(system2("git", c("rev-parse", "--verify", "-q", "main"),
                                   stdout = TRUE, stderr = FALSE))
if (length(gitref) == 1L && nzchar(gitref)) {
  changed <- suppressWarnings(system2("git", c("diff", "--name-only", "main", "--"),
                                      stdout = TRUE, stderr = FALSE))
  drift <- intersect(protected, changed)
  if (length(drift) > 0L) {
    cat("NOTE [22]: protected file(s) differ from 'main'. The current path\n",
        "           migration may account for some differences. Stage 28 does not\n",
        "           reference any of them (checked above),\n",
        "           and the Stage 09 contract tests are the authoritative numeric gate.\n",
        "           Differing: ", paste(drift, collapse = ", "), "\n", sep = "")
  }
} else {
  skip("22: git ref 'main' unavailable; protected-file drift not reported")
}
# Stage 28 must never write into the Stage 09 or legacy first-night output trees.
s28 <- normalizePath(file.path(behavior_stage_dir(ROOT, "28", "rfid_behavioral_domains", "10min")),
                     winslash = "/", mustWork = FALSE)
s09 <- normalizePath(behavior_stage_tables(ROOT, "09", "early_prediction", "10min"),
                     winslash = "/", mustWork = FALSE)
check(!startsWith(s28, s09) && !startsWith(s09, s28),
      "22: the Stage 28 output tree must be disjoint from the Stage 09 output tree")
check(!startsWith(s28, normalizePath(legacy_dir, winslash = "/", mustWork = FALSE)),
      "22: the Stage 28 output tree must be disjoint from the legacy first-night tree")
stage28_src <- readLines("Analysis/28_rfid_behavioral_domains.R", warn = FALSE)
check(!any(grepl("09_early_prediction|early_prediction|write.*first_night/10min_based",
                 stage28_src)),
      "22: Stage 28 must not reference or write into any Stage 09 artifact path")

# ------------------------- (23) orthogonal manipulation/phenotype decomposition
check(length(MMM_RFID_ORTHOGONAL_CONTRASTS) == 2L,
      "23: exactly two orthogonal Group contrasts")
cs <- MMM_RFID_ORTHOGONAL_CONTRASTS$stress_vs_CON
sr <- MMM_RFID_ORTHOGONAL_CONTRASTS$SUS_vs_RES
check(abs(sum(cs)) < 1e-12 && abs(sum(sr)) < 1e-12,
      "23: both contrasts must sum to zero")
check(abs(sum(cs * sr)) < 1e-12,
      "23: the manipulation and phenotype contrasts must be ORTHOGONAL")
check(isTRUE(all.equal(cs, c(-1, 0.5, 0.5))), "23: stress_vs_CON = (RES+SUS)/2 - CON")
check(isTRUE(all.equal(sr, c(0, -1, 1))), "23: SUS_vs_RES = SUS - RES")

set.seed(11)
# Cages must be MIXED, or Group is perfectly nested in cage and the contrasts are
# inestimable - which is exactly the pathology the real CON arm has.
ofix_group <- rep(c("CON", "RES", "SUS"), times = 24)
ofix <- tibble(
  AnimalNum = as.character(1:72), Domain = MMM_RFID_CORE_DOMAINS[1],
  Group = ofix_group,
  Sex = rep(c("Female", "Male"), 36),
  Batch = rep(c("B1", "B2", "B3"), each = 24),
  CageEpochID = paste0("cage", rep(1:18, each = 4)),
  DomainScore = ifelse(ofix_group == "CON", rnorm(72, 0), rnorm(72, 0.6)))
oc <- mmm_rfid_orthogonal_contrasts(ofix, MMM_RFID_CORE_DOMAINS[1])
check(nrow(oc) == 2L, "23: one row per orthogonal contrast")
check(setequal(oc$contrast, c("stress_vs_CON", "SUS_vs_RES")),
      "23: both contrasts must be returned")
check(oc$identification[oc$contrast == "SUS_vs_RES"] == "within_cage",
      "23: SUS_vs_RES must be labelled within-cage")
check(grepl("between_cage", oc$identification[oc$contrast == "stress_vs_CON"]),
      "23: stress_vs_CON must be labelled between-cage")
check(all(is.finite(oc$mde_hedges_g)), "23: an MDE must accompany every contrast")
# By construction RES and SUS share a mean here, so the phenotype contrast is ~0
# and the manipulation contrast is ~0.5.
check(abs(oc$estimate[oc$contrast == "SUS_vs_RES"]) <
        abs(oc$estimate[oc$contrast == "stress_vs_CON"]),
      "23: the fixture's manipulation effect must exceed its phenotype effect")

# ------------------- (24) separate families; exploratory status recorded
if (have28) {
  op <- rd(file.path(stage28("10min"), "first_night_orthogonal_contrasts_pooled.csv"))
  check(nrow(op) == 8L, "24: 4 domains x 2 orthogonal contrasts = 8 rows")
  check(dplyr::n_distinct(op$family_id) == 2L,
        "24: the manipulation and phenotype contrasts must be SEPARATE BH families")
  check(all(op$n_tests_in_family == 4L),
        "24: each orthogonal family must declare exactly 4 domain tests")

  wc <- rd(file.path(stage28("10min"), "longitudinal_window_contrasts_by_cagechange.csv"))
  check(all(c("q_narrow_within_window", "q_wide_all_windows", "analysis_status") %in% names(wc)),
        "24: window contrasts must ship BOTH multiplicity declarations and a status")
  ch <- wc[wc$contrast_kind != "per_cage_change", ]
  check(all(ch$analysis_status == "EXPLORATORY_window_chosen_after_inspection"),
        "24: window contrasts must be marked EXPLORATORY")
  pcc <- wc[wc$contrast_kind == "per_cage_change", ]
  check(all(pcc$analysis_status == "PLANNED_decomposition"),
        "24: per-cage-change estimates must be marked PLANNED")
  check(all(is.na(pcc$q_wide_all_windows)),
        "24: planned per-cage-change estimates must not carry a wide-family q")
  check(all(ch$n_wide_family == 12L),
        "24: the wide family must be 4 domains x 3 window choices = 12")
  # The wide family must genuinely be wider.
  check(all(ch$n_wide_family > ch$n_narrow_family),
        "24: the wide family must contain more tests than the narrow family")
  # BH q is NOT monotone in family size at mid ranks - a test can move to a lower
  # relative rank in the larger family and receive a smaller q. The guarantee holds
  # for the MINIMUM p, which is rank 1 in both families, and that is the value any
  # headline claim rests on. Assert exactly that, per contrast type.
  for (cn in unique(ch$contrast)) {
    sub <- ch[ch$contrast == cn & is.finite(ch$raw_p), ]
    if (nrow(sub) == 0L) next
    i <- which.min(sub$raw_p)
    check(sub$q_wide_all_windows[i] >= sub$q_narrow_within_window[i] - 1e-12,
          paste0("24: for the smallest p in contrast ", cn,
                 ", the wide-family q must not be more lenient than the narrow one"))
  }
}

# ----------------- (25) leading-bin burden audited across ALL cage changes
seed_dir <- mmm_behavior_output_active_root("rfid_leading_bin_seed_audit", project_root = ROOT)
if (dir.exists(seed_dir)) {
  bcc <- file.path(seed_dir, "leading_zero_burden_by_cagechange.csv")
  check(file.exists(bcc),
        "25: the seed audit must report the leading-bin burden for every cage change")
  b <- rd(bcc)
  check(nrow(b) >= 2L, "25: burden must be reported for more than one cage change")
  check(file.exists(file.path(seed_dir, "burden_change_by_group.csv")),
        "25: the seed audit must test whether the CC1->CC2 burden change differs by Group")
} else {
  skip("25: seed audit outputs absent (S: project root not mounted)")
}

if (length(skipped) > 0L) {
  cat("RFID domain contract: SKIPPED ", length(skipped), " data-dependent check group(s):\n",
      paste0("  - ", skipped, collapse = "\n"), "\n", sep = "")
}
cat("RFID four-domain contract checks: PASS\n")
