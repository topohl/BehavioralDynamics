# Contract tests for the production first-night domain analysis.
# Locks the audited formulas, adjacency-aware temporal features, strict
# contributor completeness, within-Sex standardization, sign orientation and
# explicit FDR family sizes.

suppressPackageStartupMessages({ library(dplyr); library(tibble); library(purrr) })

source("Analysis/_pipeline_setup.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")
source_mmm_helper("first_night_window_helpers.R")
source_mmm_helper("hmm_stage14_helpers.R")
source_mmm_helper("first_night_domain_helpers.R")
source_mmm_helper("first_night_domain_driver.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)

# ---------------------------------------------------------------- A. adjacency
# Slots 1,2,3 then a GAP then 6,7. The pair (3,6) is not adjacent and must be
# excluded; bridging it would invent a huge difference.
val <- c(1, 2, 3, 100, 101)
slot <- c(1L, 2L, 3L, 6L, 7L)
check(mmm_n_adjacent_pairs(val, slot) == 3L, "A: exactly 3 adjacent pairs (1-2, 2-3, 6-7)")
check(isTRUE(all.equal(mmm_rmssd_adjacent(val, slot), 1)),
      "A: RMSSD over adjacent pairs must be 1, not inflated by the bridged 3->100 jump")
bridged <- sqrt(mean(diff(val)^2))
check(bridged > 40, "A: the naive bridged RMSSD must be far larger (documents the bug)")

# A gap must not be closed up by ordering alone.
check(mmm_n_adjacent_pairs(c(1, 2, 3), c(1L, 5L, 9L)) == 0L,
      "A: no adjacent pairs when every slot is isolated")
check(is.na(mmm_rmssd_adjacent(c(1, 2, 3), c(1L, 5L, 9L))),
      "A: RMSSD must be NA when there are no adjacent pairs")
check(is.na(mmm_acf1_adjacent(c(1, 2), c(1L, 2L))),
      "A: ACF1 must be NA below the minimum adjacent-pair count")

# Non-finite values break adjacency for the pairs that touch them.
check(mmm_n_adjacent_pairs(c(1, NA, 3, 4), 1:4) == 1L,
      "A: an internal NA must invalidate both pairs that touch it")

# ACF1 equals the Pearson correlation of the adjacent pair set.
set.seed(1); v <- cumsum(rnorm(40)); s <- 1:40
check(isTRUE(all.equal(mmm_acf1_adjacent(v, s), cor(v[-40], v[-1]))),
      "A: on a complete grid ACF1 must equal the lag-1 Pearson correlation")

# ------------------------------------------------------ B. exact five formulas
zf <- tibble(
  AnimalNum = c("a1", "a2"), Group = c("CON", "RES"), Sex = c("Female", "Female"),
  Movement_mean_z = c(1, 2), Movement_rmssd_z = c(0.5, -0.5), Movement_acf1_z = c(0.2, -0.2),
  Entropy_mean_z = c(-1, 1), Entropy_rmssd_z = c(0.3, 0.7), Entropy_acf1_z = c(-0.4, 0.4),
  Proximity_mean_z = c(0.6, -0.6), Proximity_rmssd_z = c(0.1, 0.9), Proximity_acf1_z = c(-0.2, 0.2)
)
sc <- function(dom) mmm_first_night_score_domain(zf, dom)$DomainScore
check(isTRUE(all.equal(sc("Psychomotor activation"), zf$Movement_mean_z)), "B1: psychomotor")
check(isTRUE(all.equal(sc("Behavioral flexibility / predictability"),
                       0.5 * (zf$Entropy_mean_z + zf$Entropy_rmssd_z) - zf$Entropy_acf1_z)),
      "B2: flexibility must weight the mean/rmssd pair 0.5 each and acf1 -1.0")
check(isTRUE(all.equal(sc("Social spatial organization"),
                       0.5 * (zf$Proximity_mean_z + zf$Proximity_acf1_z) - zf$Proximity_rmssd_z)),
      "B3: social spatial organization")
check(isTRUE(all.equal(sc("Behavioral volatility / fragmentation"),
                       (zf$Movement_rmssd_z + zf$Entropy_rmssd_z + zf$Proximity_rmssd_z) / 3)),
      "B4: volatility must be the equal-weight mean of the three RMSSD terms")
check(isTRUE(all.equal(sc("Active-phase adaptation / exploration"),
                       (zf$Movement_mean_z + zf$Entropy_mean_z + zf$Proximity_mean_z) / 3 -
                         (zf$Movement_acf1_z + zf$Entropy_acf1_z) / 2)),
      "B5: adaptation must use /3 for means and /2 for the acf1 pair")

# The generic equal-weight scorer must NOT reproduce flexibility: 1/3,1/3,-1/3
# is a different construct from 0.5,0.5,-1.
generic_flex <- (zf$Entropy_mean_z + zf$Entropy_rmssd_z - zf$Entropy_acf1_z) / 3
check(!isTRUE(all.equal(sc("Behavioral flexibility / predictability"), generic_flex)),
      "B: the audited flexibility formula must differ from a generic equal-weight rowMean")

# Audit-only candidates reproduce their documented algebraic identities.
check(isTRUE(all.equal(sc("Early active spatial flexibility"),
                       sc("Behavioral flexibility / predictability") +
                         0.5 * (zf$Entropy_acf1_z - zf$Movement_acf1_z))),
      "B: #8 = #2 + 0.5*(Ea - Ma)")
check(isTRUE(all.equal(sc("Early social engagement"),
                       sc("Social spatial organization") +
                         0.5 * (zf$Proximity_mean_z - zf$Proximity_acf1_z))),
      "B: #9 = #3 + 0.5*(Pm - Pa)")
check(isTRUE(all.equal(sc("Early social withdrawal"),
                       sc("Psychomotor activation") - zf$Proximity_mean_z)),
      "B: #10 = #1 - Pm")

# --------------------------------------------------- C. strict completeness
zmiss <- zf
zmiss$Proximity_rmssd_z[1] <- NA_real_
vol <- mmm_first_night_score_domain(zmiss, "Behavioral volatility / fragmentation")
check(is.na(vol$DomainScore[1]),
      "C: a domain with a missing REQUIRED contributor must be NA, never a re-weighted mean")
check(!is.na(vol$DomainScore[2]), "C: the complete animal must still be scored")
check(vol$available_contributor_count[1] == 2L && vol$required_contributor_count[1] == 3L,
      "C: contributor counts must be reported")
check(vol$missing_contributors[1] == "Proximity_rmssd_z", "C: the missing contributor must be named")
check(!vol$complete_contributors[1], "C: complete_contributors flag must be FALSE")
naive_reweighted <- mean(c(zmiss$Movement_rmssd_z[1], zmiss$Entropy_rmssd_z[1]), na.rm = TRUE)
check(is.finite(naive_reweighted),
      "C: an na.rm=TRUE mean WOULD have produced a value (documents the silent re-weighting)")
# A domain that does not require the missing contributor is unaffected.
check(!is.na(mmm_first_night_score_domain(zmiss, "Psychomotor activation")$DomainScore[1]),
      "C: an unrelated domain must not be penalised by another domain's missing contributor")

# ------------------------------------------ D. within-Sex standardization
fs <- tibble(AnimalNum = as.character(1:8), Group = rep(c("CON", "RES"), 4),
             Sex = rep(c("Female", "Male"), each = 4),
             Movement_mean = c(1, 2, 3, 4, 101, 102, 103, 104))
st <- mmm_first_night_standardize_within_sex(fs, "Movement_mean")
zz <- st$scaled
check(abs(mean(zz$Movement_mean_z[zz$Sex == "Female"])) < 1e-12 &&
        abs(mean(zz$Movement_mean_z[zz$Sex == "Male"])) < 1e-12,
      "D: z must be mean-zero WITHIN each Sex")
check(isTRUE(all.equal(zz$Movement_mean_z[zz$Sex == "Female"], zz$Movement_mean_z[zz$Sex == "Male"])),
      "D: an additive Sex shift must vanish after within-Sex standardization")
check(nrow(st$parameters) == 2L, "D: standardization parameters must be exported per Sex")
# Zero variance must yield NA, not 0, so strict completeness catches it.
fz <- tibble(AnimalNum = as.character(1:4), Group = "CON", Sex = "Female", Movement_mean = rep(5, 4))
check(all(is.na(mmm_first_night_standardize_within_sex(fz, "Movement_mean")$scaled$Movement_mean_z)),
      "D: a zero-variance contributor must standardize to NA, never 0")

# ------------------------------------------------- E. inference and orientation
set.seed(7)
inf_dat <- tibble(
  AnimalNum = as.character(1:60), Domain = "Psychomotor activation",
  Group = rep(c("CON", "RES", "SUS"), each = 20),
  Sex = rep(c("Female", "Male"), 30),
  DomainScore = c(rnorm(20, 0), rnorm(20, -1), rnorm(20, 0.5))
)
res <- mmm_first_night_domain_inference(inf_dat, "Psychomotor activation")
ct <- res$contrasts
check(nrow(ct) == 6L, "E: 2 sexes x 3 contrasts = 6 rows")
check(all(ct$contrast_orientation == "comp - ref"), "E: orientation must be recorded as comp - ref")
mmm_assert_effect_sign_agreement(ct, label = "E fixture")
rc <- ct %>% filter(Sex == "Female", contrast == "RES-CON")
check(rc$estimate < 0 && rc$hedges_g < 0,
      "E: RES-CON must be negative for both estimate and g when RES < CON by construction")
check(rc$group_ref == "CON" && rc$group_comp == "RES", "E: ref/comp must be labelled correctly")
check(is.finite(res$interaction$F_value), "E: the Group:Sex F must be estimable")

# A deliberately sign-flipped table must be rejected.
bad <- ct; bad$hedges_g <- -bad$hedges_g
flip_err <- tryCatch({ mmm_assert_effect_sign_agreement(bad); NULL }, error = function(e) e)
check(inherits(flip_err, "error"), "E: sign disagreement between estimate and g must fail closed")

# Duplicate animals must fail closed: the panel is one value per animal.
dup <- bind_rows(inf_dat, inf_dat[1, ])
dup_err <- tryCatch({ mmm_first_night_domain_inference(dup, "Psychomotor activation"); NULL },
                    error = function(e) e)
check(inherits(dup_err, "error"), "E: duplicate animal rows must fail closed")

# ------------------------------------------------------- F. explicit FDR family
p <- c(0.001, 0.02, 0.30, NA, 0.5)
q_declared <- mmm_first_night_bh(p, declared_n = 15L, family_id = "TEST")
q_shrunk <- p.adjust(p, method = "BH")
check(all(q_declared[is.finite(p)] >= q_shrunk[is.finite(p)] - 1e-12),
      "F: adjusting against the declared n must never be more lenient than the NA-shrunk family")
check(isTRUE(all.equal(q_declared[1], 0.001 * 15)),
      "F: the smallest p must be scaled by the DECLARED family size")
check(is.na(q_declared[4]), "F: a non-estimable cell stays NA")

# G. Displayed row set is exactly five and contains no HMM-derived row.
check(length(MMM_FIRST_NIGHT_DISPLAYED_DOMAINS) == 5L, "G: exactly five displayed domains")
check(!any(grepl("Latent-state|occupancy organization|persistence|dwell",
                 MMM_FIRST_NIGHT_DISPLAYED_DOMAINS, ignore.case = TRUE)),
      "G: no HMM-derived row may appear in the displayed first-night set")
check(all(MMM_FIRST_NIGHT_DISPLAYED_DOMAINS %in% names(MMM_FIRST_NIGHT_DOMAIN_CONTRIBUTORS)),
      "G: every displayed domain must declare its contributors")
check(all(unlist(MMM_FIRST_NIGHT_DOMAIN_CONTRIBUTORS) %in%
            paste0(MMM_FIRST_NIGHT_RAW_FEATURES, "_z")),
      "G: every declared contributor must be a standardized raw RFID feature")

cat("First-night domain contract checks: PASS\n")
