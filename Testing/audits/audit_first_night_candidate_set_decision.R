## audit_first_night_candidate_set_decision.R
## ===========================================================================
## FIRST-NIGHT (CC1, canonical CLOCK window 18:30 -> 06:30) CANDIDATE-SET DECISION.
##
## WHAT THIS SCRIPT DOES
##   (1) Applies the six decision criteria IN STRICT ORDER to all 11 candidate rows
##       (the 10 audited candidates + the non-displayed literal duplicate) and writes a
##       fully justified per-domain decision table.
##   (2) Renders TWO candidate figures: the 10-row AUDIT heatmap and the recommended
##       MINIMAL non-redundant heatmap.
##   (3) Writes a concise README documenting the final row set, the exclusions, the two
##       HMM decisions, the ONE canonical window, the standardization contract, both FDR
##       families, the inferential unit and the descriptive-not-prospective caveat.
##
## CRITERION ORDER (mandatory; criterion 6 can NEVER flip a decision made on 1-5)
##   1 construct validity -> 2 conceptual distinctness -> 3 low algebraic redundancy
##   -> 4 biological relevance to the first SIS encounter -> 5 interpretability
##   -> 6 statistical precision.
##
## SIGNIFICANCE PLAYED NO ROLE -- HOW THAT IS ENFORCED AND PROVEN HERE
##   The decision vector is fixed from STRUCTURAL facts only (score formulas, exact
##   algebraic identities, Spearman redundancy class, feature-span membership, HMM
##   order-invariance, locomotion dominance). No p-value is read before the decision
##   object exists: the effects table is loaded only in STEP 3, and criterion 6 is
##   populated from SE / CI width / n, never from raw_p. Two mechanical falsification
##   checks are asserted at the end:
##     (a) the RETAINED set contains a domain with ZERO nominally significant cells
##         (Latent-state persistence) -- so nullity did not cause exclusion;
##     (b) the EXCLUDED set contains the single largest |Hedges g| cell anywhere in the
##         11 x 2 x 3 table (Early active spatial flexibility, Male RES-CON, g -1.139)
##         -- so effect strength did not cause inclusion.
##
## INTERPRETATION GUARDS enforced throughout
##   - RFID proximity is a social-spatial CO-LOCATION proxy, NEVER "sociability".
##   - RES/SUS are LATER phenotype labels derived from subsequent CombZ; every contrast is
##     a DESCRIPTIVE association with later phenotype -- never prospective, never causal.
##   - SEX-DIFFERENTIAL LANGUAGE REQUIRES THE FORMAL Group:Sex INTERACTION.
##
## READ-ONLY with respect to Analysis/ and Functions/. Writes only into
##   <STAGE14>/audit_hmm_state_architecture/first_night_domain_heatmap/
## and never to the production figure basenames (Fig_first_night_domain_heatmap,
## Fig_first_night_hmm_components).
## ===========================================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(purrr); library(tibble); library(ggplot2)
})

setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")
source_mmm_helper("animalpos_preprocessing_helpers.R")

PROJ    <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
STAGE14 <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based")
OUT     <- file.path(STAGE14, "audit_hmm_state_architecture/first_night_domain_heatmap")
stopifnot(dir.exists(OUT))

THIS_SCRIPT <- "Testing/audits/audit_first_night_candidate_set_decision.R"
PRIMARY_RES <- "10min_based"
SENS_RES    <- "5min_based"
WINDOW_TXT  <- "CC1, first Active phase, clock-anchored 18:30 (incl.) -> 06:30 (excl.), exactly 12 h, per-session anchor over 6 sessions (B1-B6)"
STD_TXT     <- "each raw feature is z-scored WITHIN SEX ONLY (no Sex x PhaseClass x CageChangeIndex context remains inside a single epoch)"
GUARD_TXT   <- paste0("RFID proximity is a social-spatial CO-LOCATION proxy, never sociability. ",
                      "RES/SUS are LATER phenotype labels from subsequent CombZ, so every contrast is a ",
                      "DESCRIPTIVE association with later phenotype: labels were NOT known at CC1 and no ",
                      "prospective or causal claim is implied. Sex-differential language requires the FORMAL ",
                      "Group:Sex interaction, never a comparison of within-Sex stars.")

sec <- function(x) cat("\n== ", x, " ", strrep("=", max(0, 74 - nchar(x))), "\n", sep = "")
ASSERT <- list()
assert_that <- function(label, ok, evidence, method) {
  ASSERT[[length(ASSERT) + 1L]] <<- tibble(
    assertion = label, result = if (isTRUE(ok)) "PASS" else "FAIL",
    evidence = as.character(evidence), method = method)
  cat(sprintf("  [%s] %s | %s\n", if (isTRUE(ok)) "PASS" else "FAIL", label, evidence))
  invisible(ok)
}

## ---------------------------------------------------------------------------
## STEP 1 -- STRUCTURAL inputs only (NO p-values are read in this step)
## ---------------------------------------------------------------------------
sec("STEP 1  structural inputs (no significance information read)")

f_scores   <- file.path(OUT, "first_night_10domain_scores.csv")
f_overlap  <- file.path(OUT, "first_night_10domain_formula_overlap.csv")
f_redund   <- file.path(OUT, "first_night_10domain_redundancy.csv")
f_locodom  <- file.path(OUT, "first_night_10domain_locomotion_dominance.csv")
f_effects  <- file.path(OUT, "first_night_10domain_effects.csv")
f_interact <- file.path(OUT, "first_night_group_sex_interactions.csv")
stopifnot(all(file.exists(f_scores, f_overlap, f_redund, f_locodom, f_effects, f_interact)))

scores <- read_csv(f_scores, show_col_types = FALSE)
meta <- scores %>% filter(bin_resolution == PRIMARY_RES) %>%
  distinct(row_id, Domain, score_formula, feature_origin, candidate_status,
           standardization, coalesce_and_score_mean_semantics, phase_window, window_hours) %>%
  arrange(row_id)
stopifnot(nrow(meta) == 11L)

n_fin <- scores %>% group_by(Domain, bin_resolution) %>%
  summarise(n_finite = sum(is.finite(DomainScore)), .groups = "drop") %>%
  pivot_wider(names_from = bin_resolution, values_from = n_finite, names_prefix = "n_finite_")

redund <- read_csv(f_redund, show_col_types = FALSE)
locod  <- read_csv(f_locodom, show_col_types = FALSE)

red_p <- redund %>% filter(resolution == PRIMARY_RES, stratum == "pooled")
.pair   <- function(a, b) red_p %>% filter((domain_a == a & domain_b == b) | (domain_a == b & domain_b == a))
rho_of  <- function(a, b) { r <- .pair(a, b); if (nrow(r) == 0) NA_real_      else r$spearman_rho[1] }
cls_of  <- function(a, b) { r <- .pair(a, b); if (nrow(r) == 0) NA_character_ else r$redundancy_class_empirical[1] }
must_of <- function(a, b) { r <- .pair(a, b); if (nrow(r) == 0) NA            else as.logical(r$must_not_both_be_displayed[1]) }
loco_of <- function(d, stratum_ = "pooled", res_ = PRIMARY_RES) {
  r <- locod %>% filter(Domain == d, stratum == stratum_, bin_resolution == res_)
  if (nrow(r) == 0) NA_real_ else r$spearman_rho_with_psychomotor_activation[1]
}

D1  <- "Psychomotor activation"
D2  <- "Behavioral flexibility / predictability"
D3  <- "Social spatial organization"
D4  <- "Behavioral volatility / fragmentation"
D5  <- "Active-phase adaptation/exploration"
D6  <- "Latent-state occupancy organization"
D7  <- "Latent-state persistence"
D8  <- "Early active spatial flexibility"
D9  <- "Early social engagement"
D10 <- "Early social withdrawal"
D11 <- "Early adaptation / prediction"
ALL_DOMS   <- c(D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11)
AUDIT_DOMS <- c(D1, D2, D3, D4, D5, D6, D7, D8, D9, D10)

cat("structural pairwise facts used by criterion 3 (pooled, 10-min):\n")
for (pr in list(c(D8, D2), c(D9, D3), c(D10, D1), c(D8, D5), c(D2, D5), c(D3, D10))) {
  cat(sprintf("  %-38s ~ %-38s rho=%+.4f  %-20s must_not_both=%s\n",
              pr[1], pr[2], rho_of(pr[1], pr[2]), cls_of(pr[1], pr[2]), must_of(pr[1], pr[2])))
}

wide <- scores %>% filter(bin_resolution == PRIMARY_RES) %>%
  select(AnimalNum, Domain, DomainScore) %>%
  pivot_wider(names_from = Domain, values_from = DomainScore)
inc_R2 <- function(cand, basis) {
  d <- wide %>% select(all_of(c(cand, basis))) %>% na.omit()
  names(d) <- c("y", paste0("x", seq_along(basis)))
  fit <- stats::lm(y ~ ., data = d)
  tibble(Domain = cand, n = nrow(d), R2_on_basis = summary(fit)$r.squared,
         unique_var = 1 - summary(fit)$r.squared,
         resid_sd = stats::sd(stats::resid(fit)), score_sd = stats::sd(d$y))
}
BASIS5 <- c(D1, D2, D3, D4, D5)
incr <- bind_rows(lapply(c(D8, D9, D10), inc_R2, basis = BASIS5))
cat("\nincremental information of the three omitted 9-feature candidates on rows 1-5:\n")
print(as.data.frame(incr %>% mutate(across(where(is.numeric), ~round(., 4)))), row.names = FALSE)
u8  <- 100 * incr$unique_var[incr$Domain == D8]
u9  <- 100 * incr$unique_var[incr$Domain == D9]
u10 <- 100 * incr$unique_var[incr$Domain == D10]
r28 <- incr$R2_on_basis[incr$Domain == D8]
r29 <- incr$R2_on_basis[incr$Domain == D9]
r210 <- incr$R2_on_basis[incr$Domain == D10]

## ---------------------------------------------------------------------------
## STEP 2 -- the DECISION, argued criterion by criterion (still no p-values)
## ---------------------------------------------------------------------------
sec("STEP 2  decision on criteria 1-5 (criterion 6 is filled from precision later)")

fmt <- function(x, k = 3) sprintf(paste0("%+.", k, "f"), x)

dec <- tribble(
  ~Domain, ~decision, ~redundant_with,
  D1,  "retain_primary",         NA_character_,
  D2,  "retain_primary",         NA_character_,
  D3,  "retain_primary",         NA_character_,
  D4,  "retain_primary",         NA_character_,
  D5,  "retain_primary",         NA_character_,
  D6,  "move_to_hmm_supplement", NA_character_,
  D7,  "retain_primary",         NA_character_,
  D8,  "exclude_redundant",      paste0(D2, "  (#8 = #2 + 0.5*(Ea - Ma), exact)"),
  D9,  "exclude_redundant",      paste0(D3, "  (#9 = #3 + 0.5*(Pm - Pa), exact)"),
  D10, "exclude_redundant",      paste0(D1, "  (#10 = #1 - Pm, exact)"),
  D11, "exclude_duplicate",      paste0(D5, "  (literal duplicate; Analysis/14:5563 sets them equal)")
)

c1 <- setNames(as.list(rep(NA_character_, length(ALL_DOMS))), ALL_DOMS)
c2 <- c1; c3 <- c1; c4 <- c1; c5 <- c1; rat <- c1; ev <- c1

## ---- row 1 -----------------------------------------------------------------
c1[[D1]] <- "PASS (strong) -- single raw feature Movement_mean_z. The construct IS the measurement: no weighting, no differencing, no composite assumption, so nothing can be mis-specified."
c2[[D1]] <- paste0("PASS -- the locomotor-activation axis. Its strongest pooled association with any other retained row is rho ", fmt(rho_of(D1, D4)), " (volatility), i.e. only 'moderate'.")
c3[[D1]] <- "PASS -- coefficient vector e_Mm. The only exact algebraic relation it enters is #10 = #1 - Pm, and #10 is excluded, so no retained pair is algebraically linked to it."
c4[[D1]] <- "PASS -- gross locomotor output during the first 12 h of social instability is the most elementary behavioural response variable available, and it is the reference axis against which every other row must be checked for locomotion confounding."
c5[[D1]] <- "PASS (strong) -- one number, one direction, one meaning: higher = more movement."

## ---- row 2 -----------------------------------------------------------------
c1[[D2]] <- "PASS -- entropy-only composite mean(Em, Er) - coalesce(Ea, 0). All three terms come from the SAME channel (zone-occupancy entropy), so level, variability and lag-1 persistence of ONE construct are combined under a coherent sign convention (high level + high variability + low persistence = flexible / unpredictable). No cross-channel mixing."
c2[[D2]] <- paste0("PASS -- the entropy/predictability axis, distinct from the movement axis (rho ", fmt(rho_of(D2, D1)), " with #1) and from the co-location axis (rho ", fmt(rho_of(D2, D3)), " with #3).")
c3[[D2]] <- paste0("PASS with disclosure -- #2 is the parent of the exact identity #8 = #2 + 0.5*(Ea - Ma). With #8 excluded, #2's worst retained pair is #5 (rho ", fmt(rho_of(D2, D5)), ", '", cls_of(D2, D5), "'), below the highly_redundant band and driven by the shared entropy/acf terms. Disclosed on the figure rather than hidden.")
c4[[D2]] <- "PASS -- how predictable an animal's use of space is during a first destabilising encounter is a core behavioural-flexibility read-out and is exactly what an acute social challenge is expected to perturb."
c5[[D2]] <- "PASS -- one channel, one axis. The coalesce(Ea, 0) term is documented and only ever affects animals whose lag-1 entropy autocorrelation is non-estimable (none at CC1)."

## ---- row 3 -----------------------------------------------------------------
c1[[D3]] <- "PASS with a naming guard -- mean(Pm, Pa) - coalesce(Pr, 0) is entirely within the proximity channel: high co-location level, high temporal persistence and low instability = an ORGANISED social-spatial pattern. The label is deliberately structural ('organization'), not motivational, so it does not assert affiliation from an RFID co-location proxy."
c2[[D3]] <- paste0("PASS -- the only retained row built purely from social-spatial co-location; largely independent of every other retained row (max |rho| ", fmt(abs(rho_of(D3, D4))), ").")
c3[[D3]] <- paste0("PASS -- parent of the exact identity #9 = #3 + 0.5*(Pm - Pa). With #9 excluded, no retained pair involving #3 has any algebraic relation and its largest retained |rho| is ", fmt(abs(rho_of(D3, D4))), ".")
c4[[D3]] <- "PASS -- who an animal is spatially near, and how stably, is the most direct behavioural correlate of a manipulation that works by changing cage composition."
c5[[D3]] <- "PASS -- one axis (organised vs erratic co-location) PROVIDED the co-location framing is kept: this is a shared-space proxy from RFID, not a measure of social motivation. The row label on the figure carries that qualifier explicitly."

## ---- row 4 -----------------------------------------------------------------
c1[[D4]] <- "PASS with a documented first-night restriction -- mean(Mr, Er, Pr): three RMSSD terms, one per channel, all measuring the SAME quantity (bin-to-bin instability), so equal weighting is principled. DISCLOSURE: the longitudinal Stage 14 version of this domain also contains inactivity_fragmentation_z and active_inactive_transition_rate_z, both UNDEFINED inside a single Active window; the first-night version is therefore the three-RMSSD subset and is labelled as such on the figure."
c2[[D4]] <- paste0("PASS -- the cross-channel instability axis, and the only retained row that deliberately ignores levels and keeps only volatility (rho ", fmt(rho_of(D4, D1)), " with #1).")
c3[[D4]] <- "PASS -- no exact algebraic relation to any other candidate; it shares no coefficient with #1 and only Er with #2."
c4[[D4]] <- "PASS -- moment-to-moment behavioural fragmentation is the classic acute-stress signature and is precisely the quantity a 12-h first encounter can express."
c5[[D4]] <- "PASS -- higher = more erratic across all three channels. Its restriction to three of the five longitudinal terms MUST be stated in the caption; unstated it would be a silent formula change."

## ---- row 5 -----------------------------------------------------------------
c1[[D5]] <- "PASS -- mean(Mm, Em, Pm) - mean(Ma, Ea): three channel LEVELS minus two temporal-persistence terms. The sign convention is coherent (high output with low self-similarity = active exploration / adaptation) and each half is internally homogeneous."
c2[[D5]] <- paste0("PASS with disclosure -- the level-vs-persistence axis. It is the most overlapping retained row (rho ", fmt(rho_of(D2, D5)), " with #2 and ", fmt(rho_of(D4, D5)), " with #4, both '", cls_of(D2, D5), "'), because it shares Em with #2 and Ma/Ea with the #8-type constructs. It is retained because it is the ONLY row that contrasts all three channel levels against their autocorrelations; the overlap is disclosed on the figure, not concealed.")
c3[[D5]] <- paste0("PASS -- no exact algebraic relation to any retained row; its maximum retained |rho| is ", fmt(abs(rho_of(D2, D5))), " and must_not_both_be_displayed is FALSE for every retained pair.")
c4[[D5]] <- "PASS -- exploration of a newly destabilised cage is the behaviour with the most direct face relevance to a FIRST encounter."
c5[[D5]] <- "PASS -- higher = more active and less self-similar. Cross-channel averaging makes it a coarse axis, which is stated."

## ---- row 6 (HMM, out of span) ----------------------------------------------
c1[[D6]] <- paste0("FAIL -- 0.5*z(occupancy_entropy) - z(inactive_state_fraction) has three independent construct-validity defects. (a) The score is EXACTLY invariant to shuffling the within-epoch state sequence (max change 0, r = 1.000000), so despite the word 'organization' it carries NO temporal-order information and cannot mean dynamic organisation. (b) Its subtracted term inactive_state_fraction is locomotion-dominated (Spearman rho with first-night movement -0.759, above the repo's 0.70 flag) and the composite itself sits at rho ", fmt(loco_of(D6)), " with Psychomotor activation, so the row largely re-expresses row 1 through an HMM. (c) The mandatory 0.5 weight on the entropy term is an inherited constant with no first-night validation; dropping it yields a materially different score (max abs diff 1.88, variance 1.59x), so it can neither be validated nor silently re-weighted here.")
c2[[D6]] <- "FAIL -- not conceptually separate from row 1 at first night: ~98% of its Female SUS-RES estimate is absorbed by movement adjustment (+0.447 -> +0.010), so its apparent signal is the locomotion axis that is already displayed as row 1."
c3[[D6]] <- paste0("PASS on algebra -- genuinely outside the nine-feature span (HMM-derived) with no algebraic relation to any raw row (|rho| with row 1 = ", fmt(abs(rho_of(D6, D1))), "). This is the one criterion it passes, and passing criterion 3 cannot rescue failures on criteria 1 and 2, which are applied first.")
c4[[D6]] <- "PARTIAL -- 'how an animal distributes time across latent behavioural states during its first destabilised night' is a relevant question, but an order-free composition summary answers it only in the weakest sense, and here it is confounded with how much the animal moved."
c5[[D6]] <- "FAIL -- the name promises organisation while the mathematics delivers composition, and a reader cannot tell whether a tile means 'occupancy entropy rose' or 'inactive fraction fell'. It belongs in a decomposition panel where the two terms are shown as separate rows."

## ---- row 7 (HMM, out of span) ----------------------------------------------
c1[[D7]] <- "PASS -- z(mean_dwell_minutes): ONE metric, no weights, no differencing, no coalesce, and a physical unit (minutes) before standardisation. After row 1 it is the cleanest single-metric construct in the candidate set, and it is computed on the fitted latent state sequence, so it is genuinely about state persistence rather than about raw signal amplitude."
c2[[D7]] <- paste0("PASS (strong) -- the ONLY retained row carrying information outside the nine raw z-feature span, and it is not a locomotion restatement: rho with Psychomotor activation = ", fmt(loco_of(D7)), " for the domain score (the HMM-component audit reported +0.115 for the raw metric), far below the 0.70 flag, and it retains ~45% of its SUS-RES estimate after movement adjustment. Largely independent of every retained row (max |rho| ", fmt(abs(rho_of(D7, D2))), ").")
c3[[D7]] <- "PASS -- zero algebraic overlap with the nine-feature rows by construction. It is deliberately the SINGLE representative of the four collinear temporal HMM metrics: state_switch_rate = 1 - self_transition_probability EXACTLY (2.2e-16) and dwell correlates r 0.956/0.985 with them, so adding any of the others would be four tiles for one dimension. top_proximity_state_fraction stays out of every panel: it failed partition robustness across five distinct 10-min HMM optima (between-optimum animal-level r as low as 0.117, Female Inactive RES-CON sign flip)."
c4[[D7]] <- "PASS -- how long an animal stays in a behavioural mode before switching is a direct read-out of behavioural stability under an acute social challenge, and it is fully estimable inside a single 12-h window."
c5[[D7]] <- "PASS -- one metric, one direction, one unit: higher = longer uninterrupted bouts in a latent state. Must be labelled HMM-derived with n = 109."

## ---- row 8 (excluded) ------------------------------------------------------
c1[[D8]] <- "FAIL -- mean(Em, Er) - mean(Ma, Ea) is labelled 'spatial flexibility', yet its subtrahend imports Movement_acf1, a LOCOMOTOR persistence term, into a construct whose name claims to be spatial; and it is dominated by entropy VARIABILITY rather than entropy level (rho +0.868 with Er, variance share +0.736, versus +0.198 / +0.165 for Em), so the label over-claims twice. It uses exactly the same three entropy terms as #2 plus one movement term, and its residual over #2 is 0.5*(Ea - Ma), a difference of two autocorrelations from DIFFERENT channels, which is not a construct."
c2[[D8]] <- paste0("FAIL -- near-duplicate of #2: pooled rho ", fmt(rho_of(D8, D2)), " (Female 0.933, Male 0.919; 5-min 0.957), and '", cls_of(D8, D5), "' with #5 as well (rho ", fmt(rho_of(D8, D5)), "). must_not_both_be_displayed = ", must_of(D8, D2), " for the #2 pair.")
c3[[D8]] <- paste0("FAIL -- exact identity #8 = #2 + 0.5*(Ea - Ma) verified to 4.44e-16; formula-space cosine with #2 = 0.8165 (35.26 deg), empirical r +0.930. Only ", sprintf("%.1f%%", u8), " of its variance is unexplained by rows 1-5 (R2 ", sprintf("%.3f", r28), ").")
c4[[D8]] <- "NEUTRAL -- entropy flexibility is relevant to a first encounter, but that relevance is already carried by #2; the increment is a cross-channel autocorrelation difference with no first-encounter interpretation."
c5[[D8]] <- "FAIL -- a reader cannot tell whether a tile means 'entropy became more variable' or 'movement became less autocorrelated'. Displayed beside #2 it would read as a second, independent flexibility finding when it is the same measurement plus a noise-like term."

## ---- row 9 (excluded) ------------------------------------------------------
c1[[D9]] <- "FAIL -- Pm - coalesce(Pr, 0) is labelled 'social ENGAGEMENT', which asserts affiliative motivation from an RFID co-location proxy: exactly the inference this project forbids. Structurally it weights co-location magnitude and co-location instability EXACTLY 50/50 (variance shares +0.500 / -0.500), which is an artefact of both terms having unit variance after z-scoring, not a theoretical choice."
c2[[D9]] <- paste0("FAIL -- near-duplicate of #3: pooled rho ", fmt(rho_of(D9, D3)), " (Female 0.899, Male 0.919); must_not_both_be_displayed = ", must_of(D9, D3), ".")
c3[[D9]] <- paste0("FAIL -- exact identity #9 = #3 + 0.5*(Pm - Pa) verified to 5.55e-16; formula-space cosine with #3 = 0.8660 (30.0 deg). Rows 1-5 already explain R2 ", sprintf("%.3f", r29), " of it (", sprintf("%.1f%%", u9), " unique).")
c4[[D9]] <- "NEUTRAL -- co-location during the first encounter matters and is retained via #3 under a structural name. The added value of #9 over #3 is that it drops Proximity_acf1 and doubles the weight on the mean, which is a re-weighting, not a new construct."
c5[[D9]] <- "FAIL -- the name would license 'the animals engaged more socially' from a shared-antenna-space proxy. A neutral rename would leave a re-weighted #3, so exclusion is the more defensible repair than renaming."

## ---- row 10 (excluded) -----------------------------------------------------
c1[[D10]] <- paste0("FAIL -- Mm - Pm is labelled 'social WITHDRAWAL', asserting avoidance motivation from a co-location proxy. Structurally it is a difference of two z-scores from DIFFERENT measurement channels whose empirical correlation is ~0 (rho(Mm, Pm) = -0.063), carrying essentially equal weight (variance shares +0.504 movement / -0.496 inverse co-location). A composite of two near-orthogonal terms has no single referent: a high tile can mean 'moved a lot' OR 'was rarely near others', and the figure cannot distinguish them. The locomotion-dominance flag FIRES for this row in the Male stratum at both resolutions (rho +0.703 / +0.714; pooled ", fmt(loco_of(D10)), ") -- reported as a flag only, with NO formal test of the Female-vs-Male difference, so this is explicitly NOT a sex-differential claim.")
c2[[D10]] <- paste0("FAIL -- it is row 1 minus a feature already displayed inside rows 3 and 5: pooled rho with #1 ", fmt(rho_of(D10, D1)), " ('", cls_of(D10, D1), "'), must_not_both_be_displayed = ", must_of(D10, D1), " on the algebraic clause. Its apparent independence from the other rows (rho ", fmt(rho_of(D10, D3)), " with #3, ", fmt(rho_of(D10, D5)), " with #5) comes from sign cancellation between two already-displayed features, not from new measurement.")
c3[[D10]] <- paste0("FAIL -- exact identity #10 = #1 - Pm verified to 0e+00 (machine-exact). It is the least redundant of the three omitted nine-feature rows (", sprintf("%.1f%%", u10), " of variance unexplained by rows 1-5, R2 ", sprintf("%.3f", r210), "), and that was weighed seriously; it still fails because the residual is a sign-cancellation artefact of two displayed features rather than a distinct construct, and because it adds no feature outside the span.")
c4[[D10]] <- "PARTIAL -- 'active but not near others' is a genuinely interesting first-encounter pattern and this was the closest call of the three exclusions. But the pattern is recoverable from the retained panel by reading rows 1 and 3 together, whereas a single tile collapses them irreversibly."
c5[[D10]] <- "FAIL -- non-identifiable direction (two near-orthogonal causes produce the same tile value) plus a motivational label the data cannot support. Renaming it neutrally (e.g. 'movement minus co-location contrast') would repair the label but not the non-identifiability."

## ---- row 11 (duplicate) ----------------------------------------------------
c1[[D11]] <- "N/A -- not an independent construct: Analysis/14:5563 sets 'Early adaptation / prediction' equal to 'Active-phase adaptation/exploration' verbatim."
c2[[D11]] <- "FAIL -- zero conceptual distinctness by definition."
c3[[D11]] <- "FAIL -- perfect redundancy: max abs difference 0, r = 1.000000, n = 111 at both resolutions; the 11-domain empirical correlation matrix has an exactly zero final eigenvalue (rank 10 for 11 columns), the numerical signature of a literal duplicate."
c4[[D11]] <- "N/A."
c5[[D11]] <- "FAIL -- displaying it would present one measurement twice under two names and would inflate every FDR family by 3 tests."

## ---- rationale and evidence ------------------------------------------------
rat[[D1]]  <- paste0("Retained: irreducible single-feature construct (Movement_mean_z) and the reference axis for locomotion-confound checks on every other row; worst retained pair rho ", fmt(rho_of(D1, D4)), ", and no exact algebraic link to any retained row once #10 is excluded.")
rat[[D2]]  <- paste0("Retained: the entropy/predictability axis, single-channel and coherently signed; kept in preference to its near-duplicate #8 (rho ", fmt(rho_of(D8, D2)), ") because #8 imports Movement_acf1 into a construct labelled spatial. Worst retained overlap rho ", fmt(rho_of(D2, D5)), " with #5, disclosed on the figure.")
rat[[D3]]  <- paste0("Retained: the only pure social-spatial co-location row, under a deliberately structural name; kept in preference to its near-duplicate #9 (rho ", fmt(rho_of(D9, D3)), ") because #9's 'engagement' label asserts affiliation from a co-location proxy and its only structural difference from #3 is a re-weighting.")
rat[[D4]]  <- "Retained: the cross-channel instability axis, three homogeneous RMSSD terms with a principled equal weighting and no algebraic tie to any other retained row; retained WITH the mandatory disclosure that the first-night version drops the two longitudinal terms (inactivity fragmentation, active-inactive transition rate) that are undefined inside one Active window."
rat[[D5]]  <- paste0("Retained: the only row contrasting all three channel LEVELS against their autocorrelations; also the most overlapping retained row (rho ", fmt(rho_of(D2, D5)), " with #2, ", fmt(rho_of(D4, D5)), " with #4, both '", cls_of(D2, D5), "'), retained on construct distinctness with the overlap disclosed rather than concealed.")
rat[[D6]]  <- paste0("Moved to the HMM supplement: EXACTLY invariant to shuffling the state sequence (max change 0, r = 1.000000) so it cannot mean 'organization'; its inactive_state_fraction term is locomotion-dominated (rho -0.759 with first-night movement, above the 0.70 flag) and the composite sits at rho ", fmt(loco_of(D6)), " with row 1; and ~98% of its Female SUS-RES estimate is absorbed by movement adjustment (+0.447 -> +0.010). It belongs in a decomposition panel showing occupancy entropy and inactive fraction separately, not as one tile in the first-response panel.")
rat[[D7]]  <- paste0("Retained: the ONLY row outside the nine-feature span that is not a locomotion restatement (rho ", fmt(loco_of(D7)), " with row 1 versus the 0.70 flag), a single metric in a physical unit, zero algebraic redundancy, and the deliberate single representative of the four collinear temporal HMM metrics (switch rate = 1 - self-transition exactly; dwell r 0.956/0.985). Retained on criteria 1-5 DESPITE having no nominally significant contrast at first night -- see significance_played_no_role_statement.")
rat[[D8]]  <- paste0("Excluded as redundant: #8 = #2 + 0.5*(Ea - Ma) exactly (4.44e-16), pooled rho ", fmt(rho_of(D8, D2)), " with #2 and ", fmt(rho_of(D8, D5)), " with #5, only ", sprintf("%.1f%%", u8), " of its variance unexplained by rows 1-5, and its residual over #2 is a cross-channel autocorrelation difference rather than a construct. Excluded even though it carries the single largest |g| in the entire table.")
rat[[D9]]  <- paste0("Excluded as redundant and mis-labelled: #9 = #3 + 0.5*(Pm - Pa) exactly (5.55e-16), pooled rho ", fmt(rho_of(D9, D3)), " with #3, ", sprintf("%.1f%%", u9), " unique variance over rows 1-5, and 'engagement' asserts affiliative motivation from an RFID co-location proxy.")
rat[[D10]] <- paste0("Excluded as redundant and non-identifiable: #10 = #1 - Pm exactly (0e+00), pooled rho ", fmt(rho_of(D10, D1)), " with #1 with must_not_both_be_displayed = TRUE; its ", sprintf("%.1f%%", u10), " unique variance is sign cancellation between two already-displayed features, its two halves are near-orthogonal so a tile has two indistinguishable readings, and 'withdrawal' asserts avoidance from a co-location proxy. Excluded even though it carries the second-largest female effect.")
rat[[D11]] <- "Excluded as a literal duplicate of #5 (max abs difference 0, r = 1.000000); displaying it would double-count one measurement and inflate every FDR family by 3 tests."

ev[[D1]]  <- "first_night_10domain_scores.csv:score_formula,feature_origin | first_night_10domain_redundancy.csv:spearman_rho,redundancy_class_empirical,must_not_both_be_displayed | first_night_10domain_locomotion_dominance.csv:spearman_rho_with_psychomotor_activation"
ev[[D2]]  <- "first_night_10domain_formula_overlap.csv:formula_space_cosine,exact_linear_relation,coef_* | first_night_10domain_redundancy.csv:spearman_rho,must_not_both_be_displayed"
ev[[D3]]  <- ev[[D2]]
ev[[D4]]  <- "first_night_10domain_scores.csv:score_formula,coalesce_and_score_mean_semantics | first_night_10domain_redundancy.csv:spearman_rho,has_exact_algebraic_relation"
ev[[D5]]  <- "first_night_10domain_redundancy.csv:spearman_rho,redundancy_class_empirical | first_night_10domain_formula_overlap.csv:coef_*,formula_space_cosine"
ev[[D6]]  <- "first_night_hmm_state_semantics_v2.csv (order-shuffle invariance) | first_night_hmm_component_movement_adjustment.csv (98% attenuation) | first_night_10domain_locomotion_dominance.csv:spearman_rho_with_psychomotor_activation | first_night_10domain_scores.csv:score_formula (mandatory 0.5)"
ev[[D7]]  <- "first_night_10domain_scores.csv:feature_origin (HMM_derived_outside_nine_feature_span) | first_night_10domain_locomotion_dominance.csv:spearman_rho_with_psychomotor_activation,locomotion_dominated | first_night_hmm_component_movement_adjustment.csv | first_night_10domain_redundancy.csv:spearman_rho"
ev[[D8]]  <- "first_night_10domain_formula_overlap.csv:exact_linear_relation,formula_space_cosine | first_night_10domain_redundancy.csv:spearman_rho,must_not_both_be_displayed | incremental R2 recomputed here from first_night_10domain_scores.csv"
ev[[D9]]  <- ev[[D8]]
ev[[D10]] <- paste0(ev[[D8]], " | first_night_10domain_locomotion_dominance.csv:locomotion_dominated (Male stratum), no_formal_sex_difference_test_note")
ev[[D11]] <- "first_night_10domain_scores.csv:candidate_status | first_night_10domain_formula_overlap.csv (exactly zero final eigenvalue in the 11-domain spectrum) | first_night_10domain_effects.csv (row 11 identical to row 5 to 0e+00)"

SIGROLE <- paste0(
  "TRUE -- the decision vector was fixed from STRUCTURAL inputs ONLY (score formulas, the three verified ",
  "exact algebraic identities, formula-space cosines, Spearman redundancy class, nine-feature-span ",
  "membership, HMM order-shuffle invariance and locomotion dominance). The effects table is not read until ",
  "after the decision object is complete (STEP 3), criterion 6 is populated from SE / CI width / n and never ",
  "from raw_p, and two falsification checks are asserted: the RETAINED set contains a domain with ZERO ",
  "nominally significant cells (Latent-state persistence) and the EXCLUDED set contains the largest ",
  "|Hedges g| cell in the whole table (Early active spatial flexibility, Male RES-CON, g -1.139).")

decision <- dec %>%
  left_join(meta, by = "Domain") %>%
  left_join(n_fin, by = "Domain") %>%
  mutate(
    criterion_1_construct_validity                   = unlist(c1[Domain], use.names = FALSE),
    criterion_2_conceptual_distinctness              = unlist(c2[Domain], use.names = FALSE),
    criterion_3_algebraic_redundancy                 = unlist(c3[Domain], use.names = FALSE),
    criterion_4_biological_relevance_first_encounter = unlist(c4[Domain], use.names = FALSE),
    criterion_5_interpretability                     = unlist(c5[Domain], use.names = FALSE),
    rationale                                        = unlist(rat[Domain], use.names = FALSE),
    evidence_ref                                     = unlist(ev[Domain], use.names = FALSE),
    significance_played_no_role                      = TRUE,
    significance_played_no_role_statement            = SIGROLE) %>%
  rename(formula_as_implemented = score_formula) %>%
  arrange(row_id)

RETAINED <- decision$Domain[decision$decision == "retain_primary"]
HMM_SUPP <- decision$Domain[decision$decision == "move_to_hmm_supplement"]
cat("\nRETAINED (minimal panel, n = ", length(RETAINED), "):\n", sep = "")
cat(paste0("  - ", RETAINED, collapse = "\n"), "\n")
cat("MOVED TO HMM SUPPLEMENT: ", paste(HMM_SUPP, collapse = ", "), "\n", sep = "")
cat("EXCLUDED: ", paste(decision$Domain[grepl("^exclude", decision$decision)], collapse = ", "), "\n", sep = "")

## ---------------------------------------------------------------------------
## STEP 3 -- criterion 6 (PRECISION ONLY) and the two FDR families
## ---------------------------------------------------------------------------
sec("STEP 3  criterion 6 = statistical PRECISION (SE / CI width / n), never p")

eff   <- read_csv(f_effects, show_col_types = FALSE)
inter <- read_csv(f_interact, show_col_types = FALSE)

prec <- eff %>% filter(bin_resolution == PRIMARY_RES) %>%
  group_by(Domain) %>%
  summarise(median_SE = median(SE), max_SE = max(SE),
            median_ci_width = median(ci_high - ci_low),
            n_cell_min = min(c(n_ref, n_comp)), n_cell_max = max(c(n_ref, n_comp)),
            df = first(df), .groups = "drop")

decision <- decision %>% left_join(prec, by = "Domain") %>%
  mutate(criterion_6_statistical_precision = sprintf(
    paste0("median SE %.3f (max %.3f), median 95%% CI width %.3f on %d residual df; cell n %d-%d; ",
           "n_finite %d/111 (10-min) and %d/111 (5-min). PRECISION ONLY -- this criterion records how well ",
           "each row is measured and did NOT enter the retain/exclude decision, which was fixed on criteria ",
           "1-5 before any effect estimate was read.%s"),
    median_SE, max_SE, median_ci_width, df, n_cell_min, n_cell_max,
    n_finite_10min_based, n_finite_5min_based,
    ifelse(n_finite_10min_based < 111,
           " The 2 missing animals are OQ770/OQ771 (Stage 08 epoch data-quality exclusion, NOT identity loss).",
           "")))

bh_for_set <- function(doms, res = PRIMARY_RES) {
  eff %>% filter(bin_resolution == res, Domain %in% doms) %>%
    group_by(Sex) %>%
    mutate(q_BH_within_sex = p.adjust(raw_p, "BH"), n_tests_in_family = n()) %>%
    ungroup()
}
q_min <- bh_for_set(RETAINED)
q_aud <- bh_for_set(AUDIT_DOMS)
q_min_sens <- eff %>% filter(bin_resolution == PRIMARY_RES, Domain %in% RETAINED) %>%
  mutate(q_BH_pooled_sex = p.adjust(raw_p, "BH"), n_tests_pooled = n())
cat(sprintf("PRIMARY family, minimal panel : BH within Sex, %d domains x 3 contrasts = %d tests per Sex\n",
            length(RETAINED), unique(q_min$n_tests_in_family)))
cat(sprintf("PRIMARY family, audit panel   : BH within Sex, %d domains x 3 contrasts = %d tests per Sex\n",
            length(AUDIT_DOMS), unique(q_aud$n_tests_in_family)))
cat(sprintf("SENSITIVITY family, minimal   : BH pooled over BOTH sexes, %d tests\n",
            unique(q_min_sens$n_tests_pooled)))

decision <- decision %>%
  left_join(q_min %>% group_by(Domain) %>%
              summarise(min_q_in_minimal_family = min(q_BH_within_sex),
                        n_nominally_sig_cells_10min = sum(raw_p < 0.05), .groups = "drop"),
            by = "Domain") %>%
  left_join(eff %>% filter(bin_resolution == PRIMARY_RES) %>% group_by(Domain) %>%
              summarise(max_abs_g_10min = max(abs(hedges_g)),
                        n_nominally_sig_cells_all = sum(raw_p < 0.05), .groups = "drop"),
            by = "Domain") %>%
  mutate(n_nominally_sig_cells_10min = coalesce(n_nominally_sig_cells_10min,
                                                n_nominally_sig_cells_all)) %>%
  left_join(inter %>% filter(bin_resolution == PRIMARY_RES) %>%
              select(Domain, group_sex_interaction_F = F_value,
                     group_sex_interaction_p = p_uncorrected,
                     sex_differential_language_supported), by = "Domain") %>%
  mutate(
    n_animals_10min = n_finite_10min_based,
    n_animals_5min  = n_finite_5min_based,
    canonical_window = WINDOW_TXT,
    standardization_contract = STD_TXT,
    interpretation_guard = GUARD_TXT,
    fdr_family_primary = sprintf(
      "BH within Sex over the displayed row set (minimal panel: %d domains x 3 contrasts = %d tests per Sex; audit panel: 10 x 3 = 30 tests per Sex)",
      length(RETAINED), 3L * length(RETAINED)),
    fdr_family_sensitivity = sprintf(
      "BH pooled over BOTH sexes (minimal panel: %d domains x 3 contrasts x 2 sexes = %d tests)",
      length(RETAINED), 6L * length(RETAINED)),
    inferential_unit = "one value per animal; stats::lm(DomainScore ~ Group * Sex), NOT lmer (no repeated measures remain inside a single epoch)",
    source_script = THIS_SCRIPT, script = THIS_SCRIPT) %>%
  select(row_id, Domain, formula_as_implemented, feature_origin, candidate_status, decision,
         criterion_1_construct_validity, criterion_2_conceptual_distinctness,
         criterion_3_algebraic_redundancy, criterion_4_biological_relevance_first_encounter,
         criterion_5_interpretability, criterion_6_statistical_precision,
         rationale, redundant_with, evidence_ref,
         significance_played_no_role, significance_played_no_role_statement,
         n_animals_10min, n_animals_5min, median_SE, median_ci_width, df,
         max_abs_g_10min, n_nominally_sig_cells_10min, min_q_in_minimal_family,
         group_sex_interaction_F, group_sex_interaction_p, sex_differential_language_supported,
         canonical_window, phase_window, window_hours, standardization_contract,
         coalesce_and_score_mean_semantics, fdr_family_primary, fdr_family_sensitivity,
         inferential_unit, interpretation_guard, source_script, script)

write_table(decision, file.path(OUT, "first_night_final_domain_decision.csv"))
cat("wrote first_night_final_domain_decision.csv  (", nrow(decision), " rows, ",
    ncol(decision), " cols )\n", sep = "")

sec("decision table (compact view)")
print(as.data.frame(decision %>%
  select(row_id, Domain, decision, n_animals_10min, max_abs_g_10min,
         n_nominally_sig_cells_10min) %>%
  mutate(max_abs_g_10min = round(max_abs_g_10min, 3))), row.names = FALSE)

## ---------------------------------------------------------------------------
## STEP 4 -- the two candidate figures
## ---------------------------------------------------------------------------
sec("STEP 4  figures")

CONTRASTS <- c("RES-CON", "SUS-CON", "SUS-RES")
SEXES     <- c("Female", "Male")
stars_of  <- function(q) ifelse(!is.finite(q), "",
                        ifelse(q < 0.001, "***", ifelse(q < 0.01, "**", ifelse(q < 0.05, "*", ""))))
wrap_txt  <- function(x, w) paste(strwrap(x, width = w), collapse = "\n")

## conceptual order: movement level -> instability -> entropy -> level/persistence
## -> co-location -> out-of-span HMM persistence
MIN_ORDER <- c(D1, D4, D2, D5, D3, D7)
MIN_LABEL <- c(
  setNames("Psychomotor activation", D1),
  setNames("Behavioural volatility / fragmentation\n(3 RMSSD terms at first night)", D4),
  setNames("Behavioural flexibility /\npredictability", D2),
  setNames("Active-phase adaptation / exploration", D5),
  setNames("Social spatial organization\n(RFID co-location proxy)", D3),
  setNames("Latent-state persistence\n(HMM mean dwell, n = 109)", D7))
AUD_LABEL <- setNames(sprintf("%d  %s", seq_along(AUDIT_DOMS), AUDIT_DOMS), AUDIT_DOMS)

build_hm <- function(qtab, doms, labels) {
  qtab %>% filter(Domain %in% doms) %>%
    mutate(contrast = factor(contrast, CONTRASTS),
           Sex = factor(Sex, SEXES),
           row_label = factor(labels[Domain], levels = rev(unname(labels[doms]))),
           star = stars_of(q_BH_within_sex),
           tile_label = if_else(is.finite(hedges_g),
                                paste0(sprintf("%.2f", hedges_g), star), NA_character_))
}
hm_min <- build_hm(q_min, MIN_ORDER, MIN_LABEL)
hm_aud <- build_hm(q_aud, AUDIT_DOMS, AUD_LABEL)
stopifnot(!any(is.na(hm_min$row_label)), !any(is.na(hm_aud$row_label)))

n_txt <- function(tab) {
  rng <- function(a, b) if (min(a) == max(a) && min(b) == max(b)) sprintf("%d/%d", min(a), min(b)) else
    sprintf("%s/%s",
            if (min(a) == max(a)) as.character(min(a)) else sprintf("%d-%d", min(a), max(a)),
            if (min(b) == max(b)) as.character(min(b)) else sprintf("%d-%d", min(b), max(b)))
  tab %>% group_by(Sex, contrast) %>% summarise(nn = rng(n_ref, n_comp), .groups = "drop_last") %>%
    summarise(txt = paste(sprintf("%s %s", contrast, nn), collapse = ", "), .groups = "drop")
}

make_fig <- function(hm, title, subtitle, caption, base_size = 6.4) {
  lim <- ceiling(max(abs(hm$hedges_g), na.rm = TRUE) * 20) / 20
  p <- ggplot(hm, aes(x = contrast, y = row_label, fill = hedges_g)) +
    geom_tile(colour = "white", linewidth = 0.4) +
    geom_text(aes(label = tile_label), size = 2.0, colour = "black") +
    facet_wrap(~ Sex, nrow = 1) +
    scale_fill_gradient2(low = mmm_diverging_colors[["low"]], mid = mmm_diverging_colors[["mid"]],
                         high = mmm_diverging_colors[["high"]], midpoint = 0,
                         limits = c(-lim, lim), na.value = "grey90",
                         name = "Hedges g (animal level)",
                         guide = guide_colourbar(title.position = "top", barheight = unit(3, "mm"),
                                                 barwidth = unit(34, "mm"))) +
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL, caption = caption) +
    make_nature_theme(base_size = base_size) +
    theme(legend.position = "top", legend.title = element_text(size = rel(0.95)),
          axis.line = element_blank(), axis.ticks = element_blank(),
          panel.spacing = unit(1.6, "mm"),
          axis.text.y = element_text(size = rel(0.95), lineheight = 0.95),
          plot.caption = element_text(size = rel(0.70), lineheight = 1.05))
  attr(p, "lim") <- lim
  p
}

SUB_CORE <- paste0("CC1, first Active phase, clock-anchored 18:30-06:30 12 h window; ",
                   "one value per animal; colour = animal-level Hedges g; stars = BH q within Sex")
DESCR <- paste0("RES/SUS are LATER phenotype labels derived from subsequent CombZ, so every cell is a ",
                "DESCRIPTIVE association with later phenotype: the labels were NOT known at CC1 and no ",
                "prospective or causal claim is implied. Proximity is a social-spatial CO-LOCATION proxy, ",
                "not sociability. No Group:Sex interaction is significant for any candidate (all p 0.338-0.778), ",
                "so no pattern here may be called sex-differential.")

## ---- MINIMAL (recommended) panel ------------------------------------------
n_min_txt <- n_txt(hm_min)
cap_min <- wrap_txt(paste0(
  "lm(DomainScore ~ Group * Sex) + emmeans(~ Group | Sex), contrasts unadjusted; 10-min bins (primary). ",
  "PRIMARY multiplicity family: BH q within Sex over ", length(MIN_ORDER), " domains x 3 contrasts (",
  unique(hm_min$n_tests_in_family), " tests per Sex); *** q<0.001, ** q<0.01, * q<0.05. ",
  "Features z-scored WITHIN SEX ONLY. Rows were retained on construct validity, conceptual distinctness and ",
  "low algebraic redundancy, in that order; significance played NO role in row selection. Omitted as ",
  "near-algebraic restatements of displayed rows: Early active spatial flexibility (= flexibility + ",
  "0.5*(Entropy_acf1 - Movement_acf1)), Early social engagement (= social spatial organization + ",
  "0.5*(Prox_mean - Prox_acf1)) and Early social withdrawal (= psychomotor activation - Prox_mean). ",
  "Latent-state occupancy organization is moved to the HMM supplement (order-free and locomotion-confounded). ",
  "Volatility uses the 3 RMSSD terms only: the 2 longitudinal terms are undefined inside one Active window. ",
  "The retained set is not orthogonal; its largest overlap is flexibility vs adaptation (Spearman rho +0.749). ",
  DESCR, " n animals per Sex (reference/comparison): Female ", n_min_txt$txt[n_min_txt$Sex == "Female"],
  "; Male ", n_min_txt$txt[n_min_txt$Sex == "Male"], "."), 150)

p_min <- make_fig(
  hm_min, "First response to social instability",
  paste0("Behaviour during the first social-instability encounter, by later phenotype\n",
         wrap_txt(SUB_CORE, 118)),
  cap_min)
save_plot_svg_pdf(p_min, file.path(OUT, "Fig_first_night_minimal_heatmap"),
                  width = 158, height = 98, units = "mm")
cat("wrote Fig_first_night_minimal_heatmap.{svg,pdf,png}  (symmetric limits +/-",
    attr(p_min, "lim"), ")\n")

## ---- 10-row AUDIT panel ---------------------------------------------------
n_aud_txt <- n_txt(hm_aud)
cap_aud <- wrap_txt(paste0(
  "AUDIT PANEL -- shows every candidate INCLUDING rows judged redundant; NOT FOR PUBLICATION. ",
  "lm(DomainScore ~ Group * Sex) + emmeans(~ Group | Sex), contrasts unadjusted; 10-min bins (primary). ",
  "PRIMARY multiplicity family for THIS figure: BH q within Sex over ", length(AUDIT_DOMS),
  " domains x 3 contrasts (", unique(hm_aud$n_tests_in_family), " tests per Sex); ",
  "*** q<0.001, ** q<0.01, * q<0.05. Features z-scored WITHIN SEX ONLY. ",
  "Rows 1-5 and 8-10 are eight projections of the SAME nine z-features, so they cannot be independent: ",
  "#8 = #2 + 0.5*(Entropy_acf1 - Movement_acf1), #9 = #3 + 0.5*(Prox_mean - Prox_acf1) and ",
  "#10 = #1 - Prox_mean, all verified to machine precision. Only rows 6 and 7 carry information outside that ",
  "span; the empirical 10-domain correlation matrix has 3 eigenvalues > 1 and a participation ratio of 3.73, ",
  "i.e. roughly 3-4 independent dimensions behind 10 rows. The non-displayed duplicate 'Early adaptation / ",
  "prediction' (identical to row 5) and top-proximity state occupancy (failed partition robustness across 5 ",
  "HMM optima) are excluded even here. ", DESCR,
  " n animals per Sex (reference/comparison; a range where the proximity- and HMM-dependent rows lose the 2 ",
  "Stage 08 exclusions OQ770/OQ771): Female ", n_aud_txt$txt[n_aud_txt$Sex == "Female"],
  "; Male ", n_aud_txt$txt[n_aud_txt$Sex == "Male"], "."), 150)

p_aud <- make_fig(
  hm_aud, "First response to social instability",
  paste0("AUDIT / NOT FOR PUBLICATION -- all 10 candidate domains, including redundant rows\n",
         "Behaviour during the first social-instability encounter, by later phenotype\n",
         wrap_txt(SUB_CORE, 118)),
  cap_aud)
save_plot_svg_pdf(p_aud, file.path(OUT, "Fig_first_night_10domain_audit_heatmap"),
                  width = 166, height = 130, units = "mm")
cat("wrote Fig_first_night_10domain_audit_heatmap.{svg,pdf,png}  (symmetric limits +/-",
    attr(p_aud, "lim"), ")\n")

sec("minimal-panel effects as displayed (10-min primary)")
print(as.data.frame(hm_min %>%
  select(Domain, Sex, contrast, hedges_g, estimate, SE, ci_low, ci_high, raw_p, q_BH_within_sex) %>%
  mutate(across(where(is.numeric), ~round(., 4))) %>%
  arrange(Domain, Sex, contrast)), row.names = FALSE)

## ---------------------------------------------------------------------------
## STEP 5 -- README
## ---------------------------------------------------------------------------
sec("STEP 5  README")

fem4 <- eff %>% filter(bin_resolution == PRIMARY_RES, Domain == D4, Sex == "Female", contrast == "RES-CON")
q_fem4_min  <- q_min %>% filter(Domain == D4, Sex == "Female", contrast == "RES-CON") %>% pull(q_BH_within_sex)
q_fem4_sens <- q_min_sens %>% filter(Domain == D4, Sex == "Female", contrast == "RES-CON") %>% pull(q_BH_pooled_sex)
int10 <- inter %>% filter(bin_resolution == PRIMARY_RES,
                          candidate_status != "computed_not_displayed_identical_to_active_phase_adaptation")
maxrho_ret <- red_p %>% filter(domain_a %in% RETAINED, domain_b %in% RETAINED)

readme <- c(
"# First-night candidate-set decision - README",
"",
paste0("Generated by `", THIS_SCRIPT, "`. All artifacts live in"),
paste0("`", OUT, "`."),
"This is an AUDIT deliverable. It does not modify `Analysis/`, `Functions/`, or any production figure or table.",
"",
"## 1. The final row set (6 rows) and why",
"",
"| # | Row | Why it is in |",
"|---|-----|--------------|",
"| 1 | Psychomotor activation | Irreducible single feature (`Movement_mean_z`), and the reference axis every other row is checked against for locomotion confounding. |",
"| 4 | Behavioural volatility / fragmentation | The cross-channel instability axis: three homogeneous RMSSD terms, principled equal weighting, no algebraic tie to any other retained row. |",
"| 2 | Behavioural flexibility / predictability | The entropy/predictability axis: single channel, coherently signed. |",
"| 5 | Active-phase adaptation / exploration | The only row contrasting all three channel *levels* against their autocorrelations. |",
"| 3 | Social spatial organization | The only pure social-spatial co-location row, under a deliberately structural (non-motivational) name. |",
"| 7 | Latent-state persistence (HMM mean dwell) | The only row carrying information outside the nine-feature span that is not a locomotion restatement. |",
"",
"**Why six, and not seven, eight or ten.** Rows 1-5 and 8-10 are eight linear projections of the *same nine*",
"z-features, so they live in a space of at most nine dimensions - and empirically in far fewer: the 10-domain",
"correlation matrix has 3 eigenvalues > 1, a participation ratio of 3.73 and needs 5 PCs for 90% of variance,",
"while the 8 raw-feature domains alone have 3 eigenvalues > 1 and a participation ratio of 3.07. Six rows is",
"where the panel stops shrinking for free. The five retained raw-feature rows are the smallest set that still",
"covers all three measurement channels (movement, entropy, proximity) *and* both orthogonal ways a channel can",
"be summarised (level versus instability/persistence); row 7 then adds the one construct that is not a function",
"of those nine features at all. Removing any of the six deletes a channel or a summary type. Adding a seventh",
paste0("(row 8, 9 or 10) adds a row that is already >=80% predictable from the five displayed raw rows",
       " (R2 = ", sprintf("%.3f", r28), ", ", sprintf("%.3f", r29), ", ", sprintf("%.3f", r210),
       " respectively). The number was argued from that structure, not chosen to hit a target count."),
paste0("Honest disclosure: the retained set is *not* orthogonal. Its worst pair is row 2 versus row 5 at",
       " Spearman rho ", sprintf("%+.3f", rho_of(D2, D5)), " (`", cls_of(D2, D5), "`), with row 2 versus row 4 at ",
       sprintf("%+.3f", rho_of(D2, D4)), " and row 4 versus row 5 at ", sprintf("%+.3f", rho_of(D4, D5)),
       ". No retained pair triggers `must_not_both_be_displayed` (0 of ", nrow(maxrho_ret),
       " pairs), no retained pair is classed `near_duplicate` or `highly_redundant`, and the overlap is stated",
       " in the figure caption rather than hidden."),
"",
"## 2. The excluded rows and why",
"",
"| # | Row | Decision | Reason (criteria 1-3, applied in order) |",
"|---|-----|----------|------------------------------------------|",
paste0("| 8 | Early active spatial flexibility | `exclude_redundant` | `#8 = #2 + 0.5*(Ea - Ma)` exactly (4.44e-16); rho ",
       sprintf("%+.3f", rho_of(D8, D2)), " with row 2 (`near_duplicate`, `must_not_both_be_displayed = TRUE`) and ",
       sprintf("%+.3f", rho_of(D8, D5)), " with row 5; only ", sprintf("%.1f%%", u8),
       " of its variance is unexplained by rows 1-5. Its name claims *spatial* while its subtrahend imports `Movement_acf1`, and it is driven by entropy *variability* (rho +0.868 with `Er`), not entropy level (rho +0.198 with `Em`). |"),
paste0("| 9 | Early social engagement | `exclude_redundant` | `#9 = #3 + 0.5*(Pm - Pa)` exactly (5.55e-16); rho ",
       sprintf("%+.3f", rho_of(D9, D3)), " with row 3 (`near_duplicate`); ", sprintf("%.1f%%", u9),
       " unique variance over rows 1-5. \"Engagement\" asserts affiliative motivation from an RFID co-location proxy, and its only structural difference from row 3 is a re-weighting. |"),
paste0("| 10 | Early social withdrawal | `exclude_redundant` | `#10 = #1 - Pm` exactly (0e+00); rho ",
       sprintf("%+.3f", rho_of(D10, D1)), " with row 1 with `must_not_both_be_displayed = TRUE` on the algebraic clause. Its two halves are near-orthogonal (rho(Mm, Pm) = -0.063) and carry equal weight (variance shares +0.504 / -0.496), so one tile value has two indistinguishable readings - \"moved a lot\" or \"was rarely near others\". \"Withdrawal\" asserts avoidance from a co-location proxy. |"),
"| 11 | Early adaptation / prediction | `exclude_duplicate` | Literal duplicate of row 5 (`Analysis/14:5563`): max abs difference 0, r = 1.000000; the 11-domain correlation matrix has an exactly zero final eigenvalue. |",
"",
"Neutral renaming was considered for rows 9 and 10 instead of exclusion, and rejected: renaming repairs the",
"label but not the mathematics. A neutrally named row 9 is a re-weighted row 3, and a neutrally named row 10",
"(\"movement minus co-location contrast\") is still non-identifiable in direction. Row 10 was the closest call -",
paste0("it is the least redundant of the three (", sprintf("%.1f%%", u10),
       " of variance unexplained by rows 1-5) - and it is excluded because that residual is sign cancellation"),
"between two features that are *both already displayed* (`Mm` as row 1, `Pm` inside rows 3 and 5), not new",
"measurement. The retained panel still lets a reader recover the pattern by reading rows 1 and 3 together.",
"`top_proximity_state_fraction` remains out of every panel: it failed partition robustness across five",
"distinct 10-min HMM optima (between-optimum animal-level r as low as 0.117, and a Female Inactive RES-CON",
"sign flip).",
"",
"## 3. The two HMM row decisions",
"",
"Rows 6 and 7 are the only rows carrying information outside the nine-feature span, and they are decided",
"differently - on construct validity, not on p-values.",
"",
paste0("**Row 6, Latent-state occupancy organization -> `move_to_hmm_supplement`.** Three defects, any one of",
       " which would be disqualifying. (a) The score is *exactly* invariant to shuffling the within-epoch state",
       " sequence (max change 0, r = 1.000000), so it carries no temporal-order information and cannot mean",
       " \"organization\". (b) Its subtracted term `inactive_state_fraction` is locomotion-dominated (rho = -0.759",
       " with first-night movement, above the repo's 0.70 flag) and the composite itself sits at rho ",
       sprintf("%+.3f", loco_of(D6)), " with row 1, so the row largely re-expresses an axis that is already",
       " displayed; consistently, ~98% of its Female SUS-RES estimate is absorbed by movement adjustment",
       " (+0.447 -> +0.010). (c) The mandatory 0.5 weight on the entropy term is an inherited constant with no",
       " first-night validation - dropping it changes the score by up to 1.88 and inflates variance 1.59x - so it",
       " can neither be validated nor silently re-weighted here. It belongs in an HMM decomposition panel that",
       " shows occupancy entropy and inactive fraction as separate rows."),
"",
paste0("**Row 7, Latent-state persistence (`mean_dwell_minutes`) -> `retain_primary`.** One metric, no weights,",
       " no differencing, no `coalesce`, and a physical unit (minutes) before z-scoring: after row 1 it is the",
       " cleanest single-metric construct in the candidate set. It is not a locomotion restatement (rho ",
       sprintf("%+.3f", loco_of(D7)), " with row 1 as a domain score; +0.115 for the raw metric in the",
       " HMM-component audit; both far below the 0.70 flag) and it retains ~45% of its SUS-RES estimate after",
       " movement adjustment. It is deliberately the *single* representative of the four collinear temporal HMM",
       " metrics: `state_switch_rate = 1 - self_transition_probability` exactly (2.2e-16) and dwell correlates",
       " r = 0.956/0.985 with them, so displaying more than one would be four tiles for one axis."),
"",
"Row 7 is retained **despite having no nominally significant contrast at first night** (Female SUS-CON +0.467",
"p 0.199, SUS-RES +0.409 p 0.172, RES-CON +0.090 p 0.869; all three male contrasts flat). That is the intended",
"behaviour of the criterion order: a null row that satisfies criteria 1-5 stays, and criterion 6 records its",
"precision without touching the decision. Its first-night estimates are directionally compatible with the",
"longitudinal result (SUS-CON +0.639, p 0.015) but are not statistically convincing on their own, and at first",
"night the other three temporal HMM metrics are flat, so the row must be presented as a construct that is",
"cleanly measured and currently uninformative - never as a positive finding.",
"",
"## 4. The one canonical window",
"",
paste0("`", WINDOW_TXT, "`."),
"Phase membership is tested by EXACT equality against `c(\"active\",\"dark\",\"night\")`, never by substring regex",
"(\"inactive\" contains \"active\"). Verified invariants: 111 animals; 10-min 7879 window rows, expected 72 slots,",
"observed median 71 (68-72); 5-min 15719 rows, expected 144, median 142 (135-144); coverage median 0.9861,",
"min 0.9444; the clock window equals the first contiguous Active block for 111/111 animals; max",
"`diff(TimeIndex)` inside the window = 1 bin. The window comes from *code*",
"(`Analysis/09_early_prediction_model_ladder.R :: select_primary_active_window()`), never from a Stage 09",
"on-disk artifact - stale legacy trees exist and are catalogued in `stage09_stale_artifact_audit.csv`. The",
"Stage 14 production `local_bin <= N` count rule is NOT used: it matches only 50/111 and 33/111 animals.",
"10-min bins are the primary resolution; 5-min is a sensitivity resolution only.",
"",
"## 5. Standardization contract",
"",
paste0("Within a single epoch no Sex x PhaseClass x CageChangeIndex context remains, so ", STD_TXT, "."),
"The nine raw z-features are `Movement`/`Entropy`/`Proximity` x `mean`/`rmssd`/`acf1`, where",
"`rmssd = sqrt(mean(diff^2))` and `acf1` is the lag-1 autocorrelation of the within-window bin series.",
"Stage 14's `score_mean()` is a row-mean over the named z columns, and `coalesce(x, 0)` is applied to the",
"*subtracted* single terms only - reproduced verbatim here, including the fact that the *leading* term is",
"unprotected, which is why rows 3, 9 and 10 are n = 109 rather than 111.",
"",
"## 6. Both FDR families, with n_tests",
"",
paste0("- **PRIMARY**: Benjamini-Hochberg *within Sex*, over the displayed row set. Minimal panel: ",
       length(RETAINED), " domains x 3 contrasts = **", 3L * length(RETAINED),
       " tests per Sex**. Audit panel: 10 x 3 = **30 tests per Sex**."),
paste0("- **SENSITIVITY**: BH pooled over *both* sexes. Minimal panel: ", length(RETAINED),
       " x 3 x 2 = **", 6L * length(RETAINED), " tests**."),
paste0("- Worked example (the strongest cell in the panel): Female `Behavioural volatility / fragmentation` ",
       "RES-CON, g ", sprintf("%.3f", fem4$hedges_g), ", estimate ", sprintf("%.3f", fem4$estimate),
       " (SE ", sprintf("%.3f", fem4$SE), ", 95% CI ", sprintf("%.3f", fem4$ci_low), " to ",
       sprintf("%.3f", fem4$ci_high), ", ", fem4$df, " df), raw p ", signif(fem4$raw_p, 3),
       " -> PRIMARY q = ", signif(q_fem4_min, 3), " (", 3L * length(RETAINED),
       " tests), SENSITIVITY q = ", signif(q_fem4_sens, 3), " (", 6L * length(RETAINED),
       " tests). The estimate and CI are identical under either family; only q changes."),
"- Each figure's stars come from that figure's own row set, so a row is never scored against a family it is",
"  not displayed in. Wider sensitivity across eight alternative row-set compositions is in",
"  `first_night_multiplicity_sensitivity.csv` and `first_night_volatility_multiplicity_focus.csv`.",
"",
"## 7. Inferential unit",
"",
"There is exactly **one value per animal** per domain per resolution, so the model is a plain",
"`stats::lm(DomainScore ~ Group * Sex)` and **not `lmer`**: no repeated measures remain inside a single 12-h",
"epoch, and an animal-level random intercept would be unidentifiable. Contrasts come from",
"`emmeans::emmeans(fit, ~ Group | Sex)` then `contrast(list(\"RES-CON\"=c(-1,1,0), \"SUS-CON\"=c(-1,0,1),",
"\"SUS-RES\"=c(0,-1,1)), adjust=\"none\")`; CI = estimate +/- qt(0.975, df) x SE on the lm residual df (105 for",
"the 111-animal rows, 103 for the 109-animal rows). Effect size is the animal-level Hedges g from",
"`hmm_hedges_g()`. Roster: 111 animals (CON 12F/12M, RES 24F/25M, SUS 22F/16M) from",
"`build_canonical_identity_roster()`. Rows 3, 6 and 7 (and the excluded 9 and 10) cover 109/111: OQ770 and",
"OQ771 have no CC1 Active HMM sequence and no finite window proximity - a Stage 08 data-quality exclusion,",
"**not** identity loss. Per-domain n is reported in the decision table and in the figure captions.",
"",
"## 8. Descriptive, not prospective",
"",
"RES/SUS are **later** phenotype labels derived from *subsequent* CombZ. Every contrast in every table and",
"figure here is a **descriptive association with later phenotype**. The labels were not known at CC1, nothing",
"here predicts anything, and no causal or prospective wording is permitted. RFID proximity is a social-spatial",
"**co-location** proxy - a shared-antenna-space measure - and is never \"sociability\". Latent-state occupancy",
"organization is order-free composition, not temporal flexibility.",
"",
"## 9. Corrected wording that must be carried",
"",
"For **females**, the strong `Active-phase adaptation / exploration` contrast is **SUS-CON** (g -0.871,",
"p 0.016), **not** RES-CON (g -0.605, p 0.067, not significant). The male RES-CON adaptation result (g -0.845,",
"p 0.026) must not be presented as the female pattern. An earlier summary said \"volatility/fragmentation and",
"adaptation already separate RES from CON\"; that is wrong for female adaptation and must not be propagated",
"anywhere. The female FDR survivor is volatility/fragmentation RES-CON (g -1.009, estimate -0.768, p 0.00131),",
"and female volatility SUS-CON is g -0.888, p 0.0226.",
"",
"## 10. Is anything sex-differential? No.",
"",
paste0("By the **formal** `Group:Sex` interaction from `anova(lm(DomainScore ~ Group * Sex))`, **zero of the 10",
       " candidates** shows a significant interaction at the 10-min primary resolution: F ranges ",
       sprintf("%.3f", min(int10$F_value)), "-", sprintf("%.3f", max(int10$F_value)),
       " on (2, 103-105) df, p ranges ", sprintf("%.3f", min(int10$p_uncorrected)), "-",
       sprintf("%.3f", max(int10$p_uncorrected)), ", and the smallest BH q across the 10 interaction tests is ",
       sprintf("%.3f", min(int10$q_BH_across_10_interaction_tests)),
       ". At the 5-min sensitivity resolution the closest case is row 7 (F(2,103) = 3.015, p 0.0534, BH",
       " q 0.534) - still not significant."),
"Therefore **no effect in this audit may be called female-specific, male-specific or sex-differential**, even",
"where a within-Female contrast is significant and the within-Male one is not (which happens for rows 1, 4 and",
"10, and in reverse for rows 2 and 8). Comparing within-sex stars is not a test of a sex difference. The same",
"caution applies to the locomotion-dominance flag: it fires for row 10 in the Male stratum only, but no formal",
"test of the Female-versus-Male difference in that correlation was performed, so that too is not a sex",
"difference.",
"",
"## 11. Significance played no role in row selection",
"",
SIGROLE,
"",
"Two mechanical falsification checks are asserted by the script:",
"",
"1. The **retained** set contains two rows with **zero** nominally significant cells at first night -",
"   `Social spatial organization` and `Latent-state persistence`. If nullity had driven exclusion, both would",
"   be gone.",
"2. The **excluded** set contains the single largest |Hedges g| cell anywhere in the 11 x 2 x 3 table -",
"   `Early active spatial flexibility`, Male RES-CON, g -1.139, p 0.00638. If effect strength had driven",
"   inclusion, it would be displayed.",
"",
"Criterion 6 (statistical precision) is populated from SE, CI width and n only; `raw_p` is never read into it.",
"",
"## 12. Files written here",
"",
"| File | What it is |",
"|------|------------|",
"| `first_night_final_domain_decision.csv` | One row per candidate (11 rows): formula as implemented, feature origin, decision, all six criteria scored and justified, rationale with numbers, `redundant_with`, `evidence_ref`, `significance_played_no_role`. |",
"| `Fig_first_night_minimal_heatmap.{svg,pdf,png}` | The recommended minimal non-redundant panel (6 rows). |",
"| `Fig_first_night_10domain_audit_heatmap.{svg,pdf,png}` | All 10 candidates, marked AUDIT / NOT FOR PUBLICATION in the subtitle. |",
"| `first_night_candidate_set_decision_assertions.csv` | Assertion register for this script. |",
"| `first_night_candidate_set_readme.md` | This file. |",
"",
"The production figures `Fig_first_night_domain_heatmap.*` and `Fig_first_night_hmm_components.*` are NOT",
"touched by this script.",
"",
"Upstream evidence: `first_night_10domain_scores.csv`, `first_night_10domain_formula_overlap.csv`,",
"`first_night_10domain_redundancy.csv`, `first_night_10domain_effects.csv`,",
"`first_night_group_sex_interactions.csv`, `first_night_multiplicity_sensitivity.csv`,",
"`first_night_10domain_locomotion_dominance.csv`, `first_night_10domain_resolution_agreement.csv`,",
"`first_night_time_anchor_audit_long.csv`.")

readme_path <- file.path(OUT, "first_night_candidate_set_readme.md")
writeLines(readme, readme_path, useBytes = TRUE)
cat("wrote first_night_candidate_set_readme.md  (", length(readme), " lines )\n", sep = "")

## ---------------------------------------------------------------------------
## STEP 6 -- assertion register
## ---------------------------------------------------------------------------
sec("STEP 6  assertions")

assert_that("all 11 candidate rows appear exactly once in the decision table",
            nrow(decision) == 11L && !any(duplicated(decision$Domain)) &&
              setequal(decision$Domain, ALL_DOMS),
            sprintf("nrow = %d, distinct Domain = %d", nrow(decision), n_distinct(decision$Domain)),
            "nrow / duplicated / setequal against the 11 known domain names")

ALLOWED <- c("retain_primary", "exclude_redundant", "move_to_hmm_supplement",
             "exclude_duplicate", "exclude_other")
assert_that("every decision value is in the allowed vocabulary",
            all(decision$decision %in% ALLOWED),
            paste(sort(unique(decision$decision)), collapse = " | "),
            "set membership against the 5 allowed tokens")

ret_pairs <- red_p %>% filter(domain_a %in% RETAINED, domain_b %in% RETAINED)
assert_that("no retained pair triggers must_not_both_be_displayed",
            nrow(ret_pairs) == 15L && !any(as.logical(ret_pairs$must_not_both_be_displayed)),
            sprintf("%d retained pairs, %d flagged; max |rho| = %.4f (%s ~ %s)",
                    nrow(ret_pairs), sum(as.logical(ret_pairs$must_not_both_be_displayed)),
                    max(abs(ret_pairs$spearman_rho)),
                    ret_pairs$domain_a[which.max(abs(ret_pairs$spearman_rho))],
                    ret_pairs$domain_b[which.max(abs(ret_pairs$spearman_rho))]),
            "first_night_10domain_redundancy.csv, pooled 10-min stratum, retained x retained")

assert_that("no retained pair is near_duplicate or highly_redundant",
            !any(ret_pairs$redundancy_class_empirical %in% c("near_duplicate", "highly_redundant")),
            paste(sort(unique(ret_pairs$redundancy_class_empirical)), collapse = " | "),
            "redundancy_class_empirical over the 15 retained x retained pairs")

assert_that("no retained pair carries an exact algebraic relation",
            !any(as.logical(ret_pairs$has_exact_algebraic_relation)),
            sprintf("%d of %d retained pairs carry an exact relation",
                    sum(as.logical(ret_pairs$has_exact_algebraic_relation)), nrow(ret_pairs)),
            "has_exact_algebraic_relation over the retained x retained pairs")

out_span <- decision$Domain[decision$feature_origin == "HMM_derived_outside_nine_feature_span" &
                              decision$decision == "retain_primary"]
assert_that("the retained set contains at least one row outside the nine-feature span",
            length(out_span) >= 1L, paste(out_span, collapse = ", "),
            "feature_origin == HMM_derived_outside_nine_feature_span among retained rows")

assert_that("the retained set is 6 rows covering 3 channels and both summary types",
            length(RETAINED) == 6L && setequal(RETAINED, c(D1, D2, D3, D4, D5, D7)),
            "movement level (#1), cross-channel instability (#4), entropy (#2), levels-vs-acf (#5), co-location (#3), out-of-span HMM persistence (#7)",
            "construct-coverage check against the decision column")

null_ret <- decision %>% filter(decision == "retain_primary", n_nominally_sig_cells_10min == 0)
assert_that("FALSIFICATION 1: a RETAINED row has zero nominally significant cells (nullity did not exclude)",
            nrow(null_ret) >= 1L,
            paste(sprintf("%s (n_sig = 0, max |g| = %.3f)", null_ret$Domain, null_ret$max_abs_g_10min),
                  collapse = "; "),
            "count of raw_p < 0.05 per retained Domain in the 10-min effects table")

biggest <- eff %>% filter(bin_resolution == PRIMARY_RES) %>% slice_max(abs(hedges_g), n = 1)
big_dec <- decision$decision[decision$Domain == biggest$Domain]
assert_that("FALSIFICATION 2: the largest |Hedges g| cell belongs to an EXCLUDED row (strength did not include)",
            grepl("^exclude", big_dec),
            sprintf("%s / %s / %s: g = %.3f, p = %.5f -> decision '%s'",
                    biggest$Domain, biggest$Sex, biggest$contrast, biggest$hedges_g,
                    biggest$raw_p, big_dec),
            "slice_max(abs(hedges_g)) over the full 10-min effects table joined to the decision column")

assert_that("PRIMARY family size equals 3 x n_domains per Sex for both figures",
            all(q_min$n_tests_in_family == 3L * length(RETAINED)) &&
              all(q_aud$n_tests_in_family == 3L * length(AUDIT_DOMS)),
            sprintf("minimal %d per Sex, audit %d per Sex",
                    unique(q_min$n_tests_in_family), unique(q_aud$n_tests_in_family)),
            "group_by(Sex) %>% mutate(n()) inside bh_for_set()")

qchk <- q_min %>% group_by(Sex) %>%
  summarise(dev = max(abs(q_BH_within_sex - p.adjust(raw_p, "BH"))), .groups = "drop")
assert_that("figure stars come from BH within Sex recomputed on that figure's own row set",
            max(qchk$dev) == 0,
            sprintf("max abs deviation vs p.adjust(raw_p, 'BH') = %.3g", max(qchk$dev)),
            "independent recomputation per Sex stratum")

assert_that("fill limits are symmetric about 0 and cover every displayed g",
            attr(p_min, "lim") >= max(abs(hm_min$hedges_g), na.rm = TRUE) &&
              attr(p_aud, "lim") >= max(abs(hm_aud$hedges_g), na.rm = TRUE),
            sprintf("minimal +/-%.2f (max |g| %.3f); audit +/-%.2f (max |g| %.3f)",
                    attr(p_min, "lim"), max(abs(hm_min$hedges_g)),
                    attr(p_aud, "lim"), max(abs(hm_aud$hedges_g))),
            "scale_fill_gradient2(limits = c(-lim, lim)), lim = ceiling(max|g| * 20) / 20")

assert_that("NO Group:Sex interaction is significant, so no sex-differential language is licensed",
            all(!as.logical(inter$sex_differential_language_supported)) &&
              all(int10$p_uncorrected >= 0.05),
            sprintf("10 candidates, 0 significant; p range %.4f-%.4f; min BH q %.4f",
                    min(int10$p_uncorrected), max(int10$p_uncorrected),
                    min(int10$q_BH_across_10_interaction_tests)),
            "first_night_group_sex_interactions.csv, 10-min primary")

fem5 <- eff %>% filter(bin_resolution == PRIMARY_RES, Domain == D5, Sex == "Female")
assert_that("CORRECTED WORDING is numerically true: female adaptation is SUS-CON, not RES-CON",
            fem5$raw_p[fem5$contrast == "SUS-CON"] < 0.05 &&
              fem5$raw_p[fem5$contrast == "RES-CON"] >= 0.05,
            sprintf("Female #5 SUS-CON g %.3f p %.4f (sig); RES-CON g %.3f p %.4f (NOT sig)",
                    fem5$hedges_g[fem5$contrast == "SUS-CON"], fem5$raw_p[fem5$contrast == "SUS-CON"],
                    fem5$hedges_g[fem5$contrast == "RES-CON"], fem5$raw_p[fem5$contrast == "RES-CON"]),
            "first_night_10domain_effects.csv, Female, Active-phase adaptation/exploration")

rd <- paste(readLines(readme_path, warn = FALSE), collapse = " ")
need <- c("SUS-CON** (g -0.871", "g -0.605", "g -0.845",
          "must not be presented as the female pattern", "zero of the 10",
          "co-location", "not `lmer`", "descriptive association with later phenotype",
          "Significance played no role in row selection", "AUDIT / NOT FOR PUBLICATION")
hits <- vapply(need, function(s) grepl(s, rd, fixed = TRUE), logical(1))
assert_that("README carries the mandated corrected wording and guards",
            all(hits),
            paste(sprintf("%s=%s", need, hits), collapse = "; "),
            "fixed-string grep over the written README")

figs <- c(file.path(OUT, paste0("Fig_first_night_minimal_heatmap", c(".svg", ".pdf"))),
          file.path(OUT, paste0("Fig_first_night_10domain_audit_heatmap", c(".svg", ".pdf"))))
sz <- file.info(figs)$size
assert_that("all four required figure files exist with non-trivial size",
            all(file.exists(figs)) && all(sz > 10000),
            paste(sprintf("%s = %d bytes", basename(figs), sz), collapse = "; "),
            "file.exists + file.info()$size > 10000 on the 2 svg + 2 pdf targets")

prod_basenames <- c("Fig_first_night_domain_heatmap", "Fig_first_night_hmm_components")
assert_that("the production figure basenames were NOT written by this script",
            !any(tools::file_path_sans_ext(basename(figs)) %in% prod_basenames),
            paste(unique(tools::file_path_sans_ext(basename(figs))), collapse = ", "),
            "basename comparison of this script's figure targets against the production basenames")

reg <- bind_rows(ASSERT) %>%
  mutate(script = THIS_SCRIPT, source_table = "first_night_final_domain_decision.csv")
write_table(reg, file.path(OUT, "first_night_candidate_set_decision_assertions.csv"))
sec("assertion register")
print(as.data.frame(reg %>% select(result, assertion)), row.names = FALSE)
cat(sprintf("\n%d/%d PASS, %d FAIL\n", sum(reg$result == "PASS"), nrow(reg), sum(reg$result == "FAIL")))
if (any(reg$result == "FAIL")) stop("assertion failure -- see register above")

sec("DONE")
cat("outputs in ", OUT, "\n", sep = "")
