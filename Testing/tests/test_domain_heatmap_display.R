# Unit tests for the Stage 14 domain-heatmap helpers
# (Functions/hmm_stage14_helpers.R, appended block: domain_phase_model_data,
# animal_level_contrast_effects, dhm_* and the HMM display helpers).
# Portable: synthetic data only, no S: access, no stage or runner is sourced.
#
# Locks: the display effect sizes equal the engine's; the batch-level fallback
# reproduces a known answer; BH families are heatmap x variant x tier x phase
# over post hoc cells only (consumed registered cells keep their own
# adjustment; a failed test enters with p = 1); a marker needs evidence and the
# colour's sign; the approved models fit on a synthetic cage design and the
# fallback replaces only the CON contrasts; HMM display labels follow the
# state means and time budgets sum to 1; canonical window values average the
# clean blocks; the resolution manifest is complete.

suppressPackageStartupMessages({ library(dplyr); library(tibble); library(purrr); library(tidyr); library(stringr) })

source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")
source_mmm_helper("phase_classification_helpers.R")
source_mmm_helper("first_night_domain_helpers.R")

fail <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
expect_error_matching <- function(expr, pattern, msg) {
  m <- tryCatch({ force(expr); NA_character_ }, error = function(e) conditionMessage(e))
  check(!is.na(m) && grepl(pattern, m, fixed = TRUE), paste0(msg, " (got: ", m, ")"))
}

# ------------------------------------------------------------ synthetic data
set.seed(14)
animals <- tibble(
  AnimalNum = sprintf("A%02d", 1:36),
  Group = rep(c("CON", "RES", "SUS"), each = 12),
  Sex = rep(rep(c("Female", "Male"), each = 6), 3)
)
epochs <- tidyr::crossing(animals, CageChangeIndex = 1:4, PhaseClass = c("Active", "Inactive")) %>%
  mutate(
    shift = case_when(Group == "RES" ~ 0.6, Group == "SUS" ~ -0.4, TRUE ~ 0),
    DomainScore = rnorm(n(), mean = shift),
    Domain = "Synthetic domain"
  )

# ------------------------------------------ A. effect sizes match the engine
md <- domain_phase_model_data(epochs, "Synthetic domain", "Active")
check(nrow(md) == 36 * 4, "A: model data keeps every finite epoch")
eff <- animal_level_contrast_effects(md, c("RES-CON", "SUS-CON", "SUS-RES", "RES-SUS"))
g <- function(sx, ct) eff$animal_level_hedges_g[eff$Sex == sx & eff$contrast == ct]
for (sx in c("Female", "Male")) {
  check(isTRUE(all.equal(g(sx, "RES-SUS"), -g(sx, "SUS-RES"))),
        paste0("A: RES-SUS must be the exact negative of SUS-RES (", sx, ")"))
  am <- md %>% group_by(AnimalNum, Group, Sex) %>% summarise(y = mean(DomainScore), .groups = "drop") %>%
    filter(Sex == sx)
  check(isTRUE(all.equal(g(sx, "RES-CON"), hmm_hedges_g(am$y[am$Group == "CON"], am$y[am$Group == "RES"]))),
        paste0("A: g must be computed on one mean per animal, comparison minus reference (", sx, ")"))
}
check(g("Female", "RES-CON") > 0, "A: a positive RES shift must give a positive RES-CON g")
if (requireNamespace("lmerTest", quietly = TRUE) && requireNamespace("emmeans", quietly = TRUE)) {
  fit_eff <- suppressMessages(fit_repeated_measures_domain_contrasts(epochs, "Synthetic domain", "Active"))$contrasts %>%
    select(Sex, contrast, n_ref_animals, n_comp_animals, mean_ref, mean_comp, animal_level_hedges_g) %>%
    arrange(Sex, contrast)
  ext_eff <- animal_level_contrast_effects(md, c("RES-CON", "SUS-CON", "SUS-RES")) %>% arrange(Sex, contrast)
  check(isTRUE(all.equal(as.data.frame(fit_eff), as.data.frame(ext_eff))),
        "A: the display helpers must return the engine's own effect sizes")
}

# ------------------------------------ B. batch-level fallback (known answer)
if (requireNamespace("data.table", quietly = TRUE)) {
  fb_dat <- data.table::data.table(
    AnimalNum = sprintf("F%02d", 1:18),
    Group = rep(c("CON", "CON", "RES", "RES", "SUS", "SUS"), 3),
    Batch = rep(c("B3", "B4", "B6"), each = 6)
  )
  d_target <- c(B3 = 1, B4 = 2, B6 = 3)
  fb_dat[, y := ifelse(Group == "RES", d_target[Batch], 0) + ifelse(Group == "SUS", 5, 0)]
  fb <- dhm_batch_level_t(fb_dat, "RES")
  check(isTRUE(all.equal(fb$statistic, 3.464102, tolerance = 1e-6)) && fb$df == 2 &&
          isTRUE(all.equal(fb$p_raw, 2 * stats::pt(-sqrt(12), 2), tolerance = 1e-12)) && fb$status == "OK",
        "B: d = (1, 2, 3) must give t = sqrt(12) on 2 df, p = 0.07418")
  check(isTRUE(all.equal(fb$estimate, 2)) && isTRUE(all.equal(fb$se, 1 / sqrt(3))), "B: estimate = mean(D_b), SE = sd(D_b)/sqrt(3)")
  fb2 <- dhm_batch_level_t(fb_dat[Batch != "B6"], "RES")
  check(fb2$status == "NOT_ESTIMABLE" && is.na(fb2$p_raw), "B: fewer than 3 batches is not estimable")
  fb3 <- dhm_batch_level_t(fb_dat[!(Batch == "B4" & Group == "CON")], "RES")
  check(fb3$status == "NOT_ESTIMABLE", "B: a batch without CON animals is not estimable")
}

# --------------------------------------------------- C. families and markers
cells <- tidyr::crossing(heatmap_id = c("h1", "h3"), variant = "picked", stat_family = c("primary", "secondary"),
                         PhaseClass = c("Active", "Inactive"), cell = 1:4) %>%
  mutate(
    test_source = if_else(cell == 4, "registered", "posthoc"),
    test_status = if_else(cell == 3 & stat_family == "secondary", "FAILED", "OK"),
    p_raw = c(0.001, 0.02, 0.04, 0.03)[cell],
    registered_p_adjusted = if_else(test_source == "registered", 0.01, NA_real_),
    estimate = c(1, -1, 1, 1)[cell],
    hedges_g = c(0.5, 0.5, 0.5, 0.5)[cell]
  )
fam <- dhm_add_families(cells)
one <- fam %>% filter(heatmap_id == "h1", stat_family == "primary", PhaseClass == "Active")
check(all(one$family_m[one$test_source == "posthoc"] == 3L), "C: a family holds only its post hoc cells")
check(isTRUE(all.equal(one$q_bh[one$test_source == "posthoc"], stats::p.adjust(c(0.001, 0.02, 0.04), "BH"))),
      "C: q = BH within the family")
check(all(is.na(one$q_bh[one$test_source == "registered"])) && one$adjusted_p[one$test_source == "registered"] == 0.01,
      "C: a consumed registered cell keeps its own adjusted p and is not in the BH family")
sec <- fam %>% filter(heatmap_id == "h1", stat_family == "secondary", PhaseClass == "Active")
check(isTRUE(all.equal(sec$q_bh[sec$test_source == "posthoc"], stats::p.adjust(c(0.001, 0.02, 1), "BH"))),
      "C: a failed test enters the BH family with p = 1")
check(length(unique(fam$family_id[fam$test_source == "posthoc"])) == 8L, "C: one family per heatmap x variant x tier x phase")
check(all(fam$family_m_phase_pooled[fam$test_source == "posthoc"] == 6L), "C: the phase-pooled sensitivity family pools both phases")
check(all(fam$marker_class[fam$cell == 1] == "posthoc"), "C: evidence with the colour's sign gives the post hoc marker")
check(all(fam$marker_class[fam$cell == 2] == "sign_conflict") && !any(fam$marker[fam$cell == 2]),
      "C: evidence with the opposite sign is a sign conflict, never a marker")
check(all(fam$marker_class[fam$cell == 4] == "registered"), "C: a registered result with evidence gives the registered marker")
check(!any(fam$global_correction_used), "C: no global correction")

# --------------------------- D. approved models on a synthetic cage design
model_stack <- all(vapply(c("data.table", "lme4", "lmerTest", "pbkrtest", "reformulas"), requireNamespace, TRUE, quietly = TRUE))
if (model_stack) {
  source_mmm_helper("rfid_canonical_inference.R")
  source_mmm_helper("posthoc_con_contrasts.R")
  set.seed(29)
  design <- map_dfr(c("B3", "B4", "B6"), function(b) {
    con <- tibble(AnimalNum = paste0(b, "_C", 1:4), Group = "CON")
    sis <- tibble(AnimalNum = paste0(b, "_S", 1:12), Group = rep(c("RES", "SUS"), 6))
    bind_rows(con, sis) %>% mutate(Batch = b)
  })
  # SIS animals are regrouped into 3 cages of 4 at every CC; each batch's 4 CON
  # animals stay one intact cage.
  sis_long <- tidyr::crossing(design %>% filter(Group != "CON"), CC = paste0("CC", 1:4)) %>%
    group_by(Batch, CC) %>% mutate(cage = paste0("sis", (sample(n()) - 1) %/% 4 + 1)) %>% ungroup()
  con_long <- tidyr::crossing(design %>% filter(Group == "CON"), CC = paste0("CC", 1:4)) %>% mutate(cage = "con")
  long <- bind_rows(sis_long, con_long) %>%
    mutate(CageEpisodeID = paste(Batch, cage, CC, sep = "|")) %>%
    group_by(AnimalNum) %>% mutate(a = rnorm(1, sd = 0.5)) %>% ungroup() %>%
    group_by(CageEpisodeID) %>% mutate(ce = rnorm(1, sd = 0.3)) %>% ungroup() %>%
    group_by(Batch) %>% mutate(bt = rnorm(1, sd = 0.3), cg = rnorm(1, sd = 0.4)) %>% ungroup() %>%
    mutate(Sex = "Female", y = bt + a + ce + if_else(Group == "CON", cg, 0) + if_else(Group == "RES", 0.8, 0) + rnorm(n(), sd = 0.5))
  coded <- mmm_ci_code_design(data.table::as.data.table(long))
  tt <- suppressWarnings(dhm_tier_tests(coded, "pooled", "synthetic"))
  check(nrow(tt$contrasts) == 3L && setequal(tt$contrasts$contrast, c("RES-SUS", "RES-CON", "SUS-CON")),
        "D: the approved models return RES-SUS, RES-CON and SUS-CON")
  check(nrow(tt$fits) == 2L && all(!tt$fits$failed), "D: two fits, neither failed")
  rs <- tt$contrasts[tt$contrasts$contrast == "RES-SUS", ]
  check(rs$status == "OK" && rs$estimate > 0 && rs$df > 20, "D: RES-SUS recovers the simulated RES > SUS shift with many df")
  con_rows <- tt$contrasts[tt$contrasts$contrast != "RES-SUS", ]
  check(all(con_rows$fallback_used | con_rows$df < 6), "D: a CON contrast has few df (about the 3 CON groups) or uses the fallback")
  old_tol <- MMM_DHM_CON_SINGULAR_TOL
  MMM_DHM_CON_SINGULAR_TOL <- Inf
  tt_fb <- suppressWarnings(dhm_tier_tests(coded, "pooled", "synthetic, forced fallback"))
  MMM_DHM_CON_SINGULAR_TOL <- old_tol
  fb_rows <- tt_fb$contrasts[tt_fb$contrasts$contrast != "RES-SUS", ]
  check(all(fb_rows$fallback_used) && all(fb_rows$df == 2) && all(grepl("batch-level t", fb_rows$test_used)),
        "D: a singular CON-group variance switches only the CON contrasts to the batch-level t")
  check(identical(tt_fb$contrasts$estimate[tt_fb$contrasts$contrast == "RES-SUS"], rs$estimate),
        "D: the fallback leaves RES-SUS unchanged")
} else {
  message("D skipped: lme4 / lmerTest / pbkrtest not installed")
}

# --------------------------------------- E. HMM display labels and time budget
states <- tibble(State = c("1", "2", "3", "4"), Movement_z = c(1.6, -0.6, 0.06, -0.6), Entropy_z = c(1.6, -0.7, 0.4, -0.7),
                 Proximity_z = c(-0.4, 0.75, -0.1, -1.1),
                 SemanticState = c("burst/high-movement", "inactive/low-exploration", "mixed", "inactive/low-exploration"))
lab <- hmm_state_display_labels(states)
check(identical(lab$DisplayState, c("Many position changes", "No position change, co-located", "Few position changes", "No position change, apart")),
      "E: display labels follow the state means")
expect_error_matching(hmm_state_display_labels(states %>% mutate(SemanticState = replace(SemanticState, 3, "social"))),
                      "Re-derive the display rule", "E: an unsupported semantic state fails closed")
occ <- tibble(AnimalNum = "A1", Group = "CON", Sex = "Female", Phase = "Inactive", CageChange = c("CC1", "CC1", "CC1", "CC2", "CC2"),
              State = c("2", "4", "3", "2", "1"), frac_time = c(0.6, 0.3, 0.1, 0.9, 0.1))
tb <- hmm_display_time_budget(occ, lab)
check(isTRUE(all.equal(sum(tb$share), 1)), "E: a time budget sums to 1")
check(isTRUE(all.equal(tb$share[tb$DisplayState == "No position change, apart"], (0.3 + 0) / 2)),
      "E: a state absent from an epoch counts 0 before averaging over cage changes")

# ---------------------------------------- F. canonical window aggregation
if (requireNamespace("data.table", quietly = TRUE)) {
  blk <- data.table::as.data.table(tidyr::crossing(AnimalNum = c("A1", "A2"), CC = paste0("CC", 1:4), phase = c("A1", "A2", "A3", "A4", "L1", "L2", "L3"))) %>%
    mutate(in_clean_set = !(CC == "CC4" & phase %in% c("A3", "A4", "L2", "L3")), complete_primary = TRUE,
           Group = "CON", Sex = "Female", Batch = "B3", CageEpisodeID = paste("B3", "sys.1", CC, sep = "|"),
           crossing_rate = as.numeric(seq_len(n())), shared_zone_use = 0.2, occupancy_dispersion = 1, posinact40 = 0.9,
           posinact60 = 0.8, n_tracked_mates = 3L, obs_s = 100) %>%
    data.table::as.data.table()
  wv <- dhm_canonical_window_values(blk, "clean_all")
  check(nrow(wv) == 2 * 4 * 2 && all(wv$n_blocks[wv$CC == "CC4" & wv$PhaseClass == "Active"] == 2L) &&
          all(wv$n_blocks[wv$CC != "CC4" & wv$PhaseClass == "Inactive"] == 3L),
        "F: all-block epochs average 4/3 blocks (CC1-CC3) and 2/1 (CC4)")
  ref <- blk[AnimalNum == "A1" & CC == "CC2" & substr(phase, 1, 1) == "A", mean(crossing_rate)]
  check(isTRUE(all.equal(wv$crossing_rate[wv$AnimalNum == "A1" & wv$CC == "CC2" & wv$PhaseClass == "Active"], ref)),
        "F: an epoch value is the equal-weight mean of its blocks")
  w1 <- dhm_canonical_window_values(blk, "cc1_L1")
  check(nrow(w1) == 2 && all(w1$PhaseClass == "Inactive") && all(w1$n_blocks == 1L), "F: the CC1 light window is the single L1 block")
  expect_error_matching(dhm_canonical_window_values(blk[!(AnimalNum == "A1" & CC == "CC2" & phase == "L3")], "clean_all"),
                        "expected clean blocks", "F: a missing clean block fails closed")
}

# ------------------------------------------- G. resolution manifest, text
check(nrow(MMM_DHM_RESOLUTION_MANIFEST) == length(MMM_DHM_VARIANTS) * 4 &&
        !anyDuplicated(MMM_DHM_RESOLUTION_MANIFEST[c("variant", "component")]),
      "G: one declared bin width per variant x component")
check(all(MMM_DHM_RESOLUTION_MANIFEST$bin_level[MMM_DHM_RESOLUTION_MANIFEST$variant == "all10min"] == "10min_based") &&
        dhm_resolution("picked", "proximity_terms") == "5min_based" && dhm_resolution("all5min", "switching_term") == "10min_based",
      "G: the declared resolutions")
expect_error_matching(dhm_resolution("picked", "unknown"), "No unique resolution", "G: an unknown component fails closed")
check(identical(dhm_minus(c(-0.004, -0.5, 1)), c("0.00", "−0.50", "1.00")), "G: minus labels never print -0.00")
check(all(nchar(strsplit(dhm_wrap_text(strrep("word ", 200), 50, 6), "\n", fixed = TRUE)[[1]]) <= floor(50 / (0.5 * 6 * 25.4 / 72))),
      "G: wrapped lines fit the printed width")

cat("test_domain_heatmap_display: all checks passed\n")
