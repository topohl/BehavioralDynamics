## audit_first_night_domain_scores.R
## ---------------------------------------------------------------------------
## FIRST-NIGHT (CC1, first Active phase) domain matrix + provenance + redundancy audit.
##
## Deliverables written to
##   <STAGE14>/audit_hmm_state_architecture/first_night_domain_heatmap/
##     1. first_night_domain_scores.csv            (animal x domain long table + provenance fields)
##     2. first_night_domain_provenance.csv        (one row per domain; exposes raw-vs-HMM window mismatch)
##     3. first_night_domain_redundancy_audit.csv  (pairwise Pearson + Spearman, within Sex and pooled)
##   plus supporting audit tables (window impurity, block geometry, coverage, duplicate check,
##   reconciliation with the two existing production CC1 artifacts, exploratory CombZ association).
##
## READ-ONLY with respect to Analysis/ and Functions/. Nothing here writes into production
## tables/ or figures/ directories.
##
## Interpretation guards enforced throughout:
##   - RFID proximity is a social-spatial CO-LOCATION proxy, never "sociability".
##   - Occupancy composition carries NO temporal-order information -> domain 6 is named
##     "Latent-state occupancy organization", never "temporal flexibility".
##   - RES/SUS are LATER phenotype labels derived from subsequent CombZ outcome data; all
##     first-night contrasts are DESCRIPTIVE associations with later phenotype.
## ---------------------------------------------------------------------------

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(purrr); library(tibble); library(readxl)
})

setwd("C:/Users/topohl/Documents/GitHub/MMMSociability")
source("Analysis/_pipeline_setup.R")
source_mmm_helper("hmm_stage14_helpers.R")

PROJ    <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/Behavior/RFID"
STAGE14 <- file.path(PROJ, "analysis_ready/12_systems_neuroscience_summary/5min_based")
AUDIT   <- file.path(STAGE14, "audit_hmm_state_architecture")
OUT     <- file.path(AUDIT, "first_night_domain_heatmap")
HMM     <- file.path(PROJ, "analysis_ready/06_behavioral_dynamics/hmm_states")
BASE5   <- file.path(PROJ, "analysis_ready/03_derived_metrics/5min_based/all_behavior_metrics.csv")
SLEEP5  <- file.path(PROJ, "analysis_ready/16_sleep_like_inactivity_metrics/5min_based/tables/sleep_like_inactivity_features.csv")
COMBZ   <- "S:/Lab_Member/Tobi/Experiments/Exp9_Social-Stress/Analysis/SIS_Analysis/E9_Behavior_Data.xlsx"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

PH_INACT <- "\\binactive\\b|\\blight\\b|\\bday\\b"
PH_ACT   <- "\\bactive\\b|\\bdark\\b|\\bnight\\b"
GROUP_LEVELS <- c("CON", "RES", "SUS")
SEX_LEVELS   <- c("Female", "Male")
THIS_SCRIPT  <- "Testing/audits/audit_first_night_domain_scores.R"

hr  <- function(x) cat("\n", strrep("=", 78), "\n", x, "\n", strrep("=", 78), "\n", sep = "")
sec <- function(x) cat("\n--- ", x, " ---\n", sep = "")

## ---- local copies of small Stage 14 helpers (production files are read-only) ----------
safe_numeric <- function(x) suppressWarnings(as.numeric(x))
parse_cc_index <- function(x) {
  idx <- suppressWarnings(as.integer(str_extract(as.character(x), "\\d+")))
  fallback <- match(as.character(x), sort(unique(as.character(x))))
  ifelse(is.finite(idx), idx, fallback)
}
get_first_cc <- function(x) {
  ux <- unique(as.character(x)); idx <- parse_cc_index(ux)
  ux[which.min(ifelse(is.finite(idx), idx, Inf))]
}
## score_mean(): verbatim Stage 14 semantics (row means over available cols, na.rm = TRUE)
score_mean <- function(dat, cols) {
  cols <- intersect(cols, names(dat))
  if (length(cols) == 0) return(rep(NA_real_, nrow(dat)))
  out <- rowMeans(as.matrix(dat[, cols, drop = FALSE]), na.rm = TRUE)
  out[!is.finite(out)] <- NA_real_
  out
}
safe_cor_p <- function(x, y, method = "spearman") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 4 || sd(x[ok]) == 0 || sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(cor.test(x[ok], y[ok], method = method)$p.value)
}
ent <- function(p) { p <- p[is.finite(p) & p > 0]; if (!length(p)) return(NA_real_); -sum(p * log(p)) }

## Contiguous-block detection: boundary where diff(TimeIndex) > 1.5 * median(diff(unique(TimeIndex)))
add_block_id <- function(ti) {
  o <- order(ti); ti_s <- ti[o]
  ut <- sort(unique(ti_s))
  step <- if (length(ut) > 1) stats::median(diff(ut)) else 1
  d <- c(0, diff(ti_s))
  blk_s <- cumsum(c(1, as.integer(d[-1] > 1.5 * step)))
  out <- integer(length(ti)); out[o] <- blk_s
  out
}

## Viterbi-sequence metrics: identical estimator to
## Testing/audits/audit_hmm_state_architecture_temporal_components.R
seq_metrics <- function(s, K) {
  s <- as.integer(s); n <- length(s)
  occ <- tabulate(s, nbins = K) / n
  H <- ent(occ)
  if (n < 2) {
    return(tibble(occupancy_entropy = H, state_switch_rate = NA_real_,
                  self_transition_probability = NA_real_, transition_entropy = NA_real_,
                  mean_dwell_bins = NA_real_, n_transitions = 0L, n_bins = n,
                  occ = list(occ)))
  }
  from <- s[-n]; to <- s[-1]; nt <- n - 1L
  TC <- matrix(0L, K, K)
  for (i in seq_len(nt)) TC[from[i], to[i]] <- TC[from[i], to[i]] + 1L
  rs <- rowSums(TC); pi_s <- rs / nt
  rowH <- vapply(seq_len(K), function(k) if (rs[k] > 0) ent(TC[k, ] / rs[k]) else 0, numeric(1))
  self_p <- sum(pi_s * ifelse(rs > 0, diag(TC) / rs, 0))
  runs <- rle(s)
  dw <- vapply(seq_len(K), function(k) {
    l <- runs$lengths[runs$values == k]; if (!length(l)) NA_real_ else mean(l)
  }, numeric(1))
  mean_dwell <- sum(occ * dw, na.rm = TRUE) / sum(occ[!is.na(dw)])
  tibble(occupancy_entropy = H, state_switch_rate = mean(from != to),
         self_transition_probability = self_p, transition_entropy = sum(pi_s * rowH),
         mean_dwell_bins = mean_dwell, n_transitions = nt, n_bins = n,
         occ = list(occ))
}

hr("STEP 0. Canonical roster (111-animal contract)")

base_raw <- read_csv(BASE5, col_types = cols(.default = col_guess(), AnimalNum = col_character()),
                     progress = FALSE)
cat("raw 5min metrics rows:", nrow(base_raw), " cols:", ncol(base_raw), "\n")

roster <- build_canonical_identity_roster(
  base_raw %>% select(AnimalNum, Group, Sex),
  "Stage 01 canonical roster (5min all_behavior_metrics)"
)
cat("canonical roster animals:", nrow(roster), "\n")
cat("roster columns:", paste(names(roster), collapse = ", "), "\n")
print(roster %>% count(Group, Sex) %>% arrange(Group, Sex) %>% as.data.frame())
stopifnot(nrow(roster) == n_distinct(roster$AnimalNum))

base <- base_raw %>%
  mutate(
    AnimalNum       = canonical_animal_id(AnimalNum),
    Phase           = as.character(Phase),
    CageChange      = as.character(CageChange),
    CageChangeIndex = parse_cc_index(CageChange),
    PhaseClass = case_when(
      str_detect(str_to_lower(Phase), PH_INACT) ~ "Inactive",
      str_detect(str_to_lower(Phase), PH_ACT)   ~ "Active",
      TRUE ~ Phase
    ),
    TimeIndex = safe_numeric(TimeIndex),
    Movement  = safe_numeric(Movement),
    Entropy   = safe_numeric(Entropy),
    ## Stage 14's first_existing_col() resolves "Proximity" to ProximityFraction. Preserved.
    Proximity = safe_numeric(ProximityFraction)
  ) %>%
  select(AnimalNum, Group, Sex, Phase, PhaseClass, CageChange, CageChangeIndex,
         TimeIndex, BinSizeSec, Movement, Entropy, Proximity) %>%
  arrange(AnimalNum, CageChange, Phase, TimeIndex)

BIN_SEC_5 <- unique(safe_numeric(base$BinSizeSec))
stopifnot(length(BIN_SEC_5) == 1)
cat("5min bin size (sec):", BIN_SEC_5, "\n")
cat("raw base animals after canonicalization:", n_distinct(base$AnimalNum), "\n")
cat("Stage 14 production chip_loss_qc_mode is 'annotate_only' -> base is NOT epoch-filtered here.\n")

hr("STEP 1. RAW first-night window: reconstruct Stage 14 `first_active` (5min_based)")

first_cc <- get_first_cc(base$CageChange)
EARLY_BINS <- 12 * 60 / 5   # Stage 14: primary_bin_level == "5min_based" -> 144
cat("first cage change:", first_cc, " | early_window_bins:", EARLY_BINS, "\n")

cc1_active <- base %>%
  filter(as.character(CageChange) == first_cc,
         PhaseClass == "Active" | str_detect(str_to_lower(Phase), PH_ACT))
cat("CC1 Active raw rows:", nrow(cc1_active),
    " animals:", n_distinct(cc1_active$AnimalNum), "\n")
cat("CC1 Active raw TimeIndex range:", min(cc1_active$TimeIndex), "-", max(cc1_active$TimeIndex), "\n")

cc1_active_blk <- cc1_active %>%
  group_by(AnimalNum) %>%
  arrange(TimeIndex, .by_group = TRUE) %>%
  mutate(local_bin = row_number(), block = add_block_id(TimeIndex)) %>%
  ungroup()

raw_block_geom <- cc1_active_blk %>%
  group_by(AnimalNum, Group, Sex) %>%
  summarise(n_blocks = n_distinct(block),
            n_bins_total = n(),
            n_bins_block1 = sum(block == 1),
            index_span_bins = max(TimeIndex) - min(TimeIndex) + 1,
            .groups = "drop") %>%
  mutate(active_hours_total = n_bins_total * BIN_SEC_5 / 3600,
         block1_hours       = n_bins_block1 * BIN_SEC_5 / 3600,
         index_span_hours   = index_span_bins * BIN_SEC_5 / 3600,
         resolution         = "5min_based")

sec("Raw CC1 Active block geometry (5min_based)")
cat("blocks per animal:\n"); print(table(raw_block_geom$n_blocks))
cat("total Active bins : median", median(raw_block_geom$n_bins_total),
    " range", paste(range(raw_block_geom$n_bins_total), collapse = "-"), "\n")
cat("total Active hours: median", round(median(raw_block_geom$active_hours_total), 2),
    " range", paste(round(range(raw_block_geom$active_hours_total), 2), collapse = "-"), "\n")
cat("block1 bins       : median", median(raw_block_geom$n_bins_block1),
    " range", paste(range(raw_block_geom$n_bins_block1), collapse = "-"), "\n")
cat("block1 hours      : median", round(median(raw_block_geom$block1_hours), 3),
    " range", paste(round(range(raw_block_geom$block1_hours), 3), collapse = "-"), "\n")

## WINDOW IMPURITY: does the fixed local_bin <= 144 cut reach past block 1?
impurity <- cc1_active_blk %>%
  filter(local_bin <= EARLY_BINS) %>%
  group_by(AnimalNum, Group, Sex) %>%
  summarise(n_bins_in_window = n(),
            n_bins_from_block1 = sum(block == 1),
            n_bins_borrowed_from_later_blocks = sum(block > 1),
            max_block_touched = max(block),
            .groups = "drop") %>%
  left_join(raw_block_geom %>% select(AnimalNum, n_bins_block1, block1_hours), by = "AnimalNum") %>%
  mutate(window_impure = n_bins_borrowed_from_later_blocks > 0,
         borrowed_hours = n_bins_borrowed_from_later_blocks * BIN_SEC_5 / 3600,
         borrowed_fraction_of_window = n_bins_borrowed_from_later_blocks / n_bins_in_window)

sec("Window impurity of the fixed local_bin <= 144 cut")
cat("animals in window:", nrow(impurity), "\n")
cat("animals with impure window (>=1 bin from night 2):", sum(impurity$window_impure),
    sprintf(" (%.1f%%)\n", 100 * mean(impurity$window_impure)))
print(table(borrowed_bins = impurity$n_bins_borrowed_from_later_blocks))
cat("borrowed bins: median", median(impurity$n_bins_borrowed_from_later_blocks),
    " mean", round(mean(impurity$n_bins_borrowed_from_later_blocks), 3),
    " max", max(impurity$n_bins_borrowed_from_later_blocks), "\n")
cat("mean borrowed fraction of the 144-bin window:",
    sprintf("%.4f", mean(impurity$borrowed_fraction_of_window)), "\n")
cat("window bins actually available: median", median(impurity$n_bins_in_window),
    " range", paste(range(impurity$n_bins_in_window), collapse = "-"), "\n")
write_csv(impurity, file.path(OUT, "first_night_raw_window_impurity.csv"))
write_csv(raw_block_geom, file.path(OUT, "first_night_raw_cc1_active_block_geometry.csv"))

## Stage 14 `first_active` reconstruction, verbatim grouping (AnimalNum x Phase)
first_active <- cc1_active %>%
  group_by(AnimalNum, Phase) %>%
  arrange(TimeIndex, .by_group = TRUE) %>%
  mutate(local_bin = row_number()) %>%
  filter(local_bin <= EARLY_BINS) %>%
  ungroup()
cat("first_active rows:", nrow(first_active), " animals:", n_distinct(first_active$AnimalNum), "\n")

hr("STEP 2. RAW domain scores 1-5 (+ duplicate row 6) -- z within SEX ONLY")

raw_feat <- first_active %>%
  group_by(AnimalNum, Group, Sex) %>%
  arrange(TimeIndex, .by_group = TRUE) %>%
  summarise(
    Movement_mean  = mean(Movement, na.rm = TRUE),
    Movement_rmssd = if (sum(is.finite(Movement)) >= 3) sqrt(mean(diff(Movement[is.finite(Movement)])^2, na.rm = TRUE)) else NA_real_,
    Movement_acf1  = if (sum(is.finite(Movement)) >= 4) safe_cor(Movement[is.finite(Movement)][-sum(is.finite(Movement))], Movement[is.finite(Movement)][-1], "pearson") else NA_real_,
    Entropy_mean   = mean(Entropy, na.rm = TRUE),
    Entropy_rmssd  = if (sum(is.finite(Entropy)) >= 3) sqrt(mean(diff(Entropy[is.finite(Entropy)])^2, na.rm = TRUE)) else NA_real_,
    Entropy_acf1   = if (sum(is.finite(Entropy)) >= 4) safe_cor(Entropy[is.finite(Entropy)][-sum(is.finite(Entropy))], Entropy[is.finite(Entropy)][-1], "pearson") else NA_real_,
    Proximity_mean  = mean(Proximity, na.rm = TRUE),
    Proximity_rmssd = if (sum(is.finite(Proximity)) >= 3) sqrt(mean(diff(Proximity[is.finite(Proximity)])^2, na.rm = TRUE)) else NA_real_,
    Proximity_acf1  = if (sum(is.finite(Proximity)) >= 4) safe_cor(Proximity[is.finite(Proximity)][-sum(is.finite(Proximity))], Proximity[is.finite(Proximity)][-1], "pearson") else NA_real_,
    n_bins = n(),
    .groups = "drop"
  )
cat("raw_feat animals:", nrow(raw_feat), "\n")

RAW_Z_COLS <- c("Movement_mean", "Movement_rmssd", "Movement_acf1",
                "Entropy_mean", "Entropy_rmssd", "Entropy_acf1",
                "Proximity_mean", "Proximity_rmssd", "Proximity_acf1")

## Single-epoch analysis => no Sex x PhaseClass x CageChangeIndex context remains.
## Standardize within SEX ONLY. This is also exactly what Stage 14's own
## sis_first_active_score_base does: standardize_within_context(..., group_cols = "Sex").
raw_z <- reduce(RAW_Z_COLS,
                function(d, v) strict_standardize_within_context(d, v, group_cols = "Sex"),
                .init = raw_feat)

raw_scores <- raw_z %>%
  mutate(
    `Psychomotor activation` = Movement_mean_z,
    `Behavioral flexibility / predictability` =
      score_mean(pick(everything()), c("Entropy_mean_z", "Entropy_rmssd_z")) - coalesce(Entropy_acf1_z, 0),
    `Social spatial organization` =
      score_mean(pick(everything()), c("Proximity_mean_z", "Proximity_acf1_z")) - coalesce(Proximity_rmssd_z, 0),
    `Behavioral volatility / fragmentation` =
      score_mean(pick(everything()), c("Movement_rmssd_z", "Entropy_rmssd_z", "Proximity_rmssd_z")),
    `Active-phase adaptation/exploration` =
      score_mean(pick(everything()), c("Movement_mean_z", "Entropy_mean_z", "Proximity_mean_z")) -
      score_mean(pick(everything()), c("Movement_acf1_z", "Entropy_acf1_z")),
    `Early adaptation / prediction` = `Active-phase adaptation/exploration`
  )

sec("Raw first-night feature missingness (animal level)")
raw_na <- raw_feat %>%
  mutate(across(all_of(RAW_Z_COLS), ~ !is.finite(.x), .names = "na_{.col}")) %>%
  mutate(n_na_features = rowSums(pick(starts_with("na_")))) %>%
  filter(n_na_features > 0) %>%
  select(AnimalNum, Group, Sex, n_bins, n_na_features, starts_with("na_"))
cat("animals with >=1 non-finite raw first-night feature:", nrow(raw_na), "\n")
if (nrow(raw_na) > 0) {
  print(as.data.frame(raw_na %>% select(AnimalNum, Group, Sex, n_bins, n_na_features,
                                        names(which(colSums(raw_na %>% select(starts_with("na_"))) > 0)))))
  cat("-> these animals still contribute to Movement/Entropy-based domains because Stage 14's",
      "score_mean() uses na.rm = TRUE; only the Proximity-only domain becomes NA.\n")
}
write_csv(raw_na, file.path(OUT, "first_night_raw_feature_missingness.csv"))

sec("Duplicate-row check: `Early adaptation / prediction` vs `Active-phase adaptation/exploration`")
dup_diff <- raw_scores$`Early adaptation / prediction` - raw_scores$`Active-phase adaptation/exploration`
dup_r <- suppressWarnings(cor(raw_scores$`Early adaptation / prediction`,
                              raw_scores$`Active-phase adaptation/exploration`, use = "complete.obs"))
cat("max |difference| =", max(abs(dup_diff), na.rm = TRUE), " | Pearson r =", dup_r, "\n")
cat("Stage 14 definition: `Early adaptation / prediction` = if_else(Active & CageChangeIndex ==\n",
    "  min(CageChangeIndex), `Active-phase adaptation/exploration`, NA) -> identical at CC1 by construction.\n")
dup_check <- tibble(
  check = "Early adaptation / prediction vs Active-phase adaptation/exploration (CC1 first night)",
  n_animals = sum(is.finite(dup_diff)),
  max_abs_difference = max(abs(dup_diff), na.rm = TRUE),
  pearson_r = dup_r,
  conclusion = "mathematically identical at CC1; only `Active-phase adaptation/exploration` is displayed",
  stage14_definition = "Early adaptation / prediction = if_else(PhaseClass=='Active' & CageChangeIndex==min(CageChangeIndex), `Active-phase adaptation/exploration`, NA_real_)"
)
write_csv(dup_check, file.path(OUT, "first_night_duplicate_domain_check.csv"))

hr("STEP 3. HMM first-night features: COMMON state space, FIRST CONTIGUOUS BLOCK")

state_labels <- list(); hmm_first_block <- list(); hmm_block_geom <- list(); hmm_coverage <- list()
BIN_SEC <- c("10min_based" = 600, "5min_based" = 300)

for (res in c("10min_based", "5min_based")) {
  sec(paste("resolution:", res))
  ss <- read_csv(file.path(HMM, res, "tables/hmm_state_summary.csv"),
                 col_types = cols(State = col_character(), .default = col_guess()), progress = FALSE)
  lab <- annotate_hmm_semantic_states(ss, res)
  state_labels[[res]] <- lab
  print(lab %>% select(State, Movement_z, Entropy_z, Proximity_z, SemanticState) %>%
          mutate(across(where(is.numeric), ~ round(.x, 4))) %>% as.data.frame())
  top_prox <- lab$State[which.max(lab$Proximity_z)]
  cat("argmax Proximity_z state (derived programmatically): S", top_prox, "\n", sep = "")
  inact_states <- lab$State[lab$SemanticState == "inactive/low-exploration"]
  soc_states   <- lab$State[lab$SemanticState == "social"]
  cat("inactive/low-exploration states: ", paste0("S", inact_states, collapse = ","),
      " | social states: ",
      if (length(soc_states)) paste0("S", soc_states, collapse = ",") else "<none>", "\n", sep = "")

  a <- read_csv(file.path(HMM, res, "tables/hmm_state_assignments.csv"),
                col_types = cols(AnimalNum = col_character(), State = col_character(),
                                 .default = col_guess()), progress = FALSE)
  aud <- audit_hmm_identity(a, roster, paste("hmm_state_assignments", res))
  assert_hmm_identity_audit(aud)
  cat("identity audit PASSED for", res, "assignments (rows:", nrow(aud$data), ")\n")

  ad <- aud$data %>%
    mutate(PhaseClass = case_when(str_detect(str_to_lower(Phase), PH_INACT) ~ "Inactive",
                                  str_detect(str_to_lower(Phase), PH_ACT)   ~ "Active",
                                  TRUE ~ as.character(Phase)),
           CageChange = as.character(CageChange),
           CageChangeIndex = parse_cc_index(CageChange),
           State = as.integer(State),
           TimeIndex = safe_numeric(TimeIndex))
  K <- max(as.integer(lab$State))
  stopifnot(K == n_distinct(ad$State))

  cc1a <- ad %>% filter(CageChangeIndex == 1, PhaseClass == "Active")
  cat("CC1 Active HMM rows:", nrow(cc1a), " animals:", n_distinct(cc1a$AnimalNum), "\n")

  cc1a <- cc1a %>% group_by(AnimalNum) %>% arrange(TimeIndex, .by_group = TRUE) %>%
    mutate(block = add_block_id(TimeIndex)) %>% ungroup()

  geom <- cc1a %>% group_by(AnimalNum, Group, Sex) %>%
    summarise(n_blocks = n_distinct(block), n_bins_total = n(), n_bins_block1 = sum(block == 1),
              index_span_bins = max(TimeIndex) - min(TimeIndex) + 1,
              gap_bins_median = {
                bs <- sort(unique(block))
                if (length(bs) > 1) {
                  starts <- vapply(bs, function(b) min(TimeIndex[block == b]), numeric(1))
                  ends   <- vapply(bs, function(b) max(TimeIndex[block == b]), numeric(1))
                  stats::median(starts[-1] - ends[-length(ends)] - 1)
                } else NA_real_
              },
              .groups = "drop") %>%
    mutate(active_hours_total = n_bins_total * BIN_SEC[[res]] / 3600,
           block1_hours       = n_bins_block1 * BIN_SEC[[res]] / 3600,
           index_span_hours   = index_span_bins * BIN_SEC[[res]] / 3600,
           gap_hours_median   = gap_bins_median * BIN_SEC[[res]] / 3600,
           resolution = res)
  cat("blocks per animal:\n"); print(table(geom$n_blocks))
  cat("total Active bins : median", median(geom$n_bins_total),
      " range", paste(range(geom$n_bins_total), collapse = "-"), "\n")
  cat("total Active hours: median", round(median(geom$active_hours_total), 2),
      " range", paste(round(range(geom$active_hours_total), 2), collapse = "-"), "\n")
  cat("block1 bins       : median", median(geom$n_bins_block1),
      " range", paste(range(geom$n_bins_block1), collapse = "-"), "\n")
  cat("block1 hours      : median", round(median(geom$block1_hours), 3),
      " range", paste(round(range(geom$block1_hours), 3), collapse = "-"), "\n")
  cat("inter-block gap   : median", round(median(geom$gap_hours_median, na.rm = TRUE), 3), "h\n")
  hmm_block_geom[[res]] <- geom

  ## No temporal gaps inside a single contiguous block -> gap-crossing artefacts cannot arise.
  gapchk <- cc1a %>% filter(block == 1) %>% group_by(AnimalNum) %>%
    summarise(max_gap = if (n() > 1) max(diff(sort(TimeIndex))) else NA_real_, .groups = "drop")
  cat("max within-block-1 TimeIndex step across animals:", max(gapchk$max_gap, na.rm = TRUE),
      "(1 == fully contiguous; no gap crossing for first-night features)\n")

  m <- cc1a %>% filter(block == 1) %>% group_by(AnimalNum, Group, Sex) %>%
    group_modify(~ seq_metrics(.x$State, K)) %>% ungroup()
  occ_mat <- do.call(rbind, m$occ)
  colnames(occ_mat) <- paste0("frac_S", seq_len(K))
  ## Semantic aggregation done on the occupancy matrix directly (tidyselect cannot
  ## handle a zero-length state set inside mutate()).
  inact_frac <- if (length(inact_states))
    rowSums(occ_mat[, paste0("frac_S", inact_states), drop = FALSE]) else rep(0, nrow(occ_mat))
  soc_frac <- if (length(soc_states))
    rowSums(occ_mat[, paste0("frac_S", soc_states), drop = FALSE]) else rep(0, nrow(occ_mat))
  topprox_frac <- occ_mat[, paste0("frac_S", top_prox)]
  m <- m %>% select(-occ) %>% bind_cols(as_tibble(occ_mat)) %>%
    mutate(
      resolution = res,
      bin_size_sec = BIN_SEC[[res]],
      mean_dwell_minutes = mean_dwell_bins * BIN_SEC[[res]] / 60,
      window_hours = n_bins * BIN_SEC[[res]] / 3600,
      inactive_state_fraction = inact_frac,
      social_state_fraction = soc_frac,
      top_proximity_state = paste0("S", top_prox),
      top_proximity_state_fraction = topprox_frac
    )
  cat("social_state_fraction: max =", max(m$social_state_fraction),
      "| all exactly zero:", all(m$social_state_fraction == 0), "\n")
  cat("occupancy_entropy: mean", round(mean(m$occupancy_entropy, na.rm = TRUE), 4),
      " range", paste(round(range(m$occupancy_entropy, na.rm = TRUE), 4), collapse = "-"), "\n")
  cat("inactive_state_fraction: mean", round(mean(m$inactive_state_fraction), 4),
      " range", paste(round(range(m$inactive_state_fraction), 4), collapse = "-"), "\n")
  cat("mean_dwell_bins mean =", round(mean(m$mean_dwell_bins, na.rm = TRUE), 4),
      "-> mean_dwell_minutes mean =", round(mean(m$mean_dwell_minutes, na.rm = TRUE), 3), "\n")
  cat("unit-identity max|dwell_min - dwell_bins*bin_sec/60| =",
      max(abs(m$mean_dwell_minutes - m$mean_dwell_bins * BIN_SEC[[res]] / 60)), "\n")
  cat("redundancy identity max|switch_rate - (1 - P_self)| =",
      max(abs(m$state_switch_rate - (1 - m$self_transition_probability)), na.rm = TRUE), "\n")
  ok <- is.finite(m$mean_dwell_bins) & is.finite(m$self_transition_probability) & m$self_transition_probability < 1
  cat("cor(mean_dwell_bins, 1/(1-P_self)) =",
      round(cor(m$mean_dwell_bins[ok], 1 / (1 - m$self_transition_probability[ok])), 4),
      "| Spearman =",
      round(cor(m$mean_dwell_bins[ok], 1 / (1 - m$self_transition_probability[ok]), method = "spearman"), 4), "\n")
  hmm_first_block[[res]] <- m

  ## coverage: which canonical animals lack CC1 Active HMM rows, and why
  miss <- setdiff(roster$AnimalNum, unique(cc1a$AnimalNum))
  sq <- read_csv(file.path(HMM, res, "tables/hmm_sequence_quality_audit.csv"),
                 col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum))
  ex <- read_csv(file.path(HMM, res, "tables/hmm_epoch_data_quality_exclusions.csv"),
                 col_types = cols(AnimalNum = col_character(), .default = col_guess()), progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum))
  cov_res <- tibble(resolution = res, AnimalNum = miss) %>%
    left_join(roster %>% select(AnimalNum, Group, Sex), by = "AnimalNum") %>%
    left_join(sq %>% filter(CageChange == "CC1", str_detect(str_to_lower(Phase), PH_ACT)) %>%
                transmute(AnimalNum, sq_input_bins = input_bins,
                          sq_complete_hmm_bins = complete_hmm_bins,
                          sq_retained_for_hmm = retained_for_hmm,
                          sq_exclusion_reason = exclusion_reason),
              by = "AnimalNum") %>%
    left_join(ex %>% filter(CageChange == "CC1", str_detect(str_to_lower(Phase), PH_ACT)) %>%
                transmute(AnimalNum, excl_input_bins = input_bins,
                          excl_complete_hmm_bins = complete_hmm_bins,
                          excl_reason = exclusion_reason),
              by = "AnimalNum") %>%
    mutate(present_in_raw_cc1_active = AnimalNum %in% unique(cc1_active$AnimalNum),
           raw_cc1_active_bins = map_int(AnimalNum, ~ sum(cc1_active$AnimalNum == .x)),
           present_in_raw_first_night_window = AnimalNum %in% unique(raw_feat$AnimalNum))
  cat("canonical animals WITHOUT CC1 Active HMM rows:", length(miss),
      if (length(miss)) paste0(" -> ", paste(miss, collapse = ", ")) else "", "\n")
  if (length(miss)) print(as.data.frame(cov_res %>% select(-resolution)))
  hmm_coverage[[res]] <- cov_res

  ## Also: how many CC1 Active exclusion rows exist at all, for context
  cat("hmm_epoch_data_quality_exclusions rows for CC1 Active:",
      nrow(ex %>% filter(CageChange == "CC1", str_detect(str_to_lower(Phase), PH_ACT))), "\n")
}

hmm_block_geom_all <- bind_rows(hmm_block_geom)
hmm_coverage_all   <- bind_rows(hmm_coverage)
write_csv(hmm_block_geom_all, file.path(OUT, "first_night_hmm_first_block_geometry.csv"))
write_csv(hmm_coverage_all,   file.path(OUT, "first_night_hmm_coverage_missing_animals.csv"))
write_csv(bind_rows(state_labels), file.path(OUT, "first_night_hmm_state_semantics.csv"))

sec("Cross-resolution persistence comparison (PHYSICAL TIME, minutes)")
xres <- bind_rows(hmm_first_block) %>% group_by(resolution) %>%
  summarise(n = n(),
            mean_dwell_bins = mean(mean_dwell_bins, na.rm = TRUE),
            mean_dwell_minutes = mean(mean_dwell_minutes, na.rm = TRUE),
            mean_switch_rate = mean(state_switch_rate, na.rm = TRUE),
            mean_occupancy_entropy = mean(occupancy_entropy, na.rm = TRUE),
            .groups = "drop")
print(as.data.frame(xres %>% mutate(across(where(is.numeric), ~ round(.x, 4)))))
if (nrow(xres) == 2) {
  a5  <- xres$mean_dwell_minutes[xres$resolution == "5min_based"]
  a10 <- xres$mean_dwell_minutes[xres$resolution == "10min_based"]
  cat(sprintf("10min vs 5min mean dwell: %.2f vs %.2f min (%+.1f%%) -- short-bout censoring at coarser bins,\n",
              a10, a5, 100 * (a10 / a5 - 1)))
  cat("  a temporal-resolution artefact, not a unit artefact.\n")
}
write_csv(xres, file.path(OUT, "first_night_hmm_cross_resolution_dwell.csv"))

hr("STEP 4. HMM domain scores 6-7 (+ supplementary) -- z within SEX ONLY")

hmm_scores <- map_dfr(names(hmm_first_block), function(res) {
  d <- hmm_first_block[[res]]
  d <- reduce(c("occupancy_entropy", "inactive_state_fraction", "social_state_fraction",
                "mean_dwell_minutes", "mean_dwell_bins", "top_proximity_state_fraction"),
              function(x, v) strict_standardize_within_context(x, v, group_cols = "Sex"), .init = d)
  ## FORMULA PRESERVATION: shipped composite is
  ##   mean(z(occupancy_entropy), z(social_state_fraction)) - z(inactive_state_fraction)
  ## and z(social) is identically 0, so it equals 0.5*z(occupancy_entropy) - z(inactive_state_fraction).
  shipped <- rowMeans(cbind(d$occupancy_entropy_z, d$social_state_fraction_z), na.rm = FALSE) -
    d$inactive_state_fraction_z
  reduced <- 0.5 * d$occupancy_entropy_z - d$inactive_state_fraction_z
  cat(res, ": max|shipped_form - (0.5*z(entropy) - z(inactive))| =",
      max(abs(shipped - reduced), na.rm = TRUE), "\n")
  cat(res, ": max|z(mean_dwell_minutes) - z(mean_dwell_bins)| =",
      max(abs(d$mean_dwell_minutes_z - d$mean_dwell_bins_z), na.rm = TRUE),
      "(positive affine rescaling -> context-z invariant)\n")
  d %>% mutate(
    `Latent-state occupancy organization` = reduced,
    `Latent-state persistence` = mean_dwell_minutes_z,
    `Low-activity high-co-occupancy state occupancy` = top_proximity_state_fraction_z,
    `Latent-state occupancy organization (unweighted-entropy sensitivity)` =
      occupancy_entropy_z - inactive_state_fraction_z
  )
})

sec("Sensitivity construct check (explicitly NOT the primary score)")
for (res in unique(hmm_scores$resolution)) {
  d <- hmm_scores %>% filter(resolution == res)
  cat(res, ": shipped-0.5 vs unweighted -> max|diff| =",
      round(max(abs(d$`Latent-state occupancy organization` -
                      d$`Latent-state occupancy organization (unweighted-entropy sensitivity)`), na.rm = TRUE), 4),
      "| r =", round(cor(d$`Latent-state occupancy organization`,
                         d$`Latent-state occupancy organization (unweighted-entropy sensitivity)`,
                         use = "complete.obs"), 4),
      "| var ratio =",
      round(var(d$`Latent-state occupancy organization (unweighted-entropy sensitivity)`, na.rm = TRUE) /
              var(d$`Latent-state occupancy organization`, na.rm = TRUE), 3), "\n")
}
write_csv(hmm_scores, file.path(OUT, "first_night_hmm_first_block_features.csv"))

hr("STEP 5. Assemble first_night_domain_scores.csv")

RAW_PHASE_WINDOW <- "CC1 Active, first 12 h = local_bin <= 144 at 5-min bins (Stage 14 first_active contract)"
raw_med_bins  <- median(raw_feat$n_bins)
raw_med_hours <- raw_med_bins * BIN_SEC_5 / 3600

raw_formulas <- c(
  "Psychomotor activation" = "Movement_mean_z",
  "Behavioral flexibility / predictability" = "mean(Entropy_mean_z, Entropy_rmssd_z) - coalesce(Entropy_acf1_z, 0)",
  "Social spatial organization" = "mean(Proximity_mean_z, Proximity_acf1_z) - coalesce(Proximity_rmssd_z, 0); Proximity = ProximityFraction, a social-spatial CO-LOCATION proxy (not sociability)",
  "Behavioral volatility / fragmentation" = "mean(Movement_rmssd_z, Entropy_rmssd_z, Proximity_rmssd_z)",
  "Active-phase adaptation/exploration" = "mean(Movement_mean_z, Entropy_mean_z, Proximity_mean_z) - mean(Movement_acf1_z, Entropy_acf1_z)",
  "Early adaptation / prediction" = "identical to `Active-phase adaptation/exploration` at CC1 (Stage 14 if_else definition)"
)

raw_long <- raw_scores %>%
  select(AnimalNum, Group, Sex, n_bins, all_of(names(raw_formulas))) %>%
  pivot_longer(all_of(names(raw_formulas)), names_to = "Domain", values_to = "DomainScore") %>%
  mutate(
    displayed = Domain != "Early adaptation / prediction",
    status = if_else(Domain == "Early adaptation / prediction",
                     "excluded_duplicate_of_active_phase_adaptation_exploration", "primary_displayed"),
    source_table = BASE5,
    source_script = THIS_SCRIPT,
    upstream_definition_script = "Analysis/14_systems_neuroscience_summary_dashboard.R (first_active window ~L959-986; domain formulas ~L5330-5400 / ~L5535-5570)",
    bin_resolution = "5min_based",
    cage_change = first_cc,
    phase_window = RAW_PHASE_WINDOW,
    aggregation_level = "animal (one row per AnimalNum; bins are never treated as independent observations)",
    feature_origin = "raw_RFID",
    standardization = "z within Sex only (no PhaseClass x CageChangeIndex context remains in a single-epoch analysis)",
    formula_as_implemented = unname(raw_formulas[Domain]),
    n_bins_in_window = n_bins,
    window_hours = n_bins * BIN_SEC_5 / 3600
  ) %>%
  select(-n_bins)

hmm_domain_defs <- tribble(
  ~col, ~displayed, ~status, ~formula_as_implemented,
  "Latent-state occupancy organization", TRUE, "primary_displayed",
  "0.5 * z(occupancy_entropy) - z(inactive_state_fraction). occupancy_entropy = -sum p_s log p_s over the 4 common-state-space states within block 1; inactive_state_fraction = summed occupancy of states labelled 'inactive/low-exploration' by annotate_hmm_semantic_states(). Coefficient 0.5 PRESERVED from the shipped composite because z(social_state_fraction) is identically 0. Occupancy is order-invariant, so this is NOT temporal flexibility.",
  "Latent-state persistence", TRUE, "primary_displayed",
  "z(mean_dwell_minutes); mean_dwell_minutes = occupancy-weighted mean Viterbi run length within block 1 * bin_size_sec / 60. HIGHER = LONGER state runs = MORE persistent / fewer switches. One metric only: switch rate, self-transition probability and dwell are the same PC1 dimension.",
  "Low-activity high-co-occupancy state occupancy", FALSE, "supplementary_pending_partition_robustness",
  "z(top_proximity_state_fraction); occupancy of the argmax-Proximity_z state, derived programmatically from hmm_state_summary.csv (never hard-coded). Named as a low-activity high-co-occupancy state, not a social state.",
  "Latent-state occupancy organization (unweighted-entropy sensitivity)", FALSE,
  "separate_sensitivity_construct_not_the_shipped_score",
  "z(occupancy_entropy) - z(inactive_state_fraction); a post-hoc reweighting of the shipped score, reported only as an explicitly labelled sensitivity construct."
)

hmm_long <- map_dfr(names(hmm_first_block), function(res) {
  d <- hmm_scores %>% filter(resolution == res)
  g <- hmm_block_geom_all %>% filter(resolution == res)
  med_h <- median(g$block1_hours)
  d %>% select(AnimalNum, Group, Sex, n_bins, window_hours, all_of(hmm_domain_defs$col)) %>%
    pivot_longer(all_of(hmm_domain_defs$col), names_to = "col", values_to = "DomainScore") %>%
    left_join(hmm_domain_defs, by = "col") %>%
    mutate(
      Domain = col,
      displayed = displayed & res == "10min_based",
      status = if (res == "10min_based") status else paste0(status, "__5min_resolution_sensitivity"),
      source_table = file.path(HMM, res, "tables/hmm_state_assignments.csv"),
      source_script = THIS_SCRIPT,
      upstream_definition_script = "Analysis/08_hmm_behavioral_states_optional.R (group-blind longitudinal Viterbi labels; NO CC1-only refit); Functions/hmm_stage14_helpers.R (semantics + composite form)",
      bin_resolution = res,
      cage_change = "CC1",
      phase_window = sprintf("CC1 Active, FIRST CONTIGUOUS DARK BLOCK (block 1 of 4); median %.2f h", med_h),
      aggregation_level = "animal (one row per AnimalNum; bins, states and transitions are never treated as independent observations)",
      feature_origin = "HMM_derived",
      standardization = "z within Sex only (single-epoch analysis)",
      n_bins_in_window = n_bins
    ) %>%
    select(-col, -n_bins)
})

domain_scores <- bind_rows(raw_long, hmm_long) %>%
  mutate(Group = factor(as.character(Group), levels = GROUP_LEVELS),
         Sex = factor(as.character(Sex), levels = SEX_LEVELS)) %>%
  arrange(Domain, bin_resolution, AnimalNum) %>%
  select(AnimalNum, Group, Sex, Domain, DomainScore, displayed, status,
         source_table, source_script, upstream_definition_script, bin_resolution,
         cage_change, phase_window, aggregation_level, feature_origin,
         standardization, formula_as_implemented, n_bins_in_window, window_hours)

write_csv(domain_scores, file.path(OUT, "first_night_domain_scores.csv"))
cat("first_night_domain_scores.csv rows:", nrow(domain_scores), "\n")
sec("Rows per Domain x resolution")
print(domain_scores %>% count(Domain, bin_resolution, displayed, status) %>%
        mutate(Domain = str_trunc(Domain, 56), status = str_trunc(status, 46)) %>% as.data.frame())

sec("DISPLAYED first-night domain set")
disp <- domain_scores %>% filter(displayed) %>% distinct(Domain, bin_resolution, feature_origin)
print(as.data.frame(disp)); cat("n displayed domains:", nrow(disp), "\n")

hr("STEP 6. n per Group x Sex on the first-night set")

sec("Finite DomainScore counts per displayed domain")
print(domain_scores %>% filter(displayed) %>% group_by(Domain, bin_resolution) %>%
        summarise(n_animals = n_distinct(AnimalNum[is.finite(DomainScore)]), .groups = "drop") %>%
        mutate(Domain = str_trunc(Domain, 56)) %>% as.data.frame())

gs <- bind_rows(
  roster %>% count(Group, Sex, name = "n") %>% mutate(set = "canonical_roster"),
  raw_feat %>% count(Group, Sex, name = "n") %>% mutate(set = "raw_first_12h_5min"),
  hmm_first_block[["10min_based"]] %>% count(Group, Sex, name = "n") %>% mutate(set = "hmm_block1_10min"),
  hmm_first_block[["5min_based"]] %>% count(Group, Sex, name = "n") %>% mutate(set = "hmm_block1_5min")
) %>%
  mutate(Group = as.character(Group), Sex = as.character(Sex)) %>%
  pivot_wider(names_from = set, values_from = n, values_fill = 0L) %>%
  arrange(Sex, Group)
sec("n per Group x Sex across feature sets")
print(as.data.frame(gs))
cat("column totals:\n")
print(gs %>% summarise(across(where(is.numeric), sum)) %>% as.data.frame())
write_csv(gs, file.path(OUT, "first_night_group_sex_n.csv"))

sec("Per-Sex n for the displayed domains (FDR family sizes)")
fam <- domain_scores %>% filter(displayed) %>%
  group_by(Sex) %>%
  summarise(n_displayed_domains = n_distinct(Domain),
            n_animals = n_distinct(AnimalNum), .groups = "drop") %>%
  mutate(n_tests_in_family = n_displayed_domains * 3,
         family_id = paste0("FIRST_NIGHT__", Sex, "__displayed_domains_x_3_contrasts"))
print(as.data.frame(fam))
write_csv(fam, file.path(OUT, "first_night_fdr_family_definition.csv"))

hr("STEP 7. Redundancy audit")

cand <- domain_scores %>%
  mutate(label = if_else(feature_origin == "HMM_derived",
                         paste0(Domain, " [", bin_resolution, "]"), Domain)) %>%
  select(AnimalNum, Group, Sex, label, DomainScore) %>%
  distinct() %>%
  pivot_wider(names_from = label, values_from = DomainScore)

CAND_LABELS <- setdiff(names(cand), c("AnimalNum", "Group", "Sex"))
cat("candidate score columns (", length(CAND_LABELS), "):\n", sep = "")
cat(paste0("  - ", CAND_LABELS, collapse = "\n"), "\n")

classify <- function(r) {
  a <- abs(r)
  case_when(is.na(a) ~ NA_character_, a > 0.999 ~ "mathematically_identical",
            a > 0.95 ~ "near_deterministic", a > 0.80 ~ "strongly_redundant",
            a > 0.50 ~ "moderately_related", TRUE ~ "largely_independent")
}

cmb <- combn(CAND_LABELS, 2)
pairs_tbl <- tibble(domain_a = cmb[1, ], domain_b = cmb[2, ])

red_audit <- map_dfr(c("Pooled", SEX_LEVELS), function(scope) {
  d <- if (scope == "Pooled") cand else cand %>% filter(as.character(Sex) == scope)
  pmap_dfr(pairs_tbl, function(domain_a, domain_b) {
    x <- d[[domain_a]]; y <- d[[domain_b]]
    ok <- is.finite(x) & is.finite(y)
    usable <- sum(ok) >= 4 && sd(x[ok]) > 0 && sd(y[ok]) > 0
    tibble(scope = scope, domain_a = domain_a, domain_b = domain_b, n = sum(ok),
           pearson_r = if (usable) cor(x[ok], y[ok]) else NA_real_,
           spearman_rho = if (usable) suppressWarnings(cor(x[ok], y[ok], method = "spearman")) else NA_real_)
  })
}) %>%
  mutate(redundancy_class_pearson = classify(pearson_r),
         redundancy_class_spearman = classify(spearman_rho),
         max_abs_cor = pmax(abs(pearson_r), abs(spearman_rho), na.rm = TRUE),
         should_not_both_be_displayed = !is.na(max_abs_cor) & max_abs_cor > 0.95,
         display_decision = case_when(
           domain_a == "Early adaptation / prediction" | domain_b == "Early adaptation / prediction" ~
             "Early adaptation / prediction DROPPED: mathematically identical to Active-phase adaptation/exploration at CC1",
           str_detect(domain_a, "unweighted-entropy sensitivity") | str_detect(domain_b, "unweighted-entropy sensitivity") ~
             "unweighted-entropy variant DROPPED from display: separate sensitivity construct (post-hoc reweighting)",
           str_detect(domain_a, "high-co-occupancy") | str_detect(domain_b, "high-co-occupancy") ~
             "top-proximity occupancy NOT displayed: supplementary_pending_partition_robustness",
           str_detect(domain_a, "\\[5min_based\\]") | str_detect(domain_b, "\\[5min_based\\]") ~
             "5min HMM copy is a resolution sensitivity, not an additional displayed row",
           !is.na(max_abs_cor) & max_abs_cor > 0.95 ~ "REVIEW: two DISPLAYED rows are near-deterministic",
           TRUE ~ "both may be displayed"
         ))

sec("Pairwise redundancy, POOLED, sorted by |r|")
print(red_audit %>% filter(scope == "Pooled") %>% arrange(desc(abs(pearson_r))) %>%
        transmute(a = str_trunc(domain_a, 44), b = str_trunc(domain_b, 44), n,
                  r = round(pearson_r, 3), rho = round(spearman_rho, 3),
                  cls = redundancy_class_pearson) %>% as.data.frame())

sec("Pairs flagged should_not_both_be_displayed (any scope)")
flagged <- red_audit %>% filter(should_not_both_be_displayed)
if (nrow(flagged) == 0) cat("none\n") else
  print(flagged %>% transmute(scope, a = str_trunc(domain_a, 40), b = str_trunc(domain_b, 40),
                              r = round(pearson_r, 4), rho = round(spearman_rho, 4),
                              cls = redundancy_class_pearson,
                              decision = str_trunc(display_decision, 60)) %>% as.data.frame())

DISPLAYED_LABELS <- domain_scores %>% filter(displayed) %>%
  mutate(label = if_else(feature_origin == "HMM_derived",
                         paste0(Domain, " [", bin_resolution, "]"), Domain)) %>%
  distinct(label) %>% pull(label)
sec("DISPLAYED-ONLY pairwise redundancy (the 7 rows that actually go on the heatmap), POOLED")
print(red_audit %>%
        filter(scope == "Pooled", domain_a %in% DISPLAYED_LABELS, domain_b %in% DISPLAYED_LABELS) %>%
        arrange(desc(abs(pearson_r))) %>%
        transmute(a = str_trunc(domain_a, 40), b = str_trunc(domain_b, 40), n,
                  r = round(pearson_r, 3), rho = round(spearman_rho, 3),
                  cls = redundancy_class_pearson) %>% as.data.frame())
cat("max |r| among DISPLAYED pairs (pooled):",
    round(max(abs(red_audit$pearson_r[red_audit$scope == "Pooled" &
                                        red_audit$domain_a %in% DISPLAYED_LABELS &
                                        red_audit$domain_b %in% DISPLAYED_LABELS]), na.rm = TRUE), 4), "\n")
sec("DISPLAYED-ONLY pairwise redundancy, WITHIN SEX (only |r| > 0.5 shown)")
print(red_audit %>%
        filter(scope != "Pooled", domain_a %in% DISPLAYED_LABELS, domain_b %in% DISPLAYED_LABELS,
               abs(pearson_r) > 0.5) %>%
        arrange(scope, desc(abs(pearson_r))) %>%
        transmute(scope, a = str_trunc(domain_a, 38), b = str_trunc(domain_b, 38), n,
                  r = round(pearson_r, 3), rho = round(spearman_rho, 3),
                  cls = redundancy_class_pearson) %>% as.data.frame())

## Locomotion dominance: correlation of every candidate with Psychomotor activation (= Movement_mean_z)
loco <- map_dfr(c("Pooled", SEX_LEVELS), function(scope) {
  d <- if (scope == "Pooled") cand else cand %>% filter(as.character(Sex) == scope)
  map_dfr(setdiff(CAND_LABELS, "Psychomotor activation"), function(lb) {
    x <- d[["Psychomotor activation"]]; y <- d[[lb]]
    tibble(scope = scope, Domain = lb, n = sum(is.finite(x) & is.finite(y)),
           spearman_rho_with_psychomotor = safe_cor(x, y, "spearman"),
           spearman_p = safe_cor_p(x, y, "spearman"),
           pearson_r_with_psychomotor = safe_cor(x, y, "pearson"))
  })
}) %>%
  group_by(scope) %>% mutate(spearman_fdr = p.adjust(spearman_p, method = "BH")) %>% ungroup() %>%
  mutate(locomotion_dominance_flag = abs(spearman_rho_with_psychomotor) >= 0.70,
         dominance_threshold = "repo convention |rho| >= 0.70")

sec("Locomotion dominance vs Psychomotor activation (Movement_mean_z), POOLED")
print(loco %>% filter(scope == "Pooled") %>% arrange(desc(abs(spearman_rho_with_psychomotor))) %>%
        transmute(Domain = str_trunc(Domain, 52), n,
                  rho = round(spearman_rho_with_psychomotor, 3),
                  q = signif(spearman_fdr, 3), flag = locomotion_dominance_flag) %>% as.data.frame())
sec("Locomotion dominance, WITHIN SEX")
print(loco %>% filter(scope != "Pooled") %>%
        transmute(scope, Domain = str_trunc(Domain, 46), n,
                  rho = round(spearman_rho_with_psychomotor, 3),
                  flag = locomotion_dominance_flag) %>% as.data.frame())

red_out <- bind_rows(
  red_audit %>% mutate(row_type = "pairwise_domain_correlation"),
  loco %>% transmute(row_type = "locomotion_dominance", scope,
                     domain_a = "Psychomotor activation", domain_b = Domain, n,
                     pearson_r = pearson_r_with_psychomotor,
                     spearman_rho = spearman_rho_with_psychomotor,
                     redundancy_class_pearson = classify(pearson_r_with_psychomotor),
                     redundancy_class_spearman = classify(spearman_rho_with_psychomotor),
                     max_abs_cor = pmax(abs(pearson_r_with_psychomotor),
                                        abs(spearman_rho_with_psychomotor), na.rm = TRUE),
                     spearman_p, spearman_fdr,
                     should_not_both_be_displayed = abs(spearman_rho_with_psychomotor) >= 0.70,
                     display_decision = if_else(abs(spearman_rho_with_psychomotor) >= 0.70,
                       "LOCOMOTION-DOMINATED (repo |rho| >= 0.70): must be interpreted alongside Psychomotor activation",
                       "not locomotion-dominated at the repo |rho| >= 0.70 threshold"))
) %>%
  relocate(row_type, scope, domain_a, domain_b)
write_csv(red_out, file.path(OUT, "first_night_domain_redundancy_audit.csv"))
write_csv(loco, file.path(OUT, "first_night_locomotion_dominance.csv"))

hr("STEP 8. first_night_domain_provenance.csv")

raw_prov <- tibble(
  Domain = names(raw_formulas),
  source_table = BASE5,
  source_script = THIS_SCRIPT,
  upstream_definition_script = "Analysis/14_systems_neuroscience_summary_dashboard.R (first_active window ~L959-986; domain formulas ~L5330-5400 / ~L5535-5570)",
  bin_resolution = "5min_based",
  cage_change = first_cc,
  phase_window = RAW_PHASE_WINDOW,
  n_bins_median = raw_med_bins,
  window_hours_median = raw_med_hours,
  aggregation_level = "animal",
  feature_origin = "raw_RFID",
  standardization = "z within Sex only",
  formula_as_implemented = unname(raw_formulas[Domain]),
  window_matches_raw_first_night = TRUE,
  window_note = sprintf(paste0("This IS the raw first-night reference window. It is a fixed 144-bin cut, NOT block-aware: ",
                               "%d/%d animals (%.1f%%) borrow >= 1 bin from night 2 (max %d bins = %.2f h; ",
                               "mean %.2f bins). Median first CONTIGUOUS block is %d bins = %.2f h."),
                        sum(impurity$window_impure), nrow(impurity), 100 * mean(impurity$window_impure),
                        max(impurity$n_bins_borrowed_from_later_blocks),
                        max(impurity$n_bins_borrowed_from_later_blocks) * BIN_SEC_5 / 3600,
                        mean(impurity$n_bins_borrowed_from_later_blocks),
                        median(raw_block_geom$n_bins_block1), median(raw_block_geom$block1_hours)),
  displayed = Domain != "Early adaptation / prediction",
  status = if_else(Domain == "Early adaptation / prediction",
                   "excluded_duplicate_of_active_phase_adaptation_exploration", "primary_displayed")
)

hmm_prov <- map_dfr(names(hmm_first_block), function(res) {
  g <- hmm_block_geom_all %>% filter(resolution == res)
  hmm_domain_defs %>% transmute(
    Domain = col,
    source_table = file.path(HMM, res, "tables/hmm_state_assignments.csv"),
    source_script = THIS_SCRIPT,
    upstream_definition_script = "Analysis/08_hmm_behavioral_states_optional.R (group-blind longitudinal Viterbi labels; NO CC1-only refit); Functions/hmm_stage14_helpers.R",
    bin_resolution = res,
    cage_change = "CC1",
    phase_window = sprintf("CC1 Active, first contiguous dark block (block 1 of %d), median %.2f h",
                           as.integer(median(g$n_blocks)), median(g$block1_hours)),
    n_bins_median = median(g$n_bins_block1),
    window_hours_median = median(g$block1_hours),
    aggregation_level = "animal",
    feature_origin = "HMM_derived",
    standardization = "z within Sex only",
    formula_as_implemented,
    window_matches_raw_first_night = FALSE,
    window_note = sprintf(paste0("PROVENANCE MISMATCH vs the raw first-night domains. This window is the FIRST CONTIGUOUS ",
                                 "BLOCK (median %d bins = %.2f h). The raw domains instead use a fixed local_bin <= 144 cut ",
                                 "at 5-min bins (median %.2f h). Stage 08 as shipped reports the ENTIRE CC1 Active epoch = ",
                                 "median %d bins = %.2f h spread over %d nights separated by ~%.2f h light phases, which is ",
                                 "NOT a first-night feature; these HMM features were therefore RECOMPUTED here restricted to ",
                                 "block 1, from the SAME common (group-blind, longitudinally fitted) state space. Within one ",
                                 "contiguous block there are NO temporal gaps, so the gap-crossing problem that affects the ",
                                 "longitudinal HMM metrics cannot arise for these first-night features."),
                          as.integer(median(g$n_bins_block1)), median(g$block1_hours), raw_med_hours,
                          as.integer(median(g$n_bins_total)), median(g$active_hours_total),
                          as.integer(median(g$n_blocks)), median(g$gap_hours_median, na.rm = TRUE)),
    displayed = displayed & res == "10min_based",
    status = if (res == "10min_based") status else paste0(status, "__5min_resolution_sensitivity")
  )
})

provenance <- bind_rows(raw_prov, hmm_prov) %>%
  mutate(inference_unit_note = "One value per animal per domain; bins, states and transitions are never independent observations.",
         phenotype_label_note = "RES/SUS are LATER phenotype labels derived from subsequent CombZ outcome data; first-night contrasts are DESCRIPTIVE associations with later phenotype, never prospective or causal.",
         proximity_note = "RFID proximity is a social-spatial CO-LOCATION proxy, not sociability; a low-movement high-proximity state is a low-activity high-co-occupancy state.",
         production_relationship = "Candidate replacement/extension for tables/sis_CC1_first_active_domain_heatmap_data.csv and its figure panel. This script EDITS NOTHING in production.")
write_csv(provenance, file.path(OUT, "first_night_domain_provenance.csv"))
sec("Provenance: window comparison (the raw-vs-HMM difference)")
print(provenance %>% transmute(Domain = str_trunc(Domain, 44), bin_resolution,
                               n_bins_median, window_h = round(window_hours_median, 2),
                               feature_origin, window_match = window_matches_raw_first_night,
                               displayed) %>% as.data.frame())

hr("STEP 9. Reconciliation with the EXISTING production CC1 artifacts (read-only)")

recon <- list()

p_early <- file.path(STAGE14, "tables/systems_sis_first_active_12h_domain_scores.csv")
if (file.exists(p_early)) {
  prod_early <- read_csv(p_early, col_types = cols(AnimalNum = col_character(), .default = col_guess()),
                         progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum)) %>%
    select(AnimalNum, Domain, prod_score = EarlyDomainScore) %>% distinct()
  cmp <- raw_long %>% select(AnimalNum, Domain, DomainScore) %>%
    inner_join(prod_early, by = c("AnimalNum", "Domain"))
  r1 <- cmp %>% group_by(Domain) %>%
    summarise(n = sum(is.finite(DomainScore) & is.finite(prod_score)),
              max_abs_diff = max(abs(DomainScore - prod_score), na.rm = TRUE),
              pearson_r = safe_cor(DomainScore, prod_score, "pearson"),
              spearman_rho = safe_cor(DomainScore, prod_score, "spearman"), .groups = "drop") %>%
    mutate(production_table = "tables/systems_sis_first_active_12h_domain_scores.csv",
           production_construction = "Stage 14 first_active 12 h window, z within Sex only, same formulas",
           relationship = if_else(max_abs_diff < 1e-8, "EXACT reproduction of production",
                                  "DISAGREEMENT with production; inspect"))
  sec("A) vs systems_sis_first_active_12h_domain_scores.csv (same contract -> expect exact)")
  print(as.data.frame(r1 %>% transmute(Domain = str_trunc(Domain, 44), n,
                                       max_abs_diff = signif(max_abs_diff, 3),
                                       pearson_r = round(pearson_r, 6), relationship)))
  recon[["early12h"]] <- r1
}

p_cc1 <- file.path(STAGE14, "tables/sis_CC1_first_active_domain_heatmap_data.csv")
if (file.exists(p_cc1)) {
  prod_cc1 <- read_csv(p_cc1, col_types = cols(AnimalNum = col_character(), .default = col_guess()),
                       progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum)) %>%
    pivot_longer(starts_with("sis_CC1_first_active_score__"),
                 names_to = "ScoreColumn", values_to = "prod_score") %>%
    mutate(match_domain = recode(ScoreColumn,
      "sis_CC1_first_active_score__psychomotor_activation" = "Psychomotor activation",
      "sis_CC1_first_active_score__behavioral_volatility_fragmentation" = "Behavioral volatility / fragmentation",
      "sis_CC1_first_active_score__behavioral_state_architecture" = "Behavioral state architecture",
      "sis_CC1_first_active_score__social_spatial_organization" = "Social spatial organization",
      "sis_CC1_first_active_score__behavioral_flexibility_predictability" = "Behavioral flexibility / predictability",
      "sis_CC1_first_active_score__active_phase_adaptation_exploration" = "Active-phase adaptation/exploration")) %>%
    select(AnimalNum, match_domain, prod_score)
  cat("production CC1 panel animals:", n_distinct(prod_cc1$AnimalNum), "\n")

  ours <- domain_scores %>%
    mutate(match_domain = case_when(
      Domain == "Latent-state occupancy organization" ~ "Behavioral state architecture",
      feature_origin == "raw_RFID" & Domain != "Early adaptation / prediction" ~ Domain,
      TRUE ~ NA_character_)) %>%
    filter(!is.na(match_domain)) %>%
    select(AnimalNum, Domain, match_domain, bin_resolution, DomainScore)
  cmp2 <- ours %>% inner_join(prod_cc1, by = c("AnimalNum", "match_domain"))
  r2 <- cmp2 %>% group_by(our_domain = Domain, production_domain = match_domain, bin_resolution) %>%
    summarise(n = sum(is.finite(DomainScore) & is.finite(prod_score)),
              max_abs_diff = max(abs(DomainScore - prod_score), na.rm = TRUE),
              pearson_r = safe_cor(DomainScore, prod_score, "pearson"),
              spearman_rho = safe_cor(DomainScore, prod_score, "spearman"), .groups = "drop") %>%
    mutate(production_table = "tables/sis_CC1_first_active_domain_heatmap_data.csv",
           production_construction = "direction-weighted rowMeans of GLOBALLY safe_scale()d CC1 features (NOT z-within-Sex); its HMM feature comes from hmm_state_occupancy.csv over the ENTIRE CC1 Active epoch (~48 h / 4 nights)",
           production_inference = "Welch two-sample t-test + Wilcoxon sensitivity, BH within Sex (Stage 14 ~L5843)",
           our_inference = "DomainScore ~ Group * Sex (plain lm, one row per animal) + emmeans planned contrasts RES-CON/SUS-CON/SUS-RES, adjust='none'; BH within Sex over displayed domains x 3 contrasts",
           relationship = "candidate REPLACEMENT/EXTENSION of the production panel; production files are NOT edited by this script")
  sec("B) vs sis_CC1_first_active_domain_heatmap_data.csv (different construction)")
  print(as.data.frame(r2 %>% transmute(our_domain = str_trunc(our_domain, 40),
                                       production_domain = str_trunc(production_domain, 36),
                                       bin_resolution, n,
                                       max_abs_diff = round(max_abs_diff, 3),
                                       r = round(pearson_r, 4), rho = round(spearman_rho, 4))))
  recon[["cc1_panel"]] <- r2
}

recon_out <- bind_rows(
  if (!is.null(recon$early12h)) recon$early12h %>% mutate(comparison = "A_stage14_first_active_12h_domain_scores") else NULL,
  if (!is.null(recon$cc1_panel)) recon$cc1_panel %>% rename(Domain = our_domain) %>%
    mutate(comparison = "B_stage14_CC1_first_active_panel") else NULL
) %>% relocate(comparison)
write_csv(recon_out, file.path(OUT, "first_night_reconciliation_with_production.csv"))

hr("STEP 10. Exploratory continuous association with CombZ (descriptive only, NOT prediction)")

combz <- read_excel(COMBZ, sheet = "zScore") %>%
  transmute(AnimalNum = canonical_animal_id(ID), CombZ = safe_numeric(CombZ)) %>%
  filter(!is.na(AnimalNum)) %>% group_by(AnimalNum) %>%
  summarise(CombZ = mean(CombZ, na.rm = TRUE), .groups = "drop")
cat("CombZ animals:", nrow(combz), " matched to canonical roster:",
    sum(combz$AnimalNum %in% roster$AnimalNum), "\n")

combz_assoc <- map_dfr(c("Pooled", SEX_LEVELS), function(scope) {
  d <- cand %>% left_join(combz, by = "AnimalNum")
  if (scope != "Pooled") d <- d %>% filter(as.character(Sex) == scope)
  map_dfr(CAND_LABELS, function(lb) {
    tibble(scope = scope, Domain = lb, n = sum(is.finite(d[[lb]]) & is.finite(d$CombZ)),
           spearman_rho = safe_cor(d[[lb]], d$CombZ, "spearman"),
           spearman_p = safe_cor_p(d[[lb]], d$CombZ, "spearman"))
  })
}) %>% group_by(scope) %>% mutate(spearman_fdr = p.adjust(spearman_p, method = "BH")) %>% ungroup() %>%
  mutate(interpretation = "EXPLORATORY DESCRIPTIVE association only. RES/SUS were derived FROM CombZ, so this is not independent prediction; there is no cross-validation here. Stage 09 is the prediction analysis and Movement_mean carries most of its cross-validated signal.")
sec("CombZ association, POOLED (exploratory, descriptive)")
print(combz_assoc %>% filter(scope == "Pooled") %>% arrange(spearman_p) %>%
        transmute(Domain = str_trunc(Domain, 52), n, rho = round(spearman_rho, 3),
                  p = signif(spearman_p, 3), q = signif(spearman_fdr, 3)) %>% as.data.frame())
write_csv(combz_assoc, file.path(OUT, "first_night_combz_exploratory_association.csv"))

hr("STEP 11. Documented deviation: volatility domain vs the epoch-level Stage 14 score")

if (file.exists(SLEEP5)) {
  sl <- read_csv(SLEEP5, col_types = cols(AnimalNum = col_character(), .default = col_guess()),
                 progress = FALSE) %>%
    mutate(AnimalNum = canonical_animal_id(AnimalNum), CageChange = as.character(CageChange),
           PhaseClass = if ("PhaseClass" %in% names(.)) as.character(PhaseClass) else as.character(Phase)) %>%
    filter(CageChange == first_cc, str_detect(str_to_lower(PhaseClass), PH_ACT))
  cat("sleep-like epoch rows for CC1 Active:", nrow(sl),
      "(epoch-level over the FULL ~48 h CC1 Active epoch, NOT restrictable to the first 12 h)\n")
  cat("-> the two sleep-like terms in Stage 14's EPOCH volatility score (inactivity_fragmentation_z,\n",
      "   active_inactive_transition_rate_z) are OMITTED from the first-night volatility domain,\n",
      "   exactly as Stage 14's own first_active score omits them.\n")
  write_csv(tibble(
    Domain = "Behavioral volatility / fragmentation",
    epoch_level_stage14_formula = "mean(Movement_rmssd_z, Entropy_rmssd_z, Proximity_rmssd_z, inactivity_fragmentation_z, active_inactive_transition_rate_z)",
    first_night_formula_used = "mean(Movement_rmssd_z, Entropy_rmssd_z, Proximity_rmssd_z)",
    reason = "sleep-like inactivity features exist only as epoch-level (~48 h CC1 Active) summaries and cannot be restricted to the first 12 h without re-implementing Stage 16; Stage 14's own first_active score also uses only the 3 rmssd terms",
    n_sleep_epoch_rows_available_cc1_active = nrow(sl)
  ), file.path(OUT, "first_night_volatility_formula_deviation.csv"))
}

hr("DONE. Files written")
print(tibble(file = sort(list.files(OUT, pattern = "\\.csv$"))) %>% as.data.frame())
cat("\nOUT =", OUT, "\n")
