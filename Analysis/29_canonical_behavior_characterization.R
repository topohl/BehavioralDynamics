# ================================================================
# Stage 29 - Canonical behavioural characterisation (Exp9 SIS RFID)
# MMMSociability
# ================================================================
# The single canonical inferential path for the home-cage RFID characterisation.
# Every choice is fixed by the frozen configuration (Functions/behavior_analysis_config.R);
# this script refuses to run unless the current configuration hashes to a frozen
# version under analysis_ready/canonical/behavior_config/ and the canonical code is committed.
#
#   0. freeze gate                      4. prespecified robustness (heteroscedastic A/B, LOBO,
#   1. bin-free metrics + runtime gates    shared-zone D1-D4 + complete case, S2/S3/S7/S7b/S11/S12/S13/S18)
#   2. primary / secondary / follow-up  5. diagnostics (zone variance, influence, homogeneity, allFit)
#   3. multiplicity (applied once)      6. secondary estimation (exposure, continuous, light phase, lag, cumulative)
#                                       7. Stage 09 declared sensitivities
# Outputs: analysis_ready/pipeline/29_canonical_behavior/{tables,audit}/. Stage 16b bundles them.
# Manual stage (not in run_all_analysis.R).
#
# DRY RUN (code testing only): set MMM_STAGE29_DRY_RUN_DIR; RES/SUS labels are permuted among SIS
# animals within Batch, CombZ is permuted, the freeze gate is skipped and output goes to that folder.
# ================================================================

suppressPackageStartupMessages({ library(data.table); library(jsonlite); library(digest) })
.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Run from the MMMSociability repo root.", call. = FALSE)
source(.pipeline_setup)
for (h in c("project_paths.R", "behavior_analysis_config.R", "rfid_event_stream.R", "rfid_binfree_metrics.R", "rfid_canonical_inference.R"))
  source_mmm_helper(h)
for (p in c("lmerTest", "pbkrtest", "glmmTMB", "clubSandwich", "sandwich", "Matrix", "reformulas"))
  if (!requireNamespace(p, quietly = TRUE)) stop("Missing package ", p, call. = FALSE)

CFG <- MMM_BEHAVIOR_CONFIG; M <- CFG$models; ALPHA <- CFG$meta$alpha
ROOT <- mmm_project_root(); AR <- file.path(ROOT, "analysis_ready")
RFID_SRC <- file.path(ROOT, "MMMSociability")
PRE <- file.path(RFID_SRC, "preprocessed_data"); RAW <- file.path(RFID_SRC, "raw_data")
if (!dir.exists(PRE)) stop("Missing ", PRE, call. = FALSE)
DRY <- nzchar(Sys.getenv("MMM_STAGE29_DRY_RUN_DIR"))
OUT <- if (DRY) Sys.getenv("MMM_STAGE29_DRY_RUN_DIR") else behavior_stage_dir(ROOT, "29", "canonical_behavior")
TAB <- file.path(OUT, "tables"); AUD <- file.path(OUT, "audit")
dir.create(TAB, recursive = TRUE, showWarnings = FALSE); dir.create(AUD, recursive = TRUE, showWarnings = FALSE)
wr <- function(x, name) data.table::fwrite(x, file.path(TAB, name))

# ---------------------------------------------------------------- 0. freeze gate
cfg_sha <- mmm_behavior_config_sha256(CFG)
frozen_dir <- file.path(AR, "canonical", "behavior_config", paste0("v", CFG$meta$config_version))
frozen_sha <- if (file.exists(file.path(frozen_dir, "config_sha256.txt"))) readLines(file.path(frozen_dir, "config_sha256.txt"))[1] else NA
if (!DRY && !identical(cfg_sha, frozen_sha))
  stop("Configuration is not frozen or differs from frozen v", CFG$meta$config_version, " (", frozen_sha, " vs ", cfg_sha, ").", call. = FALSE)
git <- function(...) system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = TRUE, stderr = TRUE)
code_files <- c("Analysis/29_canonical_behavior_characterization.R", "Functions/behavior_analysis_config.R", "Functions/rfid_event_stream.R",
                "Functions/rfid_binfree_metrics.R", "Functions/rfid_canonical_inference.R")
dirty <- git(c("status", "--porcelain", "--", code_files))
if (!DRY && length(dirty)) stop("Canonical code has uncommitted changes:\n", paste(dirty, collapse = "\n"), call. = FALSE)
RUN <- list(config_version = CFG$meta$config_version, config_sha256 = cfg_sha, git_commit = git("rev-parse", "HEAD")[1],
            dry_run = DRY, started_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"))
message(if (DRY) "Stage 29 DRY RUN (not frozen-gated): config " else "Stage 29: frozen config v", CFG$meta$config_version, " ", cfg_sha,
        " at commit ", RUN$git_commit)

# ---------------------------------------------------------------- 1. metrics and runtime gates
crit <- CFG$metrics$fragmentation$bout_criterion_s
st <- mmm_evs_build_stream(PRE, RAW)
aw <- mmm_evs_window_stream(st, "active"); lw <- mmm_evs_window_stream(st, "light")
prim <- mmm_bf_window_table(aw, crit)
file_end <- st$pos[, .(file_last = max(DateTime)), by = SourceFile]
wcov <- merge(st$windows, file_end, by = "SourceFile")
wcov[, light_complete := file_last >= light_end]
light <- mmm_bf_window_table(lw, crit)$animals[, .(AnimalNum, CC, SourceFile, light_phase_crossing_rate = crossing_rate, light_obs_s = obs_s)]
light <- merge(light, wcov[, .(SourceFile, light_complete)], by = "SourceFile")
light[light_complete == FALSE, light_phase_crossing_rate := NA_real_]
light <- light[, .(AnimalNum, CC, light_phase_crossing_rate, light_complete)]
rev <- mmm_bf_window_table(aw, crit, reversal = TRUE)$animals
alt_frag <- rbindlist(lapply(names(CFG$sensitivities$OTHER$S11_bout_criterion), function(k)
  mmm_bf_animal_metrics(aw$runs, CFG$sensitivities$OTHER$S11_bout_criterion[[k]])[, .(AnimalNum, CC, criterion = k, fragmentation)]))
cum <- rbindlist(lapply(CFG$windows$cumulative$hours, function(h) {
  x <- mmm_bf_window_table(aw, crit, clip_h = h)$animals; x[, window_h := h]; x }), fill = TRUE)

# gate 1: event counts equal Stage 01 10-min Movement sums outside the re-seeded windows
s01 <- data.table::fread(file.path(mmm_derived_metrics_output_root(ROOT), "10min_based", "all_behavior_metrics.csv"),
                         select = c("AnimalNum", "CageChange", "BinStart", "Movement", "SourceFile"))
s01[, AnimalNum := canonical_animal_id(AnimalNum)]
s01 <- merge(s01, st$windows[, .(SourceFile, ws = active_start, we = active_end)], by = "SourceFile")[BinStart >= ws & BinStart < we]
s01sum <- s01[, .(mov01 = sum(Movement)), by = .(AnimalNum, CC = paste0("CC", sub("^CC", "", CageChange)))]
old_seeds <- mmm_evs_seeds(mmm_evs_read_preprocessed(PRE), RAW, parser = MMM_EVS_STAGE01_PARSER)
key_cc <- function(s) unique(s[, .(AnimalNum, CC = paste0("CC", sub("^CC", "", as.character(CageChange))))])
new_only <- fsetdiff(key_cc(st$seeds), key_cc(old_seeds))
chk <- merge(prim$animals[, .(AnimalNum, CC, n_events)], s01sum, by = c("AnimalNum", "CC"), all = TRUE)
chk[, reseeded := paste(AnimalNum, CC) %in% paste(new_only$AnimalNum, new_only$CC)]
wr(chk, "validation_events_vs_stage01.csv")
if (nrow(new_only) != 8) stop("Expected 8 newly seeded animal-windows, found ", nrow(new_only), call. = FALSE)
if (nrow(chk[reseeded == FALSE & (is.na(n_events) | is.na(mov01) | n_events != mov01)])) stop("Event-stream gate failed.", call. = FALSE)
if (nrow(prim$animals) != 444) stop("Expected 444 active windows.", call. = FALSE)
# gate 2: anchors equal the Stage 28 provenance for 24/24 SourceFiles
prov <- data.table::fread(file.path(AR, "pipeline", "28_rfid_behavioral_domains", "10min", "tables", "acute_window_provenance_by_animal_cagechange.csv"),
                          select = c("SourceFile", "target_window_start"))
prov <- unique(prov); prov[, target_window_start := as.POSIXct(target_window_start, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")]
anc <- merge(st$windows[, .(SourceFile, active_start)], prov, by = "SourceFile")
wr(anc, "validation_anchor_vs_stage28.csv")
if (nrow(anc) != 24 || any(anc$active_start != anc$target_window_start)) stop("Anchor gate failed.", call. = FALSE)
if (!all(wcov$light_complete)) warning("Some light-phase windows are incomplete and set to NA (see audit).")
wr(wcov, "validation_window_coverage.csv")

# design table (Group from the canonical CombZ table)
cz <- data.table::fread(file.path(AR, "canonical", "later_outcome_combz", "tables", "later_outcome_combz_animal_level.csv"),
                        colClasses = c(AnimalNum = "character"))
cz[, AnimalNum := canonical_animal_id(AnimalNum)]
des <- merge(prim$animals, cz[, .(AnimalNum, Group = outcome_group, Sex, Batch_cz = paste0("B", Batch), CombZ)], by = "AnimalNum")
if (nrow(des) != 444) stop("Design join lost windows.", call. = FALSE)
des[, Batch := sub("^.*(B[0-9]).*$", "\\1", SourceFile)]
if (any(des$Batch != des$Batch_cz)) stop("Batch disagreement between stream and CombZ table.", call. = FALSE)
des[, Batch_cz := NULL]
if (DRY) {
  set.seed(1); an <- unique(des[Group != "CON", .(AnimalNum, Batch, Group)])
  an[, Group_perm := sample(Group), by = Batch]
  des <- merge(des, an[, .(AnimalNum, Group_perm)], by = "AnimalNum", all.x = TRUE)
  des[!is.na(Group_perm), Group := Group_perm][, Group_perm := NULL]
  czp <- unique(des[, .(AnimalNum, CombZ)]); czp[, CombZ := sample(CombZ)]
  des <- merge(des[, !"CombZ"], czp, by = "AnimalNum")
  message("DRY RUN: RES/SUS labels and CombZ permuted; outputs are not results.")
}
des[, CageEpisodeID := paste(Batch, System, CC, sep = "|")]
des[, n_in_cage := .N, by = CageEpisodeID]
des <- merge(des, light, by = c("AnimalNum", "CC"), all.x = TRUE)
des <- mmm_ci_code_design(des)
ec <- CFG$population$primary$expected_by_sex_group
cnt <- des[CC == "CC1" & Group != "CON", .N, by = .(Sex, Group)]
for (sx in names(ec)) for (g in names(ec[[sx]])) if (cnt[Sex == sx & Group == g, N] != ec[[sx]][[g]]) stop("Population count gate failed.", call. = FALSE)
wr(des, "canonical_window_metrics.csv"); wr(prim$dyads, "canonical_dyads.csv")

CON_SET <- c("crossing_rate", "shared_zone_use", "occupancy_dispersion", "fragmentation")
tier <- c(crossing_rate = "primary", shared_zone_use = "primary", occupancy_dispersion = "secondary", fragmentation = "secondary")
sisd <- des[Group != "CON"]
ex <- CFG$population$expected_counts
for (k in CON_SET) { e <- if (k == "shared_zone_use") ex$shared_zone_use else ex$other_constructs
  if (sisd[CC == "CC1" & is.finite(get(k)), .N] != e$CC1_animals || sisd[is.finite(get(k)), .N] != e$windows) stop("Count gate failed for ", k, call. = FALSE) }

fits <- list(); est <- list(); jt <- list(); reg <- list()
keep <- function(m) { reg[[m$info$model_id]] <<- m$info; fits[[m$info$model_id]] <<- m; m }
fsub <- function(f) gsub("AnimalID", "AnimalNum", f)
trf <- function(k, which = c("pooled", "sex")) { which <- match.arg(which); mm <- if (which == "pooled") M$TR_POOLED else M$TR_BY_SEX
  fsub(if (k == "crossing_rate") mm$formula_crossing_rate else mm$formula) }
dat <- function(k, cc1 = TRUE, d = sisd) { x <- d[is.finite(get(k))]; if (cc1) x <- x[CC == "CC1"]; x <- data.table::copy(x); x[, y := get(k)]; x }
batch_avg <- function(fit) { nm <- names(lme4::fixef(fit)); bd <- grep("^Batch", nm, value = TRUE)
  if (!length(bd)) return(list()); as.list(stats::setNames(rep(1 / (length(bd) + 1), length(bd)), bd)) }
cw <- function(...) { x <- list(...); x[lengths(x) > 0] }
tag_rows <- function(x, ...) { a <- list(...); x[, (names(a)) := a]; x }
addE <- function(m, w, estimand, ...) est[[length(est) + 1]] <<- tag_rows(mmm_ci_contrast(m, w, estimand), ...)

# ---------------------------------------------------------------- 2. primary, secondary and follow-up models
for (k in CON_SET) {
  m1 <- keep(mmm_ci_fit(fsub(M$CC1_POOLED$formula), dat(k), paste0("CC1_POOLED|", k), expected_rank = M$CC1_POOLED$expected_rank))
  addE(m1, CFG$contrasts$Q1$L, "Q1", construct = k); addE(m1, CFG$contrasts$RS_sexavg_CC1$L, "RS_sexavg_CC1", construct = k)
  mt <- keep(mmm_ci_fit(trf(k), dat(k, FALSE), paste0("TR_POOLED|", k), expected_rank = M$TR_POOLED$expected_rank))
  jt[[length(jt) + 1]] <- mmm_ci_joint(mt, MMM_CI_Q2B_ROWS, "Q2b")[, construct := k]
  addE(mt, CFG$contrasts$Q2c$L, "Q2c", construct = k)
  addE(mt, list(`g_RS:sex_c` = 1), "DiD_CC1", construct = k); addE(mt, list(g_RS = 1), "RS_sexavg_TR_CC1", construct = k)
  for (cc in 2:4) {
    addE(mt, stats::setNames(list(1, 1), c("g_RS:sex_c", paste0("g_RS:c", cc, ":sex_c"))), paste0("DiD_CC", cc), construct = k)
    addE(mt, stats::setNames(list(1), paste0("g_RS:c", cc, ":sex_c")), paste0("Q2b_component_c", cc), construct = k)
    addE(mt, stats::setNames(list(1), paste0("g_RS:c", cc)), paste0("Q2a_component_c", cc), construct = k)
    addE(mt, stats::setNames(list(1, 1), c("g_RS", paste0("g_RS:c", cc))), paste0("RS_sexavg_TR_CC", cc), construct = k)
  }
  for (sx in c("Female", "Male")) {
    ms <- keep(mmm_ci_fit(fsub(M$CC1_BY_SEX$formula), dat(k)[Sex == sx], paste0("CC1_BY_SEX|", k, "|", sx), expected_rank = M$CC1_BY_SEX$expected_rank))
    addE(ms, CFG$contrasts$RS_by_sex_CC1$L, "RS_by_sex_CC1", construct = k, sex = sx)
    ba <- batch_avg(ms$fit)
    for (gg in c("RES", "SUS")) addE(ms, cw(`(Intercept)` = 1, ba, g_RS = if (gg == "RES") 0.5 else -0.5) |> unlist(recursive = FALSE),
                                     paste0("mean_", gg, "_CC1"), construct = k, sex = sx)
    mts <- keep(mmm_ci_fit(trf(k, "sex"), dat(k, FALSE)[Sex == sx], paste0("TR_BY_SEX|", k, "|", sx), expected_rank = M$TR_BY_SEX$expected_rank))
    jt[[length(jt) + 1]] <- mmm_ci_joint(mts, MMM_CI_Q2A_ROWS, "TR_by_sex")[, `:=`(construct = k, sex = sx)]
    ba <- batch_avg(mts$fit)
    for (cc in 1:4) {
      rsw <- if (cc == 1) list(g_RS = 1) else stats::setNames(list(1, 1), c("g_RS", paste0("g_RS:c", cc)))
      addE(mts, rsw, paste0("RS_by_sex_TR_CC", cc), construct = k, sex = sx)
      for (gg in c("RES", "SUS")) { gv <- if (gg == "RES") 0.5 else -0.5
        w <- c(list(`(Intercept)` = 1), ba, if (cc > 1) stats::setNames(list(1), paste0("c", cc)), list(g_RS = gv),
               if (cc > 1) stats::setNames(list(gv), paste0("g_RS:c", cc)))
        addE(mts, w, paste0("mean_", gg, "_TR_CC", cc), construct = k, sex = sx) }
    }
  }
}
EST <- rbindlist(est, fill = TRUE); JT <- rbindlist(jt, fill = TRUE)
sdz <- sapply(CON_SET, function(k) CFG$metrics[[k]]$standardizer_sd)
EST[, estimate_standardized := ifelse(grepl("^mean_", estimand), NA_real_, estimate / sdz[construct])]   # differences only
EST[, ci_excludes_null_unadjusted := ci_low > 0 | ci_high < 0]
EST[, tested := estimand %in% c("Q1", "RS_by_sex_CC1")]
EST[tested == FALSE, p_raw := NA_real_]            # p-value policy: estimation-only estimands carry no p
EST[, role := data.table::fcase(estimand == "Q1", tier[construct], estimand == "RS_by_sex_CC1", "follow-up", default = "estimation")]

# ---------------------------------------------------------------- 3. multiplicity (applied once)
fam <- CFG$multiplicity$families
member_p <- function(tag) { sp <- strsplit(tag, "|", fixed = TRUE)[[1]]
  switch(sp[1], Q1 = EST[estimand == "Q1" & construct == sp[2], p_raw], Q2b = JT[estimand == "Q2b" & construct == sp[2], p_raw],
         RS_by_sex_CC1 = EST[estimand == "RS_by_sex_CC1" & construct == sp[2] & sex == sp[3], p_raw],
         TR_by_sex = JT[estimand == "TR_by_sex" & construct == sp[2] & sex == sp[3], p_raw], stop("unknown member ", tag)) }
MULT <- rbindlist(lapply(setdiff(names(fam), c("PR1", "PR2", "PR3")), function(f) {
  mem <- as.character(fam[[f]]$members); p <- vapply(mem, member_p, numeric(1))
  data.table(family_id = f, tier = fam[[f]]$tier, member = mem, method = fam[[f]]$method, declared_m = fam[[f]]$m, realised_m = length(p),
             p_raw = unname(p), p_adjusted = mmm_ci_holm(unname(p))) }))
if (any(MULT$declared_m != MULT$realised_m)) stop("A family's realised size differs from its declaration.", call. = FALSE)
MULT[, status := ifelse(is.na(p_raw), "FAILED", "OK")]   # fitting$failure_rule: the family keeps its declared m
MULT[, reject_at_alpha_adjusted := p_adjusted <= ALPHA]
fam_of <- function(k, q) if (q == "Q1") { if (tier[k] == "primary") "P-CC1" else "S-CC1-ORG" } else { if (tier[k] == "primary") "P-TR" else "S-TR-ORG" }
holm_member <- function(fid, member, p_member, p_partner = NULL) { f <- MULT[family_id == fid]; p <- f$p_raw
  i <- which(f$member == member); p[i] <- p_member; if (!is.null(p_partner)) p[-i] <- p_partner; (mmm_ci_holm(p) <= ALPHA)[i] }

# ---------------------------------------------------------------- 4. robustness
sens <- list(); het <- list(); zone <- list(); lobo <- list()
prim_row <- function(k, e) EST[construct == k & estimand == e & is.na(sex)]
add_sens <- function(id, k, e, x) { p <- prim_row(k, e); if (!nrow(p) || is.null(x)) return(invisible())
  sens[[length(sens) + 1]] <<- data.table(sensitivity_id = id, construct = k, estimand = e, primary_estimate = p$estimate, primary_se = p$se,
    estimate = x$estimate, se = x$se, ci_low = x$ci_low, ci_high = x$ci_high, shift_se = (x$estimate - p$estimate) / p$se,
    robustness_label = if (identical(x$status, "FAILED")) "FAILED" else mmm_ci_robust_label(p$estimate, p$se, x$estimate), status = x$status %||% "OK",
    model_id = x$model_id %||% NA_character_) }
refit <- function(k, d1, dL, tag, f1 = fsub(M$CC1_POOLED$formula), fL = trf(k), r1 = NULL, rL = NULL) {
  out <- list()
  if (!is.null(d1)) { m <- mmm_ci_fit(f1, d1, paste0(tag, "|CC1|", k), expected_rank = r1); reg[[m$info$model_id]] <<- m$info
    out$Q1 <- mmm_ci_contrast(m, CFG$contrasts$Q1$L, "Q1") }
  if (!is.null(dL)) { m <- mmm_ci_fit(fL, dL, paste0(tag, "|TR|", k), expected_rank = rL); reg[[m$info$model_id]] <<- m$info
    for (cc in 2:4) out[[paste0("c", cc)]] <- mmm_ci_contrast(m, stats::setNames(list(1), paste0("g_RS:c", cc, ":sex_c")), paste0("Q2b_component_c", cc))
    out$Q2b <- mmm_ci_joint(m, MMM_CI_Q2B_ROWS, "Q2b"); out$model <- m }
  out }
sens_all <- function(id, k, r) { if (!is.null(r$Q1)) add_sens(id, k, "Q1", r$Q1)
  for (cc in 2:4) if (!is.null(r[[paste0("c", cc)]])) add_sens(id, k, paste0("Q2b_component_c", cc), r[[paste0("c", cc)]]) }

# 4a. heteroscedastic comparators A and B for Q1 and Q2b of all four constructs
HA <- list(); HB <- list()
for (k in CON_SET) {
  rf <- EST[estimand == "RS_by_sex_CC1" & construct == k & sex == "Female"]; rm_ <- EST[estimand == "RS_by_sex_CC1" & construct == k & sex == "Male"]
  HA[[k]]$Q1 <- mmm_ci_het_A_q1(rf, rm_)
  gB <- mmm_ci_fit(fsub(M$CC1_POOLED$formula), dat(k), paste0("HET_B|CC1|", k), engine = "glmmTMB", dispformula = "~ sex_c", expected_rank = M$CC1_POOLED$expected_rank); reg[[gB$info$model_id]] <- gB$info
  HB[[k]]$Q1 <- mmm_ci_glmmtmb_tests(gB, one = CFG$contrasts$Q1$L)$one
  nu <- min(JT[estimand == "TR_by_sex" & construct == k, df2])
  HA[[k]]$Q2b <- mmm_ci_het_A_q2b(fits[[paste0("TR_BY_SEX|", k, "|Female")]], fits[[paste0("TR_BY_SEX|", k, "|Male")]], nu)
  tf <- gsub("\\(1 \\+ cc1 \\|\\| AnimalNum\\)", "diag(1 + cc1 | AnimalNum)", trf(k))
  gB2 <- mmm_ci_fit(tf, dat(k, FALSE), paste0("HET_B|TR|", k), engine = "glmmTMB", dispformula = "~ sex_c", expected_rank = M$TR_POOLED$expected_rank); reg[[gB2$info$model_id]] <- gB2$info
  HB[[k]]$Q2b <- mmm_ci_glmmtmb_tests(gB2, rows = MMM_CI_Q2B_ROWS)
}
for (k in CON_SET) for (cmp in c("A", "B")) {
  H <- if (cmp == "A") HA else HB; partner <- setdiff(CON_SET[tier == tier[k]], k)
  # Q1
  f1 <- fam_of(k, "Q1"); pr <- prim_row(k, "Q1"); x <- H[[k]]$Q1
  rej_prim <- MULT[family_id == f1 & member == paste0("Q1|", k), reject_at_alpha_adjusted]
  rej_alt <- holm_member(f1, paste0("Q1|", k), x$p_raw, H[[partner]]$Q1$p_raw)
  reasons <- c(if (!identical(rej_prim, rej_alt)) "Holm decision differs",
               if (!identical(pr$ci_excludes_null_unadjusted, x$ci_low > 0 | x$ci_high < 0)) "CI-excludes-0 status differs",
               if (mmm_ci_shift_material(pr$estimate, pr$se, x$estimate)) "estimate shift")
  het[[length(het) + 1]] <- cbind(data.table(construct = k, estimand = "Q1", comparator = cmp, primary_estimate = pr$estimate, primary_se = pr$se,
    primary_holm_reject = rej_prim, comparator_holm_reject = rej_alt), x, data.table(material = length(reasons) > 0, reasons = paste(reasons, collapse = "; ")))
  # Q2b
  f2 <- fam_of(k, "Q2b"); pj <- JT[estimand == "Q2b" & construct == k]; xj <- H[[k]]$Q2b$joint
  rej_prim <- MULT[family_id == f2 & member == paste0("Q2b|", k), reject_at_alpha_adjusted]
  rej_alt <- holm_member(f2, paste0("Q2b|", k), xj$p_raw, H[[partner]]$Q2b$joint$p_raw)
  comp <- H[[k]]$Q2b$components
  shift_any <- any(vapply(2:4, function(cc) { pc <- prim_row(k, paste0("Q2b_component_c", cc))
    mmm_ci_shift_material(pc$estimate, pc$se, comp$estimate[cc - 1]) }, logical(1)))
  reasons <- c(if (!identical(rej_prim, rej_alt)) "Holm decision differs", if ((pj$p_raw < ALPHA) != (xj$p_raw < ALPHA)) "unadjusted p < alpha status differs",
               if (shift_any) "component shift")
  het[[length(het) + 1]] <- cbind(data.table(construct = k, estimand = "Q2b", comparator = cmp, primary_F = pj$F, primary_p = pj$p_raw,
    primary_holm_reject = rej_prim, comparator_holm_reject = rej_alt), xj, data.table(material = length(reasons) > 0, reasons = paste(reasons, collapse = "; "),
    components = paste(sprintf("%s=%.4g(%.4g)", comp$component, comp$estimate, comp$se), collapse = "; ")))
}

# 4b. LOBO, other prespecified sensitivities
sexcc <- list()
hw6 <- c("314|CC4", "318|CC4", "OR620|CC4", "OR630|CC4", "OR112|CC3", "OR141|CC3")
hw_found <- des[paste(AnimalNum, CC, sep = "|") %in% hw6]
if (nrow(hw_found) != 6) stop("S13: expected the 6 frozen hardware windows, found ", nrow(hw_found), call. = FALSE)
for (k in CON_SET) {
  for (b in levels(sisd$Batch)) {
    r <- refit(k, dat(k)[Batch != b], dat(k, FALSE)[Batch != b], paste0("LOBO_", b), r1 = 7, rL = 19)
    lobo[[length(lobo) + 1]] <- data.table(construct = k, dropped_batch = b, estimand = c("Q1", paste0("Q2b_component_c", 2:4)), sex = NA_character_,
      estimate = c(r$Q1$estimate, r$c2$estimate, r$c3$estimate, r$c4$estimate), se = c(r$Q1$se, r$c2$se, r$c3$se, r$c4$se))
    for (sx in c("Female", "Male")) { dd <- dat(k)[Batch != b & Sex == sx]; if (data.table::uniqueN(dd$Batch) == 3) next
      ms <- mmm_ci_fit(fsub(M$CC1_BY_SEX$formula), dd, paste0("LOBO_", b, "|CC1_BY_SEX|", k, "|", sx), expected_rank = 3); reg[[ms$info$model_id]] <- ms$info
      x <- mmm_ci_contrast(ms, CFG$contrasts$RS_by_sex_CC1$L, "RS_by_sex_CC1")
      lobo[[length(lobo) + 1]] <- data.table(construct = k, dropped_batch = b, estimand = "RS_by_sex_CC1", sex = sx, estimate = x$estimate, se = x$se) }
  }
  if (k == "crossing_rate") {
    sens_all("S7_crossing_ri_only", k, refit(k, NULL, dat(k, FALSE), "S7", fL = fsub(M$TR_POOLED$formula), rL = 20))
    sens_all("S7b_crossing_correlated_slope", k, refit(k, NULL, dat(k, FALSE), "S7b", fL = sub("||", "|", trf(k), fixed = TRUE), rL = 20))
  }
  s2f <- sub("Batch + g_RS + c2 + c3 + c4 + g_RS:(c2 + c3 + c4)", "Batch * (c2 + c3 + c4) + g_RS * (c2 + c3 + c4)", trf(k), fixed = TRUE)
  s2f <- sub(" + (c2 + c3 + c4):sex_c", "", s2f, fixed = TRUE)
  sens_all("S2_batch_x_cc", k, refit(k, NULL, dat(k, FALSE), "S2", fL = s2f, rL = 32))
  r3 <- refit(k, NULL, dat(k, FALSE), "S3", fL = paste(trf(k), "+ (1 | Session)"), rL = 20); sens_all("S3_session_re", k, r3)
  for (cc in 2:4) sexcc[[length(sexcc) + 1]] <- mmm_ci_contrast(r3$model, stats::setNames(list(1), paste0("c", cc, ":sex_c")), paste0("Sex_by_CC_c", cc))[
    , `:=`(construct = k, role = "estimation (S3)", p_raw = NA_real_, tested = FALSE)]
  rv <- merge(sisd[, !c(k), with = FALSE], rev[, c("AnimalNum", "CC", k), with = FALSE], by = c("AnimalNum", "CC"))
  sens_all("S12_reversal_filtered", k, refit(k, dat(k, TRUE, rv), dat(k, FALSE, rv), "S12", r1 = 8, rL = 20))
  if (k %in% c("occupancy_dispersion", "shared_zone_use")) { hw <- sisd[!(paste(AnimalNum, CC, sep = "|") %in% hw6)]
    sens_all("S13_hardware_excluded", k, refit(k, dat(k, TRUE, hw), dat(k, FALSE, hw), "S13", r1 = 8, rL = 20)) }
  if (k == "fragmentation") for (cn in unique(alt_frag$criterion)) {
    af <- merge(sisd[, !"fragmentation"], alt_frag[criterion == cn, .(AnimalNum, CC, fragmentation)], by = c("AnimalNum", "CC"))
    sens_all(paste0("S11_bout_criterion_", cn), k, refit(k, dat(k, TRUE, af), dat(k, FALSE, af), paste0("S11_", cn), r1 = 8, rL = 20)) }
}

# 4c. shared-zone D1-D4, complete case, S18
k <- "shared_zone_use"; pz <- prim_row(k, "Q1")
D1c <- mmm_ci_cr2(fits[[paste0("CC1_POOLED|", k)]], "g_RS:sex_c")
d1_rej <- holm_member("P-CC1", "Q1|shared_zone_use", D1c$p_raw)
zone[[length(zone) + 1]] <- cbind(data.table(analysis = "D1_CR2_CC1", estimand = "Q1"), D1c,
  data.table(d1_supports = d1_rej && sign(D1c$estimate) == sign(pz$estimate)))
fixed_tr <- sub(" + (1 | AnimalNum) + (1 | CageEpisodeID)", "", fsub(M$TR_POOLED$formula), fixed = TRUE)
D1L <- mmm_ci_twoway_q2b(fixed_tr, dat(k, FALSE))
zone[[length(zone) + 1]] <- cbind(data.table(analysis = "D1_twoway_TR", estimand = "Q2b"), D1L,
  data.table(d1_supports = holm_member("P-TR", "Q2b|shared_zone_use", D1L$p_raw)))
gg <- unique(des[, .(AnimalNum, Group, Sex, CC, CageEpisodeID, n_in_cage)])
dy <- merge(data.table::copy(prim$dyads), gg[, .(A = AnimalNum, CC, GA = Group, Sex, CageEpisodeID, n_in_cage)], by = c("A", "CC"))
dy <- merge(dy, gg[, .(B = AnimalNum, CC, GB = Group)], by = c("B", "CC"))
dy <- dy[GA != "CON" & GB != "CON" & obs_s > 0]
dy[, `:=`(frac = same_s / obs_s, s_RS = ifelse(GA == "RES", 0.5, -0.5) + ifelse(GB == "RES", 0.5, -0.5),
          sex_c = ifelse(Sex == "Female", 0.5, -0.5), cc = as.integer(sub("CC", "", CC)))]
dy[, `:=`(c2 = as.numeric(cc == 2), c3 = as.numeric(cc == 3), c4 = as.numeric(cc == 4))]
wr(dy, "d2_dyads.csv")
scale_fac <- sisd[is.finite(shared_zone_use), .(f = mean((n_in_cage - 2) / (n_in_cage - 1))), by = CC]
d2a <- mmm_ci_d2(dy[CC == "CC1"]); d2b <- mmm_ci_d2(dy, longitudinal = TRUE)
if (d2a$n_cages[1] != CFG$population$expected_counts$shared_zone_use$CC1_cage_clusters$pooled) stop("D2: unexpected CC1 cage count.", call. = FALSE)
assess <- function(beta, beta_se, dfv, fac, pe) { implied <- beta * fac
  lo <- 2 * (beta - stats::qt(.975, dfv) * beta_se); hi <- 2 * (beta + stats::qt(.975, dfv) * beta_se)
  list(implied_animal_level = implied, same_direction = sign(implied) == sign(pe$estimate),
       broadly_compatible = (implied >= pe$ci_low & implied <= pe$ci_high) | (implied / pe$estimate >= 0.5 & implied / pe$estimate <= 2),
       clear_contradiction = (lo > 0 | hi < 0) & sign(beta) != sign(pe$estimate)) }
x <- d2a[estimand == "DiD_RR_vs_SS_CC1"]; a <- assess(x$beta, x$beta_se, x$df, scale_fac[CC == "CC1", f], pz)
zone[[length(zone) + 1]] <- cbind(data.table(analysis = "D2_dyadic_CC1"), d2a, data.table(primary_compared = c(NA, "Q1")),
  rbind(as.data.table(lapply(a, function(v) NA)), as.data.table(a)))
comp_rows <- d2b[grepl("component", estimand)]
for (cc in 2:4) { x <- comp_rows[estimand == paste0("Q2b_dyad_component_c", cc)]; pc <- prim_row(k, paste0("Q2b_component_c", cc))
  a <- assess(x$beta, x$beta_se, x$df, scale_fac[CC == paste0("CC", cc), f], pc)
  zone[[length(zone) + 1]] <- cbind(data.table(analysis = "D2_dyadic_TR", primary_compared = paste0("Q2b_component_c", cc)), x, as.data.table(a)) }
zone[[length(zone) + 1]] <- cbind(data.table(analysis = "D2_dyadic_TR"), d2b[estimand == "Q2b_dyad_joint"])
fD3 <- sub("(1 | CageEpisodeID)", "(0 + d1 | CageEpisodeID) + (0 + d2 | CageEpisodeID) + (0 + d3 | CageEpisodeID) + (0 + d4 | CageEpisodeID)",
           fsub(M$TR_POOLED$formula), fixed = TRUE)
r <- refit(k, NULL, dat(k, FALSE), "D3", fL = fD3, rL = 20); sens_all("D3_cc_specific_cage_variance", k, r)
zone[[length(zone) + 1]] <- cbind(data.table(analysis = "D3_cc_specific_cage", estimand = "Q2b"), r$Q2b)
four <- sisd[n_in_cage == 4]; sens_all("D4a_four_tracked_cages", k, refit(k, dat(k, TRUE, four), dat(k, FALSE, four), "D4a", r1 = 8, rL = 20))
sens_all("D4b_tracked_mate_factor", k, refit(k, dat(k), dat(k, FALSE), "D4b",
  f1 = sub("+ (1 | CageEpisodeID)", "+ factor(n_tracked_mates) + (1 | CageEpisodeID)", fsub(M$CC1_POOLED$formula), fixed = TRUE),
  fL = sub("+ (1 | AnimalNum)", "+ factor(n_tracked_mates) + (1 | AnimalNum)", fsub(M$TR_POOLED$formula), fixed = TRUE), r1 = 9, rL = 22))
miss <- unique(sisd[!is.finite(shared_zone_use), AnimalNum]); ccd <- sisd[!(AnimalNum %in% miss)]
if (!setequal(miss, c("OQ755", "OQ770", "OQ771"))) stop("Complete case: missing-window animals differ from the frozen set: ", paste(miss, collapse = ", "), call. = FALSE)
sens_all("complete_case_zone", k, refit(k, dat(k, TRUE, ccd), dat(k, FALSE, ccd), "complete_case", r1 = 8, rL = 20))
sens_all("S18_B1_excluded", k, refit(k, dat(k, TRUE, sisd[Batch != "B1"]), dat(k, FALSE, sisd[Batch != "B1"]), "S18", r1 = 7, rL = 19))

# ---------------------------------------------------------------- 5. diagnostics (reported; zone_variance is the only trigger)
diag <- list()
capture <- function(expr) { msgs <- character()
  val <- withCallingHandlers(expr, warning = function(w) { msgs <<- c(msgs, conditionMessage(w)); invokeRestart("muffleWarning") },
                             message = function(m) { msgs <<- c(msgs, trimws(conditionMessage(m))); invokeRestart("muffleMessage") })
  list(value = val, messages = paste(unique(msgs), collapse = " | ")) }
zv <- rbindlist(lapply(c("CC1", "TR"), function(a) { x <- if (a == "CC1") dat("shared_zone_use") else dat("shared_zone_use", FALSE)
  f <- if (a == "CC1") "y ~ Batch + (1 | CageEpisodeID)" else "y ~ Batch + c2 + c3 + c4 + (c2 + c3 + c4):sex_c + (1 | AnimalNum) + (1 | CageEpisodeID)"
  g0 <- capture(glmmTMB::glmmTMB(stats::as.formula(f), data = x, REML = FALSE))
  g1 <- capture(glmmTMB::glmmTMB(stats::as.formula(f), dispformula = ~ Sex, data = x, REML = FALSE))
  lr <- 2 * as.numeric(stats::logLik(g1$value) - stats::logLik(g0$value))
  data.table(diagnostic = "zone_variance", analysis = a, n = nrow(x), LR = lr, p = stats::pchisq(lr, 1, lower.tail = FALSE),
             dBIC = stats::BIC(g1$value) - stats::BIC(g0$value), messages = paste(g0$messages, g1$messages)) }))
zv[, triggered := p < 0.05 & dBIC < 0]; diag$zone <- zv
diag$pooled_vs_strat <- rbindlist(lapply(CON_SET, function(k) { m1 <- fits[[paste0("CC1_POOLED|", k)]]
  rbindlist(lapply(c(Female = 0.5, Male = -0.5), function(s) { x <- mmm_ci_contrast(m1, list(g_RS = 1, `g_RS:sex_c` = s), "pooled_RS")
    sx <- if (s > 0) "Female" else "Male"; st_ <- EST[estimand == "RS_by_sex_CC1" & construct == k & sex == sx]
    data.table(diagnostic = "pooled_vs_stratified_diff_sd", construct = k, sex = sx, pooled = x$estimate, stratified = st_$estimate,
               diff_sd = (x$estimate - st_$estimate) / sdz[[k]], flag = abs((x$estimate - st_$estimate) / sdz[[k]]) > 0.10) })) }))
diag$drop_cage <- rbindlist(lapply(CON_SET, function(k) { d1 <- dat(k); cages <- unique(d1$CageEpisodeID)
  v <- vapply(cages, function(cg) mmm_ci_contrast(mmm_ci_fit(fsub(M$CC1_POOLED$formula), d1[CageEpisodeID != cg], "drop_cage"), CFG$contrasts$Q1$L, "Q1")$estimate, 0)
  data.table(diagnostic = "drop_one_cage_Q1", construct = k, n_refits = length(v), min = min(v), max = max(v), primary = prim_row(k, "Q1")$estimate) }))
diag$homog <- rbindlist(lapply(CON_SET, function(k) rbindlist(lapply(c("Female", "Male"), function(sx) {
  m <- mmm_ci_fit("y ~ Batch * g_RS + (1 | CageEpisodeID)", dat(k)[Sex == sx], "homog")
  j <- mmm_ci_joint(m, grep("^Batch.*:g_RS$|^g_RS:Batch", names(lme4::fixef(m$fit)), value = TRUE), "batch_x_g_RS")
  data.table(diagnostic = "within_sex_batch_homogeneity", construct = k, sex = sx, F = j$F, df1 = j$df1, df2 = j$df2, p = j$p_raw) }))))
diag$resid <- rbindlist(lapply(CON_SET, function(k) { m1 <- fits[[paste0("CC1_POOLED|", k)]]
  data.table(m1$data[, .(AnimalNum, Sex, Group)], resid = stats::residuals(m1$fit))[, .(diagnostic = "residual_sd", construct = k, sd = stats::sd(resid), n = .N), by = .(Sex, Group)] }))
af <- rbindlist(lapply(c(paste0("TR_POOLED|crossing_rate"), paste0("TR_BY_SEX|crossing_rate|", c("Female", "Male"))), function(id) {
  cbind(data.table(diagnostic = "optimizer_check_crossing_slope", model_id = id), mmm_ci_optimizer_check(fits[[id]])) }), fill = TRUE)
af[, bobyqa_convergence_warning := vapply(model_id, function(id) !fits[[id]]$info$converged, TRUE)]
af[, failure_rule_triggered := vapply(model_id, function(id) isTRUE(fits[[id]]$info$failed), TRUE)]
diag$allfit <- af
DIAG <- rbindlist(diag, fill = TRUE)

# ---------------------------------------------------------------- 6. secondary estimation
expo <- list(); cont <- list(); lagb <- list(); cumr <- list()
ee <- function(m, w, estimand, ...) expo[[length(expo) + 1]] <<- tag_rows(mmm_ci_contrast(m, w, estimand), ...)
for (k in c(CON_SET, "light_phase_crossing_rate")) {
  d1 <- dat(k, TRUE, des); m <- mmm_ci_fit(fsub(M$EXPOSURE_CC1$formula), d1, paste0("EXPOSURE_CC1|", k), expected_rank = M$EXPOSURE_CC1$expected_rank); reg[[m$info$model_id]] <- m$info
  ee(m, list(g_SIS = 1), "SC_sexavg_CC1", construct = k); ee(m, list(`g_SIS:sex_c` = 1), "DiD_SC_CC1", construct = k)
  for (sx in c("Female", "Male")) { ms <- mmm_ci_fit(fsub(M$EXPOSURE_CC1$formula_by_sex), d1[Sex == sx], paste0("EXPOSURE_CC1_BY_SEX|", k, "|", sx), expected_rank = M$EXPOSURE_CC1$expected_rank_by_sex)
    reg[[ms$info$model_id]] <- ms$info; ee(ms, list(g_SIS = 1), "SC_by_sex_CC1", construct = k, sex = sx) }
  at <- if (k == "crossing_rate") M$EXPOSURE_TR$crossing_rate_animal_term else "(1 | AnimalID)"
  ef <- fsub(sub("(1 | AnimalID)", at, M$EXPOSURE_TR$formula, fixed = TRUE)); efs <- fsub(sub("(1 | AnimalID)", at, M$EXPOSURE_TR$formula_by_sex, fixed = TRUE))
  mL <- mmm_ci_fit(ef, dat(k, FALSE, des), paste0("EXPOSURE_TR|", k), expected_rank = M$EXPOSURE_TR$expected_rank); reg[[mL$info$model_id]] <- mL$info
  for (cc in 1:4) { w <- if (cc == 1) list(g_SIS = 1) else stats::setNames(list(1, 1), c("g_SIS", paste0("g_SIS:c", cc)))
    wd <- if (cc == 1) list(`g_SIS:sex_c` = 1) else stats::setNames(list(1, 1), c("g_SIS:sex_c", paste0("g_SIS:c", cc, ":sex_c")))
    ee(mL, w, paste0("SC_sexavg_TR_CC", cc), construct = k); ee(mL, wd, paste0("DiD_SC_TR_CC", cc), construct = k) }
  for (sx in c("Female", "Male")) { ms <- mmm_ci_fit(efs, dat(k, FALSE, des)[Sex == sx], paste0("EXPOSURE_TR_BY_SEX|", k, "|", sx), expected_rank = M$EXPOSURE_TR$expected_rank_by_sex)
    reg[[ms$info$model_id]] <- ms$info
    for (cc in 1:4) ee(ms, if (cc == 1) list(g_SIS = 1) else stats::setNames(list(1, 1), c("g_SIS", paste0("g_SIS:c", cc))),
                       paste0("SC_by_sex_TR_CC", cc), construct = k, sex = sx) }
}
k <- "light_phase_crossing_rate"
m <- mmm_ci_fit(fsub(M$CC1_POOLED$formula), dat(k), "LIGHT_CC1_POOLED", expected_rank = M$CC1_POOLED$expected_rank); reg[[m$info$model_id]] <- m$info
ee(m, CFG$contrasts$RS_sexavg_CC1$L, "RS_sexavg_CC1_light", construct = k); ee(m, CFG$contrasts$Q1$L, "Q1_light", construct = k)
for (sx in c("Female", "Male")) {
  ms <- mmm_ci_fit(fsub(M$CC1_BY_SEX$formula), dat(k)[Sex == sx], paste0("LIGHT_CC1_BY_SEX|", sx), expected_rank = M$CC1_BY_SEX$expected_rank); reg[[ms$info$model_id]] <- ms$info
  ee(ms, CFG$contrasts$RS_by_sex_CC1$L, "RS_by_sex_CC1_light", construct = k, sex = sx)
  mts <- mmm_ci_fit(fsub(M$TR_BY_SEX$formula), dat(k, FALSE)[Sex == sx], paste0("LIGHT_TR_BY_SEX|", sx), expected_rank = M$TR_BY_SEX$expected_rank); reg[[mts$info$model_id]] <- mts$info
  for (cc in 1:4) ee(mts, if (cc == 1) list(g_RS = 1) else stats::setNames(list(1, 1), c("g_RS", paste0("g_RS:c", cc))),
                     paste0("RS_by_sex_TR_CC", cc, "_light"), construct = k, sex = sx)
}
EXPO <- rbindlist(expo, fill = TRUE)[, `:=`(p_raw = NA_real_, role = "estimation only")]

for (pop in CFG$models$CONTINUOUS$populations) for (xk in CFG$models$CONTINUOUS$predictors) {
  d <- des[CC == "CC1" & is.finite(get(xk)) & is.finite(CombZ)]; if (pop == "SIS_ONLY") d <- d[Group != "CON"]
  d <- data.table::copy(d); d[, x := get(xk)]
  m <- mmm_ci_fit(M$CONTINUOUS$formula, d, paste("CONTINUOUS", pop, xk, sep = "|"), engine = "lm", expected_rank = M$CONTINUOUS$expected_rank); reg[[m$info$model_id]] <- m$info
  V <- clubSandwich::vcovCR(m$fit, cluster = d$CageEpisodeID, type = "CR2"); ct <- summary(m$fit)$coefficients; dfm <- m$fit$df.residual
  for (cf in c("x", "x:sex_c")) { cr <- clubSandwich::coef_test(m$fit, vcov = V, test = "Satterthwaite", coefs = cf)
    cont[[length(cont) + 1]] <- data.table(population = pop, predictor = xk, estimand = ifelse(cf == "x", "slope_sexavg", "slope_DiD_F_minus_M"),
      estimate = ct[cf, 1], se = ct[cf, 2], df = dfm, ci_low = ct[cf, 1] - stats::qt(.975, dfm) * ct[cf, 2], ci_high = ct[cf, 1] + stats::qt(.975, dfm) * ct[cf, 2],
      se_cr2 = cr$SE, df_cr2 = cr$df_Satt, n = nrow(d)) }
  for (sx in c("Female", "Male")) { ms <- mmm_ci_fit(M$CONTINUOUS$formula_by_sex, d[Sex == sx], paste("CONTINUOUS", pop, xk, sx, sep = "|"), engine = "lm", expected_rank = M$CONTINUOUS$expected_rank_by_sex)
    reg[[ms$info$model_id]] <- ms$info; ct <- summary(ms$fit)$coefficients; dfs <- ms$fit$df.residual
    cont[[length(cont) + 1]] <- data.table(population = pop, predictor = xk, estimand = paste0("slope_", sx), estimate = ct["x", 1], se = ct["x", 2],
      df = dfs, ci_low = ct["x", 1] - stats::qt(.975, dfs) * ct["x", 2], ci_high = ct["x", 1] + stats::qt(.975, dfs) * ct["x", 2], n = nrow(d[Sex == sx])) }
}
CONT <- rbindlist(cont, fill = TRUE)[, `:=`(p_raw = NA_real_, role = "secondary estimand (continuous view)")]

lagdir <- file.path(AR, "pipeline", "28_rfid_behavioral_domains")
for (res in c("10min", "5min")) {
  lf <- data.table::fread(file.path(lagdir, res, "tables", "acute_window_raw_features.csv"),
                          select = c("AnimalNum", "CageChangeIndex", "Movement_rmssd", "Movement_acf1", "Proximity_rmssd", "Proximity_acf1"))
  lf[, `:=`(AnimalNum = canonical_animal_id(AnimalNum), CC = paste0("CC", CageChangeIndex))]
  ld <- merge(sisd[CC == "CC1", .(AnimalNum, CC, Sex, Batch, g_RS, sex_c, CageEpisodeID)], lf, by = c("AnimalNum", "CC"))
  for (lk in c("Movement_rmssd", "Movement_acf1", "Proximity_rmssd", "Proximity_acf1")) {
    rr <- names(which(unlist(CFG$metrics$descriptive$lag_block[[lk]]) == res))
    x <- ld[is.finite(get(lk))]; x[, y := get(lk)]
    m <- mmm_ci_fit(fsub(M$CC1_POOLED$formula), x, paste0("LAG|", lk, "|", res), expected_rank = M$CC1_POOLED$expected_rank); reg[[m$info$model_id]] <- m$info
    for (e in c("Q1", "RS_sexavg_CC1")) lagb[[length(lagb) + 1]] <- mmm_ci_contrast(m, CFG$contrasts[[e]]$L, e)[, `:=`(metric = lk, resolution = res, resolution_role = rr)]
    for (sx in c("Female", "Male")) { ms <- mmm_ci_fit(fsub(M$CC1_BY_SEX$formula), x[Sex == sx], paste0("LAG|", lk, "|", res, "|", sx), expected_rank = M$CC1_BY_SEX$expected_rank)
      reg[[ms$info$model_id]] <- ms$info
      lagb[[length(lagb) + 1]] <- mmm_ci_contrast(ms, CFG$contrasts$RS_by_sex_CC1$L, "RS_by_sex_CC1")[, `:=`(metric = lk, resolution = res, resolution_role = rr, sex = sx)] }
  }
}
LAG <- rbindlist(lagb, fill = TRUE)[, `:=`(p_raw = NA_real_, role = "descriptive")]

for (h in CFG$windows$cumulative$hours) {
  mk <- as.character(CFG$windows$cumulative$metric_matrix[[paste0("h", h)]])
  cwd <- merge(sisd[CC == "CC1", .(AnimalNum, CC, Group, Sex, Batch, CageEpisodeID, g_RS, sex_c)], cum[window_h == h & CC == "CC1"], by = c("AnimalNum", "CC"))
  for (k in mk) { x <- cwd[is.finite(get(k))]; x[, y := get(k)]
    m <- mmm_ci_fit(fsub(M$CC1_POOLED$formula), x, paste0("CUM", h, "h|", k), expected_rank = M$CC1_POOLED$expected_rank); reg[[m$info$model_id]] <- m$info
    for (e in c("Q1", "RS_sexavg_CC1")) cumr[[length(cumr) + 1]] <- mmm_ci_contrast(m, CFG$contrasts[[e]]$L, e)[, `:=`(construct = k, window_h = h)]
    for (sx in c("Female", "Male")) { ms <- mmm_ci_fit(fsub(M$CC1_BY_SEX$formula), x[Sex == sx], paste0("CUM", h, "h|", k, "|", sx), expected_rank = M$CC1_BY_SEX$expected_rank)
      reg[[ms$info$model_id]] <- ms$info
      cumr[[length(cumr) + 1]] <- mmm_ci_contrast(ms, CFG$contrasts$RS_by_sex_CC1$L, "RS_by_sex_CC1")[, `:=`(construct = k, window_h = h, sex = sx)] } }
}
CUM <- rbindlist(cumr, fill = TRUE)[, `:=`(p_raw = NA_real_, role = "estimation-only temporal localisation")]
CUM[construct %in% names(sdz), estimate_standardized := estimate / sdz[construct]]

# ---------------------------------------------------------------- 7. Stage 09 declared sensitivities
s09 <- data.table::fread(file.path(AR, "pipeline", "09_early_prediction", "10min", "tables", "model_ladder_input.csv"))
s09[, AnimalNum := canonical_animal_id(AnimalNum)]
s09 <- merge(s09, des[CC == "CC1", .(AnimalNum, Batch, CageEpisodeID, sex_c)], by = "AnimalNum")
if (DRY) s09[, outcome := sample(outcome)]
loo_r2 <- function(d, form, fold) { pred <- rep(NA_real_, nrow(d))
  for (g in unique(fold)) { tr <- fold != g; f <- stats::lm(form, data = d[tr]); pred[!tr] <- stats::predict(f, newdata = d[!tr]) }
  1 - sum((d$outcome - pred)^2) / sum((d$outcome - mean(d$outcome))^2) }
forms <- list(movement_mean = outcome ~ Movement_mean, primary_behavior_family = outcome ~ Movement_mean + Movement_rmssd + Entropy_acf1)
rng <- CFG$stage09$rng; RNGkind(rng$kind, rng$normal_kind, rng$sample_kind); set.seed(rng$seed)
S09S <- rbindlist(lapply(names(forms), function(mn) { d <- s09[stats::complete.cases(s09[, all.vars(forms[[mn]]), with = FALSE])]
  obs <- c(LOAO = loo_r2(d, forms[[mn]], d$AnimalNum), LOCO_CC1_cage = loo_r2(d, forms[[mn]], d$CageEpisodeID), LOBO = loo_r2(d, forms[[mn]], d$Batch))
  perm <- replicate(1000, { dp <- data.table::copy(d); dp[, outcome := sample(outcome), by = Batch]; loo_r2(dp, forms[[mn]], dp$AnimalNum) })
  data.table(model = mn, scheme = names(obs), r2 = obs, n = nrow(d),
             restricted_perm_p = c((sum(perm >= obs[["LOAO"]]) + 1) / (length(perm) + 1), NA, NA), perm_draws = c(1000L, NA, NA)) }))
S09X <- rbindlist(lapply(as.character(CFG$stage09$registered$features), function(fx) {
  d <- s09[is.finite(get(fx))]; d[, x := get(fx)]; f <- stats::lm(outcome ~ Batch + x + x:sex_c, data = d); ct <- summary(f)$coefficients["x:sex_c", ]
  data.table(feature = fx, estimand = "batch_adjusted_x_by_sex (F - M)", estimate = ct[1], se = ct[2], df = f$df.residual,
             ci_low = ct[1] - stats::qt(.975, f$df.residual) * ct[2], ci_high = ct[1] + stats::qt(.975, f$df.residual) * ct[2], p_raw = ct[4], n = nrow(d)) }))

# ---------------------------------------------------------------- 8. write
SEXCC <- rbindlist(sexcc, fill = TRUE)[, estimate_standardized := estimate / sdz[construct]]
ALL_EST <- rbindlist(list(EST, SEXCC), fill = TRUE)
if (anyDuplicated(ALL_EST[, paste(model_id, estimand, construct, sex)])) stop("Duplicate estimate rows.", call. = FALSE)
wr(ALL_EST, "estimates.csv")
wr(JT, "joint_tests.csv"); wr(MULT, "multiplicity.csv")
wr(rbindlist(sens, fill = TRUE), "sensitivities.csv"); wr(rbindlist(het, fill = TRUE), "heteroscedastic.csv")
wr(rbindlist(zone, fill = TRUE), "shared_zone_robustness.csv")
LB <- rbindlist(lobo, fill = TRUE); wr(LB, "lobo.csv")
ref <- EST[estimand %in% unique(LB$estimand), .(construct, estimand, sex, primary_estimate = estimate)]
LB2 <- merge(LB, ref, by = c("construct", "estimand", "sex"), all.x = TRUE)
if (anyNA(LB2$primary_estimate)) stop("LOBO rows without a primary reference.", call. = FALSE)
lsum <- LB2[, .(n_refits = .N, required = if (is.na(sex[1])) 6L else 3L, n_same_sign = sum(sign(estimate) == sign(primary_estimate)),
                leverage_batches = paste(dropped_batch[sign(estimate) != sign(primary_estimate)], collapse = "; ")), by = .(construct, estimand, sex)]
if (any(lsum$n_refits != lsum$required)) stop("LOBO refit count differs from the frozen rule.", call. = FALSE)
lsum[, robust_to_single_batch_removal := n_same_sign == required]
wr(lsum, "lobo_sign_stability.csv")
wr(EXPO, "exposure_estimates.csv"); wr(CONT, "continuous_estimates.csv"); wr(LAG, "lag_block_estimates.csv"); wr(CUM, "cumulative_window_estimates.csv")
wr(S09S, "stage09_cv_sensitivities.csv"); wr(S09X, "stage09_batch_adjusted_sex_interaction.csv"); wr(DIAG, "diagnostics.csv")
wr(rbindlist(reg, fill = TRUE), "model_registry.csv")
wr(rbindlist(lapply(c(CON_SET, "light_phase_crossing_rate"), function(k) des[, .(construct = k, n = sum(is.finite(get(k))),
  mean = mean(get(k), na.rm = TRUE), sd = stats::sd(get(k), na.rm = TRUE)), by = .(CC, Sex, Group)])), "descriptive_summaries.csv")
inputs <- c(st$input_files, file.path(AR, "canonical", "later_outcome_combz", "tables", "later_outcome_combz_animal_level.csv"),
            file.path(AR, "pipeline", "09_early_prediction", "10min", "tables", "model_ladder_input.csv"),
            file.path(lagdir, "10min", "tables", "acute_window_raw_features.csv"), file.path(lagdir, "5min", "tables", "acute_window_raw_features.csv"),
            file.path(lagdir, "10min", "tables", "acute_window_provenance_by_animal_cagechange.csv"),
            file.path(mmm_derived_metrics_output_root(ROOT), "10min_based", "all_behavior_metrics.csv"))
data.table::fwrite(data.table(input = inputs, bytes = file.size(inputs), sha256 = vapply(inputs, function(f) digest::digest(file = f, algo = "sha256"), "")),
                   file.path(AUD, "run_inputs.csv"))
pk <- c("lme4", "lmerTest", "pbkrtest", "glmmTMB", "clubSandwich", "sandwich", "data.table", "Matrix", "reformulas")
RUN$finished_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"); RUN$r_version <- R.version.string
RUN$packages <- paste(sprintf("%s %s", pk, vapply(pk, function(p) as.character(utils::packageVersion(p)), "")), collapse = "; ")
RUN$n_models <- length(reg)
data.table::fwrite(as.data.table(RUN), file.path(AUD, "run_manifest.csv"))
outs <- list.files(TAB, full.names = TRUE)
data.table::fwrite(data.table(file = basename(outs), bytes = file.size(outs), sha256 = vapply(outs, function(f) digest::digest(file = f, algo = "sha256"), "")),
                   file.path(AUD, "output_manifest.csv"))
n_failed <- sum(vapply(reg, function(i) isTRUE(i$failed), TRUE))
if (n_failed) warning(n_failed, " model(s) FAILED under fitting$failure_rule; their rows are reported FAILED (see model_registry.csv).")
message("Stage 29 complete: ", length(outs), " tables, ", length(reg), " models in ", TAB)
