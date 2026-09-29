# Contract test for the analytic-identity gate (Functions/behavior_config_identity.R).
#
# The current configuration (v1.0.1) must be analytically identical to the frozen v1.0.0, and every kind of
# analytic edit must be rejected. The frozen parent is read from analysis_ready/canonical/behavior_config/v1.0.0
# when the project root is reachable, otherwise it is rebuilt from the freeze commit a9b7a2c (git show); both
# must hash to the recorded 33d22430... Nothing is written; no behavioural data are read; no Analysis/ stage runs.
#
# Portable-suite idiom: plain Rscript from the repo root, fail()/check()/ok(), no testthat.

suppressPackageStartupMessages({ library(jsonlite); library(digest) })
source("Analysis/_pipeline_setup.R")
for (h in c("project_paths.R", "behavior_analysis_config.R", "behavior_config_identity.R")) source_mmm_helper(h)

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")

PARENT_SHA <- "33d22430b0e6d3a45a28bea8546a185c523aedf72055ddf5ae208cff51f8950e"
frozen_json <- file.path(mmm_project_root(), "analysis_ready", "canonical", "behavior_config", "v1.0.0", "behavior_analysis_config.json")
if (file.exists(frozen_json)) {
  check(identical(mmm_cfg_json_file_sha256(frozen_json), PARENT_SHA), "frozen v1.0.0 JSON file hashes to the recorded config sha")
  parent <- jsonlite::fromJSON(frozen_json, simplifyVector = FALSE); src <- "frozen JSON"
} else {
  txt <- system2("git", c("-C", shQuote(MMM_REPO_ROOT), "show", "a9b7a2c:Functions/behavior_analysis_config.R"), stdout = TRUE)
  e <- new.env(); eval(parse(text = txt), envir = e)
  check(identical(mmm_behavior_config_sha256(e$MMM_BEHAVIOR_CONFIG), PARENT_SHA), "a9b7a2c config hashes to the recorded config sha")
  parent <- mmm_cfg_tree(e$MMM_BEHAVIOR_CONFIG); src <- "git a9b7a2c"
}
ok(paste("frozen parent v1.0.0 loaded from", src))

cur <- mmm_cfg_tree(MMM_BEHAVIOR_CONFIG)
check(identical(cur$meta$config_version, "1.0.1"), "the repository config is v1.0.1")

# 0. token extraction
tk <- mmm_cfg_tokens("sandwich::vcovCL(cluster = ~ AnimalID, type = 'HC3', cadjust = TRUE); rank 101 (93 cage epochs).")
check(all(c("sandwich::vcovCL", "AnimalID", "'HC3'", "TRUE", "101", "93") %in% tk) && !any(c("rank", "cage", "epochs") %in% tk), "protected tokens")
ok("token extraction keeps numbers, identifiers, quoted values and code; drops prose")

# 1. identity of the parent with itself
r0 <- mmm_cfg_analytic_identity(parent, parent, parent_sha256 = PARENT_SHA)
check(nrow(r0$differences) == 0, "a frozen config has 0 differing leaves with itself")
ok("parent vs itself: 0 differing leaves")

# 2. the repository v1.0.1 passes
r1 <- mmm_cfg_analytic_identity(parent, cur, parent_sha256 = PARENT_SHA)
check(r1$ok, paste("v1.0.1 must pass:", paste(r1$disallowed$path, r1$disallowed$reason, collapse = "; "),
                   paste(r1$checks$check[!r1$checks$ok], collapse = "; ")))
d <- r1$differences
check(!any(d$status == "changed" & d$class %in% c("number", "logical", "type")), "no numeric/logical leaf changed")
check(!any(d$status == "removed" & d$class != "populated_empty"), "no parent leaf removed")
check(all(grepl("^/(data_versions|meta|metrics|population)/", d$path[d$status == "added"])), "added leaves only in whitelisted nodes")
ok(sprintf("v1.0.1 accepted: %d leaves differ (%s), 0 disallowed; %d/%d semantic checks", nrow(d),
           paste(names(table(d$class)), table(d$class), sep = " ", collapse = ", "), sum(r1$checks$ok), nrow(r1$checks)))

# 3. every analytic edit is rejected (each applied to the accepted v1.0.1 tree)
bad <- list(
  formula            = function(x) { x$models$TR_POOLED$formula <- sub("(1 | CageEpisodeID)", "(1 | Batch)", x$models$TR_POOLED$formula, fixed = TRUE); x },
  expected_rank      = function(x) { x$models$CC1_POOLED$expected_rank <- 9L; x },
  standardizer       = function(x) { x$metrics$shared_zone_use$standardizer_sd <- 0.0608; x },
  bout_criterion     = function(x) { x$metrics$fragmentation$bout_criterion_s <- 40; x },
  family_m           = function(x) { x$multiplicity$families$`P-TR`$m <- 3L; x },
  family_members     = function(x) { x$multiplicity$families$`P-CC1`$members <- list("Q1|crossing_rate"); x },
  contrast_L         = function(x) { x$contrasts$Q2c$L$`g_RS:c2:sex_c` <- 0.3; x },
  window             = function(x) { x$windows$cumulative$hours <- list(1L, 3L, 6L, 12L); x },
  population         = function(x) { x$population$primary$expected_n <- 86L; x },
  rng                = function(x) { x$stage09$rng$seed <- 1L; x },
  S11                = function(x) { x$sensitivities$OTHER$S11_bout_criterion$half <- 20; x },
  new_analytic_node  = function(x) { x$models$NEW <- list(formula = "y ~ 1"); x },
  # beyond the 12 required kinds
  estimator_in_text  = function(x) { x$sensitivities$SHARED_ZONE$D1$longitudinal <- gsub("'HC3'", "'HC1'", x$sensitivities$SHARED_ZONE$D1$longitudinal, fixed = TRUE); x },
  formula_in_text    = function(x) { x$sensitivities$SHARED_ZONE$D2$model_tr <- sub(" + s_RS:sex_c +", " +", x$sensitivities$SHARED_ZONE$D2$model_tr, fixed = TRUE); x },
  frozen_count       = function(x) { x$population$expected_counts$shared_zone_use$windows <- 346L; x },
  leaf_removed       = function(x) { x$sensitivities$OTHER$S18_B1_excluded <- NULL; x },
  type_change        = function(x) { x$models$CC1_POOLED$expected_rank <- "8"; x },
  term_array         = function(x) { x$fitting$no_terms[[1]] <- "Sex main effect (allowed)"; x },
  test_text          = function(x) { x$contrasts$Q1$test <- "Wald z"; x },
  new_sensitivity    = function(x) { x$sensitivities$S19 <- "drop OR646"; x },
  parent_binding     = function(x) { x$data_versions$v1_original$expected_counts$shared_zone_use$windows <- 346L; x },
  parent_cc_set      = function(x) { x$data_versions$v1_original$complete_case_missing <- list("OQ770", "OQ771"); x },
  release_undeclared = function(x) { x$data_versions$release <- "v3_unknown"; x },
  change_log_label   = function(x) { x$meta$change_log[[1]]$label <- "documentation"; x },
  pre_outcome_claim  = function(x) { x$meta$frozen_before_res_sus_outcome_models <- TRUE; x },
  parent_sha         = function(x) { x$meta$analytic_parent$config_sha256 <- paste(rep("0", 64), collapse = ""); x })
for (k in names(bad)) {
  r <- mmm_cfg_analytic_identity(parent, bad[[k]](cur), parent_sha256 = PARENT_SHA)
  why <- c(if (nrow(r$disallowed)) paste(r$disallowed$path, collapse = ", "), r$checks$check[!r$checks$ok])
  check(!r$ok && length(why) >= 1, paste("analytic edit must be rejected:", k))
  ok(sprintf("rejected %-18s -> %s", k, paste(why, collapse = "; ")))
}
cat("test_behavior_config_identity: all checks passed (", length(bad), " edits rejected, 12 required)\n")
