# Contract tests for Stage 14's upstream input resolution.
#
# WHY THIS EXISTS
#
# Stage 14 selects upstream trees with first_existing_path() over an ordered
# resolution preference. Existence is not validity: a stale, undeclared
# resolution that merely happens to be on disk silently outranked the resolution
# the producer actually declares. Two concrete defects came from that:
#
#   * adaptive_recovery preferred 5min_based, but Analysis/11 writes 10min_based
#     only. The 5min tree on disk was written 2026-05-18 - before the
#     2026-09-03 exact-phase-classifier fix (12f3e76), i.e. a tree in which
#     Inactive epochs were silently relabelled Active.
#   * latent_state preferred 10min_based, but Analysis/05 declares 5min_based,
#     and that 10min tree was three days OLDER than the declared one.
#
# These tests lock both halves of the fix: the preference head must agree with
# the producer's declaration, and a pre-fix phase-dependent tree must be
# refused loudly rather than consumed.
#
# Portable: parses source and uses tempdir() fixtures. No project root needed.

suppressPackageStartupMessages({ library(stringr) })

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")

STAGE14 <- "Analysis/14_systems_neuroscience_summary_dashboard.R"
check(file.exists(STAGE14), "Stage 14 source is missing")
s14 <- readLines(STAGE14, warn = FALSE)

# ---------------------------------------------------------------------------
cat("\n[1] each domain preference head matches its producer's declared bin level\n")
# ---------------------------------------------------------------------------

#' Read the single bin level a producer declares.
declared_bin <- function(script) {
  if (!file.exists(script)) return(NA_character_)
  x <- readLines(script, warn = FALSE)
  hit <- grep('^\\s*bin_level\\s*<-\\s*"', x, value = TRUE)
  if (length(hit) == 0L) return(NA_character_)
  sub('.*<-\\s*"([^"]+)".*', "\\1", hit[1])
}

#' Read the FIRST entry of a domain's preference vector from Stage 14 source.
preference_head <- function(domain) {
  i <- grep(paste0("^\\s*", domain, "\\s*=\\s*c\\("), s14)
  if (length(i) == 0L) return(NA_character_)
  sub('.*=\\s*c\\(\\s*"([^"]+)".*', "\\1", s14[i[1]])
}

# domain in Stage 14  ->  the producer that owns that tree
DOMAIN_PRODUCER <- c(
  adaptive_recovery     = "Analysis/11_behavioral_adaptation_kinetics.R",
  sleep_like_inactivity = "Analysis/12_sleep_like_quiescence_metrics.R",
  phase_organization    = "Analysis/13_ethological_phase_organization.R",
  latent_state          = "Analysis/05_behavioral_state_space.R",
  social_reorganization = "Analysis/06_dynamic_social_networks.R",
  temporal_flexibility  = "Analysis/04_temporal_instability.R"
)

for (dom in names(DOMAIN_PRODUCER)) {
  producer <- DOMAIN_PRODUCER[[dom]]
  want <- declared_bin(producer)
  got  <- preference_head(dom)
  check(!is.na(want), paste0("cannot read declared bin_level from ", producer))
  check(!is.na(got),  paste0("cannot read preference head for domain '", dom, "'"))
  check(identical(got, want),
        paste0("Stage 14 domain '", dom, "' prefers '", got,
               "' but its producer ", producer, " declares '", want,
               "'. Preferring a resolution the producer does not write means ",
               "reading an undeclared historical tree that cannot be regenerated."))
  ok(paste0(dom, " -> ", got, " (matches ", basename(producer), ")"))
}

# ---------------------------------------------------------------------------
cat("\n[2] the phase-classifier staleness guard exists and is wired in\n")
# ---------------------------------------------------------------------------

check(any(grepl("MMM_PHASE_CLASSIFIER_FIX_UTC", s14, fixed = TRUE)),
      "Stage 14 no longer defines the phase-classifier fix instant")
check(any(grepl("assert_not_pre_phase_fix", s14, fixed = TRUE)),
      "Stage 14 no longer defines the staleness guard")

# The guard must actually be called from the resolver, not merely defined.
fep <- grep("^first_existing_path <- function", s14)
check(length(fep) == 1L, "cannot locate first_existing_path()")
body_end <- fep + which(grepl("^\\}", s14[fep:(fep + 12)]))[1] - 1L
check(any(grepl("assert_not_pre_phase_fix", s14[fep:body_end], fixed = TRUE)),
      "first_existing_path() does not call the staleness guard; a stale tree could still win on existence alone")
ok("guard defined and called from first_existing_path()")

for (src in c("15_behavioral_adaptation_kinetics", "16_sleep_like_inactivity_metrics",
              "17_ethological_phase_organization")) {
  check(any(grepl(src, s14, fixed = TRUE)),
        paste0("phase-dependent source '", src, "' is no longer gated"))
}
ok("all three phase-dependent upstream trees are registered for gating")

# ---------------------------------------------------------------------------
cat("\n[3] the guard actually refuses a pre-fix tree (behavioural test)\n")
# ---------------------------------------------------------------------------

# Rebuild the guard in isolation from the Stage 14 source so the test exercises
# the real definition rather than a copy that could drift.
env <- new.env(parent = globalenv())
start <- grep("^MMM_PHASE_CLASSIFIER_FIX_UTC <-", s14)
gfun  <- grep("^assert_not_pre_phase_fix <- function", s14)
check(length(start) == 1L, "cannot locate MMM_PHASE_CLASSIFIER_FIX_UTC")
check(length(gfun) == 1L,  "cannot locate assert_not_pre_phase_fix()")
# closing brace of the guard function: first line after it that is exactly "}"
close_rel <- which(s14[gfun:length(s14)] == "}")[1]
check(!is.na(close_rel), "cannot locate the end of assert_not_pre_phase_fix()")
blk <- s14[start:(gfun + close_rel - 1L)]
eval(parse(text = paste(blk, collapse = "\n")), envir = env)

tmp <- file.path(tempdir(), "s14guard", "17_ethological_phase_organization", "10min_based", "tables")
dir.create(tmp, recursive = TRUE, showWarnings = FALSE)
stale <- file.path(tmp, "phase_contrast_features.csv")
writeLines("a,b\n1,2", stale)

# Pre-fix mtime -> must stop().
Sys.setFileTime(stale, as.POSIXct("2026-05-22 14:13:07", tz = "UTC"))
res <- tryCatch({ env$assert_not_pre_phase_fix(stale); "no_error" },
                error = function(e) conditionMessage(e))
check(!identical(res, "no_error"),
      "the guard accepted a phase-dependent tree written BEFORE the classifier fix")
check(grepl("STALE PHASE-DEPENDENT INPUT", res, fixed = TRUE),
      paste0("the guard errored but not with the expected message: ", substr(res, 1, 120)))
ok("pre-fix phase-dependent tree is refused with a specific error")

# Post-fix mtime -> must pass.
Sys.setFileTime(stale, as.POSIXct("2026-09-09 12:00:00", tz = "UTC"))
res2 <- tryCatch({ env$assert_not_pre_phase_fix(stale); "no_error" },
                 error = function(e) conditionMessage(e))
check(identical(res2, "no_error"),
      paste0("the guard rejected a POST-fix tree, which would block valid runs: ", res2))
ok("post-fix tree passes the guard")

# A non-phase-dependent path must not be gated at all.
other <- file.path(tempdir(), "s14guard", "03_derived_metrics_probe.csv")
writeLines("a\n1", other)
Sys.setFileTime(other, as.POSIXct("2026-01-01 00:00:00", tz = "UTC"))
res3 <- tryCatch({ env$assert_not_pre_phase_fix(other); "no_error" },
                 error = function(e) conditionMessage(e))
check(identical(res3, "no_error"),
      "the guard gated a source that does not depend on phase classification")
ok("phase-agnostic sources are not gated")

cat("\nStage 14 input-resolution contract checks: PASS\n")
