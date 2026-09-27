# ================================================================
# Freeze the canonical behavioural analysis configuration
# MMMSociability -- Analysis/_supporting/freeze_behavior_analysis_config.R
# ================================================================
# Serialises Functions/behavior_analysis_config.R to canonical JSON, hashes it
# (SHA-256) and writes an immutable, versioned freeze record under
#   analysis_ready/canonical/behavior_config/v<config_version>/
# It fits no model and reads no behavioural data.
#
# Refuses to run if:
#   * the config file has uncommitted changes (the frozen version must be a commit),
#   * the version directory already exists (a frozen version is never rewritten).
# ================================================================

suppressPackageStartupMessages({ library(jsonlite); library(digest) })

.pipeline_setup_candidates <- c(file.path(getwd(), "Analysis", "_pipeline_setup.R"), file.path(getwd(), "_pipeline_setup.R"))
.pipeline_setup <- .pipeline_setup_candidates[file.exists(.pipeline_setup_candidates)][1]
if (is.na(.pipeline_setup)) stop("Run from the MMMSociability repo root.", call. = FALSE)
source(.pipeline_setup)
source_mmm_helper("project_paths.R")
source_mmm_helper("behavior_analysis_config.R")

cfg_file <- file.path(MMM_REPO_ROOT, "Functions", "behavior_analysis_config.R")
git <- function(...) system2("git", c("-C", shQuote(MMM_REPO_ROOT), ...), stdout = TRUE, stderr = TRUE)
dirty <- git("status", "--porcelain", "--", "Functions/behavior_analysis_config.R")
if (length(dirty)) stop("Config file has uncommitted changes; commit it first:\n", paste(dirty, collapse = "\n"), call. = FALSE)
commit <- git("rev-parse", "HEAD")[1]
blob <- git("rev-parse", "HEAD:Functions/behavior_analysis_config.R")[1]
branch <- git("branch", "--show-current")[1]

cfg <- MMM_BEHAVIOR_CONFIG

# Serialisation checks: jsonlite drops the names of named atomic vectors, so every named element must be a list.
named_atomic <- function(x, path = "cfg") {
  if (is.list(x)) return(unlist(Map(named_atomic, x, paste0(path, "$", ifelse(nzchar(names(x) %||% ""), names(x), seq_along(x))))))
  if (is.atomic(x) && !is.null(names(x))) path else character()
}
bad <- named_atomic(cfg)
if (length(bad)) stop("Named atomic vectors lose their names in JSON; use list():\n", paste(bad, collapse = "\n"), call. = FALSE)
name_paths <- function(x, path = "") {
  if (!is.list(x) || (is.null(names(x)) && all(vapply(x, is.atomic, TRUE)))) return(path)   # leaf or plain array
  nm <- names(x) %||% rep("", length(x)); nm[!nzchar(nm)] <- paste0("[", which(!nzchar(nm)), "]")
  c(path, unlist(Map(name_paths, x, paste0(path, "/", nm))))
}
json <- mmm_behavior_config_json(cfg)
back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
p_cfg <- name_paths(cfg); p_back <- name_paths(back)
if (!setequal(p_cfg, p_back)) stop("JSON round trip changed the name structure:\n",
  paste(c(setdiff(p_cfg, p_back), setdiff(p_back, p_cfg)), collapse = "\n"), call. = FALSE)
for (e in c("Q1", "Q2c", "RS_sexavg_CC1", "RS_by_sex_CC1"))
  if (!identical(names(back$contrasts[[e]]$L), names(cfg$contrasts[[e]]$L)) || !isTRUE(all.equal(unlist(back$contrasts[[e]]$L), unlist(cfg$contrasts[[e]]$L))))
    stop("Contrast ", e, " does not survive the JSON round trip.", call. = FALSE)
tiers <- vapply(cfg$multiplicity$families, `[[`, "", "tier")
if (!all(tiers %in% cfg$meta$tier_vocabulary)) stop("Family tier outside the tier vocabulary.", call. = FALSE)
if (any(grepl(paste(cfg$meta$prohibited_tier_words, collapse = "|"), c(tiers, names(cfg$multiplicity$families)), ignore.case = TRUE)))
  stop("Prohibited tier word used for a family.", call. = FALSE)
sha <- mmm_behavior_config_sha256(cfg)

root <- file.path(mmm_project_root(), "analysis_ready", "canonical", "behavior_config")
vdir <- file.path(root, paste0("v", cfg$meta$config_version))
if (dir.exists(vdir)) stop("Frozen version already exists (immutable): ", vdir, call. = FALSE)
dir.create(vdir, recursive = TRUE)
writeLines(json, file.path(vdir, "behavior_analysis_config.json"), useBytes = TRUE)
file.copy(cfg_file, file.path(vdir, "behavior_analysis_config.R"))
rec <- data.frame(
  config_id = cfg$meta$config_id, config_version = cfg$meta$config_version,
  config_json_sha256 = sha,
  config_json_file_sha256 = digest::digest(file = file.path(vdir, "behavior_analysis_config.json"), algo = "sha256"),
  config_r_file_sha256 = digest::digest(file = cfg_file, algo = "sha256"),
  git_commit = commit, git_blob = blob, git_branch = branch,
  frozen_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  frozen_before_res_sus_outcome_models = isTRUE(cfg$meta$frozen_before_res_sus_outcome_models),
  r_version = R.version.string, stringsAsFactors = FALSE)
write.csv(rec, file.path(vdir, "freeze_record.csv"), row.names = FALSE)
writeLines(sha, file.path(vdir, "config_sha256.txt"))
registry <- file.path(root, "CONFIG_REGISTRY.csv")
write.table(rec, registry, sep = ",", row.names = FALSE, col.names = !file.exists(registry), append = file.exists(registry))
Sys.chmod(list.files(vdir, full.names = TRUE), mode = "0444")
message("Frozen ", cfg$meta$config_id, " v", cfg$meta$config_version, "  sha256 ", sha, "  commit ", commit)
