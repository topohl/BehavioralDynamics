# Static replay order for the numbered-root audit queue. Parses code but does
# not source audit scripts, run calculations, or write analysis_ready outputs.
args <- commandArgs(trailingOnly = TRUE)
queue_path <- if (length(args) >= 1L) args[[1L]] else
  "docs/behavior_output_archive_audit_script_queue.csv"
if (length(args) > 1L) stop("Usage: Rscript Maintenance/Get-BehaviorAuditReplayPlan.R [queue.csv]")

queue <- read.csv(queue_path, stringsAsFactors = FALSE, check.names = FALSE)
if (!all(c("script", "review_state") %in% names(queue)) ||
    !nrow(queue) || anyDuplicated(queue$script)) {
  stop("Audit queue needs unique script rows and review states.")
}

calls_named <- function(node, target) {
  found <- character()
  visit <- function(x) {
    if (is.call(x)) {
      if (identical(as.character(x[[1L]])[1L], target)) {
        if (length(x) < 2L || !is.character(x[[2L]]) ||
            length(x[[2L]]) != 1L) {
          stop("Nonliteral replay identifier in ", target)
        }
        found <<- c(found, x[[2L]])
      }
      parts <- as.list(x)[-1L]
      for (j in seq_along(parts)) {
        if (!identical(deparse(parts[j]), "list()")) visit(parts[[j]])
      }
    } else if (is.expression(x) || is.pairlist(x) || is.list(x)) {
      for (j in seq_along(x)) {
        if (!identical(deparse(x[j]), "list()")) visit(x[[j]])
      }
    }
  }
  visit(node)
  unique(found)
}

input_ids <- vector("list", nrow(queue))
output_ids <- rep(NA_character_, nrow(queue))
for (i in seq_len(nrow(queue))) {
  script <- queue$script[[i]]
  if (!grepl("^Testing/audits/[A-Za-z0-9_.-]+\\.R$", script) ||
      !file.exists(script)) stop("Missing or unsafe queued audit script: ", script)
  code <- parse(file = script)
  input_ids[[i]] <- calls_named(code, "mmm_behavior_audit_replay_input_root")
  outputs <- calls_named(code, "mmm_behavior_audit_replay_output_root")
  if (length(outputs) > 1L) stop("Multiple replay output IDs in ", script)
  if (length(outputs)) output_ids[[i]] <- outputs[[1L]]
}
defined <- output_ids[!is.na(output_ids)]
if (anyDuplicated(defined)) stop("Duplicate replay output identifier.")
unknown <- setdiff(unique(unlist(input_ids)), defined)
if (length(unknown)) stop("Replay input has no queued producer: ",
                          paste(unknown, collapse = ", "))

prerequisites <- lapply(input_ids, function(ids) {
  if (!length(ids)) return(character())
  queue$script[match(ids, output_ids)]
})
ordered <- integer()
remaining <- seq_len(nrow(queue))
while (length(remaining)) {
  eligible <- remaining[vapply(remaining, function(i)
    all(prerequisites[[i]] %in% queue$script[ordered]), logical(1))]
  if (!length(eligible)) stop("Cycle in audit replay dependencies.")
  next_i <- eligible[[1L]]
  ordered <- c(ordered, next_i)
  remaining <- remaining[remaining != next_i]
}

plan <- data.frame(
  replay_order = seq_along(ordered),
  script = queue$script[ordered],
  output_id = output_ids[ordered],
  prerequisite_output_ids = vapply(input_ids[ordered], paste, collapse = ";",
                                   FUN.VALUE = character(1)),
  prerequisite_scripts = vapply(prerequisites[ordered], paste, collapse = ";",
                                 FUN.VALUE = character(1)),
  review_state = queue$review_state[ordered],
  stringsAsFactors = FALSE)
write.csv(plan, stdout(), row.names = FALSE, na = "")
