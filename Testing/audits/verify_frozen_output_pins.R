# ================================================================
# Verify: the live files pinned by the repository output pin lists still hold their pinned bytes
# MMMSociability -- Testing/audits/verify_frozen_output_pins.R
# ================================================================
# Read-only. Hashes every file named in MMM_REPO_PIN_LISTS (Functions/frozen_input_guard.R), e.g. the 72 live Stage 08
# HMM outputs, and compares it with its pin. Writes nothing; exits 1 on any missing or changed file. Run after any
# pipeline activity on the live root.
# Run from the repository root:  Rscript Testing/audits/verify_frozen_output_pins.R
# ================================================================

verify_output_pins <- function(lists) {
  do.call(rbind, lapply(names(lists), function(n) {
    p <- utils::read.csv(lists[[n]], stringsAsFactors = FALSE, colClasses = "character")
    now <- vapply(p$input, function(f) if (file.exists(f)) digest::digest(file = f, algo = "sha256") else NA_character_, "", USE.NAMES = FALSE)
    data.frame(pin_list = n, input = p$input, pinned = p$sha256, now = now, ok = !is.na(now) & now == p$sha256, stringsAsFactors = FALSE)
  }))
}

if (sys.nframe() == 0L) {
  source("Functions/frozen_input_guard.R")
  v <- verify_output_pins(.mmm_fg_repo_lists())
  for (n in unique(v$pin_list)) cat(sprintf("%s: %d of %d files hold their pinned bytes\n", n, sum(v$ok[v$pin_list == n]), sum(v$pin_list == n)))
  if (!all(v$ok)) { print(v[!v$ok, c("pin_list", "input", "pinned", "now")], row.names = FALSE); quit(status = 1) }
  cat("PASS: every pinned output unchanged\n")
}
