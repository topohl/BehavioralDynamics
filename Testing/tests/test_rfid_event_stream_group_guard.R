# Contract test for the Group guard of the canonical reader (Functions/rfid_event_stream.R, mmm_evs_read_preprocessed).
#
# Synthetic files in tempdir() only: nothing here reads project data or sources an Analysis/ stage.
# Checks: a preprocessed file carrying a Group column stops the read (the guard inspects the header, so the
# column-selecting read cannot hide it); a file without Group is read with the selected columns only.
#
# Portable-suite idiom: plain Rscript, fail()/check()/ok(), no testthat.

suppressPackageStartupMessages({ library(data.table) })
source("Analysis/_pipeline_setup.R")
source_mmm_helper("rfid_event_stream.R")

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")

rows <- data.table(DateTime = c("2022-10-28T18:30:00.000Z", "2022-10-28T18:31:10.250Z"), AnimalID = c("0001", "0001"),
                   System = c("sys.1", "sys.1"), PositionID = c(1L, 3L), CageChange = "CC1", Batch = "B1")

# 1. a file without Group is read, with the selected columns only
d1 <- file.path(tempdir(), "evs_guard_clean"); dir.create(d1, showWarnings = FALSE)
fwrite(rows, file.path(d1, "E9_SIS_B1_CC1_AnimalPos_preprocessed.csv"))
x <- mmm_evs_read_preprocessed(d1)
check(nrow(x) == 2 && !"Group" %in% names(x) && all(c("DateTime", "AnimalNum", "PositionID", "SourceFile") %in% names(x)),
      "a clean preprocessed file must be read")
check(inherits(x$DateTime, "POSIXct") && identical(attr(x$DateTime, "tzone"), "UTC"), "DateTime parsed as UTC")
ok("file without Group is read")

# 2. a file with a Group column stops the read, although Group is not among the selected columns
d2 <- file.path(tempdir(), "evs_guard_group"); dir.create(d2, showWarnings = FALSE)
fwrite(cbind(rows, Group = "SUS"), file.path(d2, "E9_SIS_B1_CC1_AnimalPos_preprocessed.csv"))
e <- tryCatch({ mmm_evs_read_preprocessed(d2); NULL }, error = function(err) conditionMessage(err))
check(!is.null(e) && grepl("Unexpected Group column", e), "a Group column must stop the read")
ok("file with Group column -> error")

unlink(c(d1, d2), recursive = TRUE)
cat("test_rfid_event_stream_group_guard: all checks passed\n")
