# Stage 08 (HMM behavioural states) registers its regularised Gaussian response method when the stage is source()d into a
# child environment, as run_all_analysis.R does (source(path, local = new.env(parent = .GlobalEnv))).
#
#  1. the setMethod() call passes the depmixS4 generic object, not the name "fit" (a name is looked up from the global
#     environment, where a child-environment run has not bound it: "no existing definition for function 'fit'")
#  2. Stage 08's own statements, from the depmixS4 check to setMethod(), evaluate without error in a child environment
#  3. depmixS4's internal fit() call dispatches the method: a near-constant response gets the emission SD floor
#
# Portable: parses Stage 08 and fits one synthetic response; nothing is read from or written to a data root.
# Portable-suite idiom: plain Rscript from the repo root, fail()/check()/ok(), no testthat.

fail  <- function(msg) stop("FAIL: ", msg, call. = FALSE)
check <- function(cond, msg) if (!isTRUE(cond)) fail(msg) else invisible(TRUE)
ok    <- function(msg) cat("  ok  ", msg, "\n")
STAGE08 <- "Analysis/08_hmm_behavioral_states_optional.R"
if (!requireNamespace("depmixS4", quietly = TRUE)) fail("depmixS4 is required")

exprs <- parse(STAGE08, keep.source = TRUE)
txt <- vapply(attr(exprs, "srcref"), function(s) paste(as.character(s), collapse = "\n"), character(1))

# ---------------------------------------------------------------- 1. the generic object, not its name
set_at <- which(startsWith(txt, "methods::setMethod("))
check(length(set_at) == 1, "1: Stage 08 has one top-level setMethod() call")
check(identical(as.character(exprs[[set_at]][[2]]), c("::", "depmixS4", "fit")), "1: setMethod() is given depmixS4::fit")
ok("setMethod() receives the depmixS4 generic object")

# ---------------------------------------------------------------- 2. the block in a child environment
floor_at <- which(startsWith(txt, "hmm_emission_sd_floor <- "))
from <- which(startsWith(txt, "if (!requireNamespace(\"depmixS4\""))
check(length(floor_at) == 1 && length(from) == 1 && from < set_at, "2: the SD floor, the depmixS4 check and setMethod() are found once each")
E <- new.env(parent = globalenv())
res <- tryCatch({ for (i in c(floor_at, from:set_at)) eval(exprs[[i]], E); NULL }, error = function(e) conditionMessage(e))
check(is.null(res), paste("2: Stage 08's class and method statements evaluate in a child environment:", res))
check(!exists("fit", envir = globalenv(), inherits = FALSE), "2: the child-environment run binds no global fit")
check(methods::existsMethod(depmixS4::fit, "HMMRegularizedNORMresponse"), "2: the method is registered on the depmixS4 generic")
ok("the class and method register from a child environment")

# ---------------------------------------------------------------- 3. dispatch from inside depmixS4
set.seed(1)
d <- data.frame(y = 0.4 + stats::rnorm(200, sd = 1e-4))
resp <- depmixS4::GLMresponse(y ~ 1, data = d, family = stats::gaussian())
reg <- methods::new("HMMRegularizedNORMresponse", resp)
fitted <- eval(quote(fit(reg)), list(reg = reg), asNamespace("depmixS4"))   # how em.depmix calls fit() on a response
check(isTRUE(all.equal(unname(depmixS4::getpars(fitted)[["sd"]]), E$hmm_emission_sd_floor)), "3: the regularised method sets the SD floor")
plain <- eval(quote(fit(resp)), list(resp = resp), asNamespace("depmixS4"))
check(unname(depmixS4::getpars(plain)[["sd"]]) < 1e-3, "3: control: the plain NORMresponse keeps its tiny SD")
ok(sprintf("depmixS4 dispatches the method (SD %.3f vs %.1e without it)", depmixS4::getpars(fitted)[["sd"]], depmixS4::getpars(plain)[["sd"]]))
cat("Stage 08 child-environment checks: PASS\n")
