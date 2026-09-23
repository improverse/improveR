# terminateStepResource: the guarded failure paths (IMR-292)
#
# These need no server. Everything terminateStepResource touches outside itself
# is mocked, which is the only way to reach the path the defect lived on: the
# server has to answer non-2xx at exactly that call, and a live test cannot ask
# it to.
#
# The defect: authenticatedREST() answers EVERY non-2xx with NULL - its
# documented contract - and this site passed that NULL straight to
# httr::status_code(), which died with
#
#   no applicable method for 'status_code' applied to an object of class NULL
#
# naming neither the step, nor the operation, nor the status, while
# lastRestError() was holding all three.
#
# Note log_warn() is mocked rather than caught with expect_warning(): it goes
# through logging::logwarn(), which writes to a handler and never raises an R
# warning condition. Mocking it is also the more direct assertion - it says what
# the function reported, not merely that something was emitted.

fakeStep <- function(runStatus = "RUNNING") {
  df <- data.frame(
    path       = "/Projects/Tests/tree/Step 1",
    name       = "Step 1",
    resourceId = "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
    entityId   = "prefix:ST-1",
    stringsAsFactors = FALSE
  )
  # runStatus = NULL must produce a resource WITHOUT the column, which is the
  # shape that made `stepToTerminate$runStatus == "RUNNING"` return logical(0).
  if (!is.null(runStatus)) df$runStatus <- runStatus
  df
}

# Collects what the function logged as a warning. log_warn() is mocked rather
# than caught with expect_warning() because it goes through logging::logwarn(),
# which writes to a handler and never raises an R warning condition. Mocking is
# also the more direct assertion: it says what the function reported.
logCollector <- function() {
  e <- new.env(parent = emptyenv())
  e$lines <- character(0)
  e$fn <- function(...) {
    e$lines <- c(e$lines, paste(vapply(list(...), function(x)
      paste(as.character(x), collapse = " "), character(1)), collapse = " "))
    invisible(NULL)
  }
  e
}

IDENT <- "/Projects/Tests/tree/Step 1"

test_that("a terminate call the server refuses reports step, operation and status|IMR-292", {
  log <- logCollector()
  result <- testthat::with_mocked_bindings(
    improveR::terminateStepResource(IDENT),
    setEditable       = function(...) invisible(TRUE),
    log_warn          = log$fn,
    refreshResource   = function(...) fakeStep("RUNNING"),
    authenticatedREST = function(...) NULL,
    lastRestError     = function(...) list(status_code = 500L, method = "POST",
                                           url = "resources/AAA/terminate",
                                           message = "Runserver is not reachable",
                                           timestamp = Sys.time()),
    .package = "improveR"
  )
  expect_false(result)
  # all three, which is the whole point of the ticket
  expect_true(any(grepl("HTTP 500", log$lines, fixed = TRUE)))
  expect_true(any(grepl("POST",     log$lines, fixed = TRUE)))
  expect_true(any(grepl("Step 1",   log$lines, fixed = TRUE)))
  # and NOT the error the defect produced
  expect_false(any(grepl("status_code", log$lines, fixed = TRUE)))
})

test_that("a failure with no recorded REST error still says so instead of dying|IMR-292", {
  log <- logCollector()
  result <- testthat::with_mocked_bindings(
    improveR::terminateStepResource(IDENT),
    setEditable       = function(...) invisible(TRUE),
    log_warn          = log$fn,
    refreshResource   = function(...) fakeStep("RUNNING"),
    authenticatedREST = function(...) NULL,
    lastRestError     = function(...) stop("nothing recorded"),
    .package = "improveR"
  )
  expect_false(result)
  expect_true(any(grepl("recorded no REST error", log$lines, fixed = TRUE)))
})

test_that("a step whose resource carries no runStatus does not raise 'invalid length argument'|IMR-292", {
  # NULL == "RUNNING" is logical(0). Before the isTRUE() guard the caller got an
  # argument-length error from `if`, which says nothing about the step.
  log <- logCollector()
  result <- testthat::with_mocked_bindings(
    improveR::terminateStepResource(IDENT),
    setEditable       = function(...) invisible(TRUE),
    log_warn          = log$fn,
    refreshResource   = function(...) fakeStep(NULL),
    authenticatedREST = function(...) stop("must not be called without a runStatus"),
    .package = "improveR"
  )
  expect_false(result)
  expect_true(any(grepl("[Cc]annot be terminated", log$lines)))
})

test_that("a step that is not running is not terminated|IMR-292", {
  log <- logCollector()
  result <- testthat::with_mocked_bindings(
    improveR::terminateStepResource(IDENT),
    setEditable       = function(...) invisible(TRUE),
    log_warn          = log$fn,
    refreshResource   = function(...) fakeStep("FINISHED"),
    authenticatedREST = function(...) stop("must not be called when not RUNNING"),
    .package = "improveR"
  )
  expect_false(result)
  expect_true(any(grepl("FINISHED", log$lines, fixed = TRUE)))
})

test_that("a step that cannot be loaded is reported, not dereferenced|IMR-292", {
  log <- logCollector()
  result <- testthat::with_mocked_bindings(
    improveR::terminateStepResource(IDENT),
    setEditable       = function(...) invisible(TRUE),
    log_warn          = log$fn,
    refreshResource   = function(...) NULL,
    authenticatedREST = function(...) stop("must not be called when the step did not load"),
    .package = "improveR"
  )
  expect_false(result)
  expect_true(any(grepl("could not be loaded", log$lines, fixed = TRUE)))
})

test_that("a terminate the server accepts returns TRUE|IMR-292", {
  log <- logCollector()
  calls <- 0L
  result <- testthat::with_mocked_bindings(
    improveR::terminateStepResource(IDENT),
    setEditable       = function(...) invisible(TRUE),
    log_warn          = log$fn,
    refreshResource   = function(...) {
      calls <<- calls + 1L
      # first call is the step to terminate; the later ones are the confirmation
      # poll, which must leave RUNNING or it runs its full 30 seconds
      if (calls == 1L) fakeStep("RUNNING") else fakeStep("TERMINATED")
    },
    authenticatedREST = function(...) structure(list(status_code = 200L),
                                                class = "response"),
    .package = "improveR"
  )
  expect_true(result)
})
