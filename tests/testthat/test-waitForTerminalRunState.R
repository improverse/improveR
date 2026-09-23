# Pure logic tests: no server, no real waiting - the clock and the sleep are
# injected, so a ten-minute timeout is proven in milliseconds (IMR-268).
# Qualified with improveR::: so the checks run against the installed package.

fakeClock <- function(seconds) {
  i <- 0L
  function() {
    i <<- i + 1L
    as.POSIXct("2026-09-09 00:00:00", tz = "UTC") + seconds[min(i, length(seconds))]
  }
}

test_that("a run that reaches FINISHED ends the wait|ics1140", {
  states <- c("QUEUED", "RUNNING", "FINISHED")
  i <- 0L
  expect_equal(
    improveR:::waitForTerminalRunState(
      fetchState = function() { i <<- i + 1L; states[i] },
      timeout = 600, sleeper = function(x) invisible(NULL)),
    "FINISHED")
  expect_equal(i, 3L)
})

test_that("FAILED ends the wait immediately instead of polling forever|ics1140", {
  polls <- 0L
  expect_error(
    improveR:::waitForTerminalRunState(
      fetchState = function() { polls <<- polls + 1L; "FAILED" },
      timeout = 600, sleeper = function(x) stop("must not sleep on a terminal state")),
    "ended in state 'FAILED'", fixed = TRUE)
  expect_equal(polls, 1L)
})

test_that("TERMINATED ends the wait immediately|ics1140", {
  expect_error(
    improveR:::waitForTerminalRunState(
      fetchState = function() "TERMINATED",
      timeout = 600, sleeper = function(x) invisible(NULL)),
    "ended in state 'TERMINATED'", fixed = TRUE)
})

test_that("a run that never finishes stops at the time limit|ics1140", {
  # Clock jumps past the limit on the second reading.
  expect_error(
    improveR:::waitForTerminalRunState(
      fetchState = function() "RUNNING",
      timeout = 600, sleeper = function(x) invisible(NULL),
      clock = fakeClock(c(0, 601))),
    "gave up after 601 s (limit 600 s)", fixed = TRUE)
})

test_that("the last seen state is named in the timeout message|ics1140", {
  expect_error(
    improveR:::waitForTerminalRunState(
      fetchState = function() "QUEUED",
      timeout = 10, sleeper = function(x) invisible(NULL),
      clock = fakeClock(c(0, 11))),
    "Last run state was 'QUEUED'", fixed = TRUE)
})

test_that("a NULL status is transient, not terminal, and does not crash|ics1140", {
  # This is the case the old loop died on: NULL$runStatus == "FINISHED" is
  # logical(0), and if(logical(0)) is an error.
  states <- list(NULL, NULL, "FINISHED")
  i <- 0L
  expect_equal(
    improveR:::waitForTerminalRunState(
      fetchState = function() { i <<- i + 1L; states[[i]] },
      timeout = 600, sleeper = function(x) invisible(NULL)),
    "FINISHED")
})

test_that("an unreadable status is reported as such at the time limit|ics1140", {
  expect_error(
    improveR:::waitForTerminalRunState(
      fetchState = function() NULL,
      timeout = 5, sleeper = function(x) invisible(NULL),
      clock = fakeClock(c(0, 6))),
    "Last run state was 'unreadable'", fixed = TRUE)
})

test_that("an error from the server call is transient, not fatal|ics1140", {
  states <- list(quote(stop("boom")), "FINISHED")
  i <- 0L
  expect_equal(
    improveR:::waitForTerminalRunState(
      fetchState = function() { i <<- i + 1L; if (i == 1L) stop("boom") else "FINISHED" },
      timeout = 600, sleeper = function(x) invisible(NULL)),
    "FINISHED")
})

test_that("IMPROVER_FINISHRUN_TIMEOUT sets the default, junk falls back|ics1140", {
  old <- Sys.getenv("IMPROVER_FINISHRUN_TIMEOUT", unset = NA)
  on.exit(if (is.na(old)) Sys.unsetenv("IMPROVER_FINISHRUN_TIMEOUT")
          else Sys.setenv(IMPROVER_FINISHRUN_TIMEOUT = old), add = TRUE)

  Sys.setenv(IMPROVER_FINISHRUN_TIMEOUT = "42")
  expect_equal(improveR:::defaultFinishRunTimeout(), 42)

  Sys.setenv(IMPROVER_FINISHRUN_TIMEOUT = "0")
  expect_equal(improveR:::defaultFinishRunTimeout(), 600)

  Sys.setenv(IMPROVER_FINISHRUN_TIMEOUT = "soon")
  expect_equal(improveR:::defaultFinishRunTimeout(), 600)

  Sys.unsetenv("IMPROVER_FINISHRUN_TIMEOUT")
  expect_equal(improveR:::defaultFinishRunTimeout(), 600)
})
