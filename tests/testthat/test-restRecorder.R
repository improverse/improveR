# The REQ-REPLAY-001 recording hook (IMR-293), FR-RPL-010 .. FR-RPL-014.
#
# No connection and no network. Every check here runs in milliseconds, which is
# the point: the recorder is the foundation of the customer-executable package,
# and its own tests must not depend on the thing it exists to replace.
#
# The two rules under test are the ones that matter more than the fields:
#   FR-RPL-014 - recording must never change a verdict, so the recorder must
#                swallow its own failures.
#   FR-RPL-034 - no credential may reach a recording.

Sys.setenv(TEST_NAME = "restRecorder")

withRecordingDir <- function(code) {
  d <- file.path(tempdir(), paste0("rec-", uuid::UUIDgenerate()))
  old <- Sys.getenv("IMPROVER_RECORD_DIR", "")
  Sys.setenv(IMPROVER_RECORD_DIR = d)
  improveR:::resetRestRecorder()
  on.exit({
    if (nzchar(old)) Sys.setenv(IMPROVER_RECORD_DIR = old) else Sys.unsetenv("IMPROVER_RECORD_DIR")
  }, add = TRUE)
  force(code(d))
}

readEntries <- function(d) {
  f <- file.path(d, "interactions.jsonl")
  if (!file.exists(f)) return(list())
  lapply(readLines(f, warn = FALSE), function(l) jsonlite::fromJSON(l, simplifyVector = FALSE))
}

test_that("recording is off unless the environment switches it on|FR-RPL-013", {
  old <- Sys.getenv("IMPROVER_RECORD_DIR", "")
  Sys.unsetenv("IMPROVER_RECORD_DIR")
  on.exit(if (nzchar(old)) Sys.setenv(IMPROVER_RECORD_DIR = old), add = TRUE)

  expect_null(improveR:::restRecordingDir())
  # And recording is a no-op rather than an error when off.
  expect_silent(improveR:::recordRestInteraction(
    "resources/{id}", "https://h/resources/7", "GET", "", NULL, Sys.time()))
})

test_that("an interaction carries the template and the concrete URL separately|FR-RPL-011,FR-RPL-012", {
  withRecordingDir(function(d) {
    improveR:::recordRestInteraction(
      urlTemplate = "resources/{resourceId}/processes/{processId}",
      url         = "https://host/api/v1/resources/AAA/processes/BBB?x=1",
      method      = "GET", data = "", result = NULL, startedAt = Sys.time())

    e <- readEntries(d)
    expect_length(e, 1L)
    # The template is the form that resolves against a server specification.
    # Keeping only the concrete URL would lose that (REQ-CLIOQ-001 FR-CLI-002).
    expect_equal(e[[1]]$urlTemplate, "resources/{resourceId}/processes/{processId}")
    expect_equal(e[[1]]$url, "https://host/api/v1/resources/AAA/processes/BBB?x=1")
    expect_false(identical(e[[1]]$urlTemplate, e[[1]]$url))
    expect_equal(e[[1]]$method, "GET")
    expect_equal(e[[1]]$testCase, "restRecorder")
    expect_true(is.numeric(e[[1]]$durationMs))
  })
})

test_that("the ordinal increments, which is what makes a state transition replayable|FR-RPL-011", {
  withRecordingDir(function(d) {
    # Three polls of the SAME url - exactly what waitForTerminalRunState does.
    # Keyed by (method, url) alone these are one key and one answer forever;
    # that is why httptest cannot serve Purpose B (ENT-05, IMR-293).
    for (i in 1:3) {
      improveR:::recordRestInteraction("resources/{id}", "https://host/api/v1/resources/S1",
                                       "GET", "", NULL, Sys.time())
    }
    e <- readEntries(d)
    expect_length(e, 3L)
    expect_equal(vapply(e, function(x) x$seq, numeric(1)), c(1, 2, 3))
    # same method, same url, three distinct keys
    expect_equal(length(unique(vapply(e, function(x) x$url, character(1)))), 1L)
  })
})

test_that("credentials never reach a recording|FR-RPL-034", {
  withRecordingDir(function(d) {
    improveR:::recordRestInteraction(
      "token", "https://host/token", "POST",
      data = list(username = "someone", password = "hunter2",
                  client_secret = "s3cr3t", refresh_token = "rt-xyz",
                  grant_type = "password"),
      result = NULL, startedAt = Sys.time())

    raw <- readLines(file.path(d, "interactions.jsonl"), warn = FALSE)
    for (secret in c("hunter2", "s3cr3t", "rt-xyz")) {
      expect_false(grepl(secret, raw, fixed = TRUE),
                   info = paste0("'", secret, "' must not appear in a recording"))
    }
    # the non-sensitive fields survive, or the recording is useless
    expect_true(grepl("someone", raw, fixed = TRUE))
    expect_true(grepl("grant_type", raw, fixed = TRUE))

    e <- readEntries(d)
    expect_equal(e[[1]]$requestBody$password, "<redacted>")
    expect_equal(e[[1]]$requestBody$client_secret, "<redacted>")
  })
})

test_that("a file upload is noted, not embedded|FR-RPL-011", {
  body <- list(file = structure(list(path = "/tmp/x"), class = "form_file"))
  red <- improveR:::redactBody(body)
  expect_true(isTRUE(red[["<fileUpload>"]]))
  expect_null(red$file)

  expect_equal(names(improveR:::redactBody(as.raw(c(1, 2, 3)))), "<binary>")
})

test_that("a recorder failure cannot fail a test|FR-RPL-014", {
  # The rule that matters most. A recorder that can break a qualification run
  # is worse than no recorder, so a broken destination must cost nothing.
  old <- Sys.getenv("IMPROVER_RECORD_DIR", "")
  on.exit(if (nzchar(old)) Sys.setenv(IMPROVER_RECORD_DIR = old) else
            Sys.unsetenv("IMPROVER_RECORD_DIR"), add = TRUE)

  # point the recorder at a path that cannot be created: an existing FILE
  blocker <- tempfile(); writeLines("not a directory", blocker)
  Sys.setenv(IMPROVER_RECORD_DIR = file.path(blocker, "under-a-file"))

  expect_error(
    improveR:::recordRestInteraction("a/{b}", "https://h/a/1", "GET", "", NULL, Sys.time()),
    NA)
  expect_silent(
    improveR:::recordRestInteraction("a/{b}", "https://h/a/1", "GET", "", NULL, Sys.time()))
})
