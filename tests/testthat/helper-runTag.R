# Reproducible names for test data.
#
# Test data used to be built from the clock at the moment of each call:
#
#   folderName <- paste0("gft-check-", format(Sys.time(), "%Y%m%d%H%M%S"))
#
# That has two problems, and the second only became visible once the first was
# fixed.
#
# 1. It cannot be replayed. Every execution asks a different question, so a
#    recording of one run cannot answer the next one. Replaying
#    test-gft-check1.R against its own recording served 735 of 735 requests from
#    the recording and still failed, because the test had invented a folder name
#    the recorded server had never been asked about (IMR-297).
#
# 2. It was never actually unique. setupLoadReview() in test-loadReview.R runs
#    six times, once per test, and each call built a review named
#    "LRV-<HHMMSS>". Those six names differed only because the six tests
#    happened to fall in different seconds. Nothing guaranteed that; two fast
#    tests in one second would have collided, and the failure would have looked
#    like a server fault ("Resource Name is not unique"). Replacing the clock
#    with one identifier per run turned that latent flake into six deterministic
#    failures, which is how it was found.
#
# So a name needs both: the run it belongs to, and which creation within the run
# it is.
#
#   runTag()      the run identifier - the SAME value every call
#   uniqueTag()   the run identifier plus a per-creation sequence number
#
# Use uniqueTag() for anything that is created on the server. Use runTag() only
# where several places must agree on one value.
#
# Both are deterministic: given the same IMPROVER_RUN_ID and the same sequence
# of calls, they produce the same names. The sequence counter is per R process,
# so a recording and its replay must run the same selection of test files - which
# record.sh and run-replay.sh both take as the same argument.

if (!nzchar(Sys.getenv("IMPROVER_RUN_ID", ""))) {
  Sys.setenv(IMPROVER_RUN_ID = format(Sys.time(), "%Y%m%d%H%M%S"))
}

.runTagEnv <- new.env(parent = emptyenv())
.runTagEnv$seq <- 0L

#' The run identifier, or its last n characters
#'
#' `runTag()` replaces `format(Sys.time(), "%Y%m%d%H%M%S")` and `runTag(6)`
#' replaces `format(Sys.time(), "%H%M%S")`. The short form is the tail of the
#' long one rather than a second, independently drifting clock reading.
runTag <- function(n = 14L) {
  id <- Sys.getenv("IMPROVER_RUN_ID", "")
  if (!nzchar(id)) {
    # Only reachable if something cleared the variable mid-run. Falling back to
    # the clock keeps the suite working; it costs replayability for this one
    # name, which is better than a name that is empty.
    id <- format(Sys.time(), "%Y%m%d%H%M%S")
  }
  if (n >= nchar(id)) id else substring(id, nchar(id) - n + 1L)
}

#' A name component that is unique within the run and reproducible across runs
#'
#' Every call returns a different value; the same call in the same position of
#' the same run returns the same value it returned last time.
uniqueTag <- function(n = 14L) {
  .runTagEnv$seq <- .runTagEnv$seq + 1L
  paste0(runTag(n), "-", .runTagEnv$seq)
}

#' The run's clock, as a time and as a date
#'
#' Some tests do not put the clock in a NAME, they send it to the server as
#' DATA and then assert the server echoed it back:
#'
#'   newTime <- Sys.time()
#'   setGridArgument(process$id, "start", newTime, update = TRUE)
#'   expect_equal(<what the server returned>, substr(as.character(newTime), 0, 14))
#'
#' That cannot replay either, and it fails in a way that looks like a product
#' defect rather than a test one:
#'
#'   `actual`:   "2026-09-16 19:"   <- what the recorded server was sent
#'   `expected`: "2026-09-17 09:"   <- what this run just read off the clock
#'
#' Derived from IMPROVER_RUN_ID so that record and replay send the same value.
#' Unlike the name helpers these do NOT vary per call: a timestamp sent as data
#' is not required to be unique, and a stable value is what makes the echo
#' assertable.
runTime <- function() {
  as.POSIXct(runTag(14L), format = "%Y%m%d%H%M%S", tz = Sys.timezone())
}

runDate <- function() as.Date(runTime())
