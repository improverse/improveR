#' Time limit for one finishRun() wait, in seconds
#'
#' Read from IMPROVER_FINISHRUN_TIMEOUT so a run can widen the limit without
#' touching code. 600 s is the per-step limit proposed for the qualification
#' run. A value of 0 or an unparseable one falls back to the default rather
#' than silently disabling the limit again.
#'
#' @return A positive number of seconds.
#' @noRd
defaultFinishRunTimeout <- function() {
  raw <- Sys.getenv("IMPROVER_FINISHRUN_TIMEOUT", "")
  if (nzchar(raw)) {
    value <- suppressWarnings(as.numeric(raw))
    if (!is.na(value) && value > 0) {
      return(value)
    }
    log_warn("IMPROVER_FINISHRUN_TIMEOUT is not a positive number: ", raw,
             " - using the default of 600 s")
  }
  600
}

#' Wait until a step run reaches a terminal state
#'
#' Polls \code{fetchState()} until the run status is terminal.
#'
#' The loop this replaces had no time limit and no notion of failure: it left
#' only on "FINISHED", so a run that ended as FAILED or TERMINATED was polled
#' forever. That is what hung the qualification run of 2026-09-03 at file 18 of
#' 44. It also crashed on a transient server error, because
#' \code{internalLoadResourceFromServer()} returns NULL on any non-2xx and
#' \code{NULL$runStatus == "FINISHED"} is \code{logical(0)}, which \code{if}
#' rejects.
#'
#' A NULL or unreadable status is treated as transient and polled again; only
#' the time limit ends that. FAILED and TERMINATED end the wait immediately -
#' waiting longer cannot change them.
#'
#' @param fetchState Function returning the current run status, or NULL.
#' @param timeout Seconds to wait before giving up. Pass Inf to wait forever.
#' @param pollInterval Seconds between polls.
#' @param sleeper Sleep function; injectable so the behaviour can be tested
#'   without real waiting.
#' @param clock Function returning the current time; injectable for the same
#'   reason.
#' @return Invisibly, the terminal state ("FINISHED").
#' @noRd
waitForTerminalRunState <- function(fetchState,
                                    timeout = defaultFinishRunTimeout(),
                                    pollInterval = 2,
                                    sleeper = Sys.sleep,
                                    clock = Sys.time) {
  startedAt <- clock()
  lastState <- NA_character_
  repeat {
    state <- tryCatch(fetchState(), error = function(e) NULL)
    if (length(state) == 1L && !is.na(state)) {
      lastState <- state
      if (state == "FINISHED") {
        return(invisible(state))
      }
      if (state %in% c("FAILED", "TERMINATED")) {
        stop(sprintf("finishRun: step ended in state '%s' after %.0f s. It will not finish.",
                     state, as.numeric(difftime(clock(), startedAt, units = "secs"))),
             call. = FALSE)
      }
    }
    elapsed <- as.numeric(difftime(clock(), startedAt, units = "secs"))
    if (elapsed >= timeout) {
      stop(sprintf(paste0("finishRun: gave up after %.0f s (limit %.0f s). Last run state was ",
                          "'%s'. Raise IMPROVER_FINISHRUN_TIMEOUT if the step legitimately ",
                          "needs longer."),
                   elapsed, timeout,
                   if (is.na(lastState)) "unreadable" else lastState),
           call. = FALSE)
    }
    sleeper(pollInterval)
  }
}
