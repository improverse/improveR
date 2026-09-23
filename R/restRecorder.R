# Recording hook for REQ-REPLAY-001 (IMR-293), FR-RPL-010 .. FR-RPL-014.
#
# Every REST call improveR makes passes through authenticatedREST() - 136 call
# sites in 52 files - so one hook here is complete by construction. That is what
# FR-RPL-010 assumes when it requires capture at the CLIENT LIBRARY level rather
# than by an intercepting proxy (EV-Q-02 in REQ-EVIDENCE-001).
#
# Two rules govern everything below:
#
#   FR-RPL-014  Recording must not alter the behaviour or the result of the
#               recorded execution. Every entry point is wrapped so that a
#               recorder failure is swallowed: the hook cannot throw, cannot
#               change control flow, and cannot change what authenticatedREST()
#               returns. A broken recorder must cost a run nothing.
#
#   FR-RPL-034  No credentials leave the process. The Authorization header is
#               never written - not redacted after the fact, never collected -
#               and known-sensitive body keys are replaced before serialisation.

recorderEnv <- new.env(parent = emptyenv())
recorderEnv$seq <- 0L

#' Is recording switched on?
#'
#' Switched by environment alone, so no test case is edited to enable it
#' (FR-RPL-013). Follows the IMPROVER_DISPLAY_REST_CALLS precedent.
#' @noRd
restRecordingDir <- function() {
  d <- Sys.getenv("IMPROVER_RECORD_DIR", "")
  if (!nzchar(d)) {
    return(NULL)
  }
  d
}

# Body keys whose values must never reach a recording. Matched case-insensitively
# against the top level of a list body.
SENSITIVE_KEYS <- c("password", "client_secret", "clientSecret", "secret",
                    "access_token", "refresh_token", "accessToken",
                    "refreshToken", "token", "reqToken", "authorization")

#' @noRd
redactBody <- function(data) {
  if (is.raw(data)) {
    return(list(`<binary>` = length(data)))
  }
  if (is.character(data) && length(data) == 1L && !nzchar(data)) {
    return(NULL)
  }
  if (is.list(data)) {
    # httr::upload_file() objects carry a path and are not payload we may keep.
    if (any(vapply(data, function(x) inherits(x, "form_file"), logical(1)))) {
      return(list(`<fileUpload>` = TRUE))
    }
    nm <- names(data)
    if (!is.null(nm)) {
      hit <- tolower(nm) %in% tolower(SENSITIVE_KEYS)
      data[hit] <- "<redacted>"
    }
    return(data)
  }
  data
}

#' @noRd
safeHeaders <- function(result) {
  h <- tryCatch(as.list(httr::headers(result)), error = function(e) list())
  h[tolower(names(h)) != "authorization"]
}

#' @noRd
safeBody <- function(result) {
  tryCatch(
    substr(as.character(httr::content(result, as = "text", encoding = "UTF-8")),
           1, 100000),
    error = function(e) "<unreadable body>"
  )
}

#' Record one interaction
#'
#' @param urlTemplate the url BEFORE placeholder substitution. FR-RPL-012 keeps
#'   it beside the concrete URL, because the template is the form that resolves
#'   against a server specification (REQ-CLIOQ-001 FR-CLI-002).
#' @param result the httr response, or NULL when the call produced none.
#' @returns Nothing usable. Called for its side effect, and silent on failure -
#'   see FR-RPL-014 above.
#' @noRd
recordRestInteraction <- function(urlTemplate, url, method, data,
                                  result, startedAt) {
  dir <- restRecordingDir()
  if (is.null(dir)) {
    return(invisible(NULL))
  }
  tryCatch(withCallingHandlers({
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
    recorderEnv$seq <- recorderEnv$seq + 1L

    entry <- list(
      # The ordinal is what makes a state transition replayable. Keyed by
      # (method, url) alone - the httptest model - every poll of a run status
      # returns the same answer forever and Purpose B cannot express the run
      # ever finishing. See IMR-293.
      seq          = recorderEnv$seq,
      ts           = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"),
      testCase     = Sys.getenv("TEST_NAME", ""),
      method       = method,
      urlTemplate  = urlTemplate,
      url          = url,
      requestBody  = redactBody(data),
      status       = if (is.null(result)) NULL else result$status_code,
      responseHeaders = if (is.null(result)) NULL else safeHeaders(result),
      responseBody = if (is.null(result)) NULL else safeBody(result),
      durationMs   = round(as.numeric(difftime(Sys.time(), startedAt,
                                               units = "secs")) * 1000)
    )

    line <- jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null",
                             digits = NA)
    con <- file(file.path(dir, "interactions.jsonl"), open = "at",
                encoding = "UTF-8")
    on.exit(close(con), add = TRUE)
    writeLines(line, con)
  },
  # A warning escapes tryCatch(error=), and file() warns before it fails. An
  # escaped warning is not "cannot change the verdict": testthat counts it, and
  # under options(warn = 2) it would fail a test outright. Muffled here, kept in
  # the debug log. Found by test-restRecorder.R, which is what it is for.
  warning = function(w) {
    tryCatch(log_debug("restRecorder: ", conditionMessage(w)),
             error = function(e2) NULL)
    invokeRestart("muffleWarning")
  }), error = function(e) {
    # Deliberately silent beyond a debug line. A recorder that can fail a
    # qualification run is worse than no recorder (FR-RPL-014).
    tryCatch(log_debug("restRecorder: could not record interaction: ",
                       conditionMessage(e)), error = function(e2) NULL)
    NULL
  })
  invisible(NULL)
}

#' Reset the ordinal, for a fresh recording
#' @noRd
resetRestRecorder <- function() {
  recorderEnv$seq <- 0L
  invisible(NULL)
}
