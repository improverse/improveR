# Decide what a NULL return from an improveR call actually means (IMR-267).
#
# The suite used to conclude "not supported on this server" from a NULL result:
#
#   if (is.null(result)) stop("setResourcePermission not supported on this server")
#
# That is not a conclusion the caller can draw. authenticatedREST() returns NULL
# for EVERY non-2xx - 400, 401, 403, 404, 417, 5xx - so a NULL says only that
# something went wrong. Measured on 2026-09-04 against improve 4.4.5-4, the 44
# HTTP errors behind such messages were 25x 403, 18x 400 and 1x 404. Not one of
# them meant "not supported", yet all of them were written into the run record
# as a statement about the server.
#
# improveR::lastRestError() has carried the status code, URL, method and body
# snippet of the last failure all along, and no test read it.
#
# The skip/fail split is deliberate:
#   404      -> the endpoint genuinely is not there. That is a real and useful
#               compatibility statement about this server version, so the check
#               is skipped with the status code in the reason.
#   401/403  -> the run's identity lacks a right. That is a misconfigured run,
#               not a server property, and must never disappear silently.
#   4xx/5xx  -> a defect somewhere. Fails.
#
# Returns the result invisibly when the call succeeded, so it can be chained.

# Accepts both shapes the package uses to signal failure: NULL from the
# loaders, FALSE from lockResource()/finishResource() and friends.
#
# cleanup is run only on failure, and only AFTER the REST error has been read.
# It must not be done the other way round: lastRestError() holds the last
# failure, and a cleanup call that itself fails - deleteGroup(NULL$id) hits
# .../groups/NULL - overwrites it, so the message would name the teardown
# instead of the call under test.
requireServerCall <- function(result, what, cleanup = NULL) {
  if (!is.null(result) && !isFALSE(result)) {
    return(invisible(result))
  }

  err <- improveR::lastRestError()
  if (is.function(cleanup)) {
    try(cleanup(), silent = TRUE)
  }
  if (is.null(err)) {
    stop(sprintf(paste0("%s returned NULL and improveR recorded no REST error. ",
                        "The call may not have been issued at all."), what),
         call. = FALSE)
  }

  status <- suppressWarnings(as.integer(err$status_code))
  detail <- sprintf("%s: HTTP %s on %s %s - %s",
                    what, err$status_code, err$method, err$url,
                    substr(as.character(err$message), 1, 200))

  if (!is.na(status) && status == 404L) {
    testthat::skip(paste0("not offered by this server (HTTP 404) - ", detail))
  }
  if (!is.na(status) && status %in% c(401L, 403L)) {
    stop(sprintf(paste0("%s\nThe run's identity is not permitted to do this. ",
                        "This is a run configuration problem, not a missing ",
                        "server feature - see the required identities in ",
                        ".Renviron (IMR-267)."), detail),
         call. = FALSE)
  }
  stop(detail, call. = FALSE)
}
