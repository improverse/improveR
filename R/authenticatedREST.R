# The functions themselves, not their names. They used to be strings, resolved
# on every call by eval(parse(text = ...)) with the comment "fix for httptest to
# work": httptest works by tracing httr::POST, and a function bound once at load
# time would never see the trace.
#
# ENT-05 chose Karate over httptest, and the recording hook (IMR-293) sits INSIDE
# authenticatedREST rather than tracing httr - so nothing traces these any more,
# and nothing needs late binding. What the indirection cost was an eval(parse())
# on every REST call in the library (IMR-303).
REST_FUNCTIONS <- list(POST   = httr::POST,
                       GET    = httr::GET,
                       PUT    = httr::PUT,
                       DELETE = httr::DELETE)

restEnv <- new.env()
restEnv$lastRestError <- NULL
restEnv$httpHandle <- NULL
restEnv$httpHandleBase <- NULL

#' Reuse a single httr/curl handle across REST calls so the underlying TCP+TLS
#' connection stays open. httr has an automatic handle_pool keyed by hostname,
#' but passing an explicit handle is more deterministic -- without it we have
#' seen new TCP/TLS handshakes added to short bulk-create paths (e.g. realise()
#' uploading 100+ inputFiles to a freshly created step). Handle is recreated
#' if the repository base URL changes (e.g. after re-connect to a different
#' server) so a stale handle never bleeds across sessions.
#' @noRd
getOrCreateRestHandle <- function(baseUrl) {
  if (is.null(restEnv$httpHandle) ||
      !identical(restEnv$httpHandleBase, baseUrl)) {
    restEnv$httpHandle <- httr::handle(baseUrl)
    restEnv$httpHandleBase <- baseUrl
  }
  return(restEnv$httpHandle)
}

# timing() liegt in prepareStep.R. Hier stand eine zweite, ebenfalls leere
# Fassung, die von der dortigen verdeckt wurde (IMR-273).

# Read the body of a REST response, or report why there is none.
#
# authenticatedREST() returns NULL for every non-2xx. Handing that straight to
# httr::content() aborts with "is.response(x) is not TRUE" - a message naming
# neither the status, nor the URL, nor the operation. Measured on 2026-09-09:
# 14 of 66 call sites did exactly that, and one of them (setGridArguments.R)
# took down a test that had nothing to do with it (IMR-270).
#
# Returns NULL on a failed call, after logging the diagnostic lastRestError()
# has been carrying all along. Callers that already treat NULL as "nothing
# there" keep working unchanged; callers that cannot use NULL now have to check
# it, which is the point.
restContent <- function(result, what, as = NULL) {
  if (is.null(result)) {
    err <- lastRestError()
    if (is.null(err)) {
      log_warn(what, " failed: the REST call returned nothing and no error was recorded")
    } else {
      log_warn(what, " failed: HTTP ", err$status_code, " on ", err$method, " ", err$url)
    }
    return(NULL)
  }
  if (is.null(as)) httr::content(result) else httr::content(result, as = as)
}

#' Last REST Error
#'
#' @description Returns details about the last failed REST call, or NULL if the last call succeeded.
#' Since improveR is single-threaded, this is safe to use after any REST operation.
#' @returns A list with \code{status_code}, \code{url}, \code{method}, \code{message},
#' and \code{timestamp}, or \code{NULL} if the last call was successful.
#' @export
lastRestError <- function() {
  return(restEnv$lastRestError)
}

#' @noRd
setLastRestError <- function(status_code, url, method, message) {
  restEnv$lastRestError <- list(
    status_code = status_code,
    url = url,
    method = method,
    message = message,
    timestamp = Sys.time()
  )
}

#' @noRd
clearLastRestError <- function() {
  restEnv$lastRestError <- NULL
}

#' authenticatedREST
#'
#' @description authenticatedREST uses the connection information from improveConnect to create a REST call to the repository.
#' The values below are example values.
#' @param url in the format /resources/\{resourceId\}
#' @param urlParams in the format list(resourceId="1B1D3B817F424BA594893A5013DBFEEA")
#' @param queryParams in the format list(isResourceVersion="true")
#' @param data this block is written to the body, either list, binary or json
#' @param restType character, default is GET, possible values are POST,GET,PUT and DELETE
#' @param contentType is directly set to the header, default is application/json, used to specify the format for data
#' @param encode is directly set to the header, default is json, should be fine for most rest calls, for file uploads use NULL
#'
#' In this example the following URL would be constructed:
#'   baseURL from improveConnect /resources/1B1D3B817F424BA594893A5013DBFEEA?isResourceVersion=true
#'
#' On success (HTTP 2xx), returns the httr response object.
#'
#' On any non-2xx response (including 404, 401, 417, other 4xx, 5xx) returns
#' NULL -- the existing contract callers rely on. Diagnostic detail is
#' captured on every non-2xx and is accessible via \code{lastRestError()}:
#' status code, URL, REST method, a snippet of the response body (up to
#' ~500 chars), and a timestamp. Failures are also logged with the URL +
#' method + status + body snippet in the message -- \code{log_error} for
#' 5xx, \code{log_warn} for other non-2xx -- so the failing call is
#' identifiable at the log site.
#'
#' Callers that need to distinguish "resource absent" (404) from other
#' failures should check \code{lastRestError()$status_code} after seeing
#' a NULL return.
#' @seealso [improveConnect()], [lastRestError()]
#' @references ics1082
#' @keywords internal
#' @noRd
authenticatedREST <- function(url,urlParams=list(),queryParams=list(),data="",restType="GET",contentType="application/json",encode="json") {
  timing(url)
  if (is.null(encode)) {
    encode <- c("multipart","form", "json", "raw")
  }

  if (startsWith(url,"/")) {
    url <- substr(url,2,nchar(url))
  }
  if (!(restType) %in% names(REST_FUNCTIONS)) {
    log_error("trying to use an undefined REST method: ", restType, " (URL: ", url, ")")
    setLastRestError(NA, url, restType, paste0("undefined REST method '", restType, "'"))
    return(NULL)
  }
  restFunction <- REST_FUNCTIONS[restType][[1]]

  # FR-RPL-012: the template is kept beside the concrete URL, because the
  # template is the form that resolves against a server specification
  # (REQ-CLIOQ-001 FR-CLI-002). It must be taken BEFORE substitution.
  recUrlTemplate <- url
  recStartedAt <- Sys.time()

  url <- replacePlaceHoldersinURL(url,urlParams)
  url <- appendQueryParams(url,queryParams)
  fullUrl <- paste0(conf()$repoUrl,url)
  log_debug(restType, "connecting to", fullUrl)

  # Display REST calls if environment variable is set
  if (Sys.getenv("IMPROVER_DISPLAY_REST_CALLS", "") != "") {
    cat(paste0("REST Call: ", restType, " ", fullUrl, "\n"))
    if (!identical(data, "")) {
      # Check if data is binary (raw type or contains file upload)
      if (is.raw(data) || (is.list(data) && any(sapply(data, function(x) inherits(x, "form_file"))))) {
        cat("Data: <binaryBlob>\n")
      } else {
        cat("Data: ", jsonlite::toJSON(data, auto_unbox = TRUE, pretty = TRUE), "\n")
      }
    }
  }

  result <- NULL
  reuseHandle <- getOrCreateRestHandle(conf()$repoUrl)
  if (!is.na(conf()$reqToken) && !is.null(conf()$reqToken)) {
    log_debug("TokenAuth")
    if (!grepl("refreshToken",fullUrl)) {
      log_debug("Check refresh Token")
      refreshToken()
    } else {
      log_debug("Skipping refresh for token endpoint")
    }

    result <- restFunction(fullUrl, body = data,
                           httr::add_headers('Authorization' = paste0("Bearer ",conf()$reqToken)),
                                             encode=encode,
                                             'Content-Type' = contentType,
                           handle = reuseHandle)


  } else {
    log_debug("User Auth")
    result <- restFunction(fullUrl, body = data,
                           httr::authenticate(conf()$user,conf()$password),
                           encode=encode,
                           'Content-Type' = contentType,
                           handle = reuseHandle)
  }
  if ((result$status_code>=200 && result$status_code<300)) {
    log_debug(result$status_code, fullUrl)
    clearLastRestError()
    recordRestInteraction(recUrlTemplate, fullUrl, restType, data, result, recStartedAt)
    timing("done")
    return(result)
  }
  # Non-2xx: contract is to return NULL. We additionally capture the
  # response body snippet, log the failure with full context (method +
  # URL + status + body), and record the same in setLastRestError() so
  # `lastRestError()` exposes WHY a NULL came back.
  body_snippet <- tryCatch(
    substr(as.character(httr::content(result, as = "text", encoding = "UTF-8")),
           1, 500),
    error = function(e) "<unreadable body>"
  )
  if (!nzchar(body_snippet)) body_snippet <- "<empty body>"

  diag <- paste0(restType, " ", fullUrl,
                 " -> HTTP ", result$status_code, ": ", body_snippet)

  if (result$status_code == 401) {
    log_error("Invalid authentication credentials (token or username/password) \u2014 ", diag)
    setLastRestError(result$status_code, fullUrl, restType,
                     paste0("Invalid authentication credentials: ", body_snippet))
  } else if (result$status_code == 417) {
    log_error("Resource could not be run \u2014 ", diag)
    setLastRestError(result$status_code, fullUrl, restType,
                     paste0("Resource could not be run: ", body_snippet))
  } else if (result$status_code >= 500) {
    log_error("Server error \u2014 ", diag)
    setLastRestError(result$status_code, fullUrl, restType, diag)
  } else {
    # other 4xx (404, 403, 410, 422, ...)
    log_warn(diag)
    setLastRestError(result$status_code, fullUrl, restType, diag)
  }
  # Non-2xx is recorded too: a replay that only carries successes cannot
  # reproduce the error paths the OQ asserts on.
  recordRestInteraction(recUrlTemplate, fullUrl, restType, data, result, recStartedAt)
  return(NULL)
}

replacePlaceHoldersinURL <- function(url,urlParams) {
  if (length(urlParams)>0) {
    for (i in 1:length(urlParams)) {
      urlParamName <- paste0("{",names(urlParams[i]),"}")
      urlParamValue <- as.character(urlParams[i][1])
      url<-gsub(urlParamName,urlParamValue,url,fixed=TRUE)
    }
  }
  return(url)
}

appendQueryParams <- function(url,queryParams) {
  queryParamFormatted <- lapply(names(queryParams),function(param) {
    paste0(param,"=",as.character(queryParams[param]))
  })
  queryParamFormattedString <- paste(queryParamFormatted,collapse = "&")
  queryParamFormattedString <- paste(url,queryParamFormattedString,sep="?")
}
