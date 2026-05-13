#' Perform a GET request and return the parsed result as a data frame
#'
#' Wraps the common pattern of \code{authenticatedREST} + null check +
#' \code{httr::content} + \code{mergeListToDataframe}. Returns \code{NULL}
#' when the request fails or yields an empty result.
#'
#' @param url REST endpoint URL template (e.g. \code{"/resources/\{resourceId\}/acl"}).
#' @param urlParams Named list of URL parameter substitutions.
#' @param queryParams Named list of query parameters.
#' @param elementsKey Optional character key to extract from the response before
#'   merging (e.g. \code{"elements"}).  When \code{NULL} the full content list
#'   is used.
#' @param nested If \code{TRUE}, use \code{mergeNestedListToDataframe} instead
#'   of \code{mergeListToDataframe}.
#' @param dates If \code{TRUE}, apply \code{convertDates()} to the result.
#' @returns A data frame (possibly with zero rows), or \code{NULL} on failure.
#' @noRd
restGetAsDf <- function(url,
                        urlParams    = list(),
                        queryParams  = list(),
                        elementsKey  = NULL,
                        nested       = FALSE,
                        dates        = FALSE) {
  result <- authenticatedREST(url, urlParams = urlParams, queryParams = queryParams)
  if (is.null(result)) return(NULL)

  cont <- httr::content(result)

  if (!is.null(elementsKey)) {
    cont <- cont[[elementsKey]]
  }

  if (is.null(cont)) return(NULL)

  df <- if (nested) mergeNestedListToDataframe(cont) else mergeListToDataframe(cont)

  if (is.null(df)) return(NULL)
  if (nrow(df) == 0) return(NULL)

  if (dates) df <- convertDates(df)

  return(df)
}

#' Resolve an identifier to a resourceId UUID
#'
#' Accepts data frames (extracts \code{$resourceId}), UUID strings (returned
#' as-is), or any identifier that \code{loadResource()} can resolve.
#'
#' @param ident Identifier — can be a path, entityId, resourceId string, or
#'   a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns Character resourceId UUID. Returns the input unchanged as a
#'   last resort when resolution fails (to allow callers to use it as a
#'   cache key without crashing).
#' @noRd
resolveToResourceId <- function(ident, from = pwd()) {
  if (is.data.frame(ident) && "resourceId" %in% names(ident)) {
    return(ident$resourceId[1])
  }
  # If it looks like a UUID already (32 hex chars), use directly
  if (is.character(ident) && length(ident) == 1 && grepl("^[A-Fa-f0-9]{32}$", ident)) {
    return(ident)
  }
  # Otherwise resolve via loadResource
  res <- loadResource(ident, from)
  if (!is.null(res) && "resourceId" %in% names(res)) {
    return(res$resourceId[1])
  }
  # Last resort: return as-is (may be a prefixed UUID like "hc4310:ABC...")
  return(ident)
}
