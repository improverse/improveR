# Guards shared by isResourceUp2Date and isFileUp2Date (IMR-287).
#
# Both functions used to hand every value straight on to the next call. When
# loadResource answered NULL - which is how it reports a failed read - the next
# line was `if (res$isVersion)`, and `if (NULL)` is where the caller's error
# came from: "argument is of length zero", naming nothing.
#
# The quieter case was the comparison itself. `NULL == "x"` is logical(0), so a
# predicate could return a zero-length value that is neither TRUE nor FALSE nor
# an error. `if (isResourceUp2Date(x))` then failed at the call site, arbitrarily
# far from the read that actually went wrong.

#' Describe why a REST-backed read came back empty
#'
#' @noRd
up2DateRestDetail <- function() {
  err <- tryCatch(lastRestError(), error = function(e) NULL)
  if (is.null(err)) {
    return("improveR recorded no REST error")
  }
  sprintf("HTTP %s on %s %s", err$status_code, err$method, err$url)
}

#' Resolve exactly one resource, or stop saying why not
#'
#' @param ident The identifier the caller passed.
#' @param from Base for relative paths.
#' @param fn Name of the calling function, used in the message.
#' @returns A one-row resource data frame. Never NULL, never more than one row.
#' @noRd
up2DateResolveOne <- function(ident, from, fn) {
  res <- loadResource(ident, from)
  identText <- paste(utils::head(as.character(ident), 3), collapse = ", ")
  if (is.null(res) || nrow(res) == 0) {
    stop(sprintf("%s: '%s' does not resolve to a resource - %s",
                 fn, identText, up2DateRestDetail()), call. = FALSE)
  }
  if (nrow(res) > 1) {
    stop(sprintf("%s: '%s' resolves to %d resources; it takes exactly one",
                 fn, identText, nrow(res)), call. = FALSE)
  }
  res
}

#' Read a resource from the server, or stop saying why not
#'
#' @noRd
up2DateServerVersion <- function(resourceId, ident, fn) {
  serverResource <- loadResourceFromServer(resourceId)
  if (is.null(serverResource) || is.null(serverResource$entityVersionId) ||
      length(serverResource$entityVersionId) != 1) {
    stop(sprintf("%s: '%s' resolved locally but could not be read from the server - %s",
                 fn, paste(utils::head(as.character(ident), 3), collapse = ", "),
                 up2DateRestDetail()), call. = FALSE)
  }
  as.character(serverResource$entityVersionId)
}
