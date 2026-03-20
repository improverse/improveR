#' Finish Resource
#'
#' Sets a resource to the "finished" state, preventing further modifications
#' until it is reopened.
#'
#' @param ident Resource identifier (path, resource ID, entity ID, etc.).
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the operation succeeded, \code{FALSE} otherwise.
#' @references ics1810
#' @export
finishResource <- function(ident, from = pwd()) {
  improveEditable()
  res <- loadResource(ident, from)
  if (is.null(res)) {
    log_warn("Resource", ident, "does not exist")
    return(FALSE)
  }
  result <- authenticatedREST("/resources/{resourceId}/finish",
                              urlParams = list(resourceId = res$resourceId),
                              restType = "PUT")
  if (!is.null(result)) {
    unloadResource(res$resourceId)
    return(TRUE)
  }
  log_warn("Failed to finish resource:", ident)
  return(FALSE)
}

#' Reopen Resource
#'
#' Reopens a resource that was previously finished, allowing modifications again.
#'
#' @param ident Resource identifier (path, resource ID, entity ID, etc.).
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the operation succeeded, \code{FALSE} otherwise.
#' @references ics1811
#' @export
reopenResource <- function(ident, from = pwd()) {
  improveEditable()
  res <- loadResource(ident, from)
  if (is.null(res)) {
    log_warn("Resource", ident, "does not exist")
    return(FALSE)
  }
  result <- authenticatedREST("/resources/{resourceId}/reopen",
                              urlParams = list(resourceId = res$resourceId),
                              restType = "PUT")
  if (!is.null(result)) {
    unloadResource(res$resourceId)
    return(TRUE)
  }
  log_warn("Failed to reopen resource:", ident)
  return(FALSE)
}
