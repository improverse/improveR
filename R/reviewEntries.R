#' Creates Review Entries For A Review
#'
#' Adds one or more resources as entries to an existing review.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param resourceIds Character vector of resource IDs (UUIDs) to add as review entries.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of the review's current entries, or \code{NULL} on failure.
#' @references ics1541
#' @export
createReviewEntry <- function(ident, resourceIds, from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find review by ident:", ident)
    return(NULL)
  }
  reviewId <- resource$resourceId

  if (!validateReview(reviewId) || any(!sapply(resourceIds, validateResource))) {
    return(NULL)
  }

  data <- list("resourceIds" = resourceIds)

  result <- authenticatedREST("/reviews/{reviewId}/entries", urlParams = list(reviewId = reviewId), data = data, restType = "POST")
  entries <- refreshReviewEntries(reviewId)

  return(entries)
}

#' Deletes All Review Entries From A Review
#'
#' Removes all entries from an existing review.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the entries were removed successfully, \code{FALSE} otherwise.
#' @references ics365
#' @export
deleteReviewEntry <- function(ident, from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find review by ident:", ident)
    return(FALSE)
  }
  reviewId <- resource$resourceId

  if (!validateReview(reviewId)) {
    return(FALSE)
  }

  result <- authenticatedREST("/reviews/{reviewId}/entries", urlParams = list(reviewId = reviewId), restType = "DELETE")
  refreshReviewEntries(reviewId)

  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to delete review entries for review:", reviewId)
  return(FALSE)
}
