#' Creates Review Entries For A Review
#' @param reviewId id (UUID) of the review
#' @param resourceIds vector of resourceIds
#' @references ics1541
#' @export
createReviewEntry <- function(reviewId, resourceIds) {
  improveEditable()

  if (!validateReview(reviewId) || any(!sapply(resourceIds, validateResource))) {
    return(NULL)
  }

  data <- list("resourceIds" = resourceIds)

  result <- authenticatedREST("/reviews/{reviewId}/entries", urlParams = list(reviewId = reviewId), data = data, restType = "POST")
  updateReviewEntries(reviewId)

  return(result)
}

#' Deletes All Review Entries From A Review
#' @param reviewId id (UUID) of the review
#' @references ics365
#' @export
deleteReviewEntry <- function(reviewId) {
  improveEditable()

  if (!validateReview(reviewId)) {
    return(NULL)
  }

  result <- authenticatedREST("/reviews/{reviewId}/reviewers", urlParams = list(reviewId = reviewId), restType = "DELETE")
  updateReviewEntries(reviewId)

  return(result)
}
