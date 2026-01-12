#' helper function to validate the state of the review
#' @param reviewId id (UUID) of the review
#' @noRd
validateReviewState <- function(reviewId) {
  reviews <- updateReviews()
  if (!"reviewStatus" %in% colnames(reviews)) {
    log_error("The 'reviewStatus' column does not exist in the 'reviews' data frame")
    return(FALSE)
  }

  filteredReview <- reviews[!is.na(reviews$id) & reviews$id == reviewId,]
  if (filteredReview$reviewStatus != "Reviewing") {
    log_error("The review with the id:", reviewId, "is not in the state 'Reviewing'")
    return(FALSE)
  }

  return(TRUE)
}

#' Creates a Comment for a Review
#' @param reviewId id (UUID) of the review
#' @param resourceId id (UUID) of the resource
#' @param comment comment
#' @param commentType type of the comment
#' @references ics1536
#' @export
createReviewComment <- function(reviewId, resourceId, comment, commentType) {
  improveEditable()

  if (!validateReview(reviewId) || !validateResource(resourceId) || !validateReviewState(reviewId)) {
    return(NULL)
  }

  data <- list("resourceId" = resourceId,
               "comment" = comment,
               "commentType" = commentType)

  result <- authenticatedREST("/reviews/{reviewId}/comments", urlParams = list(reviewId = reviewId), data = data, restType = "POST")
  updateReviewComments(reviewId)

  return(result)
}
