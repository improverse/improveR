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
#'
#' Adds a comment to a review. The review must be in the \code{"Reviewing"} state.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param resourceIdent Identifier of the resource the comment relates to. Can be
#'   a path, resource ID, entity ID, or a data frame row from \code{loadResource()}.
#' @param comment Character. The comment text.
#' @param commentType Character. Type of the comment.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of the review's current comments, or \code{NULL} on failure.
#' @references ics1536
#' @export
createReviewComment <- function(ident, resourceIdent, comment, commentType, from = pwd()) {
  improveEditable()
  reviewResource <- loadResource(ident, from)
  if (is.null(reviewResource)) {
    log_warn("cannot find review by ident:", ident)
    return(NULL)
  }
  reviewId <- reviewResource$resourceId

  targetResource <- loadResource(resourceIdent, from)
  if (is.null(targetResource)) {
    log_warn("cannot find resource by ident:", resourceIdent)
    return(NULL)
  }
  resourceId <- targetResource$resourceId

  if (!validateReview(reviewId) || !validateReviewState(reviewId)) {
    return(NULL)
  }

  data <- list("resourceId" = resourceId,
               "comment" = comment,
               "commentType" = commentType)

  result <- authenticatedREST("/reviews/{reviewId}/comments", urlParams = list(reviewId = reviewId), data = data, restType = "POST")
  comments <- updateReviewComments(reviewId)

  return(comments)
}
