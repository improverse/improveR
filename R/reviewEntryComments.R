#' helper function to validate the review entry's existence
#' @param reviewId id (UUID) of the review
#' @param entryId id (UUID) of the review entry whose existence is to be checked
#' @noRd
validateEntry <- function(reviewId, entryId) {
  reviewEntries <- updateReviewEntries(reviewId)
  if (is.null(reviewEntries) || nrow(reviewEntries) == 0) {
    log_error("No reviewer exists for the review with the id:", reviewId)
    return(FALSE)
  } else if (!"id" %in% colnames(reviewEntries)) {
    log_error("The 'id' column does not exist in the 'reviewEntries' data frame")
    return(FALSE)
  }

  filteredReviewEntry <- reviewEntries[!is.na(reviewEntries$id) & reviewEntries$id == entryId,]
  if (is.null(filteredReviewEntry) || nrow(filteredReviewEntry) == 0) {
    log_error("The review entry with the id:", entryId, "does not exist for the review with the id:", reviewId)
    return(FALSE)
  } else if (nrow(filteredReviewEntry) > 1) {
    log_error("Found more than one review entry with the id:", entryId, "for the review with the id:", reviewId)
    return(FALSE)
  }

  return(TRUE)
}

#' Creates a Comment for a Review Entry
#' @param reviewId id (UUID) of the review
#' @param entryId id (UUID) of the resource
#' @param comment comment
#' @references ics1544
#' @export
createReviewEntryComment <- function(reviewId, entryId, comment) {
  improveEditable()

  if (!validateReview(reviewId) || !validateEntry(reviewId, entryId)) {
    return(NULL)
  }

  data <- list("comment" = comment)

  result <- authenticatedREST("/reviews/{reviewId}/entries/{entryId}/comments", urlParams = list(reviewId = reviewId, entryId = entryId), data = data, restType = "POST")
  updateReviewEntryComments(reviewId, entryId)

  return(result)
}
