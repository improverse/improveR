#' helper function to validate the review entry's existence
#' @param reviewId id (UUID) of the review
#' @param entryId id (UUID) of the review entry whose existence is to be checked
#' @noRd
validateEntry <- function(reviewId, entryId) {
  reviewEntries <- refreshReviewEntries(reviewId)
  if (is.null(reviewEntries) || nrow(reviewEntries) == 0) {
    log_error("No review entries exist for the review with the id:", reviewId)
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
#'
#' Adds a comment to a specific entry within a review.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param entryId Character. ID (UUID) of the review entry.
#' @param comment Character. The comment text.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of the entry's current comments, or \code{NULL} on failure.
#' @references ics1544
#' @export
createReviewEntryComment <- function(ident, entryId, comment, from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find review by ident:", ident)
    return(NULL)
  }
  reviewId <- resource$resourceId

  if (!validateReview(reviewId) || !validateEntry(reviewId, entryId)) {
    return(NULL)
  }

  data <- list("comment" = comment)

  result <- authenticatedREST("/reviews/{reviewId}/entries/{entryId}/comments", urlParams = list(reviewId = reviewId, entryId = entryId), data = data, restType = "POST")
  # getReviewEntryComments, not refresh: the family never had a cache, so
  # refresh was unload (a discarded server call) plus load - two round trips
  # for one result. The three hollow functions are gone with IMR-278.
  entryComments <- getReviewEntryComments(reviewId, entryId)

  return(entryComments)
}
