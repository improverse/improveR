#' Add Review Entries
#'
#' Adds one or more resources as entries to an existing review.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param resourceIds Character vector of resource IDs (UUIDs) to add as review entries.
#' @param followLinks Logical. If \code{TRUE}, link targets are also added. Defaults to \code{FALSE}.
#' @param recursiveAdd Logical. If \code{TRUE}, children of folders are added recursively.
#'   Defaults to \code{FALSE}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of the review's current entries, or \code{NULL} on failure.
#' @references ics1541
#' @export
createReviewEntry <- function(ident, resourceIds, followLinks = FALSE, recursiveAdd = FALSE, from = pwd()) {
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
  queryParams <- list()
  if (isTRUE(followLinks)) queryParams$followLinks <- "true"
  if (isTRUE(recursiveAdd)) queryParams$recursiveAdd <- "true"

  result <- authenticatedREST(
    "/reviews/{reviewId}/entries",
    urlParams = list(reviewId = reviewId),
    queryParams = queryParams,
    data = data,
    restType = "POST"
  )
  entries <- refreshReviewEntries(reviewId)

  return(entries)
}

#' Delete Review Entries
#'
#' Removes specific entries from an existing review by their review entry IDs.
#' Use \code{getReviewEntries()$id} to get the entry IDs.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param reviewEntryIds Character vector of review entry IDs (UUIDs) to remove.
#'   These are the entry IDs (from \code{getReviewEntries()$id}), not resource IDs.
#' @param comment Character. Optional comment for the deletion. Defaults to empty string.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the entries were removed successfully, \code{FALSE} otherwise.
#' @references ics1542
#' @export
deleteReviewEntry <- function(ident, reviewEntryIds, comment = "", from = pwd()) {
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

  data <- list(
    "reviewEntryIds" = reviewEntryIds,
    "comment" = comment
  )

  result <- authenticatedREST(
    "/reviews/{reviewId}/entries",
    urlParams = list(reviewId = reviewId),
    data = data,
    restType = "DELETE"
  )
  refreshReviewEntries(reviewId)

  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to delete review entries for review:", reviewId)
  return(FALSE)
}

#' Approve Review Entries
#'
#' Approves one or more entries in a review. The review must be in "Reviewing" state.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param reviewEntryIds Character vector of review entry IDs (UUIDs) to approve.
#'   These are the entry IDs (from \code{getReviewEntries()$id}), not resource IDs.
#' @param comment Character. Optional comment for the approval.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the entries were approved successfully, \code{FALSE} otherwise.
#' @references ics1545
#' @export
approveReviewEntries <- function(ident, reviewEntryIds, comment = "", from = pwd()) {
  reviewEntryAction(ident, reviewEntryIds, comment, "approve", from)
}

#' Reject Review Entries
#'
#' Rejects one or more entries in a review. The review must be in "Reviewing" state.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param reviewEntryIds Character vector of review entry IDs (UUIDs) to reject.
#'   These are the entry IDs (from \code{getReviewEntries()$id}), not resource IDs.
#' @param comment Character. Optional comment for the rejection.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the entries were rejected successfully, \code{FALSE} otherwise.
#' @references ics1546
#' @export
rejectReviewEntries <- function(ident, reviewEntryIds, comment = "", from = pwd()) {
  reviewEntryAction(ident, reviewEntryIds, comment, "reject", from)
}

#' Reset Review Entry Approval State
#'
#' Resets the approval state of one or more entries in a review.
#' The review must be in "Reviewing" state.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param reviewEntryIds Character vector of review entry IDs (UUIDs) to reset.
#'   These are the entry IDs (from \code{getReviewEntries()$id}), not resource IDs.
#' @param comment Character. Optional comment for the reset.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the entries were reset successfully, \code{FALSE} otherwise.
#' @references ics1547
#' @export
resetReviewEntries <- function(ident, reviewEntryIds, comment = "", from = pwd()) {
  reviewEntryAction(ident, reviewEntryIds, comment, "reset", from)
}

#' Internal: perform an action (approve/reject/reset) on review entries
#' @noRd
reviewEntryAction <- function(ident, reviewEntryIds, comment, action, from = pwd()) {
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

  data <- list(
    "reviewEntryIds" = reviewEntryIds,
    "comment" = comment
  )

  result <- authenticatedREST(
    "/reviews/{reviewId}/entries/{action}",
    urlParams = list(reviewId = reviewId, action = action),
    data = data,
    restType = "POST"
  )
  refreshReviewEntries(reviewId)

  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to ", action, " review entries for review:", reviewId)
  return(FALSE)
}
