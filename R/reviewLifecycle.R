#' Get Review By ID
#'
#' Retrieves a single review resource by its identifier.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame with the review resource details, or \code{NULL} if not found.
#' @references ics1528
#' @export
getReviewById <- function(ident, from = pwd()) {
  improveConnected()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find review by ident:", ident)
    return(NULL)
  }
  result <- authenticatedREST("/reviews/{reviewId}",
                              urlParams = list(reviewId = resource$resourceId),
                              restType = "GET")
  if (is.null(result)) {
    log_warn("failed to retrieve review details for ident:", ident)
    return(NULL)
  }
  cont <- httr::content(result)
  if (is.null(cont)) {
    log_warn("review response contained no content for ident:", ident)
    return(NULL)
  }
  # Flatten requestor fields and strip nested lists that break as.data.frame
  if (!is.null(cont$requestor)) {
    cont$requestorId <- cont$requestor$id
    cont$requestorName <- cont$requestor$name
    cont$requestorUsername <- cont$requestor$username
  }
  cont <- cont[setdiff(names(cont), c("comments", "entries", "requestor"))]
  df <- as.data.frame(cont, stringsAsFactors = FALSE)
  return(df)
}

#' Accept Review Invitation
#'
#' Accepts a review invitation as a reviewer. The review must be in
#' \code{"Planning"} state and the current user must be an invited reviewer.
#' After accepting, the reviewer can comment on and approve/reject entries
#' once the review transitions to \code{"Reviewing"}.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param comment Optional plain text comment.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the invitation was accepted, \code{FALSE} otherwise.
#' @references ics1476
#' @export
acceptReviewInvitation <- function(ident, comment = "", from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find review by ident:", ident)
    return(FALSE)
  }
  result <- authenticatedREST("/reviews/{reviewId}/accept",
                              urlParams = list(reviewId = resource$resourceId),
                              data = comment,
                              restType = "POST",
                              contentType = "text/plain",
                              encode = "raw")
  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("failed to accept review invitation for ident:", ident)
  return(FALSE)
}


#' Decline Review Invitation
#'
#' Declines a review invitation as a reviewer. The review must be in
#' \code{"Planning"} state and the current user must be an invited reviewer.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param comment Optional plain text comment.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the invitation was declined, \code{FALSE} otherwise.
#' @references ics1477
#' @export
declineReviewInvitation <- function(ident, comment = "", from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find review by ident:", ident)
    return(FALSE)
  }
  result <- authenticatedREST("/reviews/{reviewId}/decline",
                              urlParams = list(reviewId = resource$resourceId),
                              data = comment,
                              restType = "POST",
                              contentType = "text/plain",
                              encode = "raw")
  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("failed to decline review invitation for ident:", ident)
  return(FALSE)
}


#' Change Review Status
#'
#' Changes the status of a review. Reviews have three statuses with the
#' following transitions:
#'
#' \describe{
#'   \item{\strong{Planning}}{Initial state. Entries and reviewers are being set up.}
#'   \item{\strong{Reviewing}}{Active review. Reviewers can approve/reject entries.
#'     Requires at least one entry and one reviewer.}
#'   \item{\strong{Approved}}{Final/locked state. Both the review and all reviewed
#'     resources are locked. Only an admin can revert to Reviewing.}
#' }
#'
#' Valid transitions:
#' \itemize{
#'   \item \code{Planning -> Reviewing} (requestor or admin, needs entries + reviewer)
#'   \item \code{Reviewing -> Approved} (requestor or admin, all entries must be reviewed, no rejections)
#'   \item \code{Reviewing -> Planning} (requestor or admin, resets all approvals)
#'   \item \code{Approved -> Reviewing} (admin only, reopens a locked review)
#' }
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param newStatus Character. The target status: \code{"Reviewing"}, \code{"Approved"},
#'   or \code{"Planning"}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the status was changed successfully, \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' # Start the review process
#' changeReviewStatus(review, "Reviewing")
#'
#' # Approve and lock the review (all entries must be reviewed first)
#' changeReviewStatus(review, "Approved")
#'
#' # Revert to planning (resets approvals)
#' changeReviewStatus(review, "Planning")
#' }
#'
#' @references ccs27
#' @export
changeReviewStatus <- function(ident, newStatus, from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find review by ident:", ident)
    return(FALSE)
  }
  result <- authenticatedREST("/reviews/{reviewId}/status/{newStatus}",
                              urlParams = list(reviewId = resource$resourceId,
                                               newStatus = newStatus),
                              data = list(),
                              restType = "POST")
  if (!is.null(result)) {
    refreshResource(resource$resourceId)
    return(TRUE)
  }
  log_warn("failed to change review status to '", newStatus, "' for ident:", ident)
  return(FALSE)
}
