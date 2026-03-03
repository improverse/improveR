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

#' Accept Review
#'
#' Accepts a review that is currently in the reviewing state. Sends a plain
#' text comment as the request body.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param comment Optional plain text comment for the acceptance.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the review was accepted successfully, \code{FALSE} otherwise.
#' @references ics1476
#' @export
acceptReview <- function(ident, comment = "", from = pwd()) {
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
  log_warn("failed to accept review for ident:", ident)
  return(FALSE)
}

#' Decline Review
#'
#' Declines a review that is currently in the reviewing state. Sends a plain
#' text comment as the request body.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param comment Optional plain text comment for the decline.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the review was declined successfully, \code{FALSE} otherwise.
#' @references ics1477
#' @export
declineReview <- function(ident, comment = "", from = pwd()) {
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
  log_warn("failed to decline review for ident:", ident)
  return(FALSE)
}

#' Change Review Status
#'
#' Changes the status of a review to the specified new status.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param newStatus The target status string (e.g. \code{"Reviewing"}, \code{"Closed"}).
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the status was changed successfully, \code{FALSE} otherwise.
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
    return(TRUE)
  }
  log_warn("failed to change review status to '", newStatus, "' for ident:", ident)
  return(FALSE)
}
