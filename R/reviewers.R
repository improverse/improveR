#' helper function to validate the review's existence
#' @param reviewId id (UUID) of the review whose existence is to be checked
#' @noRd
validateReview <- function(reviewId) {
  resource <- loadResource(reviewId)
  if (is.null(resource)) {
    log_error("The review with the id:", reviewId, "does not exist")
    return(FALSE)
  }
  if (resource$nodeType != "Review") {
    log_error("Resource", reviewId, "is not a Review (nodeType:", resource$nodeType, ")")
    return(FALSE)
  }
  return(TRUE)
}

#' helper function to validate the reviewer's existence
#' @param reviewId id (UUID) of the review
#' @param reviewerId id (UUID) of the reviewer whose existence is to be checked
#' @noRd
validateReviewer <- function(reviewId, reviewerId) {
  reviewers <- refreshReviewers(reviewId)
  if (is.null(reviewers) || nrow(reviewers) == 0) {
    log_error("No reviewer exists for the review with the id:", reviewId)
    return(FALSE)
  } else if (!"id" %in% colnames(reviewers)) {
    log_error("The 'id' column does not exist in the 'reviewers' data frame")
    return(FALSE)
  }

  filteredReviewer <- reviewers[!is.na(reviewers$id) & reviewers$id == reviewerId,]
  if (is.null(filteredReviewer) || nrow(filteredReviewer) == 0) {
    log_error("The reviewer with the id:", reviewerId, "does not exist for the review with the id:", reviewId)
    return(FALSE)
  } else if (nrow(filteredReviewer) > 1) {
    log_error("Found more than one reviewer with the id:", reviewerId, "for the review with the id:", reviewId)
    return(FALSE)
  }

  return(TRUE)
}

#' helper function to validate the user's existence
#' @param userId id (UUID) of the user whose existence is to be checked
#' @param username name of the user whose existence is to be checked
#' @noRd
validateUser <- function(userId, username) {
  users <- users()
  if (is.null(users) || nrow(users) == 0) {
    log_error("No user exists")
    return(FALSE)
  } else if (!"id" %in% colnames(users)) {
    log_error("The 'id' column does not exist in the 'users' data frame")
    return(FALSE)
  }

  filteredUser <- users[!is.na(users$id) & users$id == userId & !is.na(users$username) & users$username == username,]
  if (is.null(filteredUser) || nrow(filteredUser) == 0) {
    log_error("The user with the id:", userId, "and the username:", username, "does not exist")
    return(FALSE)
  } else if (nrow(filteredUser) > 1) {
    log_error("Found more than one user with the id:", userId, "and the username:", username)
    return(FALSE)
  }

  return(TRUE)
}

#' helper function to check if another reviewer with the given user already exists for the review
#' @param reviewId id (UUID) of the review
#' @param userId id (UUID) of the user
#' @param username name of the user
#' @noRd
validateDuplicateReviewer <- function(reviewId, userId, username) {
  reviewers <- refreshReviewers(reviewId)
  if (!is.null(reviewers) && nrow(reviewers) > 0) {
    # Reviewer API returns nested user fields: user.id, user.username
    userIdCol <- if ("user.id" %in% colnames(reviewers)) "user.id" else if ("userId" %in% colnames(reviewers)) "userId" else NULL
    usernameCol <- if ("user.username" %in% colnames(reviewers)) "user.username" else if ("username" %in% colnames(reviewers)) "username" else NULL
    if (is.null(userIdCol) || is.null(usernameCol)) {
      log_error("Cannot find user ID/username columns in reviewers data frame. Available columns:", paste(colnames(reviewers), collapse=", "))
      return(FALSE)
    }
    if (any((reviewers[[userIdCol]] == userId) & (reviewers[[usernameCol]] == username), na.rm = TRUE)) {
      log_error("Another reviewer with the id:", userId, "and the username:", username, "already exists for the review with the id:", reviewId)
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Adds a User as Reviewer to a Review
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param userId Character. ID (UUID) of the user to add as reviewer.
#' @param username Character. Username of the user to add as reviewer.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of the review's current reviewers, or \code{NULL} on failure.
#' @references ics368
#' @export
createReviewer <- function(ident, userId, username, from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find review by ident:", ident)
    return(NULL)
  }
  reviewId <- resource$resourceId

  if (!validateReview(reviewId) || !validateUser(userId, username) || !validateDuplicateReviewer(reviewId, userId, username)) {
    return(NULL)
  }

  data <- list("userId" = userId,
               "username" = username)

  result <- authenticatedREST("/reviews/{reviewId}/reviewers", urlParams = list(reviewId = reviewId), data = data, restType = "POST")
  reviewers <- refreshReviewers(reviewId)

  return(reviewers)
}

#' Removes a Reviewer from a Review
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param reviewerId Character. ID (UUID) of the reviewer to remove.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the reviewer was removed successfully, \code{FALSE} otherwise.
#' @references ics369
#' @export
deleteReviewer <- function(ident, reviewerId, from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find review by ident:", ident)
    return(FALSE)
  }
  reviewId <- resource$resourceId

  if (!validateReview(reviewId) || !validateReviewer(reviewId, reviewerId)) {
    return(FALSE)
  }

  result <- authenticatedREST("/reviews/{reviewId}/reviewers/{reviewerId}", urlParams = list(reviewId = reviewId, reviewerId = reviewerId), restType = "DELETE")
  refreshReviewers(reviewId)

  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to delete reviewer:", reviewerId, "from review:", reviewId)
  return(FALSE)
}
