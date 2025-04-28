#' helper function to validate the review's existence
#' @param reviewId id (UUID) of the review whose existence is to be checked
validateReview <- function(reviewId) {
  reviews <- updateReviews()
  if (is.null(reviews) || nrow(reviews) == 0) {
    log_error("No review exists")
    return(FALSE)
  } else if (!"id" %in% colnames(reviews)) {
    log_error("The 'id' column does not exist in the 'reviews' data frame")
    return(FALSE)
  }

  filteredReview <- reviews[!is.na(reviews$id) & reviews$id == reviewId,]
  if (is.null(filteredReview) || nrow(filteredReview) == 0) {
    log_error("The review with the id:", reviewId, "does not exist")
    return(FALSE)
  } else if (nrow(filteredReview) > 1) {
    log_error("Found more than one review with the id:", reviewId)
    return(FALSE)
  }

  return(TRUE)
}

#' helper function to validate the reviewer's existence
#' @param reviewId id (UUID) of the review
#' @param reviewerId id (UUID) of the reviewer whose existence is to be checked
validateReviewer <- function(reviewId, reviewerId) {
  reviewers <- updateReviewers(reviewId)
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
validateDuplicateReviewer <- function(reviewId, userId, username) {
  reviewers <- updateReviewers(reviewId)
  if (!is.null(reviewers) && !all(c("userid", "username") %in% colnames(reviewers))) {
    log_error("The 'userid' or 'username' column does not exist in the 'reviewers' data frame")
    return(FALSE)
  } else if (!is.null(reviewers) && nrow(reviewers) > 0 && any((reviewers$userId == userId) & (reviewers$username == username), na.rm = TRUE)) {
    log_error("Another reviewer with the id:", userId, "and the username:", username, "already exists for the review with the id:", reviewId)
    return(FALSE)
  }
  return(TRUE)
}

#' adds a user as reviewer to a review
#' @param reviewId id (UUID) of the review
#' @param userId id (UUID) of the user
#' @param username name of the user
#' @references ics368
#' @export
createReviewer <- function(reviewId, userId, username) {
  improveEditable()

  if (!validateReview(reviewId) || !validateUser(userId, username) || !validateDuplicateReviewer(reviewId, userId, username)) {
    return(NULL)
  }

  data <- list("userId" = userId,
               "username" = username)

  result <- authenticatedREST("/reviews/{reviewId}/reviewers", urlParams = list(reviewId = reviewId), data = data, restType = "POST")
  updateReviewers(reviewId)

  return(result)
}

#' removes a reviewer from a review
#' @param reviewId id (UUID) of the review
#' @param reviewerId id (UUID) of the reviewer
#' @references ics369
#' @export
deleteReviewer <- function(reviewId, reviewerId) {
  improveEditable()

  if (!validateReview(reviewId) || !validateReviewer(reviewId, reviewerId)) {
    return(NULL)
  }

  result <- authenticatedREST("/reviews/{reviewId}/reviewers/{reviewerId}", urlParams = list(reviewId = reviewId, reviewerId = reviewerId), restType = "DELETE")
  updateReviewers(reviewId)

  return(result)
}
