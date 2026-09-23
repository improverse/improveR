defaultKeyReviews <- function(...) {
  return("default")
}

reviewsCacheList <- list(
  reviewsCache = defaultKeyReviews
)

#' API reqeust to retrieve all registered reviews
#' @param ... additional arguments
#' @references ics348
#' @keywords internal
#' @noRd
actualLoadReviews <- function(...) {
  result <- authenticatedREST("/reviews", restType = "GET")
  if (is.null(result)) {
    return(NULL)
  }

  reviewsList <- httr::content(result)
  reviews <- reviewsList$elements
  if (is.null(reviews)) {
    return(NULL)
  }

  reviewsCleaned <- lapply(reviews, function(x) {
    x[setdiff(names(x), c("comments", "entries"))]
  })
  reviewsDf <- mergeListToDataframe(reviewsCleaned)

  if (is.null(reviewsDf) || nrow(reviewsDf) == 0) {
    return(NULL)
  }

  return(reviewsDf)
}

#' Loads All Registered Reviews
#'
#' Retrieves the list of all reviews from the repository. Results are cached.
#'
#' @returns A data frame of reviews, or \code{NULL} if none exist.
#' @references ics348
#' @export
loadReviews <- function() {
  reviews <- getFromCache(defaultKeyReviews(), actualLoadReviews, reviewsCacheList, NULL)
  return(reviews)
}

#' Unloads All Reviews
#' @references ics348
#' @returns No meaningful value - called for its side effect of dropping the list of all
#'   reviews from the cache. Note that the list is loaded once before it is dropped, so the
#'   call does reach the server.
#' @export
unloadReviews <- function() {
  loadReviews()
  removeFromCache(defaultKeyReviews(), "", reviewsCacheList)
}

#' Reloads the Reviews
#' @returns A data frame of reviews, or \code{NULL} if none exist.
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics348
#' @export
refreshReviews <- function() {
  unloadReviews()
  res <- loadReviews()
  return(res)
}

#' @rdname refreshReviews
#' @export
updateReviews <- function(...) {
  .Deprecated("refreshReviews")
  refreshReviews(...)
}

reviewersCacheList <- list(
  reviewrsCache = "resourceId"
)

#' Get Reviewers
#'
#' Retrieves all reviewers for a given review.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of reviewers, or \code{NULL} if none exist.
#' @references ics1208
#' @export
getReviewers <- function(ident, from = pwd()) {
  review <- loadResource(ident, from)
  if (is.null(review) || review$nodeType != "Review") {
    log_warn(paste0(ident, " does not specify a Review"))
  }
  result <- authenticatedREST("/reviews/{resourceId}/reviewers",
                              list(resourceId = review$resourceId))
  cont <- restContent(result, "loadReviewData")
  cont <- cont$elements
  df <- mergeListToDataframe(cont)
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  df$resourceId <- review$resourceId
  return(df)
}

#' Loads All The Reviewers For A Review
#'
#' Retrieves all reviewers for a review. Results are cached.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of reviewers, or \code{NULL} if none exist.
#' @references ics1208
#' @export
loadReviewers <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  reviewReviewers <- getFromCache(resourceId, getReviewers, reviewersCacheList, NULL)
  return(reviewReviewers)
}

#' Unloads the Reviewers for a Review
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @references ics1208
#' @returns No meaningful value - called for its side effect of dropping the reviewers of
#'   the review from the cache, so that the next load reads the server. The value handed
#'   back by the internal cache removal is an implementation detail and must not be relied
#'   on.
#' @export
unloadReviewers <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  removeFromCache(resourceId, "", reviewersCacheList)
}

#' Reloads the Reviewers for a Review
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of reviewers, or \code{NULL} if none exist.
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1208
#' @export
refreshReviewers <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  unloadReviewers(resourceId)
  res <- loadReviewers(resourceId)
  return(res)
}

#' @rdname refreshReviewers
#' @export
updateReviewers <- function(...) {
  .Deprecated("refreshReviewers")
  refreshReviewers(...)
}

reviewEntriesCacheList <- list(
  reviewEntriesCache = "resourceId"
)

#' Get Review Entries
#'
#' Retrieves all entries for a given review.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of review entries, or \code{NULL} if none exist.
#' @references ics1208
#' @export
getReviewEntries <- function(ident, from = pwd()) {
  review <- loadResource(ident, from)
  if (is.null(review) || review$nodeType != "Review") {
    log_warn(paste0(ident, " does not specify a Review"))
  }
  result <- authenticatedREST("/reviews/{resourceId}/entries",
                              list(resourceId = review$resourceId))
  cont <- restContent(result, "loadReviewData")
  cont <- cont$elements
  cont <- lapply(cont, function(entry) {
    resource <- as.data.frame(entry$resource, stringsAsFactors = FALSE)
    resource$id <- entry$id
    resource$status <- entry$status
    resource$states <- list(entry$states)
    resource$comments <- list(entry$comments)
    return(resource)
  })
  df <- mergeListToDataframe(cont)
  return(df)
}

#' Loads All The Review Entries For A Review
#'
#' Retrieves all entries for a review. Results are cached.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of review entries, or \code{NULL} if none exist.
#' @references ics1208
#' @export
loadReviewEntries <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  reviewerEntries <- getFromCache(resourceId, getReviewEntries, reviewEntriesCacheList, NULL)
  return(reviewerEntries)
}

#' Unloads the Review Entries for a Review
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @references ics1208
#' @returns No meaningful value - called for its side effect of dropping the entries of
#'   the review from the cache, so that the next load reads the server. The value handed
#'   back by the internal cache removal is an implementation detail and must not be relied
#'   on.
#' @export
unloadReviewEntries <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  removeFromCache(resourceId, "", reviewEntriesCacheList)
}

#' Reloads the Review Entries for a Review
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of review entries, or \code{NULL} if none exist.
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1208
#' @export
refreshReviewEntries <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  unloadReviewEntries(resourceId)
  res <- loadReviewEntries(resourceId)
  return(res)
}

#' @rdname refreshReviewEntries
#' @export
updateReviewEntries <- function(...) {
  .Deprecated("refreshReviewEntries")
  refreshReviewEntries(...)
}

reviewCommentsCacheList <- list(
  reviewCommentsCache = "resourceId"
)

#' Get Review Comments
#'
#' Retrieves all comments for a given review.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of review comments, or \code{NULL} if none exist.
#' @references ics1208
#' @export
getReviewComments <- function(ident, from = pwd()) {
  review <- loadResource(ident, from)
  if (is.null(review) || review$nodeType != "Review") {
    log_warn(paste0(ident, " does not specify a Review"))
  }
  result <- authenticatedREST("/reviews/{resourceId}/comments",
                              list(resourceId = review$resourceId))
  cont <- restContent(result, "loadReviewData")
  df <- mergeListToDataframe(cont)
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  return(df)
}

#' Loads All The Review Comments For A Review
#'
#' Retrieves all comments for a review. Results are cached.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of review comments, or \code{NULL} if none exist.
#' @references ics1208
#' @export
loadReviewComments <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  reviewComments <- getFromCache(resourceId, getReviewComments, reviewCommentsCacheList, NULL)
  return(reviewComments)
}

#' Unloads the Review Comments for a Review
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @references ics1208
#' @returns No meaningful value - called for its side effect of dropping the comments of
#'   the review from the cache, so that the next load reads the server. The value handed
#'   back by the internal cache removal is an implementation detail and must not be relied
#'   on.
#' @export
unloadReviewComments <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  removeFromCache(resourceId, "", reviewCommentsCacheList)
}

#' Reloads the Review Comments for a Review
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @returns A data frame of review comments, or \code{NULL} if none exist.
#' @references ics1208
#' @export
refreshReviewComments <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  unloadReviewComments(resourceId)
  res <- loadReviewComments(resourceId)
  return(res)
}

#' @rdname refreshReviewComments
#' @export
updateReviewComments <- function(...) {
  .Deprecated("refreshReviewComments")
  refreshReviewComments(...)
}

#' Get Review Entry Comments
#'
#' Retrieves all comments for a specific review entry.
#'
#' @param ident Identifier of the review. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param entryId Character. ID (UUID) of the review entry.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of review entry comments, or \code{NULL} if none exist.
#' @references ics1543
#' @export
getReviewEntryComments <- function(ident, entryId, from = pwd()) {
  review <- loadResource(ident, from)
  if (is.null(review) || review$nodeType != "Review") {
    log_warn(paste0(ident, " does not specify a Review"))
  }
  result <- authenticatedREST("/reviews/{resourceId}/entries/{entryId}/comments",
                              list(resourceId = review$resourceId, entryId = entryId))
  cont <- restContent(result, "loadReviewData")
  df <- mergeListToDataframe(cont)
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  return(df)
}
