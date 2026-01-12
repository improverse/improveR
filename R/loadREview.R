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
#' it uses caching
#' the result is returned as a data frame
#' @references ics348
#' @export
loadReviews <- function() {
  reviews <- getFromCache(defaultKeyReviews, actualLoadReviews, reviewsCacheList, NULL)
  return(reviews)
}

#' Unloads All Reviews
#' @references ics348
#' @export
unloadReviews <- function() {
  loadReviews()
  removeFromCache(defaultKeyReviews, "", reviewsCacheList)
}

#' Reloads the Reviews
#' @references ics348
#' @export
updateReviews <- function() {
  unloadReviews()
  res <- loadReviews()
  return(res)
}

reviewersCacheList <- list(
  reviewrsCache = "resourceId"
)

#' Get Reviewers
#' @param ident id
#' @param from defaults to pwd, used to resolve relative paths
#' @references ics1208
#' @export
getReviewers <- function(ident,from=pwd()) {
  review <- loadResource(ident,from)
  if (is.null(review) || review$nodeType!="Review") {
    logging::logwarn(paste0(ident," does not specify a Review"))
  }
  result <- authenticatedREST("/reviews/{resourceId}/reviewers",
                                            list(resourceId=review$resourceId))
  cont <- httr::content(result)
  cont <- cont$elements
  df <-mergeListToDataframe(cont)
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  df$resourceId <- ident
  return(df)
}

#' Loads All The Reviewers For A Review
#' it uses caching
#' the result is returned as a data frame
#' @param resourceId id (UUID) of the resource
#' @references ics1208
#' @export
loadReviewers <- function(resourceId) {
  reviewReviewers <- getFromCache(resourceId, getReviewers, reviewersCacheList, NULL)
  return(reviewReviewers)
}

#' Unloads the Reviewers for a Review
#' @param resourceId id (UUID) of the resource
#' @references ics1208
#' @export
unloadReviewers <- function(resourceId) {
  loadReviewers(resourceId)
  removeFromCache(resourceId, "", reviewersCacheList)
}

#' Reloads the Reviewers for a Review
#' @param resourceId id (UUID) of the resource
#' @references ics1208
#' @export
updateReviewers <- function(resourceId) {
  unloadReviewers(resourceId)
  res <- loadReviewers(resourceId)
  return(res)
}

reviewEntriesCacheList <- list(
  reviewEntriesCache = "resourceId"
)

#' Get Review Entries
#' @param ident id
#' @param from defaults to pwd, used to resolve relative paths
#' @references ics1208
#' @export
getReviewEntries <- function(ident,from=pwd()) {
  review <- loadResource(ident,from)
  if (is.null(review) || review$nodeType!="Review") {
    logging::logwarn(paste0(ident," does not specify a Review"))
  }
  result <- authenticatedREST("/reviews/{resourceId}/entries",
                                            list(resourceId=review$resourceId))
  cont <- httr::content(result)
  cont <- cont$elements
  cont <- lapply(cont,function(entry) {
    resource <- as.data.frame(entry$resource,stringsAsFactors=F)
    resource$id <- entry$id
    resource$status <- entry$status
    resource$states <- list(entry$states)
    resource$comments <- list(entry$comments)
    return(resource)
  })
  df <-mergeListToDataframe(cont)
  return(df)
}

#' Loads All The Review Entries For A Review
#' it uses caching
#' the result is returned as a data frame
#' @param resourceId id (UUID) of the resource
#' @references ics1208
#' @export
loadReviewEntries <- function(resourceId) {
  reviewerEntries <- getFromCache(resourceId, getReviewEntries, reviewEntriesCacheList, NULL)
  return(reviewerEntries)
}

#' Unloads the Review Entries for a Review
#' @param resourceId id (UUID) of the resource
#' @references ics1208
#' @export
unloadReviewEntries <- function(resourceId) {
  loadReviewEntries(resourceId)
  removeFromCache(resourceId, "", reviewEntriesCacheList)
}

#' Reloads the Review Entries for a Review
#' @param resourceId id (UUID) of the resource
#' @references ics1208
#' @export
updateReviewEntries <- function(resourceId) {
  unloadReviewEntries(resourceId)
  res <- loadReviewEntries(resourceId)
  return(res)
}

reviewCommentsCacheList <- list(
  reviewCommentsCache = "resourceId"
)

#' Get Review Comments
#' @param ident id
#' @param from defaults to pwd, used to resolve relative paths
#' @references ics1208
#' @export
getReviewComments <- function(ident,from=pwd()) {
  review <- loadResource(ident,from)
  if (is.null(review) || review$nodeType!="Review") {
    logging::logwarn(paste0(ident," does not specify a Review"))
  }
  result <- authenticatedREST("/reviews/{resourceId}/comments",
                                            list(resourceId=review$resourceId))
  cont <- httr::content(result)
  df <-mergeListToDataframe(cont)
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  return(df)
}

#' Loads All The Review Comments For A Review
#' it uses caching
#' the result is returned as a data frame
#' @param resourceId id (UUID) of the resource
#' @references ics1208
#' @export
loadReviewComments <- function(resourceId) {
  reviewComments <- getFromCache(resourceId, getReviewComments, reviewCommentsCacheList, NULL)
  return(reviewComments)
}

#' Unloads the Review Comments for a Review
#' @param resourceId id (UUID) of the resource
#' @references ics1208
#' @export
unloadReviewComments <- function(resourceId) {
  loadReviewComments(resourceId)
  removeFromCache(resourceId, "", reviewCommentsCacheList)
}

#' Reloads the Review Comments for a Review
#' @param resourceId id (UUID) of the resource
#' @references ics1208
#' @export
updateReviewComments <- function(resourceId) {
  unloadReviewComments(resourceId)
  res <- loadReviewComments(resourceId)
  return(res)
}

reviewEntryCommentsCacheList <- list(
  reviewEntryCommentsCache = "resourceId, entryId" # missing "double key"
)

#' Get Review Etnry Comments
#' @param resourceId id (UUID) of the resource
#' @param entryId id (UUID) of the review entry
#' @param from defaults to pwd, used to resolve relative paths
#' @references ics1543
#' @export
getReviewEntryComments <- function(resourceId, entryId, from=pwd()) {
  review <- loadResource(resourceId, from)
  if (is.null(review) || review$nodeType!="Review") {
    logging::logwarn(paste0(resourceId," does not specify a Review"))
  }
  result <- authenticatedREST("/reviews/{resourceId}/entries/{entryId}/comments",
                                            list(resourceId=review$resourceId, entryId = entryId))
  cont <- httr::content(result)
  df <-mergeListToDataframe(cont)
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }

  return(df)
}

#' Loads All The Comments For A Review Entry
#' it uses caching
#' the result is returned as a data frame
#' @param resourceId id (UUID) of the resource
#' @param entryId id (UUID) of the review entry
#' @references ics1543
#' @export
loadReviewEntryComments <- function(resourceId, entryId) {
  # reviewComments <- getFromCache(key, getReviewEntryComments, reviewEntryCommentsCacheList, NULL)
  reviewEntryComments <- getReviewEntryComments(resourceId, entryId)
  return(reviewEntryComments)
}

#' Unloads the Comments for a Review Entry
#' @param resourceId id (UUID) of the resource
#' @param entryId id (UUID) of the review entry
#' @references ics1543
#' @export
unloadReviewEntryComments <- function(resourceId, entryId) {
  loadReviewEntryComments(resourceId, entryId)
  # removeFromCache(key, "", reviewEntryCommentsCacheList)
}

#' Reloads the Comments for a Review Entry
#' @param resourceId id (UUID) of the resource
#' @param entryId id (UUID) of the review entry
#' @references ics1543
#' @export
updateReviewEntryComments <- function(resourceId, entryId) {
  unloadReviewEntryComments(resourceId, entryId)
  res <- loadReviewEntryComments(resourceId, entryId)
  return(res)
}
