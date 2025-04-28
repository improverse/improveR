historyResourceCacheList <- createCacheList("history")


#' loads the history  by the resourceId,  entity ID or entity version ID
#' it uses caching
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' resourceId can be a list
#'
#' arguments:
#' @param  ident the resource id or the entity id of the resource
#' @param from used if a relative path is used
#' @references ics1094
#' @export
loadHistory <- function(ident,from=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadHistoryFromServer,
                                   cacheList=historyResourceCacheList,
                                   from=from)
  )
}

#' unloadHistory
#' @inheritParams common_ident
#' @inheritSection common_ident Details ident

#' @references ics1094
#' @export
unloadHistory <- function(ident) {
  res <- loadResource(ident)
  removeFromCache(res$resourceId,"",historyResourceCacheList)
}

#' updateHistory
#' @inheritParams common_ident
#' @inheritSection common_ident Details ident

#' @references ics1094
#' @export
updateHistory <- function(ident) {
  unloadHistory(ident)
  res <- loadHistory(ident)
  return(res)
}

loadHistoryFromServer <- function(resource) {
  genericLoadFromServer(resource,name="history",funct=actualLoadHistory)
}

actualLoadHistory <- function(resource) {
  result<-NULL
  if (as.character(resource$resourceId)!="0") {
    result <- authenticatedREST("/resources/{resourceId}/revisionHistory",
                                list(resourceId=resource$resourceId)
    )
  } else {
    result <- data.frame(comment="root has no history")
  }
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  df<-convertDates(df)
  return(df)
}
