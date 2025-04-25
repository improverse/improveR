

referencesResourceCacheList <- createCacheList("references")


#' loads the references  by the resourceId,  entity ID or entity version ID
#' it uses caching
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' resourceId can be a list
#'
#' arguments:
#' @param  ident the resource id or the entity id of the resource
#' @param fromForRelativePathes used if a relative path is used
#' @references ics1206
#' @export
loadReferences <- function(ident,fromForRelativePathes=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadReferencesFromServer,
                                   cacheList=referencesResourceCacheList,
                                   fromForRelativePathes=fromForRelativePathes)
  )
}

#' unloadReferences
#' @param ident id
#' @references ics1206
#' @export
unloadReferences <- function(ident) {
  res <- loadResource(ident)
  r <- loadReferences(res)
  removeFromCache(res$resourceId,"",referencesResourceCacheList)
}

#' updateReferences
#' @param ident id
#' @references ics1206
#' @export
updateReferences <- function(ident) {
  unloadReferences(ident)
  res <- loadReferences(ident)
  return(res)
}

loadReferencesFromServer <- function(resource) {
  genericLoadFromServer(resource,name="references",funct=actualLoadReferences)
}

actualLoadReferences <- function(resource) {
  result<-NULL
  if (as.character(resource$resourceId)!="0") {
    result <- authenticatetREST("/resources/{resourceId}/references",
                                list(resourceId=resource$resourceId)
    )
  } else {
    result <- data.frame(comment="root has no references")
  }
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  df<-convertDates(df)
  return(df)
}

