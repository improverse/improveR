childResourceCacheList <- createCacheList("child")


#' loads all child resources  by the resourceId,  entity ID or entity version ID
#' it uses caching
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' resourceId can be a list
#'
#' arguments:
#' @param  ident the resource id or the entity id of the resource
#' @param fromForRelativePathes used if a relative path is used
#' @references ics1085
#' @export
loadChildResources <- function(ident,fromForRelativePathes=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadChildResourcesFromServer,
                                   cacheList=childResourceCacheList,
                                   fromForRelativePathes=fromForRelativePathes)
  )
}

#' unloadChildResources
#' @param ident id
#' @references ics1085
#' @export
unloadChildResources <- function(ident) {
  res <- loadResource(ident)
  loadChildResources(res$resourceId)
  removeFromCache(res$resourceId,"",childResourceCacheList)
}

#' updateChildResources
#' @param ident id
#' @references ics1085
#' @export
updateChildResources <- function(ident) {
  unloadChildResources(ident)
  res <- loadChildResources(ident)
  return(res)
}

loadChildResourcesFromServer <- function(resource) {
  genericLoadFromServer(resource,name="child",funct=actualLoadChildResources)
}

actualLoadChildResources <- function(resource) {
  result<-NULL
  if (as.character(resource$resourceId)!="0") {
    result <- authenticatetREST("/resources/{resourceId}/resources",
                                list(resourceId=resource$resourceId)
    )
  } else {
    result <- authenticatetREST("/resources")
  }
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  df<-convertDates(df)
  return(df)
}
