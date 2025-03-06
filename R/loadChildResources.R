childResourceCacheList <- createCacheList("child")


#QUESTION: spelling - pathes; should be paths; fromForRelativePathes; rename function?

#' loadChildResources
#' @description Loads all child resources by the resourceId, entity ID, or entity version ID.
#' Uses caching. Child resources can be folders, steps, workflows, or analysis trees.
#' The results are returned as a data frame or a list of data frames.
#' The dates are  converted to POSIX dates with the convertImproveTimestampToPosix function.
#' @param  ident the resource id or the entity id of the resource; can be a list
#' @param fromForRelativePathes used if a relative path is used
#' @seealso [convertImproveTimestampToPosix()]
#' @export
loadChildResources <- function(ident,fromForRelativePathes=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadChildResourcesFromServer,
                                   cacheList=childResourceCacheList,
                                   fromForRelativePathes=fromForRelativePathes)
  )
}
#QUESTION: description text ok?
#' unloadChildResources
#' @description Removes a resource from the cache.
#' @param ident id
#' @references ics1085
#' @export
unloadChildResources <- function(ident) {
  res <- loadResource(ident)
  loadChildResources(res$resourceId)
  removeFromCache(res$resourceId,"",childResourceCacheList)
}

#' updateChildResources
#' @description Updates a child resource by removing it from the cache and subsequently loading
#' it from the server.
#' @param ident id
#' @references ics1085
#' @seealso [loadChildResources()], [unloadChildResources()]
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
    result <- authenticatedREST("/resources/{resourceId}/resources", list(resourceId=resource$resourceId)
    )
  } else {
    result <- authenticatedREST("/resources")
  }
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  df <-convertDates(df)
  return(df)
}
