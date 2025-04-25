fullChildResourceCacheList <- createCacheList("fullchild")


#' loads all child resources  by the resourceId,  entity ID or entity version ID
#' the childResources REST call does not return all the fields, like run status for steps, just the fields all resources have in common, with this function, the full resources are loaded

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
loadFullChildResources <- function(ident,fromForRelativePathes=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadFullChildResourcesFromServer,
                                   cacheList=fullChildResourceCacheList,
                                   fromForRelativePathes=fromForRelativePathes)
  )
}

#' unloadFullChildResources
#' @param ident id
#' @references ics1085
#' @export
unloadFullChildResources <- function(ident) {
  res <- loadResource(ident)
  loadFullChildResources(res$resourceId)
  removeFromCache(res$resourceId,"",fullChildResourceCacheList)
}


#' updateFullChildResources
#' @param ident id
#' @references ics1085
#' @export
updateFullChildResources <- function(ident) {
  unloadFullChildResources(ident)
  res <- loadFullChildResources(ident)
  return(res)
}

loadFullChildResourcesFromServer <- function(resource) {
  genericLoadFromServer(resource,name="full-child",funct=actualLoadFullChildResources)
}

actualLoadFullChildResources <- function(resource) {
  result<-NULL
  if (as.character(resource$resourceId)!="0") {
    result <- authenticatedREST("/resources/{resourceId}/resources",
                                list(resourceId=resource$resourceId)
    )
  } else {
    result <- authenticatedREST("/resources")
  }
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  if (nrow(df)==0)
    return(df)
    return(
      loadResourceFromServer(
        df$resourceId
      )
    )
}
