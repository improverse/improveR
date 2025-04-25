metadataResourceCacheList <- createCacheList("metadata")


#' loads the history  by the resourceId,  entity ID or entity version ID
#' it uses caching
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' resourceId can be a list
#'
#' arguments:
#' @param  ident the resource id or the entity id of the resource
#' @param fromForRelativePathes used if a relative path is used
#' @references ics1096
#' @export
loadMetaData <- function(ident,fromForRelativePathes=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadMetaDataFromServer,
                                   cacheList=metadataResourceCacheList,
                                   fromForRelativePathes=fromForRelativePathes)
  )
}

#' unloadMetaData
#' @param ident id
#' @references ics1096
#' @export
unloadMetaData <- function(ident) {
  res <- loadResource(ident)
  if (!is.null(res)) {
    m<-loadMetaData(ident)
    removeFromCache(res$resourceId,"",metadataResourceCacheList)
  }
}

#' updateMetaData
#' @param ident id
#' @references ics1096
#' @export
updateMetaData <- function(ident) {
  unloadMetaData(ident)
  res <- loadMetaData(ident)
  return(res)
}


loadMetaDataFromServer <- function(resource) {
  genericLoadFromServer(resource,name="meta data",funct=actualLoadMetaData)
}

actualLoadMetaData <- function(resource) {
  result<-NULL
  if (as.character(resource$resourceId)!="0") {
    result <- authenticatetREST("/resources/{resourceId}/metadata",
                                list(resourceId=resource$resourceId)
    )
  } else {
    result <- data.frame(comment="root has no meta data")
  }
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  df<-convertDates(df)
  return(df)
}
