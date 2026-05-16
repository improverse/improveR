metadataResourceCacheList <- createCacheList("metadata")


#' Loads the History by the ResourceId, Entity ID or Entity Version ID
#' it uses caching
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' resourceId can be a list
#'
#' arguments:
#' @param  ident the resource id or the entity id of the resource
#' @param from used if a relative path is used
#' @references ics1096
#' @export
loadMetaData <- function(ident,from=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadMetaDataFromServer,
                                   cacheList=metadataResourceCacheList,
                                   from=from)
  )
}

#' Unload Meta Data
#' @param ident id
#' @references ics1096
#' @export
unloadMetaData <- function(ident) {
  res <- loadResource(ident)
  if (!is.null(res)) {
    removeFromCache(res$resourceId,"",metadataResourceCacheList)
  }
}

#' Refresh Meta Data
#' @param ident id
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1096
#' @export
refreshMetaData <- function(ident) {
  unloadMetaData(ident)
  res <- loadMetaData(ident)
  return(res)
}

#' @rdname refreshMetaData
#' @export
updateMetaData <- function(...) {
  .Deprecated("refreshMetaData")
  refreshMetaData(...)
}


loadMetaDataFromServer <- function(resource) {
  genericLoadFromServer(resource,name="meta data",funct=actualLoadMetaData)
}

actualLoadMetaData <- function(resource) {
  if (as.character(resource$resourceId) == "0") {
    return(data.frame(comment = "root has no meta data"))
  }
  result <- restGetAsDf("/resources/{resourceId}/metadata",
                        urlParams = list(resourceId = resource$resourceId),
                        dates = TRUE)
  if (is.null(result)) return(data.frame())
  return(result)
}
