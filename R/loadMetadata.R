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
#' @returns A data frame with one row per resource, carrying `resourceId`, `entityId` and
#'   the metadata entries nested in `$data` - use [strip()] to unwrap a single one. `NULL`
#'   when `ident` resolves to no resource. For several idents the result of all of them,
#'   merged.
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
#' @returns No meaningful value - called for its side effect of dropping the metadata of
#'   the resource from the cache, so that the next load reads the server. The value handed
#'   back by the internal cache removal is an implementation detail and must not be relied
#'   on.
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
#' @returns The freshly read metadata, in the same shape as [loadMetaData()] - the cached
#'   copy is dropped first, so the value comes from the server.
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
