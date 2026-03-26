

referencesResourceCacheList <- createCacheList("references")


#' Loads the References by the ResourceId, Entity ID or Entity Version ID
#' it uses caching
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' resourceId can be a list
#'
#' arguments:
#' @param  ident the resource id or the entity id of the resource
#' @param from used if a relative path is used
#' @references ics1206
#' @export
loadReferences <- function(ident,from=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadReferencesFromServer,
                                   cacheList=referencesResourceCacheList,
                                   from=from)
  )
}

#' Unload References
#' @param ident id
#' @references ics1206
#' @export
unloadReferences <- function(ident) {
  res <- loadResource(ident)
  r <- loadReferences(res)
  removeFromCache(res$resourceId,"",referencesResourceCacheList)
}

#' Refresh References
#' @param ident id
#' @references ics1206
#' @export
refreshReferences <- function(ident) {
  unloadReferences(ident)
  res <- loadReferences(ident)
  return(res)
}

#' @rdname refreshReferences
#' @export
updateReferences <- function(...) {
  .Deprecated("refreshReferences")
  refreshReferences(...)
}

loadReferencesFromServer <- function(resource) {
  genericLoadFromServer(resource,name="references",funct=actualLoadReferences)
}

actualLoadReferences <- function(resource) {
  if (as.character(resource$resourceId) == "0") {
    return(data.frame(comment = "root has no references"))
  }
  result <- restGetAsDf("/resources/{resourceId}/references",
                        urlParams = list(resourceId = resource$resourceId),
                        dates = TRUE)
  if (is.null(result)) return(data.frame())
  return(result)
}

