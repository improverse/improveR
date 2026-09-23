

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
#' @returns A data frame with one row per resource, carrying `resourceId`, `entityId` and
#'   the references nested in `$data` - use [strip()] to unwrap a single one. `NULL` when
#'   `ident` resolves to no resource. For several idents the result of all of them, merged.
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
#' @returns No meaningful value - called for its side effect of dropping the references
#'   of the resource from the cache, so that the next load reads the server. The value
#'   handed back by the internal cache removal is an implementation detail and must not be
#'   relied on.
#' @export
unloadReferences <- function(ident) {
  res <- loadResource(ident)
  removeFromCache(res$resourceId,"",referencesResourceCacheList)
}

#' Refresh References
#' @param ident id
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1206
#' @returns The freshly read references, in the same shape as [loadReferences()] - the
#'   cached copy is dropped first, so the value comes from the server.
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

