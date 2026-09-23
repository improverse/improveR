fullChildResourceCacheList <- createCacheList("fullchild")


#' loads all child resources  by the resourceId,  entity ID or entity version ID
#' the childResources REST call does not return all the fields, like run status for steps, just the fields all resources have in common, with this function, the full resources are loaded

#' It Uses Caching
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' resourceId can be a list
#'
#' arguments:
#' @param  ident the resource id or the entity id of the resource
#' @param from used if a relative path is used
#' @references ics1085
#' @returns A data frame with one row per resource, carrying `resourceId`, `entityId` and
#'   the child resources, each with their own children nested in `$data` - use [strip()] to
#'   unwrap a single one. `NULL` when `ident` resolves to no resource. For several idents
#'   the result of all of them, merged.
#' @export
loadFullChildResources <- function(ident,from=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadFullChildResourcesFromServer,
                                   cacheList=fullChildResourceCacheList,
                                   from=from)
  )
}

#' unloadFullChildResources
#' @param ident id
#' @references ics1085
#' @returns No meaningful value - called for its side effect of dropping the child
#'   resources of the resource from the cache, so that the next load reads the server. The
#'   value handed back by the internal cache removal is an implementation detail and must
#'   not be relied on.
#' @export
unloadFullChildResources <- function(ident) {
  res <- loadResource(ident)
  removeFromCache(res$resourceId,"",fullChildResourceCacheList)
}


#' refreshFullChildResources
#' @param ident id
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1085
#' @returns The freshly read child resources, in the same shape as
#'   [loadFullChildResources()] - the cached copy is dropped first, so the value comes from
#'   the server.
#' @export
refreshFullChildResources <- function(ident) {
  unloadFullChildResources(ident)
  res <- loadFullChildResources(ident)
  return(res)
}

#' @rdname refreshFullChildResources
#' @export
updateFullChildResources <- function(...) {
  .Deprecated("refreshFullChildResources")
  refreshFullChildResources(...)
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
