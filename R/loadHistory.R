historyResourceCacheList <- createCacheList("history")
parentalDescendantCacheList <- createCacheList("parentalDescendant")


#' Loads the History by the ResourceId, Entity ID or Entity Version ID
#' it uses caching
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' resourceId can be a list
#'
#' arguments:
#' @param  ident the resource id or the entity id of the resource
#' @param from used if a relative path is used
#'
#' @returns A dataframe with the following columns:
#'   - type (character)
#'   - resourceId (character)
#'   - entityId (character)
#'   - entityVersionId (character)
#'   - path (character)
#'   - name (character)
#'   - data (list)
#'   
#'   The 'data' column contains a nested dataframe with:
#'   - data/resourceId (character)
#'   - data/resourceVersionId (character)
#'   - data/nodeType (character)
#'   - data/name (character)
#'   - data/deleted (logical)
#'   - data/entityId (character)
#'   - data/entityVersionId (character)
#'   - data/revisionId (character)
#'   - data/lastModifiedOn (numeric)
#'   - data/lastModifiedByName (character)
#'   - data/fileSize (integer)
#'   - data/hasChildren (logical)
#'   - data/hasChildrenIncludingFiles (logical)
#'   - data/path (character)
#'   - data/outdatedLink (logical)
#'   - data/workingFile (logical)
#'   - data/lastModifiedOnDate (POSIXct)
#' @references ics1094
#' @export
loadHistory <- function(ident,from=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadHistoryFromServer,
                                   cacheList=historyResourceCacheList,
                                   from=from)
  )
}

#' unloadHistory
#' @inheritParams common_ident
#' @inheritSection common_ident Details ident

#' @references ics1094
#' @export
unloadHistory <- function(ident) {
  res <- loadResource(ident)
  removeFromCache(res$resourceId,"",historyResourceCacheList)
}

#' updateHistory
#' @inheritParams common_ident
#' @inheritSection common_ident Details ident

#' @references ics1094
#' @export
updateHistory <- function(ident) {
  unloadHistory(ident)
  res <- loadHistory(ident)
  return(res)
}

loadHistoryFromServer <- function(resource) {
  genericLoadFromServer(resource,name="history",funct=actualLoadHistory)
}

actualLoadHistory <- function(resource) {
  result<-NULL
  if (as.character(resource$resourceId)!="0") {
    result <- authenticatedREST("/resources/{resourceId}/revisionHistory",
                                list(resourceId=resource$resourceId)
    )
  } else {
    result <- data.frame(comment="root has no history")
  }
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  df<-convertDates(df)
  return(df)
}

#' Load the parental/descendant relationships of a resource
#'
#' Returns the parental descendants of the resource.
#' Parental descendants are
#' - copies (modified or not) of the resource to other locations
#' - output files which were created by the resource due to its role as an input file.
#'
#' The function uses caching to improve performance. The cache persists for the
#' entire R session. If parental descendant relationships change on the server,
#' use `updateParentalDescendant()` to refresh the cached data, or use
#' `unloadParentalDescendant()` to remove the cached data.
#' 
#' @param ident Character. Resource identifier to resolve (e.g. UUID, name, or any
#'   identifier accepted by `loadResource()`).
#' @param from Character. Path used to resolve the identifier (defaults to the
#'   current working directory via `pwd()`).
#'
#' @returns A dataframe with the following columns:
#'   - `type` (character)
#'   - `resourceId` (character)
#'   - `entityId` (character)
#'   - `entityVersionId` (character)
#'   - `path` (character)
#'   - `name` (character)
#'   - `data` (list)
#'   
#'   The `data` column contains a nested dataframe with:
#'   - `resourceId` (character)
#'   - `resourceVersionId` (character)
#'   - `nodeType` (character)
#'   - `name` (character)
#'   - `deleted` (logical)
#'   - `entityId` (character)
#'   - `entityVersionId` (character)
#'   - `revisionId` (character)
#'   - `lastModifiedOn` (numeric)
#'   - `lastModifiedByName` (character)
#'   - `status` (character)
#'   - `fileSize` (integer)
#'   - `hasChildren` (logical)
#'   - `hasChildrenIncludingFiles` (logical)
#'   - `path` (character)
#'   - `outdatedLink` (logical)
#'   - `workingFile` (logical)
#'   - `lastModifiedOnDate` (POSIXct)
#'
#' @examples
#' \dontrun{
#' # by UUID
#' loadParentalDescendant("123e4567-e89b-12d3-a456-426614174000")
#'
#' # by resource name within a specific repository path
#' loadParentalDescendant("my-resource-name", from = "/path/to/repo")
#' }
#'
#' @seealso [updateParentalDescendant()], [unloadParentalDescendant()], [loadResource()]
#' @export
loadParentalDescendant <- function(ident, from = pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadParentalDescendantFromServer,
                                   cacheList=parentalDescendantCacheList,
                                   from=from)
  )
}

#' Unload parental/descendant cache for a resource
#'
#' Remove cached parental/descendant information for a resource identified by \code{ident}.
#'
#' @inheritParams common_ident
#' @seealso [updateParentalDescendant()], [loadParentalDescendant()], [loadResource()]
#' @details
#' This function loads the resource corresponding to \code{ident} (via \code{loadResource})
#' and then removes any stored parental/descendant data for that resource from the
#' internal cache. Use this when relationship information for a resource has changed
#' and the cached copy should be invalidated.
#'
#' The function is invoked for its side effect.
#'
#' @return Invisibly returns \code{NULL}.
#' @examples
#' \dontrun{
#' unloadParentalDescendant(my_ident)
#' }
#'
#' @export
unloadParentalDescendant <- function(ident) {
  res <- loadResource(ident)
  removeFromCache(res$resourceId,"",parentalDescendantCacheList)
}

#' Update parental/descendant cache for a resource
#'
#' Replace cached parental/descendant information for a resource identified by \code{ident} 
#' with newly fetched data.
#'
#' @inheritParams common_ident
#' @details
#' This function loads the resource corresponding to \code{ident} (via \code{loadResource})
#' and then removes any stored parental/descendant data for that resource from the
#' internal cache. Use this when relationship information for a resource has changed
#' and the cached copy should be invalidated so subsequent operations will recompute
#' or reload up-to-date relationship data.
#'
#' The function is invoked for its side effect.
#'
#' @return Invisibly returns \code{NULL}.
#' @seealso [loadParentalDescendant()], [unloadParentalDescendant()], [loadResource()]
#' @examples
#' \dontrun{
#' updateParentalDescendant(my_ident)
#' }
#'
#' @export
updateParentalDescendant <- function(ident) {
  unloadParentalDescendant(ident)
  res <- loadParentalDescendant(ident)
  return(res)
}

loadParentalDescendantFromServer <- function(resource) {
  genericLoadFromServer(resource,name="parentalDescendant",funct=actualLoadParentalDescendant)
}

actualLoadParentalDescendant <- function(resource) {
  result <- authenticatedREST(
    url = "/resources/{resourceId}/parentalDescendant",
    urlParams = list(resourceId = resource$resourceId)
  )

  if (is.null(result)) {
    log_info(paste("No parental descendant found for:", resource$entityId))
    return(NULL)
  }

  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  df <- convertDates(df)
  return(df)
}
