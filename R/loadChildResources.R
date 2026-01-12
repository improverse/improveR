childResourceCacheList <- createCacheList("child")


#' Load Child Resources
#'
#' Loads all child resources for a given resource identifier. Child resources
#' can be folders, steps, workflows and analysis trees, but also the elements of a step's
#' inventory. Results are cached for performance.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path. Can also be a list of identifiers.
#' @param from Reference point for relative paths. Defaults to current
#'   working directory via \code{\link{pwd}}.
#'
#' @returns A list with class "improveResource" containing:
#'   \itemize{
#'     \item \code{type} - Type indicator ("child")
#'     \item \code{resourceId} - Unique resource identifier
#'     \item \code{entityId} - Entity identifier
#'     \item \code{entityVersionId} - Entity version identifier
#'     \item \code{path} - Full resource path
#'     \item \code{name} - Resource name
#'     \item \code{data} - A list containing data frames of child resources.
#'       Each data frame contains columns with resource metadata:
#'       \itemize{
#'         \item \code{resourceId}, \code{resourceVersionId} - Resource identifiers
#'         \item \code{nodeType} - Type of resource (e.g., "Step", "Analysis Tree", "File", "Folder")
#'         \item \code{name}, \code{path} - Resource name and full path
#'         \item \code{parentId} - Parent resource identifier
#'         \item \code{deleted} - Logical indicating deletion status
#'         \item \code{entityId}, \code{entityVersionId} - Entity identifiers
#'         \item \code{fullEntityId}, \code{fullEntityVersionId} - Full entity URLs
#'         \item \code{revisionId} - Revision identifier
#'         \item \code{createdByName}, \code{createdById} - Creator information
#'         \item \code{createdAt}, \code{createdAtDate} - Creation timestamp (milliseconds and POSIXct)
#'         \item \code{lastModifiedByName}, \code{lastModifiedById} - Last modifier information
#'         \item \code{lastModifiedOn}, \code{lastModifiedOnDate} - Last modification timestamp
#'         \item \code{fileSize} - File size in bytes
#'         \item \code{hasChildren}, \code{hasChildrenIncludingFiles} - Child resource indicators
#'         \item \code{outdatedLink} - Logical indicating outdated link status
#'         \item \code{finishedStatus} - Execution status (e.g., "unfinished")
#'         \item \code{workingFile} - Logical indicating working file status
#'       }
#'       Additional fields for specific node types:
#'       \itemize{
#'         \item For Steps: \code{keyStep}, \code{baseModel}, \code{fullModel},
#'           \code{finalModel}, \code{referenceModel}, \code{ownedById}, \code{ownedByName}
#'         \item For Files: \code{status}, \code{fileHash}
#'       }
#'   }
#'   Dates are converted to POSIXct format via \code{\link{convertImproveTimestampToPosix}}.
#'
#' @examples
#' \dontrun{
#' # Load children of current step
#' children <- loadChildResources(pwd())
#' children$data[[1]]
#'
#' # Load children by path
#' children <- loadChildResources("/Projects/MyWorkflow")
#' }
#'
#' @seealso
#' \code{\link{unloadChildResources}} to clear cache,
#' \code{\link{updateChildResources}} to refresh from server,
#' \code{\link{loadChildSteps}} for step-specific children
#'
#' @export
loadChildResources <- function(ident,from=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadChildResourcesFromServer,
                                   cacheList=childResourceCacheList,
                                   from=from)
  )
}

#' Unload Child Resources from Cache
#'
#' Removes child resources data from the cache for the specified resource.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path.
#'
#' @returns Invisibly returns NULL. Called for side effect of clearing cache.
#'
#' @seealso
#' \code{\link{loadChildResources}} to load child resources,
#' \code{\link{updateChildResources}} to refresh from server
#'
#' @references ics1085
#' @export
unloadChildResources <- function(ident) {
  res <- loadResource(ident)
  loadChildResources(res$resourceId)
  removeFromCache(res$resourceId,"",childResourceCacheList)
}

#' Update Child Resources from Server
#'
#' Clears cached child resources data and reloads fresh data from the server.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path.
#'
#' @returns A list with updated child resources data. See \code{\link{loadChildResources}}
#'   for details on the return structure.
#'
#' @seealso
#' \code{\link{loadChildResources}} for return structure details,
#' \code{\link{unloadChildResources}} to only clear cache
#'
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
