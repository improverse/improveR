childStepCacheList <- createCacheList("childSteps")


#' Load Child Steps
#'
#' Loads all child steps for a given Step resource identifier. Only works on
#' Step resources - returns NULL for other resource types. Child steps represent
#' hierarchical workflow structures. Results are cached for performance.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path. Can also be a list of identifiers.
#' @param from Reference point for relative paths. Defaults to current
#'   working directory via \code{\link{pwd}}.
#'
#' @returns A data frame with class "improveResource" containing:
#'   \itemize{
#'     \item \code{type} - Type indicator ("childSteps")
#'     \item \code{resourceId}, \code{entityId}, \code{entityVersionId} - Resource identifiers
#'     \item \code{path}, \code{name} - Resource location and name
#'     \item \code{data} - A list containing data frames with 30 variables per child step:
#'       \itemize{
#'         \item Standard metadata: \code{resourceId}, \code{resourceVersionId}, \code{nodeType},
#'           \code{name}, \code{path}, \code{parentId}, \code{deleted}, \code{entityId},
#'           \code{entityVersionId}, \code{fullEntityId}, \code{fullEntityVersionId},
#'           \code{revisionId}
#'         \item Audit fields: \code{lastModifiedOn}, \code{lastModifiedById},
#'           \code{lastModifiedByName}, \code{lastModifiedOnDate} (POSIXct)
#'         \item Hierarchy: \code{hasChildren}, \code{hasChildrenIncludingFiles}
#'         \item Status: \code{outdatedLink}, \code{finishedStatus}, \code{workingFile},
#'           \code{fileSize}
#'         \item Execution: \code{runStatus} (e.g., "FINISHED"), \code{runResult}
#'           (e.g., "CANCELED", "SUCCESS")
#'         \item Step classification: \code{keyStep}, \code{baseModel}, \code{fullModel},
#'           \code{finalModel}, \code{referenceModel} (logical)
#'         \item \code{children} - Nested list containing tibbles of child resources
#'           (Files, etc.) with 21 variables including \code{status}, \code{fileHash}
#'       }
#'   }
#'   Returns NULL if the resource is not a Step.
#'
#' @details
#' This function only works on Step resources. If called on a non-Step resource
#' (e.g., Folder, Analysis Tree), it will log a warning and return NULL.
#'
#' The nested \code{children} field provides access to files and other resources
#' within each child step, enabling complete workflow hierarchy traversal.
#'
#' @examples
#' \dontrun{
#' # Load child steps of current step
#' childSteps <- loadChildSteps(pwd())
#' childSteps$data[[1]]
#'
#' # Access nested children (files) of first child step
#' files <- childSteps$data[[1]]$children[[1]]
#'
#' # Load child steps by path
#' childSteps <- loadChildSteps("/Projects/MyWorkflow/MainStep")
#' }
#'
#' @seealso
#' \code{\link{unloadChildSteps}} to clear cache,
#' \code{\link{refreshChildSteps}} to refresh from server,
#' \code{\link{loadChildResources}} for all child resource types
#'
#' @references ics1205
#' @export
loadChildSteps <- function(ident,from=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadChildStepsFromServer,
                                   cacheList=childStepCacheList,
                                   from=from)
  )
}

#' Unload Child Steps from Cache
#'
#' Removes child steps data from the cache for the specified resource.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path.
#'
#' @returns Invisibly returns NULL. Called for side effect of clearing cache.
#'
#' @seealso
#' \code{\link{loadChildSteps}} to load child steps,
#' \code{\link{refreshChildSteps}} to refresh from server
#'
#' @references ics1205
#' @export
unloadChildSteps <- function(ident) {
  res <- loadResource(ident)
  loadChildSteps(res$resourceId)
  removeFromCache(res$resourceId,"",childStepCacheList)
}

#' Refresh Child Steps from Server
#'
#' Clears cached child steps data and reloads fresh data from the server.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path.
#'
#' @returns A data frame with updated child steps data. See \code{\link{loadChildSteps}}
#'   for details on the return structure.
#'
#' @seealso
#' \code{\link{loadChildSteps}} for return structure details,
#' \code{\link{unloadChildSteps}} to only clear cache
#'
#' @references ics1205
#' @export
refreshChildSteps <- function(ident) {
  unloadChildSteps(ident)
  res <- loadChildSteps(ident)
  return(res)
}

#' @rdname refreshChildSteps
#' @export
updateChildSteps <- function(...) {
  .Deprecated("refreshChildSteps")
  refreshChildSteps(...)
}

loadChildStepsFromServer <- function(resource) {
  genericLoadFromServer(resource,name="childSteps",funct=actualLoadChildSteps)
}


#nest!
actualLoadChildSteps <- function(resource) {
  if (as.character(resource$resourceId) == "0") {
    log_warn("no child steps possible in root")
    return(NULL)
  }
  if (resource$nodeType != "Step") {
    log_warn("Resource", resource$entityId, "is not a Step, so no child steps possible")
    return(NULL)
  }
  result <- restGetAsDf("/resources/{resourceId}/childSteps",
                        urlParams = list(resourceId = resource$resourceId),
                        nested = TRUE, dates = TRUE)
  if (is.null(result)) return(data.frame())
  return(result)
}
