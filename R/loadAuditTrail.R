auditTrailResourceCacheList <- createCacheList("auditTrail")


#' Load Audit Trail
#'
#' Loads the complete audit trail for a given resource identifier. The audit trail
#' contains a chronological record of all operations performed on the resource,
#' supporting regulatory compliance and data integrity requirements. Results are
#' cached for performance.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path. Can also be a list of identifiers.
#' @param from Reference point for relative paths. Defaults to current
#'   working directory via \code{\link{pwd}}.
#'
#' @returns A data frame with class "improveResource" containing:
#'   \itemize{
#'     \item \code{type} - Type indicator ("auditTrail")
#'     \item \code{resourceId}, \code{entityId}, \code{entityVersionId} - Resource identifiers
#'     \item \code{path}, \code{name} - Resource location and name
#'     \item \code{data} - A list containing data frames with audit trail entries.
#'       Each entry contains 13 variables documenting a single operation:
#'       \itemize{
#'         \item \code{id} - Unique audit entry identifier
#'         \item \code{ipAddress} - IP address of user performing the action
#'         \item \code{createdAt}, \code{createdAtDate} - Timestamp (milliseconds and POSIXct)
#'         \item \code{actor} - Actor type (e.g., "User", "System")
#'         \item \code{operation} - Operation type (e.g., "create", "update", "delete")
#'         \item \code{userVersion} - User version identifier
#'         \item \code{username} - Username who performed the action
#'         \item \code{description} - Human-readable description of the action
#'         \item \code{path} - Resource path at time of action
#'         \item \code{resourceName} - Name of the resource
#'         \item \code{entityReference} - Reference to the modified entity
#'         \item \code{entityReferenceType} - Type of entity (e.g., "Step Version", "Analysis Tree Version")
#'         \item \code{revisionId} - Revision identifier
#'         \item \code{entityId}, \code{entityVersionId} - Entity identifiers
#'       }
#'   }
#'
#' @details
#' The audit trail provides a complete, immutable record of all changes to a resource,
#' supporting regulatory compliance requirements. Each entry captures who made the
#' change, when, from where (IP address), and what was changed.
#'
#' Audit trails are essential for pharmaceutical workflows to demonstrate data integrity
#' and provide accountability for all modifications to analysis results and workflows.
#'
#' @examples
#' \dontrun{
#' # Load audit trail for current resource
#' auditTrail <- loadAuditTrail(ident, pwd())
#' auditTrail$data[[1]]
#'
#' # Review recent operations
#' recent <- auditTrail$data[[1]] |>
#'   dplyr::arrange(desc(createdAtDate)) |>
#'   head(10)
#'
#' # Load audit trail by path
#' auditTrail <- loadAuditTrail("/Projects/MyWorkflow/Step1")
#' }
#'
#' @seealso
#' \code{\link{unloadAuditTrail}} to clear cache,
#' \code{\link{refreshAuditTrail}} to refresh from server
#'
#' @references ics1097
#' @export
loadAuditTrail <- function(ident,from=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadAuditTrailFromServer,
                                   cacheList=auditTrailResourceCacheList,
                                   from=from)
  )
}

#' Unload Audit Trail from Cache
#'
#' Removes audit trail data from the cache for the specified resource.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path.
#'
#' @returns Invisibly returns NULL. Called for side effect of clearing cache.
#'
#' @details
#' Use this function when you need to ensure fresh audit trail data is loaded
#' from the server on the next \code{\link{loadAuditTrail}} call.
#'
#' @seealso
#' \code{\link{loadAuditTrail}} to load audit trail,
#' \code{\link{refreshAuditTrail}} to refresh from server
#'
#' @references ics1097
#' @export
unloadAuditTrail <- function(ident) {
  res <- loadResource(ident)
  removeFromCache(res$resourceId,"",auditTrailResourceCacheList)
}

#' Refresh Audit Trail from Server
#'
#' Clears cached audit trail data and reloads fresh data from the server.
#'
#' @param ident The resource identifier - can be a resource ID, entity ID,
#'   entity version ID, or path.
#'
#' @returns A data frame with updated audit trail data. See \code{\link{loadAuditTrail}}
#'   for details on the return structure.
#'
#' @details
#' Use this function to ensure you have the most recent audit trail entries,
#' particularly after operations that may have generated new audit records.
#'
#' @seealso
#' \code{\link{loadAuditTrail}} for return structure details,
#' \code{\link{unloadAuditTrail}} to only clear cache
#'
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1097
#' @export
refreshAuditTrail <- function(ident) {
  unloadAuditTrail(ident)
  res <- loadAuditTrail(ident)
  return(res)
}

#' @rdname refreshAuditTrail
#' @export
updateAuditTrail <- function(...) {
  .Deprecated("refreshAuditTrail")
  refreshAuditTrail(...)
}

loadAuditTrailFromServer <- function(resource) {
  genericLoadFromServer(resource,name="auditTrail",funct=actualLoadAuditTrail)
}

#' @importFrom rlang .data
actualLoadAuditTrail <- function(resource) {
  if (as.character(resource$resourceId) == "0") {
    return(NULL)
  }
  result <- restGetAsDf("/resources/{resourceId}/auditTrail",
                        urlParams = list(resourceId = resource$resourceId),
                        dates = TRUE)
  if (is.null(result)) return(data.frame())
  return(result)
}

