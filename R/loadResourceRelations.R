defaultKeyRelationTypes <- function(...) {
  return("default")
}

relationTypesCacheList <- list(
  relationTypesCache = defaultKeyRelationTypes
)

#' Retrieve all registered relation types via REST
#' @keywords internal
#' @noRd
actualLoadRelationTypes <- function(...) {
  result <- authenticatedREST("/configuration/relationTypeLov", restType = "GET")
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  if (length(cont) == 0) {
    return(NULL)
  }
  df <- mergeListToDataframe(cont)
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  # Normalise column names to match existing convention
  if ("reverseName" %in% colnames(df) && !"reverse_name" %in% colnames(df)) {
    # already camelCase from REST — keep as is
  }
  if ("systemLayerId" %in% colnames(df) && !"system_layer" %in% colnames(df)) {
    # already camelCase from REST — keep as is
  }
  return(df)
}

#' Loads All Registered Relation Types
#'
#' Retrieves the list of available relation types from the repository server.
#' Results are cached for performance.
#'
#' @returns A data frame of relation types with columns such as \code{id},
#'   \code{name}, \code{reverseName}, \code{description}, or \code{NULL} if
#'   no relation types are configured.
#' @references ics1592
#' @export
loadRelationTypes <- function() {
  relationTypes <- getFromCache("default", actualLoadRelationTypes, relationTypesCacheList, NULL)
  return(relationTypes)
}

#' Unloads All Relation Types
#' @export
unloadRelationTypes <- function() {
  loadRelationTypes()
  removeFromCache(defaultKeyRelationTypes, "", relationTypesCacheList)
}

#' Reloads the Relation Types
#' @export
refreshRelationTypes <- function() {
  unloadRelationTypes()
  res <- loadRelationTypes()
  return(res)
}

#' @rdname refreshRelationTypes
#' @export
updateRelationTypes <- function(...) {
  .Deprecated("refreshRelationTypes")
  refreshRelationTypes(...)
}

resourceRelationsCacheList <- list(
  resourceRelationsCache = "resourceId"
)

#' API reqeust to retrieve all registered resource relations
#' @param resourceId resource
#' @references ics1044
#' @keywords internal
#' @noRd
actualLoadResourceRelations <- function(resourceId) {
  result <- authenticatedREST("/resources/{resourceId}/relation", list(resourceId = resourceId), restType = "GET")
  if (is.null(result)) {
    return(NULL)
  }

  resourceRelations <- httr::content(result)
  resourceRelationsDf <- mergeListToDataframe(resourceRelations)

  if (is.null(resourceRelationsDf) || nrow(resourceRelationsDf) == 0) {
    return(NULL)
  } else if (!all(c("id", "targetResourceId", "relationType", "description", "createdBy") %in% colnames(resourceRelationsDf))) {
    log_error("The 'id', 'sourceResourceId', 'targetResourceId', 'relationType', 'description' or 'createdBy' column does not exist in the 'resourceRelations' data frame")
    return(NULL)
  }

  colnames(resourceRelationsDf)[colnames(resourceRelationsDf) == "sourceResourceId"] <- "resourceId"
  colnames(resourceRelationsDf)[colnames(resourceRelationsDf) == "relationType"] <- "relationTypeId"
  colnames(resourceRelationsDf)[colnames(resourceRelationsDf) == "createdBy"] <- "createdById"

  return(resourceRelationsDf)
}

#' Loads All Registered Resource Relations
#'
#' Retrieves all resource relations for a given resource. Results are cached.
#'
#' @param ident Identifier of the resource. Can be a path, resource ID, entity
#'   ID, or a data frame row from \code{loadResource()}. When a UUID string is
#'   passed it is used directly as the resource ID for backward compatibility.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of resource relations, or \code{NULL} if none exist.
#' @references ics1044
#' @export
loadResourceRelations <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  resourceRelations <- getFromCache(resourceId, actualLoadResourceRelations, resourceRelationsCacheList, NULL)
  return(resourceRelations)
}

#' Unloads All Resource Relations
#' @param ident Identifier of the resource.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @references ics1044
#' @export
unloadResourceRelations <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  loadResourceRelations(resourceId)
  removeFromCache(resourceId, "", resourceRelationsCacheList)
}

#' Reloads the Resource Relations
#' @param ident Identifier of the resource.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @references ics1044
#' @export
refreshResourceRelations <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  unloadResourceRelations(resourceId)
  res <- loadResourceRelations(resourceId)
  return(res)
}

#' @rdname refreshResourceRelations
#' @export
updateResourceRelations <- function(...) {
  .Deprecated("refreshResourceRelations")
  refreshResourceRelations(...)
}

# resolveToResourceId moved to restHelpers.R
