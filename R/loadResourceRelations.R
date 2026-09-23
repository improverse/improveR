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
#' @returns No meaningful value - called for its side effect of dropping the relation
#'   types from the cache, so that the next load reads the server. The value handed back by
#'   the internal cache removal is an implementation detail and must not be relied on.
#' @export
unloadRelationTypes <- function() {
  removeFromCache(defaultKeyRelationTypes(), "", relationTypesCacheList)
}

#' Reloads the Relation Types
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @returns The freshly read relation types, in the same shape as [loadRelationTypes()] -
#'   the cached copy is dropped first, so the value comes from the server.
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

#' Create a New Relation Type
#'
#' Creates a new relation type on the server and refreshes the cached list.
#'
#' @param name Character. Display name of the relation type (e.g. "tested by").
#' @param reverseName Character. Reverse display name (e.g. "tests").
#' @param description Character. Description of the relation type.
#' @returns The created relation type as a list (with \code{id}, \code{name},
#'   \code{reverseName}, \code{description}), or \code{NULL} on failure.
#' @references ics1694
#' @export
createRelationType <- function(name, reverseName, description = "") {
  improveEditable()
  # Idempotent on name: server rejects duplicates, so check the existing list
  # and return the matching record if one is already registered (ics1694).
  existing <- loadRelationTypes()
  if (!is.null(existing) && "name" %in% colnames(existing)) {
    match <- existing[existing$name == name, , drop = FALSE]
    if (nrow(match) > 0) {
      return(as.list(match[1, ]))
    }
  }
  result <- authenticatedREST(
    "configuration/relationTypeLov",
    data = list(name = name, reverseName = reverseName, description = description),
    restType = "POST"
  )
  if (is.null(result)) {
    log_warn("Failed to create relation type:", name)
    return(NULL)
  }
  created <- httr::content(result)
  refreshRelationTypes()
  return(created)
}

#' Update an Existing Relation Type
#'
#' Updates a relation type on the server and refreshes the cached list.
#'
#' @param relationTypeId Character. UUID of the relation type to update.
#' @param name Character. New display name.
#' @param reverseName Character. New reverse display name.
#' @param description Character. New description.
#' @returns The updated relation type as a list, or \code{NULL} on failure.
#' @references ics1699
#' @export
updateRelationType <- function(relationTypeId, name, reverseName, description = "") {
  improveEditable()
  result <- authenticatedREST(
    "configuration/relationTypeLov/{relationTypeId}",
    urlParams = list(relationTypeId = relationTypeId),
    data = list(name = name, reverseName = reverseName, description = description),
    restType = "PUT"
  )
  if (is.null(result)) {
    log_warn("Failed to update relation type:", relationTypeId)
    return(NULL)
  }
  updated <- httr::content(result)
  refreshRelationTypes()
  return(updated)
}

#' Delete a Relation Type
#'
#' Deletes a relation type from the server and refreshes the cached list.
#'
#' @param relationTypeId Character. UUID of the relation type to delete.
#' @returns \code{TRUE} if deleted successfully, \code{FALSE} otherwise.
#' @references ics1700
#' @export
deleteRelationType <- function(relationTypeId) {
  improveEditable()
  result <- authenticatedREST(
    "configuration/relationTypeLov/{relationTypeId}",
    urlParams = list(relationTypeId = relationTypeId),
    restType = "DELETE"
  )
  refreshRelationTypes()
  return(!is.null(result))
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
#' @returns No meaningful value - called for its side effect of dropping the relations of
#'   the resource from the cache, so that the next load reads the server. The value handed
#'   back by the internal cache removal is an implementation detail and must not be relied
#'   on.
#' @export
unloadResourceRelations <- function(ident, from = pwd()) {
  resourceId <- resolveToResourceId(ident, from)
  removeFromCache(resourceId, "", resourceRelationsCacheList)
}

#' Reloads the Resource Relations
#' @param ident Identifier of the resource.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1044
#' @returns The freshly read resource relations, in the same shape as
#'   [loadResourceRelations()] - the cached copy is dropped first, so the value comes from
#'   the server.
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
