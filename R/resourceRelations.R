#' helper function to validate resource existence
#' @param resourceId id (UUID) of the resource whose existence is to be checked
#' @noRd
validateResource <- function(resourceId) {
  resource <- tryCatch({
    loadResource(resourceId)
  }, error = function(e) {
    return(NULL)
  })

  if (is.null(resource)) {
    log_error("The resource with the id:", resourceId, "does not exist")
    return(FALSE)
  }
  return(TRUE)
}

#' helper function to validate relation existence
#' @param resourceId id (UUID) of the resource
#' @param relationId id (UUID) of the relation
#' @noRd
validateResourceRelation <- function(resourceId, relationId) {
  resourceRelations <- updateResourceRelations(resourceId)
  if (is.null(resourceRelations) || nrow(resourceRelations) == 0) {
    log_error("No resource relation exists for the resource with id:", resourceId)
    return(FALSE)
  }

  filteredResourceRelation <- resourceRelations[!is.na(resourceRelations$id) & resourceRelations$id == relationId,]
  if (is.null(filteredResourceRelation) || nrow(filteredResourceRelation) == 0) {
    log_error("The resource relation with the id:", relationId, "does not exist for the resource with the id:", resourceId)
    return(FALSE)
  } else if (nrow(filteredResourceRelation) > 1) {
    log_error("Found more than one resource relation with the id:", relationId, "for the resource with the id:", resourceId)
    return(FALSE)
  }

  return(TRUE)
}

#' helper function to check if another relation with the given targetResourceId already exists for the resource
#' @param resourceId id (UUID) of the resource
#' @param targetResourceId id (UUID) of the target resource
#' @noRd
validateTargetResourceIdDuplicate <- function(resourceId, targetResourceId) {
  resourceRelations <- updateResourceRelations(resourceId)
  if (!is.null(resourceRelations) && nrow(resourceRelations) > 0 && any(resourceRelations$targetResourceId == targetResourceId, na.rm = TRUE)) {
    log_error("Another resource relation with the targetResourceId:", targetResourceId, "already exists for the resource with the id:", resourceId)
    return(FALSE)
  }
  return(TRUE)
}

#' Resolve an identifier to a resourceId UUID
#' @param ident Resource identifier — can be a path, entityId, resourceId, or
#'   a data frame row returned by \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns Character resourceId UUID, or \code{NULL} if the resource cannot be found.
#' @noRd
resolveResourceId <- function(ident, from = pwd()) {
  if (is.data.frame(ident) && "resourceId" %in% names(ident)) {
    return(ident$resourceId[1])
  }
  res <- loadResource(ident, from)
  if (is.null(res)) return(NULL)
  return(res$resourceId)
}

#' Creates a New Resource Relation
#'
#' Creates a relation between two resources in the repository.
#'
#' @param ident Identifier of the source resource. Can be a path, resource ID,
#'   entity ID, or a data frame row from \code{loadResource()}.
#' @param targetIdent Identifier of the target resource. Same formats as \code{ident}.
#' @param relationTypeId ID (UUID) of the relation type. Use \code{loadRelationTypes()}
#'   to retrieve available types.
#' @param description Character. Description of the resource relation.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of the resource's current relations, or \code{NULL} on failure.
#' @references ics1044
#' @export
createResourceRelation <- function(ident, targetIdent, relationTypeId, description, from = pwd()) {
  improveEditable()

  resourceId <- resolveResourceId(ident, from)
  targetResourceId <- resolveResourceId(targetIdent, from)

  if (!validateParams(list(resourceId, targetResourceId, relationTypeId, description)) || !validateResource(resourceId) || !validateResource(targetResourceId) ||
      !validateRelationType(relationTypeId) || !validateTargetResourceIdDuplicate(resourceId, targetResourceId)) {
    log_warn("validation failed for creating resource relation between", ident, "and", targetIdent)
    return(NULL)
  }

  data <- list("targetResourceId" = targetResourceId,
               "relationType" = relationTypeId,
               "description" = description)

  result <- authenticatedREST("/resources/{resourceId}/relation",  urlParams = list(resourceId = resourceId), data = data, restType = "POST")
  relations <- updateResourceRelations(resourceId)

  return(relations)
}

#' Updates a Resource Relation
#'
#' @param ident Identifier of the source resource. Can be a path, resource ID,
#'   entity ID, or a data frame row from \code{loadResource()}.
#' @param relationId ID (UUID) of the relation to update.
#' @param newRelationTypeId ID (UUID) of the new relation type.
#' @param newDescription Character. New description of the resource relation.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns A data frame of the resource's current relations, or \code{NULL} on failure.
#' @references ics1044
#' @export
updateResourceRelation <- function(ident, relationId, newRelationTypeId, newDescription, from = pwd()) {
  improveEditable()

  resourceId <- resolveResourceId(ident, from)

  if (!validateParams(list(resourceId, relationId, newRelationTypeId, newDescription)) || !validateResource(resourceId) ||
      !validateResourceRelation(resourceId, relationId) || !validateRelationType(newRelationTypeId)) {
    log_warn("validation failed for updating resource relation", relationId, "on resource", ident)
    return(NULL)
  }

  resourceRelations <- updateResourceRelations(resourceId)
  targetResourceId <- resourceRelations[!is.na(resourceRelations$id) & resourceRelations$id == relationId,]$targetResourceId

  data <- list("targetResourceId" = targetResourceId,
               "relationType" = newRelationTypeId,
               "description" = newDescription)

  result <- authenticatedREST("/resources/{resourceId}/relation/{relationId}", urlParams = list(resourceId = resourceId, relationId = relationId), data = data, restType = "PUT")
  relations <- updateResourceRelations(resourceId)

  return(relations)
}

#' Deletes a Resource Relation
#'
#' @param ident Identifier of the source resource. Can be a path, resource ID,
#'   entity ID, or a data frame row from \code{loadResource()}.
#' @param relationId ID (UUID) of the relation to delete.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#' @returns \code{TRUE} if the relation was deleted successfully, \code{FALSE} otherwise.
#' @references ics1044
#' @export
deleteResourceRelation <- function(ident, relationId, from = pwd()) {
  improveEditable()

  resourceId <- resolveResourceId(ident, from)

  if (!validateParams(list(resourceId, relationId)) || !validateResource(resourceId) || !validateResourceRelation(resourceId, relationId)) {
    log_warn("validation failed for deleting resource relation", relationId, "on resource", ident)
    return(FALSE)
  }

  result <- authenticatedREST("/resources/{resourceId}/relation/{relationId}", urlParams = list(resourceId = resourceId, relationId = relationId), restType = "DELETE")
  updateResourceRelations(resourceId)

  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to delete resource relation:", relationId)
  return(FALSE)
}
