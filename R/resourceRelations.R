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

#' Creates a New Resource Relation
#' @param resourceId id (UUID) of the resource
#' @param targetResourceId id (UUID) of the target resource
#' @param relationTypeId id (UUID) of the relation type
#' @param description description of the resource relation
#' @references ics1044
#' @export
createResourceRelation <- function(resourceId, targetResourceId, relationTypeId, description) {
  improveEditable()


  if (!validateParams(list(resourceId, targetResourceId, relationTypeId, description)) || !validateResource(resourceId) || !validateResource(targetResourceId) ||
      !validateRelationType(relationTypeId) || !validateTargetResourceIdDuplicate(resourceId, targetResourceId)) {
    return(NULL)
  }

  data <- list("targetResourceId" = targetResourceId,
               "relationType" = relationTypeId,
               "description" = description)

  result <- authenticatedREST("/resources/{resourceId}/relation",  urlParams = list(resourceId = resourceId), data = data, restType = "POST")
  updateResourceRelations(resourceId)

  return(result)
}

#' Updates a Resource Relation
#' @param resourceId id (UUID) of the resource
#' @param relationId id (UUID) of the relation
#' @param newRelationTypeId id (UUID) of the relation type
#' @param newDescription description of the resource relation
#' @references ics1044
#' @export
updateResourceRelation <- function(resourceId, relationId, newRelationTypeId, newDescription) {
  improveEditable()


  if (!validateParams(list(resourceId, relationId, newRelationTypeId, newDescription)) || !validateResource(resourceId) ||
      !validateResourceRelation(resourceId, relationId) || !validateRelationType(newRelationTypeId)) {
    return(NULL)
  }

  resourceRelations <- updateResourceRelations(resourceId)
  targetResourceId <- resourceRelations[!is.na(resourceRelations$id) & resourceRelations$id == relationId,]$targetResourceId

  data <- list("targetResourceId" = targetResourceId,
               "relationType" = newRelationTypeId,
               "description" = newDescription)

  result <- authenticatedREST("/resources/{resourceId}/relation/{relationId}", urlParams = list(resourceId = resourceId, relationId = relationId), data = data, restType = "PUT")
  updateResourceRelations(resourceId)

  return(result)
}

#' Deletes a Resource Relation
#' @param resourceId id (UUID) of the resource
#' @param relationId id (UUID) of the relation
#' @references ics1044
#' @export
deleteResourceRelation <- function(resourceId, relationId) {
  improveEditable()

  if (!validateParams(list(resourceId, relationId)) || !validateResource(resourceId) || !validateResourceRelation(resourceId, relationId)) {
    return(NULL)
  }

  result <- authenticatedREST("/resources/{resourceId}/relation/{relationId}", urlParams = list(resourceId = resourceId, relationId = relationId), restType = "DELETE")
  updateResourceRelations(resourceId)

  return(result)
}
