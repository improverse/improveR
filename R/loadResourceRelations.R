defaultKeyRelationTypes <- function(...) {
  return("default")
}

relationTypesCacheList <- list(
  relationTypesCache = defaultKeyRelationTypes
)

#' API request to retrieve all registered relation types
#' @param conn database connection
#' @param ... args
actualLoadRelationTypes <- function(conn, ...) {
  #result <- DBI::dbGetQuery(conn, "SELECT * FROM RELATION_TYPE_LOV")
#TODO replace with rest call
  result <- NULL
  if (is.null(result) || nrow(result) == 0) {
    return(NULL)
  } else if (!all(c("ID", "SYSTEM_LAYER", "NAME", "REVERSE_NAME", "DESCRIPTION") %in% colnames(result))) {
    log_error("The 'id', 'system_layer', 'name', 'reverseName' or 'description' column does not exist in the 'relationTypes' data frame")
    return(NULL)
  }

  colnames(result) <- tolower(colnames(result))
  colnames(result)[colnames(result) == "reverse_name"] <- "reverseName"
  colnames(result)[colnames(result) == "system_layer"] <- "systemLayerId"
  result$id <- sapply(result$id, function(blob) {
    paste(toupper(as.character(unlist(blob))), collapse = "")
  })
  result$systemLayerId <- sapply(result$systemLayerId, function(blob) {
    paste(toupper(as.character(unlist(blob))), collapse = "")
  })

  return(result)
}

#' loads all registered relation types
#' @param conn database connection
#' @export
loadRelationTypes <- function(conn) {
  relationTypes <- getFromCache(conn, actualLoadRelationTypes, relationTypesCacheList, NULL)
  return(relationTypes)
}

#' unloads all relation types
#' @param conn database connection
#' @export
unloadRelationTypes <- function(conn) {
  loadRelationTypes(conn)
  removeFromCache(defaultKeyRelationTypes, "", relationTypesCacheList)
}

#' reloads the relation types
#' @param conn database connection
#' @export
updateRelationTypes <- function(conn) {
  unloadRelationTypes(conn)
  res <- loadRelationTypes(conn)
  return(res)
}

resourceRelationsCacheList <- list(
  resourceRelationsCache = "resourceId"
)

#' API reqeust to retrieve all registered resource relations
#' @param resourceId resource
#' @references ics1044
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

#' loads all registered resource relations
#' it uses caching
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' ident can be a list
#' @param resourceId id (UUID) of the resource
#' @references ics1044
#' @export
loadResourceRelations <- function(resourceId) {
  resourceRelations <- getFromCache(resourceId, actualLoadResourceRelations, resourceRelationsCacheList, NULL)
  return(resourceRelations)
}

#' unloads all resource relations
#' @param resourceId id (UUID) of the resource
#' @references ics1044
#' @export
unloadResourceRelations <- function(resourceId) {
  loadResourceRelations(resourceId)
  removeFromCache(resourceId, "", resourceRelationsCacheList)
}

#' reloads the resource relations
#' @param resourceId id (UUID) of the resource
#' @references ics1044
#' @export
updateResourceRelations <- function(resourceId) {
  unloadResourceRelations(resourceId)
  res <- loadResourceRelations(resourceId)
  return(res)
}
