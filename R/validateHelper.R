#' helper function to check if any parameter is NA or NULL
#' @param params list of parameters that are to be checked
validateParams <- function(params) {
  if (any(vapply(params, function(param) length(param) != 1, FALSE))) {
    log_error("Parameters must have a length of 1")
    return(FALSE)
  } else if (any(vapply(params, function(param) is.null(param) || is.na(param), FALSE))) {
    log_error("Parameters must not be NA or NULL")
    return(FALSE)
  }
  return(TRUE)
}

#' helper function to validate relation type existence
#' @param relationTypeId id (UUID) of the relation type whose existence is to be checked
#' @param conn database connection
validateRelationType <- function(relationTypeId, conn = NULL) {
  if (is.null(conn)) {
    result <- authenticatedREST('configuration/relationTypeLov', restType = "GET")
    if (is.null(result)) {
      return(FALSE)
    }
    relationTypes <- httr::content(result)
    relationTypes <- mergeListToDataframe(relationTypes)
  } else {
    relationTypes <- updateRelationTypes(conn)
  }

  if (is.null(relationTypes) || nrow(relationTypes) == 0) {
    log_error("No relation type exists")
    return(FALSE)
  } else if (!"id" %in% colnames(relationTypes)) {
    log_error("The 'id' column does not exist in the 'relationTypes' data frame")
    return(FALSE)
  }

  filteredRelationType <- relationTypes[!is.na(relationTypes$id) & relationTypes$id == relationTypeId,]
  if (is.null(filteredRelationType) || nrow(filteredRelationType) == 0) {
    log_error("The relation type with the id:", relationTypeId, "does not exist")
    return(FALSE)
  } else if (nrow(filteredRelationType) > 1) {
    log_error("Found more than one relation type with the id:", relationTypeId)
    return(FALSE)
  }

  return(TRUE)
}
