

gridArgumentsCacheList <- list(
  gridArgumentsCache="provider"
)

#' loadGridArguments
#'
#' @param gridProvider label of the grid provider
#' @references ics1216
#'
#' @export
loadGridArguments <- function(gridProvider) {
  runserverToos <- getFromCache(gridProvider,actualLoadGridArguments,gridArgumentsCacheList,NULL)
  return(runserverToos)
}

actualLoadGridArguments <- function(gridProvider) {
  gridArgumentDefinitionsResult <- authenticatedREST("/configuration/gridArguments/{gridProvider}/definitions",
                                                     urlParams = list(gridProvider=gridProvider))
  if (is.null(gridArgumentDefinitionsResult)) {
    log_warn("no gridArguments for provider",gridProvider)
    return(NULL)
  }
  gridArgumentDefinitionsContent <- httr::content(gridArgumentDefinitionsResult)
  return(
    mergeNestedListToDataframe(gridArgumentDefinitionsContent)
  )
}



#' unloadGridArguments
#' @param gridProvider label of the grid provider
#' @references ics1216
#' @export
unloadGridArguments <- function(gridProvider) {
  loadToolsForRunserver(gridProvider)
  removeFromCache(gridProvider,"",gridArgumentsCacheList)
}

#' updateGridArguments reloads the grid arguments from the repository
#' @param gridProvider label of the grid provider
#' @references ics1216
#' @export
updateGridArguments <- function(gridProvider) {
  unloadGridArguments(gridProvider)
  res <- loadGridArguments(gridProvider)
  return(res)
}

#' loadGridArgumentDefinition loads grid argument by gridProvider and name
#' @param gridProvider label of the grid provider
#' @param argumentName name of the argument
#' @references ics1216
#' @export
loadGridArgumentDefinition <- function(gridProvider,argumentName) {
  gridArguments <- loadGridArguments(gridProvider = gridProvider)
  if (is.null(gridArguments)) {
    return(NULL)
  }
  if (! (argumentName %in% gridArguments$name)) {
    log_warn(argumentName,"not defined for provider",gridProvider)
    return(NULL)
  }
  gridArgument <- gridArguments[gridArguments$name==argumentName,]

  return(gridArgument)
}
