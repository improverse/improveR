

gridArgumentsCacheList <- list(
  gridArgumentsCache="provider"
)

#' loadGridArguments
#'
#' @param gridProvider label of the grid provider
#' @references ics1216
#' @noRd
#'
loadGridArguments <- function(gridProvider) {
  runserverToos <- getFromCache(gridProvider,actualLoadGridArguments,gridArgumentsCacheList,NULL)
  return(runserverToos)
}

actualLoadGridArguments <- function(gridProvider) {
  df <- restGetAsDf("/configuration/gridArguments/{gridProvider}/definitions",
                    urlParams = list(gridProvider = gridProvider),
                    nested = TRUE)
  if (is.null(df)) {
    log_warn("no gridArguments for provider", gridProvider)
  }
  return(df)
}



#' unloadGridArguments
#' @param gridProvider label of the grid provider
#' @references ics1216
#' @noRd
unloadGridArguments <- function(gridProvider) {
  removeFromCache(gridProvider,"",gridArgumentsCacheList)
}

#' refreshGridArguments reloads the grid arguments from the repository
#' @param gridProvider label of the grid provider
#' @references ics1216
#' @noRd
refreshGridArguments <- function(gridProvider) {
  unloadGridArguments(gridProvider)
  res <- loadGridArguments(gridProvider)
  return(res)
}

#' @rdname refreshGridArguments
#' @noRd
updateGridArguments <- function(...) {
  .Deprecated("refreshGridArguments")
  refreshGridArguments(...)
}

#' loadGridArgumentDefinition loads grid argument by gridProvider and name
#' @param gridProvider label of the grid provider
#' @param argumentName name of the argument
#' @references ics1216
#' @noRd
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
