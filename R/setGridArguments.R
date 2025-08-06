
#' setGridArgument
#'
#' @param processId id of the preocess
#' @param argumentName name of the grid argument
#' @param argumentValue value of the grid argument
#' @param update updates if one value is already set, if not adds, defaults to TRUE
#' @references ics1222
#' @export
setGridArgument <- function(processId,argumentName,argumentValue,update=T) {
  improveEditable()
  process <- loadProcessesForStepById(processId = processId)

  gridArguments <- loadProcessGridArguments(process$id)
  gridArgument <- gridArguments[gridArguments$name==argumentName,]

  if (!is.null(gridArgument) && nrow(gridArgument) == 1 && update) {
    return(updateGridArgument(processId,argumentName,argumentValue))
  } else if (!is.null(gridArgument) && nrow(gridArgument) > 1 && update) {
    logging::logwarn(paste0(process$stepId," ",argumentName))
    logging::logwarn("can only update if gridargument exists only once")
    return(FALSE)
  }
  gridProvider <- processGridProvider(process)
  gridArgumentDefinitions <- loadGridArguments(gridProvider)
  gridArgument <- gridArgumentDefinitions[gridArgumentDefinitions$name==argumentName,]
  gridData <- buildDataList(gridArgument,argumentValue)
  setGridArgument <- authenticatedREST(restType = "POST",
                                                     "/resources/{resourceId}/processes/{processId}/gridArguments",
                                                     urlParams = list(resourceId=process$stepId,
                                                                      processId=process$id),
                                                     data = gridData)

  return(updateProcessGridArguments(process$id))
}

buildDataList <- function(gridArgument,argumentValue) {
  gridData <- list()
  if ("definitionId" %in% names(gridArgument)) {
    gridData$definitionId <- gridArgument$definitionId
  } else {
    gridData$definitionId <- gridArgument$id
  }
  if (gridArgument$gridArgumentType=="LOV") {
    gridData$categoryId<-gridArgument$categoryId
    categoryValues <- gridArgument$category[[1]]$values[[1]]
    lovId <- categoryValues[categoryValues$text==argumentValue,]$id
    gridData$lovValueId<-lovId
  } else if (gridArgument$gridArgumentType=="TEXT") {
    gridData$textValue <- argumentValue
  } else if (gridArgument$gridArgumentType=="DATE_TIME") {
    #dateNum<-NA
    #tryCatch({
    #  dateNum <- as.numeric(argumentValue)
    #},warning=function(a){})
    #if (!is.na(dateNum)) {
    #  gridData$dateValue <- dateNum
    #} else {
    #  gridData$dateValue <- as.numeric(format(argumentValue,"%s"))*1000
    #}
    gridData$dateValue <- as.character(round(as.numeric(argumentValue)*1000))
  }
  return(gridData)
}

#' updateGridArgument
#'
#' @param processId id of the preocess
#' @param argumentName name of the grid argument
#' @param argumentValue value of the grid argument
#' @references ics1222
#' @export
updateGridArgument <- function(processId,argumentName,argumentValue) {
  improveEditable()
  process <- loadProcessesForStepById(processId = processId)

  if (!process$gridTool) {
    logging::logwarn(process$stepId)
    logging::logwarn("No grid tool, not setting grid arguments")
    return(FALSE)
  }

  gridArguments <- loadProcessGridArguments(process$id)
  gridArgument <- gridArguments[gridArguments$name==argumentName,]
  if (nrow(gridArgument) == 1) {
    gridData <- buildDataList(gridArgument,argumentValue)
    setGridArgument <- authenticatedREST(restType = "PUT",
                                                       "/resources/{resourceId}/processes/{processId}/gridArguments/{id}",
                                                       urlParams = list(resourceId=process$stepId,
                                                                        processId=process$id,
                                                                        id=gridArgument$id),
                                                       data = gridData)

    return(updateProcessGridArguments(process$id))
  } else  {
    logging::logwarn(paste0(process$stepId," ",argumentName))
    logging::logwarn("can only update if gridargument exists only once")
    return(FALSE)
  }



}

#' deleteGridArgumentsByName
#'
#' @param processId id of the process
#' @param argumentName name of the grid argument
#' @references ics1222
#' @export
deleteGridArgumentsByName <- function(processId,argumentName) {
  improveEditable()
  gridArguments <- loadProcessGridArguments(processId)
  gridArgument <- gridArguments[gridArguments$name==argumentName,]
  return(
    deleteGridArgumentsById(processId,gridArgument$id)
  )
}

#' deleteGridArgumentsById
#'
#' @param processId id of the process
#' @param ids ids of the grid arguments
#' @references ics1222
#' @export
deleteGridArgumentsById <- function(processId,ids) {
  improveEditable()
  process <- loadProcessesForStepById(processId = processId)
  gridArguments <- lapply(ids,function(id) {
    gridArgumentsResponse <- authenticatedREST(restType = "DELETE",url = "/resources/{resourceId}/processes/{processId}/gridArguments/{id}",
                                               urlParams = list(
                                                 resourceId=process$stepId,
                                                 processId=process$id,
                                                 id=id))
    gridArgumentsContent <- httr::content(gridArgumentsResponse)
  })
  return(updateProcessGridArguments(process$id))
}
