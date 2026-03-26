
#' fullProcess
#' collect all data for one process
#' cycle with old version. test usage of correct run, with revision
#' @param process the process loaded from the repo
#' @param stepHandle the stepHandle the process will be added to
#' @noRd

fullProcess <- function(process,stepHandle) {
  processId <- process$id
  newHandle <- data.frame(handle=processId,stringsAsFactors = F)
  newHandle$runserverName <- process$runserverLabel
  newHandle$toolName <- process$toolLabel
  newHandle$runserverToolName<-process$toolInstance
  newHandle$toolArgs <- process$toolArgs
  newHandle$toolDeletePatterns <- process$toolDeletePatterns
  newHandle$toolStreamablePatterns <- process$toolStreamablePatterns
  newHandle$selected <- process$selected
  newHandle$gridTool <- process$gridTool
  newHandle$main <- process$main
  newHandle$name <- process$name
  newHandle$processType <- process$processType
  newHandle$position <- process$position
  newHandle$parentProcessId <- process$parentProcessId

  gridArguments <- refreshProcessGridArguments(process$id)

  if (!is.null(gridArguments)) {
    newHandle$gridArguments <- list(
      byNotEmptyAsDf(gridArguments,function(ga) {
        gridHandle <- data.frame(handle=stepHandle)
        gridHandle$argumentName<- ga$name

        if (ga$gridArgumentType=="LOV") {
          categoryValues <- ga$category[[1]]$values[[1]]
          gridHandle$argumentValue <- categoryValues[categoryValues$id==ga$lovValueId,]$text
        } else if (ga$gridArgumentType=="TEXT") {
          gridHandle$argumentValue <- ga$textValue
        } else if (ga$gridArgumentType=="DATE_TIME") {
          gridHandle$argumentValue <- round(as.numeric(ga$dateValue)/1000)
        }
        return(gridHandle)
      })
    )
  }
  return(newHandle)
}



getProcessVariableForFile <- function (stepIdent,resourceIdent) {
  step <- loadResource(stepIdent)
  resource <- loadResource(resourceIdent)
  process <- loadProcessesForStep(step)
  variables <- loadProcessVariables(process$id)

  variableName <- NULL
  variable <- variables[variables$valueResourceId==resource$resourceId,]
  if (nrow(variable)==1) {
    variableName<-variable$name
  }
  return(variableName)
}

add2List <- function(valueList,value) {
  if (is.null(value)) {
    return(valueList)
  }
  value <- mergeDataframeList(value)
  if (is.null(valueList)) {
    valueList <- value
  } else {
    valueList <- plyr::rbind.fill(valueList,value)
  }
  return(valueList)
}

