
#cycle with old version. test usage of correct run, with revision

#' collect all data for one process
#'
fullProcess <- function(process) {
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

  gridArguments <- process$gridArguments

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

#' handleFromStep
#' reads a step and it´s processes and generates a new handle
#' @param stepId the ident of the step
#' @references ics1213
#' @export
handleFromStep <- function(stepId) {

  stepHandle <- uuid::UUIDgenerate()

  step <- loadResource(stepId)

  processes <- loadProcessesForStep(stepIdent = stepId)
  processes <- processes[order(processes$position),]

  processDfs <- byNotEmptyAsDf(processes,fullProcess)

  newHandle <- data.frame(handle=stepHandle,stringsAsFactors = F)
  newHandle$processes <- list(processDfs)
  newHandle$treeIdent<-step$parentId

  newHandle$description<-step$description
  newHandle$rationale<-step$rationale
  newHandle$entityId<-step$entityId
  if (!startsWith(step$name,"Step ")) {
    newHandle$stepName <- step$name
  }


  selectedProcesses <- processes[processes$selected,]
  runs <- data.frame()
  if (nrow(selectedProcesses)>0) {
    runs <- loadProcessRuns(selectedProcesses[1,]$id)
  }


  inventory <- getStepResourceInventory(step,recurse=T)%>%strip()
  variables <- byNotEmptyAsDf(processes,
                              function(process) {
                                return(loadProcessVariables(process$id))
                              }
                                )

  # use DMG for files

  if (!is.data.frame(runs) || nrow(runs)==0) {
    inputFiles <- inventory
  } else {
    inputFiles <- inventory[inventory$nodeType=="File",]
    inputFiles <- inputFiles[inputFiles$createdAt<max(runs$startedAt),]
  }
  storeStep(stepHandle = stepHandle,stepList = newHandle)

  # load folders

  links <- inventory[inventory$nodeType=="Link",]
  linkHandles <- byNotEmpty(links,function(link) {
    variableName <- NULL
    variableProcess <-NULL
    variable <- variables[variables$valueResourceId==link$resourceId,]
    if (nrow(variable)==1) {
      variableName<-variable$name
      variableProcess<-loadProcessesForStepById(variable$processId)$name
    }
    addStepRemoteFile(stepHandle=stepHandle,ident = link,name = link$inventoryPath,asLink = T,variableName = variableName,variableProcess = variableProcess)

  })
  fileHandles <- byNotEmpty(inputFiles,function(f) {
    variableName <- NULL
    variableProcess <-NULL
    variable <- variables[variables$valueResourceId==f$resourceId,]
    if (nrow(variable)==1) {
      variableName<-variable$name
      variableProcess<-loadProcessesForStepById(variable$processId)$name
    }
    addStepRemoteFile(stepHandle=stepHandle,ident = f,name = f$inventoryPath,asLink = F,variableName = variableName,variableProcess = variableProcess)
    })

  externalLinks <- inventory[inventory$nodeType=="ExtLink",]
  linkHandles <- byNotEmpty(externalLinks,function(link) {

    addExtLink(stepHandle=stepHandle,name=link$inventoryPath,url = link$url)

  })


  return(stepHandle)
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

