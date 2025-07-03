
#Step is a fully loaded step, workflow is a fully loaded workflow as df
#all edit functions for a step are in a steptemplate (code sourced into env)
#a steptemplate is always in a workflowtemplate
#a workflowtemplate has a list of all used files and their references
#a workflowtemplate can directly have parameters
#a workflowtemplate has merge / intersect, subset, ... functionalities
# execute workflow is built in a way that dispatcher works also
#check if all tool parameters can be set
#check runs against processes


getStep <- function(ident) {
  ident <- "envhost1.hc.scintecodev.internal-5310:ST-79162"

  step <- loadResource(ident)
  restResult <- authenticatedREST(url = "/resources/{resourceId}",urlParams = list(resourceId = step$resourceId))
  restContent <- httr::content(restResult)

  restResult <- authenticatedREST(url = "/resources/{resourceId}/resources",urlParams = list(resourceId = step$resourceId))

  restContent <- httr::content(restResult)


  handle <- handleFromStep(ident)

}


getToolInstances <- function() {
  runservers <- improveR::loadRunservers()
  runservers <- dplyr::filter(runservers,!local)
  runserverTools <- byNotEmptyAsDf(runservers,function(runserver) {
    return(loadToolsForRunserver(runserver$id))
  })
  runserverTools <- dplyr::mutate(runserverTools,fullName=paste(categoryName,toolName,name,label))
  toolInstanceEnv <- new.env()
  x <- byNotEmpty(runserverTools,function(runserverTool) {
    assign(x=runserverTool$fullName,value=runserverTool,envir =toolInstanceEnv )
  })
}


#' handleFromStep
#' reads a step and its processes and generates a new handle
#' @param stepId the ident of the step
#' @references ics1213
#' @export
handleFromStep <- function(stepId) {

  stepHandle <- uuid::UUIDgenerate()

  ident <- "envhost1.hc.scintecodev.internal-5310:ST-79162"
  step <- loadResource(ident)
  tree <- loadResource(step$parentId)
  restResult <- authenticatedREST(url = "/resources/{resourceId}",
                                  urlParams = list(resourceId = step$resourceId),
                                  queryParams = list(optParams="processes&optParams=inventory"))

  restContent <- httr::content(restResult)

  processes <-mergeNestedListToDataframe(restContent$processes)
  processes <- processes[order(processes$position),]


  step <- loadResource(stepId)
  tree <- loadResource(step$parentId)
  processesOld <- updateProcessesForStep(stepIdent = stepId)
  processesOld <- processesOld[order(processes$position),]

  processesOldDfs <- byNotEmptyAsDf(processesOld,function(process){fullProcess(process,stepHandle)})

  processes$handle <- stepHandle
  processDfs <- dplyr::select(processes,
                              handle,runserverLabel,toolLabel,toolInstance,toolArgs,toolStreamablePattern,
                              selected,gridTool,main,name,processType,position)


  newHandle <- data.frame(handle=stepHandle,stringsAsFactors = F)
  newHandle$processes <- list(processDfs)
  newHandle$treeIdent<-step$parentId
  newHandle$treeName <- tree$name
  newHandle$treePath <- dirname(tree$path)
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

