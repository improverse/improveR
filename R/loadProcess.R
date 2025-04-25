
processesForStepsCacheList <- list(
  processesForStepsCache="stepId",
  processesForStepsByIdCache="id"
)

#' loadProcessesForStep
#'
#' @param stepIdent resourceId of the step or the step
#' @references ics1218
#' @export
loadProcessesForStep <- function(stepIdent) {
  step <- loadResource(stepIdent)
  if (is.null(step)) {
    return(NULL)
  }
  if (step$nodeType!="Step") {
    log_warn("loadProcessesForStep only possible for type Step.",stepIdent,"is of type",step$nodeType)
    return(NULL)
  }
  processes <- getFromCache(step$resourceId,actualLoadProcessesForStep,processesForStepsCacheList,NULL)
  return(processes)
}

#' loadProcessesForStepById
#'
#' @param processId processId of the process
#' @references ics1218
#' @export
loadProcessesForStepById <- function(processId) {
  processes <- getFromCache(processId,function(...){},processesForStepsCacheList,NULL)
  if (is.null(processes)) {
    log_info("no process found for the process id:",processId," please note, process can only be loaded via process ID if it has been once loaded via the step.")
  }
  return(processes)
}

actualLoadProcessesForStep <- function(stepIdent) {

  result <- authenticatedREST('resources/{stepId}/processes',
                                            urlParams = list(stepId=stepIdent
                                            ),
                                            restType = "GET")
  processes <- httr::content(result)
  #processes<-plyr::rbind.fill(lapply(processes,function(process) {
  #  process$subProcesses<-""
  #  return(as.data.frame(process,stringsAsFactors=F))}))
  processes <- mergeNestedListToDataframe(processes)
  processes$stepId <- stepIdent
  return(processes)
}

#' unloadProcessesForStep
#' @param stepIdent resourceId of the step or the step
#' @references ics1218
#' @export
unloadProcessesForStep <- function(stepIdent) {
  loadProcessesForStep(stepIdent)
  removeFromCache(stepIdent,"",processesForStepsCacheList)
}

#' updateProcessesForStep reloads the processes for a step
#' @param stepIdent resourceId of the step or the step
#' @references ics1218
#' @export
updateProcessesForStep <- function(stepIdent) {
  unloadProcessesForStep(stepIdent)
  res <- loadProcessesForStep(stepIdent)
  return(res)
}

#' processGridProvider loads the grid provider for a given process
#' @param process the data frame for the process
#' @references ics1218
#' @export
processGridProvider <- function(process) {
  tool <- processTool(process)
  return(tool$gridProvider)
}

#' processTool loads the tool for a given process
#' @param process the data frame for the process
#' @references ics1218
#' @export
processTool <- function(process) {
  runservers <- loadRunservers()
  runserver <- runservers[runservers$id==process$runserverId,]
  tools <- loadToolsForRunserver(runserver$id)
  tool <- tools[tools$id==process$runserverToolId,]
  return(tool)
}

processGridArgumentsCacheList <- list(
  processGridArgumentsCache="processId"
)

#' loadProcessGridArguments loads the tool for a given process
#' @param processId id of the process
#' @references ics1218
#' @export
loadProcessGridArguments <- function(processId) {
  dfs <- getFromCache(processId,actualLoadProcessGridArguments,processGridArgumentsCacheList,NULL)
  return(dfs)
}

actualLoadProcessGridArguments <- function(processId) {
  process <- loadProcessesForStepById(processId)
  if (is.null(process)) {
    return(NULL)
  }
  gridArgumentsResponse <- authenticatedREST("/resources/{resourceId}/processes/{processId}/gridArguments",
                                                           urlParams = list(resourceId=process$stepId,
                                                                            processId=process$id))
  gridArgumentsContent <- httr::content(gridArgumentsResponse)
  dfs <- mergeNestedListToDataframe(gridArgumentsContent)
  if (is.null(dfs) || nrow(dfs)==0) {
    #return(data.frame(processId=processId))
    return(NULL)
  }
  provider <- processGridProvider(process)
  gridArgumentDefinitions <- loadGridArguments(provider)
  colnames(gridArgumentDefinitions)[colnames(gridArgumentDefinitions) == 'id'] <- 'definitionId'
  dfs <- merge(dfs,gridArgumentDefinitions,by="definitionId")
  return(dfs)
}

#' unloadProcessGridArguments
#' @param processId id of the process
#' @references ics1218
#' @export
unloadProcessGridArguments <- function(processId) {
  loadProcessGridArguments(processId)
  removeFromCache(processId,"",processGridArgumentsCacheList)
}

#' updateLoadProcessGridArguments reloads the process grid arguments for a process
#' @param processId id of the process
#' @references ics1218
#' @export
updateProcessGridArguments <- function(processId) {
  unloadProcessGridArguments(processId)
  res <- loadProcessGridArguments(processId)
  return(res)
}


processVariablesCacheList <- list(
  processVariablesCache="processId"
)

#' loadProcessVariables loads the variables for a given process
#' @param processId id of the process
#' @references ics1218
#' @export
loadProcessVariables <- function(processId) {
  dfs <- getFromCache(processId,actualLoadProcessVariables,processVariablesCacheList,NULL)
  return(dfs)
}

actualLoadProcessVariables <- function(processId) {
  process <- loadProcessesForStepById(processId)
  if (is.null(process)) {
    return(NULL)
  }
  variableResponse <- authenticatedREST("/resources/{resourceId}/processes/{processId}/variables",
                                                         urlParams = list(resourceId=process$stepId,
                                                                          processId=process$id))
  variableResponse <- httr::content(variableResponse)
  dfs <- mergeNestedListToDataframe(variableResponse)
  return(dfs)
}

#' unloadProcessVariables
#' @param processId id of the process
#' @references ics1218
#' @export
unloadProcessVariables <- function(processId) {
  loadProcessVariables(processId)
  removeFromCache(processId,"",processVariablesCacheList)
}

#' updateProcessVariables reloads the process variables for a process
#' @param processId id of the process
#' @references ics1218
#' @export
updateProcessVariables <- function(processId) {
  unloadProcessVariables(processId)
  res <- loadProcessVariables(processId)
  return(res)
}

processRunsCacheList <- list(
  processRunsCache="processId"
)

#' loadProcessRun loads the runs for a given process
#' @param processId id of the process
#' @references ics1218
#' @export
loadProcessRuns <- function(processId) {
  dfs <- getFromCache(processId,actualLoadProcessRuns,processRunsCacheList,NULL)
  return(dfs)
}

actualLoadProcessRuns <- function(processId) {
  process <- loadProcessesForStepById(processId)
  if (is.null(process)) {
    return(NULL)
  }
  runsResponse <- authenticatedREST("/resources/{resourceId}/processes/{processId}/runs",
                                                      urlParams = list(resourceId=process$stepId,
                                                                       processId=process$id))
  if (is.null(runsResponse)) {
    return(NULL)
  }
  runsResponse <- httr::content(runsResponse)
  dfs <- mergeNestedListToDataframe(runsResponse)
  dfs$processId <- processId
  return(dfs)
}

#' unloadProcessRuns
#' @param processId id of the process
#' @references ics1218
#' @export
unloadProcessRuns <- function(processId) {
  loadProcessRuns(processId)
  removeFromCache(processId,"",processRunsCacheList)
}

#' updateProcessRuns reloads the process runs for a process
#' @param processId id of the process
#' @references ics1218
#' @export
updateProcessRuns <- function(processId) {
  unloadProcessRuns(processId)
  res <- loadProcessRuns(processId)
  return(res)
}


#' getMainProcess
#'
#' @param stepIdent resourceId of the step
#' @references ics1218
#' @export
getMainProcess <- function(stepIdent) {
  processes <- loadProcessesForStep(stepIdent)
  return(processes[processes$processType=="main",])
}
