
processesForStepsCacheList <- list(
  processesForStepsCache="stepId",
  processesForStepsByIdCache="id"
)

#' loadProcessesForStep
#'
#' @param stepIdent resourceId of the step or the step
#' @references ics1218
#' @noRd
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
#' @noRd
loadProcessesForStepById <- function(processId) {
  processes <- getFromCache(processId,function(...){},processesForStepsCacheList,NULL)
  if (is.null(processes)) {
    log_info("no process found for the process id:",processId," please note, process can only be loaded via process ID if it has been once loaded via the step.")
  }
  return(processes)
}

actualLoadProcessesForStep <- function(stepIdent) {
  processes <- restGetAsDf("resources/{stepId}/processes",
                           urlParams = list(stepId = stepIdent),
                           nested = TRUE)
  if (!is.null(processes) && nrow(processes) > 0) {
    processes$stepId <- stepIdent
  }
  return(processes)
}

#' unloadProcessesForStep
#' @param stepIdent resourceId of the step or the step
#' @references ics1218
#' @noRd
unloadProcessesForStep <- function(stepIdent) {
  step <- loadResource(stepIdent)
  if (is.null(step)) {
    return(NULL)
  }
  removeFromCache(step$resourceId,"",processesForStepsCacheList)
}

#' refreshProcessesForStep reloads the processes for a step
#' @param stepIdent resourceId of the step or the step
#' @references ics1218
#' @noRd
refreshProcessesForStep <- function(stepIdent) {
  unloadProcessesForStep(stepIdent)
  res <- loadProcessesForStep(stepIdent)
  return(res)
}

#' @rdname refreshProcessesForStep
#' @noRd
updateProcessesForStep <- function(...) {
  .Deprecated("refreshProcessesForStep")
  refreshProcessesForStep(...)
}

#' processGridProvider loads the grid provider for a given process
#' @param process the data frame for the process
#' @references ics1218
#' @noRd
processGridProvider <- function(process) {
  tool <- processTool(process)
  return(tool$gridProvider)
}

#' processTool loads the tool for a given process
#' @param process the data frame for the process
#' @references ics1218
#' @noRd
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
#' @noRd
loadProcessGridArguments <- function(processId) {
  dfs <- getFromCache(processId,actualLoadProcessGridArguments,processGridArgumentsCacheList,NULL)
  return(dfs)
}

actualLoadProcessGridArguments <- function(processId) {
  process <- loadProcessesForStepById(processId)
  if (is.null(process)) {
    return(NULL)
  }
  dfs <- restGetAsDf("/resources/{resourceId}/processes/{processId}/gridArguments",
                     urlParams = list(resourceId = process$stepId, processId = process$id),
                     nested = TRUE)
  if (is.null(dfs) || nrow(dfs) == 0) return(NULL)
  provider <- processGridProvider(process)
  gridArgumentDefinitions <- loadGridArguments(provider)
  if (is.null(gridArgumentDefinitions) || nrow(gridArgumentDefinitions) == 0) return(dfs)
  if (!("definitionId" %in% names(dfs))) return(dfs)
  colnames(gridArgumentDefinitions)[colnames(gridArgumentDefinitions) == 'id'] <- 'definitionId'
  dfs <- merge(dfs,gridArgumentDefinitions,by="definitionId")
  return(dfs)
}

#' unloadProcessGridArguments
#' @param processId id of the process
#' @references ics1218
#' @noRd
unloadProcessGridArguments <- function(processId) {
  removeFromCache(processId,"",processGridArgumentsCacheList)
}

#' refreshProcessGridArguments reloads the process grid arguments for a process
#' @param processId id of the process
#' @references ics1218
#' @noRd
refreshProcessGridArguments <- function(processId) {
  unloadProcessGridArguments(processId)
  res <- loadProcessGridArguments(processId)
  return(res)
}

#' @rdname refreshProcessGridArguments
#' @noRd
updateProcessGridArguments <- function(...) {
  .Deprecated("refreshProcessGridArguments")
  refreshProcessGridArguments(...)
}


processVariablesCacheList <- list(
  processVariablesCache="processId"
)

#' loadProcessVariables loads the variables for a given process
#' @param processId id of the process
#' @references ics1218
#' @noRd
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
  if (is.list(dfs) && length(dfs)==0) {
    return(NULL)
  }
  return(dfs)
}

#' unloadProcessVariables
#' @param processId id of the process
#' @references ics1218
#' @noRd
unloadProcessVariables <- function(processId) {
  removeFromCache(processId,"",processVariablesCacheList)
}

#' refreshProcessVariables reloads the process variables for a process
#' @param processId id of the process
#' @references ics1218
#' @noRd
refreshProcessVariables <- function(processId) {
  unloadProcessVariables(processId)
  res <- loadProcessVariables(processId)
  return(res)
}

#' @rdname refreshProcessVariables
#' @noRd
updateProcessVariables <- function(...) {
  .Deprecated("refreshProcessVariables")
  refreshProcessVariables(...)
}

processRunsCacheList <- list(
  processRunsCache="processId"
)

#' loadProcessRun loads the runs for a given process
#' @param processId id of the process
#' @references ics1218
#' @noRd
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
#' @noRd
unloadProcessRuns <- function(processId) {
  removeFromCache(processId,"",processRunsCacheList)
}

#' refreshProcessRuns reloads the process runs for a process
#' @param processId id of the process
#' @references ics1218
#' @noRd
refreshProcessRuns <- function(processId) {
  unloadProcessRuns(processId)
  res <- loadProcessRuns(processId)
  return(res)
}

#' @rdname refreshProcessRuns
#' @noRd
updateProcessRuns <- function(...) {
  .Deprecated("refreshProcessRuns")
  refreshProcessRuns(...)
}


#' Get Main Process for a Step
#'
#' Retrieves the main process configuration for a workflow step. The main process
#' defines how the step executes, including the tool (R, NONMEM, etc.), runserver,
#' and execution parameters.
#'
#' @param stepIdent Step identifier. Can be the step's path, resource ID,
#'   entity ID, or short entity ID.
#'
#' @returns A single-row data frame with 28 columns containing process configuration:
#'
#'   **Process Identification:**
#'   \describe{
#'     \item{id}{(character) Unique process identifier}
#'     \item{processType}{(character) Type of process, always "main" for this function}
#'     \item{position}{(integer) Position in the process list}
#'     \item{name}{(character) Process name (typically "Main")}
#'     \item{stepId}{(character) Step ID this process belongs to}
#'   }
#'
#'   **Runserver Configuration:**
#'   \describe{
#'     \item{runserverId}{(character) ID of the runserver}
#'     \item{runserverLabel}{(character) Label of the runserver}
#'     \item{runserverUrl}{(character) URL endpoint of the runserver}
#'   }
#'
#'   **Tool Configuration:**
#'   \describe{
#'     \item{toolId}{(character) Tool identifier}
#'     \item{toolLabel}{(character) Tool label (e.g., "R4.2")}
#'     \item{toolInstance}{(character) Tool instance name}
#'     \item{toolArgs}{(character) Tool arguments including environment variables}
#'     \item{toolStreamablePatterns}{(character) File patterns for streaming (e.g., "*.txt\\r\\n*.out")}
#'     \item{runserverToolId}{(character) Runserver-specific tool ID}
#'     \item{gridTool}{(logical) Whether this is a grid computing tool}
#'     \item{repoTool}{(logical) Whether this is a repository tool}
#'     \item{main}{(logical) Whether this is the main process}
#'   }
#'
#'   **Execution Status:**
#'   \describe{
#'     \item{runStatus}{(character) Current status (e.g., "FINISHED", "RUNNING")}
#'     \item{runResult}{(character) Execution result (e.g., "COMPLETED", "FAILED")}
#'     \item{runId}{(character) Current or last run identifier}
#'     \item{startedAt}{(character) Start timestamp in milliseconds since epoch}
#'     \item{stoppedAt}{(character) Stop timestamp in milliseconds since epoch}
#'     \item{selected}{(logical) Whether this process is selected for execution}
#'     \item{deleted}{(logical) Whether this process has been deleted}
#'   }
#'
#'   **Nested Data (list columns):**
#'   \describe{
#'     \item{variables}{List of process variables (e.g., command-file references)}
#'     \item{subProcesses}{List of subprocess configurations}
#'     \item{gridArguments}{List of grid computing arguments}
#'     \item{runs}{List of historical run information}
#'   }
#'
#' @seealso
#' \code{\link{getStep}} for retrieving full step environments,
#' \code{\link{runStepResource}} for executing steps,
#' \code{\link{loadRunserver}} for runserver details,
#' \code{\link{getToolInstances}} for available tools
#'
#' @examples
#' \dontrun{
#' # Get main process for a step
#' mainProcess <- getMainProcess("/my-project/workflow/Step 1")
#'
#' # Check execution status
#' mainProcess$runStatus
#' mainProcess$runResult
#'
#' # Get tool information
#' mainProcess$toolLabel
#' mainProcess$runserverLabel
#' }
#'
#' @references ics1218
#' @export
getMainProcess <- function(stepIdent) {
  processes <- loadProcessesForStep(stepIdent)
  return(processes[processes$processType=="main",])
}
