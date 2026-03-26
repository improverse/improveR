#' createStep
#'
#' @param treeIdent id of the containing tree
#' @param parentStepIdent optional id of the parent step
#' @param toolId optional id of the tool
#' @references ics1140
#' @noRd
createStep <- function(treeIdent,parentStepIdent=NULL,toolId=NULL) {
  improveEditable()
  tree <- loadResource(treeIdent)
  if (is.null(tree)) {
    log_warn("Target does not exist")
    return(NULL)
  }
  else {
    if (!isAllowedTarget(tree$nodeType,"Step",logWarning=T)) {
      return(NULL)
    }
    data <- list(nodeType="Step",toolCategory="R",tool="R",runserver="Run11b",comment="improveRW",toolId=toolId)
    if (!is.null(parentStepIdent)) {
      parent <- loadResource(parentStepIdent)
      data <- list(nodeType="Step",toolCategory="Nonmem",tool="7.5-gFortran",runserver="Run11b",comment="improveRW",parentStepId=parent$resourceId,toolId=toolId)
    }

    result <- authenticatedREST('/resources/{treeId}/steps',
                                              urlParams = list(treeId=tree$resourceId
                                              ),
                                              data=data,
                                              restType = "POST")
    step <- httr::content(result)
    #TODO update children ...
    unloadChildResources(treeIdent)
    unloadFullChildResources(treeIdent)
    return(loadResource(step$resourceId))
  }
}








#' setProcessVariables
#'
#' @param stepId id of the step the variables refer to
#' @param processId id of the process the variables refer to
#' @param processType processType
#' @param name name
#' @param selected selected
#' @param runserverId runserverId
#' @param toolId toolId
#' @param gridTool gridTool
#' @param runserverToolId runserverToolId
#' @param toolIgnorePatterns toolIgnorePatterns
#' @param toolArguments toolArguments
#' @param mainProcess if this process is status relevant, there needs to be exactly one mainProcess
#' @param position the position in the process list
#' @param parentProcessId only to use when processType equals sub
#' @param toolBrowserUrl a URL improve uses to automatically open while the step is running
#' @param toolDeletePatterns files that won't get checked in
#' @param toolStreamablePatterns file that can be streamed to monitor the step
#' @noRd
#'
setProcessVariables <- function(stepId, processId,processType="main",name="Main",selected=TRUE,runserverId="",toolId="",runserverToolId="",toolIgnorePatterns="",mainProcess=T,gridTool,toolArguments=NULL,position=1,parentProcessId = NULL,toolBrowserUrl=NULL,toolDeletePatterns=NULL,toolStreamablePatterns="") {

  processVariables <- list(
    processType=processType,
    position=position,
    name=name,
    selected=replaceTF(selected),
    runserverId=runserverId,
    toolId=toolId,
    runserverToolId=runserverToolId,
    toolIgnorePatterns=toolIgnorePatterns,
    main=replaceTF(mainProcess),
    gridTool=replaceTF(gridTool))

  if (!is.null(toolArguments)) {
    processVariables$toolArgs <- toolArguments
  }
  if (!is.null(parentProcessId)) {
    processVariables$parentProcessId <- parentProcessId
  }
  if (!is.null(toolBrowserUrl)) {
    processVariables$toolBrowserUrl <- toolBrowserUrl
  }
  if (!is.null(toolDeletePatterns)) {
    processVariables$toolDeletePatterns <- toolDeletePatterns
  }
  if (!is.null(toolStreamablePatterns)) {
    processVariables$toolStreamablePatterns <- toolStreamablePatterns
  }

  result <- authenticatedREST('/resources/{stepId}/processes/{processId}',
                                            urlParams = list(stepId=stepId,
                                                             processId=processId
                                            ),
                                            data=processVariables,
                                            restType = "PUT")
  result <- refreshProcessesForStep(stepId)
}


replaceTF <- function(str) {
  if (str=="TRUE") {
    return("true")
  }
  if (str=="FALSE") {
    return("false")
  }
  return(str)
}

setProcessVariablesList <- function(stepId, processId,processVariables) {

  processVariables <- processVariables[,!(names(processVariables) %in% "subProcesses")]
  processVariables <- processVariables %>%
    dplyr::mutate_all(as.character)%>%
    dplyr::mutate_all(replaceTF)


 result <- authenticatedREST('/resources/{stepId}/processes/{processId}',
                                            urlParams = list(stepId=stepId,
                                                             processId=processId
                                            ),
                                            data=as.list(processVariables),
                                            restType = "PUT")

}


#' Get Process File Variables
#'
#' Retrieves file variable definitions for a specific process within a step.
#'
#' @param ident A step identifier. Can be the step's path, resource (version) id,
#'   full entity (version) id, or short entity (version) id.
#' @param processId Identifier of the process (main or sub-process) whose variables
#'   should be returned. Use \code{\link{loadProcessesForStep}} to discover process ids.
#'
#' @returns A data frame with one row per process file variable. Columns include:
#'   \describe{
#'     \item{id}{Character. Variable identifier on the server.}
#'     \item{position}{Integer. Position ordering in the process variable list.}
#'     \item{name}{Character. Variable name.}
#'     \item{variableType}{Character. Typically \code{fileRef} or \code{filePath}.}
#'     \item{valueResourceId}{Character. Resource id when \code{variableType} is \code{fileRef}.}
#'     \item{processId}{Character. Process identifier that owns the variable.}
#'   }
#'   Additional columns may be returned depending on server version.
#'
#' @details
#' Process file variables connect a step process to its file inputs. Variables of
#' type \code{fileRef} point to improve resources, while \code{filePath} variables
#' reference literal paths for the runserver.
#'
#' @seealso \code{\link{loadProcessesForStep}} to list available processes,
#'   \code{\link{createProcessFileVariable}} to create variables
#'
#' @examples
#' \dontrun{
#' processes <- loadProcessesForStep("/improve-tutorial/Modeling/Step 1")
#' main_process <- processes[processes$processType == "main", ]$id[1]
#' variables <- getProcessFileVariables(
#'   ident = "/improve-tutorial/Modeling/Step 1",
#'   processId = main_process
#' )
#' }
#'
#' @export
getProcessFileVariables <- function(ident, processId) {
  step <- loadResource(ident)
  result <- authenticatedREST('/resources/{stepId}/processes/{processId}/variables',
                                            urlParams = list(stepId=step$resourceId,
                                                             processId=processId
                                            ),
                                            restType = "GET")

  variables <- httr::content(result)
  variablesDf <- plyr::rbind.fill(lapply(variables,as.data.frame))
  return(variablesDf)
}

#' createProcessFileVariable
#'
#' @param ident id of the step the variables refer to
#' @param processId id of the process the variables refer to
#' @param name name of the variable
#' @param variableType fileRef or filePath
#' @param position position of the variable
#' @noRd
#'
createProcessFileVariable <- function(ident, processId,name,variableType,position) {
  step <- loadResource(ident)
  variableData <- list(
    type="processVariable",
    name=name,
    variableType=variableType,
    position=position
  )
  result <- authenticatedREST('/resources/{stepId}/processes/{processId}/variables',

                              urlParams = list(stepId=step$resourceId,
                                               processId=processId
                              ),
                              data=variableData,
                              restType = "POST")

  variables <- httr::content(result)
  variablesDf <- plyr::rbind.fill(lapply(variables,as.data.frame))
  return(variablesDf)
}

#' Detach Step from Parent
#'
#' Removes the hierarchical relationship between a step and its parent, allowing
#' users to reorganize workflows, isolate steps, or prepare steps for reassignment
#' to different parents.
#'
#' @param ident Identifier of the step to detach. Can be the step's path, resource (version) id,
#'   full entity (version) id, or short entity (version) id.
#' @param from Root directory for resolving relative paths. Default is \code{pwd()}.
#'
#' @return The updated step resource, returned invisibly after cache invalidation.
#'
#' @seealso
#' \code{\link{attachStep}} to create parent-child relationships,
#' \code{\link{loadParentStep}} and \code{\link{loadChildSteps}} for navigating hierarchies,
#' \code{\link{getStep}} for retrieving complete step environments
#'
#' @examples
#' \dontrun{
#' # Detach step from its parent
#' detachStep(ident = "/improve-tutorial/demoSteps/Step 2")
#'
#' # Verify the step no longer has a parent
#' loadParentStep("/improve-tutorial/demoSteps/Step 2")
#' }
#'
#' @references ics1225
#' @export

detachStep <- function(ident,  from=pwd()) {
  stepEntity <- loadResource(ident,from)
  tree <- stepEntity$parentId
  parentStep <- loadParentStep(stepEntity) #NULL 
  result <- authenticatedREST("/resources/{treeId}/steps/{stepId}/detachStepFromParent",
                                            urlParams = list(stepId=stepEntity$resourceId,treeId=tree),
                                            restType = "PUT"
  )
  unloadChildSteps(parentStep)
  unloadParentStep(ident,from)
  return(refreshResource(ident,from))
}

#' Attach a Step to a Parent Step
#'
#' Establishes a hierarchical parent-child relationship between steps
#' by attaching a step to another step that serves as its parent.
#'
#' @param ident Identifier of the step to attach. Can be the step's path, resource (version) id,
#'   full entity (version) id, or short entity (version) id.
#' @param parent Identifier of the step that will become the parent. Can be the step's path,
#'   resource (version) id, full entity (version) id, or short entity (version) id.
#' @param from Root directory for resolving relative paths. Default is \code{pwd()}.
#'
#' @return The updated step resource, returned invisibly after cache invalidation.
#' @seealso
#' \code{\link{detachStep}} to remove parent-child relationships,
#' \code{\link{loadParentStep}} and \code{\link{loadChildSteps}} for navigating hierarchies,
#' \code{\link{getStep}} for retrieving complete step environments
#'
#' @examples
#' \dontrun{
#' # Attach Step 2 as a child of Step 1
#' attachStep(
#'   ident = "/improve-tutorial/demoSteps/Step 2",
#'   parent = "/improve-tutorial/demoSteps/Step 1"
#' )
#' }
#'
#' @references ics1225
#' @export

attachStep <- function(ident, parent, from = pwd()) {
  stepEntity <- loadResource(ident, from)
  stepParentEntity <- loadResource(parent, from)

  tree <- stepEntity$parentId

  result <- authenticatedREST(
    "/resources/{treeId}/steps/{stepId}/attachStepToParent",
    urlParams = list(stepId = stepEntity$resourceId, treeId = tree),
    queryParams = list(parentStepId = stepParentEntity$resourceId),
    restType = "PUT"
  )

  if (is.null(result)) {
    log_warn("Step was not attached.")
  }

  unloadParentStep(stepEntity, from)
  unloadChildSteps(stepParentEntity)
  return(refreshResource(ident, from))
}

#' createProcess
#'
#' @param stepId id of the step the variables refer to
#' @param processType processType
#' @param name name
#' @param selected selected
#' @param runserverId runserverId
#' @param toolId toolId
#' @param gridTool gridTool
#' @param runserverToolId runserverToolId
#' @param toolIgnorePatterns toolIgnorePatterns
#' @param toolArguments toolArguments
#' @param mainProcess if this process is status relevant, there needs to be exactly one mainProcess
#' @param position the position in the process list
#' @param parentProcessId only to use when processType equals sub
#' @param toolBrowserUrl a URL improve uses to automatically open while the step is running
#' @param toolDeletePatterns files that won't get checked in
#' @param toolStreamablePatterns file that can be streamed to monitor the step
#' @noRd
#'
createProcess <- function(stepId,processType="main",name="Main",selected=TRUE,runserverId="",toolId="",runserverToolId="",toolIgnorePatterns="",mainProcess=T,gridTool,toolArguments=NULL,position=1,parentProcessId = NULL,toolBrowserUrl=NULL,toolDeletePatterns=NULL,toolStreamablePatterns="") {

  processVariables <- list(
    processType=processType,
    position=position,
    name=name,
    selected=replaceTF(selected),
    runserverId=runserverId,
    toolId=toolId,
    runserverToolId=runserverToolId,
    toolIgnorePatterns=toolIgnorePatterns,
    main=replaceTF(mainProcess),
    gridTool=replaceTF(gridTool))

  if (!is.null(toolArguments)) {
    processVariables$toolArgs <- toolArguments
  }
  if (!is.null(parentProcessId)) {
    processVariables$parentProcessId <- parentProcessId
  }
  if (!is.null(toolBrowserUrl)) {
    processVariables$toolBrowserUrl <- toolBrowserUrl
  }
  if (!is.null(toolDeletePatterns)) {
    processVariables$toolDeletePatterns <- toolDeletePatterns
  }
  if (!is.null(toolStreamablePatterns)) {
    processVariables$toolStreamablePatterns <- toolStreamablePatterns
  }

  result <- authenticatedREST('/resources/{stepId}/processes/',
                              urlParams = list(stepId=stepId
                              ),
                              data=processVariables,
                              restType = "POST")
  result <- refreshProcessesForStep(stepId)
}


#' Set Command File on a Realised Step
#'
#' Binds a file as the command-file process variable on an already-realised step.
#' This is used when a step has been created and realised, but the command file
#' needs to be set or changed after the fact.
#'
#' @param stepIdent Identifier of the step. Can be a path, resource ID, entity ID,
#'   or a data frame row from \code{loadResource()}.
#' @param fileIdent Identifier of the file to bind as command file. Can be a path,
#'   resource ID, entity ID, or a data frame row from \code{loadResource()}.
#' @param from Base path for resolving relative paths. Defaults to \code{pwd()}.
#'
#' @returns The updated process variables data frame, invisibly. Returns \code{NULL} on failure.
#'
#' @examples
#' \dontrun{
#' setCommandFile("/Projects/MyTree/Step 1", "/Projects/Files/myScript.R")
#' }
#' @seealso \code{\link{getMainProcess}}, \code{\link{getProcessFileVariables}}
#' @references ccs40
#' @export
setCommandFile <- function(stepIdent, fileIdent, from = pwd()) {
  improveEditable()

  step <- loadResource(stepIdent, from)
  if (is.null(step)) {
    log_warn("setCommandFile: cannot find step:", stepIdent)
    return(NULL)
  }

  file <- loadResource(fileIdent, from)
  if (is.null(file)) {
    log_warn("setCommandFile: cannot find file:", fileIdent)
    return(NULL)
  }

  mainProcess <- getMainProcess(step)
  if (is.null(mainProcess)) {
    log_warn("setCommandFile: no main process found for step:", step$resourceId)
    return(NULL)
  }
  processId <- mainProcess$id

  # Get existing variables and look for command-file
  variables <- getProcessFileVariables(step, processId)
  variableId <- NULL

  if (!is.null(variables) && nrow(variables) > 0) {
    cmdFileRow <- variables[variables$name == "command-file", ]
    if (nrow(cmdFileRow) > 0) {
      variableId <- cmdFileRow$id[1]
    }
  }

  # Create the variable if it doesn't exist
  if (is.null(variableId)) {
    newVars <- createProcessFileVariable(step, processId, "command-file", "fileRef", 0)
    if (is.null(newVars)) {
      log_warn("setCommandFile: failed to create command-file variable")
      return(NULL)
    }
    cmdFileRow <- newVars[newVars$name == "command-file", ]
    if (nrow(cmdFileRow) == 0) {
      log_warn("setCommandFile: command-file variable not found after creation")
      return(NULL)
    }
    variableId <- cmdFileRow$id[1]
  }

  # Bind the file to the variable
  data <- list(
    type = "processVariable",
    id = variableId,
    name = "command-file",
    variableType = "fileRef",
    valueResourceId = file$resourceId
  )

  result <- authenticatedREST(
    "/resources/{stepId}/processes/{processId}/variables/{variableId}",
    urlParams = list(stepId = step$resourceId, processId = processId, variableId = variableId),
    data = data,
    restType = "PUT")

  if (is.null(result)) {
    log_warn("setCommandFile: failed to bind file to command-file variable")
    return(NULL)
  }

  return(invisible(getProcessFileVariables(step, processId)))
}
