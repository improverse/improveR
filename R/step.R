#' createStep
#'
#' @param treeIdent id of the containing tree
#' @param parentStepIdent optional id of the parent step
#' @param toolId optional id of the tool
#' @references ics1140
#' @export
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
#'
#' @export
setProcessVariables <- function(stepId, processId,processType="main",name="Main",selected=TRUE,runserverId="",toolId="",runserverToolId="",toolIgnorePatterns="",mainProcess=T,gridTool,toolArguments=NULL) {

  processVariables <- list(
    processType=processType,
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

  result <- authenticatedREST('/resources/{stepId}/processes/{processId}',
                                            urlParams = list(stepId=stepId,
                                                             processId=processId
                                            ),
                                            data=processVariables,
                                            restType = "PUT")
  result <- updateProcessesForStep(stepId)
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


#' getProcessFileVariables
#'
#' @param ident id of the step the variables refer to
#' @param processId id of the process the variables refer to
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

#' detaches a step from its parent
#'
#' @param ident ident of the step
#' @param from from if a relative path is used
#' @references ics1225
#' @export

detachStep <- function(ident,  from=pwd()) {
  stepEntity <- loadResource(ident,from)
  tree <- stepEntity$parentId
  parentStep <- loadParentStep(stepEntity)
  result <- authenticatedREST("/resources/{treeId}/steps/{stepId}/detachStepFromParent",
                                            urlParams = list(stepId=stepEntity$resourceId,treeId=tree),
                                            restType = "PUT"
  )
  unloadChildSteps(parentStep)
  unloadParentStep(ident,from)
  return(updateResource(ident,from))
}

#' attaches a step to its parent
#' @references ics1225
#' @param ident ident of the step
#' @param parent ident of the new parent
#' @param from from if a relative path is used
#'
#' @export

attachStep <- function(ident,parent,  from=pwd()) {
  stepEntity <- loadResource(ident,from)
  stepParentEntity <- loadResource(parent,from)
  tree <- stepEntity$parentId

  result <- authenticatedREST("/resources/{treeId}/steps/{stepId}/attachStepToParent",
                                            urlParams = list(stepId=stepEntity$resourceId,treeId=tree),
                                            queryParams = list(parentStepId=stepParentEntity$resourceId),
                                            restType = "PUT"
  )
  unloadParentStep(ident,from)
  unloadChildSteps(stepParentEntity)
  return(updateResource(ident,from))
}
