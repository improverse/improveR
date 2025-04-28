
updateHandles <- function(workflow,oldHandle,newHandle) {
  if (nrow(workflow)>0 && "usage" %in% names(workflow) && "dependencies" %in% names(workflow)) {
    for (i in 1:nrow(workflow)) {
      workflow[i,]$usage <- stringr::str_replace_all(workflow[i,]$usage,oldHandle,newHandle)
      workflow[i,]$dependencies <- stringr::str_replace_all(workflow[i,]$dependencies,oldHandle,newHandle)
      if (length(workflow[i,]$remoteFiles)==1) {
        if (is.data.frame(workflow[i,]$remoteFiles[[1]]) && nrow(workflow[i,]$remoteFiles[[1]])>0 && "sourceHandle" %in% names(workflow[i,]$remoteFiles[[1]])) {
          for (j in 1:nrow(workflow[i,]$remoteFiles[[1]])) {
            workflow[i,]$remoteFiles[[1]][j,]$sourceHandle<-stringr::str_replace_all(workflow[i,]$remoteFiles[[1]][j,]$sourceHandle,oldHandle,newHandle)
          }
        }
      }
    }
  }
  if (nrow(workflow)>0) {
    for (i in 1:nrow(workflow)) {
      if (length(workflow[i,]$localFiles)==1&&  (!is.null(workflow[i,]$localFiles[[1]])) && unique(workflow[i,]$localFiles[[1]]$stepHandle)==oldHandle){
        workflow[i,]$localFiles[[1]]$stepHandle <- newHandle
      }
      if (length(workflow[i,]$remoteFiles)==1 &&  (!is.null(workflow[i,]$remoteFiles[[1]])) && unique(workflow[i,]$remoteFiles[[1]]$stepHandle)==oldHandle){
        workflow[i,]$remoteFiles[[1]]$stepHandle <- newHandle
      }

      if (length(workflow[i,]$extLinks)==1 && (!is.null(workflow[i,]$extLinks[[1]])) && unique(workflow[i,]$extLinks[[1]]$stepHandle)==oldHandle) {
        workflow[i,]$extLinks[[1]]$stepHandle <- newHandle
      }
    }
  }

  return(workflow)
}

#' deepWorkflowCopy
#' @param workflow a workflow handle or a workflow data.frame, changes to steps in workflow data.frame will be persisted by execution. consider creating a deep copy
#' @param targetTree the tree the steps should be executed in, default NULL, if NULL treeIdent of the steps is kept
#' @param createParentalRelation create the step as child step of the step in the source workflow, default FALSE
#' @references ics1211
#' @export
deepWorkflowCopy <- function(workflow,targetTree=NULL,createParentalRelation=F) {

  if (is.character(workflow)) {
    workflow <- retrieveWorkflow(workflow)
  }

  if (is.data.frame(workflow) && nrow(workflow)>0) {
    if (!is.null(targetTree)) {
      targetTreeRes <- loadResource(targetTree)
      if (is.null(targetTreeRes) || nrow(targetTreeRes)>1 || targetTreeRes$nodeType != "Analysis Tree") {
        log_warn(targetTree," is not a single valid Analysis Tree")
        return(NULL)
      }
      workflow$treeIdent <- targetTreeRes$resourceId
    }
    workflowHandle <- paste0(uuid::UUIDgenerate())

    workflow$workflowHandle<-workflowHandle
    #adapt sourceHandles ?

    if (createParentalRelation) {
      workflow$parentIdent <- ""
      workflow$inheritFromParent <- F
    }
    for (i in 1:nrow(workflow)) {
      oldStepHandle <- workflow[i,]$handle
      stepHandle <- paste0(uuid::UUIDgenerate())
      workflow[i,]$handle <- stepHandle
      workflow <- updateHandles(workflow,oldStepHandle,stepHandle)
      if (createParentalRelation) {
        oldStep <- getStepResource(oldStepHandle)
        if (!is.null(oldStep)) {
          workflow[i,]$parentIdent <- oldStep$entityId
        } else {
          log_warn("no step in repository found for step handle")
        }

        #workflow[i,]$inheritFromParent <- F
      }

    }
    if (is.null(workflow$reuse) || !workflow$reuse) {
      workflow$entityId<-NULL
    }

    for (i in 1:nrow(workflow)) {
      storeStep(stepHandle = workflow[i,]$handle,stepList = workflow[i,])
    }
    storeWorkflow(workflowHandle,workflow)

  return(workflowHandle)

  }
  else {
    log_warn("workflow",workflow,"not found")
    return(NULL)
  }

}


#' executeWorkflow
#' @param workflow a workflow handle or a workflow data.frame, changes to steps in workflow data.frame will be persisted by execution. consider creating a deep copy
#' @references ics1212
#' @export
executeWorkflow <- function(workflow) {
  #persist changed steps if data.frame!
  if (is.character(workflow)) {
    workflow <- retrieveWorkflow(workflow)
  }
  orderedWorkflow <- executionOrder(workflow)
  executionList <- c()
  for (i in 1:nrow(orderedWorkflow)) {
    nextData <- orderedWorkflow[i,]
    nextItem <- nextData$handle

    if ("dependencies" %in% names(nextData) && !is.na(nextData$dependencies)) {
      dependencies <- unique(strsplit(nextData$dependencies,",")[[1]])
      for (j in 1:length(dependencies)) {
        dependency <- dependencies[j]
        if (dependency %in% executionList) {
          logging::loginfo("waiting to finish")
          finishRun(dependency)
          executionList <- executionList[executionList!=dependency]
        }
      }
    }
    realiseStep(nextItem)
    executionList <- c(executionList,nextItem)
  }
  if (length(executionList)>0) {
    for (i in 1:length(executionList)) {
      finishRun(executionList[i])
    }
  }
}


