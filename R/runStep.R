#' realiseStep
#'
#' @param handle check if a step with the same configuration already exists in the tree
#' @param force, force creates a new step even if an equivalent step already exists
#' @param run, automatically run the step after creation (T is overridden by the setStepBreakpoint)
#' @references ics1140
#' @export
realiseStep <-function(handle,force=T,run=T) {
  improveEditable()
  breakPoint <- getStepValue(handle,"breakpoint")
  reuse <- getStepValue(handle,"reuse")
  if (!is.null(breakPoint) && breakPoint==T) {
    run <-F
  }
  if (!is.null(reuse) && reuse==T) {
    force <-F
  }
  newStep <- NULL
  if (!force) {
    logging::logdebug("check step equality")
    newStep <- existsInTargetTree(handle)
    if (!is.null(newStep)) {
      setStepValue(handle,"entityId",as.character(newStep$entityId))
      return(handle)
    }
  }
  newStep <- createPreparedStep(handle)
  setStepValue(handle,"entityId",as.character(newStep$entityId))
  tree <- loadResource(newStep$parentId)
  setStepValue(handle,"treeIdent",tree$resourceId)
  setStepValue(handle,"treeName",tree$name)
  setStepValue(handle,"treePath",dirname(tree$path))
  if (getStepState(handle)=="INITIAL" && run) {
    runStep(handle)
  }
  return(handle)
}

#' runStep
#'
#' @param handle check if a step with the same configuration already exists in the tree
#' @references ics1140
#' @export
runStep <- function(handle) {
  improveEditable()
  newStep <- getStepResource(handle)
  result <- authenticatedREST("resources/{stepId}/run",
                                            urlParams = list(stepId=newStep$resourceId),
                                            restType = "POST")
  return(handle)
}

#' getStepResource
#'
#' @param handle load the step resource if the handle has already been executed
#' @references ics1221
#' @export
getStepResource <- function(handle) {
  entityId <- getStepValue(handle,"entityId")
  if (!is.null(entityId)) {
    return(loadResource(entityId))
  }
}

#' getStepState
#'
#' @param handle get current state of step without caching
#' @references ics1221
#' @export
getStepState <- function(handle) {
  entityId <- getStepValue(handle,"entityId")
  step<-internalLoadResourceFromServer(entityId)
  return(step$runStatus)
}

#' getStepTool
#'
#' @param handle get current tool of step without caching
#' @references ics1221
#' @export
getStepTool <- function(handle) {
  entityId <- getStepValue(handle,"entityId")
  step<-internalLoadResourceFromServer(entityId)
  return(step$toolId)
}


getStepWithoutCache <- function(handle) {
  entityId <- getStepValue(handle,"entityId")
  step<-internalLoadResourceFromServer(entityId)
  return(step)
}

#' finishRun
#'
#' @param handle check if a step with the same configuration already exists in the tree
#' @param runserverName if this is set, the step only counts as finished if it was finished with this runserver (needs to be combined with tool), overridden by settings in stephandle
#' @param runserverToolName if this is set, the step only counts as finished if it was finished with this tool (needs to be combined with runserver), overridden by settings in stephandle
#' @references ics1140
#' @export
finishRun <- function(handle,runserverName=NULL, runserverToolName=NULL) {

  frn <- getStepValue(handle,"finishRunserverName")
  frt <- getStepValue(handle,"finishRunserverTool")

  if (!is.null(frn) && !is.null(frn)) {
    runserverName<-frn
    runserverToolName<-frt
  }

  if (is.null(handle))
    return(NULL)
  toolId <- NULL
  if (!is.null(runserverName)&&!is.null(runserverToolName)) {
    toolId <- getToolId(runserverName, runserverToolName)
    print(toolId)
  } else if (!is.null(runserverName)||!is.null(runserverToolName)) {
    logging::logwarn("runServerName and runServerToolName must be provided in finishRun, or none of them")
  }
  running<-TRUE
  wrongRun <- ""

  if (!is.null(toolId)) {
    step <- getStepWithoutCache(handle)
    state<-step$runStatus
    if (state=="FINISHED") {
      processes <- actualLoadProcessesForStep(step$resourceId)
      process <- dplyr::filter(processes,.data$processType=="main")
      stepTool <- processes$runserverToolId[1]
      if (toolId!=stepTool) {
        tryCatch({
          run <- actualLoadProcessRuns(process$id)
          run <- run[run$startedAt==max(run$startedAt),]
          currentRun <-run$id
          wrongRun <- currentRun
        },
        error=function(cond) {
          Sys.sleep(5)
        }
        )
      }
    }
  }

  while(running) {
    step <- getStepWithoutCache(handle)
    state<-step$runStatus
    if (is.null(toolId)) {
      if (state=="FINISHED") {
        running<-F
      } else {
        Sys.sleep(5)
      }
    } else {
      processes <- actualLoadProcessesForStep(step$resourceId)
      process <- dplyr::filter(processes,.data$processType=="main")
      stepTool <- processes$runserverToolId[1]
      tryCatch({
        run <- actualLoadProcessRuns(process$id)
        run <- run[run$startedAt==max(run$startedAt),]
        currentRun <-run$id
        if (state=="FINISHED" && toolId==stepTool && currentRun!=wrongRun) {
          running<-F
        } else {
          if (toolId!=stepTool && state!="FINISHED") {
            wrongRun <- currentRun
          }


          Sys.sleep(5)
        }
      },
      error=function(cond) {
        Sys.sleep(5)
      }
      )
    }
  }
  return(handle)
}

loadRecurse <- function(ident,update=T) {
  if (update) {
    unloadFullChildResources(ident)
  }
  inventory <- loadFullChildResources(ident)
  inventoryResources <- inventory$data[[1]]
  inventoryFolders <- inventoryResources[inventoryResources$nodeType=="Folder",]
  if (nrow(inventoryFolders)>0) {
    newResources <- byNotEmptyAsDf(inventoryFolders,function(folder) {
      return(loadRecurse(folder))
    })
    inventoryResources<- plyr::rbind.fill(inventoryResources,newResources)
  }
  return(inventoryResources)
}

#' getStepInventory retrieves all files from the inventory of a handle, if a step was created with this handle
#'
#' @param handle check if a step with the same configuration already exists in the tree
#' @param recurse if the complete inventory should be retrieved or only the top level
#' @param update unloads the cached resources, default true
#' @references ics1221
#' @export
getStepInventory <- function(handle,recurse=F,update=T) {
  step <- getStepResource(handle)
  return(getStepResourceInventory(step,recurse,update))
}

getStepResourceInventory <- function(step,recurse=F,update=T) {
  if (update) {
    unloadChildResources(step$entityId)
  }
  inventory <- loadFullChildResources(step$entityId)
  if (recurse) {
    inventoryResources <- loadRecurse(step$entityId,update)
    inventory$data <- list(inventoryResources)
  }
  inventoryResources <- inventory$data[[1]]
  inventoryResources <- byNotEmptyAsDf(inventoryResources,function(inventoryResource) {
    resPath <- getRelativePath(step,inventoryResource)
    resPath <- substr(resPath,3,nchar(resPath))
    inventoryResource$inventoryPath <- resPath
    return(inventoryResource)
  })
  inventory$data <- list(inventoryResources)
  inventory$type <- "inventory"
  return(inventory)
}
