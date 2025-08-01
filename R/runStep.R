


#' runStepResource
#'
#' @param ident check if a step with the same configuration already exists in the tree
#' @references ics1140
#' @export
runStepResource <- function(ident) {
  improveEditable()
  newStep <- loadResource(ident)
  result <- authenticatedREST("resources/{stepId}/run",
                              urlParams = list(stepId=newStep$resourceId),
                              restType = "POST")
  invisible(ident)
}





#' finishRunResource
#'
#' @param ident the ident to the step
#' @param from root for relative pahtes
#' @param runserverName if this is set, the step only counts as finished if it was finished with this runserver (needs to be combined with tool), overridden by settings in stephandle
#' @param runserverToolName if this is set, the step only counts as finished if it was finished with this tool (needs to be combined with runserver), overridden by settings in stephandle
#' @references ics1140
#' @export
finishRunResource <- function(ident,from=pwd(),runserverName=NULL, runserverToolName=NULL) {

  step <- updateResource(ident,from)

  if (is.null(step))
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
    step <- updateResource(step)
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
    step <- updateResource(step)
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
  invisible(ident)
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
