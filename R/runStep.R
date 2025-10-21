#' Run a Step Resource
#'
#' Initiates the execution of a step on the server. This function triggers asynchronous
#' step execution, meaning the step's code and processes begin running on the improve
#' platform without blocking your R session. Control returns immediately, allowing you
#' to continue working or chain additional commands while the step runs in the background.
#'
#' @param ident A step identifier. Can be the step's path, resource (version) id,
#'   full entity (version) id, or short entity (version) id.
#'
#' @return The step identifier, returned invisibly.
#'
#' @details
#' The function performs a quick validation to ensure the repository is in an editable
#' state, confirms the step exists, and then sends a command to the server to begin
#' execution. The actual step execution happens asynchronously on the server, managed
#' by the improve platform's execution infrastructure.
#'
#' This function is commonly used in workflow orchestration scenarios where multiple
#' steps need to be executed sequentially or in parallel. For sequential workflows,
#' combine with \code{\link{finishRunResource}} to wait for completion before proceeding.
#'
#' @seealso \code{\link{finishRunResource}} to wait for step completion
#'
#' @examples
#' \dontrun{
#' # Execute a single step
#' runStepResource(ident = "/improve-tutorial/Modeling/Step 1")
#' }
#'
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
