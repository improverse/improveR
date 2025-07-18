stepDf <- NULL
this <- NULL

workflow <- new.env()
lineage <- new.env()
usage <- new.env()
parent <- new.env()
children <- new.env()
lineage[["load"]]<- function() {}
usage[["load"]]<- function() {}
parent[["load"]]<- function() {}
children[["load"]]<- function() {}


getStepValue <- function(key) {
  stepList <- this$stepDf
  if (key %in% names(stepList)) {
    return(as.character(stepList[key]))
  }
  return(NULL)
}

#' retrieveMainProcess
#' retrieves the main process data frame of a step by handle
#'
#'
#' @export

retrieveMainProcess <- function() {
  stepData <- this$stepDf
  processes <- stepData$processes[[1]]
  if (nrow(processes)>0 && ("main" %in% processes$processType)) {
    return(processes[processes$processType=="main",])
  }
  return(NULL)
}

#' getStepState
#'

#' @references ics1221
#' @export
getStepState <- function() {
  entityId <- this$getStepValue("sourceEntityId")
  step<-improveR:::internalLoadResourceFromServer(entityId)
  return(step$runStatus)
}

#' getStepResource
#'
#' @references ics1221
#' @export
getStepResource <- function() {
  entityId <- this$getStepValue("sourceEntityId")
  if (!is.null(entityId)) {
    return(improveR::loadResource(entityId))
  }
}


getStepWithoutCache <- function() {
  entityId <- this$getStepValue("sourceEntityId")
  step<-improveR:::internalLoadResourceFromServer(entityId)
  return(step)
}


#' getStepInventory retrieves all files from the inventory of a handle, if a step was created with this handle
#'

#' @param recurse if the complete inventory should be retrieved or only the top level
#' @param update unloads the cached resources, default true
#' @references ics1221
#' @export
getStepInventory <- function(recurse=F,update=T) {
  step <- this$getStepResource()
  return(improveR:::getStepResourceInventory(step,recurse,update))
}

#' Create a new step environment
#'
#' Constructs a new step object with encapsulated state, navigation environments, and public API.
#' @param stepDf Data frame with step metadata
#' @param workflow Workflow environment this step belongs to
#' @return An environment representing the step
#' @export
createStepEnv <- function(stepDf = NULL, workflow = NULL) {
  env <- new.env(parent = emptyenv())
  env$this <- env
  env$stepDf <- stepDf
  env$workflow <- workflow

  # Navigation environments for interactive exploration
  env$lineage <- new.env(parent = emptyenv())
  env$usage <- new.env(parent = emptyenv())
  env$parent <- new.env(parent = emptyenv())
  env$children <- new.env(parent = emptyenv())

  # Private helpers and state
  .step_private <- new.env(parent = emptyenv())

  # Private: get value from stepDf
  .step_private$getStepValue <- function(key) {
    stepList <- env$stepDf
    if (!is.null(stepList) && key %in% names(stepList)) {
      return(as.character(stepList[key]))
    }
    return(NULL)
  }

  #' Retrieves the main process data frame of a step
  #' @return Data frame of the main process, or NULL if not found
  env$retrieveMainProcess <- function() {
    stepData <- env$stepDf
    processes <- stepData$processes[[1]]
    if (nrow(processes) > 0 && ("main" %in% processes$processType)) {
      return(processes[processes$processType == "main", ])
    }
    return(NULL)
  }

  #' Get the run status of the step
  #' @return Status string
  env$getStepState <- function() {
    entityId <- .step_private$getStepValue("sourceEntityId")
    step <- improveR:::internalLoadResourceFromServer(entityId)
    return(step$runStatus)
  }

  #' Get the resource object for the step
  #' @return Resource object
  env$getStepResource <- function() {
    entityId <- .step_private$getStepValue("sourceEntityId")
    if (!is.null(entityId)) {
      return(improveR::loadResource(entityId))
    }
    return(NULL)
  }

  #' Get the resource object for the step, bypassing cache
  #' @return Resource object
  env$getStepWithoutCache <- function() {
    entityId <- .step_private$getStepValue("sourceEntityId")
    step <- improveR:::internalLoadResourceFromServer(entityId)
    return(step)
  }

  #' Retrieves all files from the inventory of a handle, if a step was created with this handle
  #' @param recurse If the complete inventory should be retrieved or only the top level
  #' @param update Unloads the cached resources, default TRUE
  #' @return Inventory data
  env$getStepInventory <- function(recurse = FALSE, update = TRUE) {
    step <- env$getStepResource()
    return(improveR:::getStepResourceInventory(step, recurse, update))
  }

  # Navigation envs: add placeholder load functions (to be implemented)
  env$lineage$load <- function() {}
  env$usage$load <- function() {}
  env$parent$load <- function() {}
  env$children$load <- function() {}

  env
}
