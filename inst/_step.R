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
