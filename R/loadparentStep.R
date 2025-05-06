createParentStepCacheList <- function(name) {
  myList <- list(
    childStepCache="childStep"
  )
  itemNames <- names(myList)
  itemNames <- paste0(name,itemNames)
  names(myList)<-itemNames
  return(myList)
}


parentStepCacheList <- createParentStepCacheList("parentStep")


#' loads the parent step of one step, not applicable to multiple steps
#'
#' @param ident resourceID, entityId or path to the step.
#' @param from path working directory, default is the calling step
#'
#' @return parent step as DF
#' @references ics1209
#'
#' @export
loadParentStep <- function(ident, from=pwd()) {
  resource <- loadResource(ident,from)
  if (!is.null(resource)) {
    res <- getFromCache(resource$resourceId,actualLoadParentStep,parentStepCacheList)
    res$childStep<-NULL
    return(res)
  }
  return(NULL)
}

#' unloadParentStep
#' @param ident resourceID, entityId or path to the step.
#' @param from path working directory, default is the calling step
#' @references ics1209
#' @export
unloadParentStep <- function(ident, from=pwd()) {
  resource <- loadResource(ident,from)
  if (!is.null(resource)) {
    removeFromCache(resource$resourceId,"",parentStepCacheList)
  }
}

#' updateParentStep reloads the runservers from the repository
#' @param ident resourceID, entityId or path to the step.
#' @param from path working directory, default is the calling step
#' @references ics1209
#' @export
updateParentStep <- function(ident, from=pwd()) {
  unloadParentStep(ident,from)
  res <- loadParentStep(ident,from)
  return(res)
}

actualLoadParentStep <- function(ident,from=pwd()) {
  step <- loadResource(ident,from)
  if (is.null(step)) {
    return(NULL)
  }
  if (nrow(step)!=1) {
    log_warn("loadParentStep only works for single steps")
  }
  if (step$nodeType!="Step") {
    log_warn("Tried to find parent Step for a non step: ",step$entityId," ",step$name)
    return(NULL)
  }

  result <- authenticatedREST('resources/{stepId}/parentStep',
                              urlParams = list(stepId=step$resourceId
                              ),
                              restType = "GET")
  parent <- httr::content(result)
  if (is.list(parent) && ("resourceId" %in% names(parent))) {
    parent <- loadResource(parent$resourceId)
    parent$childStep <- step$resourceId
    return(parent)
  }
  return(NULL)
}
