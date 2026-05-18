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


#' Loads the Parent Step of One Step, Not Applicable to Multiple Steps
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

#' Unload Parent Step
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

#' Refresh Parent Step from Server
#' @param ident resourceID, entityId or path to the step.
#' @param from path working directory, default is the calling step
#' @param ... For backwards compatibility with the deprecated `update*` alias; not used by `refresh*` itself.
#' @references ics1209
#' @export
refreshParentStep <- function(ident, from=pwd()) {
  unloadParentStep(ident,from)
  res <- loadParentStep(ident,from)
  return(res)
}

#' @rdname refreshParentStep
#' @export
updateParentStep <- function(...) {
  .Deprecated("refreshParentStep")
  refreshParentStep(...)
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
                              urlParams = list(stepId=step$resourceId),
                              restType = "GET")
  if (is.null(result)) {
    log_warn("Failed to load parent step for", step$entityId, "(", step$name, ")")
    return(NULL)
  }

  parent <- httr::content(result)
  if (is.null(parent)) {
    return(NULL)
  }

  if (is.list(parent) && ("resourceId" %in% names(parent))) {
    parent <- loadResource(parent$resourceId)
    parent$childStep <- step$resourceId
    return(parent)
  }
  return(NULL)
}
