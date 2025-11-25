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

  result <- tryCatch({
    authenticatedREST('resources/{stepId}/parentStep',
                      urlParams = list(stepId=step$resourceId),
                      restType = "GET")
  }, error = function(e) {
    log_warn("Failed to load parent step for ", step$entityId, " (", step$name, "): ", e$message)
    return(NULL)
  })

  if (is.null(result)) {
    return(NULL)
  }

  # Check if result is a valid HTTP response
  if (!inherits(result, "response")) {
    log_warn("Invalid response when loading parent step for ", step$entityId, " (", step$name, ")")
    return(NULL)
  }

  # Check HTTP status - parent might be deleted
  if (httr::status_code(result) >= 400) {
    log_warn("Parent step not found (possibly deleted) for ", step$entityId, " (", step$name, "): HTTP ", httr::status_code(result))
    return(NULL)
  }

  parent <- tryCatch({
    httr::content(result)
  }, error = function(e) {
    log_warn("Failed to parse parent step response for ", step$entityId, " (", step$name, "): ", e$message)
    return(NULL)
  })

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
