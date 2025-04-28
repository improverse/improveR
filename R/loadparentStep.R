parentStepCacheList <- createCacheList("parentStep")


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
    res <- getFromCache(resource$entityId,actualLoadParentStep,parentStepCacheList)
    if (!is.null(res)) {
      return(res$data[[1]])
    }
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
    removeFromCache(resource$entityId,"",parentStepCacheList)
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
  if (!is.null(res)) {
    return(res$data[[1]])
  }
  return(NULL)
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
  steps <- loadChildResources(step$parentId)$data[[1]]
  steps <- steps[steps$resourceId!=step$resourceId,]
  steps <- steps[steps$nodeType=="Step",]
  parent <- byNotEmptyAsDf(steps,function(s) {
    childSteps <- loadChildSteps(s$resourceId)$data[[1]]
    childSteps <- childSteps[childSteps$resourceId==step$resourceId,]
    if (nrow(childSteps)==1 && ("resourceId" %in% colnames(childSteps))) {return(s)}
    return(NULL)
  })
  if (nrow(parent)==0) {
    log_info("Step ",step$path," has no parent step")
    return(NULL)
  }

  resultFrame <- data.frame(type="parentStep",stringsAsFactors = F)
  resultFrame$resourceId <- step$resourceId
  resultFrame$entityId <- step$entityId
  resultFrame$entityVersionId <- step$entityVersionId
  resultFrame$path <- step$path
  resultFrame$name <- step$name
  resultFrame$data <- list(parent)

  return(resultFrame)
}
