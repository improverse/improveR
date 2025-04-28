childStepCacheList <- createCacheList("childSteps")


#' loads all child steps by the resourceId,  entity ID or entity version ID
#' it uses caching
#' the results are returned as a data frame
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' resourceId can be a list
#'
#' arguments:
#' @param  ident the resource id or the entity id of the resource
#' @param from used if a relative path is used
#' @references ics1205
#' @export
loadChildSteps <- function(ident,from=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadChildStepsFromServer,
                                   cacheList=childStepCacheList,
                                   from=from)
  )
}

#' unloadChildSteps
#' @param ident id
#' @references ics1205
#' @export
unloadChildSteps <- function(ident) {
  res <- loadResource(ident)
  loadChildSteps(res$resourceId)
  removeFromCache(res$resourceId,"",childStepCacheList)
}

#' updateChildSteps
#' @param ident id
#' @references ics1205
#' @export
updateChildSteps <- function(ident) {
  unloadChildSteps(ident)
  res <- loadChildSteps(ident)
  return(res)
}

loadChildStepsFromServer <- function(resource) {
  genericLoadFromServer(resource,name="childSteps",funct=actualLoadChildSteps)
}


#nest!
actualLoadChildSteps <- function(resource) {
  result<-NULL
  if (as.character(resource$resourceId)!="0") {
    if (resource$nodeType!="Step") {
      log_warn("Resource",resource$entityId,"is not a Step, co no child steps possible")
      return(NULL)
    }
    result <- authenticatedREST("/resources/{resourceId}/childSteps",
                                list(resourceId=resource$resourceId)
    )
  } else {
    log_warn("no child steps possible in root")
    return(NULL)
  }
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  df <- mergeNestedListToDataframe(cont)
  df<-convertDates(df)
  return(df)
}
