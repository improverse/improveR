
#TODO improve wording
#' Strip
#' @description strip removes the information data frame around subentities like meta data, history, children, or audittrail.
#' @param data a data frame with a nested data frame in $data
#' @export
strip <- function(data) {
  return(data$data[[1]])
}

templateCacheList <- list(
  resourceIdCache="resourceId",
  resourcePathCache="path",
  resourceEntityIdCache="entityId"
)

createCacheList <- function(name) {
  myList <- templateCacheList
  itemNames <- names(myList)
  itemNames <- paste0(name,itemNames)
  names(myList)<-itemNames
  return(myList)
}

genericLoadResourceSubEntities <- function(ident,func,cacheList,from=pwd(),...) {
  resources <- loadResource(ident,from)
  if (is.null(resources)) {
    return(NULL)
  }
  if (nrow(resources)>1) {
    entityIds <- resources$entityId
    resourceValues <- Map(function(resId) {
      genericLoadResourceSubEntities(resId,func,cacheList,from,...)
    },entityIds)
    return(
      mergeListToDataframe(resourceValues)
    )
  }
  res <- getFromCache(resources$entityId,func,cacheList,...)
  return(res)
}

genericLoadFromServer <- function(resource,name,funct,...) {
  improveConnected()
  resource <- loadResource(resource)
  logging::logdebug(paste0("Loading ",name," Resources for ",resource$entityId))
  df <- funct(resource,...)
  if (is.null(df)) {
    logging::logwarn(paste0(name," resource by ID: ",resource$entityId," could not be loaded"))
    return(NULL)
  }
  resultFrame <- data.frame(type=name,stringsAsFactors = F)
  resultFrame$resourceId <- resource$resourceId
  resultFrame$entityId <- resource$entityId
  resultFrame$entityVersionId <- resource$entityVersionId
  resultFrame$path <- resource$path
  resultFrame$name <- resource$name
  resultFrame$data <- list(df)
  return(resultFrame)
}
