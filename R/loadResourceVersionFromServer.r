#' loads a resource version directly from the server by the  entity version ID
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix

#' entityVersionId can be a list
#' @param entityVersionId the entity version id of the resource
#' @param invalidatesReproducibility this flag may only be changed by internal functions
#' @export

loadResourceVersionFromServer <- function(entityVersionId,invalidatesReproducibility=T) {
  if (cacheEnv$persistentCaching & cacheEnv$reproducible & invalidatesReproducibility) {
    cacheEnv$reproducible<-F
    logging::logwarn("No longer reproducible, as repo was accessed directly without cache, use loadResource")
  }

  if (length(entityVersionId)>1) {
    resources <- lapply(entityVersionId,function(resId) {
      loadResourceVersionFromServer(resId,invalidatesReproducibility)
    })
    return(
      mergeDataframeList(
        resources
        )
    )
  }
  logging::logdebug(paste0("Loading Resource for version",entityVersionId))


  entityId <- extractEntityId(entityVersionId)
  res<-loadResource(entityId)
  his<-loadHistory(entityId)
  historyData <- his$data[[1]]

  historyData$entityVersionId<-cutPrefix(historyData$entityVersionId)
  entityVersionWithoutPrefix <-strsplit(entityVersionId,":")[[1]][2]
  revisionId <- historyData[historyData$entityVersionId==entityVersionWithoutPrefix,]$revisionId


  df<-NULL

  result <- authenticatetREST("/revisions/{revisionId}/resources/{resourceId}",
                              list(resourceId=res$resourceId,
                                   revisionId=revisionId)
  )
  if (is.null(result)) {
    logging::logwarn(paste0("Resource Version with ID: ",entityVersionId," could not be loaded"))
    return(NULL)
  }
  cont <- httr::content(result)
  df <- as.data.frame(cont,stringsAsFactors = FALSE)

  df<-convertDates(df)
  df$isVersion<-T

  if (is.null(df$targetEntityId) && !is.null(df$targetId)) {
    target <- improveRcore::loadResource(df$targetId)
    df$targetEntityId<-target$entityId
  }

  if(!grepl(pattern = ":",x=df$entityId,fixed = T)) {
    df$entityId <- paste0(repoPrefix(),df$entityId)
  }
  if(!grepl(pattern = ":",x=df$entityVersionId,fixed = T)) {
    df$entityVersionId <- paste0(repoPrefix(),df$entityVersionId)
  }
  return(df)
}

extractEntityId <- function(entityVersionId) {
  prefixAndVersion <- strsplit(entityVersionId,":",fixed=T)[[1]]
  entityParts <- strsplit(prefixAndVersion[2],"-",fixed=T)[[1]]
  entityParts <- entityParts[1:2]
  entityId <- paste(
    prefixAndVersion[1],
    paste(entityParts,collapse = "-")
    ,sep=":"
  )
  return(entityId)
}

cutPrefix <- function(entityVersionId) {
  lapply(entityVersionId,function(v) {
    return(strsplit(v,":")[[1]][2])
  })

}
