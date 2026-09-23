#' Load Resource Version From Server
#' @description Loads a resource version directly from the server by the  entity version ID.
#' The results are returned as a data frame or a list of data frames.
#' The dates are also converted to POSIX dates via convertImproveTimestampToPosix.
#' entityVersionId can be a list.
#' @param entityVersionId the entity version id of the resource
#' @param invalidatesReproducibility this flag may only be changed by internal functions.
#' @seealso [convertImproveTimestampToPosix()],[loadResource()], `loadResourceByPathGeneric()`
#' @returns A one-row data frame per version with the resource fields as of that
#'   revision, dates converted to POSIX and `isVersion` set to `TRUE`. `NULL` when any step
#'   fails - the entity does not resolve, it has no history, the history is empty, or the
#'   revision could not be read; each case is logged. For a list of ids the rows of all of
#'   them, merged.
#' @export
loadResourceVersionFromServer <- function(entityVersionId,invalidatesReproducibility=T) {
  if (cacheEnv$persistentCaching & cacheEnv$reproducible & invalidatesReproducibility) {
    cacheEnv$reproducible<-F
    log_warn("No longer reproducible, as repo was accessed directly without cache, use loadResource")
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
  log_debug(paste0("Loading Resource for version",entityVersionId))


  entityId <- extractEntityId(entityVersionId)
  res<-loadResource(entityId)
  if (is.null(res)) {
    log_warn(paste0("Resource with entityId: ",entityId," could not be loaded"))
    return(NULL)
  }
  his<-loadHistory(entityId)
  if (is.null(his) || is.null(his$data) || length(his$data) == 0) {
    log_warn(paste0("History for entityId: ",entityId," could not be loaded"))
    return(NULL)
  }
  historyData <- his$data[[1]]
  if (is.null(historyData) || nrow(historyData) == 0) {
    log_warn(paste0("No history data for entityId: ",entityId))
    return(NULL)
  }

  historyData$entityVersionId<-cutPrefix(historyData$entityVersionId)
  entityVersionWithoutPrefix <-strsplit(entityVersionId,":")[[1]][2]
  revisionId <- historyData[historyData$entityVersionId==entityVersionWithoutPrefix,]$revisionId


  df<-NULL

  result <- authenticatedREST("/revisions/{revisionId}/resources/{resourceId}",
                              list(resourceId=res$resourceId,
                                   revisionId=revisionId)
  )
  if (is.null(result)) {
    log_warn(paste0("Resource Version with ID: ",entityVersionId," could not be loaded"))
    return(NULL)
  }
  cont <- httr::content(result)
  df <- as.data.frame(cont,stringsAsFactors = FALSE)

  df<-convertDates(df)
  df$isVersion<-T

  if (is.null(df$targetEntityId) && !is.null(df$targetId)) {
    target <- loadResource(df$targetId)
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
