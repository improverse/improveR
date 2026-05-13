#' Load Resource From Server
#' @description Loads a resource directly from the server by the resourceId, resourceVersion ID, or entity ID.
#' The results are returned as a data frame or a list of data frames.
#' The dates are also converted to POSIX dates via convertImproveTimestampToPosix.
#' If 0 is handed over, a virtual root resource is handed back.
#' resourceId can be a list.
#' @param resourceId the resource id or the entity id of the resource
#' @param invalidatesReproducibility this flag may only be changed by internal functions
#' @seealso [convertImproveTimestampToPosix()]
#' @export
loadResourceFromServer <- function(resourceId,invalidatesReproducibility=T) {
  improveConnected()
  if (cacheEnv$persistentCaching && cacheEnv$reproducible && invalidatesReproducibility) {
    cacheEnv$reproducible<-F
    log_warn("No longer reproducible, as repo was accessed directly without cache, use loadResource")
  }

    if (length(resourceId)>1) {
    resources <- lapply(resourceId,function(resId) {
      loadResourceFromServer(resId,invalidatesReproducibility)
    })
    return(
      mergeDataframeList(
        resources
        )
    )
  }
  log_debug(paste0("Loading Resource for ",resourceId))
  df<-NULL
  if (as.character(resourceId)!="0") {
    result <- authenticatedREST("/resources/{resourceId}",
                                list(resourceId=resourceId)
    )
    if (is.null(result)) {
      log_warn(paste0("Resource with ID: ",resourceId," could not be loaded"))
      return(NULL)
    }
    cont <- httr::content(result)
    cont$comments<-NULL
    cont$entries<-NULL
    cont$requestor<-NULL
    df <- as.data.frame(cont,stringsAsFactors = FALSE)

  } else {
    df<-getRoot()
  }
  df<-convertDates(df)
  df$isVersion<-F

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
  if("targetEntityId" %in% names(df) && !grepl(pattern = ":",x=df$targetEntityId,fixed = T)) {
    df$targetEntityId <- paste0(repoPrefix(),df$targetEntityId)
  }
  if("targetEntityVersionId" %in% names(df) && !grepl(pattern = ":",x=df$targetEntityVersionId,fixed = T)) {
    df$targetEntityVersionId <- paste0(repoPrefix(),df$targetEntityVersionId)
  }
  return(df)
}

convertDates <- function(df) {
  df <- convertDate(df,"lastModifiedOn")
  df <- convertDate(df,"lastModifiedAt")
  df <- convertDate(df,"createdAt")
  df <- convertDate(df,"dateValue")
  return(df)
}

convertDate <- function(df,field) {
  if (field %in% names(df) ) {
      df[[field]] <- as.numeric(as.character(df[[field]]))
      df[[paste0(field,"Date")]] <- convertImproveTimestampToPosix(df[[field]])
  }
  return(df)
}

getRoot <- function() {
  root <- data.frame(resourceId=0)
  root$resourceId <- "0"
  root$resourceVersionId <- "0"
  root$nodeType <- "Folder"
  root$name <- "root"
  root$parentId <- 0
  root$deleted <- FALSE
  root$entityId <- "root:root-root"
  root$entityVersionId <- 0
  root$revisionId <- 0
  root$createdByName <- "root"
  root$createdById <- 0
  root$createdAt <- "0"
  root$lastModifiedOn <- "0"
  root$lastModifiedById <- 0
  root$lastModifiedByName <- "root"
  root$fileSize <- 0
  root$hasChildren <- TRUE
  root$hasChildrenIncludingFiles <- TRUE
  root$path <- "/"
  return(root)
}
