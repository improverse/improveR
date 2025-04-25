auditTrailResourceCacheList <- createCacheList("auditTrail")


#' loads the auditTrail  by the resourceId,  entity ID or entity version ID
#' it uses caching
#' the results are returned as a data frame or a list of data frames
#' the dates are also converted to posix dates via convertImproveTimestampToPosix
#' resourceId can be a list
#'
#' arguments:
#' @param  ident the resource id or the entity id of the resource
#' @param fromForRelativePathes used if a relative path is used
#' @references ics1097
#' @export
loadAuditTrail <- function(ident,fromForRelativePathes=pwd()) {
  return(
    genericLoadResourceSubEntities(ident,
                                   func=loadAuditTrailFromServer,
                                   cacheList=auditTrailResourceCacheList,
                                   fromForRelativePathes=fromForRelativePathes)
  )
}

#' unloadAuditTrail
#' @param ident id
#' @references ics1097
#' @export
unloadAuditTrail <- function(ident) {
  res <- loadResource(ident)
  removeFromCache(res$resourceId,"",auditTrailResourceCacheList)
}

#' updateAuditTrail
#' @param ident id
#' @references ics1097
#' @export
updateAuditTrail <- function(ident) {
  unloadAuditTrail(ident)
  res <- loadAuditTrail(ident)
  return(res)
}

loadAuditTrailFromServer <- function(resource) {
  genericLoadFromServer(resource,name="auditTrail",funct=actualLoadAuditTrail)
}

#' @importFrom rlang .data
actualLoadAuditTrail <- function(resource) {
  result<-NULL
  if (as.character(resource$resourceId)!="0") {
    result <- authenticatedREST("/resources/{resourceId}/auditTrail",
                                list(resourceId=resource$resourceId)
    )
  } else {
    result <- NULL
  }
  if (is.null(result)) {
    return(NULL)
  }
  #entityIdPrefixString <- repoPrefix()
  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  #df <- dplyr::mutate(df,entityId = ifelse (is.na(.data$entityId),NA,paste0(entityIdPrefixString,.data$entityId)))
  #df <- dplyr::mutate(df,entityVersionId = ifelse (is.na(.data$entityVersionId),NA,paste0(entityIdPrefixString,.data$entityVersionId)))
  df<-convertDates(df)
  return(df)
}

