#' Retrieve Recursive Audit Trail For A Specific Folder
#'
#' @param ident the folder the audit trail is retrieved for
#' @param from startpoint for relative paths, per default: pwd is used
#' @param includeReadAccess if TRUE also read access entries of the audit trail are shown
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
getFullFolderAuditTrail <- function(ident,from=pwd(),includeReadAccess=FALSE) {

  #resource <- loadResource(ident)
  auditTrails <- loadAuditTrail(ident)$data[[1]]
  auditTrails<-collectFolderAuditTrails(ident,auditTrails)
  if (!includeReadAccess) {
    auditTrails<-auditTrails[auditTrails$operation!="read",]
  }

  if ("attributeName" %in% names(auditTrails)) {
    auditTrails<- auditTrails %>% dplyr::distinct(
      .data$revisionId,
      .data$entityId,
      .data$description,
      .data$createdAt,
      .data$attributeName,
      .data$attributeValueTxt,.keep_all=TRUE)
  } else {
    auditTrails<- auditTrails %>% dplyr::distinct(
      .data$revisionId,
      .data$entityId,
      .data$description,
      .data$createdAt,
      .keep_all=TRUE)
  }
  #convertDates(auditTrails)
  auditTrails<-auditTrails[order(auditTrails$createdAt),]
  return(auditTrails)
}

collectFolderAuditTrails <- function(ident, auditTrails) {

  children <- loadChildResources(ident)$data[[1]]
  if (nrow(children)>0) {
    childFiles <- children[children$nodeType=="Folder" | children$nodeType=="Step"| children$nodeType=="Analysis Tree"| children$nodeType=="Review",]
    if (nrow(childFiles)>0) {
      for(i in 1:nrow(childFiles)) {
        f <- childFiles[i,]
        newTrail <- collectFolderAuditTrails(f$resourceId,auditTrails)
        auditTrails<-mergeDataframeList(list(newTrail,auditTrails))
      }
    }
    newTrail <- loadAuditTrail(ident)$data[[1]]
    auditTrails<- mergeDataframeList(list(auditTrails,newTrail))
  }
  return(auditTrails)
}
