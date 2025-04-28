#' retrieves the name of the user that started the step or was used to authenticate
#' @references ics1251
#' @export

whoami <- function() {
  improveConnected()
  user <- conf()$user
  if (user!="") {
    return(user)
  }
  audit <- loadAuditTrail(pwd()$entityId)$data[[1]]
  audit<-audit[audit$entityReferenceType=="Run" & audit$operation=="create" & audit$createdAt==max(audit$createdAt),]
  return(audit$username)
}
