#' lockResource
#'
#' @param ident id of resource to be locked
#' @param from pwd for relative pathes
#' @references ics1139
#'
#' @export
lockResource <- function(ident,from=pwd()) {
  improveEditable()
  res <- updateResource(ident,from)
  if ("lockedByName" %in% names(res) && !is.na(res$lockedByName)) {
    log_warn(res,"is currently locked by user",res$lockedByName,", you have to unlock before locking")
    return(F)
  }
  result <- authenticatedREST("/resources/{resourceId}/lock",
                                            urlParams = list(resourceId=res$resourceId),
                                            restType = "PUT")
  if (result$status_code==200) {
    return(T)
  }
  return(F)
}

#' unlockResource
#'
#' @param ident id of resource to be locked
#' @param from pwd for relative pathes
#' @references ics1139
#'
#' @export
unlockResource <- function(ident,from=pwd()) {
  improveEditable()
  res <- updateResource(ident,from)
  if (!("lockedByName" %in% names(res)) || is.na(res$lockedByName)) {
    log_warn(res,"is currently not locked")
    return(F)
  }
  result <- authenticatedREST("/resources/{resourceId}/unlock",
                                            urlParams = list(resourceId=res$resourceId),
                                            restType = "PUT")
  if (result$status_code==200) {
    return(T)
  }
  return(F)
}
