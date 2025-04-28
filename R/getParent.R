#' returns the parent of anything resolved by getCorrectId
#' @param identifier, ID or resource
#' @references ics1088
#' @export
getParent <- function(identifier) {
  identifier <- getCorrectId(identifier)
  parent <- loadResourceFromServer(identifier)
  if (!("parentId" %in% names(parent))) {
    return(0)
  }
  return(parent$parentId)
}
