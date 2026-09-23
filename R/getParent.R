#' Returns the Parent of Anything Resolved by GetCorrectId
#' @param identifier ID or resource
#' @references ics1088
#' @returns The `parentId` of the resource, and `0` when the resource has no parent. Note
#'   that `0` is also what a resource that could not be loaded yields, so the value does
#'   not distinguish the root from a failed read.
#' @export
getParent <- function(identifier) {
  identifier <- getCorrectId(identifier)
  parent <- loadResourceFromServer(identifier)
  if (!("parentId" %in% names(parent))) {
    return(0)
  }
  return(parent$parentId)
}
