
storeWorkflow <- function(worflowHandle,workflow) {
  assign(worflowHandle,workflow$handle,envir=stepCache)
}


#' retrieves complete workflow description
#'
#' @param workflowHandle handle of the workflow
#'
#' @export
retrieveWorkflow <- function(workflowHandle) {
  handles <- get0(workflowHandle,envir=stepCache)
  stepDatas <- lapply (handles,function(handle) {
    return(retrieveStep(handle))
  })
  return(mergeDataframeList(stepDatas))
}
