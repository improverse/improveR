
storeWorkflow <- function(worflowHandle,workflow) {
  assign(worflowHandle,workflow$handle,envir=stepCache)
}


#' retrieves complete workflow description, can be used to refresh a workflow
#'
#' @param workflowHandle handle of the workflow
#'
#' @export
retrieveWorkflow <- function(workflowHandle) {
  if (is.data.frame(workflowHandle)) {
    workflowHandle <- unique(workflowHandle$workflowHandle)
  }
  handles <- get0(workflowHandle,envir=stepCache)
  stepDatas <- lapply (handles,function(handle) {
    return(retrieveStep(handle))
  })
  return(mergeDataframeList(stepDatas))
}
