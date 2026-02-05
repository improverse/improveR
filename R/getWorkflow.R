


#' Load Workflow Environment
#'
#' Loads a complete workflow environment by reading a tree structure (e.g., an Analysis Tree)
#' and initializing step environments for all contained steps.
#'
#' @param ident Identifier of the tree or container resource.
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#' @param includeSelf Logical. If \code{TRUE}, includes the step identified by \code{pwd()}
#'   if it falls within the loaded tree. Defaults to \code{FALSE} to prevent
#'   self-referential issues during execution.
#'
#' @returns A workflow environment containing all loaded steps. See \code{\link{createWorkflow}}
#'   for details on the workflow environment structure and available methods.
#'
#' @examples
#' \dontrun{
#' # Load workflow from an analysis tree
#' wf <- getWorkflow("/Projects/Analysis/Tree1")
#'
#' # List all steps in the loaded workflow
#' wf$df()
#' }
#'
#' @seealso \code{\link{createWorkflow}}, \code{\link{getStep}}
#' @export
getWorkflow <- function(ident,from=pwd(),includeSelf=F) {

  steps <- loadChildResources(ident,from)$data[[1]]
  if (!includeSelf) {
    steps <- steps[steps$resourceId!=pwd()$resourceId,]
  }
  steps <- steps[steps$nodeType=="Step",]
  tmpEnv <- new.env()
  tmpEnv$workflow <- NULL
  stepEnvList <- byNotEmpty(steps,function(step) {
    stepEnv <- getStep(step,workflow = tmpEnv$workflow)
    tmpEnv$workflow <-stepEnv$workflow
    return(stepEnv)
  })
  
  # If no steps exist, create an empty workflow
  if (is.null(tmpEnv$workflow)) {
    tmpEnv$workflow <- createWorkflow()
  }
  
  return(tmpEnv$workflow)
}


