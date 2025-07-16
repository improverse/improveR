


#' getWorkflow
#' reads a tree and creates stepEnvs for all steps
#' @param ident the ident of the step
#' @param from if relative path, default the starting step
#' @param includeSelf by defaults does not include the step the script is run with.
#' @export
getWorkflow <- function(ident,from=pwd(),includeSelf=F) {

  #TODO here load dmg
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
  return(tmpEnv$workflow)
}


