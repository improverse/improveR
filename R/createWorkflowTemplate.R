#' Create a workflow template environment from an existing workflow
#'
#' Extracts all steps, relationships, parameters, and files from the workflow
#' and builds a reusable workflow template environment.
#' @param workflow The workflow environment to template
#' @return An environment representing the workflow template
#' @export
createWorkflowTemplateEnv <- function(workflow) {
  env <- new.env(parent = emptyenv())
  env$this <- env
  env$workflow <- workflow

  # Extract steps and their templates
  stepTemplates <- list()
  for (stepName in names(workflow$steps)) {
    stepEnv <- workflow$steps[[stepName]]
    # Convert each step to a template environment
    stepTemplates[[stepName]] <- createStepTemplateEnv(
      stepDf = stepEnv$stepDf,
      workflow = env
    )
  }
  env$stepTemplates <- stepTemplates

  # run a collect
  # build the internalLinks
  # build the plan
  #execute the plan


  env
}
