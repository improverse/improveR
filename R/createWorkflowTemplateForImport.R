#' Create a workflow template environment specifically for import operations
#'
#' This specialized version handles workflows during import when entity IDs 
#' and resources don't exist in the new repository yet.
#' 
#' @param workflow The workflow environment to template (during import)
#' @param internalLinksData Pre-loaded internal links data from import
#' @return An environment representing the workflow template for import
#' @export
createWorkflowTemplateForImport <- function(workflow, internalLinksData = NULL) {
  env <- new.env(parent = emptyenv())
  env$this <- env
  env$workflow <- workflow
  
  # Use provided internal links data if available
  internalLinks <- if (!is.null(internalLinksData)) {
    internalLinksData
  } else if (!is.null(workflow$internalLinks)) {
    workflow$internalLinks
  } else {
    data.frame()  # Default to empty data frame
  }
  
  # Store internal links in the environment
  env$internalLinks <- internalLinks
  
  # Extract steps and their templates
  stepTemplates <- list()
  for (stepName in names(workflow$steps)) {
    stepEnv <- workflow$steps[[stepName]]
    stepDf <- stepEnv$stepDf
    
    # During import, we don't modify remoteFiles as resources don't exist yet
    # The relationships are already established by importWorkflow
    
    stepTemplates[[stepName]] <- createStepTemplateEnv(
      stepDf = stepDf,
      workflow = env
    )
    
    # Copy lineage and usage relationships
    stepTemplates[[stepName]]$lineage <- rlang::env_clone(stepEnv$lineage)
    if ("load" %in% ls(stepTemplates[[stepName]]$lineage)) {
      rm(list=c("load"), pos=stepTemplates[[stepName]]$lineage)
    }
    
    stepTemplates[[stepName]]$usage <- rlang::env_clone(stepEnv$usage)
    if ("load" %in% ls(stepTemplates[[stepName]]$usage)) {
      rm(list=c("load"), pos=stepTemplates[[stepName]]$usage)
    }
    
    stepTemplates[[stepName]]$parent <- rlang::env_clone(stepEnv$parent)
    if ("load" %in% ls(stepTemplates[[stepName]]$parent)) {
      rm(list=c("load"), pos=stepTemplates[[stepName]]$parent)
    }
    
    stepTemplates[[stepName]]$children <- rlang::env_clone(stepEnv$children)
    if ("load" %in% ls(stepTemplates[[stepName]]$children)) {
      rm(list=c("load"), pos=stepTemplates[[stepName]]$children)
    }
  }
  env$stepTemplates <- stepTemplates
  
  # Copy all the standard workflow template methods from the main function
  # These can be shared as they don't depend on entity resolution
  
  env$df <- function() {
    stepNames <- data.frame(fullName = ls(env$stepTemplates))
    stepDf <- byNotEmptyAsDf(stepNames, function(stepName) {
      fullName <- stepName$fullName
      df <- env$stepTemplates[[fullName]]$stepDf
      df$fullName <- fullName
      return(df)
    })
  }
  
  env$setWorkflowTreeRootFolder <- function(rootFolder) {
    .workflow_template_private$setValue("treePath", rootFolder)
    .workflow_template_private$setValue("treeIdent", NULL)
  }
  
  env$setWorkflowTreeName <- function(treeName) {
    .workflow_template_private$setValue("treeName", treeName)
  }
  
  # Private environment for internal state
  .workflow_template_private <- new.env(parent = emptyenv())
  
  .workflow_template_private$setValue <- function(key, value) {
    stepNames <- ls(env$stepTemplates)
    for (i in seq_along(stepNames)) {
      stepName <- stepNames[i]
      template <- env$stepTemplates[[stepName]]
      template$stepDf[[key]] <- value
    }
  }
  
  # For import, we use a simplified execution plan that relies on
  # the already-established relationships
  env$createExecutionPlan <- function() {
    plan <- env$df()
    
    # Build lineage strings from the established relationships
    for (i in seq_len(nrow(plan))) {
      stepName <- plan$fullName[i]
      template <- env$stepTemplates[[stepName]]
      
      lineageSteps <- ls(template$lineage)
      if (length(lineageSteps) > 0 && !("load" %in% lineageSteps)) {
        plan$lineage[i] <- paste(lineageSteps, collapse = ",")
      } else {
        plan$lineage[i] <- NA
      }
      
      usageSteps <- ls(template$usage)
      if (length(usageSteps) > 0 && !("load" %in% usageSteps)) {
        plan$usage[i] <- paste(usageSteps, collapse = ",")
      } else {
        plan$usage[i] <- NA
      }
    }
    
    # Order by dependencies
    orderedPlan <- .workflow_template_private$executionOrder(env, plan)
    return(orderedPlan)
  }
  
  .workflow_template_private$executionOrder <- function(env, plan) {
    .workflow_template_private$executionOrderInternal(env, plan)
  }
  
  .workflow_template_private$executionOrderInternal <- function(env, plan, startSteps = NULL, counter = 0) {
    counter <- counter + 1
    if (!"lineage" %in% names(plan)) {
      plan$lineage <- NA
    }
    if (!"usage" %in% names(plan)) {
      plan$usage <- NA
    }
    if (is.null(startSteps)) {
      startSteps <- plan[is.na(plan$lineage), ]
      plan <- plan[!is.na(plan$lineage), ]
    }
    if (is.null(startSteps) || nrow(startSteps) == 0) {
      logging::logwarn("No step without dependency, no executable order")
      return(NULL)
    }
    
    # Process each step in startSteps to see if we can add any of its dependents
    for (s in seq_len(nrow(startSteps))) {
      startStep <- startSteps[s, ]
      if (!is.na(startStep$usage)) {
        # Get the steps that use this step's output
        usageSteps <- strsplit(startStep$usage, ",", fixed = TRUE)[[1]]
        if (length(usageSteps) > 0) {
          for (usageStep in usageSteps) {
            # Check if this usage step is still in the plan
            candidateStep <- plan[plan$fullName == usageStep, ]
            if (nrow(candidateStep) == 1 && "lineage" %in% names(candidateStep)) {
              # Check if all dependencies of this candidate are already in startSteps
              dependencies <- if (!is.na(candidateStep$lineage)) {
                strsplit(candidateStep$lineage, ",", fixed = TRUE)[[1]]
              } else {
                character(0)
              }
              if (all(dependencies %in% startSteps$fullName)) {
                # All dependencies satisfied, add to startSteps
                startSteps <- plyr::rbind.fill(startSteps, candidateStep)
                plan <- plan[plan$fullName != usageStep, ]
              }
            }
          }
        }
      }
    }
    
    # Check termination conditions
    if (nrow(plan) == 0 || counter > 500) {
      if (counter > 500) {
        logging::logwarn("Could not add all steps to execution order, check for cycles")
      }
      return(startSteps)
    }
    
    # Recursive call with updated startSteps
    return(.workflow_template_private$executionOrderInternal(env, plan, startSteps, counter))
  }
  
  env$executePlan <- function(orderedWorkflow) {
    # Import doesn't execute, it just creates the steps
    # The actual execution is handled by importWorkflow
    return(TRUE)
  }
  
  env$realise <- function() {
    # For import, realise is handled differently by importWorkflow
    return(TRUE)
  }
  
  return(env)
}