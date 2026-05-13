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

    # Join internal links data to remoteFiles (same join as createWorkflowTemplateEnv).
    # This is necessary because jsonlite may not preserve sourceStep/sourceInventoryPath
    # columns consistently across all steps during the JSON roundtrip.
    if (!is.null(internalLinks) && nrow(internalLinks) > 0) {
      workflowLinks <- internalLinks[internalLinks$targetStep == stepName, ]
      if (nrow(workflowLinks) > 0) {
        remoteFiles <- stepDf$remoteFiles[[1]]
        if (!is.null(remoteFiles) && nrow(remoteFiles) > 0) {
          # Select only the columns we need from workflowLinks to avoid
          # .x/.y suffix conflicts with overlapping column names
          linkCols <- c("name", "sourceStep", "sourceInventoryPath", "targetStep")
          linkCols <- intersect(linkCols, names(workflowLinks))
          if (length(linkCols) > 0) {
            workflowLinksClean <- workflowLinks[, linkCols, drop = FALSE]
            # Remove pre-existing sourceStep/sourceInventoryPath columns from
            # remoteFiles before joining to avoid .x/.y duplicates
            dropCols <- setdiff(linkCols, "name")
            dropCols <- intersect(dropCols, names(remoteFiles))
            if (length(dropCols) > 0) {
              remoteFiles <- remoteFiles[, !names(remoteFiles) %in% dropCols, drop = FALSE]
            }
            jointRemoteFiles <- dplyr::left_join(remoteFiles, workflowLinksClean, by = "name")
            stepDf$remoteFiles <- list(jointRemoteFiles)
          }
        }
      }
    }

    stepTemplates[[stepName]] <- createStepTemplateEnv(
      stepDf = stepDf,
      workflow = env
    )

    # Copy dependencies and usage relationships
    stepTemplates[[stepName]]$dependencies <- rlang::env_clone(
      stepEnv$dependencies
    )
    if ("load" %in% ls(stepTemplates[[stepName]]$dependencies)) {
      rm(list = c("load"), pos = stepTemplates[[stepName]]$dependencies)
    }

    stepTemplates[[stepName]]$usage <- rlang::env_clone(stepEnv$usage)
    if ("load" %in% ls(stepTemplates[[stepName]]$usage)) {
      rm(list = c("load"), pos = stepTemplates[[stepName]]$usage)
    }

    stepTemplates[[stepName]]$parent <- rlang::env_clone(stepEnv$parent)
    if ("load" %in% ls(stepTemplates[[stepName]]$parent)) {
      rm(list = c("load"), pos = stepTemplates[[stepName]]$parent)
    }

    stepTemplates[[stepName]]$children <- rlang::env_clone(stepEnv$children)
    if ("load" %in% ls(stepTemplates[[stepName]]$children)) {
      rm(list = c("load"), pos = stepTemplates[[stepName]]$children)
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
    
    # Build dependencies strings from the established relationships
    for (i in seq_len(nrow(plan))) {
      stepName <- plan$fullName[i]
      template <- env$stepTemplates[[stepName]]

      dependenciesSteps <- ls(template$dependencies)
      if (length(dependenciesSteps) > 0 && !("load" %in% dependenciesSteps)) {
        plan$dependencies[i] <- paste(dependenciesSteps, collapse = ",")
      } else {
        plan$dependencies[i] <- NA
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
    workflowExecutionOrder(plan)
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