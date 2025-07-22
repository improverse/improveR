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

  internalLinks <- dplyr::select(workflow$internalLinks,entityId,fileHash,revisionId,path,targetStep,sourceStep)

  # Extract steps and their templates
  stepTemplates <- list()
  for (stepName in names(workflow$steps)) {
    stepEnv <- workflow$steps[[stepName]]
    # Convert each step to a template environment
    stepDf <- stepEnv$stepDf

    workflowLinks <- internalLinks[internalLinks$targetStep==stepName,]
    print("links")
    print(workflowLinks)
    if (nrow(workflowLinks)>0) {
      remoteFiles <- stepDf$remoteFiles[[1]]

      jointRemoteFiles <- dplyr::left_join(remoteFiles,workflowLinks,by=c("ident"="entityId"))

      jointRemoteFiles <-byNotEmptyAsDf(jointRemoteFiles,function(remoteFile) {

        if (!is.na(remoteFile$sourceStep)) {
          sourceStepPath <- improveR::loadResource(workflow$steps[[remoteFile$sourceStep]]$stepDf$sourceEntityId)$path
          inventoryPath <- paste0("./",substr(remoteFile$path,nchar(sourceStepPath)+2,nchar(remoteFile$path)))
          remoteFile$sourceInventoryPath <- inventoryPath
        }
        if (is.na(remoteFile$name)) {
          remoteFile$name<-paste0("./",improveR::loadResource(remoteFile$ident)$name)
        }
        return(remoteFile)
      })
      print("joint")
      print(jointRemoteFiles)
      stepDf$remoteFiles<-list(jointRemoteFiles)


    }
    print("final")
    print(stepDf$remoteFiles)
    stepTemplates[[stepName]] <- createStepTemplateEnv(
      stepDf = stepDf,
      workflow = env
    )
    stepTemplates[[stepName]]$lineage <- rlang::env_clone(stepEnv$lineage)
    rm(list=c("load"),pos=stepTemplates[[stepName]]$lineage)
    stepTemplates[[stepName]]$usage <- rlang::env_clone(stepEnv$usage)
    rm(list=c("load"),pos=stepTemplates[[stepName]]$usage)
    stepTemplates[[stepName]]$parent <- rlang::env_clone(stepEnv$parent)
    rm(list=c("load"),pos=stepTemplates[[stepName]]$parent)
    stepTemplates[[stepName]]$children <- rlang::env_clone(stepEnv$children)
    rm(list=c("load"),pos=stepTemplates[[stepName]]$children)
  }
  env$stepTemplates <- stepTemplates


  #' List all steps in the workflow template as a data frame
  #' @return A data.frame with step metadata
  env$df <- function() {
    stepNames <- data.frame(fullName = ls(env$stepTemplates))
    stepDf <- byNotEmptyAsDf(stepNames, function(stepName) {
      fullName <- stepName$fullName
      df <- env$stepTemplates[[fullName]]$stepDf
      df$fullName <- fullName
      return(df)
    })
  }

  #' Create a re-execution plan for outdated steps
  #' @return A data.frame describing the execution plan
  env$createExecutionPlan <- function() {
    stepsDf <- env$df()

    .workflow_template_private$collectInternalLinks(env)
    iL <- env$internalLinks

    allSteps <- stepsDf
    executionPlan <- byNotEmptyAsDf(allSteps, function(st) {
      internalLinks <- env$internalLinks[env$internalLinks$targetStep == st$fullName, ]
      usingSteps <- env$internalLinks[env$internalLinks$sourceStep == st$fullName, ]$targetStep
      usedSteps <- env$internalLinks[env$internalLinks$targetStep == st$fullName, ]$sourceStep
      st$lineage <- paste(usedSteps, collapse = ",", sep = "/")
      if (st$lineage == "") st$lineage <- NA
      st$usage <- paste(usingSteps, collapse = ",", sep = "/")
      if (st$usage == "") st$usage <- NA
      st$toUpdate<-NA
      return(st)
    })
    executionPlan <- dplyr::select(
      executionPlan,
      description,
      rationale,
      sourceEntityId,
      sourceName,
      fullName,
      toUpdate,
      lineage,
      usage
    )
    executionPlan$inPlace <- FALSE
    return(executionPlan)
  }


  #' Execute a given execution plan
  #' @param executionPlan A data.frame as returned by \code{createReexecutionPlan}
  #' @return Invisibly returns NULL
  env$executePlan <- function(executionPlan) {
    orderedWorkflow <-  .workflow_template_private$executionOrder(env, executionPlan)
    executionList <- c()
    for (i in seq_len(nrow(orderedWorkflow))) {
      nextData <- orderedWorkflow[i, ]
      nextItem <- nextData$fullName

      if ("lineage" %in% names(nextData) && !is.na(nextData$lineage)) {
        dependencies <- unique(strsplit(nextData$lineage, ",")[[1]])
        for (dependency in dependencies) {
          if (dependency %in% executionList) {
            logging::loginfo("waiting to finish")
            improveR::finishRunResource(env$stepTemplates[[dependency]]$stepDf$entityId)
            executionList <- executionList[executionList != dependency]
          }
        }
      }
      if ("toUpdate" %in% names(nextData) && !is.null(nextData$toUpdate[[1]]) && !is.na(nextData$toUpdate[[1]])) {
        updateLinks(nextData$toUpdate[[1]])
      }
      if ("inPlace" %in% names(nextData) && nextData$inPlace) {
        improveR::runStepResource(nextData$sourceEntityId)
      } else {
        stepEnv <- env$stepTemplates[[nextData$fullName]]
        stepEnv$realise()
      }
      executionList <- c(executionList, nextItem)
    }
    if (length(executionList) > 0) {
      for (item in executionList) {
        improveR::finishRunResource(env$steps[[item]]$stepDf$sourceEntityId)
      }
    }
  }


  env$realise <- function() {
    env$executePlan(env$createExecutionPlan())
  }

  return(env)
}


# --- Private/Internal Methods (not exported to user) ---

.workflow_template_private <- new.env(parent = emptyenv())

.workflow_template_private$collectInternalLinks <- function(env) {
  stepsDf <- env$df()
  remoteFiles <- byNotEmptyAsDf(stepsDf, function(stepInstance) {
    rf <- stepInstance$remoteFiles[[1]]
    if (is.null(rf) || nrow(rf)==0) {return(NULL)}
    return(rf)
  })
  if (is.null(remoteFiles) || nrow(remoteFiles)==0) {
    return(NULL)
  }
  if ("sourceStep" %in% names(remoteFiles)) {
    allTargets <- remoteFiles[!is.na(remoteFiles$sourceStep),]
    env$internalLinks <- allTargets
  }

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
    plan$usage <- ""
  }
  if (is.null(startSteps)) {
    startSteps <- plan[is.na(plan$lineage), ]
    plan <- plan[!is.na(plan$lineage), ]
  }
  if (is.null(startSteps) || nrow(startSteps) == 0) {
    logging::logwarn("No step without dependency, no executable order")
    return(NULL)
  }
  for (s in seq_len(nrow(startSteps))) {
    startStep <- startSteps[s, ]
    lineages <- strsplit(startStep$usage, ",", fixed = TRUE)[[1]]
    if (length(lineages) > 0) {
      for (lineage in lineages) {
        lineageHandle <- plan[plan$fullName == lineage, ]
        if (nrow(lineageHandle) == 1 && "lineage" %in% names(lineageHandle)) {
          dependencies <- strsplit(lineageHandle$lineage, ",", fixed = TRUE)[[1]]
          if (all(dependencies %in% startSteps$fullName)) {
            startSteps <- plyr::rbind.fill(startSteps, lineageHandle)
            plan <- plan[plan$fullName != lineage, ]
          }
        }
      }
    }
  }
  if (nrow(plan) == 0 || counter > 500) {
    if (counter > 500) {
      logging::logwarn("could not add all steps to execution order, check for cycles")
    }
    return(startSteps)
  }
  .workflow_template_private$executionOrderInternal(env, plan, startSteps, counter)
}

  # TODO resolv parent kram






