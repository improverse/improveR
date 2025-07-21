# Workflow object: encapsulated, only public API exposed

library(magrittr)

# --- Private/Internal Methods (not exported to user) ---

.workflow_private <- new.env(parent = emptyenv())

.workflow_private$collectInternalLinks <- function(env) {
  stepsDf <- env$df()
  remoteFiles <- byNotEmptyAsDf(stepsDf, function(stepInstance) {
    rf <- stepInstance$remoteFiles[[1]]
    if (is.null(rf) || nrow(rf)==0) {return(NULL)}
    rf$targetStep <- stepInstance$fullName
    return(rf)
  })
  if (is.null(remoteFiles) || nrow(remoteFiles)==0) {
    return(NULL)
  }
  remoteFiles <- remoteFiles %>% dplyr::mutate(entityId = ident) %>%
    dplyr::filter(asLink) %>%
    dplyr::select(entityId, targetStep)

  links <- improveR::loadResource(remoteFiles$entityId) %>%
    dplyr::left_join(remoteFiles)
  fullSteps <- improveR::loadResource(stepsDf$sourceEntityId)
  fullSteps <- dplyr::mutate(fullSteps, fullName = stepsDf[stepsDf$sourceEntityId == entityId, ]$fullName)
  stepPaths <- fullSteps$path
  allTargets <- NULL
  if (length(stepPaths) > 0) {
    for (i in seq_along(stepPaths)) {
      stepPath <- stepPaths[i]
      foundTargets <- links[startsWith(links$path, stepPath), ]
      if (nrow(foundTargets) > 0) {
        stepName <- fullSteps[fullSteps$path == stepPath, ]$fullName
        foundTargets$sourceStep <- stepName
        allTargets <- plyr::rbind.fill(allTargets, foundTargets)
      }
    }
    if (!is.null(allTargets) && nrow(allTargets)>0) {
      x<-byNotEmpty(allTargets,function(connect) {
        sourceStep <- connect$sourceStep
        targetStep <- connect$targetStep
        sourceStepEnv <- env$steps[[sourceStep]]
        targetStepEnv <- env$steps[[targetStep]]
        if (!(targetStep %in% ls(sourceStepEnv$usage))) {
          env$steps[[sourceStep]]$usage[[targetStep]]<-targetStepEnv
        }
        if (!(sourceStep %in% ls(targetStepEnv$lineage))) {
          env$steps[[targetStep]]$lineage[[sourceStep]]<-sourceStepEnv
        }
      })
    }
    env$internalLinks <- allTargets
  }


  #resolv parent kram

  byNotEmpty(stepsDf,function(oneStep) {
    stepEnv <- env$steps[[oneStep$fullName]]
    if (length(ls(stepEnv$parent))==1) {
      parentStep <- improveR::loadParentStep(oneStep$sourceEntityId)
      if (!is.null(parentStep) && parentStep$entityId %in% stepsDf$sourceEntityId) {
        stepEnv$parent$load()
      }
    }
  })

}

.workflow_private$executionOrder <- function(env, plan) {
  .workflow_private$executionOrderInternal(env, plan)
}

.workflow_private$executionOrderInternal <- function(env, plan, startSteps = NULL, counter = 0) {
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
  .workflow_private$executionOrderInternal(env, plan, startSteps, counter)
}

.workflow_private$getLinkTarget <- function(env, internalLink) {
  if (nrow(internalLink) == 0) {
    return(NULL)
  } else if (nrow(internalLink) > 1) {
    return(improveR:::byNotEmpty(internalLink, function(x) .workflow_private$getLinkTarget(env, x)))
  }
  stepsDf <- env$df()
  targetStep <- stepsDf[stepsDf$fullName == internalLink$targetStep, ]
  remoteFiles <- targetStep$remoteFiles[[1]]
  linkFile <- remoteFiles[remoteFiles$ident == internalLink$entityId, ]
  linkResource <- improveR::loadResource(linkFile$name, targetStep$sourceEntityId)
  return(linkResource$entityId)
}

.workflow_private$getInternalUsage <- function(env, fullName) {
  if (length(fullName) == 0) {
    return(NULL)
  } else if (length(fullName) > 1) {
    return(lapply(fullName, function(x) .workflow_private$getInternalUsage(env, x)))
  }
  stepsDf <- env$df()
  usingSteps <- env$internalLinks[env$internalLinks$sourceStep == fullName, ]$targetStep
  if (length(usingSteps) > 0) {
    return(c(usingSteps, .workflow_private$getInternalUsage(env, usingSteps)))
  }
  return(NULL)
}

.workflow_private$updateLinks <- function(env, links) {
  if (length(links) == 0) {
    invisible(NULL)
  } else if (length(links) > 1) {
    invisible(lapply(links, function(x) .workflow_private$updateLinks(env, x)))
  }
  linkRes <- improveR::loadResource(links)
  data <- list(nodeType = "Link", name = linkRes$name, comment = "update outdated")
  improveR::authenticatedREST(
    "/resources/{resourceId}",
    queryParams = list(updateLink = "true"),
    urlParams = list(resourceId = linkRes$resourceId),
    restType = "PUT",
    data = data
  )
  invisible(NULL)
}

# --- Public Methods (exposed to user) ---

#' Create a new workflow object
#'
#' This function constructs a new workflow environment for managing and executing steps.
#' The returned object exposes only the public API for workflow operations.
#'
#' @details
#' The workflow object provides methods to:
#' \itemize{
#'   \item List all steps as a data frame (\code{df})
#'   \item Find changed and outdated files (\code{changedAndOutdatedFiles})
#'   \item Create a re-execution plan for outdated steps (\code{createReexecutionPlan})
#'   \item Rerun all changed and outdated steps (\code{rerunChangedAndOutdated})
#'   \item Execute a given execution plan (\code{executePlan})
#' }
#' Internal state and helper functions are encapsulated and not exposed.
#'
#' @return An environment representing the workflow, with public methods as described.
#' @examples
#' wf <- createWorkflow()
#' # Add steps, then:
#' wf$df()
#' wf$changedAndOutdatedFiles()
#' plan <- wf$createReexecutionPlan()
#' wf$executePlan(plan)
#' @export
createWorkflow <- function() {
  env <- new.env(parent = emptyenv())
  env$this <- env
  env$steps <- new.env()
  env$files <- new.env()
  env$internalLinks <- NULL
  env$outputFiles <- new.env()

  #' List all steps in the workflow as a data frame
  #' @return A data.frame with step metadata
  env$df <- function() {
    stepNames <- data.frame(fullName = ls(env$steps))
    stepDf <- byNotEmptyAsDf(stepNames, function(stepName) {
      fullName <- stepName$fullName
      df <- env$steps[[fullName]]$stepDf
      df$fullName <- fullName
      return(df)
    })
  }

  #' Find changed and outdated files in the workflow
  #' @param tree Optional tree identifier to restrict the search
  #' @return A data.frame of changed and outdated files
  env$changedAndOutdatedFiles <- function(tree = NULL) {
    if (is.null(tree)) {
      workflowDf <- env$df()
      trees <- unique(workflowDf$treeIdent)
      result <- lapply(trees, env$changedAndOutdatedFiles)
      result <- mergeDataframeList(result)
      return(result)
    }

    dmgResult <- authenticatedREST(
      "/resources/{resourceId}/dmg",
      list(resourceId = loadResource(tree)$resourceId),
      queryParams = list(depth = 2)
    )
    dmg <- httr::content(dmgResult)

    flattenInventoryEntries <- function(taskInventory) {
      entryList <- lapply(taskInventory, function(inventoryEntry) {
        if (inventoryEntry$nodeType == "FOV") {
          return(flattenInventoryEntries(inventoryEntry$children))
        }
        if (is.null(inventoryEntry$targetId)) inventoryEntry$targetId <- NA
        if (is.null(inventoryEntry$outdatedLink)) inventoryEntry$outdatedLink <- NA
        entryDf <- data.frame(
          nodeType = inventoryEntry$nodeType,
          entityId = inventoryEntry$entityId,
          entityVersionId = inventoryEntry$entityVersionId,
          fileName = inventoryEntry$fileName,
          lastModified = as.numeric(strptime(inventoryEntry$lastModified, format = "%Y-%m-%dT%H:%M:%S%z")),
          targetId = inventoryEntry$targetId,
          outdatedLink = inventoryEntry$outdatedLink,
          stringsAsFactors = FALSE
        )
        return(entryDf)
      })
      return(entryList)
    }

    completeInventory <- lapply(dmg$tasks, function(task) {
      if (task$runStatus == "FINISHED")
        taskInventory <- task$inventory
      entryList <- flattenInventoryEntries(taskInventory)
      inventoryDf <- mergeListToDataframe(entryList)
      inventoryDf$stoppedAt <- as.numeric(strptime(task$stoppedAt, format = "%Y-%m-%dT%H:%M:%S%z"))
      inventoryDf$stepEntityId <- improveR::loadResource(task$entityId)$entityId
      inventoryDf$ownedByName <- task$ownedByName
      return(inventoryDf)
    })
    completeInventory <- mergeDataframeList(completeInventory)
    changedAndOutdated <- dplyr::filter(completeInventory, outdatedLink == TRUE | stoppedAt < lastModified)
    return(changedAndOutdated)
  }

  #' Create a re-execution plan for outdated steps
  #' @return A data.frame describing the execution plan
  env$createReexecutionPlan <- function() {
    stepsDf <- env$df()
    caof <- env$changedAndOutdatedFiles()
    .workflow_private$collectInternalLinks(env)
    iL <- env$internalLinks

    allStepsToExecute <- stepsDf[stepsDf$sourceEntityId %in% unique(caof$stepEntityId), ]
    usingSteps <- stepsDf[stepsDf$fullName %in% .workflow_private$getInternalUsage(env, allStepsToExecute$fullName), ]

    allSteps <- rbind(allStepsToExecute, usingSteps) %>%
      dplyr::distinct(fullName, .keep_all = TRUE)

    executionPlan <- byNotEmptyAsDf(allSteps, function(st) {
      outDatedLinksDf <- dplyr::filter(caof, nodeType == "LIV" & stepEntityId == st$sourceEntityId)
      outDatedLinks <- NULL
      if (nrow(outDatedLinksDf) > 0) {
        outDatedLinks <- outDatedLinksDf %>%
          dplyr::pull(entityId) %>%
          improveR::loadResource() %>%
          dplyr::pull(entityId)
      }
      internalLinks <- env$internalLinks[env$internalLinks$targetStep == st$fullName, ]
      linkIds <- .workflow_private$getLinkTarget(env, internalLinks)
      allUpdates <- Filter(function(x) !is.null(x), unique(c(outDatedLinks, linkIds)))
      st$toUpdate <- list(allUpdates)
      usingSteps <- env$internalLinks[env$internalLinks$sourceStep == st$fullName, ]$targetStep
      usedSteps <- env$internalLinks[env$internalLinks$targetStep == st$fullName, ]$sourceStep
      st$lineage <- paste(usedSteps, collapse = ",", sep = "/")
      if (st$lineage == "") st$lineage <- NA
      st$usage <- paste(usingSteps, collapse = ",", sep = "/")
      if (st$usage == "") st$usage <- NA
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
    executionPlan$inPlace <- TRUE
    return(executionPlan)
  }

  #' Rerun all changed and outdated steps in the workflow
  #' @return Invisibly returns NULL
  env$rerunChangedAndOutdated <- function() {
    plan <- env$createReexecutionPlan()
    env$executePlan(plan)
    invisible(NULL)
  }

  #' Execute a given execution plan
  #' @param executionPlan A data.frame as returned by \code{createReexecutionPlan}
  #' @return Invisibly returns NULL
  env$executePlan <- function(executionPlan) {
    orderedWorkflow <- .workflow_private$executionOrder(env, executionPlan)
    executionList <- c()
    for (i in seq_len(nrow(orderedWorkflow))) {
      nextData <- orderedWorkflow[i, ]
      nextItem <- nextData$fullName

      if ("lineage" %in% names(nextData) && !is.na(nextData$lineage)) {
        dependencies <- unique(strsplit(nextData$lineage, ",")[[1]])
        for (dependency in dependencies) {
          if (dependency %in% executionList) {
            logging::loginfo("waiting to finish")
            improveR::finishRunResource(env$steps[[dependency]]$stepDf$sourceEntityId)
            executionList <- executionList[executionList != dependency]
          }
        }
      }
      if ("toUpdate" %in% names(nextData) && !is.null(nextData$toUpdate[[1]])) {
        updateLinks(nextData$toUpdate[[1]])
      }
      if ("inPlace" %in% names(nextData) && nextData$inPlace) {
        improveR::runStepResource(nextData$sourceEntityId)
      } else {
        stepEnv <- env$steps[[nextData$fullName]]
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


  env$removeStep <- function(step) {
    stepEnv <- NULL
    if (is.environment(step)) {

      step <- step$stepDf$fullName
    } else if (is.data.frame(step)) {
      step <- step$fullName
    }
    stepEnv <- env$steps[[step]]
    rm(list=c(step),pos=env$steps)
    removeBacklinks <- function(toLink,backLink) {
      affected <- ls(stepEnv[[toLink]])
      affected<-affected[affected!="load"]
      x<-lapply(affected,function(blRemoveTarget) {
        targetEnv <- env$steps[[blRemoveTarget]][[backLink]]
        rm(list=c(step),pos=targetEnv)
      })
    }
    suppressWarnings({
      removeBacklinks("children","parent")
      removeBacklinks("parent","children")
      removeBacklinks("lineage","usage")
      removeBacklinks("usage","lineage")
    })

  }

  env$createTemplate <- function() {
    workflowTemplate <- new.env()
    steps <- ls(env$steps)
    x<- lapply(steps,function(st) {
      env$steps[[st]]$createTemplate(workflowTemplate)
    })
    return(workflowTemplate)
  }


  # Only public methods are attached to env
  env
}
