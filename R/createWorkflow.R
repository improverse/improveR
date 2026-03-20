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
    dplyr::filter(.data$asLink) %>%
    dplyr::mutate(targetName=name) %>%
    dplyr::select("entityId", "targetStep","targetName")

  # Check if any links remain after filtering for asLink (step might only have local files or copied files)
  if (is.null(remoteFiles) || nrow(remoteFiles) == 0) {
    return(NULL)
  }

  links <- loadResource(remoteFiles$entityId) %>%
    dplyr::left_join(remoteFiles,by=c("entityId"="entityId"))
  fullSteps <- loadResource(stepsDf$sourceEntityId)
  fullSteps <- dplyr::mutate(fullSteps, fullName = stepsDf[stepsDf$sourceEntityId == entityId, ]$fullName)
  stepPaths <- fullSteps$path
  allTargets <- NULL
  if (length(stepPaths) > 0) {
    for (i in seq_along(stepPaths)) {
      stepPath <- stepPaths[i]
      foundTargets <- links[startsWith(links$path, paste0(stepPath, "/")), ]
      if (nrow(foundTargets) > 0) {
        stepName <- fullSteps[fullSteps$path == stepPath, ]$fullName
        foundTargets$sourceInventoryPath <- substr(foundTargets$path,nchar(stepPath)+2,nchar(foundTargets$path))
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
        if (!(sourceStep %in% ls(targetStepEnv$dependencies))) {
          env$steps[[targetStep]]$dependencies[[sourceStep]] <- sourceStepEnv
        }
      })
    }
    env$internalLinks <- NULL
    if (!is.null(allTargets)) {
      allTargets<-dplyr::distinct(allTargets,targetStep,targetName,.keep_all = T)
      allTargets<-dplyr::mutate(allTargets,name=targetName)
      env$internalLinks <- dplyr::select(allTargets, fileSize, filehash = fileHash, targetStep, name, sourceInventoryPath, sourceStep)
    }

  }



  byNotEmpty(stepsDf,function(oneStep) {
    stepEnv <- env$steps[[oneStep$fullName]]
    if (length(ls(stepEnv$parent))==1) {
      parentStep <- loadParentStep(oneStep$sourceEntityId)
      if (!is.null(parentStep) && parentStep$entityId %in% stepsDf$sourceEntityId) {
        stepEnv$parent$load()
      }
    }
  })

}

.workflow_private$executionOrder <- function(env, plan) {
  workflowExecutionOrder(plan)
}

.workflow_private$getLinkTarget <- function(env, internalLink) {
  if (nrow(internalLink) == 0) {
    return(NULL)
  } else if (nrow(internalLink) > 1) {
    return(byNotEmpty(internalLink, function(x) .workflow_private$getLinkTarget(env, x)))
  }
  stepsDf <- env$df()
  targetStep <- stepsDf[stepsDf$fullName == internalLink$targetStep, ]
  remoteFiles <- targetStep$remoteFiles[[1]]
  linkFile <- remoteFiles[remoteFiles$ident == internalLink$entityId, ]
  linkResource <- loadResource(linkFile$name, targetStep$sourceEntityId)
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
  linkRes <- loadResource(links)
  data <- list(nodeType = "Link", name = linkRes$name, comment = "update outdated")
  authenticatedREST(
    "/resources/{resourceId}",
    queryParams = list(updateLink = "true"),
    urlParams = list(resourceId = linkRes$resourceId),
    restType = "PUT",
    data = data
  )
  invisible(NULL)
}

# --- Public Methods (exposed to user) ---

#' Create New Workflow Environment
#'
#' Constructs a new workflow environment for managing, analyzing, and executing
#' workflow steps. The returned object encapsulates the workflow state and exposes
#' a public API for operations.
#'
#' @details
#' The returned workflow environment contains the following methods:
#' \itemize{
#'   \item \code{df()}: Returns a data frame of all steps in the workflow
#'   \item \code{changedAndOutdatedFiles(tree)}: Finds files that have changed or are outdated
#'   \item \code{createReexecutionPlan()}: Generates a plan to rerun only outdated steps
#'   \item \code{rerunChangedAndOutdated()}: Executes the re-execution plan
#'   \item \code{rerunAll()}: Reruns all steps regardless of status
#'   \item \code{executePlan(plan)}: Executes a specific plan object
#' }
#' Internal state and helper functions are encapsulated and not exposed.
#'
#' @returns An R environment with class "workflow" containing the public methods
#'   listed in Details.
#'
#' @examples
#' \dontrun{
#' wf <- createWorkflow()
#' # Add steps, then:
#' wf$df()
#' wf$changedAndOutdatedFiles()
#' plan <- wf$createReexecutionPlan()
#' wf$executePlan(plan)
#' }
#' @export
createWorkflow <- function() {
  env <- new.env(parent = emptyenv())
  env$this <- env
  env$steps <- new.env()
  env$files <- new.env()
  env$internalLinks <- NULL
  env$outputFiles <- new.env()

  # List all steps in the workflow as a data frame
  # @return A data.frame with step metadata
  env$df <- function() {
    stepNames <- data.frame(fullName = ls(env$steps))
    stepDf <- byNotEmptyAsDf(stepNames, function(stepName) {
      fullName <- stepName$fullName
      df <- env$steps[[fullName]]$stepDf
      df$fullName <- fullName
      return(df)
    })
    return(stepDf)
  }

  # Find changed and outdated files in the workflow
  # @param tree Optional tree identifier to restrict the search
  # @return A data.frame of changed and outdated files
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
      if (task$runStatus == "FINISHED") {
        taskInventory <- task$inventory
        entryList <- flattenInventoryEntries(taskInventory)
        inventoryDf <- mergeListToDataframe(entryList)
        inventoryDf$stoppedAt <- as.numeric(strptime(task$stoppedAt, format = "%Y-%m-%dT%H:%M:%S%z"))
        inventoryDf$stepEntityId <- loadResource(task$entityId)$entityId
        inventoryDf$ownedByName <- task$ownedByName
        return(inventoryDf)
      } else {
        # If task is not finished, return empty dataframe
        return(data.frame())
      }
    })
    completeInventory <- mergeDataframeList(completeInventory)
    changedAndOutdated <- dplyr::filter(completeInventory, .data$outdatedLink == TRUE | .data$stoppedAt < .data$lastModified)
    return(changedAndOutdated)
  }

  # Create a re-execution plan for outdated steps
  # @return A data.frame describing the execution plan
  env$createReexecutionPlan <- function(
    includeDownstream = TRUE,
    includeAllSteps = FALSE
  ) {
    stepsDf <- env$df()
    .workflow_private$collectInternalLinks(env)
    iL <- env$internalLinks

    # Determine which steps to execute
    if (includeAllSteps) {
      # Include all steps in the workflow - no need to check changed/outdated
      allStepsToExecute <- stepsDf
      allSteps <- allStepsToExecute
    } else {
      # Only changed and outdated steps
      caof <- env$changedAndOutdatedFiles()
      allStepsToExecute <- stepsDf[
        stepsDf$sourceEntityId %in% unique(caof$stepEntityId),
      ]

      # Optionally include downstream steps that use the outputs
      if (includeDownstream && nrow(allStepsToExecute) > 0) {
        usingSteps <- stepsDf[
          stepsDf$fullName %in%
            .workflow_private$getInternalUsage(env, allStepsToExecute$fullName),
        ]
        allSteps <- rbind(allStepsToExecute, usingSteps) %>%
          dplyr::distinct(.data$fullName, .keep_all = TRUE)
      } else {
        allSteps <- allStepsToExecute
      }
    }

    if (is.null(allSteps) || nrow(allSteps) == 0) {
      return(data.frame(
        description = character(0), rationale = character(0),
        sourceEntityId = character(0), sourceName = character(0),
        fullName = character(0), toUpdate = list(),
        dependencies = character(0), usage = character(0),
        inPlace = logical(0), stringsAsFactors = FALSE
      ))
    }

    executionPlan <- byNotEmptyAsDf(allSteps, function(st) {
      outDatedLinks <- NULL
      linkIds <- NULL

      if (includeAllSteps) {
        # When rerunning all steps, update all internal links for this step
        internalLinks <- env$internalLinks[
          env$internalLinks$targetStep == st$fullName,
        ]
        if (!is.null(internalLinks) && nrow(internalLinks) > 0) {
          linkIds <- .workflow_private$getLinkTarget(env, internalLinks)
        }
        allUpdates <- linkIds
      } else {
        # Only update outdated links when doing partial rerun
        outDatedLinksDf <- dplyr::filter(
          caof,
          .data$nodeType == "LIV" & .data$stepEntityId == st$sourceEntityId
        )
        if (nrow(outDatedLinksDf) > 0) {
          outDatedLinks <- outDatedLinksDf %>%
            dplyr::pull("entityId") %>%
            loadResource() %>%
            dplyr::pull("entityId")
        }
        internalLinks <- env$internalLinks[
          env$internalLinks$targetStep == st$fullName,
        ]
        if (!is.null(internalLinks)) {
          linkIds <- .workflow_private$getLinkTarget(env, internalLinks)
        }
        allUpdates <- Filter(
          function(x) !is.null(x),
          unique(c(outDatedLinks, linkIds))
        )
      }
      st$toUpdate <- list(allUpdates)
      usingSteps <- env$internalLinks[
        env$internalLinks$sourceStep == st$fullName,
      ]$targetStep
      usedSteps <- env$internalLinks[
        env$internalLinks$targetStep == st$fullName,
      ]$sourceStep
      st$dependencies <- paste(usedSteps, collapse = ",", sep = "/")
      if (st$dependencies == "") {
        st$dependencies <- NA
      }
      st$usage <- paste(usingSteps, collapse = ",", sep = "/")
      if (st$usage == "") {
        st$usage <- NA
      }
      return(st)
    })

    executionPlan <- dplyr::select(
      executionPlan,
      "description",
      "rationale",
      "sourceEntityId",
      "sourceName",
      "fullName",
      "toUpdate",
      "dependencies",
      "usage"
    )
    executionPlan$inPlace <- TRUE
    return(executionPlan)
  }

  # Rerun all changed and outdated steps in the workflow
  # @param includeDownstream If TRUE (default), also rerun steps that use the outputs of changed steps
  # @return Invisibly returns NULL
  env$rerunChangedAndOutdated <- function(includeDownstream = TRUE) {
    plan <- env$createReexecutionPlan(
      includeDownstream = includeDownstream,
      includeAllSteps = FALSE
    )
    env$executePlan(plan)
    invisible(NULL)
  }

  # Rerun all steps in the workflow in dependencies order
  # @return Invisibly returns NULL
  env$rerunAll <- function() {
    plan <- env$createReexecutionPlan(
      includeDownstream = FALSE,
      includeAllSteps = TRUE
    )
    env$executePlan(plan)
    invisible(NULL)
  }

  # Create an execution plan for all steps in the workflow
  # @return A data.frame describing the execution plan with proper dependencies ordering
  env$createFullExecutionPlan <- function() {
    return(env$createReexecutionPlan(
      includeDownstream = FALSE,
      includeAllSteps = TRUE
    ))
  }

  # Execute a given execution plan
  # @param executionPlan A data.frame as returned by \code{createReexecutionPlan}
  # @return Invisibly returns NULL
  env$executePlan <- function(executionPlan) {
    if (is.null(executionPlan) || nrow(executionPlan) == 0) {
      log_info("No steps to execute")
      return(invisible(NULL))
    }
    orderedWorkflow <- .workflow_private$executionOrder(env, executionPlan)
    if (is.null(orderedWorkflow) || nrow(orderedWorkflow) == 0) {
      log_info("No executable order could be determined")
      return(invisible(NULL))
    }
    executionList <- c()
    for (i in seq_len(nrow(orderedWorkflow))) {
      nextData <- orderedWorkflow[i, ]
      nextItem <- nextData$fullName

      if (
        "dependencies" %in% names(nextData) && !is.na(nextData$dependencies)
      ) {
        dependencies <- unique(strsplit(nextData$dependencies, ",")[[1]])
        for (dep in dependencies) {
          if (dep %in% executionList) {
            log_info("waiting to finish")
            finishRunResource(env$steps[[dep]]$stepDf$sourceEntityId)
            executionList <- executionList[executionList != dep]
          }
        }
      }
      if ("toUpdate" %in% names(nextData) && !is.null(nextData$toUpdate[[1]])) {
        updateLinks(nextData$toUpdate[[1]])
      }
      if ("inPlace" %in% names(nextData) && nextData$inPlace) {
        runStepResource(nextData$sourceEntityId)
      } else {
        stepEnv <- env$steps[[nextData$fullName]]
        stepEnv$realise()
      }
      executionList <- c(executionList, nextItem)
    }
    if (length(executionList) > 0) {
      for (item in executionList) {
        finishRunResource(env$steps[[item]]$stepDf$sourceEntityId)
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
    rm(list = c(step), pos = env$steps)
    removeBacklinks <- function(toLink, backLink) {
      affected <- ls(stepEnv[[toLink]])
      affected <- affected[affected != "load"]
      x <- lapply(affected, function(blRemoveTarget) {
        targetEnv <- env$steps[[blRemoveTarget]][[backLink]]
        rm(list = c(step), pos = targetEnv)
      })
    }
    suppressWarnings({
      removeBacklinks("children", "parent")
      removeBacklinks("parent", "children")
      removeBacklinks("dependencies", "usage")
      removeBacklinks("usage", "dependencies")
    })
    .workflow_private$collectInternalLinks(env)
  }

  env$createTemplate <- function(addParental=F) {
    return(createWorkflowTemplateEnv(env,addParental))
  }


  # Only public methods are attached to env
  env
}
