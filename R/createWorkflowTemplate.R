testEnv <- function() {
  env <- new.env(parent = emptyenv())
  env$pwd<- function() {
    return(pwd())
  }
  return(env)
}




#' Create a workflow template environment from an existing workflow
#'
#' Extracts all steps, relationships, parameters, and files from the workflow
#' and builds a reusable workflow template environment.
#' @param workflow The workflow environment to template
#' @param addParental if T the created stepTemplates have the steps from the workflow as parents (default: FALSE)
#' @return An environment representing the workflow template
#' @export
createWorkflowTemplateEnv <- function(workflow,addParental=F) {
  env <- new.env(parent = emptyenv())
  env$this <- env
  env$workflow <- workflow




  internalLinks<-workflow$internalLinks
  if (!is.null(internalLinks) && nrow(internalLinks) > 0) {
    # Only select columns that exist in the data frame
    requiredCols <- c("entityId","fileHash","revisionId","path","targetStep","sourceStep","sourceInventoryPath","name")
    existingCols <- intersect(names(internalLinks), requiredCols)
    if (length(existingCols) > 0) {
      internalLinks <- dplyr::select(internalLinks, dplyr::all_of(existingCols))
    }
  }


  # Extract steps and their templates
  stepTemplates <- list()
  for (stepName in names(workflow$steps)) {
    stepEnv <- workflow$steps[[stepName]]
    # Convert each step to a template environment
    stepDf <- stepEnv$stepDf
    if (addParental) {
      stepDf$parentIdent<-stepDf$sourceEntityId
    }
    workflowLinks <- internalLinks[internalLinks$targetStep==stepName,]
    if (!is.null(workflowLinks) && nrow(workflowLinks)>0) {
      remoteFiles<- stepDf$remoteFiles[[1]]
      jointRemoteFiles <- dplyr::left_join(remoteFiles,workflowLinks,by=c("name"="name"))
      stepDf$remoteFiles<-list(jointRemoteFiles)
      #case import:
      if (F) {


      if (!("sourceStep"%in%names(remoteFiles))) {
        # During import, entityId might not exist, need a different join strategy
        if ("entityId" %in% names(workflowLinks) && "ident" %in% names(remoteFiles)) {
          # Normal case: join by entityId
          jointRemoteFiles <- dplyr::left_join(remoteFiles,workflowLinks,by=c("ident"="entityId"))
        } else {
          # Import case: manually match based on targetStep + name combination
          # Since we already filtered workflowLinks by targetStep (line 49), we're only
          # matching within a single step's context. File names within a single step
          # should be unique (can't have two files with same path in one step).
          # This makes the name-based matching safe within this filtered context.
          jointRemoteFiles <- remoteFiles
          if ("name" %in% names(workflowLinks) && "name" %in% names(remoteFiles)) {
            for (i in seq_len(nrow(jointRemoteFiles))) {
              # Find matching link for this specific remote file
              # Clean the name to match (remove ./ prefix if present)
              cleanName <- jointRemoteFiles$name[i]
              if (startsWith(cleanName, "./")) {
                cleanName <- substr(cleanName, 3, nchar(cleanName))
              }
              matchingLinks <- workflowLinks[workflowLinks$name == cleanName | workflowLinks$name == jointRemoteFiles$name[i],]
              if (nrow(matchingLinks) > 0) {
                # Take the first match (they should all be the same for this targetStep+name combo)
                for (col in names(matchingLinks)) {
                  if (!(col %in% names(jointRemoteFiles))) {
                    jointRemoteFiles[[col]] <- NA
                  }
                  jointRemoteFiles[i, col] <- matchingLinks[1, col]
                }
              }
            }
          }
        }

        jointRemoteFiles <-byNotEmptyAsDf(jointRemoteFiles,function(remoteFile) {

          if ("sourceStep" %in% names(remoteFile) && !is.na(remoteFile$sourceStep)) {
            # If sourceInventoryPath is already present from the workflow links, use it
            if ("sourceInventoryPath" %in% names(remoteFile) && !is.na(remoteFile$sourceInventoryPath)) {
              # Already has sourceInventoryPath, nothing to do
            } else {
              # Try to construct sourceInventoryPath from the source file
              # First, we need to find the actual source file in the source step
              sourceStepEnv <- workflow$steps[[remoteFile$sourceStep]]
              if (!is.null(sourceStepEnv)) {
                # Get all files from the source step
                sourceStepFiles <- sourceStepEnv$stepDf$localFiles[[1]]
                sourceStepRemoteFiles <- sourceStepEnv$stepDf$remoteFiles[[1]]

                # Find the file that matches this link's entity ID
                matchingFile <- NULL
                if (!is.null(sourceStepRemoteFiles) && "ident" %in% names(sourceStepRemoteFiles)) {
                  matchingFile <- sourceStepRemoteFiles[sourceStepRemoteFiles$ident == remoteFile$ident,]
                }

                if (!is.null(matchingFile) && nrow(matchingFile) > 0) {
                  # Use the name from the matching file in the source step
                  sourceFileName <- matchingFile$name[1]
                  if (startsWith(sourceFileName, "./")) {
                    remoteFile$sourceInventoryPath <- sourceFileName
                  } else {
                    remoteFile$sourceInventoryPath <- paste0("./", sourceFileName)
                  }
                } else if (!is.null(sourceStepFiles) && "name" %in% names(sourceStepFiles)) {
                  # Try to match by name in local files
                  cleanName <- remoteFile$name
                  if (startsWith(cleanName, "./")) {
                    cleanName <- substr(cleanName, 3, nchar(cleanName))
                  }
                  matchingFile <- sourceStepFiles[sourceStepFiles$name == cleanName | sourceStepFiles$name == remoteFile$name,]
                  if (nrow(matchingFile) > 0) {
                    sourceFileName <- matchingFile$name[1]
                    if (startsWith(sourceFileName, "./")) {
                      remoteFile$sourceInventoryPath <- sourceFileName
                    } else {
                      remoteFile$sourceInventoryPath <- paste0("./", sourceFileName)
                    }
                  }
                }
              }
            }
          }
          if (!("name" %in% names(remoteFile)) || is.na(remoteFile$name)) {
            # Try to get name from resource, but handle case where it doesn't exist
            if (!is.null(remoteFile$ident) && !is.na(remoteFile$ident)) {
              resource <- loadResource(remoteFile$ident)
              if (!is.null(resource) && !is.null(resource$name)) {
                remoteFile$name<-paste0("./",resource$name)
              }
            }
          }
          return(remoteFile)
        })
        stepDf$remoteFiles<-list(jointRemoteFiles)
      }
}
    }
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


  # List all steps in the workflow template as a data frame
  # @return A data.frame with step metadata
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
    .workflow_template_private$setValue("treePath",rootFolder)
    .workflow_template_private$setValue("treeIdent",NULL)
  }

  env$setWorkflowTreeName <- function(treeName) {
    .workflow_template_private$setValue("treeName",treeName)
    .workflow_template_private$setValue("treeIdent",NULL)
  }

  env$setWorkflowTreeIdent <- function(treeIdent,from=pwd()) {

    treeResource <- loadResource(treeIdent)
    if (is.null(treeResource) || treeResource$nodeType!="Analysis Tree") {
      log_error(treeIdent,"does not exist or is not a Tree")
    } else {
      .workflow_template_private$setValue("treeIdent",treeResource$resourceId)
      .workflow_template_private$setValue("treeName",treeResource$name)
      .workflow_template_private$setValue("treePath",loadResource(treeResource$parentId)$path)
    }

  }

  # Create a re-execution plan for outdated steps
  # @return A data.frame describing the execution plan
  env$createExecutionPlan <- function() {
    stepsDf <- env$df()

    .workflow_template_private$collectInternalLinks(env)
    iL <- env$internalLinks

    allSteps <- stepsDf
    executionPlan <- byNotEmptyAsDf(allSteps, function(st) {
      internalLinks <- env$internalLinks[env$internalLinks$targetStep == st$fullName, ]
      usingSteps <- env$internalLinks[env$internalLinks$sourceStep == st$fullName, ]$targetStep
      usedSteps <- env$internalLinks[env$internalLinks$targetStep == st$fullName, ]$sourceStep
      st$lineage <- paste(unique(usedSteps), collapse = ",", sep = "/")
      if (st$lineage == "") st$lineage <- NA
      st$usage <- paste(unique(usingSteps), collapse = ",", sep = "/")
      if (st$usage == "") st$usage <- NA
      st$toUpdate<-NA
      return(st)
    })
    if (!("description" %in% names(executionPlan))) executionPlan$description<-""
    if (!("rationale" %in% names(executionPlan))) executionPlan$rationale<-""
    if (!("sourceEntityId" %in% names(executionPlan))) executionPlan$sourceEntityId<-""
    if (!("sourceName" %in% names(executionPlan))) executionPlan$sourceName<-""
    executionPlan <- dplyr::select(
      executionPlan,
      "description",
      "rationale",
      "sourceEntityId",
      "sourceName",
      "fullName",
      "toUpdate",
      "lineage",
      "usage"
    )
    executionPlan$inPlace <- FALSE
    return(executionPlan)
  }


  # Execute a given execution plan
  # @param executionPlan A data.frame as returned by \code{createReexecutionPlan}
  # @return Invisibly returns NULL
  env$executePlan <- function(executionPlan) {
    orderedWorkflow <-  .workflow_template_private$executionOrder(env, executionPlan)
    workflow <- NULL
    executionList <- c()
    for (i in seq_len(nrow(orderedWorkflow))) {
      nextData <- orderedWorkflow[i, ]
      nextItem <- nextData$fullName

      if ("lineage" %in% names(nextData) && !is.na(nextData$lineage)) {
        dependencies <- unique(strsplit(nextData$lineage, ",")[[1]])
        for (dependency in dependencies) {
          if (dependency %in% executionList) {
            logging::loginfo("waiting to finish")
            finishRunResource(env$stepTemplates[[dependency]]$stepDf$entityId)
            executionList <- executionList[executionList != dependency]
          }
        }
      }
      if ("toUpdate" %in% names(nextData) && !is.null(nextData$toUpdate[[1]]) && !is.na(nextData$toUpdate[[1]])) {
        updateLinks(nextData$toUpdate[[1]])
      }
      if ("inPlace" %in% names(nextData) && nextData$inPlace) {
        runStepResource(nextData$sourceEntityId)
      } else {
        stepEnv <- env$stepTemplates[[nextData$fullName]]
        returnStep <- stepEnv$realise(workflow=workflow)
        if (is.null(workflow)) {
          workflow<-returnStep$workflow
        }
      }
      executionList <- c(executionList, nextItem)
    }
    if (length(executionList) > 0) {
      for (item in executionList) {
        finishRunResource(env$steps[[item]]$stepDf$sourceEntityId)
      }
    }
    return(workflow)
  }


  env$realise <- function() {
    return(env$executePlan(env$createExecutionPlan()))
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
      # Initialize as empty data frame when no remote files
      env$internalLinks <- data.frame()
      return(NULL)
    }
    if ("sourceStep" %in% names(remoteFiles)) {
      allTargets <- remoteFiles[!is.na(remoteFiles$sourceStep),]
      env$internalLinks <- allTargets
    } else {
      # Initialize as empty data frame when no sourceStep column
      env$internalLinks <- data.frame()
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
    for (s in seq_len(nrow(startSteps))) {
      startStep <- startSteps[s, ]
      if (!is.na(startStep$usage)) {
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

    }
    if (nrow(plan) == 0 || counter > 500) {
      if (counter > 500) {
        logging::logwarn("could not add all steps to execution order, check for cycles")
      }
      return(startSteps)
    }
    .workflow_template_private$executionOrderInternal(env, plan, startSteps, counter)
  }

  .workflow_template_private$setValue <- function(key,value) {
    stepNames <- data.frame(fullName = ls(env$stepTemplates))
    stepDf <- byNotEmpty(stepNames, function(stepName) {
      fullName <- stepName$fullName
      stepEnv <- env$stepTemplates[[fullName]]
      stepEnv$setStepValue(key,value)
      return(NULL)
    })
  }
  ############## INTERNAL METHODS END



  return(env)
}




  # TODO resolv parent kram






