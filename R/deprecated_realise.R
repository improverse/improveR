#' Deprecated realise function for compatibility with older repository versions
#'
#' This function implements the old realiseStep logic from improveRmodify
#' for compatibility with repository versions < 4.4
#'
#' @param env The step template environment
#' @param force Force creation of step even if equivalent exists
#' @param run Automatically run the step after creation
#' @return The step template environment (invisibly)
#' @keywords internal
realise_deprecated <- function(env, force = TRUE, run = TRUE,workflow=NULL) {
  improveEditable()

  # Handle breakpoint and reuse settings
  breakPoint <- env$getStepValue("breakpoint")
  reuse <- env$getStepValue("reuse")
  if (!is.null(breakPoint) && breakPoint == TRUE) {
    run <- FALSE
  }
  if (!is.null(reuse) && reuse == TRUE) {
    force <- FALSE
  }

  # Get step data from the environment
  prepStep <- env$stepDf

  # Validate tree - match the logic from the new version
  treeIdent <- prepStep$treeIdent
  if (is.null(treeIdent) || is.na(treeIdent)) {
    tryCatch({
      targetResource <- loadResource(env$stepDf$treePath)
      treeIdent <- createAnalysisTree(targetResource, treeName = env$stepDf$treeName)$resourceId
      prepStep$treeIdent <- treeIdent
      env$stepDf$treeIdent <- treeIdent
    }, error = function(e) {
      logging::logerror(e)
      logging::logerror("treeIdent or treePath and treeName need to be specified in order to create the step")
      stop("could not create step")
    })
  }

  # Create the step using the old approach
  newStep <- createPreparedStep_deprecated(env, prepStep)

  # Store the entity ID
  env$setStepValue("entityId", as.character(newStep$entityId))
  env$stepDf$templateEntityId <- env$stepDf$sourceEntityId
  env$setStepValue("sourceEntityId", as.character(newStep$entityId))
  env$stepDf$sourceEntityId <- as.character(newStep$entityId)

  # Get tree info
  tree <- loadResource(newStep$parentId)
  env$setStepValue("treeIdent", tree$resourceId)
  env$setStepValue("treeName", tree$name)
  env$setStepValue("treePath", dirname(tree$path))

  # Run if requested
  if (getStepState_deprecated(newStep) == "INITIAL" && run) {
    runStep_deprecated(newStep)
  }
  return(getStep(newStep$entityId, workflow = workflow))
}

#' Create prepared step using old API approach
#' @keywords internal
#' @noRd
createPreparedStep_deprecated <- function(env, prepStep) {
  logging::logdebug("createPreparedStep_deprecated")

  # Get runserver and tool information from the process configuration
  runserver <- NULL
  tool <- NULL
  mainProcess<-NULL
  # Check if we have process information
  if (!is.null(prepStep$processes) && length(prepStep$processes) > 0) {
    processes <- prepStep$processes[[1]]
    logging::logdebug(paste0("Found ", nrow(processes), " processes"))

    # Get the main process (or first process)
    mainProcess <- processes[processes$name == "Main", ]
    if (nrow(mainProcess) == 0) {
      mainProcess <- processes[1, ]
    }

    logging::logdebug(paste0("Main process - runserverLabel: ", mainProcess$runserverLabel))
    logging::logdebug(paste0("Main process - toolLabel: ", mainProcess$toolLabel))
    logging::logdebug(paste0("Main process - toolInstance: ", mainProcess$toolInstance))

    # Extract runserver and tool info from process
    if (!is.null(mainProcess$runserverLabel) && !is.na(mainProcess$runserverLabel) && mainProcess$runserverLabel != "") {
      runserver <- loadRunserver(mainProcess$runserverLabel)

      if (!is.null(runserver) && !is.null(mainProcess$toolLabel) && !is.na(mainProcess$toolLabel) && mainProcess$toolLabel != "") {
        tools <- loadToolsForRunserver(runserver$id)

        # Try to find the tool by label and instance
        if (!is.null(mainProcess$toolInstance) && !is.na(mainProcess$toolInstance) && mainProcess$toolInstance != "") {
          tool <- loadToolForRunserver(runserver$id,
                                      toolName = mainProcess$toolLabel,
                                      toolInstanceName = mainProcess$toolInstance)
        } else {
          tool <- loadToolForRunserver(runserver$id,
                                      toolName = mainProcess$toolLabel)
        }
      }
    }
  } else {
    logging::logdebug("No processes found in prepStep")
  }

  # Fallback to old field names if present
  if (is.null(runserver) && !is.null(prepStep$runserverName)) {
    runserver <- loadRunserver(prepStep$runserverName)
    if (!is.null(runserver) && !is.null(prepStep$toolName)) {
      tools <- loadToolsForRunserver(runserver$id)
      if (!is.null(prepStep$runserverToolName)) {
        tool <- loadToolForRunserver(runserver$id, prepStep$toolName, prepStep$runserverToolName)
      } else {
        tool <- loadToolForRunserver(runserver$id, toolName = prepStep$toolName)
      }
    }
  }

  # Always create a new step in deprecated version (don't reuse existing entityId)
  # This ensures each realise() creates a fresh step
  newStep <- NULL
  parentIdent <- if (!is.null(prepStep$parentIdent)) prepStep$parentIdent else NULL
  toolId <- if (!is.null(tool)) tool$toolId else NULL

  # Handle parent relationship
  # When a parent exists, always pick a different tool to avoid input file inheritance issues
  if (!is.null(parentIdent) && !is.null(tools)) {
    parent <- loadResource(parentIdent)
    # Always pick a different tool than parent to prevent input file conflicts
    toolId <- as.character(tools[tools$toolId != parent$toolId, ]$toolId[1])
  }

  newStep <- createStep(prepStep$treeIdent, parentIdent, toolId = toolId)

  if (is.null(newStep)) {
    stop("Failed to create step")
  }

  logging::logdebug("created step")

  # Get main process
  main <- getMainProcess(newStep$resourceId)
  processId <- main$id

  # Debug logging
  logging::logdebug(paste0("Main process found: ", processId))
  logging::logdebug(paste0("Runserver loaded: ", !is.null(runserver)))
  logging::logdebug(paste0("Tool loaded: ", !is.null(tool)))

  # Set tool arguments if provided
  toolArguments <- NULL
  if (!is.null(prepStep$commandline)) {
    toolArguments <- ""
    if (!is.null(prepStep$appendCommandline) && prepStep$appendCommandline) {
      toolArguments <- paste0(main$toolArgs, "\r\n")
    }
    toolArguments <- paste0(toolArguments, prepStep$commandline)
  }

  # Update process variables if we have runserver/tool info
  if (!is.null(runserver) && !is.null(tool)) {
    logging::logdebug(paste0("Setting process variables with tool: ", tool$name))
    setProcessVariables(newStep$resourceId, processId,
                       runserverId = runserver$id,
                       toolId = tool$toolId,
                       runserverToolId = tool$id,
                       gridTool = !is.na(tool$gridProvider),
                       toolArguments = toolArguments)
  } else if (!is.null(toolArguments)) {
    logging::logdebug("Setting only tool arguments")
    # Just update tool arguments
    setProcessVariables(newStep$resourceId, processId,
                       toolArguments = toolArguments)
  } else {
    logging::logdebug("No runserver/tool info found to set")
  }

  logging::logdebug("process variables set")

  # Update processes
  main <- updateProcessesForStep(newStep$resourceId)

  # Handle step naming
  if (!is.null(prepStep$stepName)) {
    stepName <- prepStep$stepName
    # Check if name exists
    testResource <- loadResource(paste0("./", stepName), from = newStep$parentId)
    if (!is.null(testResource)) {
      # Generate unique name
      nameNotCleared <- TRUE
      counter <- 1
      while (nameNotCleared) {
        stepName <- paste(prepStep$stepName, counter)
        counter <- counter + 1
        testResource <- loadResource(paste0("./", stepName), from = newStep$parentId)
        nameNotCleared <- !is.null(testResource)
      }
    }
    env$setStepValue("stepName", stepName)
    move(newStep, newStep$parentId, targetName = stepName)
  }

  # Set description and rationale
  if (
    !is.null(prepStep$description) &&
      prepStep$description != "" &&
      !is.na(prepStep$description)
  ) {
    changeStepDescription_deprecated(
      newStep,
      description = prepStep$description
    )
  }

  if (
    !is.null(prepStep$rationale) &&
      prepStep$rationale != "" &&
      !is.na(prepStep$rationale)
  ) {
    changeStepRationale_deprecated(newStep, rationale = prepStep$rationale)
  }

  logging::logdebug("step names and descriptions set")

  # Reload resources to get fresh state
  unloadResource(newStep)
  unloadChildResources(newStep)
  unloadFullChildResources(newStep)
  if (!is.null(newStep$parentId)) {
    unloadChildResources(newStep$parentId)
    unloadFullChildResources(newStep$parentId)
  }
  newStep <- updateResource(newStep$resourceId)

  # Update the step reference after reloading
  if (is.null(newStep)) {
    logging::logerror("Failed to reload step after modifications")
    stop("Step reload failed")
  }

  # Add files from the environment's file lists
  # Remote files
  remoteFiles <- prepStep$remoteFiles[[1]]
  if (!is.null(remoteFiles) && NROW(remoteFiles) > 0) {
    remoteFiles <- dplyr::distinct(remoteFiles, name, .keep_all = TRUE)
  }
  if (!is.null(remoteFiles) && NROW(remoteFiles) > 0) {
    for (i in 1:nrow(remoteFiles)) {
    }
    byNotEmpty(remoteFiles, function(filePrep) {
      result <- addFileToStep_deprecated(newStep, filePrep, FALSE, env)
      result
    })
  }

  logging::logdebug("remote files set")

  # Local files
  localFiles <- prepStep$localFiles[[1]]
  if (!is.null(localFiles) && nrow(localFiles) > 0) {
    byNotEmpty(localFiles, function(filePrep) {
      addFileToStep_deprecated(newStep, filePrep, TRUE, env)
    })
  }

  # External links
  extLinks <- prepStep$extLinks[[1]]
  if (!is.null(extLinks) && nrow(extLinks) > 0) {
    byNotEmpty(extLinks, function(linkPrep) {
      addExtLinkToStep_deprecated(newStep, linkPrep)
    })
  }

  logging::logdebug("local files and links set")

  # Set grid arguments
  gridArguments <- mainProcess$gridArguments[[1]]
  if (is.data.frame(gridArguments) && nrow(gridArguments) > 0) {
    byNotEmpty(gridArguments, function(gridArgument) {
      setGridArgument(processId, gridArgument$argumentName, gridArgument$argumentValue, update = TRUE)
    })
  }

  logging::logdebug("grid arguments set")

  # Final reload
  unloadResource(newStep)
  unloadChildResources(newStep)
  unloadFullChildResources(newStep)
  if (!is.null(newStep$parentId)) {
    unloadChildResources(newStep$parentId)
    unloadFullChildResources(newStep$parentId)
  }
  newStep <- loadResource(newStep$resourceId)

  logging::logdebug("reloaded")

  return(newStep)
}

#' Get step state for deprecated version
#' @keywords internal
#' @noRd
getStepState_deprecated <- function(step) {
  if (is.character(step)) {
    # It's a resource ID
    step <- updateResource(step)
  }
  return(step$runStatus)
}

#' Run step using deprecated approach
#' @keywords internal
#' @noRd
runStep_deprecated <- function(step) {
  if (is.character(step)) {
    stepResourceId <- step
  } else {
    stepResourceId <- step$resourceId
  }

  result <- authenticatedREST("/resources/{stepId}/run",
                            urlParams = list(stepId = stepResourceId),
                            restType = "POST")
  return(result)
}

#' Change step description using PUT request
#' @keywords internal
#' @noRd
changeStepDescription_deprecated <- function(step, description) {
  stepEntity <- if (is.character(step)) loadResource(step) else step
  stepEntity$description <- description

  result <- authenticatedREST("/resources/{resourceId}/",
                            urlParams = list(resourceId = stepEntity$resourceId),
                            data = as.list(stepEntity),
                            restType = "PUT")
  return(result)
}

#' Change step rationale using PUT request
#' @keywords internal
#' @noRd
changeStepRationale_deprecated <- function(step, rationale) {
  stepEntity <- if (is.character(step)) loadResource(step) else step
  stepEntity$rationale <- rationale

  result <- authenticatedREST("/resources/{resourceId}/",
                            urlParams = list(resourceId = stepEntity$resourceId),
                            data = as.list(stepEntity),
                            restType = "PUT")
  return(result)
}

#' Add file to step - deprecated version
#' @keywords internal
#' @noRd
addFileToStep_deprecated <- function(newStep, filePrep, isLocal, env) {

  if (is.null(filePrep)) {
    return()
  }

  createTarget <- newStep

  # Handle file name and folder structure
  fileName <- if ("name" %in% names(filePrep)) filePrep$name else ""
  if (is.na(fileName) || is.null(fileName)) {
    fileName <- ""
  }

  if (startsWith(fileName,"./")) {
    fileName<-substr(fileName,3,nchar(fileName))
  }
  # Check for subfolder in filename
  if (grepl("/", fileName, fixed = TRUE)) {
    pathParts <- strsplit(fileName, "/", fixed = TRUE)[[1]]
    if (length(pathParts) != 2) {
      logging::logwarn("maximum folder depth allowed is 1, by filename in realise step")
      logging::logwarn(fileName)
      return()
    }
    folderName <- pathParts[1]
    fileName <- pathParts[2]

    # Check if folder exists
    children <- loadChildResources(newStep)
    if (is.null(children) || nrow(children) == 0) {
      # No children, safe to create folder
      folder <- data.frame()
    } else {
      folder <- children[children$name == folderName, ]
    }

    if (nrow(folder) == 1 && folder$nodeType != "Folder") {
      logging::logwarn(paste(folderName, "already exists but not as folder"))
      return()
    }

    if (nrow(folder) == 1) {
      createTarget <- folder
    } else {
      # Use the step's resourceId for folder creation
      createTarget <- createFolder(newStep$resourceId, folderName = folderName)

      if (is.null(createTarget)) {
        logging::logerror(paste0("addFileToStep_deprecated: Failed to create folder '", folderName, "'"))
        return()
      }

      # Reload children after creating folder
      unloadChildResources(newStep)
      children <- loadChildResources(newStep)
    }
  }

  # Handle variable name
  if (!("variableName" %in% names(filePrep))) {
    filePrep$variableName <- ""
  }

  # Create the file
  if (is.data.frame(createTarget)) {
  }

  newFile <- NULL
  if (isLocal) {
    if (is.na(filePrep$path) || is.null(filePrep$path)) {
      return()
    }
    if (is.null(fileName) || fileName == "") {
      fileName <- basename(filePrep$path)
    }
    newFile <- createFile(targetIdent = createTarget, fileName = fileName, localPath = filePrep$path)
    if (!is.null(newFile)) {
    }
  } else {
    # Remote file - handle as link or copy
    if (!is.null(filePrep$asLink) && filePrep$asLink) {
      if (!is.null(filePrep$ident)) {
        # When in a workflow template context with internal links, resolve to new step
        fileIdentToLink <- filePrep$ident

        if (!is.null(env$workflow) && !is.null(env$workflow$internalLinks) &&
            "sourceStep" %in% names(filePrep) && !is.na(filePrep$sourceStep)) {
          # This is an internal workflow link
          internalLinks <- env$workflow$internalLinks
          currentStepName <- env$stepDf$fullName

          # Find the matching internal link
          # The internal link should have this file's name and target step
          fileName <- filePrep$name
          if (is.null(fileName) || is.na(fileName)) {
            fileName <- ""
          }
          cleanName <- fileName
          if (!startsWith(cleanName, "./")) {
            cleanName <- paste0("./",cleanName)
          }

          # Match by target step and name
          matchingLinks <- internalLinks[
            internalLinks$targetStep == currentStepName &
            (internalLinks$name == cleanName | internalLinks$name == fileName),
          ]

          if (nrow(matchingLinks) > 0) {
              matchingLinks<-matchingLinks[1,]
              # Get source step and path information
              sourceStepName <- matchingLinks$sourceStep[1]
              sourceStepEnv <- env$workflow$stepTemplates[[sourceStepName]]

              linkPath<-NULL
              #TODO check import
              if ("sourceInventoryPath" %in% names(matchingLinks) ) {
                linkPath <- matchingLinks$sourceInventoryPath[1]
                if (!startsWith(linkPath,"./")) {
                  linkPath <- paste0("./",linkPath)
                }
              }
              if (!is.null(linkPath)){
                oldStep <- loadResource(sourceStepEnv$stepDf$sourceEntityId)
                if (!is.null(oldStep)) {
                  linkRes <- loadResource(linkPath,from = oldStep)
                  fileIdentToLink <- linkRes$resourceId
                }

              }

            }

        }
        if (startsWith(fileName,prefix = "./")) {
          fileName <- substr(fileName,3,nchar(fileName))
        }
        newFile <- createLink(linkContainer = createTarget$resourceId, links = fileIdentToLink, linkName = fileName)
      }
    } else {
      # Copy the file
      if (!is.null(filePrep$ident)) {
      newFile <- copy(filePrep$ident, createTarget$resourceId, targetName = fileName)
      if (is.null(newFile)) {
        logging::logerror(paste0("addFileToStep_deprecated: Failed to copy file '", fileName, "'"))
      } else {
      }
      }
    }
  }

  # Handle process variables if specified
  if (!is.null(newFile) && !is.na(filePrep$variableName) && filePrep$variableName != "") {
    processId <- as.character(getMainProcess(newStep$resourceId)$id)
    variables <- getProcessFileVariables(newStep, processId)
    variableId <- as.character(variables[variables$name == filePrep$variableName, ]$id)

    if (length(variableId) > 0 && !is.na(variableId)) {
      result <- authenticatedREST("/resources/{resourceId}/processes/{processId}/variables/{variableId}",
                                urlParams = list(resourceId = newStep$resourceId,
                                               processId = processId,
                                               variableId = variableId),
                                data = list(type = "processVariable",
                                           id = variableId,
                                           name = filePrep$variableName,
                                           position = 1,
                                           valueResourceId = newFile$resourceId,
                                           variableType = "fileRef"),
                                restType = "PUT")
    }
  }

}

#' Add external link to step - deprecated version
#' @keywords internal
#' @noRd
addExtLinkToStep_deprecated <- function(newStep, linkPrep) {
  if (is.null(linkPrep) || is.na(linkPrep$url)) {
    return()
  }

  createTarget <- newStep
  fileName <- if ("name" %in% names(linkPrep)) linkPrep$name else "External Link"

  if (is.na(fileName) || is.null(fileName)) {
    fileName <- "External Link"
  }

  # Handle subfolder
  if (grepl("/", fileName, fixed = TRUE)) {
    pathParts <- strsplit(fileName, "/", fixed = TRUE)[[1]]
    if (length(pathParts) != 2) {
      logging::logwarn("maximum folder depth allowed is 1")
      return()
    }
    folderName <- pathParts[1]
    fileName <- pathParts[2]

    children <- loadChildResources(newStep)
    folder <- children[children$name == folderName, ]

    if (nrow(folder) == 1 && folder$nodeType != "Folder") {
      logging::logwarn(paste(folderName, "already exists but not as folder"))
      return()
    }

    if (nrow(folder) == 1) {
      createTarget <- folder
    } else {
      createTarget <- createFolder(newStep, folderName = folderName)
    }
  }

  # Create external link
  newLink <- createExternalLink(targetIdent = createTarget, linkName = basename(fileName), url = linkPrep$url)
  return(newLink)
}
