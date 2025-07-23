#' Create a new step template environment
#'
#' Constructs a new step template object with encapsulated state and public API.
#' @param stepDf Data frame with step metadata (optional)
#' @param workflow Workflow environment this template belongs to (optional)
#' @return An environment representing the step template
#' @export
createStepTemplateEnv <- function(treeIdent = NULL, stepDf = NULL, workflow = NULL) {
  stepHandle <- uuid::UUIDgenerate()
  if (is.null(stepDf)) {
    stepDf <- data.frame(handle = stepHandle, stringsAsFactors = FALSE)
  }

  if (!is.null(treeIdent)) {
    resourceId <- loadResource(treeIdent)$resourceId
    stepDf$treeIdent <- resourceId
  }

  env <- new.env(parent = emptyenv())
  env$this <- env
  env$stepDf <- stepDf
  env$workflow <- workflow
  env$step <- NULL

  # --- Private helpers ---
  .template_private <- new.env(parent = emptyenv())

  .template_private$getStepValue <- function(key) {
    stepList <- env$stepDf
    if (!is.null(stepList) && key %in% names(stepList)) {
      return(as.character(stepList[key]))
    }
    return(NULL)
  }

  .template_private$getStepWithoutCache <- function() {
    entityId <- .template_private$getStepValue("entityId")
    step <- improveR:::internalLoadResourceFromServer(entityId)
    return(step)
  }

  .template_private$create <- function() {
    improveEditable()
    logging::logdebug("createPreparedStep")
    timing("createPreparedStep")

    prepStep <- env$stepDf
    treeIdent <- env$stepDf$treeIdent
    if (is.null(treeIdent) || is.na(treeIdent)) {
      tryCatch({
        targetResource <- loadResource(env$stepDf$treePath)
        treeIdent <- createAnalysisTree(targetResource, treeName = env$stepDf$treeName)$resourceId
      }, error = function(e) {
        log_error(e)
        log_error("treeIdent or treePath and treeName need to be specified in order to create the step")
        stop("could not create step")
      })
    }

    if (is.null(prepStep$description)) { prepStep$description <- "" }
    if (is.null(prepStep$rationale)) { prepStep$rationale <- "" }
    if (is.null(prepStep$comment)) { prepStep$comment <- "" }
    if (is.null(prepStep$keyStep)) { prepStep$keyStep <- FALSE }
    if (is.null(prepStep$baseModel)) { prepStep$baseModel <- FALSE }
    if (is.null(prepStep$fullModel)) { prepStep$fullModel <- FALSE }
    if (is.null(prepStep$finalModel)) { prepStep$finalModel <- FALSE }
    if (is.null(prepStep$referenceModel)) { prepStep$referenceModel <- FALSE }

    prepStep <- dplyr::select(prepStep,
      rationale,
      description,
      comment,
      keyStep,
      baseModel,
      fullModel,
      finalModel,
      referenceModel
    )

    if ("parentIdent" %in% names(env$stepDf) && !is.null(env$stepDf$parentIdent)) {
      prepStep$parentStepId <- env$stepDf$parentIdent
    }

    prepList <- as.list(prepStep)
    processNames <- env$stepDf$processes[[1]]$name

    if (length(processNames) == 0) {
      stop("cannot create a step without process")
    }

    processes <- NULL
    subFolderNameMapping <- NULL

    for (i in seq_along(processNames)) {
      processDf <- env$prepareProcess(processNames[i])
      subFolderNameMapping <- c(subFolderNameMapping, processDf$subFolderNameMapping)
      processDf$subFolderNameMapping <- NULL
      processes <- plyr::rbind.fill(processes, processDf)
    }
    prepList$processes <- processes
    #print(jsonlite::toJSON(prepList, auto_unbox = TRUE, pretty = TRUE))

    createResult <- improveR::authenticatedREST("/resources/{treeIdent}/steps",
      urlParams = list(treeIdent = treeIdent),
      data = prepList,
      restType = "POST"
    )
    if (createResult$status_code == 201) {
      createContent <- httr::content(createResult)
      newStep <- improveR::loadResource(createContent$resourceId)
      invisible(improveR::unloadChildResources(newStep$parentId))
      invisible(improveR::unloadFullChildResources(newStep$parentId))
      env$moveSubFolderNameMapping(subFolderNameMapping, newStep)
      return(newStep)
    }
    return(NULL)
  }

  # --- Public methods (directly attached to env) ---

  env$setStepValue <- function(key, value) {
    stepList <- env$stepDf
    stepList[key] <- value
    env$stepDf <- stepList
    invisible(env)
  }

  env$setProcessValue <- function(processName, key, value) {
    stepData <- env$stepDf
    processes <- stepData$processes[[1]]
    position <- 1
    if (!is.null(processes) && nrow(processes) > 0) {
      position <- max(processes$position) + 1
    }
    if (!(processName %in% processes$name)) {
      newProcess <- data.frame(
        name = processName,
        selected = TRUE,
        main = FALSE,
        processType = "post",
        position = position,
        stringsAsFactors = FALSE
      )
      if (processName == "Main") {
        newProcess$processType <- "main"
        newProcess$main <- TRUE
      }
      processes <- plyr::rbind.fill(newProcess, processes)
    }
    processes <- byNotEmptyAsDf(processes, function(process) {
      if (process$name == processName) {
        process[[key]] <- value
      }
      return(process)
    })
    stepData$processes <- list(processes)
    env$stepDf <- stepData
    invisible(env)
  }

  env$removeStepValue <- function(key) {
    stepList <- env$stepDf
    if (key %in% names(stepList)) {
      stepList[key] <- NULL
    }
    env$stepDf <- stepList
    invisible(env)
  }

  env$addStepValue <- function(key, value) {
    stepList <- env$stepDf
    valueList <- NULL
    if (key %in% names(stepList)) {
      valueList <- stepList[key][[1]][[1]]
    }
    if (typeof(value) == "character") {
      if (is.null(valueList)) {
        valueList <- value
      } else {
        valueList <- paste(valueList, value, sep = ",")
      }
      stepList[key] <- valueList
    } else if (is.data.frame(value)) {
      if (is.null(valueList)) {
        valueList <- value
      } else {
        valueList <- plyr::rbind.fill(valueList, value)
      }
      stepList[key] <- tidyr::nest(valueList, data = tidyr::everything())
    } else {
      logging::logwarn(paste0(
        "only character or dataframe allowed in stephandle for key: ",
        key
      ))
    }
    env$stepDf <- stepList
    invisible(env)
  }

  env$removeProcessValue <- function(key, processName) {
    stepList <- env$stepDf
    processes <- byNotEmptyAsDf(processes, function(process) {
      if (process$name == processName) {
        if (key %in% names(process)) {
          process[key] <- NULL
        }
      }
      return(process)
    })
    env$stepDf <- stepList
    invisible(env)
  }

  env$addProcessValue <- function(key, value, processName) {
    stepList <- env$stepDf
    processes <- byNotEmptyAsDf(processes, function(process) {
      if (process$name == processName) {
        valueList <- NULL
        if (key %in% names(process)) {
          valueList <- process[key][[1]][[1]]
        }
        if (typeof(value) == "character") {
          if (is.null(valueList)) {
            valueList <- value
          } else {
            valueList <- paste(valueList, value, sep = ",")
          }
          process[key] <- valueList
        } else if (is.data.frame(value)) {
          if (is.null(valueList)) {
            valueList <- value
          } else {
            valueList <- plyr::rbind.fill(valueList, value)
          }
          process[key] <- tidyr::nest(valueList, data = tidyr::everything())
        } else {
          logging::logwarn(paste0(
            "only character or dataframe allowed in stephandle for key: ",
            key
          ))
        }
      }
      return(process)
    })
    stepData$processes <- list(processes)
    env$stepDf <- stepList
    invisible(env)
  }

  env$setStepTree <- function(treeIdent) {
    tree <- loadResource(treeIdent)
    env$setStepValue("treeIdent", tree$resourceId)
    invisible(env)
  }

  env$setStepWorkflow <- function(workflowHandle) {
    env$setStepValue("workflowHandle", workflowHandle)
    invisible(env)
  }

  env$setStepParent <- function(parentIdent, inheritFromParent = FALSE) {
    if (!is.null(parentIdent)) {
      parent <- loadResource(parentIdent)
      env$setStepValue("parentIdent", parent$resourceId)
      env$setStepValue("inheritFromParent", inheritFromParent)
    } else {
      env$setStepValue("parentIdent", NULL)
      env$setStepValue("inheritFromParent", NULL)
    }
    invisible(env)
  }

  env$setStepBreakpoint <- function(breakpoint = TRUE) {
    env$setStepValue("breakpoint", breakpoint)
    invisible(env)
  }

  env$setStepReuse <- function(reuse = TRUE) {
    env$setStepValue("reuse", reuse)
    invisible(env)
  }

  env$setStepFinishCondition <- function(runserverLabel, runserverTool) {
    env$setStepValue("finishRunserverLabel", runserverLabel)
    env$setStepValue("finishRunserverTool", runserverTool)
    invisible(env)
  }

  env$setStepRunserverLabel <- function(runserverLabel, process = "Main") {
    env$setProcessValue(process, "runserverLabel", runserverLabel)
    invisible(env)
  }

  env$setStepName <- function(stepName) {
    env$setStepValue("stepName", stepName)
    invisible(env)
  }

  env$setStepDescription <- function(description) {
    env$setStepValue("description", description)
    invisible(env)
  }

  env$setStepRationale <- function(rationale) {
    env$setStepValue("rationale", rationale)
    invisible(env)
  }

  env$setStepCommandLine <- function(commandline, append = TRUE, process = "Main") {
    env$setProcessValue(process, "commandline", commandline)
    env$setProcessValue(process, "appendCommandline", append)
    invisible(env)
  }

  env$setStepToolInstance <- function(toolInstance, process = "Main") {
    env$setProcessValue(process, "toolInstance", toolInstance)
    env$completeToolPresets(process)
    invisible(env)
  }

  env$setStepToolLabel <- function(toolLabel, process = "Main") {
    env$setProcessValue(process, "toolLabel", toolLabel)
    invisible(env)
  }

  env$addStepRemoteFile <- function(ident = NULL, name = NULL, asLink = TRUE, variableName = NULL, sourceHandle = NULL, sourceName = NULL, variableProcess = "Main") {
    fileList <- data.frame(stepHandle = stepHandle, stringsAsFactors = FALSE)
    if (!is.null(ident)) {
      resource <- loadResource(ident)
      if (resource$nodeType == "File") {
        fileList["ident"] <- resource$entityId
        fileList["filehash"] <- resource$fileHash
      } else if (resource$nodeType == "Link") {
        fileList["ident"] <- resource$targetEntityId
        fileList["version"] <- resource$targetRevisionId
        target <- loadResource(resource$targetEntityId)
        fileList["filehash"] <- target$fileHash
        fileList["maxVersion"] <- target$revisionId
      } else {
        logging::logwarn("Only files or resources can be added to an inventory")
        logging::logwarn(ident)
        logging::logwarn(stepHandle)
        return(env)
      }
    }
    fileList["asLink"] <- asLink
    fileList["name"] <- name
    fileList["variableName"] <- variableName
    fileList["variableProcess"] <- variableProcess
    if (!is.null(sourceHandle)) {
      fileList["sourceHandle"] <- sourceHandle
      fileList["sourceName"] <- sourceName
      env$addStepValue(stepHandle, "dependencies", sourceHandle)
    }
    env$addStepValue("remoteFiles", fileList)
    invisible(env)
  }

  env$addExtLink <- function(name, url) {
    extLinkList <- data.frame(stepHandle = stepHandle, stringsAsFactors = FALSE)
    extLinkList$name <- name
    extLinkList$url <- url
    env$addStepValue("extLinks", extLinkList)
    invisible(env)
  }

  env$removeStepRemoteFile <- function(name = NULL, ident = NULL) {
    stepData <- env$stepDf
    remoteFiles <- stepData$remoteFiles[[1]]
    remoteFiles <- byNotEmptyAsDf(remoteFiles, function(file) {
      if (!is.null(name)) {
        if ("name" %in% names(file) && !is.na(file$name)) {
          fileName <- file$name
          if (fileName != name) {
            return(file)
          }
        } else if ("ident" %in% names(file)) {
          fileName <- loadResource(file$ident)$name
          if (fileName != name) {
            return(file)
          }
        }
      } else if (!is.null(ident)) {
        if ("ident" %in% names(file) && !is.na(file$ident)) {
          fileEntityId <- loadResource(file$ident)$entityId
          compareEntityId <- loadResource(ident)$entityId
          if (fileEntityId != compareEntityId) {
            return(file)
          }
        } else {
          return(file)
        }
      }
    })
    stepData$remoteFiles <- list(remoteFiles)
    env$stepDf <- stepData
    invisible(env)
  }

  env$removeStepLocalFile <- function(stepHandle, name = NULL, path = NULL) {
    stepData <- env$stepDf
    localFiles <- stepData$localFiles[[1]]
    localFiles <- byNotEmptyAsDf(localFiles, function(file) {
      if (!is.null(name)) {
        if ("name" %in% names(file) && !is.na(file$name)) {
          fileName <- file$name
          if (fileName != name) {
            return(file)
          }
        } else if ("path" %in% names(file) && !is.na(file$path)) {
          fileName <- basename(file$path)
          if (fileName != name) {
            return(file)
          }
        }
      } else if (!is.null(path)) {
        if ("path" %in% names(file) && !is.na(file$path)) {
          filePath <- paste(dirname(file$path), basename(file$path), sep = "/")
          comparePath <- paste(dirname(path), basename(path), sep = "/")
          if (filePath != comparePath) {
            return(file)
          }
        } else {
          return(file)
        }
      }
    })
    stepData$localFiles <- list(localFiles)
    env$stepDf <- stepData
    invisible(env)
  }

  env$changeStepRemoteFile <- function(name, asLink = NULL, newName = NULL, newIdent = NULL) {
    stepData <- env$stepDf
    remoteFiles <- stepData$remoteFiles[[1]]
    remoteFiles <- byNotEmptyAsDf(remoteFiles, function(file) {
      if ("name" %in% names(file)) {
        fileName <- file$name
        if (fileName != name) {
          return(file)
        } else {
          return(env$replaceFileFields(file, asLink, newName, newIdent))
        }
      } else if ("ident" %in% names(file)) {
        fileName <- loadResource(file$ident)$name
        if (fileName != name) {
          return(file)
        } else {
          return(env$replaceFileFields(file, asLink, newName, newIdent))
        }
      }
    })
    stepData$remoteFiles <- list(remoteFiles)
    env$stepDf <- stepData
    invisible(env)
  }

  env$changeStepRemoteFileDf <- function(name, df) {
    stepData <- env$stepDf
    remoteFiles <- stepData$remoteFiles[[1]]
    remoteFiles <- byNotEmptyAsDf(remoteFiles, function(file) {
      if ("name" %in% names(file)) {
        fileName <- file$name
        if (fileName != name) {
          return(file)
        } else {
          df
        }
      }
    })
    stepData$remoteFiles <- list(remoteFiles)
    env$stepDf <- stepData
    invisible(env)
  }

  env$changeStepProcessDf <- function(stepHandle, name, df) {
    stepData <- env$stepDf
    processes <- stepData$processes[[1]]
    processes <- byNotEmptyAsDf(processes, function(proc) {
      if ("name" %in% names(proc)) {
        if (proc$name != name) {
          return(proc)
        } else {
          df
        }
      }
    })
    stepData$processes <- list(processes)
    env$stepDf <- stepData
    invisible(env)
  }

  env$replaceFileFields <- function(fileDf, asLink, newName, newIdent) {
    if (!is.null(asLink)) {
      fileDf$asLink <- asLink
    }
    if (!is.null(newName)) {
      fileDf$name <- newName
    }
    if (!is.null(newIdent)) {
      fileDf$ident <- newIdent
    }
    return(fileDf)
  }

  env$addStepGridArgument <- function(argumentName, argumentValue, process = "Main") {
    gridList <- data.frame(argumentName = argumentName, argumentValue = argumentValue)
    env$addProcessValue("gridArguments", gridList, process)
    invisible(env)
  }

  env$removeStepGridArguments <- function(process = "Main") {
    env$removeProcessValue("gridArguments", process)
    invisible(env)
  }

  env$addStepLineage <- function(stepHandle, lineageHandle) {
    env$addStepValue(stepHandle, "lineage", lineageHandle)
    env$addStepValue(lineageHandle, "usage", stepHandle)
    invisible(env)
  }

  env$addStepLocalFile <- function(path, name = NULL, variableName = NULL, variableProcess = "Main") {
    fileList <- data.frame(stepHandle = stepHandle)
    fileList["name"] <- name
    fileList["variableName"] <- variableName
    fileList["variableProcess"] <- variableProcess
    fileList["path"] <- path
    env$addStepValue(stepHandle, "localFiles", fileList)
    invisible(env)
  }

  env$completeToolPresets <- function(processName = NULL, overwrite = FALSE) {
    if (is.null(processName)) {
      processNames <- env$stepDf$processes[[1]]$name
      if (length(processNames) == 0) {
        stop("cannot complete a step without process")
      }
      processes <- NULL
      for (i in 1:length(processNames)) {
        processDf <- env$completeToolPresets(processNames[i])
        #processes<-plyr::rbind.fill(processes,processDf)
      }
    } else {
      usedTool <- env$getToolForProcess(processName)
      parameters <- usedTool$parameters[[1]]
      commandLine <- parameters[parameters$name == "Tool Arguments", ]$value
      env$setStepCommandLine(commandLine, append = FALSE, process = processName)
    }
    invisible(env)
  }

  env$getToolForProcess <- function(processName) {
    process <- dplyr::filter(env$stepDf$processes[[1]], name == processName)
    toolInstances <- improveR:::getToolInstances()
    fullToolName <- paste(process$toolLabel, process$toolInstance, process$runserverLabel)
    toolNames <- ls(envir = toolInstances)
    toolNames <- toolNames[grepl(pattern = fullToolName, x = toolNames)]
    if (length(toolNames) != 1) {
      improveR::log_error(fullToolName, "not unique or existing")
      stop("tool error")
    }
    usedTool <- toolInstances[[toolNames]]
    return(usedTool)
  }

  env$prepareProcess <- function(processName) {

    resolveRelativeFiles <- function(pF) {
      if ("sourceInventoryPath" %in% names(pF)) {
        pF <- improveR:::byNotEmptyAsDf(pF,function(processFile) {
          if (!is.na(processFile$sourceInventoryPath)) {
            processFile$ident<- improveR::loadResource(processFile$sourceInventoryPath,from=env$workflow$stepTemplates[[processFile$sourceStep]]$stepDf$entityId)$entityId
          }
          return(processFile)
        })
      }
      return(pF)
    }

    process <- dplyr::filter(env$stepDf$processes[[1]], name == processName)
    usedTool <- env$getToolForProcess(processName)
    process$runserverId <- usedTool$runserverId
    process$runserverToolId <- usedTool$id
    if (is.null(process$toolArgs)) {
      process$toolArgs <- process$commandline
    }
    process <- dplyr::select(process, name, main, runserverId, runserverToolId, toolArgs)
    subFolderNameMapping <- list()
    remoteFiles <- env$stepDf$remoteFiles[[1]]
    processFiles <- dplyr::filter(remoteFiles, variableProcess == processName & !is.na(variableName))
    processFiles <- resolveRelativeFiles(processFiles)
    if (nrow(processFiles) > 0) {





      variables <- NULL
      resources <- NULL
      for (i in 1:nrow(processFiles)) {
        processFile <- processFiles[i, ]
        variable <- data.frame(type = "processVariable",
          name = processFile$variableName,
          position = i,
          variableType = "fileRef",
          stringsAsFactors = FALSE
        )
        variables <- plyr::rbind.fill(variables, variable)
        processResource <- improveR::loadResource(processFile$ident)
        resource <- data.frame(sourceResourceId = processResource$resourceId,
          variableName = processFile$variableName,
          stringsAsFactors = FALSE
        )
        if ("name" %in% names(processFile) && !is.null(processFile$name) && !is.na(processFile$name)) {
          targetName <- processFile$name
          if (startsWith(targetName, "./")) {
            targetName <- substr(targetName, 3, nchar(targetName))
          }
          if (grepl("/", targetName, fixed = TRUE)) {
            subFolderNameMapping[[processResource$resourceId]] <- targetName
            targetName <- processResource$resourceId
          }
          resource$targetName <- targetName
        }
        if (!processFile$asLink) {
          resource$operation = "COPY"
        }
        resources <- plyr::rbind.fill(resources, resource)
      }
      process$resources <- list(resources)
      process$variables <- list(variables)
    }
    if (processName == "Main") {
      processFiles <- dplyr::filter(remoteFiles, is.null(variableName) | is.na(variableName))
      processFiles <- resolveRelativeFiles(processFiles)

      if (nrow(processFiles) > 0) {
        resources <- process$resources[[1]]
        for (i in 1:nrow(processFiles)) {
          processFile <- processFiles[i, ]
          processResource <- improveR::loadResource(processFile$ident)
          resource <- data.frame(sourceResourceId = processResource$resourceId,
            stringsAsFactors = FALSE
          )
          if ("name" %in% names(processFile) && !is.null(processFile$name) && !is.na(processFile$name)) {
            targetName <- processFile$name
            if (startsWith(targetName, "./")) {
              targetName <- substr(targetName, 3, nchar(targetName))
            }
            if (grepl("/", targetName, fixed = TRUE)) {
              subFolderNameMapping[[processResource$resourceId]] <- targetName
              targetName <- processResource$resourceId
            }
            resource$targetName <- targetName
          }
          if (!processFile$asLink) {
            resource$operation = "COPY"
          }
          resources <- plyr::rbind.fill(resources, resource)
        }
        process$resources <- list(resources)
      }
      process$resources <- list(dplyr::distinct(process$resources[[1]],targetName,.keep_all = T))
    }
    if (length(subFolderNameMapping) > 0) {
      process$subFolderNameMapping <- subFolderNameMapping
    }
    return(process)
  }

  env$moveSubFolderNameMapping <- function(subFolderNameMapping, newStep) {
    if (length(subFolderNameMapping) > 0) {
      moveResources <- names(subFolderNameMapping)
      for (i in 1:length(moveResources)) {
        moveResource <- moveResources[i]
        fileName <- subFolderNameMapping[[moveResource]]
        pathParts <- strsplit(x = fileName, split = "/", fixed = TRUE)[[1]]
        createTarget <- NULL
        if (length(pathParts) != 2) {
          improveR::log_error("maximum folder depth allowed is 1, by filename in realise step")
          improveR::log_error(fileName)
          stop()
        }
        folderName <- pathParts[1]
        fileName <- pathParts[2]
        children <- loadChildResources(newStep)
        folder <- children[children$name == folderName, ]
        if (nrow(folder) == 1 && folder$nodeType != "Folder") {
          improveR::log_error(folderName)
          improveR::log_error("already exists but not as folder")
          stop()
        }
        if (nrow(folder) == 1) {
          createTarget <- folder
        } else {
          createTarget <- createFolder(newStep, folderName = folderName)
        }
        improveR::move(file.path(newStep$path, moveResource), createTarget, fileName)
      }
    }
  }

  env$getStepEnv <- function() {
    if (is.null(env$step)) {
      env$step <- improveR::getStep(env$getStepResource())
    }
    return(env$step)
  }

  env$getStepValue <- function(key) {
    stepList <- env$stepDf
    if (key %in% names(stepList)) {
      return(as.character(stepList[key]))
    }
    return(NULL)
  }


  env$realise <- function(force = TRUE, run = TRUE,workflow=NULL) {
    improveEditable()
    breakPoint <- env$getStepValue("breakpoint")
    reuse <- env$getStepValue("reuse")
    if (!is.null(breakPoint) && breakPoint == TRUE) {
      run <- FALSE
    }
    if (!is.null(reuse) && reuse == TRUE) {
      force <- FALSE
    }
    newStep <- NULL
    if (!force) {
      logging::logdebug("check step equality")
      # newStep <- existsInTargetTree(handle)
      # if (!is.null(newStep)) {
      #   env$setStepValue(handle, "entityId", as.character(newStep$entityId))
      #   return(env)
      # }
    }
    newStep <- .template_private$create()
    env$setStepValue("entityId", as.character(newStep$entityId))
    tree <- improveR::loadResource(newStep$parentId)
    env$setStepValue("treeIdent", tree$resourceId)
    env$setStepValue("treeName", tree$name)
    env$setStepValue("treePath", dirname(tree$path))
    if (env$getStepState() == "INITIAL" && run) {
      env$run()
    }
    return(getStep(newStep$entityId,workflow = workflow))
  }

  env$run <- function() {
    improveEditable()
    newStep <- env$getStepResource()
    result <- improveR::authenticatedREST("resources/{stepId}/run",
      urlParams = list(stepId = newStep$resourceId),
      restType = "POST"
    )
    invisible(env)
  }

  env$finishRun <- function(runserverName = NULL, runserverToolName = NULL) {
    toolId <- NULL
    running <- TRUE
    while (running) {
      step <- .template_private$getStepWithoutCache()
      state <- step$runStatus
      if (is.null(toolId)) {
        if (state == "FINISHED") {
          running <- FALSE
        } else {
          Sys.sleep(2)
        }
      }
    }
    invisible(env)
  }

  env$getStepResource <- function() {
    entityId <- .template_private$getStepValue("entityId")
    if (!is.null(entityId)) {
      return(improveR::loadResource(entityId))
    }
    return(NULL)
  }

  env$getStepState <- function() {
    entityId <- .template_private$getStepValue("entityId")
    step <- improveR:::internalLoadResourceFromServer(entityId)
    return(step$runStatus)
  }

  # Attach all other public methods as needed...

  env
}

