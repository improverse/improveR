
setStepValue <- function(key,value) {
  stepList <- this$stepDf
  stepList[key]<-value
  this$stepDf <- stepList
  return(this)
}

setProcessValue <- function(processName,key,value){
  stepData <- this$stepDf
  processes <- stepData$processes[[1]]
  position<-1
  if (!is.null(processes) && nrow(processes)>0) {
    position<-max(processes$position)+1
  }
  if (!(processName %in% processes$name)) {
    newProcess <- data.frame(
      name=processName,
      selected=T,
      main=F,
      processType="post",
      position=position,
      stringsAsFactors = F)
    if (processName=="Main") {
      newProcess$processType="main"
      newProcess$main=T
    }
    processes <- plyr::rbind.fill(newProcess,processes)
  }
  processes <- byNotEmptyAsDf(processes,function(process) {
    if (process$name==processName) {
      process[[key]]<-value
    }
    return(process)
  })
  stepData$processes <- list(processes)
  this$stepDf <- stepData
  return(this)
}

getStepValue <- function(key) {
  stepList <- this$stepDf
  if (key %in% names(stepList)) {
    return(as.character(stepList[key]))
  }
  return(NULL)
}

removeStepValue <- function(key) {
  stepList <- this$stepDf
  if (key %in% names(stepList)) {
    stepList[key]<-NULL
  }
  this$stepDf <- stepList
  return(this)
}

addStepValue <- function(key,value) {
  stepList <- this$stepDf
  valueList <- NULL
  if (key %in% names(stepList)) {
    valueList <- stepList[key][[1]][[1]]
  }
  if (typeof(value)=="character") {
    if (is.null(valueList)) {
      valueList <- value
    } else {
      valueList <- paste(valueList,value,sep=",")
    }
    stepList[key]<-valueList
  } else if (is.data.frame(value)) {
    if (is.null(valueList)) {
      valueList <- value
    } else {
      valueList <- plyr::rbind.fill(valueList,value)
    }
    stepList[key]<-tidyr::nest(valueList,data = tidyr::everything())
  } else {
    logging::logwarn(paste0(
      "only character or dataframe allowed in stephandle for key: ",
      key
    ))
  }

  this$stepDf <- stepList
  return(this)
}




removeProcessValue <- function(key,processName) {
  stepList <- this$stepDf
  processes <- byNotEmptyAsDf(processes,function(process) {
    if (process$name==processName) {
      if (key %in% names(process)) {
        process[key]<-NULL
      }
    }
    return(process)
  })
  this$stepDf <- stepList
  return(this)
}

addProcessValue <- function(key,value,processName) {
  stepList <- this$stepDf

  processes <- byNotEmptyAsDf(processes,function(process) {
    if (process$name==processName) {
      valueList <- NULL
      if (key %in% names(process)) {
        valueList <- process[key][[1]][[1]]
      }
      if (typeof(value)=="character") {
        if (is.null(valueList)) {
          valueList <- value
        } else {
          valueList <- paste(valueList,value,sep=",")
        }
        process[key]<-valueList
      } else if (is.data.frame(value)) {
        if (is.null(valueList)) {
          valueList <- value
        } else {
          valueList <- plyr::rbind.fill(valueList,value)
        }
        process[key]<-tidyr::nest(valueList,data = tidyr::everything())
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
  this$stepDf <- stepList
  return(this)
}


#' sets the containing tree
#'
#' @param treeIdent ident of the containing tree
#'
setStepTree <- function(treeIdent) {
  tree <- loadResource(treeIdent)
  this$setStepValue("treeIdent",tree$resourceId)
}

#' sets the improveR workflow
#'
#' @param workflowHandle handle of the workflow
#'
setStepWorkflow <- function(workflowHandle) {
  this$setStepValue("workflowHandle",workflowHandle)
}

#' sets the parent step
#'
#' @param parentIdent ident of the containing tree
#' @param inheritFromParent if input files and settings should be taken over from the parent
#'
setStepParent <- function(parentIdent,inheritFromParent=F) {
  if (!is.null(parentIdent)) {
    parent <- loadResource(parentIdent)
    this$setStepValue("parentIdent",parent$resourceId)
    this$setStepValue("inheritFromParent",inheritFromParent)
  } else     {
    this$setStepValue("parentIdent",NULL)
    this$setStepValue("inheritFromParent",NULL)
  }
}

#' sets a step breakpoint, the step run is not started even if realiseStep is called with run=T
#'
#' @param breakpoint boolean, true inhibits step execution in workflows/realiseStep
#'
setStepBreakpoint <- function(breakpoint=T) {
  this$setStepValue("breakpoint",breakpoint)
}

#' sets a step reusage, the step is not created if an identical finished step already exists in the tree
#'
#' @param reuse boolean, true inhibits recreation of identical in workflows/realiseStep
#'
setStepReuse <- function(reuse=T) {
  this$setStepValue("reuse",reuse)
}

#' sets the finish runserver and runserver tool by name, finishStep only stops if the step is executed and finished with this combination
#'
#' @param runserverName name of the runserver
#' @param runserverTool name of the runserverTool
#'
setStepFinishCondition <- function(runserverName,runserverTool) {
  this$setStepValue("finishRunserverName",runserverName)
  this$setStepValue("finishRunserverTool",runserverTool)
}

#' sets the runserver by name
#'
#' @param runserverName name of the runserver
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
setStepRunserverName <- function(runserverName,process="Main") {
  this$setProcessValue(process,"runserverName",runserverName)
}

#' sets the name of the step
#'
#' @param stepName name of the step
#'
#' @export
setStepName <- function(stepName) {
  this$setStepValue("stepName",stepName)
}

#' sets the description of the step
#'
#' @param description description of the step
#'
#' @export
setStepDescription <- function(description) {
  this$setStepValue("description",description)
}

#' sets the rationale of the step
#'
#' @param rationale rationale of the step
#'
#' @export
setStepRationale <- function(rationale) {
  this$setStepValue("rationale",rationale)
}


#' sets the command line
#'
#' @param commandline the new command line or command line appendix
#' @param append if the complete command line is replaced or this is appended defaults to TRUE
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
setStepCommandLine <- function(commandline,append=T,process="Main") {
  this$setProcessValue(process,"commandline",commandline)
  this$setProcessValue(process,"appendCommandline",append)
}

#' sets the runserver tool by name
#'
#' @param runserverToolName name of the runserver tool
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
setStepRunserverToolName <- function(runserverToolName,process="Main") {
  this$setProcessValue(process,"runserverToolName",runserverToolName)
}

#' sets the tool by name
#'
#' @param toolName name of the  tool
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
setStepToolName <- function(toolName,process="Main") {
  this$setProcessValue(process,"toolName",toolName)
}

#' adds a new remote file to the step
#'
#' @param ident the ident of the file in the repository
#' @param name leave empty if you want to use the same name as the used file
#' @param asLink, if file should be copied or linked, boolean, defaults to link
#' @param variableName the name of the variable the file should be bound to, optional
#' @param sourceHandle stepHandle if ident is relative to another step in the workflow
#' @param sourceName name if ident is relative to another step in the workflow
#' @param variableProcess if the file is bound to a variable, the process name the variable belongs to
#'
addStepRemoteFile <- function(ident=NULL,name=NULL,asLink=T,variableName=NULL,sourceHandle=NULL,sourceName=NULL,variableProcess="Main") {


  ###TODO incorporate sourceHandles and specific versions
  fileList <- data.frame(stepHandle=this$stepDf$handle,stringsAsFactors = F)
  if (!is.null(ident)) {
    resource <- loadResource(ident)
    if (resource$nodeType=="File") {
      fileList["ident"]<-resource$entityId
      fileList["filehash"]<- resource$fileHash
    } else if (resource$nodeType=="Link"){
      fileList["ident"]<-resource$targetEntityId
      fileList["version"] <- resource$targetRevisionId
      target <- loadResource(resource$targetEntityId)
      fileList["filehash"]<- target$fileHash
      fileList["maxVersion"]<-target$revisionId
    } else {
      logging::logwarn("Only files or resources can be added to an inventory")
      logging::logwarn(ident)
      logging::logwarn(stepHandle)
      return(this)
    }
  }
  fileList["asLink"]<-asLink
  fileList["name"]<-name
  fileList["variableName"]<-variableName
  fileList["variableProcess"]<-variableProcess
  if (!is.null(sourceHandle)) {
    fileList["sourceHandle"]<-sourceHandle
    fileList["sourceName"]<-sourceName
    this$addStepValue(stepHandle,"dependencies",sourceHandle)
    #TODO dependency and usage
    #this$addStepValue(sourceHandle,"usage",stepHandle)
  }
  this$addStepValue("remoteFiles",fileList)
}

#' adds a new external link to the step
#'
#' @param name name of the link
#' @param url, the url the link targets
#'

addExtLink <- function(name,url) {
  extLinkList <- data.frame(stepHandle=stepHandle,stringsAsFactors = F)
  extLinkList$name<-name
  extLinkList$url <- url
  this$addStepValue("extLinks",extLinkList)
}


#' removes a remote file from the step
#'
#' @param name the name of the file in the inventory
#' @param ident the ident of the file in the inventory
#'
removeStepRemoteFile <- function(name=NULL,ident=NULL) {
  stepData <- this$stepDf
  remoteFiles <- stepData$remoteFiles[[1]]
  remoteFiles <- byNotEmptyAsDf(remoteFiles,function(file) {
    if (!is.null(name)) {
      if ("name" %in% names(file) && !is.na(file$name)) {
        fileName <- file$name
        if (fileName!=name) {
          return(file)
        }
      } else if ("ident" %in% names(file)) {
        fileName <- loadResource(file$ident)$name
        if (fileName!=name) {
          return(file)
        }
      }
    } else if (!is.null(ident)) {
      if ("ident" %in% names(file) && !is.na(file$ident)) {
        fileEntityId <- loadResource(file$ident)$entityId
        compareEntityId <- loadResource(ident)$entityId
        if (fileEntityId!=compareEntityId) {
          return(file)
        }
      } else {
        return(file)
      }
    }


  })
  stepData$remoteFiles <- list(remoteFiles)
  this$stepDf <- stepData
  return(this)
}


#' removes a local file from the step
#'
#' @param stepHandle id of the prepared step
#' @param name the name of the file in the inventory
#' @param path the path of the file in the filesystem
#'
#' @export
removeStepLocalFile <- function(stepHandle,name=NULL,path=NULL) {
  stepData <- this$stepDf
  localFiles <- stepData$localFiles[[1]]
  localFiles <- byNotEmptyAsDf(localFiles,function(file) {
    if (!is.null(name)) {
      if ("name" %in% names(file) && !is.na(file$name)) {
        fileName <- file$name
        if (fileName!=name) {
          return(file)
        }
      } else if ("path" %in% names(file) && !is.na(file$path)) {
        fileName <- basename(file$path)
        if (fileName!=name) {
          return(file)
        }
      }
    } else if (!is.null(path)) {
      if ("path" %in% names(file) && !is.na(file$path)) {
        filePath <- paste(dirname(file$path),basename(file$path),sep="/")
        comparePath <- paste(dirname(path),basename(path),sep="/")
        if (filePath!=comparePath) {
          return(file)
        }
      } else {
        return(file)
      }
    }


  })
  stepData$localFiles <- list(localFiles)
  this$stepDf <- stepData
  return(this)
}

#' changes a remote file in the step
#'
#' @param name the name of the file in the inventory
#' @param asLink if the file is added as link, boolean, not changed if NULL
#' @param newName the new name of the file, not changed if NULL
#' @param newIdent the new ident of the file, not changed if NULL
#'
changeStepRemoteFile <- function(name,asLink=NULL,newName=NULL,newIdent=NULL) {
  stepData <- this$stepDf
  remoteFiles <- stepData$remoteFiles[[1]]
  remoteFiles <- byNotEmptyAsDf(remoteFiles,function(file) {
    if ("name" %in% names(file)) {
      fileName <- file$name
      if (fileName!=name) {
        return(file)
      } else {
        return(replaceFileFields(file,asLink,newName,newIdent))
      }
    } else if ("ident" %in% names(file)) {
      fileName <- loadResource(file$ident)$name
      if (fileName!=name) {
        return(file)
      } else {
        return(replaceFileFields(file,asLink,newName,newIdent))
      }
    }


  })
  stepData$remoteFiles <- list(remoteFiles)
  this$stepDf <- stepData
  return(this)
}

changeStepRemoteFileDf <- function(name,df) {
  stepData <- this$stepDf
  remoteFiles <- stepData$remoteFiles[[1]]
  remoteFiles <- byNotEmptyAsDf(remoteFiles,function(file) {
    if ("name" %in% names(file)) {
      fileName <- file$name
      if (fileName!=name) {
        return(file)
      } else {
        df
      }
    }
  })
  stepData$remoteFiles <- list(remoteFiles)
  this$stepDf <- stepData
  return(this)
}

changeStepProcessDf <- function(stepHandle,name,df) {
  stepData <- this$stepDf
  processes <- stepData$processes[[1]]
  processes <- byNotEmptyAsDf(processes,function(proc) {
    if ("name" %in% names(proc)) {
      if (proc$name!=name) {
        return(proc)
      } else {
        df
      }
    }
  })
  stepData$processes <- list(processes)
  this$stepDf<-stepData
  return(this)
}

replaceFileFields <- function(fileDf,asLink,newName,newIdent) {
  if (!is.null(asLink)) {
    fileDf$asLink <- asLink
  }
  if (!is.null(newName)) {
    fileDf$name <- newName
  }
  if (!is.null(newIdent)) {
    fileDf$ident <-newIdent
  }
  return(fileDf)
}

#' adds a new grid argument to the step
#'
#' @param argumentName the ident of the file in the repository
#' @param argumentValue leave empty if you want to use the same name as the used file
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
addStepGridArgument <- function(argumentName,argumentValue,process="Main") {
  gridList <- data.frame(argumentName=argumentName,
                         argumentValue=argumentValue)
  this$addProcessValue("gridArguments",gridList,process)
}

#' removes all grid arguments from the step
#'
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
removeStepGridArguments <- function(process="Main") {
  this$removeProcessValue(stepHandle,"gridArguments",process)
  return(this)
}

#TODO new lineage usage
#' adds a new lineage and implicitly usage to the step
#'
#' @param stepHandle id of the prepared step
#' @param lineageHandle the ident of the file in the repository
#'
#' @export
addStepLineage <- function(stepHandle,lineageHandle) {
  addStepValue(stepHandle,"lineage",lineageHandle)
  addStepValue(lineageHandle,"usage",stepHandle)
}

#' adds a new local file to the step
#'
#' @param path local path to the file
#' @param name leave empty if you want to use the same name as the used file
#' @param variableName the name of the variable the file should be bound to, optional
#' @param variableProcess the name of the process for the variable the file should be bound to, optional
#'
addStepLocalFile <- function(path,name=NULL,variableName=NULL,variableProcess="Main") {
  fileList <- data.frame(stepHandle=stepHandle)
  fileList["name"]<-name
  fileList["variableName"]<-variableName
  fileList["variableProcess"]<-variableProcess
  fileList["path"]<-path
  this$addStepValue(stepHandle,"localFiles",fileList)
}



#' retrieveMainProcess
#' retrieves the main process data frame of a step by handle
#'
#'
#' @export

retrieveMainProcess <- function() {
  stepData <- this$stepDf
  processes <- stepData$processes[[1]]
  if (nrow(processes)>0 && ("main" %in% processes$processType)) {
    return(processes[processes$processType=="main",])
  }
  return(NULL)
}



#' realise
#'
#' @param handle check if a step with the same configuration already exists in the tree
#' @param force, force creates a new step even if an equivalent step already exists
#' @param run, automatically run the step after creation (T is overridden by the setStepBreakpoint)
#' @references ics1140
#' @export
realise <-function(stepEnv,force=T,run=T) {
  improveEditable()
  breakPoint <- stepEnv$getStepValue("breakpoint")
  reuse <- stepEnv$getStepValue("reuse")
  if (!is.null(breakPoint) && breakPoint==T) {
    run <-F
  }
  if (!is.null(reuse) && reuse==T) {
    force <-F
  }
  newStep <- NULL
  #if (!force) {
  #  logging::logdebug("check step equality")
    #TODO still needed?
    #newStep <- existsInTargetTree(handle)
    #if (!is.null(newStep)) {
    #  setStepValue(handle,"entityId",as.character(newStep$entityId))
    #  return(handle)
    #}
  #}
  newStep <- createPreparedStep(handle)
  setStepValue(handle,"entityId",as.character(newStep$entityId))
  tree <- loadResource(newStep$parentId)
  setStepValue(handle,"treeIdent",tree$resourceId)
  setStepValue(handle,"treeName",tree$name)
  setStepValue(handle,"treePath",dirname(tree$path))
  if (getStepState(handle)=="INITIAL" && run) {
    runStep(handle)
  }
  return(handle)
}


#' creates the prepared step in the repository
#'
#' @param stepHandle id of the prepared step
#' @references ics1140
#' @export
createPreparedStep <- function(stepEnv) {
  improveEditable()
  logging::logdebug("createPreparedStep")
  timing("createPreparedStep")
  prepStep <- this$stepDf
  mainPrep <- this$retrieveMainProcess()



  toolInstances <- improveR:::getToolInstances()
  #TODO missing category
  fullToolName <- paste(mainPrep$toolName,mainPrep$runserverToolName,mainPrep$runserverName)
  toolNames <- ls(envir=toolInstances)
  toolNames <- toolNames[grepl(pattern = fullToolName,x = toolNames)]
  if (length(toolNames)!=1) {
    improveR::log_error(fullToolName,"not unique or existing")
    stop("tool error")
  }

  mainTool <- toolInstances[[toolNames]]

  mainPrep$runServerId <- mainTool$runserverId
  mainPrep$runserverToolId <- mainTool$id
  mainPrep$

  treeIdent <- prepStep$treeIdent
  if (is.null(treeIdent) || is.na(treeIdent)) {
    tryCatch( {
      targetResource <- loadResource(prepStep$treePath)
      treeIdent <- createAnalysisTree(targetResource,treeName = prepStep$treeName)$resourceId
    },error=function(e) {
      log_error(e)
      log_error("treeIdent or treePath and treeName need to be specified in order to create the step")
      stop("could not create step")
    })
  }

  runserver <- loadRunserver(mainPrep$runserverName)
  tools  <- loadToolsForRunserver(runserver$id)
  tool <- loadToolForRunserver(runserver$id,mainPrep$toolName,mainPrep$runserverToolName)
  newStep <- NULL
  if (!is.null(prepStep$entityId)) {
    newStep <- loadResource(prepStep$entityId)
  } else {
    if (is.null(prepStep$parentIdent)) {
      newStep <- createStep(treeIdent,NULL,toolId=tool$toolId)
    } else {
      parent <- loadResource(prepStep$parentIdent)
      if (prepStep$inheritFromParent) {
        newStep <- createStep(treeIdent,prepStep$parentIdent,toolId=parent$toolId)
      } else {
        toolId <- as.character(tools[tools$toolId!=parent$toolId,]$toolId[1])
        newStep <- createStep(treeIdent,prepStep$parentIdent,toolId=toolId)
      }
    }
  }
  logging::logdebug("created")

  timing("created")


  processes <- prepStep$processes[[1]]

  if (nrow(processes)>0) {
    processes <- processes[order(processes$position),]
    for (i in 1:nrow(processes)) {
      process <- processes[i,]

      runserver <- loadRunserver(process$runserverName)
      tools  <- loadToolsForRunserver(runserver$id)
      tool <- loadToolForRunserver(runserver$id,process$toolName,process$runserverToolName)

      toolArguments <- NULL
      if (!is.null(process$commandline)) {
        toolArguments <- ""
        if (process$appendCommandline) {
          toolArguments <- paste0(repoProcess$toolArgs,"\r\n")
        }
        toolArguments <- paste0(toolArguments,process$commandline)
      }

      repoProcess <- NULL
      if (process$processType=="main") {
        repoProcess <- getMainProcess(newStep$resourceId)
        setProcessVariables(newStep$resourceId,
                            repoProcess$id,
                            runserverId=runserver$id,
                            toolId=tool$toolId,
                            runserverToolId=tool$id,
                            gridTool=!is.na(tool$gridProvider),
                            toolArguments=toolArguments,
                            position = process$position,
                            processType = process$processType,
                            name=process$name,
                            mainProcess=process$main,
                            toolDeletePatterns = process$toolDeletePatterns,
                            toolStreamablePatterns = process$toolStreamablePatterns,
                            toolIgnorePatterns = process$toolIgnorePatterns,
                            toolBrowserUrl = process$toolBrowserUrl,
                            selected = process$selected,
                            parentProcessId = process$parentProcessId
        )
      } else {
        repoProcess <- createProcess(
          newStep$resourceId,
          runserverId=runserver$id,
          toolId=tool$toolId,
          runserverToolId=tool$id,
          gridTool=!is.na(tool$gridProvider),
          toolArguments=toolArguments,
          position = process$position,
          processType = process$processType,
          name=process$name,
          mainProcess=process$main,
          toolDeletePatterns = process$toolDeletePatterns,
          toolStreamablePatterns = process$toolStreamablePatterns,
          toolIgnorePatterns = process$toolIgnorePatterns,
          toolBrowserUrl = process$toolBrowserUrl,
          selected = process$selected,
          parentProcessId = process$parentProcessId
        )
      }
      gridArguments <- process$gridArguments[[1]]
      byNotEmpty(gridArguments,function(gridArgument) {
        setGridArgument(repoProcess$id,gridArgument$argumentName,gridArgument$argumentValue,update=T)
      })

      logging::logdebug("grid arguments set")

      timing("grid arguments set")

    }
  }
  logging::logdebug("process variables set")
  timing("process variables set")
  repoProcesses <- updateProcessesForStep(newStep$resourceId)

  if (!is.null(prepStep$stepName)) {
    stepName <- prepStep$stepName
    testResource <- loadResource(paste0("./",stepName),from = newStep$parentId)
    if (!is.null(testResource)) {
      nameNotCleared <- T
      counter <- 1
      while (nameNotCleared) {
        stepName <- paste(prepStep$stepName,counter)
        counter <- counter+1
        testResource <- loadResource(paste0("./",stepName),newStep$parentId)
        nameNotCleared <- !is.null(testResource)
      }
    }
    setStepValue(stepHandle,"stepName",stepName)
    move(newStep,newStep$parentId,targetName = stepName)
  }

  if (!is.null(prepStep$description)) {
    changeStepDescription(newStep,description = prepStep$description)
  }

  if (!is.null(prepStep$rationale)) {
    changeStepRationale(newStep,rationale = prepStep$rationale)
  }

  logging::logdebug("step names and descriptions set")
  timing("step names and descriptions set")


  #a bit hacky
  #unloadResource(newStep)
  #unloadChildResources(newStep)
  #unloadFullChildResources(newStep)
  #unloadChildResources(newStep$parentId)
  #unloadFullChildResources(newStep$parentId)
  newStep <- updateResource(newStep)

  timing("all unloads")

  remoteFiles <- prepStep$remoteFiles[[1]]
  byNotEmpty(remoteFiles,function(filePrep) {
    addFileToStep(newStep,filePrep,F)
  })

  logging::logdebug("remote files set")
  timing("remote files set")

  localFiles <- prepStep$localFiles[[1]]
  byNotEmpty(localFiles,function(filePrep) {
    addFileToStep(newStep,filePrep,T)
  })

  extLinks <- prepStep$extLinks[[1]]
  byNotEmpty(extLinks,function(filePrep) {
    addExtLinkToStep(newStep,filePrep)
  })

  logging::logdebug("local files set")
  timing("local files set")


  #unloadResource(newStep)
  #unloadChildResources(newStep)
  #unloadFullChildResources(newStep)
  #unloadChildResources(newStep$parentId)
  #unloadFullChildResources(newStep$parentId)
  newStep <- updateResource(newStep)

  logging::logdebug("reloaded")
  timing("reloaded")
  return(newStep)
}




