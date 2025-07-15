
setStepValue <- function(key,value) {
  stepList <- this$stepDf
  stepList[key]<-value
  this$stepDf <- stepList
  invisible(this)
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
  invisible(this)
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
  invisible(this)
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
  invisible(this)
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
  invisible(this)
}


#' sets the containing tree
#'
#' @param treeIdent ident of the containing tree
#'
setStepTree <- function(treeIdent) {
  tree <- loadResource(treeIdent)
  this$setStepValue("treeIdent",tree$resourceId)
  invisible(this)
}

#' sets the improveR workflow
#'
#' @param workflowHandle handle of the workflow
#'
setStepWorkflow <- function(workflowHandle) {
  this$setStepValue("workflowHandle",workflowHandle)
  invisible(this)
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
  invisible(this)
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
#' @param runserverLabel name of the runserver
#' @param runserverTool name of the runserverTool
#'
setStepFinishCondition <- function(runserverLabel,runserverTool) {
  this$setStepValue("finishRunserverLabel",runserverLabel)
  this$setStepValue("finishRunserverTool",runserverTool)
}

#' sets the runserver label
#'
#' @param runserverLabel name of the runserver
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
setStepRunserverLabel <- function(runserverLabel,process="Main") {
  this$setProcessValue(process,"runserverLabel",runserverLabel)
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

#' sets the toolInstance
#'
#' @param toolInstance name of the runserver tool
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
setStepToolInstance <- function(toolInstance,process="Main") {
  this$setProcessValue(process,"toolInstance",toolInstance)
}

#' sets the tool label
#'
#' @param toolLabel name of the  tool
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
setStepToolLabel <- function(toolLabel,process="Main") {
  this$setProcessValue(process,"toolLabel",toolLabel)
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



completeToolPresets <- function(processName=NULL,overwrite=F) {
  if (is.null(processName)) {
    processNames <- this$stepDf$processes[[1]]$name

    if (length(processNames)==0) {
      stop("cannot complete a step without process")
    }

    processes <- NULL
    for (i in 1:length(processNames)) {
      processDf <- completeToolPresets(processNames[i])
      #processes<-plyr::rbind.fill(processes,processDf)
    }
  } else {
    usedTool <- this$getToolForProcess(processName)
    parameters <- usedTool$parameters[[1]]
    #TODO also take other arguments
    #check if overwrite
    commandLine <- parameters[parameters$name=="Tool Arguments",]$value
    this$setStepCommandLine(commandLine,append = F,process = processName)
    #gridTools <- usedTool$gridArguments[[1]]
  }
}

getToolForProcess <- function(processName) {
  process <- dplyr::filter(this$stepDf$processes[[1]],name==processName)
  toolInstances <- improveR:::getToolInstances()
  #TODO missing category
  fullToolName <- paste(process$toolLabel,process$toolInstance,process$runserverLabel)
  toolNames <- ls(envir=toolInstances)
  toolNames <- toolNames[grepl(pattern = fullToolName,x = toolNames)]
  if (length(toolNames)!=1) {
    improveR::log_error(fullToolName,"not unique or existing")
    stop("tool error")
  }

  usedTool <- toolInstances[[toolNames]]
  return(usedTool)
}

prepareProcess <- function(processName) {
  process <- dplyr::filter(this$stepDf$processes[[1]],name==processName)

  usedTool <- getToolForProcess(processName)

  process$runserverId <- usedTool$runserverId
  process$runserverToolId <- usedTool$id



  #TODO: appendCommandline
  if (is.null(process$toolArgs)) {
    process$toolArgs<-process$commandline
  }
  process <- dplyr::select(process,name,main,runserverId,runserverToolId,toolArgs)
  #filter out not needed values


  #subFolderNameMapping
  subFolderNameMapping <- list()

  remoteFiles <- this$stepDf$remoteFiles[[1]]
  processFiles <- dplyr::filter(remoteFiles,variableProcess==processName & !is.na(variableName))
  if (nrow(processFiles)>0) {
    variables <- NULL
    resources <- NULL
    for (i in 1:nrow(processFiles)) {
      processFile <- processFiles[i,]
      variable <- data.frame(type="processVariable" ,
                             name=processFile$variableName,
                             position=i,
                             variableType="fileRef",
                               stringsAsFactors = F)
      variables <- plyr::rbind.fill(variables,variable)
      processResource <- improveR::loadResource(processFile$ident)
      resource <- data.frame(sourceResourceId=processResource$resourceId,
                             variableName=processFile$variableName,
                             stringsAsFactors = F)
      if ("name" %in% names(processFile) && !is.null(processFile$name) && !is.na(processFile$name)) {
        targetName <- processFile$name
        if (startsWith(targetName,"./")) {
          targetName <- substr(targetName,3,nchar(targetName))
        }
        if (grepl("/",targetName,fixed = T)) {
          subFolderNameMapping[[processResource$resourceId]]<-targetName
          targetName <- processResource$resourceId
        }
        resource$targetName<-targetName
      }
      if (!processFile$asLink) {
        resource$operation="COPY"
      }
      resources <- plyr::rbind.fill(resources,resource)
    }
    process$resources<-list(resources)
    process$variables <- list(variables)
  }
  if (processName=="Main") {
    processFiles <- dplyr::filter(remoteFiles,is.null(variableName) | is.na(variableName) )
    if (nrow(processFiles)>0) {
      resources <- process$resources[[1]]
      for (i in 1:nrow(processFiles)) {
        processFile <- processFiles[i,]
        processResource <- improveR::loadResource(processFile$ident)
        resource <- data.frame(sourceResourceId=processResource$resourceId,
                               stringsAsFactors = F)
        if ("name" %in% names(processFile) && !is.null(processFile$name) && !is.na(processFile$name)) {
          targetName <- processFile$name
          if (startsWith(targetName,"./")) {
            targetName <- substr(targetName,3,nchar(targetName))
          }
          if (grepl("/",targetName,fixed = T)) {
            subFolderNameMapping[[processResource$resourceId]]<-targetName
            targetName <- processResource$resourceId
          }
          resource$targetName<-targetName
        }
        if (!processFile$asLink) {
          resource$operation="COPY"
        }
        resources <- plyr::rbind.fill(resources,resource)
      }
      print(nrow(resources))
      process$resources<-list(resources)
    }
  }
  if (length(subFolderNameMapping)>0) {
    process$subFolderNameMapping<-subFolderNameMapping
  }

  return(process)
}

moveSubFolderNameMapping <- function(subFolderNameMapping,newStep) {
  #applySubfolderMapping
  #subFolderNameMapping
  if (length(subFolderNameMapping)>0) {
    moveResources <- names(subFolderNameMapping)
    for (i in 1:length(moveResources)) {
      moveResource<- moveResources[i]
      fileName <- subFolderNameMapping[[moveResource]]
      pathParts <- strsplit(x=fileName,split="/",fixed=T)[[1]]
      createTarget <- NULL
      if (length(pathParts)!=2) {
        improveR::log_error("maximum folder depth allowed is 1, by filename in realise step")
        improveR::log_error(fileName)
        stop()
      }
      folderName <- pathParts[1]
      fileName<- pathParts[2]
      children <- loadChildResources(newStep)
      folder <- children[children$name==folderName,]
      if (nrow(folder)==1 && folder$nodeType!="Folder") {
        improveR::log_error(folderName)
        improveR::log_error("already exists but not as folder")
        stop()
      }
      if (nrow(folder)==1) {
        createTarget<-folder
      } else {
        createTarget <- createFolder(newStep,folderName=folderName)
      }
      improveR::move(file.path(newStep$path,moveResource),createTarget,fileName)
    }
  }
}


#' creates the prepared step in the repository
#'
#' @param stepHandle id of the prepared step
#' @references ics1140
#' @export
create <- function() {
  improveEditable()
  logging::logdebug("createPreparedStep")
  timing("createPreparedStep")

  prepStep <- this$stepDf
  treeIdent <- this$stepDf$treeIdent
  if (is.null(treeIdent) || is.na(treeIdent)) {
    tryCatch( {
      targetResource <- loadResource(this$stepDf$treePath)
      treeIdent <- createAnalysisTree(targetResource,treeName = this$stepDf$treeName)$resourceId
    },error=function(e) {
      log_error(e)
      log_error("treeIdent or treePath and treeName need to be specified in order to create the step")
      stop("could not create step")
    })
  }

  if (is.null(prepStep$description)) {prepStep$description=""}
  if (is.null(prepStep$rationale)) {prepStep$rationale=""}
  if (is.null(prepStep$comment)) {prepStep$comment=""}
  if (is.null(prepStep$keyStep)) {prepStep$keyStep=FALSE}
  if (is.null(prepStep$baseModel)) {prepStep$baseModel=FALSE}
  if (is.null(prepStep$fullModel)) {prepStep$fullModel=FALSE}
  if (is.null(prepStep$finalModel)) {prepStep$finalModel=FALSE}
  if (is.null(prepStep$referenceModel)) {prepStep$referenceModel=FALSE}

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

  if ("parentIdent" %in% names(this$stepDf) && !is.null(this$stepDf$parentIdent) && !is.null(this$stepDf$parentIdent)) {
    prepStep$parentStepId<-this$stepDf$parentIdent
  }

  #TODO inheritFromParent

  prepList <- as.list(prepStep)
  processNames <- this$stepDf$processes[[1]]$name

  if (length(processNames)==0) {
    stop("cannot create a step without process")
  }

  processes <- NULL
  subFolderNameMapping <- NULL

  for (i in 1:length(processNames)) {
    processDf <- prepareProcess(processNames[i])
    subFolderNameMapping <- c(subFolderNameMapping,processDf$subFolderNameMapping)
    processDf$subFolderNameMapping<-NULL
    processes<-plyr::rbind.fill(processes,processDf)
  }
  prepList$processes <- processes
  print(jsonlite::toJSON(prepList,auto_unbox = T,pretty=T))

  createResult <- improveR::authenticatedREST("/resources/{treeIdent}/steps",
                              urlParams = list(treeIdent=treeIdent),
                              data = prepList,
                              restType = "POST")
  if (createResult$status_code==201) {
    createContent <- httr::content(createResult)
    newStep <- improveR::loadResource(createContent$resourceId)
    invisible(improveR::unloadChildResources(newStep$parentId))
    invisible(improveR::unloadFullChildResources(newStep$parentId))
    moveSubFolderNameMapping(subFolderNameMapping,newStep)
    return(newStep)
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
realise <-function(force=T,run=T) {
  improveEditable()
  breakPoint <- this$getStepValue("breakpoint")
  reuse <- this$getStepValue("reuse")
  if (!is.null(breakPoint) && breakPoint==T) {
    run <-F
  }
  if (!is.null(reuse) && reuse==T) {
    force <-F
  }
  newStep <- NULL
  if (!force) {
    logging::logdebug("check step equality")
   # newStep <- existsInTargetTree(handle)
  #  if (!is.null(newStep)) {
  #    setStepValue(handle,"entityId",as.character(newStep$entityId))
  #    return(handle)
   # }
  }
  newStep <- this$create()
  this$setStepValue("entityId",as.character(newStep$entityId))
  tree <- improveR::loadResource(newStep$parentId)
  this$setStepValue("treeIdent",tree$resourceId)
  this$setStepValue("treeName",tree$name)
  this$setStepValue("treePath",dirname(tree$path))
  if (this$getStepState()=="INITIAL" && run) {
    this$run()
  }
  return(this)
}


#' run
#'
#' @param handle check if a step with the same configuration already exists in the tree
#' @references ics1140
#' @export
run <- function() {
  improveEditable()
  newStep <- this$getStepResource()
  result <- improveR::authenticatedREST("resources/{stepId}/run",
                              urlParams = list(stepId=newStep$resourceId),
                              restType = "POST")
  return(this)
}


#' getStepState
#'

#' @references ics1221
#' @export
getStepState <- function() {
  entityId <- this$getStepValue("entityId")
  step<-improveR:::internalLoadResourceFromServer(entityId)
  return(step$runStatus)
}

#' getStepResource
#'
#' @references ics1221
#' @export
getStepResource <- function() {
  entityId <- this$getStepValue("entityId")
  if (!is.null(entityId)) {
    return(improveR::loadResource(entityId))
  }
}


#' finishRun
#'
#' @param runserverName if this is set, the step only counts as finished if it was finished with this runserver (needs to be combined with tool), overridden by settings in stephandle
#' @param runserverToolName if this is set, the step only counts as finished if it was finished with this tool (needs to be combined with runserver), overridden by settings in stephandle
#' @references ics1140
#' @export
finishRun <- function(runserverName=NULL, runserverToolName=NULL) {

  #TODO finish conditions



  toolId<-NULL
  running<-TRUE
  #frn <- this$getStepValue("finishRunserverName")
  #frt <- this$getStepValue("finishRunserverTool")
  # runserverName<-NULL
  # runserverToolName<-NULL
  #
  # if (!is.null(frn) && !is.null(frn)) {
  #   runserverName<-frn
  #   runserverToolName<-frt
  # }
  #
  # toolId <- NULL
  # if (!is.null(runserverName)&&!is.null(runserverToolName)) {
  #   toolId <- getToolId(runserverName, runserverToolName)
  #   print(toolId)
  # } else if (!is.null(runserverName)||!is.null(runserverToolName)) {
  #   logging::logwarn("runServerName and runServerToolName must be provided in finishRun, or none of them")
  # }
  # running<-TRUE
  # wrongRun <- ""
  #
  # if (!is.null(toolId)) {
  #   step <- getStepWithoutCache(handle)
  #   state<-step$runStatus
  #   if (state=="FINISHED") {
  #     processes <- actualLoadProcessesForStep(step$resourceId)
  #     process <- dplyr::filter(processes,.data$processType=="main")
  #     stepTool <- processes$runserverToolId[1]
  #     if (toolId!=stepTool) {
  #       tryCatch({
  #         run <- actualLoadProcessRuns(process$id)
  #         run <- run[run$startedAt==max(run$startedAt),]
  #         currentRun <-run$id
  #         wrongRun <- currentRun
  #       },
  #       error=function(cond) {
  #         Sys.sleep(5)
  #       }
  #       )
  #     }
  #   }
  # }

  while(running) {
    step <- this$getStepWithoutCache()
    state<-step$runStatus
    if (is.null(toolId)) {
      if (state=="FINISHED") {
        running<-F
      } else {
        Sys.sleep(2)
      }
    }
    # else {
    #   processes <- actualLoadProcessesForStep(step$resourceId)
    #   process <- dplyr::filter(processes,.data$processType=="main")
    #   stepTool <- processes$runserverToolId[1]
    #   tryCatch({
    #     run <- actualLoadProcessRuns(process$id)
    #     run <- run[run$startedAt==max(run$startedAt),]
    #     currentRun <-run$id
    #     if (state=="FINISHED" && toolId==stepTool && currentRun!=wrongRun) {
    #       running<-F
    #     } else {
    #       if (toolId!=stepTool && state!="FINISHED") {
    #         wrongRun <- currentRun
    #       }
    #
    #
    #       Sys.sleep(5)
    #     }
    #   },
    #   error=function(cond) {
    #     Sys.sleep(5)
    #   }
    #   )
    # }
  }
  invisible(this)
}
getStepWithoutCache <- function() {
  entityId <- this$getStepValue("entityId")
  step<-improveR:::internalLoadResourceFromServer(entityId)
  return(step)
}


#' getStepInventory retrieves all files from the inventory of a handle, if a step was created with this handle
#'

#' @param recurse if the complete inventory should be retrieved or only the top level
#' @param update unloads the cached resources, default true
#' @references ics1221
#' @export
getStepInventory <- function(recurse=F,update=T) {
  step <- this$getStepResource()
  return(improveR:::getStepResourceInventory(step,recurse,update))
}

