
timing <- function(...) {}


#' creates the prepared step in the repository
#'
#' @param stepHandle id of the prepared step
#' @references ics1140
#' @export
createPreparedStep <- function(stepHandle) {
  improveEditable()
  logging::logdebug("createPreparedStep")
  timing("createPreparedStep")
  prepStep <- retrieveStep(stepHandle)
  mainPrep <- retrieveMainProcess(stepHandle)

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

#' changes the step description
#'
#' @param ident ident of the step
#' @param from from if a relative path is used
#' @param description new step desciption
#' @references ics1217
#' @export

changeStepDescription <- function(ident, from=pwd(),description) {
  stepEntity <- loadResource(ident,from)
  stepEntity$description<- description

  result <- authenticatedREST("/resources/{resourceId}/",
                                  urlParams = list(resourceId=stepEntity$resourceId),
                                  data=as.list(stepEntity),
                                  restType = "PUT"
  )
  return(updateResource(ident,from))
}

#' changes the step rationale
#'
#' @param ident ident of the step
#' @param from from if a relative path is used
#' @param rationale new step rationale
#' @references ics1217
#' @export

changeStepRationale <- function(ident, from=pwd(),rationale) {
  stepEntity <- loadResource(ident,from)
  stepEntity$rationale<- rationale

  result <- authenticatedREST("/resources/{resourceId}/",
                                            urlParams = list(resourceId=stepEntity$resourceId),
                                            data=as.list(stepEntity),
                                            restType = "PUT"
  )
  return(updateResource(ident,from))
}

getToolId <- function(runserverName, runserverToolName) {
  runservers <- loadRunservers()
  runserver <- runservers[runservers$label==runserverName,]

  tools  <- loadToolsForRunserver(as.character(runserver$id))
  tool <- tools[tools$name==runserverToolName,]
  return(as.character(tool$id))
}


addExtLinkToStep <- function(newStep, filePrep) {
  timing("prepareRemoteStart")


  if (is.null(filePrep)) {
    return()
  }

  createTarget <- newStep




  fileName <- filePrep$name
  if (is.na(fileName)) {
    fileName <-"External Link"
  }
  if (grepl(pattern = "/", x=fileName,fixed = T)) {
    pathParts <- strsplit(x=fileName,split="/",fixed=T)[[1]]
    if (length(pathParts)!=2) {
      logging::logwarn("maximum folder depth allowed is 1, by filename in realise step")
      logging::logwarn(fileName)
      return()
    }
    folderName <- pathParts[1]
    fileName<- pathParts[2]
    children <- loadChildResources(newStep)
    folder <- children[children$name==folderName,]
    if (nrow(folder)==1 && folder$nodeType!="Folder") {
      logging::logwarn(folderName)
      logging::logwarn("already exists but not as folder")
      return()
    }
    if (nrow(folder)==1) {
      createTarget<-folder
    } else {
      createTarget <- createFolder(newStep,folderName=folderName)
    }
  }

  newFile <- NULL

    if (is.na(filePrep$url)) {
      return()
    }
    newFile <- createExternalLink(targetIdent = createTarget,linkName = basename(filePrep$name),url = filePrep$url)

  timing("created")

}

addFileToStep <- function(newStep, filePrep,local) {
  timing("prepareRemoteStart")


  if (is.null(filePrep)) {
    return()
  }

  createTarget <- newStep



  #check all possible fields
  if (!("name" %in% names(filePrep))) {
    filePrep$name<-""
  }
  fileName <- filePrep$name
  if (is.na(fileName)) {
    fileName <-""
  }
    if (grepl(pattern = "/", x=fileName,fixed = T)) {
        pathParts <- strsplit(x=fileName,split="/",fixed=T)[[1]]
        if (length(pathParts)!=2) {
          logging::logwarn("maximum folder depth allowed is 1, by filename in realise step")
          logging::logwarn(fileName)
          return()
        }
        folderName <- pathParts[1]
        fileName<- pathParts[2]
        children <- loadChildResources(newStep)
        folder <- children[children$name==folderName,]
        if (nrow(folder)==1 && folder$nodeType!="Folder") {
          logging::logwarn(folderName)
          logging::logwarn("already exists but not as folder")
          return()
        }
        if (nrow(folder)==1) {
          createTarget<-folder
        } else {
          createTarget <- createFolder(newStep,folderName=folderName)
        }
    }

  if (!("variableName" %in% names(filePrep))) {
    filePrep$variableName<-""
  }

  newFile <- NULL
  if (local) {
    if (is.na(filePrep$path)) {
      return()
    }
    if (is.null(filePrep$name)) {
      filePrep$name <- basename(filePrep$path)
    }
    newFile <- createFile(targetIdent = createTarget,fileName = fileName,localPath = filePrep$path)
  } else {
    timing("startRemoteStart")
    if (filePrep$asLink) {
      if (("sourceHandle" %in% names(filePrep)) && !is.na(filePrep$sourceHandle) ) {
        referencedStep <- getStepResource(filePrep$sourceHandle)
        unloadChildResources(referencedStep)
        unloadFullChildResources(referencedStep)
        inventory <- getStepInventory(filePrep$sourceHandle,recurse = T,update = T)$data[[1]]
        sourceName <- filePrep$name
        if ("sourceName" %in% names(filePrep) && !is.null(filePrep$sourceName)) {
          sourceName <- filePrep$sourceName
        }
        resolvedFile <- inventory[inventory$inventoryPath==sourceName,]
        if (nrow(resolvedFile)==0) {
          log_warn(sourceName,"not found in",referencedStep)
        } else {
          newFile <- createLink(createTarget$resourceId,resolvedFile$resourceId,linkName=fileName )
        }

      } else {
        newFile <- createLink(createTarget$resourceId,filePrep$ident,linkName=fileName )
      }
    } else {
      newFile <- copy(filePrep$ident,createTarget$resourceId,targetName=fileName)
    }
  }
  timing("created")
  if (!is.na(filePrep$variableName) && !filePrep$variableName=="" && !is.null(newFile)) {
    timing("variableStart")
    stepProcesses <- loadProcessesForStep(newStep$resourceId)
    variableProcess <- stepProcesses[stepProcesses$name==filePrep$variableProcess,]
    processId <- as.character(variableProcess$id)
    variables <- getProcessFileVariables(newStep,processId)
    variableId <- as.character(variables[variables$name==filePrep$variableName,]$id)
    if (length(variableId)==0) {
      position=1
      if (!is.null(variables)) {
        position<- max(variables$position)+1
      }
      variable<- createProcessFileVariable(ident = newStep$resourceId,processId = processId,name = filePrep$variableName,variableType = "fileRef",position = position)
      variableId<-variable[[1]][1]
    }

    result <- authenticatedREST("/resources/{resourceId}/processes/{processId}/variables/{variableId}",
                                              urlParams = list(resourceId=newStep$resourceId,
                                                               processId=processId,
                                                               variableId=variableId),
                                              data = list(type= "processVariable",
                                                          id= variableId,
                                                          name= filePrep$variableName,
                                                          position= 1,
                                                          valueResourceId=newFile$resourceId,
                                                          variableType="fileRef"),
                                              restType = "PUT")
    timing("variableStart")
  }
}

