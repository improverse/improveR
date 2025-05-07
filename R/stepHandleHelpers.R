


#' sets the containing tree
#'
#' @param stepHandle id of the prepared step
#' @param treeIdent ident of the containing tree
#'
#' @export
setStepTree <- function(stepHandle,treeIdent) {
  tree <- loadResource(treeIdent)
  setStepValue(stepHandle,"treeIdent",tree$resourceId)
}

#' sets the improveR workflow
#'
#' @param stepHandle id of the prepared step
#' @param workflowHandle handle of the workflow
#'
#' @export
setStepWorkflow <- function(stepHandle,workflowHandle) {
  setStepValue(stepHandle,"workflowHandle",workflowHandle)
}

#' sets the parent step
#'
#' @param stepHandle id of the prepared step
#' @param parentIdent ident of the containing tree
#' @param inheritFromParent if input files and settings should be taken over from the parent
#'
#' @export
setStepParent <- function(stepHandle,parentIdent,inheritFromParent=F) {
  if (!is.null(parentIdent)) {
    parent <- loadResource(parentIdent)
    setStepValue(stepHandle,"parentIdent",parent$resourceId)
    setStepValue(stepHandle,"inheritFromParent",inheritFromParent)
  } else     {
    setStepValue(stepHandle,"parentIdent",NULL)
    setStepValue(stepHandle,"inheritFromParent",NULL)
  }
}

#' sets a step breakpoint, the step run is not started even if realiseStep is called with run=T
#'
#' @param stepHandle id of the prepared step
#' @param breakpoint boolean, true inhibits step execution in workflows/realiseStep
#'
#' @export
setStepBreakpoint <- function(stepHandle,breakpoint=T) {
  setStepValue(stepHandle,"breakpoint",breakpoint)
}

#' sets a step reusage, the step is not created if an identical finished step already exists in the tree
#'
#' @param stepHandle id of the prepared step
#' @param reuse boolean, true inhibits recreation of identical in workflows/realiseStep
#'
#' @export
setStepReuse <- function(stepHandle,reuse=T) {
  setStepValue(stepHandle,"reuse",reuse)
}

#' sets the finish runserver and runserver tool by name, finishStep only stops if the step is executed and finished with this combination
#'
#' @param stepHandle id of the prepared step
#' @param runserverName name of the runserver
#' @param runserverTool name of the runserverTool
#'
#' @export
setStepFinishCondition <- function(stepHandle,runserverName,runserverTool) {
  setStepValue(stepHandle,"finishRunserverName",runserverName)
  setStepValue(stepHandle,"finishRunserverTool",runserverTool)
}

#' sets the runserver by name
#'
#' @param stepHandle id of the prepared step
#' @param runserverName name of the runserver
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
setStepRunserverName <- function(stepHandle,runserverName,process="Main") {
  setProcessValue(stepHandle,process,"runserverName",runserverName)
}

#' sets the name of the step
#'
#' @param stepHandle id of the prepared step
#' @param stepName name of the step
#'
#' @export
setStepName <- function(stepHandle,stepName) {
  setStepValue(stepHandle,"stepName",stepName)
}

#' sets the description of the step
#'
#' @param stepHandle id of the prepared step
#' @param description description of the step
#'
#' @export
setStepDescription <- function(stepHandle,description) {
  setStepValue(stepHandle,"description",description)
}

#' sets the rationale of the step
#'
#' @param stepHandle id of the prepared step
#' @param rationale rationale of the step
#'
#' @export
setStepRationale <- function(stepHandle,rationale) {
  setStepValue(stepHandle,"rationale",rationale)
}


#' sets the command line
#'
#' @param stepHandle id of the prepared step
#' @param commandline the new command line or command line appendix
#' @param append if the complete command line is replaced or this is appended defaults to TRUE
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
setStepCommandLine <- function(stepHandle,commandline,append=T,process="Main") {
  setProcessValue(stepHandle,process,"commandline",commandline)
  setProcessValue(stepHandle,process,"appendCommandline",append)
}

#' sets the runserver tool by name
#'
#' @param stepHandle id of the prepared step
#' @param runserverToolName name of the runserver tool
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
setStepRunserverToolName <- function(stepHandle,runserverToolName,process="Main") {
  setProcessValue(stepHandle,process,"runserverToolName",runserverToolName)
}

#' sets the tool by name
#'
#' @param stepHandle id of the prepared step
#' @param toolName name of the  tool
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
setStepToolName <- function(stepHandle,toolName,process="Main") {
  setProcessValue(stepHandle,process,"toolName",toolName)
}

#' adds a new remote file to the step
#'
#' @param stepHandle id of the prepared step
#' @param ident the ident of the file in the repository
#' @param name leave empty if you want to use the same name as the used file
#' @param asLink, if file should be copied or linked, boolean, defaults to link
#' @param variableName the name of the variable the file should be bound to, optional
#' @param sourceHandle stepHandle if ident is relative to another step in the workflow
#' @param sourceName name if ident is relative to another step in the workflow
#' @param variableProcess if the file is bound to a variable, the process name the variable belongs to
#'
#' @export
addStepRemoteFile <- function(stepHandle,ident=NULL,name=NULL,asLink=T,variableName=NULL,sourceHandle=NULL,sourceName=NULL,variableProcess="Main") {


  ###TODO incorporate sourceHandles and specific versions
  fileList <- data.frame(stepHandle=stepHandle,stringsAsFactors = F)
  if (!is.null(ident)) {
    resource <- loadResource(ident)
    if (resource$nodeType=="File") {
      fileList["ident"]<-resource$entityId
    } else if (resource$nodeType=="Link"){

      fileList["ident"]<-resource$targetEntityId
    } else {
      logging::logwarn("Only files or resources can be added to an inventory")
      logging::logwarn(ident)
      logging::logwarn(stepHandle)
      return(stepHandle)
    }
  }
  fileList["asLink"]<-asLink
  fileList["name"]<-name
  fileList["variableName"]<-variableName
  fileList["variableProcess"]<-variableProcess
  if (!is.null(sourceHandle)) {
    fileList["sourceHandle"]<-sourceHandle
    fileList["sourceName"]<-sourceName
    addStepValue(stepHandle,"dependencies",sourceHandle)
    addStepValue(sourceHandle,"usage",stepHandle)
  }
  addStepValue(stepHandle,"remoteFiles",fileList)
}

#' adds a new external link to the step
#'
#' @param stepHandle id of the prepared step
#' @param name name of the link
#' @param url, the url the link targets
#'
#' @export
addExtLink <- function(stepHandle,name,url) {
  extLinkList <- data.frame(stepHandle=stepHandle,stringsAsFactors = F)
  extLinkList$name<-name
  extLinkList$url <- url
  addStepValue(stepHandle,"extLinks",extLinkList)
}


#' removes a remote file from the step
#'
#' @param stepHandle id of the prepared step
#' @param name the name of the file in the inventory
#' @param ident the ident of the file in the inventory
#'
#' @export
removeStepRemoteFile <- function(stepHandle,name=NULL,ident=NULL) {
  stepData <- retrieveStep(stepHandle)
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
  storeStep(stepHandle = stepHandle,stepList=stepData)
  return(stepHandle)
}


#' removes a local file from the step
#'
#' @param stepHandle id of the prepared step
#' @param name the name of the file in the inventory
#' @param path the path of the file in the filesystem
#'
#' @export
removeStepLocalFile <- function(stepHandle,name=NULL,path=NULL) {
  stepData <- retrieveStep(stepHandle)
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
  storeStep(stepHandle = stepHandle,stepList=stepData)
  return(stepHandle)
}

#' changes a remote file in the step
#'
#' @param stepHandle id of the prepared step
#' @param name the name of the file in the inventory
#' @param asLink if the file is added as link, boolean, not changed if NULL
#' @param newName the new name of the file, not changed if NULL
#' @param newIdent the new ident of the file, not changed if NULL
#'
#' @export
changeStepRemoteFile <- function(stepHandle,name,asLink=NULL,newName=NULL,newIdent=NULL) {
  stepData <- retrieveStep(stepHandle)
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
  storeStep(stepHandle = stepHandle,stepList=stepData)
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
#' @param stepHandle id of the prepared step
#' @param argumentName the ident of the file in the repository
#' @param argumentValue leave empty if you want to use the same name as the used file
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
addStepGridArgument <- function(stepHandle,argumentName,argumentValue,process="Main") {
  gridList <- data.frame(argumentName=argumentName,
                         argumentValue=argumentValue)
  addProcessValue(stepHandle,"gridArguments",gridList,process)
}

#' removes all grid arguments from the step
#'
#' @param stepHandle id of the prepared step
#' @param process the name of the process, default = Main. If the process does not yet exist it is created
#'
#' @export
removeStepGridArguments <- function(stepHandle,process="Main") {
  removeProcessValue(stepHandle,"gridArguments",process)
  return(stepHandle)
}

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
#' @param stepHandle id of the prepared step
#' @param path local path to the file
#' @param name leave empty if you want to use the same name as the used file
#' @param variableName the name of the variable the file should be bound to, optional
#' @param variableProcess the name of the process for the variable the file should be bound to, optional
#'
#' @export
addStepLocalFile <- function(stepHandle,path,name=NULL,variableName=NULL,variableProcess="Main") {
  fileList <- data.frame(stepHandle=stepHandle)
  fileList["name"]<-name
  fileList["variableName"]<-variableName
  fileList["variableProcess"]<-variableProcess
  fileList["path"]<-path
  addStepValue(stepHandle,"localFiles",fileList)
}



#' retrieveMainProcess
#' retrieves the main process data frame of a step by handle
#'
#' @param stepHandle id of the prepared step
#'
#' @export

retrieveMainProcess <- function(stepHandle) {
  stepData <- retrieveStep(stepHandle)
  processes <- stepData$processes[[1]]
  if (nrow(processes)>0 && ("main" %in% processes$processType)) {
    return(processes[processes$processType=="main",])
  }
  return(NULL)
}

#' getHandleForResource
#' returns the stepHandle for a specific step resource.
#' Only works if a prepared step has been executed or a workflow was pulled from the repository and not detached via detachWorkflowFromResources
#'
#' @param workflowHandle the workflow to seach for the step
#' @param ident ident of the resource
#' @param from, path for relative pathes, default pwd()
#'
#' @export
getHandleForResource <- function(workflowHandle,ident,from=pwd()) {
  workflow <- retrieveWorkflow(workflowHandle)
  resource <- loadResource(ident,from)
  if (nrow(resource)==1) {
    workflow <- workflow[workflow$entityId==resource$entityId,]
    if (nrow(workflow)==1) {
      return(workflow$handle)
    }
  }
  return(NULL)
}
#' detachWorkflowFromResources
#' deletes all entity ids from a workflow, switches from in place execution to creation of new steps
#'
#' @param workflowHandle handle of the workflow
#'
#' @export
detachWorkflowFromResources <- function(workflowHandle) {
  workflow <- retrieveWorkflow(workflowHandle)
  workflow$entityId <- NULL
  workflow<- persistWorkflowChanges(workflow)
  return(workflowHandle)
}

#' detachWorkflowFromTrees
#' removes coupling to a specific tree for an entire workflow.
#' if no treeIdent is provided treeName and treePath have to be provided.
#'
#' @param workflowHandle handle of the workflow
#'
#' @export
detachWorkflowFromTrees <- function(workflowHandle) {
  workflow <- retrieveWorkflow(workflowHandle)
  workflow$treeIdent <- NULL
  workflow<- persistWorkflowChanges(workflow)
  return(workflowHandle)
}

#' setWorkflowTreeRootFolder
#' sets the path to a root folder for the tree that the steps are created in. Only used if no treeIdent is set.
#' Use detachWorkflowFromTrees to remove treeIdent
#'
#' @param workflowHandle handle of the workflow
#' @param rootFolder path to the rootFolder
#'
#' @export
setWorkflowTreeRootFolder <- function(workflowHandle,rootFolder) {
  workflow <- retrieveWorkflow(workflowHandle)
  workflow$treePath <- rootFolder
  workflow<- persistWorkflowChanges(workflow)
  return(workflowHandle)
}

#' setWorkflowTreeName
#' sets the name for the tree that the steps are created in. Only used if no treeIdent is set.
#' Use detachWorkflowFromTrees to remove treeIdent
#'
#' @param workflowHandle handle of the workflow
#' @param treeName name of the tree
#'
#' @export
setWorkflowTreeName <- function(workflowHandle,treeName) {
  workflow <- retrieveWorkflow(workflowHandle)
  workflow$treeName <- treeName
  workflow<- persistWorkflowChanges(workflow)
  return(workflowHandle)
}

#' setWorkflowTreeIdent
#' sets the ident for the existing tree that the steps are created in.
#' Use detachWorkflowFromTrees to remove treeIdent again.
#'
#' @param workflowHandle handle of the workflow
#' @param treeIdent ident of the tree
#'
#' @export
setWorkflowTreeIdent <- function(workflowHandle,treeIdent) {
  tree <- loadResource(treeIdent)
  workflow <- retrieveWorkflow(workflowHandle)
  workflow$treeIdent <- tree$resourceId
  workflow<- persistWorkflowChanges(workflow)
  return(workflowHandle)
}


