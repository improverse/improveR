
#Step is a fully loaded step, workflow is a fully loaded workflow as df
#all edit functions for a step are in a steptemplate (code sourced into env)
#a steptemplate is always in a workflowtemplate
#a workflowtemplate has a list of all used files and their references
#a workflowtemplate can directly have parameters
#a workflowtemplate has merge / intersect, subset, ... functionalities
# execute workflow is built in a way that dispatcher works also
#check if all tool parameters can be set
#check runs against processes









#check grid arguments
#check links
#check external links
#check folders

#two envs: getStep and getStepTemplate. fileList and dependencies handled in workflow
#stepNames: treeName and stepName
#getLineage / get Usage in workflow
#getParentstep in workflow
#removeStep
#makeStepRelative in workflow, dependencies in workflow
#detach from resource and detach from tree in workflow


#' getStepTemplate
#' reads a step and its processes and generates a new handle
#' @param ident the ident of the step
#' @references ics1213
#' @export
getStepTemplate <- function(ident) {
  transforStepToTemplate(getStep(ident))


}

transforStepToTemplate <- function(stepEnv) {
  stepTemplateSource <- system.file("_stepTemplate.R", package = "improveR")
  source(stepTemplateSource,local=stepEnv)
  return(stepEnv)
}



createStepName <- function(step) {
  stepDf <- step$stepDf
  treeName <- stepDf$treeName
  if (is.null(treeName)) {
    treeName <-"WF"
  }
  stepName <- stepDf$sourceName
  if (is.null(stepName)) {
    stepName <- uuid::UUIDgenerate()
  }
  id <- stepDf$sourceEntityId
  if (is.null(id)) {
    id <- uuid::UUIDgenerate()
  } else {
    id <- strsplit(id,"-")[[1]]
    id <- id[length(id)]
  }
  fullStepName <- paste(treeName,stepName,id,sep = "/")
}

#' getStep
#' reads a step and its processes and generates a new handle
#' @param ident the ident of the step
#' @param workflow the step should be added to
#' @references ics1213
#' @export
getStep <- function(ident,workflow=NULL) {

  if (!is.null(workflow)) {
    stepEntity <- loadResource(ident)
    if (!is.null(stepEntity) && stepEntity$nodeType=="Step") {
      allSteps <- workflow$df()
      selectedStep <- allSteps[allSteps$sourceEntityId==stepEntity$entityId,]
      if (nrow(selectedStep)==1) {
        return(workflow$steps[[selectedStep$fullName]])
      }
    } else {
      log_warn(ident,"does not exist or is not a Step")
      return(NULL)
    }
  }

  stepDf <- getStepDf(ident)
  return(prepareStepEnv(stepDf=stepDf,workflow=workflow))
}



prepareStepEnv <- function(treeIdent=NULL,stepDf = NULL,workflow=NULL) {
    stepHandle <- uuid::UUIDgenerate()

    if (is.null(stepDf)) {
      stepDf = data.frame(handle=stepHandle,stringsAsFactors = F)
    }
    if (!is.null(treeIdent)) {
      resourceId <- loadResource(treeIdent)$resourceId
      stepDf$treeIdent<-resourceId
    }
    #stepSource <- system.file("_step.R", package = "improveR")
    #stepEnv <- new.env()
    #source(stepSource,local=stepEnv)
    #stepEnv$stepDf<-stepDf
    #stepEnv$this<-stepEnv
    if (is.null(workflow)) {
      #workflow <- new.env()
      #workflowSource <- system.file("_workflow.R", package = "improveR")
      #source(workflowSource,local=workflow)
      workflow <- createWorkflow()
    }
    stepEnv <- createStepEnv(stepDf, workflow)

    return(stepEnv)
}


getStepDf <- function(ident) {
  #toolInstances <- getToolInstances()


  stepHandle <- uuid::UUIDgenerate()

  #notRun
  #ident <- "envhost1.hc.scintecodev.internal-5310:ST-79611"

  #ident <- "envhost1.hc.scintecodev.internal-5310:ST-79162"
  step <- loadResource(ident)
  tree <- loadResource(step$parentId)
  restResult <- authenticatedREST(url = "/resources/{resourceId}",
                                  urlParams = list(resourceId = step$resourceId),
                                  queryParams = list(optParams="inventory"))

  restContent <- httr::content(restResult)


  processes <- updateProcessesForStep(stepIdent = ident)

  #if run exists take toolArgs from run
  if (nrow(processes)==0) {
    return (NULL)
  }
  processes <- processes[order(processes$position),]
  processes <- processes[processes$selected,]

  processes$handle <- stepHandle
  processDfs <- dplyr::select(processes,
                              handle,runserverLabel,toolLabel,toolInstance,toolArgs,toolStreamablePatterns,
                              selected,gridTool,main,name,processType,position)


  newHandle <- data.frame(handle=stepHandle,stringsAsFactors = F)
  newHandle$processes <- list(processDfs)
  newHandle$treeIdent<-step$parentId
  newHandle$treeName <- tree$name
  newHandle$treePath <- dirname(tree$path)
  newHandle$description<-step$description
  newHandle$rationale<-step$rationale
  newHandle$sourceEntityId<-step$entityId
  newHandle$sourceName<-step$name
  if (!startsWith(step$name,"Step ")) {
    newHandle$stepName <- step$name
  }



  #remove parentIds
  inventory <- restContent$children
  inventory <- lapply(inventory,function(entry) {
    entry$parentEntityVersionIds<-NULL
    entry$parentResourceVersionIds<-NULL
    return(entry)
  })
  inventory <- mergeListToDataframe(inventory)

  #reference should be shown
  variables <- mergeListToDataframe(processes$variables)
  variableProcesses <- dplyr::pull(dplyr::distinct(variables,processId))
  variables <- lapply(variableProcesses,
                      function(proc) {
                        return(actualLoadProcessVariables(proc))
                      }
  )
  variables <- mergeListToDataframe(variables)
  #workaround end

  variables$variableName <- variables$name
  variables$variableProcess <- processes[processes$id==variables$processId,]$name
  if (!("valueResourceId" %in% names(variables))) {
    variables$valueResourceId <- NA
  }
  variables <- dplyr::select(variables,valueResourceId,variableName,variableProcess)



  #TODO load folders
  folders <- inventory[inventory$nodeType=="FOV",]
  if (nrow(folders)>0) {
    inventory <- getStepResourceInventory(step,recurse=T)$data[[1]]
  }

  #filter only inputs
  #try with initial steps

  if ("startedAt" %in% names(processes)) {
    inventory <- inventory[inventory$revisionFromTime<processes$startedAt,]
  }

  #TODO nodeTypes

  inventory <- dplyr::left_join(inventory,variables,c("resourceId" = "valueResourceId"))

  inputFiles <- inventory[inventory$nodeType=="FIV" | inventory$nodeType=="File",]
  if (step$runStatus!="INITIAL") {
    if (!"revisionFromTime" %in% names(inputFiles)) {
      inputFiles$revisionFromTime<-inputFiles$createdAt
    }
    inputFiles <- inputFiles[inputFiles$revisionFromTime<processes[1,]$startedAt,]
  }



  #inventoryPath

  links <- inventory[inventory$nodeType=="LIV" | inventory$nodeType=="Link",]
  linkHandles <- byNotEmptyAsDf(links,function(link) {
    resource <- loadResource(link$resourceId)
    inventoryPath <- paste0(".",substr(resource$path,nchar(step$path)+1,nchar(resource$path)))

    createRemoteFileDf(stepHandle=stepHandle,ident = link$resourceId,name = inventoryPath,asLink = T,variableName = link$variableName,variableProcess = link$variableProcess)

  })
  fileHandles <- byNotEmptyAsDf(inputFiles,function(f) {
    createRemoteFileDf(stepHandle=stepHandle,ident = f,name = f$inventoryPath,asLink = F,variableName = f$variableName,variableProcess = f$variableProcess)
  })

  remoteFiles <- plyr::rbind.fill(linkHandles,fileHandles)
  newHandle$remoteFiles<-list(remoteFiles)
  #TODO not resolved type for extfolder
  externalLinks <- inventory[inventory$nodeType=="ExtLink",]
  linkHandles <- byNotEmptyAsDf(externalLinks,function(link) {

    createExtLink(stepHandle=stepHandle,name=link$inventoryPath,url = link$url)

  })
  newHandle$extLinks<-linkHandles
  return(newHandle)
}


createExtLink <- function(stepHandle,name,url) {
  extLinkList <- data.frame(stepHandle=stepHandle,stringsAsFactors = F)
  extLinkList$name<-name
  extLinkList$url <- url
  return(extLinkList)
  #addStepValue(stepHandle,"extLinks",extLinkList)
}

createRemoteFileDf <- function(stepHandle,ident=NULL,name=NULL,asLink=T,variableName=NULL,sourceHandle=NULL,sourceName=NULL,variableProcess="Main") {

  fileList <- data.frame(stepHandle=stepHandle,stringsAsFactors = F)
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
      return(NULL)
    }
  }
  fileList["asLink"]<-asLink
  fileList["name"]<-name
  fileList["variableName"]<-variableName
  fileList["variableProcess"]<-variableProcess
  if (!is.null(sourceHandle)) {
    fileList["sourceHandle"]<-sourceHandle
    fileList["sourceName"]<-sourceName
    #TODO move out?
    addStepValue(stepHandle,"dependencies",sourceHandle)
    addStepValue(sourceHandle,"usage",stepHandle)
  }
  return(fileList)
}

