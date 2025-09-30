







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
      log_warn(ident, "does not exist or is not a Step")
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
    if (is.null(workflow)) {
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

  processes <- byNotEmptyAsDf(processes,function(pro) {
    gridArguments <- updateProcessGridArguments(pro$id)
    if (!is.null(gridArguments)) {
      pro$gridArguments <- list(
        byNotEmptyAsDf(gridArguments,function(ga) {
          gridHandle <- data.frame(handle=stepHandle)
          gridHandle$argumentName<- ga$name

          if (ga$gridArgumentType=="LOV") {
            categoryValues <- ga$category[[1]]$values[[1]]
            gridHandle$argumentValue <- categoryValues[categoryValues$id==ga$lovValueId,]$text
          } else if (ga$gridArgumentType=="TEXT") {
            gridHandle$argumentValue <- ga$textValue
          } else if (ga$gridArgumentType=="DATE_TIME") {
            gridHandle$argumentValue <- round(as.numeric(ga$dateValue)/1000)
          }
          return(gridHandle)
        })
      )
    } else {
      pro$gridArguments <-list(data.frame(handle=character(), argumentName=character(), argumentValue=character()))
    }
    return(pro)
  })

  # Set default values for all fields that might be missing when not configured
  if (!("toolArgs" %in% names(processes))) {processes$toolArgs<-""}
  if (!("toolStreamablePatterns" %in% names(processes))) {processes$toolStreamablePatterns<-""}
  if (!("runserverLabel" %in% names(processes))) {processes$runserverLabel<-""}
  if (!("toolLabel" %in% names(processes))) {processes$toolLabel<-""}
  if (!("toolInstance" %in% names(processes))) {processes$toolInstance<-""}

  processDfs <- dplyr::select(processes,
                              "handle","runserverLabel","toolLabel","toolInstance","toolArgs","toolStreamablePatterns",
                              "selected","gridTool","main","name","processType","position","gridArguments")



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

  # Add parent step information if it exists
  if (!is.null(step$parentStepId)) {
    parentStep <- loadResource(step$parentStepId)
    if (!is.null(parentStep)) {
      newHandle$parentIdent <- parentStep$entityId
      newHandle$inheritFromParent <- step$inheritFromParent
    }
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
  variableProcesses <- if("processId" %in% names(variables)) {
    dplyr::pull(dplyr::distinct(variables,.data$processId))
  } else {
    c()
  }
  variables <- if(length(variableProcesses) > 0) {
    lapply(variableProcesses,
           function(proc) {
             return(actualLoadProcessVariables(proc))
           }
    )
  } else {
    list()
  }
  variables <- mergeListToDataframe(variables)
  #workaround end

  # Handle empty variables case
  if(nrow(variables) > 0) {
    variables$variableName <- variables$name
    variables <- byNotEmptyAsDf(variables,function(v) {
      v$variableProcess <- processes[processes$id==v$processId,]$name
      return(v)
    })
    if (!("valueResourceId" %in% names(variables))) {
      variables$valueResourceId <- NA
    }
    variables <- dplyr::select(variables,"valueResourceId","variableName","variableProcess")
  } else {
    # Create empty variables dataframe with correct structure
    variables <- data.frame(
      valueResourceId = character(0),
      variableName = character(0),
      variableProcess = character(0),
      stringsAsFactors = FALSE
    )
  }



  #TODO load folders
  folders <- inventory[inventory$nodeType=="FOV",]
  if (nrow(folders)>0) {
    inventory <- getStepResourceInventory(step,recurse=T)$data[[1]]
  }



  #filter only inputs
  #try with initial steps

  links <- inventory[inventory$nodeType=="LIV" | inventory$nodeType=="Link",]

  files <- inventory[inventory$nodeType=="FIV" | inventory$nodeType=="File",]
  if ("startedAt" %in% names(processes)) {
    if (("revisionFromTime" %in% names(files))) {
      files <- loadResource(files)
    }
    files <- files[files$lastModifiedOn<processes$startedAt,]
  }
  inventory <- plyr::rbind.fill(links,files)
  #TODO nodeTypes

  # Safe left join - only join if both dataframes have the required columns
  if(nrow(inventory) > 0 && nrow(variables) > 0 &&
     "resourceId" %in% names(inventory) && "valueResourceId" %in% names(variables)) {
    inventory <- dplyr::left_join(inventory,variables,c("resourceId" = "valueResourceId"))
  } else if(nrow(inventory) > 0) {
    # Add empty variable columns to inventory when no variables exist
    inventory$variableName <- NA_character_
    inventory$variableProcess <- NA_character_
  }

  inputFiles <- inventory[inventory$nodeType=="FIV" | inventory$nodeType=="File",]

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
  }
  return(fileList)
}

