#' handlesFromTree
#' reads a tree and creates handles for all steps
#' @param ident the ident of the step
#' @param from if relative path, default the starting step
#' @param includeSelf by defaults does not include the step the script is run with.
#' @export
handlesFromTree <- function(ident,from=pwd(),includeSelf=F) {
  workflowHandle <- uuid::UUIDgenerate()
  steps <- loadChildResources(ident,from)$data[[1]]
  if (!includeSelf) {
    steps <- steps[steps$resourceId!=pwd()$resourceId,]
  }
  steps <- steps[steps$nodeType=="Step",]
  handleList <- byNotEmptyAsDf(steps,function(step) {
    handle <- handleFromStep(step)
    setStepWorkflow(handle,workflowHandle)
    df <- retrieveStep(handle)
    return(df)
  })

  storeWorkflow(workflowHandle,handleList)

  if (F &&  length(handleList)>0){

    entityIdIndex <- list()
    for (i in 1:length(handleList)) {
      handle <- handleList[i][[1]]
      entityIdIndex[handle$entityId]<-list(handle)
    }

    for (i in 1:length(handleList)) {
      handle <- handleList[i][[1]]
      remoteFiles <- handle$remoteFiles
      for (j in 1:length(remoteFiles)) {
        remoteFile <- remoteFiles[j][[1]]
        if ("ident" %in% names(remoteFile)) {
          parentStep <- findContainerStep(remoteFile$ident)
          if (!is.null(parentStep)) {
            filePath <- getRelativePath(parentStep,remoteFile$ident)
            relativeStep <- entityIdIndex[parentStep$entityId][[1]]
          }
        }
      }
    }

  }


  return(workflowHandle)
}



findContainerStep <- function(ident) {
  if (is.null(ident)) {
    return(NULL)
  }
  remoteResource <- loadResource(ident)
  #print(remoteResource)
  if (remoteResource$resourceId==0) {
    return(NULL)
  }
  #print(remoteResource$nodeType)
  if (remoteResource$nodeType=="Step") {
    return(remoteResource)
  }
  return(findContainerStep(remoteResource$parentId))
}

getRelativePath <- function(parentStep,ident) {
  identPath <- loadResource(ident)$path
  stepPath <- loadResource(parentStep)$path
  relativePath <- substr(identPath,nchar(stepPath)+1,nchar(identPath))
  relativePath <- paste0(".",relativePath)
  return(relativePath)
}

#' persistWorkflowChanges
#' takes all changes made to the data frame and persists them with their step handle
#' @param workflowDf the workflow dataframe as retrieved by retrieveWorkflow
#' @references ics1215
#' @export
persistWorkflowChanges <- function(workflowDf) {
  i<-byNotEmpty(workflowDf,function(stepData) {
    storeStep(stepData$handle,stepData)
  })
  storeWorkflow(unique(workflowDf$workflowHandle)[1],workflowDf)
  return(workflowDf)
}
