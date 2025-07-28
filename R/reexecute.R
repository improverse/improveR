#' rerunSteps
#' @param steps list of steps that have to be rerun
#' @param from the tree the steps are in
#' @param includeLineage include the lineage within the tree
#' @param includeUsage include the usage of the steps
#' @param force execute if no files changed and no link outdated
#' @references ics1219
#' @export
rerunSteps <- function(steps,from=pwd()$parentId,includeLineage = F,includeUsage = T,force=F) {
  improveEditable()
  stepResources <- loadResource(steps,from)

  tree <- loadResource(unique(stepResources$parentId))
  if (nrow(tree)!=1) {
    logging::logwarn("all steps have to be in the same tree")
    return()
  }
  logging::loginfo("rerun steps")
  logging::loginfo(paste(stepResources$entityId,collapse = ", "))
  workflowHandle <- handlesFromTree(tree)
  workflow <- retrieveWorkflow(workflowHandle)

  makeStepsRelative(workflow)
  workflow <- retrieveWorkflow(workflowHandle)
  #TODO multiTree workflows
  selectedWorkflow <- workflow[workflow$entityId %in% stepResources$entityId,]

  if (includeUsage) {
    selectedWorkflow <- byNotEmptyAsDf(selectedWorkflow,function(stepData) {
      return(usage(workflow,stepData$handle))
    })
    selectedWorkflow<- dplyr::distinct(selectedWorkflow,selectedWorkflow$handle,.keep_all = T)
    logging::loginfo("usage added, list of executed steps")
    logging::loginfo(selectedWorkflow$entityId)
  }

  if (includeLineage) {
    selectedWorkflow <- byNotEmptyAsDf(selectedWorkflow,function(stepData) {
      return(lineage(workflow,stepData$handle))
    })
    selectedWorkflow<- dplyr::distinct(selectedWorkflow,selectedWorkflow$handle,.keep_all = T)
    logging::loginfo("lineage added, list of executed steps")
    logging::loginfo(selectedWorkflow$entityId)
  }
  executeHandle <- makeStepsAbsolute(workflow,selectedWorkflow)
  executeWorkflow <- retrieveWorkflow(executeHandle)
  reexecute(executeWorkflow,force)
}

  #' rerunTrees reruns one or more trees, reexecutes all steps
  #' @param trees list of steps that have to be rerun
  #' @param from the tree the steps are in
  #' @references ics1219
  #' @export
  rerunTrees <- function(trees,from=pwd()$parentId) {
    improveEditable()
    tree <- loadResource(trees,from)
    if (!is.data.frame(tree) || nrow(tree)==0) {
      logging::logwarn("Tree not found")
      logging::logwarn(trees)
      return()
    }
    if (nrow(tree)>1) {
      byNotEmpty(tree,function(t) {
        rerunTrees(t)
      })
      return()
    }

    logging::loginfo("rerun tree")
    logging::loginfo(tree$entityId)
    workflowHandle <- handlesFromTree(tree)
    workflow <- retrieveWorkflow(workflowHandle)

    makeStepsRelative(workflow)
    workflow <- retrieveWorkflow(workflowHandle)
    reexecute(workflow,T)
  }


#' rerunChangedAndOutdated
#' @param ident the tree that needs to be rerun
#' @param from pwd for defining the tree relativ
#' @param includeUsage include the usage of the steps
#' @param force execute if no files changed and no link outdated
#' @param lastSteps a list of entityIDs of steps that should be executed in the end. Those steps may not have usage
#' @references ics1219
#' @export
rerunChangedAndOutdated <- function(ident=pwd()$parentId, from=pwd(),includeUsage=T,force=F,lastSteps=NULL) {
  improveEditable()
  tree <- loadResource(ident,from)
  if (!is.data.frame(tree) || nrow(tree)==0) {
    logging::logwarn("Tree not found")
    logging::logwarn(ident)
    return()
  }
  if (nrow(tree)>1) {
    byNotEmpty(tree,function(t) {
      rerunChangedAndOutdated(t,from,includeUsage = includeUsage,force = force,lastSteps = lastSteps)
    })
    return()
  }
  logging::loginfo(
    paste("rerunChangedAndOutdated", tree$path)
  )
  workflowHandle <- handlesFromTree(tree)



  workflow <- retrieveWorkflow(workflowHandle)
  makeStepsRelative(workflow)
  workflow <- retrieveWorkflow(workflowHandle)
  if (!is.null(lastSteps)) {
    lastStepResources <- loadResource(lastSteps)
    dependentWorkflow <- workflow[workflow$entityId %in% lastStepResources$entityId,]
    normalWorkflow <- workflow[!(workflow$entityId %in% lastStepResources$entityId),]
    if ("usage" %in% names(dependentWorkflow) && any(!is.na(dependentWorkflow$usage))) {
      logging::logwarn("defined lastSteps used in other workflows")
      return()
    }
    dependencies <- paste(normalWorkflow$handle,collapse=",")
    e<-byNotEmpty(dependentWorkflow,function(dep) {

      setStepValue(dep$handle,"dependencies",dependencies)
    })
    e <- byNotEmpty(normalWorkflow,function(us) {
      f <- byNotEmpty(dependentWorkflow,function(dep) {

        addStepValue(us$handle,"usage",dep$handle)
      })
    })
  }

  workflow <- retrieveWorkflow(workflowHandle)

  outdatedSteps <- outdatedAndChangedSteps(workflow)
  if (nrow(outdatedSteps)==0) {
    logging::loginfo("Tree up to date")
    return()
  }
  logging::loginfo("following steps need to be reexecuted")
  logging::loginfo(outdatedSteps$entityId)
  if (includeUsage) {
    logging::loginfo("include Usage")
    outdatedSteps <- byNotEmptyAsDf(outdatedSteps,function(stepData) {
      return(usage(workflow,stepData$handle))
    })
    outdatedSteps<- dplyr::distinct(outdatedSteps,outdatedSteps$handle,.keep_all = T)
    logging::loginfo("usage added, full list of executed steps")
    logging::loginfo(outdatedSteps$entityId)
  }
  executeHandle <- makeStepsAbsolute(workflow,outdatedSteps)
  executeWorkflow <- retrieveWorkflow(executeHandle)
  reexecute(executeWorkflow,force)
}

outdatedAndChangedSteps <- function(workflow) {
  return(
    byNotEmptyAsDf(workflow,function(stepData) {
      if (isStepOutdatedOrChanged(stepData)) {
        return(stepData)
      }
    })
  )
}

isStepOutdatedOrChanged <- function(stepData) {
  if (isStepOutdated(stepData)) {
    return(T)
  }
  step <- getStepResource(stepData$handle)
  if (isStepChanged(step)) {
    return(T)
  }
  return(F)
}

isStepOutdated <- function(stepData) {
  step <- getStepResource(stepData$handle)
  unloadChildResources(step)
  inventory <- getStepInventory(stepData$handle,recurse=T)$data[[1]]
  outdatedLinks <- getOutdatedLinks(inventory)
  return(nrow(outdatedLinks)>0)
}

reexecute <- function(workflow,force=F) {
  orderedWorkflow <- executionOrder(workflow)
  executionList <- c()
  for (i in 1:nrow(orderedWorkflow)) {
    nextData <- orderedWorkflow[i,]
    nextItem <- nextData$handle

    if ("dependencies" %in% names(nextData) && !is.na(nextData$dependencies)) {
      dependencies <- unique(strsplit(nextData$dependencies,",")[[1]])
      for (j in 1:length(dependencies)) {
        dependency <- dependencies[j]
        if (dependency %in% executionList) {
          logging::loginfo("waiting to finish")
          finishRun(dependency)
          executionList <- executionList[executionList!=dependency]
        }
      }
    }
    if (force || isStepOutdatedOrChanged(nextData)) {
      logging::loginfo("Executing step")
      logging::loginfo(nextData$entityId)
      updateStep(nextItem)
      runStep(nextItem)
      executionList <- c(executionList,nextItem)
      #finishRun(nextItem)
    }
  }
  if (length(executionList)>0) {
    for (i in 1:length(executionList)) {
      finishRun(executionList[i])
    }
  }
}


updateStep <- function(stepHandle) {
  step <- getStepResource(stepHandle)
  unloadChildResources(step)
  inventory <- getStepInventory(stepHandle,recurse = T)$data[[1]]
  #use only the direct access data
  outdatedLinks <- getOutdatedLinks(inventory)
  byNotEmpty(outdatedLinks,function(link) {
    resource <- loadResource(link)
    process <- loadProcessesForStep(step)
    variables <- loadProcessVariables(process$id)

    variableName <- NULL
    variable <- variables[variables$valueResourceId==resource$resourceId,]
    if (nrow(variable)==1) {
      variableName<-variable$name
    }
    link <- updateLink(link)
    if (!is.null(variableName)) {
      result <- authenticatedREST("/resources/{resourceId}/processes/{processId}/variables/{variableId}",
                                                urlParams = list(resourceId=step$resourceId,
                                                                 processId=process$id,
                                                                 variableId=variable$id),
                                                data = list(type= "processVariable",
                                                            id= variable$id,
                                                            name= variableName,
                                                            position= 1,
                                                            valueResourceId=link$resourceId,
                                                            variableType="fileRef"),
                                                restType = "PUT")
    }
  })
}

updateLink <- function(resource) {
  delete(resource)
  link <-createLink(resource$parentId,links = resource$targetEntityId,linkName = resource$inventoryPath)
  #linkListResponse <- authenticatedREST("/resources/{resourceId}",
  #                                                  urlParams = list(resourceId=resource$resourceId))
  #linkList <- httr::content(linkListResponse)
  #linkListResponse <- authenticatedREST("/resources/{resourceId}",
  #                                                    urlParams = list(resourceId=resource$resourceId),
  #                                                    queryParams = list(updateLink="true"),
  #                                                    data = linkList,
  #  restType = "PUT")
  return(link)
}

getOutdatedLinks <- function(inventory) {
  links <- inventory[inventory$nodeType=="Link",]
  if (nrow(links)==0) {
    return(links)
  }
  targetEntityIds <- links$targetEntityId
  lapply(targetEntityIds, function(id) {
    targetEntities <- unloadResource(id)
  })
  targetEntities <- loadResource(targetEntityIds)
  linkTargets<-links %>%
    dplyr::select("targetRevisionId","targetEntityId","resourceId") %>%
    dplyr::rename(revisionId=.data$targetRevisionId,
                  entityId=.data$targetEntityId,
                  id=.data$resourceId) %>%
    merge(targetEntities,by=c("revisionId","entityId"))

  outdatedLinks <- links[!(links$resourceId %in% linkTargets$id),]
  return(outdatedLinks)
}


isStepChanged <- function(ident,from=pwd()) {
  step <- loadResource(ident,from)
  processes <- loadProcessesForStep(step)
  processes <- processes[processes$selected,]
  lastProcess <- processes[processes$position==max(processes$position),]
  runResponse <- authenticatedREST("/resources/{resourceId}/processes/{processId}/runs/",
                                                 urlParams = list(resourceId=step$resourceId,
                                                                  processId=lastProcess$id))
  runs <- mergeNestedListToDataframe(httr::content(runResponse))
  lastStopDate <- max(runs$stoppedAt)
  inventory <- getStepResourceInventory(step,recurse=T)$data[[1]]
  changed <- inventory[inventory$lastModifiedOn>lastStopDate,]
  if (nrow(changed)==0) {
    files <- inventory[inventory$nodeType=="File",]
    if (nrow(files)>0) {
      for (i in 1:nrow(files)) {
        f <- files[i,]
        auditTrail <- actualLoadAuditTrail(f)
        auditTrail <- auditTrail[auditTrail$operation=="update",]
        changed <- auditTrail[auditTrail$createdAt>lastStopDate,]
        if (nrow(changed)>0) {
          return(T)
        }
      }
    }
  } else {
    return(T)
  }
  return(F)
}


makeStepsAbsolute <- function(handleList,executeList) {
  wfHandle <- uuid::UUIDgenerate()
  executeList$workflowHandle<-wfHandle
  #TODO copy steps but after indexing
  storeWorkflow(wfHandle,executeList)

  if (nrow(handleList)>0 && nrow(executeList)>0){

    handleIndex <- list()
    for (i in 1:nrow(handleList)) {
      handle <- handleList[i,]
      handleIndex[handle$handle]<-list(handle)
    }
    executeIndex <- list()
    for (i in 1:nrow(executeList)) {
      handle <- executeList[i,]
      executeIndex[handle$handle]<-list(handle)
    }

    for (i in 1:length(executeList)) {
      handle <- executeList[i,]
      remoteFiles <- handle$remoteFiles[[1]]
      for (j in 1:length(remoteFiles)) {
        remoteFile <- remoteFiles[j,]
        if ("sourceHandle" %in% names(remoteFile) && !is.na(remoteFile$sourceHandle)) {

          if (!is.null(handleIndex[remoteFile$sourceHandle][[1]]) && is.null(executeIndex[remoteFile$sourceHandle][[1]])) {
            source <- handleIndex[remoteFile$sourceHandle][[1]]
            sourceStep <- loadResource(source$entityId)
            path <- paste(sourceStep$path,remoteFile$name,sep="/")
            removeStepRemoteFile(handle$handle,remoteFile$name)
            addStepRemoteFile(handle$handle,ident = path,asLink = T,variableName = handle$variableName,name=remoteFile$name)
            toRemoveDependency <- source$handle
            existingDependencies <- getStepValue(handle$handle,"dependencies")
            if (!is.na(existingDependencies) && !is.null(existingDependencies) && existingDependencies!="") {
              exDeps <- strsplit(existingDependencies,",",fixed = T)[[1]]
              exDeps <- exDeps[exDeps!=toRemoveDependency]
              if (length(exDeps)>0) {
                setStepValue(handle$handle,"dependencies",paste(exDeps,collapse = ","))
              } else {
                setStepValue(handle$handle,"dependencies",NA)
              }
            }

            #usage should also be removed in stepHandle
          }
        }
      }
    }


  }
  storeWorkflow(wfHandle,executeList)
  return(wfHandle)
}


#' makeStepsRelative
#' takes all steps in a list of steps and makes them relative to each other
#' @param workflow a dataframe of stepHandles that have already been executed
#' @references ics1220
#' @export
makeStepsRelative <- function(workflow) {

  if (is.character(workflow)) {
    workflow <- retrieveWorkflow(workflow)
  }

  if (nrow(workflow)>0){

    entityIdIndex <- list()
    for (i in 1:nrow(workflow)) {
      handle <- workflow[i,]
      entityIdIndex[handle$entityId]<-list(handle)
    }

    for (i in 1:nrow(workflow)) {
      handle <- workflow[i,]
      remoteFiles <- handle$remoteFiles[[1]]
      for (j in 1:nrow(remoteFiles)) {
        remoteFile <- remoteFiles[j,]
        if ("ident" %in% names(remoteFile)) {
          parentStep <- findContainerStep(remoteFile$ident)
          if (!is.null(parentStep) && parentStep$entityId!=handle$entityId && !is.null(entityIdIndex[parentStep$entityId][[1]])) {
            filePath <- getRelativePath(parentStep,remoteFile$ident)
            relativeStep <- entityIdIndex[parentStep$entityId][[1]]
            removeStepRemoteFile(handle$handle,ident=remoteFile$ident)
            #removeStepRemoteFile(handle$handle,substr(filePath,3,nchar(filePath)))
            addStepRemoteFile(handle$handle,
                              name=remoteFile$name,
                              asLink = T,
                              variableName = handle$variableName,
                              sourceHandle = relativeStep$handle,
                              sourceName = substr(filePath,3,nchar(filePath)))
          }
        }
      }
    }

  }

  return(unique(workflow$workflowHandle))

}
