this <- NULL

steps <- new.env()
files <- new.env()
internalLinks <- NULL
outputFiles <- new.env()
df <- function() {
  stepNames <- data.frame(fullName = ls(this$steps))
  stepDf <- byNotEmptyAsDf(stepNames,function(stepName) {
    fullName<-stepName$fullName
    df <- this$steps[[fullName]]$stepDf
    df$fullName <- fullName
    return(df)
  })
}
library(magrittr)

#ask for working copies?
#ticket, add changed flag to dmg

changedAndOutdatedFiles <- function(tree=NULL) {

  if (is.null(tree)) {
    workflowDf <- this$df()
    trees <- unique(workflowDf$treeIdent)
    result <- lapply(trees,changedAndOutdatedFiles)
    result <- mergeDataframeList(result)
    return(result)
  }


  dmgResult <- authenticatedREST("/resources/{resourceId}/dmg",
                                 list(resourceId=loadResource(tree)$resourceId),queryParams = list(depth=2))
  dmg <- httr::content(dmgResult)

  flattenInventoryEntries <- function(taskInventory) {
    entryList <- lapply(taskInventory,function(inventoryEntry) {
      if (inventoryEntry$nodeType=="FOV") {
        return(flattenInventoryEntries(inventoryEntry$children))
      }
      if (is.null(inventoryEntry$targetId)) {inventoryEntry$targetId<-NA}
      if (is.null(inventoryEntry$outdatedLink)) {inventoryEntry$outdatedLink<-NA}
      entryDf <- data.frame(nodeType=inventoryEntry$nodeType,
                            entityId=inventoryEntry$entityId,
                            entityVersionId=inventoryEntry$entityVersionId,
                            fileName=inventoryEntry$fileName,
                            lastModified=as.numeric(strptime(inventoryEntry$lastModified, format = "%Y-%m-%dT%H:%M:%S%z")),
                            targetId=inventoryEntry$targetId,
                            outdatedLink=inventoryEntry$outdatedLink,
                            stringsAsFactors = F)
      return(entryDf)
    })
    return(entryList)
  }

  completeInventory <- lapply(dmg$tasks,function(task) {
    if (task$runStatus=="FINISHED")
      taskInventory <- task$inventory
    #flatten inventory
    entryList <-flattenInventoryEntries(taskInventory)
    inventoryDf <- mergeListToDataframe(entryList)
    inventoryDf$stoppedAt<-as.numeric(strptime(task$stoppedAt, format = "%Y-%m-%dT%H:%M:%S%z"))
    inventoryDf$stepEntityId <- improveR::loadResource(task$entityId)$entityId
    inventoryDf$ownedByName <- task$ownedByName
    return(inventoryDf)
  })
  completeInventory <- mergeDataframeList(completeInventory)


  changedAndOutdated <- dplyr::filter(completeInventory,outdatedLink==T | stoppedAt<lastModified)
  return(changedAndOutdated)
}

collectInternalLinks <- function() {
  stepsDf <- this$df()
  remoteFiles <- improveR:::byNotEmptyAsDf(stepsDf,function(step) {
    rf <- step$remoteFiles[[1]]
    rf$targetStep <- step$fullName
    return(rf)
  })
  remoteFiles <- remoteFiles %>% dplyr::mutate(entityId=ident) %>%
    dplyr::filter(asLink) %>%
    dplyr::select(entityId,targetStep)

  links <- improveR::loadResource(remoteFiles$entityId) %>%
    dplyr::left_join(remoteFiles)
  fullSteps <- improveR::loadResource(stepsDf$sourceEntityId)
  fullSteps <- dplyr::mutate(fullSteps,fullName=stepsDf[stepsDf$sourceEntityId==entityId,]$fullName)
  stepPaths <- fullSteps$path
  allTargets <- NULL
  if (length(stepPaths)>0 && length(stepPaths)) {
    for (i in 1:length(stepPaths)) {
      stepPath <- stepPaths[i]
      foundTargets <- links[startsWith(links$path,stepPath),]
      if (nrow(foundTargets)>0) {
        stepName <- fullSteps[fullSteps$path==stepPath,]$fullName
        foundTargets$sourceStep <- stepName
        allTargets <- plyr::rbind.fill(allTargets,foundTargets)
      }
    }
    this$internalLinks <- allTargets
  }
  #linkRes <- improveR::loadResource("repo_name_todo:LI-2531")
  #data <- list(nodeType="Link",name=linkRes$name,comment="update outdated")
  #improveR::authenticatedREST("/resources/{resourceId}",queryParams = list(updateLink="true"),urlParams = list(resourceId=linkRes$resourceId),restType = "PUT",data = data)

}

createReexecutionPlan <- function() {

  stepsDf <- this$df()
  caof <- this$changedAndOutdatedFiles()
  this$collectInternalLinks()
  iL <- this$internalLinks

  allStepsToExecute <- stepsDf[stepsDf$sourceEntityId %in% unique(caof$stepEntityId),]
  usingSteps <- stepsDf[stepsDf$fullName %in% this$getInternalUsage(allStepsToExecute$fullName),]

  allSteps <- rbind(allStepsToExecute,usingSteps) %>%
    dplyr::distinct(fullName,.keep_all = T)

  #addLinksToUpdate
  #TODO different entityId formats
  executionPlan <- byNotEmptyAsDf(allSteps,function(st) {
    outDatedLinksDf <- dplyr::filter(caof,nodeType=="LIV" & stepEntityId==st$sourceEntityId)
    outDatedLinks <- NULL
    if (nrow(outDatedLinksDf)>0) {
      outDatedLinks <- outDatedLinksDf%>%
        dplyr::pull(entityId) %>%
        improveR::loadResource() %>%
        dplyr::pull(entityId)
    }
    internalLinks <- this$internalLinks[this$internalLinks$targetStep==st$fullName,]
    linkIds <- this$getLinkTarget(internalLinks)
    allUpdates <- Filter(function(x){return(!is.null(x))}, unique(c(outDatedLinks,linkIds)))
    st$toUpdate <- list(allUpdates)
    usingSteps <- this$internalLinks[this$internalLinks$sourceStep==st$fullName,]$targetStep
    usedSteps <- this$internalLinks[this$internalLinks$targetStep==st$fullName,]$sourceStep
    st$lineage <- paste(usedSteps,collapse = ",",sep = "/")
    if (st$lineage=="") {st$lineage<-NA}
    st$usage <- paste(usingSteps,collapse = ",",sep="/")
    if (st$usage=="") {st$usage<-NA}
    return(st)
  })
  executionPlan <- dplyr::select(executionPlan,
                                 description,
                                 rationale,
                                 sourceEntityId,
                                 sourceName,
                                 fullName,
                                 toUpdate,
                                 lineage,
                                 usage)
  executionPlan$inPlace<-T
  return(executionPlan)
}

rerunChangedAndOutdated <- function() {
  plan <- this$createReexecutionPlan()
  this$executePlan(plan)
}

executePlan <- function(executionPlan) {

  orderedWorkflow <- this$executionOrder(executionPlan)

  executionList <- c()
  for (i in 1:nrow(orderedWorkflow)) {
    nextData <- orderedWorkflow[i,]
    nextItem <- nextData$fullName

    if ("lineage" %in% names(nextData) && !is.na(nextData$lineage)) {
      dependencies <- unique(strsplit(nextData$lineage,",")[[1]])
      for (j in 1:length(dependencies)) {
        dependency <- dependencies[j]
        if (dependency %in% executionList) {
          logging::loginfo("waiting to finish")

          improveR::finishRunResource(this$steps[[dependency]]$stepDf$sourceEntityId)
          executionList <- executionList[executionList!=dependency]
        }
      }
    }
    #inplace flag
    #write executionList to file
    if ("toUpdate" %in% names(nextData) && !is.null(nextData$toUpdate[[1]])) {
      this$updateLinks(nextData$toUpdate[[1]])
    }
    if ("inPlace" %in% names(nextData) && nextData$inPlace) {
      improveR::runStepResource(nextData$sourceEntityId)
    } else {
      stepEnv <- this$steps[[nextData$fullName]]
      stepEnv$realise()
    }


    executionList <- c(executionList,nextItem)
  }
  if (length(executionList)>0) {
    for (i in 1:length(executionList)) {
      improveR::finishRunResource(this$steps[[executionList[i]]]$stepDf$sourceEntityId)
    }
  }

}


#' creates a executable order following the dependencies
#' One workflow can span multiple analsis trees
#'
#' @param workflow the workflow to get ordered
#' @references ics1231
#' @export
executionOrder <- function(plan) {
  return(
    this$executionOrderInternal(plan)
  )
}

executionOrderInternal <- function(plan,startSteps=NULL,counter=0) {
  counter <- counter+1
  if (!"lineage" %in% names(plan)) {
    plan$lineage<-NA
  }
  if (!"usage" %in% names(plan)) {
    plan$usage<-""
  }
  if (is.null(startSteps)) {
    startSteps <- plan[is.na(plan$lineage),]
    plan <- plan[!is.na(plan$lineage),]
  }
  if (is.null(startSteps) || nrow(startSteps)==0) {
    logging::logwarn("No step without dependency, no executable order")
    return(NULL)
  }
  for (s in 1:nrow(startSteps)) {
    startStep <- startSteps[s,]
    lineages <- strsplit(startStep$usage,",",fixed = T)[[1]]
    if (length(lineages)>0) {
      for(i in 1:length(lineages)) {
        lineage <- lineages[i]
        lineageHandle <- plan[plan$fullName==lineage,]
        if (nrow(lineageHandle)==1 && "lineage" %in% names(lineageHandle)) {
          dependencies <- strsplit(lineageHandle$lineage,",",fixed = T)[[1]]
          if(all(dependencies %in% startSteps$fullName)) {
            startSteps <- plyr::rbind.fill(startSteps,lineageHandle)
            plan <- plan[plan$fullName!=lineage,]
          }
        }
      }
    }
  }
  if (nrow(plan)==0 || counter > 500) {
    if (counter>500) {
      logging::logwarn("could not add all steps to execution order, check for cycles")
    }
    return(startSteps)
  }
  return(this$executionOrderInternal(plan,startSteps,counter))
}


getLinkTarget <- function(internalLink) {
  if (nrow(internalLink)==0) {
    return(NULL)
  } else if (nrow(internalLink)>1) {
    return(improveR:::byNotEmpty(internalLink,this$getLinkTarget))
  }
  stepsDf <- this$df()
  targetStep <- stepsDf[stepsDf$fullName==internalLink$targetStep,]
  remoteFiles <- targetStep$remoteFiles[[1]]
  linkFile <- remoteFiles[remoteFiles$ident==internalLink$entityId,]
  linkResource <- improveR::loadResource(linkFile$name,targetStep$sourceEntityId)
  return(linkResource$entityId)
}

getInternalUsage <- function(fullName) {
  if (length(fullName)==0) {
    return(NULL)
  } else if (length(fullName)>1) {
    return(lapply(fullName,this$getInternalUsage))
  }
  stepsDf <- this$df()
  usingSteps <- this$internalLinks[this$internalLinks$sourceStep==fullName,]$targetStep
  if (length(usingSteps)>0) {
    return(c(usingSteps,this$getInternalUsage(usingSteps)))
  }
  return(NULL)
}


updateLinks <- function(links) {

  if (length(links)==0) {
    invisible(NULL)
  } else if (length(links)>1) {
    invisible(lapply(links,this$updateLinks))
  }

  linkRes <- improveR::loadResource(links)
  data <- list(nodeType="Link",name=linkRes$name,comment="update outdated")
  improveR::authenticatedREST("/resources/{resourceId}",queryParams = list(updateLink="true"),urlParams = list(resourceId=linkRes$resourceId),restType = "PUT",data = data)
  invisible(NULL)
}
