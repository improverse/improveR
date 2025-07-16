this <- NULL

steps <- new.env()
files <- new.env()
linkTargets <- NULL
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
    inventoryDf$stepEntityId <- task$entityId
    inventoryDf$ownedByName <- task$ownedByName
    return(inventoryDf)
  })
  completeInventory <- mergeDataframeList(completeInventory)


  changedAndOutdated <- dplyr::filter(completeInventory,outdatedLink==T | stoppedAt<lastModified)
  return(changedAndOutdated)
}

collectLinkTargets <- function() {
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
    this$linkTargets <- allTargets
  }
  #linkRes <- improveR::loadResource("repo_name_todo:LI-2531")
  #data <- list(nodeType="Link",name=linkRes$name,comment="update outdated")
  #improveR::authenticatedREST("/resources/{resourceId}",queryParams = list(updateLink="true"),urlParams = list(resourceId=linkRes$resourceId),restType = "PUT",data = data)

}

