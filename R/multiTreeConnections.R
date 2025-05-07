
getLineageLinkedStepsinOtherTrees <- function(ident,from=pwd()) {
  lineageStep <- loadResource(ident,from)
  lineageTree <- loadResource(lineageStep$parentId)
  inventory <- getStepResourceInventory(lineageStep,recurse=T)%>%strip()
  if (nrow(inventory)>0) {
    linksInInventory <- inventory[inventory$nodeType=="Link" ,]
    if (nrow(linksInInventory)>0) {
      linkTargets <- loadResource(linksInInventory$targetEntityId)
      externalLinkTargets <- linkTargets[!startsWith(linkTargets$path,lineageTree$path),]
      if (nrow(externalLinkTargets)>0) {
        externalSteps <- byNotEmptyAsDf(externalLinkTargets,function(target) {
          container <- findContainerStep(target)
        })
        if (nrow(externalSteps)>0) {
          return(externalSteps)
        }
      }
    }
  }
  return(NULL)
}

#ident <- "envhost1.hc.scintecodev.internal-5310:ST-62592"
getFullLineage <- function(ident,from=pwd(),workflowEnv=new.env(),depth=-1) {
  targetStep <- loadResource(ident,from)
  if (nrow(targetStep)==1) {
    workflowHandle <- workflowEnv[[targetStep$parentId]]
    if (is.null(workflowHandle)) {
      workflowHandle <- handlesFromTree(targetStep$parentId,from,includeSelf = T)
      workflowEnv[[targetStep$parentId]]<-workflowHandle
    }
    stepHandle <- getHandleForResource(workflowHandle,targetStep)
    return(addMultiTreeLineage(workflowHandle,stepHandle,workflowEnv=workflowEnv,depth))
  }
}

addMultiTreeLineage <- function(workflowHandle,stepHandle,workflowEnv=new.env(),depth=-1) {


    lin <- lineage(workflow = workflowHandle,stepHandle = stepHandle)
    if (depth!=0) {
      depth<-depth-1
      externalLineage <- byNotEmptyAsDf(lin,function(pullLineage) {
        otherSteps <- getLineageLinkedStepsinOtherTrees(pullLineage$entityId)
        return(byNotEmptyAsDf(otherSteps,function(otherStep) {
          return(getFullLineage(otherStep,workflowEnv=workflowEnv,depth=depth))
        }))
      })

      return(
        dplyr::distinct(plyr::rbind.fill(lin,externalLineage),handle,.keep_all = T)
      )
    }
    return(lin)
}


###################USAGE
#ident <- "envhost1.hc.scintecodev.internal-5310:ST-62565"
getUsageLinkedStepsinOtherTrees <- function(ident,from=pwd()) {
  usageStep <- loadResource(ident,from)
  usageTree <- loadResource(usageStep$parentId)
  inventory <- getStepResourceInventory(usageStep,recurse=T)%>%strip()
  if (nrow(inventory)>0) {
    referencedInInventory <- inventory[inventory$nodeType=="File" ,]
    if (nrow(referencedInInventory)>0) {
      referenceLinks <- plyr::rbind.fill(loadReferences(referencedInInventory$resourceId)$data)
      if (nrow(referenceLinks)>0) {
        externalReferenceLinks <- referenceLinks[!startsWith(referenceLinks$path,usageTree$path),]
        if (nrow(externalReferenceLinks)>0) {
          externalSteps <- byNotEmptyAsDf(externalReferenceLinks,function(target) {
            container <- findContainerStep(target)
          })
          if (nrow(externalSteps)>0) {
            return(externalSteps)
          }
        }
      }
    }
  }
  return(NULL)
}

getFullUsage <- function(ident,from=pwd(),workflowEnv=new.env(),depth=-1) {
  targetStep <- loadResource(ident,from)
  if (nrow(targetStep)==1) {
    workflowHandle <- workflowEnv[[targetStep$parentId]]
    if (is.null(workflowHandle)) {
      workflowHandle <- handlesFromTree(targetStep$parentId,from,includeSelf = T)
      workflowEnv[[targetStep$parentId]]<-workflowHandle
    }
    stepHandle <- getHandleForResource(workflowHandle,targetStep)
    return(addMultiTreeUsage(workflowHandle,stepHandle,workflowEnv=workflowEnv,depth))
  }
}

addMultiTreeUsage <- function(workflowHandle,stepHandle,workflowEnv=new.env(),depth=-1) {


  used <- usage(workflow = workflowHandle,stepHandle = stepHandle)
  if (depth!=0) {
    depth<-depth-1
    externalUsage <- byNotEmptyAsDf(used,function(pullUsage) {
      otherSteps <- getUsageLinkedStepsinOtherTrees(pullUsage$entityId)
      return(byNotEmptyAsDf(otherSteps,function(otherStep) {
        return(getFullUsage(otherStep,workflowEnv=workflowEnv,depth=depth))
      }))
    })

    return(
      dplyr::distinct(plyr::rbind.fill(lin,externalUsage),handle,.keep_all = T)
    )
  }
  return(lin)
}

