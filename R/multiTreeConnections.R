
#' getLineageLinkedStepsinOtherTrees
#' returns all steps in other trees that are referenced via link from a specific step
#'
#' @param ident ident of the step
#' @param from from for relative pathes, default is pwd()
#'
#' @export
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

#' getFullLineage
#' returns a workflowHandle for the lineage workflow of a step, covering multiple trees.
#'
#' @param ident ident of the step
#' @param from from for relative pathes, default is pwd()
#' @param workflowEnv internal variable, leave NULL
#' @param depth, the depth of recursion when retrieving additional trees. -1 takes all trees into account
#'
#' @export
getFullLineage <- function(ident,from=pwd(),workflowEnv=NULL,depth=-1) {
  userCall <- F
  if (is.null(workflowEnv)) {
    workflowEnv <- new.env()
    userCall <- T
  }
  targetStep <- loadResource(ident,from)
  if (nrow(targetStep)==1) {
    workflowHandle <- workflowEnv[[targetStep$parentId]]
    if (is.null(workflowHandle)) {
      workflowHandle <- handlesFromTree(targetStep$parentId,from,includeSelf = T)
      workflowEnv[[targetStep$parentId]]<-workflowHandle
    }
    stepHandle <- getHandleForResource(workflowHandle,targetStep)
    resultWorkflow <-addMultiTreeLineage(workflowHandle,stepHandle,workflowEnv=workflowEnv,depth)
    if (userCall) {
      resultWorkflow <- deepWorkflowCopy(resultWorkflow,preserveEntityId = T)
    }
    return(resultWorkflow)
  }
}

#' addMultiTreeLineage
#' internal recursion function for getFullLineage
#'
#' @param workflowHandle the retrieved workflow
#' @param stepHandle the currently investigated step
#' @param workflowEnv internal variable
#' @param depth, the depth of recursion when retrieving additional trees. -1 takes all trees into account

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


#' getUsageLinkedStepsinOtherTrees
#' returns all steps in other trees that reference a specific step via link
#'
#' @param ident ident of the step
#' @param from from for relative pathes, default is pwd()
#'
#' @export
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

#' getFullUsage
#' returns a workflowHandle for the usage workflow of a step, covering multiple trees.
#'
#' @param ident ident of the step
#' @param from from for relative pathes, default is pwd()
#' @param workflowEnv internal variable, leave NULL
#' @param depth, the depth of recursion when retrieving additional trees. -1 takes all trees into account
#'
#' @export
getFullUsage <- function(ident,from=pwd(),workflowEnv=NULL,depth=-1) {
  userCall <- F
  if (is.null(workflowEnv)) {
    workflowEnv <- new.env()
    userCall <- T
  }
  targetStep <- loadResource(ident,from)
  if (nrow(targetStep)==1) {
    workflowHandle <- workflowEnv[[targetStep$parentId]]
    if (is.null(workflowHandle)) {
      workflowHandle <- handlesFromTree(targetStep$parentId,from,includeSelf = T)
      workflowEnv[[targetStep$parentId]]<-workflowHandle
    }
    stepHandle <- getHandleForResource(workflowHandle,targetStep)
    resultWorkflow <- addMultiTreeUsage(workflowHandle,stepHandle,workflowEnv=workflowEnv,depth)
    if (userCall) {
      resultWorkflow <- deepWorkflowCopy(resultWorkflow,preserveEntityId = T)
    }
    return(resultWorkflow)
  }
}

#' addMultiTreeUsage
#' internal recursion function for getFullUsage
#'
#' @param workflowHandle the retrieved workflow
#' @param stepHandle the currently investigated step
#' @param workflowEnv internal variable
#' @param depth, the depth of recursion when retrieving additional trees. -1 takes all trees into account
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
      dplyr::distinct(plyr::rbind.fill(used,externalUsage),handle,.keep_all = T)
    )
  }
  return(used)
}

