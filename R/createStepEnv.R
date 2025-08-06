

recursiveNavigate <- function(stepEnv,action,stepDepth,treeDepth) {
  actionEnv <- stepEnv[[action]]
  #here limit step and treeDepth to 0
  actionEnv$load()
  stepDepth <- stepDepth-1
  if (stepDepth==0) {
    invisible(NULL)
  }
  actionSteps <- ls(actionEnv)
  actionSteps <- actionSteps[actionSteps!="load"]
  x<- lapply(actionSteps,function(actionStep) {
    actionStepEnv <- actionEnv[[actionStep]]
    newTreeDepth <- treeDepth
    if (actionStepEnv$stepDf$treeIdent != stepEnv$stepDf$treeIdent) {
      newTreeDepth <- newTreeDepth-1
    }
    if (newTreeDepth!=0) {
      recursiveNavigate(actionStepEnv,action,stepDepth,newTreeDepth)
    }
  })
}



#' Create a new step environment
#'
#' Constructs a new step object with encapsulated state, navigation environments, and public API.
#' @param stepDf Data frame with step metadata
#' @param workflow Workflow environment this step belongs to
#' @return An environment representing the step
#' @export
createStepEnv <- function(stepDf = NULL, workflow = NULL) {
  env <- new.env(parent = emptyenv())
  env$this <- env
  env$stepDf <- stepDf
  env$workflow <- workflow

  # Navigation environments for interactive exploration
  env$lineage <- new.env(parent = emptyenv())
  env$usage <- new.env(parent = emptyenv())
  env$parent <- new.env(parent = emptyenv())
  env$children <- new.env(parent = emptyenv())

  # Private helpers and state
  .step_private <- new.env(parent = emptyenv())

  # Private: get value from stepDf
  .step_private$getStepValue <- function(key) {
    stepList <- env$stepDf
    if (!is.null(stepList) && key %in% names(stepList)) {
      return(as.character(stepList[key]))
    }
    return(NULL)
  }


  env$createTemplate <- function(workflow=NULL) {
    createStepTemplateEnv(treeIdent = env$stepDf$treeIdent, stepDf = env$stepDf, workflow = workflow)
  }


  # Retrieves the main process data frame of a step
  # return Data frame of the main process, or NULL if not found
  env$retrieveMainProcess <- function() {
    stepData <- env$stepDf
    processes <- stepData$processes[[1]]
    if (nrow(processes) > 0 && ("main" %in% processes$processType)) {
      return(processes[processes$processType == "main", ])
    }
    return(NULL)
  }

  # Get the run status of the step
  # return Status string
  env$getStepState <- function() {
    entityId <- .step_private$getStepValue("sourceEntityId")
    step <- internalLoadResourceFromServer(entityId)
    return(step$runStatus)
  }

  # Get the resource object for the step
  # return Resource object
  env$getStepResource <- function() {
    entityId <- .step_private$getStepValue("sourceEntityId")
    if (!is.null(entityId)) {
      return(loadResource(entityId))
    }
    return(NULL)
  }

  # Get the resource object for the step, bypassing cache
  # return Resource object
  env$getStepWithoutCache <- function() {
    entityId <- .step_private$getStepValue("sourceEntityId")
    step <- internalLoadResourceFromServer(entityId)
    return(step)
  }





  #removestep method to workflow
  #add usage,parent an children to load
  #implement fullLineage and fullusage
  #print tree

  # Retrieves4 all files from the inventory of a handle, if a step was created with this handle
  # param recurse If the complete inventory should be retrieved or only the top level
  # param update Unloads the cached resources, default TRUE
  # return Inventory data
  env$getStepInventory <- function(recurse = FALSE, update = TRUE) {
    step <- env$getStepResource()
    return(getStepResourceInventory(step, recurse, update))
  }





  # Navigation envs: add placeholder load functions (to be implemented)
  env$lineage$load <- function(stepDepth=1,treeDepth=1) {
    if (stepDepth==1 && treeDepth==1) {
      step <- env$getStepResource()
      lineageResult <- authenticatedREST("/resources/{resourceId}/dependencies",
                                         urlParams = list(resourceId=step$resourceId))
      if (is.null(lineageResult)) {
        return(NULL)
      }
      if (lineageResult$status_code==200) {
        lineageContent <- httr::content(lineageResult)

        if (length(lineageContent)>0) {
          stepsDf <- env$workflow$df()
          links <- lapply(lineageContent,function(lStep) {
            if (!(lStep$entityId %in% stepsDf$sourceEntityId)) {
              log_info("adding ",lStep$name,lStep$entityId,"to lineage")
              lStepEnv <- getStep(lStep$resourceId,workflow = env$workflow)
              env$lineage[[lStepEnv$stepDf$fullName]]<-lStepEnv
              lStepEnv$usage[[env$stepDf$fullName]]<-env
            }

          })
        }
      }
    } else {
      recursiveNavigate(env,"lineage",stepDepth,treeDepth)
    }

  }
  env$usage$load <- function(stepDepth=1,treeDepth=1) {
    if (stepDepth==1 && treeDepth==1) {
      step <- env$getStepResource()
      usageResult <- authenticatedREST("/resources/{resourceId}/usages",
                                         urlParams = list(resourceId=step$resourceId))
      if (is.null(usageResult)) {
        return(NULL)
      }
      if (usageResult$status_code==200) {
        useageContent <- httr::content(usageResult)

        if (length(useageContent)>0) {
          stepsDf <- env$workflow$df()
          links <- lapply(useageContent,function(lStep) {
            if (!(lStep$entityId %in% stepsDf$sourceEntityId)) {
              log_info("adding ",lStep$name,lStep$entityId,"to usage")
              lStepEnv <- getStep(lStep$resourceId,workflow = env$workflow)
              env$usage[[lStepEnv$stepDf$fullName]]<-lStepEnv
              lStepEnv$lineage[[env$stepDf$fullName]]<-env
            }

          })
        }
      }
    } else {
      recursiveNavigate(env,"usage",stepDepth,treeDepth)
    }
  }



  env$parent$load <- function(stepDepth=1,treeDepth=1) {
    if (stepDepth==1 && treeDepth==1) {
      step <- env$getStepResource()
      parent <- updateParentStep(step)
      if (is.null(parent)) {
        parent <- data.frame()
      }
      parentContent <- ls(env$parent)
      parentContent <- parentContent[parentContent!="load"]
      if (length(parentContent)==0 && nrow(parent)==1) {
        parentStep <- getStep(parent$resourceId,workflow = env$workflow)
        parentStep$children[[env$stepDf$fullName]]<-env
        env$parent[[parentStep$stepDf$fullName]]<-parentStep
      } else if (length(parentContent)==1 && nrow(parent)==1) {
        if (env$parent[[parentContent]]$stepDf$sourceEntityId!=parent$entityId) {
          parentStep <- getStep(parent$resourceId,workflow = env$workflow)
          parentStep$children[[env$stepDf$fullName]]<-env
          env$parent[[parentStep$stepDf$fullName]]<-parentStep
        }
      } else if (length(parentContent)==1 && nrow(parent)==0) {
        rm(list=c(parentContent),pos = env$parent)
      }
    } else {
      recursiveNavigate(env,"parent",stepDepth,treeDepth)
    }

  }
  env$children$load <- function(stepDepth=1,treeDepth=1) {
    if (stepDepth==1 && treeDepth==1) {
      step <- env$getStepResource()
      children <- updateChildSteps(step)$data[[1]]


        if (nrow(children)>0) {
          stepsDf <- env$workflow$df()
          links <- byNotEmpty(children,function(lStep) {
            lStepEnv <- NULL
            if (!(lStep$entityId %in% stepsDf$sourceEntityId)) {
              log_info("adding ",lStep$name,lStep$entityId,"to children")
              lStepEnv <- getStep(lStep$resourceId,workflow = env$workflow)
            } else {
              lStepEnv <- env$workflow$steps[[stepsDf[stepsDf$sourceEntityId==lStep$entityId,]$fullName]]
            }
              env$children[[lStepEnv$stepDf$fullName]]<-lStepEnv
              lStepEnv$parent[[env$stepDf$fullName]]<-env


          })
        }
    } else {
      recursiveNavigate(env,"children",stepDepth,treeDepth)
    }
  }




  stepName <- createStepName(env)
  env$stepDf$fullName <- stepName
  env$workflow$steps[[stepName]]<-env

  # Skip collectInternalLinks during import (will be done after all steps are created)
  if (is.null(env$workflow$isImporting) || !env$workflow$isImporting) {
    .workflow_private$collectInternalLinks(env$workflow)
  }

  return(env)
}
