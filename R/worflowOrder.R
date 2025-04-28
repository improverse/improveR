
#' retrieves a data frame holding the lineage of a step within the given workflow.
#' One workflow can span multiple analsis trees
#' the order of the steps is a possible execution order. If no execution order can be built, NULL is returned
#'
#' @param workflow as df
#' @param stepHandle id of the lineage step
#' @references ics1231
#' @export
orderedLineage <- function(workflow,stepHandle) {
  return(
    executionOrder(
      lineage(workflow = workflow,stepHandle = stepHandle)
    )
  )
}

#' retrieves a data frame holding the usage of a step within the given workflow.
#' One workflow can span multiple analsis trees
#' the order of the steps is a possible execution order
#'
#' @param workflow as df
#' @param stepHandle id of the lineage step
#' @references ics1231
#' @export
orderedUsage <- function(workflow,stepHandle) {
  return(
    executionOrder(
      usage(workflow = workflow,stepHandle = stepHandle)
    )
  )
}

#' retrieves a data frame holding the lineage of a step within the given workflow.
#' One workflow can span multiple analsis trees
#' random order returned
#'
#' @param workflow as df
#' @param stepHandle id of the lineage step
#' @references ics1231
#' @export
lineage <- function(workflow,stepHandle) {
  if (is.character(stepHandle)) {
    lin <- retrieveStep(stepHandle)
  } else {
    lin <- stepHandle
  }
  if (is.character(workflow)) {
    workflow <- retrieveWorkflow(workflow)
  }
  if (!"dependencies" %in% names(lin) || is.na(lin$dependencies)) {
    return(lin)
  }

  dependencies <- unique(strsplit(lin$dependencies,",",fixed = T)[[1]])
  for (i in 1:length(dependencies)) {
    lin <- plyr::rbind.fill(lin,lineage(workflow,dependencies[i]))
  }
  lin <- dplyr::distinct(lin,lin$handle,.keep_all = T)
  lin <- executionOrder(lin)
  return(lin)
}

#' retrieves a data frame holding the usage of a step within the given workflow.
#' One workflow can span multiple analsis trees
#' random order returned
#'
#' @param workflow as df
#' @param stepHandle id of the lineage step
#' @references ics1231
#' @export
usage <- function(workflow,stepHandle) {

  if (is.character(stepHandle)) {
    lin <- retrieveStep(stepHandle)
  } else {
    lin <- stepHandle
    stepHandle <- lin$handle
  }
  if (is.character(workflow)) {
    workflow <- retrieveWorkflow(workflow)
  }

  if (!"usage" %in% names(lin) || is.na(lin$usage)) {
    return(lin)
  }

  usages <- unique(strsplit(lin$usage,",",fixed = T)[[1]])
  if (length(usages)>0) {
    for (i in 1:length(usages)) {
      usage <- usages[i]
      if (usage!=stepHandle) {
        lin <- plyr::rbind.fill(lin,usage(workflow,usage))
      }
    }
  }
  lin <- dplyr::distinct(lin,lin$handle,.keep_all = T)
  #lin <- executionOrder(lin)
  return(lin)
}

#' creates a executable order following the dependencies
#' One workflow can span multiple analsis trees
#'
#' @param workflow the workflow to get ordered
#' @references ics1231
#' @export
executionOrder <- function(workflow) {
  return(
    executionOrderInternal(workflow)
  )
}

executionOrderInternal <- function(workflow,startSteps=NULL,counter=0) {
  counter <- counter+1
  if (!"dependencies" %in% names(workflow)) {
    workflow$dependencies<-NA
  }
  if (!"usage" %in% names(workflow)) {
    workflow$usage<-""
  }
  if (is.null(startSteps)) {
    startSteps <- workflow[is.na(workflow$dependencies),]
    workflow <- workflow[!is.na(workflow$dependencies),]
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
        lineageHandle <- workflow[workflow$handle==lineage,]
        if (nrow(lineageHandle)==1 && "dependencies" %in% names(lineageHandle)) {
          dependencies <- strsplit(lineageHandle$dependencies,",",fixed = T)[[1]]
          if(all(dependencies %in% startSteps$handle)) {
            startSteps <- plyr::rbind.fill(startSteps,lineageHandle)
            workflow <- workflow[workflow$handle!=lineage,]
          }
        }
      }
    }
  }
  if (nrow(workflow)==0 || counter > 500) {
    if (counter>500) {
      logging::logwarn("could not add all steps to execution order, check for cycles")
    }
    return(startSteps)
  }
  return(executionOrderInternal(workflow,startSteps,counter))
}
