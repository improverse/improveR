#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


stepCache <- new.env()

#' prepare step
#'
#' @param treeIdent id of the containing tree
#'
#' @export
#' @references ics1140
#' @importFrom rlang .data
prepareStep <- function(treeIdent) {
  stepHandle <- paste0(uuid::UUIDgenerate())
  storeStep(stepHandle = stepHandle,stepList = data.frame(handle=stepHandle,stringsAsFactors = F))
  setStepTree(stepHandle,treeIdent)
}

storeStep <- function(stepHandle,stepList) {
  assign(stepHandle,stepList,envir=stepCache)
}

#' retrieves complete step description
#'
#' @param stepHandle id of the prepared step
#' @references ics1140
#' @export
retrieveStep <- function(stepHandle) {
  get0(stepHandle,envir=stepCache)
}

setStepValue <- function(stepHandle,key,value) {
  stepList <- retrieveStep(stepHandle)
  stepList[key]<-value
  storeStep(stepHandle = stepHandle,stepList=stepList)
  return(stepHandle)
}

setProcessValue <- function(stepHandle,processName,key,value){
  stepData <- retrieveStep(stepHandle)
  processes <- stepData$processes[[1]]
  if (!(processName %in% processes$name)) {
    newProcess <- data.frame(
      name=processName,
      selected=T,
      main=F,
      processType="post",
      position=max(processes$position)+1,
      stringsAsFactors = F)
    processes <- plyr::rbind.fill(newProcess,processes)
  }
  processes <- byNotEmptyAsDf(processes,function(process) {
    if (process$name==processName) {
      process[[key]]<-value
    }
    return(process)
  })
  stepData$processes <- list(processes)
  storeStep(stepHandle = stepHandle,stepList=stepData)
}

getStepValue <- function(stepHandle,key) {
  stepList <- retrieveStep(stepHandle)
  if (key %in% names(stepList)) {
    return(as.character(stepList[key]))
  }
  return(NULL)
}

removeStepValue <- function(stepHandle,key) {
  stepList <- retrieveStep(stepHandle)
  if (key %in% names(stepList)) {
    stepList[key]<-NULL
  }
  storeStep(stepHandle = stepHandle,stepList=stepList)
  return(stepHandle)
}

addStepValue <- function(stepHandle,key,value) {
  stepList <- retrieveStep(stepHandle)
  valueList <- NULL
  if (key %in% names(stepList)) {
    valueList <- stepList[key][[1]][[1]]
  }
  if (typeof(value)=="character") {
    if (is.null(valueList)) {
      valueList <- value
    } else {
      valueList <- paste(valueList,value,sep=",")
    }
    stepList[key]<-valueList
  } else if (is.data.frame(value)) {
    if (is.null(valueList)) {
      valueList <- value
    } else {
      valueList <- plyr::rbind.fill(valueList,value)
    }
    stepList[key]<-tidyr::nest(valueList,data = tidyr::everything())
  } else {
    logging::logwarn(paste0(
      "only character or dataframe allowed in stephandle for key: ",
      key
    ))
  }

  storeStep(stepHandle = stepHandle,stepList=stepList)

  return(stepHandle)
}
