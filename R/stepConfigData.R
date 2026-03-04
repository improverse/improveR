defaultKey <- function(...) {
  return("default")
}


runserversCacheList <- list(
  runserversCache=defaultKey
)




actualLoadRunservers <- function(...) {
  return(restGetAsDf("configuration/runservers"))
}


#' loads all registered runservers
#' @references ics1226
#' @noRd
loadRunservers <- function() {
  runservers <- getFromCache(defaultKey,actualLoadRunservers,runserversCacheList,NULL)
  return(runservers)
}

#' Loads Runserver By Label
#' 
#' `loadRunserver()` loads a runserver by its label and returns a dataframe with pertaining details.
#' 
#' @param label the label of the runserver
#'
#' @returns A dataframe with the following columns:
#'   - `id` (character)
#'   - `url` (character)
#'   - `hostname` (character)
#'   - `label` (character)
#'   - `local` (logical)
#'   - `deleted` (logical)
#'   - `generic` (logical)
#'   - `reproducible` (logical)
#'   - `sshPort` (integer)
#' @references ics1226
#' @export
loadRunserver <- function(label) {
  runservers <- loadRunservers()
  runserver<-runservers[runservers$label==label,]
  if (nrow(runserver)==1) {
    return(runserver)
  }
  log_warn(paste0(
    "No or multiple runservers with this label found: ",label)
  )
  return(NULL)
}

#' unloadRunservers
#' @references ics1226
#' @noRd
unloadRunservers <- function() {
  loadRunservers()
  removeFromCache(defaultKey,"",runserversCacheList)
}

#' updateRunservers reloads the runservers from the repository
#' @references ics1226
#' @noRd
updateRunservers <- function() {
  unloadRunservers()
  res <- loadRunservers()
  return(res)
}


runserverToolsCacheList <- list(
  runserverToolsCache="runserverId"
)

#' loadToolsForRunserver
#'
#' @param runserverId resourceId of the runserver
#' @references ics1227
#' @noRd
loadToolsForRunserver <- function(runserverId) {
  runserverTools <- getFromCache(
    runserverId,
    actualLoadToolsForRunserver,
    runserverToolsCacheList,
    NULL
  )
  return(runserverTools)
}

#' loadToolForRunserver
#'
#' @param runserverId resourceId of the runserver
#' @param toolName name of the tool, optional, but toolname or toolInstanceName have to be given
#' @param toolInstanceName name of the tool instance, optional, but toolname or toolInstanceName have to be given
#' @references ics1227
#' @noRd
loadToolForRunserver <- function(runserverId,toolName=NULL,toolInstanceName=NULL) {
  runserverTools <- loadToolsForRunserver(runserverId)
  if (is.null(runserverTools)) {
    return(NULL)
  }
  if (is.null(toolName) && is.null(toolInstanceName)) {
    log_error("toolName or tool instance name have to be given")
    return(NULL)
  } else if (is.null(toolName)) {
    tool<-runserverTools[runserverTools$name==toolInstanceName,]
    if (nrow(tool)==1) {
      return(tool)
    }
    log_error(paste0(toolInstanceName," not unique or does not exist"))
    return(NULL)
  } else if (is.null(toolInstanceName)) {
    tool<-runserverTools[runserverTools$toolName==toolName,]
    if (nrow(tool)==1) {
      return(tool)
    }
    log_error(paste0(toolName," not unique or does not exist"))
    return(NULL)
  } else  {
    runserverTools<-runserverTools[runserverTools$toolName==toolName,]
    tool<-runserverTools[runserverTools$name==toolInstanceName,]
        if (nrow(tool)==1) {
      return(tool)
    }
    log_error(paste0(toolInstanceName," and ",toolName," not unique or does not exist"))
    return(NULL)
  }
}

actualLoadToolsForRunserver <- function(runserverId) {
  result <- authenticatedREST(
    'configuration/runservers/{runserverId}/tools',
    urlParams = list(runserverId = runserverId),
    restType = "GET"
  )

  if (is.null(result)) {
    log_warn("no tools found for runserver:", runserverId)
    return(NULL)
  }
  tools <- httr::content(result)
  toolsDf <- mergeListToDataframe(tools)

  catTools <- loadAllTools()

  colnames(catTools)[colnames(catTools) == 'id'] <- 'toolId'
  colnames(catTools)[colnames(catTools) == 'name'] <- 'toolName'
  
  #TODO
  ##remove below
  catTools %>%
    dplyr::filter(stringr::str_detect(toolName, stringr::regex("\\bR")))
  toolsDf %>% dplyr::filter(stringr::str_detect(name, stringr::regex("\\bR"))) #2 R tools present
  ##remove above

  #browser()
  fullTools <- merge(catTools, toolsDf, by = "toolId") #6 rows get lost

  runservers <- loadRunservers()
  colnames(runservers)[colnames(runservers) == 'id'] <- 'runserverId'
  fullTools <- merge(runservers, fullTools, by = "runserverId")

  return(fullTools)
}


#' unloadToolsForRunserver
#' @param runserverId resourceId of the runserver
#' @references ics1227
#' @noRd
unloadToolsForRunserver <- function(runserverId) {
  loadToolsForRunserver(runserverId)
  removeFromCache(runserverId,"",runserverToolsCacheList)
}

#' updateToolsForRunserver reloads the runserver tools from the repository
#' @param runserverId resourceId of the runserver
#' @references ics1227
#' @noRd
updateToolsForRunserver <- function(runserverId) {
  unloadToolsForRunserver(runserverId)
  res <- loadToolsForRunserver(runserverId)
  return(res)
}
