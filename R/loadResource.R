resourceCacheList <- list(
  resourceIdCache="resourceId",
  resourcePathCache="path",
  resourceEntityIdCache="entityId",
  resourceEntityVersionIdCache="entityVersionId"
  #,resourceRelativePathCache=function(resource,path) {
  #  return("NULL")
  #}
  )

resourceVersionCacheList <- list(
  versionedresourceEntityVersionIdCache="entityVersionId"
  )


#' Load Resource
#' @description Loads one or multiple resources.
#' @inheritParams common_ident
#' @param from Path working directory.
#' @returns A data frame with the fields `resourceId`, `entityId`, `entityVersionId`,
#' `path`, `name`, and `data`.
#' @export
#' @references ics1090
#' @details
#' ## ident
#' There are multiple ways to describe the ident of a resource:
#' 
#' Absolute idents:
#' * resourceId: a UUID
#' * Data frame: uses the resourceId value of the data frame
#' * entityId: pointer to the latest version of a resource. Short and long entityIds are accepted'
#' * entityVersionId: pointer to a specific version of a resource. Short and long entityIds are accepted'
#' * path: the full path to a resource, always starting with /.
#' 
#' Relative idents:
#' * All relative idents are path based, they always have to start with ./ or ../'
#' * Relative path without pwd: always starts from the return value of pwd()'
#' * Relative path and pwd as second argument: starts the relative path from the
#' absolute ident that was handed over as second argument. Sometimes still called
#' “from” but will be updated to pwd.  
#'## pwd
#'pwd shows the current “path working directory”. The improveR client is inspired
#'by a command line interface. The default start position within the improve repository
#'is the step that started an R instance with improveR.This starting point is set
#'via the IMPROVER_STEP environment variable. If this information was not
#'provided the root element is pwd. pwd  is used if you use relative paths to
#'access an element.
#' @examples
#' \dontrun{
#' loadResource("112EE78F4CDC4400836F8C059AF2EA5F") #resourceId
#' loadResource("your_server:ST-63657") #entityId
#' loadResource("your_server:ST-63657-1") #entityVersionId
#' loadResource("/projects/folder/analysis_tree/Step 3") #path
#' }
loadResource <- function(ident, from = pwd()) {

  ident <- validate_ident(ident)
  if (is.null(ident)) {return(NULL)}

  multiResource <- F
  if (is.data.frame(ident)) {
    multiResource <- nrow(ident) > 1
    ident <- ident$resourceId
  } else {
    multiResource <- length(ident) > 1
  }
  if (multiResource) {
    resources <- lapply(ident, function(resId) {
      loadResource(resId, from)
    })
    return(mergeDataframeList(resources))
  }
  ident <- getCorrectId(ident)
  if (as.character(ident) == "0" | as.character(ident) == "root:root-root") {
    return(getRoot())
  }
  res <- NULL

  if (grepl("/", ident, fixed = T) | grepl("\\", ident, fixed = T)) {

    ident <- normalisePath(ident, startPath = from)
    logging::logdebug("path recognized")
    logging::logdebug(ident)
    res <- getFromCache(ident, loadResourceByPathGeneric, resourceCacheList)
  }
  else if (isEntityVersionId(ident)) {
    logging::logdebug("resource version specifier")
    logging::logdebug(ident)
    res <- getFromCache(ident, internalLoadResourceVersionFromServer, resourceVersionCacheList)
  }
  else {
    res <- getFromCache(ident, internalLoadResourceFromServer,
                        resourceCacheList)
  }
  return(res)
}

isEntityVersionId <- function(entityVId) {
  entityParts <- strsplit(entityVId,":")[[1]]
  if (length(entityParts)==2) {
    entityParts <- strsplit(entityParts[2],"-")[[1]]
    if(length(entityParts)==3) {
      return(T)
    }
  }
  return(F)
}

#QUESTION invalidatesRepoducibility with default input F; in loadREsourceServer default is T
internalLoadResourceFromServer <- function(identifier) {
  return(loadResourceFromServer(resourceId = identifier,invalidatesReproducibility = F))
} 

internalLoadResourceVersionFromServer <- function(identifier) {
  return(loadResourceVersionFromServer(entityVersionId = identifier,invalidatesReproducibility = F))
}

#' Unload Resource
#' @description Unloads a resource.
#' @param ident id
#' @param from pwd for relative path
#' @references ics1090
#' @export
unloadResource <- function(ident,from=pwd()) {
  res <- loadResource(ident,from)
  if (!is.null(res)) {
    if (res$isVersion) {
      removeFromCache(res$entityVersionId,"",resourceVersionCacheList)
    } else {
      removeFromCache(res$entityId,"",resourceCacheList)
    }
  }
}

#' Update Resource
#' @description Updates a resource.
#' @param ident id
#' @param from pwd for relative path
#' @references ics1090
#' @export
updateResource <- function(ident,from=pwd()) {
  res <- loadResource(ident,from)
  if (!is.null(res)) {
    unloadResource(ident,from)
    res <- loadResource(ident,from)
    return(res)
  }
  return(res)
}

#' Is Resource Up2 Date
#' @description Checks if resource is up to date. 
#' @param ident id
#' @param from pwd for relative path
#' @export
isResourceUp2Date <- function(ident,from=pwd()) {
  res <- loadResource(ident,from)
  if (res$isVersion) {
    logging::logwarn("Versions are always up 2 date")
    logging::logwarn(paste0(ident," is a version ID"))
    return(TRUE)
  }
  serverResource <- loadResourceFromServer(res$resourceId)
  return(serverResource$entityVersionId==res$entityVersionId)
}


#' Common Documentation for ident Parameter
#'
#' This parameter is used to uniquely identify objects across functions.
#'
#' @name common_ident
#' @param ident id
#' @keywords internal
#'
#' @section Details ident:
#' There are multiple ways to describe the ident of a resource:
#'
#' Absolute idents:
#' * resourceId: a UUID
#' * Data frame: uses the resourceId value of the data frame
#' * entityId: pointer to the latest version of a resource. Short and long entityIds are accepted'
#' * entityVersionId: pointer to a specific version of a resource. Short and long entityIds are accepted'
#' * path: the full path to a resource, always starting with /.
#' 
#' Relative idents:
#' * All relative idents are path based, they always have to start with ./ or ../'
#' * Relative path without pwd: always starts from the return value of pwd()'
#' * Relative path and pwd as second argument: starts the relative path from the
#' absolute ident that was handed over as second argument. Sometimes still called
#' “from” but will be updated to pwd.  
NULL
