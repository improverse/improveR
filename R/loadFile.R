fileResourceCacheList <- createCacheList("file")

cacheEnv$fileCaches <- list()

getFileCache <- function(filePath,addIdToName) {
  fileCacheName <- glue::glue("file-{filePath}-{addIdToName}")
  if (!(fileCacheName %in% names(cacheEnv$fileCaches))) {
    newCache <- createCacheList(fileCacheName)
    cacheEnv$fileCaches[[fileCacheName]]<-newCache
  }
  return(cacheEnv$fileCaches[[fileCacheName]])
}

getFileVersionCache <- function(filePath,addIdToName) {
  fileCacheName <- glue::glue("fileVersion-{filePath}-{addIdToName}EntityVersionIdCache")
  if (!(fileCacheName %in% names(cacheEnv$fileCaches))) {
    newCache <- list ()
    newCache[[fileCacheName]]<-"entityVersionId"
    cacheEnv$fileCaches[[fileCacheName]]<-newCache
  }
  return(cacheEnv$fileCaches[[fileCacheName]])
}

fileResourceVersionCacheList <- list(
  fileResourceEntityIdCache="entityVersionId"
)

#' Load File
#' @description Loads a file by its resourceId, entity ID, or entity version ID.
#' Uses caching. The results are returned as a data frame or a list of data frames.
#' The dates are converted to POSIX dates with the convertImproveTimestampToPosix function.
#' resourceId can be a list.
#' @param  ident the resource id or the entity id of the resource
#' @param from used if a relative path is used
#' @param filePath local Path where the file should be stored, relative to rootPath, normally wd
#' @param addIdToName logical, if the entityId should be added to the filename
#' @param linkInInventory logical, if TRUE a link to the resource is created in the inventory
#' @references ics1099
#' @seealso [convertImproveTimestampToPosix()]
#' @export
loadFile <- function(ident,from=pwd(),filePath=".",addIdToName=FALSE,linkInInventory=FALSE) {
  resources <- loadResource(ident,from)
  if(is.null(resources)) {
    return(NULL)
  }
  versionRes <- resources[resources$isVersion,]
  noversionRes <- resources[!resources$isVersion,]

  res1<-versionLoadFile(versionRes,
                        from=from,
                        filePath=filePath,
                        addIdToName=addIdToName,
                        linkInInventory=linkInInventory)
  res2<-unversionLoadFile(noversionRes,
                        from=from,
                        filePath=filePath,
                        addIdToName=addIdToName,
                        linkInInventory=linkInInventory)
  return(rbind(res1,res2))
}

versionLoadFile <- function(resources,from,filePath,addIdToName,...) {
  if (nrow(resources)==0) {
    return(NULL)
  } else if (nrow(resources)>1) {
    entityIds <- resources$entityVersionId
    resourceValues <- Map(function(resId) {
      loadFile(resId,from,...)
    },entityIds)
    return(
      resourceValues
    )
  }

  fC <- getFileVersionCache(filePath,addIdToName)

  res <- getFromCache(resources$entityVersionId,loadFileFromServer,fC,filePath=filePath,addIdToName=addIdToName,...)
  return(res)
}

unversionLoadFile <- function(resources,from,filePath,addIdToName,...) {
  if (nrow(resources)==0) {
    return(NULL)
  } else if (nrow(resources)>1) {
    entityIds <- resources$entityId
    resourceValues <- Map(function(resId) {
      loadFile(resId,from,filePath,addIdToName,...)
    },entityIds)
    return(
      resourceValues
    )
  }

  fC <-getFileCache(filePath,addIdToName)

  res <- getFromCache(resources$entityId,loadFileFromServer,fC,filePath=filePath,addIdToName=addIdToName,...)
  return(res)
}

#QUESTION: check description text.
#' unloadFile
#' @description Removes a file from cache.
#' @inheritParams common_ident
#' @inheritSection common_ident Details ident

#' @param from used if a relative path is used
#' @param filePath local Path where the file should be stored, relative to rootPath, normally wd
#' @param addIdToName logical, if the entityId should be added to the filename
#' @references ics1099
#' @export
unloadFile <- function(ident,from=pwd(),filePath=".",addIdToName=FALSE) {
  res <- loadResource(ident,from)
  if (!is.null(res)) {

    f <- loadFile(ident,from,filePath=filePath,addIdToName=addIdToName)
    unloadResource(ident,from)

    path <- unlink(f$data[[1]],recursive = T)

    if (res$isVersion) {
      fC <- getFileVersionCache(filePath,addIdToName)
      removeFromCache(f$entityVersionId,"",fC)
    } else {
      fC <-getFileCache(filePath,addIdToName)
      removeFromCache(f$entityId,"",fC)
    }
  }
}


#' updateFile
#' @description Retrieves5 the latest version of the file from the repository.
#' @inheritParams common_ident
#' @inheritSection common_ident Details ident

#' @param from Used if a relative path is used.
#' @param filePath Local path where the file is stored (relative to rootPath).
#' @param addIdToName Logical; if TRUE the entity id is added to the filename.
#' @param linkInInventory Logical; if TRUE, creates a link to the resource in inventory.
#' @references ics1099
#' @export
updateFile <- function(ident, from = pwd(), filePath = ".",
             addIdToName = FALSE, linkInInventory = FALSE) {
  unloadFile(ident, from, filePath = filePath,
       addIdToName = addIdToName)
  res <- loadFile(ident, from, filePath,
          addIdToName, linkInInventory)
  return(res)
}

#' isFileUp2Date
#' @description Checks if a new version of the file exists in the repository.
#' @inheritParams common_ident
#' @inheritSection common_ident Details ident

#' @param from Used if a relative path is used.
#' @references ics1099
#' @export
isFileUp2Date <- function(ident,from=pwd()) {
  res <- loadResource(ident,from)
  if (res$isVersion) {
    logging::logwarn("Versions are always up 2 date")
    logging::logwarn(paste0(ident," is a version ID"))
    return(TRUE)
  }
  f <- loadFile(ident,from)
  serverResource <- loadResourceFromServer(res$resourceId)
  return(serverResource$entityVersionId==f$entityVersionId)
}


loadFileFromServer <- function(resource,filePath,addIdToName,linkInInventory) {

  genericLoadFromServer(resource,name="file",funct=actualLoadFile,filePath=filePath,addIdToName=addIdToName,linkInInventory=linkInInventory)
}


actualLoadFile <- function(resource,filePath,addIdToName,linkInInventory) {

  if (resource$nodeType=="Folder" || resource$nodeType=="Step" || resource$nodeType=="Tree") {
    resChildren <- loadChildResources(resource)$data[[1]]
    newPath <- filePath
    resF<-NULL
    if (!addIdToName) {
      newPath <- paste(filePath,resource$name,sep="/")
      fullPath<- file.path(newPath)
      if (!file.exists(fullPath)) {
        dir.create(fullPath,recursive = TRUE)
      }
      resF <- genericLoadFromServer(resource = resource,
                                                   name = resource$name,
                                                   funct = function(...){return(normalizePath(newPath,winslash = "/"))})
      resF$type="folder"
    }
    childResources <- loadFile(resChildren$resourceId,filePath=newPath,addIdToName=addIdToName,linkInInventory=linkInInventory)
    if (!is.null(resF)) {
      childResources <- plyr::rbind.fill(childResources,resF)
    }
    return(childResources)
  } else {
    if (addIdToName) {
      idString <- resource$entityId
      if (resource$isVersion) {
        idString <- resource$entityVersionId
      }
      fName <-paste0(gsub(":","_",idString,fixed = TRUE),"_",toString(resource$name))
    } else {
      fName <- toString(resource$name)
    }
    fullPath<- file.path(filePath)
    if (!file.exists(fullPath)) {
      dir.create(fullPath,recursive = TRUE)
    }
    fPath <-  file.path(fullPath,fName)
    if (!file.exists(fPath)) {
      if (resource$nodeType=="Link") {
        resource$revisionId <- resource$targetRevisionId
        resource$resourceId <- resource$targetId
      }
      logging::logdebug(paste0("download: ",fPath))
      f <- file.create(fPath)
      f <- file(fPath, "wb")
      fResult <- authenticatedREST("/revisions/{revisionId}/resources/{resourceId}/content",
                                             list(resourceId=resource$resourceId,
                                                  revisionId=resource$revisionId)
      )
      fContent <- httr::content(fResult,as="raw")
      writeBin(fContent,f)
      close(f)
      if (linkInInventory) {
        #targetFolder <- loadResourceByPath(paste(pwd()$path,filePath,sep="/"))
        #if (!is.null(targetFolder)) {
        #  createLink(resource$resourceId,fName,targetFolder$resourceId)
        #}
        markFileAsLink(resource,filePath,fName,normalizePath(fPath,winslash = "/"))
      }
    }
    return(normalizePath(fPath,winslash = "/"))
  }
}

getImproveJsonPath <- function() {

  return(
    file.path(getRootPath(),".improve.json",fsep = "/")
  )
}

# NOTE Test message:
# Warning (test-improveConnect.R:109:3): improveClose handles file cleanup correctly
# Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
# Please use `"path"` instead of `.data$path`

saveImproveJson <- function() {
  linkJsonFileName <- getImproveJsonPath()
  unlink(linkJsonFileName)
  if (!is.null(cacheEnv$createdLinks)) {
    # saveDF <- dplyr::select(cacheEnv$createdLinks,.data$path,.data$target)
    saveDF <- dplyr::select(cacheEnv$createdLinks,"path","target") #see note above
    saveDF$type<-"link"
    jsonlite::write_json(saveDF,linkJsonFileName)
  }
}
cacheEnv$createdLinks <- NULL

markFileAsLink <- function(resource,pathInInventory,name="",localPath) {


  if (startsWith(localPath,getRootPath())) {
    pathInInventory <- substr(localPath,nchar(getRootPath())+2,nchar(localPath))
  } else {
    log_warn(localPath,"not in inventoryPath",getRootPath())
    return()
  }
  linkData <- data.frame(type="link",path=pathInInventory,target=resource$resourceVersionId,resourceId=resource$resourceId,localPath=localPath)
  if (!is.null(cacheEnv$createdLinks)) {
    cacheEnv$createdLinks <- cacheEnv$createdLinks[cacheEnv$createdLinks$localPath!=localPath,]
  }
  cacheEnv$createdLinks <- plyr::rbind.fill(cacheEnv$createdLinks,linkData)
  saveImproveJson()

}


