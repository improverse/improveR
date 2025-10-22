

multiplexResourceFunction <- function(func,multiArgument,...) {
  improveEditable()
  if (is.data.frame(multiArgument) && nrow(multiArgument)==0) {
    return(NULL)
  }
  if (length(multiArgument)==0) {
    return(NULL)
  }
  res <- loadResource(multiArgument)
  if (is.null(res)) {
    log_warn("Source",multiArgument,"does not exist, could not execute")
    return(NULL)
  } else if (!is.null(res) && nrow(res)>1) {
    return(mergeListToDataframe(
      Map(function(sarg) {
      return(func(sarg,...))
    },res$path)))
  } else {
    return(func(res,...))
  }
}



singleChange <- function(source,target,changeFunction,targetName="",overwrite=F,comment) {

  if (!is.character(targetName)) {
    log_warn("name needs to be of type character")
    return(NULL)
  }
  if (!is.character(comment)) {
    log_warn("comment needs to be of type character")
    return(NULL)
  }
  sourceR <- loadResource(source)
  targetR <- loadResource(target)

  if (is.null(sourceR)) {
    log_warn("Source",source,"does not exist, could not",changeFunction)
    return(NULL)
  }
  if (is.null(targetR)) {
    log_warn("Target",target,"does not exist, could not",changeFunction)
    return(NULL)
  }
  if (!isAllowedTarget(targetR$nodeType,sourceR$nodeType,logWarning=T)) {
    return(NULL)
  }
  targetFolderId <- targetR$resourceId
  if (targetR$nodeType %in% c("File","Link")) {
    targetFolderId <- getParent(targetR)
    targetName<-targetR$name
  }
  if (targetName=="") {
    targetName <- sourceR$name
  }

  checkTarget <- loadChildResources(targetFolderId)

  if (targetName %in% checkTarget$data[[1]]$name) {
    if (overwrite) {
      children <- checkTarget$data[[1]]
      children <- children[children$name==targetName,]
      delete(children)
    } else {
      log_warn(paste(checkTarget$path,targetName,sep = "/"),"already exists, cannot",changeFunction)
      return(NULL)
    }
  }

  result <- authenticatedREST('/resources/{resourceId}/{changeFunction}',
                                            urlParams = list(resourceId=sourceR$resourceId,
                                                             changeFunction=changeFunction
                                            ),
                                            queryParams = list(targetId=targetFolderId,
                                                               newName=targetName,
                                                               comment=comment
                                            ),
                                            data=list(),
                                            restType = "POST")
  unloadResource(targetFolderId)
  unloadChildResources(targetFolderId)
  unloadChildResources(sourceR$parentId)
  copiedRes <- updateResource(sourceR$resourceId)
  resultId <- httr::content(result)$resourceId
  copiedRes <- loadResource(resultId)
  return(copiedRes)
}

singleCopy <- function(source,target,targetName=NULL,overwrite=F,comment) {
  return(singleChange(source,target,"copy",targetName,overwrite,comment))
}

singleMove <- function(source,target,targetName=NULL,overwrite=F,comment) {
  res <- singleChange(source,target,"move",targetName,overwrite,comment)
  invalidatePathCaches(source$path)
  return(res)
}

#' copies resources
#' multiple sources can be copied at once
#' if multiple sources are copied target needs to be a container and targetName empty
#' if only one source is copied, a new name can be specified, this can be done via a path or via targetName
#' @param sources the files to be copied
#' @param target the target folder
#' @param targetName name for the copied file
#' @param overwrite flag if a file already existing at the target location should be overwritten if existing. If flag is false and file exists NULL is returned
#' @param comment comment for the commit, defaults to "modified by improveRW"
#' @references ics1139
#' @export
copy <- function(sources,target,targetName="",overwrite=F,comment="modified by improveRW") {
  return(
    multiplexResourceFunction(func=singleCopy,
                            multiArgument = sources,
                            target=target,
                            targetName=targetName,
                            overwrite=overwrite,
                            comment=comment)
  )
}

#' moves resources
#' multiple sources can be moved at once
#' if multiple sources are moved target needs to be a container and targetName empty
#' if only one source is moved, a new name can be specified, this can be done via a path or via targetName
#' @param sources the files to be moved
#' @param target the target folder
#' @param targetName name for the moved file
#' @param overwrite flag if a file already existing at the target location should be overwritten if existing. If flag is false and file exists NULL is returned
#' @param comment comment for the commit, defaults to "modified by improveRW"
#' @references ics1139
#' @export
move <- function(sources,target,targetName="",overwrite=F,comment="modified by improveRW") {
  return(
    multiplexResourceFunction(func=singleMove,
                              multiArgument = sources,
                              target=target,
                              targetName=targetName,
                              overwrite=overwrite,
                              comment=comment)
  )
}

singleDelete <- function(resId) {
  resource <- loadResource(resId)
  if (is.null(resource)) {
    return(F)
  }
  result <- authenticatedREST('/resources/{resourceId}',
                                            urlParams = list(resourceId=resource$resourceId
                                            ),
                                            restType = "DELETE")
  invalidatePathCaches(resource$path,deleteLinkedFiles = T)
  if (is.null(resource$parentId)) {
    resource$parentId <-"/"
  }
  #unloadResource(resource$resourceId)
  unloadChildResources(resource$parentId)
  return(result$status_code==200)
}

#' Delete Resource(s)
#' `delete()` takes takes the ident of one or multiple resources and 
#' deletes them. Resouces can be, e.g., folders, analysis trees, steps,
#' or files.
#' @param res the resource(s) to be deleted
#' @references ics1139
#' @export
delete <- function(res) {
  result <- multiplexResourceFunction(func=singleDelete,
                                      multiArgument = res)
  if (is.null(result)) {
    return(F)
  }
  return(result)
}

singleUpdateFileContent <- function(ident,localPath,comment) {
  resource <- loadResource(ident)
  fileContent <- httr::upload_file(localPath)
  fResult <- authenticatedREST(
    "/resources/{resourceId}/content",
    urlParams =  list(resourceId=resource$resourceId),
    queryParams = list(comment=comment)
    ,data=list(file=fileContent),
    encode = NULL,
    restType = "PUT"
  )
  res <- httr::content(fResult)
  resource <- updateResource(res[[1]]$resourceId)
  return(resource)
}

#' updates file content,
#' prerequisite, must be a file
#' multiple files can be updated at once
#' @param ident the resources to be updated
#' @param localPath the path to the file with the new fileContent
#' @param comment comment for the commit, defaults to "modified by improveRW"
#' @references ics1210
#' @export
updateFileContent <- function(ident,localPath,comment="modified by improveRW") {
  return(
    multiplexResourceFunction(func=singleUpdateFileContent,
                              multiArgument = ident,
                              localPath=localPath,
                              comment=comment)
  )
}


#' uploads a complete folder
#' prerequisite, must be a folder
#' @param targetIdent the target resource
#' @param localFolder the path to the file with the new fileContent
#' @param comment comment for the commit, defaults to "modified by improveRW"
#' @references ics1210
#' @export
uploadFolder <- function(targetIdent,localFolder,comment="modified by improveRW") {
  if (dir.exists(localFolder)) {
    remoteFolder <- createFolder(targetIdent = targetIdent,
                                                 folderName = basename(localFolder),
                                                 comment = comment)
    localFolderHandle  <- dir(localFolder,full.names = T)
    if (length(localFolderHandle)>0) {
      for (i in 1:length(localFolderHandle)) {
        subFile <- localFolderHandle[i]
        if (dir.exists(subFile)) {
          uploadFolder(remoteFolder,subFile)
        } else {
          createFile(remoteFolder,
                                     localPath = normalizePath(subFile),
                                     comment = comment)
        }
      }
    }
  } else {
    log_warn(localFolder, "is not a folder")
  }

}
