#' creates a folder in the repository and returns the resource object
#' @param targetIdent one or many target folders, as resource, id or path
#' @param folderName one or many folderNames
#' @param comment defaults to Created by improveRW
#' @references ics1101
#' @export
createFolder <- function(targetIdent,folderName="",comment="Created by improveRW") {
  return(createGeneric(targetIdent,folderName,"Folder",comment=comment))
}

#' creates a file in the repository and returns the resource object
#' @param targetIdent one or many target folders, as resource, id or path
#' @param fileName one or many fileNames
#' @param localPath path to a local file for initial file contents
#' @param comment defaults to Created by improveRW
#' @references ics1102
#' @export
createFile <- function(targetIdent,fileName="",localPath="",comment="Created by improveRW") {
  improveEditable()
  if (!is.character(fileName)) {
    log_warn("name needs to be of type character")
    return(NULL)
  }
  if (!is.character(localPath)) {
    log_warn("localPath needs to be of type character")
    return(NULL)
  }
  if (localPath=="") {
    identList <- resolveImplicitResourceName(targetIdent,folderName = fileName)
    if (is.null(identList)) {
      return(NULL)
    }
    targetIdent <- identList$targetIdent
    fileName <- identList$folderName
  } else {

    if (!all(file.exists(localPath))) {
      log_warn("localPath",localPath[!file.exists(localPath)],"does not exist")
      return(NULL)
    }
    if (fileName=="") {
      fileName<-basename(localPath)
    }
  }
  fileRes <- createGeneric(targetIdent,fileName,"File",comment=comment)
  if (is.null(fileRes)) {
    return(NULL)
  }
  if (localPath!="" && length(localPath)>0) {
    for (i in 1:length(localPath)) {
      localPath<-normalizePath(localPath[i])
      fileRes <- updateFileContent(fileRes[i],localPath[i],comment=comment)
    }
  }
  return(fileRes)
}

#' creates a workflow/analysis tree in the repository and returns the resource object
#' @param targetIdent one or many target folders, as resource, id or path
#' @param treeName one or many treeNames
#' @param comment defaults to Created by improveRW
#' @references ics1103
#' @export
createAnalysisTree <- function(targetIdent,treeName="",comment="Created by improveRW") {
  return(createGeneric(targetIdent,treeName,"Analysis Tree",comment=comment))
}

#' creates an external link in the repository and returns the resource object
#' @param targetIdent one or many target folders, as resource, id or path
#' @param linkName one or many linkNames
#' @param url url for the external link
#' @param comment defaults to Created by improveRW
#' @references ics1104
#' @export
createExternalLink <- function(targetIdent,linkName="",url,comment="Created by improveRW") {
  return(createGeneric(targetIdent,linkName,"ExtLink",url = url,comment=comment))
}

resolveImplicitResourceName <- function(targetIdent,folderName) {
  if (folderName=="") {
    targetSplits <- strsplit(x = targetIdent,split="/",fixed = T)[[1]]
    len <- length(targetSplits)
    if (len<2) {
      logging::logwarn("either full path with name of new resource or target path and resource name have to be provided")
      return(NULL)
    }

    folderName<- targetSplits[len]
    targetIdent<- paste(targetSplits[1:len-1],collapse = "/")
  }
  return(list(targetIdent=targetIdent,folderName=folderName))
}

createGeneric <- function(targetIdent,folderName,type,url="",comment) {
  improveEditable()
  #TODO better path handling
  if (!is.character(folderName)) {
    log_warn("name needs to be of type character")
    return(NULL)
  }
  if (!is.character(comment)) {
    log_warn("comment needs to be of type character")
    return(NULL)
  }
  identList <- resolveImplicitResourceName(targetIdent,folderName)
  if (is.null(identList)) {
    return(NULL)
  }
  targetIdent <- identList$targetIdent
  folderName <- identList$folderName

  target <- loadResource(targetIdent)
  if (is.null(target)) {
    log_warn("Target does not exist")
    return(NULL)
  } else {
    if (nrow(target)>1) {
      return(Map(function(targ) {
        return(createGeneric(targ,folderName,type,url,comment))
      },target$path))
    } else if (nrow(target)==1) {
      if (!isAllowedTarget(target$nodeType,type,logWarning=T)) {
        return(NULL)
      }
      if (length(folderName)>1) {
        return(Map(function(fN) {
          return(createGeneric(target,fN,type,url,comment))
        },folderName))
      } else {
        targetChildren <- loadChildResources(target)$data[[1]]
        exists <- targetChildren[targetChildren$name==folderName,]

        if (nrow(exists)==1) {
          if (exists$nodeType==type) {
            log_info(folderName,"already exists in",target$path)
            return(loadResource(exists))
          }
          log_warn(folderName,"already exists in",target$path,"but is of type",exists$nodeType)
          return(NULL)
        }
        path <- utils::URLencode(target$path,reserved = TRUE)
        data = list(nodeType=type,
                    name=folderName,
                    comment=comment)
        if (type=="ExtLink") {
          data$url=url
        }
        result <- authenticatedREST("/resources",
                                                  queryParams = list(path=path),
                                                  restType = "POST",
                                                  data=data,
                                                  contentType = "application/json",
                                                  encode = "json")
        parsedResult <- httr::content(result)
        unloadChildResources(target)
        res <- loadResource(parsedResult$resourceId)
        return(res)
      }
    } else {
      logging::logwarn(paste0(targetIdent," does not exist"))
    }
  }
}


createGenericFile <- function(targetIdent,folderName,type,localPath,comment) {
  improveEditable()

  target <- loadResource(targetIdent)
  if (nrow(target)>1) {
    return(Map(function(targ) {
      return(createGeneric(targ,folderName,type,url))
    },target$path))
  } else if (nrow(target)==1) {
    if (length(folderName)>1) {
      return(Map(function(fN) {
        return(createGeneric(target,fN,type,url))
      },folderName))
    } else {
      targetChildren <- loadChildResources(target)$data[[1]]
      exists <- targetChildren[targetChildren$name==folderName,]

      if (nrow(exists)==1) {
        logging::loginfo(paste0(folderName," already exists in ",target$path))
        return(loadResource(exists))
      }

      fil <- httr::upload_file(localPath)
      result <- authenticatedREST(
        "/resources/{resourceId}",
        urlParams =  list(resourceId=target$resourceId),
        queryParams = list(comment=comment)
        ,data=list(file=fil),
        encode = NULL
        ,restType = "POST",
        contentType = "multipart/form-data"
      )


      parsedResult <- httr::content(result)
      unloadChildResources(target)
      res <- loadResource(parsedResult$resourceId)
      return(res)
    }
  } else {
    logging::logwarn(paste0(targetIdent," does not exist"))
  }
}
