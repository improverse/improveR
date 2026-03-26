#' Create Folder in improve Repository
#'
#' Creates one or multiple folders in the repository at the specified location.
#'
#' @param targetIdent Identifier(s) of the parent location where the new folder(s)
#'   should be created. Can be a path, resource ID, or entity ID.
#' @param folderName Character. Name(s) of the new folder(s).
#' @param comment Character. Comment for the creation audit entry. Defaults to
#'   "Created by improveR".
#'
#' @returns A list containing the resource object(s) of the created folder(s).
#'   Each resource object contains metadata like `resourceId`, `entityId`, `path`, etc.
#'   Returns `NULL` on failure.
#'
#' @references ics1101
#' @seealso \code{\link{createFile}}, \code{\link{createAnalysisTree}}
#'
#' @examples
#' \dontrun{
#' # Create a folder in the repository root
#' createFolder(targetIdent = "/", folderName = "folderInRoot")
#'
#' # Create a folder inside an existing folder by path
#' createFolder(
#'   targetIdent = "/improve-tutorial/",
#'   folderName = "folderInImproveTutorial"
#' )
#' }
#' @export
createFolder <- function(
  targetIdent,
  folderName = "",
  comment = "Created by improveR"
) {
  return(createGeneric(targetIdent, folderName, "Folder", comment = comment))
}

#' Create File in improve Repository
#'
#' Creates a file resource in the repository, optionally uploading content from a
#' local file.
#'
#' @param targetIdent Identifier(s) of the parent folder(s) where the file should
#'   be created. Can be a path, resource ID, or entity ID.
#' @param fileName Character. Name of the file to be created. If empty and
#'   `localPath` is provided, the basename of `localPath` is used.
#' @param localPath Character. Path to a local file whose content will be uploaded.
#'   If empty, an empty file resource is created (metadata only).
#' @param comment Character. Comment for the creation audit entry. Defaults to
#'   "Created by improveR".
#'
#' @returns A list containing the resource object(s) of the created file(s).
#'   Returns `NULL` on failure.
#'
#' @references ics1102
#' @seealso \code{\link{createFolder}}, \code{\link{updateFileContent}}
#' @export
createFile <- function(
  targetIdent,
  fileName = "",
  localPath = "",
  comment = "Created by improveR"
) {
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
      log_warn("could not resolve target path and file name from:", targetIdent)
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

#' Create Analysis Tree in improve Repository
#'
#' Creates an Analysis Tree, which is a specialized folder structure for organizing
#' pharmaceutical analysis workflows.
#'
#' @param targetIdent Identifier of the parent folder where the analysis tree
#'   should be created.
#' @param treeName Character. Name of the new analysis tree.
#' @param comment Character. Comment for the creation audit entry. Defaults to
#'   "Created by improveR".
#'
#' @returns A list containing the resource object of the created analysis tree.
#'   Returns `NULL` on failure.
#'
#' @references ics1103
#' @seealso \code{\link{createFolder}}, \code{\link{createWorkflow}}
#' @export
createAnalysisTree <- function(
  targetIdent,
  treeName = "",
  comment = "Created by improveR"
) {
  return(createGeneric(
    targetIdent,
    treeName,
    "Analysis Tree",
    comment = comment
  ))
}

#' Create External Link in improve Repository
#'
#' Creates a link resource pointing to an external URL (outside the repository),
#' such as a database, website, or external documentation system.
#'
#' @param targetIdent Identifier of the parent folder, analysis tree, or step
#'   where the link should be created.
#' @param linkName Character. Name of the link resource.
#' @param url Character. The destination URL (e.g., "https://example.com" or "db://server").
#' @param comment Character. Comment for the creation audit entry. Defaults to
#'   "Created by improveR".
#'
#' @returns A list containing the resource object of the created external link.
#'
#' @references ics1104
#' @seealso \code{\link{createFile}}
#' @export
createExternalLink <- function(
  targetIdent,
  linkName = "",
  url,
  comment = "Created by improveR"
) {
  return(createGeneric(
    targetIdent,
    linkName,
    "ExtLink",
    url = url,
    comment = comment
  ))
}

resolveImplicitResourceName <- function(targetIdent,folderName) {
  if (folderName=="") {
    targetSplits <- strsplit(x = targetIdent,split="/",fixed = T)[[1]]
    len <- length(targetSplits)
    if (len<2) {
      log_warn("either full path with name of new resource or target path and resource name have to be provided")
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
        if (is.null(result)) {
          log_warn("Failed to create resource at path:", path)
          return(NULL)
        }
        parsedResult <- httr::content(result)
        unloadChildResources(target)
        res <- loadResource(parsedResult$resourceId)
        return(res)
      }
    } else {
      log_warn(paste0(targetIdent," does not exist"))
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
        log_info(paste0(folderName," already exists in ",target$path))
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
    log_warn(paste0(targetIdent," does not exist"))
  }
}
