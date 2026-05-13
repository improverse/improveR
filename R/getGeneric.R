#' Get a File Object
#'
#' @param ident Path, resource, or entity ID of the file.
#'
#' @param addAsLink Creates a link in the inventory if TRUE; improveClean must be run at the end.
#' @param from Used for relative paths. By default, pwd is used (initiated with the step that started improveR).
#' @param caption By default, entity ID and last modified are the caption; alternative text can be provided here.
#' @param description By default, the filename is the description; alternative text can be provided here.
#' @param folderName By default, `data` is the subfolder in the workspace where the file is created.
#' @param addIdToName By default, T prepends the entity ID to the name to avoid collisions.
#' @references ics1141
#' @export
getFile <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="",folderName = "data",addIdToName = T,refresh = FALSE) {
  if (refresh) .refreshGetCaches(ident, from)
  return(
    getAbstract(ident=ident,from = from,addAsLink = addAsLink,addIdToName = addIdToName,caption = caption,description=description,folderName = folderName,func=getDesc)
  )
}

#' Get a Local Copy of a File
#'
#' @param ident Path, resource, or entity ID of the file.
#'
#' @param from Used for relative paths. By default, pwd is used (initiated with the step that started improveR).
#' @param caption By default, entity ID and last modified are the caption; alternative text can be provided here.
#' @param description By default, the filename is the description; alternative text can be provided here.
#' @references ics1141
#' @export
getCopy <- function(ident,from=pwd(),caption="",description="",refresh = FALSE) {
  if (refresh) .refreshGetCaches(ident, from)
  return(
    getAbstract(ident=ident,from = from,addAsLink = F,addIdToName=F,caption = caption,description=description,folderName = ".",func=getDesc)
  )
}

#' Retrieve a List of Files from a Folder
#'
#' @param ident Path, resource, or entity ID of the folder (one folder at a time).
#'
#' @param from Used for relative paths. By default, pwd is used (initiated with the step that started improveR).
#' @param filePattern Filter applied to the file name (example: `*.r`).
#' @param recurse If TRUE, nested folders are also parsed. Defaults to FALSE.
#' @references ics1141
#' @export
#' @importFrom rlang .data
getFilesFromFolder <- function(ident, from=pwd(),filePattern="", recurse=F) {
  fileChildren<-NULL
  resource <- loadResource(ident,from = from )
  if (is.null(resource) || nrow(resource)>1) {
    log_warn(
      "getFilesFromFolder ident must specify exactly one resource", ident
    )
    return(NULL)
  }
  if (resource$nodeType == "Folder" | resource$nodeType == "Step" | resource$nodeType == "Analysis Tree") {
    children <- loadChildResources(resource)$data[[1]]
    if (!is.null(children) && nrow(children)>0) {
      fileChildren <- dplyr::filter(children,.data$nodeType=="File" | .data$nodeType=="Link")
      failed<-F
      if (filePattern!="") {
          tryCatch( {
            fileChildren <- fileChildren[grepl(filePattern,fileChildren$name,ignore.case = T),]
          }, error=function(e) {
            log_warn(e)
            failed<-T
          }
          , warning=function(e) {
            log_warn(e)
            failed<-T
          }
          )

        if(failed) {
          return(NULL)
        }
      }
    }

    #children[children$nodeType=="File",]
    if (recurse) {
      containerChildren <- children[children$nodeType == "Folder" | children$nodeType == "Step" | children$nodeType == "AnalysisTree",]
      returnList <- lapply(containerChildren$resourceId,function(resId) {
        getFilesFromFolder(resId,filePattern = filePattern,recurse = recurse )
      })
      returnList <- plyr::rbind.fill(returnList)
      fileChildren <- plyr::rbind.fill(fileChildren,returnList)
    }
    if (!is.null(fileChildren)) {
      fileChildren <- dplyr::distinct(fileChildren,.data$resourceId,.keep_all = T)
    }
    return(fileChildren)
  } else {
    log_warn(
      "getFilesFromFolder ident must specify a container resource like Step, AnalysisTree or Folder", ident,"specifies a",resource$nodeTyp
    )
    return(NULL)
  }

}

#' abstract function for loading
#'
#' @param ident path, resource or entity ID of the picture
#'
#' @param addAsLink creates a link in inventory if TRUE, warn improveClean has to be run at the end
#' @param addIdToName adds the entity ID to the filename, enity version ID if a specific version is retrieved
#' @param from used for relative paths, by default pwd is used, which is initiated with the step that started improveR
#' @param caption by default entityID and lastModified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @param folderName where to store the file
#' @param func the processing function for this type
#' @param ... pass over arguments for the read function
#' @noRd
#'
getAbstract <- function(ident,from=pwd(),addAsLink=TRUE,addIdToName = TRUE,caption="",description="",folderName,func,...) {
  resource <- loadResource(ident,from)
  if (is.null(resource) || nrow(resource)==0) {
    ### check offline work
    return(NULL)
  }
  dataList <- NULL
  for (i in 1:nrow(resource)) {
    resourceItem <- resource[i,]
    if (resourceItem$nodeType == "File" || resourceItem$nodeType == "Link") {
      isLocalFileFromInventory <- F
      if (startsWith(resourceItem$path,pwd()$path)) {
        localPath <- paste0(".",substr(resourceItem$path,nchar(pwd()$path)+1,nchar(resourceItem$path)))
        isLocalFileFromInventory<- file.exists(localPath)
        if (isLocalFileFromInventory) {
          resourceItem$data<-localPath
        }
      }
      if (!isLocalFileFromInventory){
          resourceItem <- loadFile(resourceItem$resourceId,addIdToName = addIdToName,linkInInventory = addAsLink,filePath = folderName)
      }
      dataLine <- func(resourceItem,caption,description,...)
      if (is.null(dataList)) {
        dataList <- dataLine
      } else {
        dataList <- plyr::rbind.fill(dataLine,dataList)
      }
    } else {
      log_warn("Get functions only available for links and files.",resourceItem$entityId,"is of type",resourceItem$nodeType)
    }
  }
  return(dataList)
}

#' builds a default caption and description
#'
#' @param resourceDesc resource descriptor of the picture
#'
#' @param caption by default entityID and lastModified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @noRd
#'
getDesc <- function(resourceDesc,caption="",description="") {
  resource <- loadResource(resourceDesc$resourceId)
  desc <- buildDescriptor(resource,resourceDesc$data[[1]],caption,description)
  return(desc)
}


#' general function to build the descriptor
#'
#' @param resource resource to be described
#' @param localPath path to the file locally
#'
#' @param caption by default entityID and lastModified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @noRd
#'
buildDescriptor <- function(resource,localPath, caption="",description="") {
  if (caption=="") {
    caption <- buildCaption(resource)
  }
  if (description=="") {
    description <- resource$name
  }
  desc <- data.frame(
    caption=caption,
    path=normalizePath(localPath,winslash = "/"),
    entityId = resource$entityId,
    name=resource$name,
    description=description,
    stringsAsFactors = F
  )
  desc$resource <- list(resource)
  return(desc)
}


#' general function to build the caption for a resource
#'
#' @param resource resource to be described
#' @noRd
#'
buildCaption <- function(resource) {
  caption <- paste0("Source (Entity version ID):",
                    resource$entityVersionId,
                    "\n",
                    "Last modified on: ",
                    convertImproveTimestampToPosix(resource$lastModifiedOn))
  return(caption)
}


#' resets the step by deleting all the downloaded data, if you are using relative paths it makes sense to do this at the beginning of the step
#' it also deletes all the links that were currently created
#' @noRd
#'

resetStep <- function() {
  if(file.exists(".improve.json")) {
    file.remove(".improve.json")
  }
  contentFolders <- list("data","rmd","R","graphics","html","temp","text")
  results <- lapply(contentFolders,function(fol) {
    if(dir.exists(fol)) {
      unlink(fol,recursive = T)
    }
  })
}

#' Invalidate file + resource caches for a single ident. Used by the
#' `refresh = TRUE` path of getFile / getCopy / getGraphics / getHTML /
#' getData / getTextString / getR so the next loadFile() in getAbstract
#' downloads fresh content rather than returning a stale cached copy.
#' @noRd
.refreshGetCaches <- function(ident, from = pwd()) {
  tryCatch({
    invalidateAllFileCaches(ident, from)
    unloadResource(ident, from)
  }, error = function(e) {
    log_warn("refresh: cache invalidation failed: ", conditionMessage(e))
  })
  invisible(NULL)
}

