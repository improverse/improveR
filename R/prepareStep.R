
timing <- function(...) {}




#' changes the step description
#'
#' @param ident ident of the step
#' @param from from if a relative path is used
#' @param description new step desciption
#' @references ics1217
#' @export

changeStepDescription <- function(ident, from=pwd(),description) {
  stepEntity <- loadResource(ident,from)
  stepEntity$description<- description

  result <- authenticatedREST("/resources/{resourceId}/",
                                  urlParams = list(resourceId=stepEntity$resourceId),
                                  data=as.list(stepEntity),
                                  restType = "PUT"
  )
  return(updateResource(ident,from))
}

#' changes the step rationale
#'
#' @param ident ident of the step
#' @param from from if a relative path is used
#' @param rationale new step rationale
#' @references ics1217
#' @export

changeStepRationale <- function(ident, from=pwd(),rationale) {
  stepEntity <- loadResource(ident,from)
  stepEntity$rationale<- rationale

  result <- authenticatedREST("/resources/{resourceId}/",
                                            urlParams = list(resourceId=stepEntity$resourceId),
                                            data=as.list(stepEntity),
                                            restType = "PUT"
  )
  return(updateResource(ident,from))
}

getToolId <- function(runserverName, runserverToolName) {
  runservers <- loadRunservers()
  runserver <- runservers[runservers$label==runserverName,]

  tools  <- loadToolsForRunserver(as.character(runserver$id))
  tool <- tools[tools$name==runserverToolName,]
  return(as.character(tool$id))
}


addExtLinkToStep <- function(newStep, filePrep) {
  timing("prepareRemoteStart")


  if (is.null(filePrep)) {
    return()
  }

  createTarget <- newStep




  fileName <- filePrep$name
  if (is.na(fileName)) {
    fileName <-"External Link"
  }
  if (grepl(pattern = "/", x=fileName,fixed = T)) {
    pathParts <- strsplit(x=fileName,split="/",fixed=T)[[1]]
    if (length(pathParts)!=2) {
      logging::logwarn("maximum folder depth allowed is 1, by filename in realise step")
      logging::logwarn(fileName)
      return()
    }
    folderName <- pathParts[1]
    fileName<- pathParts[2]
    children <- loadChildResources(newStep)
    folder <- children[children$name==folderName,]
    if (nrow(folder)==1 && folder$nodeType!="Folder") {
      logging::logwarn(folderName)
      logging::logwarn("already exists but not as folder")
      return()
    }
    if (nrow(folder)==1) {
      createTarget<-folder
    } else {
      createTarget <- createFolder(newStep,folderName=folderName)
    }
  }

  newFile <- NULL

    if (is.na(filePrep$url)) {
      return()
    }
    newFile <- createExternalLink(targetIdent = createTarget,linkName = basename(filePrep$name),url = filePrep$url)

  timing("created")

}


