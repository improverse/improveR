
timing <- function(...) {}




#' Change Step Description
#'
#' Modifies the description field of an existing step, allowing users to update
#' documentation and clarify the purpose of analytical steps as analyses evolve.
#'
#' @param ident Identifier of the step. Can be the step's path, resource (version) id,
#'   full entity (version) id, or short entity (version) id.
#' @param from Root directory for resolving relative paths. Default is \code{pwd()}.
#' @param description New step description text.
#'
#' @return The updated step resource, returned invisibly after cache refresh.
#'
#' @seealso
#' \code{\link{changeStepRationale}} for updating step rationale,
#' \code{\link{getStep}} for retrieving step information
#'
#' @examples
#' \dontrun{
#' # Update step description
#' changeStepDescription(
#'   ident = "/improve-tutorial/Modeling/Step 1",
#'   description = "Initial exploratory analysis"
#' )
#' }
#'
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
  return(refreshResource(ident,from))
}

#' Change Step Rationale
#'
#' Modifies the rationale field of an existing step, allowing users to update
#' the reasoning and justification for why analytical steps were created and
#' included in the workflow.
#'
#' @param ident Identifier of the step. Can be the step's path, resource (version) id,
#'   full entity (version) id, or short entity (version) id.
#' @param from Root directory for resolving relative paths. Default is \code{pwd()}.
#' @param rationale New step rationale text.
#'
#' @return The updated step resource, returned invisibly after cache refresh.
#'
#' @seealso
#' \code{\link{changeStepDescription}} for updating step description,
#' \code{\link{getStep}} for retrieving step information
#'
#' @examples
#' \dontrun{
#' # Update step rationale
#' changeStepRationale(
#'   ident = "/improve-tutorial/Modeling/Step 1",
#'   rationale = "Investigate linear relationship between variables"
#' )
#' }
#'
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
  return(refreshResource(ident,from))
}

#' Mark Step Flags
#'
#' Sets or clears boolean flags on an existing step. Only flags that are
#' explicitly provided (non-\code{NULL}) are changed; all other step properties
#' are preserved.
#'
#' @param ident Identifier of the step. Can be the step's path, resource
#'   (version) id, full entity (version) id, or short entity (version) id.
#' @param from Root directory for resolving relative paths. Default is
#'   \code{pwd()}.
#' @param keyStep Logical or \code{NULL}. Mark as a key step.
#' @param baseModel Logical or \code{NULL}. Mark as a base model.
#' @param fullModel Logical or \code{NULL}. Mark as a full model.
#' @param finalModel Logical or \code{NULL}. Mark as the final model.
#' @param referenceModel Logical or \code{NULL}. Mark as a reference model.
#'
#' @return The updated step resource (invisibly).
#'
#' @details
#' Flags are boolean properties on step resources that help classify and
#' organise analysis steps within a workflow. They are visible in the improve
#' client and can be used for filtering and reporting.
#'
#' Pass \code{TRUE} to set a flag, \code{FALSE} to clear it, or leave as
#' \code{NULL} (default) to keep the current value.
#'
#' @examples
#' \dontrun{
#' # Mark a step as the final model
#' markStep("/Projects/analysis/Step 18", finalModel = TRUE)
#'
#' # Set multiple flags at once
#' markStep("/Projects/analysis/Step 5", keyStep = TRUE, baseModel = TRUE)
#'
#' # Clear a flag
#' markStep("/Projects/analysis/Step 5", baseModel = FALSE)
#' }
#'
#' @seealso
#' \code{\link{changeStepDescription}} for updating step description,
#' \code{\link{changeStepRationale}} for updating step rationale
#'
#' @export
markStep <- function(ident, from = pwd(),
                     keyStep = NULL, baseModel = NULL, fullModel = NULL,
                     finalModel = NULL, referenceModel = NULL) {
  stepEntity <- loadResource(ident, from)
  if (is.null(stepEntity)) {
    log_warn("Step not found:", ident)
    return(invisible(NULL))
  }

  changed <- FALSE
  if (!is.null(keyStep)) { stepEntity$keyStep <- keyStep; changed <- TRUE }
  if (!is.null(baseModel)) { stepEntity$baseModel <- baseModel; changed <- TRUE }
  if (!is.null(fullModel)) { stepEntity$fullModel <- fullModel; changed <- TRUE }
  if (!is.null(finalModel)) { stepEntity$finalModel <- finalModel; changed <- TRUE }
  if (!is.null(referenceModel)) { stepEntity$referenceModel <- referenceModel; changed <- TRUE }

  if (!changed) {
    log_info("markStep: no flags specified, nothing to update")
    return(invisible(stepEntity))
  }

  authenticatedREST(
    "/resources/{resourceId}/",
    urlParams = list(resourceId = stepEntity$resourceId),
    data = as.list(stepEntity),
    restType = "PUT"
  )
  invisible(refreshResource(ident, from))
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
      log_warn("maximum folder depth allowed is 1, by filename in realise step")
      log_warn(fileName)
      return()
    }
    folderName <- pathParts[1]
    fileName<- pathParts[2]
    children <- loadChildResources(newStep)
    folder <- children[children$name==folderName,]
    if (nrow(folder)==1 && folder$nodeType!="Folder") {
      log_warn(folderName)
      log_warn("already exists but not as folder")
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


