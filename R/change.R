

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
  if (is.null(result)) {
    log_warn("Failed to perform", changeFunction, "operation")
    return(NULL)
  }
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

#' Copy Resources to Target Location
#'
#' Duplicates one or more resources to a target folder, creating independent copies with
#' their own version history. Unlike \code{\link{createLink}}, copies are fully independent
#' and changes to the original do not affect the copy.
#'
#' @param sources Identifier(s) of resource(s) to copy. Can be a single resource or multiple
#'   resources. Accepts paths, resource ids, entity ids, or a data frame of resources.
#' @param target Identifier of the target folder or file location. Can be a path, resource id,
#'   or entity id. If copying multiple sources, must be a container (folder).
#' @param targetName Character. Optional new name for the copied resource. Only used when
#'   copying a single source. Ignored when copying multiple sources. Defaults to empty string
#'   (uses original name).
#' @param overwrite Logical. If \code{TRUE} and a resource with the same name exists at the
#'   target location, it is deleted and replaced. If \code{FALSE} (default) and a name
#'   conflict exists, returns \code{NULL} with a warning.
#' @param comment Character. Commit message for the copy operation. Defaults to
#'   "modified by improveRW".
#'
#' @return The copied resource(s) as a data frame, or \code{NULL} if the operation fails
#'   (e.g., source doesn't exist, target doesn't exist, name conflict with overwrite=FALSE,
#'   invalid target type for the source type).
#'
#' @details
#' The function supports both single and batch copying:
#' \itemize{
#'   \item \strong{Single copy:} When copying one resource, you can optionally specify a new
#'     name via \code{targetName}
#'   \item \strong{Batch copy:} When copying multiple resources, all are copied to the target
#'     folder with their original names (\code{targetName} must be empty)
#' }
#'
#' The copy operation validates:
#' \itemize{
#'   \item Source and target resources exist
#'   \item Target accepts the source type (e.g., can't copy a workflow into a file)
#'   \item No naming conflicts (unless \code{overwrite=TRUE})
#'   \item Repository is in editable mode (automatically checked)
#' }
#'
#' After copying, parent folder caches are automatically cleared to ensure fresh data
#' on subsequent queries.
#'
#' @seealso
#' \code{\link{move}} to relocate resources instead of copying,
#' \code{\link{createLink}} to create lightweight references instead of full copies,
#' \code{\link{delete}} to remove resources
#'
#' @examples
#' \dontrun{
#' # Copy a single file to a folder
#' copy(
#'   sources = "/Data/baseline.csv",
#'   target = "/Modeling/PopPK"
#' )
#'
#' # Copy with a new name
#' copy(
#'   sources = "/Data/baseline.csv",
#'   target = "/Modeling/PopPK",
#'   targetName = "input_data.csv"
#' )
#'
#' # Copy multiple files at once
#' copy(
#'   sources = c("/Data/file1.csv", "/Data/file2.csv"),
#'   target = "/Analysis"
#' )
#'
#' # Overwrite existing file
#' copy(
#'   sources = "/Data/new_baseline.csv",
#'   target = "/Modeling/baseline.csv",
#'   overwrite = TRUE
#' )
#' }
#'
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

#' Move Resources to Target Location
#'
#' Relocates one or more resources to a target folder, changing their path while preserving
#' their version history and metadata. The resource is removed from its current location and
#' placed in the new location.
#'
#' @param sources Identifier(s) of resource(s) to move. Can be a single resource or multiple
#'   resources. Accepts paths, resource ids, entity ids, or a data frame of resources.
#' @param target Identifier of the target folder or file location. Can be a path, resource id,
#'   or entity id. If moving multiple sources, must be a container (folder).
#' @param targetName Character. Optional new name for the moved resource. Only used when
#'   moving a single source. Ignored when moving multiple sources. Defaults to empty string
#'   (uses original name).
#' @param overwrite Logical. If \code{TRUE} and a resource with the same name exists at the
#'   target location, it is deleted and replaced. If \code{FALSE} (default) and a name
#'   conflict exists, returns \code{NULL} with a warning.
#' @param comment Character. Commit message for the move operation. Defaults to
#'   "modified by improveRW".
#'
#' @return The moved resource(s) as a data frame with updated paths, or \code{NULL} if the
#'   operation fails (e.g., source doesn't exist, target doesn't exist, name conflict with
#'   overwrite=FALSE, invalid target type for the source type).
#'
#' @details
#' The function supports both single and batch moving:
#' \itemize{
#'   \item \strong{Single move:} When moving one resource, you can optionally specify a new
#'     name via \code{targetName} (also serves as a rename operation)
#'   \item \strong{Batch move:} When moving multiple resources, all are moved to the target
#'     folder with their original names (\code{targetName} must be empty)
#' }
#'
#' The move operation validates:
#' \itemize{
#'   \item Source and target resources exist
#'   \item Target accepts the source type (e.g., can't move a workflow into a file)
#'   \item No naming conflicts (unless \code{overwrite=TRUE})
#'   \item Repository is in editable mode (automatically checked)
#' }
#'
#' After moving, path caches for both source and target locations are automatically
#' invalidated to ensure fresh data on subsequent queries.
#'
#' @seealso
#' \code{\link{copy}} to duplicate resources instead of moving,
#' \code{\link{delete}} to remove resources,
#' \code{\link{createLink}} to create references without moving
#'
#' @examples
#' \dontrun{
#' # Move a file to a different folder
#' move(
#'   sources = "/Data/old_location/baseline.csv",
#'   target = "/Data/current"
#' )
#'
#' # Move and rename in one operation
#' move(
#'   sources = "/Data/draft_analysis.R",
#'   target = "/Analysis",
#'   targetName = "final_analysis.R"
#' )
#'
#' # Move multiple files at once
#' move(
#'   sources = c("/Temp/file1.csv", "/Temp/file2.csv"),
#'   target = "/Data/Archive"
#' )
#'
#' # Reorganize folder structure
#' move(
#'   sources = "/Projects/Old_Structure/Workflows",
#'   target = "/Projects/New_Structure"
#' )
#' }
#'
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
    return(FALSE)
  }
  result <- authenticatedREST('/resources/{resourceId}',
                                            urlParams = list(resourceId=resource$resourceId
                                            ),
                                            restType = "DELETE")
  invalidatePathCaches(resource$path,deleteLinkedFiles = TRUE)
  if (is.null(resource$parentId)) {
    resource$parentId <-"/"
  }
  #unloadResource(resource$resourceId)
  unloadChildResources(resource$parentId)
  if (is.null(result)) {
    log_warn("Failed to delete resource:", resource$resourceId)
    return(FALSE)
  }
  return(TRUE)
}

#' Delete Resources Permanently
#'
#' Permanently removes one or more resources from the repository. Resources can be folders,
#' analysis trees, workflows, steps, files, or links. For safety, deletion is prevented
#' if any workflow step within the resource(s) has an active run status.
#'
#' @param res Identifier(s) of resource(s) to delete. Can be a single resource or multiple
#'   resources. Accepts paths, resource ids, entity ids, or a data frame of resources.
#'
#' @return Logical \code{TRUE} if deletion succeeded, \code{FALSE} if it failed (e.g.,
#'   resource doesn't exist, step has incompatible runStatus).
#'
#' @details
#' \strong{Safety constraints:} Deletion is blocked if any workflow step within the target
#' resource(s) has a runStatus other than "INITIAL" or "FINISHED". This prevents accidental
#' deletion of:
#' \itemize{
#'   \item Running steps (\code{runStatus = "RUNNING"})
#'   \item Failed steps that may need investigation (\code{runStatus = "FAILED"})
#'   \item Steps in other intermediate states
#' }
#'
#' For files and links, deletion proceeds immediately as they don't have run states.
#'
#' \strong{Important notes:}
#' \itemize{
#'   \item Deletion is \strong{permanent} - deleted resources cannot be recovered
#'   \item Parent folder caches are automatically cleared after deletion
#'   \item Path caches are invalidated for the deleted resource and any linked files
#'   \item Repository must be in editable mode (automatically checked)
#' }
#'
#' @seealso
#' \code{\link{copy}} to duplicate resources before deletion,
#' \code{\link{move}} to relocate resources instead of deleting
#'
#' @examples
#' \dontrun{
#' # Delete a single file
#' delete("/Data/temp_file.csv")
#'
#' # Delete multiple resources
#' delete(c("/Temp/file1.csv", "/Temp/file2.csv"))
#'
#' # Delete a folder and all contents (if step runStatus allows)
#' delete("/Projects/Old_Analysis")
#'
#' # Delete using a data frame of resources
#' temp_files <- query("name='temp_*'")
#' delete(temp_files)
#' }
#'
#' @references ics1139
#' @export
delete <- function(res) {
  

#runStatus of steps should only be collected if steps are actually relevant; not for files and links
resNodeType <- loadResource(res)$nodeType

if (!is.null(resNodeType) && !resNodeType %in% c("File", "Link", "Review")) {
  stepStatus <- checkRunStatus(
    res,
    verbose = TRUE,
    show_no_steps_message = FALSE
  )

  # If FALSE, deletion is blocked due to incompatible step status or non-existent resource
  if (isFALSE(stepStatus)) {
    log_warn("NO resource was deleted.")
    return(invisible(FALSE))
  }
}

  # Proceed with deletion (stepStatus is TRUE)
  result <- multiplexResourceFunction(
    func = singleDelete,
    multiArgument = res
  )

  if (is.null(result)) {
    return(FALSE)
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
  if (is.null(fResult)) {
    log_warn("Failed to update file content for resource:", resource$resourceId)
    return(NULL)
  }
  res <- httr::content(fResult)
  invalidateAllFileCaches(resource)
  resource <- refreshResource(res[[1]]$resourceId)
  return(resource)
}

#' Update File Content
#' Prerequisite: must be a file.
#' Multiple files can be updated at once.
#' @param ident Resource(s) to be updated.
#' @param localPath Path to the file with the new content.
#' @param comment Commit comment. Defaults to "modified by improveRW".
#' @references ics1210
#' @returns The refreshed resource after the upload, as [refreshResource()] returns it.
#'   `NULL` when `ident` resolves to no resource or the upload itself failed. For several
#'   idents the refreshed resources of all of them, merged.
#' @export
updateFileContent <- function(ident,localPath,comment="modified by improveRW") {
  return(
    multiplexResourceFunction(func=singleUpdateFileContent,
      multiArgument = ident,
                              localPath=localPath,
                              comment=comment)
  )
}


#' Upload a Complete Folder
#' Prerequisite: must be a folder.
#' @param targetIdent Target resource.
#' @param localFolder Path to the local folder to upload.
#' @param comment Commit comment. Defaults to "modified by improveRW".
#' @references ics1210
#' @returns No meaningful value - called for its side effect of creating the folder under
#'   `targetIdent` and filling it, recursively, with what `localFolder` holds. When
#'   `localFolder` is not a directory a warning is logged and nothing is created.
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

#' Collect steps from a resource with run status information
#'
#' Recursively searches a folder or analysis tree for all steps and returns
#' details including their run status information. If a step is provided, the
#' function returns details for that step. Can handle single resources,
#' character vectors of multiple identifiers, or data frames with multiple
#' resources.
#'
#' @param ident Identifier(s) for folder(s), analysis tree(s), or step(s).
#'   Can be a single identifier (path, resourceId, entityId), a character
#'   vector of multiple identifiers, or a data frame with multiple resources.
#' @param includeNested Logical, default \code{TRUE}. Whether to search nested
#'   folders and analysis trees recursively. Ignored when \code{ident} is a
#'   step.
#' @param showProgress Logical, default \code{FALSE}. Whether to display a
#'   spinner showing the path of each container being scanned during recursive processing.
#' @param verbose Optional parameter (default \code{NULL}). Controls whether
#'   messages are displayed when resources don't exist or no steps are found.
#' @param show_no_steps_message Logical, default \code{TRUE}. If \code{TRUE},
#'   a message is shown when no steps are found in the resource.
#'
#' @details
#' The function requires an active connection to the improve server and will
#' stop with an error if not connected. It handles vector inputs by recursively
#' calling itself for each element and combining the results.
#'
#' All resource data, including child resources and step run statuses, are
#' retrieved fresh from the server (not from cache) to ensure up-to-date
#' information.
#'
#' If a resource does not exist, the function returns \code{FALSE}. For steps,
#' it returns a single-row data frame with that step's information. For
#' containers (folders and analysis trees), it recursively searches for all
#' steps within.
#'
#' @return A data frame with columns: \code{path}, \code{name}, \code{entityId},
#'   \code{resourceId}, \code{runStatus}, \code{nodeType}. Returns \code{FALSE}
#'   if the resource does not exist. Returns an empty data frame if the resource
#'   exists but contains no steps.
#'
#' @seealso \code{\link{checkRunStatus}}
#' @keywords internal
#' @noRd
collectSteps <- function(
  ident,
  includeNested = TRUE,
  showProgress = FALSE,
  verbose = NULL,
  show_no_steps_message = TRUE
) {
  #check if connected
  if (!isTRUE(tryCatch(improveConnected(), error = \(e) FALSE))) {
    stop("Not connected to improve server. Please run improveConnect() first.")
  }

  # Handle data frame input (single resource) - extract resourceId
  if (is.data.frame(ident)) {
    ident <- getCorrectId(ident)
  }

  # Handle vector of identifiers (e.g., c(step1, step2))
  if (is.character(ident) && length(ident) > 1) {
    allResults <- data.frame()
    for (i in seq_along(ident)) {
      result <- collectSteps(
        ident[i],
        includeNested = includeNested,
        showProgress = showProgress,
        verbose = verbose,
        show_no_steps_message = show_no_steps_message
      )
      allResults <- rbind(allResults, result)
    }
    return(allResults %>% dplyr::distinct()) #steps are onyl retruned once (e.g. in case of overlapping of folder and analysis tree)
  }

  # Load the resource(s)
  resources <- refreshResource(ident)

  if (is.null(resources)) {
    if (isTRUE(verbose)) {
      log_warn(glue::glue("Source {ident} does not exist."))
    }
    return(FALSE)
  }

  # browser()
  #what if the resource is a file and not a step, tree, or folder => files don't nest steps/don't have runStatus; end evaluation here; return FALSE
  if (resources$nodeType == "File" || resources$nodeType == "Link") {
    # if (isTRUE(echo)) {
    #   message(glue::glue("Source {ident} is a File and contains no steps."))
    # }
    # return empty data.frame so checkRunStatus() treats it as "no steps" (safe to delete)
    return(data.frame())
  }

  # Handle multiple resources (data frame with multiple rows)
  if (is.data.frame(resources) && nrow(resources) > 1) {
    allResults <- data.frame()
    for (i in seq_len(nrow(resources))) {
      result <- collectSteps(
        resources[i, ],
        includeNested = includeNested,
        showProgress = showProgress,
        verbose = verbose,
        show_no_steps_message = show_no_steps_message
      )
      allResults <- rbind(allResults, result)
    }
    return(allResults)
  }

  usersAll <- dplyr::select(users(), ownedById = "id", ownedByName = "name")

  # Single resource case
  root <- resources

  # If it's a step, just return its run status
  if (root$nodeType == "Step") {
    stepInfo <- root[, c(
      "path",
      "name",
      "entityId",
      "resourceId",
      "runStatus",
      "nodeType",
      "ownedById"
    )]

    stepInfo <- stepInfo %>%
      dplyr::left_join(usersAll, by = c("ownedById" = "ownedById"))

    return(stepInfo)
  }

  # If it's not a container, error
  if (!root$nodeType %in% c("Folder", "Analysis Tree")) {
    stop("Expected Folder, Analysis Tree, or Step, got: ", root$nodeType)
  }

  # Initialize step counter for progress tracking
  stepCount <- 0

  # Recursive helper function for containers
  collectSteps <- function(resource, results = data.frame()) {
    # Load all full child resources (includes runStatus for steps)
    children <- refreshFullChildResources(resource$entityId)

    if (is.null(children) || !("data" %in% names(children))) {
      return(results)
    }

    childDf <- children$data[[1]]

    if (nrow(childDf) == 0) {
      return(results)
    }

    # Extract steps and add to results
    steps <- childDf[childDf$nodeType == "Step", ]
    if (nrow(steps) > 0) {
      stepInfo <- steps[, c(
        "path",
        "name",
        "entityId",
        "resourceId",
        "runStatus",
        "nodeType",
        "ownedById"
      )]
      results <- rbind(results, stepInfo)

      # Update progress counter
      stepCount <<- stepCount + nrow(steps)
    }

    # Recursively process Analysis Trees and Folders
    if (includeNested) {
      containers <- childDf[
        childDf$nodeType %in% c("Folder", "Analysis Tree"),
      ]

      if (nrow(containers) > 0) {
        for (i in seq_len(nrow(containers))) {
          if (showProgress) {
            if (interactive()) {
              cli::cli_progress_step("Checking {containers$path[i]}")
            }
          }
          results <- collectSteps(containers[i, ], results)
          if (showProgress) {
            if (interactive()) {
              cli::cli_progress_done()
            }
          }
        }
      }
    }

    return(results)
  }

  # Start the recursive collection for containers
  results <- collectSteps(root)

  # Attach owner name to collected steps so downstream callers can use {ownedByName}
  if (nrow(results) > 0) {
    results <- results %>% dplyr::left_join(usersAll, by = "ownedById")
  }

  # don't print; print only informative if you are only interested in steps; but delete may also refer to folder or analysis tree
  if (nrow(results) == 0 && isTRUE(verbose) && isTRUE(show_no_steps_message)) {
    message("No steps found in ", root$path)
  }

  return(results)
}


#' Check whether a resource's steps have acceptable run statuses
#'
#' Uses collectSteps() to retrieve steps for the resource identified by
#' \code{ident} and determines whether it is safe to delete or otherwise
#' proceed based on the run statuses of those steps.
#'
#' @param ident Identifier for the resource whose steps should be inspected.
#'   Passed directly to \code{collectSteps()}; typically a character path or
#'   resource identifier understood by that function.
#' @param includeNested Logical, default \code{TRUE}. If \code{TRUE}, nested
#'   steps will be included when collecting steps.
#' @param showProgress Logical, default \code{FALSE}. If \code{TRUE}, progress
#'   output may be shown while collecting steps.
#' @param verbose Optional parameter (default \code{NULL}) forwarded to
#'   \code{collectSteps()}; controls whether lower-level operations are
#'   displayed according to the behavior of \code{collectSteps()}.
#' @param show_no_steps_message Logical, default \code{TRUE}. If \code{TRUE},
#'   a message is shown when no steps are found in the resource.
#' @param returnType Character, one of \dQuote{logical} (default) or
#'   \dQuote{data}. When \dQuote{logical} the function returns \code{TRUE} when it is safe to proceed (no steps or
#'   all steps have status \dQuote{FINISHED} or \dQuote{INITIAL}) and
#'   \code{FALSE} when the resource does not exist or when there are steps
#'   with run statuses other than \dQuote{FINISHED} or \dQuote{INITIAL}.
#'   When \dQuote{data} the function returns a data frame of the breaking
#'   steps (rows with runStatus not in \dQuote{FINISHED} or \dQuote{INITIAL});
#'   if there are no breaking steps an empty data frame is returned. A
#'   warning listing the breaking steps is emitted in either case.
#'
#' @details
#' The function calls \code{collectSteps()} to obtain a data frame of steps
#' for the given resource. If \code{collectSteps()} returns \code{FALSE},
#' this function returns \code{FALSE} to indicate the resource does not
#' exist. If no steps are returned (NULL or an empty data frame), the
#' resource is considered safe to delete.
#'
#' Any step whose \code{runStatus} is not one of \code{"FINISHED"} or
#' \code{"INITIAL"} is considered a "breaking" step. If such steps are
#' present, their paths and statuses are emitted as a warning and the
#' function either returns \code{FALSE} (\code{returnType = "logical"}) or
#' the data frame of breaking steps (\code{returnType = "steps"}).
#'
#' @return When \code{returnType = "logical"} a logical scalar:
#'   \code{TRUE} when it is safe to proceed, \code{FALSE} otherwise.
#'   When \code{returnType = "steps"} a data frame containing breaking
#'   steps (possibly empty). Note that \code{FALSE} is also returned if the
#'   resource does not exist.
#'
#' @seealso collectSteps
#' @keywords internal
#' @noRd
checkRunStatus <- function(
  ident,
  includeNested = TRUE,
  showProgress = FALSE,
  verbose = NULL,
  show_no_steps_message = TRUE,
  returnType = "logical"
) {
  returnType <- match.arg(returnType, c("logical", "steps"))

  stepsCollected <- collectSteps(
    ident = ident,
    includeNested = includeNested,
    showProgress = showProgress,
    verbose = verbose,
    show_no_steps_message = show_no_steps_message
  )

  # Resource doesn't exist
  if (isFALSE(stepsCollected)) {
    return(FALSE)
  }

  # Resource exists but has no steps - safe to delete
  if (is.null(stepsCollected) || nrow(stepsCollected) == 0) {
    if (returnType == "logical") {
      return(TRUE)
    } else {
      return(stepsCollected) # empty data.frame()
    }
  }

  if (!"runStatus" %in% names(stepsCollected)) {
    if (returnType == "logical") {
      return(TRUE)
    } else {
      return(stepsCollected)
    }
  }

  breakingSteps <- stepsCollected %>%
    dplyr::filter(!.data$runStatus %in% c("FINISHED", "INITIAL"))

  if (nrow(breakingSteps) == 0) {
    if (returnType == "logical") {
      return(TRUE)
    } else {
      return(breakingSteps) # empty data.frame()
    }
  }

  if (nrow(breakingSteps) > 0) {
    log_warn(
      glue::glue_collapse(
        c(
          "The following step(s) have a run status other than 'INITIAL' or 'FINISHED':",
          glue::glue_data(
            breakingSteps,
            '* Step: "{path}" - Status: {runStatus} - Owner: {ownedByName}'
          )
        ),
        sep = "\n"
      )
    )
    if (returnType == "logical") {
      return(FALSE)
    } else {
      return(breakingSteps)
    }
  } else {
    return(breakingSteps)
  }
}
