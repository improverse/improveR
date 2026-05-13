#' Export a Folder Hierarchy to a Portable Zip File
#'
#' Exports an entire folder hierarchy — including subfolders, analysis trees (with their
#' workflow steps), standalone files, and links — to a zip file that can be imported into
#' another repository. Cross-tree links and folder-level links are preserved.
#'
#' @param folderIdent Identifier of the folder to export. Can be a path, resource ID,
#'   or entity ID.
#' @param exportName Character. Name for the exported folder package (without extension).
#'   Used as the zip filename and internal folder name.
#' @param targetFolder Character. Directory path where the zip file and mapping templates
#'   will be created. Defaults to current directory (".").
#'
#' @details
#' The export process:
#' \enumerate{
#'   \item Recursively traverses the folder hierarchy via \code{loadChildResources()}
#'   \item Classifies each child: Folder, File, Analysis Tree, Link, ExtLink
#'   \item For trees: extracts steps via \code{getWorkflow()} / \code{getStep()} into
#'     a unified step data frame (same format as workflow export)
#'   \item Collects internal links across ALL trees (cross-tree links supported)
#'   \item Downloads step files via \code{cloneCli()}; standalone folder files via
#'     \code{getData()}
#'   \item Generates manifest.json, workflow.json-compatible step data, and mapping templates
#'   \item Packages everything into a single zip file
#' }
#'
#' @section Export Structure:
#' \preformatted{
#' <exportName>/
#'   manifest.json              # Folder structure + tree/link metadata
#'   workflow.json              # Unified step data (same format as exportWorkflow)
#'   internalLinks.json         # Step-to-step links across all trees
#'   links/                     # External dependency files
#'   <stepHandle>/
#'     inputFiles/
#'     outputFiles/
#'   folderFiles/               # Standalone files from folders
#'     <resourceId>_<filename>
#' }
#'
#' @section Generated Mapping Templates:
#' Same as \code{\link{exportWorkflow}}: \code{<exportName>LinkMapping.json} and
#' \code{<exportName>ToolMapping.json} are created alongside the zip.
#'
#' @return Invisibly returns the path to the created zip file.
#'
#' @examples
#' \dontrun{
#' exportFolder("/Projects/MyFolder", "MyFolderExport")
#' # Creates: MyFolderExport.zip, MyFolderExportLinkMapping.json,
#' #          MyFolderExportToolMapping.json
#' }
#'
#' @seealso \code{\link{importFolder}} for importing exported folders,
#'   \code{\link{exportWorkflow}} for exporting single workflows
#'
#' @export
exportFolder <- function(folderIdent, exportName, targetFolder = ".") {
  rootResource <- loadResource(folderIdent)
  if (is.null(rootResource) || rootResource$nodeType != "Folder") {
    stop("folderIdent must point to an existing Folder resource")
  }

  # Phase 1: Discovery - recursive traversal
  manifest <- .traverseFolderRecursive(rootResource, "")
  folderStructure <- manifest$folderStructure
  trees <- manifest$trees

  log_info(paste("Folder export discovery complete.",
                 nrow(folderStructure), "folder entries,",
                 length(trees), "trees found"))

  # Phase 2: Build unified workflow from all trees
  unifiedResult <- .buildUnifiedWorkflow(trees, rootResource)
  workFlowDf <- unifiedResult$workFlowDf
  insideLinks <- unifiedResult$insideLinks

  # Phase 3: Create export directory
  exportFolder_path <- file.path(targetFolder, exportName, fsep = "/")
  if (dir.exists(exportFolder_path)) {
    unlink(exportFolder_path, recursive = TRUE, force = TRUE)
  }
  dir.create(exportFolder_path, recursive = TRUE)
  exportDir <- normalizePath(exportFolder_path, winslash = "/")

  # Phase 4: Export step files (reuse shared helper)
  allOutsideLinks <- NULL
  if (!is.null(workFlowDf) && nrow(workFlowDf) > 0) {
    workFlowDf <- convertParentIdentsToFullNames(workFlowDf)

    allOutsideLinks <- filterOutsideLinks(workFlowDf)
    outsideLinks <- allOutsideLinks
    if (!is.null(outsideLinks) && nrow(outsideLinks) > 0) {
      availableCols <- intersect(
        names(outsideLinks),
        c("stepHandle", "targetStep", "name", "version", "filehash", "maxVersion")
      )
      if (length(availableCols) > 0) {
        outsideLinks <- dplyr::select(outsideLinks, dplyr::all_of(availableCols))
      }
    }

    inputs <- collectInputFiles(workFlowDf)
    exportStepFiles(workFlowDf, exportDir, insideLinks, outsideLinks, inputs)

    # Write workflow.json (unified step data)
    jsonlite::write_json(
      workFlowDf,
      file.path(exportDir, "workflow.json"),
      pretty = TRUE
    )

    # Write internalLinks.json
    if (!is.null(insideLinks) && nrow(insideLinks) > 0) {
      jsonlite::write_json(
        insideLinks,
        file.path(exportDir, "internalLinks.json"),
        pretty = TRUE
      )
    }
  }

  # Phase 5: Download standalone folder files
  folderFilesDir <- file.path(exportDir, "folderFiles")
  dir.create(folderFilesDir, showWarnings = FALSE)
  fileEntries <- folderStructure[folderStructure$nodeType == "File", ]
  if (nrow(fileEntries) > 0) {
    for (i in seq_len(nrow(fileEntries))) {
      entry <- fileEntries[i, ]
      fileKey <- paste0(entry$resourceId, "_", entry$name)
      tryCatch({
        # Download the file content directly via REST
        fileResource <- loadResource(entry$resourceId)
        if (!is.null(fileResource)) {
          targetPath <- file.path(folderFilesDir, fileKey)
          fResult <- authenticatedREST(
            "/revisions/{revisionId}/resources/{resourceId}/content",
            list(
              resourceId = fileResource$resourceId,
              revisionId = fileResource$revisionId
            )
          )
          fContent <- httr::content(fResult, as = "raw")
          writeBin(fContent, targetPath)
          folderStructure[folderStructure$resourceId == entry$resourceId, "fileKey"] <- fileKey
        }
      }, error = function(e) {
        log_warn(paste("Failed to download folder file:", entry$name, "-", e$message))
      })
    }
  }
  # Remove empty folderFiles dir
  if (length(dir(folderFilesDir)) == 0) {
    unlink(folderFilesDir, recursive = TRUE)
  }

  # Phase 6: Write manifest.json
  manifestData <- list(
    version = "1.0",
    exportDate = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    rootPath = rootResource$path,
    rootResourceId = rootResource$resourceId,
    folderStructure = folderStructure,
    trees = if (length(trees) > 0) {
      data.frame(
        relativePath = vapply(trees, function(t) t$relativePath, character(1)),
        parentRelativePath = vapply(trees, function(t) t$parentRelativePath, character(1)),
        treeName = vapply(trees, function(t) t$treeName, character(1)),
        stringsAsFactors = FALSE
      )
    } else {
      data.frame(relativePath = character(), parentRelativePath = character(),
                 treeName = character(), stringsAsFactors = FALSE)
    }
  )
  jsonlite::write_json(
    manifestData,
    file.path(exportDir, "manifest.json"),
    pretty = TRUE,
    auto_unbox = TRUE
  )

  # Phase 7: Generate mapping templates (reuse shared helpers)
  if (!is.null(workFlowDf) && nrow(workFlowDf) > 0) {
    toolMapping <- generateToolMappingTemplate(workFlowDf)
    jsonlite::write_json(
      toolMapping,
      file.path(targetFolder, paste0(exportName, "ToolMapping.json")),
      pretty = TRUE
    )

    linkMappingDf <- generateLinkMappingTemplate(allOutsideLinks)
    if (!is.null(linkMappingDf)) {
      jsonlite::write_json(
        linkMappingDf,
        file.path(targetFolder, paste0(exportName, "LinkMapping.json")),
        pretty = TRUE
      )
    }
  }

  # Phase 8: Package into zip
  oldwd <- getwd()
  setwd(targetFolder)
  zipFile <- paste0(exportName, ".zip")
  if (file.exists(zipFile)) {
    unlink(zipFile)
  }
  utils::zip(zipfile = zipFile, files = exportName)
  setwd(oldwd)
  unlink(exportFolder_path, force = TRUE, recursive = TRUE)

  invisible(file.path(targetFolder, zipFile))
}


# ============================================================
# Internal helpers
# ============================================================

#' Recursively traverse a folder and build manifest
#'
#' @param resource Resource data frame for the current folder.
#' @param relativePath Current relative path from export root.
#' @return List with folderStructure (data frame) and trees (list of tree info).
#' @keywords internal
.traverseFolderRecursive <- function(resource, relativePath) {
  children <- loadChildResources(resource)$data[[1]]
  folderStructure <- data.frame(
    resourceId = character(),
    name = character(),
    relativePath = character(),
    nodeType = character(),
    parentRelativePath = character(),
    stringsAsFactors = FALSE
  )
  trees <- list()

  if (is.null(children) || nrow(children) == 0) {
    return(list(folderStructure = folderStructure, trees = trees))
  }

  for (i in seq_len(nrow(children))) {
    child <- children[i, ]
    childRelPath <- if (relativePath == "") child$name else paste(relativePath, child$name, sep = "/")

    if (child$nodeType == "Folder") {
      # Record folder entry
      entry <- data.frame(
        resourceId = child$resourceId,
        name = child$name,
        relativePath = childRelPath,
        nodeType = "Folder",
        parentRelativePath = relativePath,
        stringsAsFactors = FALSE
      )
      folderStructure <- plyr::rbind.fill(folderStructure, entry)

      # Recurse into subfolder
      subResult <- .traverseFolderRecursive(child, childRelPath)
      folderStructure <- plyr::rbind.fill(folderStructure, subResult$folderStructure)
      trees <- c(trees, subResult$trees)

    } else if (child$nodeType == "Analysis Tree") {
      # Record tree for workflow extraction
      trees[[length(trees) + 1]] <- list(
        resourceId = child$resourceId,
        relativePath = childRelPath,
        parentRelativePath = relativePath,
        treeName = child$name
      )

    } else if (child$nodeType == "File") {
      entry <- data.frame(
        resourceId = child$resourceId,
        name = child$name,
        relativePath = childRelPath,
        nodeType = "File",
        parentRelativePath = relativePath,
        fileKey = NA_character_,
        stringsAsFactors = FALSE
      )
      folderStructure <- plyr::rbind.fill(folderStructure, entry)

    } else if (child$nodeType == "Link") {
      # Load full resource to get link target info
      linkResource <- tryCatch(loadResource(child$resourceId), error = function(e) { log_warn("loadResource failed during export/import: ", conditionMessage(e)); NULL })
      linkTargetEntityId <- NA_character_
      linkTargetPath <- NA_character_
      if (!is.null(linkResource) && !is.null(linkResource$targetEntityId)) {
        linkTargetEntityId <- linkResource$targetEntityId
        linkTarget <- tryCatch(loadResource(linkResource$targetEntityId), error = function(e) { log_warn("loadResource failed during export/import: ", conditionMessage(e)); NULL })
        if (!is.null(linkTarget)) {
          linkTargetPath <- linkTarget$path
        }
      }

      entry <- data.frame(
        resourceId = child$resourceId,
        name = child$name,
        relativePath = childRelPath,
        nodeType = "Link",
        parentRelativePath = relativePath,
        linkTargetEntityId = linkTargetEntityId,
        linkTargetPath = linkTargetPath,
        stringsAsFactors = FALSE
      )
      folderStructure <- plyr::rbind.fill(folderStructure, entry)

    } else if (child$nodeType == "ExtLink") {
      # Load full resource to get URL
      extLinkResource <- tryCatch(loadResource(child$resourceId), error = function(e) { log_warn("loadResource failed during export/import: ", conditionMessage(e)); NULL })
      extUrl <- NA_character_
      if (!is.null(extLinkResource) && !is.null(extLinkResource$url)) {
        extUrl <- extLinkResource$url
      }

      entry <- data.frame(
        resourceId = child$resourceId,
        name = child$name,
        relativePath = childRelPath,
        nodeType = "ExtLink",
        parentRelativePath = relativePath,
        url = extUrl,
        stringsAsFactors = FALSE
      )
      folderStructure <- plyr::rbind.fill(folderStructure, entry)
    }
  }

  return(list(folderStructure = folderStructure, trees = trees))
}


#' Build unified workflow data from all trees in a folder
#'
#' Loads all steps from all trees and combines them into a single workflow,
#' exactly like a multi-tree workflow export. Cross-tree links are naturally
#' handled because collectInternalLinks resolves by path.
#'
#' @param trees List of tree info (resourceId, relativePath, treeName).
#' @param rootResource Root folder resource.
#' @return List with workFlowDf and insideLinks.
#' @keywords internal
.buildUnifiedWorkflow <- function(trees, rootResource) {
  if (length(trees) == 0) {
    return(list(workFlowDf = NULL, insideLinks = NULL))
  }

  # Load all trees into one unified workflow
  unifiedWorkflow <- NULL
  for (treeInfo in trees) {
    tryCatch({
      treeWorkflow <- getWorkflow(treeInfo$resourceId)
      if (is.null(unifiedWorkflow)) {
        unifiedWorkflow <- treeWorkflow
      } else {
        # Merge steps from this tree into the unified workflow
        treeStepsDf <- treeWorkflow$df()
        if (!is.null(treeStepsDf) && nrow(treeStepsDf) > 0) {
          for (stepName in names(treeWorkflow$steps)) {
            if (is.null(unifiedWorkflow$steps[[stepName]])) {
              unifiedWorkflow$steps[[stepName]] <- treeWorkflow$steps[[stepName]]
              # Update the step's workflow reference
              unifiedWorkflow$steps[[stepName]]$workflow <- unifiedWorkflow
            }
          }
        }

        # Merge internalLinks from each tree
        if (!is.null(treeWorkflow$internalLinks) && nrow(treeWorkflow$internalLinks) > 0) {
          if (is.null(unifiedWorkflow$internalLinks) || nrow(unifiedWorkflow$internalLinks) == 0) {
            unifiedWorkflow$internalLinks <- treeWorkflow$internalLinks
          } else {
            unifiedWorkflow$internalLinks <- plyr::rbind.fill(
              unifiedWorkflow$internalLinks,
              treeWorkflow$internalLinks
            )
          }
        }
      }
    }, error = function(e) {
      log_warn(paste("Failed to load workflow from tree:", treeInfo$treeName, "-", e$message))
    })
  }

  if (is.null(unifiedWorkflow)) {
    return(list(workFlowDf = NULL, insideLinks = NULL))
  }

  # Re-evaluate cross-tree links: links that look "outside" in individual trees
  # but whose source is actually a step in another merged tree should be reclassified
  # as internal links.
  allStepEntityIds <- vapply(names(unifiedWorkflow$steps), function(stepName) {
    unifiedWorkflow$steps[[stepName]]$stepDf$sourceEntityId
  }, character(1))

  for (stepName in names(unifiedWorkflow$steps)) {
    stepEnv <- unifiedWorkflow$steps[[stepName]]
    remoteFiles <- stepEnv$stepDf$remoteFiles[[1]]
    if (!is.null(remoteFiles) && nrow(remoteFiles) > 0 && "asLink" %in% names(remoteFiles)) {
      # Ensure sourceStep column exists for filtering
      if (!("sourceStep" %in% names(remoteFiles))) {
        remoteFiles$sourceStep <- NA_character_
        stepEnv$stepDf$remoteFiles[[1]] <- remoteFiles
      }
      outsideLinks <- remoteFiles[remoteFiles$asLink & is.na(remoteFiles$sourceStep), ]
      if (nrow(outsideLinks) > 0) {
        for (i in seq_len(nrow(outsideLinks))) {
          # remoteFiles may not have a 'path' column — resolve from ident
          linkIdent <- outsideLinks$ident[i]
          linkResource <- tryCatch(loadResource(linkIdent), error = function(e) { log_warn("loadResource failed during export/import: ", conditionMessage(e)); NULL })
          linkPath <- if (!is.null(linkResource)) linkResource$path else NULL
          if (!is.null(linkPath) && !is.na(linkPath)) {
            # Check if this link's source path matches any step's resource path
            for (otherStepName in names(unifiedWorkflow$steps)) {
              if (otherStepName == stepName) next
              otherStep <- unifiedWorkflow$steps[[otherStepName]]
              otherEntityId <- otherStep$stepDf$sourceEntityId
              # Load the other step to get its path
              otherResource <- tryCatch(loadResource(otherEntityId), error = function(e) { log_warn("loadResource failed during export/import: ", conditionMessage(e)); NULL })
              if (!is.null(otherResource) && !is.null(otherResource$path) &&
                  startsWith(linkPath, otherResource$path)) {
                # This is a cross-tree internal link
                # Find matching row by ident (path column may not exist in remoteFiles)
                idx <- which(remoteFiles$ident == linkIdent & remoteFiles$asLink & is.na(remoteFiles$sourceStep))
                if (length(idx) > 0) {
                  remoteFiles$sourceStep[idx[1]] <- otherStepName
                  if (!("targetStep" %in% names(remoteFiles))) {
                    remoteFiles$targetStep <- NA_character_
                  }
                  remoteFiles$targetStep[idx[1]] <- stepName
                  sourceInventoryPath <- substr(linkPath, nchar(otherResource$path) + 2, nchar(linkPath))
                  if (!("sourceInventoryPath" %in% names(remoteFiles))) {
                    remoteFiles$sourceInventoryPath <- NA_character_
                  }
                  remoteFiles$sourceInventoryPath[idx[1]] <- sourceInventoryPath
                  stepEnv$stepDf$remoteFiles[[1]] <- remoteFiles
                  log_info(paste("Reclassified cross-tree link:", outsideLinks$name[i],
                                 "from", stepName, "->", otherStepName))

                  # Also update dependencies
                  if (!is.null(stepEnv$dependencies)) {
                    stepEnv$dependencies[[otherStepName]] <- otherStep
                  }
                  if (!is.null(otherStep$usage)) {
                    otherStep$usage[[stepName]] <- stepEnv
                  }
                }
                break
              }
            }
          }
        }
      }
    }
  }

  # Create template from unified workflow
  workflowTemplate <- unifiedWorkflow$createTemplate()
  workFlowDf <- workflowTemplate$df()

  if (is.null(workFlowDf) || nrow(workFlowDf) == 0) {
    return(list(workFlowDf = NULL, insideLinks = NULL))
  }

  # Collect inside links using the shared helper
  insideLinks <- collectInsideLinks(workflowTemplate$df(), workflowTemplate)

  return(list(workFlowDf = workFlowDf, insideLinks = insideLinks))
}
