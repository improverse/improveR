#' Import a Folder Hierarchy from a Zip File into a Repository Folder
#'
#' Imports a folder hierarchy exported by \code{\link{exportFolder}}, recreating
#' the folder structure, analysis trees with their workflow steps, standalone files,
#' and links in the target repository location.
#'
#' @param folderFile Character. Path to the folder export zip file created by
#'   \code{\link{exportFolder}}.
#' @param targetFolder Character. Path to the repository folder where the hierarchy
#'   will be imported. Must be an existing folder in the improve repository.
#' @param onConflict Character. Controls behavior when the target already contains
#'   resources with matching names. One of:
#'   \describe{
#'     \item{\code{"skip"}}{(Default) Reuse existing folders and trees. For files,
#'       skip without updating content. Existing resources are never deleted.}
#'     \item{\code{"overwrite"}}{Reuse existing folders and trees. For files,
#'       update content with the exported version using \code{\link{updateFileContent}}.}
#'     \item{\code{"error"}}{Stop with an error listing all conflicting resources
#'       before any changes are made.}
#'   }
#' @param overwrite Logical. Deprecated. Use \code{onConflict} instead.
#'   \code{TRUE} maps to \code{onConflict = "overwrite"}.
#'
#' @details
#' The import process:
#' \enumerate{
#'   \item Validates the export package (manifest.json, mapping files)
#'   \item Creates the folder structure top-down by path depth
#'   \item Creates analysis trees and imports their steps using the workflow import pattern
#'   \item Uploads standalone files from the \code{folderFiles/} directory
#'   \item Resolves folder-level links (internal links via id mapping, external links)
#'   \item Resolves cross-tree step links in a second pass
#'   \item Restores parent step relationships
#' }
#'
#' @section Mapping Files:
#' Same as \code{\link{importWorkflow}}: optional \code{<exportName>LinkMapping.json}
#' and \code{<exportName>ToolMapping.json} files can be placed alongside the zip.
#'
#' @return Invisibly returns the target folder resource.
#'
#' @examples
#' \dontrun{
#' importFolder("MyFolderExport.zip", "/Projects/TargetLocation")
#' importFolder("MyFolderExport.zip", "/Projects/Target", onConflict = "overwrite")
#' importFolder("MyFolderExport.zip", "/Projects/Target", onConflict = "error")
#' }
#'
#' @seealso \code{\link{exportFolder}} for creating folder export files,
#'   \code{\link{importWorkflow}} for importing single workflows
#'
#' @export
importFolder <- function(folderFile, targetFolder, onConflict = c("skip", "overwrite", "error"),
                         overwrite = FALSE) {
  # Handle deprecated overwrite parameter
  if (!missing(overwrite) && isTRUE(overwrite)) {
    if (!missing(onConflict)) {
      warning("Both 'onConflict' and deprecated 'overwrite' specified; using 'onConflict'")
    } else {
      onConflict <- "overwrite"
    }
  }
  onConflict <- match.arg(onConflict)
  baseName <- strsplit(basename(folderFile), split = ".", fixed = TRUE)[[1]]
  baseName <- paste(baseName[1:(length(baseName) - 1)], collapse = ".")

  # Validate mapping files
  validateMappingFiles(folderFile, baseName)

  # Extract zip
  importDir <- file.path(getwd(), paste0(".import_", baseName, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(importDir, recursive = TRUE)
  zip::unzip(zipfile = folderFile, exdir = importDir)

  importDir <- dir(importDir, full.names = TRUE)
  if (length(importDir) != 1) {
    stop("Folder export zip is expected to contain exactly one folder.")
  }
  importDir <- normalizePath(importDir, winslash = "/")

  # Validate manifest
  manifestPath <- file.path(importDir, "manifest.json")
  if (!file.exists(manifestPath)) {
    stop("No manifest.json found in the exported folder package.")
  }

  manifest <- jsonlite::read_json(manifestPath, simplifyVector = TRUE)
  folderStructure <- manifest$folderStructure
  # Ensure folderStructure is a data.frame (jsonlite returns list() for empty arrays)
  if (!is.data.frame(folderStructure)) {
    folderStructure <- NULL
  }
  treesManifest <- manifest$trees
  if (!is.data.frame(treesManifest)) {
    treesManifest <- NULL
  }

  targetResource <- loadResource(targetFolder)
  if (is.null(targetResource) || targetResource$nodeType != "Folder") {
    stop("targetFolder must be an existing Folder in the repository")
  }

  log_info(paste("Importing folder export:", baseName,
                 "- structure entries:", if (!is.null(folderStructure)) nrow(folderStructure) else 0,
                 "- trees:", if (!is.null(treesManifest)) nrow(treesManifest) else 0,
                 "- onConflict:", onConflict))

  # ============================================================
  # Upfront conflict detection (for "error" mode)
  # ============================================================
  if (onConflict == "error") {
    conflicts <- .detectImportConflicts(folderStructure, treesManifest, targetResource)
    if (length(conflicts) > 0) {
      stop(paste0("Import aborted: ", length(conflicts), " conflict(s) found in target folder.\n",
                   "  Conflicting resources:\n",
                   paste0("  - ", conflicts, collapse = "\n"), "\n",
                   "Use onConflict='skip' to reuse existing or onConflict='overwrite' to replace content."))
    }
  }

  # Build id mapping: old resourceId -> new resourceId
  idMapping <- new.env(parent = emptyenv())

  # ============================================================
  # Phase 1: Create folder structure (top-down by path depth)
  # ============================================================
  if (!is.null(folderStructure) && nrow(folderStructure) > 0) {
    folders <- folderStructure[folderStructure$nodeType == "Folder", ]
    if (nrow(folders) > 0) {
      # Sort by depth (number of path separators)
      folders$depth <- vapply(folders$relativePath, function(p) {
        length(strsplit(p, "/", fixed = TRUE)[[1]])
      }, integer(1))
      folders <- folders[order(folders$depth), ]

      for (i in seq_len(nrow(folders))) {
        entry <- folders[i, ]
        parentResource <- .resolveParentResource(entry$parentRelativePath, targetResource, idMapping)
        if (!is.null(parentResource)) {
          newFolder <- createFolder(parentResource, entry$name)
          if (!is.null(newFolder)) {
            idMapping[[entry$resourceId]] <- newFolder$resourceId
            log_info(paste("Created folder:", entry$relativePath))
          }
        }
      }
    }
  }

  # ============================================================
  # Phase 2: Create analysis trees and import their steps
  # ============================================================
  workflowJsonPath <- file.path(importDir, "workflow.json")
  internalLinks <- NULL

  if (!is.null(treesManifest) && nrow(treesManifest) > 0 && file.exists(workflowJsonPath)) {
    importWF <- jsonlite::read_json(workflowJsonPath, simplifyVector = TRUE)

    if (!is.null(importWF) && nrow(importWF) > 0) {
      # Create the analysis trees first
      for (i in seq_len(nrow(treesManifest))) {
        treeEntry <- treesManifest[i, ]
        parentResource <- .resolveParentResource(treeEntry$parentRelativePath, targetResource, idMapping)
        if (!is.null(parentResource)) {
          newTree <- createAnalysisTree(parentResource, treeEntry$treeName)
          if (!is.null(newTree)) {
            log_info(paste("Created analysis tree:", treeEntry$relativePath))
            # Store the tree resource for step import targeting
            idMapping[[treeEntry$relativePath]] <- newTree$resourceId
          }
        }
      }

      # Build workflow environment from importWF (same as importWorkflow)
      workflow <- createWorkflow()
      workflow$isImporting <- TRUE

      for (i in 1:nrow(importWF)) {
        stepEnv <- createStepEnv(stepDf = importWF[i, ], workflow = workflow)
      }

      # Load internal links
      internalLinksPath <- file.path(importDir, "internalLinks.json")
      if (file.exists(internalLinksPath)) {
        internalLinks <- jsonlite::read_json(internalLinksPath, simplifyVector = TRUE)

        if (!is.null(internalLinks) && nrow(internalLinks) > 0) {
          for (j in 1:nrow(internalLinks)) {
            link <- internalLinks[j, ]
            sourceStep <- workflow$steps[[link$sourceStep]]
            targetStep <- workflow$steps[[link$targetStep]]

            if (!is.null(sourceStep) && !is.null(targetStep)) {
              if (!(link$targetStep %in% ls(sourceStep$usage))) {
                sourceStep$usage[[link$targetStep]] <- targetStep
              }
              if (!(link$sourceStep %in% ls(targetStep$dependencies))) {
                targetStep$dependencies[[link$sourceStep]] <- sourceStep
              }
            }
          }
          workflow$internalLinks <- internalLinks
        }
      } else {
        internalLinks <- data.frame()
        workflow$internalLinks <- internalLinks
      }

      workflow$isImporting <- FALSE

      # Create import template
      workflowTemplate <- createWorkflowTemplateForImport(workflow, internalLinks)

      # Set the tree root folder for each step template based on which tree it belongs to
      # We need to map each step to its tree's new location
      .setStepTreeTargets(workflowTemplate, treesManifest, targetResource, idMapping)

      # Upload links and apply mappings (reuse shared helpers)
      linkMapping <- uploadAndMapLinks(importDir, folderFile, baseName, targetResource)
      applyOutsideLinkMapping(importWF, workflowTemplate, linkMapping)
      applyToolMappingFromFile(folderFile, baseName, workflowTemplate)
      validateWorkflowTools(workflowTemplate)

      # Execute import in dependency order
      orderedWorkflow <- workflowTemplate$createExecutionPlan()
      importWF <- importStepsInOrder(orderedWorkflow, workflowTemplate, importWF, importDir)

      # Internal links are now pre-resolved in importStepsInOrder before realise,
      # so the second pass is no longer needed for folder imports.

      # Restore parent relationships
      restoreParentRelationships(importWF)
    }
  }

  # ============================================================
  # Phase 3: Upload standalone folder files
  # ============================================================
  if (!is.null(folderStructure) && nrow(folderStructure) > 0) {
    fileEntries <- folderStructure[folderStructure$nodeType == "File", ]
    if (nrow(fileEntries) > 0) {
      folderFilesDir <- file.path(importDir, "folderFiles")
      for (i in seq_len(nrow(fileEntries))) {
        entry <- fileEntries[i, ]
        if (!is.na(entry$fileKey) && entry$fileKey != "") {
          localPath <- file.path(folderFilesDir, entry$fileKey)
          if (file.exists(localPath)) {
            parentResource <- .resolveParentResource(entry$parentRelativePath, targetResource, idMapping)
            if (!is.null(parentResource)) {
              tryCatch({
                newFile <- .importFile(parentResource, entry$name, localPath, onConflict)
                if (!is.null(newFile)) {
                  idMapping[[entry$resourceId]] <- newFile$resourceId
                  log_info(paste("Created file:", entry$relativePath))
                }
              }, error = function(e) {
                log_warn(paste("Failed to create file:", entry$name, "-", e$message))
              })
            }
          }
        }
      }
    }
  }

  # ============================================================
  # Phase 4: Resolve folder-level links
  # ============================================================
  if (!is.null(folderStructure) && nrow(folderStructure) > 0) {
    linkEntries <- folderStructure[folderStructure$nodeType == "Link", ]
    if (nrow(linkEntries) > 0) {
      for (i in seq_len(nrow(linkEntries))) {
        entry <- linkEntries[i, ]
        parentResource <- .resolveParentResource(entry$parentRelativePath, targetResource, idMapping)
        if (is.null(parentResource)) next

        # Try to resolve link target
        targetEntityId <- NULL

        # Check if target is within export scope (internal link)
        if (!is.na(entry$linkTargetEntityId)) {
          # Check idMapping for internal targets (by resourceId or entityId)
          mappedId <- idMapping[[entry$linkTargetEntityId]]
          if (!is.null(mappedId)) {
            targetEntityId <- mappedId
          } else {
            # Try link mapping: outside links may have been mapped to target repo entities
            if (exists("linkMapping") && !is.null(linkMapping)) {
              for (key in ls(linkMapping)) {
                mappedEntityId <- linkMapping[[key]]
                if (!is.null(mappedEntityId)) {
                  tryCatch({
                    mappedRes <- loadResource(mappedEntityId)
                    if (!is.null(mappedRes) && !is.null(entry$linkTargetPath) &&
                        basename(entry$linkTargetPath) == mappedRes$name) {
                      targetEntityId <- mappedRes$resourceId
                      log_info(paste("Resolved folder link via link mapping:", entry$name,
                                     "->", mappedRes$name, "(", mappedEntityId, ")"))
                      break
                    }
                  }, error = function(e) NULL)
                }
              }
            }
          }

          if (is.null(targetEntityId)) {
            # Try by linkTargetPath: resolve relative to target folder
            if (!is.null(entry$linkTargetPath) && !is.na(entry$linkTargetPath)) {
              for (key in ls(idMapping)) {
                tryCatch({
                  newRes <- loadResource(idMapping[[key]])
                  if (!is.null(newRes) && !is.null(newRes$name) &&
                      basename(entry$linkTargetPath) == newRes$name) {
                    targetEntityId <- idMapping[[key]]
                    break
                  }
                }, error = function(e) NULL)
              }
            }

            # Fallback: try to find target by its original entity ID (external link)
            if (is.null(targetEntityId)) {
              tryCatch({
                extTarget <- loadResource(entry$linkTargetEntityId)
                if (!is.null(extTarget)) {
                  targetEntityId <- extTarget$resourceId
                }
              }, error = function(e) {
                log_warn(paste("Could not resolve link target for:", entry$name,
                               "- target entity:", entry$linkTargetEntityId))
              })
            }
          }
        }

        if (!is.null(targetEntityId)) {
          tryCatch({
            targetRes <- loadResource(targetEntityId)
            createLink(parentResource, targetRes, linkName = entry$name)
            log_info(paste("Created link:", entry$relativePath))
          }, error = function(e) {
            log_warn(paste("Failed to create link:", entry$name, "-", e$message))
          })
        } else {
          log_warn(paste("Could not resolve link target for:", entry$name,
                         "- target entity:", entry$linkTargetEntityId))
        }
      }
    }

    # Handle ExtLinks
    extLinkEntries <- folderStructure[folderStructure$nodeType == "ExtLink", ]
    if (nrow(extLinkEntries) > 0) {
      for (i in seq_len(nrow(extLinkEntries))) {
        entry <- extLinkEntries[i, ]
        parentResource <- .resolveParentResource(entry$parentRelativePath, targetResource, idMapping)
        if (!is.null(parentResource) && !is.na(entry$url)) {
          tryCatch({
            createExternalLink(parentResource, entry$name, entry$url)
            log_info(paste("Created external link:", entry$relativePath))
          }, error = function(e) {
            log_warn(paste("Failed to create external link:", entry$name, "-", e$message))
          })
        }
      }
    }
  }

  # Clean up import directory
  tryCatch({
    rootImportDir <- dirname(importDir)
    if (file.exists(rootImportDir) && startsWith(basename(rootImportDir), ".import_")) {
      unlink(rootImportDir, recursive = TRUE, force = TRUE)
      log_info(paste("Cleaned up import folder:", rootImportDir))
    }
  }, error = function(e) {
    warning(paste("Failed to clean up import folder:", e$message))
  })

  invisible(targetResource)
}


# ============================================================
# Internal helpers
# ============================================================

#' Resolve parent resource from relative path using id mapping
#'
#' @param parentRelativePath Relative path of the parent.
#' @param rootResource Root target resource.
#' @param idMapping Environment mapping old resourceIds to new ones.
#' @return Resource data frame for the parent, or rootResource if parentRelativePath is empty.
#' @keywords internal
.resolveParentResource <- function(parentRelativePath, rootResource, idMapping) {
  if (is.null(parentRelativePath) || is.na(parentRelativePath) || parentRelativePath == "") {
    return(rootResource)
  }

  # Try to find parent by walking the path in the target
  # The parent should already have been created and mapped
  parentPath <- paste0(rootResource$path, "/", parentRelativePath)
  tryCatch({
    parent <- loadResource(parentPath)
    if (!is.null(parent)) return(parent)
  }, error = function(e) {
    # Fall through to NULL
  })

  log_warn(paste("Could not resolve parent resource for path:", parentRelativePath))
  return(NULL)
}


#' Set tree targets for each step template during folder import
#'
#' Maps each step to its correct analysis tree in the target repository.
#'
#' @param workflowTemplate The workflow template environment.
#' @param treesManifest Data frame of tree entries from manifest.
#' @param targetResource Root target folder resource.
#' @param idMapping Environment mapping old to new resource IDs.
#' @keywords internal
.setStepTreeTargets <- function(workflowTemplate, treesManifest, targetResource, idMapping) {
  for (stepName in names(workflowTemplate$stepTemplates)) {
    template <- workflowTemplate$stepTemplates[[stepName]]
    originalTreeName <- template$stepDf$treeName

    # Find which tree this step belongs to
    matchedTree <- NULL
    if (!is.null(originalTreeName)) {
      for (i in seq_len(nrow(treesManifest))) {
        if (treesManifest$treeName[i] == originalTreeName) {
          matchedTree <- treesManifest[i, ]
          break
        }
      }
    }

    if (!is.null(matchedTree)) {
      # Look up new tree location
      newTreeResourceId <- idMapping[[matchedTree$relativePath]]
      if (!is.null(newTreeResourceId)) {
        newTree <- loadResource(newTreeResourceId)
        if (!is.null(newTree)) {
          template$stepDf$treeIdent <- newTree$resourceId
          template$stepDf$treePath <- dirname(newTree$path)
          template$stepDf$treeName <- newTree$name
          next
        }
      }
    }

    # Fallback: set tree root to target folder (tree will be auto-created by realise)
    template$stepDf$treePath <- targetResource$path
  }
}


#' Detect import conflicts in target folder
#'
#' Scans the target folder recursively for resources that would conflict with
#' the import manifest entries.
#'
#' @param folderStructure Data frame of folder structure from manifest.
#' @param treesManifest Data frame of trees from manifest.
#' @param targetResource The target folder resource.
#' @return Character vector of conflict descriptions (empty if no conflicts).
#' @keywords internal
.detectImportConflicts <- function(folderStructure, treesManifest, targetResource) {
  conflicts <- character()

  # Check folder structure entries (Folders, Files, Links, ExtLinks)
  if (!is.null(folderStructure) && nrow(folderStructure) > 0) {
    for (i in seq_len(nrow(folderStructure))) {
      entry <- folderStructure[i, ]
      parentPath <- if (is.na(entry$parentRelativePath) || entry$parentRelativePath == "") {
        targetResource$path
      } else {
        paste0(targetResource$path, "/", entry$parentRelativePath)
      }

      tryCatch({
        parentRes <- loadResource(parentPath)
        if (!is.null(parentRes)) {
          children <- loadChildResources(parentRes)$data[[1]]
          if (!is.null(children) && nrow(children) > 0 && entry$name %in% children$name) {
            conflicts <- c(conflicts, paste0(entry$nodeType, ": ", entry$relativePath))
          }
        }
      }, error = function(e) {
        # Parent doesn't exist yet — no conflict possible
      })
    }
  }

  # Check analysis trees
  if (!is.null(treesManifest) && nrow(treesManifest) > 0) {
    for (i in seq_len(nrow(treesManifest))) {
      treeEntry <- treesManifest[i, ]
      parentPath <- if (is.na(treeEntry$parentRelativePath) || treeEntry$parentRelativePath == "") {
        targetResource$path
      } else {
        paste0(targetResource$path, "/", treeEntry$parentRelativePath)
      }

      tryCatch({
        parentRes <- loadResource(parentPath)
        if (!is.null(parentRes)) {
          children <- loadChildResources(parentRes)$data[[1]]
          if (!is.null(children) && nrow(children) > 0 &&
              treeEntry$treeName %in% children$name) {
            conflicts <- c(conflicts, paste0("Analysis Tree: ", treeEntry$relativePath))
          }
        }
      }, error = function(e) {
        # Parent doesn't exist yet — no conflict possible
      })
    }
  }

  return(conflicts)
}


#' Import a single file with onConflict handling
#'
#' Creates a file or handles the conflict based on onConflict mode.
#'
#' @param parentResource The parent folder resource.
#' @param fileName Name of the file.
#' @param localPath Local path to the file content.
#' @param onConflict One of "skip", "overwrite", "error".
#' @return The file resource, or NULL on failure.
#' @keywords internal
.importFile <- function(parentResource, fileName, localPath, onConflict) {
  # Check if file already exists
  children <- loadChildResources(parentResource)$data[[1]]
  existingFile <- NULL
  if (!is.null(children) && nrow(children) > 0) {
    match <- children[children$name == fileName, ]
    if (nrow(match) == 1 && match$nodeType == "File") {
      existingFile <- loadResource(match)
    }
  }

  if (!is.null(existingFile)) {
    # File already exists — handle according to onConflict
    if (onConflict == "skip") {
      log_info(paste("Skipping existing file:", fileName, "in", parentResource$path))
      return(existingFile)
    } else if (onConflict == "overwrite") {
      log_info(paste("Overwriting existing file:", fileName, "in", parentResource$path))
      localPath <- normalizePath(localPath)
      updatedFile <- updateFileContent(existingFile, localPath,
                                       comment = "Updated by importFolder (overwrite)")
      return(updatedFile)
    }
    # "error" mode should never reach here (caught by upfront detection)
  }

  # File does not exist — create it normally
  return(createFile(parentResource, fileName, localPath))
}
