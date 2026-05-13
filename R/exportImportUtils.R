# Shared utilities for workflow and folder export/import operations
#
# These functions are extracted from exportWorkflow.R and importWorkflow.R
# to enable code reuse with exportFolder/importFolder.

#' Export/Import shared utilities
#' @importFrom stats setNames
#' @importFrom dplyr all_of
#' @name exportImportUtils
#' @keywords internal
NULL


# ============================================================
# Export helpers
# ============================================================

#' Filter outside links from workflow step data
#'
#' Extracts external file dependencies (asLink=TRUE with no sourceStep)
#' from workflow steps. Used by both exportWorkflow and exportFolder.
#'
#' @param workflowDf Data frame of workflow steps with remoteFiles column.
#' @return Data frame of outside links, or NULL if none found.
#' @keywords internal
filterOutsideLinks <- function(workflowDf) {
  outsideLinks <- byNotEmptyAsDf(workflowDf, function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles) && nrow(remoteFiles) > 0) {
      if (!("asLink" %in% names(remoteFiles))) return(NULL)
      remoteFiles$targetStep <- workflowTask$fullName
      if (!("sourceStep" %in% names(remoteFiles))) {
        remoteFiles$sourceStep <- NA
      }
      oLinks <- remoteFiles[remoteFiles$asLink & is.na(remoteFiles$sourceStep), ]
      if (nrow(oLinks) == 0) return(NULL)
      return(oLinks)
    }
    return(NULL)
  })

  if (!is.null(outsideLinks) && nrow(outsideLinks) > 0) {
    if ("version" %in% names(outsideLinks)) {
      outsideLinks <- outsideLinks %>%
        dplyr::distinct(.data$targetStep, .data$version, .keep_all = T)
    } else {
      outsideLinks <- outsideLinks %>%
        dplyr::distinct(.data$targetStep, .data$name, .keep_all = T)
    }
  }
  return(outsideLinks)
}


#' Collect inside (internal) links from workflow step data
#'
#' Extracts links between workflow steps (asLink=TRUE with sourceStep set).
#' Used by both exportWorkflow and exportFolder.
#'
#' @param workflowDf Data frame of workflow steps (from template$df()).
#' @param workflowTemplate The workflow template environment (needed for fallback resolution).
#' @return Data frame of internal links, or NULL if none found.
#' @keywords internal
collectInsideLinks <- function(workflowDf, workflowTemplate = NULL) {
  insideLinks <- byNotEmptyAsDf(workflowDf, function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      oLinks <- remoteFiles[
        remoteFiles$asLink & !is.na(remoteFiles$sourceStep),
      ]
      if (nrow(oLinks) > 0) {
        if (!("sourceInventoryPath" %in% names(oLinks)) && !is.null(workflowTemplate)) {
          log_warn("sourceInventoryPath missing from internal links for step; falling back to loadResource resolution")
          oLinks <- byNotEmptyAsDf(oLinks, function(oLink) {
            sourceStepName <- oLink$sourceStep
            sourceStepEnv <- workflowTemplate$stepTemplates[[sourceStepName]]

            oldStep <- loadResource(sourceStepEnv$stepDf$sourceEntityId)
            if (!is.null(oldStep)) {
              oLink$sourceInventoryPath <- paste0(
                "./",
                substr(oLink$path, nchar(oldStep$path) + 2, nchar(oLink$path))
              )
            }
            return(oLink)
          })
        }
        return(
          oLinks %>%
            dplyr::distinct(
              .data$targetStep,
              .data$sourceStep,
              .data$sourceInventoryPath,
              .keep_all = T
            )
        )
      }
    }
    return(NULL)
  })
  return(insideLinks)
}


#' Collect input files (non-link) from workflow step data
#'
#' @param workflowDf Data frame of workflow steps.
#' @return Data frame of input files, or NULL if none found.
#' @keywords internal
collectInputFiles <- function(workflowDf) {
  inputs <- byNotEmptyAsDf(workflowDf, function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      oLinks <- remoteFiles[!remoteFiles$asLink, ]
      if (nrow(oLinks) > 0) {
        for (i in seq_len(nrow(oLinks))) {
          if (is.na(oLinks$name[i]) || oLinks$name[i] == "") {
            res <- loadResource(oLinks$ident[i])
            if (!is.null(res) && !is.null(res$name)) {
              oLinks$name[i] <- res$name
            }
          }
        }
        oLinks$targetStep <- workflowTask$fullName
      }
      return(oLinks)
    }
    return(NULL)
  })

  if (!is.null(inputs) && nrow(inputs) > 0) {
    availableCols <- intersect(
      names(inputs),
      c("stepHandle", "targetStep", "name")
    )
    if (length(availableCols) > 0) {
      inputs <- dplyr::select(inputs, all_of(availableCols))
    }
  }
  return(inputs)
}


#' Clone steps and organize files into inputFiles/outputFiles
#'
#' Downloads step contents via cloneCli and separates files into
#' input and output directories. Used by both exportWorkflow and exportFolder.
#'
#' @param workflowDf Data frame of workflow steps.
#' @param exportDir Path to export directory.
#' @param insideLinks Data frame of internal links (or NULL).
#' @param outsideLinks Data frame of outside links (with stepHandle, version, name columns, or NULL).
#' @param inputs Data frame of input files (with stepHandle, name columns, or NULL).
#' @return Data frame with taskDir and handle columns for each cloned step.
#' @keywords internal
exportStepFiles <- function(workflowDf, exportDir, insideLinks, outsideLinks, inputs) {
  linkDir <- file.path(exportDir, "links", fsep = "/")
  if (!dir.exists(linkDir)) {
    dir.create(linkDir)
  }

  taskDirs <- byNotEmptyAsDf(workflowDf, function(exportTask) {
    taskDir <- file.path(exportDir, exportTask$handle)
    dir.create(taskDir)
    cloneCli(exportTask$sourceEntityId, taskDir)
    return(data.frame(taskDir = taskDir, handle = exportTask$handle))
  })

  if (nrow(taskDirs) > 0) {
    for (iT in 1:nrow(taskDirs)) {
      taskDf <- taskDirs[iT, ]
      unlink(
        file.path(taskDf$taskDir, ".improve", fsep = "/"),
        recursive = T,
        force = T
      )
      # Remove insideLinks
      taskInsideLinks <- NULL
      if (!is.null(insideLinks) && nrow(insideLinks) > 0) {
        taskInsideLinks <- insideLinks[
          insideLinks$stepHandle == taskDf$handle,
        ]$name
      }
      if (!is.null(taskInsideLinks) && length(taskInsideLinks) > 0) {
        for (i in 1:length(taskInsideLinks)) {
          insideLinkPath <- file.path(
            taskDf$taskDir,
            taskInsideLinks[i],
            fsep = "/"
          )
          unlink(insideLinkPath, force = T)
        }
      }

      # Handle outside links
      taskOutsideLinks <- NULL
      if (!is.null(outsideLinks) && nrow(outsideLinks) > 0) {
        taskOutsideLinks <- outsideLinks[
          outsideLinks$stepHandle == taskDf$handle,
        ]
      }
      if (!is.null(taskOutsideLinks) && nrow(taskOutsideLinks) > 0) {
        for (i in 1:nrow(taskOutsideLinks)) {
          outsideLink <- taskOutsideLinks[i, ]
          outsideLinkPath <- file.path(
            taskDf$taskDir,
            taskOutsideLinks[i, ]$name,
            fsep = "/"
          )
          createdLinks <- dir(linkDir)

          if (outsideLink$version %in% createdLinks) {
            unlink(outsideLinkPath, force = T)
          } else {
            storedLinkPath <- file.path(
              linkDir,
              outsideLink$version,
              fsep = "/"
            )
            file.rename(outsideLinkPath, storedLinkPath)
          }
        }
      }

      inputFolderPath <- file.path(
        exportDir,
        paste0(taskDf$handle, "inputFiles")
      )
      dir.create(inputFolderPath)
      inputFiles <- NULL
      if (!is.null(inputs) && nrow(inputs) > 0) {
        inputFiles <- inputs[inputs$stepHandle == taskDf$handle, ]
      }
      if (!is.null(inputFiles) && nrow(inputFiles) > 0) {
        for (i in 1:nrow(inputFiles)) {
          inputPath <- file.path(
            taskDf$taskDir,
            inputFiles[i, ]$name,
            fsep = "/"
          )
          outputPath <- file.path(
            inputFolderPath,
            inputFiles[i, ]$name,
            fsep = "/"
          )
          dir.create(dirname(outputPath), recursive = T, showWarnings = F)
          file.rename(inputPath, outputPath)
        }
      }

      outputFolderPath <- file.path(
        exportDir,
        paste0(taskDf$handle, "outFiles")
      )
      dir.create(outputFolderPath)
      outputFiles <- dir(taskDf$taskDir, all.files = T)
      x <- lapply(outputFiles, function(outputFile) {
        if (outputFile != "." && outputFile != "..") {
          file.rename(
            file.path(taskDf$taskDir, outputFile),
            file.path(outputFolderPath, outputFile)
          )
        }
      })
      file.rename(outputFolderPath, file.path(taskDf$taskDir, "outputFiles"))
      file.rename(inputFolderPath, file.path(taskDf$taskDir, "inputFiles"))
    }
  }
  return(taskDirs)
}


#' Generate tool mapping template from workflow steps
#'
#' @param workflowDf Data frame of workflow steps.
#' @return Data frame of tool mappings with key, runserverLabel, toolLabel, toolInstance, gridTool.
#' @keywords internal
generateToolMappingTemplate <- function(workflowDf) {
  toolMapping <- byNotEmptyAsDf(workflowDf, function(task) {
    processes <- task$processes[[1]]
    if (!is.null(processes) && nrow(processes) > 0) {
      requiredCols <- c(
        "runserverLabel",
        "toolLabel",
        "toolInstance",
        "gridTool"
      )
      availableCols <- intersect(names(processes), requiredCols)
      if (length(availableCols) > 0) {
        processes <- dplyr::select(processes, all_of(availableCols))
        for (col in setdiff(requiredCols, availableCols)) {
          processes[[col]] <- if (col == "gridTool") FALSE else ""
        }
        processes <- processes %>%
          dplyr::mutate(
            key = paste(
              .data$runserverLabel,
              .data$toolLabel,
              .data$toolInstance,
              .data$gridTool,
              sep = ":::"
            )
          )
        return(processes)
      }
    }
    return(NULL)
  })

  if (!is.null(toolMapping) && nrow(toolMapping) > 0) {
    toolMapping <- toolMapping %>%
      dplyr::distinct(.data$key, .keep_all = T)
    if (
      all(
        c("key", "runserverLabel", "toolLabel", "toolInstance", "gridTool") %in%
          names(toolMapping)
      )
    ) {
      toolMapping <- toolMapping[, c(
        "key",
        "runserverLabel",
        "toolLabel",
        "toolInstance",
        "gridTool"
      )]
    }
  } else {
    toolMapping <- data.frame(
      key = character(),
      runserverLabel = character(),
      toolLabel = character(),
      toolInstance = character(),
      gridTool = logical(),
      stringsAsFactors = FALSE
    )
  }
  return(toolMapping)
}


#' Generate link mapping template from outside links
#'
#' @param allOutsideLinks Data frame of all outside links (full version, with ident/version/filehash/name).
#' @return Data frame of link mappings, or NULL if no links.
#' @keywords internal
generateLinkMappingTemplate <- function(allOutsideLinks) {
  outsideLinks <- allOutsideLinks
  if (!is.null(outsideLinks) && nrow(outsideLinks) > 0) {
    availableCols <- intersect(
      names(outsideLinks),
      c("ident", "version", "filehash", "name")
    )
    if (length(availableCols) > 0) {
      outsideLinks <- outsideLinks %>%
        dplyr::select(all_of(availableCols)) %>%
        byNotEmptyAsDf(function(link) {
          sameNames <- unique(
            allOutsideLinks[allOutsideLinks$version == link$version, ]$name
          )
          link$name <- paste(sameNames, collapse = ", ")
          return(link)
        }) %>%
        dplyr::distinct(
          .data$ident,
          .data$version,
          .data$filehash,
          .data$name
        ) %>%
        dplyr::mutate(key = .data$version)
      if (
        all(
          c("key", "ident", "version", "filehash", "name") %in%
            names(outsideLinks)
        )
      ) {
        outsideLinks <- outsideLinks[, c(
          "key",
          "ident",
          "version",
          "filehash",
          "name"
        )]
      }
    }
    return(outsideLinks)
  }
  return(NULL)
}


#' Convert parentIdent resourceIds to fullNames for portability
#'
#' @param workflowDf Data frame of workflow steps.
#' @return Modified workflowDf with parentFullName column and parentIdent removed.
#' @keywords internal
convertParentIdentsToFullNames <- function(workflowDf) {
  if ("parentIdent" %in% names(workflowDf)) {
    resourceToFullName <- setNames(
      workflowDf$fullName,
      workflowDf$sourceEntityId
    )

    workflowDf$parentFullName <- NA_character_

    for (i in seq_len(nrow(workflowDf))) {
      if (!is.na(workflowDf$parentIdent[i])) {
        parentFullName <- resourceToFullName[workflowDf$parentIdent[i]]
        if (!is.null(parentFullName)) {
          workflowDf$parentFullName[i] <- parentFullName
        }
      }
    }
    workflowDf$parentIdent <- NULL
  }
  return(workflowDf)
}


# ============================================================
# Import helpers
# ============================================================

#' Validate mapping files before import
#'
#' Validates LinkMapping.json and ToolMapping.json files.
#' Stops import if invalid mappings are found.
#'
#' @param zipFile Path to the zip file being imported.
#' @param baseName Name stem used for mapping file lookup.
#' @return TRUE invisibly if all validations pass.
#' @keywords internal
validateMappingFiles <- function(zipFile, baseName) {
  workflowDir <- dirname(normalizePath(zipFile))

  # Validate LinkMapping if it exists
  linkMappingPath <- file.path(workflowDir, paste0(baseName, "LinkMapping.json"))
  if (file.exists(linkMappingPath)) {
    log_info(paste("Validating LinkMapping file:", linkMappingPath))
    tryCatch({
      linkMapping <- jsonlite::read_json(linkMappingPath, simplifyVector = TRUE)

      if (!all(c("key") %in% names(linkMapping))) {
        warning("LinkMapping.json missing required 'key' field")
      }

      invalidMappings <- character()
      if ("ident" %in% names(linkMapping)) {
        providedIdents <- linkMapping[!is.na(linkMapping$ident) & linkMapping$ident != "", ]
        if (nrow(providedIdents) > 0) {
          for (i in seq_len(nrow(providedIdents))) {
            ident <- providedIdents$ident[i]
            tryCatch({
              resource <- loadResource(ident)
              if (is.null(resource)) {
                warning(paste("LinkMapping: Resource not found for ident:", ident, "key:", providedIdents$key[i]))
                invalidMappings <- c(invalidMappings, providedIdents$key[i])
              } else if (is.null(resource$resourceId) || is.null(resource$name)) {
                warning(paste("LinkMapping: Resource missing required fields for ident:", ident, "key:", providedIdents$key[i]))
                invalidMappings <- c(invalidMappings, providedIdents$key[i])
              } else {
                log_info(paste("LinkMapping: Validated resource", ident, "->", resource$name))
                # Check filehash if present in mapping
                if ("filehash" %in% names(providedIdents) &&
                    !is.na(providedIdents$filehash[i]) && providedIdents$filehash[i] != "") {
                  expectedHash <- providedIdents$filehash[i]
                  actualHash <- resource$fileHash
                  if (!is.null(actualHash) && !is.na(actualHash) && actualHash != expectedHash) {
                    warning(paste0("LinkMapping: File hash mismatch for key '", providedIdents$key[i],
                                   "' (", resource$name, "): expected '", expectedHash,
                                   "' but target has '", actualHash,
                                   "'. The target file may be a different version."))
                  }
                }
              }
            }, error = function(e) {
              warning(paste("LinkMapping: Invalid ident", ident, "for key:", providedIdents$key[i], "Error:", e$message))
              invalidMappings <<- c(invalidMappings, providedIdents$key[i])
            })
          }
        }
      }

      if (length(invalidMappings) > 0) {
        stop(paste("Import aborted: Invalid link mappings found for keys:", paste(invalidMappings, collapse = ", "),
                   "\nPlease correct the link mapping file and try again."))
      }

      log_info(paste("LinkMapping validation completed. Found", nrow(linkMapping), "mappings,",
                     sum(!is.na(linkMapping$ident) & linkMapping$ident != ""), "with idents"))
    }, error = function(e) {
      stop(paste("Failed to parse LinkMapping.json:", e$message))
    })
  }

  # Validate ToolMapping if it exists
  toolMappingPath <- file.path(workflowDir, paste0(baseName, "ToolMapping.json"))
  if (file.exists(toolMappingPath)) {
    log_info(paste("Validating ToolMapping file:", toolMappingPath))
    tryCatch({
      toolMapping <- jsonlite::read_json(toolMappingPath, simplifyVector = TRUE)

      requiredFields <- c("key", "runserverLabel", "toolLabel", "toolInstance", "gridTool")
      if (!all(requiredFields %in% names(toolMapping))) {
        missing <- setdiff(requiredFields, names(toolMapping))
        warning(paste("ToolMapping.json missing required fields:", paste(missing, collapse = ", ")))
      }

      filledMappings <- toolMapping[!is.na(toolMapping$runserverLabel) & toolMapping$runserverLabel != "", ]
      invalidMappings <- character()
      if (nrow(filledMappings) > 0) {
        for (i in seq_len(nrow(filledMappings))) {
          mapping <- filledMappings[i, ]
          tryCatch({
            runserver <- loadRunserver(mapping$runserverLabel)
            if (is.null(runserver) || nrow(runserver) == 0) {
              warning(paste("ToolMapping: Runserver not found:", mapping$runserverLabel, "for key:", mapping$key))
              invalidMappings <- c(invalidMappings, mapping$key)
            } else {
              tool <- loadToolForRunserver(runserver$id, mapping$toolLabel, mapping$toolInstance)
              if (is.null(tool) || nrow(tool) == 0) {
                warning(paste("ToolMapping: Tool", mapping$toolLabel, "/", mapping$toolInstance,
                              "not found on", mapping$runserverLabel, "for key:", mapping$key))
                invalidMappings <- c(invalidMappings, mapping$key)
              } else {
                log_info(paste("ToolMapping: Validated", mapping$key, "->",
                               mapping$runserverLabel, mapping$toolLabel, mapping$toolInstance))
              }
            }
          }, error = function(e) {
            warning(paste("ToolMapping: Failed to validate mapping for key:", mapping$key, "Error:", e$message))
            invalidMappings <<- c(invalidMappings, mapping$key)
          })
        }
      }

      if (length(invalidMappings) > 0) {
        stop(paste("Import aborted: Invalid tool mappings found for keys:", paste(invalidMappings, collapse = ", "),
                   "\nPlease correct the tool mapping file and try again."))
      }

      unmappedTools <- toolMapping[is.na(toolMapping$runserverLabel) | toolMapping$runserverLabel == "", ]
      if (nrow(unmappedTools) > 0) {
        warning(paste("ToolMapping: Found", nrow(unmappedTools), "unmapped tool configurations.",
                      "These will use original settings which may not work in the target environment."))
        log_info(paste("Unmapped tools:", paste(unmappedTools$key, collapse = ", ")))
      }

      log_info(paste("ToolMapping validation completed. Found", nrow(toolMapping), "mappings,",
                     nrow(filledMappings), "configured"))
    }, error = function(e) {
      stop(paste("Failed to parse ToolMapping.json:", e$message))
    })
  }

  invisible(TRUE)
}


#' Upload link files and build link mapping environment
#'
#' @param importFolder Path to extracted import directory.
#' @param zipFile Path to original zip file (for finding mapping file).
#' @param baseName Base name for mapping file lookup.
#' @param importRepoFolderResource Resource for the target folder.
#' @return An environment mapping version keys to entity IDs.
#' @keywords internal
uploadAndMapLinks <- function(importFolder, zipFile, baseName, importRepoFolderResource) {
  outsideLinkFolder <- file.path(importFolder, "links", fsep = "/")
  providedLinks <- if (dir.exists(outsideLinkFolder)) dir(outsideLinkFolder) else character(0)
  linkMappingPath <- file.path(
    dirname(normalizePath(zipFile)),
    paste0(baseName, "LinkMapping.json")
  )

  linkMapping <- new.env()
  importMapping <- data.frame()
  if (file.exists(linkMappingPath)) {
    importMapping <- jsonlite::read_json(linkMappingPath, simplifyVector = T)
    x <- byNotEmpty(importMapping, function(linkMap) {
      if (is.character(linkMap$ident) && !is.na(linkMap$ident) && linkMap$ident != "") {
        linkMapping[[linkMap$key]] <- linkMap$ident
      }
    })
  }

  if (length(providedLinks) > 0) {
    for (i in 1:length(providedLinks)) {
      if (is.null(linkMapping[[providedLinks[i]]])) {
        linkName <- providedLinks[i]
        importLine <- importMapping[importMapping$key == providedLinks[i], ]
        if (nrow(importLine) > 0 && is.character(importLine$name) && !is.na(importLine$name)) {
          # Handle comma-separated names (same file used by multiple steps) — take the first
          importedName <- importLine$name
          if (grepl(",", importedName, fixed = TRUE)) {
            importedName <- trimws(strsplit(importedName, ",")[[1]][1])
          }
          linkName <- importedName
        }
        if (startsWith(linkName, "./")) {
          linkName <- substr(linkName, 3, nchar(linkName))
        }
        linkResource <- createFile(importRepoFolderResource,
                                   fileName = linkName,
                                   localPath = file.path(outsideLinkFolder, providedLinks[i]))
        linkMapping[[providedLinks[i]]] <- linkResource$entityId
      }
    }
  }
  return(linkMapping)
}


#' Apply outside link mapping to step templates
#'
#' @param importWF Data frame from workflow.json.
#' @param workflowTemplate Workflow template environment.
#' @param linkMapping Environment with version-to-entityId mappings.
#' @keywords internal
applyOutsideLinkMapping <- function(importWF, workflowTemplate, linkMapping) {
  outsideLinks <- filterOutsideLinks(importWF)
  if (!is.null(outsideLinks) && nrow(outsideLinks) > 0 &&
      "version" %in% names(outsideLinks)) {
    mappedIdents <- vapply(outsideLinks$version, function(v) {
      if (is.null(v) || is.na(v)) return(NA_character_)
      val <- linkMapping[[v]]
      if (is.null(val)) NA_character_ else val
    }, character(1))
    outsideLinks$ident <- mappedIdents
    x <- byNotEmpty(outsideLinks, function(oL) {
      targetStepName <- oL$targetStep
      targetStep <- workflowTemplate$stepTemplates[[targetStepName]]
      if (!is.null(targetStep)) {
        targetStep$changeStepRemoteFile(oL$name, oL$asLink, oL$name, oL$ident)
      }
    })
  }
}


#' Apply tool mapping from file to step templates
#'
#' @param zipFile Path to original zip file.
#' @param baseName Base name for tool mapping file lookup.
#' @param workflowTemplate Workflow template environment.
#' @keywords internal
applyToolMappingFromFile <- function(zipFile, baseName, workflowTemplate) {
  toolMappingPath <- file.path(
    dirname(normalizePath(zipFile)),
    paste0(baseName, "ToolMapping.json")
  )
  if (file.exists(toolMappingPath)) {
    log_info(paste("Applying tool mappings from", toolMappingPath))
    toolMapping <- jsonlite::read_json(toolMappingPath, simplifyVector = TRUE)

    toolMap <- list()
    for (i in seq_len(nrow(toolMapping))) {
      mapping <- toolMapping[i, ]
      toolMap[[mapping$key]] <- mapping
    }

    for (stepName in names(workflowTemplate$stepTemplates)) {
      template <- workflowTemplate$stepTemplates[[stepName]]
      if (!is.null(template$stepDf$processes)) {
        processesList <- template$stepDf$processes[[1]]
        if (!is.null(processesList) && nrow(processesList) > 0) {
          for (p in seq_len(nrow(processesList))) {
            process <- processesList[p, ]
            processKey <- paste(process$runserverLabel, process$toolLabel, process$toolInstance, process$gridTool, sep = ":::")

            if (!is.null(toolMap[[processKey]])) {
              newMapping <- toolMap[[processKey]]
              log_info(paste("Remapping tool for step", stepName, "from", processKey, "to",
                             newMapping$runserverLabel, newMapping$toolLabel, newMapping$toolInstance))

              processesList[p, "runserverLabel"] <- newMapping$runserverLabel
              processesList[p, "toolLabel"] <- newMapping$toolLabel
              processesList[p, "toolInstance"] <- newMapping$toolInstance
              processesList[p, "gridTool"] <- newMapping$gridTool
            }
          }
          template$stepDf$processes[[1]] <- processesList
        }
      }
    }
  } else {
    log_info(paste("No tool mapping file found at", toolMappingPath, "- using original tool configurations"))
  }
}


#' Import steps in execution order: realise, push files, bind variables
#'
#' This is the core import loop shared by importWorkflow and importFolder.
#'
#' @param orderedWorkflow Data frame of steps in execution order.
#' @param workflowTemplate Workflow template environment.
#' @param importWF Data frame from workflow.json (modified in place with newEntityId).
#' @param importFolder Path to extracted import directory.
#' @return Modified importWF with newEntityId column populated.
#' @keywords internal
importStepsInOrder <- function(orderedWorkflow, workflowTemplate, importWF, importFolder) {
  executionList <- c()
  for (i in 1:nrow(orderedWorkflow)) {
    nextData <- orderedWorkflow[i, ]
    nextItem <- nextData$fullName

    template <- workflowTemplate$stepTemplates[[nextItem]]

    if ("dependencies" %in% names(nextData) && !is.na(nextData$dependencies)) {
      dependencies <- unique(strsplit(nextData$dependencies, ",")[[1]])
      for (j in 1:length(dependencies)) {
        dep <- dependencies[j]
        if (dep %in% executionList) {
          log_info("waiting to finish")
          executionList <- executionList[executionList != dep]
        }
      }
    }

    # Save original remoteFiles before filtering
    originalRemoteFiles <- template$stepDf$remoteFiles[[1]]
    if (!is.null(originalRemoteFiles) && nrow(originalRemoteFiles) > 0 &&
        "asLink" %in% names(originalRemoteFiles)) {
      remoteFiles <- originalRemoteFiles[originalRemoteFiles$asLink, ]
      if ("sourceStep" %in% names(remoteFiles) && nrow(remoteFiles) > 0) {
        internalRemoteFiles <- remoteFiles[!is.na(remoteFiles$sourceStep), ]
        externalLinks <- remoteFiles[is.na(remoteFiles$sourceStep), ]

        # Resolve internal links to already-created source step outputs
        if (nrow(internalRemoteFiles) > 0) {
          for (irf in seq_len(nrow(internalRemoteFiles))) {
            srcStepName <- internalRemoteFiles$sourceStep[irf]
            srcRow <- importWF[importWF$fullName == srcStepName, ]
            if (nrow(srcRow) > 0 && !is.na(srcRow$newEntityId[1])) {
              srcStep <- tryCatch(loadResource(srcRow$newEntityId[1]), error = function(e) NULL)
              if (!is.null(srcStep)) {
                srcInv <- getStepResourceInventory(srcStep, recurse = TRUE, update = TRUE)$data[[1]]
                searchName <- internalRemoteFiles$sourceInventoryPath[irf]
                if (!is.null(searchName) && startsWith(searchName, "./")) {
                  searchName <- substr(searchName, 3, nchar(searchName))
                }
                if (!is.null(srcInv) && nrow(srcInv) > 0) {
                  match <- srcInv[srcInv$name == searchName, ]
                  if (nrow(match) > 0) {
                    # Update ident to point to the new source output
                    internalRemoteFiles$ident[irf] <- match$entityId[1]
                    log_info(paste("Pre-resolved internal link:", searchName,
                                   "from", srcStepName, "->", nextItem))
                  }
                }
              }
            }
          }
          # Include resolved internal links with the external links for realise
          remoteFiles <- plyr::rbind.fill(externalLinks, internalRemoteFiles)
        } else {
          remoteFiles <- externalLinks
        }
        template$stepDf$remoteFiles[[1]] <- remoteFiles
      } else {
        template$stepDf$remoteFiles[[1]] <- remoteFiles
      }
    }
    template$realise(run = F)

    # Store the new entityId
    importWF[importWF$fullName == nextItem, "newEntityId"] <- template$stepDf$entityId

    nextStep <- loadResource(template$stepDf$entityId)
    stepInputFolderPath <- paste0("import", uuid::UUIDgenerate())
    dir.create(stepInputFolderPath, showWarnings = F, recursive = T)
    stepInputFolderPath <- normalizePath(stepInputFolderPath, winslash = "/")
    cloneCli(nextStep, localPath = stepInputFolderPath)

    # Push input files
    inputDir <- file.path(importFolder, template$stepDf$handle, "inputFiles", fsep = "/")
    inputFiles <- if (dir.exists(inputDir)) dir(inputDir, all.files = T) else character(0)
    inputFiles <- inputFiles[!(inputFiles %in% c(".", ".."))]
    if (length(inputFiles) > 0) {
      for (iF in 1:length(inputFiles)) {
        inputPath <- file.path(importFolder, template$stepDf$handle, "inputFiles", inputFiles[iF], fsep = "/")
        outputPath <- file.path(stepInputFolderPath, inputFiles[iF], fsep = "/")

        if (!file.exists(inputPath)) next
        if (isTRUE(file.info(inputPath)$isdir)) {
          dir.create(outputPath, recursive = TRUE, showWarnings = FALSE)
          file.copy(inputPath, dirname(outputPath), recursive = TRUE)
          unlink(inputPath, recursive = TRUE)
        } else {
          dir.create(dirname(outputPath), recursive = T, showWarnings = F)
          if (!file.rename(inputPath, outputPath)) {
            file.copy(inputPath, outputPath)
            unlink(inputPath)
          }
        }
      }
    }
    pushCli(stepInputFolderPath)

    # Bind variables for uploaded input files
    allRemoteFiles <- originalRemoteFiles
    variableFiles <- data.frame()
    if (!is.null(allRemoteFiles) && nrow(allRemoteFiles) > 0 &&
        "asLink" %in% names(allRemoteFiles) &&
        "variableName" %in% names(allRemoteFiles)) {
      variableFiles <- allRemoteFiles[!allRemoteFiles$asLink & !is.na(allRemoteFiles$variableName) & allRemoteFiles$variableName != "", ]
    }

    if (nrow(variableFiles) > 0) {
      nextStep <- loadResource(template$stepDf$entityId)
      stepProcesses <- loadProcessesForStep(nextStep$resourceId)

      for (vi in 1:nrow(variableFiles)) {
        varFile <- variableFiles[vi, ]

        tryCatch({
          variableProcess <- stepProcesses[stepProcesses$name == varFile$variableProcess, ]
          if (nrow(variableProcess) == 0) {
            warning(paste("Process not found:", varFile$variableProcess, "for variable:", varFile$variableName))
            next
          }
          processId <- as.character(variableProcess$id)

          fileName <- varFile$name
          if (startsWith(fileName, "./")) {
            fileName <- substr(fileName, 3, nchar(fileName))
          }

          stepInventoryResult <- getStepResourceInventory(nextStep, recurse = FALSE, update = TRUE)
          stepInventory <- stepInventoryResult$data[[1]]
          uploadedFile <- stepInventory[stepInventory$name == fileName, ]

          if (is.null(uploadedFile) || nrow(uploadedFile) == 0) {
            warning(paste("Uploaded file not found in inventory:", fileName, "for variable:", varFile$variableName))
            next
          }

          fileResourceId <- uploadedFile$resourceId[1]

          variables <- getProcessFileVariables(nextStep, processId)
          variableId <- as.character(variables[variables$name == varFile$variableName, ]$id)

          if (length(variableId) == 0) {
            position <- 1
            if (!is.null(variables) && nrow(variables) > 0) {
              position <- max(variables$position) + 1
            }
            variable <- createProcessFileVariable(
              ident = nextStep$resourceId,
              processId = processId,
              name = varFile$variableName,
              variableType = "fileRef",
              position = position
            )
            variableId <- variable[[1]][1]
          }

          result <- authenticatedREST(
            "/resources/{resourceId}/processes/{processId}/variables/{variableId}",
            urlParams = list(
              resourceId = nextStep$resourceId,
              processId = processId,
              variableId = variableId
            ),
            data = list(
              type = "processVariable",
              id = variableId,
              name = varFile$variableName,
              position = 1,
              valueResourceId = fileResourceId,
              variableType = "fileRef"
            ),
            restType = "PUT"
          )
        }, error = function(e) {
          warning(paste("Failed to bind variable", varFile$variableName, "for file", varFile$name, ":", e$message))
        })
      }
    }

    # Push output files
    outputDir <- file.path(importFolder, template$stepDf$handle, "outputFiles", fsep = "/")
    outputFiles <- if (dir.exists(outputDir)) dir(outputDir, all.files = T) else character(0)
    outputFiles <- outputFiles[!(outputFiles %in% c(".", ".."))]
    if (length(outputFiles) > 0) {
      for (iF in 1:length(outputFiles)) {
        inputPath <- file.path(importFolder, template$stepDf$handle, "outputFiles", outputFiles[iF], fsep = "/")
        outputPath <- file.path(stepInputFolderPath, outputFiles[iF], fsep = "/")

        if (!file.exists(inputPath)) next
        if (isTRUE(file.info(inputPath)$isdir)) {
          dir.create(outputPath, recursive = TRUE, showWarnings = FALSE)
          file.copy(inputPath, dirname(outputPath), recursive = TRUE)
          unlink(inputPath, recursive = TRUE)
        } else {
          dir.create(dirname(outputPath), recursive = T, showWarnings = F)
          if (!file.rename(inputPath, outputPath)) {
            file.copy(inputPath, outputPath)
            unlink(inputPath)
          }
        }
      }
    }
    pushRunCli(stepInputFolderPath, command = "import")
    unlink(file.path(importFolder, template$stepDf$handle), recursive = T, force = T)
    unlink(stepInputFolderPath, recursive = T, force = T)
    executionList <- c(executionList, template$stepDf$handle)
  }
  return(importWF)
}


#' Resolve internal links in second pass after all steps are created
#'
#' @param internalLinks Data frame of internal links.
#' @param importWF Data frame with fullName and newEntityId columns.
#' @keywords internal
resolveInternalLinksSecondPass <- function(internalLinks, importWF) {
  if (!is.null(internalLinks) && nrow(internalLinks) > 0) {
    log_info("Resolving internal links (second pass)")

    sourceInventories <- list()
    uniqueSources <- unique(internalLinks$sourceStep)
    for (src in uniqueSources) {
      srcRow <- importWF[importWF$fullName == src, ]
      if (nrow(srcRow) > 0 && !is.na(srcRow$newEntityId[1])) {
        srcStep <- loadResource(srcRow$newEntityId[1])
        if (!is.null(srcStep)) {
          inv <- getStepResourceInventory(srcStep, recurse = TRUE, update = TRUE)
          sourceInventories[[src]] <- inv$data[[1]]
        }
      }
    }

    for (j in seq_len(nrow(internalLinks))) {
      link <- internalLinks[j, ]
      sourceFullName <- link$sourceStep
      targetFullName <- link$targetStep

      sourceRow <- importWF[importWF$fullName == sourceFullName, ]
      targetRow <- importWF[importWF$fullName == targetFullName, ]

      if (nrow(sourceRow) == 0 || nrow(targetRow) == 0) {
        log_warn("Could not find source or target step for internal link: ",
                 sourceFullName, " -> ", targetFullName)
        next
      }

      sourceEntityId <- sourceRow$newEntityId[1]
      targetEntityId <- targetRow$newEntityId[1]

      if (is.null(sourceEntityId) || is.na(sourceEntityId) ||
          is.null(targetEntityId) || is.na(targetEntityId)) {
        log_warn("Missing entityId for internal link: ",
                 sourceFullName, " (", sourceEntityId, ") -> ",
                 targetFullName, " (", targetEntityId, ")")
        next
      }

      fileName <- link$sourceInventoryPath
      if (!is.null(fileName) && !is.na(fileName)) {
        searchName <- if (startsWith(fileName, "./")) {
          substr(fileName, 3, nchar(fileName))
        } else {
          fileName
        }

        inventory <- sourceInventories[[sourceFullName]]
        resolved <- NULL
        if (!is.null(inventory) && nrow(inventory) > 0) {
          match <- inventory[inventory$name == searchName, ]
          if (nrow(match) > 0) {
            resolved <- loadResource(match$resourceId[1])
          }
        }

        if (!is.null(resolved)) {
          tryCatch({
            linkName <- link$name
            if (!is.null(linkName) && startsWith(linkName, "./")) {
              linkName <- substr(linkName, 3, nchar(linkName))
            }
            createLink(targetEntityId, resolved, linkName = linkName)
            log_info("Created internal link: ", sourceFullName, "/", searchName,
                     " -> ", targetFullName, "/", linkName)
          }, error = function(e) {
            log_warn("Failed to create internal link: ", e$message)
          })
        } else {
          log_warn("Could not resolve internal link file: ", searchName,
                   " from source step ", sourceEntityId,
                   " (inventory has ", if (!is.null(inventory)) nrow(inventory) else 0, " items)")
        }
      }
    }
  }
}


#' Restore parent relationships after all steps are created
#'
#' @param importWF Data frame with parentFullName, fullName, and newEntityId columns.
#' @keywords internal
restoreParentRelationships <- function(importWF) {
  if ("parentFullName" %in% names(importWF) && "newEntityId" %in% names(importWF)) {
    log_info("Restoring parent relationships")

    for (i in seq_len(nrow(importWF))) {
      if (!is.na(importWF$parentFullName[i])) {
        childEntityId <- importWF$newEntityId[i]
        parentFullName <- importWF$parentFullName[i]

        parentRows <- which(importWF$fullName == parentFullName)
        if (length(parentRows) == 0) {
          warning(paste("Parent step not found:", parentFullName, "for child:", importWF$fullName[i]))
        } else if (length(parentRows) > 1) {
          warning(paste("Multiple steps found with fullName:", parentFullName,
                        "- cannot determine unique parent for:", importWF$fullName[i]))
        } else {
          parentEntityId <- importWF$newEntityId[parentRows[1]]

          if (!is.null(childEntityId) && !is.na(childEntityId) && !is.null(parentEntityId) && !is.na(parentEntityId)) {
            tryCatch({
              attachStep(childEntityId, parentEntityId)
              log_info(paste("Restored parent relationship:", importWF$fullName[i], "->", parentFullName))
            }, error = function(e) {
              warning(paste("Error restoring parent relationship for", importWF$fullName[i], ":", e$message))
            })
          }
        }
      }
    }
  }
}


#' Validate that all tool configurations in a workflow template exist on the server
#'
#' Iterates all step templates, extracts unique runserver/tool/instance combos,
#' and verifies each exists on the connected server. Stops with a clear error
#' listing all missing tools if any are invalid. This catches tool misconfiguration
#' early, before per-step realise() calls would fail and cause partial imports.
#'
#' @param workflowTemplate Workflow template environment with $stepTemplates.
#' @return TRUE invisibly if all tools are valid.
#' @keywords internal
validateWorkflowTools <- function(workflowTemplate) {
  # Collect all unique tool configs from step templates
  toolConfigs <- list()
  for (stepName in names(workflowTemplate$stepTemplates)) {
    template <- workflowTemplate$stepTemplates[[stepName]]
    if (!is.null(template$stepDf$processes)) {
      processesList <- template$stepDf$processes[[1]]
      if (!is.null(processesList) && nrow(processesList) > 0) {
        for (p in seq_len(nrow(processesList))) {
          process <- processesList[p, ]
          key <- paste(process$runserverLabel, process$toolLabel,
                       process$toolInstance, sep = ":::")
          if (is.null(toolConfigs[[key]])) {
            toolConfigs[[key]] <- list(
              runserverLabel = process$runserverLabel,
              toolLabel = process$toolLabel,
              toolInstance = process$toolInstance
            )
          }
        }
      }
    }
  }

  if (length(toolConfigs) == 0) {
    log_info("No tool configurations found in workflow template - skipping tool validation")
    return(invisible(TRUE))
  }

  invalidTools <- character()
  for (key in names(toolConfigs)) {
    config <- toolConfigs[[key]]
    tryCatch({
      runserver <- loadRunserver(config$runserverLabel)
      if (is.null(runserver) || nrow(runserver) == 0) {
        invalidTools <- c(invalidTools, paste0(key, " (runserver '",
                                                config$runserverLabel, "' not found)"))
        next
      }
      tool <- loadToolForRunserver(runserver$id, config$toolLabel, config$toolInstance)
      if (is.null(tool) || nrow(tool) == 0) {
        invalidTools <- c(invalidTools, paste0(key, " (tool '",
                                                config$toolLabel, "/", config$toolInstance,
                                                "' not found on '", config$runserverLabel, "')"))
      }
    }, error = function(e) {
      invalidTools <<- c(invalidTools, paste0(key, " (error: ", e$message, ")"))
    })
  }

  if (length(invalidTools) > 0) {
    stop(paste0("Import aborted: The following tool configurations are not available on the target server:\n",
                paste("  -", invalidTools, collapse = "\n"),
                "\nPlease provide a ToolMapping.json file to remap tools for this environment."))
  }

  log_info(paste("Tool validation passed:", length(toolConfigs), "unique tool configurations verified"))
  invisible(TRUE)
}


#' Move files between directories with cross-device fallback
#'
#' @param sourcePath Source file path.
#' @param destPath Destination file path.
#' @keywords internal
moveFile <- function(sourcePath, destPath) {
  if (!file.exists(sourcePath)) return(invisible(FALSE))
  dir.create(dirname(destPath), recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(file.info(sourcePath)$isdir)) {
    dir.create(destPath, recursive = TRUE, showWarnings = FALSE)
    file.copy(sourcePath, dirname(destPath), recursive = TRUE)
    unlink(sourcePath, recursive = TRUE)
  } else {
    if (!file.rename(sourcePath, destPath)) {
      file.copy(sourcePath, destPath)
      unlink(sourcePath)
    }
  }
  invisible(TRUE)
}
