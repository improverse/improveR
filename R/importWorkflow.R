# Internal function to validate mapping files before import
.validateMappingFiles <- function(workflowFile, workflowName) {
  workflowDir <- dirname(normalizePath(workflowFile))
  
  # Validate LinkMapping if it exists
  linkMappingPath <- file.path(workflowDir, paste0(workflowName, "LinkMapping.json"))
  if (file.exists(linkMappingPath)) {
    logging::loginfo(paste("Validating LinkMapping file:", linkMappingPath))
    tryCatch({
      linkMapping <- jsonlite::read_json(linkMappingPath, simplifyVector = TRUE)
      
      # Check required fields
      if (!all(c("key") %in% names(linkMapping))) {
        warning("LinkMapping.json missing required 'key' field")
      }
      
      # Validate idents if provided - stop import if any are invalid
      invalidMappings <- character()
      if ("ident" %in% names(linkMapping)) {
        providedIdents <- linkMapping[!is.na(linkMapping$ident) & linkMapping$ident != "", ]
        if (nrow(providedIdents) > 0) {
          for (i in seq_len(nrow(providedIdents))) {
            ident <- providedIdents$ident[i]
            validationFailed <- FALSE
            tryCatch({
              resource <- loadResource(ident)
              if (is.null(resource)) {
                warning(paste("LinkMapping: Resource not found for ident:", ident, "key:", providedIdents$key[i]))
                invalidMappings <- c(invalidMappings, providedIdents$key[i])
                validationFailed <- TRUE
              } else if (is.null(resource$resourceId) || is.null(resource$name)) {
                warning(paste("LinkMapping: Resource missing required fields for ident:", ident, "key:", providedIdents$key[i]))
                invalidMappings <- c(invalidMappings, providedIdents$key[i])
                validationFailed <- TRUE
              } else {
                logging::loginfo(paste("LinkMapping: Validated resource", ident, "->", resource$name))
              }
            }, error = function(e) {
              warning(paste("LinkMapping: Invalid ident", ident, "for key:", providedIdents$key[i], "Error:", e$message))
              invalidMappings <- c(invalidMappings, providedIdents$key[i])
              validationFailed <- TRUE
            })
          }
        }
      }
      
      # Stop import if there are invalid mappings
      if (length(invalidMappings) > 0) {
        stop(paste("Import aborted: Invalid link mappings found for keys:", paste(invalidMappings, collapse = ", "),
                   "\nPlease correct the link mapping file and try again."))
      }
      
      logging::loginfo(paste("LinkMapping validation completed. Found", nrow(linkMapping), "mappings,",
                           sum(!is.na(linkMapping$ident) & linkMapping$ident != ""), "with idents"))
    }, error = function(e) {
      stop(paste("Failed to parse LinkMapping.json:", e$message))
    })
  }
  
  # Validate ToolMapping if it exists
  toolMappingPath <- file.path(workflowDir, paste0(workflowName, "ToolMapping.json"))
  if (file.exists(toolMappingPath)) {
    logging::loginfo(paste("Validating ToolMapping file:", toolMappingPath))
    tryCatch({
      toolMapping <- jsonlite::read_json(toolMappingPath, simplifyVector = TRUE)
      
      # Check required fields
      requiredFields <- c("key", "runserverLabel", "toolLabel", "toolInstance", "gridTool")
      if (!all(requiredFields %in% names(toolMapping))) {
        missing <- setdiff(requiredFields, names(toolMapping))
        warning(paste("ToolMapping.json missing required fields:", paste(missing, collapse = ", ")))
      }
      
      # Validate filled mappings - stop import if any are invalid
      filledMappings <- toolMapping[!is.na(toolMapping$runserverLabel) & toolMapping$runserverLabel != "", ]
      invalidMappings <- character()
      if (nrow(filledMappings) > 0) {
        for (i in seq_len(nrow(filledMappings))) {
          mapping <- filledMappings[i, ]
          # Check if runserver exists
          validationFailed <- FALSE
          tryCatch({
            runserver <- loadRunserver(mapping$runserverLabel)
            if (is.null(runserver) || nrow(runserver) == 0) {
              warning(paste("ToolMapping: Runserver not found:", mapping$runserverLabel, "for key:", mapping$key))
              invalidMappings <- c(invalidMappings, mapping$key)
              validationFailed <- TRUE
            } else {
              # Check if tool exists on runserver
              tool <- loadToolForRunserver(runserver$id, mapping$toolLabel, mapping$toolInstance)
              if (is.null(tool) || nrow(tool) == 0) {
                warning(paste("ToolMapping: Tool", mapping$toolLabel, "/", mapping$toolInstance, 
                            "not found on", mapping$runserverLabel, "for key:", mapping$key))
                invalidMappings <- c(invalidMappings, mapping$key)
                validationFailed <- TRUE
              } else {
                logging::loginfo(paste("ToolMapping: Validated", mapping$key, "->", 
                                     mapping$runserverLabel, mapping$toolLabel, mapping$toolInstance))
              }
            }
          }, error = function(e) {
            warning(paste("ToolMapping: Failed to validate mapping for key:", mapping$key, "Error:", e$message))
            invalidMappings <- c(invalidMappings, mapping$key)
            validationFailed <- TRUE
          })
        }
      }
      
      # Stop import if there are invalid mappings
      if (length(invalidMappings) > 0) {
        stop(paste("Import aborted: Invalid tool mappings found for keys:", paste(invalidMappings, collapse = ", "),
                   "\nPlease correct the tool mapping file and try again."))
      }
      
      # Warn about unmapped tools
      unmappedTools <- toolMapping[is.na(toolMapping$runserverLabel) | toolMapping$runserverLabel == "", ]
      if (nrow(unmappedTools) > 0) {
        warning(paste("ToolMapping: Found", nrow(unmappedTools), "unmapped tool configurations.",
                     "These will use original settings which may not work in the target environment."))
        logging::loginfo(paste("Unmapped tools:", paste(unmappedTools$key, collapse = ", ")))
      }
      
      logging::loginfo(paste("ToolMapping validation completed. Found", nrow(toolMapping), "mappings,",
                           nrow(filledMappings), "configured"))
    }, error = function(e) {
      stop(paste("Failed to parse ToolMapping.json:", e$message))
    })
  }
  
  invisible(TRUE)
}

#' Import a Workflow from a Zip File into a Repository Folder
#'
#' This function imports a workflow from a specified zip file into a given repository folder.
#' It reconstructs the workflow structure, maintains step dependencies, handles file links,
#' and applies optional tool and link mappings for environment portability.
#'
#' @param workflowFile Character. Path to the workflow zip file to import. The zip file should
#'   contain a single folder with:
#'   \itemize{
#'     \item \code{workflow.json} - Required. Contains workflow metadata and step definitions
#'     \item \code{internalLinks.json} - Optional. Defines dependencies between workflow steps
#'     \item \code{links/} - Optional directory containing external file dependencies
#'     \item Step folders named by handle containing \code{inputFiles/} and \code{outputFiles/}
#'   }
#' @param importRepoFolder Character. Path to the repository folder where the workflow will be imported.
#'   Must be an existing folder in the improve repository.
#'
#' @details
#' The import process includes the following steps:
#' \enumerate{
#'   \item Extracts the workflow zip file to a temporary directory
#'   \item Validates the workflow structure and required files
#'   \item Creates workflow and step template environments
#'   \item Reconstructs step dependencies from internalLinks.json
#'   \item Uploads external link files to the repository
#'   \item Applies optional link mappings from \code{<workflowName>LinkMapping.json}
#'   \item Applies optional tool mappings from \code{<workflowName>ToolMapping.json}
#'   \item Creates steps in dependency order
#'   \item Uploads input and output files for each step
#' }
#'
#' @section Mapping Files:
#' Two optional JSON mapping files can be placed alongside the workflow zip:
#' 
#' \strong{LinkMapping.json} - Maps external file references to existing repository resources:
#' \preformatted{
#' [
#'   {
#'     "key": "unique-file-identifier",
#'     "ident": "repository-resource-id",
#'     "name": "optional-display-name"
#'   }
#' ]
#' }
#'
#' \strong{ToolMapping.json} - Maps tool configurations between environments:
#' \preformatted{
#' [
#'   {
#'     "key": "sourceServer:::sourceTool:::sourceInstance:::gridTool",
#'     "runserverLabel": "targetServer",
#'     "toolLabel": "targetTool",
#'     "toolInstance": "targetInstance",
#'     "gridTool": true/false
#'   }
#' ]
#' }
#'
#' @return None. The function is called for its side effects. Steps are created in the
#'   target repository with preserved dependencies and file relationships.
#'
#' @examples
#' \dontrun{
#' # Basic import
#' importWorkflow("myWorkflow.zip", "/Projects/TargetFolder")
#' 
#' # Import with link mapping
#' # Create myWorkflowLinkMapping.json in same directory as zip
#' # Then import:
#' importWorkflow("myWorkflow.zip", "/Projects/TargetFolder")
#' 
#' # Import with tool mapping for different environment
#' # Create myWorkflowToolMapping.json for server configuration
#' importWorkflow("myWorkflow.zip", "/Projects/Production")
#' }
#'
#' @seealso \code{\link{exportWorkflow}} for creating workflow export files
#'
#' @importFrom zip unzip
#' @importFrom jsonlite read_json
#' @importFrom uuid UUIDgenerate
#' @export
importWorkflow <- function(workflowFile,importRepoFolder) {
  workflowName <- strsplit(basename(workflowFile),split = ".",fixed = T)[[1]]
  workflowName <- paste(workflowName[1:(length(workflowName)-1)],collapse = ".")

  # Validate mapping files before starting import
  .validateMappingFiles(workflowFile, workflowName)
  
  importFolder <- tempfile()
  dir.create(importFolder)
  zip::unzip(zipfile = workflowFile,exdir = importFolder)

  importFolder <- dir(importFolder,full.names = T)
  if (length(importFolder)!=1) {
    stop("workflow file is expected to be a zip file containing exactly one folder.")
  }
  if (!file.exists(file.path(importFolder,"workflow.json"))) {
    stop("no workflow.json file found in the first subfolder of the zip file")
  }
  importFolder <- normalizePath(importFolder,winslash = "/")
  importRepoFolderResource <- loadResource(importRepoFolder)
  if (is.null(importRepoFolderResource) || importRepoFolderResource$nodeType!="Folder") {
    stop("importRepoFolder has to be a folder in the repository and exist")
  }


  importWF <- jsonlite::read_json(
    file.path(importFolder,"workflow.json",fsep = "/"),
    simplifyVector = T
  )
  if (nrow(importWF)==0) {
    return(NULL)
  }
  workflow<-createWorkflow()
  
  # Set flag to skip collectInternalLinks during import
  workflow$isImporting <- TRUE
  
  for (i in 1:nrow(importWF)) {
    stepEnv <- createStepEnv(stepDf = importWF[i,],workflow = workflow)
  }
  
  # Load and apply internal links if they exist
  internalLinksPath <- file.path(importFolder, "internalLinks.json", fsep = "/")
  internalLinks <- NULL  # Initialize to NULL
  if (file.exists(internalLinksPath)) {
    internalLinks <- jsonlite::read_json(internalLinksPath, simplifyVector = TRUE)
    
    # Reconstruct step dependencies using saved relationships
    if (!is.null(internalLinks) && nrow(internalLinks) > 0) {
      for (j in 1:nrow(internalLinks)) {
        link <- internalLinks[j,]
        sourceStep <- workflow$steps[[link$sourceStep]]
        targetStep <- workflow$steps[[link$targetStep]]
        
        if (!is.null(sourceStep) && !is.null(targetStep)) {
          # Establish usage relationship (source uses target's output)
          if (!(link$targetStep %in% ls(sourceStep$usage))) {
            sourceStep$usage[[link$targetStep]] <- targetStep
          }
          # Establish lineage relationship (target depends on source)
          if (!(link$sourceStep %in% ls(targetStep$lineage))) {
            targetStep$lineage[[link$sourceStep]] <- sourceStep
          }
        }
      }
      workflow$internalLinks <- internalLinks
    }
  } else {
    # Initialize as empty data frame when no internal links file exists
    internalLinks <- data.frame()
    workflow$internalLinks <- internalLinks
  }
  
  # Clear import flag
  workflow$isImporting <- FALSE
  
  # Use the import-specific template creator that doesn't rely on entity IDs
  workflowTemplate <- createWorkflowTemplateForImport(workflow, internalLinks)
  # Use the path from the loaded resource
  workflowTemplate$setWorkflowTreeRootFolder(importRepoFolderResource$path)



  #uploadLinks
  #map outsideLinks
  outsideLinkFolder <- file.path(importFolder,"links",fsep = "/")
  providedLinks <- dir(outsideLinkFolder)
  linkMappingPath <- file.path(
    dirname(normalizePath(workflowFile)),
    paste0(workflowName,"LinkMapping.json")
    )

  #TODO check with ident / version /SHA
  linkMapping <- new.env()
  importMapping <- data.frame()
  if (file.exists(linkMappingPath)) {
    importMapping <-jsonlite::read_json(linkMappingPath,simplifyVector = T)
    x<-byNotEmpty(importMapping,function(linkMap){
      if (is.character(linkMap$ident) && linkMap$ident!="") {
        linkMapping[[linkMap$key]]<-linkMap$ident
      }
    })
  }


  if (length(providedLinks)>0) {
    for (i in 1:length(providedLinks)) {
      if (is.null(linkMapping[[providedLinks[i]]])) {
        linkName <- providedLinks[i]
        importLine <- importMapping[importMapping$key==providedLinks[i],]
        if (nrow(importLine) >0 && is.character(importLine$name) && !grepl(pattern = ",",x = importLine$name,fixed = T)) {
          linkName <- importLine$name
        }
        if (startsWith(linkName,"./")) {
          linkName<- substr(linkName,3,nchar(linkName))
        }
        linkResource <- createFile(importRepoFolderResource,
                                   fileName = linkName,
                                   localPath = file.path(outsideLinkFolder,providedLinks[i]))
        linkMapping[[providedLinks[i]]] <- linkResource$entityId
      }

    }
  }


  outsideLinks <- filterOutsideLinks(importWF)
  mappedIdents <- unlist(
    lapply(outsideLinks$version,function(v){return(linkMapping[[v]])})
  )
  outsideLinks$ident<-mappedIdents
  x<-byNotEmpty(outsideLinks,function(oL) {
    #changeStepRemoteFileDf(oL$stepHandle,oL$name,oL)
    targetStepName <- oL$targetStep
    targetStep <- workflowTemplate$stepTemplates[[targetStepName]]
    targetStep$changeStepRemoteFile(oL$name,oL$asLink,oL$name,oL$ident)
  })

  # Map tools to target environment
  toolMappingPath <- file.path(
    dirname(normalizePath(workflowFile)),
    paste0(workflowName,"ToolMapping.json")
  )
  if (file.exists(toolMappingPath)) {
    logging::loginfo(paste("Applying tool mappings from", toolMappingPath))
    toolMapping <- jsonlite::read_json(toolMappingPath, simplifyVector = TRUE)
    
    # Create a mapping lookup
    toolMap <- list()
    for (i in seq_len(nrow(toolMapping))) {
      mapping <- toolMapping[i,]
      toolMap[[mapping$key]] <- mapping
    }
    
    # Apply tool mappings to each step template
    for (stepName in names(workflowTemplate$stepTemplates)) {
      template <- workflowTemplate$stepTemplates[[stepName]]
      if (!is.null(template$stepDf$processes)) {
        processesList <- template$stepDf$processes[[1]]
        if (!is.null(processesList) && nrow(processesList) > 0) {
          for (p in seq_len(nrow(processesList))) {
            process <- processesList[p,]
            # Create key from current process configuration
            processKey <- paste(process$runserverLabel, process$toolLabel, process$toolInstance, process$gridTool, sep=":::")
            
            # Check if we have a mapping for this tool configuration
            if (!is.null(toolMap[[processKey]])) {
              newMapping <- toolMap[[processKey]]
              logging::loginfo(paste("Remapping tool for step", stepName, "from", processKey, "to", 
                                   newMapping$runserverLabel, newMapping$toolLabel, newMapping$toolInstance))
              
              # Update the process with new tool configuration
              processesList[p, "runserverLabel"] <- newMapping$runserverLabel
              processesList[p, "toolLabel"] <- newMapping$toolLabel
              processesList[p, "toolInstance"] <- newMapping$toolInstance
              processesList[p, "gridTool"] <- newMapping$gridTool
            }
          }
          # Update the template with modified processes
          template$stepDf$processes[[1]] <- processesList
        }
      }
    }
  } else {
    logging::loginfo(paste("No tool mapping file found at", toolMappingPath, "- using original tool configurations"))
  }


  orderedWorkflow  <- workflowTemplate$createExecutionPlan()
  executionList <- c()
  for (i in 1:nrow(orderedWorkflow)) {
    nextData <- orderedWorkflow[i,]
    nextItem <- nextData$fullName

    template <- workflowTemplate$stepTemplates[[nextItem]]


    if ("lineage" %in% names(nextData) && !is.na(nextData$lineage)) {
      dependencies <- unique(strsplit(nextData$lineage,",")[[1]])
      for (j in 1:length(dependencies)) {
        dependency <- dependencies[j]
        if (dependency %in% executionList) {
          logging::loginfo("waiting to finish")
          #finishRun(dependency)
          executionList <- executionList[executionList!=dependency]
        }
      }
    }
    #create step, add links
    #remove inputfiles
    remoteFiles <- template$stepDf$remoteFiles[[1]]
    remoteFiles <- remoteFiles[remoteFiles$asLink,]
    template$stepDf$remoteFiles[[1]]<- remoteFiles
    template$realise(run=F)
    
    # Store the new entityId back in importWF dataframe for parent relationship restoration
    importWF[importWF$fullName == nextItem, "newEntityId"] <- template$stepDf$entityId

    nextStep <- loadResource(template$stepDf$entityId)
    stepInputFolderPath <- paste0("import",uuid::UUIDgenerate()  )
    dir.create(stepInputFolderPath,showWarnings = F,recursive = T)
    stepInputFolderPath <- normalizePath(stepInputFolderPath,winslash = "/")
    cloneCli(nextStep,localPath = stepInputFolderPath)

    # push input files
    inputFiles <- dir(file.path(importFolder,template$stepDf$handle,"inputFiles",fsep = "/"),all.files = T)
    inputFiles <- inputFiles[!(inputFiles %in% c(".",".."))]
    if (length(inputFiles)>0) {
      for (iF in 1:length(inputFiles)) {
        inputPath <- file.path(importFolder,template$stepDf$handle,"inputFiles",inputFiles[iF],fsep = "/")
        outputPath <- file.path(stepInputFolderPath,inputFiles[iF],fsep = "/")
        
        # Check if it's a directory
        if (file.info(inputPath)$isdir) {
          # For directories, use recursive copy
          dir.create(outputPath, recursive = TRUE, showWarnings = FALSE)
          file.copy(inputPath, dirname(outputPath), recursive = TRUE)
          unlink(inputPath, recursive = TRUE)
        } else {
          # For files, create parent directory and move
          dir.create(dirname(outputPath),recursive = T,showWarnings = F)
          file.rename(inputPath,outputPath)
        }
      }
    }
    pushCli(stepInputFolderPath)
    #TODO map variables

    # push output files
    #push run
    outputFiles <- dir(file.path(importFolder,template$stepDf$handle,"outputFiles",fsep = "/"),all.files = T)
    outputFiles <- outputFiles[!(outputFiles %in% c(".",".."))]
    if (length(outputFiles)>0) {
      for (iF in 1:length(outputFiles)) {
        inputPath <- file.path(importFolder,template$stepDf$handle,"outputFiles",outputFiles[iF],fsep = "/")
        outputPath <- file.path(stepInputFolderPath,outputFiles[iF],fsep = "/")
        
        # Check if it's a directory
        if (file.info(inputPath)$isdir) {
          # For directories, use recursive copy
          dir.create(outputPath, recursive = TRUE, showWarnings = FALSE)
          file.copy(inputPath, dirname(outputPath), recursive = TRUE)
          unlink(inputPath, recursive = TRUE)
        } else {
          # For files, create parent directory and move
          dir.create(dirname(outputPath),recursive = T,showWarnings = F)
          file.rename(inputPath,outputPath)
        }
      }
    }
    pushRunCli(stepInputFolderPath,command="import")
    unlink(file.path(importFolder,template$stepDf$handle),recursive = T,force = T)
    unlink(stepInputFolderPath,recursive = T,force=T)
    executionList <- c(executionList,template$stepDf$handle)
  }
  #if (length(executionList)>0) {
  #  for (i in 1:length(executionList)) {
  #    finishRun(executionList[i])
  #  }
  #}
  
  # Restore parent relationships after all steps are created
  if ("parentFullName" %in% names(importWF) && "newEntityId" %in% names(importWF)) {
    logging::loginfo("Restoring parent relationships")
    
    # Update parent relationships using attachStep
    for (i in seq_len(nrow(importWF))) {
      if (!is.na(importWF$parentFullName[i])) {
        childEntityId <- importWF$newEntityId[i]
        parentFullName <- importWF$parentFullName[i]
        
        # Find the parent's entityId by looking up its fullName
        parentRows <- which(importWF$fullName == parentFullName)
        if (length(parentRows) == 0) {
          warning(paste("Parent step not found:", parentFullName, "for child:", importWF$fullName[i]))
        } else if (length(parentRows) > 1) {
          warning(paste("Multiple steps found with fullName:", parentFullName, 
                       "- cannot determine unique parent for:", importWF$fullName[i]))
        } else {
          # Exactly one parent found
          parentEntityId <- importWF$newEntityId[parentRows[1]]
          
          if (!is.null(childEntityId) && !is.null(parentEntityId) && !is.na(parentEntityId)) {
            tryCatch({
              attachStep(childEntityId, parentEntityId)
              logging::loginfo(paste("Restored parent relationship:", importWF$fullName[i], "->", parentFullName))
              
              # TODO: Handle inheritFromParent flag if needed
              # The attachStep function might not handle this flag directly
              
            }, error = function(e) {
              warning(paste("Error restoring parent relationship for", importWF$fullName[i], ":", e$message))
            })
          }
        }
      }
    }
  }
}




