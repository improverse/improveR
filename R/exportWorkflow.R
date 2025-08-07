# Internal helper function to filter outside links
filterOutsideLinks <- function(workflow) {
   outsideLinks <- byNotEmptyAsDf(workflow,function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      remoteFiles$targetStep <- workflowTask$fullName
      if (!("sourceStep" %in% names(remoteFiles))) {
        remoteFiles$sourceStep<-NA
      }
      oLinks <- remoteFiles[remoteFiles$asLink & is.na(remoteFiles$sourceStep),]
      return(oLinks)
    }
    return(NULL)
  })
  
  # Handle case where no outside links exist
  if (!is.null(outsideLinks) && nrow(outsideLinks) > 0) {
    outsideLinks <- outsideLinks %>%
      dplyr::distinct(.data$targetStep,.data$version,.keep_all = T)
  }
  return(outsideLinks)
}


#' Export a Workflow to a Portable Zip File
#'
#' This function exports a workflow to a zip file that can be imported into another repository.
#' It preserves workflow structure, step dependencies, file relationships, and includes all
#' necessary files for reconstruction in a different environment.
#'
#' @param workflow Workflow environment object created by \code{\link{createWorkflow}} containing
#'   the steps and their relationships to export.
#' @param workflowName Character. Name for the exported workflow (without extension). This will
#'   be used as the zip filename and internal folder name.
#' @param targetFolder Character. Directory path where the zip file will be created. 
#'   Defaults to current directory (".").
#'
#' @details
#' The export process includes the following steps:
#' \enumerate{
#'   \item Creates a workflow template from the provided workflow
#'   \item Creates a temporary export directory structure
#'   \item Generates workflow.json with complete step definitions
#'   \item Exports internal links between steps to internalLinks.json
#'   \item Downloads and includes external link files
#'   \item Clones each step's input and output files
#'   \item Creates mapping templates for links and tools
#'   \item Packages everything into a single zip file
#' }
#'
#' @section Export Structure:
#' The exported zip file contains:
#' \itemize{
#'   \item \code{workflow.json} - Complete workflow metadata and step definitions
#'   \item \code{internalLinks.json} - Dependencies between workflow steps
#'   \item \code{links/} - Directory containing external file dependencies
#'   \item Step folders (named by handle) containing:
#'     \itemize{
#'       \item \code{inputFiles/} - Input files for the step
#'       \item \code{outputFiles/} - Output files from the step execution
#'     }
#' }
#'
#' @section Generated Mapping Templates:
#' The function also creates template mapping files alongside the zip:
#' 
#' \strong{<workflowName>LinkMapping.json} - Template for mapping external files:
#' \preformatted{
#' [
#'   {
#'     "key": "file-identifier",
#'     "ident": "",  # Fill in target repository resource ID
#'     "name": "original-filename",
#'     "hash": "file-hash-for-validation"
#'   }
#' ]
#' }
#'
#' \strong{<workflowName>ToolMapping.json} - Template for tool configuration mapping:
#' \preformatted{
#' [
#'   {
#'     "key": "server:::tool:::instance:::gridTool",
#'     "runserverLabel": "",  # Fill in target server
#'     "toolLabel": "",       # Fill in target tool
#'     "toolInstance": "",    # Fill in target instance
#'     "gridTool": true/false
#'   }
#' ]
#' }
#'
#' @return Character. Path to the created zip file.
#'
#' @examples
#' \dontrun{
#' # Create and export a workflow
#' wf <- createWorkflow()
#' # ... add steps to workflow ...
#' 
#' # Export to current directory
#' exportWorkflow(wf, "myWorkflow")
#' # Creates: myWorkflow.zip, myWorkflowLinkMapping.json, myWorkflowToolMapping.json
#' 
#' # Export to specific directory
#' exportWorkflow(wf, "analysis_v2", targetFolder = "/exports/workflows")
#' 
#' # The generated mapping files can be edited before import to:
#' # - Point links to existing resources in target repository
#' # - Map tools to different compute servers
#' # - Adapt to different environment configurations
#' }
#'
#' @seealso \code{\link{importWorkflow}} for importing exported workflows
#'
#' @export
exportWorkflow <- function(workflow,workflowName,targetFolder=".") {
  # Create a template from the workflow for consistent export format
  workflowTemplate <- workflow$createTemplate()
  
  #list of external links
  workFlowDf<- workflowTemplate$df()
  
  # Check if workflow is empty
  if (is.null(workFlowDf) || nrow(workFlowDf) == 0) {
    warning("Cannot export empty workflow - workflow contains no steps")
    return(invisible(NULL))
  }
  
  # Convert parentIdent from resourceId to parent's fullName for portability
  if ("parentIdent" %in% names(workFlowDf)) {
    # Create a mapping of resourceIds to fullNames
    resourceToFullName <- setNames(workFlowDf$fullName, workFlowDf$sourceEntityId)
    
    # Replace parentIdent resourceIds with parent's fullName
    for (i in seq_len(nrow(workFlowDf))) {
      if (!is.na(workFlowDf$parentIdent[i])) {
        parentFullName <- resourceToFullName[workFlowDf$parentIdent[i]]
        if (!is.null(parentFullName)) {
          workFlowDf$parentFullName[i] <- parentFullName
        }
      }
    }
    # Remove the resourceId-based parentIdent since we now have parentFullName
    # We'll restore it during import
    workFlowDf$parentIdent <- NULL
  }
  
  outsideLinks <- filterOutsideLinks(workFlowDf)
  
  # Only select columns if outsideLinks exist
  if (!is.null(outsideLinks) && nrow(outsideLinks) > 0) {
    # Check which columns exist before selecting
    availableCols <- intersect(names(outsideLinks), 
                               c("stepHandle","targetStep","name","version","filehash","maxVersion"))
    if (length(availableCols) > 0) {
      outsideLinks <- dplyr::select(outsideLinks, all_of(availableCols))
    }
  }

  insideLinks <- byNotEmptyAsDf(workflowTemplate$df(),function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      oLinks <- remoteFiles[remoteFiles$asLink & !is.na(remoteFiles$sourceStep),]
      if (nrow(oLinks)>0) {
        return(oLinks %>% dplyr::distinct(.data$targetStep,.data$sourceStep,.data$sourceInventoryPath,.keep_all = T))
      }
    }
    return(NULL)
  })



  inputs <- byNotEmptyAsDf(workFlowDf,function(workflowTask) {
    remoteFiles <- workflowTask$remoteFiles[[1]]
    if (!is.null(remoteFiles)) {
      oLinks <- remoteFiles[!remoteFiles$asLink,]
      if (nrow(oLinks)>0) {
        # Only set name from resource if it's not already specified
        # This preserves subfolder paths like "input/data.csv"
        for (i in seq_len(nrow(oLinks))) {
          if (is.na(oLinks$name[i]) || oLinks$name[i] == "") {
            oLinks$name[i] <- loadResource(oLinks$ident[i])$name
          }
        }
        oLinks$targetStep<-workflowTask$fullName
      }
      return(oLinks)
    }
    return(NULL)
  })
  
  # Only select columns if inputs exist and have the required columns
  if (!is.null(inputs) && nrow(inputs) > 0) {
    availableCols <- intersect(names(inputs), c("stepHandle","targetStep","name"))
    if (length(availableCols) > 0) {
      inputs <- dplyr::select(inputs, all_of(availableCols))
    }
  }

  #pull all steps
  workflowFolder <- file.path(targetFolder,workflowName,fsep = "/")
  # Remove existing folder if it exists to ensure clean export
  if (dir.exists(workflowFolder)) {
    unlink(workflowFolder, recursive = TRUE, force = TRUE)
  }
  dir.create(workflowFolder)
  exportDir <- normalizePath(workflowFolder,winslash = "/")
  linkDir <- file.path(exportDir,"links",fsep = "/")
  dir.create(linkDir)


  taskDirs <- byNotEmptyAsDf(workFlowDf,function(exportTask) {
    taskDir <- file.path(exportDir,exportTask$handle)
    dir.create(taskDir)
    cloneCli(exportTask$sourceEntityId,taskDir)
    return(data.frame(taskDir=taskDir,handle=exportTask$handle))
  })

  #seperate in inputs, outside links, outputs

  if (nrow(taskDirs)>0) {
    for (iT in 1:nrow(taskDirs)) {
      taskDf <- taskDirs[iT,]
      unlink(file.path(taskDf$taskDir,".improve",fsep = "/"),recursive = T,force = T)
      #remove insideLinks
      taskInsideLinks <- NULL
      if (!is.null(insideLinks) && nrow(insideLinks) > 0) {
        taskInsideLinks <- insideLinks[insideLinks$stepHandle==taskDf$handle,]$name
      }
      if (!is.null(taskInsideLinks) && length(taskInsideLinks)>0) {
        for (i in 1:length(taskInsideLinks)) {
          insideLinkPath <- file.path(taskDf$taskDir,taskInsideLinks[i],fsep = "/")
          unlink(insideLinkPath,force = T)
        }
      }

      # Handle outside links if they exist
      taskOutsideLinks <- NULL
      if (!is.null(outsideLinks) && nrow(outsideLinks) > 0) {
        taskOutsideLinks <- outsideLinks[outsideLinks$stepHandle==taskDf$handle,]
      }
      if (!is.null(taskOutsideLinks) && nrow(taskOutsideLinks)>0) {
        for (i in 1:nrow(taskOutsideLinks)) {
          outsideLink <- taskOutsideLinks[i,]
          outsideLinkPath <- file.path(taskDf$taskDir,taskOutsideLinks[i,]$name,fsep = "/")
          createdLinks <- dir(linkDir)

          if (outsideLink$version %in% createdLinks) {
            unlink(outsideLinkPath,force = T)
          } else {
            storedLinkPath <- file.path(linkDir,outsideLink$version,fsep = "/")
            file.rename(outsideLinkPath,storedLinkPath)
          }
        }
      }

      inputFolderPath <- file.path(exportDir,
                                   paste0(taskDf$handle,"inputFiles"))
      dir.create(inputFolderPath)
      inputFiles <- NULL
      if (!is.null(inputs) && nrow(inputs) > 0) {
        inputFiles <- inputs[inputs$stepHandle==taskDf$handle,]
      }
      if (!is.null(inputFiles) && nrow(inputFiles)>0) {
        for (i in 1:nrow(inputFiles)) {
          inputPath <- file.path(taskDf$taskDir,inputFiles[i,]$name,fsep = "/")
          outputPath <- file.path(inputFolderPath,inputFiles[i,]$name,fsep = "/")
          dir.create(dirname(outputPath),recursive = T,showWarnings = F)
          file.rename(inputPath,outputPath)
        }
      }

      outputFolderPath <- file.path(exportDir,
                                    paste0(taskDf$handle,"outFiles"))
      dir.create(outputFolderPath)
      outputFiles <- dir(taskDf$taskDir,all.files = T)
      x<- lapply(outputFiles, function(outputFile) {
        if (outputFile!="." && outputFile!="..") {
          file.rename(
            file.path(taskDf$taskDir,outputFile),
            file.path(outputFolderPath,outputFile)
          )
        }
      })
      file.rename(outputFolderPath,file.path(taskDf$taskDir,"outputFiles"))
      file.rename(inputFolderPath,file.path(taskDf$taskDir,"inputFiles"))
    }
  }
  jsonlite::write_json(workFlowDf,
                       file.path(exportDir,"workflow.json"),
                       pretty = T)

  # Export internal links for dependency reconstruction during import
  if (!is.null(insideLinks) && nrow(insideLinks) > 0) {
    # Save all internal link relationships data for proper reconstruction
    internalLinksExport <- insideLinks

    jsonlite::write_json(internalLinksExport,
                         file.path(exportDir, "internalLinks.json"),
                         pretty = TRUE)
  }

  #create tool mapping

  toolMapping <- byNotEmptyAsDf(workFlowDf,function(task) {
    processes <- task$processes[[1]]
    if (!is.null(processes) && nrow(processes) > 0) {
      # Check which columns exist
      requiredCols <- c("runserverLabel","toolLabel","toolInstance","gridTool")
      availableCols <- intersect(names(processes), requiredCols)
      if (length(availableCols) > 0) {
        processes <- dplyr::select(processes, all_of(availableCols))
        # Add missing columns with defaults
        for (col in setdiff(requiredCols, availableCols)) {
          processes[[col]] <- if (col == "gridTool") FALSE else ""
        }
        processes <- processes %>%
          dplyr::mutate(key=paste(.data$runserverLabel,.data$toolLabel,.data$toolInstance,.data$gridTool,sep=":::"))
        return(processes)
      }
    }
    return(NULL)
  })
  
  # Only process toolMapping if it exists and has data
  if (!is.null(toolMapping) && nrow(toolMapping) > 0) {
    toolMapping <- toolMapping %>%
      dplyr::distinct(.data$key,.keep_all = T)
    # Reorder columns if they all exist
    if (all(c("key", "runserverLabel", "toolLabel", "toolInstance", "gridTool") %in% names(toolMapping))) {
      toolMapping <- toolMapping[,c("key", "runserverLabel", "toolLabel", "toolInstance", "gridTool")]
    }
  } else {
    # Create empty tool mapping with correct structure
    toolMapping <- data.frame(
      key = character(),
      runserverLabel = character(),
      toolLabel = character(),
      toolInstance = character(),
      gridTool = logical(),
      stringsAsFactors = FALSE
    )
  }

  jsonlite::write_json(toolMapping,
                       file.path(targetFolder,
                                 paste0(workflowName,"ToolMapping.json")),
                       pretty=TRUE)

  #create link mapping
  #add stepname / treename
  outsideLinks <- filterOutsideLinks(workFlowDf)
  if (!is.null(outsideLinks) && nrow(outsideLinks)>0) {
    # Select only existing columns
    availableCols <- intersect(names(outsideLinks), 
                               c("ident","version","filehash","name"))
    if (length(availableCols) > 0) {
      outsideLinks <- outsideLinks %>%
        dplyr::select(all_of(availableCols)) %>%
        byNotEmptyAsDf(function(link) {
          sameNames <- unique(
            outsideLinks[outsideLinks$version==link$version,]$name
          )
          link$name <- paste(sameNames,collapse = ", ")
          return(link)
        }) %>%
        dplyr::distinct(.data$ident,.data$version,.data$filehash,.data$name) %>%
        dplyr::mutate(key=.data$version)
      # Reorder columns if they all exist
      if (all(c("key", "ident", "version", "filehash", "name") %in% names(outsideLinks))) {
        outsideLinks <- outsideLinks[,c("key", "ident", "version", "filehash", "name")]
      }
    }
    jsonlite::write_json(outsideLinks,
                       file.path(targetFolder,
                                 paste0(workflowName,"LinkMapping.json")),
                       pretty=TRUE)
  }


  # Change to parent directory to create proper zip structure
  oldwd <- getwd()
  setwd(targetFolder)
  zipFile <- paste0(workflowName,".zip")
  # Remove existing zip if it exists
  if (file.exists(zipFile)) {
    unlink(zipFile)
  }
  utils::zip(zipfile = zipFile, files = workflowName)
  setwd(oldwd)
  #print(workflowFolder)
  unlink(workflowFolder,force = T,recursive = T)
}
