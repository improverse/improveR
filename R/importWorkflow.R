#' Import a Workflow from a zip File into a Repository Folder
#'
#' The `importWorkflow()` function imports a workflow from a specified zip file into a given repository folder.
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
#'   \item Creates steps in dependencies order
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
importWorkflow <- function(workflowFile, importRepoFolder) {
  workflowName <- strsplit(basename(workflowFile), split = ".", fixed = T)[[1]]
  workflowName <- paste(workflowName[1:(length(workflowName) - 1)], collapse = ".")

  # Validate mapping files before starting import
  validateMappingFiles(workflowFile, workflowName)

  # Use a folder in the working directory instead of tempfile for better compatibility
  importFolder <- file.path(getwd(), paste0(".import_", workflowName, "_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  dir.create(importFolder, recursive = TRUE)
  zip::unzip(zipfile = workflowFile, exdir = importFolder)

  importFolder <- dir(importFolder, full.names = T)
  if (length(importFolder) != 1) {
    stop("workflow file is expected to be a zip file containing exactly one folder.")
  }
  if (!file.exists(file.path(importFolder, "workflow.json"))) {
    stop("no workflow.json file found in the first subfolder of the zip file")
  }
  importFolder <- normalizePath(importFolder, winslash = "/")
  importRepoFolderResource <- loadResource(importRepoFolder)
  if (is.null(importRepoFolderResource) || importRepoFolderResource$nodeType != "Folder") {
    stop("importRepoFolder has to be a folder in the repository and exist")
  }

  importWF <- jsonlite::read_json(
    file.path(importFolder, "workflow.json", fsep = "/"),
    simplifyVector = T
  )
  if (nrow(importWF) == 0) {
    return(NULL)
  }
  workflow <- createWorkflow()

  # Set flag to skip collectInternalLinks during import
  workflow$isImporting <- TRUE

  for (i in 1:nrow(importWF)) {
    stepEnv <- createStepEnv(stepDf = importWF[i, ], workflow = workflow)
  }

  # Load and apply internal links if they exist
  internalLinksPath <- file.path(importFolder, "internalLinks.json", fsep = "/")
  internalLinks <- NULL
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

  # Clear import flag
  workflow$isImporting <- FALSE

  # Use the import-specific template creator
  workflowTemplate <- createWorkflowTemplateForImport(workflow, internalLinks)
  workflowTemplate$setWorkflowTreeRootFolder(importRepoFolderResource$path)

  # Upload links and apply mappings
  linkMapping <- uploadAndMapLinks(importFolder, workflowFile, workflowName, importRepoFolderResource)
  applyOutsideLinkMapping(importWF, workflowTemplate, linkMapping)
  applyToolMappingFromFile(workflowFile, workflowName, workflowTemplate)
  validateWorkflowTools(workflowTemplate)

  # Execute import in dependency order
  orderedWorkflow <- workflowTemplate$createExecutionPlan()
  importWF <- importStepsInOrder(orderedWorkflow, workflowTemplate, importWF, importFolder)

  # Second pass: resolve internal links
  resolveInternalLinksSecondPass(internalLinks, importWF)

  # Restore parent relationships
  restoreParentRelationships(importWF)

  # Clean up the import folder
  tryCatch({
    rootImportFolder <- dirname(importFolder)
    if (file.exists(rootImportFolder) && startsWith(basename(rootImportFolder), ".import_")) {
      unlink(rootImportFolder, recursive = TRUE, force = TRUE)
      log_info(paste("Cleaned up import folder:", rootImportFolder))
    }
  }, error = function(e) {
    warning(paste("Failed to clean up import folder:", e$message))
  })
}
