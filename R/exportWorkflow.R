#' Export a Workflow to a Portable Zip File
#'
#' The `exportWorkflow()` function exports a workflow to a zip file that can be imported into another repository.
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
exportWorkflow <- function(workflow, workflowName, targetFolder = ".") {
  # Create a template from the workflow for consistent export format
  workflowTemplate <- workflow$createTemplate()

  #list of external links
  workFlowDf <- workflowTemplate$df()

  # Check if workflow is empty
  if (is.null(workFlowDf) || nrow(workFlowDf) == 0) {
    warning("Cannot export empty workflow - workflow contains no steps")
    return(invisible(NULL))
  }

  # Convert parentIdent from resourceId to parent's fullName for portability
  workFlowDf <- convertParentIdentsToFullNames(workFlowDf)

  allOutsideLinks <- filterOutsideLinks(workFlowDf)
  outsideLinks <- allOutsideLinks

  # Only select columns if outsideLinks exist
  if (!is.null(outsideLinks) && nrow(outsideLinks) > 0) {
    availableCols <- intersect(
      names(outsideLinks),
      c("stepHandle", "targetStep", "name", "version", "filehash", "maxVersion")
    )
    if (length(availableCols) > 0) {
      outsideLinks <- dplyr::select(outsideLinks, all_of(availableCols))
    }
  }

  insideLinks <- collectInsideLinks(workflowTemplate$df(), workflowTemplate)
  inputs <- collectInputFiles(workFlowDf)

  # Create export directory
  workflowFolder <- file.path(targetFolder, workflowName, fsep = "/")
  if (dir.exists(workflowFolder)) {
    unlink(workflowFolder, recursive = TRUE, force = TRUE)
  }
  dir.create(workflowFolder)
  exportDir <- normalizePath(workflowFolder, winslash = "/")

  # Clone steps and organize files
  exportStepFiles(workFlowDf, exportDir, insideLinks, outsideLinks, inputs)

  # Write workflow.json
  jsonlite::write_json(
    workFlowDf,
    file.path(exportDir, "workflow.json"),
    pretty = T
  )

  # Export internal links for dependencies reconstruction during import
  if (!is.null(insideLinks) && nrow(insideLinks) > 0) {
    jsonlite::write_json(
      insideLinks,
      file.path(exportDir, "internalLinks.json"),
      pretty = TRUE
    )
  }

  # Create and write tool mapping
  toolMapping <- generateToolMappingTemplate(workFlowDf)
  jsonlite::write_json(
    toolMapping,
    file.path(targetFolder, paste0(workflowName, "ToolMapping.json")),
    pretty = TRUE
  )

  # Create and write link mapping
  linkMappingDf <- generateLinkMappingTemplate(allOutsideLinks)
  if (!is.null(linkMappingDf)) {
    jsonlite::write_json(
      linkMappingDf,
      file.path(targetFolder, paste0(workflowName, "LinkMapping.json")),
      pretty = TRUE
    )
  }

  # Package into zip
  oldwd <- getwd()
  if (is.null(oldwd)) oldwd <- tempdir()
  setwd(targetFolder)
  zipFile <- paste0(workflowName, ".zip")
  if (file.exists(zipFile)) {
    unlink(zipFile)
  }
  tryCatch(
    utils::zip(zipfile = zipFile, files = workflowName),
    finally = setwd(oldwd)
  )
  unlink(workflowFolder, force = TRUE, recursive = TRUE)
}
