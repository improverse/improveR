

#' Get Data Table from improve Repository
#'
#' Retrieves a data table from the improve repository and loads it into R with
#' metadata including caption and description. Automatically detects file format
#' and uses appropriate reader (.xls/.xlsx as Excel, .rds as RDS, .sas7bdat as SAS,
#' others as CSV).
#'
#' @param ident Path, resource ID, entity ID, or entity version ID of the data
#'   file. Accepts relative paths (starting with `./` or `../`), absolute paths
#'   (starting with `/`), UUIDs, or improve identifiers. See [loadResource()] for
#'   full details on identifier formats
#' @param from Root path for resolving relative paths. Defaults to `pwd()`, which
#'   is the step that initiated the R session (set via `IMPROVER_STEP` environment
#'   variable)
#' @param addAsLink Logical. If `TRUE` (default), creates a link in the inventory
#'   for provenance tracking. Use `improveClean()` at workflow end to clean up
#'   links. Set to `FALSE` for one-off loads without provenance tracking
#' @param caption Custom caption text. If empty string (default), uses
#'   "Source (Entity version ID): <entityVersionId>, Last modified on: <timestamp>"
#' @param description Custom description text. If empty string (default), uses
#'   the filename
#' @param parser Optional custom parsing function that takes a local file path as
#'   first argument and returns a data frame. Overrides automatic format detection
#' @param ... Additional arguments passed to the underlying read function:
#'   `readxl::read_excel()`, `readRDS()`, `haven::read_sas()`, or `utils::read.csv()`
#'
#' @returns A single-row data frame (descriptor) with the following columns, or
#'   `NULL` if the resource cannot be found:
#'
#'   \describe{
#'     \item{caption}{Character. Caption text for display/reporting}
#'     \item{path}{Character. Local file path (normalized with forward slashes)}
#'     \item{entityId}{Character. The improve entity ID of the resource}
#'     \item{name}{Character. Original filename from the repository}
#'     \item{description}{Character. Description text for the resource}
#'     \item{resource}{List column containing the full resource metadata data frame
#'       from the improve server (includes fields like `resourceId`, `entityVersionId`,
#'       `nodeType`, `lastModifiedOn`, etc.). Access via `result$resource[[1]]`}
#'     \item{data}{List column containing the parsed data frame. Access the actual
#'       data via `result$data[[1]]`}
#'     \item{dataType}{Character. File type detected: "Excel", "RDS", "SAS", "CSV",
#'       or "Custom parser"}
#'   }
#'
#'   If `ident` matches multiple resources, returns a data frame with multiple rows
#'   (one per resource).
#'
#' @details
#' File format detection is case-insensitive and based on file extension:
#' \itemize{
#'   \item `.xls`, `.xlsx`: Excel via `readxl::read_excel()`
#'   \item `.rds`: R Data Serialization via `readRDS()`
#'   \item `.sas7bdat`: SAS via `haven::read_sas()`
#'   \item All others: CSV via `utils::read.csv()` (note: `stringsAsFactors`
#'     behavior depends on R version)
#' }
#'
#' The returned descriptor integrates with improve's provenance tracking when
#' `addAsLink = TRUE`. The file is downloaded to a local `data/` subdirectory
#' in the current workspace.
#'
#' @examples
#' \dontrun{
#' # Load CSV data from current step
#' data_desc <- getData("analysis_data.csv")
#' df <- data_desc$data[[1]]
#'
#' # Check what was loaded
#' data_desc$name       # Original filename
#' data_desc$entityId   # improve entity ID
#' data_desc$dataType   # "CSV"
#'
#' # Access full resource metadata
#' resource_info <- data_desc$resource[[1]]
#' resource_info$lastModifiedOn
#'
#' # Load Excel file with custom caption
#' excel_desc <- getData(
#'   "results/summary.xlsx",
#'   caption = "Study Results Summary"
#' )
#'
#' # Load without provenance tracking (one-off use)
#' temp_data <- getData("temp_file.csv", addAsLink = FALSE)
#'
#' # Use custom parser for special format
#' custom_desc <- getData(
#'   "special_format.txt",
#'   parser = function(path) read.delim(path, sep = "|")
#' )
#'
#' # Handle case when resource not found
#' result <- getData("nonexistent.csv")
#' if (is.null(result)) {
#'   message("Resource not found")
#' }
#' }
#'
#' @seealso
#' \code{\link{getTextString}} for loading text files,
#' \code{\link{getGraphics}} for loading image files,
#' \code{\link{getR}} for loading R objects,
#' \code{\link{loadResource}} for loading resource metadata without file content
#'
#' @references ics1141
#' @export
getData <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="",parser=NULL,refresh = FALSE,...) {
  if (refresh) .refreshGetCaches(ident, from)
  return(
    getAbstract(ident=ident,from = from,addAsLink = addAsLink,addIdToName = T, caption = caption,description=description,folderName = "data",func=getDataByResource,parser=parser,...)
  )
}

#' Get Text File as Character String
#'
#' Retrieves a text file from the improve repository and loads it as a character
#' string with metadata including caption and description. All lines are collapsed
#' into a single string with newline separators.
#'
#' @param ident Path, resource ID, or entity ID of the text file. Can be a relative
#'   path (from current step), absolute path, or improve identifier
#' @param from Root path for resolving relative paths. Defaults to `pwd()` (current
#'   step location)
#' @param addAsLink Logical. If `TRUE`, creates a link in the inventory for
#'   provenance tracking. Use `improveClean()` at workflow end to clean up links
#' @param caption Custom caption text. If empty, defaults to entityID and lastModified
#'   timestamp
#' @param description Custom description text. If empty, defaults to filename
#' @param ... Additional arguments passed to `readLines()`
#'
#' @returns A list object (descriptor) with the following components:
#'   - `data`: Character string containing the complete file contents with lines
#'     collapsed using newline (`\n`) separators
#'   - `dataType`: Character string set to "Text"
#'   - `path`: Character string with local file path
#'   - `caption`: Character string with caption text
#'   - `description`: Character string with description text
#'   - Additional metadata fields from the resource descriptor
#'
#' @details
#' The function reads all lines from the text file using `readLines()` and collapses
#' them into a single character string. The returned descriptor integrates with
#' improve's provenance tracking when `addAsLink = TRUE`. Access the text content
#' via `result$data`.
#'
#' @examples
#' \dontrun{
#' # Load text file from current step
#' text_desc <- getTextString("report_text.txt")
#' content <- text_desc$data  # Extract text string
#'
#' # Load file with custom metadata
#' script_desc <- getTextString(
#'   "scripts/analysis.R",
#'   caption = "Main Analysis Script",
#'   description = "Primary statistical analysis code"
#' )
#' }
#'
#' @seealso
#' \code{\link{getData}} for loading data tables,
#' \code{\link{getR}} for loading R objects,
#' \code{\link{getHTML}} for loading HTML files
#'
#' @references ics1141
#' @export
getTextString <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="",refresh = FALSE,...) {
  if (refresh) .refreshGetCaches(ident, from)
  return(
    getAbstract(ident=ident,from = from,addAsLink = addAsLink,addIdToName = T,caption = caption,description=description,folderName = "text",func = getTextByResource,...)
  )
}




#' retrieves data table for rMarkdown, .xls and .xlsx files assumed to be excel, .rds as RDS, .sas7bdat as SAS all others as CSV
#'
#' @param resourceDesc resource descriptor of the data file
#'
#' @param caption by default entityID and lastModified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @param parser a custom parsing function can be handed over. It needs to take a local path as first argument
#' @param ... pass over arguments for the read function
#' @noRd
#'
getDataByResource <- function(resourceDesc,caption="",description="",parser=NULL,...) {
  resource <- loadResource(resourceDesc$resourceId)
  desc <- buildDescriptor(resource,resourceDesc$data[[1]],caption,description)
  resName <- tolower(resource$name)
  if (!is.null(parser)) {
    desc$data <- list(parser(desc$path,...))
    desc$dataType <- "Custom parser"
  }
  else if (endsWith(resName,".xls") | endsWith(resName,".xlsx")) {
    desc$data <- list(readxl::read_excel(path=desc$path,...))
    desc$dataType <- "Excel"
  } else if (endsWith(resName,".rds")){
    desc$data <- list(readRDS(desc$path))
    desc$dataType <- "RDS"
  } else if (endsWith(resName,".sas7bdat")){
    desc$data <- list(haven::read_sas(desc$path,...))
    desc$dataType <- "SAS"
  } else {
    desc$data <-list( utils::read.csv(file=desc$path,...))
    desc$dataType <- "CSV"
  }
  return(desc)
}

#' retrieves text string for rMarkdown
#'
#' @param resourceDesc resource descriptor of the picture
#'
#' @param caption by default entityID and lastModified are the caption, here alternative text can be provided
#' @param description by default filename is the caption, here alternative text can be provided
#' @param ... pass over arguments for the read function
#' @noRd
#'
getTextByResource <- function(resourceDesc,caption="",description="",...) {
  resource <- loadResource(resourceDesc$resourceId)
  desc <- buildDescriptor(resource,resourceDesc$data[[1]],caption,description)
  resName <- tolower(resource$name)
  desc$data <- paste(readLines(desc$path), collapse="\n")
  desc$dataType <- "Text"
  return(desc)
}


