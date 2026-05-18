#' Get R Script or RDS Object from improve Repository
#'
#' Retrieves an R script (.R) or R data object (.rds) from the improve repository
#' and returns metadata including the local file path. The file is downloaded but
#' not loaded into memory - use \code{sourceR()} to execute scripts or \code{readRDS()}
#' on the returned path to load RDS objects.
#'
#' @param ident Path, resource ID, or entity ID of the R file. Can be a relative
#'   path (from current step), absolute path, or improve identifier
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()} (current
#'   step location)
#' @param addAsLink Logical. If \code{TRUE}, creates a link in the inventory for
#'   provenance tracking. Use \code{improveClean()} at workflow end to clean up links
#' @param caption Custom caption text. If empty, defaults to entityID and lastModified
#'   timestamp
#' @param description Custom description text. If empty, defaults to filename
#' @param refresh If TRUE, invalidate cached file content and resource metadata for `ident` before fetching. Defaults to FALSE.
#'
#' @returns A data frame with the following columns:
#'   \describe{
#'     \item{caption}{Character. Source information including entity version ID and
#'       last modified timestamp}
#'     \item{path}{Character. Local file system path to the downloaded R file. Use this
#'       with \code{source()} or \code{readRDS()}}
#'     \item{entityId}{Character. Entity identifier on improve server}
#'     \item{name}{Character. Filename of the R script or RDS object}
#'     \item{description}{Character. Description text (defaults to filename)}
#'     \item{resource}{List. Complete resource metadata from improve server (29 fields)}
#'   }
#'
#' @details
#' Unlike \code{getData()} which loads data into memory, \code{getR()} only downloads
#' the R file and returns its metadata. The file is cached locally at the path specified
#' in the \code{path} column.
#'
#' Common workflows:
#' \itemize{
#'   \item For R scripts: Use \code{sourceR()} to execute, or \code{source(result$path)}
#'   \item For RDS objects: Use \code{readRDS(result$path)} to load the object
#'   \item For metadata only: Access \code{result$entityId}, \code{result$name}, etc.
#' }
#'
#' @examples
#' \dontrun{
#' # Get R script metadata
#' script_info <- getR("analysis/prepare_data.R")
#' script_path <- script_info$path
#'
#' # Load and execute the script
#' source(script_path)
#'
#' # Or use sourceR() helper (automatically sources)
#' sourceR("analysis/prepare_data.R")
#'
#' # Get RDS object
#' model_info <- getR("models/final_model.rds")
#' model <- readRDS(model_info$path)
#' }
#'
#' @seealso
#' \code{\link{sourceR}} for executing R scripts directly,
#' \code{\link{getData}} for loading data tables,
#' \code{\link{improveInit}} for initializing R modules
#'
#' @references ics1141
#' @export
getR <- function(ident,from=pwd(),addAsLink=TRUE,caption="",description="",refresh = FALSE) {
  if (refresh) .refreshGetCaches(ident, from)
  return(
    getAbstract(
      ident = ident,
      from = from,
      addAsLink = addAsLink,
      addIdToName = TRUE,
      caption = caption,
      description = description,
      folderName = "R",
      func = getDesc,
      refresh = refresh
    )
  )
}


#' Source R Scripts from improve Repository
#'
#' Downloads and sources R scripts stored in the improve repository. If the target
#' is a folder or list of resources, the function recursively sources each script.
#'
#' @param ident Path, resource ID, or entity ID of the R script.
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#' @param addAsLink Logical. If \code{TRUE}, creates a link in the inventory for
#'   provenance tracking. Use \code{improveClean()} at workflow end to clean up links.
#'
#' @returns Invisibly returns \code{NULL}. Scripts are sourced for their side effects.
#'
#' @details
#' This function wraps \code{\link{getR}} to download the script, then calls
#' \code{source()} on the local file path. When multiple resources are returned,
#' each is sourced in sequence. Sourcing executes code in the current session, so
#' ensure the script content is trusted and compatible with your environment.
#'
#' @seealso \code{\link{getR}} to download scripts or RDS metadata,
#'   \code{\link{improveInit}} for module initialization
#'
#' @examples
#' \dontrun{
#' # Source a single script
#' sourceR("analysis/prepare_data.R")
#' }
#'
#' @param refresh If TRUE, invalidate cached file content and resource metadata for `ident` before fetching. Defaults to FALSE.
#' @references ics1141
#' @export
sourceR <- function(ident,from=pwd(),addAsLink=TRUE,refresh = FALSE) {

  rObject <- getR(ident=ident,from = from,addAsLink = addAsLink, refresh = refresh)
  if (is.null(rObject$path)) {
    if (length(rObject)==0) {
      log_error("No R script found")
      log_error(ident)
      return()
    }
    for (i in 1:length(rObject)) {
        sourceR(rObject[i][[1]]$resource,addAsLink = addAsLink, refresh = refresh)
    }
  }  else {

source(
  normalizePath(rObject$path,winslash = "/")
)
  }

}


#' Initialize R Modules from improve Repository
#'
#' Connects to improve, sources a module definition script, and runs its init
#' function to return a module environment.
#'
#' @param ident Path, resource ID, or entity ID of the module definition script.
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#' @param logLevel Log level passed to \code{\link{improveConnect}}.
#' @param secure Logical. If \code{TRUE}, validates certificates when connecting.
#'
#' @returns An environment returned by the module's \code{<module>.init} function,
#'   or \code{NULL} if initialization fails.
#'
#' @details
#' The function expects a module definition file with a name pattern like
#' \code{myModule.R}. After sourcing it, \code{improveInit()} looks for a function
#' named \code{myModule.init} and calls it with the module root path. The root
#' path is resolved from the parent directory of the module script.
#'
#' @seealso \code{\link{sourceR}} to source scripts without initialization,
#'   \code{\link{improveConnect}} to connect explicitly
#'
#' @examples
#' \dontrun{
#' # Initialize a module and capture its environment
#' module_env <- improveInit("modules/myModule.R")
#' }
#'
#' @export
improveInit <- function(ident, from = pwd(), logLevel = "INFO", secure = TRUE) {
  log_info("initialising module for ", ident)
  improveConnect(logLevel = logLevel, secure = secure)
  initFile <- loadResource(ident, from)
  if (is.null(initFile)) {
    log_error("no module definition found at", ident, from)
    return(NULL)
  }
  src <- sourceR(initFile)

  initFileBaseName <- strsplit(initFile$name, ".", fixed = T)[[1]]
  if (length(initFileBaseName) < 2) {
    log_error(
      "There needs to be at least one . in the filename, ",
      initFile$name
    )
    return(NULL)
  }
  initFileBaseName <- paste(
    initFileBaseName[1:length(initFileBaseName) - 1],
    collapse = "."
  )
  initFunctionName <- paste0(initFileBaseName, ".init")
  myInit <- match.fun(initFunctionName)
  #getrootpath for further resolution
  directory <- loadResource(initFile$parentId)
  rootPath <- directory$path
  env1 <- myInit(rootPath)
  return(env1)
}
