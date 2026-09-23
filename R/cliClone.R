#' Clone CLI Resource
#'
#' Clones a resource from the CLI.
#'
#' @param ident The identifier of the resource to clone.
#' @param localPath The local path where the resource will be cloned.
#' @returns The CLI's output, invisibly: a character vector of the lines it wrote to
#'   stdout and stderr. A non-zero exit code is logged as a warning, not raised, so a
#'   returned value is no proof that the clone succeeded - check the local path.
#'   Stops with an error when `ident` does not exist in the repository.
#' @export
cloneCli <- function(ident, localPath) {
  # Tolerate callers that defensively shQuote/double-quote the path
  # (a pattern that pre-dates the system2 quoting fix in executeCli).
  localPath <- gsub("^['\"]|['\"]$", "", localPath)
  checkInit()
  resource <- loadResource(ident)
  if (is.null(resource)) {
    stop(paste(ident, "does not exist in", getCICOApiURL()))
  }
  renewAccessToken()
  accessToken <- conf()$reqToken

  args <- c("clone",
            "-accessToken", accessToken,
            "-resource", resource$entityId,
            "-repository", localPath,
            "-userProfile", cliEnv$userProfile)
  executeCli(args)
}
