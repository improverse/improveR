#' Clone CLI Resource
#'
#' Clones a resource from the CLI.
#'
#' @param ident The identifier of the resource to clone.
#' @param localPath The local path where the resource will be cloned.
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

  if (hasPicocli()) {
    args <- picoArgs("clone",
              "--access-token", accessToken,
              "--profile", cliProfileName(),
              "-C", localPath,
              resource$entityId)
  } else {
    args <- c("clone",
              "-accessToken", accessToken,
              "-resource", resource$entityId,
              "-repository", localPath,
              "-userProfile", cliEnv$userProfile)
  }
  executeCli(args)
}
