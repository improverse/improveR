#' Pull CLI
#'
#' Pulls changes from the CLI. After pulling, the resource cache is invalidated
#' to ensure the in-memory state matches what was just downloaded.
#'
#' @param localPath The local repository path.
#' @param preview Logical. If TRUE, only preview what would be pulled.
#' @export
pullCli <- function(localPath, preview = FALSE) {
  # Tolerate callers that defensively shQuote/double-quote the path
  # (a pattern that pre-dates the system2 quoting fix in executeCli).
  localPath <- gsub("^['\"]|['\"]$", "", localPath)
  checkInit()
  renewAccessToken()
  accessToken <- conf()$reqToken

  if (hasPicocli()) {
    args <- c("pull",
              "--access-token", accessToken,
              "-C", localPath)
    if (preview) args <- c(args, "--preview")
  } else {
    args <- c("pull",
              "-accessToken", accessToken,
              "-repository", localPath)
  }
  executeCli(args)

  # Invalidate caches — pull may reflect server-side changes
  if (!preview) {
    resource <- tryCatch(getLocalRepoResource(localPath),
                         error = function(e) {
                           log_warn("pullCli: could not read local repo info for '", localPath,
                                    "': ", conditionMessage(e),
                                    ". Cache invalidation skipped — subsequent reads may return stale data.")
                           NULL
                         })
    if (!is.null(resource)) {
      unloadResource(resource$resourceId)
      unloadChildResources(resource$resourceId)
    }
  }
}
