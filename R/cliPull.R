#' Pull CLI
#'
#' Pulls changes from the CLI. After pulling, the resource cache is invalidated
#' to ensure the in-memory state matches what was just downloaded.
#'
#' @param localPath The local repository path.
#' @param preview Logical. If TRUE, only preview what would be pulled.
#' @returns With `preview = TRUE`, a data frame of what a pull would fetch, invisibly:
#'   one row per file with `path` and `change` (`"added"`, `"modified"` or
#'   `"deleted"`), and zero rows when the clone is up to date. The preview is computed
#'   in improveR from the clone's own baseline and one read of the resource; the CLI is
#'   not called and nothing is written (IMR-283).
#'
#'   Otherwise the CLI's output, invisibly: a character vector of the lines it wrote to
#'   stdout and stderr. A non-zero exit code is logged as a warning, not raised, so a
#'   returned value is no proof that the pull succeeded. The caches for the local
#'   repository's resource and its children are dropped afterwards; if the repository
#'   cannot be identified that step is skipped and a warning says so.
#' @export
pullCli <- function(localPath, preview = FALSE) {
  # Tolerate callers that defensively shQuote/double-quote the path
  # (a pattern that pre-dates the system2 quoting fix in executeCli).
  localPath <- gsub("^['\"]|['\"]$", "", localPath)
  checkInit()

  # Answered without pulling (IMR-283). The clone records the revision it was
  # taken from; comparing it with the resource's current revision says whether
  # there is anything to fetch. One read, no write.
  if (isTRUE(preview)) {
    changes <- previewPull(localPath)
    log_info("pull preview: ", nrow(changes), " change(s) would be fetched into ", localPath)
    return(invisible(changes))
  }

  renewAccessToken()
  accessToken <- conf()$reqToken

  args <- c("pull",
            "-accessToken", accessToken,
            "-repository", localPath)
  executeCli(args)

  # Invalidate caches - pull may reflect server-side changes
  if (!preview) {
    resource <- tryCatch(getLocalRepoResource(localPath),
                         error = function(e) {
                           log_warn("pullCli: could not read local repo info for '", localPath,
                                    "': ", conditionMessage(e),
                                    ". Cache invalidation skipped - subsequent reads may return stale data.")
                           NULL
                         })
    if (!is.null(resource)) {
      unloadResource(resource$resourceId)
      unloadChildResources(resource$resourceId)
    }
  }
}
