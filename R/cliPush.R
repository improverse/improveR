#' Push CLI
#'
#' Pushes local changes to the improve server. After pushing, the resource
#' cache is invalidated to ensure subsequent queries reflect the updated
#' server state.
#'
#' @param localPath The local repository path.
#' @param comment Optional commit comment.
#' @param force Logical. If \code{TRUE}, overwrite remote changes (skip conflict check).
#' @param preview Logical. If \code{TRUE}, preview only without writing to the server.
#' @param includeFiles Character. Comma-separated glob patterns to include.
#' @param excludeFiles Character. Comma-separated glob patterns to exclude.
#' @param files Character vector. Optional specific file/directory paths to push.
#' @param json Logical. If \code{TRUE}, requests JSON output and returns a
#'   parsed R list (useful for \code{preview = TRUE}).
#' @return CLI output: character vector of stdout lines when \code{json = FALSE},
#'   or a parsed list when \code{json = TRUE}. Returned invisibly.
#' @export
pushCli <- function(localPath, comment = NULL, force = FALSE, preview = FALSE,
                    includeFiles = NULL, excludeFiles = NULL, files = NULL,
                    json = FALSE) {
  localPath <- gsub("^['\"]|['\"]$", "", localPath)
  checkInit()

  # preview is answered here and the CLI is never called (IMR-283). The
  # released CLI has no --preview, and the previous code declared the option,
  # dropped it, and pushed anyway - a dry run that writes. The clone carries
  # the hash of every file at checkout, so what a push would send is a local
  # computation, exact and without touching the server.
  if (isTRUE(preview)) {
    changes <- previewPush(localPath)
    log_info("push preview: ", nrow(changes), " change(s) would be sent from ", localPath)
    return(invisible(changes))
  }
  if (isTRUE(force)) {
    stop("pushCli(force = TRUE) is not supported by this CLI (version ",
         cliDetectedVersion(), "): the option is not passed on, and the push ",
         "would run without it. Call without force (IMR-283).", call. = FALSE)
  }

  renewAccessToken()
  token <- conf()$reqToken

  args <- c("push",
            "-accessToken", token,
            "-repository", localPath)
  if (!is.null(comment)) args <- c(args, "-comment", comment)
  if (!is.null(includeFiles)) args <- c(args, "-includeFiles", paste(includeFiles, collapse = ","))
  if (!is.null(excludeFiles)) args <- c(args, "-excludeFiles", paste(excludeFiles, collapse = ","))
  output <- executeCli(args, json = json)

  if (!preview) {
    resource <- tryCatch(getLocalRepoResource(localPath),
                         error = function(e) {
                           log_warn("pushCli: could not read local repo info for '", localPath,
                                    "': ", conditionMessage(e),
                                    ". Cache invalidation skipped - subsequent reads may return stale data.")
                           NULL
                         })
    if (!is.null(resource)) {
      unloadResource(resource$resourceId)
      unloadChildResources(resource$resourceId)
    }
  }

  invisible(output)
}
