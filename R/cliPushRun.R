#' Push Run CLI
#'
#' Pushes local changes and creates a run for the cloned step.
#' After pushing, the resource cache is invalidated.
#'
#' @param localPath The local repository path.
#' @param command The command executable to run. Default is "Rstudio".
#' @param comment Optional commit comment.
#' @param force Logical. If \code{TRUE}, overwrite remote changes (skip conflict check).
#' @param preview Logical. If \code{TRUE}, preview only without writing to the server.
#' @param includeFiles Character. Comma-separated glob patterns to include.
#' @param excludeFiles Character. Comma-separated glob patterns to exclude.
#' @param runserverUrl Character. Runserver URL (defaults to local IP).
#' @param commandArgs Character vector. Additional command arguments.
#'   Use for arguments that start with \code{-}.
#' @param json Logical. If \code{TRUE}, requests JSON output and returns a
#'   parsed R list (useful for \code{preview = TRUE}).
#' @return CLI output: character vector of stdout lines when \code{json = FALSE},
#'   or a parsed list when \code{json = TRUE}. Returned invisibly.
#' @export
pushRunCli <- function(localPath, command = "Rstudio", comment = NULL,
                       force = FALSE, preview = FALSE,
                       includeFiles = NULL, excludeFiles = NULL,
                       runserverUrl = NULL, commandArgs = NULL,
                       json = FALSE) {
  localPath <- gsub("^['\"]|['\"]$", "", localPath)
  checkInit()
  renewAccessToken()
  token <- conf()$reqToken

  if (hasPicocli()) {
    args <- c("push", "run",
              "--access-token", token,
              "-C", localPath,
              "--command", command)
    if (!is.null(comment)) args <- c(args, "-m", comment)
    if (force) args <- c(args, "--force")
    if (preview) args <- c(args, "--preview")
    if (!is.null(includeFiles)) {
      args <- c(args, "--include-files", paste(includeFiles, collapse = ","))
    } else {
      args <- c(args, "--include-files", "**")
    }
    if (!is.null(excludeFiles)) args <- c(args, "--exclude-files", paste(excludeFiles, collapse = ","))
    if (!is.null(runserverUrl)) args <- c(args, "--runserver-url", runserverUrl)
    if (!is.null(commandArgs)) args <- c(args, "--", commandArgs)
  } else {
    args <- c("push", "run",
              "-accessToken", token,
              "-repository", localPath,
              "-command", command)
    if (!is.null(includeFiles)) args <- c(args, "-includeFiles", paste(includeFiles, collapse = ","))
    if (!is.null(excludeFiles)) args <- c(args, "-excludeFiles", paste(excludeFiles, collapse = ","))
    if (!is.null(commandArgs)) args <- c(args, commandArgs)
  }
  output <- executeCli(args, json = json)

  if (!preview) {
    resource <- tryCatch(getLocalRepoResource(localPath),
                         error = function(e) {
                           log_warn("pushRunCli: could not read local repo info for '", localPath,
                                    "': ", conditionMessage(e),
                                    ". Cache invalidation skipped — subsequent reads may return stale data.")
                           NULL
                         })
    if (!is.null(resource)) {
      unloadResource(resource$resourceId)
      unloadChildResources(resource$resourceId)
    }
  }

  invisible(output)
}
