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
  renewAccessToken()
  token <- conf()$reqToken

  if (hasPicocli()) {
    args <- c("push",
              "--access-token", token,
              "-C", localPath)
    if (!is.null(comment)) args <- c(args, "-m", comment)
    if (force) args <- c(args, "--force")
    if (preview) args <- c(args, "--preview")
    if (!is.null(includeFiles)) args <- c(args, "--include-files", paste(includeFiles, collapse = ","))
    if (!is.null(excludeFiles)) args <- c(args, "--exclude-files", paste(excludeFiles, collapse = ","))
    if (!is.null(files)) args <- c(args, files)
  } else {
    args <- c("push",
              "-accessToken", token,
              "-repository", localPath)
    if (!is.null(comment)) args <- c(args, "-comment", comment)
    if (!is.null(includeFiles)) args <- c(args, "-includeFiles", paste(includeFiles, collapse = ","))
    if (!is.null(excludeFiles)) args <- c(args, "-excludeFiles", paste(excludeFiles, collapse = ","))
  }
  output <- executeCli(args, json = json)

  if (!preview) {
    resource <- tryCatch(getLocalRepoResource(localPath),
                         error = function(e) {
                           log_warn("pushCli: could not read local repo info for '", localPath,
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
