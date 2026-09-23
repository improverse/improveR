#' Create Content Cache
#'
#' Creates a content cache for the specified location.
#'
#' @param localPath The local path for the content cache.
#' @returns The CLI's output, invisibly: a character vector of the lines it wrote to
#'   stdout and stderr. A non-zero exit code is logged as a warning, not raised, so a
#'   returned value is no proof that the cache was created.
#' @export
createContentCache <- function(localPath) {
  checkInit()

  args <- c("contentCache", "create",
            "-location", localPath,
            "-userProfile", cliEnv$userProfile)
  executeCli(args)
}
