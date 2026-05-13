#' Create Content Cache
#'
#' Creates a content cache for the specified location.
#'
#' @param localPath The local path for the content cache.
#' @export
createContentCache <- function(localPath) {
  checkInit()

  if (hasPicocli()) {
    args <- c("cache", "create",
              "--profile", cliEnv$userProfile,
              "-C", localPath)
  } else {
    args <- c("contentCache", "create",
              "-location", localPath,
              "-userProfile", cliEnv$userProfile)
  }
  executeCli(args)
}
