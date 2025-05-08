#' Create Content Cache
#'
#' Creates a content cache for the specified location.
#'
#' @param localPath The local path for the content cache.
#' @export
createContentCache <- function(localPath) {
  checkInit()
  command <- glue::glue("contentCache create -location {localPath} -userProfile {cliEnv$userProfile}")
  executeCli(command)
}
