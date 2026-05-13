#' Pull CLI
#'
#' Pulls changes from the CLI. After pulling, the resource cache is invalidated
#' to ensure the in-memory state matches what was just downloaded.
#'
#' @param localPath The local repository path.
#' @export
pullCli <- function(localPath) {
  checkInit()
  renewAccessToken()
  accessToken <- conf()$reqToken
  command <- glue::glue("pull -accessToken {accessToken} -repository {localPath}")
  executeCli(command)

  # Invalidate caches — pull may reflect server-side changes
  resource <- tryCatch(getLocalRepoResource(localPath), error = function(e) NULL)
  if (!is.null(resource)) {
    unloadResource(resource$resourceId)
    unloadChildResources(resource$resourceId)
  }
}
