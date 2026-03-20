#' Push CLI
#'
#' Pushes changes to the CLI. After pushing, the resource cache is invalidated
#' to ensure subsequent queries reflect the updated server state.
#'
#' @param localPath The local repository path.
#' @export
pushCli <- function(localPath) {
  checkInit()
  renewAccessToken()
  accessToken <- conf()$reqToken
  command <- glue::glue("push -accessToken {accessToken} -repository {localPath}")
  executeCli(command)

  # Invalidate caches — push changes files/inventory on the server
  resource <- tryCatch(getLocalRepoResource(localPath), error = function(e) NULL)
  if (!is.null(resource)) {
    unloadResource(resource$resourceId)
    unloadChildResources(resource$resourceId)
  }
}
