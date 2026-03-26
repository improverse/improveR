#' Push Run CLI
#'
#' Pushes and runs a command on the CLI. After pushing, the resource cache is
#' invalidated to ensure subsequent queries reflect the updated server state.
#'
#' @param localPath The local repository path.
#' @param command The command to run. Default is "Rstudio".
#' @export
pushRunCli <- function(localPath, command = "Rstudio") {
  checkInit()
  renewAccessToken()
  accessToken <- conf()$reqToken
  command <- glue::glue("push run -accessToken {accessToken} -repository {localPath} -command {command}")
  executeCli(command)

  # Invalidate caches — push run changes files/inventory/run status on the server
  resource <- tryCatch(getLocalRepoResource(localPath), error = function(e) NULL)
  if (!is.null(resource)) {
    unloadResource(resource$resourceId)
    unloadChildResources(resource$resourceId)
  }
}
