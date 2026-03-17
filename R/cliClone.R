#' Clone CLI Resource
#'
#' Clones a resource from the CLI.
#'
#' @param ident The identifier of the resource to clone.
#' @param localPath The local path where the resource will be cloned.
#' @export
cloneCli <- function(ident, localPath) {
  checkInit()
  resource <- loadResource(ident)
  if (is.null(resource)) {
    stop(paste(ident, "does not exist in", getCICOApiURL()))
  }
  shellFile <- cliPath()
  renewAccessToken()
  accessToken <- conf()$reqToken
  command <- glue::glue("clone -accessToken {accessToken} -resource {resource$entityId} -repository {localPath} -userProfile {cliEnv$userProfile}")
  executeCli(command)
}
