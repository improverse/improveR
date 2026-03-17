#' Push CLI
#'
#' Pushes changes to the CLI.
#'
#' @param localPath The local repository path.
#' @export
pushCli <- function(localPath) {
  checkInit()
  renewAccessToken()
  accessToken <- conf()$reqToken
  command <- glue::glue("push -accessToken {accessToken} -repository {localPath}")
  executeCli(command)
}
