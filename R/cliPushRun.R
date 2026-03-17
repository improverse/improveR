#' Push Run CLI
#'
#' Pushes and runs a command on the CLI.
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
}
