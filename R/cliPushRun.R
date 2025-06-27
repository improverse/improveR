#' Push Run CLI
#'
#' Pushes and runs a command on the CLI.
#'
#' @param localPath The local repository path.
#' @param command The command to run. Default is "Rstudio".
#' @export
pushRunCli <- function(localPath, command = "Rstudio") {
  checkInit()
  command <- glue::glue("push run -repository {localPath} -command {command}")
  executeCli(command)
}
