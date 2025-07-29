#' Pull CLI
#'
#' Pulls changes from the CLI.
#'
#' @param localPath The local repository path.
#' @export
pullCli <- function(localPath) {
  checkInit()
  accessToken <- conf()$reqToken
  command <- glue::glue("pull -accessToken {accessToken} -repository {localPath}")
  executeCli(command)
}
