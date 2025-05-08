#' Pull CLI
#'
#' Pulls changes from the CLI.
#'
#' @param localPath The local repository path.
#' @export
pullCli <- function(localPath) {
  checkInit()
  command <- glue::glue("pull -repository {localPath}")
  executeCli(command)
}
