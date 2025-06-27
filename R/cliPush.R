#' Push CLI
#'
#' Pushes changes to the CLI.
#'
#' @param localPath The local repository path.
#' @export
pushCli <- function(localPath) {
  checkInit()
  command <- glue::glue("push -repository {localPath}")
  executeCli(command)
}
