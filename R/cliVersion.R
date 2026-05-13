#' Version CLI
#'
#' Displays the version of the CLI.
#' @return The version string (invisibly).
#' @export
versionCli <- function() {
  executeCli(c("version"))
}
