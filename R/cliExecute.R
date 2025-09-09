#' Execute CLI Command
#'
#' Executes a CLI command using the appropriate CLI executable.
#'
#' @param cliString A string representing the CLI command to execute.
#' @noRd
executeCli <- function(cliString) {
  shellFile <- cliPath()
  result <- system(paste(shellFile, cliString),intern = T)
}


