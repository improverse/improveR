#' Execute CLI Command
#'
#' Executes a CLI command using the appropriate CLI executable.
#'
#' @param cliString A string representing the CLI command to execute.
#' @noRd
executeCli <- function(cliString) {
  shellFile <- cliPath()
  result <- system(paste(shellFile, cliString), intern = TRUE)
  exitCode <- attr(result, "status")
  if (!is.null(exitCode) && exitCode != 0) {
    log_warn("CLI command failed (exit code ", exitCode, "): ",
             paste(tail(result, 3), collapse = "\n"))
  }
  invisible(result)
}


