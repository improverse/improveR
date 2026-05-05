# ── CLI Diff Command ──────────────────────────────────────────────────────────

#' Show File Content at a Specific Revision or Unified Diff
#'
#' With \code{base = TRUE}: streams the baseline (last-pulled) revision bytes
#' for \code{path} to stdout.
#' With \code{theirs = TRUE}: streams the latest server revision bytes for
#' \code{path} to stdout.
#' Without either flag: emits a unified diff (local vs baseline) to stdout.
#'
#' Requires CLI version 4.5.0 or later. Stops with an informative error if
#' the installed CLI does not support the \code{diff} command.
#'
#' @param localPath The local repository path.
#' @param path Character. File path relative to the repository root.
#' @param base Logical. If \code{TRUE}, stream baseline revision content.
#' @param theirs Logical. If \code{TRUE}, stream latest server revision content.
#' @return CLI output (character vector of lines), returned invisibly.
#' @export
diffCli <- function(localPath, path, base = FALSE, theirs = FALSE) {
  requirePicocli("diff")
  ver <- cliDetectedVersion()
  if (!is.null(ver) && compareVersion(ver, "4.5.0") < 0) {
    stop(
      "'diff' requires CLI version 4.5.0 or later (detected: ", ver, "). ",
      "Update the improve-cli JAR in improveRcontributions.",
      call. = FALSE
    )
  }
  renewAccessToken()
  args <- picoArgs("diff",
            "--access-token", conf()$reqToken,
            "-C", localPath)
  if (base) args <- c(args, "--base")
  if (theirs) args <- c(args, "--theirs")
  args <- c(args, path)
  executeCli(args)
}
