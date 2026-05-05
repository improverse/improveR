# ── New CLI Commands (picocli surface) ────────────────────────────────────────
#
# These commands are only available with the new CLI (4.4.2+, picocli surface).
# They require hasPicocli() == TRUE.
#

#' Require Picocli
#' @noRd
requirePicocli <- function(cmd) {
  if (!hasPicocli()) {
    stop(
      "'", cmd, "' requires the new CLI (4.4.2+). ",
      "Update improveRcontributions or set IMPROVE_CLI_PATH to a 4.4.2+ binary.",
      call. = FALSE
    )
  }
}

#' Build picocli args with --pico prefix
#' @noRd
picoArgs <- function(...) {
  c("--pico", ...)
}

#' Show Repository Status
#'
#' Shows local/remote changes and conflicts for a local repository.
#' Returns structured data when \code{json = TRUE}.
#'
#' @param localPath The local repository path.
#' @param json Logical. If \code{TRUE}, returns parsed JSON output. Default \code{TRUE}.
#' @return Parsed status data (if \code{json = TRUE}) or character vector of output lines.
#' @export
statusCli <- function(localPath, json = TRUE) {
  requirePicocli("status")
  renewAccessToken()
  args <- picoArgs("status",
            "--access-token", conf()$reqToken,
            "-C", localPath)
  executeCli(args, json = json)
}

#' Merge Conflict Files
#'
#' Attempts to merge specific conflict files in the local repository.
#'
#' @param localPath The local repository path.
#' @param files Character vector. File paths (relative to repo root) to merge.
#' @param mergeTool Character. External merge tool command. If \code{NULL}, uses
#'   the built-in three-way merge.
#' @param json Logical. If \code{TRUE}, returns parsed JSON output. Default \code{FALSE}.
#' @return CLI output (invisibly).
#' @export
mergeCli <- function(localPath, files, mergeTool = NULL, json = FALSE) {
  requirePicocli("merge")
  renewAccessToken()
  args <- picoArgs("merge",
            "--access-token", conf()$reqToken,
            "-C", localPath)
  if (!is.null(mergeTool)) args <- c(args, "--mergetool", mergeTool)
  args <- c(args, files)
  executeCli(args, json = json)
}

#' Reset Conflict Files
#'
#' Resets conflict files back to the baseline (last pull) state.
#'
#' @param localPath The local repository path.
#' @param files Character vector. File paths to reset. Ignored if \code{all = TRUE}.
#' @param all Logical. If \code{TRUE}, reset all conflict files.
#' @param force Logical. If \code{TRUE}, reset the whole workspace to baseline state.
#'   Requires \code{confirm = TRUE}.
#' @param confirm Logical. If \code{TRUE}, skip confirmation prompt (required for \code{force}).
#' @param revision Character. Revision ID to reset to (optional).
#' @param json Logical. If \code{TRUE}, returns parsed JSON output. Default \code{FALSE}.
#' @return CLI output (invisibly).
#' @export
resetCli <- function(localPath, files = NULL, all = FALSE, force = FALSE,
                     confirm = FALSE, revision = NULL, json = FALSE) {
  requirePicocli("reset")
  renewAccessToken()
  args <- picoArgs("reset",
            "--access-token", conf()$reqToken,
            "-C", localPath)
  if (all) args <- c(args, "--all")
  if (force) args <- c(args, "--force")
  if (confirm) args <- c(args, "--yes")
  if (!is.null(revision)) args <- c(args, "--revision", revision)
  if (!is.null(files) && !all) args <- c(args, files)
  executeCli(args, json = json)
}

#' Resolve Conflict Files
#'
#' Marks conflict files as resolved after manual edits.
#'
#' @param localPath The local repository path.
#' @param files Character vector. File paths (relative to repo root) to mark as resolved.
#' @param json Logical. If \code{TRUE}, returns parsed JSON output. Default \code{FALSE}.
#' @return CLI output (invisibly).
#' @export
resolveCli <- function(localPath, files, json = FALSE) {
  requirePicocli("resolve")
  renewAccessToken()
  args <- picoArgs("resolve",
            "--access-token", conf()$reqToken,
            "-C", localPath,
            files)
  executeCli(args, json = json)
}

#' Clean Generated Output
#'
#' Deletes generated tool output in the local workspace using the
#' \code{toolDeletePatterns} stored when the repository was initialized.
#'
#' @param localPath The local repository path.
#' @param preview Logical. If \code{TRUE}, only preview what would be deleted.
#' @return CLI output (invisibly).
#' @export
cleanCli <- function(localPath, preview = FALSE) {
  requirePicocli("clean")
  args <- picoArgs("clean", "-C", localPath)
  if (preview) args <- c(args, "--preview")
  executeCli(args)
}

#' Add Input File Patterns
#'
#' Adds include file patterns to the local repository input configuration.
#' These patterns control which files are tracked and pushed.
#'
#' @param localPath The local repository path.
#' @param includeFiles Character. Comma-separated glob patterns to include
#'   (e.g. \code{"*.R,*.csv,data/"}).
#' @param clean Logical. If \code{TRUE}, clear existing patterns before adding new ones.
#' @return CLI output (invisibly).
#' @export
addInputsCli <- function(localPath, includeFiles, clean = FALSE) {
  requirePicocli("add-inputs")
  args <- picoArgs("add-inputs",
            "-C", localPath,
            "--include-files", includeFiles)
  if (clean) args <- c(args, "--clean")
  executeCli(args)
}

#' Create Step on Server
#'
#' Creates a root or child step in an analysis tree on the improve server.
#' Use \code{analysisTree} for a root step, \code{parentStep} for a child,
#' or both to create a child in a different analysis tree.
#'
#' @param toolCategory Character. Tool category as defined in improve (see \code{toolsCli()}).
#' @param tool Character. Tool name registered in improve.
#' @param analysisTree Character. Resource/entity ID of the analysis tree (for root steps).
#' @param parentStep Character. Resource/entity ID of the parent step (for child steps).
#' @param comment Character. Optional check-in comment.
#' @return CLI output (invisibly).
#' @export
createStepCli <- function(toolCategory, tool, analysisTree = NULL,
                          parentStep = NULL, comment = NULL) {
  requirePicocli("create-step")
  renewAccessToken()
  args <- picoArgs("create-step",
            "--access-token", conf()$reqToken,
            "--tool-category", toolCategory,
            "--tool", tool,
            "--profile", cliProfileName())
  if (!is.null(analysisTree)) args <- c(args, "--analysis-tree", analysisTree)
  if (!is.null(parentStep)) args <- c(args, "--parent-step", parentStep)
  if (!is.null(comment)) args <- c(args, "-m", comment)
  executeCli(args)
}

#' List Available Tools
#'
#' Lists tool categories and tools available in improve. Useful for
#' determining the \code{toolCategory} and \code{tool} arguments for
#' \code{\link{createStepCli}}.
#'
#' @return CLI output with tool listing.
#' @export
toolsCli <- function() {
  requirePicocli("tools")
  renewAccessToken()
  args <- picoArgs("tools",
            "--access-token", conf()$reqToken,
            "--profile", cliProfileName())
  executeCli(args)
}

#' Repository Info
#'
#' Displays information about a local repository and its server-side root
#' resource (profile, server URL, user, path, IDs).
#'
#' @param localPath The local repository path.
#' @return CLI output with repository info.
#' @export
infoCli <- function(localPath) {
  requirePicocli("info")
  renewAccessToken()
  args <- picoArgs("info",
            "--access-token", conf()$reqToken,
            "-C", localPath)
  executeCli(args)
}
