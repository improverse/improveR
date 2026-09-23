# ── CLI Detection ─────────────────────────────────────────────────────────────
#
# Auto-detects which CLI is available and its capabilities.
# Supports three modes:
#   - "jar"           : New CLI JAR via java -jar (from improveRcontributions)
#   - "legacy_binary" : Old platform-specific CLI binary (via IMPROVE_CLI_PATH)
#   - "none"          : No CLI found
#

cliInfoEnv <- new.env(parent = emptyenv())

# Asks a CLI executable for its version. Used for the legacy binary, whose
# version cannot be read off a file name. Returns "unknown" rather than failing:
# the version is documentation for the evidence package, not a precondition.
versionFromCli <- function(command) {
  out <- tryCatch(
    suppressWarnings(system2(command, "version", stdout = TRUE, stderr = TRUE)),
    error = function(e) character(0)
  )
  m <- regmatches(paste(out, collapse = " "),
                  regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", paste(out, collapse = " ")))
  if (length(m) == 1L) m else "unknown"
}

# Reads the version out of a JAR file name, e.g. improve-cli-4.4.5.jar -> "4.4.5".
# Returns "unknown" when the name carries none.
versionFromJarName <- function(jarPath) {
  m <- regmatches(basename(jarPath), regexpr("[0-9]+\\.[0-9]+\\.[0-9]+", basename(jarPath)))
  if (length(m) == 1L) m else "unknown"
}

#' Detect CLI Version and Capabilities
#'
#' Called automatically on first CLI use. Checks for the CLI JAR in
#' \code{improveRcontributions} or a legacy binary via \code{IMPROVE_CLI_PATH}.
#' Results are cached for the session.
#'
#' @noRd
detectCli <- function() {
  if (!is.null(cliInfoEnv$detected)) return(invisible(NULL))

  # Option 1: IMPROVE_CLI_PATH (old CLI with its own bundled JRE)
  envPath <- Sys.getenv("IMPROVE_CLI_PATH", "")
  if (envPath != "" && file.exists(envPath)) {
    cliInfoEnv$mode <- "legacy_binary"
    cliInfoEnv$binaryPath <- envPath
    cliInfoEnv$version <- versionFromCli(envPath)
    cliInfoEnv$detected <- TRUE
    log_info("CLI: legacy binary at ", envPath, " v", cliInfoEnv$version)
    return(invisible(NULL))
  }

  # Option 2: whatever improveRcontributions::cliPath() offers.
  #
  # There used to be an option before this one that called
  # improveRcontributions::cliJarPath() and ::javaPath() directly. Neither
  # function exists in that package - not unexported, absent - so both calls
  # raised, both tryCatch handlers returned NULL, and the branch could never be
  # taken. It failed silently, because tryCatch(..., error = function(e) NULL)
  # makes "this function does not exist" indistinguishable from "there is no JAR
  # installed". Removed in IMR-302; cliPath() reaches the same JAR and is the
  # supported way in.
  #
  # cliPath() does not return a path. It returns a command line, e.g.
  #   "/usr/bin/java -jar /.../improve-cli-4.5.0.jar"
  # The previous version passed that straight to file.exists(), which is always
  # FALSE - so this branch never fired and the whole file was unreachable
  # (IMR-272). Both shapes are accepted now: a plain executable, and a command
  # line with a JAR in it.
  if (requireNamespace("improveRcontributions", quietly = TRUE)) {
    cliCmd <- tryCatch(improveRcontributions::cliPath(), error = function(e) NULL)
    if (!is.null(cliCmd) && nzchar(cliCmd)) {
      jarInCmd <- regmatches(cliCmd, regexpr("[^ ]+\\.jar", cliCmd))
      if (length(jarInCmd) == 1L && file.exists(jarInCmd)) {
        # A JAR invoked through some java command - same shape as option 2.
        cliInfoEnv$mode <- "jar"
        cliInfoEnv$jarPath <- jarInCmd
        cliInfoEnv$javaCmd <- sub(" +-jar.*$", "", cliCmd)
        cliInfoEnv$version <- versionFromJarName(jarInCmd)
        cliInfoEnv$detected <- TRUE
        log_info("CLI: JAR ", jarInCmd, " v", cliInfoEnv$version, " (via improveRcontributions)")
        return(invisible(NULL))
      }
      if (file.exists(cliCmd)) {
        cliInfoEnv$mode <- "legacy_binary"
        cliInfoEnv$binaryPath <- cliCmd
        cliInfoEnv$version <- versionFromCli(cliCmd)
        cliInfoEnv$detected <- TRUE
        log_info("CLI: legacy binary at ", cliCmd, " v", cliInfoEnv$version)
        return(invisible(NULL))
      }
      log_warn("improveRcontributions::cliPath() returned something that is neither an ",
               "existing file nor a command with an existing JAR: ", cliCmd)
    }
  }

  cliInfoEnv$detected <- TRUE
  cliInfoEnv$mode <- "none"
  log_warn("No CLI found. Install improveRcontributions or set IMPROVE_CLI_PATH.")
}

#' Get CLI Mode
#' @return Character: "jar", "legacy_binary", or "none"
#' @noRd
cliMode <- function() {
  detectCli()
  cliInfoEnv$mode
}

#' Get Detected CLI Version
#' @return Character version string, or NULL
#' @noRd
cliDetectedVersion <- function() {
  detectCli()
  cliInfoEnv$version
}

#' Reset CLI Detection Cache
#'
#' Forces re-detection on next CLI call. Useful after changing
#' \code{IMPROVE_CLI_PATH} or installing a new \code{improveRcontributions}.
#' @noRd
resetCliDetection <- function() {
  rm(list = ls(envir = cliInfoEnv), envir = cliInfoEnv)
}
