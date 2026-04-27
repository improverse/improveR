# ── CLI Detection ─────────────────────────────────────────────────────────────
#
# Auto-detects which CLI is available and its capabilities.
# Supports three modes:
#   - "jar"           : New CLI JAR via java -jar (from improveRcontributions)
#   - "legacy_binary" : Old platform-specific CLI binary (via IMPROVE_CLI_PATH)
#   - "none"          : No CLI found
#

cliInfoEnv <- new.env(parent = emptyenv())

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
    cliInfoEnv$hasPicocli <- FALSE
    cliInfoEnv$version <- NULL
    cliInfoEnv$detected <- TRUE
    log_info("CLI: legacy binary at ", envPath)
    return(invisible(NULL))
  }

  # Option 2: JAR from improveRcontributions
  if (requireNamespace("improveRcontributions", quietly = TRUE)) {
    jarPath <- tryCatch(improveRcontributions::cliJarPath(), error = function(e) NULL)
    javaCmd <- tryCatch(improveRcontributions::javaPath(), error = function(e) NULL)

    if (!is.null(jarPath) && !is.null(javaCmd)) {
      # Get version
      versionOut <- tryCatch(
        system2(javaCmd, c("-jar", jarPath, "version"), stdout = TRUE, stderr = TRUE),
        error = function(e) ""
      )
      version <- sub(".*?(\\d+\\.\\d+\\.\\d+).*", "\\1", paste(versionOut, collapse = " "))
      if (!grepl("^\\d+\\.\\d+\\.\\d+$", version)) version <- "unknown"

      # Check picocli support
      hasPico <- FALSE
      picoOut <- tryCatch(
        system2(javaCmd, c("-jar", jarPath, "--pico", "--help"), stdout = TRUE, stderr = TRUE),
        error = function(e) ""
      )
      picoExit <- attr(picoOut, "status")
      if (is.null(picoExit) || picoExit == 0) {
        hasPico <- any(grepl("imp", picoOut))
      }

      cliInfoEnv$mode <- "jar"
      cliInfoEnv$jarPath <- jarPath
      cliInfoEnv$javaCmd <- javaCmd
      cliInfoEnv$version <- version
      cliInfoEnv$hasPicocli <- hasPico
      cliInfoEnv$detected <- TRUE
      log_info("CLI: JAR ", jarPath, " v", version, " picocli=", hasPico)
      return(invisible(NULL))
    }
  }

  # Option 3: Legacy binary via cliPath() (old improveRcontributions with platform archives)
  if (requireNamespace("improveRcontributions", quietly = TRUE)) {
    legacyPath <- tryCatch(improveRcontributions::cliPath(), error = function(e) NULL)
    if (!is.null(legacyPath) && file.exists(legacyPath)) {
      cliInfoEnv$mode <- "legacy_binary"
      cliInfoEnv$binaryPath <- legacyPath
      cliInfoEnv$hasPicocli <- FALSE
      cliInfoEnv$version <- NULL
      cliInfoEnv$detected <- TRUE
      log_info("CLI: legacy binary at ", legacyPath)
      return(invisible(NULL))
    }
  }

  cliInfoEnv$detected <- TRUE
  cliInfoEnv$mode <- "none"
  cliInfoEnv$hasPicocli <- FALSE
  log_warn("No CLI found. Install improveRcontributions or set IMPROVE_CLI_PATH.")
}

#' Check if Picocli Surface is Available
#' @return Logical
#' @noRd
hasPicocli <- function() {
  detectCli()
  cliInfoEnv$hasPicocli
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
