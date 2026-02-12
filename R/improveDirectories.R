# Improve directory management functions
# CODING GUIDELINE: Always use forward slashes for paths on all platforms
# Use normalizePath(path, winslash = "/") to ensure consistent path separators

# Package-level cache for WSL mount prefix (NULL = not yet detected)
.wslMountPrefix <- new.env(parent = emptyenv())
.wslMountPrefix$value <- NULL

#' Convert Windows-style path to WSL absolute path
#'
#' @description Detects paths like "C:/..." on Linux/WSL and converts them
#' to absolute paths using the WSL mount prefix (/mnt/c/ or /c/).
#' The detected prefix is cached in a package variable.
#' @param path Character string with path to convert
#' @return Converted path (or original if not a Windows-style path or not on Linux)
#' @noRd
convertWslPath <- function(path) {
  if (.Platform$OS.type == "windows") return(path)
  if (!grepl("^[A-Za-z]:/", path)) return(path)

  drive_letter <- tolower(substr(path, 1, 1))
  rest_of_path <- substring(path, 3) # everything after "C:"

  if (is.null(.wslMountPrefix$value)) {
    # Detect WSL mount prefix: try /mnt/c/ first, then /c/
    if (dir.exists(paste0("/mnt/", drive_letter))) {
      .wslMountPrefix$value <- "/mnt/"
    } else if (dir.exists(paste0("/", drive_letter))) {
      .wslMountPrefix$value <- "/"
    } else {
      .wslMountPrefix$value <- ""
    }
  }

  if (.wslMountPrefix$value == "") return(path)

  converted <- paste0(.wslMountPrefix$value, drive_letter, rest_of_path)
  return(converted)
}

#' Get improve directory (internal implementation)
#'
#' @param env_var Environment variable name to check first
#' @param subdir Subdirectory name for defaults
#' @param windows_base Base directory for Windows (e.g., "LOCALAPPDATA", "USERPROFILE")
#' @param unix_hidden Whether to use hidden directory on Unix (prefix with .)
#' @return Character string with path to directory
#' @noRd
getImproveDir <- function(env_var, subdir, windows_base = "LOCALAPPDATA", unix_hidden = TRUE) {
  # Check if environment variable is set
  existing_dir <- Sys.getenv(env_var, "")

  if (existing_dir != "") {
    # On WSL, convert Windows-style paths (e.g. "C:/...") to absolute WSL paths
    existing_dir <- convertWslPath(existing_dir)

    # Environment variable is set - try to use it
    if (dir.exists(existing_dir)) {
      return(existing_dir)  # Already exists, use it
    } else {
      # Try to create the specified directory
      tryCatch({
        dir.create(existing_dir, recursive = TRUE, showWarnings = FALSE)
        if (dir.exists(existing_dir)) {
          log_info("Created specified directory:", existing_dir)
          return(existing_dir)
        } else {
          log_warn("Could not create specified", env_var, ":", existing_dir, "- using default")
        }
      }, error = function(e) {
        log_warn("Could not create specified", env_var, ":", existing_dir, "- using default")
      })
    }
  }

  # Determine platform-appropriate default directory
  if (.Platform$OS.type == "windows") {
    # Windows: Use specified base directory
    base_dir <- Sys.getenv(windows_base, "")
    if (base_dir == "" && windows_base == "LOCALAPPDATA") {
      # Fallback to APPDATA if LOCALAPPDATA not available
      base_dir <- Sys.getenv("APPDATA", "")
    }
    if (base_dir == "") {
      # Final fallback to USERPROFILE
      base_dir <- Sys.getenv("USERPROFILE", "")
      if (base_dir != "") {
        base_dir <- file.path(base_dir, "AppData", "Local")
      }
    }

    if (base_dir != "") {
      computed_dir <- file.path(base_dir, subdir)
    } else {
      # Tempdir fallback
      log_warn("Could not determine Windows user directory, using temporary directory")
      computed_dir <- file.path(tempdir(), subdir)
    }
  } else {
    # Unix/Linux/macOS: Use ~/subdir or ~/.subdir
    home_dir <- tryCatch({
      normalizePath("~", mustWork = FALSE)
    }, warning = function(w) {
      return("")
    }, error = function(e) {
      return("")
    })

    if (home_dir != "" && home_dir != "~") {
      dir_name <- if (unix_hidden) paste0(".", subdir) else subdir
      computed_dir <- file.path(home_dir, dir_name)
    } else {
      # Tempdir fallback
      log_warn("Could not determine home directory, using temporary directory")
      computed_dir <- file.path(tempdir(), subdir)
    }
  }

  # Create directory if it doesn't exist
  if (!dir.exists(computed_dir)) {
    dir.create(computed_dir, recursive = TRUE, showWarnings = FALSE)
    log_info("Created improve directory:", computed_dir)
  }

  # Set environment variable so next call uses same location
  do.call(Sys.setenv, stats::setNames(list(computed_dir), env_var))

  # Normalize path to use forward slashes on all platforms
  return(normalizePath(computed_dir, winslash = "/", mustWork = FALSE))
}

#' Get improve internal directory
#'
#' @description Gets the internal working directory for improve system files.
#' Provides sensible cross-platform defaults if internalFolder is not set.
#' @return Character string with path to internal directory
#' @export
getImproveInternalDir <- function() {
  getImproveDir("internalFolder", "improVerse", "LOCALAPPDATA", unix_hidden = TRUE)
}

#' Get improve workspace directory
#'
#' @description Gets the workspace directory for user files and projects.
#' @return Character string with path to workspace directory
#' @export
getImproveWorkspaceDir <- function() {
  getImproveDir("IMPROVER_WORKSPACE", "improVerse", "USERPROFILE", unix_hidden = FALSE)
}
