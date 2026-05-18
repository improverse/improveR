cacheEnv <- new.env(parent = emptyenv())


#' improveConnected
#'
#' @description improveConnected checks if improveConnect was called.
#' @param silent if TRUE no log message is printed
#' @seealso [improveConnect()], [improveDisconnect()]
#' @export

improveConnected <- function(silent = FALSE) {
  if (is.null(cacheEnv$initialized) || cacheEnv$initialized == FALSE) {
    if (!silent) {
      log_error("No connection detected. improveConnect was not called or an error occurred during connection.")
      stop("not connected")
    }
    return(invisible(FALSE))
  } else if (cacheEnv$initialized == TRUE) {
    if (!silent) {
      log_debug("Connection already established.")
      log_debug(cacheEnv$conf$repoUrl)
    }
    return(invisible(TRUE))
  } else {
    # Handle any other state if needed, for example:
    if (!silent) {
      log_warn("Connection status is not recognized.")
    }
    return(invisible(FALSE))
  }
}



#' improveDisconnect
#'
#' @description improveDisconnect removes all connection information.
#' @param env default is 'cacheEnv'
#' @seealso [improveConnect()], [improveConnected()]
#' @export
improveDisconnect <- function(env = cacheEnv) {
  authenticationProvider <- env$authenticationProvider
  editable <- env$editable
  rm(list = ls(envir = env), envir = env)
  env$authenticationProvider <- authenticationProvider
  env$editable <- editable
}

#' Clear Connection Data
#'
#' @description deletes repoUrl, stepId, and token from the environment variables and calls improveDisconnect
#' @param includeRepoData includes the repository URL and the selected step in the clear process
#' @seealso [improveDisconnect()]
#' @export
clearConnectionData <- function(includeRepoData=F) {
  if (includeRepoData) {
    Sys.setenv(IMPROVER_STEP="")
    Sys.setenv(IMPROVER_REPO_URL="")
    # IMPROVER_REFRESH_TOKEN is a long-lived OAuth credential. Wipe it only
    # when the caller is doing a full identity reset (logout, switching
    # repos, etc.). Soft-clear paths - runner between-file reconnects,
    # improveRtestsupport's connectAs handoff, intra-session token refresh
    # recovery - must preserve it so the next improveConnect can mint a
    # fresh access token without spawning a device-code flow that requires
    # human interaction (which is impossible in a non-interactive suite).
    Sys.setenv(IMPROVER_REFRESH_TOKEN="")
  }
  Sys.setenv(IMPROVER_USER="")
  Sys.setenv(IMPROVER_PASSWORD="")
  Sys.setenv(IMPROVER_TOKEN="")
  # Reset CLI user profile so configureUserProfile() re-evaluates on next connect
  cliEnv$userProfile <- NULL
  improveDisconnect()
}

#' improveConnect
#'
#' @description
#' improveConnect establishes a connection to the improve repository using one of two authentication methods:
#'
#' ## Authentication Methods:
#' 1. **Run Tokens (Production):** When a step is executed from improve platform, a run token is automatically provided
#' 2. **OAuth (Interactive):** User authentication with browser or headless mode for development/interactive use
#'
#' ## Required Environment Variables:
#'
#' **For Run Token Authentication:**
#' * IMPROVER_TOKEN: the run token (mandatory)
#' * IMPROVER_REPO_URL: repository URL (mandatory)
#' * IMPROVER_STEP: step ID for relative path resolution (optional)
#' * IMPROVER_WORKSPACE: workspace directory (optional, defaults to current directory)
#'
#' **For OAuth Authentication:**
#' * IMPROVER_REPO_URL: repository URL (mandatory)
#' * IMPROVER_STEP: step ID for relative path resolution (mandatory)
#' * IMPROVER_HEADLESS_OAUTH: set to any non-empty value to enable headless OAuth mode (optional)
#' * IMPROVER_WORKSPACE: workspace directory (optional, defaults to current directory)
#'
#' ## Optional Environment Variables (both methods):
#' * IMPROVER_SECURITY: set to "insecure" to disable certificate verification
#'
#' ## Connection information sources (in descending priority):
#' 1. via the command line
#' 1. via environment variables
#' 1. via a conf.json file
#'
#' ## Command line
#' Command line arguments have to be in the correct order.
#' 1. run token (reqToken)
#' 1. step ID (shortEntityId) for relative path resolution
#' 1. the repository URL in the format https://repoaddress:repoPort/repository
#' 1. run workspace path
#'
#' ## Conf.json file
#' The file has to be located in the working directory and contain the following details:
#' * reqToken: run token (if not provided, OAuth authentication will be used)
#' * stepId: step ID for relative path resolution (mandatory for OAuth)
#' * repoUrl: repository URL (mandatory)
#' * runWorkspace: workspace directory
#'
#' @param logLevel Log verbosity level. Possible values: DEBUG, INFO, WARN, ERROR.
#' Default is "INFO". Can be overridden by environment variable IMPROVE_LOG_LEVEL.
#' @param secure If TRUE (default), SSL certificates are validated. If FALSE, certificate
#' validation is disabled - this allows connections with expired or self-signed certificates
#' but is NOT recommended for production use. Can be overridden by setting environment
#' variable IMPROVER_SECURITY="insecure".
#' @param offlinePossible If TRUE, the setup continues even if no connection is possible.
#' Default is FALSE.
#' @param persistentCaching If TRUE, caches are persisted to and reloaded from `.improver.cache`
#' file. Default is FALSE.
#'
#' @returns Invisibly returns `NULL`. Called for its side effects:
#'   \itemize{
#'     \item Establishes authenticated connection to the improve repository
#'     \item Sets internal configuration
#'     \item Initializes logging with specified verbosity level
#'     \item Optionally loads persistent cache from `.improver.cache`
#'   }
#'   After successful connection, use \code{\link{pwd}} to verify the current
#'   working step and \code{\link{whoami}} to confirm the authenticated user.
#'
#' @examples
#' \dontrun{
#' # Basic connection using environment variables
#' # (Set IMPROVER_REPO_URL and IMPROVER_STEP in .Renviron)
#' improveConnect()
#'
#' # Verify connection
#' whoami()
#' pwd()
#'
#' # Connection with debug logging for troubleshooting
#' improveConnect(logLevel = "DEBUG")
#'
#' # Development connection with self-signed certificates
#' improveConnect(secure = FALSE)
#'
#' # Enable write operations after connecting
#' improveConnect()
#' setEditable(TRUE)
#' }
#'
#' @export
#' @seealso
#' \code{\link{improveConnected}} to check if connected,
#' \code{\link{improveDisconnect}} to close connection,
#' \code{\link{setEditable}} to enable write operations,
#' \code{\link{whoami}} to get current user,
#' \code{\link{pwd}} to get current step
improveConnect <- function(logLevel = "INFO", secure = TRUE, offlinePossible = FALSE, persistentCaching = FALSE) {

  # Handle security environment variable override
  secureFlag <- Sys.getenv("IMPROVER_SECURITY")
  if (!is.null(secureFlag) && secureFlag == "insecure") {
    secure <- F
  }

  # Check for log level environment variable override
  # Only use env var if no explicit parameter was passed
  if (missing(logLevel)) {
    envLogLevel <- Sys.getenv("IMPROVE_LOG_LEVEL", "")
    if (envLogLevel != "") {
      logLevel <- envLogLevel
    }
  }

  cacheEnv$initialized <- TRUE
  cacheEnv$logLevel <- logLevel
  cacheEnv$secure <- secure
  cacheEnv$offlinePossible <- offlinePossible
  cacheEnv$offline <- FALSE
  cacheEnv$persistentCaching <- persistentCaching
  cacheEnv$reproducible <- FALSE


  if (!secure) {
    httr::set_config(httr::config(ssl_verifypeer = 0L))
    httr::set_config(httr::config(ssl_verifyhost = 0L))
  }

  initImproveLogging(logLevel)

  # persistentCaching
  if (persistentCaching) {
    if (file.exists(".improver.cache")) {
      # TODO check if loads into correct context
      loadedEnv <- readRDS(".improver.cache")
      vals <- ls(envir = loadedEnv)
      devnull <- lapply(vals, function(val) {
        copyEnv <- get(val, envir = loadedEnv)
        assign(val, copyEnv, envir = cacheEnv)
      })
      log_info("Loaded cached data from file .improver.cache")
    }
    registerCloseFunction("9saveCache", saveCache)
    cacheEnv$reproducible <- T
  }

  # Try to apply tokens from any active refresher
  tryCatch({
    refresher <- getActiveTokenRefresher()
    if (!is.null(refresher) && refresher$isRunning()) {
      tokenData <- refresher$getToken()
      if (!is.null(tokenData)) {
        log_debug("Applied tokens from active refresher")
      }
    }
  }, error = function(e) {
    # Ignore if no refresher available
  })




  noCommandArgs <- length(commandArgs(trailingOnly = TRUE))
  if (noCommandArgs >= 4) {
    reqToken <- commandArgs(trailingOnly = TRUE)[[1]]
    stepId <- commandArgs(trailingOnly = TRUE)[[2]]
    repoUrl <- commandArgs(trailingOnly = TRUE)[[3]]
    serverAddress <- paste0(repoUrl, "/api/v1/")
    runWorkspace <- paste0(commandArgs(trailingOnly = TRUE)[[4]], "/")

    confData <- data.frame(reqToken = reqToken, stepId = stepId, repoUrl = serverAddress, runWorkspace = runWorkspace, stringsAsFactors = F)
    assign("conf", confData, cacheEnv)
    log_info("Access Data parsed from Command Line:")
    if (noCommandArgs > 4 && commandArgs(trailingOnly = TRUE)[[5]] == "write") {
      log_info("(also written to conf.json)")
      confJson <- jsonlite::toJSON(confData)
      output <- file("conf.json", "wb")
      write(confJson, output)
      close(output)
    }
  } else if (Sys.getenv("IMPROVER_REPO_URL") != "") {
    reqToken <- Sys.getenv("IMPROVER_TOKEN")
    stepId <- Sys.getenv("IMPROVER_STEP")
    repoUrl <- Sys.getenv("IMPROVER_REPO_URL")

    # If no run token provided, use OAuth authentication
    if (reqToken=="") {
      improveOAuth(repoUrl, shortEntityId = stepId, secure = secure)
      return()
    }
    workspace <- Sys.getenv("IMPROVER_WORKSPACE")
    if (workspace == "") {
      workspace <- getwd()
    }
    serverAddress <- paste0(repoUrl, "/api/v1/")
    runWorkspace <- paste0(workspace, "/")
    confData <- data.frame(repoUrl = serverAddress, runWorkspace = runWorkspace, user = "", reqToken = reqToken, stepId = stepId, stringsAsFactors = F)
    assign("conf", confData, cacheEnv)
    log_info("Access Data from Environment variables:")
  } else {
    tryCatch(
      {
        conf <- as.data.frame(jsonlite::read_json("conf.json"), stringsAsFactors = FALSE)
        # TODO this is a fix for docker mapping
        conf$runWorkspace <- paste0(getwd(), "/")
        log_info("Access Data read from conf.json:")
        assign("conf", conf, cacheEnv)
      },
      error = function(cond) {
        if (!offlinePossible) {
          log_error(cond)
          log_error("Neither commandline arguments, environment variables nor conf.json supplied.")
          log_error("Expected commandline arguments:")
          log_error("<command-file>")
          log_error("<jwt>")
          log_error("<step-entityId>")
          log_error("<repoUrl>")
          log_error("<runWorkspace>")
          log_error("Expected environment variables:")
          log_error("For Run Token Authentication:")
          log_error("  IMPROVER_TOKEN (mandatory)")
          log_error("  IMPROVER_REPO_URL (mandatory)")
          log_error("  IMPROVER_STEP (optional)")
          log_error("For OAuth Authentication:")
          log_error("  IMPROVER_REPO_URL (mandatory)")
          log_error("  IMPROVER_STEP (mandatory)")
          log_error("  IMPROVER_HEADLESS_OAUTH (optional)")
          log_error("  IMPROVER_WORKSPACE (optional)")
          stop("Not configured correctly")
        }
        cacheEnv$offline <- T
      }
    )
  }

  if (!is.na(conf()$reqToken) && !is.null(conf()$reqToken) && conf()$reqToken != "") {
    log_info("Using run token authentication")
  } else {
    log_info("OAuth authentication completed")
  }
  log_info(paste0("StepId: ", conf()$stepId))

  # Validate the connection by making a lightweight API call.
  # This catches stale tokens early - without this check, improveConnect()
  # silently "succeeds" with expired tokens and the user gets no feedback.
  if (!cacheEnv$offline) {
    connectionValid <- tryCatch({
      testResult <- authenticatedREST("/resources", restType = "GET")
      !is.null(testResult)
    }, error = function(e) FALSE)

    if (!connectionValid) {
      isOAuth <- is.null(conf()$reqToken) || is.na(conf()$reqToken) || conf()$reqToken == ""
      if (isOAuth) {
        log_warn("Connection validation failed - token may be stale. Re-authenticating via OAuth...")
        repoUrl <- Sys.getenv("IMPROVER_REPO_URL")
        stepId <- Sys.getenv("IMPROVER_STEP")
        clearConnectionData()
        cacheEnv$initialized <- TRUE
        cacheEnv$logLevel <- logLevel
        cacheEnv$secure <- secure
        improveOAuth(repoUrl, shortEntityId = stepId, secure = secure)
        return()
      } else {
        if (offlinePossible) {
          log_warn("Connection validation failed - continuing in offline mode")
          cacheEnv$offline <- TRUE
        } else {
          log_error("Connection validation failed - run token appears to be invalid.")
          log_error("The provided IMPROVER_TOKEN may have expired or the server is unreachable.")
          cacheEnv$initialized <- FALSE
          stop("Connection validation failed: run token is invalid or server is unreachable")
        }
      }
    }
  }

  if (!is.null(conf()$stepId) & !is.na(conf()$stepId) & conf()$stepId != "") {
    rootStep <- loadResource(conf()$stepId)
    assign(x = "pwd", value = rootStep, envir = cacheEnv)
  }

  # Discover repoPrefix when no stepId is provided.
  # Query /resources to get any resource and extract the prefix from its entityId.
  if (repoPrefix() == "" && !cacheEnv$offline) {
    tryCatch({
      probeResult <- authenticatedREST("/resources", restType = "GET")
      if (!is.null(probeResult)) {
        probeContent <- httr::content(probeResult)
        items <- if (is.list(probeContent) && length(probeContent) > 0) probeContent else NULL
        if (!is.null(items)) {
          firstEntityId <- if (is.list(items[[1]])) items[[1]]$entityId else NULL
          if (!is.null(firstEntityId) && grepl(":", firstEntityId, fixed = TRUE)) {
            prefix <- substr(firstEntityId, 1, regexpr(":", firstEntityId, fixed = TRUE))
            cacheEnv$discoveredRepoPrefix <- prefix
            log_info(paste0("Discovered repoPrefix: ", prefix))
          }
        }
      }
    }, error = function(e) {
      log_warn(paste0("Could not discover repoPrefix: ", e$message))
    })
  }

  log_info(paste0("repoUrl: ", conf()$repoUrl))
  log_info(paste0("runWorkspace: ", conf()$runWorkspace))
  setRootPath(getwd())
  registerCloseFunction("1removeImproveJson", improveClose)

  # Check repository version after configuration is complete
  if (!cacheEnv$offline) {
    repoBaseUrl <- gsub("/api/v1/$", "/", conf()$repoUrl)
    repoVersionUrl <- paste0(repoBaseUrl, "repository?status")
    tryCatch({
      versionResult <- unauthenticatedREST(repoVersionUrl, restType = "GET")
      if (!is.null(versionResult) && versionResult$status_code == 200) {
        # Parse HTML content
        versionContent <- httr::content(versionResult, "text", encoding = "UTF-8")
        # Extract version from HTML - looking for "Version: X.X.X-X (hash)"
        versionMatch <- regmatches(versionContent, regexpr("Version:\\s*([0-9\\.\\-]+)\\s*\\([a-f0-9]+\\)", versionContent))
        if (length(versionMatch) > 0) {
          # Extract just the version number
          version <- gsub("Version:\\s*([0-9\\.\\-]+).*", "\\1", versionMatch[1])
          log_info(paste0("Connected to repository version: ", version))
          cacheEnv$repositoryVersion <- version
        } else {
          log_warn("Could not parse repository version from response")
        }
      } else {
        log_warn("Could not retrieve repository version information")
      }
    }, error = function(e) {
      log_warn(paste0("Failed to check repository version: ", e$message))
    })
  }
}

#' checkConnect
#'
#' @description Checks if the current connection is still valid by attempting to load the IMPROVER_STEP resource.
#' If the connection is invalid, it clears connection data and attempts to reconnect.
#' @param secure Logical. If TRUE (default), validates the connection by attempting to load IMPROVER_STEP and reconnects on failure. If FALSE, returns FALSE without attempting recovery.
#' @return invisible TRUE if connection is valid, otherwise attempts reconnection
#' @export
checkConnect <- function(secure = TRUE) {
  # First check if we're connected at all
  if (!improveConnected(silent = TRUE)) {
    log_info("Not connected. Attempting to connect...")
    improveConnect(secure = secure)
    return(invisible(TRUE))
  }

  # Try to load the step resource to verify connection is still valid
  stepId <- conf()$stepId
  if (!is.null(stepId) && !is.na(stepId) && stepId != "") {
    stepResource <- refreshResource(stepId)
    if (!is.null(stepResource)) {
      log_debug("Connection verified - step resource loaded successfully")
      return(invisible(TRUE))
    } else {
      log_warn("Step resource returned NULL - reconnecting")
    }
  } else {
    log_debug("No step ID configured - checking basic connectivity")
    result <- authenticatedREST("/users")
    if (!is.null(result)) {
      log_debug("Connection verified via users endpoint")
      return(invisible(TRUE))
    }
  }

  # If we get here, connection is invalid - clear and reconnect
  log_info("Connection invalid - clearing connection data and reconnecting")
  clearConnectionData()
  improveConnect(secure = secure)
  return(invisible(TRUE))
}

saveCache <- function() {
  c <- conf()
  cacheEnv$conf <- ""
  saveRDS(cacheEnv, ".improver.cache")
  cacheEnv$conf <- c
}

conf <- function() {
  return(cacheEnv$conf)
}

setUser <- function(userName) {
  cacheEnv$conf$user <- userName
}

lastIndexOf <- function(haystack, needle) {
  posList <- unlist(gregexpr(needle, haystack))
  return(posList[length(posList)])
}

#' Get the Repository Entity ID Prefix
#'
#' Returns the prefix used for entity IDs on the connected repository
#' (e.g. \code{"hc4310:"}). When no step ID is available, the prefix is
#' auto-discovered during \code{\link{improveConnect}}.
#'
#' @returns Character string with the prefix (including trailing colon),
#'   or \code{""} if not available.
#' @export
repoPrefix <- function() {
  discovered <- get0("discoveredRepoPrefix", envir = cacheEnv)
  if (!is.null(discovered) && discovered != "") {
    return(discovered)
  }
  if (is.null(conf()$stepId) | lastIndexOf(conf()$stepId, ":") == -1) {
    return("")
  }
  substr(conf()$stepId, 0, lastIndexOf(conf()$stepId, ":"))
}

setRootPath <- function(path) {
  assign("ROOT_PATH", normalizePath(path, winslash = "/"), envir = cacheEnv)
}

getRootPath <- function() {
  return(get0("ROOT_PATH", envir = cacheEnv))
}

#' improveClose
#' @description Cleans up everything for checkin.
#' @export
improveClose <- function() {
  cleaned <- NULL
  if (!is.null(cacheEnv$createdLinks)) {
    if (length(cacheEnv$createdLinks) > 0) {
      for (i in 1:nrow(cacheEnv$createdLinks)) {
        linkEntry <- cacheEnv$createdLinks[i, ]
        if (file.exists(linkEntry$localPath)) {
          cleaned <- plyr::rbind.fill(cleaned, linkEntry)
        }
      }
      cacheEnv$createdLinks <- cleaned
      if (!is.null(cacheEnv$createdLinks)) {
        for (i in 1:nrow(cacheEnv$createdLinks)) {
          linkEntry <- cacheEnv$createdLinks[i, ]
          if (file.exists(linkEntry$localPath)) {
            file.remove(linkEntry$localPath)
          }
        }
      }
      saveImproveJson()
    }
  }
}

#' Get Log File
#'
#' @description Returns the path to the current log file, or NULL if logging to file is not enabled.
#' @return Character string with the log file path, or NULL if no log file is configured
#' @export
#' @examples
#' # Get current log file path
#' log_file <- getLogFile()
#' if (!is.null(log_file)) {
#'   cat("Logs are being written to:", log_file, "\n")
#' }
getLogFile <- function() {
  logFile <- Sys.getenv("improver.logfile", "")
  if (logFile == "") {
    return(NULL)
  }
  return(logFile)
}

#' Get Repository Version
#'
#' @description Returns the version of the connected repository, if available.
#' @return Character string with the repository version, or NULL if not available
#' @export
getRepositoryVersion <- function() {
  improveConnected()
  return(get0("repositoryVersion", envir = cacheEnv))
}
