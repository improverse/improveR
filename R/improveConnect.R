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

#' clearConnectionData
#'
#' @description deletes repoUrl, stepId, and token from the environment variables and calls improveDisconnect
#' @param includeRepoData includes the repository URL and the selected step in the clear process
#' @seealso [improveDisconnect()]
#' @export
clearConnectionData <- function(includeRepoData=F) {
  if (includeRepoData) {
    Sys.setenv(IMPROVER_STEP="")
    Sys.setenv(IMPROVER_REPO_URL="")
  }
  Sys.setenv(IMPROVER_USER="")
  Sys.setenv(IMPROVER_PASSWORD="")
  Sys.setenv(IMPROVER_TOKEN="")
  Sys.setenv(IMPROVER_REFRESH_TOKEN="")
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
#' @export
#' @seealso [improveConnected()], [improveDisconnect()]
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
      logging::loginfo("Loaded cached data from file .improver.cache")
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
    logging::loginfo("Access Data parsed from Command Line:")
    if (noCommandArgs > 4 && commandArgs(trailingOnly = TRUE)[[5]] == "write") {
      logging::loginfo("(also written to conf.json)")
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
      improveOAuth(repoUrl,shortEntityId = stepId)
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
    logging::loginfo("Access Data from Environment variables:")
  } else {
    tryCatch(
      {
        conf <- as.data.frame(jsonlite::read_json("conf.json"), stringsAsFactors = FALSE)
        # TODO this is a fix for docker mapping
        conf$runWorkspace <- paste0(getwd(), "/")
        logging::loginfo("Access Data read from conf.json:")
        assign("conf", conf, cacheEnv)
      },
      error = function(cond) {
        if (!offlinePossible) {
          logging::logerror(cond)
          logging::logerror("Neither commandline arguments, environment variables nor conf.json supplied.")
          logging::logerror("Expected commandline arguments:")
          logging::logerror("<command-file>")
          logging::logerror("<jwt>")
          logging::logerror("<step-entityId>")
          logging::logerror("<repoUrl>")
          logging::logerror("<runWorkspace>")
          logging::logerror("Expected environment variables:")
          logging::logerror("For Run Token Authentication:")
          logging::logerror("  IMPROVER_TOKEN (mandatory)")
          logging::logerror("  IMPROVER_REPO_URL (mandatory)")
          logging::logerror("  IMPROVER_STEP (optional)")
          logging::logerror("For OAuth Authentication:")
          logging::logerror("  IMPROVER_REPO_URL (mandatory)")
          logging::logerror("  IMPROVER_STEP (mandatory)")
          logging::logerror("  IMPROVER_HEADLESS_OAUTH (optional)")
          logging::logerror("  IMPROVER_WORKSPACE (optional)")
          stop("Not configured correctly")
        }
        cacheEnv$offline <- T
      }
    )
  }

  if (!is.na(conf()$reqToken) && !is.null(conf()$reqToken) && conf()$reqToken != "") {
    logging::loginfo("Using run token authentication")
    logging::loginfo(paste0("Token: ", conf()$reqToken))
  } else {
    logging::loginfo("OAuth authentication completed")
  }
  logging::loginfo(paste0("StepId: ", conf()$stepId))
  if (!is.null(conf()$stepId) & !is.na(conf()$stepId)) {
    rootStep <- loadResource(conf()$stepId)
    assign(x = "pwd", value = rootStep, envir = cacheEnv)
  }
  logging::loginfo(paste0("repoUrl: ", conf()$repoUrl))
  logging::loginfo(paste0("runWorkspace: ", conf()$runWorkspace))
  setRootPath(getwd())
  registerCloseFunction("1removeImproveJson", improveClose)

  # Check repository version after configuration is complete
  if (!cacheEnv$offline) {
    repoBaseUrl <- gsub("/api/v1/$", "/", conf()$repoUrl)
    repoVersionUrl <- paste0(repoBaseUrl, "repository?status")
    tryCatch({
      versionResult <- unauthenticatedREST(repoVersionUrl, restType = "GET")
      if (versionResult$status_code == 200) {
        # Parse HTML content
        versionContent <- httr::content(versionResult, "text", encoding = "UTF-8")
        # Extract version from HTML - looking for "Version: X.X.X-X (hash)"
        versionMatch <- regmatches(versionContent, regexpr("Version:\\s*([0-9\\.\\-]+)\\s*\\([a-f0-9]+\\)", versionContent))
        if (length(versionMatch) > 0) {
          # Extract just the version number
          version <- gsub("Version:\\s*([0-9\\.\\-]+).*", "\\1", versionMatch[1])
          logging::loginfo(paste0("Connected to repository version: ", version))
          cacheEnv$repositoryVersion <- version
        } else {
          logging::logwarn("Could not parse repository version from response")
        }
      } else {
        logging::logwarn("Could not retrieve repository version information")
      }
    }, error = function(e) {
      logging::logwarn(paste0("Failed to check repository version: ", e$message))
    })
  }
}

#' checkConnect
#'
#' @description Checks if the current connection is still valid by attempting to load the IMPROVER_STEP resource.
#' If the connection is invalid, it clears connection data and attempts to reconnect.
#' @return invisible TRUE if connection is valid, otherwise attempts reconnection
#' @export
checkConnect <- function() {
  # First check if we're connected at all
  if (!improveConnected(silent = TRUE)) {
    logging::loginfo("Not connected. Attempting to connect...")
    improveConnect()
    return(invisible(TRUE))
  }

  # Try to load the step resource to verify connection is still valid
  stepId <- conf()$stepId
  if (!is.null(stepId) && !is.na(stepId) && stepId != "") {
    tryCatch({
      # Use updateResource to force a fresh load from server
      stepResource <- updateResource(stepId)
      if (!is.null(stepResource)) {
        logging::logdebug("Connection verified - step resource loaded successfully")
        return(invisible(TRUE))
      } else {
        logging::logwarn("Step resource returned NULL - reconnecting")
      }
    }, error = function(e) {
      logging::logwarn(paste0("Failed to load step resource: ", e$message))
    })
  } else {
    logging::logdebug("No step ID configured - checking basic connectivity")
    # If no step ID, just try a basic API call
    tryCatch({
      result <- authenticatedREST("/users")
      if (!is.null(result) && result$status_code >= 200 && result$status_code < 300) {
        logging::logdebug("Connection verified via users endpoint")
        return(invisible(TRUE))
      }
    }, error = function(e) {
      logging::logwarn(paste0("Failed to verify connection: ", e$message))
    })
  }

  # If we get here, connection is invalid - clear and reconnect
  logging::loginfo("Connection invalid - clearing connection data and reconnecting")
  clearConnectionData()
  improveConnect()
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

repoPrefix <- function() {
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

#' getLogFile
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

#' getRepositoryVersion
#'
#' @description Returns the version of the connected repository, if available.
#' @return Character string with the repository version, or NULL if not available
#' @export
getRepositoryVersion <- function() {
  improveConnected()
  return(get0("repositoryVersion", envir = cacheEnv))
}
