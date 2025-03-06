cacheEnv <- new.env(parent = emptyenv())


#NOTE: Log message type was changed from log_error to log_info; Postive response added.

#' improveConnected 
#'
#' @description 
#' improveConnected checks if improveConnect was called.
#' @seealso [improveConnect()], [improveDisconnect()]
#' @export

# improveConnected <- function(silent = FALSE) {
#   if (is.null(cacheEnv$initialized)) {
#     if (!silent) {
#       log_info("No connection detected. improveConnect was not called or an error occurred during connection.")
#     }
#     return(invisible(FALSE))
#     stop("not connected")
#   } else if (!is.null(cacheEnv$initialized) && cacheEnv$initialized == TRUE) {
#     if (!silent) {
#       log_info("Connection already established.")
#     }
#     return(invisible(TRUE))
#   }
# }

improveConnected <- function(silent = FALSE) {
  if (is.null(cacheEnv$initialized) | cacheEnv$initialized == FALSE) {
    if (!silent) {
      log_info("No connection detected. improveConnect was not called or an error occurred during connection.")
    }
    return(invisible(FALSE))
  } else if (cacheEnv$initialized == TRUE) {
    if (!silent) {
      log_info("Connection already established.")
    }
    return(invisible(TRUE))
  } else {
    # Handle any other state if needed, for example:
    if (!silent) {
      log_info("Connection status is not recognized.")
    }
    return(invisible(FALSE))
  }
}



#' improveDisconnect
#'
#' @description improveDisconnect removes all connection information.
#' @param env default is 'cacheEnv'
#' @references [ics1081](obsidian://open?vault=improve-specs&file=specifications%2Frepository%2Fimprover%2FClient%2FResource%2Fconnections%2Fics1081%20improveConnect)
#' @seealso [improveConnect()], [improveConnected()]
#' @export
improveDisconnect <- function(env=cacheEnv) {
  rm(list = ls(envir = env), envir = env)
}

#' improveConnect
#'
#' @description
#' improveConnect looks for connection information and sets this connection information as default.
#' There are three ways to set connection information (in descending priority): 
#' 1. via the command line
#' 1. via environment variables
#' 1. via a conf.json file
#'
#' ## Command line 
#' Command line arguments have to be in the correct order.
#' 1. token ID of the step that was used to initiate the connection
#' 1. the repo URL in the format https://repoaddress:repoPort/repository
#' 
#' ## Enviornment variables
#' The following environment variables have to be set:
#' * IMPROVER_TOKEN: mandatory if user and password are empty<br>
#' * IMPROVER_STEP<br>
#' * IMPROVER_REPO_URL mandatory<br>
#' * IMPROVER_USER<br>
#' * IMPROVER_PASSWORD<br>
#' * IMPROVER_WORKSPACE<br>
#' * IMPROVER_SECURITY: insecure if there is no valid certificate
#'
#' ## Conf.json file
#' The file has to be located in the working directory and contain the following
#' details:
#' * reqToken: mandatory if user and password are empty
#' * stepId
#' * repoUrl: mandatory
#' * runWorkspace
#' * user
#' * password
#'
#' @param logLevel possible LogLevels: DEBUG, INFO, WARN, ERROR
#' @param secure if TRUE the certificates are checked
#' @param offlinePossible if TRUE the setup continues even if no connection is possible
#' @param persistentCaching default is FALSE; persists and reloads the caches on the filesystem in `.improver.cache`
#' if the environment variable improver.logfile is set. The logging is additionally added to this file
#' @references [ics1081](obsidian://open?vault=improve-specs&file=specifications%2Frepository%2Fimprover%2FClient%2FResource%2Fconnections%2Fics1081%20improveConnect)
#' @export
#' @seealso [improveConnected()], [improveDisconnect()]
improveConnect <- function(logLevel = "INFO", secure = TRUE, offlinePossible = FALSE, persistentCaching = FALSE) {
  # TODO load pwd

  secureFlag <- Sys.getenv("IMPROVER_SECURITY")
  if (!is.null(secureFlag) && secureFlag == "insecure") {
    secure <- F
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
    user <- Sys.getenv("IMPROVER_USER")
    password <- Sys.getenv("IMPROVER_PASSWORD")
    if (!password == "") {
      Sys.unsetenv("IMPROVER_PASSWORD")
      improveLogin(repo = repoUrl, user = user, password = password, logLevel = logLevel, secure = secure, shortEntityId = stepId)
      return()
    }
    workspace <- Sys.getenv("IMPROVER_WORKSPACE")
    if (workspace == "") {
      workspace <- getwd()
    }
    serverAddress <- paste0(repoUrl, "/api/v1/")
    runWorkspace <- paste0(workspace, "/")
    confData <- data.frame(repoUrl = serverAddress, runWorkspace = runWorkspace, user = "")
    confData$stepId <- stepId
    if (reqToken != "") {
      if (user != "") {
        confData$user <- user
      }
      confData$reqToken <- reqToken
    } else {
      confData$user <- user
      confData$password <- password
    }
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
          logging::logerror("IMPROVER_TOKEN mandatory if user and password are empty")
          logging::logerror("IMPROVER_STEP")
          logging::logerror("IMPROVER_REPO_URL mandatory")
          logging::logerror("IMPROVER_USER")
          logging::logerror("IMPROVER_PASSWORD")
          logging::logerror("IMPROVER_WORKSPACE")
          stop("Not configured correctly")
        }
        cacheEnv$offline <- T
      }
    )
  }
  if (is.na(conf()$reqToken) || is.null(conf()$reqToken) || conf()$reqToken == "") {
    if (Sys.getenv("IMPROVER_USER") == "") {
      logging::logwarn("Using Basic Authentication, use this just for development, the password is stored in plain text in conf.json")
      logging::loginfo(paste0("User: ", conf()$user))
    }
  } else {
    logging::loginfo(paste0("Token: ", conf()$reqToken))
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

# IMPROVE CLOSE
# QUESTION Why does improveClose uses a two-step approach with the intermediate collection of links to files
# before deleting them? Why not immediately deleting them once it has been established that the 
# file exists? 
# NOTE FUNCTION CODE WAS MODIFIED
## - function takes now cacheEnv as an input; otherwise testthat 
##   and the improveClose do not refer to the same enviornemnt; 

#' improveClose
#'
#' @description Cleans up everything for checkin.
#' @export
improveClose <- function(cacheEnv) {
  cleaned <- NULL
  if (!is.null(cacheEnv$createdLinks)) {
    if (length(cacheEnv$createdLinks) > 0) {    #checks number of cols
      for (i in 1:nrow(cacheEnv$createdLinks)) {
        linkEntry <- cacheEnv$createdLinks[i, ]  #assumes that df createdLinks has more than 1 col; otherwise vector not df retruned
        if (file.exists(linkEntry$localPath)) {
          cleaned <- plyr::rbind.fill(cleaned,linkEntry)
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
