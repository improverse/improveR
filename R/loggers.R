logEnv <- new.env()
logEnv$redirectToList <- FALSE
logEnv$logs <- list()
logEnv$context <- "base"

#' redirectLogs
#' @description redirectLogs redirects the log from the default logstream to a structured list, with a different context.
#' @param redirect Boolean, TRUE turns on redirection,  FALSE turns it off.
#' @noRd
redirectLogs <- function(redirect) {
  logEnv$redirectToList <- redirect
}
#' resetLogs
#' @description resetLogs resets the strutured list from redirectLogs.
#' @seealso [redirectLogs()]
#' @noRd
resetLogs <- function(){
  logEnv$logs <- list()
}

#' hasCurrentError checks if there is an error message in the current context
#' @noRd
hasCurrentError <- function() {

  currentLogs <- logEnv$logs[[logEnv$context]]
  if (is.null(currentLogs)) {
    return(FALSE)
  }
  return(!is.null(currentLogs[["error"]]))
}

#' printLogs
#' @description printLogs prints the strucured log list to the console.
#' @param reset Boolean, TRUE resets the list after printing.
#' @noRd
printLogs <- function(reset=FALSE) {
  a<-lapply(names(logEnv$logs),function(logName) {
    print(logName)
    subLogs <- logEnv$logs[[logName]]
    a<-lapply(names(subLogs),function(logName) {
      print(logName)
      mesgs <- subLogs[[logName]]
      a<-lapply(mesgs,print)
    })
  })
  if (reset) {
    resetLogs()
  }
}

#' setLogContext
#' @description setLogContext sets the context for the structured log list.
#' @param context a string, the context name.
#' @noRd
setLogContext <- function(context) {
  logEnv$context <- context
}

#' Log_debug
#' @param ... combines all items to one log message
#' @returns No meaningful value - called for its side effect of writing the message.
#'   Where the log is redirected to a structured list (internal `redirectLogs`) the message
#'   is appended to that list under the current context; otherwise it is handed to the
#'   `logging` package.
#' @export
log_debug <- function(...) {
  msg <- pasteAndResolveResource(...)
  if (logEnv$redirectToList) {
    log("debug",msg)
  } else {
    logging::logdebug(msg)
  }

}

#' Log_info
#' @param ... combines all items to one log message
#' @returns No meaningful value - called for its side effect of writing the message.
#'   Where the log is redirected to a structured list (internal `redirectLogs`) the message
#'   is appended to that list under the current context; otherwise it is handed to the
#'   `logging` package.
#' @export
log_info <- function(...) {
  msg <- pasteAndResolveResource(...)
  if (logEnv$redirectToList) {
    log("info",msg)
  } else {
    logging::loginfo(msg)
  }
}

#' Log_warn
#' @param ... combines all items to one log message
#' @returns No meaningful value - called for its side effect of writing the message.
#'   Where the log is redirected to a structured list (internal `redirectLogs`) the message
#'   is appended to that list under the current context; otherwise it is handed to the
#'   `logging` package.
#' @export
log_warn <- function(...) {
  msg <- pasteAndResolveResource(...)
  if (logEnv$redirectToList) {
    log("warn",msg)
  } else {
    logging::logwarn(msg)
  }
}

#' Log_error
#' @param ... combines all items to one log message
#' @returns No meaningful value - called for its side effect of writing the message.
#'   Where the log is redirected to a structured list (internal `redirectLogs`) the message
#'   is appended to that list under the current context; otherwise it is handed to the
#'   `logging` package.
#' @export
log_error <- function(...) {
  msg <- pasteAndResolveResource(...)
  if (logEnv$redirectToList) {
    log("error",msg)
  } else {
    logging::logerror(msg)
  }
}

pasteAndResolveResource <- function(...) {
  args <- list(...)
  if (length(args)>0) {
    mesg <- ""
    for (i in 1:length(args)) {
      msgPart <- args[i]
      if (is.list(msgPart) && ("entityId" %in% names(msgPart[[1]]))) {
        msgPart <- paste(msgPart[[1]]$entityId,collapse = ", ")
      }
      mesg <- paste(mesg,msgPart)
    }
    return(substr(mesg,2,nchar(mesg)))
  } else {
    return("empty")
  }
}


log <- function(level,message) {
  entry <- logEnv$context
  logList <- list()
  if (is.character(entry) && entry %in% names(logEnv$logs)) {
    logList <- logEnv$logs[[entry]]
  }
  messageList <- list()
  if (level %in% names(logList)) {
    messageList <- logList[[level]]
  }
  messageList<- c(messageList,message)
  logList[[level]]<-messageList
  logEnv$logs[[entry]]<-logList
}



