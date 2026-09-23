parseLogFile <- function() {
  logFile <- Sys.getenv("improver.logfile")
  if (logFile=="") {
    log_info("no logfile written, write logfile by setting improver.logfile as environment variable")
    return(NULL)
  }
  tryCatch({
    logLines <- readLines(logFile)
    if (length(logLines) == 0L) {
      return(data.frame())
    }
    # Vectorised: build one data.frame for the whole file. Building one
    # data.frame per line and combining them via plyr::rbind.fill made this
    # function the dominant cost of the test suite - see IMR-259.
    tokens <- strsplit(logLines," ",fixed=T)
    date <- vapply(tokens,function(t) t[1],character(1))
    time <- vapply(tokens,function(t) t[2],character(1))
    content <- vapply(tokens,function(t) t[3],character(1))
    # strsplit(x,"::")[[1]][1] yields NA for "" and for NA, sub() does not.
    # Kept explicit so malformed lines behave exactly as before.
    type <- ifelse(is.na(content) | content == "", NA_character_,
                   sub("::.*$","",content))
    message <- substr(logLines,nchar(type)+nchar(date)+nchar(time)+5,nchar(logLines))
    data.frame( type=type,
                date=date,
                time=time,
                message=message,
                stringsAsFactors = F)
  }, error= function(e) {
    return(NULL)
  })

}

initImproveLogging <- function(logLevel) {
  logFile <- Sys.getenv("improver.logfile")
  
  if (logFile != "") {
    # File logging only - no console output
    logging::basicConfig(level = logLevel)
    # Remove default console handler
    logging::removeHandler("basic.stdout")
    # Add file handler
    logging::addHandler(logging::writeToFile, logger = "", file = logFile)
  } else {
    # Console logging only
    logging::basicConfig(level = logLevel)
  }
}

#NOTE: margittr pipe replaced by base pipe to reduce dependencies
improveLastLogMessage<- function (type="",number=1) {
  logItems <- parseLogFile()
  if (is.null(logItems)) {
    return(NULL)
  }
  if (type!="") {
    # Base subsetting on purpose: inside dplyr::filter() a bare `type` would
    # resolve to the column rather than to the argument, leaving the filter
    # without effect - which is the defect fixed here (IMR-259).
    logItems <- logItems[!is.na(logItems$type) & logItems$type==type, , drop=FALSE]
  }
  # as.character() on a data.frame deparses it: with number > 1 it returned the
  # literal text 'c("a", "b")', with no match the literal text 'character(0)'.
  return(
    utils::tail(logItems$message, number)
  )
}
