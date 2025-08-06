parseLogFile <- function() {
  logFile <- Sys.getenv("improver.logfile")
  if (logFile=="") {
    log_info("no logfile written, write logfile by setting improver.logfile as environment variable")
    return(NULL)
  }
  tryCatch({
    logLines <- as.list(readLines(logFile))
    logItems <- lapply(logLines,function(line) {
      tokens <- strsplit(line," ",fixed=T)[[1]]
      date <- tokens[1]
      time <- tokens[2]

      contentTokens <- strsplit(tokens[3],"::",fixed=T)[[1]]
      type <- contentTokens[1]
      typeLength <- nchar(type)
      message <- substr(line,typeLength+nchar(date)+nchar(time)+5,nchar(line))
      item <- data.frame( type=type,
                          date=date,
                          time=time,
                          message=message,
                          stringsAsFactors = F)
      return(item)
    })
    logItemsDf <- mergeDataframeList(logItems)
  }, error= function() {
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

#NOTE: margittr pipe replaced by base pipe to reduce dependency
improveLastLogMessage<- function (type="",number=1) {
  logItems <- parseLogFile()
  if (is.null(logItems)) {
    return(NULL)
  }
  if (type!="") {
    logItems |> dplyr::filter(.data$type==type)
  }
  return(
    logItems |> utils::tail(number) |> dplyr::select("message") |> as.character()
  )
}
