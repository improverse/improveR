#' convertImproveTimestampToPosix
#' @description Takes a timestamp as used in improve and converts it to a POSIX R time
#' @param timestamp a character, numeric string or list
#'
#' @export
convertImproveTimestampToPosix <- function(timestamp) {
  posixs<-lapply(timestamp,function(t) {
    return(as.POSIXct(as.numeric(t)/1000, origin="1970-01-01")) #QUESTION: Should be add timezone; default is current timezone; if tests are called in a different tz, the test will fail
  })
  if (length(posixs)==1) {
    return(posixs[[1]])
  } 
  posixV <- do.call("c",posixs)
  return(posixV)
}
