#' takes a timestamp as used in improve and converts it to a POSIX R time
#' @param timestamp as character, numeric or list
#'
#' @export
convertImproveTimestampToPosix <- function(timestamp) {
  posixs<-lapply(timestamp,function(t) {
    return(as.POSIXct(as.numeric(t)/1000, origin="1970-01-01"))
  })
  if (length(posixs)==1) {
    return(posixs[[1]])
  }
  posixV <- do.call("c",posixs)
  return(posixV)
}
