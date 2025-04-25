#' returns a data frame of all users
#'
#' @ics1142
#' @export
users <- function() {
  improveR::improveConnected()

  result <- authenticatetREST("/users")
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  df<-convertDates(df)
  return(df)
}
