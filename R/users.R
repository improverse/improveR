#' returns a data frame of all users
#'
#' @ics1142
#' @export
users <- function() {
  improveConnected()

  result <- authenticatedREST("/users")
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  df <- mergeListToDataframe(cont)
  df<-convertDates(df)
  return(df)
}
