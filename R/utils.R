
#'byNotEmpty by that automatically uses the complete dataframe and returns null if an empty or non existing data frame is handed over
#' @param df the data frame
#' @param func the function that is applied to each row of the data frame
#' @export
byNotEmpty <- function(df,func) {
  if (!is.data.frame(df) || nrow(df)==0) {
    return(NULL)
  }
  return(
    by(df,seq_len(nrow(df)),func)
  )
}

#'byNotEmptyAsDf by that automatically uses the complete dataframe and returns null if an empty or non existing data frame is handed over
#' it automatically tries to convert the result list to a data frame again
#' @param df the data frame
#' @param func the function that is applied to each row of the data frame
#' @export
byNotEmptyAsDf <- function(df,func) {
  if (!is.data.frame(df) || nrow(df)==0) {
    return(NULL)
  }
  return(
    mergeDataframeList(
      by(df,seq_len(nrow(df)),func)
    )
  )
}
