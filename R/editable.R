
#' setEditable
#'
#' @description setEditable sets the editable mode, default true
#' @param editable if true it is possible to edit contents in the repository
#' @seealso [improveConnected()]
#' @export
setEditable <- function(editable=T) {
  cacheEnv$editable <- editable
}

#' improveEditable
#'
#' @description improveEditable checks if improveConnect was called and if the editable flag was set
#' @seealso [improveConnected()]
#' @export
improveEditable <- function() {
  improveConnected()
  if (!isEditable()) {
    stop("improve is connected but the editable flag was not set. use setEditable.")
  }
}


isEditable <- function() {
  improveConnected()
  return(
    (!is.null(cacheEnv$editable) && cacheEnv$editable)
  )
}

#' improveConnected
#'
#' @description improveConnected checks if improveConnect was called.
#' @param silent if TRUE no log message is printed
#' @seealso [improveConnect()], [improveDisconnect()]
#' @export


