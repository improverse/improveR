#' improveCloseToken
#' @description Locks an object.
#' @export
improveCloseToken = new.env(parent=emptyenv())

.onLoad <- function(libname, pkgname) {
  reg.finalizer(improveCloseToken, function(a) {
    # Clean up shared token refresh if running
    tryCatch({
      stopSharedTokenRefresh()
    }, error = function(e) {
      # Ignore errors during cleanup
    })

    functions <- sort(ls(envir=improveCloseToken))
    if (length(functions)>0) {
      for (i in 1:length(functions)) {
        func <- get(functions[1],envir=improveCloseToken)
        func()
      }
    }
  }, onexit= TRUE)

}


#NOTE alphabetical order of number is not always as expected. 1, 10, 2. Details needs rewording.
#' registerCloseFunction
#' @details registerCloseFunction registers a function that is called when the R session is ended.
#' In a batch job the R session is ended when the run is over. In RStudio the session is ended,
#' when the user either closes RStudio with the close button or selects terminate or restart session.
#' the close functions are executed in alphabetical order, so numbering as prefix can guarantee an order.
#' @param name Name of the function. This name is used for the alphabetical ordering and unregistering.
#' @param func the function that is executed when ending the R session.
#' @export
registerCloseFunction <- function(name,func) {
  if (name %in% ls(envir=improveCloseToken)) {
    logging::logwarn(paste0("Function ",name," was already registered, will be overwritten"))
  }
  assign(name,func,envir=improveCloseToken)
}

#' unregisterCloseFunction
#' @description unregisterCloseFunction removes a close function by the name it was registered with.
#' @param name The name under which the function has been registered.
#' @export
#' @seealso [registerCloseFunction()], [listCloseFunctions()]
unregisterCloseFunction <- function(name) {
  if (name %in% ls(envir=improveCloseToken)) {
    rm(list = ls(pattern = name,envir=improveCloseToken),envir=improveCloseToken)
  }
  else {
    logging::logwarn(paste0("Function ",name," was not registered, will not be unregistered"))
  }
}

#' listCloseFunctions
#' @description Lists the names of all functions registered by registerCloseFunction.
#' @seealso [registerCloseFunction()]
#' @export
listCloseFunctions <- function() {
  ls(envir=improveCloseToken)
}
