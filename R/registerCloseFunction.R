#' lock object
#' @export
improveCloseToken = new.env(parent=emptyenv())


.onLoad <- function(libname, pkgname) {
  reg.finalizer(improveCloseToken, function(a) {
    functions <- sort(ls(envir=improveCloseToken))
    if (length(functions)>0) {
      for (i in 1:length(functions)) {
        func <- get(functions[1],envir=improveCloseToken)
        func()
      }
    }
  }, onexit= TRUE)

}

#' registerCloseFunction registers a function that is called when the r session is ended. In a batch job the r session is ended when the run is over, in rstudio the session is ended, when the user either closes rstudio with the close button or selects terminate or restart session
#' the close functions are executed in alphabetical order, so numbering as prefix can guarantee an order
#' @param name name of the function, this name is used for the alphabetical ordering and unregistering
#' @param func, the function that is executed
#' @export
registerCloseFunction <- function(name,func) {
  if (name %in% ls(envir=improveCloseToken)) {
    logging::logwarn(paste0("Function ",name," was already registered, will be overwritten"))
  }
  assign(name,func,envir=improveCloseToken)
}

#' unregisterCloseFunction removes a close function by the name it was registered with
#' @param name the registered name
#' @export
unregisterCloseFunction <- function(name) {
  if (name %in% ls(envir=improveCloseToken)) {
    rm(list = ls(pattern = name,envir=improveCloseToken),envir=improveCloseToken)
  }
  else {
    logging::logwarn(paste0("Function ",name," was not registered, will not be unregistered"))
  }
}

#' lists the name of all functions registered by registerCloseFunction
#' @export
listCloseFunctions <- function() {
  ls(envir=improveCloseToken)
}
