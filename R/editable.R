#' Enable or Disable Repository Write Mode
#'
#' Controls whether write operations (create, update, delete) are permitted
#' on the improve repository. By default, connections are read-only for safety.
#' Call this function with `TRUE` to enable modifications.
#'
#' @param editable Logical. If `TRUE` (default), enables write operations to the
#'   repository. If `FALSE`, restricts the session to read-only operations.
#'
#' @returns Invisibly returns `NULL`. Called for its side effect of setting
#'   the editable flag in the session cache.
#'
#' @details
#' **Warning:** Enabling write mode allows operations that modify repository
#' content, including:
#' \itemize{
#'   \item Creating new resources, steps, and workflows
#'   \item Updating existing files and metadata
#'   \item Deleting resources
#' }
#'
#' The editable state persists for the duration of the R session or until
#' explicitly changed by calling this function again.
#'
#' @examples
#' \dontrun{
#' # Connect to improve server (read-only by default)
#' improveConnect()
#'
#' # Enable write operations
#' setEditable(TRUE)
#'
#' # Perform modifications...
#' # createFile(...)
#'
#' # Disable write operations when done
#' setEditable(FALSE)
#' }
#'
#' @seealso
#' \code{\link{improveEditable}} to check and enforce write mode,
#' \code{\link{improveConnect}} to establish connection
#'
#' @export
setEditable <- function(editable=T) {
  cacheEnv$editable <- editable
}

#' Verify Write Mode is Enabled (Guard Function)
#'
#' Checks that the session is both connected to the improve repository AND
#' has write mode enabled. Stops with an error if either condition is not met.
#' Use this as a guard at the start of functions that modify repository content.
#'
#' @returns Invisibly returns `TRUE` if both conditions are met. Throws an error
#'   with a descriptive message if:
#'   \itemize{
#'     \item Not connected to the repository (calls \code{\link{improveConnected}})
#'     \item Connected but write mode is not enabled
#'   }
#'
#' @details
#' This function serves as a guard clause for write operations. It ensures that:
#' \enumerate{
#'   \item \code{\link{improveConnect}} has been called successfully
#'   \item \code{\link{setEditable}(TRUE)} has been called to enable write mode
#' }
#'
#' Package functions that modify repository content should call this function
#' at their start to fail fast with a clear error message rather than failing
#' later with an ambiguous API error.
#'
#' @examples
#' \dontrun{
#' # This function is typically used inside other functions:
#' my_write_function <- function(path, data) {
#'   improveEditable()  # Guard: stops if not connected or not editable
#'   # ... perform write operations ...
#' }
#'
#' # Direct usage to check write capability:
#' improveConnect()
#' setEditable(TRUE)
#' improveEditable()  # Passes silently
#' }
#'
#' @seealso
#' \code{\link{setEditable}} to enable write mode,
#' \code{\link{improveConnected}} to check connection only
#'
#' @export
improveEditable <- function() {
  improveConnected()
  if (!isEditable()) {
    stop("improve is connected but the editable flag was not set. use `setEditable`.")
  }
}


isEditable <- function() {
  improveConnected()
  return(
    (!is.null(cacheEnv$editable) && cacheEnv$editable)
  )
}

#' Check connection with improve
#'
#' @description improveConnected checks if improveConnect was called.
#' @param silent if TRUE no log message is printed
#' @seealso [improveConnect()], [improveDisconnect()]
#' @export


