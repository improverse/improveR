#' Get Current Authenticated User
#'
#' Retrieves the username of the currently authenticated user or the user who
#' initiated the current workflow step.
#'
#' @returns A character string containing the username. Returns either:
#'   \itemize{
#'     \item The authenticated user from the current session configuration, or
#'     \item The username from the audit trail of the workflow run that initiated
#'       the current step (fallback when session user is not set)
#'   }
#'
#' @details
#' The function uses a two-tier approach to identify the user:
#'
#' \enumerate{
#'   \item \strong{Primary}: Checks the session configuration for the
#'     authenticated user (set during \code{improveConnect()})
#'   \item \strong{Fallback}: If no session user is found, queries the audit
#'     trail to identify who created the current workflow run
#' }
#'
#' This dual mechanism ensures user identification even in batch processing
#' scenarios where explicit authentication may not be present.
#'
#' @examples
#' \dontrun{
#' # Connect to improve server
#' improveConnect()
#'
#' # Get current user
#' current_user <- whoami()
#' cat("Current user:", current_user, "\n")
#'
#' # Use in logging or audit contexts
#' log_message <- paste("Analysis performed by:", whoami())
#'
#' # Check permissions before operations
#' if (whoami() %in% authorized_users) {
#'   # Perform sensitive operation
#' }
#' }
#'
#' @seealso
#' \code{\link{improveConnect}} for authentication,
#' \code{\link{loadAuditTrail}} for audit trail access,
#' \code{\link{pwd}} for current step information
#'
#' @references ics1251
#' @export

whoami <- function() {
  improveConnected()

  # 1. Check session configuration (set by OAuth login)
  user <- conf()$user
  if (!is.null(user) && user != "") {
    return(user)
  }

  # 2. On server >= 4.4, use the REST /users/whoami endpoint
  repoVersion <- tryCatch(getRepositoryVersion(), error = function(e) NULL)
  if (!is.null(repoVersion)) {
    majorMinor <- tryCatch({
      parts <- strsplit(repoVersion, "[.-]")[[1]]
      as.numeric(paste0(parts[1], ".", parts[2]))
    }, error = function(e) 0)
    if (majorMinor >= 4.4) {
      result <- tryCatch(
        authenticatedREST("/users/whoami", restType = "GET"),
        error = function(e) NULL
      )
      if (!is.null(result)) {
        cont <- httr::content(result)
        if (!is.null(cont$username) && cont$username != "") {
          return(cont$username)
        }
      }
    }
  }

  # 3. Fallback: audit trail lookup (original logic for older servers / batch mode)
  audit <- loadAuditTrail(pwd()$entityId)$data[[1]]
  audit <- audit[audit$entityReferenceType == "Run" &
                   audit$operation == "create" &
                   audit$createdAt == max(audit$createdAt), ]
  return(audit$username)
}
