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
  user <- conf()$user
  if (user!="") {
    return(user)
  }
  audit <- loadAuditTrail(pwd()$entityId)$data[[1]]
  audit<-audit[audit$entityReferenceType=="Run" & audit$operation=="create" & audit$createdAt==max(audit$createdAt),]
  return(audit$username)
}
