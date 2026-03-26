#' Get Latest Revision
#'
#' Retrieves the latest revision (transaction) from the repository.
#'
#' @returns A list with the latest revision data, or \code{NULL} if not available.
#' @references ccs10
#' @export
getLatestRevision <- function() {
  improveConnected()
  result <- authenticatedREST("/revisions/latest", restType = "GET")
  if (is.null(result)) {
    log_warn("Failed to get latest revision")
    return(NULL)
  }
  return(httr::content(result))
}

#' Create Transaction
#'
#' Opens a new transaction on the repository, optionally locking a resource.
#'
#' @param comment Optional comment for the transaction.
#' @param lockResourceId Optional resource ID to lock within the transaction.
#' @returns A list with the created transaction data, or \code{NULL} on failure.
#' @references ccs11
#' @export
createTransaction <- function(comment = "", lockResourceId = NULL) {
  improveEditable()
  data <- list(comment = comment)
  if (!is.null(lockResourceId)) {
    data$lockResourceId <- lockResourceId
  }
  result <- authenticatedREST("/revisions/createTransaction",
                              data = data,
                              restType = "POST")
  if (is.null(result)) {
    log_warn("Failed to create transaction")
    return(NULL)
  }
  return(httr::content(result))
}

