#' List All Users in improve Repository
#'
#' Retrieves a list of all users registered in the improve repository.
#'
#' @returns A data frame containing user information with columns:
#'   \describe{
#'     \item{id}{Character. Unique user identifier}
#'     \item{username}{Character. Username for authentication}
#'     \item{firstName}{Character. User's first name}
#'     \item{lastName}{Character. User's last name}
#'     \item{email}{Character. User's email address}
#'     \item{active}{Logical. Whether the user account is active}
#'     \item{createdAt}{POSIXct. Creation timestamp}
#'     \item{lastModified}{POSIXct. Last modification timestamp}
#'   }
#'   Returns `NULL` if the user list cannot be retrieved.
#'
#' @references ics1142
#' @seealso \code{\link{whoami}} to get the current user
#' @examples
#' \dontrun{
#' # List all users
#' all_users <- users()
#'
#' # Find a specific user
#' admin_user <- all_users[all_users$username == "admin", ]
#' }
#' @export
users <- function() {
  improveConnected()

  return(restGetAsDf("/users", dates = TRUE))
}

#' Get Effective User Rights for Resource
#'
#' Determines the effective permissions (rights) for a specific user on a given
#' resource, taking into account group memberships and inherited permissions.
#'
#' @param ident Identifier of the resource to check permissions for. Can be a
#'   path, resource ID, or entity ID.
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#' @param memberId Character. The ID of the user/member to check permissions for.
#'   If \code{NULL} (default), checks permissions for the currently authenticated user.
#'
#' @returns A list containing the effective rights:
#'   \itemize{
#'     \item \code{read} - Logical. Permission to view/download the resource.
#'     \item \code{modify} - Logical. Permission to update (for files) or upload
#'       (for folders).
#'     \item \code{delete} - Logical. Permission to delete the resource.
#'     \item \code{admin} - Logical. Full administrative rights.
#'   }
#'   Returns `NULL` if the resource or user cannot be found.
#'
#' @examples
#' \dontrun{
#' # Check my rights on a folder
#' my_rights <- effectiveRights("/Projects/Analysis")
#' if (my_rights$modify) {
#'   message("I can write to this folder")
#' }
#'
#' # Check rights for another user
#' user_id <- users()[1, "id"]
#' their_rights <- effectiveRights("/Projects/Analysis", memberId = user_id)
#' }
#' @seealso \code{\link{users}}, \code{\link{whoami}}
#' @export
effectiveRights <- function(ident,from=pwd(), memberId = NULL) {
  improveConnected()

  resource <- loadResource(ident,from)
  if (is.null(resource)) {
    log_warn("cannot find resource by ident:",ident)
    return(NULL)
  }
  resourceId <- resource$resourceId
  # If memberId not provided, get the current user's ID
  if (is.null(memberId)) {
    currentUsername <- whoami()
    allUsers <- users()
    if (!is.null(allUsers)) {
      currentUser <- allUsers[allUsers$username == currentUsername, ]
      if (nrow(currentUser) > 0) {
        memberId <- currentUser$id[1]
      } else {
        stop(paste0("Could not find user ID for username: ", currentUsername))
      }
    } else {
      stop("Could not retrieve users list")
    }
  }

  result <- authenticatedREST("/resources/{resourceId}/effectiveRights/{memberId}",
                              urlParams = list(resourceId = resourceId, memberId = memberId))
  if (is.null(result)) {
    return(NULL)
  }
  cont <- httr::content(result)
  return(cont)
}
