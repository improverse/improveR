#' returns a data frame of all users
#'
#' @references ics1142
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

#' returns effective rights for a user on a specific resource
#'
#' @description Gets the effective permissions for a user on a specific resource.
#' The response includes read and modify permissions which map to:
#' - read: retrieve file operations
#' - modify: update file (if resource is a file) or upload file (if resource is a folder)
#' @param ident Ident of the requested resource resourceId, entityId or path
#' @param from root for a relativePath
#' @param memberId The ID of the user/member to check permissions for. If NULL, uses the current authenticated user.
#' @return A list containing the effective rights/permissions for the user on the resource
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
