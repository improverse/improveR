#' Lock Resource for Exclusive Editing
#'
#' Locks a resource on the improve server to prevent concurrent modifications by other users.
#' This ensures data integrity during editing operations and prevents conflicts in collaborative
#' pharmaceutical workflows.
#'
#' @param ident Resource identifier (entityId, resourceId, or path). See \code{\link{common_ident}}
#'   for supported identifier formats.
#' @param from Base path for resolving relative paths. Defaults to current working directory
#'   from \code{\link{pwd}}.
#'
#' @returns Logical value:
#' \describe{
#'   \item{TRUE}{Resource successfully locked by current user}
#'   \item{FALSE}{Lock failed - resource already locked by another user or server error}
#' }
#'
#' @details
#' \strong{Lock Behavior:}
#' \itemize{
#'   \item Only one user can hold a lock on a resource at a time
#'   \item Lock prevents other users from editing until released with \code{\link{unlockResource}}
#'   \item Lock is automatically released when session ends or times out
#'   \item Requires editable session mode (see \code{\link{setEditable}})
#' }
#'
#' \strong{Lock Status:}
#' If the resource is already locked, a warning message indicates which user holds the lock.
#' You must wait for them to unlock or contact an administrator for lock override.
#'
#' \strong{Best Practice:}
#' Always unlock resources when finished editing to avoid blocking collaborators.
#' Use \code{try()} or \code{on.exit()} to ensure unlocking even if errors occur.
#'
#' @references ics1139
#'
#' @seealso
#' \code{\link{unlockResource}} to release lock,
#' \code{\link{setEditable}} to enable write operations,
#' \code{\link{updateResource}} for refreshing resource state
#'
#' @examples
#' \dontrun{
#' # Connect and enable editing
#' improveConnect()
#' setEditable(TRUE)
#'
#' # Lock a resource before editing
#' if (lockResource("MyAnalysis/data.csv")) {
#'   # Perform editing operations
#'   # ...
#'
#'   # Always unlock when done
#'   unlockResource("MyAnalysis/data.csv")
#' } else {
#'   message("Resource is locked by another user")
#' }
#'
#' # Safe pattern with automatic unlock
#' locked <- lockResource("MyAnalysis/results")
#' if (locked) {
#'   on.exit(unlockResource("MyAnalysis/results"))
#'   # Edit operations here
#' }
#' }
#'
#' @export
lockResource <- function(ident,from=pwd()) {
  improveEditable()
  res <- updateResource(ident,from)
  if ("lockedByName" %in% names(res) && !is.na(res$lockedByName)) {
    log_warn(res,"is currently locked by user",res$lockedByName,", you have to unlock before locking")
    return(FALSE)
  }
  result <- authenticatedREST("/resources/{resourceId}/lock",
                                            urlParams = list(resourceId=res$resourceId),
                                            restType = "PUT")
  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to lock resource:", res$resourceId)
  return(FALSE)
}

#' Unlock Resource to Allow Collaborative Access
#'
#' Releases a lock on a resource, allowing other users to edit it. This should be called
#' after completing editing operations to restore collaborative access in pharmaceutical
#' workflows.
#'
#' @param ident Resource identifier (entityId, resourceId, or path). See \code{\link{common_ident}}
#'   for supported identifier formats.
#' @param from Base path for resolving relative paths. Defaults to current working directory
#'   from \code{\link{pwd}}.
#'
#' @returns Logical value:
#' \describe{
#'   \item{TRUE}{Resource successfully unlocked}
#'   \item{FALSE}{Unlock failed - resource not locked or server error}
#' }
#'
#' @details
#' \strong{Unlock Behavior:}
#' \itemize{
#'   \item Only the user who locked the resource (or administrator) can unlock it
#'   \item Unlocking restores collaborative access for all team members
#'   \item Requires editable session mode (see \code{\link{setEditable}})
#'   \item Attempting to unlock an already-unlocked resource returns FALSE with a warning
#' }
#'
#' \strong{Lock Ownership:}
#' If you attempt to unlock a resource locked by another user, the operation will fail.
#' Contact an administrator for force-unlock if the original user is unavailable.
#'
#' \strong{Best Practice:}
#' Always unlock resources promptly after editing to avoid blocking collaborators.
#' Use \code{on.exit()} in functions to ensure unlock happens even if errors occur.
#'
#' \strong{Warning:}
#' Do not force-unlock resources while another user is actively editing, as this may
#' cause data conflicts or loss of unsaved changes.
#'
#' @references ics1139
#'
#' @seealso
#' \code{\link{lockResource}} to acquire lock,
#' \code{\link{setEditable}} to enable write operations,
#' \code{\link{updateResource}} for refreshing resource state
#'
#' @examples
#' \dontrun{
#' # Basic lock/unlock pattern
#' improveConnect()
#' setEditable(TRUE)
#'
#' lockResource("MyAnalysis/data.csv")
#' # ... perform edits ...
#' unlockResource("MyAnalysis/data.csv")
#'
#' # Safe pattern with automatic unlock on function exit
#' edit_resource <- function(path) {
#'   if (!lockResource(path)) {
#'     stop("Could not acquire lock")
#'   }
#'   on.exit(unlockResource(path))
#'
#'   # Edit operations here - unlock happens automatically
#'   # even if an error occurs
#' }
#'
#' # Check unlock status
#' if (unlockResource("MyAnalysis/results")) {
#'   message("Resource unlocked successfully")
#' } else {
#'   message("Resource was not locked or unlock failed")
#' }
#' }
#'
#' @export
unlockResource <- function(ident,from=pwd()) {
  improveEditable()
  res <- updateResource(ident,from)
  if (!("lockedByName" %in% names(res)) || is.na(res$lockedByName)) {
    log_warn(res,"is currently not locked")
    return(FALSE)
  }
  result <- authenticatedREST("/resources/{resourceId}/unlock",
                                            urlParams = list(resourceId=res$resourceId),
                                            restType = "PUT")
  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to unlock resource:", res$resourceId)
  return(FALSE)
}
