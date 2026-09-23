#' Get Resource ACL Entries
#'
#' Retrieves all Access Control List (ACL) entries for a given resource.
#'
#' @param ident Identifier of the resource. Can be a path, resource ID, or entity ID.
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#'
#' @returns A data frame of ACL entries with columns such as id, resourceId,
#'   memberId, visible, read, modify, changeRights, traversableContainer,
#'   traversableLeaf, inherit, rightsArea, orderNr.
#'   Returns \code{NULL} if the resource cannot be found.
#'   Returns \code{NULL} if there are no ACL entries.
#'
#' @examples
#' \dontrun{
#' acl <- getResourcePermissions("/Projects/MyProject")
#' }
#' @seealso \code{\link{effectiveRights}}, \code{\link{effectiveUserPermissions}}
#' @export
getResourcePermissions <- function(ident, from = pwd()) {
  improveConnected()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find resource by ident:", ident)
    return(NULL)
  }
  df <- restGetAsDf("/resources/{resourceId}/acl",
                     urlParams = list(resourceId = resource$resourceId))
  if (is.null(df)) {
    log_warn("getResourcePermissions: no ACL entries found for resource:", resource$resourceId)
  }
  return(df)
}

#' Get Effective Permissions for All Users
#'
#' Retrieves the effective permissions for all users on a given resource,
#' taking into account group memberships and inherited permissions.
#'
#' @param ident Identifier of the resource. Can be a path, resource ID, or entity ID.
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#'
#' @returns A data frame of all users with their effective rights on the resource.
#'   Returns \code{NULL} if the resource cannot be found.
#'   Returns \code{NULL} if there are no entries.
#'
#' @examples
#' \dontrun{
#' perms <- effectiveUserPermissions("/Projects/MyProject")
#' }
#' @seealso \code{\link{effectiveRights}}, \code{\link{getResourcePermissions}}
#' @export
effectiveUserPermissions <- function(ident, from = pwd()) {
  improveConnected()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find resource by ident:", ident)
    return(NULL)
  }
  df <- restGetAsDf("/resources/{resourceId}/effectiveUserRights",
                     urlParams = list(resourceId = resource$resourceId))
  if (is.null(df)) {
    log_warn("effectiveUserPermissions: no effective user permissions found for resource:", resource$resourceId)
  }
  return(df)
}

#' List All Groups
#'
#' Retrieves a list of all groups in the improve repository.
#'
#' @returns A data frame of groups.
#'   Returns \code{NULL} if the groups cannot be retrieved.
#'
#' @examples
#' \dontrun{
#' groups <- loadGroups()
#' }
#' @seealso \code{\link{loadGroup}}, \code{\link{loadGroupUsers}}
#' @export
loadGroups <- function() {
  improveConnected()
  return(restGetAsDf("/groups"))
}

#' Get Group Details
#'
#' Retrieves details for a specific group by its ID.
#'
#' @param groupId Character. The ID of the group.
#'
#' @returns A list with group details.
#'   Returns \code{NULL} if the group cannot be found.
#'
#' @examples
#' \dontrun{
#' group <- loadGroup("some-group-id")
#' }
#' @seealso \code{\link{loadGroups}}, \code{\link{loadGroupUsers}}
#' @export
loadGroup <- function(groupId) {
  improveConnected()
  result <- authenticatedREST(
    "/groups/{groupId}",
    urlParams = list(groupId = groupId))
  if (is.null(result)) {
    log_warn("loadGroup: failed to retrieve group:", groupId)
    return(NULL)
  }
  cont <- httr::content(result)
  return(cont)
}

#' Get Group Members
#'
#' Retrieves all users belonging to a specific group.
#'
#' @param groupId Character. The ID of the group.
#'
#' @returns A data frame of users in the group.
#'   Returns \code{NULL} if the group cannot be found.
#'   Returns \code{NULL} if the group has no members.
#'
#' @examples
#' \dontrun{
#' members <- loadGroupUsers("some-group-id")
#' }
#' @seealso \code{\link{loadGroup}}, \code{\link{loadGroups}}
#' @export
loadGroupUsers <- function(groupId) {
  improveConnected()
  return(restGetAsDf("/groups/{groupId}/users",
                     urlParams = list(groupId = groupId)))
}

# ---------------------------------------------------------------------------
# Write operations: Groups
# ---------------------------------------------------------------------------

#' Create a Group
#'
#' Creates a new group in the improve repository.
#'
#' @param name Character. The name of the group to create.
#'
#' @returns A list with the created group details (including \code{id} and \code{name}).
#'   Returns \code{NULL} on failure.
#'
#' @examples
#' \dontrun{
#' group <- createGroup("Modelers")
#' }
#' @seealso \code{\link{deleteGroup}}, \code{\link{loadGroups}}, \code{\link{addGroupUser}}
#' @export
createGroup <- function(name) {
  improveEditable()
  data <- list(name = name)
  result <- authenticatedREST(
    "/groups",
    data = data,
    restType = "POST")
  if (is.null(result)) {
    log_warn("createGroup: failed to create group:", name)
    return(NULL)
  }
  cont <- httr::content(result)
  return(cont)
}

#' Delete a Group
#'
#' Deletes an existing group from the improve repository.
#'
#' @param groupId Character. The ID of the group to delete.
#'
#' @returns \code{TRUE} if the group was deleted successfully, \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' deleteGroup("some-group-id")
#' }
#' @seealso \code{\link{createGroup}}, \code{\link{loadGroups}}
#' @export
deleteGroup <- function(groupId) {
  improveEditable()
  result <- authenticatedREST(
    "/groups/{groupId}",
    urlParams = list(groupId = groupId),
    restType = "DELETE")
  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to delete group:", groupId)
  return(FALSE)
}

#' Add a User to a Group
#'
#' Adds an existing user to a group by creating a group membership.
#'
#' @param groupId Character. The ID of the group.
#' @param userId Character. The ID of the user to add.
#'
#' @returns A list of the user's group memberships after the addition,
#'   or \code{NULL} on failure.
#'
#' @examples
#' \dontrun{
#' addGroupUser("group-id", "user-id")
#' }
#' @seealso \code{\link{removeGroupUser}}, \code{\link{loadGroupUsers}}, \code{\link{createGroup}}
#' @export
addGroupUser <- function(groupId, userId) {
  improveEditable()
  group <- loadGroup(groupId)
  if (is.null(group)) {
    log_warn("cannot find group:", groupId)
    return(NULL)
  }
  data <- list(id = group$id, name = group$name)
  result <- authenticatedREST(
    "/users/{userId}/groupMemberships",
    urlParams = list(userId = userId),
    data = data,
    restType = "POST")
  if (is.null(result)) {
    log_warn("addGroupUser: failed to add user:", userId, "to group:", groupId)
    return(NULL)
  }
  cont <- httr::content(result)
  return(cont)
}

#' Remove a User from a Group
#'
#' Removes a user from an existing group.
#'
#' @param groupId Character. The ID of the group.
#' @param userId Character. The ID of the user to remove.
#'
#' @returns \code{TRUE} if the user was removed successfully, \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' removeGroupUser("group-id", "user-id")
#' }
#' @seealso \code{\link{addGroupUser}}, \code{\link{loadGroupUsers}}
#' @export
removeGroupUser <- function(groupId, userId) {
  improveEditable()
  result <- authenticatedREST(
    "/users/{userId}/groupMemberships/{groupId}",
    urlParams = list(userId = userId, groupId = groupId),
    restType = "DELETE")
  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to remove user from group:", userId, "from", groupId)
  return(FALSE)
}

#' Add a Subgroup to a Group
#'
#' Adds a group as a member of another group (group nesting).
#'
#' @param parentGroupId Character. The ID of the parent group.
#' @param childGroupId Character. The ID of the child group to add.
#'
#' @returns The server response content, or \code{NULL} on failure.
#'
#' @examples
#' \dontrun{
#' addSubgroup("parent-group-id", "child-group-id")
#' }
#' @seealso \code{\link{createGroup}}, \code{\link{loadGroupUsers}}
#' @export
addSubgroup <- function(parentGroupId, childGroupId) {
  improveEditable()
  # GroupEntry body per ics728 spec — requires 'id' field (the child group's GUID)
  data <- list(id = childGroupId)
  result <- authenticatedREST(
    "/groups/{groupId}/groups",
    urlParams = list(groupId = parentGroupId),
    data = data,
    restType = "POST")
  if (is.null(result)) {
    log_warn("addSubgroup: failed to add child group:", childGroupId, "to parent group:", parentGroupId)
    return(NULL)
  }
  cont <- httr::content(result)
  return(cont)
}

# ---------------------------------------------------------------------------
# Write operations: ACL
# ---------------------------------------------------------------------------

#' Set a Permission on a Resource
#'
#' Creates an ACL entry on a resource, granting specific rights to a user or group.
#' Uses POST to create a single new ACL entry. The \code{orderNr} and \code{rightsArea}
#' fields are required by the server DTO (ics769).
#'
#' @param ident Identifier of the resource. Can be a path, resource ID, or entity ID.
#' @param memberId Character. The ID of the user or group to grant permissions to.
#' @param visible Logical. Whether the member can see the resource exists.
#' @param read Logical. Whether the member can read the content.
#' @param modify Logical. Whether the member can modify the content.
#' @param changeRights Logical. Whether the member can change permissions.
#' @param inherit Logical. Whether this ACL entry inherits to child resources. Default \code{TRUE}.
#' @param orderNr Integer. The evaluation order of this ACL entry. Entries are
#'   evaluated from the lowest number upwards and \strong{the first entry that
#'   matches decides} - it is not a most-permissive-wins rule. A user who is in
#'   two groups with conflicting entries gets the right from whichever entry has
#'   the lower \code{orderNr}.
#'
#'   Must be unique per resource, and the server enforces it: an entry whose
#'   \code{orderNr} is already taken on that resource is refused, and this
#'   function returns \code{NULL}.
#'
#'   Measured against repository 4315 on 2026-09-18 (IMR-305); see
#'   \code{test-permissionOrder.R}. Default \code{1}.
#' @param rightsArea Integer. The rights area: 1 = Primary, 2 = Publish. Default \code{1}.
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#'
#' @returns A list with the created ACL entry details, or \code{NULL} on failure.
#'
#' @examples
#' \dontrun{
#' setResourcePermission("/Projects/MyProject", memberId = "group-id",
#'   visible = TRUE, read = TRUE, modify = TRUE, changeRights = FALSE)
#' }
#' @seealso \code{\link{removeResourcePermission}}, \code{\link{getResourcePermissions}},
#'   \code{\link{replaceResourcePermissions}}
#' @export
setResourcePermission <- function(ident, memberId, visible = TRUE, read = TRUE,
                                  modify = FALSE, changeRights = FALSE,
                                  inherit = TRUE, orderNr = 1L,
                                  rightsArea = 1L, from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find resource by ident:", ident)
    return(NULL)
  }
  data <- list(
    type = "accesscontrol",
    memberId = memberId,
    visible = visible,
    read = read,
    modify = modify,
    changeRights = changeRights,
    traversableContainer = visible,
    traversableLeaf = visible,
    inherit = inherit,
    orderNr = as.integer(orderNr),
    rightsArea = as.integer(rightsArea)
  )
  result <- authenticatedREST(
    "/resources/{resourceId}/acl",
    urlParams = list(resourceId = resource$resourceId),
    data = data,
    restType = "POST")
  if (is.null(result)) {
    log_warn("setResourcePermission: failed to set permission for member:", memberId, "on resource:", resource$resourceId)
    return(NULL)
  }
  cont <- httr::content(result)
  return(cont)
}

#' Replace All Permissions on a Resource (Bulk PUT)
#'
#' Atomically replaces the entire ACL for a resource. All entries in the list
#' replace the current ACL. Existing entries not present are deleted, entries
#' matched by \code{id} are updated, entries without an \code{id} are created.
#' The position in the list determines \code{orderNr} (starting at 1), and that
#' number is the evaluation order: the first entry in the list is evaluated
#' first and, where two entries disagree about the same user, decides.
#'
#' This endpoint is confirmed working on server 4.4.1-16 and is the most
#' reliable way to set permissions.
#'
#' @param ident Identifier of the resource. Can be a path, resource ID, or entity ID.
#' @param aclEntries A list of ACL entry lists. Each entry should contain at minimum:
#'   \code{memberId}, \code{visible}, \code{read}, \code{modify}, \code{changeRights},
#'   \code{inherit}, \code{rightsArea}. Optional: \code{id} (to update existing),
#'   \code{traversableContainer}, \code{traversableLeaf}.
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#'
#' @returns A list of the resulting ACL entries, or \code{NULL} on failure.
#'
#' @examples
#' \dontrun{
#' replaceResourcePermissions("/Projects/MyProject", list(
#'   list(memberId = "group1-id", visible = TRUE, read = TRUE,
#'        modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L),
#'   list(memberId = "group2-id", visible = TRUE, read = TRUE,
#'        modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
#' ))
#' }
#' @seealso \code{\link{setResourcePermission}}, \code{\link{getResourcePermissions}}
#' @export
replaceResourcePermissions <- function(ident, aclEntries, from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find resource by ident:", ident)
    return(NULL)
  }
  # Normalise each entry: fill in defaults, set orderNr from position
  nnd <- function(val, default) if (is.null(val)) default else val
  normalised <- lapply(seq_along(aclEntries), function(i) {
    e <- aclEntries[[i]]
    vis <- nnd(e$visible, TRUE)
    entry <- list(
      type         = nnd(e$type, "accesscontrol"),
      memberId     = e$memberId,
      visible      = vis,
      read         = nnd(e$read, TRUE),
      modify       = nnd(e$modify, FALSE),
      changeRights = nnd(e$changeRights, FALSE),
      traversableContainer = nnd(e$traversableContainer, vis),
      traversableLeaf      = nnd(e$traversableLeaf, vis),
      inherit      = nnd(e$inherit, TRUE),
      orderNr      = as.integer(i),
      rightsArea   = as.integer(nnd(e$rightsArea, 1L))
    )
    if (!is.null(e$id))         entry$id         <- e$id
    if (!is.null(e$resourceId)) entry$resourceId <- e$resourceId
    entry
  })
  result <- authenticatedREST(
    "/resources/{resourceId}/acl",
    urlParams = list(resourceId = resource$resourceId),
    data = normalised,
    restType = "PUT")
  if (is.null(result)) {
    log_warn("replaceResourcePermissions: failed to replace ACL for resource:", resource$resourceId)
    return(NULL)
  }
  cont <- httr::content(result)
  return(cont)
}

#' Update an Existing Permission on a Resource
#'
#' Updates an existing ACL entry on a resource. Unlike \code{setResourcePermission}
#' which creates a new entry (POST), this uses PUT to modify an existing ACL entry.
#' This is needed for changing rights ordering (\code{orderNr}).
#'
#' @param ident Identifier of the resource. Can be a path, resource ID, or entity ID.
#' @param aclId Character. The ID of the ACL entry to update (from \code{getResourcePermissions()}).
#' @param memberId Character. The ID of the user or group.
#' @param visible Logical. Whether the member can see the resource exists.
#' @param read Logical. Whether the member can read the content.
#' @param modify Logical. Whether the member can modify the content.
#' @param changeRights Logical. Whether the member can change permissions.
#' @param inherit Logical. Whether this ACL entry inherits to child resources. Default \code{TRUE}.
#' @param orderNr Integer or NULL. The evaluation order of this ACL entry.
#'   Entries are evaluated from the lowest number upwards and the first entry
#'   that matches decides. Must be unique per resource; the server refuses a
#'   duplicate. If \code{NULL}, the server default is used. See
#'   \code{\link{setResourcePermission}} for the measured behaviour.
#' @param rightsArea Integer. Rights area identifier on the server. Default \code{1L}.
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#'
#' @returns A list with the updated ACL entry details, or \code{NULL} on failure.
#'
#' @examples
#' \dontrun{
#' acl <- getResourcePermissions("/Projects/MyProject")
#' # Move an ACL entry to highest priority
#' updateResourcePermission("/Projects/MyProject",
#'   aclId = acl$id[1], memberId = acl$memberId[1],
#'   visible = TRUE, read = TRUE, modify = TRUE,
#'   changeRights = FALSE, orderNr = 1)
#' }
#' @seealso \code{\link{setResourcePermission}}, \code{\link{removeResourcePermission}}, \code{\link{getResourcePermissions}}
#' @export
updateResourcePermission <- function(ident, aclId, memberId, visible = TRUE, read = TRUE,
                                     modify = FALSE, changeRights = FALSE,
                                     inherit = TRUE, orderNr = NULL,
                                     rightsArea = 1L, from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find resource by ident:", ident)
    return(NULL)
  }
  data <- list(
    type = "accesscontrol",
    id = aclId,
    memberId = memberId,
    visible = visible,
    read = read,
    modify = modify,
    changeRights = changeRights,
    traversableContainer = visible,
    traversableLeaf = visible,
    inherit = inherit,
    rightsArea = as.integer(rightsArea)
  )
  if (!is.null(orderNr)) {
    data$orderNr <- as.integer(orderNr)
  }
  result <- authenticatedREST(
    "/resources/{resourceId}/acl/{aclId}",
    urlParams = list(resourceId = resource$resourceId, aclId = aclId),
    data = data,
    restType = "PUT")
  if (is.null(result)) {
    log_warn("updateResourcePermission: failed to update ACL entry:", aclId, "on resource:", resource$resourceId)
    return(NULL)
  }
  cont <- httr::content(result)
  return(cont)
}

#' Remove a Permission from a Resource
#'
#' Deletes a specific ACL entry from a resource.
#'
#' @param ident Identifier of the resource. Can be a path, resource ID, or entity ID.
#' @param aclId Character. The ID of the ACL entry to remove (from \code{getResourcePermissions()}).
#' @param from Root path for resolving relative paths. Defaults to \code{pwd()}.
#'
#' @returns \code{TRUE} if the entry was removed successfully, \code{FALSE} otherwise.
#'
#' @examples
#' \dontrun{
#' acl <- getResourcePermissions("/Projects/MyProject")
#' removeResourcePermission("/Projects/MyProject", acl$id[1])
#' }
#' @seealso \code{\link{setResourcePermission}}, \code{\link{getResourcePermissions}}
#' @export
removeResourcePermission <- function(ident, aclId, from = pwd()) {
  improveEditable()
  resource <- loadResource(ident, from)
  if (is.null(resource)) {
    log_warn("cannot find resource by ident:", ident)
    return(FALSE)
  }
  result <- authenticatedREST(
    "/resources/{resourceId}/acl/{aclId}",
    urlParams = list(resourceId = resource$resourceId, aclId = aclId),
    restType = "DELETE")
  if (!is.null(result)) {
    return(TRUE)
  }
  log_warn("Failed to remove resource permission:", aclId, "from resource:", resource$resourceId)
  return(FALSE)
}
