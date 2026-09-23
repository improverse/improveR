# Permissions & Access Control

## Overview

Access control in improve is group-based. You create groups, add users
to them, then assign permissions to groups on resources. Permissions can
inherit down the folder hierarchy.

## Groups

### Create and manage groups

``` r
library(improveR)

# Create a group
group <- createGroup("Modelers")

# Add users to the group
allUsers <- users()
addGroupUser(group$id, allUsers$id[allUsers$username == "jane"])
addGroupUser(group$id, allUsers$id[allUsers$username == "bob"])

# Check members
members <- loadGroupUsers(group$id)

# Remove a member
removeGroupUser(group$id, allUsers$id[allUsers$username == "bob"])

# Delete a group
deleteGroup(group$id)
```

## Setting permissions

Permissions are set on resources (folders, files, analysis trees, steps)
for groups. Each permission entry specifies:

| Field          | Description                   |
|----------------|-------------------------------|
| `visible`      | Can see the resource          |
| `read`         | Can read content              |
| `modify`       | Can modify content            |
| `changeRights` | Can change permissions        |
| `inherit`      | Propagates to child resources |

### Single permission

``` r
# Grant read access to a folder
setResourcePermission(folder,
  memberId = group$id,
  visible = TRUE, read = TRUE,
  modify = FALSE, changeRights = FALSE)
```

### Multiple permissions at once

``` r
# Set multiple permissions (replaces all existing)
replaceResourcePermissions(folder, list(
  list(memberId = adminGroup$id,
       visible = TRUE, read = TRUE, modify = TRUE,
       changeRights = TRUE, inherit = TRUE, rightsArea = 1L),
  list(memberId = readonlyGroup$id,
       visible = TRUE, read = TRUE, modify = FALSE,
       changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
))
```

### Update and remove

``` r
# Read current permissions
acl <- getResourcePermissions(folder)

# Update a specific entry
updateResourcePermission(folder,
  aclEntryId = acl$id[1],
  modify = TRUE)  # grant modify to first entry

# Remove a specific entry
removeResourcePermission(folder, acl$id[2])
```

## Order of evaluation

Every entry carries an `orderNr`. Entries are evaluated from the
**lowest number upwards, and the first entry that matches decides**.

This is the rule that catches people out: it is *not*
most-permissive-wins, and it is not least-permissive-wins either.
Whichever matching entry has the lower `orderNr` is the one that
applies, and the rest are never consulted.

So a user who belongs to two groups with conflicting entries gets the
right from whichever entry sits lower in the order:

``` r
# Reviewers may read. Contractors may not see the folder at all.
replaceResourcePermissions(folder, list(
  list(memberId = reviewers$id,   orderNr = 1L,
       visible = TRUE,  read = TRUE,  modify = FALSE,
       changeRights = FALSE, inherit = TRUE, rightsArea = 1L),
  list(memberId = contractors$id, orderNr = 2L,
       visible = FALSE, read = FALSE, modify = FALSE,
       changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
))

# A user in BOTH groups can read: orderNr 1 matches first and decides.
# Swap the two orderNr values and the same user sees nothing at all -
# the entries are identical, only their order changed.
```

If you want a denial to win, give it the lower `orderNr`. Adding a more
restrictive entry with a higher number has no effect on anyone already
matched by an earlier one.

### orderNr must be unique per resource

The server enforces it. An entry whose `orderNr` is already taken on
that resource is refused, and the call answers with `NULL` rather than
raising:

``` r
setResourcePermission(folder, memberId = group$id, orderNr = 1L, read = TRUE)
result <- setResourcePermission(folder, memberId = other$id, orderNr = 1L, read = TRUE)
is.null(result)   # TRUE - orderNr 1 was already in use, nothing was written
```

Check the return value. A `NULL` here means the permission was not set.

Both behaviours were measured against a live repository rather than
inferred from the API documentation; see `test-permissionOrder.R` in the
package tests.

## Inheritance

When `inherit = TRUE`, child folders and files receive the same
permissions.

``` r
# Parent folder: full access, inherited
replaceResourcePermissions(parentFolder, list(
  list(memberId = group$id,
       visible = TRUE, read = TRUE, modify = TRUE,
       changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
))

# Child folder automatically gets the same permissions
childPerms <- effectiveUserPermissions(childFolder)
```

When `inherit = FALSE`, the permission applies only to the resource
itself:

``` r
# Read-only at root level, not inherited to children
replaceResourcePermissions(rootFolder, list(
  list(memberId = group$id,
       visible = TRUE, read = TRUE, modify = FALSE,
       changeRights = FALSE, inherit = FALSE, rightsArea = 1L)
))

# Children do NOT get this permission
```

## Checking effective permissions

``` r
# What permissions does a specific user have?
rights <- effectiveRights(folder, memberId = userId)
rights$read    # TRUE/FALSE
rights$modify  # TRUE/FALSE
```

[`effectiveUserPermissions()`](https://improverse.github.io/improveR/reference/effectiveUserPermissions.md)
answers for **every user in the repository**, not only for those named
in an entry on this resource. On a production server that is thousands
of rows, in no particular order, so filter for the user you mean rather
than reading off the top:

``` r
perms <- effectiveUserPermissions(folder)
nrow(perms)                                  # every user, not every ACL entry

# The row for one user:
perms[perms$user.username == "someUser", ]
```

Taking `perms[1, ]` gives an arbitrary user’s rights and looks like an
answer.
