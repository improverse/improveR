# Set a Permission on a Resource

Creates an ACL entry on a resource, granting specific rights to a user
or group. Uses POST to create a single new ACL entry. The `orderNr` and
`rightsArea` fields are required by the server DTO (ics769).

## Usage

``` r
setResourcePermission(
  ident,
  memberId,
  visible = TRUE,
  read = TRUE,
  modify = FALSE,
  changeRights = FALSE,
  inherit = TRUE,
  orderNr = 1L,
  rightsArea = 1L,
  from = pwd()
)
```

## Arguments

- ident:

  Identifier of the resource. Can be a path, resource ID, or entity ID.

- memberId:

  Character. The ID of the user or group to grant permissions to.

- visible:

  Logical. Whether the member can see the resource exists.

- read:

  Logical. Whether the member can read the content.

- modify:

  Logical. Whether the member can modify the content.

- changeRights:

  Logical. Whether the member can change permissions.

- inherit:

  Logical. Whether this ACL entry inherits to child resources. Default
  `TRUE`.

- orderNr:

  Integer. The evaluation order of this ACL entry. Entries are evaluated
  from the lowest number upwards and **the first entry that matches
  decides** - it is not a most-permissive-wins rule. A user who is in
  two groups with conflicting entries gets the right from whichever
  entry has the lower `orderNr`.

  Must be unique per resource, and the server enforces it: an entry
  whose `orderNr` is already taken on that resource is refused, and this
  function returns `NULL`.

  Measured against repository 4315 on 2026-09-18 (IMR-305); see
  `test-permissionOrder.R`. Default `1`.

- rightsArea:

  Integer. The rights area: 1 = Primary, 2 = Publish. Default `1`.

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A list with the created ACL entry details, or `NULL` on failure.

## See also

[`removeResourcePermission`](https://improverse.github.io/improveR/reference/removeResourcePermission.md),
[`getResourcePermissions`](https://improverse.github.io/improveR/reference/getResourcePermissions.md),
[`replaceResourcePermissions`](https://improverse.github.io/improveR/reference/replaceResourcePermissions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
setResourcePermission("/Projects/MyProject", memberId = "group-id",
  visible = TRUE, read = TRUE, modify = TRUE, changeRights = FALSE)
} # }
```
