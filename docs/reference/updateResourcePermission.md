# Update an Existing Permission on a Resource

Updates an existing ACL entry on a resource. Unlike
`setResourcePermission` which creates a new entry (POST), this uses PUT
to modify an existing ACL entry. This is needed for changing rights
ordering (`orderNr`).

## Usage

``` r
updateResourcePermission(
  ident,
  aclId,
  memberId,
  visible = TRUE,
  read = TRUE,
  modify = FALSE,
  changeRights = FALSE,
  inherit = TRUE,
  orderNr = NULL,
  rightsArea = 1L,
  from = pwd()
)
```

## Arguments

- ident:

  Identifier of the resource. Can be a path, resource ID, or entity ID.

- aclId:

  Character. The ID of the ACL entry to update (from
  [`getResourcePermissions()`](https://improverse.github.io/improveR/reference/getResourcePermissions.md)).

- memberId:

  Character. The ID of the user or group.

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

  Integer or NULL. The evaluation order of this ACL entry. Entries are
  evaluated from the lowest number upwards and the first entry that
  matches decides. Must be unique per resource; the server refuses a
  duplicate. If `NULL`, the server default is used. See
  [`setResourcePermission`](https://improverse.github.io/improveR/reference/setResourcePermission.md)
  for the measured behaviour.

- rightsArea:

  Integer. Rights area identifier on the server. Default `1L`.

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A list with the updated ACL entry details, or `NULL` on failure.

## See also

[`setResourcePermission`](https://improverse.github.io/improveR/reference/setResourcePermission.md),
[`removeResourcePermission`](https://improverse.github.io/improveR/reference/removeResourcePermission.md),
[`getResourcePermissions`](https://improverse.github.io/improveR/reference/getResourcePermissions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
acl <- getResourcePermissions("/Projects/MyProject")
# Move an ACL entry to highest priority
updateResourcePermission("/Projects/MyProject",
  aclId = acl$id[1], memberId = acl$memberId[1],
  visible = TRUE, read = TRUE, modify = TRUE,
  changeRights = FALSE, orderNr = 1)
} # }
```
