# Replace All Permissions on a Resource (Bulk PUT)

Atomically replaces the entire ACL for a resource. All entries in the
list replace the current ACL. Existing entries not present are deleted,
entries matched by `id` are updated, entries without an `id` are
created. The position in the list determines `orderNr` (starting at 1),
and that number is the evaluation order: the first entry in the list is
evaluated first and, where two entries disagree about the same user,
decides.

## Usage

``` r
replaceResourcePermissions(ident, aclEntries, from = pwd())
```

## Arguments

- ident:

  Identifier of the resource. Can be a path, resource ID, or entity ID.

- aclEntries:

  A list of ACL entry lists. Each entry should contain at minimum:
  `memberId`, `visible`, `read`, `modify`, `changeRights`, `inherit`,
  `rightsArea`. Optional: `id` (to update existing),
  `traversableContainer`, `traversableLeaf`.

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A list of the resulting ACL entries, or `NULL` on failure.

## Details

This endpoint is confirmed working on server 4.4.1-16 and is the most
reliable way to set permissions.

## See also

[`setResourcePermission`](https://improverse.github.io/improveR/reference/setResourcePermission.md),
[`getResourcePermissions`](https://improverse.github.io/improveR/reference/getResourcePermissions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
replaceResourcePermissions("/Projects/MyProject", list(
  list(memberId = "group1-id", visible = TRUE, read = TRUE,
       modify = TRUE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L),
  list(memberId = "group2-id", visible = TRUE, read = TRUE,
       modify = FALSE, changeRights = FALSE, inherit = TRUE, rightsArea = 1L)
))
} # }
```
