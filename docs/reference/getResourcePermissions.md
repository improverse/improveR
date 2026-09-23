# Get Resource ACL Entries

Retrieves all Access Control List (ACL) entries for a given resource.

## Usage

``` r
getResourcePermissions(ident, from = pwd())
```

## Arguments

- ident:

  Identifier of the resource. Can be a path, resource ID, or entity ID.

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of ACL entries with columns such as id, resourceId,
memberId, visible, read, modify, changeRights, traversableContainer,
traversableLeaf, inherit, rightsArea, orderNr. Returns `NULL` if the
resource cannot be found. Returns `NULL` if there are no ACL entries.

## See also

[`effectiveRights`](https://improverse.github.io/improveR/reference/effectiveRights.md),
[`effectiveUserPermissions`](https://improverse.github.io/improveR/reference/effectiveUserPermissions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
acl <- getResourcePermissions("/Projects/MyProject")
} # }
```
