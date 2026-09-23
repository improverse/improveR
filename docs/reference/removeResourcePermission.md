# Remove a Permission from a Resource

Deletes a specific ACL entry from a resource.

## Usage

``` r
removeResourcePermission(ident, aclId, from = pwd())
```

## Arguments

- ident:

  Identifier of the resource. Can be a path, resource ID, or entity ID.

- aclId:

  Character. The ID of the ACL entry to remove (from
  [`getResourcePermissions()`](https://improverse.github.io/improveR/reference/getResourcePermissions.md)).

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

`TRUE` if the entry was removed successfully, `FALSE` otherwise.

## See also

[`setResourcePermission`](https://improverse.github.io/improveR/reference/setResourcePermission.md),
[`getResourcePermissions`](https://improverse.github.io/improveR/reference/getResourcePermissions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
acl <- getResourcePermissions("/Projects/MyProject")
removeResourcePermission("/Projects/MyProject", acl$id[1])
} # }
```
