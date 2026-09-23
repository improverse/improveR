# Get Effective Permissions for All Users

Retrieves the effective permissions for all users on a given resource,
taking into account group memberships and inherited permissions.

## Usage

``` r
effectiveUserPermissions(ident, from = pwd())
```

## Arguments

- ident:

  Identifier of the resource. Can be a path, resource ID, or entity ID.

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of all users with their effective rights on the resource.
Returns `NULL` if the resource cannot be found. Returns `NULL` if there
are no entries.

## See also

[`effectiveRights`](https://improverse.github.io/improveR/reference/effectiveRights.md),
[`getResourcePermissions`](https://improverse.github.io/improveR/reference/getResourcePermissions.md)

## Examples

``` r
if (FALSE) { # \dontrun{
perms <- effectiveUserPermissions("/Projects/MyProject")
} # }
```
