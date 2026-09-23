# Get Effective User Rights for Resource

Determines the effective permissions (rights) for a specific user on a
given resource, taking into account group memberships and inherited
permissions.

## Usage

``` r
effectiveRights(ident, from = pwd(), memberId = NULL)
```

## Arguments

- ident:

  Identifier of the resource to check permissions for. Can be a path,
  resource ID, or entity ID.

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

- memberId:

  Character. The ID of the user/member to check permissions for. If
  `NULL` (default), checks permissions for the currently authenticated
  user.

## Value

A list containing the effective rights:

- `read` - Logical. Permission to view/download the resource.

- `modify` - Logical. Permission to update (for files) or upload (for
  folders).

- `delete` - Logical. Permission to delete the resource.

- `admin` - Logical. Full administrative rights.

Returns `NULL` if the resource or user cannot be found.

## See also

[`users`](https://improverse.github.io/improveR/reference/users.md),
[`whoami`](https://improverse.github.io/improveR/reference/whoami.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Check my rights on a folder
my_rights <- effectiveRights("/Projects/Analysis")
if (my_rights$modify) {
  message("I can write to this folder")
}

# Check rights for another user
user_id <- users()[1, "id"]
their_rights <- effectiveRights("/Projects/Analysis", memberId = user_id)
} # }
```
