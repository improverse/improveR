# Remove a User from a Group

Removes a user from an existing group.

## Usage

``` r
removeGroupUser(groupId, userId)
```

## Arguments

- groupId:

  Character. The ID of the group.

- userId:

  Character. The ID of the user to remove.

## Value

`TRUE` if the user was removed successfully, `FALSE` otherwise.

## See also

[`addGroupUser`](https://improverse.github.io/improveR/reference/addGroupUser.md),
[`loadGroupUsers`](https://improverse.github.io/improveR/reference/loadGroupUsers.md)

## Examples

``` r
if (FALSE) { # \dontrun{
removeGroupUser("group-id", "user-id")
} # }
```
