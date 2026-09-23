# Add a User to a Group

Adds an existing user to a group by creating a group membership.

## Usage

``` r
addGroupUser(groupId, userId)
```

## Arguments

- groupId:

  Character. The ID of the group.

- userId:

  Character. The ID of the user to add.

## Value

A list of the user's group memberships after the addition, or `NULL` on
failure.

## See also

[`removeGroupUser`](https://improverse.github.io/improveR/reference/removeGroupUser.md),
[`loadGroupUsers`](https://improverse.github.io/improveR/reference/loadGroupUsers.md),
[`createGroup`](https://improverse.github.io/improveR/reference/createGroup.md)

## Examples

``` r
if (FALSE) { # \dontrun{
addGroupUser("group-id", "user-id")
} # }
```
