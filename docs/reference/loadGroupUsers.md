# Get Group Members

Retrieves all users belonging to a specific group.

## Usage

``` r
loadGroupUsers(groupId)
```

## Arguments

- groupId:

  Character. The ID of the group.

## Value

A data frame of users in the group. Returns `NULL` if the group cannot
be found. Returns `NULL` if the group has no members.

## See also

[`loadGroup`](https://improverse.github.io/improveR/reference/loadGroup.md),
[`loadGroups`](https://improverse.github.io/improveR/reference/loadGroups.md)

## Examples

``` r
if (FALSE) { # \dontrun{
members <- loadGroupUsers("some-group-id")
} # }
```
