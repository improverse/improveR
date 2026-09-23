# Add a Subgroup to a Group

Adds a group as a member of another group (group nesting).

## Usage

``` r
addSubgroup(parentGroupId, childGroupId)
```

## Arguments

- parentGroupId:

  Character. The ID of the parent group.

- childGroupId:

  Character. The ID of the child group to add.

## Value

The server response content, or `NULL` on failure.

## See also

[`createGroup`](https://improverse.github.io/improveR/reference/createGroup.md),
[`loadGroupUsers`](https://improverse.github.io/improveR/reference/loadGroupUsers.md)

## Examples

``` r
if (FALSE) { # \dontrun{
addSubgroup("parent-group-id", "child-group-id")
} # }
```
