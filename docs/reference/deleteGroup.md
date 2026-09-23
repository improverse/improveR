# Delete a Group

Deletes an existing group from the improve repository.

## Usage

``` r
deleteGroup(groupId)
```

## Arguments

- groupId:

  Character. The ID of the group to delete.

## Value

`TRUE` if the group was deleted successfully, `FALSE` otherwise.

## See also

[`createGroup`](https://improverse.github.io/improveR/reference/createGroup.md),
[`loadGroups`](https://improverse.github.io/improveR/reference/loadGroups.md)

## Examples

``` r
if (FALSE) { # \dontrun{
deleteGroup("some-group-id")
} # }
```
