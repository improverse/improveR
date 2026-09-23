# Get Group Details

Retrieves details for a specific group by its ID.

## Usage

``` r
loadGroup(groupId)
```

## Arguments

- groupId:

  Character. The ID of the group.

## Value

A list with group details. Returns `NULL` if the group cannot be found.

## See also

[`loadGroups`](https://improverse.github.io/improveR/reference/loadGroups.md),
[`loadGroupUsers`](https://improverse.github.io/improveR/reference/loadGroupUsers.md)

## Examples

``` r
if (FALSE) { # \dontrun{
group <- loadGroup("some-group-id")
} # }
```
