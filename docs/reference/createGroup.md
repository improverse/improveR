# Create a Group

Creates a new group in the improve repository.

## Usage

``` r
createGroup(name)
```

## Arguments

- name:

  Character. The name of the group to create.

## Value

A list with the created group details (including `id` and `name`).
Returns `NULL` on failure.

## See also

[`deleteGroup`](https://improverse.github.io/improveR/reference/deleteGroup.md),
[`loadGroups`](https://improverse.github.io/improveR/reference/loadGroups.md),
[`addGroupUser`](https://improverse.github.io/improveR/reference/addGroupUser.md)

## Examples

``` r
if (FALSE) { # \dontrun{
group <- createGroup("Modelers")
} # }
```
