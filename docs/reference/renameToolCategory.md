# Rename a Tool Category

Updates the name of an existing tool category.

## Usage

``` r
renameToolCategory(categoryId, name)
```

## Arguments

- categoryId:

  Character. The ID of the tool category to rename.

- name:

  Character. The new name.

## Value

A list with the updated category details, or `NULL` on failure.

## References

ccs33

## See also

[`loadToolCategories`](https://improverse.github.io/improveR/reference/loadToolCategories.md),
[`createToolCategory`](https://improverse.github.io/improveR/reference/createToolCategory.md)
