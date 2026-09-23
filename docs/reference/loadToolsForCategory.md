# Load Tools for a Category

Retrieves all tools registered under a specific tool category.

## Usage

``` r
loadToolsForCategory(categoryId)
```

## Arguments

- categoryId:

  Character. The ID of the tool category.

## Value

A data frame of tools with columns such as id, name, categoryId. Returns
`NULL` if no tools exist in the category.

## References

ics1230

## See also

[`loadToolCategories`](https://improverse.github.io/improveR/reference/loadToolCategories.md),
[`createTool`](https://improverse.github.io/improveR/reference/createTool.md)
