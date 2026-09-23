# Create a Tool in a Category

Creates a new tool in the specified category. Idempotent: if a tool with
the given name already exists in the category, it is returned instead.

## Usage

``` r
createTool(categoryId, toolName)
```

## Arguments

- categoryId:

  Character. The ID of the tool category.

- toolName:

  Character. The name of the tool to create.

## Value

A list with the tool details (id, name, categoryId), or `NULL` on
failure.

## References

ccs35

## See also

[`loadToolsForCategory`](https://improverse.github.io/improveR/reference/loadToolsForCategory.md),
[`createToolCategory`](https://improverse.github.io/improveR/reference/createToolCategory.md)
