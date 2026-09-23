# Add Review Entries

Adds one or more resources as entries to an existing review.

## Usage

``` r
createReviewEntry(
  ident,
  resourceIds,
  followLinks = FALSE,
  recursiveAdd = FALSE,
  from = pwd()
)
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- resourceIds:

  Character vector of resource IDs (UUIDs) to add as review entries.

- followLinks:

  Logical. If `TRUE`, link targets are also added. Defaults to `FALSE`.

- recursiveAdd:

  Logical. If `TRUE`, children of folders are added recursively.
  Defaults to `FALSE`.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of the review's current entries, or `NULL` on failure.

## References

ics1541
