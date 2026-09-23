# Get Review Comments

Retrieves all comments for a given review.

## Usage

``` r
getReviewComments(ident, from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of review comments, or `NULL` if none exist.

## References

ics1208
