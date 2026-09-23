# Reloads the Review Comments for a Review

Reloads the Review Comments for a Review

## Usage

``` r
refreshReviewComments(ident, from = pwd())

updateReviewComments(...)
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

A data frame of review comments, or `NULL` if none exist.

## References

ics1208
