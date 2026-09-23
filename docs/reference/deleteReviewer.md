# Removes a Reviewer from a Review

Removes a Reviewer from a Review

## Usage

``` r
deleteReviewer(ident, reviewerId, from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- reviewerId:

  Character. ID (UUID) of the reviewer to remove.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

`TRUE` if the reviewer was removed successfully, `FALSE` otherwise.

## References

ics369
