# Unloads the Review Comments for a Review

Unloads the Review Comments for a Review

## Usage

``` r
unloadReviewComments(ident, from = pwd())
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

No meaningful value - called for its side effect of dropping the
comments of the review from the cache, so that the next load reads the
server. The value handed back by the internal cache removal is an
implementation detail and must not be relied on.

## References

ics1208
