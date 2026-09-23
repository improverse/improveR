# Get Review Entry Comments

Retrieves all comments for a specific review entry.

## Usage

``` r
getReviewEntryComments(ident, entryId, from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- entryId:

  Character. ID (UUID) of the review entry.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of review entry comments, or `NULL` if none exist.

## References

ics1543
