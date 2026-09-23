# Creates a Comment for a Review Entry

Adds a comment to a specific entry within a review.

## Usage

``` r
createReviewEntryComment(ident, entryId, comment, from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- entryId:

  Character. ID (UUID) of the review entry.

- comment:

  Character. The comment text.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of the entry's current comments, or `NULL` on failure.

## References

ics1544
