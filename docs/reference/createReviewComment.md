# Creates a Comment for a Review

Adds a comment to a review. The review must be in the `"Reviewing"`
state.

## Usage

``` r
createReviewComment(ident, resourceIdent, comment, commentType, from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- resourceIdent:

  Identifier of the resource the comment relates to. Can be a path,
  resource ID, entity ID, or a data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- comment:

  Character. The comment text.

- commentType:

  Character. Type of the comment.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of the review's current comments, or `NULL` on failure.

## References

ics1536
