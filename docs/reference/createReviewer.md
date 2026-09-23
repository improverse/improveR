# Adds a User as Reviewer to a Review

Adds a User as Reviewer to a Review

## Usage

``` r
createReviewer(ident, userId, username, from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- userId:

  Character. ID (UUID) of the user to add as reviewer.

- username:

  Character. Username of the user to add as reviewer.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of the review's current reviewers, or `NULL` on failure.

## References

ics368
