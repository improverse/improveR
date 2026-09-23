# Creates a New Review

Creates a review resource under the specified parent folder, containing
the given resources and assigned to the given reviewers.

## Usage

``` r
createReview(
  name,
  parentIdent,
  comment,
  templateId,
  resourceIds,
  reviewerIds,
  dueDate,
  from = pwd()
)
```

## Arguments

- name:

  Character. Name of the review.

- parentIdent:

  Identifier of the parent folder. Can be a path, resource ID, entity
  ID, or a data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- comment:

  Character. Review comment.

- templateId:

  Character. ID (UUID) of the review template.

- resourceIds:

  Character vector of resource IDs (UUIDs) to include in the review.

- reviewerIds:

  Character vector of reviewer user IDs (UUIDs).

- dueDate:

  Character. Due date in `yyyy-mm-dd` format.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame with the created review resource, or `NULL` on failure.

## References

ics1527
