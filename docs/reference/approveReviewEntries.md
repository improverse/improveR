# Approve Review Entries

Approves one or more entries in a review. The review must be in
"Reviewing" state.

## Usage

``` r
approveReviewEntries(ident, reviewEntryIds, comment = "", from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- reviewEntryIds:

  Character vector of review entry IDs (UUIDs) to approve. These are the
  entry IDs (from `getReviewEntries()$id`), not resource IDs.

- comment:

  Character. Optional comment for the approval.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

`TRUE` if the entries were approved successfully, `FALSE` otherwise.

## References

ics1545
