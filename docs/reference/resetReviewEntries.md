# Reset Review Entry Approval State

Resets the approval state of one or more entries in a review. The review
must be in "Reviewing" state.

## Usage

``` r
resetReviewEntries(ident, reviewEntryIds, comment = "", from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- reviewEntryIds:

  Character vector of review entry IDs (UUIDs) to reset. These are the
  entry IDs (from `getReviewEntries()$id`), not resource IDs.

- comment:

  Character. Optional comment for the reset.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

`TRUE` if the entries were reset successfully, `FALSE` otherwise.

## References

ics1547
