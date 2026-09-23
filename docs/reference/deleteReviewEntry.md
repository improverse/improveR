# Delete Review Entries

Removes specific entries from an existing review by their review entry
IDs. Use `getReviewEntries()$id` to get the entry IDs.

## Usage

``` r
deleteReviewEntry(ident, reviewEntryIds, comment = "", from = pwd())
```

## Arguments

- ident:

  Identifier of the review. Can be a path, resource ID, entity ID, or a
  data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- reviewEntryIds:

  Character vector of review entry IDs (UUIDs) to remove. These are the
  entry IDs (from `getReviewEntries()$id`), not resource IDs.

- comment:

  Character. Optional comment for the deletion. Defaults to empty
  string.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

`TRUE` if the entries were removed successfully, `FALSE` otherwise.

## References

ics1542
