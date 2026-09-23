# Load Favorite Children

Retrieves the child resources of a favorites folder. If `parentId` is
`NULL`, returns the top-level favorite resources.

## Usage

``` r
loadFavoriteChildren(parentId = NULL)
```

## Arguments

- parentId:

  Optional resource ID of a favorites folder. If `NULL`, retrieves
  top-level favorites.

## Value

A data frame of favorite child resources, or `NULL` if none exist.

## References

ics1801, ics1802
