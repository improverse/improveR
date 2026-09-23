# Create Favorite Folder

Creates a new folder inside the user's favorites collection for
organising favorite resources.

## Usage

``` r
createFavoriteFolder(name, parentId = NULL, comment = "")
```

## Arguments

- name:

  Name of the new favorites folder.

- parentId:

  Optional resource ID of the parent favorites folder. If `NULL`, the
  folder is created at the top level.

- comment:

  Optional comment for the operation.

## Value

A data frame with the created favorites folder details, or `NULL` on
failure.

## References

ics1805, ics1806
