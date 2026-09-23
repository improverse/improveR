# Add Favorite Link

Creates a link to an existing resource inside the user's favorites
collection.

## Usage

``` r
addFavoriteLink(targetIdent, name, parentId = NULL, comment = "", from = pwd())
```

## Arguments

- targetIdent:

  Identifier of the resource to add as a favorite. Can be a path,
  resource ID, entity ID, or a data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- name:

  Display name for the favorite link.

- parentId:

  Optional resource ID of the parent favorites folder. If `NULL`, the
  link is created at the top level.

- comment:

  Optional comment for the operation.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame with the created favorite link details, or `NULL` on
failure.

## References

ics1803, ics1804
