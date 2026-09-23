# Load File

Loads a file by its resourceId, entity ID, or entity version ID. Uses
caching. The results are returned as a data frame or a list of data
frames. The dates are converted to POSIX dates with the
convertImproveTimestampToPosix function. resourceId can be a list.

## Usage

``` r
loadFile(
  ident,
  from = pwd(),
  filePath = ".",
  addIdToName = FALSE,
  linkInInventory = FALSE
)
```

## Arguments

- ident:

  the resource id or the entity id of the resource

- from:

  used if a relative path is used

- filePath:

  local Path where the file should be stored, relative to rootPath,
  normally wd

- addIdToName:

  logical, if the entityId should be added to the filename

- linkInInventory:

  logical, if TRUE a link to the resource is created in the inventory

## Value

A data frame with one row per file, carrying the resource fields and the
local path of the downloaded content in `data`. Versions and
non-versions are fetched separately and returned in one frame. `NULL`
when `ident` resolves to no resource.

## References

ics1099

## See also

[`convertImproveTimestampToPosix()`](https://improverse.github.io/improveR/reference/convertImproveTimestampToPosix.md)
