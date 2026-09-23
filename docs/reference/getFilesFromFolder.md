# Retrieve a List of Files from a Folder

Retrieve a List of Files from a Folder

## Usage

``` r
getFilesFromFolder(ident, from = pwd(), filePattern = "", recurse = F)
```

## Arguments

- ident:

  Path, resource, or entity ID of the folder (one folder at a time).

- from:

  Used for relative paths. By default, pwd is used (initiated with the
  step that started improveR).

- filePattern:

  Filter applied to the file name (example: `*.r`).

- recurse:

  If TRUE, nested folders are also parsed. Defaults to FALSE.

## Value

A data frame of the child resources of node type `File` or `Link`,
distinct by `resourceId`, in the shape
[`loadChildResources()`](https://improverse.github.io/improveR/reference/loadChildResources.md)
returns. `NULL` in three cases that are worth telling apart: `ident`
does not resolve to exactly one resource, the resource is not a
container (folder, step or analysis tree), or `filePattern` is not a
usable regular expression. An empty folder also yields `NULL`, not a
zero-row data frame.

## References

ics1141
