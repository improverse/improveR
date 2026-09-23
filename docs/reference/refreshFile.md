# refreshFile

Retrieves the latest version of the file from the repository.

## Usage

``` r
refreshFile(
  ident,
  from = pwd(),
  filePath = ".",
  addIdToName = FALSE,
  linkInInventory = FALSE
)

updateFile(...)
```

## Arguments

- ident:

  id

- from:

  Used if a relative path is used.

- filePath:

  Local path where the file is stored (relative to rootPath).

- addIdToName:

  Logical; if TRUE the entity id is added to the filename.

- linkInInventory:

  Logical; if TRUE, creates a link to the resource in inventory.

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

The freshly downloaded file, in the same shape as
[`loadFile()`](https://improverse.github.io/improveR/reference/loadFile.md):
one row per file with the local path in `data`. Every file cache for
`ident` and the resource itself are dropped first, so the content comes
from the server.

## Details ident

There are multiple ways to describe the ident of a resource:

Absolute idents:

- resourceId: a UUID

- Data frame: uses the resourceId value of the data frame

- entityId: pointer to the latest version of a resource. Short and long
  entityIds are accepted'

- entityVersionId: pointer to a specific version of a resource. Short
  and long entityIds are accepted'

- path: the full path to a resource, always starting with /.

Relative idents:

- All relative idents are path based, they always have to start with ./
  or ../'

- Relative path without pwd: always starts from the return value of
  pwd()'

- Relative path and pwd as second argument: starts the relative path
  from the absolute ident that was handed over as second argument.
  Sometimes still called “from” but will be updated to pwd.

## References

ics1099
