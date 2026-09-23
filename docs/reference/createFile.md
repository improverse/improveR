# Create File in improve Repository

Creates a file resource in the repository, optionally uploading content
from a local file.

## Usage

``` r
createFile(
  targetIdent,
  fileName = "",
  localPath = "",
  comment = "Created by improveR"
)
```

## Arguments

- targetIdent:

  Identifier(s) of the parent folder(s) where the file should be
  created. Can be a path, resource ID, or entity ID.

- fileName:

  Character. Name of the file to be created. If empty and `localPath` is
  provided, the basename of `localPath` is used.

- localPath:

  Character. Path to a local file whose content will be uploaded. If
  empty, an empty file resource is created (metadata only).

- comment:

  Character. Comment for the creation audit entry. Defaults to "Created
  by improveR".

## Value

A list containing the resource object(s) of the created file(s). Returns
`NULL` on failure.

## References

ics1102

## See also

[`createFolder`](https://improverse.github.io/improveR/reference/createFolder.md),
[`updateFileContent`](https://improverse.github.io/improveR/reference/updateFileContent.md)
