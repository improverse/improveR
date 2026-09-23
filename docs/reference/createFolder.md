# Create Folder in improve Repository

Creates one or multiple folders in the repository at the specified
location.

## Usage

``` r
createFolder(targetIdent, folderName = "", comment = "Created by improveR")
```

## Arguments

- targetIdent:

  Identifier(s) of the parent location where the new folder(s) should be
  created. Can be a path, resource ID, or entity ID.

- folderName:

  Character. Name(s) of the new folder(s).

- comment:

  Character. Comment for the creation audit entry. Defaults to "Created
  by improveR".

## Value

A list containing the resource object(s) of the created folder(s). Each
resource object contains metadata like `resourceId`, `entityId`, `path`,
etc. Returns `NULL` on failure.

## References

ics1101

## See also

[`createFile`](https://improverse.github.io/improveR/reference/createFile.md),
[`createAnalysisTree`](https://improverse.github.io/improveR/reference/createAnalysisTree.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a folder in the repository root
createFolder(targetIdent = "/", folderName = "folderInRoot")

# Create a folder inside an existing folder by path
createFolder(
  targetIdent = "/improve-tutorial/",
  folderName = "folderInImproveTutorial"
)
} # }
```
