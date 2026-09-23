# Import a Folder Hierarchy from a Zip File into a Repository Folder

Imports a folder hierarchy exported by
[`exportFolder`](https://improverse.github.io/improveR/reference/exportFolder.md),
recreating the folder structure, analysis trees with their workflow
steps, standalone files, and links in the target repository location.

## Usage

``` r
importFolder(
  folderFile,
  targetFolder,
  onConflict = c("skip", "overwrite", "error"),
  overwrite = FALSE
)
```

## Arguments

- folderFile:

  Character. Path to the folder export zip file created by
  [`exportFolder`](https://improverse.github.io/improveR/reference/exportFolder.md).

- targetFolder:

  Character. Path to the repository folder where the hierarchy will be
  imported. Must be an existing folder in the improve repository.

- onConflict:

  Character. Controls behavior when the target already contains
  resources with matching names. One of:

  `"skip"`

  :   (Default) Reuse existing folders and trees. For files, skip
      without updating content. Existing resources are never deleted.

  `"overwrite"`

  :   Reuse existing folders and trees. For files, update content with
      the exported version using
      [`updateFileContent`](https://improverse.github.io/improveR/reference/updateFileContent.md).

  `"error"`

  :   Stop with an error listing all conflicting resources before any
      changes are made.

- overwrite:

  Logical. Deprecated. Use `onConflict` instead. `TRUE` maps to
  `onConflict = "overwrite"`.

## Value

Invisibly returns the target folder resource.

## Details

The import process:

1.  Validates the export package (manifest.json, mapping files)

2.  Creates the folder structure top-down by path depth

3.  Creates analysis trees and imports their steps using the workflow
    import pattern

4.  Uploads standalone files from the `folderFiles/` directory

5.  Resolves folder-level links (internal links via id mapping, external
    links)

6.  Resolves cross-tree step links in a second pass

7.  Restores parent step relationships

## Mapping Files

Same as
[`importWorkflow`](https://improverse.github.io/improveR/reference/importWorkflow.md):
optional `<exportName>LinkMapping.json` and
`<exportName>ToolMapping.json` files can be placed alongside the zip.

## See also

[`exportFolder`](https://improverse.github.io/improveR/reference/exportFolder.md)
for creating folder export files,
[`importWorkflow`](https://improverse.github.io/improveR/reference/importWorkflow.md)
for importing single workflows

## Examples

``` r
if (FALSE) { # \dontrun{
importFolder("MyFolderExport.zip", "/Projects/TargetLocation")
importFolder("MyFolderExport.zip", "/Projects/Target", onConflict = "overwrite")
importFolder("MyFolderExport.zip", "/Projects/Target", onConflict = "error")
} # }
```
