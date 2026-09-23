# Export a Folder Hierarchy to a Portable Zip File

Exports an entire folder hierarchy — including subfolders, analysis
trees (with their workflow steps), standalone files, and links — to a
zip file that can be imported into another repository. Cross-tree links
and folder-level links are preserved.

## Usage

``` r
exportFolder(folderIdent, exportName, targetFolder = ".")
```

## Arguments

- folderIdent:

  Identifier of the folder to export. Can be a path, resource ID, or
  entity ID.

- exportName:

  Character. Name for the exported folder package (without extension).
  Used as the zip filename and internal folder name.

- targetFolder:

  Character. Directory path where the zip file and mapping templates
  will be created. Defaults to current directory (".").

## Value

Invisibly returns the path to the created zip file.

## Details

The export process:

1.  Recursively traverses the folder hierarchy via
    [`loadChildResources()`](https://improverse.github.io/improveR/reference/loadChildResources.md)

2.  Classifies each child: Folder, File, Analysis Tree, Link, ExtLink

3.  For trees: extracts steps via
    [`getWorkflow()`](https://improverse.github.io/improveR/reference/getWorkflow.md)
    /
    [`getStep()`](https://improverse.github.io/improveR/reference/getStep.md)
    into a unified step data frame (same format as workflow export)

4.  Collects internal links across ALL trees (cross-tree links
    supported)

5.  Downloads step files via
    [`cloneCli()`](https://improverse.github.io/improveR/reference/cloneCli.md);
    standalone folder files via
    [`getData()`](https://improverse.github.io/improveR/reference/getData.md)

6.  Generates manifest.json, workflow.json-compatible step data, and
    mapping templates

7.  Packages everything into a single zip file

## Export Structure

    <exportName>/
      manifest.json              # Folder structure + tree/link metadata
      workflow.json              # Unified step data (same format as exportWorkflow)
      internalLinks.json         # Step-to-step links across all trees
      links/                     # External dependency files
      <stepHandle>/
        inputFiles/
        outputFiles/
      folderFiles/               # Standalone files from folders
        <resourceId>_<filename>

## Generated Mapping Templates

Same as
[`exportWorkflow`](https://improverse.github.io/improveR/reference/exportWorkflow.md):
`<exportName>LinkMapping.json` and `<exportName>ToolMapping.json` are
created alongside the zip.

## See also

[`importFolder`](https://improverse.github.io/improveR/reference/importFolder.md)
for importing exported folders,
[`exportWorkflow`](https://improverse.github.io/improveR/reference/exportWorkflow.md)
for exporting single workflows

## Examples

``` r
if (FALSE) { # \dontrun{
exportFolder("/Projects/MyFolder", "MyFolderExport")
# Creates: MyFolderExport.zip, MyFolderExportLinkMapping.json,
#          MyFolderExportToolMapping.json
} # }
```
