# Import a Workflow from a zip File into a Repository Folder

The `importWorkflow()` function imports a workflow from a specified zip
file into a given repository folder. It reconstructs the workflow
structure, maintains step dependencies, handles file links, and applies
optional tool and link mappings for environment portability.

## Usage

``` r
importWorkflow(workflowFile, importRepoFolder)
```

## Arguments

- workflowFile:

  Character. Path to the workflow zip file to import. The zip file
  should contain a single folder with:

  - `workflow.json` - Required. Contains workflow metadata and step
    definitions

  - `internalLinks.json` - Optional. Defines dependencies between
    workflow steps

  - `links/` - Optional directory containing external file dependencies

  - Step folders named by handle containing `inputFiles/` and
    `outputFiles/`

- importRepoFolder:

  Character. Path to the repository folder where the workflow will be
  imported. Must be an existing folder in the improve repository.

## Value

None. The function is called for its side effects. Steps are created in
the target repository with preserved dependencies and file
relationships.

## Details

The import process includes the following steps:

1.  Extracts the workflow zip file to a temporary directory

2.  Validates the workflow structure and required files

3.  Creates workflow and step template environments

4.  Reconstructs step dependencies from internalLinks.json

5.  Uploads external link files to the repository

6.  Applies optional link mappings from `<workflowName>LinkMapping.json`

7.  Applies optional tool mappings from `<workflowName>ToolMapping.json`

8.  Creates steps in dependencies order

9.  Uploads input and output files for each step

## Mapping Files

Two optional JSON mapping files can be placed alongside the workflow
zip:

**LinkMapping.json** - Maps external file references to existing
repository resources:

    [
      {
        "key": "unique-file-identifier",
        "ident": "repository-resource-id",
        "name": "optional-display-name"
      }
    ]

**ToolMapping.json** - Maps tool configurations between environments:

    [
      {
        "key": "sourceServer:::sourceTool:::sourceInstance:::gridTool",
        "runserverLabel": "targetServer",
        "toolLabel": "targetTool",
        "toolInstance": "targetInstance",
        "gridTool": true/false
      }
    ]

## See also

[`exportWorkflow`](https://improverse.github.io/improveR/reference/exportWorkflow.md)
for creating workflow export files

## Examples

``` r
if (FALSE) { # \dontrun{
# Basic import
importWorkflow("myWorkflow.zip", "/Projects/TargetFolder")

# Import with link mapping
# Create myWorkflowLinkMapping.json in same directory as zip
# Then import:
importWorkflow("myWorkflow.zip", "/Projects/TargetFolder")

# Import with tool mapping for different environment
# Create myWorkflowToolMapping.json for server configuration
importWorkflow("myWorkflow.zip", "/Projects/Production")
} # }
```
