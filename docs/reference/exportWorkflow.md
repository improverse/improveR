# Export a Workflow to a Portable Zip File

The `exportWorkflow()` function exports a workflow to a zip file that
can be imported into another repository. It preserves workflow
structure, step dependencies, file relationships, and includes all
necessary files for reconstruction in a different environment.

## Usage

``` r
exportWorkflow(workflow, workflowName, targetFolder = ".")
```

## Arguments

- workflow:

  Workflow environment object created by
  [`createWorkflow`](https://improverse.github.io/improveR/reference/createWorkflow.md)
  containing the steps and their relationships to export.

- workflowName:

  Character. Name for the exported workflow (without extension). This
  will be used as the zip filename and internal folder name.

- targetFolder:

  Character. Directory path where the zip file will be created. Defaults
  to current directory (".").

## Value

Character. Path to the created zip file.

## Details

The export process includes the following steps:

1.  Creates a workflow template from the provided workflow

2.  Creates a temporary export directory structure

3.  Generates workflow.json with complete step definitions

4.  Exports internal links between steps to internalLinks.json

5.  Downloads and includes external link files

6.  Clones each step's input and output files

7.  Creates mapping templates for links and tools

8.  Packages everything into a single zip file

## Export Structure

The exported zip file contains:

- `workflow.json` - Complete workflow metadata and step definitions

- `internalLinks.json` - Dependencies between workflow steps

- `links/` - Directory containing external file dependencies

- Step folders (named by handle) containing:

  - `inputFiles/` - Input files for the step

  - `outputFiles/` - Output files from the step execution

## Generated Mapping Templates

The function also creates template mapping files alongside the zip:

**LinkMapping.json** - Template for mapping external files:

    [
      {
        "key": "file-identifier",
        "ident": "",  # Fill in target repository resource ID
        "name": "original-filename",
        "hash": "file-hash-for-validation"
      }
    ]

**ToolMapping.json** - Template for tool configuration mapping:

    [
      {
        "key": "server:::tool:::instance:::gridTool",
        "runserverLabel": "",  # Fill in target server
        "toolLabel": "",       # Fill in target tool
        "toolInstance": "",    # Fill in target instance
        "gridTool": true/false
      }
    ]

## See also

[`importWorkflow`](https://improverse.github.io/improveR/reference/importWorkflow.md)
for importing exported workflows

## Examples

``` r
if (FALSE) { # \dontrun{
# Export to current directory
exportWorkflow(wf, "myWorkflow")
# Creates: myWorkflow.zip, myWorkflowLinkMapping.json, myWorkflowToolMapping.json

# Export to specific directory
exportWorkflow(wf, "analysis_v2", targetFolder = "/exports/workflows")

# The generated mapping files can be edited before import to:
# - Point links to existing resources in target repository
# - Map tools to different compute servers
# - Adapt to different environment configurations
} # }
```
