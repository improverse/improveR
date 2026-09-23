# Create a workflow template environment specifically for import operations

This specialized version handles workflows during import when entity IDs
and resources don't exist in the new repository yet.

## Usage

``` r
createWorkflowTemplateForImport(workflow, internalLinksData = NULL)
```

## Arguments

- workflow:

  The workflow environment to template (during import)

- internalLinksData:

  Pre-loaded internal links data from import

## Value

An environment representing the workflow template for import
