# Collect inside (internal) links from workflow step data

Extracts links between workflow steps (asLink=TRUE with sourceStep set).
Used by both exportWorkflow and exportFolder.

## Usage

``` r
collectInsideLinks(workflowDf, workflowTemplate = NULL)
```

## Arguments

- workflowDf:

  Data frame of workflow steps (from template\$df()).

- workflowTemplate:

  The workflow template environment (needed for fallback resolution).

## Value

Data frame of internal links, or NULL if none found.
