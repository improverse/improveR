# Filter outside links from workflow step data

Extracts external file dependencies (asLink=TRUE with no sourceStep)
from workflow steps. Used by both exportWorkflow and exportFolder.

## Usage

``` r
filterOutsideLinks(workflowDf)
```

## Arguments

- workflowDf:

  Data frame of workflow steps with remoteFiles column.

## Value

Data frame of outside links, or NULL if none found.
