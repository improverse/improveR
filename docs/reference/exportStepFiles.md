# Clone steps and organize files into inputFiles/outputFiles

Downloads step contents via cloneCli and separates files into input and
output directories. Used by both exportWorkflow and exportFolder.

## Usage

``` r
exportStepFiles(workflowDf, exportDir, insideLinks, outsideLinks, inputs)
```

## Arguments

- workflowDf:

  Data frame of workflow steps.

- exportDir:

  Path to export directory.

- insideLinks:

  Data frame of internal links (or NULL).

- outsideLinks:

  Data frame of outside links (with stepHandle, version, name columns,
  or NULL).

- inputs:

  Data frame of input files (with stepHandle, name columns, or NULL).

## Value

Data frame with taskDir and handle columns for each cloned step.
