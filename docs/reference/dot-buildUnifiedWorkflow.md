# Build unified workflow data from all trees in a folder

Loads all steps from all trees and combines them into a single workflow,
exactly like a multi-tree workflow export. Cross-tree links are
naturally handled because collectInternalLinks resolves by path.

## Usage

``` r
.buildUnifiedWorkflow(trees, rootResource)
```

## Arguments

- trees:

  List of tree info (resourceId, relativePath, treeName).

- rootResource:

  Root folder resource.

## Value

List with workFlowDf and insideLinks.
