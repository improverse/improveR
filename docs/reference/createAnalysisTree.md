# Create Analysis Tree in improve Repository

Creates an Analysis Tree, which is a specialized folder structure for
organizing pharmaceutical analysis workflows.

## Usage

``` r
createAnalysisTree(targetIdent, treeName = "", comment = "Created by improveR")
```

## Arguments

- targetIdent:

  Identifier of the parent folder where the analysis tree should be
  created.

- treeName:

  Character. Name of the new analysis tree.

- comment:

  Character. Comment for the creation audit entry. Defaults to "Created
  by improveR".

## Value

A list containing the resource object of the created analysis tree.
Returns `NULL` on failure.

## References

ics1103

## See also

[`createFolder`](https://improverse.github.io/improveR/reference/createFolder.md),
[`createWorkflow`](https://improverse.github.io/improveR/reference/createWorkflow.md)
