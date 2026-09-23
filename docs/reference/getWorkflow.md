# Load Workflow Environment

Loads a complete workflow environment by reading a tree structure (e.g.,
an Analysis Tree) and initializing step environments for all contained
steps.

## Usage

``` r
getWorkflow(ident, from = pwd(), includeSelf = F)
```

## Arguments

- ident:

  Identifier of the tree or container resource.

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

- includeSelf:

  Logical. If `TRUE`, includes the step identified by
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md) if
  it falls within the loaded tree. Defaults to `FALSE` to prevent
  self-referential issues during execution.

## Value

A workflow environment containing all loaded steps. See
[`createWorkflow`](https://improverse.github.io/improveR/reference/createWorkflow.md)
for details on the workflow environment structure and available methods.

## See also

[`createWorkflow`](https://improverse.github.io/improveR/reference/createWorkflow.md),
[`getStep`](https://improverse.github.io/improveR/reference/getStep.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load workflow from an analysis tree
wf <- getWorkflow("/Projects/Analysis/Tree1")

# List all steps in the loaded workflow
wf$df()
} # }
```
