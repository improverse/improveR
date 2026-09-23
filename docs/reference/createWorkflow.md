# Create New Workflow Environment

Constructs a new workflow environment for managing, analyzing, and
executing workflow steps. The returned object encapsulates the workflow
state and exposes a public API for operations.

## Usage

``` r
createWorkflow()
```

## Value

An R environment with class "workflow" containing the public methods
listed in Details.

## Details

The returned workflow environment contains the following methods:

- [`df()`](https://rdrr.io/r/stats/Fdist.html): Returns a data frame of
  all steps in the workflow

- `changedAndOutdatedFiles(tree)`: Finds files that have changed or are
  outdated

- `createReexecutionPlan()`: Generates a plan to rerun only outdated
  steps

- `rerunChangedAndOutdated()`: Executes the re-execution plan

- `rerunAll()`: Reruns all steps regardless of status

- `executePlan(plan)`: Executes a specific plan object

Internal state and helper functions are encapsulated and not exposed.

## Examples

``` r
if (FALSE) { # \dontrun{
wf <- createWorkflow()
# Add steps, then:
wf$df()
wf$changedAndOutdatedFiles()
plan <- wf$createReexecutionPlan()
wf$executePlan(plan)
} # }
```
