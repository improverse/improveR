# Run a Step Resource

Initiates the execution of a step on the server. This function triggers
asynchronous step execution, meaning the step's code and processes begin
running on the improve platform without blocking your R session. Control
returns immediately, allowing you to continue working or chain
additional commands while the step runs in the background.

## Usage

``` r
runStepResource(ident, resetInventory = FALSE, filesToKeep = NULL)
```

## Arguments

- ident:

  A step identifier. Can be the step's path, resource (version) id, full
  entity (version) id, or short entity (version) id.

- resetInventory:

  Logical. If `TRUE`, the server resets the step's inventory before
  running. Defaults to `FALSE` (backwards compatible).

- filesToKeep:

  Character vector. Resource GUIDs of inventory items to keep when
  `resetInventory = TRUE`. Ignored when `resetInventory` is `FALSE`.
  Defaults to `NULL` (keep nothing / reset all).

## Value

The step identifier, returned invisibly.

## Details

The function performs a quick validation to ensure the repository is in
an editable state, confirms the step exists, and then sends a command to
the server to begin execution. The actual step execution happens
asynchronously on the server, managed by the improve platform's
execution infrastructure.

This function is commonly used in workflow orchestration scenarios where
multiple steps need to be executed sequentially or in parallel. For
sequential workflows, combine with
[`finishRunResource`](https://improverse.github.io/improveR/reference/finishRunResource.md)
to wait for completion before proceeding.

## References

ics1140

## See also

[`finishRunResource`](https://improverse.github.io/improveR/reference/finishRunResource.md)
to wait for step completion

## Examples

``` r
if (FALSE) { # \dontrun{
# Execute a single step
runStepResource(ident = "/improve-tutorial/Modeling/Step 1")
} # }
```
