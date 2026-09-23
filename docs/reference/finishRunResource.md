# Wait for Step Execution to Finish

Blocks until a running step finishes on the improve server. Optionally
constrains completion to a specific runserver and tool combination.

## Usage

``` r
finishRunResource(
  ident,
  from = pwd(),
  runserverName = NULL,
  runserverToolName = NULL,
  timeout = defaultFinishRunTimeout()
)
```

## Arguments

- ident:

  A step identifier. Can be the step's path, resource (version) id, full
  entity (version) id, or short entity (version) id.

- from:

  Root path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

- runserverName:

  Optional runserver name. When set, completion is only acknowledged if
  the step finished on this runserver (must be paired with
  `runserverToolName`).

- runserverToolName:

  Optional tool name. When set, completion is only acknowledged if the
  step finished with this tool (must be paired with `runserverName`).

- timeout:

  Seconds to wait before giving up. Defaults to
  `IMPROVER_FINISHRUN_TIMEOUT` when that holds a positive number, and to
  600 otherwise. Pass `Inf` to wait forever. Before IMR-268 there was no
  timeout at all: the wait loop left only on `FINISHED`, so a run ending
  `FAILED` or `TERMINATED` was polled forever.

## Value

The step identifier, returned invisibly after the run completes.

## Details

The function polls the step status every 5 seconds until the run status
is `FINISHED`. If `runserverName` and `runserverToolName` are provided,
the function checks the most recent main process run and only returns
when the finished run matches the specified runserver tool.

This is typically used after
[`runStepResource`](https://improverse.github.io/improveR/reference/runStepResource.md)
when orchestrating sequential workflows.

## References

ics1140

## See also

[`runStepResource`](https://improverse.github.io/improveR/reference/runStepResource.md)
to start a step,
[`terminateStepResource`](https://improverse.github.io/improveR/reference/terminateStepResource.md)
to stop a running step

## Examples

``` r
if (FALSE) { # \dontrun{
# Run a step and wait for completion
runStepResource(ident = "/improve-tutorial/Modeling/Step 1")
finishRunResource(ident = "/improve-tutorial/Modeling/Step 1")
} # }
```
