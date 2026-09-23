# Terminate a Running Step Resource

Stops the execution of a currently running step on the server. This
function sends a termination request to the improve platform, which will
halt the step's processes and mark it as terminated. This is useful when
you need to cancel a long-running step that is no longer needed or when
troubleshooting workflow execution issues.

## Usage

``` r
terminateStepResource(ident, verbose = FALSE)
```

## Arguments

- ident:

  A step identifier. Can be the step's path, resource (version) id, full
  entity (version) id, or short entity (version) id.

- verbose:

  Logical. If `TRUE`, prints verbose output with HTTP status messages
  during termination (200: Step terminated, 304: Could not start run,
  500: Runserver not reachable) and status confirmation messages.
  Defaults to `FALSE`.

## Value

Logical value returned invisibly: `TRUE` if the termination request was
successful (HTTP status 200), `FALSE` otherwise.

## Details

The function first validates that the repository is in an editable state
using
[`setEditable()`](https://improverse.github.io/improveR/reference/setEditable.md),
then loads the specified step resource and sends a termination command
to the server. The server will stop all running processes associated
with the step.

After a successful termination request, the function automatically polls
the step's status for up to 30 seconds (checking every 0.5 seconds) to
confirm the server has updated the runStatus. This prevents race
conditions when immediately checking status after termination, which is
particularly important in rendered documents where code executes rapidly
without human delays.

Note that terminating a step does not delete any outputs or results that
may have already been generated before termination.

## References

ics1140

## See also

[`runStepResource`](https://improverse.github.io/improveR/reference/runStepResource.md)
to execute a step,
[`finishRunResource`](https://improverse.github.io/improveR/reference/finishRunResource.md)
to wait for step completion

## Examples

``` r
if (FALSE) { # \dontrun{
# Terminate a running step
terminateStepResource(ident = "/improve-tutorial/Modeling/Step 1")

# Terminate with verbose output
terminateStepResource(ident = "/improve-tutorial/Modeling/Step 1", verbose = TRUE)
} # }
```
