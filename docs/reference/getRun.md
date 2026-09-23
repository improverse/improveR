# Get Run

Retrieves a specific run by its ID for a process belonging to a step.

## Usage

``` r
getRun(ident, processId, runId)
```

## Arguments

- ident:

  Step identifier (path, resource ID, entity ID, etc.).

- processId:

  The ID of the process.

- runId:

  The ID of the run.

## Value

A data frame with the run data, or `NULL` if not found.

## References

ics1330
