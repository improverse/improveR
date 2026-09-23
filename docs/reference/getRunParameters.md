# Get Run Parameters

Retrieves the parameters of a specific run for a process belonging to a
step.

## Usage

``` r
getRunParameters(ident, processId, runId)
```

## Arguments

- ident:

  Step identifier (path, resource ID, entity ID, etc.).

- processId:

  The ID of the process.

- runId:

  The ID of the run.

## Value

A data frame with the run parameters, or `NULL` if not found.

## References

ics1334
