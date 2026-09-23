# Get Latest Run

Retrieves the latest run for a process belonging to a step.

## Usage

``` r
getLatestRun(ident, processId)
```

## Arguments

- ident:

  Step identifier (path, resource ID, entity ID, etc.).

- processId:

  The ID of the process.

## Value

A data frame with the latest run data, or `NULL` if not found.

## References

ics1329
