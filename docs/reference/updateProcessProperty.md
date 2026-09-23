# Update a Single Process Property on a Live Step

Reads the current process state from the server, sets the specified key
to the new value, and writes the complete process back. This
read-modify-write approach ensures that existing properties (toolArgs,
runserverId, etc.) are not lost.

## Usage

``` r
updateProcessProperty(stepIdent, processName, key, value, from = pwd())
```

## Arguments

- stepIdent:

  Step identifier (path, resourceId, entityId, or dataframe).

- processName:

  Character. Name of the process (e.g. `"Main"`).

- key:

  Character. Property name to set (e.g. `"checkoutIncludePatterns"`).

- value:

  The value to set.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

The refreshed process dataframe for the step, invisibly.
