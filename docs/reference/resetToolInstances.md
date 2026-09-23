# Reset Tool Instance Cache

Clears the cached tool instance configuration data, forcing
[`getToolInstances`](https://improverse.github.io/improveR/reference/getToolInstances.md)
to reload fresh tool definitions from the server on the next call. Use
this when tool configurations, runservers, or grid parameters have been
updated on the improve platform.

## Usage

``` r
resetToolInstances()
```

## Value

Invisible `NULL`. Called for its side effect of clearing the cache.

## Details

Tool instances define how workflow steps execute, including which
software to use (R, NONMEM, Monolix), execution parameters, and grid
computing configurations. These definitions are cached for performance
after the first
[`getToolInstances()`](https://improverse.github.io/improveR/reference/getToolInstances.md)
call. When administrators update tool configurations on the server, call
`resetToolInstances()` to ensure your R session uses the latest
definitions.

The function removes all objects from the internal tool instance cache
environment, so the next call to
[`getToolInstances()`](https://improverse.github.io/improveR/reference/getToolInstances.md)
will fetch fresh data from the server.

## See also

[`getToolInstances`](https://improverse.github.io/improveR/reference/getToolInstances.md)
to retrieve tool instance configurations,
[`getMainProcess`](https://improverse.github.io/improveR/reference/getMainProcess.md)
to access the tool instance for a specific step

## Examples

``` r
if (FALSE) { # \dontrun{
# After tool configuration changes on server
resetToolInstances()

# Next call will fetch fresh data
tools <- getToolInstances()
} # }
```
