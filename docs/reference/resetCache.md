# Reset Cache

Empties the entire cache and removes a cacheFile, if it exists. Sets the
step to non-reproducible. The reset cache is similar to its state after
calling improveConnect.

## Usage

``` r
resetCache()
```

## Value

No meaningful value - called for its side effects: the cache file is
deleted, `cacheEnv` is emptied,
[`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md)
is called again with the settings the session had, and the session is
marked non-reproducible (ics1091). The authentication provider and the
`editable` flag are carried over.

## References

ics1091

## See also

[`improveConnect()`](https://improverse.github.io/improveR/reference/improveConnect.md)
