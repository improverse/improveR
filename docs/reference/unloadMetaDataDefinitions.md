# Unload Meta Data Definitions

Unload Meta Data Definitions

## Usage

``` r
unloadMetaDataDefinitions(scope = "Improve Client")
```

## Arguments

- scope:

  the metadata scope

## Value

No meaningful value - called for its side effect of dropping the
metadata definitions of the scope from the cache. They are
repository-wide configuration, so this affects every later call in the
session, not only the caller's.

## References

ics1137
