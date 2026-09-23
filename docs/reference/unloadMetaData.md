# Unload Meta Data

Unload Meta Data

## Usage

``` r
unloadMetaData(ident)
```

## Arguments

- ident:

  id

## Value

No meaningful value - called for its side effect of dropping the
metadata of the resource from the cache, so that the next load reads the
server. The value handed back by the internal cache removal is an
implementation detail and must not be relied on.

## References

ics1096
