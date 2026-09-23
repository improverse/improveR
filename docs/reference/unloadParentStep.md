# Unload Parent Step

Unload Parent Step

## Usage

``` r
unloadParentStep(ident, from = pwd())
```

## Arguments

- ident:

  resourceID, entityId or path to the step.

- from:

  path working directory, default is the calling step

## Value

No meaningful value - called for its side effect of dropping the parent
step of the resource from the cache, so that the next load reads the
server. The value handed back by the internal cache removal is an
implementation detail and must not be relied on.

## References

ics1209
