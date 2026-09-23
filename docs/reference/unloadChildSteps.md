# Unload Child Steps from Cache

Removes child steps data from the cache for the specified resource.

## Usage

``` r
unloadChildSteps(ident)
```

## Arguments

- ident:

  The resource identifier - can be a resource ID, entity ID, entity
  version ID, or path.

## Value

Invisibly returns NULL. Called for side effect of clearing cache.

## References

ics1205

## See also

[`loadChildSteps`](https://improverse.github.io/improveR/reference/loadChildSteps.md)
to load child steps,
[`refreshChildSteps`](https://improverse.github.io/improveR/reference/refreshChildSteps.md)
to refresh from server
