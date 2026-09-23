# Unload Child Resources from Cache

Removes child resources data from the cache for the specified resource.

## Usage

``` r
unloadChildResources(ident)
```

## Arguments

- ident:

  The resource identifier - can be a resource ID, entity ID, entity
  version ID, or path.

## Value

Invisibly returns NULL. Called for side effect of clearing cache.

## References

ics1085

## See also

[`loadChildResources`](https://improverse.github.io/improveR/reference/loadChildResources.md)
to load child resources,
[`refreshChildResources`](https://improverse.github.io/improveR/reference/refreshChildResources.md)
to refresh from server
