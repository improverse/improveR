# Refresh Child Resources from Server

Clears cached child resources data and reloads fresh data from the
server.

## Usage

``` r
refreshChildResources(ident)

updateChildResources(...)
```

## Arguments

- ident:

  The resource identifier - can be a resource ID, entity ID, entity
  version ID, or path.

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

A list with updated child resources data. See
[`loadChildResources`](https://improverse.github.io/improveR/reference/loadChildResources.md)
for details on the return structure.

## References

ics1085

## See also

[`loadChildResources`](https://improverse.github.io/improveR/reference/loadChildResources.md)
for return structure details,
[`unloadChildResources`](https://improverse.github.io/improveR/reference/unloadChildResources.md)
to only clear cache
