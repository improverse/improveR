# Refresh Child Steps from Server

Clears cached child steps data and reloads fresh data from the server.

## Usage

``` r
refreshChildSteps(ident)

updateChildSteps(...)
```

## Arguments

- ident:

  The resource identifier - can be a resource ID, entity ID, entity
  version ID, or path.

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

A data frame with updated child steps data. See
[`loadChildSteps`](https://improverse.github.io/improveR/reference/loadChildSteps.md)
for details on the return structure.

## References

ics1205

## See also

[`loadChildSteps`](https://improverse.github.io/improveR/reference/loadChildSteps.md)
for return structure details,
[`unloadChildSteps`](https://improverse.github.io/improveR/reference/unloadChildSteps.md)
to only clear cache
