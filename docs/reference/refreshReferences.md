# Refresh References

Refresh References

## Usage

``` r
refreshReferences(ident)

updateReferences(...)
```

## Arguments

- ident:

  id

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

The freshly read references, in the same shape as
[`loadReferences()`](https://improverse.github.io/improveR/reference/loadReferences.md) -
the cached copy is dropped first, so the value comes from the server.

## References

ics1206
