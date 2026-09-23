# Refresh Meta Data

Refresh Meta Data

## Usage

``` r
refreshMetaData(ident)

updateMetaData(...)
```

## Arguments

- ident:

  id

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

The freshly read metadata, in the same shape as
[`loadMetaData()`](https://improverse.github.io/improveR/reference/loadMetaData.md) -
the cached copy is dropped first, so the value comes from the server.

## References

ics1096
