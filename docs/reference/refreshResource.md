# Refresh Resource

Refreshes a resource from the server by clearing cache and reloading.

## Usage

``` r
refreshResource(ident, from = pwd())

updateResource(...)
```

## Arguments

- ident:

  id

- from:

  pwd for relative path

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

The freshly read resource, in the same shape as
[`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).
`NULL` when `ident` resolves to no resource - in that case nothing is
unloaded and nothing is read a second time.

## References

ics1090
