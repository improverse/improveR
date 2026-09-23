# refreshFullChildResources

refreshFullChildResources

## Usage

``` r
refreshFullChildResources(ident)

updateFullChildResources(...)
```

## Arguments

- ident:

  id

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

The freshly read child resources, in the same shape as
[`loadFullChildResources()`](https://improverse.github.io/improveR/reference/loadFullChildResources.md) -
the cached copy is dropped first, so the value comes from the server.

## References

ics1085
