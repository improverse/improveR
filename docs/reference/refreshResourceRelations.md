# Reloads the Resource Relations

Reloads the Resource Relations

## Usage

``` r
refreshResourceRelations(ident, from = pwd())

updateResourceRelations(...)
```

## Arguments

- ident:

  Identifier of the resource.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

The freshly read resource relations, in the same shape as
[`loadResourceRelations()`](https://improverse.github.io/improveR/reference/loadResourceRelations.md) -
the cached copy is dropped first, so the value comes from the server.

## References

ics1044
