# Reloads the Relation Types

Reloads the Relation Types

## Usage

``` r
refreshRelationTypes()

updateRelationTypes(...)
```

## Arguments

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

The freshly read relation types, in the same shape as
[`loadRelationTypes()`](https://improverse.github.io/improveR/reference/loadRelationTypes.md) -
the cached copy is dropped first, so the value comes from the server.
