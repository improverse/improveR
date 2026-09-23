# Refresh Parent Step from Server

Refresh Parent Step from Server

## Usage

``` r
refreshParentStep(ident, from = pwd())

updateParentStep(...)
```

## Arguments

- ident:

  resourceID, entityId or path to the step.

- from:

  path working directory, default is the calling step

- ...:

  For backwards compatibility with the deprecated `update*` alias; not
  used by `refresh*` itself.

## Value

The freshly read parent step, in the same shape as
[`loadParentStep()`](https://improverse.github.io/improveR/reference/loadParentStep.md) -
the cached copy is dropped first, so the value comes from the server.

## References

ics1209
