# Finish Resource

Sets a resource to the "finished" state, preventing further
modifications until it is reopened.

## Usage

``` r
finishResource(ident, from = pwd())
```

## Arguments

- ident:

  Resource identifier (path, resource ID, entity ID, etc.).

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

`TRUE` if the operation succeeded, `FALSE` otherwise.

## References

ics1810
