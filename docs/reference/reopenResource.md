# Reopen Resource

Reopens a resource that was previously finished, allowing modifications
again.

## Usage

``` r
reopenResource(ident, from = pwd())
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

ics1811
