# Unloads All Resource Relations

Unloads All Resource Relations

## Usage

``` r
unloadResourceRelations(ident, from = pwd())
```

## Arguments

- ident:

  Identifier of the resource.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

No meaningful value - called for its side effect of dropping the
relations of the resource from the cache, so that the next load reads
the server. The value handed back by the internal cache removal is an
implementation detail and must not be relied on.

## References

ics1044
