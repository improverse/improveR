# Loads All Registered Resource Relations

Retrieves all resource relations for a given resource. Results are
cached.

## Usage

``` r
loadResourceRelations(ident, from = pwd())
```

## Arguments

- ident:

  Identifier of the resource. Can be a path, resource ID, entity ID, or
  a data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).
  When a UUID string is passed it is used directly as the resource ID
  for backward compatibility.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of resource relations, or `NULL` if none exist.

## References

ics1044
