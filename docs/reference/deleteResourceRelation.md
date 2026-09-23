# Deletes a Resource Relation

Deletes a Resource Relation

## Usage

``` r
deleteResourceRelation(ident, relationId, from = pwd())
```

## Arguments

- ident:

  Identifier of the source resource. Can be a path, resource ID, entity
  ID, or a data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- relationId:

  ID (UUID) of the relation to delete.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

`TRUE` if the relation was deleted successfully, `FALSE` otherwise.

## References

ics1044
