# Updates a Resource Relation

Updates a Resource Relation

## Usage

``` r
updateResourceRelation(
  ident,
  relationId,
  newRelationTypeId,
  newDescription,
  from = pwd()
)
```

## Arguments

- ident:

  Identifier of the source resource. Can be a path, resource ID, entity
  ID, or a data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- relationId:

  ID (UUID) of the relation to update.

- newRelationTypeId:

  ID (UUID) of the new relation type.

- newDescription:

  Character. New description of the resource relation.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of the resource's current relations, or `NULL` on failure.

## References

ics1044
