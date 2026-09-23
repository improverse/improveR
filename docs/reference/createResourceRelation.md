# Creates a New Resource Relation

Creates a relation between two resources in the repository.

## Usage

``` r
createResourceRelation(
  ident,
  targetIdent,
  relationTypeId,
  description,
  from = pwd()
)
```

## Arguments

- ident:

  Identifier of the source resource. Can be a path, resource ID, entity
  ID, or a data frame row from
  [`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md).

- targetIdent:

  Identifier of the target resource. Same formats as `ident`.

- relationTypeId:

  ID (UUID) of the relation type. Use
  [`loadRelationTypes()`](https://improverse.github.io/improveR/reference/loadRelationTypes.md)
  to retrieve available types.

- description:

  Character. Description of the resource relation.

- from:

  Base path for resolving relative paths. Defaults to
  [`pwd()`](https://improverse.github.io/improveR/reference/pwd.md).

## Value

A data frame of the resource's current relations, or `NULL` on failure.

## References

ics1044
