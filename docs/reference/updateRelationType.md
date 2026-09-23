# Update an Existing Relation Type

Updates a relation type on the server and refreshes the cached list.

## Usage

``` r
updateRelationType(relationTypeId, name, reverseName, description = "")
```

## Arguments

- relationTypeId:

  Character. UUID of the relation type to update.

- name:

  Character. New display name.

- reverseName:

  Character. New reverse display name.

- description:

  Character. New description.

## Value

The updated relation type as a list, or `NULL` on failure.

## References

ics1699
