# Create a New Relation Type

Creates a new relation type on the server and refreshes the cached list.

## Usage

``` r
createRelationType(name, reverseName, description = "")
```

## Arguments

- name:

  Character. Display name of the relation type (e.g. "tested by").

- reverseName:

  Character. Reverse display name (e.g. "tests").

- description:

  Character. Description of the relation type.

## Value

The created relation type as a list (with `id`, `name`, `reverseName`,
`description`), or `NULL` on failure.

## References

ics1694
