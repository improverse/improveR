# Loads All Registered Relation Types

Retrieves the list of available relation types from the repository
server. Results are cached for performance.

## Usage

``` r
loadRelationTypes()
```

## Value

A data frame of relation types with columns such as `id`, `name`,
`reverseName`, `description`, or `NULL` if no relation types are
configured.

## References

ics1592
