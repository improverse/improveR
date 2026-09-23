# Loads the History by the ResourceId, Entity ID or Entity Version ID it uses caching the results are returned as a data frame or a list of data frames the dates are also converted to posix dates via convertImproveTimestampToPosix resourceId can be a list

arguments:

## Usage

``` r
loadMetaData(ident, from = pwd())
```

## Arguments

- ident:

  the resource id or the entity id of the resource

- from:

  used if a relative path is used

## Value

A data frame with one row per resource, carrying `resourceId`,
`entityId` and the metadata entries nested in `$data` - use
[`strip()`](https://improverse.github.io/improveR/reference/strip.md) to
unwrap a single one. `NULL` when `ident` resolves to no resource. For
several idents the result of all of them, merged.

## References

ics1096
