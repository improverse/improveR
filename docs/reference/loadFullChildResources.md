# loads all child resources by the resourceId, entity ID or entity version ID the childResources REST call does not return all the fields, like run status for steps, just the fields all resources have in common, with this function, the full resources are loaded It Uses Caching the results are returned as a data frame or a list of data frames the dates are also converted to posix dates via convertImproveTimestampToPosix resourceId can be a list

arguments:

## Usage

``` r
loadFullChildResources(ident, from = pwd())
```

## Arguments

- ident:

  the resource id or the entity id of the resource

- from:

  used if a relative path is used

## Value

A data frame with one row per resource, carrying `resourceId`,
`entityId` and the child resources, each with their own children nested
in `$data` - use
[`strip()`](https://improverse.github.io/improveR/reference/strip.md) to
unwrap a single one. `NULL` when `ident` resolves to no resource. For
several idents the result of all of them, merged.

## References

ics1085
