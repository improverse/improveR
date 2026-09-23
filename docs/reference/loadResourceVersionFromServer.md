# Load Resource Version From Server

Loads a resource version directly from the server by the entity version
ID. The results are returned as a data frame or a list of data frames.
The dates are also converted to POSIX dates via
convertImproveTimestampToPosix. entityVersionId can be a list.

## Usage

``` r
loadResourceVersionFromServer(entityVersionId, invalidatesReproducibility = T)
```

## Arguments

- entityVersionId:

  the entity version id of the resource

- invalidatesReproducibility:

  this flag may only be changed by internal functions.

## Value

A one-row data frame per version with the resource fields as of that
revision, dates converted to POSIX and `isVersion` set to `TRUE`. `NULL`
when any step fails - the entity does not resolve, it has no history,
the history is empty, or the revision could not be read; each case is
logged. For a list of ids the rows of all of them, merged.

## See also

[`convertImproveTimestampToPosix()`](https://improverse.github.io/improveR/reference/convertImproveTimestampToPosix.md),[`loadResource()`](https://improverse.github.io/improveR/reference/loadResource.md),
`loadResourceByPathGeneric()`
