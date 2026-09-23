# Load Resource From Server

Loads a resource directly from the server by the resourceId,
resourceVersion ID, or entity ID. The results are returned as a data
frame or a list of data frames. The dates are also converted to POSIX
dates via convertImproveTimestampToPosix. If 0 is handed over, a virtual
root resource is handed back. resourceId can be a list.

## Usage

``` r
loadResourceFromServer(resourceId, invalidatesReproducibility = T)
```

## Arguments

- resourceId:

  the resource id or the entity id of the resource

- invalidatesReproducibility:

  this flag may only be changed by internal functions

## Value

A one-row data frame per resource with the resource fields, dates
converted to POSIX, `isVersion` set to `FALSE`, and entity ids completed
with the repository prefix. The virtual root for `resourceId = "0"`.
`NULL` when the resource could not be read. For a list of ids the rows
of all of them, merged.

## See also

[`convertImproveTimestampToPosix()`](https://improverse.github.io/improveR/reference/convertImproveTimestampToPosix.md)
