# Deletes Metadata From A Resource

Deletes Metadata From A Resource

## Usage

``` r
deleteMetaDate(ident, descriptorName)
```

## Arguments

- ident:

  the resource id of the resource this metadata value is attached to

- descriptorName:

  this descriptorName has to exist, check in preferences

## Value

The refreshed metadata of the resource, as
[`refreshMetaData()`](https://improverse.github.io/improveR/reference/refreshMetaData.md)
returns it: a data frame with the metadata table nested in `$data`.
`NULL` when `ident` resolves to no resource, or is an empty data frame
or an empty vector. For several idents the refreshed metadata of all of
them, merged into one data frame. Note that the value reports the state
**after** the write, not whether the write itself succeeded.

## References

ics1137
