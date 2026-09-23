# Adds Metadata Value To A Resource, Always Adds, Never Updates

Adds Metadata Value To A Resource, Always Adds, Never Updates

## Usage

``` r
addBulkMetaDate(ident, descriptorNameValueList, scope = "Improve Client")
```

## Arguments

- ident:

  the resource id of the resource this metadata value is attached to

- descriptorNameValueList:

  a list of lists, containing descriptorName and value: this
  descriptorName has to exist, check in preferences, the value has to
  match the type of the descriptor. For dates, posix date has to be
  used, for LOVs the text has to exist

- scope:

  the scope for metadata, default "Improve Client"

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
