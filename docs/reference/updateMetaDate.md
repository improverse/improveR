# Updates Metadata Value To A Resource

Updates Metadata Value To A Resource

## Usage

``` r
updateMetaDate(ident, descriptorName, value)
```

## Arguments

- ident:

  the resource this metadata value is attached to

- descriptorName:

  name of the metadate

- value:

  the value has to match the type of the descriptor. For dates, posix
  date has to be used, for LOVs the text has to exist

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
